module BndModule

  use KindModule,                   only: DP, LGP, I4B
  use ConstantsModule,              only: LENAUXNAME, LENBOUNDNAME, LENFTYPE,  &
                                          DZERO, DONE,                         &
                                          LENMODELNAME, LENPACKAGENAME,        &
                                          LENMEMPATH, MAXCHARLEN, LINELENGTH,  &
                                          DNODATA, LENLISTLABEL, LENPAKLOC,    &
                                          TABLEFT, TABCENTER
  use SimVariablesModule,           only: errmsg
  use SimModule,                    only: count_errors, store_error, ustop,    &
                                          store_error_unit
  use NumericalPackageModule,       only: NumericalPackageType
  use ObsModule,                    only: ObsType, obs_cr
  use TdisModule,                   only: delt, totimc
  use ObserveModule,                only: ObserveType
  use InputOutputModule,            only: GetUnit, openfile
  use TimeArraySeriesManagerModule, only: TimeArraySeriesManagerType
  use TimeSeriesLinkModule,         only: TimeSeriesLinkType
  use TimeSeriesManagerModule,      only: TimeSeriesManagerType
  use ListModule,                   only: ListType
  use PackageMoverModule,           only: PackageMoverType
  use BaseDisModule,                only: DisBaseType
  use BlockParserModule,            only: BlockParserType
  use TableModule,                  only: TableType, table_cr

  implicit none

  private
  public :: BndType, AddBndToList, GetBndFromList
  public :: save_print_model_flows
  private :: CastAsBndClass

  type, extends(NumericalPackageType) :: BndType
    ! -- characters
    character(len=LENLISTLABEL), pointer :: listlabel  => null()                 !title of table written for RP
    character(len=LENPACKAGENAME) :: text = ''
    character(len=LENAUXNAME), dimension(:), pointer,                           &
                                 contiguous :: auxname => null()                 !vector of auxname
    character(len=LENBOUNDNAME), dimension(:), pointer,                         &
                                 contiguous :: boundname => null()               !vector of boundnames
    !
    ! -- scalars
    integer(I4B), pointer :: isadvpak    => null()                               !flag indicating package is advanced (1) or not (0)
    integer(I4B), pointer :: ibcnum      => null()                               !consecutive package number for this boundary condition
    integer(I4B), pointer :: maxbound    => null()                               !max number of boundaries
    integer(I4B), pointer :: nbound      => null()                               !number of boundaries for current stress period
    integer(I4B), pointer :: ncolbnd     => null()                               !number of columns of the bound array
    integer(I4B), pointer :: iscloc      => null()                               !bound column to scale with SFAC
    integer(I4B), pointer :: naux        => null()                               !number of auxiliary variables
    integer(I4B), pointer :: inamedbound => null()                               !flag to read boundnames
    integer(I4B), pointer :: iauxmultcol => null()                               !column to use as multiplier for column iscloc
    integer(I4B), pointer :: npakeq      => null()                               !number of equations in this package (normally 0 unless package adds rows to matrix)
    integer(I4B), pointer :: ioffset     => null()                               !offset of this package in the model
    ! -- arrays
    integer(I4B), dimension(:), pointer, contiguous :: nodelist => null()        !vector of reduced node numbers
    integer(I4B), dimension(:), pointer, contiguous :: noupdateauxvar => null()  !override auxvars from being updated
    real(DP), dimension(:,:), pointer, contiguous :: bound => null()             !array of package specific boundary numbers
    real(DP), dimension(:), pointer, contiguous :: hcof => null()                !diagonal contribution
    real(DP), dimension(:), pointer, contiguous :: rhs => null()                 !right-hand side contribution
    real(DP), dimension(:,:), pointer, contiguous :: auxvar => null()            !auxiliary variable array
    real(DP), dimension(:), pointer, contiguous :: simvals => null()             !simulated values
    real(DP), dimension(:), pointer, contiguous  :: simtomvr => null()           !simulated to mover values
    !
    ! -- water mover flag and object
    integer(I4B), pointer :: imover => null()
    type(PackageMoverType), pointer :: pakmvrobj => null()
    !
    ! -- timeseries
    type(TimeSeriesManagerType), pointer :: TsManager => null()                  ! time series manager
    type(TimeArraySeriesManagerType), pointer :: TasManager => null()            ! time array series manager
    integer(I4B) :: indxconvertflux = 0                                          ! indxconvertflux is column of bound to multiply by area to convert flux to rate
    logical(LGP) :: AllowTimeArraySeries = .false.
    !
    ! -- pointers for observations
    integer(I4B), pointer :: inobspkg => null()                                  ! unit number for obs package
    type(ObsType), pointer :: obs => null()                                      ! observation package
    !
    ! -- pointers to model/solution variables
    integer(I4B), pointer :: neq                                                 !number of equations for model
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null()          !ibound array
    real(DP), dimension(:), pointer, contiguous :: xnew => null()                !dependent variable (head) for this time step
    real(DP), dimension(:), pointer, contiguous :: xold => null()                !dependent variable for last time step
    real(DP), dimension(:), pointer, contiguous :: flowja => null()              !intercell flows
    integer(I4B), dimension(:), pointer, contiguous :: icelltype => null()       !pointer to icelltype array in NPF
    character(len=LENMEMPATH) :: ictMemPath = ''                                 !< memory path to the icelltype data (for GWF this is in NPF)
    !
    ! -- table objects
    type(TableType), pointer :: inputtab => null()
    type(TableType), pointer :: outputtab => null()
    type(TableType), pointer :: errortab => null()

    
  contains
    procedure :: bnd_df
    procedure :: bnd_ac
    procedure :: bnd_mc
    procedure :: bnd_ar
    procedure :: bnd_rp
    procedure :: bnd_ad
    procedure :: bnd_ck
    procedure :: bnd_cf
    procedure :: bnd_fc
    procedure :: bnd_fn
    procedure :: bnd_nur
    procedure :: bnd_cc
    procedure :: bnd_cq
    procedure :: bnd_bd
    procedure :: bnd_ot
    procedure :: bnd_ot_model_flows
    procedure :: bnd_ot_package_flows
    procedure :: bnd_ot_dv
    procedure :: bnd_ot_bdsummary
    procedure :: bnd_da

    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: pack_initialize
    procedure :: read_options      => bnd_read_options
    procedure :: read_dimensions   => bnd_read_dimensions
    procedure :: read_initial_attr => bnd_read_initial_attr
    procedure :: bnd_options
    procedure :: bnd_cq_simrate
    procedure :: bnd_cq_simtomvr
    procedure :: set_pointers
    procedure :: define_listlabel
    procedure, private :: pak_setup_outputtab
    !
    ! -- procedures to support observations
    procedure, public :: bnd_obs_supported
    procedure, public :: bnd_df_obs
    procedure, public :: bnd_bd_obs
    procedure, public :: bnd_ot_obs
    procedure, public :: bnd_rp_obs
    !
    ! -- procedure to support time series
    procedure, public :: bnd_rp_ts
    !
  end type BndType

  contains

  subroutine bnd_df(this, neq, dis)
! ******************************************************************************
! bnd_df -- Define package options and dimensions
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    use TimeSeriesManagerModule, only: tsmanager_cr
    use TimeArraySeriesManagerModule, only: tasmanager_cr
    ! -- dummy
    class(BndType),intent(inout) :: this
    integer(I4B), intent(inout) :: neq
    class(DisBaseType), pointer :: dis
    ! -- local
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- set pointer to dis object for the model
    this%dis => dis
    !
    ! -- Create time series managers
    call tsmanager_cr(this%TsManager, this%iout)
    call tasmanager_cr(this%TasManager, dis, this%iout)
    !
    ! -- create obs package
    call obs_cr(this%obs, this%inobspkg)
    !
    ! -- Write information to model list file
    write(this%iout,1) this%filtyp, trim(adjustl(this%text)), this%inunit
1   format(1X,/1X,a,' -- ',a,' PACKAGE, VERSION 8, 2/22/2014',                 &
    ' INPUT READ FROM UNIT ',I0)
    !
    ! -- Initialize block parser
    call this%parser%Initialize(this%inunit, this%iout)
    !
    ! -- set and read options
    call this%read_options()
    !
    ! -- Now that time series will have been read, need to call the df
    !    routine to define the manager
    call this%tsmanager%tsmanager_df()
    call this%tasmanager%tasmanager_df()
    !
    ! -- read the package dimensions block
    call this%read_dimensions()
    !
    ! -- update package moffset for packages that add rows
    if (this%npakeq > 0) then
      this%ioffset = neq - this%dis%nodes
    end if
    !
    ! -- update neq
    neq = neq + this%npakeq
    !
    ! -- Store information needed for observations
    if (this%bnd_obs_supported()) then
      call this%obs%obs_df(this%iout, this%packName, this%filtyp, this%dis)
      call this%bnd_df_obs()
    endif
    !
    ! -- return
    return
  end subroutine bnd_df

  subroutine bnd_ac(this, moffset, sparse)
! ******************************************************************************
! bnd_ac -- Add package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SparseModule, only: sparsematrix
    use SimModule, only: store_error, ustop
    ! -- dummy
    class(BndType),intent(inout) :: this
    integer(I4B), intent(in) :: moffset
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    ! -- format
! ------------------------------------------------------------------------------
    !
    !
    ! -- return
    return
  end subroutine bnd_ac

  subroutine bnd_mc(this, moffset, iasln, jasln)
! ******************************************************************************
! bnd_mc -- Map package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType),intent(inout) :: this
    integer(I4B), intent(in) :: moffset
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
    ! -- format
! ------------------------------------------------------------------------------
    !
    !
    ! -- return
    return
  end subroutine bnd_mc

  subroutine bnd_ar(this)
! ******************************************************************************
! bnd_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(BndType),intent(inout) :: this
    ! -- local
    ! -- format
! ------------------------------------------------------------------------------
    !
    call this%obs%obs_ar()
    !
    ! -- Allocate arrays in package superclass
    call this%allocate_arrays()
    !
    ! -- read optional initial package parameters
    call this%read_initial_attr()
    !
    ! -- setup pakmvrobj for standard stress packages
    if (this%imover == 1) then
      allocate(this%pakmvrobj)
      call this%pakmvrobj%ar(this%maxbound, 0, this%memoryPath)
    endif
    !
    ! -- return
    return
  end subroutine bnd_ar
  
  subroutine bnd_rp(this)
! ******************************************************************************
! bnd_rp -- Read and Prepare
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use TdisModule, only: kper, nper
    use SimModule, only: ustop, store_error, store_error_unit
    ! -- dummy
    class(BndType),intent(inout) :: this
    ! -- local
    integer(I4B) :: ierr, nlist
    logical(LGP) :: isfound
    character(len=LINELENGTH) :: line
    ! -- formats
    character(len=*),parameter :: fmtblkerr = &
      "('Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*),parameter :: fmtlsp = &
      "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
    character(len=*), parameter :: fmtnbd = &
      "(1X,/1X,'THE NUMBER OF ACTIVE ',A,'S (',I6, &
       &') IS GREATER THAN MAXIMUM(',I6,')')"
! ------------------------------------------------------------------------------
    !
    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
    if(this%inunit == 0) return
    !
    ! -- get stress period data
    if (this%ionper < kper) then
      !
      ! -- get period block
      call this%parser%GetBlock('PERIOD', isfound, ierr, &
                                supportOpenClose=.true.)
      if (isfound) then
        !
        ! -- read ionper and check for increasing period numbers
        call this%read_check_ionper()
      else
        !
        ! -- PERIOD block not found
        if (ierr < 0) then
          ! -- End of file found; data applies for remainder of simulation.
          this%ionper = nper + 1
        else
          ! -- Found invalid block
          call this%parser%GetCurrentLine(line)
          write(errmsg, fmtblkerr) adjustl(trim(line))
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
      endif
    end if
    !
    ! -- read data if ionper == kper
    if(this%ionper == kper) then
      nlist = -1
      ! -- Remove all time-series and time-array-series links associated with
      !    this package.
      call this%TsManager%Reset(this%packName)
      call this%TasManager%Reset(this%packName)
      !
      ! -- Read data as a list
      call this%dis%read_list(this%parser%iuactive, this%iout,                 &
                               this%iprpak, nlist, this%inamedbound,           &
                               this%iauxmultcol, this%nodelist,                &
                               this%bound, this%auxvar, this%auxname,          &
                               this%boundname, this%listlabel,                 &
                               this%packName, this%tsManager, this%iscloc)
      this%nbound = nlist
      !
      ! Define the tsLink%Text value(s) appropriately.
      ! E.g. for WEL package, entry 1, assign tsLink%Text = 'Q'
      ! For RIV package, entry 1 text = 'STAGE', entry 2 text = 'COND',
      !                  entry 3 text = 'RBOT'; etc.
      call this%bnd_rp_ts()
      !
      ! -- Terminate the block
      call this%parser%terminateblock()
      !
    else
      write(this%iout,fmtlsp) trim(this%filtyp)
    endif
    !
    ! -- return
    return
  end subroutine bnd_rp

  subroutine bnd_ad(this)
    use ConstantsModule, only: DZERO
    class(BndType) :: this
    !this package has no AD routine
    real(DP) :: begintime, endtime
    !
    ! -- Initialize time variables
    begintime = totimc
    endtime = begintime + delt
    !
    ! -- Advance the time series managers
    call this%TsManager%ad()
    call this%TasManager%ad()
    !
    ! -- For each observation, push simulated value and corresponding
    !    simulation time from "current" to "preceding" and reset
    !    "current" value.
    call this%obs%obs_ad()
    !
    return
  end subroutine bnd_ad

  subroutine bnd_ck(this)
! ******************************************************************************
! bnd_ck -- Check boundary condition data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error
    ! -- dummy
    class(BndType),intent(inout) :: this
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- check stress period data
    ! -- each package must override generic functionality
    !
    ! -- return
    return
  end subroutine bnd_ck

  subroutine bnd_cf(this, reset_mover)
! ******************************************************************************
! bnd_cf -- This is the package specific routine where a package adds its
!           contributions to this%rhs and this%hcof
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    class(BndType) :: this
    logical(LGP), intent(in), optional :: reset_mover
! ------------------------------------------------------------------------------
    ! -- bnd has no cf routine
    !
    ! -- return
    return
  end subroutine bnd_cf

  subroutine bnd_fc(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! bnd_fc -- Copy rhs and hcof into solution rhs and amat
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i, n, ipos
! ------------------------------------------------------------------------------
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      n = this%nodelist(i)
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + this%hcof(i)
    enddo
    !
    ! -- return
    return
  end subroutine bnd_fc

  subroutine bnd_fn(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! bnd_fn -- add additional terms to convert conductance formulation
!           to Newton-Raphson formulation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
! ------------------------------------------------------------------------------

    !
    ! -- No addition terms for newton-raphson with constant conductance
    !    boundary conditions
    !
    ! -- return
    return
  end subroutine bnd_fn

  subroutine bnd_nur(this, neqpak, x, xtemp, dx, inewtonur, dxmax, locmax)
! ******************************************************************************
! bnd_nur -- under-relaxation
! Subroutine: (1) Under-relaxation of Groundwater Flow Model Package Heads
!                 for current outer iteration using the cell bottoms at the
!                 bottom of the model
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType), intent(inout) :: this
    integer(I4B), intent(in) :: neqpak
    real(DP), dimension(neqpak), intent(inout) :: x
    real(DP), dimension(neqpak), intent(in) :: xtemp
    real(DP), dimension(neqpak), intent(inout) :: dx
    integer(I4B), intent(inout) :: inewtonur
    real(DP), intent(inout) :: dxmax
    integer(I4B), intent(inout) :: locmax
    ! -- local
! ------------------------------------------------------------------------------

    !
    ! -- Newton-Raphson under-relaxation
    !
    ! -- return
    return
  end subroutine bnd_nur

  subroutine bnd_cc(this, innertot, kiter, iend, icnvgmod, cpak, ipak, dpak)
! ******************************************************************************
! bnd_cc -- additional convergence check for advanced packages
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType), intent(inout) :: this
    integer(I4B), intent(in) :: innertot
    integer(I4B), intent(in) :: kiter
    integer(I4B),intent(in) :: iend
    integer(I4B), intent(in) :: icnvgmod
    character(len=LENPAKLOC), intent(inout) :: cpak
    integer(I4B), intent(inout) :: ipak
    real(DP), intent(inout) :: dpak
    ! -- local
! ------------------------------------------------------------------------------

    !
    ! -- No addition convergence check for boundary conditions
    !
    ! -- return
    return
  end subroutine bnd_cc

  subroutine bnd_cq(this, x, flowja, iadv)
! ******************************************************************************
! bnd_cq -- calculate flows for advanced packages
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType), intent(inout) :: this
    real(DP), dimension(:), intent(in) :: x
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    integer(I4B), optional, intent(in) :: iadv
    ! -- local
    integer(I4B) :: imover
! ------------------------------------------------------------------------------
    !
    ! -- check for iadv optional variable to indicate this is an advanced
    !    package and that mover calculations should not be done here
    if (present(iadv)) then
      if (iadv == 1) then
        imover = 0
      else
        imover = 1
      end if
    else
      imover = this%imover
    end if
    !
    ! -- Calculate package flows.  In the first call, simval is calculated
    !    from hcof, rhs, and head.  The second call may reduce the value in
    !    simval by what is sent to the mover.  The mover rate is stored in
    !    simtomvr.  imover is set to zero here for advanced packages, which
    !    handle and store mover quantities separately.
    call this%bnd_cq_simrate(x, flowja, imover)
    if (imover == 1) then
      call this%bnd_cq_simtomvr(flowja)
    end if
    !
    ! -- return
    return
  end subroutine bnd_cq

  subroutine bnd_cq_simrate(this, hnew, flowja, imover)
! ******************************************************************************
! bnd_cq_simrate -- Calculate flow between package and aquifer and store in
! simval
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(BndType) :: this
    real(DP), dimension(:), intent(in) :: hnew
    real(DP), dimension(:), intent(inout) :: flowja
    integer(I4B), intent(in) :: imover
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: node
    integer(I4B) :: idiag
    real(DP) :: rrate
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- If no boundaries, skip flow calculations.
    if (this%nbound > 0) then
      !
      ! -- Loop through each boundary calculating flow.
      do i = 1, this%nbound
        node = this%nodelist(i)
        !
        ! -- If cell is no-flow or constant-head, then ignore it.
        rrate = DZERO
        if (node > 0) then
          idiag = this%dis%con%ia(node)
          if(this%ibound(node) > 0) then
            !
            ! -- Calculate the flow rate into the cell.
            rrate = this%hcof(i) * hnew(node) - this%rhs(i)
          end if
          flowja(idiag) = flowja(idiag) + rrate
        end if
        !
        ! -- Save simulated value to simvals array.
        this%simvals(i) = rrate
        !
      end do
    endif
    !
    ! -- return
    return
  end subroutine bnd_cq_simrate

  subroutine bnd_cq_simtomvr(this, flowja)
! ******************************************************************************
! bnd_cq_simtomvr -- Calculate the flow between package and boundary that
! is sent to the mover.  Store this value in simtomvr
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(BndType) :: this
    real(DP), dimension(:), intent(inout) :: flowja
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: node
    real(DP) :: q
    real(DP) :: fact
    real(DP) :: rrate
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- If no boundaries, skip flow calculations.
    if (this%nbound > 0) then
      !
      ! -- Loop through each boundary calculating flow.
      do i = 1, this%nbound
        node = this%nodelist(i)
        !
        ! -- If cell is no-flow or constant-head, then ignore it.
        rrate = DZERO
        if (node > 0) then
          if(this%ibound(node) > 0) then
            !
            ! -- Calculate the flow rate into the cell.
            q = this%simvals(i)
            
            
            if (q < DZERO) then
              rrate = this%pakmvrobj%get_qtomvr(i)
              !
              ! -- Evaluate if qtomvr exceeds the calculated rrate.
              !    When fact is greater than 1, qtomvr is numerically
              !    larger than rrate (which should never happen) and 
              !    represents a water budget error. When this happens,
              !    rrate is set to 0. so that the water budget error is
              !    correctly accounted for in the listing water budget. 
              fact = -rrate / q
              if (fact > DONE) then
                ! -- all flow goes to mover
                q = DZERO
              else
                ! -- magnitude of rrate (which is negative) is reduced by 
                !    qtomvr (which is positive)
                q = q + rrate
              end if
              this%simvals(i) = q
              
              if (rrate > DZERO) then
                rrate = -rrate
              end if
            end if
          end if
        end if
        !
        ! -- Save simulated value to simtomvr array.
        this%simtomvr(i) = rrate
        !
      end do
    endif
    !
    ! -- return
    return
  end subroutine bnd_cq_simtomvr
  
  subroutine bnd_bd(this, model_budget)
    ! -- add package ratin/ratout to model budget
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    class(BndType) :: this
    type(BudgetType), intent(inout) :: model_budget
    character (len=LENPACKAGENAME) :: text
    real(DP) :: ratin
    real(DP) :: ratout
    integer(I4B) :: isuppress_output
    isuppress_output = 0
    call rate_accumulator(this%simvals(1:this%nbound), ratin, ratout)
    call model_budget%addentry(ratin, ratout, delt, this%text,                 &
                               isuppress_output, this%packName)
    if (this%imover == 1 .and. this%isadvpak == 0) then
      text = trim(adjustl(this%text)) // '-TO-MVR'
      text = adjustr(text)
      call rate_accumulator(this%simtomvr(1:this%nbound), ratin, ratout)
      call model_budget%addentry(ratin, ratout, delt, text,                    &
                                 isuppress_output, this%packName)
    end if
    
  end subroutine bnd_bd

  subroutine bnd_ot_package_flows(this, icbcfl, ibudfl)
    class(BndType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    !
    ! -- override for advanced packages
  end subroutine bnd_ot_package_flows
  
  subroutine bnd_ot_dv(this, idvsave, idvprint)
    class(BndType) :: this
    integer(I4B), intent(in) :: idvsave
    integer(I4B), intent(in) :: idvprint
    !
    ! -- override for advanced packages
  end subroutine bnd_ot_dv
  
  subroutine bnd_ot_bdsummary(this, kstp, kper, iout)
    class(BndType) :: this
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    integer(I4B), intent(in) :: iout
    !
    ! -- override for advanced packages
  end subroutine bnd_ot_bdsummary
  
  subroutine bnd_ot_model_flows(this,  icbcfl, ibudfl, icbcun, imap)
! ******************************************************************************
! bnd_ot_model_flows -- write flows to binary file and/or print flows to budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    use ConstantsModule, only: LENBOUNDNAME, DZERO
    ! -- dummy
    class(BndType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    integer(I4B), dimension(:), optional, intent(in) :: imap
    ! -- local
    character (len=LINELENGTH) :: title
    character(len=20) :: nodestr
    character (len=LENPACKAGENAME) :: text
    integer(I4B) :: nodeu
    integer(I4B) :: maxrows
    integer(I4B) :: imover
    integer(I4B) :: i
    integer(I4B) :: node
    integer(I4B) :: n2
    integer(I4B) :: ibinun
    integer(I4B) :: naux
    integer(I4B) :: nbound
    real(DP) :: rrate
    ! -- for observations
    character(len=LENBOUNDNAME) :: bname
    ! -- formats
! ------------------------------------------------------------------------------
    ! -- TODO: use new save_print_model_flows subroutine for all this
    !
    ! -- Set mover flag, and shut off if this is an advanced package.
    imover = this%imover
    if (this%isadvpak /= 0) imover = 0
    !
    ! -- set table kstp and kper
    if (this%iprflow /= 0) then
      call this%outputtab%set_kstpkper(kstp, kper)
    end if
    !
    ! -- set maxrows
    maxrows = 0
    if (ibudfl /= 0 .and. this%iprflow /= 0) then
      do i = 1, this%nbound
        node = this%nodelist(i)
        if (node > 0) then
          maxrows = maxrows + 1
        end if
      end do
      if (maxrows > 0) then
        call this%outputtab%set_maxbound(maxrows)
      end if
      title = trim(adjustl(this%text)) // ' PACKAGE (' // trim(this%packName) //     &
              ') FLOW RATES'
      call this%outputtab%set_title(title)
    end if
    !
    ! -- Set unit number for binary output
    if (this%ipakcb < 0) then
      ibinun = icbcun
    else if (this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    end if
    if (icbcfl == 0) then
      ibinun = 0
    end if
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if(ibinun /= 0) then
      !
      ! -- Count nbound as the number of entries with node > 0
      !    SFR, for example, can have a 'none' connection, which
      !    means it should be excluded from budget file
      nbound = 0
      do i = 1, this%nbound
        node = this%nodelist(i)
        if (node > 0) nbound = nbound + 1
      end do
      naux = this%naux
      call this%dis%record_srcdst_list_header(this%text, this%name_model,      &
                  this%name_model, this%name_model, this%packName, naux,       &
                  this%auxname, ibinun, nbound, this%iout)
    endif
    !
    ! -- If no boundaries, skip flow calculations.
    if (this%nbound > 0) then
      !
      ! -- Loop through each boundary calculating flow.
      do i = 1, this%nbound
        node = this%nodelist(i)
        ! -- assign boundary name
        if (this%inamedbound > 0) then
          bname = this%boundname(i)
        else
          bname = ''
        end if
        !
        ! -- If cell is no-flow or constant-head, then ignore it.
        rrate = DZERO
        if (node > 0) then
            !
            ! -- Use simval, which was calculated in cq()
            rrate = this%simvals(i)
            !
            ! -- Print the individual rates if the budget is being printed
            !    and PRINT_FLOWS was specified (this%iprflow<0)
            if (ibudfl /= 0) then
              if (this%iprflow /= 0) then
                !
                ! -- set nodestr and write outputtab table
                nodeu = this%dis%get_nodeuser(node)
                call this%dis%nodeu_to_string(nodeu, nodestr)
                call this%outputtab%print_list_entry(i, trim(adjustl(nodestr)),  &
                                                     rrate, bname)
              end if
            end if
          !
          ! -- If saving cell-by-cell flows in list, write flow
          if (ibinun /= 0) then
            n2 = i
            if (present(imap)) n2 = imap(i)
            call this%dis%record_mf6_list_entry(ibinun, node, n2, rrate,         &
                                                naux, this%auxvar(:,i),          &
                                                olconv2=.FALSE.)
          end if
        end if
        !
      end do
      if (ibudfl /= 0) then
        if (this%iprflow /= 0) then
           write(this%iout,'(1x)')
        end if
      end if

    endif
    !
    ! -- Write/save mover rates
    if (imover == 1) then
      text = trim(adjustl(this%text)) // '-TO-MVR'
      text = adjustr(text)
      if (ibudfl /= 0 .and. this%iprflow /= 0) then
        title = trim(adjustl(this%text)) // ' PACKAGE (' //                    &
                trim(this%packName) // ') FLOW RATES TO-MVR'
        call this%outputtab%set_title(title)
      end if
      !
      ! -- If MOVER cell-by-cell flows will be saved as a list, write header.
      if(ibinun /= 0) then
        naux = this%naux
        call this%dis%record_srcdst_list_header(text, this%name_model,         &
                    this%name_model, this%name_model, this%packName, naux,     &
                    this%auxname, ibinun, nbound, this%iout)
      end if
      !
      ! -- If no boundaries, skip flow calculations.
      if (this%nbound > 0) then
        !
        ! -- Loop through each boundary calculating flow.
        do i = 1, this%nbound
          node = this%nodelist(i)
          ! -- assign boundary name
          if (this%inamedbound>0) then
            bname = this%boundname(i)
          else
            bname = ''
          end if
          !
          ! -- If cell is no-flow or constant-head, then ignore it.
          rrate = DZERO
          if (node > 0) then
            if(this%ibound(node) > 0) then
              !
              ! -- Set rrate to simtomvr, which was calculated in cq()
              rrate = this%simtomvr(i)
              !
              ! -- Print the individual rates if the budget is being printed
              !    and PRINT_FLOWS was specified (this%iprflow<0)
              if(ibudfl /= 0) then
                if(this%iprflow /= 0) then
                  !
                  ! -- set nodestr and write outputtab table
                  nodeu = this%dis%get_nodeuser(node)
                  call this%dis%nodeu_to_string(nodeu, nodestr)
                  call this%outputtab%print_list_entry(i, trim(adjustl(nodestr)),&
                                                       rrate, bname)
                end if
              end if
            end if
            !
            ! -- If saving cell-by-cell flows in list, write flow
            if (ibinun /= 0) then
              n2 = i
              if (present(imap)) n2 = imap(i)
              call this%dis%record_mf6_list_entry(ibinun, node, n2, rrate,     &
                                                  naux, this%auxvar(:,i),      &
                                                  olconv2=.FALSE.)
            end if
          end if
          !
        end do
      end if

    end if
    !
    ! -- return
    return
  end subroutine bnd_ot_model_flows

  subroutine bnd_ot(this, kstp, kper, iout, ihedfl, ibudfl)
! ******************************************************************************
! bnd_ot -- Output package budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType) :: this
    integer(I4B),intent(in) :: kstp
    integer(I4B),intent(in) :: kper
    integer(I4B),intent(in) :: iout
    integer(I4B),intent(in) :: ihedfl
    integer(I4B),intent(in) :: ibudfl
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine bnd_ot

  subroutine bnd_da(this)
! ******************************************************************************
! bnd_da -- Deallocate objects
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(BndType) :: this
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- deallocate arrays
    call mem_deallocate(this%nodelist)
    call mem_deallocate(this%noupdateauxvar)
    call mem_deallocate(this%bound)
    call mem_deallocate(this%hcof)
    call mem_deallocate(this%rhs)
    call mem_deallocate(this%simvals)
    call mem_deallocate(this%simtomvr)
    call mem_deallocate(this%auxvar)
    call mem_deallocate(this%boundname, 'BOUNDNAME', this%memoryPath)
    call mem_deallocate(this%auxname, 'AUXNAME', this%memoryPath)
    nullify(this%icelltype)
    !
    ! -- pakmvrobj
    if (this%imover /= 0) then
      call this%pakmvrobj%da()
      deallocate(this%pakmvrobj)
      nullify(this%pakmvrobj)
    endif
    !
    ! -- input table object
    if (associated(this%inputtab)) then
      call this%inputtab%table_da()
      deallocate(this%inputtab)
      nullify(this%inputtab)
    end if
    !
    ! -- output table object
    if (associated(this%outputtab)) then
      call this%outputtab%table_da()
      deallocate(this%outputtab)
      nullify(this%outputtab)
    end if
    !
    ! -- error table object
    if (associated(this%errortab)) then
      call this%errortab%table_da()
      deallocate(this%errortab)
      nullify(this%errortab)
    end if
    !
    ! -- deallocate character variables
    call mem_deallocate(this%listlabel, 'LISTLABEL', this%memoryPath)
    !
    ! -- Deallocate scalars
    call mem_deallocate(this%isadvpak)
    call mem_deallocate(this%ibcnum)
    call mem_deallocate(this%maxbound)
    call mem_deallocate(this%nbound)
    call mem_deallocate(this%ncolbnd)
    call mem_deallocate(this%iscloc)
    call mem_deallocate(this%naux)
    call mem_deallocate(this%inamedbound)
    call mem_deallocate(this%iauxmultcol)
    call mem_deallocate(this%inobspkg)
    call mem_deallocate(this%imover)
    call mem_deallocate(this%npakeq)
    call mem_deallocate(this%ioffset)
    !
    ! -- deallocate methods on objects
    call this%obs%obs_da()
    call this%TsManager%da()
    call this%TasManager%da()
    !
    ! -- deallocate objects
    deallocate(this%obs)
    deallocate(this%TsManager)
    deallocate(this%TasManager)
    nullify(this%TsManager)
    nullify(this%TasManager)
    !
    ! -- Deallocate parent object
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine bnd_da

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- Allocate Package Members
! Subroutine: (1) allocate
!             (2) initialize
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    use MemoryHelperModule, only: create_mem_path
    ! -- dummy
    class(BndType) :: this
    ! -- local
    integer(I4B), pointer :: imodelnewton => NULL()
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- allocate character variables
    call mem_allocate(this%listlabel, LENLISTLABEL, 'LISTLABEL', this%memoryPath)
    !
    ! -- allocate integer variables
    call mem_allocate(this%isadvpak, 'ISADVPAK', this%memoryPath)
    call mem_allocate(this%ibcnum, 'IBCNUM', this%memoryPath)
    call mem_allocate(this%maxbound, 'MAXBOUND', this%memoryPath)
    call mem_allocate(this%nbound, 'NBOUND', this%memoryPath)
    call mem_allocate(this%ncolbnd, 'NCOLBND', this%memoryPath)
    call mem_allocate(this%iscloc, 'ISCLOC', this%memoryPath)
    call mem_allocate(this%naux, 'NAUX', this%memoryPath)
    call mem_allocate(this%inamedbound, 'INAMEDBOUND', this%memoryPath)
    call mem_allocate(this%iauxmultcol, 'IAUXMULTCOL', this%memoryPath)
    call mem_allocate(this%inobspkg, 'INOBSPKG', this%memoryPath)
    !
    ! -- allocate the object and assign values to object variables
    call mem_allocate(this%imover, 'IMOVER', this%memoryPath)
    !
    ! -- allocate scalars for packages that add rows to the matrix (e.g. MAW)
    call mem_allocate(this%npakeq, 'NPAKEQ', this%memoryPath)
    call mem_allocate(this%ioffset, 'IOFFSET', this%memoryPath)
    !
    ! -- allocate TS objects
    allocate(this%TsManager)
    allocate(this%TasManager)
    !
    ! -- allocate text strings
    call mem_allocate(this%auxname, LENAUXNAME, 0, 'AUXNAME', this%memoryPath)
    !
    ! -- Initialize variables
    this%isadvpak = 0
    this%ibcnum = 0
    this%maxbound = 0
    this%nbound = 0
    this%ncolbnd = 0
    this%iscloc = 0
    this%naux = 0
    this%inamedbound = 0
    this%iauxmultcol = 0
    this%inobspkg = 0
    this%imover = 0
    this%npakeq = 0
    this%ioffset = 0
    !
    ! -- Set pointer to model inewton variable
    call mem_setptr(imodelnewton, 'INEWTON', create_mem_path(this%name_model))
    this%inewton = imodelnewton
    imodelnewton => null()
    !
    ! -- return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this, nodelist, auxvar)
! ******************************************************************************
! allocate_arrays -- Allocate Package Members
! Subroutine: (1) allocate
!             (2) initialize
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(BndType) :: this
    integer(I4B), dimension(:), pointer, contiguous, optional :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, optional :: auxvar
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: j
! ------------------------------------------------------------------------------
    !
    ! -- Point nodelist if it is passed in, otherwise allocate
    if(present(nodelist)) then
      this%nodelist => nodelist
    else
      call mem_allocate(this%nodelist, this%maxbound, 'NODELIST',                &
                        this%memoryPath)
      do j = 1, this%maxbound
        this%nodelist(j) = 0
      end do
    endif
    !
    ! -- noupdateauxvar (allows an external caller to stop auxvars from being
    !    recalculated
    call mem_allocate(this%noupdateauxvar, this%naux, 'NOUPDATEAUXVAR', this%memoryPath)
    this%noupdateauxvar(:) = 0
    !
    ! -- Allocate the bound array
    call mem_allocate(this%bound, this%ncolbnd, this%maxbound, 'BOUND',        &
                      this%memoryPath)
    !
    ! -- Allocate hcof and rhs
    call mem_allocate(this%hcof, this%maxbound, 'HCOF', this%memoryPath)
    call mem_allocate(this%rhs, this%maxbound, 'RHS', this%memoryPath)
    !
    ! -- Allocate the simvals array
    call mem_allocate(this%simvals, this%maxbound, 'SIMVALS', this%memoryPath)
    if (this%imover == 1) then
      call mem_allocate(this%simtomvr, this%maxbound, 'SIMTOMVR', this%memoryPath)
      do i = 1, this%maxbound
        this%simtomvr(i) = DZERO
      enddo
    else
      call mem_allocate(this%simtomvr, 0, 'SIMTOMVR', this%memoryPath)
    endif
    !
    ! -- Point or allocate auxvar
    if(present(auxvar)) then
      this%auxvar => auxvar
    else
      call mem_allocate(this%auxvar, this%naux, this%maxbound, 'AUXVAR',         &
                        this%memoryPath)
      do i = 1, this%maxbound
        do j = 1, this%naux
          this%auxvar(j, i) = DZERO
        end do
      end do
    endif
    !
    ! -- Allocate boundname
    if (this%inamedbound /= 0) then
      call mem_allocate(this%boundname, LENBOUNDNAME, this%maxbound,             &
                        'BOUNDNAME', this%memoryPath)
    end if
    !
    ! -- Set pointer to ICELLTYPE. For GWF boundary packages, 
    !    this%ictMemPath will be 'NPF'.  If boundary packages do not set
    !    this%ictMemPath, then icelltype will remain as null()
    if (this%ictMemPath /= '') then
      call mem_setptr(this%icelltype, 'ICELLTYPE', this%ictMemPath)
    end if
    !
    ! -- Initialize values
    do j = 1, this%maxbound
      do i = 1, this%ncolbnd
        this%bound(i, j) = DZERO
      end do
    end do
    do i = 1, this%maxbound
      this%hcof(i) = DZERO
      this%rhs(i) = DZERO
    end do
    !
    ! -- setup the output table
    call this%pak_setup_outputtab()
    !
    ! -- return
    return
  end subroutine allocate_arrays

  subroutine pack_initialize(this)
! ******************************************************************************
! pack_initialize -- Allocate and/or initialize selected members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BndType) :: this
! ------------------------------------------------------------------------------
    !
    return
  end subroutine pack_initialize

  subroutine set_pointers(this, neq, ibound, xnew, xold, flowja)
! ******************************************************************************
! set_pointers -- Set pointers to model arrays and variables so that a package
!                 has access to these things.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BndType) :: this
    integer(I4B), pointer :: neq
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    real(DP), dimension(:), pointer, contiguous :: xnew
    real(DP), dimension(:), pointer, contiguous :: xold
    real(DP), dimension(:), pointer, contiguous :: flowja
! ------------------------------------------------------------------------------
    !
    ! -- Set the pointers
    this%neq => neq
    this%ibound => ibound
    this%xnew => xnew
    this%xold => xold
    this%flowja => flowja
    !
    ! -- return
  end subroutine set_pointers

  subroutine bnd_read_options(this)
! ******************************************************************************
! read_options -- Read the base package options supported by BndType
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule,   only: urdaux
    use MemoryManagerModule, only: mem_reallocate
    use SimModule,           only: ustop, store_error, store_error_unit
    ! -- dummy
    class(BndType),intent(inout) :: this
    ! -- local
    character(len=:), allocatable :: line
    character(len=LINELENGTH) :: fname
    character(len=LINELENGTH) :: keyword
    character(len=LENAUXNAME) :: sfacauxname
    character(len=LENAUXNAME), dimension(:), allocatable :: caux
    integer(I4B) :: lloc
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: n
    integer(I4B) :: ierr
    integer(I4B) :: inobs
    logical(LGP) :: isfound
    logical(LGP) :: endOfBlock
    logical(LGP) :: foundchildclassoption
    ! -- format
    character(len=*),parameter :: fmtflow = &
      "(4x, 'FLOWS WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
    character(len=*),parameter :: fmtflow2 = &
      "(4x, 'FLOWS WILL BE SAVED TO BUDGET FILE SPECIFIED IN OUTPUT CONTROL')"
    character(len=*), parameter :: fmttas = &
      "(4x, 'TIME-ARRAY SERIES DATA WILL BE READ FROM FILE: ', a)"
    character(len=*), parameter :: fmtts = &
      "(4x, 'TIME-SERIES DATA WILL BE READ FROM FILE: ', a)"
    character(len=*), parameter :: fmtnme = &
      "(a, i0, a)"
! ------------------------------------------------------------------------------
    !
    ! -- set default options
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)') 'PROCESSING '//trim(adjustl(this%text)) &
        //' OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) then
          exit
        end if
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case('AUX', 'AUXILIARY')
            call this%parser%GetRemainingLine(line)
            lloc = 1
            call urdaux(this%naux, this%parser%iuactive, this%iout, lloc,        &
                        istart, istop, caux, line, this%text)
            call mem_reallocate(this%auxname, LENAUXNAME, this%naux,             &
                                'AUXNAME', this%memoryPath)
            do n = 1, this%naux
              this%auxname(n) = caux(n)
            end do
            deallocate(caux)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtflow2)
          case ('PRINT_INPUT')
            this%iprpak = 1
            write(this%iout,'(4x,a)') 'LISTS OF '//trim(adjustl(this%text))// &
              ' CELLS WILL BE PRINTED.'
          case ('PRINT_FLOWS')
            this%iprflow = 1
            write(this%iout,'(4x,a)') trim(adjustl(this%text))// &
              ' FLOWS WILL BE PRINTED TO LISTING FILE.'
          case ('BOUNDNAMES')
            this%inamedbound = 1
            write(this%iout,'(4x,a)') trim(adjustl(this%text))// &
              ' BOUNDARIES HAVE NAMES IN LAST COLUMN.'
          case ('TS6')
            call this%parser%GetStringCaps(keyword)
            if(trim(adjustl(keyword)) /= 'FILEIN') then
              errmsg = 'TS6 keyword must be followed by "FILEIN" ' //          &
                       'then by filename.'
              call store_error(errmsg)
            endif
            call this%parser%GetString(fname)
            write(this%iout,fmtts)trim(fname)
            call this%TsManager%add_tsfile(fname, this%inunit)
          case ('TAS6')
            if (this%AllowTimeArraySeries) then
              if (.not. this%dis%supports_layers()) then
                errmsg = 'TAS6 FILE cannot be used ' // &
                         'with selected discretization type.'
                call store_error(errmsg)
              endif
            else
              errmsg = 'The ' // trim(this%filtyp) // &
                       ' package does not support TIMEARRAYSERIESFILE'
              call store_error(errmsg)
              call this%parser%StoreErrorUnit()
              call ustop()
            endif
            call this%parser%GetStringCaps(keyword)
            if(trim(adjustl(keyword)) /= 'FILEIN') then
              errmsg = 'TAS6 keyword must be followed by "FILEIN" ' //         &
                       'then by filename.'
              call store_error(errmsg)
              call this%parser%StoreErrorUnit()
              call ustop()
            endif
            call this%parser%GetString(fname)
            write(this%iout,fmttas)trim(fname)
            call this%TasManager%add_tasfile(fname)
          case ('AUXMULTNAME')
            call this%parser%GetStringCaps(sfacauxname)
            this%iauxmultcol = -1
            write(this%iout, '(4x,a,a)')                                       &
                             'AUXILIARY MULTIPLIER NAME: ', sfacauxname
          case ('OBS6')
            call this%parser%GetStringCaps(keyword)
            if(trim(adjustl(keyword)) /= 'FILEIN') then
              errmsg = 'OBS6 keyword must be followed by "FILEIN" ' //         &
                       'then by filename.'
              call store_error(errmsg)
              call this%parser%StoreErrorUnit()
              call ustop()
            endif
            if (this%obs%active) then
              errmsg = 'Multiple OBS6 keywords detected in OPTIONS block. ' // &
                       'Only one OBS6 entry allowed for a package.'
              call store_error(errmsg)
            endif
            this%obs%active = .true.
            call this%parser%GetString(this%obs%inputFilename)
            inobs = GetUnit()
            call openfile(inobs, this%iout, this%obs%inputFilename, 'OBS')
            this%obs%inUnitObs = inobs
          !
          ! -- right now these are options that are only available in the
          !    development version and are not included in the documentation.
          !    These options are only available when IDEVELOPMODE in
          !    constants module is set to 1
          case ('DEV_NO_NEWTON')
            call this%parser%DevOpt()
            this%inewton = 0
            write(this%iout, '(4x,a)')                                           &
              'NEWTON-RAPHSON method disabled for unconfined cells'
          case default
            !
            ! -- Check for child class options
            call this%bnd_options(keyword, foundchildclassoption)
            !
            ! -- No child class options found, so print error message
            if(.not. foundchildclassoption) then
              write(errmsg,'(a,3(1x,a))')                                        &
                'UNKNOWN', trim(adjustl(this%text)), 'OPTION:', trim(keyword)
              call store_error(errmsg)
            endif
        end select
      end do
      write(this%iout,'(1x,a)') 'END OF '//trim(adjustl(this%text)) // ' OPTIONS'
    else
      write(this%iout,'(1x,a)') 'NO '//trim(adjustl(this%text)) //               &
        ' OPTION BLOCK DETECTED.'
    end if
    !
    ! -- AUXMULTNAME was specified, so find column of auxvar that will be multiplier
    if(this%iauxmultcol < 0) then
      !
      ! -- Error if no aux variable specified
      if(this%naux == 0) then
        write(errmsg,'(a,2(1x,a))')                                              &
          'AUXMULTNAME WAS SPECIFIED AS', trim(adjustl(sfacauxname)),            &
          'BUT NO AUX VARIABLES SPECIFIED.'
        call store_error(errmsg)
      endif
      !
      ! -- Assign mult column
      this%iauxmultcol = 0
      do n = 1, this%naux
        if(sfacauxname == this%auxname(n)) then
          this%iauxmultcol = n
          exit
        endif
      enddo
      !
      ! -- Error if aux variable cannot be found
      if(this%iauxmultcol == 0) then
        write(errmsg,'(a,2(1x,a))')                                              &
          'AUXMULTNAME WAS SPECIFIED AS', trim(adjustl(sfacauxname)),            &
          'BUT NO AUX VARIABLE FOUND WITH THIS NAME.'
        call store_error(errmsg)
      endif
    end if
    !
    ! -- terminate if errors were detected
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine bnd_read_options

  subroutine bnd_read_dimensions(this)
! ******************************************************************************
! bnd_read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    ! -- dummy
    class(BndType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: keyword
    logical(LGP) :: isfound
    logical(LGP) :: endOfBlock
    integer(I4B) :: ierr
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- get dimensions block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse dimensions block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('MAXBOUND')
            this%maxbound = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)') 'MAXBOUND = ', this%maxbound
          case default
            write(errmsg,'(a,3(1x,a))') &
              'UNKNOWN', trim(this%text), 'DIMENSION:', trim(keyword)
            call store_error(errmsg)
        end select
      end do
      !
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' DIMENSIONS'
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- verify dimensions were set
    if(this%maxbound <= 0) then
      write(errmsg, '(a)') 'MAXBOUND MUST BE AN INTEGER GREATER THAN ZERO.'
      call store_error(errmsg)
    end if
    !
    ! -- terminate if there are errors
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- return
    return
  end subroutine bnd_read_dimensions

  subroutine bnd_read_initial_attr(this)
! ******************************************************************************
! bndreadinitialparms -- Read initial parameters for this package
!                         Most packages do not need initial parameter data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType),intent(inout) :: this
    ! -- local
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine bnd_read_initial_attr

  subroutine bnd_options(this, option, found)
! ******************************************************************************
! bnd_options -- set options for a class derived from BndType
! This subroutine can be overridden by specific packages to set custom options
! that are not part of the package superclass.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType),intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical(LGP), intent(inout) :: found
! ------------------------------------------------------------------------------
    !
    ! Return with found = .false.
    found = .false.
    !
    ! -- return
    return
  end subroutine bnd_options

  subroutine pak_setup_outputtab(this)
! ******************************************************************************
! bnd_options -- set options for a class derived from BndType
! This subroutine can be overridden by specific packages to set custom options
! that are not part of the package superclass.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: text
    integer(I4B) :: ntabcol
! ------------------------------------------------------------------------------
    !
    ! -- allocate and initialize the output table
    if (this%iprflow /= 0) then
      !
      ! -- dimension table
      ntabcol = 3
      if (this%inamedbound > 0) then
        ntabcol = ntabcol + 1
      end if
      !
      ! -- initialize the output table object
      title = trim(adjustl(this%text)) // ' PACKAGE (' // trim(this%packName) //     &
              ') FLOW RATES'
      call table_cr(this%outputtab, this%packName, title)
      call this%outputtab%table_df(this%maxbound, ntabcol, this%iout,            &
                                    transient=.TRUE.)
      text = 'NUMBER'
      call this%outputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'CELLID'
      call this%outputtab%initialize_column(text, 20, alignment=TABLEFT)
      text = 'RATE'
      call this%outputtab%initialize_column(text, 15, alignment=TABCENTER)
      if (this%inamedbound > 0) then
        text = 'NAME'
        call this%outputtab%initialize_column(text, LENBOUNDNAME, alignment=TABLEFT)
      end if
    end if
    !
    ! -- return
    return
  end subroutine pak_setup_outputtab


  subroutine define_listlabel(this)
    ! define_listlabel
    ! -- List-type packages should always override define_listlabel
    !    to enable "NAME" to be added to label.
    class(BndType), intent(inout) :: this
    !
    return
  end subroutine define_listlabel

  ! -- Procedures related to observations

  function bnd_obs_supported(this) result(supported)
    ! **************************************************************************
    ! bnd_obs_supported
    !   -- Return true if package supports observations. Default is false.
    !   -- Needs to be a BndType procedure.
    !   -- Override for packages that do support observations.
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    ! -- return variable
    logical(LGP) :: supported
    ! -- dummy
    class(BndType) :: this
    ! --------------------------------------------------------------------------
    supported = .false.
    !
    ! -- Return
    return
  end function bnd_obs_supported

  subroutine bnd_df_obs(this)
    ! **************************************************************************
    ! bnd_df_obs
    !   -- Store observation type(s) supported by package.
    !   -- Needs to be a BndType procedure.
    !   -- Override in any package that supports observations.
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
      class(BndType) :: this
    ! --------------------------------------------------------------------------
    ! -- do nothing here. Override as needed.
    return
  end subroutine bnd_df_obs

  subroutine bnd_rp_obs(this)
    ! -- This procedure should be overridden in any package that
    !    supports observations and needs to check user input
    !    or process observation input using package data in some
    !    other way.
    ! -- dummy
    class(BndType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: j
    class(ObserveType), pointer :: obsrv => null()
    character(len=LENBOUNDNAME) :: bname
    logical(LGP) :: jfound
    !
    if (.not. this%bnd_obs_supported()) return
    !
    do i = 1, this%obs%npakobs
      obsrv => this%obs%pakobs(i)%obsrv
      !
      ! -- indxbnds needs to be reset each stress period because 
      !    list of boundaries can change each stress period.
      call obsrv%ResetObsIndex()
      obsrv%BndFound = .false.
      !
      bname = obsrv%FeatureName
      if (bname /= '') then
        !
        ! -- Observation location(s) is(are) based on a boundary name.
        !    Iterate through all boundaries to identify and store
        !    corresponding index(indices) in bound array.
        jfound = .false.
        do j=1,this%nbound
          if (this%boundname(j) == bname) then
            jfound = .true.
            obsrv%BndFound = .true.
            obsrv%CurrentTimeStepEndValue = DZERO
            call obsrv%AddObsIndex(j)
          end if
        end do
      else
        !
        ! -- Observation location is a single node number
        jfound = .false.
        jloop: do j=1,this%nbound
          if (this%nodelist(j) == obsrv%NodeNumber) then
            jfound = .true.
            obsrv%BndFound = .true.
            obsrv%CurrentTimeStepEndValue = DZERO
            call obsrv%AddObsIndex(j)
          end if
        end do jloop
      end if
    end do
    !
    if (count_errors() > 0) then
      call store_error_unit(this%inunit)
      call ustop()
    endif
    !
    return
  end subroutine bnd_rp_obs

  subroutine bnd_bd_obs(this)
    ! obs_bd
    ! -- Generic procedure to save simulated values for
    !    all observations defined for a package.
    class(BndType) :: this
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: n
    real(DP) :: v
    type(ObserveType), pointer :: obsrv => null()
    !---------------------------------------------------------------------------
    !
    call this%obs%obs_bd_clear()
    !
    ! -- Save simulated values for all of package's observations.
    do i=1,this%obs%npakobs
      obsrv => this%obs%pakobs(i)%obsrv
      if (obsrv%BndFound) then
        do n = 1, obsrv%indxbnds_count
          if (obsrv%ObsTypeId == 'TO-MVR') then
            if (this%imover == 1) then
              v = this%pakmvrobj%get_qtomvr(obsrv%indxbnds(n))
              if (v > DZERO) then
                v = -v
              end if
            else
              v = DNODATA
            end if
          else
            v = this%simvals(obsrv%indxbnds(n))
          end if
          call this%obs%SaveOneSimval(obsrv, v)
        enddo
      else
        call this%obs%SaveOneSimval(obsrv, DNODATA)
      endif
    enddo
    !
    return
  end subroutine bnd_bd_obs

  subroutine bnd_ot_obs(this)
    ! -- Generic procedure to save simulated values for
    !    all observations defined for a package.
    ! -- dummy
    class(BndType) :: this
    ! -- local
    !---------------------------------------------------------------------------
    !
    call this%obs%obs_ot()
    !
    return
  end subroutine bnd_ot_obs

  ! -- Procedures related to time series

  subroutine bnd_rp_ts(this)
    ! -- Generic procedure to assign tsLink%Text appropriately for
    !    all time series in use by package.
    !    Override as needed.
    ! -- dummy
    class(BndType), intent(inout) :: this
    !
    return
  end subroutine bnd_rp_ts

  ! -- Procedures related to casting

  function CastAsBndClass(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(BndType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (BndType)
      res => obj
    end select
    return
  end function CastAsBndClass

  subroutine AddBndToList(list, bnd)
    implicit none
    ! -- dummy
    type(ListType),          intent(inout) :: list
    class(BndType), pointer, intent(inout) :: bnd
    ! -- local
    class(*), pointer :: obj
    !
    obj => bnd
    call list%Add(obj)
    !
    return
  end subroutine AddBndToList

  function GetBndFromList(list, idx) result (res)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B),        intent(in)    :: idx
    class(BndType),    pointer    :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsBndClass(obj)
    !
    return
  end function GetBndFromList

  subroutine save_print_model_flows(icbcfl, ibudfl, icbcun, iprflow, &
    outputtab, nbound, nodelist, flow, ibound, title, text, ipakcb, dis, naux, &
    textmodel, textpackage, dstmodel, dstpackage, auxname, auxvar, iout, &
    inamedbound, boundname, imap)
! ******************************************************************************
! save_print_model_flows -- write flows to binary file and/or print flows to budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    use ConstantsModule, only: LENBOUNDNAME, DZERO
    ! -- dummy
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    integer(I4B), intent(in) :: iprflow !< print flows to list file
    type(TableType), intent(inout) :: outputtab
    integer(I4B), intent(in) :: nbound
    integer(I4B), dimension(:), contiguous, intent(in) :: nodelist
    real(DP), dimension(:), contiguous, intent(in) :: flow
    integer(I4B), dimension(:), contiguous, intent(in) :: ibound
    character(len=*), intent(in) :: title
    character(len=*), intent(in) :: text
    integer(I4B), intent(in) :: ipakcb
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: naux
    character(len=*), intent(in) :: textmodel
    character(len=*), intent(in) :: textpackage
    character(len=*), intent(in) :: dstmodel
    character(len=*), intent(in) :: dstpackage
    character(len=*), dimension(:), intent(in) :: auxname
    real(DP), dimension(:, :), intent(in) :: auxvar
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(in) :: inamedbound
    character(len=LENBOUNDNAME), dimension(:), contiguous :: boundname
    integer(I4B), dimension(:), optional, intent(in) :: imap
    ! -- local
    character(len=20) :: nodestr
    integer(I4B) :: nodeu
    integer(I4B) :: maxrows
    integer(I4B) :: i
    integer(I4B) :: node
    integer(I4B) :: n2
    integer(I4B) :: ibinun
    integer(I4B) :: nboundcount
    real(DP) :: rrate
    ! -- for observations
    character(len=LENBOUNDNAME) :: bname
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- set table kstp and kper
    if (iprflow /= 0) then
      call outputtab%set_kstpkper(kstp, kper)
    end if
    !
    ! -- set maxrows
    maxrows = 0
    if (ibudfl /= 0 .and. iprflow /= 0) then
      do i = 1, nbound
        node = nodelist(i)
        if (node > 0) then
          if (ibound(node) > 0) then
            maxrows = maxrows + 1
          end if
        end if
      end do
      if (maxrows > 0) then
        call outputtab%set_maxbound(maxrows)
      end if
      call outputtab%set_title(title)
    end if
    !
    ! -- Set unit number for binary output
    if (ipakcb < 0) then
      ibinun = icbcun
    else if (ipakcb == 0) then
      ibinun = 0
    else
      ibinun = ipakcb
    end if
    if (icbcfl == 0) then
      ibinun = 0
    end if
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if(ibinun /= 0) then
      !
      ! -- Count nbound as the number of entries with node > 0
      !    SFR, for example, can have a 'none' connection, which
      !    means it should be excluded from budget file
      nboundcount = 0
      do i = 1, nbound
        node = nodelist(i)
        if (node > 0) nboundcount = nboundcount + 1
      end do
      call dis%record_srcdst_list_header(text, textmodel, textpackage, &
                  dstmodel, dstpackage, naux,                          &
                  auxname, ibinun, nboundcount, iout)
    endif
    !
    ! -- If no boundaries, skip flow calculations.
    if (nbound > 0) then
      !
      ! -- Loop through each boundary calculating flow.
      do i = 1, nbound
        node = nodelist(i)
        ! -- assign boundary name
        if (inamedbound > 0) then
          bname = boundname(i)
        else
          bname = ''
        end if
        !
        ! -- If cell is no-flow or constant-head, then ignore it.
        rrate = DZERO
        if (node > 0) then
            !
            ! -- Use simval, which was calculated in cq()
            rrate = flow(i)
            !
            ! -- Print the individual rates if the budget is being printed
            !    and PRINT_FLOWS was specified (iprflow < 0)
            if (ibudfl /= 0) then
              if (iprflow /= 0) then
                !
                ! -- set nodestr and write outputtab table
                nodeu = dis%get_nodeuser(node)
                call dis%nodeu_to_string(nodeu, nodestr)
                call outputtab%print_list_entry(i, trim(adjustl(nodestr)),  &
                                                     rrate, bname)
              end if
            end if
          !
          ! -- If saving cell-by-cell flows in list, write flow
          if (ibinun /= 0) then
            n2 = i
            if (present(imap)) n2 = imap(i)
            call dis%record_mf6_list_entry(ibinun, node, n2, rrate,         &
                                                naux, auxvar(:,i),          &
                                                olconv2=.FALSE.)
          end if
        end if
        !
      end do
      if (ibudfl /= 0) then
        if (iprflow /= 0) then
           write(iout,'(1x)')
        end if
      end if

    endif
    !
    ! -- return
    return
  end subroutine save_print_model_flows

end module BndModule

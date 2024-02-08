!> @brief This module contains the storage package methods
!!
!! This module contains the methods used to add the effects of storage
!! on the groundwater flow equation. The contribution of specific
!! storage and specific yield can be represented.
!!
!<
module GwfStoModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DEM6, DEM4, DHALF, DONE, DTWO, &
                             LENBUDTXT, LINELENGTH
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, count_errors
  use SmoothingModule, only: sQuadraticSaturation, &
                             sQuadraticSaturationDerivative, &
                             sQSaturation, sLinearSaturation
  use BaseDisModule, only: DisBaseType
  use NumericalPackageModule, only: NumericalPackageType
  use BlockParserModule, only: BlockParserType
  use GwfStorageUtilsModule, only: SsCapacity, SyCapacity, SsTerms, SyTerms
  use InputOutputModule, only: GetUnit, openfile
  use TvsModule, only: TvsType, tvs_cr
  use MatrixBaseModule

  implicit none
  public :: GwfStoType, sto_cr

  character(len=LENBUDTXT), dimension(2) :: budtxt = & !< text labels for budget terms
    &['          STO-SS', '          STO-SY']

  type, extends(NumericalPackageType) :: GwfStoType
    integer(I4B), pointer :: istor_coef => null() !< indicates if ss is the storage coefficient
    integer(I4B), pointer :: iconf_ss => null() !< indicates if ss is 0 below the top of a layer
    integer(I4B), pointer :: iorig_ss => null() !< indicates if the original storage specific storage formulation should be used
    integer(I4B), pointer :: iss => null() !< steady state flag: 1 = steady, 0 = transient
    integer(I4B), pointer :: iusesy => null() !< flag set if any cell is convertible (0, 1)
    integer(I4B), dimension(:), pointer, contiguous :: iconvert => null() !< confined (0) or convertible (1)
    real(DP), dimension(:), pointer, contiguous :: ss => null() !< specfic storage or storage coefficient
    real(DP), dimension(:), pointer, contiguous :: sy => null() !< specific yield
    real(DP), dimension(:), pointer, contiguous :: strgss => null() !< vector of specific storage rates
    real(DP), dimension(:), pointer, contiguous :: strgsy => null() !< vector of specific yield rates
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null() !< pointer to model ibound
    real(DP), pointer :: satomega => null() !< newton-raphson saturation omega
    integer(I4B), pointer :: integratechanges => null() !< indicates if mid-simulation ss and sy changes should be integrated via an additional matrix formulation term
    integer(I4B), pointer :: intvs => null() !< TVS (time-varying storage) unit number (0 if unused)
    type(TvsType), pointer :: tvs => null() !< TVS object
    real(DP), dimension(:), pointer, contiguous, private :: oldss => null() !< previous time step specific storage
    real(DP), dimension(:), pointer, contiguous, private :: oldsy => null() !< previous time step specific yield
  contains
    procedure :: sto_ar
    procedure :: sto_rp
    procedure :: sto_ad
    procedure :: sto_fc
    procedure :: sto_fn
    procedure :: sto_cq
    procedure :: sto_bd
    procedure :: sto_save_model_flows
    procedure :: sto_da
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
    !procedure, private :: register_handlers
    procedure, private :: read_options
    procedure, private :: read_data
    procedure, private :: save_old_ss_sy
  end type

contains

  !> @ brief Create a new package object
  !!
  !!  Create a new storage (STO) object
  !!
  !<
  subroutine sto_cr(stoobj, name_model, inunit, iout)
    ! -- dummy variables
    type(GwfStoType), pointer :: stoobj !< GwfStoType object
    character(len=*), intent(in) :: name_model !< name of model
    integer(I4B), intent(in) :: inunit !< package input file unit
    integer(I4B), intent(in) :: iout !< model listing file unit
    !
    ! -- Create the object
    allocate (stoobj)
    !
    ! -- create name and memory path
    call stoobj%set_names(1, name_model, 'STO', 'STO')
    !
    ! -- Allocate scalars
    call stoobj%allocate_scalars()
    !
    ! -- Set variables
    stoobj%inunit = inunit
    stoobj%iout = iout
    !
    ! -- Initialize block parser
    call stoobj%parser%Initialize(stoobj%inunit, stoobj%iout)
    !
    ! -- return
    return
  end subroutine sto_cr

  !> @ brief Allocate and read method for package
  !!
  !!  Method to allocate and read static data for the STO package.
  !!
  !<
  subroutine sto_ar(this, dis, ibound)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    use MemoryHelperModule, only: create_mem_path
    ! -- dummy variables
    class(GwfStoType) :: this !< GwfStoType object
    class(DisBaseType), pointer, intent(in) :: dis !< model discretization object
    integer(I4B), dimension(:), pointer, contiguous :: ibound !< model ibound array
    ! -- local variables
    ! -- formats
    character(len=*), parameter :: fmtsto = &
      "(1x,/1x,'STO -- STORAGE PACKAGE, VERSION 1, 5/19/2014', &
      &' INPUT READ FROM UNIT ', i0, //)"
    !
    ! --print a message identifying the storage package.
    write (this%iout, fmtsto) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis => dis
    this%ibound => ibound
    !
    ! -- set pointer to gwf iss
    call mem_setptr(this%iss, 'ISS', create_mem_path(this%name_model))
    !
    ! -- Allocate arrays
    call this%allocate_arrays(dis%nodes)
    !!
    !! -- Register side effect handlers
    !call this%register_handlers()
    !
    ! -- Read storage options
    call this%read_options()
    !
    ! -- read the data block
    call this%read_data()
    !
    ! -- TVS
    if (this%intvs /= 0) then
      call this%tvs%ar(this%dis)
    end if
    !
    ! -- return
    return
  end subroutine sto_ar

  !> @ brief Read and prepare method for package
  !!
  !!  Method to read and prepare stress period data for the STO package.
  !!
  !<
  subroutine sto_rp(this)
    ! -- modules
    use TdisModule, only: kper, nper
    implicit none
    ! -- dummy variables
    class(GwfStoType) :: this !< GwfStoType object
    ! -- local variables
    integer(I4B) :: ierr
    logical :: isfound, readss, readsy, endOfBlock
    character(len=16) :: css(0:1)
    character(len=LINELENGTH) :: line, keyword
    ! -- formats
    character(len=*), parameter :: fmtlsp = &
      &"(1X,/1X,'REUSING ',A,' FROM LAST STRESS PERIOD')"
    character(len=*), parameter :: fmtblkerr = &
      &"('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    ! -- data
    data css(0)/'       TRANSIENT'/
    data css(1)/'    STEADY-STATE'/
! ------------------------------------------------------------------------------
    !
    ! -- Store ss and sy values from end of last stress period if needed
    if (this%integratechanges /= 0) then
      call this%save_old_ss_sy()
    end if
    !
    ! -- get stress period data
    if (this%ionper < kper) then
      !
      ! -- get period block
      call this%parser%GetBlock('PERIOD', isfound, ierr, &
                                supportOpenClose=.true., &
                                blockRequired=.false.)
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
          write (errmsg, fmtblkerr) adjustl(trim(line))
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end if
      end if
    end if
    !
    ! -- read data if ionper == kper
    ! are these here to anticipate reading ss,sy per stress period?
    readss = .false.
    readsy = .false.

    !stotxt = aname(2)
    if (this%ionper == kper) then
      write (this%iout, '(//,1x,a)') 'PROCESSING STORAGE PERIOD DATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('STEADY-STATE')
          this%iss = 1
        case ('TRANSIENT')
          this%iss = 0
        case default
          write (errmsg, '(a,a)') 'Unknown STORAGE data tag: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END PROCESSING STORAGE PERIOD DATA'
      !else
      !  write(this%iout,fmtlsp) 'STORAGE VALUES'
    end if

    write (this%iout, '(//1X,A,I0,A,A,/)') &
      'STRESS PERIOD ', kper, ' IS ', trim(adjustl(css(this%iss)))
    !
    ! -- TVS
    if (this%intvs /= 0) then
      call this%tvs%rp()
    end if
    !
    ! -- return
    return
  end subroutine sto_rp

  !> @ brief Advance the package
  !!
  !!  Advance data in the STO package.
  !!
  !<
  subroutine sto_ad(this)
    ! -- modules
    use TdisModule, only: kstp
    ! -- dummy variables
    class(GwfStoType) :: this !< GwfStoType object
    !
    ! -- Store ss and sy values from end of last time step if needed
    if (this%integratechanges /= 0 .and. kstp > 1) then
      call this%save_old_ss_sy()
    end if
    !
    ! -- TVS
    if (this%intvs /= 0) then
      call this%tvs%ad()
    end if
    !
    ! -- return
    return
  end subroutine sto_ad

  !> @ brief Fill A and right-hand side for the package
  !!
  !!  Fill the coefficient matrix and right-hand side with the STO package terms.
  !!
  !<
  subroutine sto_fc(this, kiter, hold, hnew, matrix_sln, idxglo, rhs)
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy variables
    class(GwfStoType) :: this !< GwfStoType object
    integer(I4B), intent(in) :: kiter !< outer iteration number
    real(DP), intent(in), dimension(:) :: hold !< previous heads
    real(DP), intent(in), dimension(:) :: hnew !< current heads
    class(MatrixBaseType), pointer :: matrix_sln !< A matrix
    integer(I4B), intent(in), dimension(:) :: idxglo !< global index model to solution
    real(DP), intent(inout), dimension(:) :: rhs !< right-hand side
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: tled
    real(DP) :: sc1
    real(DP) :: sc2
    real(DP) :: rho1
    real(DP) :: rho2
    real(DP) :: sc1old
    real(DP) :: sc2old
    real(DP) :: rho1old
    real(DP) :: rho2old
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: snold
    real(DP) :: snnew
    real(DP) :: aterm
    real(DP) :: rhsterm
    ! -- formats
    character(len=*), parameter :: fmtsperror = &
      &"('DETECTED TIME STEP LENGTH OF ZERO.  GWF STORAGE PACKAGE CANNOT BE ', &
      &'USED UNLESS DELT IS NON-ZERO.')"
    !
    ! -- test if steady-state stress period
    if (this%iss /= 0) return
    !
    ! -- Ensure time step length is not zero
    if (delt == DZERO) then
      write (errmsg, fmtsperror)
      call store_error(errmsg, terminate=.TRUE.)
    end if
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- loop through and calculate storage contribution to hcof and rhs
    do n = 1, this%dis%nodes
      idiag = this%dis%con%ia(n)
      if (this%ibound(n) < 1) cycle
      !
      ! -- aquifer elevations and thickness
      tp = this%dis%top(n)
      bt = this%dis%bot(n)
      !
      ! -- aquifer saturation
      if (this%iconvert(n) == 0) then
        snold = DONE
        snnew = DONE
      else
        snold = sQuadraticSaturation(tp, bt, hold(n), this%satomega)
        snnew = sQuadraticSaturation(tp, bt, hnew(n), this%satomega)
      end if
      !
      ! -- storage coefficients
      sc1 = SsCapacity(this%istor_coef, tp, bt, this%dis%area(n), this%ss(n))
      rho1 = sc1 * tled
      !
      if (this%integratechanges /= 0) then
        ! -- Integration of storage changes (e.g. when using TVS):
        !    separate the old (start of time step) and new (end of time step)
        !    primary storage capacities
        sc1old = SsCapacity(this%istor_coef, tp, bt, this%dis%area(n), &
                            this%oldss(n))
        rho1old = sc1old * tled
      else
        ! -- No integration of storage changes: old and new values are
        !    identical => normal MF6 storage formulation
        rho1old = rho1
      end if
      !
      ! -- calculate specific storage terms
      call SsTerms(this%iconvert(n), this%iorig_ss, this%iconf_ss, tp, bt, &
                   rho1, rho1old, snnew, snold, hnew(n), hold(n), &
                   aterm, rhsterm)
      !
      ! -- add specific storage terms to amat and rhs
      call matrix_sln%add_value_pos(idxglo(idiag), aterm)
      rhs(n) = rhs(n) + rhsterm
      !
      ! -- specific yield
      if (this%iconvert(n) /= 0) then
        rhsterm = DZERO
        !
        ! -- secondary storage coefficient
        sc2 = SyCapacity(this%dis%area(n), this%sy(n))
        rho2 = sc2 * tled
        !
        if (this%integratechanges /= 0) then
          ! -- Integration of storage changes (e.g. when using TVS):
          !    separate the old (start of time step) and new (end of time step)
          !    secondary storage capacities
          sc2old = SyCapacity(this%dis%area(n), this%oldsy(n))
          rho2old = sc2old * tled
        else
          ! -- No integration of storage changes: old and new values are
          !    identical => normal MF6 storage formulation
          rho2old = rho2
        end if
        !
        ! -- calculate specific storage terms
        call SyTerms(tp, bt, rho2, rho2old, snnew, snold, &
                     aterm, rhsterm)
!
        ! -- add specific yield terms to amat and rhs
        call matrix_sln%add_value_pos(idxglo(idiag), aterm)
        rhs(n) = rhs(n) + rhsterm
      end if
    end do
    !
    ! -- return
    return
  end subroutine sto_fc

  !> @ brief Fill Newton-Raphson terms in A and right-hand side for the package
  !!
  !!  Fill the coefficient matrix and right-hand side with STO package
  !!  with Newton-Raphson terms.
  !!
  !<
  subroutine sto_fn(this, kiter, hold, hnew, matrix_sln, idxglo, rhs)
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy variables
    class(GwfStoType) :: this !< GwfStoType object
    integer(I4B), intent(in) :: kiter !< outer iteration number
    real(DP), intent(in), dimension(:) :: hold !< previous heads
    real(DP), intent(in), dimension(:) :: hnew !< current heads
    class(MatrixBaseType), pointer :: matrix_sln !< A matrix
    integer(I4B), intent(in), dimension(:) :: idxglo !< global index model to solution
    real(DP), intent(inout), dimension(:) :: rhs !< right-hand side
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: tled
    real(DP) :: sc1
    real(DP) :: sc2
    real(DP) :: rho1
    real(DP) :: rho2
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: tthk
    real(DP) :: h
    real(DP) :: snnew
    real(DP) :: derv
    real(DP) :: rterm
    real(DP) :: drterm
    !
    ! -- test if steady-state stress period
    if (this%iss /= 0) return
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- loop through and calculate storage contribution to hcof and rhs
    do n = 1, this%dis%nodes
      idiag = this%dis%con%ia(n)
      if (this%ibound(n) <= 0) cycle
      !
      ! -- aquifer elevations and thickness
      tp = this%dis%top(n)
      bt = this%dis%bot(n)
      tthk = tp - bt
      h = hnew(n)
      !
      ! -- aquifer saturation
      snnew = sQuadraticSaturation(tp, bt, h)
      !
      ! -- storage coefficients
      sc1 = SsCapacity(this%istor_coef, tp, bt, this%dis%area(n), this%ss(n))
      sc2 = SyCapacity(this%dis%area(n), this%sy(n))
      rho1 = sc1 * tled
      rho2 = sc2 * tled
      !
      ! -- calculate newton terms for specific storage
      !    and specific yield
      if (this%iconvert(n) /= 0) then
        !
        ! -- calculate saturation derivative
        derv = sQuadraticSaturationDerivative(tp, bt, h)
        !
        ! -- newton terms for specific storage
        if (this%iconf_ss == 0) then
          if (this%iorig_ss == 0) then
            drterm = -rho1 * derv * (h - bt) + rho1 * tthk * snnew * derv
          else
            drterm = -(rho1 * derv * h)
          end if
          call matrix_sln%add_value_pos(idxglo(idiag), drterm)
          rhs(n) = rhs(n) + drterm * h
        end if
        !
        ! -- newton terms for specific yield
        !    only calculated if the current saturation
        !    is less than one
        if (snnew < DONE) then
          ! -- calculate newton terms for specific yield
          if (snnew > DZERO) then
            rterm = -rho2 * tthk * snnew
            drterm = -rho2 * tthk * derv
            call matrix_sln%add_value_pos(idxglo(idiag), drterm + rho2)
            rhs(n) = rhs(n) - rterm + drterm * h + rho2 * bt
          end if
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine sto_fn

  !> @ brief Calculate flows for package
  !!
  !!  Flow calculation for the STO package components. Components include
  !!  specific storage and specific yield storage.
  !!
  !<
  subroutine sto_cq(this, flowja, hnew, hold)
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy variables
    class(GwfStoType) :: this !< GwfStoType object
    real(DP), dimension(:), contiguous, intent(inout) :: flowja !< connection flows
    real(DP), dimension(:), contiguous, intent(in) :: hnew !< current head
    real(DP), dimension(:), contiguous, intent(in) :: hold !< previous head
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: rate
    real(DP) :: tled
    real(DP) :: sc1
    real(DP) :: sc2
    real(DP) :: rho1
    real(DP) :: rho2
    real(DP) :: sc1old
    real(DP) :: sc2old
    real(DP) :: rho1old
    real(DP) :: rho2old
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: snold
    real(DP) :: snnew
    real(DP) :: aterm
    real(DP) :: rhsterm
    !
    ! -- initialize strg arrays
    do n = 1, this%dis%nodes
      this%strgss(n) = DZERO
      this%strgsy(n) = DZERO
    end do
    !
    ! -- Set strt to zero or calculate terms if not steady-state stress period
    if (this%iss == 0) then
      !
      ! -- set variables
      tled = DONE / delt
      !
      ! -- Calculate storage change
      do n = 1, this%dis%nodes
        if (this%ibound(n) <= 0) cycle
        ! -- aquifer elevations and thickness
        tp = this%dis%top(n)
        bt = this%dis%bot(n)
        !
        ! -- aquifer saturation
        if (this%iconvert(n) == 0) then
          snold = DONE
          snnew = DONE
        else
          snold = sQuadraticSaturation(tp, bt, hold(n), this%satomega)
          snnew = sQuadraticSaturation(tp, bt, hnew(n), this%satomega)
        end if
        !
        ! -- primary storage coefficient
        sc1 = SsCapacity(this%istor_coef, tp, bt, this%dis%area(n), this%ss(n))
        rho1 = sc1 * tled
        !
        if (this%integratechanges /= 0) then
          ! -- Integration of storage changes (e.g. when using TVS):
          !    separate the old (start of time step) and new (end of time step)
          !    primary storage capacities
          sc1old = SsCapacity(this%istor_coef, tp, bt, this%dis%area(n), &
                              this%oldss(n))
          rho1old = sc1old * tled
        else
          ! -- No integration of storage changes: old and new values are
          !    identical => normal MF6 storage formulation
          rho1old = rho1
        end if
        !
        ! -- calculate specific storage terms and rate
        call SsTerms(this%iconvert(n), this%iorig_ss, this%iconf_ss, tp, bt, &
                     rho1, rho1old, snnew, snold, hnew(n), hold(n), &
                     aterm, rhsterm, rate)
        !
        ! -- save rate
        this%strgss(n) = rate
        !
        ! -- add storage term to flowja
        idiag = this%dis%con%ia(n)
        flowja(idiag) = flowja(idiag) + rate
        !
        ! -- specific yield
        rate = DZERO
        if (this%iconvert(n) /= 0) then
          !
          ! -- secondary storage coefficient
          sc2 = SyCapacity(this%dis%area(n), this%sy(n))
          rho2 = sc2 * tled
          !
          if (this%integratechanges /= 0) then
            ! -- Integration of storage changes (e.g. when using TVS):
            !    separate the old (start of time step) and new (end of time
            !    step) secondary storage capacities
            sc2old = SyCapacity(this%dis%area(n), this%oldsy(n))
            rho2old = sc2old * tled
          else
            ! -- No integration of storage changes: old and new values are
            !    identical => normal MF6 storage formulation
            rho2old = rho2
          end if
          !
          ! -- calculate specific yield storage terms and rate
          call SyTerms(tp, bt, rho2, rho2old, snnew, snold, &
                       aterm, rhsterm, rate)

        end if
        this%strgsy(n) = rate
        !
        ! -- add storage term to flowja
        idiag = this%dis%con%ia(n)
        flowja(idiag) = flowja(idiag) + rate
      end do
    end if
    !
    ! -- return
    return
  end subroutine sto_cq

  !> @ brief Model budget calculation for package
  !!
  !!  Budget calculation for the STO package components. Components include
  !!  specific storage and specific yield storage.
  !!
  !<
  subroutine sto_bd(this, isuppress_output, model_budget)
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    ! -- dummy variables
    class(GwfStoType) :: this !< GwfStoType object
    integer(I4B), intent(in) :: isuppress_output !< flag to suppress model output
    type(BudgetType), intent(inout) :: model_budget !< model budget object
    ! -- local variables
    real(DP) :: rin
    real(DP) :: rout
    !
    ! -- Add confined storage rates to model budget
    call rate_accumulator(this%strgss, rin, rout)
    call model_budget%addentry(rin, rout, delt, budtxt(1), &
                               isuppress_output, '         STORAGE')
    !
    ! -- Add unconfined storage rates to model budget
    if (this%iusesy == 1) then
      call rate_accumulator(this%strgsy, rin, rout)
      call model_budget%addentry(rin, rout, delt, budtxt(2), &
                                 isuppress_output, '         STORAGE')
    end if
    !
    ! -- return
    return
  end subroutine sto_bd

  !> @ brief Save model flows for package
  !!
  !!  Save cell-by-cell budget terms for the STO package.
  !!
  !<
  subroutine sto_save_model_flows(this, icbcfl, icbcun)
    ! -- dummy variables
    class(GwfStoType) :: this !< GwfStoType object
    integer(I4B), intent(in) :: icbcfl !< flag to output budget data
    integer(I4B), intent(in) :: icbcun !< cell-by-cell file unit number
    ! -- local variables
    integer(I4B) :: ibinun
    integer(I4B) :: iprint, nvaluesp, nwidthp
    character(len=1) :: cdatafmp = ' ', editdesc = ' '
    real(DP) :: dinact
    !
    ! -- Set unit number for binary output
    if (this%ipakcb < 0) then
      ibinun = icbcun
    elseif (this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    end if
    if (icbcfl == 0) ibinun = 0
    !
    ! -- Record the storage rates if requested
    if (ibinun /= 0) then
      iprint = 0
      dinact = DZERO
      !
      ! -- storage(ss)
      call this%dis%record_array(this%strgss, this%iout, iprint, -ibinun, &
                                 budtxt(1), cdatafmp, nvaluesp, &
                                 nwidthp, editdesc, dinact)
      !
      ! -- storage(sy)
      if (this%iusesy == 1) then
        call this%dis%record_array(this%strgsy, this%iout, iprint, -ibinun, &
                                   budtxt(2), cdatafmp, nvaluesp, &
                                   nwidthp, editdesc, dinact)
      end if
    end if
    !
    ! -- return
    return
  end subroutine sto_save_model_flows

  !> @ brief Deallocate package memory
  !!
  !!  Deallocate STO package scalars and arrays.
  !!
  !<
  subroutine sto_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy variables
    class(GwfStoType) :: this !< GwfStoType object
    !
    ! -- TVS
    if (this%intvs /= 0) then
      call this%tvs%da()
      deallocate (this%tvs)
    end if
    !
    ! -- Deallocate arrays if package is active
    if (this%inunit > 0) then
      call mem_deallocate(this%iconvert)
      call mem_deallocate(this%ss)
      call mem_deallocate(this%sy)
      call mem_deallocate(this%strgss)
      call mem_deallocate(this%strgsy)
      !
      ! -- deallocate TVS arrays
      if (associated(this%oldss)) then
        call mem_deallocate(this%oldss)
      end if
      if (associated(this%oldsy)) then
        call mem_deallocate(this%oldsy)
      end if
    end if
    !
    ! -- Deallocate scalars
    call mem_deallocate(this%istor_coef)
    call mem_deallocate(this%iconf_ss)
    call mem_deallocate(this%iorig_ss)
    call mem_deallocate(this%iusesy)
    call mem_deallocate(this%satomega)
    call mem_deallocate(this%integratechanges)
    call mem_deallocate(this%intvs)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- return
    return
  end subroutine sto_da

  !> @ brief Allocate scalars
  !!
  !! Allocate and initialize scalars for the STO package. The base numerical
  !! package allocate scalars method is also called.
  !!
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy variables
    class(GwfStoType) :: this !< GwfStoType object
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- allocate scalars
    call mem_allocate(this%istor_coef, 'ISTOR_COEF', this%memoryPath)
    call mem_allocate(this%iconf_ss, 'ICONF_SS', this%memoryPath)
    call mem_allocate(this%iorig_ss, 'IORIG_SS', this%memoryPath)
    call mem_allocate(this%iusesy, 'IUSESY', this%memoryPath)
    call mem_allocate(this%satomega, 'SATOMEGA', this%memoryPath)
    call mem_allocate(this%integratechanges, 'INTEGRATECHANGES', &
                      this%memoryPath)
    call mem_allocate(this%intvs, 'INTVS', this%memoryPath)
    !
    ! -- initialize scalars
    this%istor_coef = 0
    this%iconf_ss = 0
    this%iorig_ss = 0
    this%iusesy = 0
    this%satomega = DZERO
    this%integratechanges = 0
    this%intvs = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars

  !> @ brief Allocate package arrays
  !!
  !!  Allocate and initialize STO package arrays.
  !!
  !<
  subroutine allocate_arrays(this, nodes)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy variables
    class(GwfStoType), target :: this !< GwfStoType object
    integer(I4B), intent(in) :: nodes !< active model nodes
    ! -- local variables
    integer(I4B) :: n
    !
    ! -- Allocate arrays
    call mem_allocate(this%iconvert, nodes, 'ICONVERT', this%memoryPath)
    call mem_allocate(this%ss, nodes, 'SS', this%memoryPath)
    call mem_allocate(this%sy, nodes, 'SY', this%memoryPath)
    call mem_allocate(this%strgss, nodes, 'STRGSS', this%memoryPath)
    call mem_allocate(this%strgsy, nodes, 'STRGSY', this%memoryPath)
    !
    ! -- Initialize arrays
    this%iss = 0
    do n = 1, nodes
      this%iconvert(n) = 1
      this%ss(n) = DZERO
      this%sy(n) = DZERO
      this%strgss(n) = DZERO
      this%strgsy(n) = DZERO
      if (this%integratechanges /= 0) then
        this%oldss(n) = DZERO
        if (this%iusesy /= 0) then
          this%oldsy(n) = DZERO
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine allocate_arrays

  !> @ brief Read options for package
  !!
  !!  Read options block for STO package.
  !!
  !<
  subroutine read_options(this)
    ! -- modules
    ! -- dummy variables
    class(GwfStoType) :: this !< GwfStoType object
    ! -- local variables
    character(len=LINELENGTH) :: keyword, fname
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtisvflow = &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE &
      &WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtflow = &
      &"(4x, 'FLOWS WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
    character(len=*), parameter :: fmtorigss = &
      "(4X,'ORIGINAL_SPECIFIC_STORAGE OPTION:',/, &
      &1X,'The original specific storage formulation will be used')"
    character(len=*), parameter :: fmtstoc = &
      "(4X,'STORAGECOEFFICIENT OPTION:',/, &
      &1X,'Read storage coefficient rather than specific storage')"
    character(len=*), parameter :: fmtconfss = &
      "(4X,'SS_CONFINED_ONLY OPTION:',/, &
      &1X,'Specific storage changes only occur under confined conditions')"
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING STORAGE OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('SAVE_FLOWS')
          this%ipakcb = -1
          write (this%iout, fmtisvflow)
        case ('STORAGECOEFFICIENT')
          this%istor_coef = 1
          write (this%iout, fmtstoc)
        case ('SS_CONFINED_ONLY')
          this%iconf_ss = 1
          this%iorig_ss = 0
          write (this%iout, fmtconfss)
        case ('TVS6')
          if (this%intvs /= 0) then
            errmsg = 'Multiple TVS6 keywords detected in OPTIONS block.'// &
                     ' Only one TVS6 entry allowed.'
            call store_error(errmsg, terminate=.TRUE.)
          end if
          call this%parser%GetStringCaps(keyword)
          if (trim(adjustl(keyword)) /= 'FILEIN') then
            errmsg = 'TVS6 keyword must be followed by "FILEIN" '// &
                     'then by filename.'
            call store_error(errmsg, terminate=.TRUE.)
          end if
          call this%parser%GetString(fname)
          this%intvs = GetUnit()
          call openfile(this%intvs, this%iout, fname, 'TVS')
          call tvs_cr(this%tvs, this%name_model, this%intvs, this%iout)
          !
          ! -- right now these are options that are only available in the
          !    development version and are not included in the documentation.
          !    These options are only available when IDEVELOPMODE in
          !    constants module is set to 1
        case ('DEV_ORIGINAL_SPECIFIC_STORAGE')
          this%iorig_ss = 1
          write (this%iout, fmtorigss)
        case ('DEV_OLDSTORAGEFORMULATION')
          call this%parser%DevOpt()
          this%iconf_ss = 1
          this%iorig_ss = 0
          write (this%iout, fmtconfss)
        case default
          write (errmsg, '(a,a)') 'Unknown STO option: ', &
            trim(keyword)
          call store_error(errmsg, terminate=.TRUE.)
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF STORAGE OPTIONS'
    end if
    !
    ! -- set omega value used for saturation calculations
    if (this%inewton > 0) then
      this%satomega = DEM6
    end if
    !
    ! -- return
    return
  end subroutine read_options

  !> @ brief Read data for package
  !!
  !!  Read griddata block for STO package.
  !!
  !<
  subroutine read_data(this)
    ! -- modules
    ! -- dummy variables
    class(GwfStotype) :: this !< GwfStoType object
    ! -- local variables
    character(len=LINELENGTH) :: keyword
    character(len=:), allocatable :: line
    character(len=LINELENGTH) :: cellstr
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    logical :: readiconv
    logical :: readss
    logical :: readsy
    logical :: isconv
    character(len=24), dimension(4) :: aname
    integer(I4B) :: n
    ! -- formats
    !data
    data aname(1)/'                ICONVERT'/
    data aname(2)/'        SPECIFIC STORAGE'/
    data aname(3)/'          SPECIFIC YIELD'/
    data aname(4)/'     STORAGE COEFFICIENT'/
    !
    ! -- initialize
    isfound = .false.
    readiconv = .false.
    readss = .false.
    readsy = .false.
    isconv = .false.
    !
    ! -- get stodata block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING GRIDDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        call this%parser%GetRemainingLine(line)
        lloc = 1
        select case (keyword)
        case ('ICONVERT')
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%iconvert, &
                                        aname(1))
          readiconv = .true.
        case ('SS')
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%ss, &
                                        aname(2))
          readss = .true.
        case ('SY')
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%sy, &
                                        aname(3))
          readsy = .true.
        case default
          write (errmsg, '(a,a)') 'Unknown GRIDDATA tag: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END PROCESSING GRIDDATA'
    else
      write (errmsg, '(a)') 'Required GRIDDATA block not found.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Check for ICONVERT
    if (.not. readiconv) then
      write (errmsg, '(a, a, a)') 'Error in GRIDDATA block: ', &
        trim(adjustl(aname(1))), ' not found.'
      call store_error(errmsg)
    else
      isconv = .false.
      do n = 1, this%dis%nodes
        if (this%iconvert(n) /= 0) then
          isconv = .true.
          this%iusesy = 1
          exit
        end if
      end do
    end if
    !
    ! -- Check for SS
    if (.not. readss) then
      write (errmsg, '(a, a, a)') 'Error in GRIDDATA block: ', &
        trim(adjustl(aname(2))), ' not found.'
      call store_error(errmsg)
    end if
    !
    ! -- Check for SY
    if (.not. readsy .and. isconv) then
      write (errmsg, '(a, a, a)') 'Error in GRIDDATA block: ', &
        trim(adjustl(aname(3))), ' not found.'
      call store_error(errmsg)
    end if
    !
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Check SS and SY for negative values
    do n = 1, this%dis%nodes
      if (this%ss(n) < DZERO) then
        call this%dis%noder_to_string(n, cellstr)
        write (errmsg, '(a,2(1x,a),1x,g0,1x,a)') &
          'Error in SS DATA: SS value in cell', trim(adjustl(cellstr)), &
          'is less than zero (', this%ss(n), ').'
        call store_error(errmsg)
      end if
      if (readsy) then
        if (this%sy(n) < DZERO) then
          call this%dis%noder_to_string(n, cellstr)
          write (errmsg, '(a,2(1x,a),1x,g0,1x,a)') &
            'Error in SY DATA: SY value in cell', trim(adjustl(cellstr)), &
            'is less than zero (', this%sy(n), ').'
          call store_error(errmsg)
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine read_data

  !> @ brief Save old storage property values
  !!
  !!  Save ss and sy values from the previous time step for use with storage
  !!  integration when integratechanges is non-zero.
  !!
  !<
  subroutine save_old_ss_sy(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy variables
    class(GwfStoType) :: this !< GwfStoType object
    ! -- local variables
    integer(I4B) :: n
    !
    ! -- Allocate TVS arrays if needed
    if (.not. associated(this%oldss)) then
      call mem_allocate(this%oldss, this%dis%nodes, 'OLDSS', this%memoryPath)
    end if
    if (this%iusesy == 1 .and. .not. associated(this%oldsy)) then
      call mem_allocate(this%oldsy, this%dis%nodes, 'OLDSY', this%memoryPath)
    end if
    !
    ! -- Save current specific storage
    do n = 1, this%dis%nodes
      this%oldss(n) = this%ss(n)
    end do
    !
    ! -- Save current specific yield, if used
    if (this%iusesy == 1) then
      do n = 1, this%dis%nodes
        this%oldsy(n) = this%sy(n)
      end do
    end if
    !
    ! -- Return
    return
  end subroutine save_old_ss_sy

end module GwfStoModule

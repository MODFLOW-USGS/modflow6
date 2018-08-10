module GwfCsubModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DPREC, DZERO, DEM6, DHALF, DEM1,                   &
                             DONE, DTWO, DTHREE,                                &
                             DGRAVITY, DTEN, DHUNDRED, DNODATA, DHNOFLO,        &
                             LENFTYPE, LENPACKAGENAME,                          &
                             LINELENGTH, LENBOUNDNAME, NAMEDBOUNDFLAG,          &
                             LENBUDTXT, LENAUXNAME, LENORIGIN
  use SmoothingModule,        only: sQuadraticSaturation,                       &
                                    sQuadraticSaturationDerivative
  use NumericalPackageModule, only: NumericalPackageType
  use ObserveModule,        only: ObserveType
  use ObsModule,            only: ObsType, obs_cr
  use BlockParserModule,      only: BlockParserType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use InputOutputModule, only: get_node, extract_idnum_or_bndname
  use BaseDisModule, only: DisBaseType
  use SimModule, only: count_errors, store_error, store_error_unit, ustop
  use ArrayHandlersModule, only: ExpandArray
  use SortModule, only: qsort, selectn
  !
  implicit none
  !
  private
  public :: csub_cr
  public :: GwfCsubType
  !
  character(len=LENBUDTXT), dimension(4) :: budtxt =                            & !text labels for budget terms
      [' CSUB-AQELASTIC',                                                       & 
       '   CSUB-ELASTIC', ' CSUB-INELASTIC',                                    &
       ' CSUB-WATERCOMP']
  character(len=LENBUDTXT), dimension(5) :: comptxt =                           & !text labels for compaction terms
      ['CSUB-COMPACTION', ' CSUB-INELASTIC', '   CSUB-ELASTIC',                 &
       '  CSUB-SKELETAL', ' CSUB-ZDISPLACE']
  
  !
  ! -- local paramter - derivative of the log of effective stress
  real(DP), parameter :: dlog10es = 0.4342942_DP
  !
  ! CSUB type
  type, extends(NumericalPackageType) :: GwfCsubType
    character(len=LENBOUNDNAME), pointer, dimension(:)   :: boundname => null() !vector of boundnames
    character(len=LENAUXNAME), allocatable, dimension(:) :: auxname             !name for each auxiliary variable
    character(len=500) :: listlabel   = ''                                      !title of table written for RP
    character(len=LENORIGIN) :: stoname
    integer(I4B), pointer :: istounit               => null()
    integer(I4B), pointer :: istrainib              => null()
    integer(I4B), pointer :: istrainsk              => null()
    integer(I4B), pointer :: ioutcomp               => null()
    integer(I4B), pointer :: ioutcompi              => null()
    integer(I4B), pointer :: ioutcompe              => null()
    integer(I4B), pointer :: ioutcomps              => null()
    integer(I4B), pointer :: ioutzdisp              => null()
    integer(I4B), pointer :: iupdatematprop         => null()
    integer(I4B), pointer :: istoragec              => null()
    integer(I4B), pointer :: icellf                 => null()
    integer(I4B), pointer :: idbsatscaling          => null()
    integer(I4B), pointer :: ibedstressoff          => null()
    integer(I4B), pointer :: ispecified_pcs         => null()
    integer(I4B), pointer :: ispecified_dbh         => null()
    integer(I4B), pointer :: igeostressoff          => null()
    integer(I4B), pointer :: inamedbound            => null()   !flag to read boundnames
    integer(I4B), pointer :: naux                   => null()   !number of auxiliary variables
    integer(I4B), pointer :: ninterbeds             => null()
    integer(I4B), pointer :: ndelaycells            => null()
    integer(I4B), pointer :: ndelaybeds             => null()
    integer(I4B), pointer :: initialized            => null()
    integer(I4B), pointer :: igeocalc               => null()
    integer(I4B), pointer :: idbhalfcell            => null()
    integer(I4B), pointer :: idbfullcell            => null()
    real(DP), pointer :: time_alpha                 => null()   !time factor to apply to the current and previous effective stress
    real(DP), pointer :: gammaw                     => null()   !product of fluid density, and gravity
    real(DP), pointer :: beta                       => null()   !water compressibility
    real(DP), pointer :: brg                        => null()   !product of gammaw and water compressibility
    real(DP), pointer :: dbfact                     => null()
    real(DP), pointer :: dbfacti                    => null()
    real(DP), pointer :: satomega                   => null()      !newton-raphson saturation omega

    integer(I4B), pointer :: gwfiss                 => NULL()   !pointer to model iss flag
    integer(I4B), pointer :: gwfiss0                => NULL()   !iss flag for last stress period
    integer(I4B), dimension(:), pointer :: ibound   => null()   !pointer to model ibound
    integer(I4B), dimension(:), pointer :: stoiconv => null()   !pointer to iconvert in storage
    real(DP), dimension(:), pointer :: stosc1       => null()   !pointer to sc1 in storage
    real(DP), dimension(:), pointer :: buff         => null()   !buff array
    real(DP), dimension(:), pointer :: buffusr      => null()   !buffusr array
    integer, dimension(:), pointer :: nodelist      => null()   !reduced node that the interbed is attached to
    integer, dimension(:), pointer :: unodelist     => null()   !user node that the interbed is attached to

    real(DP), dimension(:), pointer :: sgm          => null()   !specific gravity moist sediments
    real(DP), dimension(:), pointer :: sgs          => null()   !specific gravity saturated sediments
    real(DP), dimension(:), pointer :: sig0         => null()   !geostatic offset
    real(DP), dimension(:), pointer :: ske_cr       => null()   !skeletal specified storage
    real(DP), dimension(:), pointer :: sk_theta     => null()   !current skeletal (aquifer) porosity
    real(DP), dimension(:), pointer :: sk_thick     => null()   !current skeletal (aquifer) thickness
    real(DP), dimension(:), pointer :: sk_theta0    => null()   !previous skeletal (aquifer) porosity
    real(DP), dimension(:), pointer :: sk_thick0    => null()   !previous skeletal (aquifer) thickness
    real(DP), dimension(:), pointer :: sk_gs        => null()   !geostatic stress for a cell
    real(DP), dimension(:), pointer :: sk_es        => null()   !skeletal (aquifer) effective stress
    real(DP), dimension(:), pointer :: sk_es0       => null()   !skeletal (aquifer) effective stress for the previous time step
    real(DP), dimension(:), pointer :: sk_comp      => null()   !skeletal (aquifer) incremental compaction
    real(DP), dimension(:), pointer :: sk_tcomp     => null()   !skeletal (aquifer) total compaction
    real(DP), dimension(:), pointer :: sk_stor      => null()   !skeletal (aquifer) storage
    real(DP), dimension(:), pointer :: sk_wcstor    => null()   !skeletal (aquifer) water compressibility storage
    real(DP), dimension(:), pointer :: sk_ske       => null()   !skeletal (aquifer) elastic storage coefficient
    real(DP), dimension(:), pointer :: sk_sk        => null()   !skeletal (aquifer) first storage coefficient
    real(DP), dimension(:), pointer :: sk_thickini  => null()   !initial skeletal (aquifer) thickness
    !
    ! -- interbed variables
    integer(I4B), dimension(:), pointer :: idelay   => null()   !0 = nodelay, > 0 = delay
    real(DP), dimension(:), pointer :: ci           => null()   !compression index
    real(DP), dimension(:), pointer :: rci          => null()   !recompression index
    real(DP), dimension(:), pointer :: es           => null()   !effective stress
    real(DP), dimension(:), pointer :: es0          => null()   !last effective stress
    real(DP), dimension(:), pointer :: pcs          => null()   !preconsolidation stress
    real(DP), dimension(:), pointer :: thick        => null()   !current interbed thickness
    real(DP), dimension(:), pointer :: theta        => null()   !current interbed porosity
    real(DP), dimension(:), pointer :: thick0       => null()   !previous interbed thickness
    real(DP), dimension(:), pointer :: theta0       => null()   !previous interbed porosity
    real(DP), dimension(:), pointer :: rnb          => null()   !interbed system material factor
    real(DP), dimension(:), pointer :: kv           => null()   !vertical hydraulic conductivity of interbed
    real(DP), dimension(:), pointer :: h0           => null()   !initial head in interbed
    real(DP), dimension(:), pointer :: comp         => null()   !interbed incremental compaction
    real(DP), dimension(:), pointer :: tcomp        => null()   !total interbed compaction
    real(DP), dimension(:), pointer :: tcompi       => null()   !total inelastic interbed compaction
    real(DP), dimension(:), pointer :: tcompe       => null()   !total elastic interbed compaction
    real(DP), pointer, dimension(:,:) :: auxvar     => null()   !auxiliary variable array
    real(DP), dimension(:), pointer :: storagee     => null()   !elastic storage
    real(DP), dimension(:), pointer :: storagei     => null()   !inelastic storage
    real(DP), dimension(:), pointer :: ske          => null()   !elastic storage coefficient
    real(DP), dimension(:), pointer :: sk           => null()   !first storage coefficient
    real(DP), dimension(:), pointer :: thickini     => null()   !initial interbed thickness
    !
    ! -- delay interbed arrays
    real(DP), dimension(:), pointer   :: dbdz       => null()   !delay bed dz
    real(DP), dimension(:,:), pointer :: dbz        => null()   !delay bed cell z
    real(DP), dimension(:,:), pointer :: dbh        => null()   !delay bed cell h
    real(DP), dimension(:,:), pointer :: dbh0       => null()   !delay bed cell previous h
    real(DP), dimension(:,:), pointer :: dbtheta    => null()   !delay bed cell porosity
    real(DP), dimension(:,:), pointer :: dbtheta0   => null()   !delay bed cell previous porosity
    real(DP), dimension(:,:), pointer :: dbgeo      => null()   !delay bed cell geostatic stress
    real(DP), dimension(:,:), pointer :: dbgeo0     => null()   !delay bed cell previous geostatic stress
    real(DP), dimension(:,:), pointer :: dbes       => null()   !delay bed cell effective stress
    real(DP), dimension(:,:), pointer :: dbes0      => null()   !delay bed cell previous effective stress
    real(DP), dimension(:,:), pointer :: dbpcs      => null()   !delay bed cell preconsolidation stress
    !
    ! -- delay interbed solution arrays
    real(DP), dimension(:), pointer :: dbal         => null()   !delay bed lower diagonal
    real(DP), dimension(:), pointer :: dbad         => null()   !delay bed diagonal
    real(DP), dimension(:), pointer :: dbau         => null()   !delay bed upper diagonal
    real(DP), dimension(:), pointer :: dbrhs        => null()   !delay bed right hand side
    real(DP), dimension(:), pointer :: dbdh         => null()   !delay bed dh
    real(DP), dimension(:), pointer :: dbaw         => null()   !delay bed work vector
    !
    ! -- observation data
    integer(I4B), pointer :: inobspkg               => null()   !unit number for obs package
    type(ObsType), pointer :: obs                   => null()   !observation package

  contains
    procedure :: define_listlabel
    procedure :: read_options
    procedure :: csub_ar
    procedure :: csub_da
    procedure :: csub_rp
    procedure :: csub_ad
    procedure :: csub_fc
    procedure :: csub_fn
    procedure :: csub_cc
    procedure :: csub_fp
    procedure :: bdcalc => csub_bdcalc
    procedure :: bdsav => csub_bdsav
    procedure :: read_dimensions => csub_read_dimensions
    procedure, private :: csub_allocate_scalars
    procedure, private :: csub_allocate_arrays
    procedure, private :: csub_read_packagedata
    !
    ! -- helper methods
    procedure, private :: csub_calc_void
    procedure, private :: csub_calc_theta
    procedure, private :: csub_calc_znode
    procedure, private :: csub_calc_sfacts
    procedure, private :: csub_adj_matprop
    !
    ! -- stress methods
    !procedure, private :: csub_sk_calc_znode
    procedure, private :: csub_sk_calc_stress
    !
    ! -- coarse-grained skeletal methods
    procedure, private :: csub_sk_update
    procedure, private :: csub_sk_calc_comp
    procedure, private :: csub_sk_calc_sske
    procedure, private :: csub_sk_fc
    procedure, private :: csub_sk_fn
    procedure, private :: csub_sk_wcomp_fc
    procedure, private :: csub_sk_wcomp_fn
    !
    ! -- interbed methods
    procedure, private :: csub_interbed_set_initial
    procedure, private :: csub_interbed_fc
    procedure, private :: csub_interbed_wcomp_fc
    !
    ! -- no-delay interbed methods
    procedure, private :: csub_nodelay_update
    procedure, private :: csub_nodelay_fc
    procedure, private :: csub_nodelay_calc_comp
    !
    ! -- delay interbed methods
    procedure, private :: csub_delay_chk
    procedure, private :: csub_delay_calc_zcell
    procedure, private :: csub_delay_calc_stress
    procedure, private :: csub_delay_calc_ssksske
    procedure, private :: csub_delay_calc_comp
    procedure, private :: csub_delay_calc_dstor
    procedure, private :: csub_delay_fc
    procedure, private :: csub_delay_sln
    procedure, private :: csub_delay_assemble
    procedure, private :: csub_delay_calc_err
    !
    ! -- methods for observations
    procedure, public :: csub_obs_supported
    procedure, public :: csub_df_obs
    procedure, private :: csub_rp_obs
    procedure, private :: csub_bd_obs
    !
    ! -- method for time series
    !procedure, public :: bnd_rp_ts => csub_rp_ts
  end type GwfCsubType

contains

  subroutine csub_cr(csubobj, name_model, istounit, stoname, inunit, iout)
! ******************************************************************************
! csub_cr -- Create a New CSUB Object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    implicit none
    type(GwfCsubType), pointer :: csubobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: istounit
    character(len=*), intent(in) :: stoname
    integer(I4B), intent(in) :: iout
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(csubobj)

    ! -- create name and origin
    call csubobj%set_names(1, name_model, 'CSUB', 'CSUB')
    !
    ! -- Allocate scalars
    call csubobj%csub_allocate_scalars()
    !
    ! -- Set variables
    csubobj%istounit = istounit
    csubobj%stoname = stoname
    csubobj%inunit = inunit
    csubobj%iout = iout
    !
    ! -- Initialize block parser
    call csubobj%parser%Initialize(csubobj%inunit, csubobj%iout)
    !
    ! -- return
    return
  end subroutine csub_cr


   subroutine csub_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfCsubType),   intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- call standard NumericalPackageType allocate scalars
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- allocate the object and assign values to object variables
    call mem_allocate(this%istounit, 'ISTOUNIT', this%origin)
    call mem_allocate(this%inobspkg, 'INOBSPKG', this%origin)
    call mem_allocate(this%ninterbeds, 'NINTERBEDS', this%origin)
    call mem_allocate(this%ndelaycells, 'NDELAYCELLS', this%origin)
    call mem_allocate(this%ndelaybeds, 'NDELAYBEDS', this%origin)
    call mem_allocate(this%initialized, 'INITIALIZED', this%origin)
    call mem_allocate(this%igeocalc, 'IGEOCALC', this%origin)
    call mem_allocate(this%idbsatscaling, 'IDBSATSCALING', this%origin)
    call mem_allocate(this%ibedstressoff, 'IBEDSTRESSOFF', this%origin)
    call mem_allocate(this%ispecified_pcs, 'ISPECIFIED_PCS', this%origin)
    call mem_allocate(this%ispecified_dbh, 'ISPECIFIED_DBH', this%origin)
    call mem_allocate(this%igeostressoff, 'IGEOSTRESSOFF', this%origin)
    call mem_allocate(this%inamedbound, 'INAMEDBOUND', this%origin)
    call mem_allocate(this%naux, 'NAUX', this%origin)
    call mem_allocate(this%istoragec, 'ISTORAGEC', this%origin)
    call mem_allocate(this%istrainib, 'ISTRAINIB', this%origin)
    call mem_allocate(this%istrainsk, 'ISTRAINSK', this%origin)
    call mem_allocate(this%ioutcomp, 'IOUTCOMP', this%origin)
    call mem_allocate(this%ioutcompi, 'IOUTCOMPI', this%origin)
    call mem_allocate(this%ioutcompe, 'IOUTCOMPE', this%origin)
    call mem_allocate(this%ioutcomps, 'IOUTCOMPS', this%origin)
    call mem_allocate(this%ioutzdisp, 'IOUTZDISP', this%origin)
    call mem_allocate(this%iupdatematprop, 'IUPDATEMATPROP', this%origin)
    call mem_allocate(this%idbhalfcell, 'IDBHALFCELL', this%origin)
    call mem_allocate(this%idbfullcell, 'IDBFULLCELL', this%origin)
    call mem_allocate(this%time_alpha, 'TIME_ALPHA', this%origin)
    call mem_allocate(this%gammaw, 'GAMMAW', this%origin)
    call mem_allocate(this%beta, 'BETA', this%origin)
    call mem_allocate(this%brg, 'BRG', this%origin)
    call mem_allocate(this%dbfact, 'DBFACT', this%origin)
    call mem_allocate(this%dbfacti, 'DBFACTI', this%origin)
    call mem_allocate(this%satomega, 'SATOMEGA', this%origin)
    call mem_allocate(this%icellf, 'ICELLF', this%origin)
    call mem_allocate(this%ninterbeds, 'NIBCCELLS', this%origin)
    call mem_allocate(this%gwfiss0, 'GWFISS0', this%origin)
    !
    ! -- Allocate text strings
    allocate(this%auxname(0))
    !
    ! -- initialize values
    this%istounit = 0
    this%inobspkg = 0
    this%ninterbeds = 0
    this%ndelaycells = 19
    this%ndelaybeds = 0
    this%initialized = 0
    this%igeocalc = 1
    this%idbsatscaling = 0
    this%ibedstressoff = 0
    this%ispecified_pcs = 0
    this%ispecified_dbh = 0
    this%igeostressoff = 0
    this%inamedbound = 0
    this%naux = 0
    this%istoragec = 1
    this%istrainib = 0
    this%istrainsk = 0
    this%ioutcomp = 0
    this%ioutcompi = 0
    this%ioutcompe = 0
    this%ioutcomps = 0
    this%ioutzdisp = 0
    this%iupdatematprop = 0
    this%idbhalfcell = 0
    this%idbfullcell = 0
    this%time_alpha = DONE
    this%gammaw = DGRAVITY * 1000._DP
    this%beta = 4.6512e-10_DP
    this%brg = this%gammaw * this%beta
    this%dbfact = DONE
    this%dbfacti = DONE
    !
    ! -- set omega value used for saturation calculations
    if (this%inewton > 0) then
      this%satomega = DEM6
    else
      this%satomega = DZERO
    end if
    this%icellf = 0
    this%ninterbeds = 0
    this%gwfiss0 = 0
    !
    ! -- return
    return
   end subroutine csub_allocate_scalars

  subroutine csub_cc(this, iend, icnvg, nodes, hnew)
! **************************************************************************
! csub_cc -- Final convergence check for package
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    use TdisModule, only:delt
    ! -- dummy
    class(GwfCsubType) :: this
    integer(I4B), intent(in) :: iend
    integer(I4B), intent(inout) :: icnvg
    integer(I4B), intent(in) :: nodes
    real(DP), dimension(nodes), intent(in) :: hnew
    ! -- local
    integer(I4B) :: ifirst
    integer(I4B) :: ib
    integer(I4B) :: node
    integer(I4B) :: idelay
    real(DP) :: area
    real(DP) :: hcell
    real(DP) :: err
    real(DP) :: stoe
    real(DP) :: stoi
    real(DP) :: tled
    real(DP) :: hcof
    real(DP) :: rhs
    real(DP) :: v1
    real(DP) :: v2
    real(DP) :: df
    real(DP) :: avgf
    real(DP) :: pd
    ! format
02000 format(4x,'CSUB PACKAGE FAILED CONVERGENCE CRITERIA',//,                  &
             4x,a10,2(1x,a15),/,4x,42('-'))
02010 format(4x,i10,2(1x,G15.7))
02020 format(4x,42('-'))
02030 format('CONVERGENCE FAILED AS A RESULT OF CSUB PACKAGE',1x,a)
! --------------------------------------------------------------------------
    ifirst = 1
    tled = DONE / DELT
    !if (this%iconvchk /= 0) then
    if (this%gwfiss == 0) then
      final_check: do ib = 1, this%ninterbeds
        idelay = this%idelay(ib)
        if (idelay == 0) cycle
        node = this%nodelist(ib)
        area = this%dis%get_area(node)
        hcell = hnew(node)
        !
        ! --
        call this%csub_delay_calc_err(ib, hcell, err)
        !
        ! --
        call this%csub_delay_calc_dstor(ib, stoe, stoi)
        v1 = (stoe + stoi) * area * this%rnb(ib) * tled
        !
        !
        call this%csub_delay_fc(ib, hcof, rhs)
        v2 = (-hcof * hcell - rhs) * area * this%rnb(ib)

        df = v2 - v1
        avgf = DHALF * (v1 + v2)
        pd = DZERO
        if (avgf > DZERO) then
          pd = DHUNDRED * df / avgf
        end if
        
        if (ABS(pd) > DEM1) then
          icnvg = 0
          ! write convergence check information if this is the last outer iteration
          if (iend == 1) then
            if (ifirst == 1) then
              ifirst = 0
              write(*,2030) this%name
              write(this%iout, 2000) '   INTEBED',                                 &
                '      PCT DIFF.', 'PCT DIFF. CRIT.'
            end if
            write(*,2010) ib, pd, DEM1
            write(this%iout,2010) ib, pd, DEM1
          else
            exit final_check
          end if
        end if
      end do final_check
      if (ifirst == 0) then
        write(this%iout,2020)
      end if
    end if
    !
    ! -- return
    return
  end subroutine csub_cc


   subroutine csub_bdcalc(this, nodes, hnew, hold, isuppress_output,            &
                          model_budget)
! ******************************************************************************
! csub_bd -- calculate budget for skeletal storage, interbeds, and water
!            compression   
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
   ! -- modules
    use TdisModule, only: delt
    use ConstantsModule, only: LENBOUNDNAME, DZERO, DONE
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(GwfCsubType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: hnew
    real(DP), intent(in), dimension(nodes) :: hold
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    ! -- local
    integer(I4B) :: ib
    integer(I4B) :: node
    real(DP) :: es
    real(DP) :: pcs
    real(DP) :: rho1
    real(DP) :: rho2
    real(DP) :: tled
    real(DP) :: tledm
    real(DP) :: es0
    real(DP) :: rrate
    real(DP) :: ratein
    real(DP) :: rateout
    real(DP) :: comp
    real(DP) :: compi
    real(DP) :: compe
    real(DP) :: area
    real(DP) :: h
    real(DP) :: hcof
    real(DP) :: rhs
    real(DP) :: stoe
    real(DP) :: stoi
    real(DP) :: err
    real(DP) :: rateskin
    real(DP) :: rateskout
    real(DP) :: rateibein
    real(DP) :: rateibeout
    real(DP) :: rateibiin
    real(DP) :: rateibiout
    real(DP) :: rratewc
    real(DP) :: ratewcin
    real(DP) :: ratewcout
    ! -- for observations
    integer(I4B) :: iprobslocal
    ! -- formats
! --------------------------------------------------------------------------
    !
    ! -- Suppress saving of simulated values; they
    !    will be saved at end of this procedure.
    iprobslocal = 0
    ratein = DZERO
    rateout= DZERO
    ratewcin = DZERO
    ratewcout = DZERO
    !
    ! -- coarse-grained skeletal storage
    rateskin = DZERO
    rateskout= DZERO
    tled = DONE / DELT
    do node = 1, this%dis%nodes
      area = this%dis%get_area(node)
      comp = DZERO
      rrate = DZERO
      rratewc = DZERO
      if (this%gwfiss == 0) then
        if (this%ibound(node) > 0) then
          !
          ! -- calculate coarse-grained skeletal storage terms
          call this%csub_sk_fc(node, tled, area, hnew(node), hold(node),        &
                               hcof, rhs)
          rrate = hcof * hnew(node) - rhs
          !
          ! -- calculate compaction
          call this%csub_sk_calc_comp(node, hnew(node), hold(node), comp)
          !
          ! -- budget terms
          if (rrate < DZERO) then
            rateskout = rateskout - rrate
          else
            rateskin = rateskin + rrate
          end if
          !
          ! -- calculate coarse-grained skeletal water compressibility storage terms
          call this%csub_sk_wcomp_fc(node, tled, area, hnew(node), hold(node),  &
                                     hcof, rhs)
          rratewc = hcof * hnew(node) - rhs
          !
          ! -- water compressibility budget terms
          if (rratewc < DZERO) then
            ratewcout = ratewcout - rratewc
          else
            ratewcin = ratewcin + rratewc
          end if
        end if
      end if
      !
      ! -- update coarse-grained skeletal storage and water
      !    compresion variables
      this%sk_stor(node) = rrate
      this%sk_wcstor(node) = rratewc
      !
      ! -- update incremental compaction
      !comp = rrate * DELT / area
      this%sk_comp(node) = comp
      ! 
      !
      ! -- update states if required
      if (isuppress_output == 0) then
        !
        ! -- update total compaction
        !comp = rrate * DELT / area
        this%sk_tcomp(node) = this%sk_tcomp(node) + comp
        !
        ! - calculate strain and change in skeletal void ratio and thickness
        if (this%iupdatematprop /= 0) then
          call this%csub_sk_update(node)
        end if
      end if
    end do
    !
    ! -- interbed storage
    rateibein = DZERO
    rateibeout = DZERO
    rateibiin = DZERO
    rateibiout = DZERO

    tled = DONE
    tledm = DONE / DELT
    do ib = 1, this%ninterbeds
      rratewc = DZERO
      if (this%gwfiss == 0) then
        node = this%nodelist(ib)
        area = this%dis%get_area(node)
        !
        ! -- skip inactive and constant head cells
        if (this%ibound(node) < 1) cycle
        !
        ! -- no delay interbeds
        if (this%idelay(ib) == 0) then
          stoi = DZERO
          !
          ! -- calculate compaction
          call this%csub_nodelay_calc_comp(ib, hnew(node), hold(node), comp,    &
                                           rho1, rho2)
          !
          ! -- interbed stresses
          es = this%sk_es(node)
          pcs = this%pcs(ib)
          es0 = this%sk_es0(node)
          !
          ! -- calculate inelastic and elastic compaction
          if (this%igeocalc == 0) then
            h = hnew(node)
            if (rho2 /= rho1) then
              stoi = rho2 * (pcs - h)
              stoe = rho1 * (es0 - pcs)
            else
              stoe = comp
            end if
          else
            if (rho2 /= rho1) then
              stoi = -pcs * rho2 + (rho2 * es)
              stoe = pcs * rho1 - (rho1 * es0)
            else
              stoe = comp
            end if
          end if
          compe = stoe
          compi = stoi
          stoe = stoe * area
          stoi = stoi * area
          this%storagee(ib) = stoe * tledm
          this%storagei(ib) = stoi * tledm
          !
          ! -- update compaction
          this%comp(ib) = comp
          !
          ! -- update states if required
          if (isuppress_output == 0) then
            !
            ! -- update total compaction
            this%tcomp(ib) = this%tcomp(ib) + comp
            this%tcompe(ib) = this%tcompe(ib) + compe
            this%tcompi(ib) = this%tcompi(ib) + compi
            
            !
            ! - calculate strain and change in interbed void ratio and thickness
            if (this%iupdatematprop /= 0) then
              call this%csub_nodelay_update(ib)
            end if
          end if
          !
          ! -- delay interbeds
        else
          h = hnew(node)
          call this%csub_delay_calc_dstor(ib, stoe, stoi)
          this%storagee(ib) = stoe * area * this%rnb(ib) * tledm
          this%storagei(ib) = stoi * area * this%rnb(ib) * tledm
          !
          !
          call this%csub_delay_fc(ib, hcof, rhs)
          err = -hcof * h - rhs
          err = err / tledm
          !
          ! -- update states if required
          if (isuppress_output == 0) then
            !
            ! -- calculate sum of compaction in delay interbed
            call this%csub_delay_calc_comp(ib)
          end if
        end if
        !
        ! -- budget terms
        if (this%storagee(ib) < DZERO) then
          rateibeout = rateibeout - this%storagee(ib)
        else
          rateibein = rateibein + this%storagee(ib)
        end if
        if (this%storagei(ib) < DZERO) then
          rateibiout = rateibiout - this%storagei(ib)
        else
          rateibiin = rateibiin + this%storagei(ib)
        end if
        !
        ! -- interbed water compressibility
        call this%csub_interbed_wcomp_fc(ib, node, tledm, area,                 &
                                         hnew(node), hold(node), hcof, rhs)
        rratewc = hcof * hnew(node) - rhs
        this%sk_wcstor(node) = this%sk_wcstor(node) + rratewc
        !
        ! -- water compressibility budget terms
        if (rratewc < DZERO) then
          ratewcout = ratewcout - rratewc
        else
          ratewcin = ratewcin + rratewc
        end if
      else
        this%storagee(ib) = DZERO
        this%storagei(ib) = DZERO
      end if
    end do
    !
    ! -- Add contributions to model budget 
    !
    ! -- interbed elastic storage
    call model_budget%addentry(rateskin, rateskout, delt, budtxt(1),            &
                                isuppress_output, '            CSUB')
    if (this%ninterbeds > 0) then
      !
      ! -- interbed elastic storage
      call model_budget%addentry(rateibein, rateibeout, delt, budtxt(2),        &
                                  isuppress_output, '            CSUB')
      !
      ! -- interbed elastic storage
      call model_budget%addentry(rateibiin, rateibiout, delt, budtxt(3),        &
                                  isuppress_output, '            CSUB')
    end if
    call model_budget%addentry(ratewcin, ratewcout, delt, budtxt(4),            &
                                isuppress_output, '            CSUB')
    !
    ! -- For continuous observations, save simulated values.
    if (this%obs%npakobs > 0) then
      call this%csub_bd_obs()
    end if
    !
    ! -- terminate if errors encountered when updating material properties
    if (this%iupdatematprop /= 0) then
      if (this%time_alpha > DZERO) then
        if (count_errors() > 0) then
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
      end if
    end if
    !
    ! -- return
    return

   end subroutine csub_bdcalc

  subroutine csub_bdsav(this, idvfl, icbcfl, icbcun)
! ******************************************************************************
! sto_bdsav -- Save budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: kstp, kper, delt, pertim, totim
    use InputOutputModule, only: ulasav, ubdsv06
    ! -- dummy
    class(GwfCsubType) :: this
    integer(I4B), intent(in) :: idvfl
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    character(len=1) :: cdatafmp=' ', editdesc=' '
    integer(I4B) :: ibinun
    integer(I4B) :: iprint, nvaluesp, nwidthp
    integer(I4B) :: ib
    integer(I4B) :: node
    integer(I4B) :: nodem
    integer(I4B) :: nodeu
    integer(I4B) :: i
    integer(I4B) :: k
    integer(I4B) :: ncpl
    integer(I4B) :: nlay
    integer(I4B) :: naux
    real(DP) :: dinact
    real(DP) :: Q
    
! ------------------------------------------------------------------------------
    !
    ! -- Set unit number for binary output
    if(this%ipakcb < 0) then
      ibinun = icbcun
    elseif(this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    endif
    if(icbcfl == 0) ibinun = 0
    !
    ! -- Record the storage rates if requested
    if (ibinun /= 0) then
      iprint = 0
      dinact = DZERO
      !
      ! -- skeletal storage (sske)
      call this%dis%record_array(this%sk_stor, this%iout, iprint, -ibinun,      &
                                 budtxt(1), cdatafmp, nvaluesp,                 &
                                 nwidthp, editdesc, dinact)
      if (this%ninterbeds > 0) then
        naux = 0
        ! -- interbed elastic storage
        call ubdsv06(kstp, kper, budtxt(2), this%name_model, this%name,         &
                     this%name_model, this%name,                                &
                     ibinun, naux, this%auxname, this%ninterbeds, 1, 1,         &
                     this%ninterbeds, this%iout, delt, pertim, totim)
        do ib = 1, this%ninterbeds
          q = this%storagee(ib)
          node = this%nodelist(ib)
          call this%dis%record_mf6_list_entry(ibinun, node, node, q, naux,      &
                                              this%auxvar(:,ib))
        end do
        ! -- interbed inelastic storage
        call ubdsv06(kstp, kper, budtxt(3), this%name_model, this%name,         &
                     this%name_model, this%name,                                &
                     ibinun, naux, this%auxname, this%ninterbeds, 1, 1,         &
                     this%ninterbeds, this%iout, delt, pertim, totim)
        do ib = 1, this%ninterbeds
          q = this%storagei(ib)
          node = this%nodelist(ib)
          call this%dis%record_mf6_list_entry(ibinun, node, node, q, naux,      &
                                              this%auxvar(:,ib))
        end do
      end if
      !
      ! -- water compressibility
      call this%dis%record_array(this%sk_wcstor, this%iout, iprint, -ibinun,  &
                                 budtxt(4), cdatafmp, nvaluesp,               &
                                 nwidthp, editdesc, dinact)
    end if
    !
    ! -- Save compaction results
    !
    ! -- Set unit number for binary compaction and z-displacement output
    if(this%ioutcomp /= 0 .or. this%ioutzdisp /= 0) then
      ibinun = 1
    else
      ibinun = 0
    endif
    if(idvfl == 0) ibinun = 0
    !
    ! -- save compaction results
    if (ibinun /= 0) then
      iprint = 0
      dinact = DHNOFLO
      !
      ! -- fill buff with total compaction
      do node = 1, this%dis%nodes
        this%buff(node) = this%sk_tcomp(node)
      end do
      do ib = 1, this%ninterbeds
        node = this%nodelist(ib)
        this%buff(node) = this%buff(node) + this%tcomp(ib)
      end do
      !
      ! -- write compaction data to binary file
      if (this%ioutcomp /= 0) then
        ibinun = this%ioutcomp
        call this%dis%record_array(this%buff, this%iout, iprint, ibinun,        &
                                   comptxt(1), cdatafmp, nvaluesp,              &
                                   nwidthp, editdesc, dinact)
      end if
      !
      ! -- calculate z-displacement (subsidence) and write data to binary file
      if (this%ioutzdisp /= 0) then
        ibinun = this%ioutzdisp
        !
        ! -- initialize buffusr 
        do nodeu = 1, this%dis%nodesuser
          this%buffusr(nodeu) = DZERO
        end do
        !
        ! -- fill buffusr with buff
        do node = 1, this%dis%nodes
          nodeu = this%dis%get_nodeuser(node)
          this%buffusr(nodeu) = this%buff(node) 
        end do
        !
        ! -- calculate z-displacement
        ncpl = this%dis%get_ncpl()
        !
        ! -- disu
        if (this%dis%ndim == 1) then
          ! TO DO - 
        ! -- disv or dis
        else
          nlay = this%dis%nodesuser / ncpl
          do k = nlay - 1, 1, -1
            do i = 1, ncpl
              node = (k - 1) * ncpl + i
              nodem = k * ncpl + i
              this%buffusr(node) = this%buffusr(node) + this%buffusr(nodem)
            end do
          end do
        end if
        !
        ! -- fill buff with data from buffusr
        do nodeu = 1, this%dis%nodesuser
          node = this%dis%get_nodenumber_idx1(nodeu, 1)
          if (node /= 0) then
            this%buff(node) = this%buffusr(nodeu)
          end if
        end do
        !
        ! -- write z-displacement
        call this%dis%record_array(this%buff, this%iout, iprint, ibinun,        &
                                   comptxt(5), cdatafmp, nvaluesp,              &
                                   nwidthp, editdesc, dinact)
      
      end if
    end if
    !
    ! -- Set unit number for binary inelastic interbed compaction
    if(this%ioutcompi /= 0) then
      ibinun = this%ioutcompi
    else
      ibinun = 0
    endif
    if(idvfl == 0) ibinun = 0
    !
    ! -- save inelastic interbed compaction results
    if(ibinun /= 0) then
      iprint = 0
      dinact = DHNOFLO
      !
      ! -- fill buff with inelastic interbed compaction
      do node = 1, this%dis%nodes
        this%buff(node) = DZERO
      end do
      do ib = 1, this%ninterbeds
        node = this%nodelist(ib)
        this%buff(node) = this%buff(node) + this%tcompi(ib)
      end do
      !
      ! -- write inelastic interbed compaction data to binary file
      call this%dis%record_array(this%buff, this%iout, iprint, ibinun,          &
                                 comptxt(2), cdatafmp, nvaluesp,                &
                                 nwidthp, editdesc, dinact)
    end if
    !
    ! -- Set unit number for binary elastic interbed compaction
    if(this%ioutcompe /= 0) then
      ibinun = this%ioutcompe
    else
      ibinun = 0
    endif
    if(idvfl == 0) ibinun = 0
    !
    ! -- save elastic interbed compaction results
    if(ibinun /= 0) then
      iprint = 0
      dinact = DHNOFLO
      !
      ! -- fill buff with elastic interbed compaction
      do node = 1, this%dis%nodes
        this%buff(node) = DZERO
      end do
      do ib = 1, this%ninterbeds
        node = this%nodelist(ib)
        this%buff(node) = this%buff(node) + this%tcompe(ib)
      end do
      !
      ! -- write elastic interbed compaction data to binary file
      call this%dis%record_array(this%buff, this%iout, iprint, ibinun,          &
                                 comptxt(3), cdatafmp, nvaluesp,                &
                                 nwidthp, editdesc, dinact)
    end if
    !
    ! -- Set unit number for binary skeletal compaction
    if(this%ioutcomps /= 0) then
      ibinun = this%ioutcomps
    else
      ibinun = 0
    endif
    if(idvfl == 0) ibinun = 0
    !
    ! -- save skeletal compaction results
    if(ibinun /= 0) then
      iprint = 0
      dinact = DHNOFLO
      !
      ! -- fill buff with skeletal compaction
      do node = 1, this%dis%nodes
        this%buff(node) = this%sk_tcomp(node)
      end do
      !
      ! -- write skeletal compaction data to binary file
      call this%dis%record_array(this%buff, this%iout, iprint, ibinun,          &
                                 comptxt(4), cdatafmp, nvaluesp,                &
                                 nwidthp, editdesc, dinact)
    end if
    !
    ! -- Save observations.
    if (this%obs%npakobs > 0) then
      call this%obs%obs_ot()
    end if
    !
    ! -- Return
    return
  end subroutine csub_bdsav

  subroutine csub_fp(this)
! **************************************************************************
! csub_cc -- Final processing for package
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    use InputOutputModule, only: UWWORD
    ! -- dummy
    class(GwfCsubType) :: this
    ! -- local
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: linesep
    character(len=LINELENGTH) :: msg
    character(len=10) :: ctype
    character(len=20) :: cellid
    character(len=10) :: cflag
    character(len=16) :: text
    integer(I4B) :: i
    integer(I4B) :: ib
    integer(I4B) :: i0
    integer(I4B) :: i1
    integer(I4B) :: node
    integer(I4B) :: iloc
    integer(I4B) :: n
    integer(I4B) :: nn
    integer(I4B) :: idelay
    integer(I4B) :: iexceed
    integer(I4B), parameter :: ncells = 20
    integer(I4B) :: nlen
    real(DP) :: rval
    real(DP) :: strain
    real(DP) :: pctcomp
    integer(I4B), dimension(:), allocatable :: imap_sel
    real(DP), dimension(:), allocatable :: pctcomp_arr
    ! format
02000 FORMAT (1X,///1X,A,1X,A,1X,A)
! --------------------------------------------------------------------------
    if (this%ninterbeds > 0) then
      nlen = min(ncells,this%ninterbeds)
      allocate(imap_sel(nlen))
      allocate(pctcomp_arr(this%ninterbeds))
      iexceed = 0
      do ib = 1, this%ninterbeds
        strain = this%tcomp(ib) / this%thickini(ib)
        pctcomp = DHUNDRED * strain
        pctcomp_arr(ib) = pctcomp
        if (pctcomp >= DONE) then
          iexceed = iexceed + 1
        end if
      end do
      call selectn(imap_sel, pctcomp_arr, reverse=.TRUE.)
      !
      ! -- summary interbed strain table
      i0 = max(1, this%ninterbeds-ncells+1)
      i1 = this%ninterbeds
      msg = ''
      if (iexceed /= 0) then
        write(msg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                                  &
          '-- LARGEST', (i1 - i0 + 1), 'OF', this%ninterbeds,                   &
          'INTERBED STRAIN VALUES SHOWN'
      end if
      write (this%iout, 2000) trim(this%name), 'INTERBED STRAIN SUMMARY',       &
        trim(adjustl(msg))
      iloc = 1
      line = ''
      call UWWORD(line, iloc, 10, 1, 'interbed', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, 'interbed', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 20, 1, 'interbed', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1, 'initial', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1, 'final', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1, 'total', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1, 'final', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1, 'percent', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, '', n, rval, CENTER=.TRUE.)
      ! -- create line separator
      linesep = repeat('-', iloc)
      ! -- write first line
      write(this%iout,'(1X,A)') linesep(1:iloc)
      write(this%iout,'(1X,A)') line(1:iloc)
      ! -- create second header line
      iloc = 1
      line = ''
      call UWWORD(line, iloc, 10, 1, 'number', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, 'type', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 20, 1, 'location', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1, 'thickness', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1, 'thickness', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1, 'compaction', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1, 'strain', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1, 'compaction', n, rval, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, 'flag', n, rval, CENTER=.TRUE.)
      ! -- write second line
      write(this%iout,'(1X,A)') line(1:iloc)
      write(this%iout,'(1X,A)') linesep(1:iloc)
      ! -- write data
      if (iexceed /= 0) then
        do i = 1, nlen
          ib = imap_sel(i)
          idelay = this%idelay(ib)
          if (idelay == 0) then
            ctype = 'no-delay'
          else
            ctype = 'delay'
          end if
          strain = this%tcomp(ib) / this%thickini(ib)
          pctcomp = DHUNDRED * strain
          if (pctcomp >= 5.0_DP) then
            cflag = '**>=5%'
          else if (pctcomp >= DONE) then
            cflag = '*>=1%'
          else
            cflag = ''
          end if
          node = this%nodelist(ib)
          call this%dis%noder_to_string(node, cellid)
          iloc = 1
          line = ''
          call UWWORD(line, iloc, 10, 2, text, ib, rval)
          call UWWORD(line, iloc, 10, 1, ctype, n, rval)
          call UWWORD(line, iloc, 20, 1, cellid, n, rval, CENTER=.TRUE.)
          call UWWORD(line, iloc, 16, 3, text, n, this%thickini(ib))
          call UWWORD(line, iloc, 16, 3, text, n, this%thick(ib))
          call UWWORD(line, iloc, 16, 3, text, n, this%tcomp(ib))
          call UWWORD(line, iloc, 16, 3, text, n, strain)
          call UWWORD(line, iloc, 16, 3, text, n, pctcomp)
          call UWWORD(line, iloc, 10, 1, cflag, ib, rval)
          write(this%iout, '(1X,A)') line(1:iloc)
        end do
        write(this%iout, '(/1X,A,1X,I0,1X,A,1X,I0,1X,A,/1X,A,/1X,A)') &
          'PERCENT COMPACTION IS GREATER THAN OR EQUAL TO 1 PERCENT IN',      &
          iexceed, 'OF', this%ninterbeds, 'INTERBED(S).',                     &
          'USE THE STRAIN_CSV_INTERBED OPTION TO OUTPUT A CSV ' //            &
          'FILE WITH PERCENT COMPACTION ', 'VALUES FOR ALL INTERBEDS.'
      else
        msg = 'PERCENT COMPACTION WAS LESS THAN 1 PERCENT IN ALL INTERBEDS'
        write(this%iout, '(/1X,A)') trim(adjustl(msg))
      end if
      !
      ! -- write csv file
      if (this%istrainib /= 0) then
        iloc = 1
        line = ''
        call UWWORD(line, iloc, 20, 1, 'interbed_number', n, rval, SEP=',')
        call UWWORD(line, iloc, 20, 1, 'interbed_type', n, rval, SEP=',')
        call UWWORD(line, iloc, 22, 1, 'cellid', n, rval, SEP=',')
        call UWWORD(line, iloc, 20, 1, 'initial_thickness', n, rval, SEP=',')
        call UWWORD(line, iloc, 20, 1, 'final_thickness', n, rval, SEP=',')
        call UWWORD(line, iloc, 20, 1, 'total_compaction', n, rval, SEP=',')
        call UWWORD(line, iloc, 20, 1, 'total_strain', n, rval, SEP=',')
        call UWWORD(line, iloc, 20, 1, 'percent_compaction', n, rval)
        ! -- write second line
        write(this%istrainib,'(1X,A)') line(1:iloc)
        ! -- write data
        do ib = 1, this%ninterbeds
          idelay = this%idelay(ib)
          if (idelay == 0) then
            ctype = 'no-delay'
          else
            ctype = 'delay'
          end if
          strain = this%tcomp(ib) / this%thickini(ib)
          pctcomp = DHUNDRED * strain
          node = this%nodelist(ib)
          call this%dis%noder_to_string(node, cellid)
          iloc = 1
          line = ''
          call UWWORD(line, iloc, 20, 2, text, ib, rval, SEP=',')
          call UWWORD(line, iloc, 20, 1, ctype, n, rval, SEP=',')
          call UWWORD(line, iloc, 22, 1, '"'//trim(adjustl(cellid))//'"',       &
                      n, rval, SEP=',')
          call UWWORD(line, iloc, 20, 3, text, n, this%thickini(ib), SEP=',')
          call UWWORD(line, iloc, 20, 3, text, n, this%thick(ib), SEP=',')
          call UWWORD(line, iloc, 20, 3, text, n, this%tcomp(ib), SEP=',')
          call UWWORD(line, iloc, 20, 3, text, n, strain, SEP=',')
          call UWWORD(line, iloc, 20, 3, text, n, pctcomp)
          write(this%istrainib, '(1X,A)') line(1:iloc)
        end do
      end if
      !
      ! -- deallocate temporary storage
      deallocate(imap_sel)
      deallocate(pctcomp_arr)
    end if
    
    nlen = min(ncells,this%dis%nodes)
    allocate(imap_sel(nlen))
    allocate(pctcomp_arr(this%dis%nodes))
    iexceed = 0
    do node = 1, this%dis%nodes
      strain = DZERO
      if (this%sk_thickini(node) > DZERO) then
        strain = this%sk_tcomp(node) / this%sk_thickini(node)
      end if
      pctcomp = DHUNDRED * strain
      pctcomp_arr(node) = pctcomp
      if (pctcomp >= DONE) then
        iexceed = iexceed + 1
      end if
    end do
    call selectn(imap_sel, pctcomp_arr, reverse=.TRUE.)
    !
    ! -- summary skeletal strain table
    i0 = max(1, this%dis%nodes-ncells+1)
    i1 = this%dis%nodes
    msg = ''
    if (iexceed /= 0) then
      write(msg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                                    &
        '-- LARGEST ', (i1 - i0 + 1), 'OF', this%dis%nodes,                     &
        'CELL SKELETAL VALUES SHOWN'
    end if
    write (this%iout, 2000) trim(this%name), 'SKELETAL STRAIN SUMMARY',         &
      trim(adjustl(msg))
    iloc = 1
    line = ''
    call UWWORD(line, iloc, 20, 1, 'cell', n, rval, CENTER=.TRUE.)
    call UWWORD(line, iloc, 16, 1, 'initial', n, rval, CENTER=.TRUE.)
    call UWWORD(line, iloc, 16, 1, 'final', n, rval, CENTER=.TRUE.)
    call UWWORD(line, iloc, 16, 1, 'total', n, rval, CENTER=.TRUE.)
    call UWWORD(line, iloc, 16, 1, 'final', n, rval, CENTER=.TRUE.)
    call UWWORD(line, iloc, 16, 1, 'percent', n, rval, CENTER=.TRUE.)
    call UWWORD(line, iloc, 10, 1, '', n, rval, CENTER=.TRUE.)
    ! -- create line separator
    linesep = repeat('-', iloc)
    ! -- write first line
    write(this%iout,'(1X,A)') linesep(1:iloc)
    write(this%iout,'(1X,A)') line(1:iloc)
    ! -- create second header line
    iloc = 1
    line = ''
    call UWWORD(line, iloc, 20, 1, 'location', n, rval, CENTER=.TRUE.)
    call UWWORD(line, iloc, 16, 1, 'thickness', n, rval, CENTER=.TRUE.)
    call UWWORD(line, iloc, 16, 1, 'thickness', n, rval, CENTER=.TRUE.)
    call UWWORD(line, iloc, 16, 1, 'compaction', n, rval, CENTER=.TRUE.)
    call UWWORD(line, iloc, 16, 1, 'strain', n, rval, CENTER=.TRUE.)
    call UWWORD(line, iloc, 16, 1, 'compaction', n, rval, CENTER=.TRUE.)
    call UWWORD(line, iloc, 10, 1, 'flag', n, rval, CENTER=.TRUE.)
    ! -- write second line
    write(this%iout,'(1X,A)') line(1:iloc)
    write(this%iout,'(1X,A)') linesep(1:iloc)
    ! -- write data
    if (iexceed /= 0) then
      do nn = 1, nlen
        node = imap_sel(nn)
        if (this%sk_thickini(node) > DZERO) then
          strain = this%sk_tcomp(node) / this%sk_thickini(node)
        else
          strain = DZERO
        end if
        pctcomp = DHUNDRED * strain
        if (pctcomp >= 5.0_DP) then
          cflag = '**>=5%'
        else if (pctcomp >= DONE) then
          cflag = '*>=1%'
        else
          cflag = ''
        end if
        call this%dis%noder_to_string(node, cellid)
        iloc = 1
        line = ''
        call UWWORD(line, iloc, 20, 1, cellid, n, rval, CENTER=.TRUE.)
        call UWWORD(line, iloc, 16, 3, text, n, this%sk_thickini(node))
        call UWWORD(line, iloc, 16, 3, text, n, this%sk_thick(node))
        call UWWORD(line, iloc, 16, 3, text, n, this%sk_tcomp(node))
        call UWWORD(line, iloc, 16, 3, text, n, strain)
        call UWWORD(line, iloc, 16, 3, text, n, pctcomp)
        call UWWORD(line, iloc, 10, 1, cflag, ib, rval)
        write(this%iout, '(1X,A)') line(1:iloc)
      end do
      write(this%iout, '(/1X,A,1X,I0,1X,A,1X,I0,1X,A,/1X,A,/1X,A)') &
        'SKELETAL STORAGE PERCENT COMPACTION IS GREATER THAN OR ' //          &
        'EQUAL TO 1 PERCENT IN', iexceed, 'OF', this%dis%nodes, 'CELL(S).',   &
        'USE THE STRAIN_CSV_SKELETAL OPTION TO OUTPUT A CSV ' //              &
        'FILE WITH PERCENT COMPACTION ', 'VALUES FOR ALL CELLS.'
    else
      msg = 'SKELETAL STORAGE PERCENT COMPACTION WAS LESS THAN ' //           &
            '1 PERCENT IN ALL CELLS '
      write(this%iout, '(/1X,A)') trim(adjustl(msg))
    end if
    !
    ! -- write csv file
    if (this%istrainsk /= 0) then
      iloc = 1
      line = ''
      call UWWORD(line, iloc, 20, 1, 'node', n, rval, SEP=',')
      call UWWORD(line, iloc, 22, 1, 'cellid', n, rval, SEP=',')
      call UWWORD(line, iloc, 20, 1, 'initial_thickness', n, rval, SEP=',')
      call UWWORD(line, iloc, 20, 1, 'final_thickness', n, rval, SEP=',')
      call UWWORD(line, iloc, 20, 1, 'total_compaction', n, rval, SEP=',')
      call UWWORD(line, iloc, 20, 1, 'total_strain', n, rval, SEP=',')
      call UWWORD(line, iloc, 20, 1, 'percent_compaction', n, rval)
      ! -- write second line
      write(this%istrainsk,'(1X,A)') line(1:iloc)
      ! -- write data
      do node = 1, this%dis%nodes
        if (this%sk_thickini(node) > DZERO) then
          strain = this%sk_tcomp(node) / this%sk_thickini(node)
        else
          strain = DZERO
        end if
        pctcomp = DHUNDRED * strain
        call this%dis%noder_to_string(node, cellid)
        iloc = 1
        line = ''
        call UWWORD(line, iloc, 20, 2, text, node, rval, SEP=',')
        call UWWORD(line, iloc, 22, 1, '"'//trim(adjustl(cellid))//'"',       &
                    n, rval, SEP=',')
        call UWWORD(line, iloc, 20, 3, text, n, this%sk_thickini(node),       &
                    SEP=',')
        call UWWORD(line, iloc, 20, 3, text, n, this%sk_thick(node), SEP=',')
        call UWWORD(line, iloc, 20, 3, text, n, this%sk_tcomp(node), SEP=',')
        call UWWORD(line, iloc, 20, 3, text, n, strain, SEP=',')
        call UWWORD(line, iloc, 20, 3, text, n, pctcomp)
        write(this%istrainsk, '(1X,A)') line(1:iloc)
      end do
    end if
    !
    ! -- deallocate temporary storage
    deallocate(imap_sel)
    deallocate(pctcomp_arr)
    !
    ! -- return
    return
  end subroutine csub_fp

  subroutine csub_read_packagedata(this)
! ******************************************************************************
! pak1read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use MemoryManagerModule, only: mem_reallocate
!    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    ! -- dummy
    class(GwfCsubType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: cellid
    character(len=LENBOUNDNAME) :: bndName, bndNameTemp
    character(len=7) :: cdelay
    character(len=9) :: cno
    character(len=50), dimension(:), allocatable :: caux
    integer(I4B) :: ival
    logical :: isfound, endOfBlock
    integer(I4B) :: n
    integer(I4B) :: nn
    integer(I4B) :: ib
    !integer(I4B) :: j
    integer(I4B) :: iaux
    integer(I4B) :: itmp
    integer(I4B) :: ierr
    integer(I4B) :: ndelaybeds
    integer(I4B) :: idelay
    integer(I4B) :: node
    real(DP) :: rval
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: baq
    integer, allocatable, dimension(:) :: nboundchk
    !
    ! -- initialize temporary variables
    ndelaybeds = 0
    !
    ! -- allocate temporary arrays
    allocate(nboundchk(this%ninterbeds))
    do n = 1, this%ninterbeds
      nboundchk(n) = 0
    end do
    !
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr,                     &
                              supportopenclose=.true.)
    !
    ! -- parse locations block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%name))// &
        ' PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        ! -- read interbed number
        itmp = this%parser%GetInteger()

        if (itmp < 1 .or. itmp > this%ninterbeds) then
          write(errmsg,'(4x,a,1x,i0,1x,a,1x,i0)') &
            '****ERROR. INTERBED NUMBER (', itmp, ') MUST BE > 0 and <= ', this%ninterbeds
          call store_error(errmsg)
          cycle
        end if

        ! -- increment nboundchk
        nboundchk(itmp) = nboundchk(itmp) + 1

        ! -- read cellid
        call this%parser%GetCellid(this%dis%ndim, cellid)
        nn = this%dis%noder_from_cellid(cellid, &
                                      this%parser%iuactive, this%iout)
        n = this%dis%nodeu_from_cellid(cellid, &
                                      this%parser%iuactive, this%iout)
        top = this%dis%top(nn)
        bot = this%dis%bot(nn)
        baq = top - bot
        ! -- determine if a valid cell location was provided
          if (nn < 1) then
            write(errmsg,'(4x,a,1x,i4,1x)') &
              '****ERROR. INVALID cellid FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
          end if
        !todo error trapping here...
        this%nodelist(itmp) = nn
        this%unodelist(itmp) = n

        ! cdelay
        call this%parser%GetStringCaps(cdelay)
        select case (cdelay)
          case ('NODELAY')
            ival = 0
          case ('DELAY')
            ndelaybeds = ndelaybeds + 1
            ival = ndelaybeds
          case default
            write(errmsg,'(4x,a,1x,a,1x,i0,1x)') &
              '****ERROR. INVALID CDELAY ', trim(adjustl(cdelay)), &
              ' FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
            cycle
          end select
        idelay = ival
        this%idelay(itmp) = ival

        ! get initial preconsolidation stress
        this%pcs(itmp) = this%parser%GetDouble()

        ! -- get thickness or cell fraction
        rval = this%parser%GetDouble()
        if (this%icellf == 0) then
          if (rval <= 0.0 .or. rval > baq) then
              write(errmsg,'(4x,a,1x,g0,1x,a,1x,g0,1x,a,1x,i0)') &
                '****ERROR. thick (', rval,') MUST BE > 0 AND LESS THAN ', baq, &
                'FOR PACKAGEDATA ENTRY', itmp
              call store_error(errmsg)
          end if
        else
          if (rval <= DZERO .or. rval > DONE) then
              write(errmsg,'(4x,a,1x,i0)') &
                '****ERROR. frac MUST BE > 0 AND < 1 FOR PACKAGEDATA ENTRY', itmp
              call store_error(errmsg)
          end if
          rval = rval * baq
        end if
        this%thick(itmp) = rval
        this%thickini(itmp) = rval

        ! -- get rnb
        rval = this%parser%GetDouble()
        if (idelay > 0) then
          if (rval < DONE) then
              write(errmsg,'(4x,a,1x,g0,1x,a,1x,a,1x,i0)') &
                '****ERROR. rnb (', rval,') MUST BE >= 1.', &
                'FOR PACKAGEDATA ENTRY', itmp
              call store_error(errmsg)
          end if
        else
          rval = DONE
        end if
        this%rnb(itmp) = rval

        ! -- get skv or ci
        rval =  this%parser%GetDouble()
        if (rval <= 0.0) then
            write(errmsg,'(4x,a,1x,i4,1x)') &
              '****ERROR. INVALID (ci,ssv) FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
        end if
        this%ci(itmp) = rval

        ! -- get ske or rci
        rval =  this%parser%GetDouble()
        if (rval <= 0.0) then
            write(errmsg,'(4x,a,1x,i4,1x)') &
              '****ERROR. INVALID (rci,sse) FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
        end if
        this%rci(itmp) = rval

        ! -- get porosity
        rval =  this%parser%GetDouble()
        this%theta(itmp) = rval
        if (rval <= DZERO .or. rval > DONE) then
            write(errmsg,'(4x,a,1x,i4,1x)') &
              '****ERROR. INVALID porosity FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
        end if

        ! -- get kv
        rval =  this%parser%GetDouble()
        if (idelay > 0) then
          if (rval <= 0.0) then
             write(errmsg,'(4x,a,1x,i4,1x)') &
               '****ERROR. kv MUST BE GREATER THAN ZERO FOR PACKAGEDATA ENTRY', itmp
             call store_error(errmsg)
          end if
        end if
        this%kv(itmp) = rval

        ! -- get h0
        rval =  this%parser%GetDouble()
        this%h0(itmp) = rval

        ! -- get bound names
        write (cno,'(i9.9)') nn
          bndName = 'nsystem' // cno
        call this%parser%GetStringCaps(bndNameTemp)
        if (bndNameTemp /= '') then
        bndName = bndNameTemp(1:16)
        endif
        this%boundname(itmp) = bndName
      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%name))//' PACKAGEDATA'
    !else
    !  call store_error('ERROR.  REQUIRED PACKAGEDATA BLOCK NOT FOUND.')
    endif
    !
    ! -- Check to make sure that every reach is specified and that no reach
    !    is specified more than once.
    do ib = 1, this%ninterbeds
      if (nboundchk(ib) == 0) then
        write(errmsg, '(a, i0, a)') 'ERROR: INFORMATION FOR INTERBED ', ib,     &
                                    ' NOT SPECIFIED IN PACKAGEDATA BLOCK.'
        call store_error(errmsg)
      else if (nboundchk(ib) > 1) then
        write(errmsg, '(a, i0, i0)') 'ERROR: INFORMATION SPECIFIED ',           &
                                     nboundchk(ib), ' TIMES FOR INTERBED ', ib
        call store_error(errmsg)
      endif
    end do
    deallocate(nboundchk)
    !
    ! -- set the number of delay interbeds
    this%ndelaybeds = ndelaybeds
    !
    ! -- process delay interbeds
    if (ndelaybeds > 0) then
      !
      ! -- check that delay interbeds are not specified in convertible cells
      !    unless DELAY_SATURATION_SCALING option is specified
      ierr = 0
      if (this%idbsatscaling == 0) then
        do ib = 1, this%ninterbeds
          idelay = this%idelay(ib)
          if (idelay == 0) then
            cycle
          end if
          node = this%nodelist(ib)
          call this%dis%noder_to_string(node, cellid)
          if (this%stoiconv(node) /= 0) then
            write(errmsg, '(a,i0,a,1x,a,1x,a)') &
              'ERROR: DELAY INTERBED (', ib,                                    &
              ') SPECIFIED IN CONVERTIBLE CELL', trim(adjustl(cellid)),         &
              'USE THE DELAY_SATURATION_SCALING OPTION TO ALLOW ' //            &
              'DELAY BEDS IN CONVERTIBLE CELLS'
            call store_error(errmsg)
          end if
        end do
      end if
      !
      ! -- reallocate and initialize delay interbed arrays
      if (ierr == 0) then
        !
        ! -- reallocate delay bed arrays
        call mem_reallocate(this%dbdz, ndelaybeds, 'dbdz', trim(this%origin))
        call mem_reallocate(this%dbz, this%ndelaycells, ndelaybeds, 'dbz', trim(this%origin))
        call mem_reallocate(this%dbh, this%ndelaycells, ndelaybeds, 'dbh', trim(this%origin))
        call mem_reallocate(this%dbh0, this%ndelaycells, ndelaybeds, 'dbh0', trim(this%origin))
        call mem_reallocate(this%dbtheta, this%ndelaycells, ndelaybeds, 'dbtheta', trim(this%origin))
        call mem_reallocate(this%dbtheta0, this%ndelaycells, ndelaybeds, 'dbtheta0', trim(this%origin))
        call mem_reallocate(this%dbgeo, this%ndelaycells, ndelaybeds, 'dbgeo', trim(this%origin))
        call mem_reallocate(this%dbgeo0, this%ndelaycells, ndelaybeds, 'dbgeo0', trim(this%origin))
        call mem_reallocate(this%dbes, this%ndelaycells, ndelaybeds, 'dbes', trim(this%origin))
        call mem_reallocate(this%dbes0, this%ndelaycells, ndelaybeds, 'dbes0', trim(this%origin))
        call mem_reallocate(this%dbpcs, this%ndelaycells, ndelaybeds, 'dbpcs', trim(this%origin))
        !
        ! -- reallocate delay interbed solution arrays
        call mem_reallocate(this%dbal, this%ndelaycells, 'dbal', trim(this%origin))
        call mem_reallocate(this%dbad, this%ndelaycells, 'dbad', trim(this%origin))
        call mem_reallocate(this%dbau, this%ndelaycells, 'dbau', trim(this%origin))
        call mem_reallocate(this%dbrhs, this%ndelaycells, 'dbrhs', trim(this%origin))
        call mem_reallocate(this%dbdh, this%ndelaycells, 'dbdh', trim(this%origin))
        call mem_reallocate(this%dbaw, this%ndelaycells, 'dbaw', trim(this%origin))
        !
        ! -- initialize delay bed storage
        do ib = 1, this%ninterbeds
          idelay = this%idelay(ib)
          if (idelay == 0) then
            cycle
          end if
          !
          ! -- adjust thickness if solving half cell problem
          this%thick(ib) = this%dbfacti * this%thick(ib)
          !
          ! -- calculate delay bed cell thickness
          if (this%idbhalfcell == 0) then
            this%dbdz(idelay) = this%thick(ib) / real(this%ndelaycells, DP)
          else
            this%dbdz(idelay) = this%thick(ib) / (real(this%ndelaycells, DP) - DHALF)
          end if
          !
          ! -- initialize delay interbed variables
          do n = 1, this%ndelaycells
            this%dbh(n, idelay) = this%h0(ib)
            this%dbh0(n, idelay) = this%h0(ib)
            this%dbtheta(n, idelay) = this%theta(ib)
            this%dbtheta0(n, idelay) = this%theta(ib)
            this%dbgeo(n, idelay) = DZERO
            this%dbgeo0(n, idelay) = DZERO
            this%dbes(n, idelay) = DZERO
            this%dbes0(n, idelay) = DZERO
            this%dbpcs(n, idelay) = this%pcs(ib)
          end do
          ! 
          ! -- initialize elevation of delay bed cells
          call this%csub_delay_calc_zcell(ib)

        end do
        !
        ! -- initialize delay bed solution arrays
        do n = 1, this%ndelaycells
          this%dbal(n) = DZERO
          this%dbad(n) = DZERO
          this%dbau(n) = DZERO
          this%dbrhs(n) = DZERO
          this%dbdh(n) = DZERO
          this%dbaw(n) = DZERO
        end do
      end if
    end if
    !
    ! TODO - check the total frac for each nbound node to make sure < 1.0
    !
    ! -- return
    return
  end subroutine csub_read_packagedata

  subroutine read_options(this)
! ******************************************************************************
! read_options -- set options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: MAXCHARLEN, DZERO
    use OpenSpecModule, only: access, form
    use InputOutputModule, only: urword, getunit, urdaux, openfile
    implicit none
    ! -- dummy
    class(GwfCsubType),   intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: keyword
    character(len=LINELENGTH) :: line
    character(len=MAXCHARLEN) :: fname
    logical :: isfound
    logical :: endOfBlock
    integer(I4B) :: lloc
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: ierr
    integer(I4B) :: inobs
    integer(I4B) :: ibrg
    real(DP) :: time_weight
    ! -- formats
    character(len=*),parameter :: fmtflow = &
      "(4x, 'FLOWS WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
    character(len=*),parameter :: fmtflow2 = &
      "(4x, 'FLOWS WILL BE SAVED TO BUDGET FILE SPECIFIED IN OUTPUT CONTROL')"
    character(len=*),parameter :: fmtssessv = &
      "(4x, 'USING SSE AND SSV INSTEAD OF CR AND CC.')"
    character(len=*),parameter :: fmtoffset = &
      "(4x, 'INITIAL_STRESS TREATED AS AN OFFSET.')"
    character(len=*),parameter :: fmtopt = &
      "(4x, A)"
    character(len=*),parameter :: fmtopti = &
      "(4x, A, 1X, I0)"
    character(len=*),parameter :: fmtoptr = &
      "(4x, A, 1X, G0)"
    character(len=*),parameter :: fmtfileout = &
      "(4x, 'CSUB ', 1x, a, 1x, ' WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
! -----------------------------------------------------------------------------
    !
    ! -- initialize variables
    ibrg = 0
    time_weight = DNODATA
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING CSUB OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case('AUX', 'AUXILIARY')
            call this%parser%GetRemainingLine(line)
            lloc = 1
            call urdaux(this%naux, this%parser%iuactive, this%iout, lloc, &
                        istart, istop, this%auxname, line, this%name)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtflow2)
          case ('PRINT_INPUT')
            this%iprpak = 1
            write(this%iout,'(4x,a)') 'LISTS OF '//trim(adjustl(this%name))// &
              ' CELLS WILL BE PRINTED.'
          case ('PRINT_FLOWS')
            this%iprflow = 1
            write(this%iout,'(4x,a)') trim(adjustl(this%name))// &
              ' FLOWS WILL BE PRINTED TO LISTING FILE.'
          case ('BOUNDNAMES')
            this%inamedbound = 1
            write(this%iout,'(4x,a)') trim(adjustl(this%name))// &
              ' BOUNDARIES HAVE NAMES IN LAST COLUMN.'          ! user specified number of delay cells used in each system of delay intebeds
          case ('OBS6')
            call this%parser%GetStringCaps(keyword)
            if(trim(adjustl(keyword)) /= 'FILEIN') then
              errmsg = 'OBS6 keyword must be followed by "FILEIN" ' //         &
                       'then by filename.'
              call store_error(errmsg)
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
            this%inobspkg = inobs
            
            call this%obs%obs_df(this%iout, this%name, this%filtyp, this%dis)
            call this%csub_df_obs()
          !
          ! -- CSUB specific options
          case ('TIME_WEIGHT')
            time_weight = this%parser%GetDouble()
          case ('GAMMAW')
            this%gammaw =  this%parser%GetDouble()
            write(this%iout, fmtoptr) 'GAMMAW =', this%gammaw
            ibrg = 1
          case ('BETA')
            this%beta =  this%parser%GetDouble()
            write(this%iout, fmtoptr) 'BETA =', this%beta
            ibrg = 1
          case ('HEAD_BASED')
            this%igeocalc = 0
            write(this%iout, fmtopt) 'HEAD-BASED FORMULATION WILL BE USED'
          case ('NDELAYCELLS')
            this%ndelaycells =  this%parser%GetInteger()
            write(this%iout, fmtopti) 'NUMBER OF DELAY CELLS =', this%ndelaycells
          case ('DELAY_SATURATION_SCALING')
            this%idbsatscaling = 1
            write(this%iout, fmtopt) &
              'DELAY-INTERBEDS WILL BE ALLOWED IN CONVERTIBLE CELLS'
          case ('SPECIFIED_INITIAL_INTERBED_STATE')
            this%ispecified_pcs = 1
            this%ispecified_dbh = 1
            write(this%iout, fmtopt) &
              'PRECONSOLIDATION STRESS AND DELAY INTERBED HEADS WILL BE ' //    &
              'SPECIFIED AS ABSOLUTE VALUES INSTEAD OF RELATIVE TO ' //         &
              'INITIAL STRESS CONDITIONS AND GWF HEADS'
          case ('SPECIFIED_INITIAL_PRECONSOLIDATION_STRESS')
            this%ispecified_pcs = 1
            write(this%iout, fmtopt) &
              'PRECONSOLIDATION STRESS WILL BE SPECIFIED AS ABSOLUTE VALUES ' //&
              'INSTEAD OF RELATIVE TO INITIAL STRESS CONDITIONS'
          case ('SPECIFIED_INITIAL_DELAY_HEAD')
            this%ispecified_dbh = 1
            write(this%iout, fmtopt) &
              'DELAY INTERBED HEADS WILL BE SPECIFIED AS ABSOLUTE VALUES ' //   &
              'INSTEAD OF RELATIVE TO INITIAL GWF HEADS'
          !
          ! -- initial effective stress will be calculated as an offset from 
          !    calculated initial stress
          case ('INTERBED_STRESS_OFFSET')
            this%ibedstressoff = 1
            write(this%iout, fmtopt) &
              'OFFSET WILL BE APPLIED TO INITIAL INTERBED EFFECTIVE STRESS'
          !
          ! -- an offset will be applied to geostatic stress
          case ('GEO_STRESS_OFFSET')
            this%igeostressoff = 1
            write(this%iout, fmtopt) &
              'A USER-DEFINED OFFSET WILL BE APPLIED TO GEOSTATIC STRESS'
          !
          ! -- compression indicies (CR amd CC) will be specified instead of 
          !    storage coefficients (SSE and SSV) 
          case ('COMPRESSION_INDICES')
            this%istoragec = 0
            write(this%iout, fmtopt) &
              'COMPRESSION INDICES WILL BE SPECIFIED INSTEAD OF ' //            &
              'ELASTIC AND INELASTIC SPECIFIC COEFFICIENTS'
          ! -- strain table options
          case ('STRAIN_CSV_INTERBED')
            call this%parser%GetStringCaps(keyword)
            if (keyword == 'FILEOUT') then
              call this%parser%GetString(fname)
              this%istrainib = getunit()
              call openfile(this%istrainib, this%iout, fname, 'CSV_OUTPUT',     &
                            filstat_opt='REPLACE')
              write(this%iout,fmtfileout) &
                'INTERBED STRAIN CSV', fname, this%istrainib
            else
              errmsg = 'OPTIONAL STRAIN_CSV_INTERBED KEYWORD MUST BE ' //       &
                       'FOLLOWED BY FILEOUT'
              call store_error(errmsg)
            end if
          case ('STRAIN_CSV_SKELETAL')
            call this%parser%GetStringCaps(keyword)
            if (keyword == 'FILEOUT') then
              call this%parser%GetString(fname)
              this%istrainsk = getunit()
              call openfile(this%istrainsk, this%iout, fname, 'CSV_OUTPUT',     &
                            filstat_opt='REPLACE')
              write(this%iout,fmtfileout) &
                'SKELETAL STRAIN CSV', fname, this%istrainsk
            else
              errmsg = 'OPTIONAL STRAIN_CSV_SKELETAL KEYWORD MUST BE ' //       &
                       'FOLLOWED BY FILEOUT'
              call store_error(errmsg)
            end if
          ! -- compaction output
          case ('COMPACTION')
            call this%parser%GetStringCaps(keyword)
            if (keyword == 'FILEOUT') then
              call this%parser%GetString(fname)
              this%ioutcomp = getunit()
              call openfile(this%ioutcomp, this%iout, fname, 'DATA(BINARY)',    &
                            form, access, 'REPLACE')
              write(this%iout,fmtfileout) &
                'COMPACTION', fname, this%ioutcomp
            else 
              errmsg = 'OPTIONAL COMPACTION KEYWORD MUST BE ' //                &
                       'FOLLOWED BY FILEOUT'
              call store_error(errmsg)
            end if
          case ('COMPACTION_INELASTIC')
            call this%parser%GetStringCaps(keyword)
            if (keyword == 'FILEOUT') then
              call this%parser%GetString(fname)
              this%ioutcompi = getunit()
              call openfile(this%ioutcompi, this%iout, fname,                   &
                            'DATA(BINARY)', form, access, 'REPLACE')
              write(this%iout,fmtfileout) &
                'COMPACTION_INELASTIC', fname, this%ioutcompi
            else 
              errmsg = 'OPTIONAL COMPACTION_INELASTIC KEYWORD MUST BE ' //      &
                       'FOLLOWED BY FILEOUT'
              call store_error(errmsg)
            end if
          case ('COMPACTION_ELASTIC')
            call this%parser%GetStringCaps(keyword)
            if (keyword == 'FILEOUT') then
              call this%parser%GetString(fname)
              this%ioutcompe = getunit()
              call openfile(this%ioutcompe, this%iout, fname,                   &
                            'DATA(BINARY)', form, access, 'REPLACE')
              write(this%iout,fmtfileout) &
                'COMPACTION_ELASTIC', fname, this%ioutcompe
            else 
              errmsg = 'OPTIONAL COMPACTION_ELASTIC KEYWORD MUST BE ' //        &
                       'FOLLOWED BY FILEOUT'
              call store_error(errmsg)
            end if
          case ('COMPACTION_SKELETAL')
            call this%parser%GetStringCaps(keyword)
            if (keyword == 'FILEOUT') then
              call this%parser%GetString(fname)
              this%ioutcomps = getunit()
              call openfile(this%ioutcomps, this%iout, fname,                   &
                            'DATA(BINARY)', form, access, 'REPLACE')
              write(this%iout,fmtfileout) &
                'COMPACTION_SKELETAL', fname, this%ioutcomps
            else 
              errmsg = 'OPTIONAL COMPACTION_ELASTIC KEYWORD MUST BE ' //        &
                       'FOLLOWED BY FILEOUT'
              call store_error(errmsg)
            end if
          case ('ZDISPLACEMENT')
            call this%parser%GetStringCaps(keyword)
            if (keyword == 'FILEOUT') then
              call this%parser%GetString(fname)
              this%ioutzdisp = getunit()
              call openfile(this%ioutzdisp, this%iout, fname,                   &
                            'DATA(BINARY)', form, access, 'REPLACE')
              write(this%iout,fmtfileout) &
                'ZDISPLACEMENT', fname, this%ioutzdisp
            else 
              errmsg = 'OPTIONAL ZDISPLACEMENT KEYWORD MUST BE ' //             &
                       'FOLLOWED BY FILEOUT'
              call store_error(errmsg)
            end if
          ! variable thickness and void ratio
          case ('UPDATE_MATERIAL_PROPERTIES')
            this%iupdatematprop = 1
            write(this%iout, fmtopt) 'THICKNESS AND VOID RATIO WILL BE ' //     &
                                     'ADJUSTED DURING THE SIMULATION'
          ! cell fraction will be specified instead of interbed thickness
          case ('CELL_FRACTION')
            this%icellf = 1
            write(this%iout, fmtopt) 'INTERBED THICKNESS WILL BE SPECIFIED ' //  &
                                     'AS A CELL FRACTION'
          ! half cell formulation for delay interbed
          case ('DELAY_FULL_CELL')
            this%idbfullcell = 1
            write(this%iout, fmtopt) 'HEAD-BASED DELAY INTERBEDS WILL BE ' //  &
                                     'SIMULATED USING A FULL-CELL FORMULATION'
          !
          ! -- right now these are options that are only available in the
          !    development version and are not included in the documentation.
          !    These options are only available when IDEVELOPMODE in
          !    constants module is set to 1
          
          !
          ! default case
          case default
            write(errmsg,'(4x,a,3(1x,a))') '****ERROR. UNKNOWN ',               &
                                           trim(adjustl(this%name)),                &
                                           'OPTION: ', trim(keyword)
            call store_error(errmsg)
        end select
      end do
      write(this%iout,'(1x,a)') 'END OF ' // trim(adjustl(this%name)) // ' OPTIONS'
    end if
    !
    ! -- process itime_weight, if effective stress formulation
    if (this%igeocalc /= 0) then
      if (time_weight /= DNODATA) then
        if (time_weight == DZERO) then
          this%time_alpha = time_weight
        else if (time_weight == DONE) then
          this%time_alpha = time_weight
        else
          this%time_alpha = DNODATA
          errmsg = 'TIME_WEIGHT MUST BE 0 or 1.'
          call store_error(errmsg)
        end if
        write(this%iout, fmtoptr) 'TIME_WEIGHT =', time_weight
        write(this%iout, fmtoptr) 'TIME_ALPHA =', this%time_alpha
      end if
    end if
    !
    ! -- recalculate BRG if necessary
    if (ibrg /= 0) then
      this%brg = this%gammaw * this%beta
    end if
    write(this%iout, fmtoptr) 'GAMMAW * BETA =', this%brg
    !
    ! -- terminate if errors encountered in reach block
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine read_options

  subroutine csub_allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- Allocate Package Members
! Subroutine: (1) allocate
!             (2) initialize
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    implicit none
    class(GwfCsubType),   intent(inout) :: this
    ! -- local variables
    character(len=LENORIGIN) :: stoname
    integer(I4B) :: j
    integer(I4B) :: n
    integer(I4B) :: iblen
    integer(I4B) :: naux

    ! -- grid based data
    if (this%ioutcomp == 0 .and. this%ioutcompi == 0 .and.                      &
        this%ioutcompe == 0 .and. this%ioutcomps == 0 .and.                     &
        this%ioutzdisp == 0) then
      call mem_allocate(this%buff, 1, 'BUFF', trim(this%origin))
    else
      call mem_allocate(this%buff, this%dis%nodes, 'BUFF', trim(this%origin))
    end if
    if (this%ioutcomp == 0 .and. this%ioutzdisp == 0) then
      call mem_allocate(this%buffusr, 1, 'buffusr', trim(this%origin))
    else
      call mem_allocate(this%buffusr, this%dis%nodesuser, 'buffusr',            &
                        trim(this%origin))
    end if
    if (this%igeocalc == 0) then
      call mem_allocate(this%sgm, 1, 'sgm', trim(this%origin))
      call mem_allocate(this%sgs, 1, 'sgs', trim(this%origin))
      !call mem_allocate(this%sk_znode, 1, 'sk_znode', trim(this%origin))
    else
      call mem_allocate(this%sgm, this%dis%nodes, 'sgm', trim(this%origin))
      call mem_allocate(this%sgs, this%dis%nodes, 'sgs', trim(this%origin))
      !call mem_allocate(this%sk_znode, this%dis%nodes, 'sk_znode', trim(this%origin))
    end if
    call mem_allocate(this%ske_cr, this%dis%nodes, 'ske_cr', trim(this%origin))
    call mem_allocate(this%sk_theta, this%dis%nodes, 'sk_theta', trim(this%origin))
    call mem_allocate(this%sk_thick, this%dis%nodes, 'sk_thick', trim(this%origin))
    if (this%iupdatematprop == 0) then
      call mem_setptr(this%sk_theta0, 'sk_theta', trim(this%origin))
      call mem_setptr(this%sk_thick0, 'sk_thick', trim(this%origin))
    else
      call mem_allocate(this%sk_theta0, this%dis%nodes, 'sk_theta0', trim(this%origin))
      call mem_allocate(this%sk_thick0, this%dis%nodes, 'sk_thick0', trim(this%origin))
    end if
    call mem_allocate(this%sk_es, this%dis%nodes, 'sk_es', trim(this%origin))
    call mem_allocate(this%sk_es0, this%dis%nodes, 'sk_es0', trim(this%origin))
    call mem_allocate(this%sk_comp, this%dis%nodes, 'sk_comp', trim(this%origin))
    call mem_allocate(this%sk_tcomp, this%dis%nodes, 'sk_tcomp', trim(this%origin))
    call mem_allocate(this%sk_stor, this%dis%nodes, 'sk_stor', trim(this%origin))
    call mem_allocate(this%sk_wcstor, this%dis%nodes, 'sk_wcstor', trim(this%origin))
    call mem_allocate(this%sk_ske, this%dis%nodes, 'sk_ske', trim(this%origin))
    call mem_allocate(this%sk_sk, this%dis%nodes, 'sk_sk', trim(this%origin))
    call mem_allocate(this%sk_thickini, this%dis%nodes, 'sk_thickini', trim(this%origin))
    if (this%igeostressoff == 1) then
      call mem_allocate(this%sig0, this%dis%nodes, 'sig0', trim(this%origin))
    else
      call mem_allocate(this%sig0, 1, 'sig0', trim(this%origin))
    end if
    ! -- interbed data
    iblen = 1
    if (this%ninterbeds > 0) then
      iblen = this%ninterbeds
    end if
    naux = 1
    if (this%naux > 0) then
      naux = this%naux
    end if
    call mem_allocate(this%auxvar, naux, iblen, 'AUXVAR', this%origin)
    do n = 1, iblen
      do j = 1, naux
        this%auxvar(j, n) = DZERO
      end do
    end do
    call mem_allocate(this%unodelist, iblen, 'unodelist', trim(this%origin))
    call mem_allocate(this%nodelist, iblen, 'nodelist', trim(this%origin))
    call mem_allocate(this%sk_gs, this%dis%nodes, 'SK_GS', trim(this%origin))
    call mem_allocate(this%pcs, iblen, 'pcs', trim(this%origin))
    call mem_allocate(this%thick, iblen, 'thick', trim(this%origin))
    call mem_allocate(this%theta, iblen, 'theta', trim(this%origin))
    if (this%iupdatematprop == 0) then
      call mem_setptr(this%theta0, 'theta', trim(this%origin))
      call mem_setptr(this%thick0, 'thick', trim(this%origin))
    else
      call mem_allocate(this%theta0, iblen, 'theta0', trim(this%origin))
      call mem_allocate(this%thick0, iblen, 'thick0', trim(this%origin))
    end if
    call mem_allocate(this%rnb, iblen, 'rnb', trim(this%origin))
    call mem_allocate(this%kv, iblen, 'kv', trim(this%origin))
    call mem_allocate(this%h0, iblen, 'h0', trim(this%origin))
    call mem_allocate(this%ci, iblen, 'ci', trim(this%origin))
    call mem_allocate(this%rci, iblen, 'rci', trim(this%origin))
    call mem_allocate(this%idelay, iblen, 'idelay', trim(this%origin))
    call mem_allocate(this%comp, iblen, 'comp', trim(this%origin))
    call mem_allocate(this%tcomp, iblen, 'tcomp', trim(this%origin))
    call mem_allocate(this%tcompi, iblen, 'tcompi', trim(this%origin))
    call mem_allocate(this%tcompe, iblen, 'tcompe', trim(this%origin))
    call mem_allocate(this%storagee, iblen, 'storagee', trim(this%origin))
    call mem_allocate(this%storagei, iblen, 'storagei', trim(this%origin))
    call mem_allocate(this%ske, iblen, 'ske', trim(this%origin))
    call mem_allocate(this%sk, iblen, 'sk', trim(this%origin))
    call mem_allocate(this%thickini, iblen, 'thickini', trim(this%origin))
    !
    ! -- delay bed storage
    call mem_allocate(this%dbdz, 0, 'dbdz', trim(this%origin))
    call mem_allocate(this%dbz, 0, 0, 'dbz', trim(this%origin))
    call mem_allocate(this%dbh, 0, 0, 'dbh', trim(this%origin))
    call mem_allocate(this%dbh0, 0, 0, 'dbh0', trim(this%origin))
    call mem_allocate(this%dbtheta, 0, 0, 'dbtheta', trim(this%origin))
    call mem_allocate(this%dbtheta0, 0, 0, 'dbtheta0', trim(this%origin))
    call mem_allocate(this%dbgeo, 0, 0, 'dbgeo', trim(this%origin))
    call mem_allocate(this%dbgeo0, 0, 0, 'dbgeo0', trim(this%origin))
    call mem_allocate(this%dbes, 0, 0, 'dbes', trim(this%origin))
    call mem_allocate(this%dbes0, 0, 0, 'dbes0', trim(this%origin))
    call mem_allocate(this%dbpcs, 0, 0, 'dbpcs', trim(this%origin))
    !
    ! -- delay interbed solution arrays
    call mem_allocate(this%dbal, 0, 'dbal', trim(this%origin))
    call mem_allocate(this%dbad, 0, 'dbad', trim(this%origin))
    call mem_allocate(this%dbau, 0, 'dbau', trim(this%origin))
    call mem_allocate(this%dbrhs, 0, 'dbrhs', trim(this%origin))
    call mem_allocate(this%dbdh, 0, 'dbdh', trim(this%origin))
    call mem_allocate(this%dbaw, 0, 'dbaw', trim(this%origin))
    !
    ! -- allocate boundname
    allocate(this%boundname(this%ninterbeds))
    !
    ! -- set pointers to gwf variables
    call mem_setptr(this%gwfiss, 'ISS', trim(this%name_model))
    !
    ! -- set pointers to variables in the storage package
    stoname = trim(this%name_model) // ' ' // trim(this%stoname)
    call mem_setptr(this%stoiconv, 'ICONVERT', trim(stoname))
    call mem_setptr(this%stosc1, 'SC1', trim(stoname))
    !
    ! -- initialize variables that are not specified by user
    do n = 1, this%dis%nodes
      this%sk_gs(n) = DZERO
      this%sk_es(n) = DZERO
      this%sk_comp(n) = DZERO
      this%sk_tcomp(n) = DZERO
      this%sk_wcstor(n) = DZERO
      if (this%igeostressoff == 1) then
        this%sig0(n) = DZERO
      end if
    end do
    do n = 1, this%ninterbeds
      this%theta(n) = DZERO
      this%tcomp(n) = DZERO
      this%tcompi(n) = DZERO
      this%tcompe(n) = DZERO
    end do
    !
    ! -- return
    return

  end subroutine csub_allocate_arrays

   subroutine csub_da(this)
! ******************************************************************************
! csub_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    implicit none
    ! -- dummy
    class(GwfCsubType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays
    if(this%inunit > 0) then
      call mem_deallocate(this%unodelist)
      call mem_deallocate(this%nodelist)
      call mem_deallocate(this%idelay)
      !
      ! -- grid-based storage data
      call mem_deallocate(this%buff)
      call mem_deallocate(this%buffusr)
      call mem_deallocate(this%sgm)
      call mem_deallocate(this%sgs)
      call mem_deallocate(this%ske_cr)
      if (this%iupdatematprop == 0) then
        nullify(this%sk_theta0)
        nullify(this%sk_thick0)
      else
        call mem_deallocate(this%sk_theta0)
        call mem_deallocate(this%sk_thick0)
      end if
      call mem_deallocate(this%sk_theta)
      call mem_deallocate(this%sk_thick)
      call mem_deallocate(this%sig0)
      call mem_deallocate(this%sk_gs)
      call mem_deallocate(this%sk_es)
      call mem_deallocate(this%sk_es0)
      call mem_deallocate(this%sk_comp)
      call mem_deallocate(this%sk_tcomp)
      call mem_deallocate(this%sk_stor)
      call mem_deallocate(this%sk_wcstor)
      call mem_deallocate(this%sk_ske)
      call mem_deallocate(this%sk_sk)
      call mem_deallocate(this%sk_thickini)
      !
      ! -- interbed storage
      deallocate(this%boundname)
      deallocate(this%auxname)
      call mem_deallocate(this%auxvar)
      call mem_deallocate(this%ci)
      call mem_deallocate(this%rci)
      call mem_deallocate(this%pcs)
      if (this%iupdatematprop == 0) then
        nullify(this%theta0)
        nullify(this%thick0)
      else
        call mem_deallocate(this%theta0)
        call mem_deallocate(this%thick0)
      end if
      call mem_deallocate(this%thick)
      call mem_deallocate(this%theta)
      call mem_deallocate(this%rnb)
      call mem_deallocate(this%kv)
      call mem_deallocate(this%h0)
      call mem_deallocate(this%comp)
      call mem_deallocate(this%tcomp)
      call mem_deallocate(this%tcompi)
      call mem_deallocate(this%tcompe)
      call mem_deallocate(this%storagee)
      call mem_deallocate(this%storagei)
      call mem_deallocate(this%ske)
      call mem_deallocate(this%sk)
      call mem_deallocate(this%thickini)
      !
      ! -- delay bed storage
      call mem_deallocate(this%dbdz)
      call mem_deallocate(this%dbz)
      call mem_deallocate(this%dbh)
      call mem_deallocate(this%dbh0)
      call mem_deallocate(this%dbtheta)
      call mem_deallocate(this%dbtheta0)
      call mem_deallocate(this%dbgeo)
      call mem_deallocate(this%dbgeo0)
      call mem_deallocate(this%dbes)
      call mem_deallocate(this%dbes0)
      call mem_deallocate(this%dbpcs)
      !
      ! -- delay interbed solution arrays
      call mem_deallocate(this%dbal)
      call mem_deallocate(this%dbad)
      call mem_deallocate(this%dbau)
      call mem_deallocate(this%dbrhs)
      call mem_deallocate(this%dbdh)
      call mem_deallocate(this%dbaw)
      !
      ! -- pointers to gwf variables
      nullify(this%gwfiss)
      !
      ! -- pointers to storage variables
      nullify(this%stoiconv)
      nullify(this%stosc1)
    end if
    !
    ! -- deallocate scalars
    call mem_deallocate(this%istounit)
    call mem_deallocate(this%inobspkg)
    call mem_deallocate(this%ninterbeds)
    call mem_deallocate(this%ndelaycells)
    call mem_deallocate(this%ndelaybeds)
    call mem_deallocate(this%initialized)
    call mem_deallocate(this%ibedstressoff)
    call mem_deallocate(this%ispecified_pcs)
    call mem_deallocate(this%ispecified_dbh)
    call mem_deallocate(this%igeostressoff)
    call mem_deallocate(this%inamedbound)
    call mem_deallocate(this%naux)
    call mem_deallocate(this%istoragec)
    call mem_deallocate(this%istrainib)
    call mem_deallocate(this%istrainsk)
    call mem_deallocate(this%ioutcomp)
    call mem_deallocate(this%ioutcompi)
    call mem_deallocate(this%ioutcompe)
    call mem_deallocate(this%ioutcomps)
    call mem_deallocate(this%ioutzdisp)
    call mem_deallocate(this%iupdatematprop)
    call mem_deallocate(this%idbfullcell)
    call mem_deallocate(this%idbhalfcell)
    call mem_deallocate(this%time_alpha)
    call mem_deallocate(this%gammaw)
    call mem_deallocate(this%beta)
    call mem_deallocate(this%brg)
    call mem_deallocate(this%dbfact)
    call mem_deallocate(this%dbfacti)
    call mem_deallocate(this%satomega)
    call mem_deallocate(this%icellf)
    call mem_deallocate(this%gwfiss0)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
   end subroutine csub_da

   subroutine csub_read_dimensions(this)
! ******************************************************************************
! pak1read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH, LENBOUNDNAME
    use KindModule, only: I4B
!    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(GwfCsubType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LENBOUNDNAME) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- initialize dimensions to -1
    this%ninterbeds = -1
    !
    ! -- get dimensions block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse dimensions block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%name))// &
        ' DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NIBCCELLS')
            this%ninterbeds = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)')'NIBCCELLS = ', this%ninterbeds
          case default
            write(errmsg,'(4x,a,a)') &
              '****ERROR. UNKNOWN '//trim(this%name)//' DIMENSION: ', &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%name))//' DIMENSIONS'
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
      call ustop()
    end if
    !
    ! -- verify dimensions were set correctly
    if (this%ninterbeds < 0) then
      write(errmsg, '(1x,a)') &
        'ERROR:  nibccells WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.'
      call store_error(errmsg)
    end if
    !
    ! -- stop if errors were encountered in the DIMENSIONS block
    ierr = count_errors()
    if (ierr > 0) then
      call ustop()
    end if

    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- return
    return
  end subroutine csub_read_dimensions

  subroutine csub_ar(this, dis, ibound)
! ******************************************************************************
! csub_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    use ConstantsModule, only: LINELENGTH
    use KindModule, only: I4B
    use InputOutputModule, only: urword, uget_block, u8rdcom, &
                                 uterminate_block
!    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    implicit none
    ! -- dummy
    class(GwfCsubType),intent(inout) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer          :: ibound
    ! -- local
    logical :: isfound, endOfBlock
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: keyword
    character(len=20) :: cellid
    integer(I4B) :: iske
    integer(I4B) :: istheta
    integer(I4B) :: isgm
    integer(I4B) :: isgs
    integer(I4B) :: idelay
    integer(I4B) :: ierr
    integer(I4B) :: lloc
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: ib
    integer(I4B) :: node
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: thick
    real(DP) :: theta
    real(DP) :: v
    ! -- format
    character(len=*), parameter :: fmtcsub =                                    &
      "(1x,/1x,'CSUB -- COMPACTION PACKAGE, VERSION 1, 3/16/2018',             &
      ' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the csub package.
    write(this%iout, fmtcsub) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis     => dis
    this%ibound  => ibound
    !
    ! -- create obs package
    call obs_cr(this%obs, this%inobspkg)
    !
    ! -- Read csub options
    call this%read_options()
    !
    ! - observation data
    call this%obs%obs_ar()
    !
    ! -- read dimensions
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr)
    if (isfound) then
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        call this%parser%GetRemainingLine(line)
        lloc = 1
        select case (keyword)
          case ('NINTERBEDS')
              this%ninterbeds =  this%parser%GetInteger()
              write(this%iout, '(4x,a,1x,i0)') 'NUMBER OF INTERBED SYSTEMS =',  &
                                                this%ninterbeds
          case default
              write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN DIMENSIONS TAG: ',        &
                                       trim(keyword)
              call store_error(errmsg)
        end select
      end do
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
    end if
    !
    ! -- terminate if errors dimensions block data
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if

    ! -- Allocate arrays in 
    call this%csub_allocate_arrays()
    !
    ! -- initialize local variables
    iske = 0
    istheta = 0
    isgm = 0
    isgs = 0
    !
    ! -- read griddata block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    if (isfound) then
        do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        call this%parser%GetRemainingLine(line)
        lloc = 1
        select case (keyword)
        case ('SKE_CR')
          call this%dis%read_grid_array(line, lloc, istart, istop,              &
                                        this%iout, this%parser%iuactive,        &
                                        this%ske_cr, 'SKE_CR')
          iske = 1
        case ('SK_THETA')
          call this%dis%read_grid_array(line, lloc, istart, istop,              &
                                        this%iout, this%parser%iuactive,        &
                                        this%sk_theta, 'SK_THETA')
          istheta = 1
        case ('SGM')
            if (this%igeocalc == 1) then
              call this%dis%read_grid_array(line, lloc, istart, istop,          &
                                            this%iout, this%parser%iuactive,    &
                                            this%sgm, 'SGM')
              isgm = 1
            else
              write(errmsg,'(4x,a,2(1x,a))') 'ERROR. SGM GRIDDATA CANNOT BE ',  &
                                             'SPECIFIED IF USING THE HEAD-',    &
                                             'BASED FORMULATION'
              call store_error(errmsg)
            end if
        case ('SGS')
            if (this%igeocalc == 1) then
              call this%dis%read_grid_array(line, lloc, istart, istop,          &
                                            this%iout, this%parser%iuactive,    &
                                            this%sgs, 'SGS')
              isgs = 1
            else
              write(errmsg,'(4x,a,2(1x,a))') 'ERROR. SGS GRIDDATA CANNOT BE ',  &
                                             'SPECIFIED IF USING THE HEAD-',    &
                                             'BASED FORMULATION'
              call store_error(errmsg)
            end if
        case default
            write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN GRIDDATA TAG: ',            &
                                     trim(keyword)
            call store_error(errmsg)
        end select
      end do
    else
      call store_error('ERROR.  REQUIRED GRIDDATA BLOCK NOT FOUND.')
    end if
    !
    ! -- detemine if sk_ske and sk_theta have been specified
    if (iske == 0) then
      write(errmsg,'(4x,a)') 'ERROR. SK_SKE GRIDDATA MUST BE SPECIFIED'
      call store_error(errmsg)
    end if
    if (istheta == 0) then
      write(errmsg,'(4x,a)') 'ERROR. SK_THETA GRIDDATA MUST BE SPECIFIED'
      call store_error(errmsg)
    end if
    !
    ! -- determine if sgm and sgs have been specified, if effective stress formulation
    if (this%igeocalc > 0) then
      if (isgm == 0) then
        write(errmsg,'(4x,a,2(1x,a))') 'ERROR. SGM GRIDDATA MUST BE SPECIFIED', &
                                       'IF USING THE EFFECTIVE-STRESS',         &
                                       'FORMULATION'
        call store_error(errmsg)
      end if
      if (isgs == 0) then
        write(errmsg,'(4x,a,2(1x,a))') 'ERROR. SGS GRIDDATA MUST BE SPECIFIED', &
                                       'SPECIFIED IF USING THE EFFECTIVE-',     &
                                       'STRESS FORMULATION'
        call store_error(errmsg)
      end if
    end if
    !
    ! -- set idbhalfcell
    if (this%igeocalc == 0) then
      if (this%idbfullcell == 0) then
        ! -- use half cell for head-based delay bed if delay_full_cell option not specified
        this%idbhalfcell = 1
        this%dbfact = DTWO
        this%dbfacti = DHALF
      ! -- use full cell for head-based delay bed if delay_full_cell option specified
      else
        this%idbhalfcell = 0
      end if
    else
      this%idbfullcell = 1
    end if
    !
    ! -- read interbed data
    if (this%ninterbeds > 0) then
      call this%csub_read_packagedata()
    end if
    !
    ! -- calculate the aquifer void ratio and thickness without the interbeds
    do node = 1, this%dis%nodes
      if (this%igeocalc /= 0) then
        theta = this%sk_theta(node)
        if (theta > DONE .or. theta < DZERO) then
          ! add error message
          write(errmsg,'(4x,a,1x,a,g0,a,1x,a,1x,a)') &
                                       'ERROR. AQUIFER POROSITY IS LESS THAN', &
                                       '0 OR GREATER THAN 1 (', theta, ')',    &
                                       'in cell', ''
        end if
      end if
      top = this%dis%top(node)
      bot = this%dis%bot(node)
      this%sk_thick(node) = top - bot
    end do
    !
    ! -- subtract the interbed thickness from aquifer thickness
    do ib = 1, this%ninterbeds
      node = this%nodelist(ib)
      idelay = this%idelay(ib)
      if (idelay == 0) then
        v = this%thick(ib)
      else
        v = this%dbfact * this%rnb(ib) * this%thick(ib)
      end if
      this%sk_thick(node) = this%sk_thick(node) - v
    end do
    !
    ! -- evaluate if any sk_thick values are less than 0
    !    also set initial skeletal thickness (sk_thickini)
    do node = 1, this%dis%nodes
      thick = this%sk_thick(node)
      if (thick < DZERO) then
        call this%dis%noder_to_string(node, cellid)
        write(errmsg,'(4x,a,1x,g0,a,1x,a,1x,a)')                                &
          'ERROR. AQUIFER THICKNESS IS LESS THAN ZERO (',                       &
           thick, ')', 'in cell', trim(adjustl(cellid))
        call store_error(errmsg)
      end if
      this%sk_thickini(node) = thick
    end do
    !
    ! -- evaluate if non-zero specific storage values are specified in the 
    !    STO package
    do node = 1, this%dis%nodes
      if (this%stosc1(node) /= DZERO) then
        write(errmsg,'(4x,a,3(1x,a))')                                          &
          'ERROR. SPECIFIC STORAGE VALUES IN THE STORAGE (STO) PACKAGE MUST',   &
          'BE 0 WHEN USING THE', trim(adjustl(this%name)), 'PACKAGE'
        call store_error(errmsg)
        exit
      end if
    end do    
    !
    ! -- terminate if errors griddata, packagedata blocks, TDIS, or STO data
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine csub_ar


!  subroutine csub_sk_calc_znode(this, nodes, hnew)
!! ******************************************************************************
!! csub_sk_calc_znode -- calculate the z of the gwf node using current (xnew) 
!!                       water levels
!! ******************************************************************************
!!
!!    SPECIFICATIONS:
!! ------------------------------------------------------------------------------
!    implicit none
!    class(GwfCsubType) :: this
!    integer(I4B), intent(in) :: nodes
!    real(DP), dimension(nodes), intent(in) :: hnew
!    ! local
!    integer(I4B) :: n
!    real(DP) :: top
!    real(DP) :: bot
!    real(DP) :: hcell
!
!! ------------------------------------------------------------------------------
!
!    if (this%igeocalc /= 0) then
!      do n = 1, nodes
!        bot = this%dis%bot(n)
!        top = this%dis%top(n)
!        hcell = hnew(n)
!        if (hcell > top .or. hcell < bot) then
!            this%sk_znode(n) = (top + bot) * DHALF
!        else
!            this%sk_znode(n) = (hcell + bot) * DHALF
!        end if
!      end do
!    end if
!    !
!    ! -- return
!    return
!  end subroutine csub_sk_calc_znode


  subroutine csub_sk_calc_stress(this, nodes, hnew)
! ******************************************************************************
! csub_sk_calc_stress -- calculate the geostatic stress for every gwf node 
!                           in the model
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(GwfCsubType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), dimension(nodes), intent(in) :: hnew
    ! -- local
    integer(I4B) :: node
    integer(I4B) :: ii
    integer(I4B) :: m
    integer(I4B) :: iis
    real(DP) :: gs
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: va_scale
    real(DP) :: hcell
    real(DP) :: gs_conn
    real(DP) :: area_node
    real(DP) :: area_conn
    real(DP) :: es
    real(DP) :: hs
    real(DP) :: hwva
    real(DP) :: sadd
    character(len=LINELENGTH) :: errmsg

! ------------------------------------------------------------------------------
    !
    ! -- calculate geostatic stress if necessary
    if (this%igeocalc /= 0) then
      do node = 1, this%dis%nodes
        !
        ! -- calculate geostatic stress for this node
        !    this represents the geostatic stress component 
        !    for the cell
        top = this%dis%top(node)
        bot = this%dis%bot(node)
        hcell = hnew(node)
        gs = DZERO
        if (hcell >= top) then
            gs = (top - bot) * this%sgs(node)
        else if (hcell <= bot) then
            gs = (top - bot) * this%sgm(node)
        else
            gs = ((top - hcell) * this%sgm(node)) +                             &
                 ((hcell - bot) * this%sgs(node))
        end if
        !
        ! -- add user-specified overlying geostatic stress
        sadd = DZERO
        if (this%igeostressoff /= 0) then
          sadd = this%sig0(node)
        end if
        this%sk_gs(node) = gs + sadd
      end do
      !
      ! -- calculate geostatic stress above cell
      do node = 1, this%dis%nodes
        !
        ! -- area of cell 
        area_node = this%dis%get_area(node)
        !
        ! -- geostatic stress of cell
        gs = this%sk_gs(node)
        !
        ! -- Add geostatic stress of overlying cells (ihc=0)
        !    m < node = m is vertically above node
        do ii = this%dis%con%ia(node) + 1, this%dis%con%ia(node + 1) - 1
          !
          ! -- Set the m cell number
          m = this%dis%con%ja(ii)
          iis = this%dis%con%jas(ii)
          !
          ! -- vertical connection
          if (this%dis%con%ihc(iis) == 0) then
            !
            ! -- node has an overlying cell
            if (m < node) then
              !
              ! -- dis and disv discretization
              if (this%dis%ndim /= 1) then
                gs = gs + this%sk_gs(m)
              !
              ! -- disu discretization
              !    *** this needs to be checked ***
              else
                area_conn = this%dis%get_area(m)
                hwva = this%dis%con%hwva(iis)
                va_scale = this%dis%con%hwva(iis) / this%dis%get_area(m)
                gs_conn = this%sk_gs(m)
                gs = gs + (gs_conn * va_scale)
              end if

            end if
          end if
        end do
        !
        ! -- geostatic stress for cell with geostatic stress 
        !    of overlying cells
        this%sk_gs(node) = gs
      end do
      !
      ! -- calculate effective stress for a cell
      do node = 1, this%dis%nodes
        bot = this%dis%bot(node)
        hcell = hnew(node)
        hs = DZERO
        if (hcell > bot) then
          hs = hcell - bot
        end if
        es = this%sk_gs(node) - hs
        this%sk_es(node) = es
      end do
   end if
   !
   ! -- return
   return

  end subroutine csub_sk_calc_stress
  
  
  subroutine csub_nodelay_update(this, i)
! ******************************************************************************
! csub_nodelay_update -- Update material properties for no-delay interbeds.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B),intent(in) :: i
    ! locals
    character(len=LINELENGTH) :: errmsg
    real(DP) :: comp
    real(DP) :: thick
    real(DP) :: theta
! ------------------------------------------------------------------------------
!
! -- update thickness and theta
    comp = this%comp(i)
    if (ABS(comp) > DZERO) then
      thick = this%thick0(i)
      theta = this%theta0(i)
      call this%csub_adj_matprop(comp, thick, theta)
      if (thick <= DZERO) then
        write(errmsg,'(4x,2a,1x,i0,1x,a,1x,g0,1x,a)')                           &
          '****ERROR. ADJUSTED THICKNESS FOR NO-DELAY ',                        &
          'INTERBED', i, 'IS <= 0 (', thick, ')'
        call store_error(errmsg)
      end if
      if (theta <= DZERO) then
        write(errmsg,'(4x,2a,1x,i0,1x,a,1x,g0,1x,a)')                           &
          '****ERROR. ADJUSTED THETA FOR NO-DELAY ',                            &
          'INTERBED (', i, ') IS <= 0 (', theta, ')'
        call store_error(errmsg)
      end if
      this%thick(i) = thick
      this%theta(i) = theta
    end if
    !
    ! -- return
    return
  end subroutine csub_nodelay_update

  
  subroutine csub_nodelay_fc(this, ib, hcell, hcellold, rho1, rho2, rhs, argtled)
! ******************************************************************************
! csub_nodelay_fc -- Calculate rho1, rho2, and rhs for no-delay interbeds
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt
    implicit none
    ! -- dummy variables
    class(GwfCsubType) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    real(DP), intent(inout) :: rho1
    real(DP), intent(inout) :: rho2
    real(DP), intent(inout) :: rhs
    real(DP), intent(in), optional :: argtled
    ! -- local variables
    integer(I4B) :: node
    real(DP) :: tled
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: znode
    real(DP) :: znode0
    real(DP) :: snold
    real(DP) :: snnew
    real(DP) :: sto_fac
    real(DP) :: sto_fac0
    real(DP) :: area
    real(DP) :: theta
    real(DP) :: theta0
    real(DP) :: es
    real(DP) :: es0
    real(DP) :: f
    real(DP) :: f0
! ------------------------------------------------------------------------------
    if (present(argtled)) then
      tled = argtled
    else
      tled = DONE / delt
    endif
    node = this%nodelist(ib)
    area = this%dis%get_area(node)
    bot = this%dis%bot(node)
    top = this%dis%top(node)
    ! -- aquifer saturation
    if (this%stoiconv(node) /= 0) then
      snold = sQuadraticSaturation(top, bot, hcellold, this%satomega)
      snnew = sQuadraticSaturation(top, bot, hcell, this%satomega)
    else
      snold = DONE
      snnew = DONE
    end if
    if (this%time_alpha == DZERO) then
      snold = snnew
    end if
    if (this%igeocalc == 0) then
      f = DONE
      f0 = DONE
    else
      znode = this%csub_calc_znode(node, hcell)
      znode0 = this%csub_calc_znode(node, hcellold)
      if (this%time_alpha == DZERO) then
        znode0 = znode
      end if
      es = this%sk_es(node)
      es0 = this%sk_es0(node)
      theta = this%theta(ib)
      theta0 = this%theta0(ib)
      call this%csub_calc_sfacts(node, bot, znode, znode0, theta, theta0,          &
                                 es, es0, f, f0)
    end if
    sto_fac = tled * snnew * this%thick(ib) * f
    sto_fac0 = tled * snold * this%thick0(ib) * f0
    !
    ! -- calculate rho1 and rho2
    rho1 = this%rci(ib) * sto_fac0
    rho2 = this%rci(ib) * sto_fac
    if (this%igeocalc == 0) then
      if (hcell < this%pcs(ib)) then
        rho2 = this%ci(ib) * sto_fac
      end if
      rhs = -(rho2 * this%pcs(ib) + rho1 * (this%sk_es0(node) - this%pcs(ib)))
    else
      if (this%sk_es(node) > this%pcs(ib)) then
          rho2 = this%ci(ib) * sto_fac
      end if
      rhs = -rho2 * (this%sk_gs(node) + bot) + (this%pcs(ib) * (rho2 - rho1)) + &
             (rho1 * this%sk_es0(node))
    end if
    !
    ! -- save ske and sk
    this%ske(ib) = rho1
    this%sk(ib) = rho2
    !
    ! -- return
    return

  end subroutine csub_nodelay_fc


  subroutine csub_nodelay_calc_comp(this, ib, hcell, hcellold, comp, rho1, rho2)
! ******************************************************************************
! csub_nodelay_calc_comp -- Calculate compaction, rho1, and rho2 for no-delay
!                           interbeds
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy variables
    class(GwfCsubType) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    real(DP), intent(inout) :: comp
    real(DP), intent(inout) :: rho1
    real(DP), intent(inout) :: rho2
    ! -- local variables
    integer(I4B) :: node
    real(DP) :: es
    real(DP) :: es0
    real(DP) :: pcs
    real(DP) :: tled
    real(DP) :: rhs
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    node = this%nodelist(ib)
    tled = DONE
    es = this%sk_es(node)
    es0 = this%sk_es0(node)
    pcs = this%pcs(ib)
    !
    ! -- calculate no-delay interbed rho1 and rho2
    call this%csub_nodelay_fc(ib, hcell, hcellold, rho1, rho2, rhs, tled)
    !
    ! -- calculate no-delay interbed compaction
    if (this%igeocalc == 0) then
      comp = rho2 * (pcs - hcell) + rho1 * (es0 - pcs)
    else
      comp = -pcs * (rho2 - rho1) - (rho1 * es0) + (rho2 * es)
    end if
    !
    ! -- return
    return

  end subroutine csub_nodelay_calc_comp

  
  subroutine csub_rp(this)
! ******************************************************************************
! csub_rp -- Read and Prepare
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use TdisModule, only: kper, nper
!    use SimModule, only: store_error, ustop, count_errors
    implicit none
    ! -- dummy
    class(GwfCsubType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, keyword
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*),parameter :: fmtblkerr =                                  &
      "('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*),parameter :: fmtlsp =                                     &
      "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
! ------------------------------------------------------------------------------
    !
    if(this%inunit == 0) return
    !
    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
    if (this%ionper < kper) then
      !
      ! -- get period block
      ! When reading a list, OPEN/CLOSE is handled by list reader,
      ! so supportOpenClose needs to be false in call the GetBlock.
      ! When reading as arrays, set supportOpenClose as desired.
      call this%parser%GetBlock('PERIOD', isfound, ierr)
      if(isfound) then
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
        end if
      endif
    end if
    !
    ! -- Read data if ionper == kper
    if(this%ionper == kper) then
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        call this%parser%GetRemainingLine(line)
        lloc = 1
        !
        ! -- Parse the keywords
        select case (keyword)
        case ('SIG0')
          if (this%igeostressoff == 1) then
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                          this%parser%iuactive, this%sig0, 'SIG0')
          else
            write (errmsg, '(a)') &
              '****ERROR. SIG0 SPECIFIED BUT GEO_STRESS_OFFSET NOT SPECIFIED IN OPTIONS BLOCK'
            call store_error(trim(errmsg))
            call this%parser%StoreErrorUnit()
          end if
        case default
          call store_error('****ERROR. LOOKING FOR VALID VARIABLE NAME.  FOUND: ')
          call store_error(trim(line))
          call this%parser%StoreErrorUnit()
        end select
      end do
    !
    else
      write(this%iout,fmtlsp) trim(this%filtyp)
    endif
    !
    ! -- terminate if errors encountered in reach block
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- read observations
    call this%csub_rp_obs()
    !
    ! -- return
    return
  end subroutine csub_rp

  subroutine csub_ad(this, nodes, hnew)
! ******************************************************************************
! csub_ad -- Advance csub data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: nper, kper, kstp
    ! -- dummy
    class(GwfCsubType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), dimension(nodes), intent(in) :: hnew
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: ib
    integer(I4B) :: n
    integer(I4B) :: idelay
    integer(I4B) :: node
    real(DP) :: h
    real(DP) :: es
    real(DP) :: pcs
! ------------------------------------------------------------------------------
    !
    ! -- evaluate if steady-state stress periods are specified for more 
    !    than the first and last stress period if interbeds are simulated
    if (this%ninterbeds > 0) then
      if (kper > 1 .and. kper < nper) then
        if (this%gwfiss /= 0) then
          write(errmsg, '(1x,a,i0,a,1x,a,1x,a,1x,i0,1x,a)')                     &
            'ERROR:  Only the first and last (', nper, ')',                     &
            'stress period can be steady if interbeds are simulated.',          &
            'Stress period', kper, 'has been defined to be steady state.'
          call store_error(errmsg)
          call ustop()
        end if
      end if
    end if
!    !
!    ! -- Advance the time series manager
!    call this%TsManager%ad()
    !
    ! -- set initial conditions
    if (this%initialized == 0) then
      if (this%gwfiss == 0) then
        call this%csub_interbed_set_initial(nodes, hnew)
      end if
    end if
    !
    ! -- update state variables
    !
    ! -- coarse-grained materials
    do node = 1, nodes
      this%sk_comp(node) = DZERO
      if (this%igeocalc == 0) then
        this%sk_es0(node)= hnew(node)
      else
        this%sk_es0(node) = this%sk_es(node)
      end if
      if (this%iupdatematprop /= 0) then
        this%sk_theta0(node) = this%sk_theta(node)
        this%sk_thick0(node) = this%sk_thick(node)
      end if
    end do
    !
    ! -- interbeds
    do ib = 1, this%ninterbeds
      idelay = this%idelay(ib)
      !
      ! -- no delay interbeds
      if (idelay == 0) then
        this%comp(ib) = DZERO
        node = this%nodelist(ib)
        if (this%iupdatematprop /= 0) then
          this%theta0(ib) = this%theta(ib)
          this%thick0(ib) = this%thick(ib)
        end if
        if (kper == 1 .and. kstp == 1) then
          if (this%igeocalc == 0) then
            h = hnew(node)
            pcs = this%pcs(ib)
            if (pcs > h) then
              this%pcs(ib) = h
            end if
          end if
        end if
        if (this%initialized /= 0) then
          es = this%sk_es(node)
          pcs = this%pcs(ib)
          if (this%igeocalc == 0) then
            h = hnew(node)
            if (h < pcs) then
              this%pcs(ib) = h
            end if
          else
            if (es > pcs) then
              this%pcs(ib) = es
            end if
          end if
        end if
      !
      ! -- delay interbeds
      else
        !
        ! -- update state if previous period was steady state
        if (kper > 1) then
          if (this%gwfiss0 /= 0) then
            node = this%nodelist(ib)
            h = hnew(node)
            do n = 1, this%ndelaycells
              if (this%igeocalc == 0) then
                this%dbh(n, idelay) = h
              else
                this%dbh(n, idelay) = h
              end if
            end do
          end if
        end if
        !
        ! -- update preconsolidation stress, stresses, and head
        do n = 1, this%ndelaycells
          ! update preconsolidation stress
          if (this%initialized /= 0) then
            if (this%igeocalc == 0) then
              if (this%dbh(n, idelay) < this%dbpcs(n, idelay)) then
                this%dbpcs(n, idelay) = this%dbh(n, idelay)
              end if
              this%dbes(n, idelay) = this%dbh(n, idelay)
            else
              if (this%dbes(n, idelay) > this%dbpcs(n, idelay)) then
                this%dbpcs(n, idelay) = this%dbes(n, idelay)
              end if
            end if
          end if
          this%dbh0(n, idelay) = this%dbh(n, idelay)
          this%dbgeo0(n, idelay) = this%dbgeo(n, idelay)
          this%dbes0(n, idelay) = this%dbes(n, idelay)
        end do
      end if
    end do
    !
    ! -- set gwfiss0
    this%gwfiss0 = this%gwfiss
    !
    ! -- For each observation, push simulated value and corresponding
    !    simulation time from "current" to "preceding" and reset
    !    "current" value.
    call this%obs%obs_ad()
    !
    ! -- return
    return
  end subroutine csub_ad

  subroutine csub_interbed_set_initial(this, nodes, hnew)
! ******************************************************************************
! csub_interbed_set_initial -- Set initial state for interbeds
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), dimension(nodes), intent(in) :: hnew
    integer(I4B) :: ib
    integer(I4B) :: node
    integer(I4B) :: n
    integer(I4B) :: idelay
    real(DP) :: pcs0
    real(DP) :: pcs
    real(DP) :: fact
    real(DP) :: bot
    real(DP) :: void
    real(DP) :: znode
    real(DP) :: hcell
    real(DP) :: dzhalf
    real(DP) :: ztop
    real(DP) :: zbot
    real(DP) :: sadd
    real(DP) :: dbpcs
! ------------------------------------------------------------------------------
    !
    ! -- update geostatic load calculation
    !call this%csub_sk_calc_znode(nodes, hnew)
    call this%csub_sk_calc_stress(nodes, hnew)
    !
    ! -- coarse-grained materials
    do node = 1, nodes
      ! scale cr and cc
      bot = this%dis%bot(node)
      if (this%istoragec == 1) then
        if (this%igeocalc == 0) then
          fact = DONE
        else
          void = this%csub_calc_void(this%sk_theta(node))
          znode = this%csub_calc_znode(node, hnew(node))
          fact = this%sk_es(node) - (znode - bot) * (this%sgs(node) - DONE)
          fact = fact * (DONE + void)
        end if
      else
          fact = dlog10es
      end if
      this%ske_cr(node) = this%ske_cr(node) * fact
      ! -- initialize previous initial stress
      this%sk_es0(node) = this%sk_es(node)
    end do
    !
    ! -- check that aquifer head is greater than or equal to the
    !    top of each delay interbed
    do ib = 1, this%ninterbeds
      idelay = this%idelay(ib)
      if (idelay == 0) then
        cycle
      end if
      node = this%nodelist(ib)
      hcell = hnew(node)
      call this%csub_delay_chk(ib, hcell)
    end do
    !
    ! -- terminate if the aquifer head is below the top of delay interbeds
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- interbeds
    do ib = 1, this%ninterbeds
      idelay = this%idelay(ib)
      node = this%nodelist(ib)
      bot = this%dis%bot(node)
      hcell = hnew(node)
      pcs = this%pcs(ib)
      pcs0 = pcs
      if (this%igeocalc == 0) then
        !if (this%ibedstressoff == 1) then
        !  pcs = this%sk_es(node) + pcs0
        !else
        !  if (pcs > this%sk_es(node)) then
        !    pcs = this%sk_es(node)
        !  end if
        !end if
        if (this%ispecified_pcs == 0) then
          pcs = this%sk_es(node) + pcs0
        else
          if (pcs > this%sk_es(node)) then
            pcs = this%sk_es(node)
          end if
        end if
      else
        !
        ! -- transfer initial preconsolidation stress (and apply offset if needed)
        !if (this%ibedstressoff == 1) then
        !    pcs = this%sk_es(node) + pcs0
        !else
        !  if (pcs < this%sk_es(node)) then
        !    pcs = this%sk_es(node)
        !  end if
        !end if
        if (this%ispecified_pcs == 0) then
            pcs = this%sk_es(node) + pcs0
        else
          if (pcs < this%sk_es(node)) then
            pcs = this%sk_es(node)
          end if
        end if
      end if
      this%pcs(ib) = pcs
      !
      ! -- delay bed          
      if (idelay /= 0) then
        dzhalf = DHALF * this%dbdz(idelay)
        !
        ! -- fill delay bed head with aquifer head or offset from aquifer head
        !    heads need to be filled first since used to calculate 
        !    the effective stress for each delay bed
        do n = 1, this%ndelaycells
          !if (this%ibedstressoff == 1) then
          !  this%dbh(n, idelay) = hcell + this%dbh(n, idelay)
          !else
          !  this%dbh(n, idelay) = hcell
          !end if
          if (this%ispecified_dbh == 0) then
            this%dbh(n, idelay) = hcell + this%dbh(n, idelay)
          else
            this%dbh(n, idelay) = hcell
          end if
          this%dbh0(n, idelay) = this%dbh(n, idelay)
        end do            
        !
        ! -- fill delay bed effective stress
        call this%csub_delay_calc_stress(ib, hcell)
        !
        ! -- fill delay bed pcs          
        pcs = this%pcs(ib)
        do n = 1, this%ndelaycells
          if (this%igeocalc == 0) then
            this%dbpcs(n, idelay) = pcs
          else
            zbot = this%dbz(n, idelay) - dzhalf
            sadd = this%sgs(node) * (zbot - bot)
            dbpcs = pcs - (zbot - bot) * (this%sgs(node) - DONE)
            !if (this%dbes(n, idelay) > dbpcs) then
            !  dbpcs = this%dbes(n, idelay)
            !end if
            this%dbpcs(n, idelay) = dbpcs
          end if
          this%dbes0(n, idelay) = this%dbes(n, idelay)
        end do 
      end if
      !    
      ! scale cr and cc
      if (this%istoragec == 1) then
        if (this%igeocalc == 0) then
          fact = DONE
        else
          void = this%csub_calc_void(this%theta(ib))
          if (idelay == 0) then
            ztop = hcell
          else
            ztop = this%dbz(1, idelay) + dzhalf
          end if
          znode = this%csub_calc_znode(node, ztop)
          fact = this%sk_es(node) - (znode - bot) * (this%sgs(node) - DONE)
          fact = fact * (DONE + void)
        end if
      else
          fact = dlog10es
      end if
      this%ci(ib) = this%ci(ib) * fact
      this%rci(ib) = this%rci(ib) * fact
    end do
    this%initialized = 1
    !
    ! -- return
    return
  end subroutine csub_interbed_set_initial

  subroutine csub_fc(this, kiter, nodes, hold, hnew, nja, njasln, amat, &
                     idxglo, rhs)
! ******************************************************************************
! csub_fc -- Fill the solution amat and rhs with storage contribution terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt
    ! -- dummy
    class(GwfCsubType) :: this
    integer(I4B),intent(in) :: kiter
    integer(I4B),intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: hold
    real(DP), intent(in), dimension(nodes) :: hnew
    integer(I4B),intent(in) :: nja
    integer(I4B),intent(in) :: njasln
    real(DP), dimension(njasln),intent(inout) :: amat
    integer(I4B), intent(in),dimension(nja) :: idxglo
    real(DP),intent(inout),dimension(nodes) :: rhs
    ! -- local
    integer(I4B) :: ib
    integer(I4B) :: node
    integer(I4B) :: idiag
    integer(I4B) :: idelay
    real(DP) :: tled
    real(DP) :: area
    real(DP) :: hcell
    real(DP) :: hcof
    real(DP) :: rhsterm
    real(DP) :: comp
! ------------------------------------------------------------------------------
    !
    ! -- update geostatic load calculation
    call this%csub_sk_calc_stress(nodes, hnew)
    !
    ! -- formulate csub terms
    if (this%gwfiss == 0) then
      tled = DONE / delt
      !
      ! -- coarse-grained skeletal storage
      do node = 1, this%dis%nodes
        idiag = this%dis%con%ia(node)
        area = this%dis%get_area(node)
        !
        ! -- skip inactive cells
        if (this%ibound(node) < 1) cycle
        !
        ! -- update skeletal material properties
        if (this%iupdatematprop /= 0) then
          if (this%time_alpha > DZERO) then
            !
            ! -- calculate compaction
            call this%csub_sk_calc_comp(node, hnew(node), hold(node), comp)
            this%sk_comp(node) = comp
            !
            ! -- update skeletal thickness and void ratio
            call this%csub_sk_update(node)
          end if
        end if
        !
        ! -- calculate coarse-grained skeletal storage terms
        call this%csub_sk_fc(node, tled, area, hnew(node), hold(node),          &
                             hcof, rhsterm)
        !
        ! -- add skeletal storage terms to amat and rhs for skeletal storage
        amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
        rhs(node) = rhs(node) + rhsterm
        !
        ! -- calculate coarse-grained skeletal water compressibility 
        !    storage terms
        call this%csub_sk_wcomp_fc(node, tled, area, hnew(node), hold(node),    &
                                   hcof, rhsterm)
        !
        ! -- add water compression storage terms to amat and rhs for 
        !   skeletal storage
        amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
        rhs(node) = rhs(node) + rhsterm
      end do
      !
      ! -- interbed storage
      if (this%ninterbeds /= 0) then
        !
        ! -- check that aquifer head is greater than or equal to the
        !    top of each delay interbed
        do ib = 1, this%ninterbeds
          idelay = this%idelay(ib)
          if (idelay == 0) then
            cycle
          end if
          node = this%nodelist(ib)
          hcell = hnew(node)
          call this%csub_delay_chk(ib, hcell)
        end do
        !
        ! -- terminate if the aquifer head is below the top of delay interbeds
        if (count_errors() > 0) then
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
        !
        ! -- calculate the contribution of interbeds to the 
        !    groundwater flow equation
        do ib = 1, this%ninterbeds
          node = this%nodelist(ib)
          idiag = this%dis%con%ia(node)
          area = this%dis%get_area(node)
          call this%csub_interbed_fc(ib, node, area, hnew(node), hold(node),    &
                                     hcof, rhsterm)
          amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
          rhs(node) = rhs(node) + rhsterm
          !
          ! -- calculate interbed water compressibility terms
          call this%csub_interbed_wcomp_fc(ib, node, tled, area,                &
                                           hnew(node), hold(node),              &
                                           hcof, rhsterm)
          !
          ! -- add water compression storage terms to amat and rhs for interbed
          amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
          rhs(node) = rhs(node) + rhsterm
        end do
      end if
    end if    
    !
    ! -- terminate if errors encountered when updating material properties
    if (this%iupdatematprop /= 0) then
      if (this%time_alpha > DZERO) then
        if (count_errors() > 0) then
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
      end if
    end if
    !
    ! -- return
    return
  end subroutine csub_fc

  subroutine csub_fn(this, kiter, nodes, hold, hnew, nja, njasln, amat, &
                     idxglo, rhs)
! ******************************************************************************
! sto_fn -- Fill the solution amat and rhs with storage contribution newton
!               term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt
    ! -- dummy
    class(GwfCsubType) :: this
    integer(I4B),intent(in) :: kiter
    integer(I4B),intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: hold
    real(DP), intent(in), dimension(nodes) :: hnew
    integer(I4B),intent(in) :: nja
    integer(I4B),intent(in) :: njasln
    real(DP), dimension(njasln),intent(inout) :: amat
    integer(I4B), intent(in),dimension(nja) :: idxglo
    real(DP),intent(inout),dimension(nodes) :: rhs
    ! -- local
    integer(I4B) :: node
    integer(I4B) :: idiag
    real(DP) :: tled
    real(DP) :: area
    real(DP) :: hcof
    real(DP) :: rhsterm
! ------------------------------------------------------------------------------
    !
    ! -- formulate csub terms
    if (this%gwfiss == 0) then
      tled = DONE / delt
      !
      ! -- coarse-grained skeletal storage
      do node = 1, this%dis%nodes
        idiag = this%dis%con%ia(node)
        area = this%dis%get_area(node)
        !
        ! -- skip inactive cells
        if (this%ibound(node) < 1) cycle
        !
        ! -- skip non-convertible cells
        if (this%stoiconv(node) == 0) cycle
        !
        ! -- calculate coarse-grained skeletal storage newton terms
        call this%csub_sk_fn(node, tled, area, hnew(node), hcof, rhsterm)
        !
        ! -- add skeletal storage newton terms to amat and rhs for 
        !   skeletal storage
        amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
        rhs(node) = rhs(node) + rhsterm
        !
        ! -- calculate coarse-grained skeletal water compressibility storage 
        !    newton terms
        call this%csub_sk_wcomp_fn(node, tled, area, hnew(node), hcof, rhsterm)
        !
        ! -- add water compression storage newton terms to amat and rhs for 
        !    skeletal storage
        amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
        rhs(node) = rhs(node) + rhsterm
      end do
    end if    
    !
    ! -- return
    return
  end subroutine csub_fn
  
  subroutine csub_sk_fc(this, node, tled, area, hcell, hcellold, hcof, rhs)
! ******************************************************************************
! csub_sk_fc -- Formulate the HCOF and RHS skeletal storage terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(GwfCsubType) :: this
    integer(I4B),intent(in) :: node
    real(DP), intent(in) :: tled
    real(DP), intent(in) :: area
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rhs
    ! locals
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: tthk
    real(DP) :: tthk0
    real(DP) :: snold
    real(DP) :: snnew
    real(DP) :: sske
    real(DP) :: sske0
    real(DP) :: rho1
    real(DP) :: rho2
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- aquifer elevations and thickness
    top = this%dis%top(node)
    bot = this%dis%bot(node)
    tthk = this%sk_thick(node)
    tthk0 = this%sk_thick0(node)
    ! -- aquifer saturation
    if (this%stoiconv(node) /= 0) then
      snold = sQuadraticSaturation(top, bot, hcellold, this%satomega)
      snnew = sQuadraticSaturation(top, bot, hcell, this%satomega)
    else
      snold = DONE
      snnew = DONE
    end if
    if (this%time_alpha == DZERO) then
      snold = snnew
    end if
    !
    ! -- storage coefficients
    call this%csub_sk_calc_sske(node, sske, sske0, hcell, hcellold)
    rho1 = sske0 * area * tthk0 * tled
    rho2 = sske * area * tthk * tled
    !
    ! -- update sk and ske
    this%sk_ske(node) = sske0 * tthk0 * snold
    this%sk_sk(node) = sske * tthk * snnew
    !
    ! -- calculate hcof term
    hcof = -rho2 * snnew
    !
    ! -- calculate rhs term
    if (this%igeocalc == 0) then
      rhs = -rho1 * snold * hcellold
    else
      rhs = rho1 * snold * this%sk_es0(node) -                                  &
            rho2 * snnew * (this%sk_gs(node) + bot) 
    end if
    !
    ! -- return
    return
  end subroutine csub_sk_fc
  
  subroutine csub_sk_fn(this, node, tled, area, hcell, hcof, rhs)
! ******************************************************************************
! csub_sk_fn -- Formulate skeletal storage newton terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(GwfCsubType) :: this
    integer(I4B),intent(in) :: node
    real(DP), intent(in) :: tled
    real(DP), intent(in) :: area
    real(DP), intent(in) :: hcell
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rhs
    ! locals
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: tthk
    real(DP) :: derv
    real(DP) :: sske
    real(DP) :: sske0
    real(DP) :: rho2
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- aquifer elevations and thickness
    top = this%dis%top(node)
    bot = this%dis%bot(node)
    tthk = this%sk_thick(node)
    !
    ! -- calculate saturation derivitive
    derv = sQuadraticSaturationDerivative(top, bot, hcell)    
    !
    ! -- storage coefficients
    call this%csub_sk_calc_sske(node, sske, sske0, hcell, hcell)
    rho2 = sske * area * tthk * tled
    !
    ! -- calculate hcof term
    hcof = -rho2 * derv * hcell
    !
    ! -- calculate rhs term
    rhs = hcof * hcell
    !
    ! -- return
    return
  end subroutine csub_sk_fn

  
  subroutine csub_interbed_fc(this, ib, node, area, hcell, hcellold, hcof, rhs)
! ******************************************************************************
! csub_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip if no ibcs
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(GwfCsubType) :: this
    integer(I4B),intent(in) :: ib
    integer(I4B),intent(in) :: node
    real(DP), intent(in) :: area
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rhs
    ! locals
    real(DP) :: comp
    real(DP) :: rho1
    real(DP) :: rho2
    real(DP) :: f
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- skip inactive and constant head cells
    if (this%ibound(node) > 0) then
      if (this%idelay(ib) == 0) then
        !
        ! -- update material properties
        if (this%iupdatematprop /= 0) then
          if (this%time_alpha > DZERO) then
            !
            ! -- calculate compaction
            call this%csub_nodelay_calc_comp(ib, hcell, hcellold, comp,         &
                                             rho1, rho2)
            this%comp(ib) = comp
            !
            ! -- update thickness and void ratio
            call this%csub_nodelay_update(ib)
          end if
        end if
        !
        ! -- calculate no-delay interbed rho1 and rho2
        call this%csub_nodelay_fc(ib, hcell, hcellold, rho1, hcof, rhs)
        f = area
      else
        !
        ! -- calculate delay interbed hcof and rhs
        call this%csub_delay_sln(ib, hcell)
        call this%csub_delay_fc(ib, hcof, rhs)
        f = area * this%rnb(ib)
      end if
      rhs = rhs * f
      hcof = -hcof * f
    end if
    !
    ! -- return
    return
  end subroutine csub_interbed_fc

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- create the header list label
    this%listlabel = trim(this%filtyp) // ' NO.'
    if(this%dis%ndim == 3) then
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'ROW'
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'COL'
    elseif(this%dis%ndim == 2) then
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'CELL2D'
    else
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'NODE'
    endif
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'INITIAL_STRESS'
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'FRAC'
    if (this%istoragec == 1) then
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'SSE'
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'SSV'
    else
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'RCI'
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'CI'
    endif
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'THETA'
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'IDELAY'
    if(this%inamedbound == 1) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    endif
    !
    ! -- return
    return
  end subroutine define_listlabel

  subroutine csub_sk_calc_sske(this, n, sske, sske0, hcell, hcellold)
! ******************************************************************************
! csub_sk_calc_sske -- Calculate sske for a gwf cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(inout) :: sske
    real(DP), intent(inout) :: sske0
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    ! -- local variables
    real(DP) :: bot
    real(DP) :: znode
    real(DP) :: znode0
    real(DP) :: es
    real(DP) :: es0
    real(DP) :: theta
    real(DP) :: theta0
    real(DP) :: f
    real(DP) :: f0
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    sske = DZERO
    sske0 = DZERO
    !
    ! -- calculate factor for the head-based case
    if (this%igeocalc == 0) then
      f = DONE
      f0 = DONE
    !
    ! -- calculate factor for the effective stress case
    else
      bot = this%dis%bot(n)
      znode = this%csub_calc_znode(n, hcell)
      znode0 = this%csub_calc_znode(n, hcellold)
      if (this%time_alpha == DZERO) then
        znode0 = znode
      end if
      es = this%sk_es(n)
      es0 = this%sk_es0(n)
      theta = this%sk_theta(n)
      theta0 = this%sk_theta0(n)
      call this%csub_calc_sfacts(n, bot, znode, znode0, theta, theta0,          &
                                 es, es0, f, f0)
    end if
    sske = f * this%ske_cr(n)
    sske0 = f0 * this%ske_cr(n)
    !
    ! -- return
    return
  end subroutine csub_sk_calc_sske
  
  subroutine csub_sk_calc_comp(this, node, hcell, hcellold, comp)
! ******************************************************************************
! csub_sk_calc_comp -- Calculate skeletal compaction
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(GwfCsubType) :: this
    integer(I4B),intent(in) :: node
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    real(DP), intent(inout) :: comp
    ! locals
    real(DP) :: area
    real(DP) :: tled
    real(DP) :: hcof
    real(DP) :: rhs
! ------------------------------------------------------------------------------
!
! -- initialize variables
    area = DONE
    tled = DONE
    !
    ! -- calculate terms
    call this%csub_sk_fc(node, tled, area, hcell, hcellold, hcof, rhs)
    !
    ! - calculate compaction
    comp = hcof * hcell - rhs
    !
    ! -- return
    return
  end subroutine  csub_sk_calc_comp

  
  subroutine csub_sk_update(this, node)
! ******************************************************************************
! csub_sk_update -- Update material properties for coarse grained sediments.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B),intent(in) :: node
    ! locals
    character(len=LINELENGTH) :: errmsg
    character(len=20) :: cellid
    real(DP) :: comp
    real(DP) :: thick
    real(DP) :: theta
! ------------------------------------------------------------------------------
!
! -- update thickness and theta
    comp = this%sk_comp(node)
    call this%dis%noder_to_string(node, cellid)
    if (ABS(comp) > DZERO) then
      thick = this%sk_thick0(node)
      theta = this%sk_theta0(node)
      call this%csub_adj_matprop(comp, thick, theta)
      if (thick <= DZERO) then
        write(errmsg,'(4x,a,1x,a,1x,a,1x,g0,1x,a)')                             &
          '****ERROR. ADJUSTED THICKNESS FOR CELL', trim(adjustl(cellid)),      &
          'IS <= 0 (', thick, ')'
        call store_error(errmsg)
      end if
      if (theta <= DZERO) then
        write(errmsg,'(4x,a,1x,a,1x,a,1x,g0,1x,a)')                             &
          '****ERROR. ADJUSTED THETA FOR CELL', trim(adjustl(cellid)),          &
          'IS <= 0 (', theta, ')'
        call store_error(errmsg)
      end if
      this%sk_thick(node) = thick
      this%sk_theta(node) = theta
    end if
    !
    ! -- return
    return
  end subroutine csub_sk_update

  
  subroutine csub_sk_wcomp_fc(this, node, tled, area, hcell, hcellold,          &
                              hcof, rhs)
! ******************************************************************************
! csub_sk_wcomp_fc -- Calculate water compressibility term for a gwf cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B),intent(in) :: node
    real(DP), intent(in) :: tled
    real(DP), intent(in) :: area
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rhs
    ! locals
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: tthk
    real(DP) :: tthk0
    real(DP) :: snold
    real(DP) :: snnew
    real(DP) :: wc1
    real(DP) :: wc2
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- aquifer elevations and thickness
    top = this%dis%top(node)
    bot = this%dis%bot(node)
    tthk = this%sk_thick(node)
    tthk0 = this%sk_thick0(node)
    ! -- aquifer saturation
    if (this%stoiconv(node) /= 0) then
      snold = sQuadraticSaturation(top, bot, hcellold, this%satomega)
      snnew = sQuadraticSaturation(top, bot, hcell, this%satomega)
    else
      snold = DONE
      snnew = DONE
    end if
    if (this%time_alpha == DZERO) then
      snold = snnew
    end if
    !
    ! -- storage coefficients
    wc1 = this%gammaw * this%beta * area * tthk0 * this%sk_theta0(node) * tled
    wc2 = this%gammaw * this%beta * area * tthk * this%sk_theta(node) * tled
    !
    ! -- calculate hcof term
    hcof = -wc2 * snnew
    !
    ! -- calculate rhs term
    rhs = -wc1 * snold * hcellold
    !
    ! -- return
    return
  end subroutine csub_sk_wcomp_fc

  
  subroutine csub_sk_wcomp_fn(this, node, tled, area, hcell, hcof, rhs)
! ******************************************************************************
! csub_sk_wcomp_fc -- Calculate water compressibility newton terms for a 
!                       gwf cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B),intent(in) :: node
    real(DP), intent(in) :: tled
    real(DP), intent(in) :: area
    real(DP), intent(in) :: hcell
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rhs
    ! locals
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: tthk
    real(DP) :: derv
    real(DP) :: wc2
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- aquifer elevations and thickness
    top = this%dis%top(node)
    bot = this%dis%bot(node)
    tthk = this%sk_thick(node)
    !
    ! -- calculate saturation derivitive
    derv = sQuadraticSaturationDerivative(top, bot, hcell)    
    !
    ! -- storage coefficients
    wc2 = this%gammaw * this%beta * area * tthk * this%sk_theta(node) * tled
    !
    ! -- calculate hcof term
    hcof = -wc2 * derv * hcell
    !
    ! -- calculate rhs term
    rhs = hcof * hcell
    !
    ! -- return
    return
  end subroutine csub_sk_wcomp_fn

  
  subroutine csub_interbed_wcomp_fc(this, ib, node, tled, area,                 &
                                      hcell, hcellold, hcof, rhs)
! ******************************************************************************
! csub_interbed_wcomp_fc -- Calculate water compressibility term for an 
!                           interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B),intent(in) :: ib
    integer(I4B),intent(in) :: node
    real(DP), intent(in) :: tled
    real(DP), intent(in) :: area
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rhs
    ! locals
    integer(I4B) :: n
    integer(I4B) :: idelay
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: snold
    real(DP) :: snnew
    real(DP) :: f
    real(DP) :: fmult
    real(DP) :: dz
    real(DP) :: wc1
    real(DP) :: wc2
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- aquifer elevations and thickness
    top = this%dis%top(node)
    bot = this%dis%bot(node)
    ! -- aquifer saturation
    if (this%stoiconv(node) /= 0) then
      snold = sQuadraticSaturation(top, bot, hcellold, this%satomega)
      snnew = sQuadraticSaturation(top, bot, hcell, this%satomega)
    else
      snold = DONE
      snnew = DONE
    end if
    if (this%time_alpha == DZERO) then
      snold = snnew
    end if
    !
    !
    idelay = this%idelay(ib)
    f = this%gammaw * this%beta * area * tled
    if (idelay == 0) then
      wc1 = f * this%theta0(ib) * this%thick0(ib)
      wc2 = f * this%theta(ib) * this%thick(ib)
      hcof = -wc2 * snnew
      rhs = -wc1 * snold * hcellold
    else
      if (this%thick(ib) > DZERO) then
        dz = this%dbfact * this%dbdz(idelay)
        do n = 1, this%ndelaycells
          fmult = DONE
          !
          ! -- scale factor for half-cell problem
          if (this%idbhalfcell /= 0) then
            if (n == 1) then
              fmult = DHALF
            end if
          end if
          wc2 = fmult * f * dz * this%dbtheta(n, idelay)
          wc1 = wc2
          rhs = rhs - (wc1 * snold * this%dbh0(n, idelay) -                     &
                       wc2 * snnew * this%dbh(n, idelay))
        end do
        rhs = rhs * this%rnb(ib)
        !fmult = this%dbfact * this%thick(ib) * this%dbtheta(1, idelay)
        !wc2 = fmult * f
        !wc1 = wc2
        !rhs = -this%rnb(ib) * (wc1 * snold * hcellold - wc2 * snnew * hcell)
      end if
    end if
    !
    ! -- return
    return
  end subroutine csub_interbed_wcomp_fc
  
  
  function csub_calc_void(this, theta) result(void)
! ******************************************************************************
! csub_calc_void -- Calculate void ratio from the porosity
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    ! -- dummy
    real(DP), intent(in) :: theta
    ! -- local variables
    real(DP) :: void
! ------------------------------------------------------------------------------
    void = theta / (DONE - theta)
    !
    ! -- return
    return
  end function csub_calc_void
  
  
  function csub_calc_theta(this, void) result(theta)
! ******************************************************************************
! csub_calc_theta -- Calculate porosity from the void ratio
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    ! -- dummy
    real(DP), intent(in) :: void
    ! -- local variables
    real(DP) :: theta
! ------------------------------------------------------------------------------
    theta = void / (DONE + void)
    !
    ! -- return
    return
  end function csub_calc_theta
  
  
  function csub_calc_znode(this, node, hcell) result(znode)
! ******************************************************************************
! csub_calc_znode -- Calculate elevation of the center of the saturated 
!                    cell thickness
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    ! -- dummy
    integer(I4B), intent(in) :: node
    real(DP), intent(in) :: hcell
    ! -- local variables
    real(DP) :: znode
    real(DP) :: v
    real(DP) :: top
    real(DP) :: bot
! ------------------------------------------------------------------------------
    top = this%dis%top(node)
    bot = this%dis%bot(node)
    if (hcell > top) then
      v = top
    else if (hcell < bot) then
      v = bot
    else
      v = hcell
    end if
    znode = (v + bot) * DHALF
    !
    ! -- return
    return
  end function csub_calc_znode

  
  subroutine csub_calc_sfacts(this, node, bot, znode, znode0, theta, theta0,    &
                              es, es0, fact, fact0)
! ******************************************************************************
! csub_calc_sfacts -- Calculate sske and sske0 factor for a gwf cell or 
!                     interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: node
    real(DP), intent(in) :: bot
    real(DP), intent(in) :: znode
    real(DP), intent(in) :: znode0
    real(DP), intent(in) :: theta
    real(DP), intent(in) :: theta0
    real(DP), intent(in) :: es
    real(DP), intent(in) :: es0
    real(DP), intent(inout) :: fact
    real(DP), intent(inout) :: fact0
    ! -- local variables
    real(DP) :: esv
    real(DP) :: void
    real(DP) :: denom
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    fact = DZERO
    fact0 = DZERO
    !
    ! -- calculate factor for the effective stress case
    esv = this%time_alpha * es +                                             &
          (DONE - this%time_alpha) * es0
    void = this%csub_calc_void(theta)
    denom = (DONE + void) * (esv - (znode - bot) * (this%sgs(node) - DONE))
    if (denom /= DZERO) then
      fact = DONE / denom
    end if
    esv = es0
    void = this%csub_calc_void(theta0)
    denom = (DONE + void) * (esv - (znode0 - bot) * (this%sgs(node) - DONE))
    if (denom /= DZERO) then
      fact0 = DONE / denom
    end if
    !
    ! -- return
    return
  end subroutine csub_calc_sfacts  

  
  subroutine csub_adj_matprop(this, comp, thick, theta)
! ******************************************************************************
! csub_adj_matprop -- Adjust theta and thickness based on compaction.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    real(DP), intent(in) :: comp
    real(DP), intent(inout) :: thick
    real(DP), intent(inout) :: theta
    ! -- local variables
    real(DP) :: strain
    real(DP) :: void
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    strain = DZERO
    void = this%csub_calc_void(theta)
    !
    ! -- calculate strain
    if (thick > DZERO) strain = -comp / thick
    !
    ! -- update void ratio, theta, and thickness
    void = void + strain * (DONE + void)
    theta = this%csub_calc_theta(void)
    thick = thick - comp
    !
    ! -- return
    return
  end subroutine csub_adj_matprop   

  subroutine csub_delay_sln(this, ib, hcell)
! ******************************************************************************
! csub_delay_sln -- Calculate flow in delay interbeds.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(in) :: hcell
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: icnvg
    integer(I4B) :: iter
    integer(I4B) :: idelay
    real(DP) :: dh
    real(DP) :: dhmax
    real(DP) :: dhmax0
    real(DP), parameter :: dclose = DHUNDRED * DPREC
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    !
    ! -- calculate geostatic and effective stress for each delay bed cell
    call this%csub_delay_calc_stress(ib, hcell)
    !
    ! -- terminate if the aquifer head is below the top of delay interbeds
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- solve for delay bed heads
    if (this%thick(ib) > DZERO) then
      icnvg = 0
      iter = 0
      idelay = this%idelay(ib)
      do
        iter = iter + 1
        !
        ! -- assemble coefficients
        call this%csub_delay_assemble(ib, hcell)
        !
        ! -- solve for head change in delay interbed cells
        call csub_delay_solve(this%ndelaycells,                                 & 
                              this%dbal, this%dbad, this%dbau,                  &
                              this%dbrhs, this%dbdh, this%dbaw)
        !
        ! -- calculate maximum head change and update delay bed heads 
        dhmax = DZERO
        do n = 1, this%ndelaycells
          dh = this%dbdh(n) - this%dbh(n, idelay) 
          if (abs(dh) > abs(dhmax)) then
            dhmax = dh !this%dbdh(n)
          end if
          ! update delay bed heads
          this%dbh(n, idelay) = this%dbdh(n)
        end do
        !
        ! -- update delay bed stresses
        call this%csub_delay_calc_stress(ib, hcell)
        !
        ! -- check delay bed convergence 
        if (abs(dhmax) < dclose) then
          icnvg = 1
        else if (iter /= 1) then
          if (abs(dhmax)-abs(dhmax0) < DPREC) then
            icnvg = 1
          end if
        end if
        if (icnvg == 1) then
          exit
        end if
        dhmax0 = dhmax
      end do
    end if
    !
    ! -- return
    return
  end subroutine csub_delay_sln


  subroutine csub_delay_calc_zcell(this, ib)
! ******************************************************************************
! csub_delay_calc_zcell -- Calculate z for delay interbeds cells.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: node
    integer(I4B) :: idelay
    real(DP) :: bot
    real(DP) :: top
    real(DP) :: znode
    real(DP) :: dzz
    real(DP) :: z
    real(DP) :: b
    real(DP) :: dz
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    node = this%nodelist(ib)
    b = this%thick(ib)
    bot = this%dis%bot(node)
    if (this%idbhalfcell == 0) then
      top = bot + b
    else
      top = bot + DTWO * b
    end if
    !
    ! -- calculate znode based on assumption that the delay bed bottom 
    !    is equal to the cell bottom
    znode = this%csub_calc_znode(node, top)
    dz = DHALF * this%dbdz(idelay)
    if (this%idbhalfcell == 0) then
        dzz = DHALF * b
        z = znode + dzz
    else
        z = znode + dz 
    end if
    ! -- calculate z for each delay interbed node
    do n = 1, this%ndelaycells
      z = z - dz
      this%dbz(n, idelay) = z
      z = z - dz
    end do
    !
    ! -- return
    return

  end subroutine csub_delay_calc_zcell

  subroutine csub_delay_chk(this, ib, hcell)
! ******************************************************************************
! csub_delay_chk -- Check the head relative to the top of a delay interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(in) :: hcell
    ! -- local variables
    character(len=LINELENGTH) :: errmsg
    character(len=20) :: cellid
    integer(I4B) :: idelay
    integer(I4B) :: node
    real(DP) :: dzhalf
    real(DP) :: top
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    node = this%nodelist(ib)
    dzhalf = DHALF * this%dbdz(idelay)
    top = this%dbz(1, idelay) + dzhalf
    !
    ! -- check that aquifer head is above the top of the interbed
    if (hcell < top) then
      call this%dis%noder_to_string(node, cellid)
      write(errmsg, '(a,g0,a,1x,a,1x,a,1x,i0,1x,a,g0,a)') &
        'ERROR: HEAD (', hcell, ') IN CONVERTIBLE CELL', trim(adjustl(cellid)), &
        'IS LESS THAN THE TOP OF DELAY INTERBED', ib,                           &
        '(', top,')'
      call store_error(errmsg)
    end if
    !
    ! -- return
    return

  end subroutine csub_delay_chk


  subroutine csub_delay_calc_stress(this, ib, hcell)
! ******************************************************************************
! csub_delay_calc_stress -- Calculate geostatic and effective stress in delay
!                      interbeds.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(in) :: hcell
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: idelay
    integer(I4B) :: node
    real(DP) :: sigma
    real(DP) :: topaq
    real(DP) :: botaq
    real(DP) :: dzhalf
    real(DP) :: sadd
    real(DP) :: sgm
    real(DP) :: sgs
    real(DP) :: h
    real(DP) :: z
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: u
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    node = this%nodelist(ib)
    sigma = this%sk_gs(node)
    topaq = this%dis%top(node)
    botaq = this%dis%bot(node)
    dzhalf = DHALF * this%dbdz(idelay)
    top = this%dbz(1, idelay) + dzhalf
    !
    ! -- calculate the geostatic load in the cell at the top of the interbed
    if (this%igeocalc > 0) then
      sgm = this%sgm(node)
      sgs = this%sgs(node)
      if (hcell > top) then
        sadd = (top - botaq) * sgs
      else if (hcell < botaq) then
        sadd = (top - botaq) * sgm
      else
        sadd = ((top - hcell) * sgm) + ((hcell - botaq) * sgs)
      end if
      sigma = sigma - sadd
    end if
    !
    ! -- calculate geostatic and effective stress for each interbed node
    do n = 1, this%ndelaycells
      h = this%dbh(n, idelay)
      if (this%igeocalc == 0) then
        this%dbes(n, idelay) = h
      else
        !
        ! -- geostatic calculated at the bottom of the delay cell
        z = this%dbz(n, idelay)
        top = z + dzhalf
        bot = z - dzhalf
        u = h - bot
        if (h > top) then
            sadd = (top - bot) * sgs
        else if (h < bot) then
            sadd = (top - bot) * sgm
        else
            sadd = ((top - h) * sgm) + ((h - bot) * sgs)
        end if
        sigma = sigma + sadd
        this%dbgeo(n, idelay) = sigma
        this%dbes(n, idelay) = sigma - u
      end if
    end do
    !
    ! -- return
    return
  end subroutine csub_delay_calc_stress

  subroutine csub_delay_calc_ssksske(this, ib, n, ssk, sske)
! ******************************************************************************
! csub_delay_calc_ssksske -- Calculate ssk and sske for a node in a delay
!                            interbed cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    integer(I4B), intent(in) :: n
    real(DP), intent(inout) :: ssk
    real(DP), intent(inout) :: sske
    ! -- local variables
    integer(I4B) :: idelay
    integer(I4B) :: node
    real(DP) :: znode
    real(DP) :: zbot
    real(DP) :: es
    real(DP) :: es0
    real(DP) :: void
    real(DP) :: denom
    real(DP) :: denom0
    real(DP) :: f
    real(DP) :: f0
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    sske = DZERO
    ssk = DZERO
    idelay = this%idelay(ib)
    !
    ! -- calculate factor for the head-based case
    if (this%igeocalc == 0) then
      f = DONE
      !
      ! -- scale factor for half-cell problem
      if (this%idbhalfcell /= 0) then
        if (n == 1) then
          f = DHALF
        end if
      end if
      f0 = f
    !
    ! -- calculate factor for the effective stress case
    else
      node = this%nodelist(ib)
      void = this%csub_calc_void(this%dbtheta(n, idelay))
      !
      ! -- get elevation of delay node and calculate the bottom 
      !    of the delay node
      znode = this%dbz(n, idelay)
      zbot = znode - DHALF * this%dbdz(idelay)
      !
      ! -- set the appropriate current and previous effective stress
      es = this%dbes(n, idelay)
      es0 = this%dbes0(n, idelay)
      es = this%time_alpha * es + (DONE - this%time_alpha) * es0
      !
      ! -- denom and denom0 are calculate at the center of the node and
      !    result in a mean sske and ssk for the delay cell
      denom = (DONE + void) * (es - (znode - zbot) * (this%sgs(node) - DONE))
      denom0 = (DONE + void) * (es0 - (znode - zbot) * (this%sgs(node) - DONE))
      if (denom /= DZERO) then
        f = DONE / denom
      else
        f = DZERO
      end if
      if (denom0 /= DZERO) then
        f0 = DONE / denom0
      else
        f0 = DZERO
      end if
    end if
    sske = f0 * this%rci(ib)
    if (this%igeocalc == 0) then
      if (this%dbh(n, idelay) < this%dbpcs(n, idelay)) then
        ssk = f * this%ci(ib)
      else
        ssk = f * this%rci(ib)
      end if
    else
      if (this%dbes(n, idelay) > this%dbpcs(n, idelay)) then
        ssk = f * this%ci(ib)
      else
        ssk = f * this%rci(ib)
      end if
    end if
    !
    ! -- return
    return
  end subroutine csub_delay_calc_ssksske


  subroutine csub_delay_assemble(this, ib, hcell)
! ******************************************************************************
! csub_delay_assemble -- Assemble coefficients for delay interbeds cells.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt
    ! -- dummy variables
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(in) :: hcell
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: node
    integer(I4B) :: idelay
    real(DP) :: dz
    real(DP) :: dzhalf
    real(DP) :: c
    real(DP) :: c2
    real(DP) :: c3
    real(DP) :: fmult
    real(DP) :: sske
    real(DP) :: ssk
    real(DP) :: z
    real(DP) :: ztop
    real(DP) :: zbot
    real(DP) :: h
    real(DP) :: aii
    real(DP) :: r
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    node = this%nodelist(ib)
    dz = this%dbdz(idelay)
    dzhalf = DHALF * dz
    fmult = dz / delt
    c = this%kv(ib) / dz
    c2 = DTWO * c
    c3 = DTHREE * c
    !
    !
    do n = 1, this%ndelaycells
      !
      ! -- calculate  ssk and sske
      call this%csub_delay_calc_ssksske(ib, n, ssk, sske)
      !
      ! -- diagonal and right hand side
      aii = -ssk * fmult
      z = this%dbz(n, idelay)
      ztop = z + dzhalf
      zbot = z - dzhalf
      h = this%dbh(n, idelay)
      if (this%igeocalc == 0) then
        r = -fmult * &
             (ssk * (this%dbpcs(n, idelay)) + &
              sske * (this%dbh0(n, idelay) - this%dbpcs(n, idelay)))
      else
        r = -fmult * &
             (ssk * (this%dbgeo(n, idelay) + zbot - this%dbpcs(n, idelay)) +    &
              sske * (this%dbpcs(n, idelay) - this%dbes0(n, idelay)))
      end if
      !
      ! -- add connection to the gwf cell
      if (n == 1 .or. n == this%ndelaycells) then
        !
        ! -- gwf cell is connected to n=ndelaycell for half cell connection 
        if (this%idbhalfcell == 0 .or. n == this%ndelaycells) then
            aii = aii - c3
            r = r - c2 * hcell
        else
            aii = aii - c
        end if
      else
        aii = aii - c2
      end if
      !
      ! -- off diagonals
      ! -- lower
      if (n > 1) then
        this%dbal(n) = c
      end if
      !
      ! -- upper
      if (n < this%ndelaycells) then
        this%dbau(n) = c
      end if
      this%dbad(n) = aii
      this%dbrhs(n) = r
    end do
    !
    ! -- return
    return

  end subroutine csub_delay_assemble

  subroutine csub_delay_solve(n, tl, td, tu, b, x, w)
! ******************************************************************************
! csub_delay_solve -- Solve for head change in delay interbeds cells.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    integer(I4B), intent(in) :: n
    real(DP), dimension(n), intent(in) :: tl
    real(DP), dimension(n), intent(in) :: td
    real(DP), dimension(n), intent(in) :: tu
    real(DP), dimension(n), intent(in) :: b
    real(DP), dimension(n), intent(inout) :: x
    real(DP), dimension(n), intent(inout) :: w
    ! -- local variables
    integer(I4B) :: j
    real(DP) :: bet
    real(DP) :: beti
! ------------------------------------------------------------------------------
    ! -- initialize variables
    w(1) = DZERO
    bet = td(1)
    beti = DONE / bet
    x(1) = b(1) * beti
    ! -- decomposition and forward substitution
    do j = 2, n
      w(j) = tu(j-1) * beti
      bet = td(j) - tl(j) * w(j)
      beti = DONE / bet
      x(j) = (b(j) - tl(j) * x(j-1)) * beti
    end do
    ! -- backsubstitution
    do j = n-1, 1, -1
      x(j) = x(j) - w(j+1) * x(j+1)
    end do
    ! -- return
    return
  end subroutine csub_delay_solve
  
  
  subroutine csub_delay_calc_err(this, ib, hcell, err)
! ******************************************************************************
! csub_delay_calc_err -- Calculate error in a delay interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(in) :: hcell
    real(DP), intent(inout) :: err
    ! -- local variables
    integer(I4B) :: idelay
    integer(I4B) :: n
    real(DP) :: v
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    err = DZERO
    !
    !
    if (this%thick(ib) > DZERO) then
      call this%csub_delay_assemble(ib, hcell)
      do n = 1, this%ndelaycells
        v = this%dbad(n) * this%dbh(n, idelay)
        if (n > 1) then
          v = v + this%dbal(n) * this%dbh(n-1, idelay)
        end if
        if (n < this%ndelaycells) then
          v = v + this%dbau(n) * this%dbh(n+1, idelay)
        end if
        v = v - this%dbrhs(n)
        err = err + v
      end do
    end if
    !
    ! -- return
    return
  end subroutine csub_delay_calc_err

 
  subroutine csub_delay_calc_dstor(this, ib, stoe, stoi)
! ******************************************************************************
! csub_delay_calc_dstor -- Calculate change in storage in a delay interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy variables
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(inout) :: stoe
    real(DP), intent(inout) :: stoi
    ! -- local variables
    integer(I4B) :: idelay
    integer(I4B) :: n
    real(DP) :: sske
    real(DP) :: ssk
    real(DP) :: fmult
    real(DP) :: v1
    real(DP) :: v2
    real(DP) :: ske
    real(DP) :: sk
    real(DP) :: z
    real(DP) :: zbot
    real(DP) :: dzhalf
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    stoe = DZERO
    stoi = DZERO
    ske = DZERO
    sk = DZERO
    !
    !
    if (this%thick(ib) > DZERO) then
      fmult = this%dbfact * this%dbdz(idelay)
      dzhalf = DHALF * this%dbdz(idelay)
      do n = 1, this%ndelaycells
        call this%csub_delay_calc_ssksske(ib, n, ssk, sske)
        if (this%igeocalc == 0) then
          v1 = ssk * (this%dbpcs(n, idelay) - this%dbh(n, idelay))
          v2 = sske * (this%dbh0(n, idelay) - this%dbpcs(n, idelay))
        else
          !v1 = ssk * (this%dbes(n, idelay) - this%dbpcs(n, idelay))
          z = this%dbz(n, idelay)
          zbot = z - dzhalf
          v1 = ssk * (this%dbgeo(n, idelay) - this%dbh(n, idelay) + zbot - &
                      this%dbpcs(n, idelay))
          v2 = sske * (this%dbpcs(n, idelay) - this%dbes0(n, idelay))
        end if
        !
        ! -- calculate inelastic and elastic storage components
        if (ssk /= sske) then
          stoi = stoi + v1 * fmult
          stoe = stoe + v2 * fmult
        else
          stoe = stoe + (v1 + v2) * fmult
        end if
        !
        ! calculate inelastic and elastic storativity
        ske = ske + sske * fmult
        sk = sk + ssk * fmult
      end do
    end if
    !
    ! -- save ske and sk
    this%ske(ib) = ske
    this%sk(ib) = sk
    !
    ! -- return
    return
  end subroutine csub_delay_calc_dstor

  subroutine csub_delay_calc_comp(this, ib)
! ******************************************************************************
! csub_delay_calc_comp -- Calculate compaction in a delay interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    ! -- local variables
    integer(I4B) :: idelay
    integer(I4B) :: n
    real(DP) :: comp
    real(DP) :: compi
    real(DP) :: compe
    real(DP) :: sske
    real(DP) :: ssk
    real(DP) :: fmult
    real(DP) :: v
    real(DP) :: v1
    real(DP) :: v2
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    comp = DZERO
    compi = DZERO
    compe = DZERO
    !
    !
    if (this%thick(ib) > DZERO) then
      fmult = this%dbdz(idelay) * this%dbfact
      do n = 1, this%ndelaycells
        call this%csub_delay_calc_ssksske(ib, n, ssk, sske)
        if (this%igeocalc == 0) then
          v1 = ssk * (this%dbpcs(n, idelay) - this%dbh(n, idelay))
          v2 = sske * (this%dbh0(n, idelay) - this%dbpcs(n, idelay))
        else
          v1 = ssk * (this%dbes(n, idelay) - this%dbpcs(n, idelay))
          v2 = sske * (this%dbpcs(n, idelay) - this%dbes0(n, idelay))
        end if
        v = (v1 + v2) * fmult
        comp = comp + v
        !
        ! -- calculate inelastic and elastic storage components
        if (ssk /= sske) then
          compi = compi + v1 * fmult
          compe = compe + v2 * fmult
        else
          compe = compe + (v1 + v2) * fmult
        end if
      end do
    end if
    !
    ! -- fill compaction
    this%comp(ib) = comp
    this%tcomp(ib) = this%tcomp(ib) + comp * this%rnb(ib)
    this%tcompi(ib) = this%tcompi(ib) + compi * this%rnb(ib)
    this%tcompe(ib) = this%tcompe(ib) + compe * this%rnb(ib)
    !
    ! -- return
    return
  end subroutine csub_delay_calc_comp

  subroutine csub_delay_fc(this, ib, hcof, rhs)
! ******************************************************************************
! csub_delay_fc -- Calculate hcof and rhs for delay interbed contribution to
!                  GWF cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rhs
    ! -- local variables
    integer(I4B) :: idelay
    real(DP) :: c1
    real(DP) :: c2
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    hcof = DZERO
    rhs = DZERO
    if (this%thick(ib) > DZERO) then
      ! -- calculate terms for gwf matrix
      if (this%idbhalfcell == 0) then
        c1 = DTWO * this%kv(ib) / this%dbdz(idelay)
        rhs = -c1 * this%dbh(1, idelay)
      else
        c1 = DZERO
      end if
      c2 = this%dbfact * DTWO * this%kv(ib) / this%dbdz(idelay)
      rhs = rhs - c2 * this%dbh(this%ndelaycells, idelay)
      hcof = c1 + c2
    end if
    !
    ! -- return
    return
  end subroutine csub_delay_fc
  
  
  !
  ! -- Procedures related to observations (type-bound)
  logical function csub_obs_supported(this)
  ! ******************************************************************************
  ! csub_obs_supported
  !   -- Return true because csub package supports observations.
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    class(GwfCsubType) :: this
  ! ------------------------------------------------------------------------------
    csub_obs_supported = .true.
    return
  end function csub_obs_supported


  subroutine csub_df_obs(this)
  ! ******************************************************************************
  ! csub_df_obs (implements bnd_df_obs)
  !   -- Store observation type supported by csub package.
  !   -- Overrides BndType%bnd_df_obs
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfCsubType) :: this
    ! -- local
    integer(I4B) :: indx
  ! ------------------------------------------------------------------------------
    !
    ! -- Store obs type and assign procedure pointer
    !    for csub observation type.
    call this%obs%StoreObsType('csub', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for inelastic-csub observation type.
    call this%obs%StoreObsType('inelastic-csub', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for elastic-csub observation type.
    call this%obs%StoreObsType('elastic-csub', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for skeletal-csub observation type.
    call this%obs%StoreObsType('skeletal-csub', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for csub-cell observation type.
    call this%obs%StoreObsType('csub-cell', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for interbed ske observation type.
    call this%obs%StoreObsType('ske', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for interbed sk observation type.
    call this%obs%StoreObsType('sk', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for ske-cell observation type.
    call this%obs%StoreObsType('ske-cell', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for sk-cell observation type.
    call this%obs%StoreObsType('sk-cell', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for geostatic-stress-cell observation type.
    call this%obs%StoreObsType('gstress-cell', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for effective-stress-cell observation type.
    call this%obs%StoreObsType('estress-cell', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for total-compaction observation type.
    call this%obs%StoreObsType('interbed-compaction', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for inelastic-compaction observation type.
    call this%obs%StoreObsType('inelastic-compaction', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for inelastic-compaction observation type.
    call this%obs%StoreObsType('elastic-compaction', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for skeletal-compaction observation type.
    call this%obs%StoreObsType('skeletal-compaction', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for compaction-cell observation type.
    call this%obs%StoreObsType('compaction-cell', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for total-compaction observation type.
    call this%obs%StoreObsType('thickness', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for preconstress-cell observation type.
    call this%obs%StoreObsType('preconstress-cell', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for preconstress observation type.
    call this%obs%StoreObsType('preconstress', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for delay-head observation type.
    call this%obs%StoreObsType('delay-head', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    return
  end subroutine csub_df_obs


  subroutine csub_bd_obs(this)
    ! **************************************************************************
    ! csub_bd_obs
    !   -- Calculate observations this time step and call
    !      ObsType%SaveOneSimval for each GwfCsubType observation.
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    ! -- dummy
    class(GwfCsubType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: n
    integer(I4B) :: nn
    integer(I4B) :: idelay
    integer(I4B) :: ncol
    real(DP) :: v
    real(DP) :: r
    character(len=100) :: msg
    type(ObserveType), pointer :: obsrv => null()
    !---------------------------------------------------------------------------
    !
    ! Write simulated values for all csub observations
    if (this%obs%npakobs>0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        nn = size(obsrv%indxbnds)
        v = DZERO
        do j = 1, nn
          n = obsrv%indxbnds(j)
          select case (obsrv%ObsTypeId)
            case ('CSUB')
              v = this%storagee(n) + this%storagei(n)
            case ('INELASTIC-CSUB')
              v = this%storagei(n)
            case ('ELASTIC-CSUB')
              v = this%storagee(n)
            case ('SKELETAL-CSUB')
              v = this%sk_stor(n)
            case ('CSUB-CELL')
              ! -- add the skeletal component
              if (j == 1) then
                v = this%sk_stor(n)
              else
                v = this%storagee(n) + this%storagei(n)
              end if
            case ('SKE')
              v = this%ske(n)
            case ('SK')
              v = this%sk(n)
            case ('SKE-CELL')
              ! -- add the skeletal component
              if (j == 1) then
                v = this%sk_ske(n)
              else
                v = this%ske(n)
              end if
            case ('SK-CELL')
              ! -- add the skeletal component
              if (j == 1) then
                v = this%sk_sk(n)
              else
                v = this%sk(n)
              end if
            case ('GSTRESS-CELL')
              v = this%sk_gs(n)
            case ('ESTRESS-CELL')
              v = this%sk_es(n)
            case ('INTERBED-COMPACTION')
              v = this%tcomp(n)
            case ('INELASTIC-COMPACTION')
              v = this%tcompi(n)
            case ('ELASTIC-COMPACTION')
              v = this%tcompe(n)
            case ('SKELETAL-COMPACTION')
              v = this%sk_tcomp(n)
            case ('COMPACTION-CELL')
              ! -- add the skeletal component
              if (j == 1) then
                v = this%sk_tcomp(n)
              else
                v = this%tcomp(n)
              end if
            case ('THICKNESS')
              v = this%thick(n)
            !case ('PRECONSTRESS')
            !   if (n > this%ndelaycells) then
            !    r = real(n, DP) / real(this%ndelaycells, DP)
            !    idelay = int(floor(r)) + 1
            !    ncol = mod(n, this%ndelaycells)
            !  else
            !    idelay = 1
            !    ncol = n
            !  end if
            !  v = this%dbpcs(ncol, idelay)
            case ('DELAY-HEAD', 'PRECONSTRESS')
              if (n > this%ndelaycells) then
                r = real(n, DP) / real(this%ndelaycells, DP)
                idelay = int(floor(r)) + 1
                ncol = mod(n, this%ndelaycells)
              else
                idelay = 1
                ncol = n
              end if
              select case(obsrv%ObsTypeId)
                case ('PRECONSTRESS')
                  v = this%dbpcs(ncol, idelay)
                case ('DELAY-HEAD')
                  v = this%dbh(ncol, idelay)
              end select
            case ('PRECONSTRESS-CELL')
              v = this%pcs(n)
            case default
              msg = 'Error: Unrecognized observation type: ' // trim(obsrv%ObsTypeId)
              call store_error(msg)
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      end do
    end if
    !
    ! -- write summary of package block error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    return
  end subroutine csub_bd_obs


  subroutine csub_rp_obs(this)
    ! -- dummy
    class(GwfCsubType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, j, n
    integer(I4B) :: n2
    integer(I4B) :: idelay
    class(ObserveType), pointer :: obsrv => null()
    character(len=LENBOUNDNAME) :: bname
    character(len=200) :: ermsg
    logical :: jfound
    !
    if (.not. this%csub_obs_supported()) return
    !
    do i=1,this%obs%npakobs
      obsrv => this%obs%pakobs(i)%obsrv
      !
      ! -- indxbnds needs to be deallocated and reallocated (using
      !    ExpandArray) each stress period because list of boundaries
      !    can change each stress period.
      if (allocated(obsrv%indxbnds)) then
        deallocate(obsrv%indxbnds)
      endif
      obsrv%BndFound = .false.
      !
      bname = obsrv%FeatureName
      if (bname /= '') then
        ! -- Observation location(s) is(are) based on a boundary name.
        !    Iterate through all boundaries to identify and store
        !    corresponding index(indices) in bound array.
        jfound = .false.
        do j=1,this%ninterbeds
          if (this%boundname(j) == bname) then
            jfound = .true.
            obsrv%BndFound = .true.
            obsrv%CurrentTimeStepEndValue = DZERO
            call ExpandArray(obsrv%indxbnds)
            n = size(obsrv%indxbnds)
            obsrv%indxbnds(n) = j
          endif
        enddo
      else if (obsrv%ObsTypeId == 'GSTRESS-CELL' .or.                           &
               obsrv%ObsTypeId == 'ESTRESS-CELL' .or.                           &
               obsrv%ObsTypeId == 'SKELETAL-CSUB' .or.                          &
               obsrv%ObsTypeId == 'SKELETAL-COMPACTION') then
        jfound = .true.
        obsrv%BndFound = .true.
        obsrv%CurrentTimeStepEndValue = DZERO
        call ExpandArray(obsrv%indxbnds)
        n = size(obsrv%indxbnds)
        obsrv%indxbnds(n) = obsrv%NodeNumber
      else if (obsrv%ObsTypeId == 'PRECONSTRESS') then
        n = obsrv%NodeNumber
        idelay = this%idelay(n)
        j = (idelay - 1) * this%ndelaycells + 1
        n2 = obsrv%NodeNumber2
        if (n2 < 1 .or. n2 > this%ndelaycells) then
          write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
            'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
            ' interbed cell must be > 0 and <=', this%ndelaycells, &
            '(specified value is ', n2, ')'
          call store_error(ermsg)
        else
          j = (idelay - 1) * this%ndelaycells + n2
        end if
        call ExpandArray(obsrv%indxbnds)
        obsrv%indxbnds(1) = j
      else
        ! -- Observation location is a single node number
        jfound = .false.
        ! -- save node number in first position
        if (obsrv%ObsTypeId == 'CSUB-CELL' .or.                                 &
            obsrv%ObsTypeId == 'SKE-CELL' .or.                                  &
            obsrv%ObsTypeId == 'SK-CELL' .or.                                   &
            obsrv%ObsTypeId == 'COMPACTION-CELL') then 
          if (.NOT. obsrv%BndFound) then
            jfound = .true.
            obsrv%BndFound = .true.
            obsrv%CurrentTimeStepEndValue = DZERO
            call ExpandArray(obsrv%indxbnds)
            n = size(obsrv%indxbnds)
            obsrv%indxbnds(n) = obsrv%NodeNumber
          end if
        end if
        jloop: do j = 1, this%ninterbeds
          if (this%nodelist(j) == obsrv%NodeNumber) then
            jfound = .true.
            obsrv%BndFound = .true.
            obsrv%CurrentTimeStepEndValue = DZERO
            call ExpandArray(obsrv%indxbnds)
            n = size(obsrv%indxbnds)
            obsrv%indxbnds(n) = j
          endif
        enddo jloop
      endif
    enddo
    !
    if (count_errors() > 0) then
      call store_error_unit(this%inunit)
      call ustop()
    endif
    !
    return
  end subroutine csub_rp_obs
  !
  ! -- Procedures related to observations (NOT type-bound)
  subroutine csub_process_obsID(obsrv, dis, inunitobs, iout)
    ! -- This procedure is pointed to by ObsDataType%ProcesssIdPtr. It processes
    !    the ID string of an observation definition for csub-package observations.
    ! -- dummy
    type(ObserveType),      intent(inout) :: obsrv
    class(DisBaseType), intent(in)    :: dis
    integer(I4B),            intent(in)    :: inunitobs
    integer(I4B),            intent(in)    :: iout
    ! -- local
    integer(I4B) :: nn1
    integer(I4B) :: nn2
    integer(I4B) :: icol, istart, istop
    character(len=LINELENGTH) :: strng
    character(len=LENBOUNDNAME) :: bndname
    logical :: flag_string
    ! formats
 30 format(i10)
    !
    strng = obsrv%IDstring
    ! -- Extract reach number from strng and store it.
    !    If 1st item is not an integer(I4B), it should be a
    !    boundary name--deal with it.
    icol = 1
    ! -- get reach number or boundary name
    if (obsrv%ObsTypeId=='DELAY-HEAD' .or. & 
        obsrv%ObsTypeId=='PRECONSTRESS') then
      call extract_idnum_or_bndname(strng, icol, istart, istop, nn1, bndname)
    else
      nn1 = dis%noder_from_string(icol, istart, istop, inunitobs, &
                                  iout, strng, flag_string)
    end if
    if (nn1 == NAMEDBOUNDFLAG) then
      obsrv%FeatureName = bndname
    else
      if (obsrv%ObsTypeId=='DELAY-HEAD' .or. &
          obsrv%ObsTypeId=='PRECONSTRESS') then
        call extract_idnum_or_bndname(strng, icol, istart, istop, nn2, bndname)
        if (nn2 == NAMEDBOUNDFLAG) then
          obsrv%FeatureName = bndname
          ! -- reset nn1
          nn1 = nn2
        else
          obsrv%NodeNumber2 = nn2
        end if
      end if
    endif
    ! -- store reach number (NodeNumber)
    obsrv%NodeNumber = nn1
    !
    return
  end subroutine csub_process_obsID


end module GwfCsubModule

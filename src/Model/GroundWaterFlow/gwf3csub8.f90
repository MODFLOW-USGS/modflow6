module GwfCsubModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DPREC, DZERO, DEM20, DEM15, DEM10, DEM8, DEM7,     &
                             DEM6, DEM4, DP9, DHALF, DEM1, DONE, DTWO, DTHREE,  &
                             DGRAVITY, DTEN, DHUNDRED, DNODATA, DHNOFLO,        &
                             LENFTYPE, LENPACKAGENAME,                          &
                             LINELENGTH, LENBOUNDNAME, NAMEDBOUNDFLAG,          &
                             LENBUDTXT, LENAUXNAME, LENORIGIN
  use GenericUtilities, only: is_same
  use SmoothingModule,        only: sQuadraticSaturation,                       &
                                    sQuadraticSaturationDerivative
  use NumericalPackageModule, only: NumericalPackageType
  use ObserveModule,        only: ObserveType
  use ObsModule,            only: ObsType, obs_cr
  use BlockParserModule,      only: BlockParserType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use InputOutputModule, only: get_node, extract_idnum_or_bndname, UWWORD
  use BaseDisModule, only: DisBaseType
  use SimModule, only: count_errors, store_error, store_error_unit, ustop
  use ArrayHandlersModule, only: ExpandArray
  use SortModule, only: qsort, selectn
  !
  use TimeSeriesLinkModule,         only: TimeSeriesLinkType
  use TimeSeriesManagerModule,      only: TimeSeriesManagerType, tsmanager_cr
  use ListModule,                   only: ListType
  !
  implicit none
  !
  private
  public :: csub_cr
  public :: GwfCsubType
  !
  character(len=LENBUDTXT), dimension(4) :: budtxt =                            & !text labels for budget terms
      [' CSUB-CGELASTIC',                                                       & 
       '   CSUB-ELASTIC', ' CSUB-INELASTIC',                                    &
       ' CSUB-WATERCOMP']
  character(len=LENBUDTXT), dimension(6) :: comptxt =                           & !text labels for compaction terms
      ['CSUB-COMPACTION', ' CSUB-INELASTIC', '   CSUB-ELASTIC',                 &
       '  CSUB-INTERBED', '    CSUB-COARSE', ' CSUB-ZDISPLACE']
  
  !
  ! -- local parameter - derivative of the log of effective stress
  real(DP), parameter :: dlog10es = 0.4342942_DP
  !
  ! CSUB type
  type, extends(NumericalPackageType) :: GwfCsubType
    character(len=LENBOUNDNAME), dimension(:),                                  &
                                 pointer, contiguous :: boundname => null()      !vector of boundnames
    character(len=LENBOUNDNAME), dimension(:) ,                                 &
                                 pointer, contiguous :: sig0bname => null()      !vector of sig0bnames
    character(len=LENAUXNAME), allocatable, dimension(:) :: auxname              !name for each auxiliary variable
    character(len=500) :: listlabel   = ''                                       !title of table written for RP
    character(len=LENORIGIN) :: stoname
    integer(I4B), pointer :: istounit => null()
    integer(I4B), pointer :: istrainib => null()
    integer(I4B), pointer :: istrainsk => null()
    integer(I4B), pointer :: ioutcomp => null()
    integer(I4B), pointer :: ioutcompi => null()
    integer(I4B), pointer :: ioutcompe => null()
    integer(I4B), pointer :: ioutcompib => null()
    integer(I4B), pointer :: ioutcomps => null()
    integer(I4B), pointer :: ioutzdisp => null()
    integer(I4B), pointer :: iupdatematprop => null()
    integer(I4B), pointer :: istoragec => null()
    integer(I4B), pointer :: icellf => null()
    integer(I4B), pointer :: ispecified_pcs => null()
    integer(I4B), pointer :: ispecified_dbh => null()
    integer(I4B), pointer :: inamedbound => null()                               !flag to read boundnames
    integer(I4B), pointer :: naux => null()                                      !number of auxiliary variables
    integer(I4B), pointer :: ninterbeds => null()
    integer(I4B), pointer :: maxsig0 => null()
    integer(I4B), pointer :: nbound => null()                                    !number of boundaries for current stress period
    integer(I4B), pointer :: ncolbnd => null()                                   !number of columns of the bound array
    integer(I4B), pointer :: iscloc => null()                                    !bound column to scale with SFAC
    integer(I4B), pointer :: iauxmultcol => null()                               !column to use as multiplier for column iscloc
    integer(I4B), pointer :: ndelaycells => null()
    integer(I4B), pointer :: ndelaybeds => null()
    integer(I4B), pointer :: initialized => null()
    integer(I4B), pointer :: ieslag => null()
    integer(I4B), pointer :: ipch => null()
    logical, pointer :: lhead_based => null()
    integer(I4B), pointer :: iupdatestress => null()
    real(DP), pointer :: epsilon => null()                                       !epsilon for stress smoothing
    real(DP), pointer :: cc_crit => null()                                       !convergence criteria for csub-gwf convergence check
    real(DP), pointer :: gammaw => null()                                        !product of fluid density, and gravity
    real(DP), pointer :: beta => null()                                          !water compressibility
    real(DP), pointer :: brg => null()                                           !product of gammaw and water compressibility
    real(DP), pointer :: dbfact => null()
    real(DP), pointer :: dbfacti => null()
    real(DP), pointer :: satomega => null()                                      !newton-raphson saturation omega

    integer(I4B), pointer :: gwfiss => NULL()                                    !pointer to model iss flag
    integer(I4B), pointer :: gwfiss0 => NULL()                                   !iss flag for last stress period
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null()          !pointer to model ibound
    integer(I4B), dimension(:), pointer, contiguous :: stoiconv => null()        !pointer to iconvert in storage
    real(DP), dimension(:), pointer, contiguous :: stosc1 => null()              !pointer to sc1 in storage
    real(DP), dimension(:), pointer, contiguous :: buff => null()                !buff array
    real(DP), dimension(:), pointer, contiguous :: buffusr => null()             !buffusr array
    integer, dimension(:), pointer, contiguous :: nodelist => null()             !reduced node that the interbed is attached to
    integer, dimension(:), pointer, contiguous :: unodelist => null()            !user node that the interbed is attached to
    !
    ! -- coarse-grained storage variables
    real(DP), dimension(:), pointer, contiguous :: sgm => null()                 !specific gravity moist sediments
    real(DP), dimension(:), pointer, contiguous :: sgs => null()                 !specific gravity saturated sediments
    real(DP), dimension(:), pointer, contiguous :: cg_ske_cr => null()           !coarse-grained specified storage
    real(DP), dimension(:), pointer, contiguous :: cg_gs => null()               !geostatic stress for a cell
    real(DP), dimension(:), pointer, contiguous :: cg_es => null()               !coarse-grained (aquifer) effective stress
    real(DP), dimension(:), pointer, contiguous :: cg_es0 => null()              !coarse-grained (aquifer) effective stress for the previous time step
    real(DP), dimension(:), pointer, contiguous :: cg_pcs => null()              !coarse-grained (aquifer) preconsolidation stress
    real(DP), dimension(:), pointer, contiguous :: cg_comp => null()             !coarse-grained (aquifer) incremental compaction
    real(DP), dimension(:), pointer, contiguous :: cg_tcomp => null()            !coarse-grained (aquifer) total compaction
    real(DP), dimension(:), pointer, contiguous :: cg_stor => null()             !coarse-grained (aquifer) storage
    real(DP), dimension(:), pointer, contiguous :: cg_ske => null()              !coarse-grained (aquifer) elastic storage coefficient
    real(DP), dimension(:), pointer, contiguous :: cg_sk => null()               !coarse-grained (aquifer) first storage coefficient
    real(DP), dimension(:), pointer, contiguous :: cg_thickini => null()         !initial coarse-grained (aquifer) thickness
    real(DP), dimension(:), pointer, contiguous :: cg_thetaini => null()         !initial coarse-grained (aquifer) porosity
    real(DP), dimension(:), pointer, contiguous :: cg_thick => null()            !current coarse-grained (aquifer) thickness
    real(DP), dimension(:), pointer, contiguous :: cg_thick0 => null()           !previous coarse-grained (aquifer) thickness
    real(DP), dimension(:), pointer, contiguous :: cg_theta => null()            !current coarse-grained (aquifer) porosity
    real(DP), dimension(:), pointer, contiguous :: cg_theta0 => null()           !previous coarse-grained (aquifer) porosity
    !
    ! -- cell storage variables
    real(DP), dimension(:), pointer, contiguous :: cell_wcstor => null()         !cell water compressibility storage
    real(DP), dimension(:), pointer, contiguous :: cell_thick => null()          !cell compressible material thickness
    !
    ! -- interbed variables
    integer(I4B), dimension(:), pointer, contiguous :: idelay => null()          !0 = nodelay, > 0 = delay
    integer(I4B), dimension(:), pointer, contiguous :: ielastic => null()        !0 = inelastic and elastic, > 0 = elastic
    integer(I4B), dimension(:), pointer, contiguous :: iconvert => null()        !0 = elastic, > 0 = inelastic
    real(DP), dimension(:), pointer, contiguous :: ci => null()                  !compression index
    real(DP), dimension(:), pointer, contiguous :: rci => null()                 !recompression index
    real(DP), dimension(:), pointer, contiguous :: pcs => null()                 !preconsolidation stress
    real(DP), dimension(:), pointer, contiguous :: rnb => null()                 !interbed system material factor
    real(DP), dimension(:), pointer, contiguous :: kv => null()                  !vertical hydraulic conductivity of interbed
    real(DP), dimension(:), pointer, contiguous :: h0 => null()                  !initial head in interbed
    real(DP), dimension(:), pointer, contiguous :: comp => null()                !interbed incremental compaction
    real(DP), dimension(:), pointer, contiguous :: tcomp => null()               !total interbed compaction
    real(DP), dimension(:), pointer, contiguous :: tcompi => null()              !total inelastic interbed compaction
    real(DP), dimension(:), pointer, contiguous :: tcompe => null()              !total elastic interbed compaction
    real(DP), dimension(:), pointer, contiguous :: storagee => null()            !elastic storage
    real(DP), dimension(:), pointer, contiguous :: storagei => null()            !inelastic storage
    real(DP), dimension(:), pointer, contiguous :: ske => null()                 !elastic storage coefficient
    real(DP), dimension(:), pointer, contiguous :: sk => null()                  !first storage coefficient
    real(DP), dimension(:), pointer, contiguous :: thickini => null()            !initial interbed thickness
    real(DP), dimension(:), pointer, contiguous :: thetaini => null()            !initial interbed theta
    real(DP), dimension(:), pointer, contiguous :: thick => null()               !current interbed thickness
    real(DP), dimension(:), pointer, contiguous :: thick0 => null()              !previous interbed thickness
    real(DP), dimension(:), pointer, contiguous :: theta => null()               !current interbed porosity
    real(DP), dimension(:), pointer, contiguous :: theta0 => null()              !previous interbed porosity
    real(DP), dimension(:,:), pointer, contiguous :: auxvar => null()            !auxiliary variable array
    !
    ! -- delay interbed arrays
    integer(I4B), dimension(:,:), pointer, contiguous :: idbconvert => null()    !0 = elastic, > 0 = inelastic
    real(DP), dimension(:), pointer, contiguous :: dbdhmax => null()             !delay bed maximum head change
    real(DP), dimension(:,:), pointer, contiguous :: dbz => null()               !delay bed cell z
    real(DP), dimension(:,:), pointer, contiguous :: dbrelz => null()            !delay bed cell z relative to znode
    real(DP), dimension(:,:), pointer, contiguous :: dbh => null()               !delay bed cell h
    real(DP), dimension(:,:), pointer, contiguous :: dbh0 => null()              !delay bed cell previous h
    real(DP), dimension(:,:), pointer, contiguous :: dbgeo => null()             !delay bed cell geostatic stress
    real(DP), dimension(:,:), pointer, contiguous :: dbes => null()              !delay bed cell effective stress
    real(DP), dimension(:,:), pointer, contiguous :: dbes0 => null()             !delay bed cell previous effective stress
    real(DP), dimension(:,:), pointer, contiguous :: dbpcs => null()             !delay bed cell preconsolidation stress
    real(DP), dimension(:), pointer, contiguous :: dbflowtop => null()           !delay bed flow through interbed top
    real(DP), dimension(:), pointer, contiguous :: dbflowbot => null()           !delay bed flow through interbed bottom
    real(DP), dimension(:,:), pointer, contiguous :: dbdzini => null()           !initial delay bed cell thickness
    real(DP), dimension(:,:), pointer, contiguous :: dbthetaini => null()        !initial delay bed cell porosity
    real(DP), dimension(:,:), pointer, contiguous :: dbdz => null()              !delay bed dz
    real(DP), dimension(:,:), pointer, contiguous :: dbdz0 => null()             !delay bed previous dz
    real(DP), dimension(:,:), pointer, contiguous :: dbtheta => null()           !delay bed cell porosity
    real(DP), dimension(:,:), pointer, contiguous :: dbtheta0 => null()          !delay bed cell previous porosity
    real(DP), dimension(:, :), pointer, contiguous :: dbcomp => null()           !delay bed incremental compaction
    real(DP), dimension(:, :), pointer, contiguous :: dbtcomp => null()          !delay bed total interbed compaction
    !
    ! -- delay interbed solution arrays
    real(DP), dimension(:), pointer, contiguous :: dbal => null()                !delay bed lower diagonal
    real(DP), dimension(:), pointer, contiguous :: dbad => null()                !delay bed diagonal
    real(DP), dimension(:), pointer, contiguous :: dbau => null()                !delay bed upper diagonal
    real(DP), dimension(:), pointer, contiguous :: dbrhs => null()               !delay bed right hand side
    real(DP), dimension(:), pointer, contiguous :: dbdh => null()                !delay bed dh
    real(DP), dimension(:), pointer, contiguous :: dbaw => null()                !delay bed work vector
    !
    ! -- period data
    integer(I4B), dimension(:), pointer, contiguous :: nodelistsig0 => null()    !vector of reduced node numbers
    real(DP), dimension(:,:), pointer, contiguous :: bound => null()             !array of package specific boundary numbers
    real(DP), dimension(:,:), pointer, contiguous :: auxvarsig0 => null()        !auxiliary variable array
    !
    ! -- timeseries
    type(TimeSeriesManagerType), pointer :: TsManager => null()                  ! time series manager
    !
    ! -- observation data
    integer(I4B), pointer :: inobspkg => null()                                  !unit number for obs package
    type(ObsType), pointer :: obs => null()                                      !observation package

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
    procedure, private :: csub_calc_adjes
    procedure, private :: csub_calc_sat
    procedure, private :: csub_calc_sat_derivative
    procedure, private :: csub_calc_sfacts
    procedure, private :: csub_adj_matprop
    procedure, private :: csub_calc_interbed_thickness
    procedure, private :: csub_calc_delay_flow
    procedure, private :: csub_delay_eval
    !
    ! -- stress methods
    procedure, private :: csub_cg_calc_stress
    procedure, private :: csub_cg_chk_stress
    !
    ! -- initial states
    procedure, private :: csub_set_initial_state
    !
    ! -- coarse-grained coarse-grained methods
    procedure, private :: csub_cg_update
    procedure, private :: csub_cg_calc_comp
    procedure, private :: csub_cg_calc_sske
    procedure, private :: csub_cg_fc
    procedure, private :: csub_cg_fn
    procedure, private :: csub_cg_wcomp_fc
    procedure, private :: csub_cg_wcomp_fn
    !
    ! -- interbed methods
    procedure, private :: csub_interbed_fc
    procedure, private :: csub_interbed_fn
    procedure, private :: csub_interbed_wcomp_fc
    procedure, private :: csub_interbed_wcomp_fn
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
    procedure, private :: csub_delay_update
    procedure, private :: csub_delay_calc_dstor
    procedure, private :: csub_delay_fc
    procedure, private :: csub_delay_sln
    procedure, private :: csub_delay_assemble
    !
    ! -- methods for observations
    procedure, public :: csub_obs_supported
    procedure, public :: csub_df_obs
    procedure, private :: csub_rp_obs
    procedure, private :: csub_bd_obs
    !
    ! -- method for time series
    procedure, private :: csub_rp_ts
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
    call mem_allocate(this%maxsig0, 'MAXSIG0', this%origin)
    call mem_allocate(this%nbound, 'NBOUND', this%origin)
    call mem_allocate(this%ncolbnd, 'NCOLBND', this%origin)
    call mem_allocate(this%iscloc, 'ISCLOC', this%origin)
    call mem_allocate(this%iauxmultcol, 'IAUXMULTCOL', this%origin)
    call mem_allocate(this%ndelaycells, 'NDELAYCELLS', this%origin)
    call mem_allocate(this%ndelaybeds, 'NDELAYBEDS', this%origin)
    call mem_allocate(this%initialized, 'INITIALIZED', this%origin)
    call mem_allocate(this%ieslag, 'IESLAG', this%origin)
    call mem_allocate(this%ipch, 'IPCH', this%origin)
    call mem_allocate(this%lhead_based, 'LHEAD_BASED', this%origin)
    call mem_allocate(this%iupdatestress, 'IUPDATESTRESS', this%origin)
    call mem_allocate(this%ispecified_pcs, 'ISPECIFIED_PCS', this%origin)
    call mem_allocate(this%ispecified_dbh, 'ISPECIFIED_DBH', this%origin)
    call mem_allocate(this%inamedbound, 'INAMEDBOUND', this%origin)
    call mem_allocate(this%naux, 'NAUX', this%origin)
    call mem_allocate(this%istoragec, 'ISTORAGEC', this%origin)
    call mem_allocate(this%istrainib, 'ISTRAINIB', this%origin)
    call mem_allocate(this%istrainsk, 'ISTRAINSK', this%origin)
    call mem_allocate(this%ioutcomp, 'IOUTCOMP', this%origin)
    call mem_allocate(this%ioutcompi, 'IOUTCOMPI', this%origin)
    call mem_allocate(this%ioutcompe, 'IOUTCOMPE', this%origin)
    call mem_allocate(this%ioutcompib, 'IOUTCOMPIB', this%origin)
    call mem_allocate(this%ioutcomps, 'IOUTCOMPS', this%origin)
    call mem_allocate(this%ioutzdisp, 'IOUTZDISP', this%origin)
    call mem_allocate(this%iupdatematprop, 'IUPDATEMATPROP', this%origin)
    call mem_allocate(this%epsilon, 'EPSILON', this%origin)
    call mem_allocate(this%cc_crit, 'CC_CRIT', this%origin)
    call mem_allocate(this%gammaw, 'GAMMAW', this%origin)
    call mem_allocate(this%beta, 'BETA', this%origin)
    call mem_allocate(this%brg, 'BRG', this%origin)
    call mem_allocate(this%satomega, 'SATOMEGA', this%origin)
    call mem_allocate(this%icellf, 'ICELLF', this%origin)
    call mem_allocate(this%gwfiss0, 'GWFISS0', this%origin)
    !
    ! -- allocate TS object
    allocate(this%TsManager)
    !
    ! -- Allocate text strings
    allocate(this%auxname(0))
    !
    ! -- initialize values
    this%istounit = 0
    this%inobspkg = 0
    this%ninterbeds = 0
    this%maxsig0 = 0
    this%nbound = 0
    this%ncolbnd = 1
    this%iscloc = 0
    this%iauxmultcol = 0
    this%ndelaycells = 19
    this%ndelaybeds = 0
    this%initialized = 0
    this%ieslag = 0
    this%ipch = 0
    this%lhead_based = .FALSE.
    this%iupdatestress = 1
    this%ispecified_pcs = 0
    this%ispecified_dbh = 0
    this%inamedbound = 0
    this%naux = 0
    this%istoragec = 1
    this%istrainib = 0
    this%istrainsk = 0
    this%ioutcomp = 0
    this%ioutcompi = 0
    this%ioutcompe = 0
    this%ioutcompib = 0
    this%ioutcomps = 0
    this%ioutzdisp = 0
    this%iupdatematprop = 0
    this%epsilon = DZERO
    this%cc_crit = DEM7
    this%gammaw = DGRAVITY * 1000._DP
    this%beta = 4.6512e-10_DP
    this%brg = this%gammaw * this%beta
    !
    ! -- set omega value used for saturation calculations
    if (this%inewton /= 0) then
      this%satomega = DEM6
      this%epsilon = DHALF * DEM6
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

  subroutine csub_cc(this, iend, icnvg, nodes, hnew, hold, hclose, rclose)
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
    real(DP), dimension(nodes), intent(in) :: hold
    real(DP), intent(in) :: hclose
    real(DP), intent(in) :: rclose
    ! -- local
    integer(I4B) :: ifirst
    integer(I4B) :: ib
    integer(I4B) :: node
    integer(I4B) :: idelay
    integer(I4B) :: ihmax
    integer(I4B) :: irmax
    real(DP) :: hmax
    real(DP) :: rmax
    real(DP) :: dh
    real(DP) :: area
    real(DP) :: hcell
    real(DP) :: hcellold
    real(DP) :: snnew
    real(DP) :: snold
    real(DP) :: stoe
    real(DP) :: stoi
    real(DP) :: tled
    real(DP) :: hcof
    real(DP) :: rhs
    real(DP) :: v1
    real(DP) :: v2
    real(DP) :: df
    ! format
02000 format(4x,'CSUB PACKAGE FAILED CONVERGENCE CRITERIA',//,                  &
             4x,'INTERBED MAX. HEAD CHANGE ',1x,'INTERBED MAX. FLOW DIFF',/,    &                           
             4x,2(a10,1x,a15,1x),/,4x,53('-'))
02010 format(4x,2(i10,1x,G15.7,1x))
02020 format(4x,53('-'))
02030 format('CONVERGENCE FAILED AS A RESULT OF CSUB PACKAGE',1x,a)
! --------------------------------------------------------------------------
    ifirst = 1
    if (this%gwfiss == 0) then
      ihmax = 0
      irmax = 0
      hmax = DZERO
      rmax = DZERO
      if (DELT > DZERO) then
        tled = DONE / DELT
      else
        tled = DZERO
      end if
      final_check: do ib = 1, this%ninterbeds
        idelay = this%idelay(ib)
        !
        ! -- skip nodelay interbeds
        if (idelay == 0) cycle
        !
        ! -- evaluate the maximum head change in the interbed
        dh = this%dbdhmax(idelay)
        if (abs(dh) > abs(hmax)) then
          ihmax = ib
          hmax = dh
        end if
        !
        ! -- evaluate difference between storage changes
        !    in the interbed and exchange between the interbed
        !    and the gwf cell
        node = this%nodelist(ib)
        area = this%dis%get_area(node)
        hcell = hnew(node)
        hcellold = hold(node)
        !
        ! -- calculate cell saturation
        call this%csub_calc_sat(node, hcell, hcellold, snnew, snold)
        !
        ! -- calculate the change in storage
        call this%csub_delay_calc_dstor(ib, hcell, stoe, stoi)
        v1 = (stoe + stoi) * area * this%rnb(ib) * snnew * tled
        !
        ! -- calculate the flow between the interbed and the cell
        call this%csub_delay_fc(ib, hcof, rhs)
        v2 = (-hcof * hcell - rhs) * area * this%rnb(ib) * snnew
        !
        ! -- calculate the difference between the interbed change in
        !    storage and the flow between the interbed and the cell
        df = v2 - v1
        !
        ! -- evaluate difference relative to rrmax
        if (abs(df) > abs(rmax)) then
          irmax = ib
          rmax = df
        end if
      end do final_check
      if (abs(hmax) > hclose .or. abs(rmax) > rclose) then
        icnvg = 0
        ! write convergence check information if this is the last outer iteration
        if (iend == 1) then
          write(*,2030) this%name
          write(this%iout, 2000)                                              &
            '  LOCATION', '    HEAD CHANGE',                                  &
            '  LOCATION', 'FLOW DIFFERENCE'
          write(*,2010) ihmax, hmax, irmax, rmax
          write(this%iout,2010) ihmax, hmax, irmax, rmax
        end if
      end if
    end if
    if (icnvg == 0) then
      write(this%iout,2020)
    end if
    !
    ! -- return
    return
  end subroutine csub_cc

  subroutine csub_bdcalc(this, nodes, hnew, hold, isuppress_output,             &
                          model_budget)
! ******************************************************************************
! csub_bd -- calculate budget for coarse-grained storage, interbeds, and water
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
    integer(I4B) :: idelay
    integer(I4B) :: ielastic
    integer(I4B) :: iconvert
    integer(I4B) :: node
    integer(I4B) :: nn
    integer(I4B) :: n
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
    real(DP) :: h0
    real(DP) :: snnew
    real(DP) :: snold
    real(DP) :: hcof
    real(DP) :: rhs
    real(DP) :: stoe
    real(DP) :: stoi
    real(DP) :: b
    real(DP) :: q
    real(DP) :: ratecgin
    real(DP) :: ratecgout
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
    ! -- coarse-grained coarse-grained storage
    ratecgin = DZERO
    ratecgout= DZERO
    do node = 1, this%dis%nodes
      area = this%dis%get_area(node)
      comp = DZERO
      rrate = DZERO
      rratewc = DZERO
      if (this%gwfiss == 0) then
        if (DELT > DZERO) then
          tled = DONE / DELT
        else
          tled = DZERO
        end if
        if (this%ibound(node) > 0 .and. this%cg_thickini(node) > DZERO) then
          !
          ! -- calculate coarse-grained storage terms
          call this%csub_cg_fc(node, tled, area, hnew(node), hold(node),        &
                               hcof, rhs)
          rrate = hcof * hnew(node) - rhs
          !
          ! -- calculate compaction
          call this%csub_cg_calc_comp(node, hnew(node), hold(node), comp)
          !
          ! -- budget terms
          if (rrate < DZERO) then
            ratecgout = ratecgout - rrate
          else
            ratecgin = ratecgin + rrate
          end if
          !
          ! -- calculate coarse-grained water compressibility storage terms
          call this%csub_cg_wcomp_fc(node, tled, area, hnew(node), hold(node),  &
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
      ! -- update coarse-grained storage and water
      !    compresion variables
      this%cg_stor(node) = rrate
      this%cell_wcstor(node) = rratewc
      this%cell_thick(node) = this%cg_thick(node)
      !
      ! -- update incremental coarse-grained compaction
      this%cg_comp(node) = comp
      ! 
      !
      ! -- update states if required
      if (isuppress_output == 0) then
        !
        ! -- calculate strain and change in coarse-grained void ratio and thickness
        if (this%iupdatematprop /= 0) then
          call this%csub_cg_update(node)
        end if
        !
        ! -- update total compaction
        this%cg_tcomp(node) = this%cg_tcomp(node) + comp
      end if
    end do
    !
    ! -- interbed storage
    rateibein = DZERO
    rateibeout = DZERO
    rateibiin = DZERO
    rateibiout = DZERO

    tled = DONE
    do ib = 1, this%ninterbeds
      rratewc = DZERO
      idelay = this%idelay(ib)
      ielastic = this%ielastic(ib)
      if (this%gwfiss == 0) then
        if (DELT > DZERO) then
          tledm = DONE / DELT
        else
          tledm = DZERO
        end if
        node = this%nodelist(ib)
        area = this%dis%get_area(node)
        !
        ! -- skip inactive and constant head cells
        if (this%ibound(node) < 1) cycle
        !
        ! -- no delay interbeds
        if (idelay == 0) then
          iconvert = this%iconvert(ib)
          b = this%thick(ib)
          stoi = DZERO
          !
          ! -- calculate compaction
          call this%csub_nodelay_calc_comp(ib, hnew(node), hold(node), comp,    &
                                           rho1, rho2)
          !
          ! -- interbed stresses
          es = this%cg_es(node)
          pcs = this%pcs(ib)
          es0 = this%cg_es0(node)
          !
          ! -- calculate inelastic and elastic compaction
          if (ielastic > 0 .or. iconvert == 0) then
            stoe = comp
          else
            stoi = -pcs * rho2 + (rho2 * es)
            stoe = pcs * rho1 - (rho1 * es0)
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
            ! -- calculate strain and change in interbed void ratio and thickness
            if (this%iupdatematprop /= 0) then
              call this%csub_nodelay_update(ib)
            end if
            !
            ! -- update total compaction
            this%tcomp(ib) = this%tcomp(ib) + comp
            this%tcompe(ib) = this%tcompe(ib) + compe
            this%tcompi(ib) = this%tcompi(ib) + compi
          end if
          !
          ! -- delay interbeds
        else
          b = this%thick(ib) * this%rnb(ib)
          h = hnew(node)
          h0 = hold(node)
          !
          ! -- calculate cell saturation
          call this%csub_calc_sat(node, h, h0, snnew, snold)
          !
          ! -- calculate inelastic and elastic storage contributions
          call this%csub_delay_calc_dstor(ib, h, stoe, stoi)
          this%storagee(ib) = stoe * area * this%rnb(ib) * snnew * tledm
          this%storagei(ib) = stoi * area * this%rnb(ib) * snnew * tledm
          !
          ! -- calculate flow across the top and bottom of the delay interbed
          q = this%csub_calc_delay_flow(ib, 1, h) * area * this%rnb(ib)
          this%dbflowtop(idelay) = q
          nn = this%ndelaycells
          q = this%csub_calc_delay_flow(ib, nn, h) * area * this%rnb(ib)
          this%dbflowbot(idelay) = q
          !
          ! -- update states if required
          if (isuppress_output == 0) then
            !
            ! -- calculate sum of compaction in delay interbed
            call this%csub_delay_calc_comp(ib, h, h0, comp, compi, compe)
            !
            ! - calculate strain and change in interbed void ratio and thickness
            if (this%iupdatematprop /= 0) then
              call this%csub_delay_update(ib)
            end if
            !
            ! -- update total compaction for interbed
            this%tcomp(ib) = this%tcomp(ib) + comp
            this%tcompi(ib) = this%tcompi(ib) + compi
            this%tcompe(ib) = this%tcompe(ib) + compe
            !
            ! -- update total compaction for each delay bed cell
            do n = 1, this%ndelaycells
              this%dbtcomp(n, idelay) = this%dbtcomp(n, idelay) + this%dbcomp(n, idelay)
            end do
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
        this%cell_wcstor(node) = this%cell_wcstor(node) + rratewc
        this%cell_thick(node) = this%cell_thick(node) + b
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
        if (idelay /= 0) then
          this%dbflowtop(idelay) = DZERO
          this%dbflowbot(idelay) = DZERO
        end if
      end if
    end do
    !
    ! -- Add contributions to model budget 
    !
    ! -- interbed elastic storage
    call model_budget%addentry(ratecgin, ratecgout, delt, budtxt(1),            &
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
      if (count_errors() > 0) then
        call this%parser%StoreErrorUnit()
        call ustop()
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
      ! -- coarse-grained storage (sske)
      call this%dis%record_array(this%cg_stor, this%iout, iprint, -ibinun,      &
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
      call this%dis%record_array(this%cell_wcstor, this%iout, iprint, -ibinun,  &
                                 budtxt(4), cdatafmp, nvaluesp,                 &
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
        this%buff(node) = this%cg_tcomp(node)
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
                                   comptxt(6), cdatafmp, nvaluesp,              &
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
    ! -- Set unit number for binary interbed compaction
    if(this%ioutcompib /= 0) then
      ibinun = this%ioutcompib
    else
      ibinun = 0
    endif
    if(idvfl == 0) ibinun = 0
    !
    ! -- save interbed compaction results
    if(ibinun /= 0) then
      iprint = 0
      dinact = DHNOFLO
      !
      ! -- fill buff with interbed compaction
      do node = 1, this%dis%nodes
        this%buff(node) = DZERO
      end do
      do ib = 1, this%ninterbeds
        node = this%nodelist(ib)
        this%buff(node) = this%buff(node) + this%tcompe(ib) + this%tcompi(ib)
      end do
      !
      ! -- write interbed compaction data to binary file
      call this%dis%record_array(this%buff, this%iout, iprint, ibinun,          &
                                 comptxt(4), cdatafmp, nvaluesp,                &
                                 nwidthp, editdesc, dinact)
    end if
    !
    ! -- Set unit number for binary coarse-grained compaction
    if(this%ioutcomps /= 0) then
      ibinun = this%ioutcomps
    else
      ibinun = 0
    endif
    if(idvfl == 0) ibinun = 0
    !
    ! -- save coarse-grained compaction results
    if(ibinun /= 0) then
      iprint = 0
      dinact = DHNOFLO
      !
      ! -- fill buff with coarse-grained compaction
      do node = 1, this%dis%nodes
        this%buff(node) = this%cg_tcomp(node)
      end do
      !
      ! -- write coarse-grained compaction data to binary file
      call this%dis%record_array(this%buff, this%iout, iprint, ibinun,          &
                                 comptxt(5), cdatafmp, nvaluesp,                &
                                 nwidthp, editdesc, dinact)
    end if
    !
    ! -- Save observations.
    if (this%obs%npakobs > 0) then
      call this%obs%obs_ot()
    end if
    !
    ! -- check that final effective stress values for the time step
    !    are greater than zero
    if (this%gwfiss == 0) then
      call this%csub_cg_chk_stress()
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
    real(DP) :: b0
    real(DP) :: b1
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
        idelay = this%idelay(ib)
        b0 = this%thickini(ib)
        strain = this%tcomp(ib) / b0
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
          b0 = this%thickini(ib)
          b1 = this%csub_calc_interbed_thickness(ib)
          if (idelay == 0) then
            ctype = 'no-delay'
          else
            ctype = 'delay'
            b0 = b0 * this%rnb(ib)
          end if
          strain = this%tcomp(ib) / b0
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
          call UWWORD(line, iloc, 16, 3, text, n, b0)
          call UWWORD(line, iloc, 16, 3, text, n, b1)
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
          b0 = this%thickini(ib)
          b1 = this%csub_calc_interbed_thickness(ib)
          if (idelay == 0) then
            ctype = 'no-delay'
          else
            ctype = 'delay'
            b0 = b0 * this%rnb(ib)
          end if
          strain = this%tcomp(ib) / b0
          pctcomp = DHUNDRED * strain
          node = this%nodelist(ib)
          call this%dis%noder_to_string(node, cellid)
          iloc = 1
          line = ''
          call UWWORD(line, iloc, 20, 2, text, ib, rval, SEP=',')
          call UWWORD(line, iloc, 20, 1, ctype, n, rval, SEP=',')
          call UWWORD(line, iloc, 22, 1, '"'//trim(adjustl(cellid))//'"',       &
                      n, rval, SEP=',')
          call UWWORD(line, iloc, 20, 3, text, n, b0, SEP=',')
          call UWWORD(line, iloc, 20, 3, text, n, b1, SEP=',')
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
      if (this%cg_thickini(node) > DZERO) then
        strain = this%cg_tcomp(node) / this%cg_thickini(node)
      end if
      pctcomp = DHUNDRED * strain
      pctcomp_arr(node) = pctcomp
      if (pctcomp >= DONE) then
        iexceed = iexceed + 1
      end if
    end do
    call selectn(imap_sel, pctcomp_arr, reverse=.TRUE.)
    !
    ! -- summary coarse-grained strain table
    i0 = max(1, this%dis%nodes-ncells+1)
    i1 = this%dis%nodes
    msg = ''
    if (iexceed /= 0) then
      write(msg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                                    &
        '-- LARGEST ', (i1 - i0 + 1), 'OF', this%dis%nodes,                     &
        'CELL COARSE-GRAINED VALUES SHOWN'
    end if
    write (this%iout, 2000) trim(this%name), 'COARSE-GRAINED STRAIN SUMMARY',   &
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
        if (this%cg_thickini(node) > DZERO) then
          strain = this%cg_tcomp(node) / this%cg_thickini(node)
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
        call UWWORD(line, iloc, 16, 3, text, n, this%cg_thickini(node))
        call UWWORD(line, iloc, 16, 3, text, n, this%cg_thick(node))
        call UWWORD(line, iloc, 16, 3, text, n, this%cg_tcomp(node))
        call UWWORD(line, iloc, 16, 3, text, n, strain)
        call UWWORD(line, iloc, 16, 3, text, n, pctcomp)
        call UWWORD(line, iloc, 10, 1, cflag, ib, rval)
        write(this%iout, '(1X,A)') line(1:iloc)
      end do
      write(this%iout, '(/1X,A,1X,I0,1X,A,1X,I0,1X,A,/1X,A,/1X,A)') &
        'COARSE-GRAINED STORAGE PERCENT COMPACTION IS GREATER THAN OR ' //    &
        'EQUAL TO 1 PERCENT IN', iexceed, 'OF', this%dis%nodes, 'CELL(S).',   &
        'USE THE STRAIN_CSV_COARSE OPTION TO OUTPUT A CSV ' //                &
        'FILE WITH PERCENT COMPACTION ', 'VALUES FOR ALL CELLS.'
    else
      msg = 'COARSE-GRAINED STORAGE PERCENT COMPACTION WAS LESS THAN ' //     &
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
        if (this%cg_thickini(node) > DZERO) then
          strain = this%cg_tcomp(node) / this%cg_thickini(node)
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
        call UWWORD(line, iloc, 20, 3, text, n, this%cg_thickini(node),       &
                    SEP=',')
        call UWWORD(line, iloc, 20, 3, text, n, this%cg_thick(node), SEP=',')
        call UWWORD(line, iloc, 20, 3, text, n, this%cg_tcomp(node), SEP=',')
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
    use MemoryManagerModule, only: mem_allocate, mem_setptr
!    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    ! -- dummy
    class(GwfCsubType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: cellid
    character(len=20) :: scellid
    character(len=10) :: text
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: linesep
    character(len=LENBOUNDNAME) :: bndName, bndNameTemp
    character(len=7) :: cdelay
    character(len=9) :: cno
    integer(I4B) :: ival
    logical :: isfound, endOfBlock
    integer(I4B) :: n
    integer(I4B) :: nn
    integer(I4B) :: ib
    integer(I4B) :: itmp
    integer(I4B) :: ierr
    integer(I4B) :: ndelaybeds
    integer(I4B) :: idelay
    integer(I4B) :: iloc
    integer(I4B) :: isep
    real(DP) :: rval
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: baq
    real(DP) :: q
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
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%name))//        &
        ' PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        ! -- read interbed number
        itmp = this%parser%GetInteger()

        if (itmp < 1 .or. itmp > this%ninterbeds) then
          write(errmsg,'(4x,a,1x,i0,1x,a,1x,i0)')                                &
            '****ERROR. INTERBED NUMBER (', itmp, ') MUST BE > 0 and <= ',       &
            this%ninterbeds
          call store_error(errmsg)
          cycle
        end if

        ! -- increment nboundchk
        nboundchk(itmp) = nboundchk(itmp) + 1

        ! -- read cellid
        call this%parser%GetCellid(this%dis%ndim, cellid)
        nn = this%dis%noder_from_cellid(cellid,                                  &
                                        this%parser%iuactive, this%iout)
        n = this%dis%nodeu_from_cellid(cellid,                                   &
                                       this%parser%iuactive, this%iout)
        top = this%dis%top(nn)
        bot = this%dis%bot(nn)
        baq = top - bot
        ! -- determine if a valid cell location was provided
        if (nn < 1) then
          write(errmsg,'(4x,a,1x,i4,1x)')                                        &
            '****ERROR. INVALID cellid FOR PACKAGEDATA ENTRY', itmp
          call store_error(errmsg)
        end if
        
        ! -- set nodelist and unodelist
        this%nodelist(itmp) = nn
        this%unodelist(itmp) = n

        ! -- get cdelay
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

        ! -- get initial preconsolidation stress
        this%pcs(itmp) = this%parser%GetDouble()

        ! -- get thickness or cell fraction
        rval = this%parser%GetDouble()
        if (this%icellf == 0) then
          if (rval < DZERO .or. rval > baq) then
              write(errmsg,'(4x,a,1x,g0,1x,a,1x,g0,1x,a,1x,i0)') &
                '****ERROR. thick (', rval,') MUST BE >= 0 AND <= ', baq,        &
                'FOR PACKAGEDATA ENTRY', itmp
              call store_error(errmsg)
          end if
        else
          if (rval < DZERO .or. rval > DONE) then
              write(errmsg,'(4x,a,1x,i0)') &
                '****ERROR. frac MUST BE >= 0 AND <= 1 FOR PACKAGEDATA ENTRY',   &
                itmp
              call store_error(errmsg)
          end if
          rval = rval * baq
        end if
        this%thickini(itmp) = rval
        if (this%iupdatematprop /= 0) then
          this%thick(itmp) = rval
        end if

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
        !
        ! -- get skv or ci
        rval =  this%parser%GetDouble()
        if (rval < DZERO) then
            write(errmsg,'(4x,a,1x,i0)') &
              '****ERROR. (skv,ci) MUST BE > 0 FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
        end if
        this%ci(itmp) = rval
        !
        ! -- get ske or rci
        rval =  this%parser%GetDouble()
        if (rval < DZERO) then
            write(errmsg,'(4x,a,1x,i0)') &
              '****ERROR. (ske,rci) MUST BE > 0 FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
        end if
        this%rci(itmp) = rval
        !       
        ! -- set ielastic
        if (this%ci(itmp) == this%rci(itmp)) then
          this%ielastic(itmp) = 1
        else
          this%ielastic(itmp) = 0
        end if
        !
        ! -- get porosity
        rval =  this%parser%GetDouble()
        this%thetaini(itmp) = rval
        if (this%iupdatematprop /= 0) then
          this%theta(itmp) = rval
        end if
        if (rval <= DZERO .or. rval > DONE) then
            write(errmsg,'(4x,a,1x,a,1x,i0)') &
              '****ERROR. theta MUST BE > 0 and <= 1 FOR PACKAGEDATA ENTRY',     &
              'ENTRY', itmp
            call store_error(errmsg)
        end if
        !
        ! -- get kv
        rval =  this%parser%GetDouble()
        if (idelay > 0) then
          if (rval <= 0.0) then
             write(errmsg,'(4x,a,1x,i0,1x)') &
               '****ERROR. kv MUST BE > 0 FOR PACKAGEDATA ENTRY', itmp
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
        if (this%inamedbound /= 0) then
          call this%parser%GetStringCaps(bndNameTemp)
          if (bndNameTemp /= '') then
            bndName = bndNameTemp(1:16)
          else
             write(errmsg,'(4x,2(a,1x),i4)')                          &
               '****ERROR. BOUNDNAME MUST BE SPECIFIED FOR ',         &
               'PACKAGEDATA ENTRY', itmp
             call store_error(errmsg)
          end if
        end if
        this%boundname(itmp) = bndName
      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%name))//' PACKAGEDATA'
    !else
    !  call store_error('ERROR.  REQUIRED PACKAGEDATA BLOCK NOT FOUND.')
    endif
    !
    ! -- write summary of interbed data
    if (this%iprpak == 1) then
      write(this%iout, '(//1X,A)') 'INTERBED DATA'
      iloc = 1
      line = ''
      call UWWORD(line, iloc, 10, 1, 'INTERBED', n, q, left=.TRUE.)
      call UWWORD(line, iloc, 20, 1, 'CELLID', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, 'CDELAY', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, 'PCS', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, 'THICK', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, 'RNB', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, 'SSV_CC', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, 'SSE_CR', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, 'THETA', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, 'KV', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 10, 1, 'H0', n, q, CENTER=.TRUE.)
      if (this%inamedbound /= 0) then
        call UWWORD(line, iloc, LENBOUNDNAME, 1,                      &
                    'BOUNDNAME', n, q, LEFT=.TRUE.)
      end if
      linesep = repeat('-', iloc)
      isep = iloc
      write(this%iout, '(1X,A)') line(1:iloc)
      write(this%iout, '(1X,A)') linesep(1:iloc)
      do ib = 1, this%ninterbeds
        iloc = 1
        line = ''
        call UWWORD(line, iloc, 10, 2, text, ib, q, left=.TRUE.)
        call this%dis%noder_to_string(this%nodelist(ib), scellid)
        call UWWORD(line, iloc, 20, 1, scellid, n, q, center=.TRUE.)
        if (this%idelay(ib) == 0) then
          text = 'NODELAY'
        else
          text = 'DELAY'
        end if
        call UWWORD(line, iloc, 10, 1, text, n, q, center=.TRUE.)
        call UWWORD(line, iloc, 10, 3,                                &
                    text, n, this%pcs(ib), center=.TRUE.)
        call UWWORD(line, iloc, 10, 3,                                &
                    text, n, this%thickini(ib), center=.TRUE.)
        call UWWORD(line, iloc, 10, 3,                                &
                    text, n, this%rnb(ib), center=.TRUE.)
        call UWWORD(line, iloc, 10, 3,                                &
                    text, n, this%ci(ib), center=.TRUE.)
        call UWWORD(line, iloc, 10, 3,                                &
                    text, n, this%rci(ib), center=.TRUE.)
        call UWWORD(line, iloc, 10, 3,                                &
                    text, n, this%thetaini(ib), center=.TRUE.)
        if (this%idelay(ib) == 0) then
          text = '-'
          call UWWORD(line, iloc, 10, 1, text, n, q, center=.TRUE.)
          call UWWORD(line, iloc, 10, 1, text, n, q, center=.TRUE.)
        else
          call UWWORD(line, iloc, 10, 3,                              &
                      text, n, this%kv(ib), center=.TRUE.)
          call UWWORD(line, iloc, 10, 3,                              &
                      text, n, this%h0(ib), center=.TRUE.)
        end if
        if (this%inamedbound /= 0) then
          call UWWORD(line, iloc, LENBOUNDNAME, 1,                    &
                      this%boundname(ib), n, q, left=.TRUE.)
        end if
        write(this%iout, '(1X,A)') line(1:iloc)
      end do
      write(this%iout, '(1X,A/)') linesep(1:isep)  
    end if
    !
    ! -- Check to make sure that every interbed is specified and that no 
    !    interbed is specified more than once.
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
      ! -- reallocate and initialize delay interbed arrays
      if (ierr == 0) then
        call mem_allocate(this%idbconvert, this%ndelaycells, ndelaybeds,         & 
                            'idbconvert', trim(this%origin))
        call mem_allocate(this%dbdhmax, ndelaybeds,                              &
                            'dbdhmax', trim(this%origin))
        call mem_allocate(this%dbz, this%ndelaycells, ndelaybeds,                &  
                            'dbz', trim(this%origin))
        call mem_allocate(this%dbrelz, this%ndelaycells, ndelaybeds,             &
                            'dbrelz', trim(this%origin))
        call mem_allocate(this%dbh, this%ndelaycells, ndelaybeds,                &
                            'dbh', trim(this%origin))
        call mem_allocate(this%dbh0, this%ndelaycells, ndelaybeds,               &
                            'dbh0', trim(this%origin))
        call mem_allocate(this%dbgeo, this%ndelaycells, ndelaybeds,              &  
                            'dbgeo', trim(this%origin))
        call mem_allocate(this%dbes, this%ndelaycells, ndelaybeds,               &
                            'dbes', trim(this%origin))
        call mem_allocate(this%dbes0, this%ndelaycells, ndelaybeds,              &
                            'dbes0', trim(this%origin))
        call mem_allocate(this%dbpcs, this%ndelaycells, ndelaybeds,              &
                            'dbpcs', trim(this%origin))
        call mem_allocate(this%dbflowtop, ndelaybeds,                            &
                            'dbflowtop', trim(this%origin))
        call mem_allocate(this%dbflowbot, ndelaybeds,                            &  
                            'dbflowbot', trim(this%origin))
        call mem_allocate(this%dbdzini, this%ndelaycells, ndelaybeds,            &
                            'dbdzini', trim(this%origin))
        call mem_allocate(this%dbthetaini, this%ndelaycells, ndelaybeds,         &  
                            'dbthetaini', trim(this%origin))
        call mem_allocate(this%dbcomp, this%ndelaycells, ndelaybeds,             &
                            'dbcomp', trim(this%origin))
        call mem_allocate(this%dbtcomp, this%ndelaycells, ndelaybeds,            &  
                            'dbtcomp', trim(this%origin))
        !
        ! -- allocate delay bed arrays
        if (this%iupdatematprop == 0) then
          call mem_setptr(this%dbdz, 'dbdzini', trim(this%origin))
          call mem_setptr(this%dbdz0, 'dbdzini', trim(this%origin))
          call mem_setptr(this%dbtheta, 'dbthetaini', trim(this%origin))
          call mem_setptr(this%dbtheta0, 'dbthetaini', trim(this%origin))
        else
          call mem_allocate(this%dbdz, this%ndelaycells, ndelaybeds,             &
                              'dbdz', trim(this%origin))
          call mem_allocate(this%dbdz0, this%ndelaycells, ndelaybeds,            & 
                              'dbdz0', trim(this%origin))  
          call mem_allocate(this%dbtheta, this%ndelaycells, ndelaybeds,          & 
                              'dbtheta', trim(this%origin))
          call mem_allocate(this%dbtheta0, this%ndelaycells, ndelaybeds,         & 
                              'dbtheta0', trim(this%origin))
        end if
        !
        ! -- allocate delay interbed solution arrays
        call mem_allocate(this%dbal, this%ndelaycells,                           &
                            'dbal', trim(this%origin))
        call mem_allocate(this%dbad, this%ndelaycells,                           &
                            'dbad', trim(this%origin))
        call mem_allocate(this%dbau, this%ndelaycells,                           &
                            'dbau', trim(this%origin))
        call mem_allocate(this%dbrhs, this%ndelaycells,                          & 
                            'dbrhs', trim(this%origin))
        call mem_allocate(this%dbdh, this%ndelaycells,                           & 
                            'dbdh', trim(this%origin))
        call mem_allocate(this%dbaw, this%ndelaycells,                           &
                            'dbaw', trim(this%origin))
        !
        ! -- initialize delay bed storage
        do ib = 1, this%ninterbeds
          idelay = this%idelay(ib)
          if (idelay == 0) then
            cycle
          end if
          !
          ! -- initialize delay interbed variables
          do n = 1, this%ndelaycells
            rval = this%thickini(ib) / real(this%ndelaycells, DP)
            this%dbdzini(n, idelay) = rval
            this%dbh(n, idelay) = this%h0(ib)
            this%dbh0(n, idelay) = this%h0(ib)
            this%dbthetaini(n, idelay) = this%thetaini(ib)
            this%dbgeo(n, idelay) = DZERO
            this%dbes(n, idelay) = DZERO
            this%dbes0(n, idelay) = DZERO
            this%dbpcs(n, idelay) = this%pcs(ib)
            this%dbcomp(n, idelay) = DZERO
            this%dbtcomp(n, idelay) = DZERO
            if (this%iupdatematprop /= 0) then
              this%dbdz(n, idelay) = this%dbdzini(n, idelay)
              this%dbdz0(n, idelay) = this%dbdzini(n, idelay)
              this%dbtheta(n, idelay) = this%theta(ib)
              this%dbtheta0(n, idelay) = this%theta(ib)
            end if
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
    ! -- check that ndelaycells is odd when using
    !    the effective stress formulation
    if (ndelaybeds > 0) then
      q = MOD(real(this%ndelaycells, DP), DTWO)
      if (q == DZERO) then
        write(errmsg, '(a,1x,i0,2(1x,a))')                          &
          'ERROR: NDELAYCELLS (', this%ndelaycells, ') MUST BE AN', &
          'ODD NUMBER WHEN USING THE EFFECTIVE STRESS FORMULATION.'
        call store_error(errmsg)
      end if
    end if
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
    !character(len=LINELENGTH) :: cvalue
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
    integer(I4B) :: ieslag
    integer(I4B) :: isetgamma
    ! -- formats
    character(len=*), parameter :: fmtts = &
      "(4x, 'TIME-SERIES DATA WILL BE READ FROM FILE: ', a)"
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
    ieslag = 0
    isetgamma = 0
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
              ' BOUNDARIES HAVE NAMES IN LAST COLUMN.'          ! user specified boundnames
          case ('TS6')
            call this%parser%GetStringCaps(keyword)
            if(trim(adjustl(keyword)) /= 'FILEIN') then
              errmsg = 'TS6 keyword must be followed by "FILEIN" ' //          &
                       'then by filename.'
              call store_error(errmsg)
              call this%parser%StoreErrorUnit()
              call ustop()
            endif
            call this%parser%GetString(fname)
            write(this%iout,fmtts)trim(fname)
            call this%TsManager%add_tsfile(fname, this%inunit)
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
          case ('GAMMAW')
            this%gammaw =  this%parser%GetDouble()
            ibrg = 1
          case ('BETA')
            this%beta =  this%parser%GetDouble()
            ibrg = 1
          case ('HEAD_BASED')
            this%ipch = 1
            this%lhead_based = .TRUE.
          case ('INITIAL_PRECONSOLIDATION_HEAD')
            this%ipch = 1
          case ('NDELAYCELLS')
            this%ndelaycells =  this%parser%GetInteger()
          !
          ! -- compression indicies (CR amd CC) will be specified instead of 
          !    storage coefficients (SSE and SSV) 
          case ('COMPRESSION_INDICES')
            this%istoragec = 0
          !
          ! -- variable thickness and void ratio
          case ('UPDATE_MATERIAL_PROPERTIES')
            this%iupdatematprop = 1
          !
          ! -- cell fraction will be specified instead of interbed thickness
          case ('CELL_FRACTION')
            this%icellf = 1
          !
          ! -- specified initial pcs and delay bed heads
          case ('SPECIFIED_INITIAL_INTERBED_STATE')
            this%ispecified_pcs = 1
            this%ispecified_dbh = 1
          !
          ! -- specified initial pcs
          case ('SPECIFIED_INITIAL_PRECONSOLIDATION_STRESS')
            this%ispecified_pcs = 1
          !
          ! -- specified initial delay bed heads
          case ('SPECIFIED_INITIAL_DELAY_HEAD')
            this%ispecified_dbh = 1
          !
          ! -- lag the effective stress used to calculate storage properties
          case ('EFFECTIVE_STRESS_LAG')
            ieslag = 1
          !
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
          case ('STRAIN_CSV_COARSE')
            call this%parser%GetStringCaps(keyword)
            if (keyword == 'FILEOUT') then
              call this%parser%GetString(fname)
              this%istrainsk = getunit()
              call openfile(this%istrainsk, this%iout, fname, 'CSV_OUTPUT',     &
                            filstat_opt='REPLACE')
              write(this%iout,fmtfileout) &
                'COARSE STRAIN CSV', fname, this%istrainsk
            else
              errmsg = 'OPTIONAL STRAIN_CSV_COARSE KEYWORD MUST BE ' //         &
                       'FOLLOWED BY FILEOUT'
              call store_error(errmsg)
            end if
          !
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
          case ('COMPACTION_INTERBED')
            call this%parser%GetStringCaps(keyword)
            if (keyword == 'FILEOUT') then
              call this%parser%GetString(fname)
              this%ioutcompib = getunit()
              call openfile(this%ioutcompib, this%iout, fname,                  &
                            'DATA(BINARY)', form, access, 'REPLACE')
              write(this%iout,fmtfileout) &
                'COMPACTION_INTERBED', fname, this%ioutcompib
            else 
              errmsg = 'OPTIONAL COMPACTION_INTERBED KEYWORD MUST BE ' //       &
                       'FOLLOWED BY FILEOUT'
              call store_error(errmsg)
            end if
          case ('COMPACTION_COARSE')
            call this%parser%GetStringCaps(keyword)
            if (keyword == 'FILEOUT') then
              call this%parser%GetString(fname)
              this%ioutcomps = getunit()
              call openfile(this%ioutcomps, this%iout, fname,                   &
                            'DATA(BINARY)', form, access, 'REPLACE')
              write(this%iout,fmtfileout) &
                'COMPACTION_COARSE', fname, this%ioutcomps
            else 
              errmsg = 'OPTIONAL COMPACTION_COARSE KEYWORD MUST BE ' //         &
                       'FOLLOWED BY FILEOUT'
              call store_error(errmsg)
            end if
          !
          ! -- zdisplacement output
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
          !
          ! -- right now these are options that are only available in the
          !    development version and are not included in the documentation.
          !    These options are only available when IDEVELOPMODE in
          !    constants module is set to 1
          
          !
          ! default case
          case default
            write(errmsg,'(4x,a,3(1x,a))') '****ERROR. UNKNOWN ',                &
                                           trim(adjustl(this%name)),             &
                                           'OPTION: ', trim(keyword)
            call store_error(errmsg)
        end select
      end do
      write(this%iout,'(1x,a)') 'END OF ' //                                     &
                                trim(adjustl(this%name)) // ' OPTIONS'
    end if
    !
    ! -- write messages for options
    write(this%iout, '(//2(1X,A))') trim(adjustl(this%name)),                    &
                                    'PACKAGE SETTINGS'
    write(this%iout, fmtopti) 'NUMBER OF DELAY CELLS =',                         &
                              this%ndelaycells
    if (this%lhead_based .EQV. .TRUE.) then
      write(this%iout, '(4x,a)') &
        'HEAD-BASED FORMULATION'
    else
      write(this%iout, '(4x,a)') &
        'EFFECTIVE-STRESS FORMULATION'
    end if
    if (this%istoragec == 0) then
      write(this%iout, '(4x,a,1(/,6x,a))') &
        'COMPRESSION INDICES WILL BE SPECIFIED INSTEAD OF ELASTIC AND',          &
        'INELASTIC SPECIFIC STORAGE COEFFICIENTS'
    else
      write(this%iout, '(4x,a,1(/,6x,a))') &
        'ELASTIC AND INELASTIC SPECIFIC STORAGE COEFFICIENTS WILL BE ',          &
        'SPECIFIED'
    end if
    if (this%iupdatematprop /= 1) then
      write(this%iout, '(4x,a,1(/,6x,a))') &
        'THICKNESS AND VOID RATIO WILL NOT BE ADJUSTED DURING THE',              &
        'SIMULATION'
    else
      write(this%iout, '(4x,a)') &
        'THICKNESS AND VOID RATIO WILL BE ADJUSTED DURING THE SIMULATION'
    end if
    if (this%icellf /= 1) then
      write(this%iout, '(4x,a)') &
        'INTERBED THICKNESS WILL BE SPECIFIED AS A THICKNESS'
    else
      write(this%iout,'(4x,a,1(/,6x,a))') &
        'INTERBED THICKNESS WILL BE SPECIFIED AS A AS A CELL FRACTION'
    end if
    if (this%ispecified_pcs /= 1) then
      if (this%ipch /= 0) then
        write(this%iout, '(4x,a,1(/,6x,a))') &
          'PRECONSOLIDATION HEAD WILL BE SPECIFIED RELATIVE TO INITIAL',         &
          'STRESS CONDITIONS'
      else
        write(this%iout, '(4x,a,1(/,6x,a))') &
          'PRECONSOLIDATION STRESS WILL BE SPECIFIED RELATIVE TO INITIAL',       &
          'STRESS CONDITIONS'
      end if
    else
      if (this%ipch /= 0) then
        write(this%iout, '(4x,a,1(/,6x,a))') &
          'PRECONSOLIDATION HEAD WILL BE SPECIFIED AS ABSOLUTE VALUES',          &
          'INSTEAD OF RELATIVE TO INITIAL HEAD CONDITIONS'
      else
        write(this%iout, '(4x,a,1(/,6x,a))') &
          'PRECONSOLIDATION STRESS WILL BE SPECIFIED AS ABSOLUTE VALUES',        &
          'INSTEAD OF RELATIVE TO INITIAL STRESS CONDITIONS'
      end if
    end if
    if (this%ispecified_dbh /= 1) then
      write(this%iout, '(4x,a,1(/,6x,a))') &
        'DELAY INTERBED HEADS WILL BE SPECIFIED RELATIVE TO INITIAL ',           &
        'GWF HEADS'
    else
      write(this%iout, '(4x,a,1(/,6x,a))') &
        'DELAY INTERBED HEADS WILL BE SPECIFIED AS ABSOLUTE VALUES INSTEAD',      &
        'OF RELATIVE TO INITIAL GWF HEADS'
    end if
    !
    ! -- process effective_stress_lag, if effective stress formulation
    if (this%lhead_based .EQV. .FALSE.) then
      if (ieslag /= 0) then
        write(this%iout, '(4x,a,1(/,6x,a))') &
          'SPECIFIC STORAGE VALUES WILL BE CALCULATED USING THE EFFECTIVE',      &
          'STRESS FROM THE PREVIOUS TIME STEP'
      else
        write(this%iout, '(4x,a,1(/,6x,a))') &
          'SPECIFIC STORAGE VALUES WILL BE CALCULATED USING THE CURRENT',        & 
          'EFFECTIVE STRESS'
      end if
    else
      if (ieslag /= 0) then
        ieslag = 0
        write(this%iout, '(4x,a,2(/,6x,a))') &
          'EFFECTIVE_STRESS_LAG HAS BEEN SPECIFIED BUT HAS NO EFFECT WHEN USING',&
          'THE HEAD-BASED FORMULATION (HEAD_BASED HAS BEEN SPECIFIED IN THE',    &
          'OPTIONS BLOCK)'
      end if
    end if
    this%ieslag = ieslag 
    !
    ! -- recalculate BRG if necessary and output 
    !    water compressibility values
    if (ibrg /= 0) then
      this%brg = this%gammaw * this%beta
    end if
    write(this%iout, fmtoptr) 'GAMMAW        =', this%gammaw
    write(this%iout, fmtoptr) 'BETA          =', this%beta
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
    integer(I4B) :: ilen
    integer(I4B) :: naux

    ! -- grid based data
    if (this%ioutcomp == 0 .and. this%ioutcompi == 0 .and.                      &
        this%ioutcompe == 0 .and. this%ioutcompib == 0 .and.                    &
        this%ioutcomps == 0 .and. this%ioutzdisp == 0) then
      call mem_allocate(this%buff, 1, 'BUFF', trim(this%origin))
    else
      call mem_allocate(this%buff, this%dis%nodes, 'BUFF', trim(this%origin))
    end if
    if (this%ioutcomp == 0 .and. this%ioutzdisp == 0) then
      call mem_allocate(this%buffusr, 1, 'buffusr', trim(this%origin))
    else
      call mem_allocate(this%buffusr, this%dis%nodesuser, 'buffusr',             &
                        trim(this%origin))
    end if
    call mem_allocate(this%sgm, this%dis%nodes, 'sgm', trim(this%origin))
    call mem_allocate(this%sgs, this%dis%nodes, 'sgs', trim(this%origin))
    call mem_allocate(this%cg_ske_cr, this%dis%nodes, 'cg_ske_cr',               &
                      trim(this%origin))
    call mem_allocate(this%cg_es, this%dis%nodes, 'cg_es', trim(this%origin))
    call mem_allocate(this%cg_es0, this%dis%nodes, 'cg_es0', trim(this%origin))
    call mem_allocate(this%cg_pcs, this%dis%nodes, 'cg_pcs', trim(this%origin))
    call mem_allocate(this%cg_comp, this%dis%nodes, 'cg_comp', trim(this%origin))
    call mem_allocate(this%cg_tcomp, this%dis%nodes, 'cg_tcomp',                 & 
                      trim(this%origin))
    call mem_allocate(this%cg_stor, this%dis%nodes, 'cg_stor', trim(this%origin))
    call mem_allocate(this%cg_ske, this%dis%nodes, 'cg_ske', trim(this%origin))
    call mem_allocate(this%cg_sk, this%dis%nodes, 'cg_sk', trim(this%origin))
    call mem_allocate(this%cg_thickini, this%dis%nodes, 'cg_thickini',           & 
                      trim(this%origin))
    call mem_allocate(this%cg_thetaini, this%dis%nodes, 'cg_thetaini',           & 
                      trim(this%origin))
    if (this%iupdatematprop == 0) then
      call mem_setptr(this%cg_thick, 'cg_thickini', trim(this%origin))
      call mem_setptr(this%cg_thick0, 'cg_thickini', trim(this%origin))
      call mem_setptr(this%cg_theta, 'cg_thetaini', trim(this%origin))
      call mem_setptr(this%cg_theta0, 'cg_thetaini', trim(this%origin))
    else
      call mem_allocate(this%cg_thick, this%dis%nodes, 'cg_thick',               &
                        trim(this%origin))
      call mem_allocate(this%cg_thick0, this%dis%nodes, 'cg_thick0',             &
                        trim(this%origin))
      call mem_allocate(this%cg_theta, this%dis%nodes, 'cg_theta',               &
                        trim(this%origin))
      call mem_allocate(this%cg_theta0, this%dis%nodes, 'cg_theta0',             &
                        trim(this%origin))
    end if
    !
    ! -- cell storage data
    call mem_allocate(this%cell_wcstor, this%dis%nodes, 'cell_wcstor',           &
                      trim(this%origin))
    call mem_allocate(this%cell_thick, this%dis%nodes, 'cell_thick',             &
                      trim(this%origin))
    !
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
    call mem_allocate(this%cg_gs, this%dis%nodes, 'cg_gs', trim(this%origin))
    call mem_allocate(this%pcs, iblen, 'pcs', trim(this%origin))
    call mem_allocate(this%rnb, iblen, 'rnb', trim(this%origin))
    call mem_allocate(this%kv, iblen, 'kv', trim(this%origin))
    call mem_allocate(this%h0, iblen, 'h0', trim(this%origin))
    call mem_allocate(this%ci, iblen, 'ci', trim(this%origin))
    call mem_allocate(this%rci, iblen, 'rci', trim(this%origin))
    call mem_allocate(this%idelay, iblen, 'idelay', trim(this%origin))
    call mem_allocate(this%ielastic, iblen, 'ielastic', trim(this%origin))
    call mem_allocate(this%iconvert, iblen, 'iconvert', trim(this%origin))
    call mem_allocate(this%comp, iblen, 'comp', trim(this%origin))
    call mem_allocate(this%tcomp, iblen, 'tcomp', trim(this%origin))
    call mem_allocate(this%tcompi, iblen, 'tcompi', trim(this%origin))
    call mem_allocate(this%tcompe, iblen, 'tcompe', trim(this%origin))
    call mem_allocate(this%storagee, iblen, 'storagee', trim(this%origin))
    call mem_allocate(this%storagei, iblen, 'storagei', trim(this%origin))
    call mem_allocate(this%ske, iblen, 'ske', trim(this%origin))
    call mem_allocate(this%sk, iblen, 'sk', trim(this%origin))
    call mem_allocate(this%thickini, iblen, 'thickini', trim(this%origin))
    call mem_allocate(this%thetaini, iblen, 'thetaini', trim(this%origin))
    if (this%iupdatematprop == 0) then
      call mem_setptr(this%thick, 'thickini', trim(this%origin))
      call mem_setptr(this%thick0, 'thickini', trim(this%origin))
      call mem_setptr(this%theta, 'thetaini', trim(this%origin))
      call mem_setptr(this%theta0, 'thetaini', trim(this%origin))
    else
      call mem_allocate(this%thick, iblen, 'thick', trim(this%origin))
      call mem_allocate(this%thick0, iblen, 'thick0', trim(this%origin))
      call mem_allocate(this%theta, iblen, 'theta', trim(this%origin))
      call mem_allocate(this%theta0, iblen, 'theta0', trim(this%origin))
    end if
    !
    ! -- delay bed storage - allocated in csub_read_packagedata
    !    after number of delay beds is defined
    !
    ! -- allocate boundname
    allocate(this%boundname(this%ninterbeds))
    !
    ! -- allocate the nodelist and bound arrays
    if (this%maxsig0 > 0) then
      ilen = this%maxsig0
    else
      ilen = 1
    end if
    call mem_allocate(this%nodelistsig0, ilen, 'NODELISTSIG0', this%origin)
    this%nodelistsig0 = 0
    call mem_allocate(this%bound, this%ncolbnd, ilen, 'BOUND',                  &
                      this%origin)
    call mem_allocate(this%auxvarsig0, this%naux, ilen, 'AUXVARSIG0',           &
                      this%origin)
    !
    ! -- Allocate sig0boundname
    allocate(this%sig0bname(ilen))
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
      this%cg_gs(n) = DZERO
      this%cg_es(n) = DZERO
      this%cg_comp(n) = DZERO
      this%cg_tcomp(n) = DZERO
      this%cell_wcstor(n) = DZERO
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
      call mem_deallocate(this%ielastic)
      call mem_deallocate(this%iconvert)
      !
      ! -- grid-based storage data
      call mem_deallocate(this%buff)
      call mem_deallocate(this%buffusr)
      call mem_deallocate(this%sgm)
      call mem_deallocate(this%sgs)
      call mem_deallocate(this%cg_ske_cr)
      !call mem_deallocate(this%cg_theta)
      !call mem_deallocate(this%cg_thick)
      call mem_deallocate(this%cg_gs)
      call mem_deallocate(this%cg_es)
      call mem_deallocate(this%cg_es0)
      call mem_deallocate(this%cg_pcs)
      call mem_deallocate(this%cg_comp)
      call mem_deallocate(this%cg_tcomp)
      call mem_deallocate(this%cg_stor)
      call mem_deallocate(this%cg_ske)
      call mem_deallocate(this%cg_sk)
      if (this%iupdatematprop == 0) then
        nullify(this%cg_thick)
        nullify(this%cg_thick0)
        nullify(this%cg_theta)
        nullify(this%cg_theta0)
      else
        call mem_deallocate(this%cg_thick)
        call mem_deallocate(this%cg_thick0)
        call mem_deallocate(this%cg_theta)
        call mem_deallocate(this%cg_theta0)
      end if
      call mem_deallocate(this%cg_thickini)
      call mem_deallocate(this%cg_thetaini)
      !
      ! -- cell storage
      call mem_deallocate(this%cell_wcstor)
      call mem_deallocate(this%cell_thick)
      !
      ! -- interbed storage
      deallocate(this%boundname)
      deallocate(this%sig0bname)
      deallocate(this%auxname)
      call mem_deallocate(this%auxvar)
      call mem_deallocate(this%ci)
      call mem_deallocate(this%rci)
      call mem_deallocate(this%pcs)
      !call mem_deallocate(this%thick)
      !call mem_deallocate(this%theta)
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
      if (this%iupdatematprop == 0) then
        nullify(this%thick)
        nullify(this%thick0)
        nullify(this%theta)
        nullify(this%theta0)
      else
        call mem_deallocate(this%thick)
        call mem_deallocate(this%thick0)
        call mem_deallocate(this%theta)
        call mem_deallocate(this%theta0)
      end if
      call mem_deallocate(this%thickini)
      call mem_deallocate(this%thetaini)
      !
      ! -- delay bed storage
      if (this%ndelaybeds > 0) then
        if (this%iupdatematprop == 0) then
          nullify(this%dbdz)
          nullify(this%dbdz0)
          nullify(this%dbtheta)
          nullify(this%dbtheta0)
        else
          call mem_deallocate(this%dbdz)
          call mem_deallocate(this%dbdz0)
          call mem_deallocate(this%dbtheta)
          call mem_deallocate(this%dbtheta0)
        end if
        call mem_deallocate(this%idbconvert)
        call mem_deallocate(this%dbdhmax)
        call mem_deallocate(this%dbz)
        call mem_deallocate(this%dbrelz)
        call mem_deallocate(this%dbh)
        call mem_deallocate(this%dbh0)
        call mem_deallocate(this%dbgeo)
        call mem_deallocate(this%dbes)
        call mem_deallocate(this%dbes0)
        call mem_deallocate(this%dbpcs)
        call mem_deallocate(this%dbflowtop)
        call mem_deallocate(this%dbflowbot)
        call mem_deallocate(this%dbdzini)
        call mem_deallocate(this%dbthetaini)
        call mem_deallocate(this%dbcomp)
        call mem_deallocate(this%dbtcomp)
        !
        ! -- delay interbed solution arrays
        call mem_deallocate(this%dbal)
        call mem_deallocate(this%dbad)
        call mem_deallocate(this%dbau)
        call mem_deallocate(this%dbrhs)
        call mem_deallocate(this%dbdh)
        call mem_deallocate(this%dbaw)
      end if
      !
      ! -- period data
      call mem_deallocate(this%nodelistsig0)
      call mem_deallocate(this%bound)
      call mem_deallocate(this%auxvarsig0)
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
    call mem_deallocate(this%maxsig0)
    call mem_deallocate(this%nbound)
    call mem_deallocate(this%ncolbnd)
    call mem_deallocate(this%iscloc)
    call mem_deallocate(this%iauxmultcol)
    call mem_deallocate(this%ndelaycells)
    call mem_deallocate(this%ndelaybeds)
    call mem_deallocate(this%initialized)
    call mem_deallocate(this%ieslag)
    call mem_deallocate(this%ipch)
    call mem_deallocate(this%lhead_based)
    call mem_deallocate(this%iupdatestress)
    call mem_deallocate(this%ispecified_pcs)
    call mem_deallocate(this%ispecified_dbh)
    call mem_deallocate(this%inamedbound)
    call mem_deallocate(this%naux)
    call mem_deallocate(this%istoragec)
    call mem_deallocate(this%istrainib)
    call mem_deallocate(this%istrainsk)
    call mem_deallocate(this%ioutcomp)
    call mem_deallocate(this%ioutcompi)
    call mem_deallocate(this%ioutcompe)
    call mem_deallocate(this%ioutcompib)
    call mem_deallocate(this%ioutcomps)
    call mem_deallocate(this%ioutzdisp)
    call mem_deallocate(this%iupdatematprop)
    call mem_deallocate(this%epsilon)
    call mem_deallocate(this%cc_crit)
    call mem_deallocate(this%gammaw)
    call mem_deallocate(this%beta)
    call mem_deallocate(this%brg)
    call mem_deallocate(this%satomega)
    call mem_deallocate(this%icellf)
    call mem_deallocate(this%gwfiss0)
    !
    ! -- deallocate methods on objects
    if(this%inunit > 0) then
      call this%obs%obs_da()
      call this%TsManager%da()
      !
      ! -- deallocate objects
      deallocate(this%obs)
      deallocate(this%TsManager)
    end if
    !
    ! -- nullify TsManager
    nullify(this%TsManager)
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
          case ('NINTERBEDS')
            this%ninterbeds = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)')'NINTERBEDS = ', this%ninterbeds
          case ('MAXSIG0')
            this%maxsig0 = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)')'MAXSIG0 = ', this%maxsig0
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
        'ERROR:  ninterbeds WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.'
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
    implicit none
    ! -- dummy
    class(GwfCsubType),intent(inout) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer, contiguous :: ibound
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
    integer(I4B) :: istoerr
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: thick
    real(DP) :: cg_ske_cr
    real(DP) :: theta
    real(DP) :: v
    ! -- format
    character(len=*), parameter :: fmtcsub =                                   &
      "(1x,/1x,'CSUB -- COMPACTION PACKAGE, VERSION 1, 12/15/2019',            &
     &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the csub package.
    write(this%iout, fmtcsub) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis     => dis
    this%ibound  => ibound
    !
    ! -- Create time series managers
    call tsmanager_cr(this%TsManager, this%iout)
    !
    ! -- create obs package
    call obs_cr(this%obs, this%inobspkg)
    !
    ! -- Read csub options
    call this%read_options()
    !
    ! -- Now that time series will have been read, need to call the df
    !    routine to define the manager
    call this%tsmanager%tsmanager_df()
    !
    ! -- Read the csub dimensions
    call this%read_dimensions()
    !
    ! - observation data
    call this%obs%obs_ar()
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
        case ('CG_SKE_CR')
          call this%dis%read_grid_array(line, lloc, istart, istop,              &
                                        this%iout, this%parser%iuactive,        &
                                        this%cg_ske_cr, 'CG_SKE_CR')
          iske = 1
        case ('CG_THETA')
          call this%dis%read_grid_array(line, lloc, istart, istop,              &
                                        this%iout, this%parser%iuactive,        &
                                        this%cg_thetaini, 'CG_THETA')
          istheta = 1
        case ('SGM')
            call this%dis%read_grid_array(line, lloc, istart, istop,          &
                                          this%iout, this%parser%iuactive,    &
                                          this%sgm, 'SGM')
            isgm = 1
        case ('SGS')
            call this%dis%read_grid_array(line, lloc, istart, istop,          &
                                          this%iout, this%parser%iuactive,    &
                                          this%sgs, 'SGS')
            isgs = 1
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
    ! -- detemine if cg_ske and cg_theta have been specified
    if (iske == 0) then
      write(errmsg,'(4x,a)') 'ERROR. cg_SKE GRIDDATA MUST BE SPECIFIED'
      call store_error(errmsg)
    end if
    if (istheta == 0) then
      write(errmsg,'(4x,a)') 'ERROR. cg_THETA GRIDDATA MUST BE SPECIFIED'
      call store_error(errmsg)
    end if
    !
    ! -- determine if sgm and sgs have been specified, if not assign default values
    if (isgm == 0) then
      do node = 1, this%dis%nodes
        this%sgm(node) = 1.7D0
      end do
    end if
    if (isgs == 0) then
      do node = 1, this%dis%nodes
        this%sgs(node) = 2.0D0
      end do
    end if
    !
    ! -- evaluate the coarse-grained material properties and if 
    !    non-zero specific storage values are specified in the 
    !    STO package
    istoerr = 0
    do node = 1, this%dis%nodes
      call this%dis%noder_to_string(node, cellid)
      cg_ske_cr = this%cg_ske_cr(node)
      theta = this%cg_thetaini(node)
      !
      ! -- coarse-grained storage error condition
      if (cg_ske_cr < DZERO) then
        write(errmsg,'(4x,a,g0,a,1x,a,1x,a)') &
          'ERROR. COARSE-GRAINED MATERIAL cg_ske_cr (', cg_ske_cr, ') IS LESS',        &
          'THAN ZERO IN CELL', trim(adjustl(cellid))
      end if
      !
      ! -- storage (STO) package error condition
      if (this%stosc1(node) /= DZERO) then
        istoerr = 1
      end if
      !
      ! -- porosity error condition
      if (theta > DONE .or. theta < DZERO) then
        write(errmsg,'(4x,a,g0,a,1x,a,1x,a)') &
          'ERROR. COARSE-GRAINED MATERIAL THETA (', theta, ') IS LESS',          &
          'THAN ZERO OR GREATER THAN 1 IN CELL', trim(adjustl(cellid))
      end if
    end do
    !
    ! -- write single message if storage (STO) package has non-zero specific
    !    storage values
    if (istoerr /= 0) then
      write(errmsg,'(4x,a,3(1x,a))') &
        'ERROR. SPECIFIC STORAGE VALUES IN THE STORAGE (STO) PACKAGE MUST',      &
        'BE ZERO IN ALL ACTIVE CELLS WHEN USING THE', trim(adjustl(this%name)),  &
        'PACKAGE'
      call store_error(errmsg)
    end if
    !
    ! -- read interbed data
    if (this%ninterbeds > 0) then
      call this%csub_read_packagedata()
    end if
    !
    ! -- calculate the coarse-grained material thickness without the interbeds
    do node = 1, this%dis%nodes
      top = this%dis%top(node)
      bot = this%dis%bot(node)
      this%cg_thickini(node) = top - bot
    end do
    !
    ! -- subtract the interbed thickness from aquifer thickness
    do ib = 1, this%ninterbeds
      node = this%nodelist(ib)
      idelay = this%idelay(ib)
      if (idelay == 0) then
        v = this%thickini(ib)
      else
        v = this%rnb(ib) * this%thickini(ib)
      end if
      this%cg_thickini(node) = this%cg_thickini(node) - v
    end do
    !
    ! -- evaluate if any cg_thick values are less than 0
    do node = 1, this%dis%nodes
      thick = this%cg_thickini(node)
      if (thick < DZERO) then
        call this%dis%noder_to_string(node, cellid)
        write(errmsg,'(4x,a,1x,g0,a,1x,a,1x,a)')                                &
          'ERROR. AQUIFER THICKNESS IS LESS THAN ZERO (',                       &
           thick, ')', 'in cell', trim(adjustl(cellid))
        call store_error(errmsg)
      end if
    end do
    !
    ! -- terminate if errors griddata, packagedata blocks, TDIS, or STO data
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- set current coarse-grained thickness (cg_thick) and
    !    current coarse-grained porosity (cg_theta). Only needed
    !    if updating material properties
    if (this%iupdatematprop /= 0) then
      do node = 1, this%dis%nodes
        this%cg_thick(node) = this%cg_thickini(node)
        this%cg_theta(node) = this%cg_thetaini(node)
      end do
    end if
    !
    ! -- return
    return
  end subroutine csub_ar


  subroutine csub_cg_calc_stress(this, nodes, hnew)
! ******************************************************************************
! csub_cg_calc_stress -- calculate the geostatic stress for every gwf node 
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
    integer(I4B) :: nn
    integer(I4B) :: m
    integer(I4B) :: iis
    real(DP) :: gs
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: thick
    real(DP) :: va_scale
    real(DP) :: hcell
    real(DP) :: gs_conn
    real(DP) :: area_node
    real(DP) :: area_conn
    real(DP) :: es
    real(DP) :: hs
    real(DP) :: hwva
    real(DP) :: sadd

! ------------------------------------------------------------------------------
    !
    ! -- calculate geostatic stress if necessary
    if (this%iupdatestress /= 0) then
      do node = 1, this%dis%nodes
        !
        ! -- calculate geostatic stress for this node
        !    this represents the geostatic stress component 
        !    for the cell
        top = this%dis%top(node)
        bot = this%dis%bot(node)
        thick = top - bot
        !
        ! -- calculate cell contribution to geostatic stress
        if (this%ibound(node) /= 0) then
          hcell = hnew(node)
        else
          hcell = bot
        end if
        if (hcell < bot) then
          gs = thick * this%sgm(node)
        else if (hcell < top) then
          gs = (top - hcell) * this%sgm(node) + (hcell - bot) * this%sgs(node)
        else
          gs = thick * this%sgs(node)
        end if
        !
        ! -- cell contribution to geostatic stress
        this%cg_gs(node) = gs
      end do
      !
      ! -- add user specified overlying geostatic stress
      do nn = 1, this%nbound
        node = this%nodelistsig0(nn)
        sadd = this%bound(1, nn)
        this%cg_gs(node) = this%cg_gs(node) + sadd
      end do
      !
      ! -- calculate geostatic stress above cell
      do node = 1, this%dis%nodes
        !
        ! -- area of cell 
        area_node = this%dis%get_area(node)
        !
        ! -- geostatic stress of cell
        gs = this%cg_gs(node)
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
                gs = gs + this%cg_gs(m)
              !
              ! -- disu discretization
              !    *** this needs to be checked ***
              else
                area_conn = this%dis%get_area(m)
                hwva = this%dis%con%hwva(iis)
                va_scale = this%dis%con%hwva(iis) / this%dis%get_area(m)
                gs_conn = this%cg_gs(m)
                gs = gs + (gs_conn * va_scale)
              end if

            end if
          end if
        end do
        !
        ! -- geostatic stress for cell with geostatic stress 
        !    of overlying cells
        this%cg_gs(node) = gs
      end do
    end if
    !
    ! -- save effective stress from the last iteration and
    !    calculate the new effective stress for a cell
    do node = 1, this%dis%nodes
      top = this%dis%top(node)
      bot = this%dis%bot(node)
      if (this%ibound(node) /= 0) then
        hcell = hnew(node)
        if (hcell < bot) then
          hcell = bot
        end if
      else
        hcell = bot
      end if
      hs = hcell - bot
      !
      ! -- calculate effective stress
      es = this%cg_gs(node) - hs
      this%cg_es(node) = es
    end do
    !
    ! -- return
    return

  end subroutine csub_cg_calc_stress


  subroutine csub_cg_chk_stress(this)
! ******************************************************************************
! csub_cg_chk_stress -- check that the effective stress for every gwf node 
!                       in the model is a positive value
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(GwfCsubType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=20) :: cellid
    integer(I4B) :: ierr
    integer(I4B) :: node
    real(DP) :: gs
    real(DP) :: bot
    real(DP) :: hcell
    real(DP) :: es
    real(DP) :: u

! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    ierr = 0
    !
    ! -- check geostatic stress if necessary
    !
    ! -- save effective stress from the last iteration and
    !    calculate the new effective stress for a cell
    do node = 1, this%dis%nodes
      if (this%ibound(node) < 1) cycle
      bot = this%dis%bot(node)
      gs = this%cg_gs(node)
      es = this%cg_es(node)
      u = DZERO
      if (this%ibound(node) /= 0) then
        u = gs - es
      end if
      hcell = u + bot
      if (this%lhead_based .EQV. .FALSE.) then
        if (es < DEM6) then
          ierr = ierr + 1
          call this%dis%noder_to_string(node, cellid)
          write(errmsg, '(a,g0.7,a,1x,a)')                                       &
            'ERROR: SMALL TO NEGATIVE EFFECTIVE STRESS (', es, ') IN CELL',      &
            trim(adjustl(cellid))
          call store_error(errmsg)
          write(errmsg, '(4x,a,1x,g0.7,3(1x,a,1x,g0.7),1x,a)')                   &
            '(', es, '=', this%cg_gs(node), '- (', hcell, '-', bot, ')'
          call store_error(errmsg)
        end if
      end if
    end do
    !
    ! -- write a summary error message
    if (ierr > 0) then
        write(errmsg, '(a,1x,i0,3(1x,a))')                                       &
          'ERROR SOLUTION: SMALL TO NEGATIVE EFFECTIVE STRESS VALUES IN', ierr,  &
          'CELLS CAN BE ELIMINATED BY INCREASING STORAGE VALUES AND/OR ',        &
          'ADDING/MODIFYINGSTRESS BOUNDARIES TO PREVENT WATER-LEVELS FROM',      &
          'EXCEEDING THE TOP OF THE MODEL' 
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
        call ustop()
    end if
    !
    ! -- return
    return

  end subroutine csub_cg_chk_stress
  
  
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
    comp = this%tcomp(i) + this%comp(i)
    if (ABS(comp) > DZERO) then
      thick = this%thickini(i)
      theta = this%thetaini(i)
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

  
  subroutine csub_nodelay_fc(this, ib, hcell, hcellold, rho1, rho2, rhs,         &
                             argtled)
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
    real(DP) :: thick
    real(DP) :: znode
    real(DP) :: snold
    real(DP) :: snnew
    real(DP) :: sto_fac
    real(DP) :: sto_fac0
    real(DP) :: area
    real(DP) :: theta
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
    thick = this%thickini(ib)
    !
    ! -- set iconvert
    this%iconvert(ib) = 0
    !
    ! -- aquifer saturation
    call this%csub_calc_sat(node, hcell, hcellold, snnew, snold)
    if (this%lhead_based .EQV. .TRUE.) then
      f = DONE
      f0 = DONE
    else
      znode = this%csub_calc_znode(top, bot, hcell)
      es = this%cg_es(node)
      es0 = this%cg_es0(node)
      theta = this%thetaini(ib)
      !
      ! -- calculate the compression index factors for the delay 
      !    node relative to the center of the cell based on the 
      !    current and previous head
      call this%csub_calc_sfacts(node, bot, znode, theta, es, es0, f)
    end if
    sto_fac = tled * snnew * thick * f
    sto_fac0 = tled * snold * thick * f
    !
    ! -- calculate rho1 and rho2
    rho1 = this%rci(ib) * sto_fac0
    rho2 = this%rci(ib) * sto_fac
    if (this%cg_es(node) > this%pcs(ib)) then
      this%iconvert(ib) = 1
      rho2 = this%ci(ib) * sto_fac
    end if
    if (this%ielastic(ib) /= 0) then
      rhs = rho1 * this%cg_es0(node) -                                           &
            rho2 * (this%cg_gs(node) + bot)
    else
      rhs = -rho2 * (this%cg_gs(node) + bot) +                                   &
            (this%pcs(ib) * (rho2 - rho1)) +                                     &
            (rho1 * this%cg_es0(node))
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
    es = this%cg_es(node)
    es0 = this%cg_es0(node)
    pcs = this%pcs(ib)
    !
    ! -- calculate no-delay interbed rho1 and rho2
    call this%csub_nodelay_fc(ib, hcell, hcellold, rho1, rho2, rhs, argtled=tled)
    !
    ! -- calculate no-delay interbed compaction
    if (this%ielastic(ib) /= 0) then
      comp = rho2 * es - rho1 * es0
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
    character(len=LINELENGTH) :: line, errmsg
    integer(I4B) :: ierr
    integer(I4B) :: nlist
    logical :: isfound
    ! -- formats
    character(len=*),parameter :: fmtblkerr =                                  &
      "('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*),parameter :: fmtlsp =                                     &
      "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
! ------------------------------------------------------------------------------
    !
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
      call this%TsManager%Reset(this%name)
      !
      ! -- Read data as a list
      call this%dis%read_list(this%parser%iuactive, this%iout,                 &
                               this%iprpak, nlist, this%inamedbound,           &
                               this%iauxmultcol, this%nodelistsig0,            &
                               this%bound, this%auxvarsig0, this%auxname,      &
                               this%sig0bname, this%listlabel,                 &
                               this%name, this%tsManager, this%iscloc)
      this%nbound = nlist
      !
      ! Define the tsLink%Text value(s) appropriately.
      ! E.g. for CSUB package, entry 1, assign tsLink%Text = 'SIG0'
      call this%csub_rp_ts()
      !
      ! -- Terminate the block
      call this%parser%terminateblock()
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
    use TdisModule, only: nper, kper
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
    !
    ! -- set initial states
    if (this%initialized == 0) then
      if (this%gwfiss == 0) then
        call this%csub_set_initial_state(nodes, hnew)
      end if
    end if
    !
    ! -- update state variables
    !
    ! -- coarse-grained materials
    do node = 1, nodes
      this%cg_comp(node) = DZERO
      this%cg_es0(node) = this%cg_es(node)
      if (this%iupdatematprop /= 0) then
        this%cg_thick0(node) = this%cg_thick(node)
        this%cg_theta0(node) = this%cg_theta(node)
      end if
    end do
    !
    ! -- interbeds
    do ib = 1, this%ninterbeds
      idelay = this%idelay(ib)
      !
      ! -- update common terms for no-delay and delay interbeds
      this%comp(ib) = DZERO
      node = this%nodelist(ib)
      if (this%initialized /= 0) then
        es = this%cg_es(node)
        pcs = this%pcs(ib)
        if (es > pcs) then
          this%pcs(ib) = es
        end if
      end if
      if (this%iupdatematprop /= 0) then
        this%thick0(ib) = this%thick(ib)
        this%theta0(ib) = this%theta(ib)
      end if
      !
      ! -- update delay interbed terms
      if (idelay /= 0) then
        !
        ! -- update state if previous period was steady state
        if (kper > 1) then
          if (this%gwfiss0 /= 0) then
            node = this%nodelist(ib)
            h = hnew(node)
            do n = 1, this%ndelaycells
              this%dbh(n, idelay) = h
            end do
          end if
        end if
        !
        ! -- update preconsolidation stress, stresses, head, dbdz0, and theta0
        do n = 1, this%ndelaycells
          ! update preconsolidation stress
          if (this%initialized /= 0) then
            if (this%dbes(n, idelay) > this%dbpcs(n, idelay)) then
              this%dbpcs(n, idelay) = this%dbes(n, idelay)
            end if
          end if
          this%dbh0(n, idelay) = this%dbh(n, idelay)
          this%dbes0(n, idelay) = this%dbes(n, idelay)
          if (this%iupdatematprop /= 0) then
            this%dbdz0(n, idelay) = this%dbdz(n, idelay)
            this%dbtheta0(n, idelay) = this%dbtheta(n, idelay)
          end if
        end do
      end if
    end do
    !
    ! -- set gwfiss0
    this%gwfiss0 = this%gwfiss
    !
    ! -- Advance the time series managers
    call this%TsManager%ad()
    !
    ! -- For each observation, push simulated value and corresponding
    !    simulation time from "current" to "preceding" and reset
    !    "current" value.
    call this%obs%obs_ad()
    !
    ! -- return
    return
  end subroutine csub_ad

  subroutine csub_set_initial_state(this, nodes, hnew)
! ******************************************************************************
! csub_set_initial_state -- Set initial state for coarse-grained materials 
!                           and interbeds
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), dimension(nodes), intent(in) :: hnew
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: linesep
    character(len=16) :: text
    character(len=20) :: cellid
    character (len=LINELENGTH) :: errmsg
    integer(I4B) :: ib
    integer(I4B) :: node
    integer(I4B) :: n
    integer(I4B) :: idelay
    integer(I4B) :: iloc
    integer(I4B) :: isep
    real(DP) :: pcs0
    real(DP) :: pcs
    real(DP) :: fact
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: void
    real(DP) :: es
    real(DP) :: znode
    real(DP) :: hcell
    real(DP) :: dzhalf
    real(DP) :: zbot
    real(DP) :: dbpcs
    real(DP) :: q
! ------------------------------------------------------------------------------
    !
    ! -- update geostatic load calculation
    call this%csub_cg_calc_stress(nodes, hnew)
    !
    ! -- initialize coarse-grained material effective stress 
    !    for the previous time step and the previous iteration
    do node = 1, nodes
      this%cg_es0(node) = this%cg_es(node)
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
    ! -- initialize interbed initial states
    do ib = 1, this%ninterbeds
      idelay = this%idelay(ib)
      node = this%nodelist(ib)
      top = this%dis%top(node)
      bot = this%dis%bot(node)
      hcell = hnew(node)
      pcs = this%pcs(ib)
      pcs0 = pcs
      if (this%ispecified_pcs == 0) then
        ! relative pcs...subtract head (u) from sigma'
        if (this%ipch /= 0) then
          pcs = this%cg_es(node) - pcs0
        else
          pcs = this%cg_es(node) + pcs0
        end if
      else
        ! specified pcs...substract head (u) from sigma
        if (this%ipch /= 0) then
          pcs = this%cg_gs(node) - (pcs0 - bot)
        end if
        if (pcs < this%cg_es(node)) then
          pcs = this%cg_es(node)
        end if
      end if
      this%pcs(ib) = pcs
      !
      ! -- delay bed initial states         
      if (idelay /= 0) then
        dzhalf = DHALF * this%dbdzini(1, idelay)
        !
        ! -- fill delay bed head with aquifer head or offset from aquifer head
        !    heads need to be filled first since used to calculate 
        !    the effective stress for each delay bed
        do n = 1, this%ndelaycells
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
          zbot = this%dbz(n, idelay) - dzhalf
          ! -- adjust pcs to bottom of each delay bed cell
          !    not using csub_calc_adjes() since smoothing not required
          dbpcs = pcs - (zbot - bot) * (this%sgs(node) - DONE)
          this%dbpcs(n, idelay) = dbpcs
          !
          ! -- initialize effective stress for previous time step
          this%dbes0(n, idelay) = this%dbes(n, idelay)
        end do 
      end if
    end do
    !
    ! -- scale coarse-grained materials cr
    do node = 1, nodes
      top = this%dis%top(node)
      bot = this%dis%bot(node)
      !
      ! -- user-specified specific storage
      if (this%istoragec == 1) then
        !
        ! -- retain specific storage values since they are constant
        if (this%lhead_based .EQV. .TRUE.) then
          fact = DONE
        !
        ! -- convert specific storage values since they are simulated to 
        !    be a function of the average effective stress
        else
          void = this%csub_calc_void(this%cg_theta(node))
          es = this%cg_es(node)
          hcell = hnew(node) 
          znode = this%csub_calc_znode(top, bot, hcell)
          fact = this%csub_calc_adjes(node, es, bot, znode)
          fact = fact * (DONE + void)
        end if
      !
      ! -- user-specified compression indices - multiply by dlog10es
      else
          fact = dlog10es
      end if
      this%cg_ske_cr(node) = this%cg_ske_cr(node) * fact
      !
      ! -- write error message if negative compression indices
      if (fact <= DZERO) then
        call this%dis%noder_to_string(node, cellid)
        write(errmsg,'(4x,a,1x,a)')                                              &
          '****ERROR. NEGATIVE RECOMPRESSION INDEX CALCULATED FOR CELL',         &
          trim(adjustl(cellid))
        call store_error(errmsg)
      end if
    end do
    !
    ! -- scale interbed cc and cr
    do ib = 1, this%ninterbeds
      idelay = this%idelay(ib)
      node = this%nodelist(ib)
      top = this%dis%top(node)
      bot = this%dis%bot(node)
      !
      ! -- user-specified specific storage
      if (this%istoragec == 1) then
        !
        ! -- retain specific storage values since they are constant
        if (this%lhead_based .EQV. .TRUE.) then
          fact = DONE
        !
        ! -- convert specific storage values since they are simulated to 
        !    be a function of the average effective stress
        else
          void = this%csub_calc_void(this%theta(ib))
          es = this%cg_es(node)
          hcell = hnew(node)
          znode = this%csub_calc_znode(top, bot, hcell)
          fact = this%csub_calc_adjes(node, es, bot, znode)
          fact = fact * (DONE + void)
        end if
      !
      ! -- user-specified compression indices - multiply by dlog10es
      else
          fact = dlog10es
      end if
      this%ci(ib) = this%ci(ib) * fact
      this%rci(ib) = this%rci(ib) * fact
      !
      ! -- write error message if negative compression indices 
      if (fact <= DZERO) then
        call this%dis%noder_to_string(node, cellid)
        write(errmsg,'(4x,a,1x,i0,2(1x,a))')                                     &
          '****ERROR. NEGATIVE COMPRESSION INDICES CALCULATED FOR INTERBED',     &
          ib, 'IN CELL', trim(adjustl(cellid))
        call store_error(errmsg)
      end if
    end do
    !
    ! -- write current stress and initial preconsolidation stress
    if (this%iprpak == 1) then
      write(this%iout, '(//1X,A,/1X,A)')                                         &
        'CALCULATED INITIAL INTERBED GEOSTATIC, EFFECTIVE,',                     &
        'AND PRECONSOLIDATION STRESS'
      !
      ! -- first header line
      iloc = 1
      line = ''
      call UWWORD(line, iloc, 10, 1, 'INTERBED', n, q, left=.TRUE.)
      call UWWORD(line, iloc, 16, 1,                                             & 
                  'GEOSTATIC', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1,                                             & 
                  'EFFECTIVE', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1,                                             & 
                  'PRECONSOLIDATION', n, q, CENTER=.TRUE.)
      linesep = repeat('-', iloc)
      isep = iloc
      write(this%iout, '(1X,A)') linesep(1:iloc)
      write(this%iout, '(1X,A)') line(1:iloc)
      !
      ! -- second header line
      iloc = 1
      line = ''
      call UWWORD(line, iloc, 10, 1, 'NUMBER', n, q, left=.TRUE.)
      call UWWORD(line, iloc, 16, 1,                                             & 
                  'STRESS', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1,                                             & 
                  'STRESS', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 16, 1,                                             & 
                  'STRESS', n, q, CENTER=.TRUE.)
      write(this%iout, '(1X,A)') line(1:iloc)
      write(this%iout, '(1X,A)') linesep(1:iloc)

      do ib = 1, this%ninterbeds
        iloc = 1
        line = ''
        call UWWORD(line, iloc, 10, 2, text, ib, q, left=.TRUE.)
        node = this%nodelist(ib)
        call UWWORD(line, iloc, 16, 3,                                           &
                    text, n, this%cg_gs(node), center=.TRUE.)
        call UWWORD(line, iloc, 16, 3,                                           &
                    text, n, this%cg_es(node), center=.TRUE.)
        call UWWORD(line, iloc, 16, 3,                                           & 
                    text, n, this%pcs(ib), CENTER=.TRUE.)
        write(this%iout, '(1X,A)') line(1:iloc)
      end do
      write(this%iout, '(1X,A,/)') linesep(1:isep)
      !
      ! -- write effective stress and preconsolidation stress 
      !    for delay beds
      do ib = 1, this%ninterbeds
        idelay = this%idelay(ib)
        if (idelay /= 0) then
          write(this%iout, '(//1X,A,/1X,A,1X,I0)')                               &
            'CALCULATED INITIAL INTERBED GEOSTATIC, EFFECTIVE,',                 &
            'AND PRECONSOLIDATION STRESS FOR INTERBED', ib
          !
          ! -- first header line
          iloc = 1
          line = ''
          call UWWORD(line, iloc, 10, 1, 'DELAY', n, q, left=.TRUE.)
          call UWWORD(line, iloc, 16, 1,                                         &
                      'GEOSTATIC', n, q, CENTER=.TRUE.)
          call UWWORD(line, iloc, 16, 1,                                         &
                      'EFFECTIVE', n, q, CENTER=.TRUE.)
          call UWWORD(line, iloc, 16, 1,                                         &
                      'PRECONSOLIDATION', n, q, CENTER=.TRUE.)
          linesep = repeat('-', iloc)
          isep = iloc
          write(this%iout, '(1X,A)') linesep(1:iloc)
          write(this%iout, '(1X,A)') line(1:iloc)
          !
          ! -- second header line
          iloc = 1
          line = ''
          call UWWORD(line, iloc, 10, 1, 'CELL', n, q, left=.TRUE.)
          call UWWORD(line, iloc, 16, 1,                                         &
                      'STRESS', n, q, CENTER=.TRUE.)
          call UWWORD(line, iloc, 16, 1,                                         &
                      'STRESS', n, q, CENTER=.TRUE.)
          call UWWORD(line, iloc, 16, 1,                                         &
                      'STRESS', n, q, CENTER=.TRUE.)
          write(this%iout, '(1X,A)') line(1:iloc)
          write(this%iout, '(1X,A)') linesep(1:iloc)

          do n = 1, this%ndelaycells
            iloc = 1
            line = ''
            call UWWORD(line, iloc, 10, 2, text, n, q, left=.TRUE.)
            call UWWORD(line, iloc, 16, 3,                                       &
                        text, n, this%dbgeo(n, idelay),                          &
                        center=.TRUE.)
            call UWWORD(line, iloc, 16, 3,                                       &
                        text, n, this%dbes(n, idelay),                           &
                        center=.TRUE.)
            call UWWORD(line, iloc, 16, 3,                                       &
                        text, n, this%dbpcs(n, idelay), center=.TRUE.)
            write(this%iout, '(1X,A)') line(1:iloc)
          end do
          write(this%iout, '(1X,A,/)') linesep(1:isep)
        end if
      end do
    
      !
      ! -- write calculated compression indices
      if (this%istoragec == 1) then
        if (this%lhead_based .EQV. .FALSE.) then
          write(this%iout, '(//1X,A)')                                &
            'CALCULATED COMPRESSION INDICES'
          iloc = 1
          line = ''
          call UWWORD(line, iloc, 10, 1,                              &
                      'INTERBED', n, q, left=.TRUE.)
          call UWWORD(line, iloc, 16, 1, 'CC', n, q, CENTER=.TRUE.)
          call UWWORD(line, iloc, 16, 1, 'CR', n, q, CENTER=.TRUE.)
          linesep = repeat('-', iloc)
          isep = iloc
          write(this%iout, '(1X,A)') linesep(1:iloc)
          write(this%iout, '(1X,A)') line(1:iloc)
          write(this%iout, '(1X,A)') linesep(1:iloc)
          do ib = 1, this%ninterbeds
            fact = DONE / dlog10es
            iloc = 1
            line = ''
            call UWWORD(line, iloc, 10, 2,                            &
                        text, ib, q, left=.TRUE.)
            call UWWORD(line, iloc, 16, 3,                            & 
                        text, n, this%ci(ib) * fact, CENTER=.TRUE.)
            call UWWORD(line, iloc, 16, 3,                            & 
                        text, n, this%rci(ib) * fact, CENTER=.TRUE.)
            write(this%iout, '(1X,A)') line(1:iloc)
          end do
          write(this%iout, '(1X,A,/)') linesep(1:isep)
        end if
      end if
    end if
    !
    ! -- terminate if any initialization errors have been detected
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- set initialized
    this%initialized = 1
    !
    ! -- set flag to retain initial stresses for entire simulation
    if (this%lhead_based .EQV. .TRUE.) then
      this%iupdatestress = 0
    end if
    !
    ! -- return
    return
  end subroutine csub_set_initial_state

  subroutine csub_fc(this, kiter, hold, hnew, njasln, amat, idxglo, rhs)
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
    real(DP), intent(in), dimension(:) :: hold
    real(DP), intent(in), dimension(:) :: hnew
    integer(I4B),intent(in) :: njasln
    real(DP), dimension(njasln),intent(inout) :: amat
    integer(I4B), intent(in),dimension(:) :: idxglo
    real(DP),intent(inout),dimension(:) :: rhs
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
    call this%csub_cg_calc_stress(this%dis%nodes, hnew)
    !
    ! -- formulate csub terms
    if (this%gwfiss == 0) then
      !
      ! -- initialize tled
      tled = DONE / delt
      !
      ! -- coarse-grained storage
      do node = 1, this%dis%nodes
        idiag = this%dis%con%ia(node)
        area = this%dis%get_area(node)
        !
        ! -- skip inactive cells
        if (this%ibound(node) < 1) cycle
        !
        ! -- update coarse-grained material properties
        if (this%iupdatematprop /= 0) then
          if (this%ieslag == 0) then
            !
            ! -- calculate compaction
            call this%csub_cg_calc_comp(node, hnew(node), hold(node), comp)
            this%cg_comp(node) = comp
            !
            ! -- update coarse-grained thickness and void ratio
            call this%csub_cg_update(node)
          end if
        end if
        !
        ! -- calculate coarse-grained storage terms
        call this%csub_cg_fc(node, tled, area, hnew(node), hold(node),          &
                             hcof, rhsterm)
        !
        ! -- add coarse-grained storage terms to amat and rhs for coarse-grained storage
        amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
        rhs(node) = rhs(node) + rhsterm
        !
        ! -- calculate coarse-grained water compressibility 
        !    storage terms
        if (this%brg /= DZERO) then
          call this%csub_cg_wcomp_fc(node, tled, area, hnew(node), hold(node),  &
                                     hcof, rhsterm)
          !
          ! -- add water compression storage terms to amat and rhs for 
          !   coarse-grained storage
          amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
          rhs(node) = rhs(node) + rhsterm
        end if
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
          if (this%brg /= DZERO) then
            call this%csub_interbed_wcomp_fc(ib, node, tled, area,              &
                                             hnew(node), hold(node),            &
                                             hcof, rhsterm)
            !
            ! -- add water compression storage terms to amat and rhs for interbed
            amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
            rhs(node) = rhs(node) + rhsterm
          end if
        end do
      end if
    end if    
    !
    ! -- terminate if the head in the cell is below the top of the cell for
    !    non-convertible cells or top of the interbed for delay beds or if
    !    errors encountered when updating material properties
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine csub_fc

  subroutine csub_fn(this, kiter, hold, hnew, njasln, amat, idxglo, rhs)
! ******************************************************************************
! csub_fn -- Fill the solution amat and rhs with csub contribution newton
!               term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt
    ! -- dummy
    class(GwfCsubType) :: this
    integer(I4B),intent(in) :: kiter
    real(DP), intent(in), dimension(:) :: hold
    real(DP), intent(in), dimension(:) :: hnew
    integer(I4B),intent(in) :: njasln
    real(DP), dimension(njasln),intent(inout) :: amat
    integer(I4B), intent(in),dimension(:) :: idxglo
    real(DP),intent(inout),dimension(:) :: rhs
    ! -- local
    integer(I4B) :: node
    integer(I4B) :: idiag
    integer(I4B) :: ib
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
      ! -- coarse-grained storage
      do node = 1, this%dis%nodes
        idiag = this%dis%con%ia(node)
        area = this%dis%get_area(node)
        !
        ! -- skip inactive cells
        if (this%ibound(node) < 1) cycle
        !
        ! -- calculate coarse-grained storage newton terms
        call this%csub_cg_fn(node, tled, area,                                   &
                               hnew(node), hcof, rhsterm)
        !
        ! -- add coarse-grained storage newton terms to amat and rhs for 
        !   coarse-grained storage
        amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
        rhs(node) = rhs(node) + rhsterm
        !
        ! -- calculate coarse-grained water compressibility storage 
        !    newton terms
        if (this%brg /= DZERO) then
          call this%csub_cg_wcomp_fn(node, tled, area, hnew(node), hold(node),   &
                                     hcof, rhsterm)
          !
          ! -- add water compression storage newton terms to amat and rhs for 
          !    coarse-grained storage
          amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
          rhs(node) = rhs(node) + rhsterm
        end if
      end do
      !
      ! -- interbed storage
      if (this%ninterbeds /= 0) then
        !
        ! -- calculate the interbed newton terms for the 
        !    groundwater flow equation
        do ib = 1, this%ninterbeds
          node = this%nodelist(ib)
          !
          ! -- skip inactive cells
          if (this%ibound(node) < 1) cycle
          !
          ! -- calculate interbed newton terms
          idiag = this%dis%con%ia(node)
          area = this%dis%get_area(node)
          call this%csub_interbed_fn(ib, node, area, hnew(node), hold(node),  &
                                      hcof, rhsterm)
          !
          ! -- add interbed newton terms to amat and rhs
          amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
          rhs(node) = rhs(node) + rhsterm
          !
          ! -- calculate interbed water compressibility terms
          if (this%brg /= DZERO) then
            call this%csub_interbed_wcomp_fn(ib, node, tled, area,              &
                                             hnew(node), hold(node),            &
                                             hcof, rhsterm)
            !
            ! -- add interbed water compression newton terms to amat and rhs
            amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
            rhs(node) = rhs(node) + rhsterm
          end if
        end do
      end if
    end if    
    !
    ! -- return
    return
  end subroutine csub_fn
  
  subroutine csub_cg_fc(this, node, tled, area, hcell, hcellold, hcof, rhs)
! ******************************************************************************
! csub_cg_fc -- Formulate the HCOF and RHS coarse-grained storage terms
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
    real(DP) :: snold
    real(DP) :: snnew
    real(DP) :: sske
    real(DP) :: rho1
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- aquifer elevations and thickness
    top = this%dis%top(node)
    bot = this%dis%bot(node)
    tthk = this%cg_thickini(node)
    !
    ! -- calculate hcof and rhs terms if coarse-grained materials present
    if (tthk > DZERO) then
      !
      ! -- calculate aquifer saturation
      call this%csub_calc_sat(node, hcell, hcellold, snnew, snold)
      !
      ! -- storage coefficients
      call this%csub_cg_calc_sske(node, sske, hcell)
      rho1 = sske * area * tthk * tled
      !
      ! -- update sk and ske
      this%cg_ske(node) = sske * tthk * snold
      this%cg_sk(node) = sske * tthk * snnew
      !
      ! -- calculate hcof and rhs term
      hcof = -rho1 * snnew
      rhs = rho1 * snold * this%cg_es0(node) -                                     &
            rho1 * snnew * (this%cg_gs(node) + bot) 
    end if
    !
    ! -- return
    return
  end subroutine csub_cg_fc
  
  subroutine csub_cg_fn(this, node, tled, area, hcell, hcof, rhs)
! ******************************************************************************
! csub_cg_fn -- Formulate coarse-grained storage newton terms
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
    real(DP) :: satderv
    real(DP) :: sske
    real(DP) :: rho1
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- aquifer elevations and thickness
    top = this%dis%top(node)
    bot = this%dis%bot(node)
    tthk = this%cg_thickini(node)
    !
    ! -- calculate newton terms if coarse-grained materials present
    if (tthk > DZERO) then
      !
      ! -- calculate saturation derivative
      satderv = this%csub_calc_sat_derivative(node, hcell)    
      !
      ! -- storage coefficients
      call this%csub_cg_calc_sske(node, sske, hcell)
      rho1 = sske * area * tthk * tled
      !
      ! -- calculate hcof term
      hcof = rho1 * (this%cg_gs(node) - hcell + bot) * satderv
      !
      ! -- Add additional term if using lagged effective stress
      if (this%ieslag /= 0) then
        hcof = hcof - rho1 * this%cg_es0(node) * satderv
      end if
      !
      ! -- calculate rhs term
      rhs = hcof * hcell
    end if
    !
    ! -- return
    return
  end subroutine csub_cg_fn
  
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
    character(len=20) :: cellid
    character (len=LINELENGTH) :: errmsg
    integer(I4B) :: idelaycalc
    real(DP) :: snnew
    real(DP) :: snold
    real(DP) :: comp
    real(DP) :: compi
    real(DP) :: compe
    real(DP) :: rho1
    real(DP) :: rho2
    real(DP) :: f
    real(DP) :: top
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    comp = DZERO
    compi = DZERO
    compe = DZERO
    !
    ! -- skip inactive and constant head cells
    if (this%ibound(node) > 0) then
      if (this%idelay(ib) == 0) then
        !
        ! -- update material properties
        if (this%iupdatematprop /= 0) then
          if (this%ieslag == 0) then
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
        ! -- calculate cell saturation
        call this%csub_calc_sat(node, hcell, hcellold, snnew, snold)
        !
        ! -- check that the delay bed should be evaluated
        idelaycalc = this%csub_delay_eval(ib, node, hcell)
        !
        ! -- calculate delay interbed hcof and rhs
        if (idelaycalc > 0) then
          !
          ! -- update material properties
          if (this%iupdatematprop /= 0) then
            if (this%ieslag == 0) then
              !
              ! -- calculate compaction
              call this%csub_delay_calc_comp(ib, hcell, hcellold,      &
                                             comp, compi, compe)
              this%comp(ib) = comp
              !
              ! -- update thickness and void ratio
              call this%csub_delay_update(ib)
            end if
          end if
          !
          ! -- calculate delay interbed hcof and rhs
          call this%csub_delay_sln(ib, hcell)
          call this%csub_delay_fc(ib, hcof, rhs)
        ! -- create error message
        else
          if (idelaycalc < 0) then
            call this%dis%noder_to_string(node, cellid)
            write(errmsg,'(4x,a,1x,g0,1x,a,1x,a,1x,a,1x,g0,1x,a,1x,i0)')         &
              '****ERROR. HEAD (', hcell, ') IN NON-CONVERTIBLE CELL (',         &
              trim(adjustl(cellid)), ') DROPPED BELOW THE TOP OF THE CELL (',    &
              top, ') FOR DELAY INTERBED ', ib
            call store_error(errmsg)
          end if
        end if
        f = area * this%rnb(ib) * snnew
      end if
      rhs = rhs * f
      hcof = -hcof * f
    end if
    !
    ! -- return
    return
  end subroutine csub_interbed_fc
  
  subroutine csub_interbed_fn(this, ib, node, area, hcell, hcellold, hcof, rhs)
! ******************************************************************************
! csub_interbed_fn -- Formulate interbed newton terms
! Subroutine: (1) skip if no interbeds
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt
    implicit none
    class(GwfCsubType) :: this
    integer(I4B),intent(in) :: ib
    integer(I4B),intent(in) :: node
    real(DP), intent(in) :: area
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rhs
    ! -- locals
    integer(I4B) :: idelay
    integer(I4B) :: idelaycalc
    real(DP) :: hcofn
    real(DP) :: rhsn
    real(DP) :: snnew
    real(DP) :: snold
    real(DP) :: satderv
    real(DP) :: tled
    real(DP) :: tthk
    real(DP) :: f
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: rho1
    real(DP) :: rho2
    real(DP) :: dz
    real(DP) :: c
    real(DP) :: h1
    real(DP) :: hn
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    rhsn = DZERO
    hcofn = DZERO
    satderv = DZERO
    idelay = this%idelay(ib)
    top = this%dis%top(node)
    bot = this%dis%bot(node)
    !
    ! -- skip inactive and constant head cells
    if (this%ibound(node) > 0) then
      tled = DONE / delt
      tthk = this%thickini(ib)
      !
      ! -- calculate cell saturation
      call this%csub_calc_sat(node, hcell, hcellold, snnew, snold)
      !
      ! -- no-delay interbeds
      if (idelay == 0) then
        !
        ! -- initialize factor
        f = DONE
        !
        ! -- calculate the saturation derivative
        satderv = this%csub_calc_sat_derivative(node, hcell)    
        !
        ! -- calculate storage coefficient
        call this%csub_nodelay_fc(ib, hcell, hcellold, rho1, rho2, rhsn)
        !
        ! -- calculate hcofn term
        if (this%ielastic(ib) /= 0) then
          hcofn = rho2 * (this%cg_gs(node) - hcell + bot) * satderv
        else
          hcofn = rho2 * (this%cg_gs(node) - hcell + bot - this%pcs(ib)) *       &
                  satderv
        end if
        !
        ! -- Add additional term if using lagged effective stress
        if (this%ieslag /= 0) then
          if (this%ielastic(ib) /= 0) then
            hcofn = hcofn - rho1 * this%cg_es0(node) * satderv
          else
            hcofn = hcofn - rho1 * (this%pcs(ib) - this%cg_es0(node)) * satderv
          end if
        end if
      !
      ! -- delay interbeds
      else
        !
        ! -- calculate factor
        f = this%rnb(ib)
        !
        ! -- check that the delay bed should be evaluated
        idelaycalc = this%csub_delay_eval(ib, node, hcell)
        !
        ! -- calculate newton terms if delay bed is not stranded
        !    newton terms are calculated the same if using the 
        !    head-based and effective-stress formulations
        if (idelaycalc > 0) then
          !
          ! calculate delay interbed terms
          dz = this%dbdzini(1, idelay)
          c = DTWO * this%kv(ib) / dz
          h1 = this%dbh(1, idelay)
          hn = this%dbh(this%ndelaycells, idelay)
          !
          ! -- calculate the saturation derivative
          satderv = this%csub_calc_sat_derivative(node, hcell)    
          !
          ! -- calculate the saturation derivative term
          hcofn = satderv * c * area * (hn + h1 - DTWO * hcell)
        end if
        !
        ! -- update hcof and rhs
        hcof = f * hcofn
        rhs = f * hcofn * hcell
      end if
    end if
    !
    ! -- return
    return
  end subroutine csub_interbed_fn

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
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'SIG0'
    if(this%inamedbound == 1) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    endif
    !
    ! -- return
    return
  end subroutine define_listlabel

  subroutine csub_cg_calc_sske(this, n, sske, hcell)
! ******************************************************************************
! csub_cg_calc_sske -- Calculate sske for a gwf cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(inout) :: sske
    real(DP), intent(in) :: hcell
    ! -- local variables
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: znode
    real(DP) :: es
    real(DP) :: es0
    real(DP) :: theta
    real(DP) :: f
    real(DP) :: f0
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    sske = DZERO
    !
    ! -- calculate factor for the head-based case
    if (this%lhead_based .EQV. .TRUE.) then
      f = DONE
      f0 = DONE
    !
    ! -- calculate factor for the effective stress case
    else
      top = this%dis%top(n)
      bot = this%dis%bot(n)
      znode = this%csub_calc_znode(top, bot, hcell)
      es = this%cg_es(n)
      es0 = this%cg_es0(n)
      theta = this%cg_thetaini(n)
      !
      ! -- calculate the compression index factors for the delay 
      !    node relative to the center of the cell based on the 
      !    current and previous head
      call this%csub_calc_sfacts(n, bot, znode, theta, es, es0, f)
    end if
    sske = f * this%cg_ske_cr(n)
    !
    ! -- return
    return
  end subroutine csub_cg_calc_sske
  
  subroutine csub_cg_calc_comp(this, node, hcell, hcellold, comp)
! ******************************************************************************
! csub_cg_calc_comp -- Calculate coarse-grained compaction
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
    call this%csub_cg_fc(node, tled, area, hcell, hcellold, hcof, rhs)
    !
    ! - calculate compaction
    comp = hcof * hcell - rhs
    !
    ! -- return
    return
  end subroutine  csub_cg_calc_comp

  
  subroutine csub_cg_update(this, node)
! ******************************************************************************
! csub_cg_update -- Update material properties for coarse grained sediments.
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
    comp = this%cg_tcomp(node) + this%cg_comp(node)
    call this%dis%noder_to_string(node, cellid)
    if (ABS(comp) > DZERO) then
      thick = this%cg_thickini(node)
      theta = this%cg_thetaini(node)
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
      this%cg_thick(node) = thick
      this%cg_theta(node) = theta
    end if
    !
    ! -- return
    return
  end subroutine csub_cg_update

  
  subroutine csub_cg_wcomp_fc(this, node, tled, area, hcell, hcellold,          &
                              hcof, rhs)
! ******************************************************************************
! csub_cg_wcomp_fc -- Calculate water compressibility term for a gwf cell.
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
    tthk = this%cg_thick(node)
    tthk0 = this%cg_thick0(node)
    !
    ! -- aquifer saturation
    call this%csub_calc_sat(node, hcell, hcellold, snnew, snold)
    !
    ! -- storage coefficients
    wc1 = this%brg * area * tthk0 * this%cg_theta0(node) * tled
    wc2 = this%brg * area * tthk * this%cg_theta(node) * tled
    !
    ! -- calculate hcof term
    hcof = -wc2 * snnew
    !
    ! -- calculate rhs term
    rhs = -wc1 * snold * hcellold
    !
    ! -- return
    return
  end subroutine csub_cg_wcomp_fc

  
  subroutine csub_cg_wcomp_fn(this, node, tled, area, hcell, hcellold, hcof, rhs)
! ******************************************************************************
! csub_cg_wcomp_fn -- Calculate water compressibility newton-rephson terms for 
!                     a gwf cell.
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
    real(DP) :: satderv
    real(DP) :: f
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
    tthk = this%cg_thick(node)
    !
    ! -- calculate saturation derivitive
    satderv = this%csub_calc_sat_derivative(node, hcell)    
    !
    ! -- calculate water compressibility factor
    f = this%brg * area * tled
    !
    ! -- water compressibility coefficient
    !wc2 = this%brg * area * tthk * this%cg_theta(node) * tled
    wc2 = f * tthk * this%cg_theta(node)
    !
    ! -- calculate hcof term
    hcof = -wc2 * hcell * satderv
    !
    ! -- Add additional term if using lagged effective stress
    if (this%ieslag /= 0) then
      tthk0 = this%cg_thick0(node)
      !wc1 = this%brg * area * tthk0 * this%cg_theta0(node) * tled
      wc1 = f * tthk0 * this%cg_theta0(node)
      hcof = hcof + wc1 * hcellold * satderv
    end if
    !
    ! -- calculate rhs term
    rhs = hcof * hcell
    !
    ! -- return
    return
  end subroutine csub_cg_wcomp_fn

  
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
    real(DP) :: dz0
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
    !
    ! -- calculate cell saturation
    call this%csub_calc_sat(node, hcell, hcellold, snnew, snold)
    !
    !
    idelay = this%idelay(ib)
    f = this%brg * area * tled
    !
    ! -- no-delay interbeds
    if (idelay == 0) then
      wc1 = f * this%theta0(ib) * this%thick0(ib)
      wc2 = f * this%theta(ib) * this%thick(ib)
      hcof = -wc2 * snnew
      rhs = -wc1 * snold * hcellold
    !
    ! -- delay interbeds
    else
      !
      ! -- calculate cell saturation
      call this%csub_calc_sat(node, hcell, hcellold, snnew, snold)
      !
      ! -- calculate contribution for each delay interbed cell
      if (this%thick(ib) > DZERO) then
        do n = 1, this%ndelaycells
          fmult = DONE
          dz = this%dbdz(n, idelay)
          dz0 = this%dbdz0(n, idelay)
          wc2 = fmult * f * dz * this%dbtheta(n, idelay)
          wc1 = fmult * f * dz0 * this%dbtheta0(n, idelay)
          rhs = rhs - (wc1 * snold * this%dbh0(n, idelay) -                     &
                       wc2 * snnew * this%dbh(n, idelay))
        end do
        rhs = rhs * this%rnb(ib) * snnew
      end if
    end if
    !
    ! -- return
    return
  end subroutine csub_interbed_wcomp_fc

  
  subroutine csub_interbed_wcomp_fn(this, ib, node, tled, area,                 &
                                    hcell, hcellold, hcof, rhs)
! ******************************************************************************
! csub_interbed_wcomp_fn -- Calculate water compressibility newton-raphson 
!                           terms for an interbed.
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
    integer(I4B) :: idelay
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: f
    real(DP) :: wc1
    real(DP) :: wc2
    real(DP) :: satderv
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- aquifer elevations and thickness
    top = this%dis%top(node)
    bot = this%dis%bot(node)
    !
    !
    idelay = this%idelay(ib)
    f = this%brg * area * tled
    !
    ! -- no-delay interbeds
    if (idelay == 0) then
      !
      ! -- calculate saturation derivitive
      satderv = this%csub_calc_sat_derivative(node, hcell)    
      !
      ! -- calculate the current water compressibility factor
      wc2 = f * this%theta(ib) * this%thick(ib)
      !
      ! -- calculate derivative term
      hcof = -wc2 * hcell * satderv
      !
      ! -- Add additional term if using lagged effective stress
      if (this%ieslag /= 0) then
        wc1 = f * this%theta0(ib) * this%thick0(ib)
        hcof = hcof + wc1 * hcellold * satderv
      end if
      !
      ! -- set rhs
      rhs = hcof * hcell
    !
    ! -- delay interbeds
    else
      ! -- delay beds are not head dependent
    end if
    !
    ! -- return
    return
  end subroutine csub_interbed_wcomp_fn  
  
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
  
  
  function csub_calc_interbed_thickness(this, ib) result(thick)
! ******************************************************************************
! csub_calc_interbed_thickness -- Calculate interbed thickness
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    ! -- dummy
    integer(I4B), intent(in) :: ib
    ! -- local variables
    integer(I4B) :: idelay
    real(DP) :: thick
! ------------------------------------------------------------------------------
    idelay = this%idelay(ib)
    thick = this%thick(ib)
    if (idelay /= 0) then
      thick = thick * this%rnb(ib)
    end if
    !
    ! -- return
    return
  end function csub_calc_interbed_thickness
  
  
  function csub_calc_znode(this, z1, z0, z) result(znode)
! ******************************************************************************
! csub_calc_znode -- Calculate elevation of the node between the specified 
!                    elevation z and the bottom elevation z0. If z is greater 
!                    the top elevation z1, the node elevation is halfway between
!                    the top (z1) and bottom (z0) elevations. if z is less than
!                    the bottom elevation (z0) the node elevation is set to z0.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    ! -- dummy
    real(DP), intent(in) :: z1
    real(DP), intent(in) :: z0
    real(DP), intent(in) :: z
    ! -- local variables
    real(DP) :: znode
    real(DP) :: v
! ------------------------------------------------------------------------------
    if (z > z1) then
      v = z1
    else if (z < z0) then
      v = z0
    else
      v = z
    end if
    znode = (v + z0) * DHALF
    !
    ! -- return
    return
  end function csub_calc_znode
  
  function csub_calc_adjes(this, node, es0, z0, z) result(es)
! ******************************************************************************
! csub_calc_adjes -- Calculate the effective stress at specified elevation z
!                    using the provided effective stress (es0) calculated at 
!                    elevation z0 (which is <= z)  
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    ! -- dummy
    integer(I4B), intent(in) :: node
    real(DP), intent(in) :: es0
    real(DP), intent(in) :: z0
    real(DP), intent(in) :: z
    ! -- local variables
    real(DP) :: es
! ------------------------------------------------------------------------------
    !
    ! -- adjust effective stress to vertical node position
    es = es0 - (z - z0) * (this%sgs(node) - DONE)
    !
    ! -- return
    return
  end function csub_calc_adjes

  function csub_delay_eval(this, ib, node, hcell) result(idelaycalc)
! ******************************************************************************
! csub_delay_eval -- Determine if the delay interbed should be solved,
!                    is stranded, or is a run-time error 
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    ! -- dummy
    integer(I4B), intent(in) :: ib
    integer(I4B), intent(in) :: node
    real(DP), intent(in) :: hcell
    ! -- local variables
    integer(I4B) :: idelaycalc
    real(DP) :: top
! ------------------------------------------------------------------------------
    idelaycalc = 1
    !
    ! -- non-convertible cell
    if (this%stoiconv(node) == 0) then
      top = this%dis%top(node)
      ! -- run-time error
      if (hcell < top) then
        idelaycalc = -999
      end if
    !
    ! -- convertible cell
    else
      top = this%dis%bot(node) + this%thickini(ib)
      !
      ! -- stranded cell
      if (hcell < top) then
        idelaycalc = 0
      end if
    end if
    !
    ! -- return
    return
  end function csub_delay_eval
  
   
  subroutine csub_calc_sat(this, node, hcell, hcellold, snnew, snold)
! ******************************************************************************
! csub_calc_sat -- Calculate current and previous cell saturation for a cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: node
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    real(DP), intent(inout) :: snnew
    real(DP), intent(inout) :: snold
    ! -- local variables
    real(DP) :: top
    real(DP) :: bot
! ------------------------------------------------------------------------------
    if (this%stoiconv(node) /= 0) then
      top = this%dis%top(node)
      bot = this%dis%bot(node)
      snnew = sQuadraticSaturation(top, bot, hcell, this%satomega)
      snold = sQuadraticSaturation(top, bot, hcellold, this%satomega)
    else
      snnew = DONE
      snold = DONE
    end if
    if (this%ieslag /= 0) then
      snold = snnew
    end if
    !
    ! -- return
    return
  end subroutine csub_calc_sat  
  
  function csub_calc_sat_derivative(this, node, hcell) result(satderv)
! ******************************************************************************
! csub_calc_sat_derivative -- Calculate current saturation derivative for a cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: node
    real(DP), intent(in) :: hcell
    ! -- local variables
    real(DP) :: satderv
    real(DP) :: top
    real(DP) :: bot
! ------------------------------------------------------------------------------
    if (this%stoiconv(node) /= 0) then
      top = this%dis%top(node)
      bot = this%dis%bot(node)
      satderv = sQuadraticSaturationDerivative(top, bot, hcell, this%satomega)
    else
      satderv = DZERO
    end if
    !
    ! -- return
    return
  end function csub_calc_sat_derivative  
  
  subroutine csub_calc_sfacts(this, node, bot, znode, theta, es, es0, fact)
! ******************************************************************************
! csub_calc_sfacts -- Calculate sske and factor for a gwf cell or 
!                     interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: node
    real(DP), intent(in) :: bot
    real(DP), intent(in) :: znode
    real(DP), intent(in) :: theta
    real(DP), intent(in) :: es
    real(DP), intent(in) :: es0
    real(DP), intent(inout) :: fact
    ! -- local variables
    real(DP) :: esv
    real(DP) :: void
    real(DP) :: denom
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    fact = DZERO
    if (this%ieslag /= 0) then
      esv = es0
    else
      esv = es
    end if
    !
    ! -- calculate storage factors for the effective stress case
    void = this%csub_calc_void(theta)
    denom = this%csub_calc_adjes(node, esv, bot, znode)
    denom = denom * (DONE + void)
    if (denom /= DZERO) then
      fact = DONE / denom
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

  subroutine csub_delay_sln(this, ib, hcell, update)
! ******************************************************************************
! csub_delay_sln -- Calculate flow in delay interbeds.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(in) :: hcell
    logical, intent(in), optional :: update
    ! -- local variables
    logical :: lupdate
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
    if (present(update)) then
      lupdate = update
    else
      lupdate = .true.
    end if
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
    if (this%thickini(ib) > DZERO) then
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
            dhmax = dh 
            if (lupdate) then
              this%dbdhmax(idelay) = dhmax
            end if
          end if
          ! -- update delay bed heads
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
    real(DP) :: zr
    real(DP) :: b
    real(DP) :: dz
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    node = this%nodelist(ib)
    b = this%thickini(ib)
    bot = this%dis%bot(node)
    top = bot + b
    !
    ! -- calculate znode based on assumption that the delay bed bottom 
    !    is equal to the cell bottom
    znode = this%csub_calc_znode(top, bot, top)
    dz = DHALF * this%dbdzini(1, idelay)
    dzz = DHALF * b
    z = znode + dzz
    zr = dzz
    !
    ! -- calculate z and z relative to znode for each delay 
    !    interbed node
    do n = 1, this%ndelaycells
      ! z of node relative to bottom of cell
      z = z - dz
      this%dbz(n, idelay) = z
      z = z - dz
      ! z relative to znode
      zr = zr - dz
      if (ABS(zr) < dz) then
        zr = DZERO
      end if
      this%dbrelz(n, idelay) = zr
      zr = zr - dz
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
    dzhalf = DHALF * this%dbdzini(1, idelay)
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
    sigma = this%cg_gs(node)
    topaq = this%dis%top(node)
    botaq = this%dis%bot(node)
    dzhalf = DHALF * this%dbdzini(1, idelay)
    top = this%dbz(1, idelay) + dzhalf
    !
    ! -- calculate the geostatic load in the cell at the top of the interbed.
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
    !
    ! -- calculate geostatic and effective stress for each interbed node.
    do n = 1, this%ndelaycells
      h = this%dbh(n, idelay)
      !
      ! -- geostatic calculated at the bottom of the delay cell
      z = this%dbz(n, idelay)
      top = z + dzhalf
      bot = z - dzhalf
      if (h > top) then
          sadd = (top - bot) * sgs
      else if (h < bot) then
          sadd = (top - bot) * sgm
      else
          sadd = ((top - h) * sgm) + ((h - bot) * sgs)
      end if
      sigma = sigma + sadd
      u = h - bot
      this%dbgeo(n, idelay) = sigma
      this%dbes(n, idelay) = sigma - u
    end do
    !
    ! -- return
    return
  end subroutine csub_delay_calc_stress

  subroutine csub_delay_calc_ssksske(this, ib, n, hcell, ssk, sske)
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
    real(DP), intent(in) :: hcell
    real(DP), intent(inout) :: ssk
    real(DP), intent(inout) :: sske
    ! -- local variables
    integer(I4B) :: idelay
    integer(I4B) :: ielastic
    integer(I4B) :: node
    real(DP) :: z1
    real(DP) :: z0
    real(DP) :: zcell
    real(DP) :: znode
    real(DP) :: zbot
    real(DP) :: es
    real(DP) :: es0
    real(DP) :: theta
    real(DP) :: f
    real(DP) :: f0
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    sske = DZERO
    ssk = DZERO
    idelay = this%idelay(ib)
    ielastic = this%ielastic(ib)
    !
    ! -- calculate factor for the head-based case
    if (this%lhead_based .EQV. .TRUE.) then
      f = DONE
      f0 = f
    !
    ! -- calculate factor for the effective stress case
    else
      node = this%nodelist(ib)
      theta = this%dbthetaini(n, idelay)
      !
      ! -- set top and bottom of layer and elevation of
      !    node relative to the bottom of the cell
      z1 = this%dis%top(node)
      z0 = this%dis%bot(node)
      zbot = this%dbz(n, idelay) - DHALF * this%dbdzini(1, idelay)
      !
      ! -- set location of delay node relative to the center
      !    of the cell based on current head
      zcell = this%csub_calc_znode(z1, z0, hcell)
      znode = zcell + this%dbrelz(n, idelay)
      !
      ! -- set the effective stress
      es = this%dbes(n, idelay)
      es0 = this%dbes0(n, idelay)
      !
      ! -- calculate the compression index factors for the delay 
      !    node relative to the center of the cell based on the 
      !    current and previous head
      call this%csub_calc_sfacts(node, zbot, znode, theta, es, es0, f)
    end if
    this%idbconvert(n, idelay) = 0
    sske = f * this%rci(ib)
    ssk = f * this%rci(ib)
    if (ielastic == 0) then
      if (this%dbes(n, idelay) > this%dbpcs(n, idelay)) then
        this%idbconvert(n, idelay) = 1
        ssk = f * this%ci(ib)
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
    integer(I4B) :: ielastic
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
    ielastic = this%ielastic(ib)
    node = this%nodelist(ib)
    dz = this%dbdzini(1, idelay)
    dzhalf = DHALF * dz
    fmult = dz / delt
    c = this%kv(ib) / dz
    c2 = DTWO * c
    c3 = DTHREE * c
    !
    !
    do n = 1, this%ndelaycells
      !
      ! -- current and previous delay cell states
      z = this%dbz(n, idelay)
      ztop = z + dzhalf
      zbot = z - dzhalf
      h = this%dbh(n, idelay)
      !
      ! -- calculate  ssk and sske
      call this%csub_delay_calc_ssksske(ib, n, hcell, ssk, sske)
      !
      ! -- calculate diagonal
      aii = -ssk * fmult
      !
      ! -- calculate right hand side
      if (ielastic /= 0) then
        r = -fmult *                                                             &
            (ssk * (this%dbgeo(n, idelay) + zbot) -                              &
             sske * this%dbes0(n, idelay))
      else
        r = -fmult *                                                             &
            (ssk * (this%dbgeo(n, idelay) + zbot - this%dbpcs(n, idelay)) +      &
             sske * (this%dbpcs(n, idelay) - this%dbes0(n, idelay)))
      end if
      !
      ! -- add connection to the gwf cell
      if (n == 1 .or. n == this%ndelaycells) then
        aii = aii - c3
        r = r - c2 * hcell
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
      !
      ! -- diagonal
      this%dbad(n) = aii
      !
      ! -- right hand side
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
 
  subroutine csub_delay_calc_dstor(this, ib, hcell, stoe, stoi)
! ******************************************************************************
! csub_delay_calc_dstor -- Calculate change in storage in a delay interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy variables
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(inout) :: hcell
    real(DP), intent(inout) :: stoe
    real(DP), intent(inout) :: stoi
    ! -- local variables
    integer(I4B) :: idelay
    integer(I4B) :: ielastic
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
    ielastic = this%ielastic(ib)
    stoe = DZERO
    stoi = DZERO
    ske = DZERO
    sk = DZERO
    !
    !
    if (this%thickini(ib) > DZERO) then
      fmult = this%dbdzini(1, idelay)
      dzhalf = DHALF * this%dbdzini(1, idelay)
      do n = 1, this%ndelaycells
        call this%csub_delay_calc_ssksske(ib, n, hcell, ssk, sske)
        z = this%dbz(n, idelay)
        zbot = z - dzhalf
        if (ielastic /= 0) then
          v1 = ssk * (this%dbgeo(n, idelay) - this%dbh(n, idelay) + zbot) -      &
                sske * this%dbes0(n, idelay)
          v2 = DZERO
        else
          v1 = ssk * (this%dbgeo(n, idelay) - this%dbh(n, idelay) + zbot -       &
                      this%dbpcs(n, idelay))
          v2 = sske * (this%dbpcs(n, idelay) - this%dbes0(n, idelay))
        end if
        !
        ! -- calculate inelastic and elastic storage components
        if (this%idbconvert(n, idelay) /= 0) then
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

  subroutine csub_delay_calc_comp(this, ib, hcell, hcellold, comp, compi, compe)
! ******************************************************************************
! csub_delay_calc_comp -- Calculate compaction in a delay interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    real(DP), intent(inout) :: comp
    real(DP), intent(inout) :: compi
    real(DP), intent(inout) :: compe
    ! -- local variables
    integer(I4B) :: idelay
    integer(I4B) :: ielastic
    integer(I4B) :: node
    integer(I4B) :: n
    real(DP) :: snnew
    real(DP) :: snold
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
    ielastic = this%ielastic(ib)
    node = this%nodelist(ib)
    comp = DZERO
    compi = DZERO
    compe = DZERO
    !
    ! -- calculate cell saturation
    call this%csub_calc_sat(node, hcell, hcellold, snnew, snold)
    !
    !
    if (this%thickini(ib) > DZERO) then
      fmult = this%dbdzini(1, idelay)
      do n = 1, this%ndelaycells
        call this%csub_delay_calc_ssksske(ib, n, hcell, ssk, sske)
        if (ielastic /= 0) then
          v1 = ssk * this%dbes(n, idelay) - sske * this%dbes0(n, idelay)
          v2 = DZERO
        else
          v1 = ssk * (this%dbes(n, idelay) - this%dbpcs(n, idelay))
          v2 = sske * (this%dbpcs(n, idelay) - this%dbes0(n, idelay))
        end if
        v = (v1 + v2) * fmult
        comp = comp + v
        !
        ! -- save compaction data
        this%dbcomp(n, idelay) = v * snnew
        !
        ! -- calculate inelastic and elastic storage components
        if (this%idbconvert(n, idelay) /= 0) then
          compi = compi + v1 * fmult
          compe = compe + v2 * fmult
        else
          compe = compe + (v1 + v2) * fmult
        end if
      end do
    end if
    !
    ! -- fill compaction
    comp = comp * this%rnb(ib) * snnew
    compi = compi * this%rnb(ib) * snnew
    compe = compe * this%rnb(ib) * snnew
    !
    ! -- return
    return
  end subroutine csub_delay_calc_comp

  subroutine csub_delay_update(this, ib)
! ******************************************************************************
! csub_delay_update -- Update delay interbed thickness and porosity.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    ! -- local variables
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: idelay
    integer(I4B) :: n
    real(DP) :: comp
    real(DP) :: thick
    real(DP) :: theta
    real(DP) :: tthick
    real(DP) :: wtheta
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    comp = DZERO
    tthick = DZERO
    wtheta = DZERO
    !
    !
    do n = 1, this%ndelaycells
      !
      ! -- initialize compaction for delay cell
      comp = this%dbtcomp(n, idelay) + this%dbcomp(n, idelay)
      !
      ! -- scale compaction by rnb to get the compaction for
      !    the interbed system (as opposed to the full system)
      comp = comp / this%rnb(ib)
      !
      ! -- update thickness and theta
      if (ABS(comp) > DZERO) then
        thick = this%dbdzini(n, idelay)
        theta = this%dbthetaini(n, idelay)
        call this%csub_adj_matprop(comp, thick, theta)
        if (thick <= DZERO) then
          write(errmsg,'(4x,2(a,1x,i0,1x),a,1x,g0,1x,a)')                         &
            '****ERROR. ADJUSTED THICKNESS FOR DELAY INTERBED (',                 &
            ib, ') CELL (', n, ') IS <= 0 (', thick, ')'
          call store_error(errmsg)
        end if
        if (theta <= DZERO) then
          write(errmsg,'(4x,2(a,1x,i0,1x),a,1x,g0,1x,a)')                         &
            '****ERROR. ADJUSTED THETA FOR DELAY INTERBED (',                     &
            ib, ') CELL (', n, 'IS <= 0 (', theta, ')'
          call store_error(errmsg)
        end if
        this%dbdz(n, idelay) = thick
        this%dbtheta(n, idelay) = theta
        tthick = tthick + thick
        wtheta = wtheta + thick * theta
      else
        thick = this%dbdz(n, idelay)
        theta = this%dbtheta(n, idelay)
        tthick = tthick + thick
        wtheta = wtheta + thick * theta
      end if      
    end do
    !
    ! -- calculate thickness weighted theta and save thickness and weighted 
    !    theta values for delay interbed
    if (tthick > DZERO) then
      wtheta = wtheta / tthick
    else
      tthick = DZERO
      wtheta = DZERO
    end if
    this%thick(ib) = tthick
    this%theta(ib) = wtheta
    !
    ! -- return
    return
  end subroutine csub_delay_update

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
    if (this%thickini(ib) > DZERO) then
      ! -- calculate terms for gwf matrix
      c1 = DTWO * this%kv(ib) / this%dbdzini(1, idelay)
      rhs = -c1 * this%dbh(1, idelay)
      c2 = DTWO *                                                  &
           this%kv(ib) / this%dbdzini(this%ndelaycells, idelay)
      rhs = rhs - c2 * this%dbh(this%ndelaycells, idelay)
      hcof = c1 + c2
    end if
    !
    ! -- return
    return
  end subroutine csub_delay_fc
  
  function csub_calc_delay_flow(this, ib, n, hcell) result(q)
! ******************************************************************************
! csub_calc_delay_flow -- Calculate flow across top or bottom of delay interbed
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    ! -- dummy
    integer(I4B), intent(in) :: ib
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: hcell
    ! -- local variables
    integer(I4B) :: idelay
    real(DP) :: q
    real(DP) :: c
! ------------------------------------------------------------------------------
    idelay = this%idelay(ib)
    c = DTWO * this%kv(ib) / this%dbdzini(n, idelay)
    q = c * (hcell - this%dbh(n, idelay))
    !
    ! -- return
    return
  end function csub_calc_delay_flow
  
  
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
    !    for coarse-csub observation type.
    call this%obs%StoreObsType('coarse-csub', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for csub-cell observation type.
    call this%obs%StoreObsType('csub-cell', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for watercomp-csub observation type.
    call this%obs%StoreObsType('wcomp-csub-cell', .false., indx)
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
    !    for coarse-compaction observation type.
    call this%obs%StoreObsType('coarse-compaction', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for compaction-cell observation type.
    call this%obs%StoreObsType('compaction-cell', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for interbed thickness observation type.
    call this%obs%StoreObsType('thickness', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for coarse-thickness observation type.
    call this%obs%StoreObsType('coarse-thickness', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for thickness-cell observation type.
    call this%obs%StoreObsType('thickness-cell', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for interbed theta observation type.
    call this%obs%StoreObsType('theta', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for coarse-theta observation type.
    call this%obs%StoreObsType('coarse-theta', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for theta-cell observation type.
    call this%obs%StoreObsType('theta-cell', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for preconstress-cell observation type.
    call this%obs%StoreObsType('preconstress-cell', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for delay-preconstress observation type.
    call this%obs%StoreObsType('delay-preconstress', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for delay-head observation type.
    call this%obs%StoreObsType('delay-head', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for delay-gstress observation type.
    call this%obs%StoreObsType('delay-gstress', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for delay-estress observation type.
    call this%obs%StoreObsType('delay-estress', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for delay-compaction observation type.
    call this%obs%StoreObsType('delay-compaction', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for delay-thickness observation type.
    call this%obs%StoreObsType('delay-thickness', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for delay-theta observation type.
    call this%obs%StoreObsType('delay-theta', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for delay-flowtop observation type.
    call this%obs%StoreObsType('delay-flowtop', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for delay-flowbot observation type.
    call this%obs%StoreObsType('delay-flowbot', .true., indx)
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
    integer(I4B) :: node
    real(DP) :: v
    real(DP) :: r
    real(DP) :: f
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
            case ('COARSE-CSUB')
              v = this%cg_stor(n)
            case ('WCOMP-CSUB-CELL')
              v = this%cell_wcstor(n)
            case ('CSUB-CELL')
              ! -- add the coarse component
              if (j == 1) then
                v = this%cg_stor(n)
              else
                v = this%storagee(n) + this%storagei(n)
              end if
            case ('SKE')
              v = this%ske(n)
            case ('SK')
              v = this%sk(n)
            case ('SKE-CELL')
              ! -- add the coarse component
              if (j == 1) then
                v = this%cg_ske(n)
              else
                v = this%ske(n)
              end if
            case ('SK-CELL')
              ! -- add the coarse component
              if (j == 1) then
                v = this%cg_sk(n)
              else
                v = this%sk(n)
              end if
            case ('THETA')
              v = this%theta(n)
            case ('COARSE-THETA')
              v = this%cg_theta(n)
            case ('THETA-CELL')
              ! -- add the coarse component
              if (j == 1) then
                f = this%cg_thick(n) / this%cell_thick(n)
                v = f * this%cg_theta(n)
              else
                node = this%nodelist(n) 
                f = this%csub_calc_interbed_thickness(n) / this%cell_thick(node)
                v = f * this%theta(n)
              end if
            case ('GSTRESS-CELL')
              v = this%cg_gs(n)
            case ('ESTRESS-CELL')
              v = this%cg_es(n)
            case ('INTERBED-COMPACTION')
              v = this%tcomp(n)
            case ('INELASTIC-COMPACTION')
              v = this%tcompi(n)
            case ('ELASTIC-COMPACTION')
              v = this%tcompe(n)
            case ('COARSE-COMPACTION')
              v = this%cg_tcomp(n)
            case ('COMPACTION-CELL')
              ! -- add the coarse component
              if (j == 1) then
                v = this%cg_tcomp(n)
              else
                v = this%tcomp(n)
              end if
            case ('THICKNESS')
              idelay = this%idelay(n)
              v = this%thick(n)
              if (idelay /= 0) then
                v = v * this%rnb(n)
              end if
            case ('COARSE-THICKNESS')
              v = this%cg_thick(n)
            case ('THICKNESS-CELL')
              v = this%cell_thick(n)
            case ('DELAY-HEAD', 'DELAY-PRECONSTRESS',                           &
                  'DELAY-GSTRESS', 'DELAY-ESTRESS',                             &
                  'DELAY-COMPACTION', 'DELAY-THICKNESS',                        &
                  'DELAY-THETA')
              if (n > this%ndelaycells) then
                r = real(n, DP) / real(this%ndelaycells, DP)
                idelay = int(floor(r)) + 1
                ncol = mod(n, this%ndelaycells)
              else
                idelay = 1
                ncol = n
              end if
              select case(obsrv%ObsTypeId)
                case ('DELAY-PRECONSTRESS')
                  v = this%dbpcs(ncol, idelay)
                case ('DELAY-HEAD')
                  v = this%dbh(ncol, idelay)
                case ('DELAY-GSTRESS')
                  v = this%dbgeo(ncol, idelay)
                case ('DELAY-ESTRESS')
                  v = this%dbes(ncol, idelay)
                case ('DELAY-COMPACTION')
                  v = this%dbtcomp(ncol, idelay)
                case ('DELAY-THICKNESS')
                  v = this%dbdz(ncol, idelay)
                case ('DELAY-THETA')
                  v = this%dbtheta(ncol, idelay)
              end select
            case ('PRECONSTRESS-CELL')
              v = this%pcs(n)
            case ('DELAY-FLOWTOP')
              idelay = this%idelay(n)
              v = this%dbflowtop(idelay)
            case ('DELAY-FLOWBOT')
              idelay = this%idelay(n)
              v = this%dbflowbot(idelay)
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
        do j = 1, this%ninterbeds
          if (this%boundname(j) == bname) then
            jfound = .true.
            obsrv%BndFound = .true.
            obsrv%CurrentTimeStepEndValue = DZERO
            call ExpandArray(obsrv%indxbnds)
            n = size(obsrv%indxbnds)
            obsrv%indxbnds(n) = j
          end if
        end do
      ! -- one value per cell
      else if (obsrv%ObsTypeId == 'GSTRESS-CELL' .or.                           &
               obsrv%ObsTypeId == 'ESTRESS-CELL' .or.                           &
               obsrv%ObsTypeId == 'THICKNESS-CELL' .or.                         &
               obsrv%ObsTypeId == 'COARSE-CSUB' .or.                            &
               obsrv%ObsTypeId == 'WCOMP-CSUB-CELL' .or.                        &
               obsrv%ObsTypeId == 'COARSE-COMPACTION' .or.                      &
               obsrv%ObsTypeId == 'COARSE-THETA' .or.                           &
               obsrv%ObsTypeId == 'COARSE-THICKNESS') then
        jfound = .true.
        obsrv%BndFound = .true.
        obsrv%CurrentTimeStepEndValue = DZERO
        call ExpandArray(obsrv%indxbnds)
        n = size(obsrv%indxbnds)
        obsrv%indxbnds(n) = obsrv%NodeNumber
      else if (obsrv%ObsTypeId == 'DELAY-PRECONSTRESS' .or.                     &
               obsrv%ObsTypeId == 'DELAY-HEAD' .or.                             &
               obsrv%ObsTypeId == 'DELAY-GSTRESS' .or.                          &
               obsrv%ObsTypeId == 'DELAY-ESTRESS' .or.                          &
               obsrv%ObsTypeId == 'DELAY-COMPACTION' .or.                       &
               obsrv%ObsTypeId == 'DELAY-THICKNESS' .or.                        &
               obsrv%ObsTypeId == 'DELAY-THETA') then
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
      ! -- interbed value
      else if (obsrv%ObsTypeId == 'CSUB' .or.                                   &
               obsrv%ObsTypeId == 'INELASTIC-CSUB' .or.                         &
               obsrv%ObsTypeId == 'ELASTIC-CSUB' .or.                           &
               obsrv%ObsTypeId == 'SK' .or.                                     &
               obsrv%ObsTypeId == 'SK' .or.                                     &
               obsrv%ObsTypeId == 'SKE' .or.                                    &
               obsrv%ObsTypeId == 'THICKNESS' .or.                              &
               obsrv%ObsTypeId == 'THETA' .or.                                  &
               obsrv%ObsTypeId == 'INTERBED-COMPACTION' .or.                    &
               obsrv%ObsTypeId == 'INELASTIC-COMPACTION' .or.                   &
               obsrv%ObsTypeId == 'ELASTIC-COMPACTION') then
        j = obsrv%NodeNumber
        idelay = this%idelay(j)
        if (j < 1 .or. j > this%ninterbeds) then
          write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
            'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
            ' interbed cell must be > 0 and <=', this%ninterbeds, &
            '(specified value is ', j, ')'
          call store_error(ermsg)
        else
          obsrv%BndFound = .true.
          obsrv%CurrentTimeStepEndValue = DZERO
          call ExpandArray(obsrv%indxbnds)
          n = size(obsrv%indxbnds)
          obsrv%indxbnds(n) = j
        end if
      else if (obsrv%ObsTypeId == 'DELAY-FLOWTOP' .or.                          &
               obsrv%ObsTypeId == 'DELAY-FLOWBOT') then
        j = obsrv%NodeNumber
        if (j < 1 .or. j > this%ninterbeds) then
          write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
            'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
            ' interbed cell must be > 0 and <=', this%ninterbeds, &
            '(specified value is ', j, ')'
          call store_error(ermsg)
        end if
        idelay = this%idelay(j)
        if (idelay == 0) then
          write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a)') &
            'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
            ' interbed', j, 'must be a delay interbed'
          call store_error(ermsg)
        else
          obsrv%BndFound = .true.
          obsrv%CurrentTimeStepEndValue = DZERO
          call ExpandArray(obsrv%indxbnds)
          n = size(obsrv%indxbnds)
          obsrv%indxbnds(n) = j
        end if
      else
        ! -- Accumulate values in a single cell
        ! -- Observation location is a single node number
        jfound = .false.
        ! -- save node number in first position
        if (obsrv%ObsTypeId == 'CSUB-CELL' .or.                                 &
            obsrv%ObsTypeId == 'SKE-CELL' .or.                                  &
            obsrv%ObsTypeId == 'SK-CELL' .or.                                   &
            obsrv%ObsTypeId == 'THETA-CELL' .or.                                &
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
    !--------------------------------------------------------------------------
    !
    strng = obsrv%IDstring
    ! -- Extract reach number from strng and store it.
    !    If 1st item is not an integer(I4B), it should be a
    !    boundary name--deal with it.
    icol = 1
    ! -- get icsubno number or boundary name
    if (obsrv%ObsTypeId=='CSUB' .or.                                            &
        obsrv%ObsTypeId == 'INELASTIC-CSUB' .or.                                &
        obsrv%ObsTypeId == 'ELASTIC-CSUB' .or.                                  &
        obsrv%ObsTypeId=='SK' .or.                                              &
        obsrv%ObsTypeId=='SKE' .or.                                             &
        obsrv%ObsTypeId=='THETA' .or.                                           &
        obsrv%ObsTypeId=='THICKNESS' .or.                                       &
        obsrv%ObsTypeId == 'INTERBED-COMPACTION' .or.                           &
        obsrv%ObsTypeId == 'INELASTIC-COMPACTION' .or.                          &
        obsrv%ObsTypeId == 'ELASTIC-COMPACTION' .or.                            &
        obsrv%ObsTypeId=='DELAY-HEAD' .or.                                      & 
        obsrv%ObsTypeId=='DELAY-GSTRESS' .or.                                   & 
        obsrv%ObsTypeId=='DELAY-ESTRESS' .or.                                   & 
        obsrv%ObsTypeId=='DELAY-PRECONSTRESS' .or.                              &
        obsrv%ObsTypeId=='DELAY-COMPACTION' .or.                                &
        obsrv%ObsTypeId=='DELAY-THICKNESS' .or.                                 &
        obsrv%ObsTypeId=='DELAY-THETA' .or.                                     &
        obsrv%ObsTypeId=='DELAY-FLOWTOP' .or.                                   &
        obsrv%ObsTypeId=='DELAY-FLOWBOT') then
      call extract_idnum_or_bndname(strng, icol, istart, istop, nn1, bndname)
    else
      nn1 = dis%noder_from_string(icol, istart, istop, inunitobs, &
                                  iout, strng, flag_string)
    end if
    if (nn1 == NAMEDBOUNDFLAG) then
      obsrv%FeatureName = bndname
    else
      if (obsrv%ObsTypeId=='DELAY-HEAD' .or.                                    &
          obsrv%ObsTypeId=='DELAY-GSTRESS' .or.                                 &
          obsrv%ObsTypeId=='DELAY-ESTRESS' .or.                                 &
          obsrv%ObsTypeId=='DELAY-PRECONSTRESS' .or.                            &
          obsrv%ObsTypeId=='DELAY-COMPACTION' .or.                              &
          obsrv%ObsTypeId=='DELAY-THICKNESS' .or.                               &
          obsrv%ObsTypeId=='DELAY-THETA') then
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

  !
  ! -- Procedure related to time series
  subroutine csub_rp_ts(this)
    ! -- Assign tsLink%Text appropriately for
    !    all time series in use by package.
    !    In the CSUB package only the SIG0 variable
    !    can be controlled by time series.
    ! -- dummy
    class(GwfCsubType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, nlinks
    type(TimeSeriesLinkType), pointer :: tslink => null()
    !
    nlinks = this%TsManager%boundtslinks%Count()
    do i=1,nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        if (tslink%JCol==1) then
          tslink%Text = 'SIG0'
        endif
      endif
    enddo
    !
    return
  end subroutine csub_rp_ts


end module GwfCsubModule

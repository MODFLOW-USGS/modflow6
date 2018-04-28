module GwfCsubModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DPREC, DZERO, DEM6, DHALF, DONE, DTWO, DTHREE,     &
                             DGRAVITY, DTEN, DNODATA,                           &
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
    integer(I4B), pointer :: iupdatematprop         => null()
    integer(I4B), pointer :: istoragec              => null()
    integer(I4B), pointer :: icellf                 => null()
    integer(I4B), pointer :: ibedstressoff          => null()
    integer(I4B), pointer :: igeostressoff          => null()
    integer(I4B), pointer :: inamedbound            => null()   !flag to read boundnames
    integer(I4B), pointer :: naux                   => null()   !number of auxiliary variables
    integer(I4B), pointer :: ninterbeds             => null()
    integer(I4B), pointer :: ndelaycells            => null()
    integer(I4B), pointer :: ndelaybeds             => null()
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

    logical :: first_time
    integer(I4B), pointer :: gwfiss                 => NULL()   !pointer to model iss flag
    integer(I4B), pointer :: gwfiss0                => NULL()   !iss flag for last stress period
    integer(I4B), dimension(:), pointer :: ibound   => null()   !pointer to model ibound
    integer(I4B), dimension(:), pointer :: stoiconv => null()   !pointer to iconvert in storage
    real(DP), dimension(:), pointer :: stosc1       => null()   !pointer to sc1 in storage
    integer, dimension(:), pointer :: nodelist      => null()   !reduced node that the interbed is attached to
    integer, dimension(:), pointer :: unodelist     => null()   !user node that the interbed is attached to

    !real(DP), dimension(:), pointer :: sk_znode     => null()   !elevation of node center
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
    real(DP), dimension(:), pointer :: gwflow       => null()   !gwf-flow
    real(DP), pointer, dimension(:,:) :: auxvar     => null()   !auxiliary variable array
    real(DP), dimension(:), pointer :: storagee     => null()   !elastic storage
    real(DP), dimension(:), pointer :: storagei     => null()   !inelastic storage
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
    procedure, private :: csub_delay_calc_zcell
    procedure, private :: csub_delay_calc_stress
    procedure, private :: csub_delay_calc_ssksske
    procedure, private :: csub_delay_calc_comp
    procedure, private :: csub_delay_calc_dstor
    procedure, private :: csub_delay_calc_err
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
    call mem_allocate(this%igeocalc, 'IGEOCALC', this%origin)
    call mem_allocate(this%ibedstressoff, 'IBEDSTRESSOFF', this%origin)
    call mem_allocate(this%igeostressoff, 'IGEOSTRESSOFF', this%origin)
    call mem_allocate(this%inamedbound, 'INAMEDBOUND', this%origin)
    call mem_allocate(this%naux, 'NAUX', this%origin)
    call mem_allocate(this%istoragec, 'ISTORAGEC', this%origin)
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
    this%igeocalc = 1
    this%ibedstressoff = 0
    this%igeostressoff = 0
    this%inamedbound = 0
    this%naux = 0
    this%istoragec = 1
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
    this%first_time = .TRUE.
    this%gwfiss0 = 0
    !
    ! -- return
    return
   end subroutine csub_allocate_scalars


   subroutine csub_bdcalc(this, nodes, hnew, hold, isuppress_output, model_budget)
! ******************************************************************************
! csub_bd -- calculate budget for skeletal storage, interbeds, and water
!            compression   
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
   ! -- modules
    use TdisModule, only: kstp, kper, delt
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
    !character (len=LENPACKAGENAME) :: text
    character(len=LINELENGTH) :: msg
    !integer(I4B) :: imover
    integer(I4B) :: i, n
    real(DP) :: es
    real(DP) :: pcs
    real(DP) :: rho1
    real(DP) :: rho2
    real(DP) :: tled
    real(DP) :: tledm
    real(DP) :: delt_sto
    real(DP) :: es0
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: thk_node
    real(DP) :: thick
    real(DP) :: theta
    real(DP) :: rrate
    real(DP) :: ratein
    real(DP) :: rateout
    real(DP) :: comp
    real(DP) :: area
    real(DP) :: h
    real(DP) :: hcof
    real(DP) :: rhs
    real(DP) :: stoe
    real(DP) :: stoi
    real(DP) :: err
    real(DP) :: qaq
    real(DP) :: dsto
    real(DP) :: qaqa
    real(DP) :: qaqrhs
    real(DP) :: void
    real(DP) :: rateskin
    real(DP) :: rateskout
    real(DP) :: rateibein
    real(DP) :: rateibeout
    real(DP) :: rateibiin
    real(DP) :: rateibiout
    real(DP) :: rratewc
    real(DP) :: ratewcin
    real(DP) :: ratewcout
    integer(I4B) :: ibc
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
    do n = 1, this%dis%nodes
      area = this%dis%get_area(n)
      comp = DZERO
      rrate = DZERO
      rratewc = DZERO
      if (this%gwfiss == 0) then
        if (this%ibound(n) > 0) then
          !
          ! -- calculate coarse-grained skeletal storage terms
          call this%csub_sk_fc(n, tled, area, hnew(n), hold(n), hcof, rhs)
          rrate = hcof * hnew(n) - rhs
          !
          ! -- calculate compaction
          call this%csub_sk_calc_comp(n, hnew(n), hold(n), comp)
          !
          ! -- budget terms
          if (rrate < DZERO) then
            rateskout = rateskout - rrate
          else
            rateskin = rateskin + rrate
          end if
          !
          ! -- calculate coarse-grained skeletal water compressibility storage terms
          call this%csub_sk_wcomp_fc(n, tled, area, hnew(n), hold(n), hcof, rhs)
          rratewc = hcof * hnew(n) - rhs
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
      this%sk_stor(n) = rrate
      this%sk_wcstor(n) = rratewc
      !
      ! -- update incremental compaction
      !comp = rrate * DELT / area
      this%sk_comp(n) = comp
      ! 
      !
      ! -- update states if required
      if (isuppress_output == 0) then
        !
        ! -- update total compaction
        !comp = rrate * DELT / area
        this%sk_tcomp(n) = this%sk_tcomp(n) + comp
        !
        ! - calculate strain and change in skeletal void ratio and thickness
        if (this%iupdatematprop /= 0) then
          call this%csub_sk_update(n)
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
    do i = 1, this%ninterbeds
      rratewc = DZERO
      if (this%gwfiss == 0) then
        n = this%nodelist(i)
        area = this%dis%get_area(n)
        !
        ! -- skip inactive and constant head cells
        if (this%ibound(n) < 1) cycle
        !
        ! -- no delay interbeds
        if (this%idelay(i) == 0) then
          stoi = DZERO
          !
          ! -- calculate compaction
          call this%csub_nodelay_calc_comp(i, hnew(n), hold(n), comp, rho1, rho2)
          !
          ! -- interbed stresses
          es = this%sk_es(n)
          pcs = this%pcs(i)
          es0 = this%sk_es0(n)
          !
          ! -- calculate inelastic and elastic compaction
          if (this%igeocalc == 0) then
            h = hnew(n)
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
          delt_sto = comp * area
          stoe = stoe * area
          stoi = stoi * area
          this%storagee(i) = stoe * tledm
          this%storagei(i) = stoi * tledm
          !
          ! -- update compaction
          this%comp(i) = comp
          !
          ! -- update states if required
          if (isuppress_output == 0) then
            !
            ! -- update total compaction
            this%tcomp(i) = this%tcomp(i) + comp
            !
            ! - calculate strain and change in interbed void ratio and thickness
            if (this%iupdatematprop /= 0) then
              call this%csub_nodelay_update(i)
            end if
          end if
          !
          ! -- delay interbeds
        else
          h = hnew(n)
          call this%csub_delay_calc_dstor(i, stoe, stoi)
          dsto = (stoe + stoi) * tledm
          delt_sto = (stoe + stoi) * area * this%rnb(i) * tledm
          this%storagee(i) = stoe * area * this%rnb(i) * tledm
          this%storagei(i) = stoi * area * this%rnb(i) * tledm
          !
          ! -- update states if required
          if (isuppress_output == 0) then
            !
            ! -- calculate sum of compaction in delay interbed
            call this%csub_delay_calc_comp(i)
          end if
        end if
        this%gwflow(i) = delt_sto
        !
        ! -- budget terms
        if (this%storagee(i) < DZERO) then
          rateibeout = rateibeout - this%storagee(i)
        else
          rateibein = rateibein + this%storagee(i)
        end if
        if (this%storagei(i) < DZERO) then
          rateibiout = rateibiout - this%storagei(i)
        else
          rateibiin = rateibiin + this%storagei(i)
        end if
        !
        ! -- interbed water compressibility
        call this%csub_interbed_wcomp_fc(i, n, tledm, area,                     &
                                         hnew(n), hold(n), hcof, rhs)
        rratewc = hcof * hnew(n) - rhs
        this%sk_wcstor(n) = this%sk_wcstor(n) + rratewc
        !
        ! -- water compressibility budget terms
        if (rratewc < DZERO) then
          ratewcout = ratewcout - rratewc
        else
          ratewcin = ratewcin + rratewc
        end if
      else
        this%storagee(i) = DZERO
        this%storagei(i) = DZERO
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

  subroutine csub_bdsav(this, icbcfl, icbcun)
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
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    character(len=1) :: cdatafmp=' ', editdesc=' '
    integer(I4B) :: ibinun
    integer(I4B) :: iprint, nvaluesp, nwidthp
    integer(I4B) :: n
    integer(I4B) :: node
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
      call this%dis%record_array(this%sk_stor, this%iout, iprint, -ibinun,    &
                                 budtxt(1), cdatafmp, nvaluesp,               &
                                 nwidthp, editdesc, dinact)
      !!
      !! -- storage(sy)
      !if (this%iusesy == 1) then
      !  call this%dis%record_array(this%strgsy, this%iout, iprint, -ibinun,  &
      !                             budtxt(2), cdatafmp, nvaluesp,            &
      !                             nwidthp, editdesc, dinact)
      !end if
      if (this%ninterbeds > 0) then
        naux = 0
        ! -- interbed elastic storage
        call ubdsv06(kstp, kper, budtxt(2), this%name_model, this%name,         &
                     this%name_model, this%name,                                &
                     ibinun, naux, this%auxname, this%ninterbeds, 1, 1,         &
                     this%ninterbeds, this%iout, delt, pertim, totim)
        do n = 1, this%ninterbeds
          q = this%storagee(n)
          node = this%nodelist(n)
          call this%dis%record_mf6_list_entry(ibinun, node, node, q, naux,      &
                                              this%auxvar(:,n))
        end do
        ! -- interbed inelastic storage
        call ubdsv06(kstp, kper, budtxt(3), this%name_model, this%name,         &
                     this%name_model, this%name,                                &
                     ibinun, naux, this%auxname, this%ninterbeds, 1, 1,         &
                     this%ninterbeds, this%iout, delt, pertim, totim)
        do n = 1, this%ninterbeds
          q = this%storagei(n)
          node = this%nodelist(n)
          call this%dis%record_mf6_list_entry(ibinun, node, node, q, naux,      &
                                              this%auxvar(:,n))
        end do
      end if
      !
      ! -- water compressibility
      call this%dis%record_array(this%sk_wcstor, this%iout, iprint, -ibinun,  &
                                 budtxt(4), cdatafmp, nvaluesp,               &
                                 nwidthp, editdesc, dinact)
    end if
    !
    ! -- Save observations.
    if (this%obs%npakobs > 0) then
      !call this%csub_bd_obs()
      call this%obs%obs_ot()
    end if
    !
    ! -- Return
    return
  end subroutine csub_bdsav

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
    character(len=LINELENGTH) :: text, keyword
    character(len=LINELENGTH) :: strttext, cellid
    character(len=LENBOUNDNAME) :: bndName, bndNameTemp
    character(len=7) :: cdelay
    character(len=9) :: cno
    character(len=50), dimension(:), allocatable :: caux
    integer(I4B) :: ival
    logical :: isfound, endOfBlock
    real(DP) :: rval, top, bot
    integer(I4B) :: n, nn
    integer(I4B) :: j, jj
    integer(I4B) :: iaux
    integer(I4B) :: itmp
    integer(I4B) :: ierr
    integer(I4B) :: ndelaybeds
    integer(I4B) :: idelay
    real(DP) :: endtim
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
    do n = 1, this%ninterbeds
      if (nboundchk(n) == 0) then
        write(errmsg, '(a, i0, a)') 'ERROR: INFORMATION FOR INTERBED ', n,     &
                                    ' NOT SPECIFIED IN PACKAGEDATA BLOCK.'
        call store_error(errmsg)
      else if (nboundchk(n) > 1) then
        write(errmsg, '(a, i0, i0)') 'ERROR: INFORMATION SPECIFIED ',          &
                                     nboundchk(n), ' TIMES FOR INTERBED ', n
        call store_error(errmsg)
      endif
    end do
    deallocate(nboundchk)
    !
    ! -- reallocate and initialize delay bed arrays
    this%ndelaybeds = ndelaybeds
    if (ndelaybeds > 0) then
      !
      ! -- delay bed storage
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
      ! -- delay interbed solution arrays
      call mem_reallocate(this%dbal, this%ndelaycells, 'dbal', trim(this%origin))
      call mem_reallocate(this%dbad, this%ndelaycells, 'dbad', trim(this%origin))
      call mem_reallocate(this%dbau, this%ndelaycells, 'dbau', trim(this%origin))
      call mem_reallocate(this%dbrhs, this%ndelaycells, 'dbrhs', trim(this%origin))
      call mem_reallocate(this%dbdh, this%ndelaycells, 'dbdh', trim(this%origin))
      call mem_reallocate(this%dbaw, this%ndelaycells, 'dbaw', trim(this%origin))
      !
      ! -- initialize delay bed storage
      do n = 1, this%ninterbeds
        idelay = this%idelay(n)
        if (idelay == 0) then
          cycle
        end if
        !
        ! -- adjust thickness if solving half cell problem
        this%thick(n) = this%dbfacti * this%thick(n)
        !
        ! -- calculate delay bed cell thickness
        if (this%idbhalfcell == 0) then
          this%dbdz(idelay) = this%thick(n) / real(this%ndelaycells, DP)
        else
          this%dbdz(idelay) = this%thick(n) / (real(this%ndelaycells, DP) - DHALF)
        end if
        do j = 1, this%ndelaycells
          this%dbh(j, idelay) = this%h0(n)
          this%dbh0(j, idelay) = this%h0(n)
          this%dbtheta(j, idelay) = this%theta(n)
          this%dbtheta0(j, idelay) = this%theta(n)
          this%dbgeo(j, idelay) = DZERO
          this%dbgeo0(j, idelay) = DZERO
          this%dbes(j, idelay) = DZERO
          this%dbes0(j, idelay) = DZERO
          this%dbpcs(j, idelay) = this%pcs(n)
        end do
      end do
      do n = 1, this%ndelaycells
        this%dbal(n) = DZERO
        this%dbad(n) = DZERO
        this%dbau(n) = DZERO
        this%dbrhs(n) = DZERO
        this%dbdh(n) = DZERO
        this%dbaw(n) = DZERO
      end do
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
!    use SimModule, only: ustop, store_error
    use InputOutputModule, only: urword, getunit, urdaux, openfile
    implicit none
    ! -- dummy
    class(GwfCsubType),   intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: keyword
    character(len=LINELENGTH) :: line
    logical :: isfound
    logical :: endOfBlock
    integer(I4B) :: lloc
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: n    
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
          ! offset is applied to geostatic stress
          ! initial stress is actually an offset from calculated initial stress
          case ('INTERBED_STRESS_OFFSET')
            this%ibedstressoff = 1
            write(this%iout, fmtopt) 'OFFSET WILL BE APPLIED TO INITIAL INTERBED EFFECTIVE STRESS'
          case ('GEO_STRESS_OFFSET')
            this%igeostressoff = 1
            write(this%iout, fmtopt) 'OFFSET WILL BE APPLIED TO STRESSES'
          ! compression indicies (CR amd CC) will be specified instead of 
          ! storage coefficients (SSE and SSV) 
          case ('COMPRESSION_INDICES')
            this%istoragec = 0
            write(this%iout, fmtopt) 'COMPRESSION INDICES WILL BE SPECIFIED ' //       &
                                     'INSTEAD OF ELASTIC AND INELASTIC SPECIFIC ' //   &
                                     'COEFFICIENTS'
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
    call mem_allocate(this%gwflow, iblen, 'gwflow', trim(this%origin))
    call mem_allocate(this%storagee, iblen, 'storagee', trim(this%origin))
    call mem_allocate(this%storagei, iblen, 'storagei', trim(this%origin))
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
      !call mem_deallocate(this%sk_znode)
      call mem_deallocate(this%sig0)
      call mem_deallocate(this%sk_gs)
      call mem_deallocate(this%sk_es)
      call mem_deallocate(this%sk_es0)
      call mem_deallocate(this%sk_comp)
      call mem_deallocate(this%sk_tcomp)
      call mem_deallocate(this%sk_stor)
      call mem_deallocate(this%sk_wcstor)
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
      call mem_deallocate(this%gwflow)
      call mem_deallocate(this%storagee)
      call mem_deallocate(this%storagei)
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
    call mem_deallocate(this%ibedstressoff)
    call mem_deallocate(this%igeostressoff)
    call mem_deallocate(this%inamedbound)
    call mem_deallocate(this%naux)
    call mem_deallocate(this%istoragec)
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
    character(len=LINELENGTH) :: line, errmsg, aname, keyword
    integer(I4B) :: ierr
    integer(I4B) :: lloc
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: ival
    integer(I4B) :: nlist
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: node
    integer(I4B) :: iske
    integer(I4B) :: istheta
    integer(I4B) :: isgm
    integer(I4B) :: isgs
    real(DP) :: rval
    real(DP) :: pcs_n
    real(DP) :: area
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: thick
    real(DP) :: theta
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
    do n = 1, this%dis%nodes
      if (this%igeocalc /= 0) then
        theta = this%sk_theta(n)
        if (theta > DONE .or. theta < DZERO) then
          ! add error message
          write(errmsg,'(4x,a,1x,a,g0,a,1x,a,1x,a)') &
                                       'ERROR. AQUIFER POROSITY IS LESS THAN', &
                                       '0 OR GREATER THAN 1 (', theta, ')',    &
                                       'in cell', ''
        end if
      end if
      top = this%dis%top(n)
      bot = this%dis%bot(n)
      this%sk_thick(n) = top - bot
    end do
    !
    ! -- subtract the interbed thickness from aquifer thickness
    do n = 1, this%ninterbeds
      node = this%nodelist(n)
      this%sk_thick(node) = this%sk_thick(node) - this%rnb(n) * this%thick(n)
    end do
    !
    ! -- evaluate if any sk_thick values are less than 0
    do n = 1, this%dis%nodes
      thick = this%sk_thick(n)
      if (thick < DZERO) then
        write(errmsg,'(4x,a,1x,g0,a,1x,a,1x,a)') &
                                      'ERROR. AQUIFER THICKNESS IS LESS THAN ZERO (', &
                                       thick, ')', 'in cell', ''
        call store_error(errmsg)
      end if
    end do
    !
    ! -- read observations
    call this%csub_rp_obs()
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
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: ii
    integer(I4B) :: m
    integer(I4B) :: temp
    integer(I4B) :: iis
    real(DP) :: gs
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: va_scale
    real(DP) :: hcell
    real(DP) :: gs_conn
    real(DP) :: area_n
    real(DP) :: area_conn
    real(DP) :: es
    real(DP) :: hs
    real(DP) :: hwva
    real(DP) :: sadd
    character(len=LINELENGTH) :: errmsg
    !character(len=LINELENGTH) :: msg1
    !character(len=LINELENGTH) :: msg2

! ------------------------------------------------------------------------------
    !
    ! -- calculate geostatic stress if necessary
    if (this%igeocalc /= 0) then
      do n = 1, this%dis%nodes
        !
        ! -- calculate geostatic stress for this node
        !    this represents the incremental value for the cell
        bot = this%dis%bot(n)
        top = this%dis%top(n)
        hcell = hnew(n)
        gs = DZERO
        if (hcell >= top) then
            gs = (top-bot) * this%sgs(n)
        else if (hcell <= bot) then
            gs = (top-bot) * this%sgm(n)
        else
            gs = ((top-hcell) * this%sgm(n)) + ((hcell-bot) * this%sgs(n))
        end if
        this%sk_gs(n) = gs
      end do
      !
      ! -- calculate the area weighted geostatic stress above cell
      !   *** this needs to be checked for a complicated discretization ***
      do n = 1, this%dis%nodes
        gs = this%sk_gs(n)
        !
        ! -- Go through the connecting cells
        do ii = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          !
          ! -- Set the m cell number
          !m = this%dis%con%jas(ii)
          m = this%dis%con%ja(ii)
          iis = this%dis%con%jas(ii)
          !
          ! -- Calculate conductance depending on whether connection is
          !    vertical (0), horizontal (1), or staggered horizontal (2)
          ! m < n = m is vertically above n
          area_n = this%dis%get_area(n)
          if (this%dis%con%ihc(iis) == 0 .and. m < n) then
              area_conn = this%dis%get_area(m)
              hwva = this%dis%con%hwva(iis)
              va_scale = this%dis%con%hwva(iis) / this%dis%get_area(m)
              gs_conn = this%sk_gs(m)
              !call this%dis%noder_to_string(n,msg1)
              !call this%dis%noder_to_string(m,msg2)
              !write(*,*) n, trim(msg1), gs, m, trim(msg2), gs_conn
              gs = gs + (gs_conn * va_scale)
          end if
        end do
        !write (*,*) n, gs
        this%sk_gs(n) = gs
      end do
      !
      ! -- calculate effective stress for a cell
      do n = 1, this%dis%nodes
        bot = this%dis%bot(n)
        hcell = hnew(n)
        hs = DZERO
        if (hcell > bot) then
          hs = hcell - bot
        end if
        es = this%sk_gs(n) - hs
        sadd = DZERO
        if (this%igeostressoff /= 0) then
          sadd = this%sig0(n)
        end if
        this%sk_es(n) = es + sadd
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

  
  subroutine csub_nodelay_fc(this, i, hcell, hcellold, rho1, rho2, rhs, argtled)
! ******************************************************************************
! csub_nodelay_fc -- Calculate rho1, rho2, and rhs for no-delay interbeds
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt, kstp, kper
    implicit none
    ! -- dummy variables
    class(GwfCsubType) :: this
    integer(I4B), intent(in) :: i
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    real(DP), intent(inout) :: rho1
    real(DP), intent(inout) :: rho2
    real(DP), intent(inout) :: rhs
    real(DP), intent(in), optional :: argtled
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: tled
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: znode
    real(DP) :: znode0
    real(DP) :: snold
    real(DP) :: snnew
    real(DP) :: thk_fac
    real(DP) :: sto_fac
    real(DP) :: sto_fac0
    real(DP) :: area
!    real(DP) :: x
!    real(DP) :: thk_node
!    real(DP) :: thk_ibs
    real(DP) :: denom
    real(DP) :: pcs_n
    real(DP) :: fact
    real(DP) :: theta
    real(DP) :: theta0
    real(DP) :: void
    real(DP) :: es
    real(DP) :: es0
    real(DP) :: f
    real(DP) :: f0
    character(len=LINELENGTH) :: msg
! ------------------------------------------------------------------------------
    if (present(argtled)) then
      tled = argtled
    else
      tled = DONE / delt
    endif
    n = this%nodelist(i)
    area = this%dis%get_area(n)
    bot = this%dis%bot(n)
    top = this%dis%top(n)
    ! -- aquifer saturation
    if (this%stoiconv(n) /= 0) then
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
      !znode = this%sk_znode(n)
      znode = this%csub_calc_znode(n, hcell)
      znode0 = this%csub_calc_znode(n, hcellold)
      if (this%time_alpha == DZERO) then
        znode0 = znode
      end if
      es = this%sk_es(n)
      es0 = this%sk_es0(n)
      theta = this%theta(i)
      theta0 = this%theta0(i)
      call this%csub_calc_sfacts(n, bot, znode, znode0, theta, theta0,          &
                                 es, es0, f, f0)
    end if
    sto_fac = tled * snnew * this%thick(i) * f
    sto_fac0 = tled * snold * this%thick0(i) * f0
    !
    ! -- calculate rho1 and rho2
    rho1 = this%rci(i) * sto_fac0
    rho2 = this%rci(i) * sto_fac
    if (this%igeocalc == 0) then
      if (hcell < this%pcs(i)) then
        rho2 = this%ci(i) * sto_fac
      end if
      rhs = -(rho2 * this%pcs(i) + rho1 * (this%sk_es0(n) - this%pcs(i)))
    else
      if (this%sk_es(n) > this%pcs(i)) then
          rho2 = this%ci(i) * sto_fac
      end if
      rhs = -rho2 * (this%sk_gs(n) + bot) + (this%pcs(i) * (rho2 - rho1)) + &
             (rho1 * this%sk_es0(n))
    end if
    !
    ! -- return
    return

  end subroutine csub_nodelay_fc


  subroutine csub_nodelay_calc_comp(this, i, hcell, hcellold, comp, rho1, rho2)
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
    integer(I4B), intent(in) :: i
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
    node = this%nodelist(i)
    tled = DONE
    es = this%sk_es(node)
    es0 = this%sk_es0(node)
    pcs = this%pcs(i)
    !
    ! -- calculate no-delay interbed rho1 and rho2
    call this%csub_nodelay_fc(i, hcell, hcellold, rho1, rho2, rhs, tled)
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
    !!
    !! -- read observations
    !call this%csub_rp_obs()
    !
    ! -- return
    return
  end subroutine csub_rp

  subroutine csub_ad(this, nodes, hnew)
! ******************************************************************************
! csub_ad -- Advance ibc data
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
    integer(I4B) :: n
    integer(I4B) :: j
    integer(I4B) :: idelay
    integer(I4B) :: node
    real(DP) :: h
    real(DP) :: es
    real(DP) :: pcs
    real(DP) :: b
    real(DP) :: bot
    real(DP) :: fact
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
    if (this%first_time) then
      if (this%gwfiss == 0) then
        call this%csub_interbed_set_initial(nodes, hnew)
      end if
    end if
    !
    ! -- update state variables
    !
    ! -- coarse-grained materials
    do n = 1, nodes
      this%sk_comp(n) = DZERO
      if (this%igeocalc == 0) then
        this%sk_es0(n)= hnew(n)
      else
        this%sk_es0(n) = this%sk_es(n)
      end if
      if (this%iupdatematprop /= 0) then
        this%sk_theta0(n) = this%sk_theta(n)
        this%sk_thick0(n) = this%sk_thick(n)
      end if
    end do
    !
    ! -- interbeds
    do n = 1, this%ninterbeds
      idelay = this%idelay(n)
      ! no delay beds
      if (idelay == 0) then
        this%comp(n) = DZERO
        node = this%nodelist(n)
        if (this%iupdatematprop /= 0) then
          this%theta0(n) = this%theta(n)
          this%thick0(n) = this%thick(n)
        end if
        if (kper == 1 .and. kstp == 1) then
          if (this%igeocalc == 0) then
            h = hnew(node)
            pcs = this%pcs(n)
            if (pcs > h) then
              this%pcs(n) = h
            end if
          end if
        end if
        if (.not. this%first_time) then
          es = this%sk_es(node)
          pcs = this%pcs(n)
          if (this%igeocalc == 0) then
            h = hnew(node)
            if (h < pcs) then
              this%pcs(n) = h
            end if
          else
            if (es > pcs) then
              this%pcs(n) = es
            end if
          end if
        end if
      !
      ! -- delay beds
      else
        ! update state if previous period was steady state
        if (kper > 1) then
          if (this%gwfiss0 /= 0) then
            node = this%nodelist(n)
            h = hnew(node)
            do j = 1, this%ndelaycells
              if (this%igeocalc == 0) then
                this%dbh(j, idelay) = h
              else
                this%dbh(j, idelay) = h
              end if
            end do
          end if
        end if
        b = DZERO
        do j = 1, this%ndelaycells
          b = b + this%dbdz(idelay)
          ! update preconsolidation stress
          if (.not. this%first_time) then
            if (this%igeocalc == 0) then
              if (this%dbh(j, idelay) < this%dbpcs(j, idelay)) then
                this%dbpcs(j, idelay) = this%dbh(j, idelay)
              end if
              this%dbes(j, idelay) = this%dbh(j, idelay)
            else
              if (this%dbes(j, idelay) > this%dbpcs(j, idelay)) then
                this%dbpcs(j, idelay) = this%dbes(j, idelay)
              end if
            end if
          end if
          this%dbh0(j, idelay) = this%dbh(j, idelay)
          this%dbgeo0(j, idelay) = this%dbgeo(j, idelay)
          this%dbes0(j, idelay) = this%dbes(j, idelay)
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
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: j
    integer(I4B) :: idelay
    real(DP) :: pcs
    real(DP) :: fact
    real(DP) :: bot
    real(DP) :: void
    real(DP) :: znode
! ------------------------------------------------------------------------------
    !
    ! -- update geostatic load calculation
    !call this%csub_sk_calc_znode(nodes, hnew)
    call this%csub_sk_calc_stress(nodes, hnew)
    !
    ! -- coarse-grained materials
    do n = 1, nodes
      ! scale cr and cc
      bot = this%dis%bot(n)
      if (this%istoragec == 1) then
        if (this%igeocalc == 0) then
          fact = DONE
        else
          void = this%csub_calc_void(this%sk_theta(n))
          !fact = this%sk_es(n) - (this%sk_znode(n) - bot) * (this%sgs(n) - DONE)
          znode = this%csub_calc_znode(n, hnew(n))
          fact = this%sk_es(n) - (znode - bot) * (this%sgs(n) - DONE)
          fact = fact * (DONE + void)
        end if
      else
          fact = dlog10es
      end if
      this%ske_cr(n) = this%ske_cr(n) * fact
      ! -- iniitalize previous initial stress
      this%sk_es0(n) = this%sk_es(n)
    end do
    !
    ! -- interbeds
    do i = 1, this%ninterbeds
      n = this%nodelist(i)
      pcs = this%pcs(i)
      if (this%igeocalc == 0) then
        if (this%sk_es(n) < pcs) then
          pcs = this%sk_es(n)
        end if
      else
        ! -- transfer initial preconsolidation stress (and apply offset if needed)
        if (this%ibedstressoff == 1) then
            pcs = this%sk_es(n) + this%pcs(i)
        else
          if (pcs < this%sk_es(n)) then
            pcs = this%sk_es(n)
          end if
        end if
      end if
      this%pcs(i) = pcs
      ! -- fill delay bed pcs          
      idelay = this%idelay(i)
      if (idelay > 0) then
        do j = 1, this%ndelaycells
          if (this%igeocalc == 0) then
          else
            this%dbpcs(j, idelay) = this%pcs(i)
          end if
          this%dbes0(j, idelay) = this%sk_es(n)
        end do            
      end if
          
      ! scale cr and cc
      bot = this%dis%bot(n)
      if (this%istoragec == 1) then
        if (this%igeocalc == 0) then
          fact = DONE
        else
          void = this%csub_calc_void(this%theta(i))
          !fact = this%sk_es(n) - (this%sk_znode(n) - bot) * (this%sgs(n) - DONE)
          znode = this%csub_calc_znode(n, hnew(n))
          fact = this%sk_es(n) - (znode - bot) * (this%sgs(n) - DONE)
          fact = fact * (DONE + void)
        end if
      else
          fact = dlog10es
      end if
      this%ci(i) = this%ci(i) * fact
      this%rci(i) = this%rci(i) * fact
    end do
    this%first_time = .false.
    !
    ! -- return
    return
  end subroutine csub_interbed_set_initial

  subroutine csub_fc(this, kiter, nodes, hold, hnew, nja, njasln, amat, &
                     idxglo, rhs)
! ******************************************************************************
! sto_fc -- Fill the solution amat and rhs with storage contribution terms
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
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: tled
    real(DP) :: area
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: tthk
    real(DP) :: snold
    real(DP) :: snnew
    real(DP) :: sske
    real(DP) :: rho1
    real(DP) :: hcell
    real(DP) :: hcof
    real(DP) :: rhsterm
    real(DP) :: wc1
    real(DP) :: comp
! ------------------------------------------------------------------------------
    !
    ! -- update geostatic load calculation
    !call this%csub_sk_calc_znode(nodes, hnew)
    call this%csub_sk_calc_stress(nodes, hnew)
    !
    ! -- formulate csub terms
    if (this%gwfiss == 0) then
      tled = DONE / delt
      !
      ! -- coarse-grained skeletal storage
      do n = 1, this%dis%nodes
        idiag = this%dis%con%ia(n)
        area = this%dis%get_area(n)
        !
        ! -- skip inactive cells
        if (this%ibound(n) < 1) cycle
        !
        ! -- update skeletal material properties
        if (this%iupdatematprop /= 0) then
          if (this%time_alpha > DZERO) then
            !
            ! -- calculate compaction
            call this%csub_sk_calc_comp(n, hnew(n), hold(n), comp)
            this%sk_comp(n) = comp
            !
            ! -- update skeletal thickness and void ratio
            call this%csub_sk_update(n)
          end if
        end if
        !
        ! -- calculate coarse-grained skeletal storage terms
        call this%csub_sk_fc(n, tled, area, hnew(n), hold(n), hcof, rhsterm)
        !
        ! -- add skeletal storage terms to amat and rhs for skeletal storage
        amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
        rhs(n) = rhs(n) + rhsterm
        !
        ! -- calculate coarse-grained skeletal water compressibility storage terms
        call this%csub_sk_wcomp_fc(n, tled, area, hnew(n), hold(n), hcof, rhsterm)
        !
        ! -- add water compression storage terms to amat and rhs for skeletal storage
        amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
        rhs(n) = rhs(n) + rhsterm
      end do
      !
      ! -- interbed storage
      if (this%ninterbeds /= 0) then
        do i = 1, this%ninterbeds
          n = this%nodelist(i)
          idiag = this%dis%con%ia(n)
          area = this%dis%get_area(n)
          hcell = hnew(n)
          call this%csub_interbed_fc(i, n, tled, area,                          &
                                     hnew(n), hold(n), hcof, rhsterm)
          amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
          rhs(n) = rhs(n) + rhsterm
          !
          ! -- calculate interbed water compressibility terms
          call this%csub_interbed_wcomp_fc(i, n, tled, area,                    &
                                           hnew(n), hold(n), hcof, rhsterm)
          !
          ! -- add water compression storage terms to amat and rhs for interbed
          amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
          rhs(n) = rhs(n) + rhsterm
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
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: tled
    real(DP) :: area
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: tthk
    real(DP) :: snold
    real(DP) :: snnew
    real(DP) :: sske
    real(DP) :: rho1
    real(DP) :: hcell
    real(DP) :: hcof
    real(DP) :: rhsterm
    real(DP) :: wc1
! ------------------------------------------------------------------------------
    !
    ! -- formulate csub terms
    if (this%gwfiss == 0) then
      tled = DONE / delt
      !
      ! -- coarse-grained skeletal storage
      do n = 1, this%dis%nodes
        idiag = this%dis%con%ia(n)
        area = this%dis%get_area(n)
        !
        ! -- skip inactive cells
        if (this%ibound(n) < 1) cycle
        !
        ! -- skip non-convertible cells
        if (this%stoiconv(n) == 0) cycle
        !
        ! -- calculate coarse-grained skeletal storage newton terms
        call this%csub_sk_fn(n, tled, area, hnew(n), hcof, rhsterm)
        !
        ! -- add skeletal storage newton terms to amat and rhs for 
        !   skeletal storage
        amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
        rhs(n) = rhs(n) + rhsterm
        !
        ! -- calculate coarse-grained skeletal water compressibility storage 
        !    newton terms
        call this%csub_sk_wcomp_fn(n, tled, area, hnew(n), hcof, rhsterm)
        !
        ! -- add water compression storage newton terms to amat and rhs for 
        !    skeletal storage
        amat(idxglo(idiag)) = amat(idxglo(idiag)) + hcof
        rhs(n) = rhs(n) + rhsterm
      end do
    end if    
    !
    ! -- return
    return
  end subroutine csub_fn
  
  subroutine csub_sk_fc(this, n, tled, area, hcell, hcellold, hcof, rhs)
! ******************************************************************************
! csub_sk_fc -- Formulate the HCOF and RHS skeletal storage terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(GwfCsubType) :: this
    integer(I4B),intent(in) :: n
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
    real(DP) :: f
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- aquifer elevations and thickness
    top = this%dis%top(n)
    bot = this%dis%bot(n)
    tthk = this%sk_thick(n)
    tthk0 = this%sk_thick0(n)
    ! -- aquifer saturation
    if (this%stoiconv(n) /= 0) then
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
    call this%csub_sk_calc_sske(n, sske, sske0, hcell, hcellold)
    rho1 = sske0 * area * tthk0 * tled
    rho2 = sske * area * tthk * tled
    !
    ! -- calculate hcof term
    hcof = -rho2 * snnew
    !
    ! -- calculate rhs term
    if (this%igeocalc == 0) then
      rhs = -rho1 * snold * hcellold
    else
      rhs = rho1 * snold * this%sk_es0(n) -                                     &
            rho2 * snnew * (this%sk_gs(n) + bot) 
    end if
    !
    ! -- return
    return
  end subroutine csub_sk_fc
  
  subroutine csub_sk_fn(this, n, tled, area, hcell, hcof, rhs)
! ******************************************************************************
! csub_sk_fn -- Formulate skeletal storage newton terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(GwfCsubType) :: this
    integer(I4B),intent(in) :: n
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
    real(DP) :: f
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- aquifer elevations and thickness
    top = this%dis%top(n)
    bot = this%dis%bot(n)
    tthk = this%sk_thick(n)
    !
    ! -- calculate saturation derivitive
    derv = sQuadraticSaturationDerivative(top, bot, hcell)    
    !
    ! -- storage coefficients
    call this%csub_sk_calc_sske(n, sske, sske0, hcell, hcell)
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

  
  subroutine csub_interbed_fc(this, i, n, tled, area, hcell, hcellold,  &
                                      hcof, rhs)
! ******************************************************************************
! csub_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip if no ibcs
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt, kper
    implicit none
    class(GwfCsubType) :: this
    integer(I4B),intent(in) :: i
    integer(I4B),intent(in) :: n
    real(DP), intent(in) :: tled
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
    if (this%ibound(n) > 0) then
      if (this%idelay(i) == 0) then
        !
        ! -- update material properties
        if (this%iupdatematprop /= 0) then
          if (this%time_alpha > DZERO) then
            !
            ! -- calculate compaction
            call this%csub_nodelay_calc_comp(i, hcell, hcellold, comp, rho1, rho2)
            this%comp(i) = comp
            !
            ! -- update thickness and void ratio
            call this%csub_nodelay_update(i)
          end if
        end if
        !
        ! -- calculate no-delay interbed rho1 and rho2
        call this%csub_nodelay_fc(i, hcell, hcellold, rho1, hcof, rhs)
        f = area
      else
        !
        ! -- calculate delay interbed hcof and rhs
        call this%csub_delay_sln(i, hcell)
        call this%csub_delay_fc(i, hcof, rhs)
        f = area * this%rnb(i)
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
    integer(I4B) :: idelay
    real(DP) :: bot
    real(DP) :: znode
    real(DP) :: znode0
    real(DP) :: es
    real(DP) :: es0
    real(DP) :: theta
    real(DP) :: theta0
    real(DP) :: void
    real(DP) :: denom
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
      !znode = this%sk_znode(n)
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
  
  subroutine csub_sk_calc_comp(this, n, hcell, hcellold, comp)
! ******************************************************************************
! csub_sk_calc_comp -- Calculate skeletal compaction
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(GwfCsubType) :: this
    integer(I4B),intent(in) :: n
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
    call this%csub_sk_fc(n, tled, area, hcell, hcellold, hcof, rhs)
    !
    ! - calculate compaction
    comp = hcof * hcell - rhs
    !
    ! -- return
    return
  end subroutine  csub_sk_calc_comp

  
  subroutine csub_sk_update(this, n)
! ******************************************************************************
! csub_sk_update -- Update material properties for coarse grained sediments.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B),intent(in) :: n
    ! locals
    character(len=LINELENGTH) :: errmsg
    character(len=20) :: cellid
    real(DP) :: comp
    real(DP) :: thick
    real(DP) :: theta
! ------------------------------------------------------------------------------
!
! -- update thickness and theta
    comp = this%sk_comp(n)
    call this%dis%noder_to_string(n, cellid)
    if (ABS(comp) > DZERO) then
      thick = this%sk_thick0(n)
      theta = this%sk_theta0(n)
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
      this%sk_thick(n) = thick
      this%sk_theta(n) = theta
    end if
    !
    ! -- return
    return
  end subroutine csub_sk_update

  
  subroutine csub_sk_wcomp_fc(this, n, tled, area, hcell, hcellold, hcof, rhs)
! ******************************************************************************
! csub_sk_wcomp_fc -- Calculate water compressibility term for a gwf cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B),intent(in) :: n
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
    real(DP) :: wc1
    real(DP) :: wc2
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- aquifer elevations and thickness
    top = this%dis%top(n)
    bot = this%dis%bot(n)
    tthk = this%sk_thick(n)
    tthk0 = this%sk_thick0(n)
    ! -- aquifer saturation
    if (this%stoiconv(n) /= 0) then
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
    wc1 = this%gammaw * this%beta * area * tthk0 * this%sk_theta0(n) * tled
    wc2 = this%gammaw * this%beta * area * tthk * this%sk_theta(n) * tled
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

  
  subroutine csub_sk_wcomp_fn(this, n, tled, area, hcell, hcof, rhs)
! ******************************************************************************
! csub_sk_wcomp_fc -- Calculate water compressibility newton terms for a 
!                       gwf cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B),intent(in) :: n
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
    real(DP) :: wc2
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    hcof = DZERO
    !
    ! -- aquifer elevations and thickness
    top = this%dis%top(n)
    bot = this%dis%bot(n)
    tthk = this%sk_thick(n)
    !
    ! -- calculate saturation derivitive
    derv = sQuadraticSaturationDerivative(top, bot, hcell)    
    !
    ! -- storage coefficients
    wc2 = this%gammaw * this%beta * area * tthk * this%sk_theta(n) * tled
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

  
  subroutine csub_interbed_wcomp_fc(this, i, n, tled, area,                   &
                                      hcell, hcellold, hcof, rhs)
! ******************************************************************************
! csub_interbed_wcomp_fc -- Calculate water compressibility term for an 
!                           interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B),intent(in) :: i
    integer(I4B),intent(in) :: n
    real(DP), intent(in) :: tled
    real(DP), intent(in) :: area
    real(DP), intent(in) :: hcell
    real(DP), intent(in) :: hcellold
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rhs
    ! locals
    integer(I4B) :: j
    integer(I4B) :: idelay
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: tthk
    real(DP) :: tthk0
    real(DP) :: snold
    real(DP) :: snnew
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
    top = this%dis%top(n)
    bot = this%dis%bot(n)
    tthk = this%sk_thick(n)
    ! -- aquifer saturation
    if (this%stoiconv(n) /= 0) then
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
    idelay = this%idelay(i)
    f = this%gammaw * this%beta * area * tled
    if (idelay == 0) then
      wc1 = f * this%theta0(i) * this%thick0(i)
      wc2 = f * this%theta(i) * this%thick(i)
      hcof = -wc2 * snnew
      rhs = -wc1 * snold * hcellold
    else
      if (this%thick(i) > DZERO) then
        do j = 1, this%ndelaycells
          wc2 = f * this%dbdz(idelay) * this%dbtheta(j, idelay)
          rhs = rhs - wc2 * (snold * this%dbh0(j, idelay) -                     &
                             snnew * this%dbh(j, idelay))
        end do
        rhs = rhs * this%rnb(i)
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
    real(DP) :: f
    real(DP) :: f0
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
    denom = (DONE + void) * (esv - (znode - bot)) * (this%sgs(node) - DONE)
    if (denom /= DZERO) then
      fact = DONE / denom
    end if
    esv = es0
    void = this%csub_calc_void(theta0)
    denom = (DONE + void) * (esv - (znode - bot)) * (this%sgs(node) - DONE)
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
    integer(I4B) :: node
    integer(I4B) :: idelay
    integer(I4B) :: icnvg
    integer(I4B) :: iter
    real(DP) :: dh
    real(DP) :: dhmax
    real(DP) :: area
    real(DP) :: c1
    real(DP) :: c2
    real(DP) :: f
    real(DP), parameter :: dclose = DTEN * DPREC
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    !
    ! -- calculate z for each delay bed cell
    call this%csub_delay_calc_zcell(ib, hcell)
    !
    ! -- calculate geostatic stress for each delay bed cell
    call this%csub_delay_calc_stress(ib, hcell)
    !
    ! -- solve for delay bed heads
    if (this%thick(ib) > DZERO) then
      icnvg = 0
      iter = 0
      idelay = this%idelay(ib)
      do
        iter = iter + 1
        ! -- assemble coefficients
        call this%csub_delay_assemble(ib, hcell)
        ! -- solve for head change in delay intebed cells
        call csub_delay_solve(this%ndelaycells, this%dbal, this%dbad, this%dbau, &
                         this%dbrhs, this%dbdh, this%dbaw)
        ! -- update delay bed head and check convergence
        dhmax = DZERO
        do n = 1, this%ndelaycells
          dh = this%dbdh(n) - this%dbh(n, idelay) 
          if (abs(dh) > abs(dhmax)) then
            dhmax = this%dbdh(n)
          end if
          ! update delay bed heads
          this%dbh(n, idelay) = this%dbdh(n)
        end do
        if (abs(dhmax) < dclose) then
          icnvg = 1
        end if
        if (icnvg == 1) then
          exit
        end if
      end do
    end if
    !
    ! -- return%
    return
  end subroutine csub_delay_sln


  subroutine csub_delay_calc_zcell(this, ib, hcell)
! ******************************************************************************
! csub_delay_calc_zcell -- Calculate z for delay interbeds cells.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(in) :: hcell
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: node
    integer(I4B) :: idelay
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
    !znode = this%sk_znode(node)
    znode = this%csub_calc_znode(node, hcell)
    b = this%thick(ib)
    dz = this%dbdz(idelay)
    if (this%idbhalfcell == 0) then
        dzz = DHALF * b
        z = znode + dzz
    else
        dz = DHALF * dz
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
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    n = this%nodelist(ib)
    idelay = this%idelay(ib)
    node = this%nodelist(ib)
    sigma = this%sk_gs(node)
    topaq = this%dis%top(node)
    botaq = this%dis%bot(node)
    dzhalf = DHALF * this%dbdz(idelay)
    sgm = this%sgm(node)
    sgs = this%sgs(node)
    top = this%dbz(1, idelay) + dzhalf
    !
    ! -- calculate the geostatic load in the aquifer at the top of the interbed
    if (this%igeocalc > 0) then
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
      ! -- geostatic calculated at the bottom of the delay cell
      ! -- CHECK ***evaluate if this is the correct location***
      else
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
        this%dbgeo(n, idelay) = sigma
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
    real(DP) :: es
    real(DP) :: void
    real(DP) :: denom
    real(DP) :: f
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
      if (this%idbhalfcell /= 0) then
        if (n == 1) then
          f = DHALF
        end if
      end if
    !
    ! -- calculate factor for the effective stress case
    else
      es = this%dbes0(n, idelay)
      void = this%csub_calc_void(this%dbtheta(n, idelay))
      denom = (DONE + void) * es
      if (denom /= DZERO) then
        f = DONE / denom
      else
        f = DZERO
      end if
    end if
    sske = f * this%rci(ib)
    if (this%igeocalc == 0) then
      if (this%dbh(n, idelay) < this%dbpcs(n, idelay)) then
        ssk = f * this%ci(ib)
      else
        ssk = sske
      end if
    else
      if (es > this%dbpcs(n, idelay)) then
        ssk = f * this%ci(ib)
      else
        ssk = sske
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
    real(DP) :: c
    real(DP) :: c2
    real(DP) :: c3
    real(DP) :: f
    real(DP) :: sske
    real(DP) :: ssk
    real(DP) :: z
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: h
    real(DP) :: aii
    real(DP) :: r
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    node = this%nodelist(ib)
    !
    !
    do n = 1, this%ndelaycells
      dz = this%dbdz(idelay)
      c = this%kv(ib) / dz
      c2 = DTWO * c
      c3 = DTHREE * c
      f = dz / delt
      call this%csub_delay_calc_ssksske(ib, n, ssk, sske)
      ! -- diagonal and right hand side
      aii = -ssk * f
      z = this%dbz(n, idelay)
      top = z + dz
      bot = z - dz
      h = this%dbh(n, idelay)
      if (this%igeocalc == 0) then
        r = -f * &
             (ssk * (this%dbpcs(n, idelay)) + &
              sske * (this%dbh0(n, idelay) - this%dbpcs(n, idelay)))
      else
        r = -f * &
             (ssk * (this%dbgeo(n, idelay) + z - this%dbpcs(n, idelay)) + &
              sske * (this%dbpcs(n, idelay) - this%dbes0(n, idelay)))
      end if
      if (n == 1 .or. n == this%ndelaycells) then
        if (this%idbhalfcell == 0 .or. n == this%ndelaycells) then
            aii = aii - c3
            r = r - c2 * hcell
        else
            aii = aii - c
        end if
      else
        aii = aii - c2
      end if
      ! -- off diagonals
      ! -- lower
      if (n > 1) then
        this%dbal(n) = c
      end if
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

  subroutine csub_delay_calc_dstor(this, ib, rhse, rhsi)
! ******************************************************************************
! csub_delay_calc_dstor -- Calculate change in storage in a delay interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfCsubType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(inout) :: rhse
    real(DP), intent(inout) :: rhsi
    ! -- local variables
    integer(I4B) :: idelay
    integer(I4B) :: n
    real(DP) :: comp
    real(DP) :: dz
    real(DP) :: sske
    real(DP) :: ssk
    real(DP) :: es
    real(DP) :: denom
    real(DP) :: f
    real(DP) :: fmult
    real(DP) :: v1
    real(DP) :: v2
    real(DP) :: void
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    rhse = DZERO
    rhsi = DZERO
    !
    !
    if (this%thick(ib) > DZERO) then
      do n = 1, this%ndelaycells
        dz = this%dbdz(idelay)
        void = this%csub_calc_void(this%dbtheta(n, idelay))
        fmult = DONE
        call this%csub_delay_calc_ssksske(ib, n, ssk, sske)
        if (this%igeocalc == 0) then
          v1 = ssk * (this%dbpcs(n, idelay) - this%dbh(n, idelay))
          v2 = sske * (this%dbh0(n, idelay) - this%dbpcs(n, idelay))
          fmult = dz
        else
          denom = (DONE + void) * this%dbes0(n, idelay)
          if (denom /= DZERO) then
            f = dz / denom
          else
            f = DZERO
          end if
          v1 = ssk * (this%dbes(n, idelay) - this%dbpcs(n, idelay))
          v2 = sske * (this%dbpcs(n, idelay) - this%dbes0(n, idelay))
        end if
        if (ssk /= sske) then
          rhsi = rhsi + v1 * fmult
          rhse = rhse + v2 * fmult
        else
          rhse = rhse + (v1 + v2) * fmult
        end if
      end do
    end if
    !
    ! -- return
    return
  end subroutine csub_delay_calc_dstor

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
    real(DP) :: hcof
    real(DP) :: v
    real(DP) :: void
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
    real(DP) :: dz
    real(DP) :: sske
    real(DP) :: ssk
    real(DP) :: es
    real(DP) :: denom
    real(DP) :: f
    real(DP) :: v
    real(DP) :: void
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    comp = DZERO
    !
    !
    if (this%thick(ib) > DZERO) then
      do n = 1, this%ndelaycells
        dz = this%dbdz(idelay)
        void = this%csub_calc_void(this%dbtheta(n, idelay))
        call this%csub_delay_calc_ssksske(ib, n, ssk, sske)
        if (this%igeocalc == 0) then
          v = ssk * (this%dbpcs(n, idelay) - this%dbh(n, idelay))
          v = v + sske * (this%dbh0(n, idelay) - this%dbpcs(n, idelay))
          v = v * dz
        else
          denom = (DONE + void) * this%dbes0(n, idelay)
          if (denom /= DZERO) then
            f = dz / denom
          else
            f = DZERO
          end if
          v = ssk * (this%dbes(n, idelay) - this%dbpcs(n, idelay))
          v = v + sske * (this%dbpcs(n, idelay) - this%dbes0(n, idelay))
        end if
        comp = comp + v
      end do
    end if
    !
    ! -- adjust compaction
    comp = comp * this%dbfact
    !
    ! -- fill compaction
    this%comp(ib) = comp
    this%tcomp(ib) = this%tcomp(ib) + comp * this%rnb(ib)
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
    integer(I4B) :: node
    real(DP) :: area
    real(DP) :: f
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
      node = this%nodelist(ib)
      area = this%dis%get_area(node)
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
  !   -- Store observation type supported by ibc package.
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
    !    for ibc observation type.
    call this%obs%StoreObsType('ibc', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for compaction observation type.
    call this%obs%StoreObsType('compaction', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for total-compaction observation type.
    call this%obs%StoreObsType('total-compaction', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for total-compaction observation type.
    call this%obs%StoreObsType('thickness', .true., indx)
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
    !    for compaction-cell observation type.
    call this%obs%StoreObsType('compaction-cell', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for compaction-skeletal observation type.
    call this%obs%StoreObsType('compaction-skeletal', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for effective-stress-cell observation type.
    call this%obs%StoreObsType('preconstress-cell', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => csub_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for effective-stress-cell observation type.
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
    ! Write simulated values for all ibc observations
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
              v = this%gwflow(n)
            case ('COMPACTION')
              v = this%comp(n)
            case ('TOTAL-COMPACTION')
              v = this%tcomp(n)
            case ('THICKNESS')
              v = this%thick(n)
            case ('GSTRESS-CELL')
              v = this%sk_gs(n)
            case ('ESTRESS-CELL')
              v = this%sk_es(n)
            case ('COMPACTION-SKELETAL')
              v = this%sk_tcomp(n)
            case ('COMPACTION-CELL')
              ! -- add the skeletal component
              if (j == 1) then
                v = this%sk_tcomp(n)
              else
                v = this%tcomp(n)
              end if
            case ('PRECONSTRESS')
               if (n > this%ndelaycells) then
                r = real(n, DP) / real(this%ndelaycells, DP)
                idelay = int(floor(r)) + 1
                ncol = mod(n, this%ndelaycells)
              else
                idelay = 1
                ncol = n
              end if
              v = this%dbpcs(ncol, idelay)
            case ('DELAY-HEAD')
              if (n > this%ndelaycells) then
                r = real(n, DP) / real(this%ndelaycells, DP)
                idelay = int(floor(r)) + 1
                ncol = mod(n, this%ndelaycells)
              else
                idelay = 1
                ncol = n
              end if
              v = this%dbh(ncol, idelay)
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
      else if (obsrv%ObsTypeId == 'GSTRESS-CELL' .or. &
               obsrv%ObsTypeId == 'ESTRESS-CELL' .or. &
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
        if (obsrv%ObsTypeId == 'COMPACTION-CELL') then 
          n = size(obsrv%indxbnds)
          if (n == 0) then
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
    !    the ID string of an observation definition for ibc-package observations.
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

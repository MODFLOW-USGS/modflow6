module LakModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, LENBOUNDNAME, LENTIMESERIESNAME,      &
                             DZERO, DPREC, DEM30, DEM9, DEM6, DEM5,            &
                             DEM4, DEM2, DEM1, DHALF, DP7, DONE,               &
                             DTWO, DPI, DTHREE, DEIGHT, DTEN, DHUNDRED, DEP20, &
                             DONETHIRD, DTWOTHIRDS, DFIVETHIRDS,               &
                             DGRAVITY, DCD,                                    &
                             NAMEDBOUNDFLAG, LENFTYPE, LENPACKAGENAME,         &
                             DNODATA
  use MemoryTypeModule, only: MemoryTSType
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_setptr,     &
                                 mem_deallocate
  use SmoothingModule,  only: sQuadraticSaturation, sQSaturation,              &
                              sQuadraticSaturationDerivative,                  &
                              sQSaturationDerivative
  use BndModule, only: BndType
  use BudgetModule, only : BudgetType

  use ObserveModule,        only: ObserveType
  use ObsModule, only: ObsType
  use InputOutputModule, only: get_node, URWORD, extract_idnum_or_bndname
  use BaseDisModule, only: DisBaseType
  use SimModule,           only: count_errors, store_error, ustop, &
                                 store_error_unit
  use ArrayHandlersModule, only: ExpandArray
  use BlockParserModule,   only: BlockParserType
  use BaseDisModule,       only: DisBaseType
  !
  implicit none
  !
  private
  public :: lak_create
  !
  character(len=LENFTYPE)       :: ftype = 'LAK'
  character(len=LENPACKAGENAME) :: text  = '             LAK'
  !
  type LakTabType
    real(DP), pointer, dimension(:)  :: tabstage => null()
    real(DP), pointer, dimension(:)  :: tabvolume => null()
    real(DP), pointer, dimension(:)  :: tabsarea => null()
    real(DP), pointer, dimension(:)  :: tabwarea => null()
  end type LakTabType
  !
  type, extends(BndType) :: LakType
    ! -- scalars
    ! -- characters
    character(len=16), dimension(:), pointer :: clakbudget => NULL()
    character(len=16), dimension(:), pointer :: cauxcbc => NULL()
    ! -- integers
    integer(I4B), pointer :: iprhed => null()
    integer(I4B), pointer :: istageout => null()
    integer(I4B), pointer :: ibudgetout => null()
    integer(I4B), pointer :: cbcauxitems => NULL()
    integer(I4B), pointer :: isteady => NULL()
    integer(I4B), pointer :: nlakes => NULL()
    integer(I4B), pointer :: noutlets => NULL()
    integer(I4B), pointer :: ntables => NULL()
    real(DP), pointer :: convlength => NULL()
    real(DP), pointer :: convtime => NULL()
    real(DP), pointer :: outdmax => NULL()
    integer(I4B), pointer :: igwhcopt => NULL()
    integer(I4B), pointer :: iconvchk => NULL()
    integer(I4B), pointer :: iconvresidchk => NULL()
    real(DP), pointer :: surfdep => NULL()
    real(DP), pointer :: delh => NULL()
    real(DP), pointer :: pdmax => NULL()
    integer(I4B), pointer :: check_attr => NULL()
    ! -- for budgets
    integer(I4B), pointer :: bditems => NULL()
    ! -- vectors
    ! -- lake data
    integer(I4B), pointer, dimension(:) :: nlakeconn => null()
    integer(I4B), pointer, dimension(:) :: idxlakeconn => null()
    integer(I4B), pointer, dimension(:) :: ntabrow => null()
    real(DP), pointer, dimension(:)  :: strt => null()
    real(DP), pointer, dimension(:)  :: laketop => null()
    real(DP), pointer, dimension(:)  :: lakebot => null()
    real(DP), pointer, dimension(:)  :: sareamax => null()
    character(len=LENBOUNDNAME), pointer, dimension(:) :: lakename => null()
    character (len=8), pointer, dimension(:) :: status => null()
    real(DP), pointer, dimension(:)  :: avail => null()
    real(DP), pointer, dimension(:)  :: lkgwsink => null()
    ! -- time series aware data
    type (MemoryTSType), pointer, dimension(:) :: stage => null()
    type (MemoryTSType), pointer, dimension(:) :: rainfall => null()
    type (MemoryTSType), pointer, dimension(:) :: evaporation => null()
    type (MemoryTSType), pointer, dimension(:) :: runoff => null()
    type (MemoryTSType), pointer, dimension(:) :: inflow => null()
    type (MemoryTSType), pointer, dimension(:) :: withdrawal => null()
    type (MemoryTSType), pointer, dimension(:) :: lauxvar => null()
    !
    ! -- table data
    type (LakTabType), pointer, dimension(:) :: laketables => null()
    !
    ! -- lake solution data
    integer(I4B), pointer, dimension(:) :: ncncvr => null()
    real(DP), pointer, dimension(:)  :: surfin => null()
    real(DP), pointer, dimension(:)  :: surfout => null()
    real(DP), pointer, dimension(:)  :: surfout1 => null()
    real(DP), pointer, dimension(:)  :: precip => null()
    real(DP), pointer, dimension(:)  :: precip1 => null()
    real(DP), pointer, dimension(:)  :: evap => null()
    real(DP), pointer, dimension(:)  :: evap1 => null()
    real(DP), pointer, dimension(:)  :: evapo => null()
    real(DP), pointer, dimension(:)  :: withr => null()
    real(DP), pointer, dimension(:)  :: withr1 => null()
    real(DP), pointer, dimension(:)  :: flwin => null()
    real(DP), pointer, dimension(:)  :: flwiter => null()
    real(DP), pointer, dimension(:)  :: flwiter1 => null()
    real(DP), pointer, dimension(:)  :: seep => null()
    real(DP), pointer, dimension(:)  :: seep1 => null()
    real(DP), pointer, dimension(:)  :: seep0 => null()
    real(DP), pointer, dimension(:)  :: stageiter => null()
    real(DP), pointer, dimension(:)  :: chterm => null()
    !
    ! -- lake convergence
    integer(I4B), pointer, dimension(:) :: iseepc => null()
    integer(I4B), pointer, dimension(:) :: idhc => null()
    real(DP), pointer, dimension(:) :: en1 => null()
    real(DP), pointer, dimension(:) :: en2 => null()
    real(DP), pointer, dimension(:) :: r1 => null()
    real(DP), pointer, dimension(:) :: r2 => null()
    real(DP), pointer, dimension(:) :: dh0 => null()
    real(DP), pointer, dimension(:) :: s0 => null()
    !
    ! -- lake connection data
    integer(I4B), pointer, dimension(:) :: imap => null()
    integer(I4B), pointer, dimension(:) :: cellid => null()
    integer(I4B), pointer, dimension(:) :: nodesontop => null()
    integer(I4B), pointer, dimension(:) :: ictype => null()
    real(DP), pointer, dimension(:)  :: bedleak => null()
    real(DP), pointer, dimension(:)  :: belev => null()
    real(DP), pointer, dimension(:)  :: telev => null()
    real(DP), pointer, dimension(:)  :: connlength => null()
    real(DP), pointer, dimension(:)  :: connwidth => null()
    real(DP), pointer, dimension(:)  :: sarea => null()
    real(DP), pointer, dimension(:)  :: warea => null()
    real(DP), pointer, dimension(:)  :: satcond => null()
    real(DP), pointer, dimension(:)  :: simcond => null()
    real(DP), pointer, dimension(:)  :: simlakgw => null()
    !
    ! -- lake outlet data
    integer(I4B), pointer, dimension(:) :: lakein => null()
    integer(I4B), pointer, dimension(:) :: lakeout => null()
    integer(I4B), pointer, dimension(:) :: iouttype => null()
    type (MemoryTSType), pointer, dimension(:) :: outrate => null()
    type (MemoryTSType), pointer, dimension(:) :: outinvert => null()
    type (MemoryTSType), pointer, dimension(:) :: outwidth => null()
    type (MemoryTSType), pointer, dimension(:) :: outrough => null()
    type (MemoryTSType), pointer, dimension(:) :: outslope => null()
    real(DP), pointer, dimension(:)  :: simoutrate => null()
    !
    ! -- lake output data
    real(DP), dimension(:), pointer, contiguous :: qauxcbc => null()
    real(DP), dimension(:), pointer, contiguous :: dbuff => null()
    real(DP), dimension(:), pointer, contiguous :: qleak => null()
    real(DP), dimension(:), pointer, contiguous :: qsto => null()
    !
    ! -- derived types
    type(BudgetType), pointer :: budget => NULL()
    ! -- pointer to gwf iss and gwf hk
    integer(I4B), pointer :: gwfiss => NULL()
    real(DP), dimension(:), pointer :: gwfk11 => NULL()
    real(DP), dimension(:), pointer :: gwfk33 => NULL()
    real(DP), dimension(:), pointer :: gwfsat => NULL()
    integer(I4B), pointer :: gwfik33 => NULL()
    !
    ! -- package x, xold, and ibound
    integer(I4B), pointer, dimension(:) :: iboundpak     => null() !package ibound
    real(DP), pointer, dimension(:)     :: xnewpak       => null() !package x vector
    real(DP), pointer, dimension(:)     :: xoldpak       => null() !package xold vector
    !
    ! -- type bound procedures
    contains
    procedure :: lak_allocate_scalars
    procedure :: lak_allocate_arrays
    procedure :: bnd_options => lak_options
    procedure :: read_dimensions => lak_read_dimensions
    procedure :: read_initial_attr => lak_read_initial_attr
    procedure :: set_pointers => lak_set_pointers
    procedure :: bnd_ar => lak_ar
    procedure :: bnd_rp => lak_rp
    procedure :: bnd_ad => lak_ad
    procedure :: bnd_cf => lak_cf
    procedure :: bnd_fc => lak_fc
    procedure :: bnd_fn => lak_fn
    procedure :: bnd_cc => lak_cc
    procedure :: bnd_bd => lak_bd
    procedure :: bnd_ot => lak_ot
    procedure :: bnd_da => lak_da
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => lak_obs_supported
    procedure, public :: bnd_df_obs => lak_df_obs
    procedure, public :: bnd_rp_obs => lak_rp_obs
    ! -- private procedures
    procedure, private :: lak_read_lakes
    procedure, private :: lak_read_lake_connections
    procedure, private :: lak_read_outlets
    procedure, private :: lak_read_tables
    procedure, private :: lak_read_table
    !procedure, private :: lak_check_attributes
    procedure, private :: lak_check_valid
    procedure, private :: lak_set_stressperiod
    procedure, private :: lak_set_attribute_error
    procedure, private :: lak_cfupdate
    procedure, private :: lak_bd_obs
    procedure, private :: lak_calculate_sarea
    procedure, private :: lak_calculate_warea
    procedure, private :: lak_calculate_conn_warea
    procedure, private :: lak_calculate_vol
    procedure, private :: lak_calculate_conductance
    procedure, private :: lak_calculate_cond_head
    procedure, private :: lak_calculate_conn_conductance
    procedure, private :: lak_calculate_conn_exchange
    procedure, private :: lak_estimate_conn_exchange
    procedure, private :: lak_calculate_storagechange
    procedure, private :: lak_calculate_rainfall
    procedure, private :: lak_calculate_runoff
    procedure, private :: lak_calculate_inflow
    procedure, private :: lak_calculate_external
    procedure, private :: lak_calculate_withdrawal
    procedure, private :: lak_calculate_evaporation
    procedure, private :: lak_calculate_outlet_inflow
    procedure, private :: lak_calculate_outlet_outflow
    procedure, private :: lak_get_internal_inlet
    procedure, private :: lak_get_internal_outlet
    procedure, private :: lak_get_external_outlet
    procedure, private :: lak_get_internal_mover
    procedure, private :: lak_get_external_mover
    procedure, private :: lak_get_outlet_tomover
    procedure, private :: lak_accumulate_chterm
    procedure, private :: lak_vol2stage
    procedure, private :: lak_solve
    procedure, private :: lak_calculate_available
    procedure, private :: lak_calculate_residual
    procedure, private :: lak_linear_interpolation
  end type LakType

contains

  subroutine lak_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! lak_create -- Create a New LAKE Package
! Subroutine: (1) create new-style package
!             (2) point bndobj to the new package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BndType), pointer :: packobj
    integer(I4B),intent(in) :: id
    integer(I4B),intent(in) :: ibcnum
    integer(I4B),intent(in) :: inunit
    integer(I4B),intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    type(LakType), pointer :: lakobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(lakobj)
    packobj => lakobj
    !
    ! -- create name and origin
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call lakobj%lak_allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()

    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 3
    packobj%iscloc = 0  ! not supported
    !
    ! -- return
    return
  end subroutine lak_create

  subroutine lak_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),   intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- allocate the object and assign values to object variables
    call mem_allocate(this%iprhed, 'IPRHED', this%origin)
    call mem_allocate(this%istageout, 'ISTAGEOUT', this%origin)
    call mem_allocate(this%ibudgetout, 'IBUDGETOUT', this%origin)
    call mem_allocate(this%isteady, 'ISTEADY', this%origin)
    call mem_allocate(this%nlakes, 'NLAKES', this%origin)
    call mem_allocate(this%noutlets, 'NOUTLETS', this%origin)
    call mem_allocate(this%ntables, 'NTABLES', this%origin)
    call mem_allocate(this%convlength, 'CONVLENGTH', this%origin)
    call mem_allocate(this%convtime, 'CONVTIME', this%origin)
    call mem_allocate(this%outdmax, 'OUTDMAX', this%origin)
    call mem_allocate(this%igwhcopt, 'IGWHCOPT', this%origin)
    call mem_allocate(this%iconvchk, 'ICONVCHK', this%origin)
    call mem_allocate(this%iconvresidchk, 'ICONVRESIDCHK', this%origin)
    call mem_allocate(this%surfdep, 'SURFDEP', this%origin)
    call mem_allocate(this%delh, 'DELH', this%origin)
    call mem_allocate(this%pdmax, 'PDMAX', this%origin)
    call mem_allocate(this%check_attr, 'check_attr', this%origin)
    call mem_allocate(this%bditems, 'BDITEMS', this%origin)
    call mem_allocate(this%cbcauxitems, 'CBCAUXITEMS', this%origin)
    !
    ! -- Set values
    this%iprhed = 0
    this%istageout = 0
    this%ibudgetout = 0
    this%nlakes = 0
    this%noutlets = 0
    this%ntables = 0
    this%convlength = DONE
    this%convtime = DONE
    this%outdmax = DZERO
    this%igwhcopt = 0
    this%iconvchk = 1
    this%iconvresidchk = 1
    this%surfdep = DZERO
    this%delh = DEM5
    this%pdmax = DEM1
    this%isteady = 0
    this%bditems = 11
    this%cbcauxitems = 1
    !this%imover = 0
    !
    ! -- return
    return
  end subroutine lak_allocate_scalars

  subroutine lak_allocate_arrays(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(LakType),   intent(inout) :: this
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_arrays()
    !
    ! -- allocate character array for budget text
    allocate(this%clakbudget(this%bditems))
    !
    !-- fill clakbudget
    this%clakbudget(1)  = '             GWF'
    this%clakbudget(2)  = '        RAINFALL'
    this%clakbudget(3)  = '     EVAPORATION'
    this%clakbudget(4)  = '          RUNOFF'
    this%clakbudget(5)  = '      EXT-INFLOW'
    this%clakbudget(6)  = '      WITHDRAWAL'
    this%clakbudget(7)  = '     EXT-OUTFLOW'
    this%clakbudget(8)  = '         STORAGE'
    this%clakbudget(9)  = '        CONSTANT'
    this%clakbudget(10) = '        FROM-MVR'
    this%clakbudget(11) = '          TO-MVR'
    !
    ! -- allocate and initialize dbuff
    if (this%istageout > 0) then
      call mem_allocate(this%dbuff, this%nlakes, 'DBUFF', this%origin)
      do i = 1, this%nlakes
        this%dbuff(i) = DZERO
      end do
    else
      call mem_allocate(this%dbuff, 0, 'DBUFF', this%origin)
    end if
    !
    ! -- allocate character array for budget text
    allocate(this%cauxcbc(this%cbcauxitems))
    !
    ! -- allocate and initialize qauxcbc
    call mem_allocate(this%qauxcbc, this%cbcauxitems, 'QAUXCBC', this%origin)
    do i = 1, this%cbcauxitems
      this%qauxcbc(i) = DZERO
    end do
    !
    ! -- allocate qleak and qsto
    call mem_allocate(this%qleak, this%maxbound, 'QLEAK', this%origin)
    do i = 1, this%maxbound
      this%qleak(i) = DZERO
    end do
    call mem_allocate(this%qsto, this%nlakes, 'QSTO', this%origin)
    do i = 1, this%nlakes
      this%qsto(i) = DZERO
    end do
    !
    ! -- return
    return
  end subroutine lak_allocate_arrays

  subroutine lak_read_lakes(this)
! ******************************************************************************
! pak1read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    ! -- dummy
    class(LakType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: text
    character(len=LENBOUNDNAME) :: bndName, bndNameTemp
    character(len=9) :: cno
    character(len=50), dimension(:), allocatable :: caux
    integer(I4B) :: ierr, ival
    logical :: isfound, endOfBlock
    integer(I4B) :: n
    integer(I4B) :: ii, jj
    integer(I4B) :: iaux
    integer(I4B) :: itmp
    integer(I4B) :: nlak
    integer(I4B) :: nconn
    integer(I4B), dimension(:), pointer :: nboundchk
    ! -- format
    !
    ! -- code
    !
    ! -- initialize itmp
    itmp = 0
    !
    ! -- allocate lake data
    call mem_allocate(this%nlakeconn, this%nlakes, 'NLAKECONN', this%origin)
    call mem_allocate(this%idxlakeconn, this%nlakes+1, 'IDXLAKECONN', this%origin)
    call mem_allocate(this%ntabrow, this%nlakes, 'NTABROW', this%origin)
    call mem_allocate(this%strt, this%nlakes, 'STRT', this%origin)
    call mem_allocate(this%laketop, this%nlakes, 'LAKETOP', this%origin)
    call mem_allocate(this%lakebot, this%nlakes, 'LAKEBOT', this%origin)
    call mem_allocate(this%sareamax, this%nlakes, 'SAREAMAX', this%origin)
    call mem_allocate(this%stage, this%nlakes, 'STAGE', this%origin)
    call mem_allocate(this%rainfall, this%nlakes, 'RAINFALL', this%origin)
    call mem_allocate(this%evaporation, this%nlakes, 'EVAPORATION', this%origin)
    call mem_allocate(this%runoff, this%nlakes, 'RUNOFF', this%origin)
    call mem_allocate(this%inflow, this%nlakes, 'INFLOW', this%origin)
    call mem_allocate(this%withdrawal, this%nlakes, 'WITHDRAWAL', this%origin)
    call mem_allocate(this%lauxvar, this%naux*this%nlakes, 'LAUXVAR', this%origin)
    call mem_allocate(this%avail, this%nlakes, 'AVAIL', this%origin)
    call mem_allocate(this%lkgwsink, this%nlakes, 'LKGWSINK', this%origin)
    call mem_allocate(this%ncncvr, this%nlakes, 'NCNCVR', this%origin)
    call mem_allocate(this%surfin, this%nlakes, 'SURFIN', this%origin)
    call mem_allocate(this%surfout, this%nlakes, 'SURFOUT', this%origin)
    call mem_allocate(this%surfout1, this%nlakes, 'SURFOUT1', this%origin)
    call mem_allocate(this%precip, this%nlakes, 'PRECIP', this%origin)
    call mem_allocate(this%precip1, this%nlakes, 'PRECIP1', this%origin)
    call mem_allocate(this%evap, this%nlakes, 'EVAP', this%origin)
    call mem_allocate(this%evap1, this%nlakes, 'EVAP1', this%origin)
    call mem_allocate(this%evapo, this%nlakes, 'EVAPO', this%origin)
    call mem_allocate(this%withr, this%nlakes, 'WITHR', this%origin)
    call mem_allocate(this%withr1, this%nlakes, 'WITHR1', this%origin)
    call mem_allocate(this%flwin, this%nlakes, 'FLWIN', this%origin)
    call mem_allocate(this%flwiter, this%nlakes, 'FLWITER', this%origin)
    call mem_allocate(this%flwiter1, this%nlakes, 'FLWITER1', this%origin)
    call mem_allocate(this%seep, this%nlakes, 'SEEP', this%origin)
    call mem_allocate(this%seep1, this%nlakes, 'SEEP1', this%origin)
    call mem_allocate(this%seep0, this%nlakes, 'SEEP0', this%origin)
    call mem_allocate(this%stageiter, this%nlakes, 'STAGEITER', this%origin)
    call mem_allocate(this%chterm, this%nlakes, 'CHTERM', this%origin)
    !
    ! -- lake boundary and stages
    call mem_allocate(this%iboundpak, this%nlakes, 'IBOUND', this%origin)
    call mem_allocate(this%xnewpak, this%nlakes, 'XNEWPAK', this%origin)
    call mem_allocate(this%xoldpak, this%nlakes, 'XOLDPAK', this%origin)
    !
    ! -- lake iteration variables
    call mem_allocate(this%iseepc, this%nlakes, 'ISEEPC', this%origin)
    call mem_allocate(this%idhc, this%nlakes, 'IDHC', this%origin)
    call mem_allocate(this%en1, this%nlakes, 'EN1', this%origin)
    call mem_allocate(this%en2, this%nlakes, 'EN2', this%origin)
    call mem_allocate(this%r1, this%nlakes, 'R1', this%origin)
    call mem_allocate(this%r2, this%nlakes, 'R2', this%origin)
    call mem_allocate(this%dh0, this%nlakes, 'DH0', this%origin)
    call mem_allocate(this%s0, this%nlakes, 'S0', this%origin)
    !
    ! -- allocate character storage not managed by the memory manager
    allocate(this%lakename(this%nlakes)) ! ditch after boundnames allocated??
    allocate(this%status(this%nlakes))
    !
    do n = 1, this%nlakes
      this%ntabrow(n) = 0
      this%status(n) = 'ACTIVE'
      this%laketop(n) = -DEP20
      this%lakebot(n) =  DEP20
      this%sareamax(n) = DZERO
      this%iboundpak(n) = 1
      this%xnewpak(n) = DEP20
      this%xoldpak(n) = DEP20
    end do
    !
    ! -- allocate local storage for aux variables
    if (this%naux > 0) then
      allocate(caux(this%naux))
    end if
    !
    ! -- allocate and initialize temporary variables
    allocate(nboundchk(this%nlakes))
    do n = 1, this%nlakes
      nboundchk(n) = 0
    end do
    !
    ! -- read lake well data
    ! -- get lakes block
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr, supportOpenClose=.true.)
    !
    ! -- parse locations block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' PACKAGEDATA'
      nlak = 0
      nconn = 0
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        n = this%parser%GetInteger()

        if (n < 1 .or. n > this%nlakes) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. lakeno MUST BE > 0 and <= ', this%nlakes
          call store_error(errmsg)
          cycle
        end if
        
        ! -- increment nboundchk
        nboundchk(n) = nboundchk(n) + 1

        ! -- strt
        this%strt(n) = this%parser%GetDouble()

        ! nlakeconn
        ival = this%parser%GetInteger()

        if (ival < 0) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. nlakecon MUST BE >= 0 for lake ', n
          call store_error(errmsg)
        end if

        nconn = nconn + ival
        this%nlakeconn(n) = ival

        ! -- get aux data
        do iaux = 1, this%naux
          call this%parser%GetString(caux(iaux))
        end do

        ! -- set default bndName
        write (cno,'(i9.9)') n
        bndName = 'Lake' // cno

        ! -- lakename
        if (this%inamedbound /= 0) then
          call this%parser%GetStringCaps(bndNameTemp)
          if (bndNameTemp /= '') then
            bndName = bndNameTemp(1:16)
          endif
        end if
        this%lakename(n) = bndName

        ! -- fill time series aware data
        ! -- fill aux data
        do iaux = 1, this%naux
          !
          ! -- Assign boundary name
          if (this%inamedbound==1) then
            bndName = this%lakename(n)
          else
            bndName = ''
          end if
          text = caux(iaux)
          jj = 1 !iaux
          ii = (n-1) * this%naux + iaux
          call read_single_value_or_time_series(text, &
                                                this%lauxvar(ii)%value, &
                                                this%lauxvar(ii)%name, &
                                                DZERO,  &
                                                this%Name, 'AUX', this%TsManager, &
                                                this%iprpak, n, jj, &
                                                this%auxname(iaux), &
                                                bndName, this%parser%iuactive)
        end do

        nlak = nlak + 1
      end do
      !
      ! -- check for duplicate or missing lakes
      do n = 1, this%nlakes
        if (nboundchk(n) == 0) then
          write(errmsg,'(a,1x,i0)')  'ERROR.  NO DATA SPECIFIED FOR LAKE', n
          call store_error(errmsg)
        else if (nboundchk(n) > 1) then
          write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                             &
            'ERROR.  DATA FOR LAKE', n, 'SPECIFIED', nboundchk(n), 'TIMES'
          call store_error(errmsg)
        end if
      end do

      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' PACKAGEDATA'
    else
      call store_error('ERROR.  REQUIRED PACKAGEDATA BLOCK NOT FOUND.')
    end if
    !
    ! -- terminate if any errors were detected
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- set MAXBOUND
    this%MAXBOUND = nconn
    write(this%iout,'(//4x,a,i7)') 'MAXBOUND = ', this%maxbound

    ! -- set idxlakeconn
    this%idxlakeconn(1) = 1
    do n = 1, this%nlakes
      this%idxlakeconn(n+1) = this%idxlakeconn(n) + this%nlakeconn(n)
    end do
    !
    ! -- deallocate local storage for aux variables
    if (this%naux > 0) then
      deallocate(caux)
    end if
    !
    ! -- deallocate local storage for nboundchk
    deallocate(nboundchk)
    !
    ! -- return
    return
  end subroutine lak_read_lakes

  subroutine lak_read_lake_connections(this)
! ******************************************************************************
! lak_read_lake_connections -- Read the lake connections for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(LakType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: keyword, cellid
    integer(I4B) :: ierr, ival
    logical :: isfound, endOfBlock
    real(DP) :: rval
    integer(I4B) :: j, n
    integer(I4B) :: nn
    integer(I4B) :: ipos, ipos0
    integer(I4B) :: icellid, icellid0
    real(DP) :: top, bot
    integer(I4B), dimension(:), pointer :: nboundchk

    ! -- format
    !
    ! -- code
    !
    ! -- allocate local storage
    allocate(nboundchk(this%MAXBOUND))
    do n = 1, this%MAXBOUND
      nboundchk(n) = 0
    end do
    !
    ! -- get connectiondata block
    call this%parser%GetBlock('CONNECTIONDATA', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse connectiondata block if detected
    if (isfound) then

      ! -- allocate connection data using memory manager
      call mem_allocate(this%imap, this%MAXBOUND, 'IMAP', this%origin)
      call mem_allocate(this%cellid, this%MAXBOUND, 'CELLID', this%origin)
      call mem_allocate(this%nodesontop, this%MAXBOUND, 'NODESONTOP', this%origin)
      call mem_allocate(this%ictype, this%MAXBOUND, 'ICTYPE', this%origin)
      call mem_allocate(this%bedleak, this%MAXBOUND, 'BEDLEAK', this%origin) ! don't need to save this - use a temporary vector
      call mem_allocate(this%belev, this%MAXBOUND, 'BELEV', this%origin)
      call mem_allocate(this%telev, this%MAXBOUND, 'TELEV', this%origin)
      call mem_allocate(this%connlength, this%MAXBOUND, 'CONNLENGTH', this%origin)
      call mem_allocate(this%connwidth, this%MAXBOUND, 'CONNWIDTH', this%origin)
      call mem_allocate(this%sarea, this%MAXBOUND, 'SAREA', this%origin)
      call mem_allocate(this%warea, this%MAXBOUND, 'WAREA', this%origin)
      call mem_allocate(this%satcond, this%MAXBOUND, 'SATCOND', this%origin)
      call mem_allocate(this%simcond, this%MAXBOUND, 'SIMCOND', this%origin)
      call mem_allocate(this%simlakgw, this%MAXBOUND, 'SIMLAKGW', this%origin)
      

      ! -- process the lake connection data
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' LAKE_CONNECTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        n = this%parser%GetInteger()

        if (n < 1 .or. n > this%nlakes) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. lakeno MUST BE > 0 and <= ', this%nlakes
          call store_error(errmsg)
          cycle
        end if

        ! -- read connection number
        ival = this%parser%GetInteger()
        if (ival <1 .or. ival > this%nlakeconn(n)) then
          write(errmsg,'(4x,a,1x,i4,1x,a,1x,i6)') &
            '****ERROR. iconn FOR LAKE ', n, 'MUST BE > 1 and <= ', this%nlakeconn(n)
          call store_error(errmsg)
          cycle
        end if

        j = ival
        ipos = this%idxlakeconn(n) + ival - 1

        ! -- set imap
        this%imap(ipos) = n
        
        !
        ! -- increment nboundchk
        nboundchk(ipos) = nboundchk(ipos) + 1

        ! -- read gwfnodes from the line
        call this%parser%GetCellid(this%dis%ndim, cellid)
        nn = this%dis%noder_from_cellid(cellid, &
                                    this%parser%iuactive, this%iout)
        !
        ! -- determine if a valid cell location was provided
        if (nn < 1) then
          write(errmsg,'(4x,a,1x,i4,1x,a,1x,i4)') &
            '****ERROR. INVALID cellid FOR LAKE ', n, 'connection', j
          call store_error(errmsg)
        end if

        ! -- set gwf cellid for connection
        this%cellid(ipos) = nn
        this%nodesontop(ipos) = nn

        ! -- read ictype
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('VERTICAL')
            this%ictype(ipos) = 0
          case ('HORIZONTAL')
            this%ictype(ipos) = 1
          case ('EMBEDDEDH')
            this%ictype(ipos) = 2
          case ('EMBEDDEDV')
            this%ictype(ipos) = 3
          case default
            write(errmsg,'(4x,a,1x,i4,1x,a,1x,i4,1x,a,a,a)') &
              '****ERROR. UNKNOWN ctype FOR LAKE ', n, 'connection', j, &
              '(', trim(keyword), ')'
            call store_error(errmsg)
        end select

        ! -- bed leakance
        this%bedleak(ipos) = this%parser%GetDouble()

        if (this%bedleak(ipos) < dzero) then
          write(errmsg,'(4x,a,1x,i4,1x,a)') &
            '****ERROR. bedleak FOR LAKE ', n, 'MUST BE >= 0'
          call store_error(errmsg)
        end if

        ! -- belev
        this%belev(ipos) = this%parser%GetDouble()

        ! -- telev
        this%telev(ipos) = this%parser%GetDouble()

        ! -- connection length
        rval = this%parser%GetDouble()
        if (rval < dzero)  then
          if (this%ictype(ipos) == 1 .or. this%ictype(ipos) == 2 .or. this%ictype(ipos) == 3) then
            write(errmsg,'(4x,a,1x,i4,1x,a,1x,i4,1x,a)') &
              '****ERROR. connection length (connlength) FOR LAKE ', n, ' HORIZONTAL CONNECTION ', j, &
              'MUST BE >= 0'
            call store_error(errmsg)
          else
            rval = DZERO
          end if
        end if
        this%connlength(ipos) = rval

        ! -- connection width
        rval = this%parser%GetDouble()
        if (rval < dzero)  then
          if (this%ictype(ipos) == 1) then
            write(errmsg,'(4x,a,1x,i4,1x,a,1x,i4,1x,a)') &
              '****ERROR. cell width (connwidth) FOR LAKE ', n, ' HORIZONTAL CONNECTION ', j, &
              'MUST BE >= 0'
            call store_error(errmsg)
          else
            rval = DZERO
          end if
        end if
        this%connwidth(ipos) = rval
      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' CONNECTIONDATA'
    else
      call store_error('ERROR.  REQUIRED CONNECTIONDATA BLOCK NOT FOUND.')
    end if
    !
    ! -- terminate if any errors were detected
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- check that embedded lakes have only one connection
    do n = 1, this%nlakes
      j = 0
      do ipos = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
        if (this%ictype(ipos) /= 2 .and. this%ictype(ipos) /= 3) cycle
        j = j + 1
        if (j > 1) then
           write(errmsg,'(4x,a,1x,i4,1x,a,1x,i4,1x,a)') &
                  '****ERROR. nlakeconn FOR LAKE', n, 'EMBEDDED CONNECTION', j, &
                  ' EXCEEDS 1.'
           call store_error(errmsg)
        end if
      end do
    end do
    ! -- check that an embedded lake is not in the same cell as a lake
    !   with a vertical connection
    do n = 1, this%nlakes
      ipos0 = this%idxlakeconn(n)
      icellid0 = this%cellid(ipos0)
      if (this%ictype(ipos0) /= 2 .and. this%ictype(ipos0) /= 3) cycle
      do nn = 1, this%nlakes
        if (nn == n) cycle
        j = 0
        do ipos = this%idxlakeconn(nn), this%idxlakeconn(nn+1)-1
          j = j + 1
          icellid = this%cellid(ipos)
          if (icellid == icellid0) then
            if (this%ictype(ipos) == 0) then
                write(errmsg,'(4x,a,1x,i4,1x,a,1x,i4,1x,a,1x,i4,1x,a)') &
                      '****ERROR. EMBEDDED LAKE', n,                   &
                      'CANNOT COINCIDE WITH VERTICAL CONNECTION', j,   &
                      'IN LAKE', nn, '.'
                call store_error(errmsg)
            end if
          end if
        end do
      end do
    end do
    !
    ! -- process the data
    do n = 1, this%nlakes
      j = 0
      do ipos = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
        j = j + 1
        nn = this%cellid(ipos)
        top = this%dis%top(nn)
        bot = this%dis%bot(nn)
        ! vertical connection
        if (this%ictype(ipos) == 0) then
          this%telev(ipos) = top + this%surfdep
          this%belev(ipos) = top
          this%lakebot(n) = min(this%belev(ipos), this%lakebot(n))
        ! horizontal connection
        else if (this%ictype(ipos) == 1) then
          if (this%belev(ipos) == this%telev(ipos)) then
            this%telev(ipos) = top
            this%belev(ipos) = bot
          else
            if (this%belev(ipos) >= this%telev(ipos)) then
              write(errmsg,'(4x,a,1x,i4,1x,a,1x,i4,1x,a)') &
                '****ERROR. telev FOR LAKE ', n, ' HORIZONTAL CONNECTION ', j, &
                'MUST BE >= belev'
              call store_error(errmsg)
            else if (this%belev(ipos) < bot) then
              write(errmsg,'(4x,a,1x,i4,1x,a,1x,i4,1x,a,1x,g15.7,1x,a)') &
                '****ERROR. belev FOR LAKE ', n, ' HORIZONTAL CONNECTION ', j, &
                'MUST BE >= cell bottom (', bot, ')'
              call store_error(errmsg)
            else if (this%telev(ipos) > top) then
              write(errmsg,'(4x,a,1x,i4,1x,a,1x,i4,1x,a,1x,g15.7,1x,a)') &
                '****ERROR. telev FOR LAKE ', n, ' HORIZONTAL CONNECTION ', j, &
                'MUST BE <= cell top (', top, ')'
              call store_error(errmsg)
            end if
          end if
          this%laketop(n) = max(this%telev(ipos), this%laketop(n))
          this%lakebot(n) = min(this%belev(ipos), this%lakebot(n))
        ! embedded connections
        else if (this%ictype(ipos) == 2 .or. this%ictype(ipos) == 3) then
          this%telev(ipos) = top
          this%belev(ipos) = bot
          this%lakebot(n) = bot
        end if
        !
        ! -- check for missing or duplicate lake connections
        if (nboundchk(ipos) == 0) then
          write(errmsg,'(a,1x,i0,1x,a,1x,i0)')                                  &
            'ERROR.  NO DATA SPECIFIED FOR LAKE', n, 'CONNECTION', j
          call store_error(errmsg)
        else if (nboundchk(ipos) > 1) then
          write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a,1x,i0,1x,a)')                  &
            'ERROR.  DATA FOR LAKE', n, 'CONNECTION', j,                        &
            'SPECIFIED', nboundchk(ipos), 'TIMES'
          call store_error(errmsg)
        end if
        !
        ! -- set laketop if it has not been assigned
      end do
      if (this%laketop(n) == -DEP20) then
        this%laketop(n) = this%lakebot(n) + 100.
      end if
    end do
    !
    ! -- deallocate local variable
    deallocate(nboundchk)
    !
    ! -- write summary of lake_connection error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine lak_read_lake_connections

  subroutine lak_read_tables(this)
! ******************************************************************************
! lak_read_tables -- Read the lake tables for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(LakType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    integer(I4B) :: n
    integer(I4B) :: ntabs
    integer(I4B), dimension(:), pointer :: nboundchk
! ------------------------------------------------------------------------------

    ! -- format
    !
    ! -- code
    !
    ! -- skip of no outlets
    if (this%ntables < 1) return
    !
    ! -- allocate and initialize nboundchk
    allocate(nboundchk(this%nlakes))
    do n = 1, this%nlakes
      nboundchk(n) = 0
    end do
    !
    ! -- allocate derived type for table data
    allocate(this%laketables(this%nlakes))
    !
    ! -- get lake_tables block
    call this%parser%GetBlock('TABLES', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse lake_tables block if detected
    if (isfound) then
      ntabs = 0
      ! -- process the lake connection data
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' LAKE_TABLES'
      readtable: do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        n = this%parser%GetInteger()

        if (n < 1 .or. n > this%nlakes) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. lakeno MUST BE > 0 and <= ', this%nlakes
          call store_error(errmsg)
          cycle readtable
        end if
        
        ! -- increment ntab and nboundchk
        ntabs = ntabs + 1
        nboundchk(n) = nboundchk(n) + 1

        ! -- read FILE keyword
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case('TAB6')
            call this%parser%GetStringCaps(keyword)
            if(trim(adjustl(keyword)) /= 'FILEIN') then
              errmsg = 'TAB6 keyword must be followed by "FILEIN" ' //      &
                        'then by filename.'
              call store_error(errmsg)
              cycle readtable
            end if
            call this%parser%GetString(line)
            call this%lak_read_table(n, line)
          case default
            write(errmsg,'(4x,a,1x,i4,1x,a)') &
              '****ERROR. LAKE TABLE ENTRY for LAKE ', n, 'MUST INCLUDE TAB6 KEYWORD'
            call store_error(errmsg)
            cycle readtable
        end select
      end do readtable
      
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' LAKE_TABLES'
      !
      ! -- check for missing or duplicate lake connections
      if (ntabs < this%ntables) then
        write(errmsg,'(a,1x,i0,1x,a,1x,i0)')                                    &
          'ERROR.  TABLE DATA ARE SPECIFIED', ntabs,                            &
          'TIMES BUT NTABLES IS SET TO', this%ntables
        call store_error(errmsg)
      end if
      do n = 1, this%nlakes
        if (this%ntabrow(n) > 0 .and. nboundchk(n) > 1) then
          write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                             &
            'ERROR.  TABLE DATA FOR LAKE', n, 'SPECIFIED', nboundchk(n), 'TIMES'
          call store_error(errmsg)
        end if
      end do
    else
      call store_error('ERROR.  REQUIRED TABLES BLOCK NOT FOUND.')
    end if
    !
    ! -- deallocate local storage
    deallocate(nboundchk)
    !
    ! -- write summary of lake_table error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if

    !
    ! -- return
    return
  end subroutine lak_read_tables

  subroutine lak_read_table(this, ilak, filename)
! ******************************************************************************
! lak_read_table -- Read the lake table for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use InputOutputModule, only: openfile
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(LakType), intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    character (len=*), intent(in) :: filename

    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: keyword
    character(len=13) :: arrName
    character(len=4) :: citem
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    integer(I4B) :: iu
    integer(I4B) :: n
    integer(I4B) :: ipos, i
    integer(I4B) :: j
    integer(I4B) :: jmin
    integer(I4B) :: iconn
    real(DP) :: vol
    real(DP) :: sa
    real(DP) :: wa
    real(DP) :: v
    type(BlockParserType) :: parser
! ------------------------------------------------------------------------------

    ! -- format
    !
    ! -- code
    !
    ! -- initialize locals
    n = 0
    j = 0
    !
    ! -- open the table file
    iu = 0
    call openfile(iu, this%iout, filename, 'LAKE TABLE')
    call parser%Initialize(iu, this%iout)
    !
    ! -- get dimensions block
    call parser%GetBlock('DIMENSIONS', isfound, ierr, supportOpenClose=.true.)
    !
    ! -- parse well_connections block if detected
    if (isfound) then
      ! -- process the lake connection data
      if (this%iprpak /= 0) then
        write(this%iout,'(/1x,a)')                                              &
          'PROCESSING '//trim(adjustl(this%text))//' DIMENSIONS'
      end if
      readdims: do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NROW')
            n = parser%GetInteger()

            if (n < 1) then
              write(errmsg,'(4x,a)') &
                '****ERROR. LAKE TABLE NROW MUST BE > 0'
              call store_error(errmsg)
            end if
          case ('NCOL')
            j = parser%GetInteger()

            if (this%ictype(ilak) == 2 .or. this%ictype(ilak) == 3) then
              jmin = 4
            else
              jmin = 3
            end if
            if (j < jmin) then
              write(errmsg,'(4x,a,1x,i0)') &
                '****ERROR. LAKE TABLE NCOL MUST BE >= ', jmin
              call store_error(errmsg)
            end if

          case default
            write(errmsg,'(4x,a,a)') &
              '****ERROR. UNKNOWN '//trim(this%text)//' DIMENSIONS KEYWORD: ', &
                                     trim(keyword)
            call store_error(errmsg)
        end select
      end do readdims
      if (this%iprpak /= 0) then
        write(this%iout,'(1x,a)')                                               &
          'END OF '//trim(adjustl(this%text))//' DIMENSIONS'
      end if
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
    end if
    !
    ! -- check that ncol and nrow have been specified
    if (n < 1) then
      write(errmsg,'(4x,a)') &
        '****ERROR. NROW NOT SPECIFIED IN THE LAKE TABLE DIMENSIONS BLOCK'
      call store_error(errmsg)
    end if
    if (j < 1) then
      write(errmsg,'(4x,a)') &
        '****ERROR. NCOL NOT SPECIFIED IN THE LAKE TABLE DIMENSIONS BLOCK'
      call store_error(errmsg)
    end if
    !
    ! -- only read the lake table data if n and j are specified to be greater
    !    than zero
    if (n * j > 0) then
      !
      ! -- allocate space
      this%ntabrow(ilak) = n
      write (citem,'(i4.4)') ilak
      ! -- build arrName for outlet
      arrName = 'TABSTAGE' // citem
      call mem_allocate(this%laketables(ilak)%tabstage, n, arrName, this%origin)
      arrName = 'TABVOLUME' // citem
      call mem_allocate(this%laketables(ilak)%tabvolume, n, arrName, this%origin)
      arrName = 'TABSAREA' // citem
      call mem_allocate(this%laketables(ilak)%tabsarea, n, arrName, this%origin)
      ipos = this%idxlakeconn(ilak)
      if (this%ictype(ipos) == 2 .or. this%ictype(ipos) == 3) then
        arrName = 'tabwarea' // citem
        call mem_allocate(this%laketables(ilak)%tabwarea, n, arrName, this%origin)
      end if


      ! -- get table block
      call parser%GetBlock('TABLE', isfound, ierr, supportOpenClose=.true.)
      !
      ! -- parse well_connections block if detected
      if (isfound) then

        ! -- process the table data
        if (this%iprpak /= 0) then
          write(this%iout,'(/1x,a)')                                            &
            'PROCESSING '//trim(adjustl(this%text))//' TABLE'
        end if
        iconn = this%idxlakeconn(ilak)
        ipos = 0
        readtabledata: do
          call parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
          ipos = ipos + 1
          if (ipos > this%ntabrow(ilak)) then
            cycle readtabledata
          end if
          this%laketables(ilak)%tabstage(ipos) = parser%GetDouble()
          this%laketables(ilak)%tabvolume(ipos) = parser%GetDouble()
          this%laketables(ilak)%tabsarea(ipos) = parser%GetDouble()
          if (this%ictype(iconn) == 2 .or. this%ictype(iconn) == 3) then
            this%laketables(ilak)%tabwarea(ipos) = parser%GetDouble()
          end if
        end do readtabledata
        
        if (this%iprpak /= 0) then
          write(this%iout,'(1x,a)')                                             &
            'END OF '//trim(adjustl(this%text))//' TABLE'
        end if
      else
        call store_error('ERROR.  REQUIRED TABLE BLOCK NOT FOUND.')
      end if
      !
      ! -- error condition if number of rows read are not equal to nrow
      if (ipos /= this%ntabrow(ilak)) then
        write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a)') &
          'ERROR. NROW SET TO',this%ntabrow(ilak), 'BUT', ipos, 'ROWS WERE READ'
        call store_error(errmsg)
      end if
      !
      ! -- set lake bottom based on table if it is an embedded lake
      iconn = this%idxlakeconn(ilak)
      if (this%ictype(iconn) == 2 .or. this%ictype(iconn) == 3) then
        do n = 1, this%ntabrow(ilak)
          vol = this%laketables(ilak)%tabvolume(n)
          sa = this%laketables(ilak)%tabsarea(n)
          wa = this%laketables(ilak)%tabwarea(n)
          v = vol * sa * wa
          ! -- check if all entries are zero
          if (v > DZERO) exit
          ! -- set lake bottom
          this%lakebot(ilak) = this%laketables(ilak)%tabstage(n)
          this%belev(ilak) = this%laketables(ilak)%tabstage(n)
        end do
        ! -- set maximum surface area for rainfall
        n = this%ntabrow(ilak)
        this%sareamax(ilak) = this%laketables(ilak)%tabsarea(n)
      end if
      !
      ! -- verify the table data
      do n = 2, this%ntabrow(ilak)
        if (this%laketables(ilak)%tabstage(n) <= this%laketables(ilak)%tabstage(n-1)) then
          write(errmsg,'(4x,a,1x,i4,1x,a,1x,g15.6,1x,a,1x,i6,1x,a,1x,i4,1x,a,1x,g15.6,1x,a)') &
            '****ERROR. TABLE STAGE ENTRY', n, '(', this%laketables(ilak)%tabstage(n), &
            ') FOR LAKE ', ilak, 'MUST BE GREATER THAN THE PREVIOUS STAGE ENTRY', &
            n-1, '(', this%laketables(ilak)%tabstage(n-1), ')'
          call store_error(errmsg)
        end if
        if (this%laketables(ilak)%tabvolume(n) <= this%laketables(ilak)%tabvolume(n-1)) then
          write(errmsg,'(4x,a,1x,i4,1x,a,1x,g15.6,1x,a,1x,i6,1x,a,1x,i4,1x,a,1x,g15.6,1x,a)') &
            '****ERROR. TABLE VOLUME ENTRY', n, '(', this%laketables(ilak)%tabvolume(n), &
            ') FOR LAKE ', ilak, 'MUST BE GREATER THAN THE PREVIOUS VOLUME ENTRY', &
            n-1, '(', this%laketables(ilak)%tabvolume(n-1), ')'
          call store_error(errmsg)
        end if
        if (this%laketables(ilak)%tabsarea(n) < this%laketables(ilak)%tabsarea(n-1)) then
          write(errmsg,'(4x,a,1x,i4,1x,a,1x,g15.6,1x,a,1x,i6,1x,a,1x,i4,1x,a,1x,g15.6,1x,a)') &
            '****ERROR. TABLE SURFACE AREA ENTRY', n, '(', this%laketables(ilak)%tabsarea(n), &
            ') FOR LAKE ', ilak, 'MUST BE GREATER THAN OR EQUAL TO THE PREVIOUS SURFACE AREA ENTRY', &
            n-1, '(', this%laketables(ilak)%tabsarea(n-1), ')'
          call store_error(errmsg)
        end if
        iconn = this%idxlakeconn(ilak)
        if (this%ictype(iconn) == 2 .or. this%ictype(iconn) == 3) then
          if (this%laketables(ilak)%tabwarea(n) < this%laketables(ilak)%tabwarea(n-1)) then
            write(errmsg,'(4x,a,1x,i4,1x,a,1x,g15.6,1x,a,1x,i6,1x,a,1x,i4,1x,a,1x,g15.6,1x,a)') &
              '****ERROR. TABLE EXCHANGE AREA ENTRY', n, '(', this%laketables(ilak)%tabwarea(n), &
              ') FOR LAKE ', ilak, 'MUST BE GREATER THAN OR EQUAL TO THE PREVIOUS EXCHANGE AREA ENTRY', &
              n-1, '(', this%laketables(ilak)%tabwarea(n-1), ')'
            call store_error(errmsg)
          end if
        end if
      end do
    end if
    !
    ! -- write summary of lake table error messages
    if (count_errors() > 0) then
      call parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! Close the table file and clear other parser members
    call parser%Clear()
    !
    ! -- return
    return
  end subroutine lak_read_table

  subroutine lak_read_outlets(this)
! ******************************************************************************
! lak_read_outlets -- Read the lake outlets for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    ! -- dummy
    class(LakType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: text, keyword
    character(len=LENBOUNDNAME) :: bndName
    character(len=9) :: citem
    integer(I4B) :: ierr, ival
    logical :: isfound, endOfBlock
    integer(I4B) :: n
    !integer(I4B) :: ii, jj, kk, nn
    integer(I4B) :: jj
    real(DP) :: endtim
    integer(I4B), dimension(:), pointer :: nboundchk
! ------------------------------------------------------------------------------

    ! -- format
    !
    ! -- code
    !
    ! -- skip if no outlets
    if (this%noutlets < 1) return
    !
    ! -- allocate and initialize local variables
    allocate(nboundchk(this%noutlets))
    do n = 1, this%noutlets
      nboundchk(n) = 0
    end do
    !
    ! -- get well_connections block
    call this%parser%GetBlock('OUTLETS', isfound, ierr, supportOpenClose=.true.)
    !
    ! -- parse outlets block if detected
    if (isfound) then

      ! -- allocate outlet data using memory manager
      call mem_allocate(this%lakein, this%NOUTLETS, 'LAKEIN', this%origin)
      call mem_allocate(this%lakeout, this%NOUTLETS, 'LAKEOUT', this%origin)
      call mem_allocate(this%iouttype, this%NOUTLETS, 'IOUTTYPE', this%origin)
      call mem_allocate(this%outrate, this%NOUTLETS, 'OUTRATE', this%origin)
      call mem_allocate(this%outinvert, this%NOUTLETS, 'OUTINVERT', this%origin)
      call mem_allocate(this%outwidth, this%NOUTLETS, 'OUTWIDTH', this%origin)
      call mem_allocate(this%outrough, this%NOUTLETS, 'OUTROUGH', this%origin)
      call mem_allocate(this%outslope, this%NOUTLETS, 'OUTSLOPE', this%origin)
      call mem_allocate(this%simoutrate, this%NOUTLETS, 'SIMOUTRATE', this%origin)

      ! -- process the lake connection data
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' OUTLETS'
      readoutlet: do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        n = this%parser%GetInteger()

        if (n < 1 .or. n > this%noutlets) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. outletno MUST BE > 0 and <= ', this%noutlets
          call store_error(errmsg)
          cycle readoutlet
        end if
        !
        ! -- increment nboundchk
        nboundchk(n) = nboundchk(n) + 1
        !
        ! -- read outlet lakein
        ival = this%parser%GetInteger()
        if (ival <1 .or. ival > this%noutlets) then
          write(errmsg,'(4x,a,1x,i4,1x,a,1x,i6)') &
            '****ERROR. lakein FOR OUTLET ', n, 'MUST BE > 0 and <= ', this%noutlets
          call store_error(errmsg)
          cycle readoutlet
        end if
        this%lakein(n) = ival

        ! -- read outlet lakeout
        ival = this%parser%GetInteger()
        if (ival <0 .or. ival > this%nlakes) then
          write(errmsg,'(4x,a,1x,i4,1x,a,1x,i6)') &
            '****ERROR. lakeout FOR OUTLET ', n, 'MUST BE >= 0 and <= ', this%noutlets
          call store_error(errmsg)
          cycle readoutlet
        end if
        this%lakeout(n) = ival

        ! -- read ictype
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('SPECIFIED')
            this%iouttype(n) = 0
          case ('MANNING')
            this%iouttype(n) = 1
          case ('WEIR')
            this%iouttype(n) = 2
          case default
            write(errmsg,'(4x,a,1x,i4,1x,a,a,a)') &
              '****ERROR. UNKNOWN couttype FOR OUTLET ', n, &
              '(', trim(keyword), ')'
            call store_error(errmsg)
            cycle readoutlet
          end select

        ! -- build bndname for outlet
        write (citem,'(i9.9)') n
        bndName = 'OUTLET' // citem

        ! -- set a few variables for timeseries aware variables
        endtim = DZERO
        jj = 1

        ! -- outlet invert
        call this%parser%GetString(text)
        call read_single_value_or_time_series(text, &
                                              this%outinvert(n)%value, &
                                              this%outinvert(n)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, 'INVERT', &
                                              bndName, this%parser%iuactive)

        ! -- outlet width
        call this%parser%GetString(text)
        call read_single_value_or_time_series(text, &
                                              this%outwidth(n)%value, &
                                              this%outwidth(n)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, 'WIDTH', &
                                              bndName, this%parser%iuactive)

        ! -- outlet roughness
        call this%parser%GetString(text)
        call read_single_value_or_time_series(text, &
                                              this%outrough(n)%value, &
                                              this%outrough(n)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, 'ROUGH', &
                                              bndName, this%parser%iuactive)

        ! -- outlet slope
        call this%parser%GetString(text)
        call read_single_value_or_time_series(text, &
                                              this%outslope(n)%value, &
                                              this%outslope(n)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, 'SLOPE', &
                                              bndName, this%parser%iuactive)


      end do readoutlet
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' OUTLETS'
      
      !
      ! -- check for duplicate or missing outlets
      do n = 1, this%noutlets
        if (nboundchk(n) == 0) then
          write(errmsg,'(a,1x,i0)')  'ERROR.  NO DATA SPECIFIED FOR OUTLET', n
          call store_error(errmsg)
        else if (nboundchk(n) > 1) then
          write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                             &
            'ERROR.  DATA FOR OUTLET', n, 'SPECIFIED', nboundchk(n), 'TIMES'
          call store_error(errmsg)
        end if
      end do
      
    else
      call store_error('ERROR.  REQUIRED OUTLETS BLOCK NOT FOUND.')
    end if
    !
    ! -- write summary of lake_connection error messages
    ierr = count_errors()
    if (ierr > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- deallocate local storage
    deallocate(nboundchk)
    !
    ! -- return
    return
  end subroutine lak_read_outlets

  subroutine lak_read_dimensions(this)
! ******************************************************************************
! pak1read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(LakType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- initialize dimensions to -1
    this%nlakes= -1
    this%maxbound = -1
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
          case ('NLAKES')
            this%nlakes = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)')'NLAKES = ', this%nlakes
          case ('NOUTLETS')
            this%noutlets = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)')'NOUTLETS = ', this%noutlets
          case ('NTABLES')
            this%ntables = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)')'NTABLES = ', this%ntables
          case default
            write(errmsg,'(4x,a,a)') &
              '****ERROR. UNKNOWN '//trim(this%text)//' DIMENSION: ', &
                                     trim(keyword)
            call store_error(errmsg)
        end select
      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' DIMENSIONS'
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
    end if

    if (this%nlakes < 0) then
      write(errmsg, '(1x,a)') &
        'ERROR:  NLAKES WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.'
      call store_error(errmsg)
    end if
    !
    ! -- stop if errors were encountered in the DIMENSIONS block
    ierr = count_errors()
    if (ierr > 0) then
      call ustop()
    end if
    !
    ! -- read lakes block
    call this%lak_read_lakes()
    !
    ! -- read lake_connections block
    call this%lak_read_lake_connections()
    !
    ! -- read tables block
    call this%lak_read_tables()
    !
    ! -- read outlets block
    call this%lak_read_outlets()
    !
    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- return
    return
  end subroutine lak_read_dimensions


  subroutine lak_read_initial_attr(this)
! ******************************************************************************
! pak1read_dimensions -- Read the initial parameters for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors
    use BudgetModule, only: budget_cr
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    ! -- dummy
    class(LakType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: text
    integer(I4B) :: j, jj, n
    integer(I4B) :: nn
    integer(I4B) :: idx
    integer(I4B) :: ival
    real(DP) :: endtim
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: k
    real(DP) :: area
    real(DP) :: length
    real(DP) :: s
    real(DP) :: dx
    real(DP) :: c
    real(DP) :: sa
    real(DP) :: wa
    real(DP) :: v
    real(DP) :: fact
    real(DP) :: c1
    real(DP) :: c2
    real(DP), allocatable, dimension(:) :: clb, caq
    character (len=10), dimension(0:3) :: ctype
    character (len=15) :: nodestr
    !data
    data ctype(0) /'VERTICAL  '/
    data ctype(1) /'HORIZONTAL'/
    data ctype(2) /'EMBEDDEDH '/
    data ctype(3) /'EMBEDDEDV '/
    ! -- format
! ------------------------------------------------------------------------------

    ! -- setup the lake budget
    call budget_cr(this%budget, this%origin)
    ival = this%bditems
    call this%budget%budget_df(ival, this%name, 'L**3')
    !
    ! -- initialize xnewpak and set stage
    do n = 1, this%nlakes
      this%xnewpak(n) = this%strt(n)
      write(text,'(g15.7)') this%strt(n)
      endtim = DZERO
      jj = 1    ! For STAGE
      call read_single_value_or_time_series(text, &
                                            this%stage(n)%value, &
                                            this%stage(n)%name, &
                                            endtim,  &
                                            this%name, 'BND', this%TsManager, &
                                            this%iprpak, n, jj, 'STAGE', &
                                            this%lakename(n), this%inunit)

    end do
    !
    ! -- initialize status (iboundpak) of lakes to active
    do n = 1, this%nlakes
      if (this%status(n) == 'CONSTANT') then
        this%iboundpak(n) = -1
      else if (this%status(n) == 'INACTIVE') then
        this%iboundpak(n) = 0
      else if (this%status(n) == 'ACTIVE ') then
        this%iboundpak(n) = 1
      end if
    end do
    !
    ! -- set boundname for each connection
    if (this%inamedbound /= 0) then
      do n = 1, this%nlakes
        do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
          this%boundname(j) = this%lakename(n)
        end do
      end do
    endif
    !
    ! -- set pointer to gwf iss and gwf hk
    call mem_setptr(this%gwfiss, 'ISS', trim(this%name_model))
    call mem_setptr(this%gwfk11, 'K11', trim(this%name_model)//' NPF')
    call mem_setptr(this%gwfk33, 'K33', trim(this%name_model)//' NPF')
    call mem_setptr(this%gwfik33, 'IK33', trim(this%name_model)//' NPF')
    call mem_setptr(this%gwfsat, 'SAT', trim(this%name_model)//' NPF')
    !
    ! -- allocate temporary storage
    allocate(clb(this%MAXBOUND))
    allocate(caq(this%MAXBOUND))

    ! -- calculate saturated conductance for each connection
    do n = 1, this%nlakes
      do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
        nn = this%cellid(j)
        top = this%dis%top(nn)
        bot = this%dis%bot(nn)
        ! vertical connection
        if (this%ictype(j) == 0) then
          area = this%dis%area(nn)
          this%sarea(j) = area
          this%warea(j) = area
          this%sareamax(n) = this%sareamax(n) + area
          if (this%gwfik33 == 0) then
            k = this%gwfk11(nn)
          else
            k = this%gwfk33(nn)
          endif
          length = DHALF * (top - bot)
        ! horizontal connection
        else if (this%ictype(j) == 1) then
          area = (this%telev(j) - this%belev(j)) * this%connwidth(j)
          ! -- recalculate area if connected cell is confined and lake
          !    connection top and bot are equal to the cell top and bot
          if (top == this%telev(j) .and. bot == this%belev(j)) then
            if (this%icelltype(nn) == 0) then
              area = this%gwfsat(nn) * (top - bot) * this%connwidth(j)
            end if
          end if
          this%sarea(j) = DZERO
          this%warea(j) = area
          this%sareamax(n) = this%sareamax(n) + DZERO
          k = this%gwfk11(nn)
          length = this%connlength(j)
        ! embedded horizontal connection
        else if (this%ictype(j) == 2) then
          area = DONE
          this%sarea(j) = DZERO
          this%warea(j) = area
          this%sareamax(n) = this%sareamax(n) + DZERO
          k = this%gwfk11(nn)
          length = this%connlength(j)
        ! embedded vertical connection
        else if (this%ictype(j) == 3) then
          area = DONE
          this%sarea(j) = DZERO
          this%warea(j) = area
          this%sareamax(n) = this%sareamax(n) + DZERO
          if (this%gwfik33 == 0) then
            k = this%gwfk11(nn)
          else
            k = this%gwfk33(nn)
          endif
          length = this%connlength(j)
        end if
        if (this%bedleak(j) > DZERO) then
          clb(j) = done / this%bedleak(j)
        else
          clb(j) = DZERO
        end if
        if (k > DZERO) then
          caq(j) = length / k
        else
          caq(j) = DZERO
        end if
        if (clb(j)*caq(j) > DZERO) then
          this%satcond(j) = area / (clb(j) + caq(j))
        else
          this%satcond(j) = DZERO
        end if
      end do
    end do
    !
    ! -- write a summary of the conductance
    if (this%iprpak > 0) then
      write(this%iout,'(//,29x,a,/)') 'INTERFACE CONDUCTANCE BETWEEN LAKE AND AQUIFER CELLS'
      write(this%iout,'(1x,a)') &
     &  '      LAKE CONNECTION                 CONNECTION    LAKEBED' // &
     &  '              C O N D U C T A N C E S        '
      write(this%iout,'(1x,a)') &
     &  '    NUMBER     NUMBER CELLID          DIRECTION    LEAKANCE' // &
     &  '        LAKEBED        AQUIFER       COMBINED'
      write(this%iout,"(1x,104('-'))")
      do n = 1, this%nlakes
        idx = 0
        do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
          idx = idx + 1
          fact = DONE
          if (this%ictype(j) == 1) then
            fact = this%telev(j) - this%belev(j)
            if (ABS(fact) > DZERO) then
              fact = DONE / fact
            end if
          end if
          nn = this%cellid(j)
          area = this%warea(j)
          c1 = DZERO
          if (clb(j) > DZERO) then
            c1 = area * fact / clb(j)
          end if
          c2 = DZERO
          if (caq(j) > DZERO) then
            c2 = area * fact / caq(j)
          end if
          call this%dis%noder_to_string(nn, nodestr)
          write(this%iout,'(1x,i10,1x,i10,1x,a15,1x,a10,4(1x,g14.5))') &
    &        n, idx, nodestr, ctype(this%ictype(j)), this%bedleak(j), &
    &        c1, c2, this%satcond(j) * fact
        end do
      end do
      write(this%iout,"(1x,104('-'))")
      write(this%iout,'(1x,a)') 'IF VERTICAL CONNECTION, CONDUCTANCE (L^2/T) IS BETWEEN AQUIFER CELL AND OVERLYING LAKE CELL.'
      write(this%iout,'(1x,a)')   'IF HORIZONTAL CONNECTION, CONDUCTANCES ARE PER UNIT SATURATED THICKNESS (L/T).'
      write(this%iout,'(1x,a)')   'IF EMBEDDED CONNECTION, CONDUCTANCES ARE PER UNIT EXCHANGE AREA (1/T).'

      !        write(this%iout,*) n, idx, nodestr, this%sarea(j), this%warea(j)
      !
      ! -- calculate stage, surface area, wetted area, volume relation
      do n = 1, this%nlakes
        write (this%iout,'(//1x,a,1x,i10)') 'STAGE/VOLUME RELATION FOR LAKE  ', n
        write (this%iout,'(/1x,5(a14))') '         STAGE', '  SURFACE AREA', &
    &                                    '   WETTED AREA', '   CONDUCTANCE', &
    &                                    '        VOLUME'
        write (this%iout,"(1x,70('-'))")
        dx = (this%laketop(n) - this%lakebot(n)) / 150.
        s = this%lakebot(n)
        do j = 1, 151
          call this%lak_calculate_conductance(n, s, c)
          call this%lak_calculate_sarea(n, s, sa)
          call this%lak_calculate_warea(n, s, wa, s)
          call this%lak_calculate_vol(n, s, v)
          write (this%iout,'(1x,5(E14.5))') s, sa, wa, c, v
          s = s + dx
        end do
        write (this%iout,"(1x,70('-'))")

        write (this%iout,'(//1x,a,1x,i10)') 'STAGE/VOLUME RELATION FOR LAKE  ', n
        write (this%iout,'(/1x,4(a14))') '              ', '              ', &
    &                                    '    CALCULATED', '         STAGE'
        write (this%iout,'(1x,4(a14))')  '         STAGE', '        VOLUME', &
    &                                    '         STAGE', '    DIFFERENCE'
        write (this%iout,"(1x,56('-'))")
        s = this%lakebot(n) - dx
        do j = 1, 156
          call this%lak_calculate_vol(n, s, v)
          call this%lak_vol2stage(n, v, c)
          write (this%iout,'(1x,4(E14.5))') s, v, c, s-c
          s = s + dx
        end do
        write (this%iout,"(1x,56('-'))")
      end do
    end if
    !
    ! -- finished with pointer to gwf hydraulic conductivity
    this%gwfk11 => null()
    this%gwfk33 => null()
    this%gwfsat => null()
    this%gwfik33 => null()
    !
    ! -- deallocate temporary storage
    deallocate(clb)
    deallocate(caq)
    !
    ! -- return
    return
  end subroutine lak_read_initial_attr

! -- simple subroutine for linear interpolation of two vectors
!       function assumes x data is sorted in ascending order
  subroutine lak_linear_interpolation(this, n, x, y, z, v)
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: n
    real(DP), dimension(n), intent(in) :: x
    real(DP), dimension(n), intent(in) :: y
    real(DP), intent(in) :: z
    real(DP), intent(inout) :: v
    ! -- local
    integer(I4B) :: i
    real(DP) :: dx, dydx
    ! code
    v = DZERO
    ! below bottom of range - set to lowest value
    if (z <= x(1)) then
      v = y(1)
    ! above highest value
    ! slope calculated from interval between n and n-1
    else if (z > x(n)) then
      dx   = x(n) - x(n-1)
      dydx = DZERO
      if (ABS(dx) > DZERO) then
        dydx = ( y(n) - y(n-1) ) / dx
      end if
      dx   = (z - x(n))
      v = y(n) + dydx * dx
    ! between lowest and highest value in current interval
    else
      do i = 2, n
        dx   = x(i) - x(i-1)
        dydx = DZERO
        if (z >= x(i-1) .and. z <= x(i)) then
          if (ABS(dx) > DZERO) then
            dydx = ( y(i) - y(i-1) ) / dx
          end if
          dx   = (z - x(i-1))
          v = y(i-1) + dydx * dx
          exit
        end if
      end do
    end if
    ! return
    return
  end subroutine lak_linear_interpolation

  subroutine lak_calculate_sarea(this, ilak, stage, sarea)
! ******************************************************************************
! lak_calculate_sarea -- Calculate the surface area of a lake at a given stage.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(in) :: stage
    real(DP), intent(inout) :: sarea
    ! -- local
    integer(I4B) :: i
    real(DP) :: topl
    real(DP) :: botl
    real(DP) :: sat
    real(DP) :: sa
    ! -- formats
! ------------------------------------------------------------------------------
    sarea = DZERO
    if (this%ntabrow(ilak) > 0) then
      i = this%ntabrow(ilak)
      if (stage <= this%laketables(ilak)%tabstage(1)) then
        sarea = this%laketables(ilak)%tabsarea(1)
      else if (stage >= this%laketables(ilak)%tabstage(i)) then
        sarea = this%laketables(ilak)%tabsarea(i)
      else
        call this%lak_linear_interpolation(i, this%laketables(ilak)%tabstage, &
                                           this%laketables(ilak)%tabsarea, &
                                           stage, sarea)
      end if
    else
      do i = this%idxlakeconn(ilak), this%idxlakeconn(ilak+1)-1
        topl = this%telev(i)
        botl = this%belev(i)
        sat = sQuadraticSaturation(topl, botl, stage)
        sa = sat * this%sarea(i)
        sarea = sarea + sa
      end do
    end if
    !
    ! -- return
    return
  end subroutine lak_calculate_sarea

  subroutine lak_calculate_warea(this, ilak, stage, warea, hin)
! ******************************************************************************
! lak_calculate_warea -- Calculate the wetted area of a lake at a given stage.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(in) :: stage
    real(DP), intent(inout) :: warea
    real(DP), optional, intent(inout) :: hin
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: igwfnode
    real(DP) :: head
    real(DP) :: wa
    ! -- formats
! ------------------------------------------------------------------------------
    warea = DZERO
    do i = this%idxlakeconn(ilak), this%idxlakeconn(ilak+1)-1
      if (present(hin)) then
        head = hin
      else
        igwfnode = this%cellid(i)
        head = this%xnew(igwfnode)
      end if
      call this%lak_calculate_conn_warea(ilak, i, stage, head, wa)
      warea = warea + wa
    end do
    !
    ! -- return
    return
  end subroutine lak_calculate_warea

  subroutine lak_calculate_conn_warea(this, ilak, iconn, stage, head, wa)
! ******************************************************************************
! lak_calculate_conn_warea -- Calculate the wetted area of a lake connection
!                             at a given stage.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    integer(I4B), intent(in) :: iconn
    real(DP), intent(in) :: stage
    real(DP), intent(in) :: head
    real(DP), intent(inout) :: wa
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: node
    real(DP) :: topl
    real(DP) :: botl
    real(DP) :: vv
    real(DP) :: sat
    ! -- formats
! ------------------------------------------------------------------------------
    wa = DZERO
    topl = this%telev(iconn)
    botl = this%belev(iconn)
    call this%lak_calculate_cond_head(ilak, iconn, stage, head, vv)
    if (this%ictype(iconn) == 2 .or. this%ictype(iconn) == 3) then
      if (vv > topl) vv = topl
      i = this%ntabrow(ilak)
      if (vv <= this%laketables(ilak)%tabstage(1)) then
        wa = this%laketables(ilak)%tabwarea(1)
      else if (vv >= this%laketables(ilak)%tabstage(i)) then
        wa = this%laketables(ilak)%tabwarea(i)
      else
        call this%lak_linear_interpolation(i, this%laketables(ilak)%tabstage, &
                                           this%laketables(ilak)%tabwarea, &
                                           vv, wa)
      end if
    else
      node = this%cellid(iconn)
      ! -- confined cell
      if (this%icelltype(node) == 0) then
        sat = DONE
      ! -- convertible cell
      else
        sat = sQuadraticSaturation(topl, botl, vv)
      end if
      wa = sat * this%warea(iconn)
    end if
    !
    ! -- return
    return
  end subroutine lak_calculate_conn_warea


  subroutine lak_calculate_vol(this, ilak, stage, volume)
! ******************************************************************************
! lak_calculate_vol -- Calculate the volume of a lake at a given stage.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(in) :: stage
    real(DP), intent(inout) :: volume
    ! -- local
    integer(I4B) :: i
    real(DP) :: topl
    real(DP) :: botl
    real(DP) :: ds
    real(DP) :: sa
    real(DP) :: v
    real(DP) :: sat
    ! -- formats
! ------------------------------------------------------------------------------
    volume = DZERO
    if (this%ntabrow(ilak) > 0) then
      i = this%ntabrow(ilak)
      if (stage <= this%laketables(ilak)%tabstage(1)) then
        volume = this%laketables(ilak)%tabvolume(1)
      else if (stage >= this%laketables(ilak)%tabstage(i)) then
        ds = stage - this%laketables(ilak)%tabstage(i)
        sa = this%laketables(ilak)%tabsarea(i)
        volume = this%laketables(ilak)%tabvolume(i) + ds * sa
      else
        call this%lak_linear_interpolation(i, this%laketables(ilak)%tabstage, &
                                           this%laketables(ilak)%tabvolume, &
                                           stage, volume)
      end if
    else
      do i = this%idxlakeconn(ilak), this%idxlakeconn(ilak+1)-1
        topl = this%telev(i)
        botl = this%belev(i)
        sat = sQuadraticSaturation(topl, botl, stage)
        sa = sat * this%sarea(i)
        if (stage < botl) then
          v = DZERO
        else if (stage > botl .and. stage < topl) then
          v = sa * (stage - botl)
        else
          v = sa * (topl - botl) + sa * (stage - topl)
        end if
        volume = volume + v
      end do
    end if
    !
    ! -- return
    return
  end subroutine lak_calculate_vol


  subroutine lak_calculate_conductance(this, ilak, stage, conductance)
! ******************************************************************************
! lak_calculate_conductance -- Calculate the total conductance for a lake at a
!                              provided stage.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(in) :: stage
    real(DP), intent(inout) :: conductance
    ! -- local
    integer(I4B) :: i
    real(DP) :: c
    ! -- formats
! ------------------------------------------------------------------------------
    conductance = DZERO
    do i = this%idxlakeconn(ilak), this%idxlakeconn(ilak+1)-1
      call this%lak_calculate_conn_conductance(ilak, i, stage, stage, c)
      conductance = conductance + c
    end do
    !
    ! -- return
    return
  end subroutine lak_calculate_conductance

  subroutine lak_calculate_cond_head(this, ilak, iconn, stage, head, vv)
! ******************************************************************************
! lak_calculate_conn_head -- Calculate the controlling lake stage or groundwater
!                            head used to calculate the conductance for a lake
!                            connection from a provided stage and groundwater
!                            head.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    integer(I4B), intent(in) :: iconn
    real(DP), intent(in) :: stage
    real(DP), intent(in) :: head
    real(DP), intent(inout) :: vv
    ! -- local
    real(DP) :: ss
    real(DP) :: hh
    real(DP) :: topl
    real(DP) :: botl
    ! -- formats
! ------------------------------------------------------------------------------
    topl = this%telev(iconn)
    botl = this%belev(iconn)
    ss = min(stage, topl)
    hh = min(head, topl)
    if (this%igwhcopt > 0) then
      vv = hh
    else if (this%inewton > 0) then
      vv = max(ss, hh)
    else
      vv = DHALF * (ss + hh)
    end if
    !
    ! -- return
    return
  end subroutine lak_calculate_cond_head


  subroutine lak_calculate_conn_conductance(this, ilak, iconn, stage, head, cond)
! ******************************************************************************
! lak_calculate_conn_conductance -- Calculate the conductance for a lake
!                                   connection at a provided stage
!                                   and groundwater head.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    integer(I4B), intent(in) :: iconn
    real(DP), intent(in) :: stage
    real(DP), intent(in) :: head
    real(DP), intent(inout) :: cond
    ! -- local
    integer(I4B) :: node
    !real(DP) :: ss
    !real(DP) :: hh
    real(DP) :: vv
    real(DP) :: topl
    real(DP) :: botl
    real(DP) :: sat
    real(DP) :: wa
    ! -- formats
! ------------------------------------------------------------------------------
    cond = DZERO
    topl = this%telev(iconn)
    botl = this%belev(iconn)
    call this%lak_calculate_cond_head(ilak, iconn, stage, head, vv)
    sat = sQuadraticSaturation(topl, botl, vv)
    ! vertical connection
    ! use full saturated conductance if top and bottom of the lake connection
    ! are equal
    if (this%ictype(iconn) == 0) then
      if (ABS(topl-botl) < DPREC) then
        sat = DONE
      end if
    ! horizontal connection
    ! use full saturated conductance if the connected cell is not convertible
    else if (this%ictype(iconn) == 1) then
      node = this%cellid(iconn)
      if (this%icelltype(node) == 0) then
        sat = DONE
      end if
    ! embedded connection
    else if (this%ictype(iconn) == 2 .or. this%ictype(iconn) == 3) then
      node = this%cellid(iconn)
      if (this%icelltype(node) == 0) then
        vv = this%telev(iconn)
        call this%lak_calculate_conn_warea(ilak, iconn, vv, vv, wa)
      else
        call this%lak_calculate_conn_warea(ilak, iconn, stage, head, wa)
      end if
      sat = wa
    end if
    cond = sat * this%satcond(iconn)
    !
    ! -- return
    return
  end subroutine lak_calculate_conn_conductance


  subroutine lak_calculate_conn_exchange(this, ilak, iconn, stage, head, flow, cond)
! ******************************************************************************
! lak_calculate_conn_exchange -- Calculate the groundwater-lake flow at a
!                                provided stage and groundwater head.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    integer(I4B), intent(in) :: iconn
    real(DP), intent(in) :: stage
    real(DP), intent(in) :: head
    real(DP), intent(inout) :: flow
    real(DP), intent(inout) :: cond
    ! -- local
    real(DP) :: botl
    real(DP) :: ss
    real(DP) :: hh
    ! -- formats
! ------------------------------------------------------------------------------
    flow = DZERO
    call this%lak_calculate_conn_conductance(ilak, iconn, stage, head, cond)
    botl = this%belev(iconn)
    ss = max(stage, botl)
    hh = max(head, botl)
    flow = cond * (hh - ss)
    !
    ! -- return
    return
  end subroutine lak_calculate_conn_exchange


  subroutine lak_estimate_conn_exchange(this, iflag, ilak, iconn, idry, stage, &
                                        head, flow, cond, source)
! ******************************************************************************
! lak_estimate_conn_exchange -- Calculate the groundwater-lake flow at a
!                               provided stage and groundwater head.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: iflag
    integer(I4B), intent(in) :: ilak
    integer(I4B), intent(in) :: iconn
    integer(I4B), intent(inout) :: idry
    real(DP), intent(in) :: stage
    real(DP), intent(in) :: head
    real(DP), intent(inout) :: flow
    real(DP), intent(inout) :: cond
    real(DP), intent(inout) :: source
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    flow = DZERO
    idry = 0
    call this%lak_calculate_conn_exchange(ilak, iconn, stage, head, flow, cond)
    if (iflag == 1) then
      if (flow > DZERO) then
        source = source + flow
      end if
    else if (iflag == 2) then
      if (-flow > source) then
        flow = -source
        source = DZERO
        idry = 1
      else if (flow < DZERO) then
        source = source + flow
      end if
    end if
    !
    ! -- return
    return
  end subroutine lak_estimate_conn_exchange

  subroutine lak_calculate_storagechange(this, ilak, stage, stage0, delt, dvr)
! ******************************************************************************
! lak_calculate_storagechange -- Calculate the inflow terms to a lake at a
!                         provided stage.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(in) :: stage
    real(DP), intent(in) :: stage0
    real(DP), intent(in) :: delt
    real(DP), intent(inout) :: dvr
    ! -- local
    real(DP) :: v
    real(DP) :: v0
    ! -- formats
! ------------------------------------------------------------------------------
    dvr = DZERO
    if (this%isteady /= 1) then
      call this%lak_calculate_vol(ilak, stage, v)
      call this%lak_calculate_vol(ilak, stage0, v0)
      dvr = (v0 - v) / delt
    end if
    !
    ! -- return
    return
  end subroutine lak_calculate_storagechange

  subroutine lak_calculate_rainfall(this, ilak, stage, ra)
! ******************************************************************************
! lak_calculate_rainfall -- Calculate the rainfall for a lake .
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(in) :: stage
    real(DP), intent(inout) :: ra
    ! -- local
    integer(I4B) :: iconn
    real(DP) :: sa
    ! -- formats
! ------------------------------------------------------------------------------
    ! -- rainfall
    iconn = this%idxlakeconn(ilak)
    if (this%ictype(iconn) == 2 .or. this%ictype(iconn) == 3) then
      sa = this%sareamax(ilak)
    else
      call this%lak_calculate_sarea(ilak, stage, sa)
    end if
    ra = this%rainfall(ilak)%value * sa !this%sareamax(ilak)
    !
    ! -- return
    return
  end subroutine lak_calculate_rainfall

  subroutine lak_calculate_runoff(this, ilak, ro)
! ******************************************************************************
! lak_calculate_runoff -- Calculate runoff to a lake.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(inout) :: ro
    ! -- formats
! ------------------------------------------------------------------------------
    ! -- runoff
    ro = this%runoff(ilak)%value
    !
    ! -- return
    return
  end subroutine lak_calculate_runoff

  subroutine lak_calculate_inflow(this, ilak, qin)
! ******************************************************************************
! lak_calculate_inflow -- Calculate specified inflow to a lake.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(inout) :: qin
    ! -- formats
! ------------------------------------------------------------------------------
    ! -- inflow to lake
    qin = this%inflow(ilak)%value
    !
    ! -- return
    return
  end subroutine lak_calculate_inflow

  subroutine lak_calculate_external(this, ilak, ex)
! ******************************************************************************
! lak_calculate_external -- Calculate the external flow terms to a lake.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(inout) :: ex
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- If mover is active, add receiver water to rhs and
    !    store available water (as positive value)
    ex = DZERO
    if (this%imover == 1) then
      ex = this%pakmvrobj%get_qfrommvr(ilak)
    end if
    !
    ! -- return
    return
  end subroutine lak_calculate_external

  subroutine lak_calculate_withdrawal(this, ilak, avail, wr)
! ******************************************************************************
! lak_calculate_withdrawal -- Calculate the withdrawal from a lake subject to
!                             an available volume.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(inout) :: avail
    real(DP), intent(inout) :: wr
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    ! -- withdrawals - limit to sum of inflows and available volume
    wr = this%withdrawal(ilak)%value
    if (wr > avail) then
      wr = -avail
    else
      if (wr > DZERO) then
        wr = -wr
      end if
    end if
    avail = avail + wr
    !
    ! -- return
    return
  end subroutine lak_calculate_withdrawal

  subroutine lak_calculate_evaporation(this, ilak, stage, avail, ev)
! ******************************************************************************
! lak_calculate_evaporation -- Calculate the evaporation from a lake at a
!                              provided stage subject to an available volume.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(in) :: stage
    real(DP), intent(inout) :: avail
    real(DP), intent(inout) :: ev
    ! -- local
    real(DP) :: sa
    ! -- formats
! ------------------------------------------------------------------------------
    ! -- evaporation - limit to sum of inflows and available volume
    call this%lak_calculate_sarea(ilak, stage, sa)
    ev = sa * this%evaporation(ilak)%value
    if (ev > avail) then
      ev = -avail
    else
      ev = -ev
    end if
    avail = avail + ev
    !
    ! -- return
    return
  end subroutine lak_calculate_evaporation

  subroutine lak_calculate_outlet_inflow(this, ilak, outinf)
! ******************************************************************************
! lak_calculate_outlet_inflow -- Calculate the outlet inflow to a lake.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(inout) :: outinf
    ! -- local
    integer(I4B) :: n
    ! -- formats
! ------------------------------------------------------------------------------
    !
    outinf = DZERO
    do n = 1, this%noutlets
      if (this%lakeout(n) == ilak) then
        outinf = outinf - this%simoutrate(n)
        if (this%imover == 1) then
          outinf = outinf - this%pakmvrobj%get_qtomvr(n)
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine lak_calculate_outlet_inflow

  subroutine lak_calculate_outlet_outflow(this, ilak, stage, avail, outoutf)
! ******************************************************************************
! lak_calculate_outlet_outflow -- Calculate the outlet outflow from a lake.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(in) :: stage
    real(DP), intent(inout) :: avail
    real(DP), intent(inout) :: outoutf
    ! -- local
    integer(I4B) :: n
    real(DP) :: g
    real(DP) :: d
    real(DP) :: c
    real(DP) :: gsm
    real(DP) :: rate
    ! -- formats
! ------------------------------------------------------------------------------
    !
    outoutf = DZERO
    do n = 1, this%noutlets
      if (this%lakein(n) == ilak) then
        rate = DZERO
        d = stage - this%outinvert(n)%value
        if (this%outdmax > DZERO) then
          if (d > this%outdmax) d = this%outdmax
        end if
        g = DGRAVITY * this%convlength * this%convtime * this%convtime
        select case (this%iouttype(n))
          ! specified rate
          case(0)
            rate = this%outrate(n)%value
            if (-rate > avail) then
              rate = -avail
            end if
          ! manning
          case (1)
            if (d > DZERO) then
              c = (this%convlength**DONETHIRD) * this%convtime
              gsm = DZERO
              if (this%outrough(n)%value > DZERO) then
                gsm = DONE / this%outrough(n)%value
              end if
              rate = -c * gsm * this%outwidth(n)%value * ( d**DFIVETHIRDS ) * sqrt(this%outslope(n)%value)
            end if
          ! weir
          case (2)
            if (d > DZERO) then
              rate = -DTWOTHIRDS * DCD * this%outwidth(n)%value * d * sqrt(DTWO * g * d)
            end if
        end select
        !if (-rate > avail) then
        !  rate = -avail
        !end if
        this%simoutrate(n) = rate
        avail = avail + rate
        outoutf = outoutf + rate
      end if
    end do
    !
    ! -- return
    return
  end subroutine lak_calculate_outlet_outflow

  subroutine lak_get_internal_inlet(this, ilak, outinf)
! ******************************************************************************
! lak_get_internal_inlet -- Get the outlet inflow to a lake from another lake.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(inout) :: outinf
    ! -- local
    integer(I4B) :: n
    ! -- formats
! ------------------------------------------------------------------------------
    outinf = DZERO
    do n = 1, this%noutlets
      if (this%lakeout(n) == ilak) then
        outinf = outinf - this%simoutrate(n)
        if (this%imover == 1) then
          outinf = outinf - this%pakmvrobj%get_qtomvr(n)
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine lak_get_internal_inlet

  subroutine lak_get_internal_outlet(this, ilak, outoutf)
! ******************************************************************************
! lak_get_internal_outlet -- Get the outlet from a lake to another lake.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(inout) :: outoutf
    ! -- local
    integer(I4B) :: n
    ! -- formats
! ------------------------------------------------------------------------------
    outoutf = DZERO
    do n = 1, this%noutlets
      if (this%lakein(n) == ilak) then
        if (this%lakeout(n) < 1) cycle
        outoutf = outoutf + this%simoutrate(n)
      end if
    end do
    !
    ! -- return
    return
  end subroutine lak_get_internal_outlet

  subroutine lak_get_external_outlet(this, ilak, outoutf)
! ******************************************************************************
! lak_get_external_outlet -- Get the outlet outflow from a lake to an external
!                            boundary.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(inout) :: outoutf
    ! -- local
    integer(I4B) :: n
    ! -- formats
! ------------------------------------------------------------------------------
    outoutf = DZERO
    do n = 1, this%noutlets
      if (this%lakein(n) == ilak) then
        if (this%lakeout(n) > 0) cycle
        outoutf = outoutf + this%simoutrate(n)
      end if
    end do
    !
    ! -- return
    return
  end subroutine lak_get_external_outlet

  subroutine lak_get_external_mover(this, ilak, outoutf)
! ******************************************************************************
! lak_get_external_mover -- Get the mover outflow from a lake to an external
!                           boundary.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(inout) :: outoutf
    ! -- local
    integer(I4B) :: n
    ! -- formats
! ------------------------------------------------------------------------------
    outoutf = DZERO
    if (this%imover == 1) then
      do n = 1, this%noutlets
        if (this%lakein(n) == ilak) then
          if (this%lakeout(n) > 0) cycle
          outoutf = outoutf + this%pakmvrobj%get_qtomvr(n)
        end if
      end do
    end if
    !
    ! -- return
    return
  end subroutine lak_get_external_mover

  subroutine lak_get_internal_mover(this, ilak, outoutf)
! ******************************************************************************
! lak_get_internal_mover -- Get the mover outflow from a lake to another lake.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(inout) :: outoutf
    ! -- local
    integer(I4B) :: n
    ! -- formats
! ------------------------------------------------------------------------------
    outoutf = DZERO
    if (this%imover == 1) then
      do n = 1, this%noutlets
        if (this%lakein(n) == ilak) then
          if (this%lakeout(n) < 1) cycle
          outoutf = outoutf + this%pakmvrobj%get_qtomvr(n)
        end if
      end do
    end if
    !
    ! -- return
    return
  end subroutine lak_get_internal_mover

  subroutine lak_get_outlet_tomover(this, ilak, outoutf)
! ******************************************************************************
! llak_get_outlet_tomover -- Get the outlet to mover from a lake.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(inout) :: outoutf
    ! -- local
    integer(I4B) :: n
    ! -- formats
! ------------------------------------------------------------------------------
    outoutf = DZERO
    if (this%imover == 1) then
      do n = 1, this%noutlets
        if (this%lakein(n) == ilak) then
          outoutf = outoutf + this%pakmvrobj%get_qtomvr(n)
        end if
      end do
    end if
    !
    ! -- return
    return
  end subroutine lak_get_outlet_tomover

  subroutine lak_vol2stage(this, ilak, vol, stage)
! ******************************************************************************
! lak_vol2stage-- Determine the stage from a provided volume.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(in) :: vol
    real(DP), intent(inout) :: stage
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: ibs
    real(DP) :: s0, s1, sm
    real(DP) :: v0, v1, vm
    real(DP) :: f0, f1, fm
    real(DP) :: sa
    real(DP) :: en0, en1
    real(DP) :: ds, ds0
    real(DP) :: denom
    ! -- formats
! ------------------------------------------------------------------------------
    s0 = this%lakebot(ilak)
    call this%lak_calculate_vol(ilak, s0, v0)
    s1 = this%laketop(ilak)
    call this%lak_calculate_vol(ilak, s1, v1)
    ! -- zero volume
    if (vol <= v0) then
      stage = s0
    ! -- linear relation between stage and volume above top of lake
    else if (vol >= v1) then
      call this%lak_calculate_sarea(ilak, s1, sa)
      stage = s1 + (vol - v1) / sa
    ! -- use combination of secant and bisection
    else
      en0 = s0
      en1 = s1
      ! sm = s1  ! causes divide by zero in 1st line in secantbisection loop
      ! sm = s0  ! causes divide by zero in 1st line in secantbisection loop
      sm = DZERO
      f0 = vol - v0
      f1 = vol - v1
      ibs = 0
      secantbisection: do i = 1, 150
        denom = f1 - f0
        if (denom /= DZERO) then
          ds = f1 * (s1 - s0) / denom
        else
          ibs = 13
        end if
        if (i == 1) then
          ds0 = ds
        end if
        ! -- use bisection if end points are exceeded
        if (sm < en0 .or. sm > en1) ibs = 13
        ! -- use bisection if secant method stagnates or if
        !    ds exceeds previous ds - bisection would occur
        !    after conditions exceeded in 13 iterations
        if (ds*ds0 < DPREC .or. ABS(ds) > ABS(ds0)) ibs = ibs + 1
        if (ibs > 12) then
          ds = DHALF * (s1 - s0)
          ibs = 0
        end if
        sm = s1 - ds
        if (ABS(ds) < DEM6) then
          !write(*,'(i4,4(g15.6))') i, sm, vol, ds, fm
          exit secantbisection
        end if
        call this%lak_calculate_vol(ilak, sm, vm)
        fm = vol - vm
        s0 = s1
        f0 = f1
        s1 = sm
        f1 = fm
        ds0 = ds
      end do secantbisection
      stage = sm
      if (ABS(ds) >= DEM6) then
        write(this%iout, '(1x,a,1x,i5,4(1x,a,1x,g15.6))') &
     &   'LAK_VOL2STAGE failed for lake', ilak, 'volume error =', fm, &
     &   'finding stage (', stage, ') for volume =', vol, &
     &    'final change in stage =', ds
      end if
    end if
    !
    ! -- return
    return
  end subroutine lak_vol2stage


  function lak_check_valid(this, itemno) result(ierr)
! ******************************************************************************
!  lak_check_valid -- Determine if a valid lake or outlet number has been
!                     specified.
! ******************************************************************************
    use SimModule, only: ustop, store_error
    ! -- return
    integer(I4B) :: ierr
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: itemno
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: ival
    ! -- formats
! ------------------------------------------------------------------------------
    ierr = 0
    ival = abs(itemno)
    if (itemno > 0) then
      if (ival < 1 .or. ival > this%nlakes) then
        write(errmsg,'(4x,a,1x,i6,1x,a,1x,i6)') &
          '****ERROR. LAKENO ', itemno, 'MUST BE > 0 and <= ', this%nlakes
        call store_error(errmsg)
        ierr = 1
      end if
    else
      if (ival < 1 .or. ival > this%noutlets) then
        write(errmsg,'(4x,a,1x,i6,1x,a,1x,i6)') &
          '****ERROR. IOUTLET ', itemno, 'MUST BE > 0 and <= ', this%noutlets
        call store_error(errmsg)
        ierr = 1
      end if
    end if
  end function lak_check_valid

  subroutine lak_set_stressperiod(this, itemno, line)
! ******************************************************************************
! lak_set_stressperiod -- Set a stress period attribute for lakweslls(itemno)
!                         using keywords.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !use ConstantsModule, only: LINELENGTH, DTWO
    use TdisModule, only: kper, perlen, totimsav
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: itemno
    character (len=*), intent(in) :: line
    ! -- local
    character(len=LINELENGTH) :: text
    character(len=LINELENGTH) :: caux
    character(len=LINELENGTH) :: keyword
    character(len=LINELENGTH) :: errmsg
    character(len=LENBOUNDNAME) :: bndName
    character(len=9) :: citem
    integer(I4B) :: ierr
    integer(I4B) :: itmp
    integer(I4B) :: ival, istart, istop
    integer(I4B) :: i0
    integer(I4B) :: lloc
    integer(I4B) :: ii
    integer(I4B) :: jj
    integer(I4B) :: iaux
    real(DP) :: rval
    real(DP) :: endtim
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Find time interval of current stress period.
    endtim = totimsav + perlen(kper)
    !
    ! -- write abs(itemno) to citem string
    itmp = ABS(itemno)
    write (citem,'(i9.9)') itmp
    !
    ! -- Assign boundary name
    if (this%inamedbound==1) then
      bndName = this%boundname(itemno)
    else
      bndName = ''
    end if
    !
    ! -- read line
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, this%iout, this%inunit)
    i0 = istart
    keyword = line(istart:istop)
    select case (line(istart:istop))
      case ('STATUS')
        ierr = this%lak_check_valid(itemno)
        if (ierr /= 0) goto 999
        !bndName = this%boundname(itemno)
        call urword(line, lloc, istart, istop, 1, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        this%status(itmp) = text
        if (text == 'CONSTANT') then
          this%iboundpak(itmp) = -1
        else if (text == 'INACTIVE') then
          this%iboundpak(itmp) = 0
        else if (text == 'ACTIVE') then
          this%iboundpak(itmp) = 1
        else
          write(errmsg,'(4x,a,a)') &
            '****ERROR. UNKNOWN '//trim(this%text)//' LAK STATUS KEYWORD: ', &
            text
          call store_error(errmsg)
        end if
      case ('STAGE')
        ierr = this%lak_check_valid(itemno)
        if (ierr /= 0) goto 999
        !bndName = this%boundname(itemno)
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For STAGE
        call read_single_value_or_time_series(text, &
                                              this%stage(itmp)%value, &
                                              this%stage(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'STAGE', &
                                              bndName, this%inunit)
      case ('RAINFALL')
        ierr = this%lak_check_valid(itemno)
        if (ierr /= 0) goto 999
        !bndName = this%boundname(itemno)
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For RAINFALL
        call read_single_value_or_time_series(text, &
                                              this%rainfall(itmp)%value, &
                                              this%rainfall(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'RAINFALL', &
                                              bndName, this%inunit)
      case ('EVAPORATION')
        ierr = this%lak_check_valid(itemno)
        if (ierr /= 0) goto 999
        !bndName = this%boundname(itemno)
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For EVAPORATION
        call read_single_value_or_time_series(text, &
                                              this%evaporation(itmp)%value, &
                                              this%evaporation(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'EVAPORATION', &
                                              bndName, this%inunit)
      case ('RUNOFF')
        ierr = this%lak_check_valid(itemno)
        if (ierr /= 0) goto 999
        !bndName = this%boundname(itemno)
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For RUNOFF
        call read_single_value_or_time_series(text, &
                                              this%runoff(itmp)%value, &
                                              this%runoff(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'RUNOFF', &
                                              bndName, this%inunit)
      case ('INFLOW')
        ierr = this%lak_check_valid(itemno)
        if (ierr /= 0) goto 999
        !bndName = this%boundname(itemno)
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For specified INFLOW
        call read_single_value_or_time_series(text, &
                                              this%inflow(itmp)%value, &
                                              this%inflow(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'INFLOW', &
                                              bndName, this%inunit)
      case ('WITHDRAWAL')
        ierr = this%lak_check_valid(itemno)
        if (ierr /= 0) goto 999
        !bndName = this%boundname(itemno)
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For specified WITHDRAWAL
        call read_single_value_or_time_series(text, &
                                              this%withdrawal(itmp)%value, &
                                              this%withdrawal(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'WITHDRAWAL', &
                                              bndName, this%inunit)
      case ('RATE')
        ierr = this%lak_check_valid(-itemno)
        if (ierr /= 0) goto 999
        bndName = 'OUTLET' // citem
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For specified OUTLET RATE
        call read_single_value_or_time_series(text, &
                                              this%outrate(itmp)%value, &
                                              this%outrate(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'OUTRATE', &
                                              bndName, this%inunit)
      case ('INVERT')
        ierr = this%lak_check_valid(-itemno)
        if (ierr /= 0) goto 999
        bndName = 'OUTLET' // citem
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For OUTLET INVERT
        call read_single_value_or_time_series(text, &
                                              this%outinvert(itmp)%value, &
                                              this%outinvert(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'OUTINVERT', &
                                              bndName,this%inunit)
      case ('WIDTH')
        ierr = this%lak_check_valid(-itemno)
        if (ierr /= 0) goto 999
        bndName = 'OUTLET' // citem
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For OUTLET WIDTH
        call read_single_value_or_time_series(text, &
                                              this%outwidth(itmp)%value, &
                                              this%outwidth(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'OUTWIDTH', &
                                              bndName, this%inunit)
      case ('ROUGH')
        ierr = this%lak_check_valid(-itemno)
        if (ierr /= 0) goto 999
        bndName = 'OUTLET' // citem
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For OUTLET ROUGHNESS
        call read_single_value_or_time_series(text, &
                                              this%outrough(itmp)%value, &
                                              this%outrough(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'OUTROUGH', &
                                              bndName, this%inunit)
      case ('SLOPE')
        ierr = this%lak_check_valid(-itemno)
        if (ierr /= 0) goto 999
        bndName = 'OUTLET' // citem
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For OUTLET SLOPE
        call read_single_value_or_time_series(text, &
                                              this%outslope(itmp)%value, &
                                              this%outslope(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'OUTSLOPE', &
                                              bndName, this%inunit)
      case ('AUXILIARY')
        ierr = this%lak_check_valid(itemno)
        if (ierr /= 0) goto 999
        !bndName = this%boundname(itemno)
        call urword(line, lloc, istart, istop, 1, ival, rval, this%iout, this%inunit)
        caux = line(istart:istop)
        do iaux = 1, this%naux
          if (trim(adjustl(caux)) /= trim(adjustl(this%auxname(iaux)))) cycle
          call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
          text = line(istart:istop)
          jj = 1 !iaux
          ii = (itmp-1) * this%naux + iaux
          call read_single_value_or_time_series(text, &
                                                this%lauxvar(ii)%value, &
                                                this%lauxvar(ii)%name, &
                                                endtim,  &
                                                this%Name, 'AUX', this%TsManager, &
                                                this%iprpak, itmp, jj, &
                                                this%auxname(iaux), bndName, &
                                                this%inunit)
          exit
        end do
      case default
        write(errmsg,'(4x,a,a)') &
          '****ERROR. UNKNOWN '//trim(this%text)//' LAK DATA KEYWORD: ', &
                                  line(istart:istop)
        call store_error(errmsg)
        call ustop()
    end select
    !
    ! -- terminate if any errors were detected
999 if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- write keyword data to output file
    if (this%iprpak /= 0) then
      write (this%iout, '(3x,i10,1x,a)') itmp, line(i0:istop)
    end if
    !
    ! -- return
    return
  end subroutine lak_set_stressperiod


  subroutine lak_set_attribute_error(this, ilak, keyword, msg)
! ******************************************************************************
! lak_set_attribute_error -- Issue a parameter error for lakweslls(ilak)
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SimModule, only: store_error
    ! -- dummy
    class(LakType),intent(inout) :: this
    integer(I4B), intent(in) :: ilak
    character (len=*), intent(in) :: keyword
    character (len=*), intent(in) :: msg
    ! -- local
    character(len=LINELENGTH) :: errmsg
    ! -- formats
! ------------------------------------------------------------------------------
    if (len(msg) == 0) then
      write(errmsg,'(4x,a,1x,a,1x,a,1x,i6,1x,a)') &
        '****ERROR.', keyword, ' for LAKE', ilak, 'has already been set.'
    else
      write(errmsg,'(4x,a,1x,a,1x,a,1x,i6,1x,a)') &
        '****ERROR.', keyword, ' for LAKE', ilak, msg
    end if
    call store_error(errmsg)
    ! -- return
    return
  end subroutine lak_set_attribute_error

  subroutine lak_options(this, option, found)
! ******************************************************************************
! lak_options -- set options specific to LakType
!
! lak_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: MAXCHARLEN, DZERO
    use OpenSpecModule, only: access, form
    use SimModule, only: ustop, store_error
    use InputOutputModule, only: urword, getunit, openfile
    ! -- dummy
    class(LakType),   intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical,          intent(inout) :: found
    ! -- local
    character(len=MAXCHARLEN) :: fname, keyword
    real(DP) :: r
    ! -- formats
    character(len=*),parameter :: fmtlengthconv = &
      "(4x, 'LENGTH CONVERSION VALUE (',g15.7,') SPECIFIED.')"
    character(len=*),parameter :: fmttimeconv = &
      "(4x, 'TIME CONVERSION VALUE (',g15.7,') SPECIFIED.')"
    character(len=*),parameter :: fmtoutdmax = &
      "(4x, 'MAXIMUM OUTLET WATER DEPTH (',g15.7,') SPECIFIED.')"
    character(len=*),parameter :: fmtlakeopt = &
      "(4x, 'LAKE ', a, ' VALUE (',g15.7,') SPECIFIED.')"
    character(len=*),parameter :: fmtlakbin = &
      "(4x, 'LAK ', 1x, a, 1x, ' WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
! ------------------------------------------------------------------------------
    !
    select case (option)
      case ('PRINT_STAGE')
        this%iprhed = 1
        write(this%iout,'(4x,a)') trim(adjustl(this%text))// &
          ' STAGES WILL BE PRINTED TO LISTING FILE.'
        found = .true.
      case('STAGE')
        call this%parser%GetStringCaps(keyword)
        if (keyword == 'FILEOUT') then
          call this%parser%GetString(fname)
          this%istageout = getunit()
          call openfile(this%istageout, this%iout, fname, 'DATA(BINARY)',  &
                       form, access, 'REPLACE')
          write(this%iout,fmtlakbin) 'STAGE', fname, this%istageout
          found = .true.
        else
          call store_error('OPTIONAL STAGE KEYWORD MUST BE FOLLOWED BY FILEOUT')
        end if
      case('BUDGET')
        call this%parser%GetStringCaps(keyword)
        if (keyword == 'FILEOUT') then
          call this%parser%GetString(fname)
          this%ibudgetout = getunit()
          call openfile(this%ibudgetout, this%iout, fname, 'DATA(BINARY)',  &
                        form, access, 'REPLACE')
          write(this%iout,fmtlakbin) 'BUDGET', fname, this%ibudgetout
          found = .true.
        else
          call store_error('OPTIONAL BUDGET KEYWORD MUST BE FOLLOWED BY FILEOUT')
        end if
      case('MOVER')
        this%imover = 1
        write(this%iout, '(4x,A)') 'MOVER OPTION ENABLED'
        found = .true.
      case('LENGTH_CONVERSION')
        this%convlength = this%parser%GetDouble()
        write(this%iout, fmtlengthconv) this%convlength
        found = .true.
      case('TIME_CONVERSION')
        this%convtime = this%parser%GetDouble()
        write(this%iout, fmttimeconv) this%convtime
        found = .true.
      case('SURFDEP')
        r = this%parser%GetDouble()
        if (r < DZERO) then
          r = DZERO
        end if
        this%surfdep = r
        write(this%iout, fmtlakeopt) 'SURFDEP', this%surfdep
        found = .true.
      !
      ! -- right now these are options that are only available in the
      !    development version and are not included in the documentation.
      !    These options are only available when IDEVELOPMODE in
      !    constants module is set to 1
      case('GROUNDWATER_HEAD_CONDUCTANCE')
        call this%parser%DevOpt()
        this%igwhcopt = 1
        write(this%iout, '(4x,a)')                                             &
     &    'CONDUCTANCE FOR HORIZONTAL CONNECTIONS WILL BE CALCULATED ' //      &
     &    'USING THE GROUNDWATER HEAD'
        found = .true.
      case('MAXIMUM_OUTLET_DEPTH')
        call this%parser%DevOpt()
        this%outdmax = this%parser%GetDouble()
        write(this%iout, fmtoutdmax) this%outdmax
        found = .true.
      case('NO_FINAL_CHECK')
        call this%parser%DevOpt()
        this%iconvchk = 0
        write(this%iout, '(4x,a)')                                             &
     &    'A FINAL CONVERGENCE CHECK OF THE CHANGE IN LAKE STAGES ' //         &
     &    'WILL NOT BE MADE'
        found = .true.
      case('NO_FINAL_RESIDUAL_CHECK')
        call this%parser%DevOpt()
        this%iconvresidchk = 0
        write(this%iout, '(4x,a)')                                             &
     &    'A FINAL CONVERGENCE CHECK OF THE CHANGE IN LAKE RESIDUALS ' //      &
     &    'WILL NOT BE MADE'
        found = .true.
      case('MAXIMUM_PERCENT_DIFFERENCE')
        call this%parser%DevOpt()
        r = this%parser%GetDouble()
        if (r < DZERO) then
          r = DEM1
        end if
        this%pdmax = r
        write(this%iout, fmtlakeopt) 'MAXIMUM_PERCENT_DIFFERENCE', this%pdmax
        found = .true.
      case default
        !
        ! -- No options found
        found = .false.
    end select
    !
    ! -- return
    return
  end subroutine lak_options

  subroutine lak_ar(this)
  ! ******************************************************************************
  ! lak_ar -- Allocate and Read
  ! Subroutine: (1) create new-style package
  !             (2) point bndobj to the new package
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      ! -- dummy
      class(LakType),intent(inout) :: this
      ! -- local
      ! -- format
  ! ------------------------------------------------------------------------------
    !
    call this%obs%obs_ar()
    !
    ! -- Allocate arrays in LAK and in package superclass
    call this%lak_allocate_arrays()
    !
    ! -- read optional initial package parameters
    call this%read_initial_attr()
    !
    ! -- setup pakmvrobj
    if (this%imover /= 0) then
      allocate(this%pakmvrobj)
      call this%pakmvrobj%ar(this%noutlets, this%nlakes, this%origin)
    endif
    !
    ! -- return
    return
  end subroutine lak_ar


  subroutine lak_rp(this)
! ******************************************************************************
! lak_rp -- Read and Prepare
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use TdisModule, only: kper, nper
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(LakType),intent(inout) :: this
    ! -- local
    integer(I4B) :: ierr
    integer(I4B) :: node, n
    logical :: isfound, endOfBlock
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: itemno
    integer(I4B) :: j
    integer(I4B) :: isfirst
    ! -- formats
    character(len=*),parameter :: fmtblkerr = &
      "('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*),parameter :: fmtlsp = &
      "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
    character(len=*),parameter :: fmtstdy = &
      "(1X,//21X,'MULTI-AQUIFER WELL DATA'," // &
      "/21X,'FOR STRESS PERIOD',I6," // &
      "/20X,A16,1X,'LAK LAKES'," // &
      "//29X,'RATE DATA'," // &
      "/1X,65('-'),/1X,'  WELL NO.     STATUS       RATE SPEC. HEAD " // &
      " PUMP ELEV  RED. LEN.')"
    character(len=*), parameter :: fmtwelld = &
      "(1X,I10,1X,A10,1X,G10.4,1X,A10,G10.3,1X,A10)"
    character(len=*),parameter :: fmtfwh = &
      "(1X,//21X,'MULTI-AQUIFER WELL DATA'," // &
      "/21X,'FOR STRESS PERIOD',I6," // &
      "//25X,'FLOWING WELL DATA'," // &
      "/1X,65('-'),/12X,'  WELL NO.  ELEVATION   CONDUCT.  RED. LEN.')"
    character(len=*), parameter :: fmtfwd = &
      "(12X,I10,1X,3(G10.4,1X))"
    character(len=*),parameter :: fmtsoh = &
      "(1X,//21X,'MULTI-AQUIFER WELL DATA'," // &
      "/21X,'FOR STRESS PERIOD',I6," // &
      "//25X,'WELL SHUTOFF DATA'," // &
      "/1X,65('-'),/12X,'  WELL NO.  ELEVATION       MINQ       MAXQ')"
    character(len=*), parameter :: fmtsod = &
      "(12X,I10,1X,G10.4,1X,2(A10,1X))"
    character(len=*), parameter :: fmtline = &
      "(1X,65('-'),//)"
! ------------------------------------------------------------------------------
    !
    ! -- initialize flags
    isfirst = 1
    !
    ! -- set nbound to maxbound
    this%nbound = this%maxbound
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
      if(isfound) then
        !
        ! -- save last value and read period number
        this%lastonper = this%ionper
        this%ionper = this%parser%GetInteger()
        !
        ! -- check to make sure period blocks are increasing
        if (this%ionper < this%lastonper) then
          write(errmsg, '(a, i0)') &
            'ERROR IN STRESS PERIOD ', kper
          call store_error(errmsg)
          write(errmsg, '(a, i0)') &
            'PERIOD NUMBERS NOT INCREASING.  FOUND ', this%ionper
          call store_error(errmsg)
          write(errmsg, '(a, i0)') &
            'BUT LAST PERIOD BLOCK WAS ASSIGNED ', this%lastonper
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
      else
        !
        ! -- PERIOD block not found
        if (ierr < 0) then
          ! -- End of file found; data applies for remainder of simulation.
          this%ionper = nper + 1
        else
          ! -- Found invalid block
          write(errmsg, fmtblkerr) adjustl(trim(line))
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
      endif
    end if
    !
    ! -- Read data if ionper == kper
    if(this%ionper == kper) then
      !
      ! -- Remove all time-series links associated with this package
      !call this%TsManager%Reset(this%name)
      !
      ! -- set steady-state flag based on gwfiss
      this%isteady = this%gwfiss

      this%check_attr = 1
      stressperiod: do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        if (isfirst /= 0) then
          isfirst = 0
          if (this%iprpak /= 0) then
            write(this%iout,'(/1x,a,1x,i6,/)')                                  &
              'READING '//trim(adjustl(this%text))//' DATA FOR PERIOD', kper
            write(this%iout,'(3x,a)')  '     LAKE KEYWORD AND DATA'
            write(this%iout,'(3x,78("-"))')
          end if
        end if
        itemno = this%parser%GetInteger()
        call this%parser%GetRemainingLine(line)
        call this%lak_set_stressperiod(itemno, line)
      end do stressperiod

      if (this%iprpak /= 0) then
        write(this%iout,'(/1x,a,1x,i6,/)')                                      &
          'END OF '//trim(adjustl(this%text))//' DATA FOR PERIOD', kper
      end if
    !
    else
      write(this%iout,fmtlsp) trim(this%filtyp)
    endif
    !
    !write summary of lake stress period error messages
    ierr = count_errors()
    if (ierr > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- fill arrays
    do n = 1, this%nlakes
      do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
        node = this%cellid(j)
        this%nodelist(j) = node

        this%bound(1,j) = this%xnewpak(n)

        this%bound(2,j) = this%satcond(j)

        this%bound(3,j) = this%belev(j)

      end do
    end do
    !
    ! -- return
    return
  end subroutine lak_rp

  subroutine lak_ad(this)
! ******************************************************************************
! lak_ad -- Add package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Advance the time series
    call this%TsManager%ad()
    !
    ! -- copy xnew into xold and set xnewpak to stage%value for
    !    constant stage lakes
    do n = 1, this%nlakes
      this%xoldpak(n) = this%xnewpak(n)
      this%stageiter(n) = this%xnewpak(n)
      if (this%iboundpak(n) < 0) then
        this%xnewpak(n) = this%stage(n)%value
      end if
      this%seep0(n) = DZERO
    end do
    !
    ! -- pakmvrobj ad
    if (this%imover == 1) then
      call this%pakmvrobj%ad()
    end if
    !
    ! -- For each observation, push simulated value and corresponding
    !    simulation time from "current" to "preceding" and reset
    !    "current" value.
    call this%obs%obs_ad()
    !
    ! -- return
    return
  end subroutine lak_ad

  subroutine lak_cf(this)
  ! ******************************************************************************
  ! lak_cf -- Formulate the HCOF and RHS terms
  ! Subroutine: (1) skip if no lakes
  !             (2) calculate hcof and rhs
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    class(LakType) :: this
    integer(I4B) :: j, n
    integer(I4B) :: igwfnode
    real(DP) ::  hlak, blak
  ! ------------------------------------------------------------------------------
    !!
    !! -- Calculate lak conductance and update package RHS and HCOF
    !call this%lak_cfupdate()
    !
    ! --
    do n = 1, this%nlakes
      this%seep0(n) = this%seep(n)
    end do
    !
    !
    do n = 1, this%nlakes
    !  write(*,'(4x,1x,i4.4,1x,g15.7)') n, this%xnewpak(n)
      this%s0(n) = this%xnewpak(n)
    end do
    !
    ! -- pakmvrobj cf
    if(this%imover == 1) then
      call this%pakmvrobj%cf()
    end if
    !
    ! -- find highest active cell
    do n = 1, this%nlakes
      do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
        ! -- skip horizontal connections
        if (this%ictype(j) /= 0) then
          cycle
        end if
        igwfnode = this%nodesontop(j)
        if (this%ibound(igwfnode) == 0) then
          call this%dis%highest_active(igwfnode, this%ibound)
        end if
        this%nodelist(j) = igwfnode
        this%cellid(j) = igwfnode
      end do
    end do
    !
    ! -- reset ibound for cells where lake stage is above the bottom
    !    of the lake in the cell - only applied to vertical connections
    do n = 1, this%nlakes
      hlak = this%xnewpak(n)
      ! -- skip inactive lakes
      if (this%iboundpak(n) == 0) cycle
      do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
        igwfnode = this%cellid(j)
        ! -- skip inactive or constant head cells
        if (this%ibound(igwfnode) < 1) then
          cycle
        end if
        ! -- skip horizontal connections
        if (this%ictype(j) /= 0) then
          cycle
        end if
        ! -- skip embedded lakes
        if (this%ictype(j) == 2 .or. this%ictype(j) == 3) then
          cycle
        end if
        blak = this%belev(j)
        if (hlak > blak) then
          this%ibound(igwfnode) = 10000
        else
          this%ibound(igwfnode) = 1
        end if
      end do

    end do
    !
    ! -- Return
    return
  end subroutine lak_cf

  subroutine lak_fc(this, rhs, ia, idxglo, amatsln)
  ! **************************************************************************
  ! lak_fc -- Copy rhs and hcof into solution rhs and amat
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
    ! -- dummy
    class(LakType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: j, n
    integer(I4B) :: igwfnode
    integer(I4B) :: ipossymd
! --------------------------------------------------------------------------
    !
    ! -- pakmvrobj fc
    if(this%imover == 1) then
      call this%pakmvrobj%fc()
    end if
    !!
    !!
    !do n = 1, this%nlakes
    !!  write(*,'(4x,1x,i4.4,1x,g15.7)') n, this%xnewpak(n)
    !  this%s0(n) = this%xnewpak(n)
    !end do
    !
    !
    ! -- make a stab at a solution
    call this%lak_solve()
    !
    ! -- add terms to the gwf matrix
    do n = 1, this%nlakes
      do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
        igwfnode = this%cellid(j)
        if (this%ibound(igwfnode) < 1) cycle
        ipossymd = idxglo(ia(igwfnode))
        amatsln(ipossymd) = amatsln(ipossymd) + this%hcof(j)
        rhs(igwfnode) = rhs(igwfnode) + this%rhs(j)
      end do
    end do
    !
    ! -- write some output to the screen
    !do n = 1, this%nlakes
    !!  write(*,'(4x,i4,2(1x,g15.7))') n, this%seep0(n), this%seep(n)
    !  write(*,'(4x,i4,2(1x,g15.7))') n, this%s0(n), this%xnewpak(n)
    !end do
    !
    ! -- return
    return
  end subroutine lak_fc

  subroutine lak_fn(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! lak_fn -- Fill newton terms
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(LakType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: j, n
    integer(I4B) :: ipos
    integer(I4B) :: igwfnode
    integer(I4B) :: idry
    real(DP) :: hlak
    real(DP) :: avail
    real(DP) :: ra
    real(DP) :: ro
    real(DP) :: qinf
    real(DP) :: ex
    real(DP) :: head
    real(DP) :: clak1
    real(DP) :: q
    real(DP) :: q1
    real(DP) :: rterm
    real(DP) :: drterm
! --------------------------------------------------------------------------
    do n = 1, this%nlakes
      if (this%iboundpak(n) == 0) cycle
      hlak = this%xnewpak(n)
      call this%lak_calculate_available(n, hlak, avail, &
                                        ra, ro, qinf, ex, this%delh)
      do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
        igwfnode = this%cellid(j)
        ipos = ia(igwfnode)
        head = this%xnew(igwfnode)
        if (-this%hcof(j) > DZERO) then
          if (this%ibound(igwfnode) > 0) then
            ! -- estimate lake-aquifer exchange with perturbed groundwater head
            !    exchange is relative to the lake
            !avail = DEP20
            call this%lak_estimate_conn_exchange(2, n, j, idry, hlak, head+this%delh, q1, clak1, avail)
            q1 = -q1
            ! -- calculate unperturbed lake-aquifer exchange
            q = this%hcof(j) * head - this%rhs(j)
            ! -- calculate rterm
            rterm = this%hcof(j) * head
            ! -- calculate derivative
            drterm = (q1 - q) / this%delh
            ! -- add terms to convert conductance formulation into
            !    newton-raphson formulation
            amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + drterm - this%hcof(j)
            rhs(igwfnode) = rhs(igwfnode) - rterm + drterm * head
          end if
        end if
      end do
    end do

    !
    ! -- return
    return
  end subroutine lak_fn

  subroutine lak_cc(this, iend, icnvg)
! **************************************************************************
! lak_cc -- Final convergence check for package
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(LakType), intent(inout) :: this
    integer(I4B), intent(in) :: iend
    integer(I4B), intent(inout) :: icnvg
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: ifirst
    real(DP) :: dh
    real(DP) :: residb
    real(DP) :: inf
    real(DP) :: outf
    real(DP) :: avgf
    real(DP) :: ra
    real(DP) :: ro
    real(DP) :: qinf
    real(DP) :: ex
    real(DP) :: pd
    ! format
02000 format(4x,'LAKE PACKAGE FAILED CONVERGENCE CRITERIA',//,                  &
             4x,a10,4(1x,a15),/,4x,74('-'))
02010 format(4x,i10,4(1x,G15.7))
02020 format(4x,74('-'))

! --------------------------------------------------------------------------
    ifirst = 1
    if (this%iconvchk /= 0) then
      final_check: do n = 1, this%nlakes
        if (this%iboundpak(n) < 1) cycle
        dh = ABS(this%s0(n) - this%xnewpak(n))
        call this%lak_calculate_residual(n, this%xnewpak(n), residb)
        call this%lak_calculate_available(n, this%xnewpak(n), inf, &
                                          ra, ro, qinf, ex)
        outf = inf - residb
        avgf = DHALF * (inf + outf)
        pd = DZERO
        if (this%iconvresidchk /= 0) then
          if (avgf > DZERO) then
            pd = 100.d0 * residb / avgf
          end if
        end if
        !write(*,'(1x,i4,6(1x,g10.4))') n, this%s0(n), this%xnewpak(n), residb, outf, inf, pd
        if (dh > this%delh .or. ABS(pd) > this%pdmax) then
          icnvg = 0
          ! write convergence check information if this is the last outer iteration
          if (iend == 1) then
            if (ifirst == 1) then
              ifirst = 0
              write(this%iout, 2000) '      LAKE',                                 &
                '        MAX. DH', '    DH CRITERIA',                              &
                '      PCT DIFF.', 'PCT DIFF. CRIT.'
            end if
            write(this%iout,2010) n, dh, this%delh, pd, this%pdmax
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
  end subroutine lak_cc


  subroutine lak_bd(this, x, idvfl, icbcfl, ibudfl, icbcun, iprobs,            &
                    isuppress_output, model_budget, imap, iadv)
! ******************************************************************************
! lak_bd -- Calculate Volumetric Budget for the lake
! Note that the compact budget will always be used.
! Subroutine: (1) Process each package entry
!             (2) Write output
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper, delt, pertim, totim
    use ConstantsModule, only: LENBOUNDNAME, DHNOFLO, DHDRY
    use BudgetModule, only: BudgetType
    use InputOutputModule, only: ulasav, ubdsv06
    ! -- dummy
    class(LakType) :: this
    real(DP),dimension(:),intent(in) :: x
    integer(I4B), intent(in) :: idvfl
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    integer(I4B), intent(in) :: iprobs
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    integer(I4B), dimension(:), optional, intent(in) :: imap
    integer(I4B), optional, intent(in) :: iadv
    ! -- local
    integer(I4B) :: ibinun
    real(DP) :: rrate
    real(DP) :: gwfratin, gwfratout
    real(DP) :: rainin, rainout
    real(DP) :: evapin, evapout
    real(DP) :: within, without
    real(DP) :: roin, roout
    real(DP) :: qinfin, qinfout
    real(DP) :: extin, extout
    real(DP) :: storatin, storatout
    real(DP) :: ratin, ratout
    real(DP) :: chratin, chratout
    real(DP) :: mvrratin
    real(DP) :: qtomvr
    integer(I4B) :: naux
    ! -- for budget
    integer(I4B) :: i, j, n, n2
    integer(I4B) :: ii
    integer(I4B) :: igwfnode
    integer(I4B) :: nlen, n1
    real(DP) :: hlak, hgwf
    real(DP) :: v0, v1
    real(DP) :: blak
    real(DP) :: s
    real(DP) :: d
    real(DP) :: v
    real(DP) :: q
    real(DP) :: q2
    ! -- for observations
    integer(I4B) :: iprobslocal
    ! -- formats
    character(len=*), parameter :: fmttkk = &
      "(1X,/1X,A,'   PERIOD ',I0,'   STEP ',I0)"
! ------------------------------------------------------------------------------
    !
    ! -- recalculate package HCOF and RHS terms with latest groundwater and
    !    lak heads prior to calling base budget functionality
    !call this%lak_cfupdate()
    !
    ! -- update the lake hcof and rhs terms
    call this%lak_solve(.false.)
    !
    ! -- Suppress saving of simulated values; they
    !    will be saved at end of this procedure.
    iprobslocal = 0
    ! -- call base functionality in bnd_bd
    call this%BndType%bnd_bd(x, idvfl, icbcfl, ibudfl, icbcun, iprobslocal,    &
                             isuppress_output, model_budget, this%imap,        &
                             iadv=1)
    !
    ! -- lak budget routines (start by resetting)
    call this%budget%reset()
    !
    ! -- add to lake budget terms
    ! -- gwf flow
    gwfratin = DZERO
    gwfratout = DZERO
    rainin = DZERO
    rainout = DZERO
    evapin = DZERO
    evapout = DZERO
    within = DZERO
    without = DZERO
    roin = DZERO
    roout = DZERO
    qinfin = DZERO
    qinfout = DZERO
    extin = DZERO
    extout = DZERO
    storatin = DZERO
    storatout = DZERO
    ratin = DZERO
    ratout = DZERO
    chratin = DZERO
    chratout = DZERO
    mvrratin = DZERO
    qtomvr = DZERO
    do n = 1, this%nlakes
      this%chterm(n) = DZERO
      if (this%iboundpak(n) == 0) cycle
      hlak = this%xnewpak(n)
      call this%lak_calculate_vol(n, hlak, v1)
      ! -- add budget terms for active lakes
      if (this%iboundpak(n) /= 0) then
        ! -- rainfall
        rrate = this%precip(n)
        call this%lak_accumulate_chterm(n, rrate, chratin, chratout)
        !
        ! -- See if flow is into lake or out of lake.
        if (rrate < DZERO) then
          !
          ! -- Flow is out of lake subtract rate from ratout.
          rainout = rainout - rrate
        else
          !
          ! -- Flow is into lake; add rate to ratin.
          rainin = rainin + rrate
        end if
        ! -- evaporation
        rrate = this%evap(n)
        call this%lak_accumulate_chterm(n, rrate, chratin, chratout)
        !
        ! -- See if flow is into lake or out of lake.
        if (rrate < DZERO) then
          !
          ! -- Flow is out of lake subtract rate from ratout.
          evapout = evapout - rrate
        else
          !
          ! -- Flow is into lake; add rate to ratin.
          evapin = evapin + rrate
        end if
        ! -- runoff
        rrate = this%runoff(n)%value
        call this%lak_accumulate_chterm(n, rrate, chratin, chratout)
        !
        ! -- See if flow is into lake or out of lake.
        if (rrate < DZERO) then
          !
          ! -- Flow is out of lake subtract rate from ratout.
          roout = roout - rrate
        else
          !
          ! -- Flow is into lake; add rate to ratin.
          roin = roin + rrate
        end if
        ! -- inflow
        rrate = this%inflow(n)%value
        call this%lak_accumulate_chterm(n, rrate, chratin, chratout)
        !
        ! -- See if flow is into lake or out of lake.
        if (rrate < DZERO) then
          !
          ! -- Flow is out of lake subtract rate from ratout.
          qinfout = qinfout - rrate
        else
          !
          ! -- Flow is into lake; add rate to ratin.
          qinfin = qinfin + rrate
        end if
        ! -- withdrawals
        rrate = this%withr(n)
        call this%lak_accumulate_chterm(n, rrate, chratin, chratout)
        !
        ! -- See if flow is into lake or out of lake.
        if (rrate < DZERO) then
          !
          ! -- Flow is out of lake subtract rate from ratout.
          without = without - rrate
        else
          !
          ! -- Flow is into lake; add rate to ratin.
          within = within + rrate
        end if
        !
        ! -- add lake storage changes
        rrate = DZERO
        if (this%iboundpak(n) > 0) then
          if (this%isteady /= 1) then
            call this%lak_calculate_vol(n, this%xoldpak(n), v0)
            rrate = -(v1 - v0) / delt
            call this%lak_accumulate_chterm(n, rrate, chratin, chratout)
          !else
          !  rrate = -v1 / delt
          end if
        end if
        this%qsto(n) = rrate
        !
        ! -- See if storage flow is into maw or out of maw.
        if(rrate < DZERO) then
          !
          ! -- Flow is out of lake subtract rate from ratout.
          storatout = storatout - rrate
        else
          !
          ! -- Flow is into lake; add rate to ratin.
          storatin = storatin + rrate
        endif
        !
        ! -- add external outlets
        call this%lak_get_external_outlet(n, rrate)
        call this%lak_accumulate_chterm(n, rrate, chratin, chratout)
        if (this%imover == 1) then
          call this%lak_get_external_mover(n, q)
          rrate = rrate + q
          call this%lak_get_outlet_tomover(n, q2)
          qtomvr = qtomvr + q2
        end if
        !
        ! -- See if flow is into lake or out of lake.
        if (rrate < DZERO) then
          !
          ! -- Flow is out of lake subtract rate from ratout.
          extout = extout - rrate
        else
          !
          ! -- Flow is into lake; add rate to ratin.
          extin = extin + rrate
        end if
        !
        ! -- add mover terms
        if (this%imover == 1) then
          if (this%iboundpak(n) /= 0) then
            rrate = this%pakmvrobj%get_qfrommvr(n)
          else
            rrate = DZERO
          end if
          call this%lak_accumulate_chterm(n, rrate, chratin, chratout)
          mvrratin = mvrratin + rrate
        endif
      end if
    end do
    !
    ! -- gwf flow and constant flow to lake
    do n = 1, this%nlakes
      if (this%iboundpak(n) == 0) cycle
      rrate = DZERO
      hlak = this%xnewpak(n)
      do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
        igwfnode = this%cellid(j)
        hgwf = this%xnew(igwfnode)
        blak = this%belev(j)
        if (-this%hcof(j) > DZERO) then
          if (hgwf >= blak) then
            s = max(hlak, blak)
            rrate = this%hcof(j) * (s - hgwf)
          else
            rrate = this%rhs(j)
          end if
        else
          rrate = this%rhs(j)
        end if
        this%qleak(j) = rrate
        call this%lak_accumulate_chterm(n, rrate, chratin, chratout)
        !
        ! -- See if flow is into lake or out of lake.
        if(rrate < DZERO) then
          !
          ! -- Flow is out of lake subtract rate from ratout.
          gwfratout = gwfratout - rrate
        else
          !
          ! -- Flow is into lake; add rate to ratin.
          gwfratin = gwfratin + rrate
        endif
      end do

    end do
    ! -- add calculated terms
    call this%budget%addentry(qinfin, qinfout, delt,  &
                              this%clakbudget(5), isuppress_output)
    if (this%imover == 1) then
      call this%budget%addentry(mvrratin, DZERO, delt,  &
                                this%clakbudget(10), isuppress_output)
    end if
    call this%budget%addentry(rainin, rainout, delt,  &
                              this%clakbudget(2), isuppress_output)
    call this%budget%addentry(roin, roout, delt,  &
                              this%clakbudget(4), isuppress_output)
    call this%budget%addentry(gwfratin, gwfratout, delt,  &
                              this%clakbudget(1), isuppress_output)
    call this%budget%addentry(evapin, evapout, delt,  &
                              this%clakbudget(3), isuppress_output)
    call this%budget%addentry(within, without, delt,  &
                              this%clakbudget(6), isuppress_output)
    call this%budget%addentry(extin, extout, delt,  &
                              this%clakbudget(7), isuppress_output)
    if (this%imover == 1) then
      call this%budget%addentry(DZERO, qtomvr, delt,  &
                                this%clakbudget(11), isuppress_output)
    end if
    call this%budget%addentry(storatin, storatout, delt,  &
                              this%clakbudget(8), isuppress_output)
    call this%budget%addentry(chratin, chratout, delt,  &
                              this%clakbudget(9), isuppress_output)
    ! -- For continuous observations, save simulated values.
    if (this%obs%npakobs > 0 .and. iprobs > 0) then
      call this%lak_bd_obs()
    endif
    !
    ! -- set unit number for binary dependent variable output
    ibinun = 0
    if(this%istageout /= 0) then
      ibinun = this%istageout
    end if
    if(idvfl == 0) ibinun = 0
    if (isuppress_output /= 0) ibinun = 0
    !
    ! -- write lake binary output
    if (ibinun > 0) then
      do n = 1, this%nlakes
        v = this%xnewpak(n)
        d = v - this%lakebot(n)
        if (this%iboundpak(n) < 1) then
          v = DHNOFLO
        else if (d <= DZERO) then
          v = DHDRY
        end if
        this%dbuff(n) = v
      end do
      call ulasav(this%dbuff, '           STAGE', kstp, kper, pertim, totim,   &
                  this%nlakes, 1, 1, ibinun)
    end if
    !
    ! -- Set unit number for binary budget output
    ibinun = 0
    if(this%ibudgetout /= 0) then
      ibinun = this%ibudgetout
    end if
    if(icbcfl == 0) ibinun = 0
    if (isuppress_output /= 0) ibinun = 0
    !
    ! -- write lake binary budget output
    if (ibinun > 0) then
      ! FLOW JA FACE - lake to lake connections using outlets
      nlen = 0
      do n = 1, this%noutlets
        if (this%lakein(n) > 0 .and. this%lakeout(n) > 0) then
          nlen = nlen + 1
        end if
      end do
      if (nlen > 0) then
        naux = 0
        call ubdsv06(kstp, kper, '    FLOW-JA-FACE', this%name_model, this%name, &
                     this%name_model, this%name,                                 &
                     ibinun, naux, this%cauxcbc, nlen*2, 1, 1,                   &
                     nlen*2, this%iout, delt, pertim, totim)
        do n = 1, this%noutlets
          if (this%lakein(n) > 0 .and. this%lakeout(n) > 0) then
             q = this%simoutrate(n)
             if (this%imover == 1) then
               q = q + this%pakmvrobj%get_qtomvr(n)
             end if
             n1 = this%lakein(n)
             n2 = this%lakeout(n)
             call this%dis%record_mf6_list_entry(ibinun, n1, n2, q, naux,     &
                                                    this%qauxcbc,                 &
                                                    olconv=.FALSE.,               &
                                                    olconv2=.FALSE.)
             call this%dis%record_mf6_list_entry(ibinun, n2, n1, -q, naux,    &
                                                    this%qauxcbc,                 &
                                                    olconv=.FALSE.,               &
                                                    olconv2=.FALSE.)

          end if
        end do
      end if
      ! LEAKAGE
      naux = this%cbcauxitems
      this%cauxcbc(1) = '       FLOW-AREA'
      call ubdsv06(kstp, kper, this%clakbudget(1), this%name_model, this%name, &
                   this%name_model, this%name_model,                           &
                   ibinun, naux, this%cauxcbc, this%maxbound, 1, 1,            &
                   this%maxbound, this%iout, delt, pertim, totim)
      do n = 1, this%nlakes
        hlak = this%xnewpak(n)
        do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
          n2 = this%cellid(j)
          hgwf = this%xnew(n2)
          call this%lak_calculate_conn_warea(n, j, hlak, hgwf, this%qauxcbc(1))
          q = this%qleak(j)
          call this%dis%record_mf6_list_entry(ibinun, n, n2, q, naux,        &
                                                  this%qauxcbc,                  &
                                                  olconv=.FALSE.)

        end do
      end do
      ! INFLOW
      naux = 0
      call ubdsv06(kstp, kper, this%clakbudget(5), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%nlakes, 1, 1,              &
                   this%nlakes, this%iout, delt, pertim, totim)
      do n = 1, this%nlakes
        q = this%inflow(n)%value
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! RUNOFF
      naux = 0
      call ubdsv06(kstp, kper, this%clakbudget(4), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%nlakes, 1, 1,              &
                   this%nlakes, this%iout, delt, pertim, totim)
      do n = 1, this%nlakes
        q = this%runoff(n)%value
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! RAIN
      naux = 0
      call ubdsv06(kstp, kper, this%clakbudget(2), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%nlakes, 1, 1,              &
                   this%nlakes, this%iout, delt, pertim, totim)
      do n = 1, this%nlakes
        q = this%precip(n)
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! EVAPORATION
      naux = 0
      call ubdsv06(kstp, kper, this%clakbudget(3), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%nlakes, 1, 1,              &
                   this%nlakes, this%iout, delt, pertim, totim)
      do n = 1, this%nlakes
        q = this%evap(n)
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! WITHDRAWAL
      naux = 0
      call ubdsv06(kstp, kper, this%clakbudget(6), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%nlakes, 1, 1,              &
                   this%nlakes, this%iout, delt, pertim, totim)
      do n = 1, this%nlakes
        q = this%withr(n)
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! EXTERNAL OUTFLOW
      naux = 0
      call ubdsv06(kstp, kper, this%clakbudget(7), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%nlakes, 1, 1,              &
                   this%nlakes, this%iout, delt, pertim, totim)
      do n = 1, this%nlakes
        call this%lak_get_external_outlet(n, q)
        ! subtract tomover from external outflow
        call this%lak_get_external_mover(n, v)
        q = q + v
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! STORAGE
      naux = this%cbcauxitems
      this%cauxcbc(1) = '          VOLUME'
      call ubdsv06(kstp, kper, this%clakbudget(8), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%cauxcbc, this%nlakes, 1, 1,              &
                   this%nlakes, this%iout, delt, pertim, totim)
      do n = 1, this%nlakes
        call this%lak_calculate_vol(n, this%xnewpak(n), v1)
        q = this%qsto(n)
        this%qauxcbc(1) = v1
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%qauxcbc,                  &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! CONSTANT FLOW
      naux = 0
      call ubdsv06(kstp, kper, this%clakbudget(9), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%nlakes, 1, 1,              &
                   this%nlakes, this%iout, delt, pertim, totim)
      do n = 1, this%nlakes
        q = this%chterm(n)
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! MOVER
      if (this%imover == 1) then
        ! FROM MOVER
        naux = 0
        call ubdsv06(kstp, kper, this%clakbudget(10), this%name_model,         &
                     this%name, this%name_model, this%name,                    &
                     ibinun, naux, this%auxname,                               &
                     this%nlakes, 1, 1,                                        &
                     this%nlakes, this%iout, delt, pertim, totim)
        do n = 1, this%nlakes
          q = this%pakmvrobj%get_qfrommvr(n)
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,       &
                                                  this%auxvar(:,n),            &
                                                  olconv=.FALSE.,              &
                                                  olconv2=.FALSE.)
        end do
        ! TO MOVER
        naux = 0
        call ubdsv06(kstp, kper, this%clakbudget(11), this%name_model,         &
                     this%name, this%name_model, this%name,                    &
                     ibinun, naux, this%auxname,                               &
                     this%noutlets, 1, 1,                                      &
                     this%noutlets, this%iout, delt, pertim, totim)
        do n = 1, this%noutlets
          q = this%pakmvrobj%get_qtomvr(n)
          if (q > DZERO) then
            q = -q
          end if
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,       &
                                                  this%auxvar(:,n),            &
                                                  olconv=.FALSE.,              &
                                                  olconv2=.FALSE.)
        end do
      end if
      ! AUXILIARY VARIABLES
      naux = this%naux
      if (naux > 0) then
        call ubdsv06(kstp, kper, '       AUXILIARY', this%name_model, this%name,&
                     this%name_model, this%name,                                &
                     ibinun, naux, this%auxname, this%nlakes, 1, 1,             &
                     this%nlakes, this%iout, delt, pertim, totim)
        do n = 1, this%nlakes
          q = DZERO
          ! fill auxvar
          do i = 1, naux
            ii = (n-1) * naux + i
            this%auxvar(i,n) = this%lauxvar(ii)%value
          end do
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,       &
                                                  this%auxvar(:,n),            &
                                                  olconv=.FALSE.,              &
                                                  olconv2=.FALSE.)
        end do
      end if
    end if
    ! -- return
    return
  end subroutine lak_bd

  subroutine lak_ot(this, kstp, kper, iout, ihedfl, ibudfl)
    ! **************************************************************************
    ! pak1t -- Output package budget
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    use InputOutputModule, only: UWWORD
    ! -- dummy
    class(LakType) :: this
    integer(I4B),intent(in) :: kstp
    integer(I4B),intent(in) :: kper
    integer(I4B),intent(in) :: iout
    integer(I4B),intent(in) :: ihedfl
    integer(I4B),intent(in) :: ibudfl
    ! -- locals
    character(len=LINELENGTH) :: line, linesep
    character(len=16) :: text
    integer(I4B) :: n
    integer(I4B) :: j
    integer(I4B) :: iloc
    real(DP) :: hlak
    real(DP) :: q
    real(DP) :: qin
    real(DP) :: qinternalin
    real(DP) :: qro
    real(DP) :: qrai
    real(DP) :: qleakin
    real(DP) :: qleakout
    real(DP) :: qevt
    real(DP) :: qwdw
    real(DP) :: qext
    real(DP) :: qinternalout
    real(DP) :: qsto
    real(DP) :: qch
    real(DP) :: qtomover
    real(DP) :: qfrommover
    real(DP) :: qtin
    real(DP) :: qtout
    real(DP) :: qerr
    real(DP) :: qavg
    real(DP) :: qerrpd
    ! format
 2000 FORMAT ( 1X, ///1X, A, A, A, '   PERIOD ', I6, '   STEP ', I8)
    ! --------------------------------------------------------------------------
    !
    ! -- write lake stage
    if (ihedfl /= 0 .and. this%iprhed /= 0) then
      write (iout, 2000) 'LAKE (', trim(this%name), ') STAGE', kper, kstp
      iloc = 1
      line = ''
      if (this%inamedbound==1) then
        call UWWORD(line, iloc, 16, 1, 'lake', n, q, left=.TRUE.)
      end if
      call UWWORD(line, iloc, 6, 1, 'lake', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE.)
      ! -- create line separator
      linesep = repeat('-', iloc)
      ! -- write first line
      write(iout,'(1X,A)') linesep(1:iloc)
      write(iout,'(1X,A)') line(1:iloc)
      ! -- create second header line
      iloc = 1
      line = ''
      if (this%inamedbound==1) then
        call UWWORD(line, iloc, 16, 1, 'name', n, q, left=.TRUE.)
      end if
      call UWWORD(line, iloc, 6, 1, 'no.', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'stage', n, q, CENTER=.TRUE.)
      ! -- write second line
      write(iout,'(1X,A)') line(1:iloc)
      write(iout,'(1X,A)') linesep(1:iloc)
      ! -- write data
      do n = 1, this%nlakes
        iloc = 1
        line = ''
        if (this%inamedbound==1) then
          call UWWORD(line, iloc, 16, 1, this%lakename(n), n, q, left=.TRUE.)
        end if
        call UWWORD(line, iloc, 6, 2, text, n, q)
        call UWWORD(line, iloc, 11, 3, text, n, this%xnewpak(n))
        write(iout, '(1X,A)') line(1:iloc)
      end do
    end if
    !
    ! -- write lake rates
    if (ibudfl /= 0 .and. this%iprflow /= 0) then
      write (iout, 2000) 'LAKE (', trim(this%name), ') FLOWS', kper, kstp
      iloc = 1
      line = ''
      if (this%inamedbound==1) then
        call UWWORD(line, iloc, 16, 1, 'lake', n, q, left=.TRUE.)
      end if
      call UWWORD(line, iloc, 6, 1, 'lake', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE., SEP=' ')
      if (this%noutlets > 0) then
        call UWWORD(line, iloc, 11, 1, 'internal', n, q, CENTER=.TRUE., SEP=' ')
      end if
      call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE., SEP=' ')
      if (this%imover == 1) then
        call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE., SEP=' ')
      end if
      call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE., SEP=' ')
      if (this%noutlets > 0) then
        call UWWORD(line, iloc, 11, 1, 'external', n, q, CENTER=.TRUE., SEP=' ')
        call UWWORD(line, iloc, 11, 1, 'internal', n, q, CENTER=.TRUE., SEP=' ')
        if (this%imover == 1) then
          call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE., SEP=' ')
        end if
      end if
      call UWWORD(line, iloc, 11, 1, 'constant', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'percent', n, q, CENTER=.TRUE.)
      ! -- create line separator
      linesep = repeat('-', iloc)
      ! -- write first line
      write(iout,'(1X,A)') linesep(1:iloc)
      write(iout,'(1X,A)') line(1:iloc)
      ! -- create second header line
      iloc = 1
      line = ''
      if (this%inamedbound==1) then
        call UWWORD(line, iloc, 16, 1, 'name', n, q, left=.TRUE.)
      end if
      call UWWORD(line, iloc, 6, 1, 'no.', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'inflow', n, q, CENTER=.TRUE., SEP=' ')
      if (this%noutlets > 0) then
        call UWWORD(line, iloc, 11, 1, 'inflow', n, q, CENTER=.TRUE., SEP=' ')
      end if
      call UWWORD(line, iloc, 11, 1, 'runoff', n, q, CENTER=.TRUE., SEP=' ')
      if (this%imover == 1) then
        call UWWORD(line, iloc, 11, 1, 'from mvr', n, q, CENTER=.TRUE., SEP=' ')
      end if
      call UWWORD(line, iloc, 11, 1, 'rainfall', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'leakage in', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'leakage out', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'evaporation', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'withdrawal', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'storage', n, q, CENTER=.TRUE., SEP=' ')
      if (this%noutlets > 0) then
        call UWWORD(line, iloc, 11, 1, 'outflow', n, q, CENTER=.TRUE., SEP=' ')
        call UWWORD(line, iloc, 11, 1, 'outflow', n, q, CENTER=.TRUE., SEP=' ')
        if (this%imover == 1) then
          call UWWORD(line, iloc, 11, 1, 'to mvr', n, q, CENTER=.TRUE., SEP=' ')
        end if
      end if
      call UWWORD(line, iloc, 11, 1, 'flow', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'in - out', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'difference', n, q, CENTER=.TRUE.)
      ! -- write second line
      write(iout,'(1X,A)') line(1:iloc)
      write(iout,'(1X,A)') linesep(1:iloc)
      !
      ! -- write data
      do n = 1, this%nlakes
        iloc = 1
        line = ''
        if (this%inamedbound==1) then
          call UWWORD(line, iloc, 16, 1, this%lakename(n), n, q, left=.TRUE.)
        end if
        call UWWORD(line, iloc, 6, 2, text, n, q)
        qtin = DZERO
        qtout = DZERO
        qin = this%inflow(n)%value
        call UWWORD(line, iloc, 11, 3, text, n, qin, SEP=' ')
        qinternalin = DZERO
        if (this%noutlets > 0) then
          call this%lak_get_internal_inlet(n, qinternalin)
          call UWWORD(line, iloc, 11, 3, text, n, qinternalin, SEP=' ')
        end if
        qro = this%runoff(n)%value
        call UWWORD(line, iloc, 11, 3, text, n, qro, SEP=' ')
        qfrommover = DZERO
        if (this%imover == 1) then
          qfrommover = this%pakmvrobj%get_qfrommvr(n)
          call UWWORD(line, iloc, 11, 3, text, n, qfrommover, SEP=' ')
        end if
        qrai = this%precip(n)
        call UWWORD(line, iloc, 11, 3, text, n, qrai, SEP=' ')
        ! leakage
        qleakin = DZERO
        qleakout = DZERO
        hlak = this%xnewpak(n)
        do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
          q = this%qleak(j)
          if (q < DZERO) then
            qleakout = qleakout + q
            qtout = qtout + q
          else
            qleakin = qleakin + q
            qtin = qtin + q
          end if
        end do
        call UWWORD(line, iloc, 11, 3, text, n, qleakin, SEP=' ')
        call UWWORD(line, iloc, 11, 3, text, n, qleakout, SEP=' ')
        qevt = this%evap(n)
        call UWWORD(line, iloc, 11, 3, text, n, qevt, SEP=' ')
        qwdw = this%withr(n)
        call UWWORD(line, iloc, 11, 3, text, n, qwdw, SEP=' ')
        ! storage changes
        qsto = DZERO
        qsto = this%qsto(n)
        if (qsto < DZERO) then
          qtout = qtout + qsto
        else
          qtin = qtin + qsto
        end if
        call UWWORD(line, iloc, 11, 3, text, n, qsto, SEP=' ')
        qext = DZERO
        qtomover = DZERO
        qinternalout = DZERO
        if (this%noutlets > 0) then
          ! external outflow
          call this%lak_get_external_outlet(n, qext)
          ! internal outflow
          call this%lak_get_internal_outlet(n, qinternalout)
          ! subtract tomover from external or internal outflow
          if (this%imover == 1) then
            if (qext < DZERO) then
              call this%lak_get_external_mover(n, qtomover)
              qext = qext + qtomover
            else if (qinternalout < DZERO) then
              call this%lak_get_internal_mover(n, qtomover)
              qinternalout = qinternalout + qtomover
            end if
            if (qtomover > DZERO) then
              qtomover = -qtomover
            end if
          end if
          ! write external outflow, internal outflow, and tomover
          call UWWORD(line, iloc, 11, 3, text, n, qext, SEP=' ')
          call UWWORD(line, iloc, 11, 3, text, n, qinternalout, SEP=' ')
          if (this%imover == 1) then
            call UWWORD(line, iloc, 11, 3, text, n, qtomover, SEP=' ')
          end if
        end if
        ! constant flow
        qch = this%chterm(n)
        if (qch < DZERO) then
          qtout = qtout + qch
        else
          qtin = qtin + qch
        end if
        call UWWORD(line, iloc, 11, 3, text, n, qch, SEP=' ')
        ! complete qtin
        qtin = qtin + qin + qinternalin + qro + qfrommover + qrai
        ! complete qtout
        qtout = qtout + qevt + qwdw + qext + qtomover + qinternalout
        ! error
        qerr = qtin + qtout
        ! percent difference
        qavg = DHALF * (qtin - qtout)
        qerrpd = DZERO
        if (qavg /= DZERO) then
          qerrpd = DHUNDRED * qerr / qavg
        end if
        call UWWORD(line, iloc, 11, 3, text, n, qerr, SEP=' ')
        call UWWORD(line, iloc, 11, 3, text, n, qerrpd)
        ! -- write data for lake
        write(iout, '(1X,A)') line(1:iloc)
      end do

    end if
    !
    ! -- Output lake budget
    call this%budget%budget_ot(kstp, kper, iout)
    !
    ! -- return
    return
  end subroutine lak_ot

  subroutine lak_da(this)
    ! **************************************************************************
    ! lak_da -- Deallocate objects
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(LakType) :: this
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: iconn
    ! -- format
    ! --------------------------------------------------------------------------
    !
    ! -- arrays
    deallocate(this%lakename)
    deallocate(this%status)
    deallocate(this%clakbudget)
    call mem_deallocate(this%dbuff)
    deallocate(this%cauxcbc)
    call mem_deallocate(this%qauxcbc)
    call mem_deallocate(this%qleak)
    call mem_deallocate(this%qsto)
    !
    ! -- tables
    do n = 1, this%nlakes
      if (this%ntabrow(n) > 0) then
        call mem_deallocate(this%laketables(n)%tabstage)
        call mem_deallocate(this%laketables(n)%tabvolume)
        call mem_deallocate(this%laketables(n)%tabsarea)
        iconn = this%idxlakeconn(n)
        if (this%ictype(iconn) == 2 .or. this%ictype(iconn) == 3) then
          call mem_deallocate(this%laketables(n)%tabwarea)
        end if
      end if
    end do
    if (this%ntables > 0) then
      deallocate(this%laketables)
    end if
    !
    ! -- Lake objects
    call this%budget%budget_da()
    deallocate(this%budget)
    !
    ! -- outlets
    if (this%noutlets > 0) then
      call mem_deallocate(this%lakein)
      call mem_deallocate(this%lakeout)
      call mem_deallocate(this%iouttype)
      call mem_deallocate(this%outrate)
      call mem_deallocate(this%outinvert)
      call mem_deallocate(this%outwidth)
      call mem_deallocate(this%outrough)
      call mem_deallocate(this%outslope)
      call mem_deallocate(this%simoutrate)
    endif
    !
    ! -- scalars
    call mem_deallocate(this%iprhed)
    call mem_deallocate(this%istageout)
    call mem_deallocate(this%ibudgetout)
    call mem_deallocate(this%isteady)
    call mem_deallocate(this%nlakes)
    call mem_deallocate(this%noutlets)
    call mem_deallocate(this%ntables)
    call mem_deallocate(this%convlength)
    call mem_deallocate(this%convtime)
    call mem_deallocate(this%outdmax)
    call mem_deallocate(this%igwhcopt)
    call mem_deallocate(this%iconvchk)
    call mem_deallocate(this%iconvresidchk)
    call mem_deallocate(this%surfdep)
    call mem_deallocate(this%delh)
    call mem_deallocate(this%pdmax)
    call mem_deallocate(this%check_attr)
    call mem_deallocate(this%bditems)
    call mem_deallocate(this%cbcauxitems)
    !
    call mem_deallocate(this%nlakeconn)
    call mem_deallocate(this%idxlakeconn)
    call mem_deallocate(this%ntabrow)
    call mem_deallocate(this%strt)
    call mem_deallocate(this%laketop)
    call mem_deallocate(this%lakebot)
    call mem_deallocate(this%sareamax)
    call mem_deallocate(this%stage)
    call mem_deallocate(this%rainfall)
    call mem_deallocate(this%evaporation)
    call mem_deallocate(this%runoff)
    call mem_deallocate(this%inflow)
    call mem_deallocate(this%withdrawal)
    call mem_deallocate(this%lauxvar)
    call mem_deallocate(this%avail)
    call mem_deallocate(this%lkgwsink)
    call mem_deallocate(this%ncncvr)
    call mem_deallocate(this%surfin)
    call mem_deallocate(this%surfout)
    call mem_deallocate(this%surfout1)
    call mem_deallocate(this%precip)
    call mem_deallocate(this%precip1)
    call mem_deallocate(this%evap)
    call mem_deallocate(this%evap1)
    call mem_deallocate(this%evapo)
    call mem_deallocate(this%withr)
    call mem_deallocate(this%withr1)
    call mem_deallocate(this%flwin)
    call mem_deallocate(this%flwiter)
    call mem_deallocate(this%flwiter1)
    call mem_deallocate(this%seep)
    call mem_deallocate(this%seep1)
    call mem_deallocate(this%seep0)
    call mem_deallocate(this%stageiter)
    call mem_deallocate(this%chterm)
    !
    ! -- lake boundary and stages
    call mem_deallocate(this%iboundpak)
    call mem_deallocate(this%xnewpak)
    call mem_deallocate(this%xoldpak)
    !
    ! -- lake iteration variables
    call mem_deallocate(this%iseepc)
    call mem_deallocate(this%idhc)
    call mem_deallocate(this%en1)
    call mem_deallocate(this%en2)
    call mem_deallocate(this%r1)
    call mem_deallocate(this%r2)
    call mem_deallocate(this%dh0)
    call mem_deallocate(this%s0)
    !
    ! -- lake connection variables
    call mem_deallocate(this%imap)
    call mem_deallocate(this%cellid)
    call mem_deallocate(this%nodesontop)
    call mem_deallocate(this%ictype)
    call mem_deallocate(this%bedleak)
    call mem_deallocate(this%belev)
    call mem_deallocate(this%telev)
    call mem_deallocate(this%connlength)
    call mem_deallocate(this%connwidth)
    call mem_deallocate(this%sarea)
    call mem_deallocate(this%warea)
    call mem_deallocate(this%satcond)
    call mem_deallocate(this%simcond)
    call mem_deallocate(this%simlakgw)
    !
    ! -- Parent object
    call this%BndType%bnd_da()
    !
    ! -- Return
    return
  end subroutine lak_da


  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(LakType), intent(inout) :: this
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
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'STRESS RATE'
    if(this%inamedbound == 1) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    endif
    !
    ! -- return
    return
  end subroutine define_listlabel


  subroutine lak_set_pointers(this, neq, ibound, xnew, xold, flowja)
! ******************************************************************************
! set_pointers -- Set pointers to model arrays and variables so that a package
!                 has access to these things.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(LakType) :: this
    integer(I4B), pointer :: neq
    integer(I4B), dimension(:), pointer :: ibound
    real(DP), dimension(:), pointer :: xnew
    real(DP), dimension(:), pointer :: xold
    real(DP), dimension(:), pointer :: flowja
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- call base BndType set_pointers
    call this%BndType%set_pointers(neq, ibound, xnew, xold, flowja)
    !
    ! -- Set the LAK pointers
    !
    ! -- set package pointers
    !istart = this%dis%nodes + this%ioffset + 1
    !iend = istart + this%nlakes - 1
    !this%iboundpak => this%ibound(istart:iend)
    !this%xnewpak => this%xnew(istart:iend)
    !
    ! -- initialize xnewpak
    !do n = 1, this%nlakes
    !  this%xnewpak(n) = DEP20
    !end do
    !
    ! -- return
  end subroutine lak_set_pointers

  !
  ! -- Procedures related to observations (type-bound)
  logical function lak_obs_supported(this)
  ! ******************************************************************************
  ! lak_obs_supported
  !   -- Return true because LAK package supports observations.
  !   -- Overrides BndType%bnd_obs_supported()
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
  ! ------------------------------------------------------------------------------
    class(LakType) :: this
    lak_obs_supported = .true.
    return
  end function lak_obs_supported


  subroutine lak_df_obs(this)
  ! ******************************************************************************
  ! lak_df_obs (implements bnd_df_obs)
  !   -- Store observation type supported by LAK package.
  !   -- Overrides BndType%bnd_df_obs
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    ! -- dummy
    class(LakType) :: this
    ! -- local
    integer(I4B) :: indx
  ! ------------------------------------------------------------------------------
    !
    ! -- Store obs type and assign procedure pointer
    !    for stage observation type.
    call this%obs%StoreObsType('stage', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for ext-inflow observation type.
    call this%obs%StoreObsType('ext-inflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for outlet-inflow observation type.
    call this%obs%StoreObsType('outlet-inflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for inflow observation type.
    call this%obs%StoreObsType('inflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for from-mvr observation type.
    call this%obs%StoreObsType('from-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for rainfall observation type.
    call this%obs%StoreObsType('rainfall', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for runoff observation type.
    call this%obs%StoreObsType('runoff', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for lak observation type.
    call this%obs%StoreObsType('lak', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for evaporation observation type.
    call this%obs%StoreObsType('evaporation', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for withdrawal observation type.
    call this%obs%StoreObsType('withdrawal', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for ext-outflow observation type.
    call this%obs%StoreObsType('ext-outflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for storage observation type.
    call this%obs%StoreObsType('storage', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for constant observation type.
    call this%obs%StoreObsType('constant', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for outlet observation type.
    call this%obs%StoreObsType('outlet', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for volume observation type.
    call this%obs%StoreObsType('volume', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for surface-area observation type.
    call this%obs%StoreObsType('surface-area', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for wetted-area observation type.
    call this%obs%StoreObsType('wetted-area', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for conductance observation type.
    call this%obs%StoreObsType('conductance', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => lak_process_obsID
    !
    return
  end subroutine lak_df_obs


  subroutine lak_bd_obs(this)
    ! **************************************************************************
    ! lak_bd_obs
    !   -- Calculate observations this time step and call
    !      ObsType%SaveOneSimval for each LakType observation.
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    ! -- dummy
    class(LakType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, igwfnode, j, jj, n, nn
    real(DP) :: hgwf, hlak, v, v2
    character(len=100) :: errmsg
    type(ObserveType), pointer :: obsrv => null()
    !---------------------------------------------------------------------------
    !
    ! Write simulated values for all LAK observations
    if (this%obs%npakobs > 0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        nn = size(obsrv%indxbnds)
        do j = 1, nn
          v = DNODATA
          jj = obsrv%indxbnds(j)
          select case (obsrv%ObsTypeId)
            case ('STAGE')
              if (this%iboundpak(jj) /= 0) then
                v = this%xnewpak(jj)
              end if
            case ('EXT-INFLOW')
              if (this%iboundpak(jj) /= 0) then
                call this%lak_calculate_inflow(jj, v)
              end if
            case ('OUTLET-INFLOW')
              if (this%iboundpak(jj) /= 0) then
                call this%lak_calculate_outlet_inflow(jj, v)
              end if
            case ('INFLOW')
              if (this%iboundpak(jj) /= 0) then
                call this%lak_calculate_inflow(jj, v)
                call this%lak_calculate_outlet_inflow(jj, v2)
                v = v + v2
              end if
            case ('FROM-MVR')
              if (this%iboundpak(jj) /= 0) then
                if (this%imover == 1) then
                  v = this%pakmvrobj%get_qfrommvr(jj)
                end if
              end if
            case ('RAINFALL')
              if (this%iboundpak(jj) /= 0) then
                v = this%precip(jj)
              end if
            case ('RUNOFF')
              if (this%iboundpak(jj) /= 0) then
                v = this%runoff(jj)%value
              end if
            case ('LAK')
              n = this%imap(jj)
              if (this%iboundpak(n) /= 0) then
                igwfnode = this%cellid(jj)
                hgwf = this%xnew(igwfnode)
                if (this%hcof(jj) /= DZERO) then
                  v = -(this%hcof(jj) * (this%xnewpak(n) - hgwf))
                else
                  v = -this%rhs(jj)
                end if
              end if
            case ('EVAPORATION')
              if (this%iboundpak(jj) /= 0) then
                v = this%evap(jj)
              end if
            case ('WITHDRAWAL')
              if (this%iboundpak(jj) /= 0) then
                v = this%withr(jj)
              end if
            case ('EXT-OUTFLOW')
              n = this%lakein(jj)
              if (this%iboundpak(n) /= 0) then
                if (this%lakeout(jj) == 0) then
                  v = this%simoutrate(jj)
                  if (v < DZERO) then
                    if (this%imover == 1) then
                      v = v + this%pakmvrobj%get_qtomvr(jj)
                    end if
                  end if
                end if
              end if
            case ('TO-MVR')
              n = this%lakein(jj)
              if (this%iboundpak(n) /= 0) then
                if (this%imover == 1) then
                  v = this%pakmvrobj%get_qtomvr(jj)
                  if (v > DZERO) then
                    v = -v
                  end if
                end if
              end if
            case ('STORAGE')
              if (this%iboundpak(jj) /= 0) then
                v = this%qsto(jj)
              end if
            case ('CONSTANT')
              if (this%iboundpak(jj) /= 0) then
                v = this%chterm(jj)
              end if
            case ('OUTLET')
              n = this%lakein(jj)
              if (this%iboundpak(jj) /= 0) then
                v = this%simoutrate(jj)
                !if (this%imover == 1) then
                !  v = v + this%pakmvrobj%get_qtomvr(jj)
                !end if
              end if
            case ('VOLUME')
              if (this%iboundpak(jj) /= 0) then
                call this%lak_calculate_vol(jj, this%xnewpak(jj), v)
              end if
            case ('SURFACE-AREA')
              if (this%iboundpak(jj) /= 0) then
                hlak = this%xnewpak(jj)
                call this%lak_calculate_sarea(jj, hlak, v)
              end if
            case ('WETTED-AREA')
              n = this%imap(jj)
              if (this%iboundpak(n) /= 0) then
                hlak = this%xnewpak(n)
                nn = size(obsrv%indxbnds)
                igwfnode = this%cellid(jj)
                hgwf = this%xnew(igwfnode)
                call this%lak_calculate_conn_warea(n, jj, hlak, hgwf, v)
              end if
            case ('CONDUCTANCE')
              n = this%imap(jj)
              if (this%iboundpak(n) /= 0) then
                hlak = this%xnewpak(n)
                nn = size(obsrv%indxbnds)
                igwfnode = this%cellid(jj)
                hgwf = this%xnew(igwfnode)
                call this%lak_calculate_conn_conductance(n, jj, hlak, hgwf, v)
              end if
            case default
              errmsg = 'Error: Unrecognized observation type: ' // &
                        trim(obsrv%ObsTypeId)
              call store_error(errmsg)
              call ustop()
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      end do
    end if
    !
    return
  end subroutine lak_bd_obs


  subroutine lak_rp_obs(this)
    ! -- dummy
    class(LakType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, j, n, nn1, nn2
    integer(I4B) :: jj
    character(len=200) :: ermsg
    character(len=LENBOUNDNAME) :: bname
    logical :: jfound
    class(ObserveType),   pointer :: obsrv => null()
    ! --------------------------------------------------------------------------
    ! -- formats
10  format('Error: Boundary "',a,'" for observation "',a, &
           '" is invalid in package "',a,'"')
30  format('Error: Boundary name not provided for observation "',a, &
           '" in package "',a,'"')
60  format('Error: Invalid node number in OBS input: ',i0)
    !
    do i = 1, this%obs%npakobs
      obsrv => this%obs%pakobs(i)%obsrv
      !
      ! -- indxbnds needs to be deallocated and reallocated (using
      !    ExpandArray) each stress period because list of boundaries
      !    can change each stress period.
      if (allocated(obsrv%indxbnds)) then
        deallocate(obsrv%indxbnds)
      end if
      !
      ! -- get node number 1
      nn1 = obsrv%NodeNumber
      if (nn1 == NAMEDBOUNDFLAG) then
        bname = obsrv%FeatureName
        if (bname /= '') then
          ! -- Observation lake is based on a boundary name.
          !    Iterate through all lakes to identify and store
          !    corresponding index in bound array.
          jfound = .false.
          if (obsrv%ObsTypeId=='LAK' .or.   &
               obsrv%ObsTypeId=='CONDUCTANCE' .or.       &
               obsrv%ObsTypeId=='WETTED-AREA') then
            do j = 1, this%nlakes
              do jj = this%idxlakeconn(j), this%idxlakeconn(j+1) - 1
                if (this%boundname(jj) == bname) then
                  jfound = .true.
                  call ExpandArray(obsrv%indxbnds)
                  n = size(obsrv%indxbnds)
                  obsrv%indxbnds(n) = jj
                end if
              end do
            end do
          else if (obsrv%ObsTypeId=='EXT-OUTFLOW' .or.   &
                   obsrv%ObsTypeId=='TO-MVR' .or. &
                   obsrv%ObsTypeId=='OUTLET') then
            do j = 1, this%noutlets
              jj = this%lakein(j)
              if (this%lakename(jj) == bname) then
                jfound = .true.
                call ExpandArray(obsrv%indxbnds)
                n = size(obsrv%indxbnds)
                obsrv%indxbnds(n) = j
              end if
            end do
          else
            do j = 1, this%nlakes
              if (this%lakename(j) == bname) then
                jfound = .true.
                call ExpandArray(obsrv%indxbnds)
                n = size(obsrv%indxbnds)
                obsrv%indxbnds(n) = j
              end if
            end do
          end if
          if (.not. jfound) then
            write(ermsg,10)trim(bname), trim(obsrv%Name), trim(this%name)
            call store_error(ermsg)
          end if
        end if
      else
        call ExpandArray(obsrv%indxbnds)
        n = size(obsrv%indxbnds)
        if (n == 1) then
          if (obsrv%ObsTypeId=='LAK' .or.   &
               obsrv%ObsTypeId=='CONDUCTANCE' .or.       &
               obsrv%ObsTypeId=='WETTED-AREA') then
            nn2 = obsrv%NodeNumber2
            j = this%idxlakeconn(nn1) + nn2 - 1
            obsrv%indxbnds(1) = j
          else
            obsrv%indxbnds(1) = nn1
          end if
        else
          ermsg = 'Programming error in lak_rp_obs'
          call store_error(ermsg)
        endif
      end if
      !
      ! -- catch non-cumulative observation assigned to observation defined
      !    by a boundname that is assigned to more than one element
      if (obsrv%ObsTypeId == 'STAGE') then
        n = size(obsrv%indxbnds)
        if (n > 1) then
          write (ermsg, '(4x,a,4(1x,a))') &
            'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
            'for observation', trim(adjustl(obsrv%Name)), &
            ' must be assigned to a lake with a unique boundname.'
          call store_error(ermsg)
        end if
      end if
      !
      ! -- check that index values are valid
      if (obsrv%ObsTypeId=='TO-MVR' .or. &
          obsrv%ObsTypeId=='EXT-OUTFLOW' .or. &
          obsrv%ObsTypeId=='OUTLET') then
        do j = 1, size(obsrv%indxbnds)
          nn1 =  obsrv%indxbnds(j)
          if (nn1 < 1 .or. nn1 > this%noutlets) then
            write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              ' outlet must be > 0 and <=', this%noutlets, &
              '(specified value is ', nn1, ')'
            call store_error(ermsg)
          end if
        end do
      else if (obsrv%ObsTypeId=='LAK' .or.   &
               obsrv%ObsTypeId=='CONDUCTANCE' .or.       &
               obsrv%ObsTypeId=='WETTED-AREA') then
        do j = 1, size(obsrv%indxbnds)
          nn1 =  obsrv%indxbnds(j)
          if (nn1 < 1 .or. nn1 > this%maxbound) then
            write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              ' lake connection number must be > 0 and <=', this%maxbound, &
              '(specified value is ', nn1, ')'
            call store_error(ermsg)
          end if
        end do
      else
        do j = 1, size(obsrv%indxbnds)
          nn1 =  obsrv%indxbnds(j)
          if (nn1 < 1 .or. nn1 > this%nlakes) then
            write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              ' lake must be > 0 and <=', this%nlakes, &
              '(specified value is ', nn1, ')'
            call store_error(ermsg)
          end if
        end do
      end if
    end do
    if (count_errors() > 0) call ustop()
    !
    return
  end subroutine lak_rp_obs


  !
  ! -- Procedures related to observations (NOT type-bound)
  subroutine lak_process_obsID(obsrv, dis, inunitobs, iout)
    ! -- This procedure is pointed to by ObsDataType%ProcesssIdPtr. It processes
    !    the ID string of an observation definition for LAK package observations.
    ! -- dummy
    type(ObserveType),      intent(inout) :: obsrv
    class(DisBaseType), intent(in)    :: dis
    integer(I4B),            intent(in)    :: inunitobs
    integer(I4B),            intent(in)    :: iout
    ! -- local
    integer(I4B) :: nn1, nn2
    integer(I4B) :: icol, istart, istop
    character(len=LINELENGTH) :: strng
    character(len=LENBOUNDNAME) :: bndname
    ! formats
    !
    strng = obsrv%IDstring
    ! -- Extract lake number from strng and store it.
    !    If 1st item is not an integer(I4B), it should be a
    !    lake name--deal with it.
    icol = 1
    ! -- get lake number or boundary name
    call extract_idnum_or_bndname(strng, icol, istart, istop, nn1, bndname)
    if (nn1 == NAMEDBOUNDFLAG) then
      obsrv%FeatureName = bndname
    else
      if (obsrv%ObsTypeId=='LAK' .or. obsrv%ObsTypeId=='CONDUCTANCE' .or. &
          obsrv%ObsTypeId=='WETTED-AREA') then
        call extract_idnum_or_bndname(strng, icol, istart, istop, nn2, bndname)
        if (nn2 == NAMEDBOUNDFLAG) then
          obsrv%FeatureName = bndname
          ! -- reset nn1
          nn1 = nn2
        else
          obsrv%NodeNumber2 = nn2
        end if
        !! -- store connection number (NodeNumber2)
        !obsrv%NodeNumber2 = nn2
      endif
    endif
    ! -- store lake number (NodeNumber)
    obsrv%NodeNumber = nn1
    !
    return
  end subroutine lak_process_obsID

  !
  ! -- private LAK methods
  !
  subroutine lak_accumulate_chterm(this, ilak, rrate, chratin, chratout)
    ! **************************************************************************
    ! lak_accumulate_chterm -- Accumulate constant head terms for budget.
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    ! -- dummy
    class(LakType) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(in) :: rrate
    real(DP), intent(inout) :: chratin
    real(DP), intent(inout) :: chratout
    ! -- locals
    real(DP) :: q
    ! format
    ! code
    if (this%iboundpak(ilak) < 0) then
      q = -rrate
      this%chterm(ilak) = this%chterm(ilak) + q
      !
      ! -- See if flow is into lake or out of lake.
      if (q < DZERO) then
        !
        ! -- Flow is out of lake subtract rate from ratout.
        chratout = chratout - q
      else
        !
        ! -- Flow is into lake; add rate to ratin.
        chratin = chratin + q
      end if
    end if
    ! -- return
    return
  end subroutine lak_accumulate_chterm


  subroutine lak_cfupdate(this)
  ! ******************************************************************************
  ! lak_cfupdate -- Update LAK satcond and package rhs and hcof
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      class(LakType), intent(inout) :: this
      integer(I4B) :: j, n, node
      real(DP) :: hlak, head, clak, blak
  ! ------------------------------------------------------------------------------
  !
  ! -- Return if no lak lakes
      if(this%nbound.eq.0) return
  !
  ! -- Calculate hcof and rhs for each lak entry
      do n = 1, this%nlakes
        hlak = this%xnewpak(n)
        do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
          node = this%cellid(j)
          head = this%xnew(node)

          this%hcof(j) = DZERO
          this%rhs(j) = DZERO
          !
          ! -- set bound, hcof, and rhs components
          call this%lak_calculate_conn_conductance(n, j, hlak, head, clak)
          this%simcond(j) = clak

          this%bound(2,j) = clak

          blak = this%bound(3,j)

          this%hcof(j) = -clak
          !
          ! -- fill rhs
          if (hlak < blak) then
            this%rhs(j) = -clak * blak
          else
            this%rhs(j) = -clak * hlak
          end if
        end do
      end do
      !
      ! -- Return
      return
  end subroutine lak_cfupdate

  subroutine lak_solve(this, update)
  ! **************************************************************************
  ! lak_solve -- Solve for lake stage
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
    use TdisModule,only:delt
    logical, intent(in), optional :: update
    ! -- dummy
    class(LakType), intent(inout) :: this
    ! -- local
    logical :: lupdate
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: n
    integer(I4B) :: iicnvg
    integer(I4B) :: iter
    integer(I4B) :: maxiter
    integer(I4B) :: ncnv
    integer(I4B) :: idry
    integer(I4B) :: igwfnode
    integer(I4B) :: ibflg
    integer(I4B) :: idhp
    real(DP) :: hlak
    real(DP) :: hlak0
    real(DP) :: v0
    real(DP) :: v1
    real(DP) :: head
    real(DP) :: ra
    real(DP) :: ro
    real(DP) :: qinf
    real(DP) :: ex
    real(DP) :: ev
    real(DP) :: outinf
    real(DP) :: s
    real(DP) :: qlakgw
    real(DP) :: qlakgw1
    real(DP) :: clak
    real(DP) :: clak1
    real(DP) :: avail
    real(DP) :: resid
    real(DP) :: resid1
    real(DP) :: residb
    real(DP) :: wr
    real(DP) :: derv
    real(DP) :: dh
    real(DP) :: adh
    real(DP) :: adh0
    real(DP) :: delh
    real(DP) :: ts
! --------------------------------------------------------------------------
    !
    ! -- set lupdate
    if (present(update)) then
      lupdate = update
    else
      lupdate = .true.
    end if
    !
    ! -- initialize
    avail = DZERO
    delh = this%delh
    !
    ! -- initialize
    do n = 1, this%nlakes
      this%ncncvr(n) = 0
      this%surfin(n) = DZERO
      this%surfout(n) = DZERO
      this%surfout1(n) = DZERO
      if (this%xnewpak(n) < this%lakebot(n)) then
        this%xnewpak(n) = this%lakebot(n)
      end if
      if (this%isteady /= 0) then
        this%xoldpak(n) = this%xnewpak(n)
      end if
      ! -- lake iteration items
      this%iseepc(n) = 0
      this%idhc(n) = 0
      this%en1(n) = this%lakebot(n)
      call this%lak_calculate_residual(n, this%en1(n), this%r1(n))
      this%en2(n) = this%laketop(n)
      call this%lak_calculate_residual(n, this%en2(n), this%r2(n))
    end do
    do n = 1, this%noutlets
      this%simoutrate(n) = DZERO
    end do
    !
    ! -- sum up inflows from mover inflows
    do n = 1, this%nlakes
      call this%lak_calculate_outlet_inflow(n, this%surfin(n))
    end do
    !
    ! -- sum up overland runoff, inflows, and external flows into lake
    !    (includes lake volume)
    do n = 1, this%nlakes
      hlak0 = this%xoldpak(n)
      call this%lak_calculate_runoff(n, ro)
      call this%lak_calculate_inflow(n, qinf)
      call this%lak_calculate_external(n, ex)
      ! --
      call this%lak_calculate_vol(n, hlak0, v0)
      this%flwin(n) = this%surfin(n) + ro + qinf + ex + v0 / delt
    end do
    !
    ! -- sum up inflows from upstream outlets
    do n = 1, this%nlakes
      call this%lak_calculate_outlet_inflow(n, outinf)
      this%flwin(n) = this%flwin(n) + outinf
    end do

    iicnvg = 0
    maxiter = 150

    ! -- outer loop
    converge: do iter = 1, maxiter
      ncnv = 0
      do n = 1, this%nlakes
        if (this%ncncvr(n) == 0) ncnv = 1
      end do
      if (iter == maxiter) ncnv = 0
      if (ncnv == 0) iicnvg = 1

      ! -- initialize variables
      do n = 1, this%nlakes
        this%evap(n) = DZERO
        this%precip(n) = DZERO
        this%precip1(n) = DZERO
        this%seep(n) = DZERO
        this%seep1(n) = DZERO
        this%evap(n) = DZERO
        this%evap1(n) = DZERO
        this%evapo(n) = DZERO
        this%withr(n) = DZERO
        this%withr1(n) = DZERO
        this%flwiter(n) = this%flwin(n)
        this%flwiter1(n) = this%flwin(n)
        if (this%isteady /= 0) then
          this%flwiter(n) = DEP20 !1.D+10
          this%flwiter1(n) = DEP20 !1.D+10
        end if
      end do

      estseep: do i = 1, 2
        lakseep: do n = 1, this%nlakes
          ! -- skip inactive lakes
          if (this%iboundpak(n) == 0) then
            cycle lakseep
          end if
          ! - set xoldpak to xnewpak if steady-state
          if (this%isteady /= 0) then
            this%xoldpak(n) = this%xnewpak(n)
          end if
          hlak = this%xnewpak(n)
          calcconnseep: do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
            igwfnode = this%cellid(j)
            head = this%xnew(igwfnode)
            if (this%ncncvr(n) /= 2) then
              if (this%ibound(igwfnode) > 0) then
                call this%lak_estimate_conn_exchange(i, n, j, idry, hlak, head, qlakgw, clak, this%flwiter(n))
                call this%lak_estimate_conn_exchange(i, n, j, idry, hlak+delh, head, qlakgw1, clak1, this%flwiter1(n))
                !write(1051,'(2(i10),4(g15.7))') j, idry, clak, hlak, head, qlakgw
                !
                ! -- add to gwf matrix
                if (ncnv == 0 .and. i == 2) then
                  if (j == this%maxbound) then
                    this%ncncvr(n) = 2
                  end if
                  if (idry /= 1) then
                    if (head >= this%belev(j)) then
                      s = max(hlak, this%belev(j))
                      this%hcof(j) = -clak
                      this%rhs(j) = -clak * s
                    else
                      this%hcof(j) = DZERO
                      this%rhs(j) = qlakgw
                    end if
                  else
                    this%hcof(j) = DZERO
                    this%rhs(j) = qlakgw
                  end if
                end if
                if (i == 2) then
                  this%seep(n) = this%seep(n) + qlakgw
                  this%seep1(n) = this%seep1(n) + qlakgw1
                end if
              end if
            end if

          end do calcconnseep
        end do lakseep
      end do estseep

      laklevel: do n = 1, this%nlakes
        ibflg = 0
        hlak = this%xnewpak(n)
        if (iter < maxiter) then
          this%stageiter(n) = this%xnewpak(n)
        end if
        call this%lak_calculate_rainfall(n, hlak, ra)
        this%precip(n) = ra
        this%flwiter(n) = this%flwiter(n) + ra
        call this%lak_calculate_rainfall(n, hlak+delh, ra)
        this%precip1(n) = ra
        this%flwiter1(n) = this%flwiter1(n) + ra
        !
        ! -- limit withdrawals to lake inflows and lake storage
        call this%lak_calculate_withdrawal(n, this%flwiter(n), wr)
        this%withr = wr
        call this%lak_calculate_withdrawal(n, this%flwiter1(n), wr)
        this%withr1 = wr
        !
        ! -- limit evaporation to lake inflows and lake storage
        call this%lak_calculate_evaporation(n, hlak, this%flwiter(n), ev)
        this%evap(n) = ev
        call this%lak_calculate_evaporation(n, hlak+delh, this%flwiter1(n), ev)
        this%evap1(n) = ev
        !
        ! -- no outlet flow if evaporation consumes all water
        call this%lak_calculate_outlet_outflow(n, hlak+delh,                   &
                                               this%flwiter1(n),               &
                                               this%surfout1(n))
        call this%lak_calculate_outlet_outflow(n, hlak, this%flwiter(n),       &
                                               this%surfout(n))
        !
        ! -- update the surface inflow values
        call this%lak_calculate_outlet_inflow(n, this%surfin(n))
        !
        !
        if (ncnv == 1) then
          if (this%iboundpak(n) > 0 .and. lupdate .eqv. .true.) then
            !
            ! -- recalculate flwin
            hlak0 = this%xoldpak(n)
            call this%lak_calculate_vol(n, hlak0, v0)
            call this%lak_calculate_runoff(n, ro)
            call this%lak_calculate_inflow(n, qinf)
            call this%lak_calculate_external(n, ex)
            this%flwin(n) = this%surfin(n) + ro + qinf + ex + v0 / delt
            !
            ! -- compute new lake stage using Newton's method
            resid = this%precip(n) + this%evap(n) + this%withr(n) + ro +       &
                    qinf + ex + this%surfin(n) +                               &
                    this%surfout(n) + this%seep(n)
            resid1 = this%precip1(n) + this%evap1(n) + this%withr1(n) + ro +   &
                     qinf + ex + this%surfin(n) +                              &
                     this%surfout1(n) + this%seep1(n)

            !call this%lak_calculate_residual(n, this%xnewpak(n), residb)
            !
            ! -- add storage changes for transient stress periods
            hlak = this%xnewpak(n)
            if (this%isteady /= 1) then
              call this%lak_calculate_vol(n, hlak, v1)
              resid = resid + (v0 - v1) / delt
              call this%lak_calculate_vol(n, hlak+delh, v1)
              resid1 = resid1 + (v0 - v1) / delt
            !else
            !  call this%lak_calculate_vol(n, hlak, v1)
            !  resid = resid - v1 / delt
            !  call this%lak_calculate_vol(n, hlak+delh, v1)
            !  resid1 = resid1 - v1 / delt
            end if

            !
            ! -- determine the derivative and the stage change
            if (ABS(resid1-resid) > DZERO) then
              derv = (resid1 - resid) / delh
              dh = DZERO
              if (ABS(derv) > DPREC) then
                dh = resid / derv
              end if
              !write(*,'(i1,3(1x,g15.7))') 0, resid, resid1, dh
            else
              if (resid < DZERO) then
                resid = DZERO
              end if
              call this%lak_vol2stage(n, resid, dh)
              dh = hlak - dh
              this%ncncvr(n) = 1
              !write(*,'(i1,3(1x,g15.7))') 1, resid, resid1, dh
            end if
            !
            ! -- determine if the updated stage is outside the endpoints
            ts = hlak-dh
            if (iter == 1) this%dh0(n) = dh
            adh = ABS(dh)
            adh0 = ABS(this%dh0(n))
            if ((ts >= this%en2(n)) .or. (ts <= this%en1(n))) then
              ! -- use bisection if dh is increasing or updated stage is below the
              !    bottom of the lake
              if ((adh > adh0) .or. (ts-this%lakebot(n)) < DPREC) then
                ibflg = 1
                ts = DHALF * (this%en1(n) + this%en2(n))
                call this%lak_calculate_residual(n, ts, residb)
                dh = hlak - ts
              end if
            end if
            !
            ! -- set seep0 on the first lake iteration
            if (iter == 1) then
              this%seep0(n) = this%seep(n)
            end if
            !
            ! -- check for slow convergence
            if (this%seep(n)*this%seep0(n) < DPREC) then
              this%iseepc(n) = this%iseepc(n) + 1
            else
              this%iseepc(n) = 0
            end if
            ! -- determine of convergence is slow and oscillating
            idhp = 0
            if (dh*this%dh0(n) < DPREC) idhp = 1
            ! -- determine if stage change is increasing
            adh = ABS(dh)
            if (adh > adh0) idhp = 1
            ! -- increment idhc convergence flag
            if (idhp == 1) then
              this%idhc(n) = this%idhc(n) + 1
            end if
            !
            ! -- switch to bisection when the Newton-Raphson method oscillates
            !    or when convergence is slow
            if (ibflg == 1) then
              if (this%iseepc(n) > 7 .or. this%idhc(n) > 12) then
                ibflg = 1
                ts = DHALF * (this%en1(n) + this%en2(n))
                call this%lak_calculate_residual(n, ts, residb)
                dh = hlak - ts
              end if
            end if
            if (ibflg == 1) then
              !write(*,*) 'using bisection'
              ! -- change end points
              ! -- root is between r1 and residb
              if (this%r1(n)*residb < DZERO) then
                this%en2(n) = ts
                this%r2(n) = residb
              ! -- root is between fp and f2
              else
                this%en1(n) = ts
                this%r1(n) = residb
              end if
            end if
          else
            dh = DZERO
          end if
          !
          ! -- update lake stage
          hlak = hlak - dh
          if (hlak < this%lakebot(n)) then
            hlak = this%lakebot(n)
          end if
          if (ABS(dh) < delh) then
            this%ncncvr(n) = 1
          end if
          this%xnewpak(n) = hlak
          !
          !write(*,'(4x,2(i4.4,1x),2(g15.7,1x))') n, iter, this%seep0(n), this%seep(n)
          !
          ! -- save iterates for lake
          this%seep0(n) = this%seep(n)
          this%dh0(n) = dh
        end if
      end do laklevel

      if (iicnvg == 1) exit converge

    end do converge
    !
    ! -- Mover terms: store outflow after diversion loss
    !    as qformvr and reduce outflow (qd)
    !    by how much was actually sent to the mover
    if (this%imover == 1) then
      do n = 1, this%noutlets
        call this%pakmvrobj%accumulate_qformvr(n, -this%simoutrate(n))
      end do
    end if
    !
    ! -- return
    return
  end subroutine lak_solve


  subroutine lak_calculate_available(this, n, hlak, avail, &
                                     ra, ro, qinf, ex, headp)
    ! **************************************************************************
    ! lak_calculate_available -- Calculate the available volumetric rate for
    !                            a lake given a passed stage
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    use TdisModule,only:delt
    ! -- dummy
    class(LakType), intent(inout) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: hlak
    real(DP), intent(inout) :: avail
    real(DP), intent(inout)  :: ra
    real(DP), intent(inout)  :: ro
    real(DP), intent(inout)  :: qinf
    real(DP), intent(inout)  :: ex
    real(DP), intent(in), optional :: headp
    ! -- local
    integer(I4B) :: j
    integer(I4B) :: idry
    integer(I4B) :: igwfnode
    real(DP) :: hp
    real(DP) :: head
    real(DP) :: qlakgw
    real(DP) :: clak
    real(DP) :: v0
    ! code
    !
    ! -- set hp
    if (present(headp)) then
      hp = headp
    else
      hp = DZERO
    end if
    !
    ! -- initialize
    avail = DZERO
    !
    ! -- calculate the aquifer sources to the lake
    do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
      igwfnode = this%cellid(j)
      if (this%ibound(igwfnode) == 0) cycle
      head = this%xnew(igwfnode) + hp
      call this%lak_estimate_conn_exchange(1, n, j, idry, hlak, head, qlakgw, clak, avail)
    end do
    !
    ! -- add rainfall
    call this%lak_calculate_rainfall(n, hlak, ra)
    avail = avail + ra
    !
    ! -- calculate runoff
    call this%lak_calculate_runoff(n, ro)
    avail = avail + ro
    !
    ! -- calculate inflow
    call this%lak_calculate_inflow(n, qinf)
    avail = avail + qinf
    !
    ! -- calculate external flow terms
    call this%lak_calculate_external(n, ex)
    avail = avail + ex
    !
    ! -- calculate volume available in storage
    call this%lak_calculate_vol(n, this%xoldpak(n), v0)
    avail = avail + v0 / delt
    !
    ! -- return
    return
  end subroutine lak_calculate_available


  subroutine lak_calculate_residual(this, n, hlak, resid, headp)
    ! **************************************************************************
    ! lak_calculate_residual -- Calculate the residual for a lake given a
    !                           passed stage
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    use TdisModule,only:delt
    ! -- dummy
    class(LakType), intent(inout) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: hlak
    real(DP), intent(inout) :: resid
    real(DP), intent(in), optional :: headp
    ! -- local
    integer(I4B) :: j
    integer(I4B) :: idry
    integer(I4B) :: igwfnode
    real(DP) :: hp
    real(DP) :: avail
    real(DP) :: head
    real(DP) :: ra
    real(DP) :: ro
    real(DP) :: qinf
    real(DP) :: ex
    real(DP) :: ev
    real(DP) :: wr
    real(DP) :: sout
    real(DP) :: sin
    real(DP) :: qlakgw
    real(DP) :: clak
    real(DP) :: seep
    real(DP) :: hlak0
    real(DP) :: v0
    real(DP) :: v1
    !
    ! -- code
    !
    ! -- set hp
    if (present(headp)) then
      hp = headp
    else
      hp = DZERO
    end if
    !
    ! -- initialize
    resid = DZERO
    avail = DZERO
    seep = DZERO
    !
    ! -- calculate the available water
    call this%lak_calculate_available(n, hlak, avail, &
                                      ra, ro, qinf, ex, hp)
    !!
    !! -- calculate the aquifer sources to the lake
    !do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
    !  igwfnode = this%cellid(j)
    !  head = this%xnew(igwfnode) + hp
    !  call this%lak_estimate_conn_exchange(1, n, j, idry, hlak, head, qlakgw, clak, avail)
    !end do
    !!
    !! -- add rainfall
    !call this%lak_calculate_rainfall(n, hlak, ra)
    !avail = avail + ra
    !!
    !! -- calculate runoff
    !call this%lak_calculate_runoff(n, ro)
    !avail = avail + ro
    !!
    !! -- calculate inflow
    !call this%lak_calculate_inflow(n, qinf)
    !avail = avail + qinf
    !!
    !! -- calculate external flow terms
    !call this%lak_calculate_external(n, ex)
    !avail = avail + ex
    !
    ! -- calculate groundwater seepage
    do j = this%idxlakeconn(n), this%idxlakeconn(n+1)-1
      igwfnode = this%cellid(j)
      if (this%ibound(igwfnode) == 0) cycle
      head = this%xnew(igwfnode) + hp
      call this%lak_estimate_conn_exchange(2, n, j, idry, hlak, head, qlakgw, clak, avail)
      seep = seep + qlakgw
    end do
    !
    ! -- limit withdrawals to lake inflows and lake storage
    call this%lak_calculate_withdrawal(n, avail, wr)
    !
    ! -- limit evaporation to lake inflows and lake storage
    call this%lak_calculate_evaporation(n, hlak, avail, ev)
    !
    ! -- no outlet flow if evaporation consumes all water
    call this%lak_calculate_outlet_outflow(n, hlak, avail, sout)
    !
    ! -- update the surface inflow values
    call this%lak_calculate_outlet_inflow(n, sin)
    !
    ! -- calculate residual
    resid = ra + ev + wr + ro + qinf + ex + sin + sout + seep
    !
    ! -- include storage
    if (this%isteady /= 1) then
      hlak0 = this%xoldpak(n)
      call this%lak_calculate_vol(n, hlak0, v0)
      call this%lak_calculate_vol(n, hlak, v1)
      resid = resid + (v0 - v1) / delt
    end if
    !
    ! -- return
    return
  end subroutine lak_calculate_residual


end module LakModule

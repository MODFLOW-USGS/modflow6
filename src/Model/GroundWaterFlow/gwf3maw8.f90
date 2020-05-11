module MawModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, LENBOUNDNAME, LENTIMESERIESNAME,        &
                             DZERO, DEM6, DEM4, DEM2, DQUARTER, DHALF, DP7,      &
                             DP9, DONE, DTWO, DPI, DTWOPI, DEIGHT, DHUNDRED,     &
                             DEP20, NAMEDBOUNDFLAG, LENPACKAGENAME, LENAUXNAME,  &
  &                          LENFTYPE, DHNOFLO, DHDRY, DNODATA, MAXCHARLEN,     &
                             TABLEFT, TABCENTER, TABRIGHT,                      &
                             TABSTRING, TABUCSTRING, TABINTEGER, TABREAL
  use SmoothingModule,  only: sQuadraticSaturation, sQSaturation,                &
                              sQuadraticSaturationDerivative,                    &
                              sQSaturationDerivative
  use BndModule, only: BndType
  use BudgetObjectModule, only: BudgetObjectType, budgetobject_cr
  use TableModule, only: TableType, table_cr
  use ObserveModule,        only: ObserveType
  use ObsModule, only: ObsType
  use InputOutputModule, only: get_node, URWORD, extract_idnum_or_bndname
  use BaseDisModule, only: DisBaseType
  use SimModule,        only: count_errors, store_error, store_error_unit, ustop
  use ArrayHandlersModule, only: ExpandArray
  use BlockParserModule,   only: BlockParserType
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_setptr,       &
                                 mem_deallocate
  !
  implicit none
  
  public :: MawType
  
  !
  character(len=LENFTYPE)       :: ftype = 'MAW'
  character(len=LENPACKAGENAME) :: text  = '             MAW'
  !
  type :: MawWellTSType
    character (len=LENTIMESERIESNAME), pointer :: name => null()
    real(DP), pointer :: value => null()
  end type MawWellTSType

  type :: MawWellType
    ! -- vectors
    integer(I4B), dimension(:), pointer, contiguous :: gwfnodes => NULL()
    real(DP), dimension(:), pointer, contiguous :: sradius => NULL()
    real(DP), dimension(:), pointer, contiguous :: hk => NULL()
    real(DP), dimension(:), pointer, contiguous :: satcond => NULL()
    real(DP), dimension(:), pointer, contiguous :: simcond => NULL()
    real(DP), dimension(:), pointer, contiguous :: topscrn => NULL()
    real(DP), dimension(:), pointer, contiguous :: botscrn => NULL()
  end type MawWellType
  !
  private
  public :: maw_create
  !
  type, extends(BndType) :: MawType
    !
    ! -- scalars
    ! -- characters
    !
    character(len=16), dimension(:), pointer, contiguous :: cmawbudget => NULL()
    character(len=LENAUXNAME), dimension(:), pointer,                           &
                               contiguous :: cauxcbc => NULL()
    !
    ! -- integers
    integer(I4B), pointer :: iprhed => null()
    integer(I4B), pointer :: iheadout => null()
    integer(I4B), pointer :: ibudgetout => null()
    integer(I4B), pointer :: cbcauxitems => NULL()
    integer(I4B), pointer :: iflowingwells => NULL()
    integer(I4B), pointer :: imawiss => NULL()
    integer(I4B), pointer :: imawissopt => NULL()
    integer(I4B), pointer :: nmawwells => NULL()
    integer(I4B), pointer :: check_attr => NULL()
    integer(I4B), pointer :: ishutoffcnt => NULL()
    integer(I4B), pointer :: ieffradopt => NULL()
    real(DP), pointer :: satomega => null()
    !
    ! -- for underrelaxation of estimated well q if using shutoff
    real(DP), pointer :: theta => NULL()
    real(DP), pointer :: kappa => NULL()
    !
    ! -- derived types
    type(MawWellType), dimension(:), pointer, contiguous :: mawwells => NULL()
    !
    ! -- from MawWellType (for each well)
    character (len=8), dimension(:), pointer, contiguous :: status => null()
    integer(I4B), dimension(:), pointer, contiguous :: ngwfnodes => null()
    integer(I4B), dimension(:), pointer, contiguous :: ieqn => null()
    integer(I4B), dimension(:), pointer, contiguous :: ishutoff => null()
    integer(I4B), dimension(:), pointer, contiguous :: ifwdischarge => null()
    real(DP), dimension(:), pointer, contiguous :: strt => null()
    real(DP), dimension(:), pointer, contiguous :: radius => null()
    real(DP), dimension(:), pointer, contiguous :: area => null()
    real(DP), dimension(:), pointer, contiguous :: pumpelev => null()
    real(DP), dimension(:), pointer, contiguous :: bot => null()
    real(DP), dimension(:), pointer, contiguous :: ratesim => null()
    real(DP), dimension(:), pointer, contiguous :: reduction_length => null()
    real(DP), dimension(:), pointer, contiguous :: fwelev => null()
    real(DP), dimension(:), pointer, contiguous :: fwcond => null()
    real(DP), dimension(:), pointer, contiguous :: fwrlen => null()
    real(DP), dimension(:), pointer, contiguous :: fwcondsim => null()
    real(DP), dimension(:), pointer, contiguous :: xsto => null()
    real(DP), dimension(:), pointer, contiguous :: xoldsto => null()
    real(DP), dimension(:), pointer, contiguous :: shutoffmin => null()
    real(DP), dimension(:), pointer, contiguous :: shutoffmax => null()
    real(DP), dimension(:), pointer, contiguous :: shutofflevel => null()
    real(DP), dimension(:), pointer, contiguous :: shutoffweight => null()
    real(DP), dimension(:), pointer, contiguous :: shutoffdq => null()
    real(DP), dimension(:), pointer, contiguous :: shutoffqold => null()
    !
    ! -- from MawWellType (for each connections)
    !
    ! -- for budgets
    integer(I4B), pointer :: bditems => NULL()
    type(BudgetObjectType), pointer :: budobj => null()
    !
    ! -- table objects
    type(TableType), pointer :: headtab => null()
    !
    ! -- pointer to gwf iss, k11, k22.
    integer(I4B), pointer :: gwfiss => NULL()
    real(DP), dimension(:), pointer, contiguous :: gwfk11 => NULL()
    real(DP), dimension(:), pointer, contiguous :: gwfk22 => NULL()
    integer(I4B), pointer :: gwfik22 => NULL()
    real(DP), dimension(:), pointer, contiguous :: gwfsat => NULL()
    !
    ! -- arrays for handling the rows added to the solution matrix
    integer(I4B), dimension(:), pointer, contiguous :: idxlocnode => null()      !map position in global rhs and x array of pack entry
    integer(I4B), dimension(:), pointer, contiguous :: idxdglo => null()         !map position in global array of package diagonal row entries
    integer(I4B), dimension(:), pointer, contiguous :: idxoffdglo => null()      !map position in global array of package off diagonal row entries
    integer(I4B), dimension(:), pointer, contiguous :: idxsymdglo => null()      !map position in global array of package diagonal entries to model rows
    integer(I4B), dimension(:), pointer, contiguous :: idxsymoffdglo => null()   !map position in global array of package off diagonal entries to model rows
    integer(I4B), dimension(:), pointer, contiguous :: iboundpak => null()       !package ibound
    real(DP), dimension(:), pointer, contiguous  :: xnewpak => null()            !package x vector
    real(DP), dimension(:), pointer, contiguous  :: xoldpak => null()            !package xold vector
    real(DP), dimension(:), pointer, contiguous  :: cterm => null()              !package c vector
    !
    ! -- vector data (start of flattening for future removal of MawWellType)
    character (len=LENBOUNDNAME), dimension(:), pointer,                        &
                                  contiguous :: cmawname => null()
    integer(I4B), dimension(:), pointer, contiguous :: idxmawconn => null()
    !
    ! -- time-series aware data
    real(DP), dimension(:), pointer, contiguous :: rate => null()
    real(DP), dimension(:), pointer, contiguous :: well_head => null()
    real(DP), dimension(:,:), pointer, contiguous :: mauxvar => null()
    !
    ! -- imap vector
    integer(I4B), dimension(:), pointer, contiguous :: imap => null()
    !
    ! -- maw output data
    real(DP), dimension(:), pointer, contiguous :: qauxcbc => null()
    real(DP), dimension(:), pointer, contiguous :: dbuff => null()
    real(DP), dimension(:), pointer, contiguous :: qleak => null()
    real(DP), dimension(:), pointer, contiguous :: qout => null()
    real(DP), dimension(:), pointer, contiguous :: qfw => null()
    real(DP), dimension(:), pointer, contiguous :: qsto => null()
    real(DP), dimension(:), pointer, contiguous :: qconst => null()
    ! -- type bound procedures
    contains
    procedure :: maw_allocate_scalars
    procedure :: maw_allocate_well_arrays
    procedure :: maw_allocate_conn_arrays
    procedure :: bnd_options => maw_options
    procedure :: read_dimensions => maw_read_dimensions
    procedure :: read_initial_attr => maw_read_initial_attr
    procedure :: set_pointers => maw_set_pointers
    procedure :: bnd_ac => maw_ac
    procedure :: bnd_mc => maw_mc
    procedure :: bnd_ar => maw_ar
    procedure :: bnd_rp => maw_rp
    procedure :: bnd_ad => maw_ad
    procedure :: bnd_cf => maw_cf
    procedure :: bnd_fc => maw_fc
    procedure :: bnd_fn => maw_fn
    procedure :: bnd_nur => maw_nur
    procedure :: bnd_bd => maw_bd
    procedure :: bnd_ot => maw_ot
    procedure :: bnd_da => maw_da
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => maw_obs_supported
    procedure, public :: bnd_df_obs => maw_df_obs
    procedure, public :: bnd_rp_obs => maw_rp_obs
    ! -- private procedures
    procedure, private :: maw_read_wells
    procedure, private :: maw_read_well_connections
    procedure, private :: maw_deallocate_well
    procedure, private :: maw_check_attributes
    procedure, private :: maw_set_stressperiod
    procedure, private :: maw_set_attribute_error
    procedure, private :: maw_calculate_saturation
    procedure, private :: maw_calculate_satcond
    procedure, private :: maw_calculate_wellq
    procedure, private :: maw_calculate_qpot
    procedure, private :: maw_cfupdate
    procedure, private :: maw_bd_obs
    ! -- budget
    procedure, private :: maw_setup_budobj
    procedure, private :: maw_fill_budobj
    ! -- table
    procedure, private :: maw_setup_tableobj
  end type MawType

contains

  subroutine maw_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! maw_create -- Create a New Multi-Aquifer Well Package
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
    type(MawType), pointer :: mawobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(mawobj)
    packobj => mawobj
    !
    ! -- create name and origin
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call mawobj%maw_allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()

    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 4
    packobj%iscloc = 0  ! not supported
    packobj%ictorigin = 'NPF'
    !
    ! -- return
    return
  end subroutine maw_create

  subroutine maw_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(MawType),   intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- allocate the object and assign values to object variables
    call mem_allocate(this%iprhed, 'IPRHED', this%origin)
    call mem_allocate(this%iheadout, 'IHEADOUT', this%origin)
    call mem_allocate(this%ibudgetout, 'IBUDGETOUT', this%origin)
    call mem_allocate(this%iflowingwells, 'IFLOWINGWELLS', this%origin)
    call mem_allocate(this%imawiss, 'IMAWISS', this%origin)
    call mem_allocate(this%imawissopt, 'IMAWISSOPT', this%origin)
    call mem_allocate(this%nmawwells, 'NMAWWELLS', this%origin)
    call mem_allocate(this%check_attr, 'check_attr', this%origin)
    call mem_allocate(this%ishutoffcnt, 'ISHUTOFFCNT', this%origin)
    call mem_allocate(this%ieffradopt, 'IEFFRADOPT', this%origin)
    call mem_allocate(this%satomega, 'SATOMEGA', this%origin)
    call mem_allocate(this%bditems, 'BDITEMS', this%origin)
    call mem_allocate(this%theta, 'THETA', this%origin)
    call mem_allocate(this%kappa, 'KAPPA', this%origin)
    call mem_allocate(this%cbcauxitems, 'CBCAUXITEMS', this%origin)
    !
    ! -- Set values
    this%iprhed = 0
    this%iheadout = 0
    this%ibudgetout = 0
    this%iflowingwells = 0
    this%imawiss = 0
    this%imawissopt = 0
    this%ieffradopt = 0
    this%satomega = DZERO
    this%bditems = 8
    this%theta = DP7
    this%kappa = DEM4
    this%cbcauxitems = 1
    !this%imover = 0
    !
    ! -- return
    return
  end subroutine maw_allocate_scalars

  subroutine maw_allocate_well_arrays(this)
! ******************************************************************************
! maw_allocate_arrays -- allocate well arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(MawType),   intent(inout) :: this
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: jj
! ------------------------------------------------------------------------------
    !
    ! -- allocate character array for budget text
    allocate(this%cmawbudget(this%bditems))
    !
    !-- fill cmawbudget
    this%cmawbudget(1) = '             GWF'
    this%cmawbudget(2) = '            RATE'
    this%cmawbudget(3) = '         STORAGE'
    this%cmawbudget(4) = '        CONSTANT'
    this%cmawbudget(5) = '         FW-RATE'
    this%cmawbudget(6) = '        FROM-MVR'
    this%cmawbudget(7) = '     RATE-TO-MVR'
    this%cmawbudget(8) = '  FW-RATE-TO-MVR'
    !
    ! -- allocate character arrays
    allocate(this%cmawname(this%nmawwells))
    allocate(this%status(this%nmawwells))
    !
    ! -- allocate well data pointers in memory manager
    call mem_allocate(this%ngwfnodes, this%nmawwells, 'NGWFNODES', this%origin)
    call mem_allocate(this%ieqn, this%nmawwells, 'IEQN', this%origin)
    call mem_allocate(this%ishutoff, this%nmawwells, 'ISHUTOFF', this%origin)
    call mem_allocate(this%ifwdischarge, this%nmawwells, 'IFWDISCHARGE',         &
                      this%origin)
    call mem_allocate(this%strt, this%nmawwells, 'STRT', this%origin)
    call mem_allocate(this%radius, this%nmawwells, 'RADIUS', this%origin)
    call mem_allocate(this%area, this%nmawwells, 'AREA', this%origin)
    call mem_allocate(this%pumpelev, this%nmawwells, 'PUMPELEV', this%origin)
    call mem_allocate(this%bot, this%nmawwells, 'BOT', this%origin)
    call mem_allocate(this%ratesim, this%nmawwells, 'RATESIM', this%origin)
    call mem_allocate(this%reduction_length, this%nmawwells, 'REDUCTION_LENGTH', &
                      this%origin)
    call mem_allocate(this%fwelev, this%nmawwells, 'FWELEV', this%origin)
    call mem_allocate(this%fwcond, this%nmawwells, 'FWCONDS', this%origin)
    call mem_allocate(this%fwrlen, this%nmawwells, 'FWRLEN', this%origin)
    call mem_allocate(this%fwcondsim, this%nmawwells, 'FWCONDSIM', this%origin)
    call mem_allocate(this%xsto, this%nmawwells, 'XSTO', this%origin)
    call mem_allocate(this%xoldsto, this%nmawwells, 'XOLDSTO', this%origin)
    call mem_allocate(this%shutoffmin, this%nmawwells, 'SHUTOFFMIN', this%origin)
    call mem_allocate(this%shutoffmax, this%nmawwells, 'SHUTOFFMAX', this%origin)
    call mem_allocate(this%shutofflevel, this%nmawwells, 'SHUTOFFLEVEL',         &
                      this%origin)
    call mem_allocate(this%shutoffweight, this%nmawwells, 'SHUTOFFWEIGHT',       &
                      this%origin)
    call mem_allocate(this%shutoffdq, this%nmawwells, 'SHUTOFFDQ', this%origin)
    call mem_allocate(this%shutoffqold, this%nmawwells, 'SHUTOFFQOLD',           &
                      this%origin)
    !
    ! -- timeseries aware variables
    call mem_allocate(this%rate, this%nmawwells, 'RATE', this%origin)
    call mem_allocate(this%well_head, this%nmawwells, 'WELL_HEAD', this%origin)
    if (this%naux > 0) then
      jj = this%naux
    else
      jj = 1
    end if
    call mem_allocate(this%mauxvar, jj, this%nmawwells, 'MAUXVAR',               &
                      this%origin)
    !
    ! -- initialize well data
    do i = 1, this%nmawwells
      this%status(i) = 'ACTIVE'
      this%ngwfnodes(i) = 0
      this%ieqn(i) = 0
      this%ishutoff(i) = 0
      this%ifwdischarge(i) = 0
      this%strt(i) = DEP20
      this%radius(i) = DEP20
      this%area(i) = DZERO
      this%pumpelev(i) = DEP20
      this%bot(i) = DEP20
      this%ratesim(i) = DZERO
      this%reduction_length(i) = DEP20
      this%fwelev(i) = DZERO
      this%fwcond(i) = DZERO
      this%fwrlen(i) = DZERO
      this%fwcondsim(i) = DZERO
      this%xsto(i) = DZERO
      this%xoldsto(i) = DZERO
      this%shutoffmin(i) = DZERO
      this%shutoffmax(i) = DZERO
      this%shutofflevel(i) = DEP20
      this%shutoffweight(i) = DONE
      this%shutoffdq(i) = DONE
      this%shutoffqold(i) = DONE
      !
      ! -- timeseries aware variables
      this%rate(i) = DZERO
      this%well_head(i) = DZERO
      do jj = 1, max(1, this%naux)
        this%mauxvar(jj, i) = DZERO
      end do
    end do
    !
    ! -- allocate idxmawconn
    call mem_allocate(this%idxmawconn, this%nmawwells+1, 'IDXMAWCONN',           &
                      this%origin)
    !
    ! -- initialize idxmawconn
    do i = 1, this%nmawwells + 1
      this%idxmawconn(i) = 0
    end do
    !
    ! -- allocate and initialize dbuff
    if (this%iheadout > 0) then
      call mem_allocate(this%dbuff, this%nmawwells, 'DBUFF', this%origin)
      do i = 1, this%nmawwells
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
    ! -- allocate flowing well data
    if (this%iflowingwells /= 0) then
      call mem_allocate(this%qfw, this%nmawwells, 'QFW', this%origin)
    else
      call mem_allocate(this%qfw, 1, 'QFW', this%origin)
    end if
    call mem_allocate(this%qout, this%nmawwells, 'QOUT', this%origin)
    call mem_allocate(this%qsto, this%nmawwells, 'QSTO', this%origin)
    call mem_allocate(this%qconst, this%nmawwells, 'QCONST', this%origin)
    !
    ! -- initialize flowing well, storage, and constant flow terms
    do i = 1, this%nmawwells
      if (this%iflowingwells /= 0) then
        this%qfw(i) = DZERO
      end if
      this%qsto(i) = DZERO
      this%qconst(i) = DZERO
    end do
    !
    ! -- return
    return
  end subroutine maw_allocate_well_arrays

  subroutine maw_allocate_conn_arrays(this)
! ******************************************************************************
! maw_allocate_conn_arrays -- allocate connection arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(MawType),   intent(inout) :: this
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_arrays()
    !
    ! -- allocate imap
    call mem_allocate(this%imap, this%MAXBOUND, 'IMAP', this%origin)
    !
    ! -- initialize imap
    do i = 1, this%maxbound
      this%imap(i) = 0
    end do
    !
    ! -- allocate qleak
    call mem_allocate(this%qleak, this%maxbound, 'QLEAK', this%origin)
    do i = 1, this%maxbound
      this%qleak(i) = DZERO
    end do
    !
    ! -- return
    return
  end subroutine maw_allocate_conn_arrays

  subroutine maw_read_wells(this)
! ******************************************************************************
! pak1read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use TimeSeriesManagerModule, only: read_value_or_time_series_adv
    ! -- dummy
    class(MawType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: text
    character(len=LINELENGTH) :: keyword
    character(len=LINELENGTH) :: cstr
    character(len=LINELENGTH) :: strttext
    character(len=LENBOUNDNAME) :: bndName
    character(len=LENBOUNDNAME) :: bndNameTemp
    character(len=9) :: cno
    character(len=50), dimension(:), allocatable :: caux
    integer(I4B) :: ival
    logical :: isfound, endOfBlock
    real(DP) :: rval
    integer(I4B) :: n
    integer(I4B) :: ii
    integer(I4B) :: jj
    !integer(I4B) :: iaux
    integer(I4B) :: ieqn
    integer(I4B) :: itmp
    integer(I4B) :: ierr
    real(DP), pointer :: bndElem => null()
    integer(I4B), dimension(:), pointer, contiguous :: nboundchk
    ! -- format
    character(len=*),parameter :: fmthdbot = &
      "('well head (', G0, ') must be >= BOTTOM_ELEVATION (', G0, ').')"
! ------------------------------------------------------------------------------
    !
    ! -- code
    !
    ! -- allocate and initialize temporary variables
    allocate(nboundchk(this%nmawwells))
    do n = 1, this%nmawwells
      nboundchk(n) = 0
    end do
    !
    ! -- initialize itmp
    itmp = 0
    !
    ! -- allocate space for mawwells data
    allocate(this%mawwells(this%nmawwells))
    !
    ! -- set npakeq to nmawwells
    this%npakeq = this%nmawwells
    !
    ! -- allocate local storage for aux variables
    if (this%naux > 0) then
      allocate(caux(this%naux))
    end if
    !
    ! -- allocate well data
    call this%maw_allocate_well_arrays()
    !
    ! -- read maw well data
    ! -- get wells block
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr,                     &
                              supportopenclose=.true.)
    !
    ! -- parse locations block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')                                                 &
        'PROCESSING ' // trim(adjustl(this%text)) // ' PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        ival = this%parser%GetInteger()
        n = ival

        if (n < 1 .or. n > this%nmawwells) then
          write(errmsg,'(a,1x,i0,a)')                                            &
            'IMAW must be greater than 0 and less than or equal to',             &
            this%nmawwells, '.'
          call store_error(errmsg)
          cycle
        end if
        !
        ! -- increment nboundchk
        nboundchk(n) = nboundchk(n) + 1
        !
        ! -- radius
        rval = this%parser%GetDouble()
        if (rval <= DZERO) then
          write(errmsg,'(a,1x,i0,1x,a)')                                         &
            'Radius for well', n, 'must be greater than zero.'
          call store_error(errmsg)
        end if
        this%radius(n) = rval
        this%area(n) = DPI * rval**DTWO
        !
        ! -- well bottom
        this%bot(n) = this%parser%GetDouble()
        !
        ! -- strt
        call this%parser%GetString(strttext)
        !
        ! -- ieqn
        call this%parser%GetStringCaps(keyword)
        if (keyword=='SPECIFIED') then
          ieqn = 0
        else if (keyword=='THEIM' .or. keyword=='THIEM') then
          ieqn = 1
        else if (keyword=='SKIN') then
          ieqn = 2
        else if (keyword=='CUMULATIVE') then
          ieqn = 3
        else if (keyword=='MEAN') then
          ieqn = 4
        else
          write(errmsg,'(a,1x,i0,1x,a)')                                         &
            'CONDEQN for well', n,                                               &
            "must be 'CONDUCTANCE', 'THIEM', 'MEAN', or 'SKIN'."
        end if
        this%ieqn(n) = ieqn
        !
        ! -- ngwnodes
        ival = this%parser%GetInteger()
        if (ival < 1) then
          write(errmsg,'(a,1x,i0,1x,a)')                                         &
            'NGWFNODES for well', n, 'must be greater than zero.'
          call store_error(errmsg)
        end if
        
        if (ival > 0) then
          this%ngwfnodes(n) = ival
        end if
        !
        ! -- allocate storage for connection data needed for the MAW well
        allocate(this%mawwells(n)%gwfnodes(this%ngwfnodes(n)))
        allocate(this%mawwells(n)%satcond(this%ngwfnodes(n)))
        allocate(this%mawwells(n)%simcond(this%ngwfnodes(n)))
        allocate(this%mawwells(n)%topscrn(this%ngwfnodes(n)))
        allocate(this%mawwells(n)%botscrn(this%ngwfnodes(n)))
        if (this%ieqn(n) == 2 .OR. this%ieqn(n) == 3 .OR.                        &
            this%ieqn(n) == 4) then
          allocate(this%mawwells(n)%hk(this%ngwfnodes(n)))
        end if
        if (this%ieqn(n) == 2 .OR. this%ieqn(n) == 3 .OR.                        &
            this%ieqn(n) == 4) then
          allocate(this%mawwells(n)%sradius(this%ngwfnodes(n)))
        end if
        !
        ! -- increment maxbound
        itmp = itmp + this%ngwfnodes(n)
        !
        ! -- set default bndName
        write (cno,'(i9.9)') n
        bndName = 'MAWWELL' // cno
        !
        ! -- get aux data
        do jj = 1, this%naux
          call this%parser%GetString(caux(jj))
        end do
        !
        ! -- read well name
        this%cmawname(n) = bndName
        if (this%inamedbound /= 0) then
          call this%parser%GetStringCaps(bndNameTemp)
          if (bndNameTemp /= '') then
            this%cmawname(n) = bndNameTemp(1:16)
          end if
        else
          bndName = ''
        end if
        !
        ! fill timeseries aware data
        jj = 1    ! For WELL_HEAD
        bndElem => this%well_head(n)
        call read_value_or_time_series_adv(strttext, n, jj, bndElem, this%name,  &
                                           'BND', this%tsManager, this%iprpak,   &
                                           'WELL_HEAD')
        !
        ! -- set starting head value
        this%strt(n) = this%well_head(n)
        !
        ! -- check for error condition
        if (this%strt(n) < this%bot(n)) then
          write(cstr, fmthdbot) this%strt(n), this%bot(n)
          call this%maw_set_attribute_error(n, 'STRT', trim(cstr))
        end if
        !
        ! -- fill aux data
        do jj = 1, this%naux
          text = caux(jj)
          ii = n
          bndElem => this%mauxvar(jj, ii)
          call read_value_or_time_series_adv(text, ii, jj, bndElem, this%name,   &
                                             'AUX', this%tsManager, this%iprpak, &
                                             this%auxname(jj))
        end do
      end do

      write(this%iout,'(1x,a)')                                                  &
        'END OF ' // trim(adjustl(this%text)) // ' PACKAGEDATA'
      !
      ! -- check for duplicate or missing wells
      do n = 1,  this%nmawwells
        if (nboundchk(n) == 0) then
          write(errmsg,'(a,1x,i0,a)')  'No data specified for maw well', n, '.'
          call store_error(errmsg)
        else if (nboundchk(n) > 1) then
          write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                             &
            'Data for maw well', n, 'specified', nboundchk(n), 'times.'
          call store_error(errmsg)
        end if
      end do
    else
      call store_error('Required packagedata block not found.')
    end if
    !
    ! -- terminate if any errors were detected
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- set MAXBOUND
    this%maxbound = itmp
    write(this%iout,'(//4x,a,i7)') 'MAXBOUND = ', this%maxbound
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
  end subroutine maw_read_wells

  subroutine maw_read_well_connections(this)
! ******************************************************************************
! pak1read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(MawType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: cellid
    character(len=30) :: nodestr
    integer(I4B) :: ierr, ival
    integer(I4b) :: ipos
    logical :: isfound, endOfBlock
    real(DP) :: rval
    real(DP) :: topnn
    real(DP) :: botnn
    real(DP) :: botw
    integer(I4B) :: j
    integer(I4B) :: jj
    integer(I4B) :: n
    integer(I4B) :: nn
    integer(I4B) :: nn2
    integer(I4B), dimension(:), pointer, contiguous :: nboundchk
    integer(I4B), dimension(:), pointer, contiguous :: iachk
    
! ------------------------------------------------------------------------------
    ! -- format
    !
    ! -- code
    !
    ! -- allocate and initialize local storage
    allocate(iachk(this%nmawwells+1))
    iachk(1) = 1
    do n = 1, this%nmawwells
      iachk(n+1) = iachk(n) +  this%ngwfnodes(n)
    end do
    allocate(nboundchk(this%maxbound))
    do n = 1, this%maxbound
      nboundchk(n) = 0
    end do
    !
    ! -- get well_connections block
    call this%parser%GetBlock('CONNECTIONDATA', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse well_connections block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' CONNECTIONDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        ival = this%parser%GetInteger()
        n = ival

        if (n < 1 .or. n > this%nmawwells) then
          write(errmsg,'(a,1x,i0,a)')                                            &
            'IMAW must be greater than 0 and less than or equal to ',            &
            this%nmawwells, '.'
          call store_error(errmsg)
          cycle
        end if
        !
        ! -- read connection number
        ival = this%parser%GetInteger()
        if (ival < 1 .or. ival > this%ngwfnodes(n)) then
          write(errmsg,'(a,1x,i0,1x,a,1x,i0,a)')                                 &
            'JCONN for well ', n,                                                &
            'must be greater than 1 and less than or equal to ',                 &
            this%ngwfnodes(n), '.'
          call store_error(errmsg)
          cycle
        end if
        
        ipos = iachk(n) + ival - 1
        nboundchk(ipos) = nboundchk(ipos) + 1
        
        j = ival
        !
        ! -- read gwfnodes from the line
        call this%parser%GetCellid(this%dis%ndim, cellid)
        nn  = this%dis%noder_from_cellid(cellid, this%inunit, this%iout)
        topnn = this%dis%top(nn)
        botnn = this%dis%bot(nn)
        botw = this%bot(n)
        !
        ! -- set gwf node number for connection
        this%mawwells(n)%gwfnodes(j) = nn
        !
        ! -- top of screen
        rval = this%parser%GetDouble()
        if (this%ieqn(n) /= 4) then
          rval = topnn
        else
          if (rval > topnn) then
            rval = topnn
          end if
        end if
        this%mawwells(n)%topscrn(j)  = rval
        !
        ! -- bottom of screen
        rval = this%parser%GetDouble()
        if (this%ieqn(n) /= 4) then
          rval = botnn
        else
          if (rval < botnn) then
            rval = botnn
          end if
        end if
        this%mawwells(n)%botscrn(j)  = rval
        !
        ! -- adjust the bottom of the well for all conductance approaches
        !    except for "mean"
        if (this%ieqn(n) /= 4) then
          if (rval < botw) then
            botw = rval
            this%bot(n) = rval
          end if
        end if
        !
        ! -- hydraulic conductivity or conductance
        rval = this%parser%GetDouble()
        if (this%ieqn(n) == 0) then
          this%mawwells(n)%satcond(j)  = rval
        else if (this%ieqn(n) == 2 .OR. this%ieqn(n) == 3 .OR.                   &
                 this%ieqn(n) == 4) then
          this%mawwells(n)%hk(j) = rval
        end if
        !
        ! -- skin radius
        rval = this%parser%GetDouble()
        if (this%ieqn(n) == 2 .OR. this%ieqn(n) == 3 .OR.                        &
            this%ieqn(n) == 4) then
          this%mawwells(n)%sradius(j) = rval
        end if
      end do
      write(this%iout,'(1x,a)')                                                  &
        'END OF ' // trim(adjustl(this%text)) // ' CONNECTIONDATA'
      
      ipos = 0
      do n = 1, this%nmawwells
        do j = 1, this%ngwfnodes(n)
          ipos = ipos + 1
          !
          ! -- check for missing or duplicate maw well connections
          if (nboundchk(ipos) == 0) then
            write(errmsg,'(a,1x,i0,1x,a,1x,i0,a)')                               &
              'No data specified for maw well', n, 'connection', j, '.'
            call store_error(errmsg)
          else if (nboundchk(ipos) > 1) then
            write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a,1x,i0,1x,a)')                 &
              'Data for maw well', n, 'connection', j,                           &
              'specified', nboundchk(n), 'times.'
            call store_error(errmsg)
          end if
        end do
      end do
      !
      ! -- make sure that more than one connection per cell is only specified 
      !    wells using the mean conducance type
      do n = 1, this%nmawwells
        if (this%ieqn(n) /= 4) then
          do j = 1, this%ngwfnodes(n)
            nn = this%mawwells(n)%gwfnodes(j) 
            do jj = 1, this%ngwfnodes(n)
              ! skip current maw node
              if (jj == j) then
                cycle
              end if
              nn2 =  this%mawwells(n)%gwfnodes(jj) 
              if (nn2 == nn) then
                call this%dis%noder_to_string(nn, nodestr)
                write(errmsg,'(a,1x,i0,1x,a,1x,i0,3(1x,a))')                     &
                  'Only one connection can be specified for maw well',           &
                  n, 'connection', j, 'to gwf cell', trim(adjustl(nodestr)),     &
                  'unless the mean condeqn is specified.'
                call store_error(errmsg)
              end if
            end do
          end do
        end if
      end do
    else
      call store_error('Required connectiondata block not found.')
    end if
    !
    ! -- deallocate local variable
    deallocate(iachk)
    deallocate(nboundchk)
    !
    ! -- write summary of maw well_connection error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine maw_read_well_connections


  subroutine maw_read_dimensions(this)
! ******************************************************************************
! pak1read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(MawType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LENBOUNDNAME) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- initialize dimensions to -1
    this%nmawwells= -1
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
          case ('NMAWWELLS')
            this%nmawwells = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)')'NMAWWELLS = ', this%nmawwells
          case default
            write(errmsg,'(4x,a,a)') &
              '****ERROR. UNKNOWN '//trim(this%text)//' DIMENSION: ', &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' DIMENSIONS'
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
      call ustop()
    end if
    !
    ! -- verify dimensions were set correctly
    if (this%nmawwells < 0) then
      write(errmsg, '(1x,a)') &
        'ERROR:  NMAWWELLS WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.'
      call store_error(errmsg)
    end if
    !
    ! -- stop if errors were encountered in the DIMENSIONS block
    if (count_errors() > 0) then
      call ustop()
    end if
    !
    ! -- read wells block
    call this%maw_read_wells()
    !
    ! -- allocate arrays
    !
    ! -- read well_connections block
    call this%maw_read_well_connections()
    !
    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- setup the budget object
    call this%maw_setup_budobj()
    !
    ! -- setup the head table object
    call this%maw_setup_tableobj()
    !
    ! -- return
    return
  end subroutine maw_read_dimensions


  subroutine maw_read_initial_attr(this)
! ******************************************************************************
! maw_read_initial_attr -- Read the initial parameters for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(MawType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: text
    integer(I4B) :: ntabcols
    integer(I4B) :: j, n
    integer(I4B) :: nn
    integer(I4B) :: inode
    integer(I4B) :: idx
    real(DP) :: k11, k22
    character (len=10), dimension(0:4) :: ccond
    character (len=30) :: nodestr
    ! -- data
    data ccond(0) /'SPECIFIED '/
    data ccond(1) /'THIEM     '/
    data ccond(2) /'SKIN      '/
    data ccond(3) /'CUMULATIVE'/
    data ccond(4) /'MEAN      '/
    ! -- format
    character(len=*), parameter :: fmtwelln = &
      "(1X,//43X,'MULTI-AQUIFER WELL DATA'" // &
      "/1X,109('-')," // &
      "/1X,7(A10,1X),A16)"
    character(len=*), parameter :: fmtwelld = &
      "(1X,I10,1X,4(G10.3,1X),I10,1X,A10,1X,A16)"
    character(len=*), parameter :: fmtline = &
      "(1X,119('-'),//)"
    character(len=*), parameter :: fmtwellcn = &
      "(1X,//37X,'MULTI-AQUIFER WELL CONNECTION DATA'" // &
      "/1X,119('-')," // &
      "/1X,2(A10,1X),A20,7(A10,1X))"
    character(len=*), parameter :: fmtwellcd = &
      "(1X,2(I10,1X),A20,1X,2(G10.3,1X),2(A10,1X),3(G10.3,1X))"
! ------------------------------------------------------------------------------
    !
    ! -- initialize xnewpak
    do n = 1, this%nmawwells
      this%xnewpak(n) = this%strt(n)
    end do
    !
    ! -- initialize status (iboundpak) of maw wells to active
    do n = 1, this%nmawwells
      select case (this%status(n))
        case('CONSTANT')
          this%iboundpak(n) = -1
        case('INACTIVE')
          this%iboundpak(n) = 0
        case('ACTIVE')
          this%iboundpak(n) = 1
      end select
    end do
    !
    ! -- set idxmawconn and imap for each connection
    idx = 0
    this%idxmawconn(1) = 1
    do n = 1, this%nmawwells
      do j = 1, this%ngwfnodes(n)
        idx = idx + 1
        this%imap(idx) = n
      end do
      this%idxmawconn(n+1) = idx + 1
    end do
    !
    ! -- set boundname for each connection
    if (this%inamedbound /= 0) then
      idx = 0
      do n = 1, this%nmawwells
        do j = 1, this%ngwfnodes(n)
          idx = idx + 1
          this%boundname(idx) = this%cmawname(n)
        end do
      end do
    else
      do n = 1, this%nmawwells
        this%cmawname(n) = ''
      end do
    end if
    !
    ! -- set imap and boundname for each connection
    if (this%inamedbound /= 0) then
      idx = 0
      do n = 1, this%nmawwells
        do j = 1, this%ngwfnodes(n)
          idx = idx + 1
          this%boundname(idx) = this%cmawname(n)
          this%imap(idx) = n
        end do
      end do
    end if
    !
    ! -- set pointer to gwf iss and gwf hk
    call mem_setptr(this%gwfiss, 'ISS', trim(this%name_model))
    call mem_setptr(this%gwfk11, 'K11', trim(this%name_model)//' NPF')
    call mem_setptr(this%gwfk22, 'K22', trim(this%name_model)//' NPF')
    call mem_setptr(this%gwfik22, 'IK22', trim(this%name_model)//' NPF')
    call mem_setptr(this%gwfsat, 'SAT', trim(this%name_model)//' NPF')
    !
    ! -- qa data
    call this%maw_check_attributes()
    !
    ! -- Calculate the saturated conductance
    do n = 1, this%nmawwells
      !
      ! -- calculate saturated conductance only if CONDUCTANCE was not
      !    specified for each maw-gwf connection (CONDUCTANCE keyword).
      do j = 1, this%ngwfnodes(n)
        if (this%ieqn(n) /= 0) then
          inode = this%mawwells(n)%gwfnodes(j)
          call this%maw_calculate_satcond(n, j, inode)
        end if
      end do
    end do
    !
    ! -- write summary of static well data
    ! -- write well data
    if (this%iprpak /= 0) then
      ntabcols = 7
      if (this%inamedbound /= 0) then
        ntabcols = ntabcols + 1
      end if
      title = trim(adjustl(this%text)) // ' PACKAGE (' //                        &
              trim(adjustl(this%name)) //') STATIC WELL DATA'
      call table_cr(this%inputtab, this%name, title)
      call this%inputtab%table_df(this%nmawwells, ntabcols, this%iout)
      text = 'NUMBER'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'RADIUS'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'AREA'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'WELL BOTTOM'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'STARTING HEAD'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'NUMBER OF GWF NODES'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'CONDUCT. EQUATION'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      if (this%inamedbound /= 0) then
        text = 'NAME'
        call this%inputtab%initialize_column(text, 20, alignment=TABLEFT)
      end if    
      do n = 1, this%nmawwells
        call this%inputtab%add_term(n)
        call this%inputtab%add_term(this%radius(n))
        call this%inputtab%add_term(this%area(n))
        call this%inputtab%add_term(this%bot(n))
        call this%inputtab%add_term(this%strt(n))
        call this%inputtab%add_term(this%ngwfnodes(n))
        call this%inputtab%add_term(ccond(this%ieqn(n)))
        if (this%inamedbound /= 0) then 
          call this%inputtab%add_term(this%cmawname(n))
        end if
      end do
    end if
    !
    ! -- write well connection data
    if (this%iprpak /= 0) then
      ntabcols = 10
      title = trim(adjustl(this%text)) // ' PACKAGE (' //                        &
              trim(adjustl(this%name)) //') STATIC WELL CONNECTION DATA'
      call table_cr(this%inputtab, this%name, title)
      call this%inputtab%table_df(this%maxbound, ntabcols, this%iout)
      text = 'NUMBER'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'WELL CONNECTION'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'CELLID'
      call this%inputtab%initialize_column(text, 20, alignment=TABLEFT)
      text = 'TOP OF SCREEN'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'BOTTOM OF SCREEN'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'SKIN RADIUS'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'SKIN K'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'K11'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'K22'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'SATURATED WELL CONDUCT.'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      !
      ! -- write the data to the table
      do n = 1, this%nmawwells
        do j = 1, this%ngwfnodes(n)
          call this%inputtab%add_term(n)
          call this%inputtab%add_term(j)
          nn = this%mawwells(n)%gwfnodes(j)
          call this%dis%noder_to_string(nn, nodestr)
          call this%inputtab%add_term(nodestr)
          call this%inputtab%add_term(this%mawwells(n)%topscrn(j))
          call this%inputtab%add_term(this%mawwells(n)%botscrn(j))
          if (this%ieqn(n) == 2 .or.                                             &
              this%ieqn(n) == 3 .or.                                             &
              this%ieqn(n) == 4) then
            call this%inputtab%add_term(this%mawwells(n)%sradius(j))
            call this%inputtab%add_term(this%mawwells(n)%hk(j))
          else
            call this%inputtab%add_term(' ')      
            call this%inputtab%add_term(' ')      
          end if      
          if (this%ieqn(n) == 1 .or.                                             &
              this%ieqn(n) == 2 .or.                                             &
              this%ieqn(n) == 3) then
            k11 = this%gwfk11(nn)
            if (this%gwfik22 == 0) then
              k22 = this%gwfk11(nn)
            else
              k22 = this%gwfk22(nn)
            end if
            call this%inputtab%add_term(k11)
            call this%inputtab%add_term(k22)
          else
            call this%inputtab%add_term(' ')
            call this%inputtab%add_term(' ')
          end if
          call this%inputtab%add_term(this%mawwells(n)%satcond(j))
        end do
      end do
    end if
    !
    ! -- finished with pointer to gwf hydraulic conductivity
    this%gwfk11 => null()
    this%gwfk22 => null()
    this%gwfik22 => null()
    this%gwfsat => null()
    !
    ! -- check for any error conditions
    if (count_errors() > 0) then
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine maw_read_initial_attr


  subroutine maw_set_stressperiod(this, imaw, line)
! ******************************************************************************
! maw_set_stressperiod -- Set a stress period attribute for mawweslls(imaw)
!                         using keywords.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    !use TdisModule, only: kper
    use TimeSeriesManagerModule, only: read_value_or_time_series_adv
    ! -- dummy
    class(MawType),intent(inout) :: this
    integer(I4B), intent(in) :: imaw
    character (len=*), intent(in) :: line
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: errmsgr
    character(len=LINELENGTH) :: text
    character(len=LINELENGTH) :: cstr
    character(len=LINELENGTH) :: caux
    character(len=LINELENGTH) :: keyword
    character(len=LENBOUNDNAME) :: bndName
    character(len=9) :: cmaw
    integer(I4B) :: ival, istart, istop
    integer(I4B) :: i0
    integer(I4B) :: lloc
    integer(I4B) :: ii
    integer(I4B) :: jj
    integer(I4B) :: idx
    !integer(I4B) :: iaux
    real(DP) :: rval
    real(DP), pointer :: bndElem => null()
    integer(I4B) :: istat
    ! -- formats
    character(len=*),parameter :: fmthdbot = &
      "('well head (',G0,') must be >= BOTTOM_ELEVATION (',G0, ').')"
! ------------------------------------------------------------------------------
    !
    ! -- Assign boundary name, if available
    !
    ! -- set default bndName
    write (cmaw,'(i9.9)') imaw
    !bndName = 'MAWWELL' // cmaw
    if (this%inamedbound==1) then
      idx = 0
      do ii = 1, imaw
        do jj = 1, this%ngwfnodes(ii)
          idx = idx + 1
        end do
      end do
      bndName = this%boundname(idx)
    else
      bndName = ''
    end if
    !
    ! -- read line
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval,                        &
                this%iout, this%inunit)
    i0 = istart
    keyword = line(istart:istop)
    select case (line(istart:istop))
      case ('STATUS')
        call urword(line, lloc, istart, istop, 1, ival, rval,                    &
                    this%iout, this%inunit)
        text = line(istart:istop)
        this%status(imaw) = text(1:8)
        select case(text)
          case('CONSTANT')
            this%iboundpak(imaw) = -1
          case('INACTIVE')
            this%iboundpak(imaw) = 0
          case('ACTIVE')
            this%iboundpak(imaw) = 1
          case default
            write(errmsg,'(a,a)')                                                &
              'Unknown ' // trim(this%text) // " maw status keyword: '",         &
              trim(text) // "'."
            call store_error(errmsg)
        end select
        !if (text == 'CONSTANT') then
        !  this%iboundpak(imaw) = -1
        !else if (text == 'INACTIVE') then
        !  this%iboundpak(imaw) = 0
        !else if (text == 'ACTIVE') then
        !  this%iboundpak(imaw) = 1
        !else
        !  write(errmsg,'(a,a)')                                                  &
        !    'Unknown ' // trim(this%text) // " maw status keyword: '",           &
        !    trim(text) // "'."
        !  call store_error(errmsg)
        !end if
      case ('RATE')
        call urword(line, lloc, istart, istop, 0, ival, rval,                    &
                    this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For RATE
        bndElem => this%rate(imaw)
        call read_value_or_time_series_adv(text, imaw, jj, bndElem, this%name,   &
                                           'BND', this%tsManager, this%iprpak,   &
                                           'RATE')
     case ('WELL_HEAD')
        call urword(line, lloc, istart, istop, 0, ival, rval,                    &
                    this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For WELL_HEAD
        bndElem => this%well_head(imaw)
        call read_value_or_time_series_adv(text, imaw, jj, bndElem, this%name,   &
                                           'BND', this%tsManager, this%iprpak,   &
                                           'WELL_HEAD')
        !
        ! -- set xnewpak to well_head
        this%xnewpak(imaw) = this%well_head(imaw)
        !
        ! -- check for error condition
        if (this%well_head(imaw) < this%bot(imaw)) then
          write(cstr, fmthdbot)                                                  &
            this%well_head(imaw), this%bot(imaw)
          call this%maw_set_attribute_error(imaw, 'WELL HEAD', trim(cstr))
        end if
      case ('FLOWING_WELL')
        call urword(line, lloc, istart, istop, 3, ival, rval,                    &
                    this%iout, this%inunit)
        this%fwelev(imaw) = rval
        call urword(line, lloc, istart, istop, 3, ival, rval,                    &
                    this%iout, this%inunit)
        this%fwcond(imaw) = rval
        call urword(line, lloc, istart, istop, 3, ival, rval,                    &
                    -this%iout, this%inunit)
        this%fwrlen(imaw) = rval
      case ('RATE_SCALING')
        call urword(line, lloc, istart, istop, 3, ival, rval,                    &
                    this%iout, this%inunit)
        this%pumpelev(imaw) = rval
        call urword(line, lloc, istart, istop, 3, ival, rval,                    &
                    this%iout, this%inunit)
        this%reduction_length(imaw) = rval
        if (rval < DZERO) then
          call this%maw_set_attribute_error(imaw, trim(keyword),                 &
            'must be greater than or equal to 0.')
        end if
      case ('HEAD_LIMIT')
        call urword(line, lloc, istart, istop, 1, ival, rval,                    &
                    this%iout, this%inunit)
        if (line(istart:istop) == 'OFF') then
          this%shutofflevel(imaw) = DEP20
        else
          read (line(istart:istop), *, iostat=istat, iomsg=errmsgr)              &
            this%shutofflevel(imaw)
          if (istat /= 0) then
            errmsg = 'Could not read HEAD_LIMIT value. ' // trim(errmsgr)
            call store_error(errmsg)
          end if
        end if
      case ('SHUT_OFF')
        call urword(line, lloc, istart, istop, 3, ival, rval,                    &
                    this%iout, this%inunit)
        this%shutoffmin(imaw) = rval
        call urword(line, lloc, istart, istop, 3, ival, rval,                    &
                    this%iout, this%inunit)
        this%shutoffmax(imaw) = rval
      case ('AUXILIARY')
        call urword(line, lloc, istart, istop, 1, ival, rval,                    &
                    this%iout, this%inunit)
        caux = line(istart:istop)
        do jj = 1, this%naux
          if (trim(adjustl(caux)) /= trim(adjustl(this%auxname(jj)))) cycle
          call urword(line, lloc, istart, istop, 0, ival, rval,                  &
                      this%iout, this%inunit)
          text = line(istart:istop)
          ii = imaw
          bndElem => this%mauxvar(jj, ii)
          call read_value_or_time_series_adv(text, ii, jj, bndElem, this%name,   &
                                             'AUX', this%tsManager, this%iprpak, &
                                             this%auxname(jj))
          exit
        end do
      case default
        write(errmsg,'(a,a)') &
          'Unknown ' // trim(this%text) // " maw data keyword: '",               &
          line(istart:istop) // "'."
        call store_error(errmsg)
      end select

    !
    ! -- return
    return
  end subroutine maw_set_stressperiod


  subroutine maw_set_attribute_error(this, imaw, keyword, msg)
! ******************************************************************************
! maw_set_attribute_error -- Issue a parameter error for mawweslls(imaw)
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SimModule, only: store_error
    ! -- dummy
    class(MawType),intent(inout) :: this
    integer(I4B), intent(in) :: imaw
    character (len=*), intent(in) :: keyword
    character (len=*), intent(in) :: msg
    ! -- local
    character(len=LINELENGTH) :: errmsg
    ! -- formats
! ------------------------------------------------------------------------------
    if (len(msg) == 0) then
      write(errmsg,'(4x,a,1x,a,1x,a,1x,i0,1x,a)') &
        '****ERROR.', keyword, ' for MAW well', imaw, 'has already been set.'
    else
      write(errmsg,'(4x,a,1x,a,1x,a,1x,i0,1x,a)') &
        '****ERROR.', keyword, ' for MAW well', imaw, msg
    end if
    call store_error(errmsg)
    ! -- return
    return
  end subroutine maw_set_attribute_error


  subroutine maw_check_attributes(this)
! ******************************************************************************
! maw_check_attributes -- Issue parameter errors for mawwells(imaw)
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SimModule, only: store_error
    ! -- dummy
    class(MawType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: cgwfnode
    integer(I4B) :: idx
    integer(I4B) :: n
    integer(I4B) :: j
    ! -- formats
! ------------------------------------------------------------------------------
    idx = 1
    do n = 1, this%nmawwells
      if (this%ngwfnodes(n) < 1) then
        call this%maw_set_attribute_error(n, 'NGWFNODES', 'must be greater ' //  &
                                          'than 0.')
      end if
      if (this%radius(n) == DEP20) then
        call this%maw_set_attribute_error(n, 'RADIUS', 'has not been specified.')
      end if
      if (this%shutoffmin(n) > DZERO) then
        if (this%shutoffmin(n) >= this%shutoffmax(n)) then
          call this%maw_set_attribute_error(n, 'SHUT_OFF', 'shutoffmax must ' // &
                                            'be greater than shutoffmin.')
        end if
      end if
      do j = 1, this%ngwfnodes(n)
        !
        ! -- write gwfnode number
        write(cgwfnode,'(a,i0,a)') 'gwfnode(', j,')'
        !
        ! -- connection screen data
        if (this%mawwells(n)%botscrn(j) >= this%mawwells(n)%topscrn(j)) then
          call this%maw_set_attribute_error(n, 'SCREEN_TOP', 'screen bottom ' // &
                                            'must be less tha screen top. ' //   &
                                            trim(cgwfnode))
        end if
        !
        ! -- connection skin radius
        if (this%ieqn(n) == 2 .OR. this%ieqn(n) == 3 .OR.                        &
            this%ieqn(n) == 4) then
          if (this%mawwells(n)%sradius(j) > DZERO) then
            if (this%mawwells(n)%sradius(j) <= this%radius(n)) then
              call this%maw_set_attribute_error(n, 'RADIUS_SKIN', 'skin ' //     &
                                                'radius must be greater ' //     &
                                                'than or equal to well ' //      &
                                                'radius. ' // trim(cgwfnode))
            end if
          end if
          !
          ! -- skin hydraulic conductivity
          if (this%mawwells(n)%hk(j) <= DZERO) then
            call this%maw_set_attribute_error(n, 'HK_SKIN', 'skin hyraulic ' //  &
                                              'conductivity must be greater ' // &
                                              'than zero. ' // trim(cgwfnode))
          end if
        else if (this%ieqn(n) == 0) then
          !
          ! -- saturated conductance
          if (this%mawwells(n)%satcond(j) < DZERO) then
            call this%maw_set_attribute_error(n, 'HK_SKIN',                      &
                                              'skin hyraulic conductivity ' //   &
                                              'must be greater than or ' //      &
                                              'equal to zero when using ' //     &
                                              'SPECIFIED condeqn. ' //           &
                                              trim(cgwfnode))
          end if    
        end if
        idx = idx + 1
      end do
    end do
    ! -- reset check_attr
    this%check_attr = 0
    ! -- return
    return
  end subroutine maw_check_attributes

  subroutine maw_ac(this, moffset, sparse)
! ******************************************************************************
! bnd_ac -- Add package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(MawType),intent(inout) :: this
    integer(I4B), intent(in) :: moffset
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    integer(I4B) :: j, n
    integer(I4B) :: jj, jglo
    integer(I4B) :: nglo
    ! -- format
! ------------------------------------------------------------------------------
    !
    !
    ! -- Add package rows to sparse
    do n = 1, this%nmawwells
      nglo = moffset + this%dis%nodes + this%ioffset + n
      call sparse%addconnection(nglo, nglo, 1)
      do j = 1, this%ngwfnodes(n)
        jj = this%mawwells(n)%gwfnodes(j)
        jglo = jj + moffset
        call sparse%addconnection(nglo, jglo, 1)
        call sparse%addconnection(jglo, nglo, 1)
      end do

    end do
    !
    ! -- return
    return
  end subroutine maw_ac

  subroutine maw_mc(this, moffset, iasln, jasln)
! ******************************************************************************
! bnd_ac -- map package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(MawType),intent(inout) :: this
    integer(I4B), intent(in) :: moffset
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
    integer(I4B) :: n, j, ii, jj, iglo, jglo
    integer(I4B) :: ipos
    ! -- format
! ------------------------------------------------------------------------------
    !
    !
    allocate(this%idxlocnode(this%nmawwells))
    allocate(this%idxdglo(this%maxbound))
    allocate(this%idxoffdglo(this%maxbound))
    allocate(this%idxsymdglo(this%maxbound))
    allocate(this%idxsymoffdglo(this%maxbound))
    !
    ! -- Find the position of each connection in the global ia, ja structure
    !    and store them in idxglo.  idxglo allows this model to insert or
    !    retrieve values into or from the global A matrix
    ! -- maw rows
    ipos = 1
    do n = 1, this%nmawwells
      iglo = moffset + this%dis%nodes + this%ioffset + n
      this%idxlocnode(n) = this%dis%nodes + this%ioffset + n
      do ii = 1, this%ngwfnodes(n)
        j =  this%mawwells(n)%gwfnodes(ii)
        jglo = j + moffset
        searchloop: do jj = iasln(iglo), iasln(iglo + 1) - 1
          if(jglo == jasln(jj)) then
            this%idxdglo(ipos) = iasln(iglo)
            this%idxoffdglo(ipos) = jj
            exit searchloop
          end if
        end do searchloop
        ipos = ipos + 1
      end do
    end do
    ! -- maw contributions gwf portion of global matrix
    ipos = 1
    do n = 1, this%nmawwells
      do ii = 1, this%ngwfnodes(n)
        iglo = this%mawwells(n)%gwfnodes(ii) + moffset
        jglo = moffset + this%dis%nodes + this%ioffset + n
        symsearchloop: do jj = iasln(iglo), iasln(iglo + 1) - 1
          if(jglo == jasln(jj)) then
            this%idxsymdglo(ipos) = iasln(iglo)
            this%idxsymoffdglo(ipos) = jj
            exit symsearchloop
          end if
        end do symsearchloop
        ipos = ipos + 1
      end do
    end do
    !
    ! -- return
    return
  end subroutine maw_mc

  subroutine maw_options(this, option, found)
! ******************************************************************************
! maw_options -- set options specific to MawType
!
! maw_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: MAXCHARLEN, DZERO
    use OpenSpecModule, only: access, form
    use InputOutputModule, only: urword, getunit, openfile
    ! -- dummy
    class(MawType),   intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical,          intent(inout) :: found
    ! -- local
    character(len=MAXCHARLEN) :: fname, keyword
    ! -- formats
    character(len=*),parameter :: fmtflowingwells = &
      "(4x, 'FLOWING WELLS WILL BE SIMULATED.')"
    character(len=*),parameter :: fmtshutdown = &
      "(4x, 'SHUTDOWN ', a, ' VALUE (',g15.7,') SPECIFIED.')"
    character(len=*),parameter :: fmtnostoragewells = &
      "(4x, 'WELL STORAGE WILL NOT BE SIMULATED.')"
    character(len=*),parameter :: fmtmawbin = &
      "(4x, 'MAW ', 1x, a, 1x, ' WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
! ------------------------------------------------------------------------------
    !
    ! -- Check for 'FLOWING_WELLS' and set this%iflowingwells
    select case (option)
      case ('PRINT_HEAD')
        this%iprhed = 1
        write(this%iout,'(4x,a)') trim(adjustl(this%text))// &
          ' HEADS WILL BE PRINTED TO LISTING FILE.'
        found = .true.
      case('HEAD')
        call this%parser%GetStringCaps(keyword)
        if (keyword == 'FILEOUT') then
          call this%parser%GetString(fname)
          this%iheadout = getunit()
          call openfile(this%iheadout, this%iout, fname, 'DATA(BINARY)',  &
                       form, access, 'REPLACE')
          write(this%iout,fmtmawbin) 'HEAD', fname, this%iheadout
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
          write(this%iout,fmtmawbin) 'BUDGET', fname, this%ibudgetout
          found = .true.
        else
          call store_error('OPTIONAL BUDGET KEYWORD MUST BE FOLLOWED BY FILEOUT')
        end if
      case('FLOWING_WELLS')
        this%iflowingwells = 1
        !
        ! -- Write option and return with found set to true
        if(this%iflowingwells > 0) &
          write(this%iout, fmtflowingwells)
        found = .true.
      case('SHUTDOWN_THETA')
        this%theta = this%parser%GetDouble()
        write(this%iout, fmtshutdown) 'THETA', this%theta
        found = .true.
      case('SHUTDOWN_KAPPA')
        this%kappa = this%parser%GetDouble()
        write(this%iout, fmtshutdown) 'KAPPA', this%kappa
        found = .true.
      case('MOVER')
        this%imover = 1
        write(this%iout, '(4x,A)') 'MOVER OPTION ENABLED'
        found = .true.
      case('NO_WELL_STORAGE')
        this%imawissopt = 1
        write(this%iout, fmtnostoragewells)
        found = .true.
      !
      ! -- right now these are options that are only available in the
      !    development version and are not included in the documentation.
      !    These options are only available when IDEVELOPMODE in
      !    constants module is set to 1
      case('DEV_PEACEMAN_EFFECTIVE_RADIUS')
        call this%parser%DevOpt()
        this%ieffradopt = 1
        write(this%iout, '(4x,a)')                                             &
     &    'EFFECTIVE RADIUS FOR STRUCTURED GRIDS WILL BE CALCULATED ' //       &
     &    'USING PEACEMAN 1983'
        found = .true.
      case default
        !
        ! -- No options found
        found = .false.
    end select
    !
    ! -- return
    return
  end subroutine maw_options

  subroutine maw_ar(this)
  ! ******************************************************************************
  ! maw_ar -- Allocate and Read
  ! Subroutine: (1) create new-style package
  !             (2) point bndobj to the new package
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      ! -- dummy
      class(MawType),intent(inout) :: this
      ! -- local
      ! -- format
  ! ------------------------------------------------------------------------------
    !
    call this%obs%obs_ar()
    !
    ! -- set omega value used for saturation calculations
    if (this%inewton > 0) then
      this%satomega = DEM6
    end if
    !
    ! -- Allocate connection arrays in MAW and in package superclass
    call this%maw_allocate_conn_arrays()
    !
    ! -- read optional initial package parameters
    call this%read_initial_attr()
    !
    ! -- setup pakmvrobj
    if (this%imover /= 0) then
      allocate(this%pakmvrobj)
      call this%pakmvrobj%ar(this%nmawwells, this%nmawwells, this%origin)
    end if
    !
    ! -- return
    return
  end subroutine maw_ar


  subroutine maw_rp(this)
! ******************************************************************************
! maw_rp -- Read and Prepare
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use TdisModule, only: kper, nper
    ! -- dummy
    class(MawType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: text
    character(len=LINELENGTH) :: errmsg
    character (len=16) :: csteady
    integer(I4B) :: ierr
    integer(I4B) :: node, n
    logical :: isfound, endOfBlock
    integer(I4B) :: ntabcols
    integer(I4B) :: ntabrows
    integer(I4B) :: imaw
    integer(I4B) :: ibnd
    integer(I4B) :: j
    !integer(I4B) :: isfirst
    ! -- formats
    character(len=*),parameter :: fmtblkerr = &
      "('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*),parameter :: fmtlsp = &
      "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
! ------------------------------------------------------------------------------
    !
    ! -- set steady-state flag based on gwfiss
    this%imawiss = this%gwfiss
    ! -- reset maw steady flag if 'STEADY-STATE' specified in the OPTIONS block
    if (this%imawissopt == 1) then
      this%imawiss = 1
    end if
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
          write(errmsg, fmtblkerr) adjustl(trim(line))
          call store_error(errmsg)
          call ustop()
        end if
      end if
    end if
    !
    ! -- Read data if ionper == kper
    if(this%ionper == kper) then
      !
      ! -- setup table for period data
      if (this%iprpak /= 0) then
        !
        ! -- reset the input table object
        title = trim(adjustl(this%text)) // ' PACKAGE (' //                      &
                trim(adjustl(this%name)) //') DATA FOR PERIOD'
        write(title, '(a,1x,i6)') trim(adjustl(title)), kper
        call table_cr(this%inputtab, this%name, title)
        call this%inputtab%table_df(1, 5, this%iout, finalize=.FALSE.)
        text = 'NUMBER'
        call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
        text = 'KEYWORD'
        call this%inputtab%initialize_column(text, 20, alignment=TABLEFT)
        do n = 1, 3
          write(text, '(a,1x,i6)') 'VALUE', n
          call this%inputtab%initialize_column(text, 15, alignment=TABCENTER)
        end do
      end if
      !
      ! -- set flag to check attributes
      this%check_attr = 1
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit

        imaw = this%parser%GetInteger()
        if (imaw < 1 .or. imaw > this%nmawwells) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. IMAW MUST BE > 0 and <= ', this%nmawwells
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end if

        call this%parser%GetRemainingLine(line)
        call this%maw_set_stressperiod(imaw, line)
        !
        ! -- write line to table
        if (this%iprpak /= 0) then
          call this%inputtab%add_term(imaw)
          call this%inputtab%line_to_columns(line)
        end if
      end do
      if (this%iprpak /= 0) then
        call this%inputtab%finalize_table()
        write(this%iout,'(/1x,a,1x,i6,/)')                                      &
          'END OF '//trim(adjustl(this%text))//' DATA FOR PERIOD', kper
      end if
    !
    else
      write(this%iout,fmtlsp) trim(this%filtyp)
    end if
    !
    !write summary of maw well stress period error messages
    if (count_errors() > 0) then
      call ustop()
    end if
    !
    ! -- qa data if necessary
    if (this%check_attr /= 0) then
      call this%maw_check_attributes()

      ! -- write summary of stress period data for MAW
      if (this%iprpak == 1) then
        if (this%imawiss /= 0) then
          csteady = 'STEADY-STATE    '
        else
          csteady = 'TRANSIENT       '
        end if
        !
        ! -- reset the input table object for rate data
        title = trim(adjustl(this%text)) // ' PACKAGE (' //                      &
                trim(adjustl(this%name)) //') ' // trim(adjustl(csteady)) //     &
                ' RATE DATA FOR PERIOD'
        write(title, '(a,1x,i6)') trim(adjustl(title)), kper
        ntabcols = 6
        call table_cr(this%inputtab, this%name, title)
        call this%inputtab%table_df(this%nmawwells, ntabcols, this%iout)
        text = 'NUMBER'
        call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)      
        text = 'STATUS'
        call this%inputtab%initialize_column(text, 12, alignment=TABCENTER)      
        text = 'RATE'
        call this%inputtab%initialize_column(text, 12, alignment=TABCENTER)      
        text = 'SPECIFIED HEAD'
        call this%inputtab%initialize_column(text, 12, alignment=TABCENTER)      
        text = 'PUMP ELEVATION'
        call this%inputtab%initialize_column(text, 12, alignment=TABCENTER)      
        text = 'REDUCTION LENGTH'
        call this%inputtab%initialize_column(text, 12, alignment=TABCENTER)
        do n = 1, this%nmawwells
          call this%inputtab%add_term(n)
          call this%inputtab%add_term(this%status(n))
          call this%inputtab%add_term(this%rate(n))
          if (this%iboundpak(n) < 0) then
            call this%inputtab%add_term(this%well_head(n))
          else
            call this%inputtab%add_term(' ')
          end if
          call this%inputtab%add_term(this%pumpelev(n))
          if (this%reduction_length(n) /= DEP20) then
            call this%inputtab%add_term(this%reduction_length(n))
          else
            call this%inputtab%add_term(' ')
          end if
        end do
        !
        ! -- flowing wells
        if (this%iflowingwells /= 0) then
          !
          ! -- reset the input table object for flowing well data
          title = trim(adjustl(this%text)) // ' PACKAGE (' //                    &
                  trim(adjustl(this%name)) //') ' // trim(adjustl(csteady)) //   &
                  ' FLOWING WELL DATA FOR PERIOD'
          write(title, '(a,1x,i6)') trim(adjustl(title)), kper
          ntabcols = 4
          ntabrows = 0
          do n = 1, this%nmawwells
            if (this%fwcond(n) > DZERO) then
              ntabrows = ntabrows + 1
            end if
          end do
          if (ntabrows > 0) then
            call table_cr(this%inputtab, this%name, title)
            call this%inputtab%table_df(ntabrows, ntabcols, this%iout)
            text = 'NUMBER'
            call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)      
            text = 'ELEVATION'
            call this%inputtab%initialize_column(text, 12, alignment=TABCENTER)      
            text = 'CONDUCT.'
            call this%inputtab%initialize_column(text, 12, alignment=TABCENTER)      
            text = 'REDUCTION LENGTH'
            call this%inputtab%initialize_column(text, 12, alignment=TABCENTER)
            do n = 1, this%nmawwells
              if (this%fwcond(n) > DZERO) then
                call this%inputtab%add_term(n)
                call this%inputtab%add_term(this%fwelev(n))
                call this%inputtab%add_term(this%fwcond(n))
                call this%inputtab%add_term(this%fwrlen(n))
              end if
            end do
          end if
        end if
        !
        ! -- reset the input table object for shutoff data
        title = trim(adjustl(this%text)) // ' PACKAGE (' //                      &
                trim(adjustl(this%name)) //') '// trim(adjustl(csteady)) //      &
                ' WELL SHUTOFF DATA FOR PERIOD'
        write(title, '(a,1x,i6)') trim(adjustl(title)), kper
        ntabcols = 4
        ntabrows = 0
        do n = 1, this%nmawwells
          if (this%shutofflevel(n) /= DEP20) then
            ntabrows = ntabrows + 1
          end if
        end do
        if (ntabrows > 0) then
          call table_cr(this%inputtab, this%name, title)
          call this%inputtab%table_df(ntabrows, ntabcols, this%iout)
          text = 'NUMBER'
          call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)      
          text = 'ELEVATION'
          call this%inputtab%initialize_column(text, 12, alignment=TABCENTER)      
          text = 'MINIMUM. Q'
          call this%inputtab%initialize_column(text, 12, alignment=TABCENTER)      
          text = 'MAXIMUM Q'
          call this%inputtab%initialize_column(text, 12, alignment=TABCENTER)
          do n = 1, this%nmawwells
            if (this%shutofflevel(n) /= DEP20) then
              call this%inputtab%add_term(n)
              call this%inputtab%add_term(this%shutofflevel(n))
              call this%inputtab%add_term(this%shutoffmin(n))
              call this%inputtab%add_term(this%shutoffmax(n))
            end if
          end do
        end if
      end if
    end if
    !
    ! -- fill arrays
    ibnd = 1
    do n = 1, this%nmawwells
      do j = 1, this%ngwfnodes(n)
        node = this%mawwells(n)%gwfnodes(j)
        this%nodelist(ibnd) = node

        this%bound(1,ibnd) = this%xnewpak(n)

        this%bound(2,ibnd) = this%mawwells(n)%satcond(j)

        this%bound(3,ibnd) = this%mawwells(n)%botscrn(j)

        if (this%iboundpak(n) > 0) then
          this%bound(4,ibnd) = this%rate(n)
        else
          this%bound(4,ibnd) = DZERO
        end if
        ibnd = ibnd + 1
      end do
    end do
    !
    ! -- return
    return
  end subroutine maw_rp

  subroutine maw_ad(this)
! ******************************************************************************
! maw_ad -- Add package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only : kper, kstp
    ! -- dummy
    class(MawType) :: this
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: j
    integer(I4B) :: jj
    integer(I4B) :: ibnd
! ------------------------------------------------------------------------------
    !
    ! -- Advance the time series
    call this%TsManager%ad()
    !
    ! -- update auxiliary variables by copying from the derived-type time
    !    series variable into the bndpackage auxvar variable so that this
    !    information is properly written to the GWF budget file
    if (this%naux > 0) then
      ibnd = 1
      do n = 1, this%nmawwells
        do j = 1, this%ngwfnodes(n)
          do jj = 1, this%naux
            if (this%noupdateauxvar(jj) /= 0) cycle
            this%auxvar(jj, ibnd) = this%mauxvar(jj, n)
          end do
          ibnd = ibnd + 1
        end do
      end do
    end if
    !
    ! -- copy xnew into xold
    do n = 1, this%nmawwells
      this%xoldpak(n) = this%xnewpak(n)
      this%xoldsto(n) = this%xsto(n)
      if (this%iboundpak(n) < 0) then
        this%xnewpak(n) = this%well_head(n)
      end if
    end do
    !
    !--use the appropriate xoldsto if intial heads are above the
    !  specified flowing well discharge elevation
    if (kper==1 .and. kstp==1) then
      do n = 1, this%nmawwells
        if (this%fwcond(n) > DZERO) then
          if (this%xoldsto(n) > this%fwelev(n)) then
            this%xoldsto(n) = this%fwelev(n)
          end if
        end if
      end do
    end if
    !
    ! -- reset ishutoffcnt (equivalent to kiter) to zero
    this%ishutoffcnt = 0
    !
    ! -- pakmvrobj ad
    if(this%imover == 1) then
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
  end subroutine maw_ad

  subroutine maw_cf(this, reset_mover)
  ! ******************************************************************************
  ! maw_cf -- Formulate the HCOF and RHS terms
  ! Subroutine: (1) skip if no multi-aquifer wells
  !             (2) calculate hcof and rhs
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    ! -- dummy
    class(MawType) :: this
    logical, intent(in), optional :: reset_mover
    ! -- local
    logical :: lrm
  ! ------------------------------------------------------------------------------
    !
    ! -- Calculate maw conductance and update package RHS and HCOF
    call this%maw_cfupdate()
    !
    ! -- pakmvrobj cf
    lrm = .true.
    if (present(reset_mover)) lrm = reset_mover
    if(this%imover == 1 .and. lrm) then
      call this%pakmvrobj%cf()
    end if
    !
    ! -- Return
    return
  end subroutine maw_cf

  subroutine maw_fc(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! maw_fc -- Copy rhs and hcof into solution rhs and amat
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule,only: delt
    ! -- dummy
    class(MawType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: j, n
    integer(I4B) :: idx
    integer(I4B) :: iloc, isymloc
    integer(I4B) :: igwfnode
    integer(I4B) :: iposd, iposoffd
    integer(I4B) :: isymnode
    integer(I4B) :: ipossymd, ipossymoffd
    real(DP) :: hmaw
    real(DP) :: bmaw
    real(DP) :: bnode
    real(DP) :: sat
    real(DP) :: cfw
    real(DP) :: cmaw
    real(DP) :: cterm
    real(DP) :: scale
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: rate
    real(DP) :: ratefw
! --------------------------------------------------------------------------
    !
    ! -- pakmvrobj fc
    if(this%imover == 1) then
      call this%pakmvrobj%fc()
    end if
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    idx = 1
    do n = 1, this%nmawwells
      iloc = this%idxlocnode(n)
      !
      ! -- update head value for constant head maw wells
      if (this%iboundpak(n) < 0) then
        this%xnewpak(n) = this%well_head(n)
      end if
      hmaw = this%xnewpak(n)
      !
      ! -- add pumping rate to active or constant maw well
      if (this%iboundpak(n) == 0) then
        this%ratesim(n) = DZERO
      else
        call this%maw_calculate_wellq(n, hmaw, rate)
        this%ratesim(n) = rate
        rhs(iloc) = rhs(iloc) - rate
        !
        ! -- location of diagonal for maw row
        iposd = this%idxdglo(idx)
        ! -- add flowing well
        this%xsto(n) = hmaw
        ratefw = DZERO
        if (this%iflowingwells > 0) then
          if (this%fwcond(n) > DZERO) then
            bt = this%fwelev(n)
            tp = bt + this%fwrlen(n)
            scale = sQSaturation(tp, bt, hmaw)
            cfw = scale * this%fwcond(n)
            this%ifwdischarge(n) = 0
            if (cfw > DZERO) then
              this%ifwdischarge(n) = 1
              this%xsto(n) = bt
            end if
            this%fwcondsim(n) = cfw
            amatsln(iposd) = amatsln(iposd) - cfw
            rhs(iloc) = rhs(iloc) - cfw * bt
            ratefw = cfw * (bt - hmaw)
          end if
        end if
        !
        ! -- add maw storage changes
        if (this%imawiss /= 1) then
          if (this%ifwdischarge(n) /= 1) then
            amatsln(iposd) = amatsln(iposd) - (this%area(n) / delt)
            rhs(iloc) = rhs(iloc) - (this%area(n) * this%xoldsto(n) / delt)
          else
            cterm = this%xoldsto(n) - this%fwelev(n)
            rhs(iloc) = rhs(iloc) - (this%area(n) * cterm / delt)
          end if
        end if
        !
        ! -- If mover is active, add receiver water to rhs and
        !    store available water (as positive value)
        if(this%imover == 1) then
          rhs(iloc) = rhs(iloc) - this%pakmvrobj%get_qfrommvr(n)
          call this%pakmvrobj%accumulate_qformvr(n, -rate)  !pumped water
          call this%pakmvrobj%accumulate_qformvr(n, -ratefw) !flowing water
        end if
        !
      end if
      do j = 1, this%ngwfnodes(n)
        if (this%iboundpak(n) /= 0) then
          igwfnode = this%mawwells(n)%gwfnodes(j)
          call this%maw_calculate_saturation(n, j, igwfnode, sat)
          cmaw = this%mawwells(n)%satcond(j) * sat
          this%mawwells(n)%simcond(j) = cmaw

          bnode = this%dis%bot(igwfnode)
          bmaw = this%mawwells(n)%botscrn(j)
          !
          ! -- calculate cterm - relative to gwf
          cterm = DZERO
          if (hmaw < bmaw) then
            cterm = cmaw * (bmaw - hmaw)
          end if
          !
          ! -- add to maw row
          iposd = this%idxdglo(idx)
          iposoffd = this%idxoffdglo(idx)
          amatsln(iposd) = amatsln(iposd) - cmaw
          amatsln(iposoffd) = cmaw
          !
          ! -- add correction term
          rhs(iloc) = rhs(iloc) + cterm
          !
          ! -- add to gwf row for maw connection
          isymnode = this%mawwells(n)%gwfnodes(j)
          isymloc = ia(isymnode)
          ipossymd = this%idxsymdglo(idx)
          ipossymoffd = this%idxsymoffdglo(idx)
          amatsln(ipossymd) = amatsln(ipossymd) - cmaw
          amatsln(ipossymoffd) = cmaw
          !
          ! -- add correction term
          rhs(isymnode) = rhs(isymnode) - cterm
        end if
        ! -- increment maw connection counter
        idx = idx + 1
      end do
    end do
    !
    ! -- return
    return
  end subroutine maw_fc

  subroutine maw_fn(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! maw_fn -- Fill newton terms
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    use TdisModule,only:delt
    ! -- dummy
    class(MawType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: j, n
    integer(I4B) :: idx
    integer(I4B) :: iloc, isymloc
    integer(I4B) :: igwfnode
    integer(I4B) :: iposd, iposoffd
    integer(I4B) :: isymnode
    integer(I4B) :: ipossymd, ipossymoffd
    real(DP) :: hmaw
    real(DP) :: tmaw
    real(DP) :: bmaw
    real(DP) :: sat
    real(DP) :: cmaw
    real(DP) :: scale
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: cfw
    real(DP) :: rate
    real(DP) :: rate2
    real(DP) :: rterm
    real(DP) :: derv
    real(DP) :: drterm
    real(DP) :: hgwf
    real(DP) :: hups
    real(DP) :: term
! --------------------------------------------------------------------------
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    idx = 1
    do n = 1, this%nmawwells
      iloc = this%idxlocnode(n)
      hmaw = this%xnewpak(n)
      !
      ! -- add pumping rate to active or constant maw well
      if (this%iboundpak(n) /= 0) then
        iposd = this%idxdglo(idx)
        scale = DONE
        drterm = DZERO
        rate = this%ratesim(n)
        !
        !-- calculate final derivative for pumping rate
        call this%maw_calculate_wellq(n, hmaw+DEM4, rate2)
        drterm = (rate2 - rate) / DEM4
        !
        !-- fill amat and rhs with newton-raphson terms
        amatsln(iposd) = amatsln(iposd) + drterm
        rhs(iloc) = rhs(iloc) + drterm * hmaw
        !
        ! -- add flowing well
        if (this%iflowingwells > 0) then
          if (this%fwcond(n) > DZERO) then
            bt = this%fwelev(n)
            tp = bt + this%fwrlen(n)
            scale = sQSaturation(tp, bt, hmaw)
            cfw = scale * this%fwcond(n)
            this%ifwdischarge(n) = 0
            if (cfw > DZERO) then
              this%ifwdischarge(n) = 1
            end if
            this%fwcondsim(n) = cfw
            rate = cfw * (bt - hmaw)
            rterm = -cfw * hmaw
            !
            ! --calculate derivative for flowing well
            if (hmaw < tp) then
              derv = sQSaturationDerivative(tp, bt, hmaw)
              drterm = -(cfw + this%fwcond(n) * derv * (hmaw - bt))
              !
              ! -- fill amat and rhs with newton-raphson terms
              amatsln(iposd) = amatsln(iposd) -                                &
                               this%fwcond(n) * derv * (hmaw - bt)
              rhs(iloc) = rhs(iloc) - rterm + drterm * hmaw
            end if
          end if
        end if
        !
        ! -- add maw storage changes
        if (this%imawiss /= 1) then
          if (this%ifwdischarge(n) /= 1) then
            rate = this%area(n) * hmaw / delt
            rterm = -rate
            !
            ! -- calculate storage derivative
            drterm = -this%area(n) / delt
            !
            ! -- fill amat and rhs with storage components
            rhs(iloc) = rhs(iloc) - rterm + drterm * hmaw
          end if
        end if
      end if
      do j = 1, this%ngwfnodes(n)
        if (this%iboundpak(n) /= 0) then
          igwfnode = this%mawwells(n)%gwfnodes(j)
          hgwf = this%xnew(igwfnode)
          !
          ! -- calculate upstream weighted conductance
          call this%maw_calculate_saturation(n, j, igwfnode, sat)
          cmaw = this%mawwells(n)%satcond(j) * sat
          this%mawwells(n)%simcond(j) = cmaw
          !
          ! -- set top and bottom of the screen
          tmaw = this%mawwells(n)%topscrn(j)
          bmaw = this%mawwells(n)%botscrn(j)
          !
          ! -- add to maw row
          iposd = this%idxdglo(idx)
          iposoffd = this%idxoffdglo(idx)
          !
          ! -- add to gwf row for maw connection
          isymnode = this%mawwells(n)%gwfnodes(j)
          isymloc = ia(isymnode)
          ipossymd = this%idxsymdglo(idx)
          ipossymoffd = this%idxsymoffdglo(idx)
          !
          ! -- calculate newton corrections
          hups = hmaw
          if (hgwf > hups) hups = hgwf
          drterm = sQuadraticSaturationDerivative(tmaw, bmaw, hups, this%satomega)
          !
          ! -- maw is upstream
          if (hmaw > hgwf) then
            term = drterm * this%mawwells(n)%satcond(j) * (hmaw - hgwf)
            rhs(iloc) = rhs(iloc) + term * hmaw
            rhs(isymnode) = rhs(isymnode) - term * hmaw
            amatsln(iposd) = amatsln(iposd) + term
            if (this%ibound(igwfnode) > 0) then
              amatsln(ipossymoffd) = amatsln(ipossymoffd) - term
            end if
          !
          ! -- gwf is upstream
          else
            term = -drterm * this%mawwells(n)%satcond(j) * (hgwf - hmaw)
            rhs(iloc) = rhs(iloc) + term * hgwf
            rhs(isymnode) = rhs(isymnode) - term * hgwf
            if (this%iboundpak(n) > 0) then
              amatsln(iposoffd) = amatsln(iposoffd) + term
            end if
            amatsln(ipossymd) = amatsln(ipossymd) - term
          end if
        end if
        !
        ! -- increment maw connection counter
        idx = idx + 1
      end do
    end do
    !
    ! -- return
    return
  end subroutine maw_fn


  subroutine maw_nur(this, neqpak, x, xtemp, dx, inewtonur, dxmax, locmax)
! ******************************************************************************
! maw_nur -- under-relaxation
! Subroutine: (1) Under-relaxation of Groundwater Flow Model MAW Package Heads
!                 for current outer iteration using the well bottom
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(MawType), intent(inout) :: this
    integer(I4B), intent(in) :: neqpak
    real(DP), dimension(neqpak), intent(inout) :: x
    real(DP), dimension(neqpak), intent(in) :: xtemp
    real(DP), dimension(neqpak), intent(inout) :: dx
    integer(I4B), intent(inout) :: inewtonur
    real(DP), intent(inout) :: dxmax
    integer(I4B), intent(inout) :: locmax
    ! -- local
    integer(I4B) :: n
    real(DP) :: botw
    real(DP) :: xx
    real(DP) :: dxx
! ------------------------------------------------------------------------------

    !
    ! -- Newton-Raphson under-relaxation
    do n = 1, this%nmawwells
      if (this%iboundpak(n) < 1) cycle
      botw = this%bot(n)
      !
      ! -- only apply Newton-Raphson under-relaxation if
      !    solution head is below the bottom of the well
      if (x(n) < botw) then
        inewtonur = 1
        xx = xtemp(n)*(DONE-DP9) + botw*DP9
        dxx = x(n) - xx
        if (abs(dxx) > abs(dxmax)) then
          locmax = n
          dxmax = dxx
        end if
        x(n) = xx
        dx(n) = DZERO
      end if
    end do
    !
    ! -- return
    return
  end subroutine maw_nur


  subroutine maw_bd(this, x, idvfl, icbcfl, ibudfl, icbcun, iprobs,            &
                    isuppress_output, model_budget, imap, iadv)
! ******************************************************************************
! bnd_bd -- Calculate Volumetric Budget
! Note that the compact budget will always be used.
! Subroutine: (1) Process each package entry
!             (2) Write output
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper, delt, pertim, totim
    use ConstantsModule, only: LENBOUNDNAME
    use InputOutputModule, only: ulasav, ubdsv06
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(MawType) :: this
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
    ! -- for budget
    integer(I4B) :: j, n
    integer(I4B) :: igwfnode
    integer(I4B) :: ibnd
    real(DP) :: hmaw, hgwf
    real(DP) :: cfw
    real(DP) :: bmaw, cmaw
    real(DP) :: cterm
    real(DP) :: v
    real(DP) :: d
    ! -- for observations
    integer(I4B) :: iprobslocal
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- recalculate package HCOF and RHS terms with latest groundwater and
    !    maw heads prior to calling base budget functionality
    call this%maw_cfupdate()
    !
    ! -- Suppress saving of simulated values; they
    !    will be saved at end of this procedure.
    iprobslocal = 0
    !
    ! -- call base functionality in bnd_bd
    call this%BndType%bnd_bd(x, idvfl, icbcfl, ibudfl, icbcun, iprobslocal,    &
                             isuppress_output, model_budget, this%imap,        &
                             iadv=1)
    !
    ! -- calculate maw budget flow and storage terms
    do n = 1, this%nmawwells
      this%qout(n) = DZERO
      this%qsto(n) = DZERO
      if (this%iflowingwells > 0) this%qfw(n) = DZERO
      if (this%iboundpak(n) == 0) cycle
      !
      ! -- set hmaw and xsto
      hmaw = this%xnewpak(n)
      this%xsto(n) = hmaw
      !
      ! -- add pumping rate to active maw well
      rrate = this%ratesim(n)
      !
      ! -- If flow is out of maw set qout to rrate.
      if (rrate < DZERO) then
        this%qout(n) = rrate
      end if
      !
      ! -- add flowing well
      if (this%iflowingwells > 0) then
        if (this%fwcond(n) > DZERO) then
          cfw = this%fwcondsim(n)
          this%xsto(n) = this%fwelev(n)
          rrate = cfw * (this%fwelev(n) - hmaw)
          this%qfw(n) = rrate
          !
          ! -- Subtract flowing well rrate from qout.
          this%qout(n) = this%qout(n) + rrate
        end if
      end if
      !
      ! -- Calculate qsto
      if (this%imawiss /= 1) then
        rrate = -this%area(n) * (this%xsto(n) - this%xoldsto(n)) / delt
        this%qsto(n) = rrate
      end if
    end do
    !
    ! -- gwf and constant flow
    ibnd = 1
    do n = 1, this%nmawwells
      rrate = DZERO
      hmaw = this%xnewpak(n)
      this%qconst(n) = DZERO
      do j = 1, this%ngwfnodes(n)
        this%qleak(ibnd) = DZERO
        if (this%iboundpak(n) == 0) cycle
        igwfnode = this%mawwells(n)%gwfnodes(j)
        hgwf = this%xnew(igwfnode)
        cmaw = this%mawwells(n)%simcond(j)

        bmaw = this%mawwells(n)%botscrn(j)
        !
        ! -- calculate cterm - relative to gwf
        cterm = DZERO
        if (hmaw < bmaw) then
          cterm = cmaw * (bmaw - hmaw)
        end if
        rrate = -(cmaw * (hmaw - hgwf) + cterm)
        this%qleak(ibnd) = rrate
        if (this%iboundpak(n) < 0) then
          this%qconst(n) = this%qconst(n) - rrate
          !
          ! -- If flow is out increment qout by -rrate.
          if (-rrate < DZERO) then
            this%qout(n) = this%qout(n) - rrate
          end if
        end if
        ibnd = ibnd + 1
      end do
      !
      ! -- add additional flow terms to constant head term
      if (this%iboundpak(n) < 0) then
        !
        ! -- add well pumping rate
        this%qconst(n) = this%qconst(n) - this%ratesim(n)
        !
        ! -- add flowing well rate
        if (this%iflowingwells > 0) then
          this%qconst(n) = this%qconst(n) - this%qfw(n)
        end if
        !
        ! -- add storage term
        if (this%imawiss /= 1) then
          this%qconst(n) = this%qconst(n) - this%qsto(n)
        end if
      end if
    end do
    !
    ! -- For continuous observations, save simulated values.
    if (this%obs%npakobs > 0 .and. iprobs > 0) then
      call this%maw_bd_obs()
    end if
    !
    ! -- set unit number for binary dependent variable output
    ibinun = 0
    if(this%iheadout /= 0) then
      ibinun = this%iheadout
    end if
    if(idvfl == 0) ibinun = 0
    if (isuppress_output /= 0) ibinun = 0
    !
    ! -- write maw binary output
    if (ibinun > 0) then
      do n = 1, this%nmawwells
        v = this%xnewpak(n)
        d = v - this%bot(n)
        if (this%iboundpak(n) == 0) then
          v = DHNOFLO
        else if (d <= DZERO) then
          v = DHDRY
        end if
        this%dbuff(n) = v
      end do
      call ulasav(this%dbuff, '            HEAD',                               &
                  kstp, kper, pertim, totim,                                    &
                  this%nmawwells, 1, 1, ibinun)
    end if
    !
    ! -- fill the budget object
    call this%maw_fill_budobj()
    !
    ! -- write the flows from the budobj
    ibinun = 0
    if(this%ibudgetout /= 0) then
      ibinun = this%ibudgetout
    end if
    if(icbcfl == 0) ibinun = 0
    if (isuppress_output /= 0) ibinun = 0
    if (ibinun > 0) then
      call this%budobj%save_flows(this%dis, ibinun, kstp, kper, delt, &
                                  pertim, totim, this%iout)
    end if
    !
    ! -- return
    return
  end subroutine maw_bd


  subroutine maw_ot(this, kstp, kper, iout, ihedfl, ibudfl)
    ! **************************************************************************
    ! maw_ot -- Output package budget
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    !
    ! -- dummy
    class(MawType) :: this
    integer(I4B),intent(in) :: kstp
    integer(I4B),intent(in) :: kper
    integer(I4B),intent(in) :: iout
    integer(I4B),intent(in) :: ihedfl
    integer(I4B),intent(in) :: ibudfl
    ! -- locals
    integer(I4B) :: n
    ! format
    ! --------------------------------------------------------------------------
     !
     ! -- write maw head table
     if (ihedfl /= 0 .and. this%iprhed /= 0) then
      !
      ! -- fill stage data
      do n = 1, this%nmawwells
        if(this%inamedbound==1) then
          call this%headtab%add_term(this%cmawname(n))
        end if
        call this%headtab%add_term(n)
        call this%headtab%add_term(this%xnewpak(n))
      end do
     end if
    !
    ! -- Output maw flow table
    if (ibudfl /= 0 .and. this%iprflow /= 0) then
      call this%budobj%write_flowtable(this%dis)
    end if
    !
    ! -- Output maw budget
    call this%budobj%write_budtable(kstp, kper, iout)
    !
    ! -- return
    return
  end subroutine maw_ot

  subroutine maw_da(this)
! ******************************************************************************
! maw_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(MawType) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- mawwells derived type array
    do n = 1, this%nmawwells
      call this%maw_deallocate_well(n)
    end do
    deallocate(this%mawwells)
    !
    ! -- budobj
    call this%budobj%budgetobject_da()
    deallocate(this%budobj)
    nullify(this%budobj)
    !
    ! -- head table
    if (this%iprhed > 0) then
      call this%headtab%table_da()
      deallocate(this%headtab)
      nullify(this%headtab)
    end if
    !
    ! -- character arrays
    deallocate(this%cmawbudget)
    deallocate(this%cmawname)
    deallocate(this%status)
    
    call mem_deallocate(this%idxmawconn)
    !
    ! -- deallocate well data pointers in memory manager
    call mem_deallocate(this%ngwfnodes)
    call mem_deallocate(this%ieqn)
    call mem_deallocate(this%ishutoff)
    call mem_deallocate(this%ifwdischarge)
    call mem_deallocate(this%strt)
    call mem_deallocate(this%radius)
    call mem_deallocate(this%area)
    call mem_deallocate(this%pumpelev)
    call mem_deallocate(this%bot)
    call mem_deallocate(this%ratesim)
    call mem_deallocate(this%reduction_length)
    call mem_deallocate(this%fwelev)
    call mem_deallocate(this%fwcond)
    call mem_deallocate(this%fwrlen)
    call mem_deallocate(this%fwcondsim)
    call mem_deallocate(this%xsto)
    call mem_deallocate(this%xoldsto)
    call mem_deallocate(this%shutoffmin)
    call mem_deallocate(this%shutoffmax)
    call mem_deallocate(this%shutofflevel)
    call mem_deallocate(this%shutoffweight)
    call mem_deallocate(this%shutoffdq)
    call mem_deallocate(this%shutoffqold)
    !
    ! -- timeseries aware variables
    call mem_deallocate(this%mauxvar)
    call mem_deallocate(this%rate)
    call mem_deallocate(this%well_head)
    !
    ! --
    call mem_deallocate(this%imap)
    call mem_deallocate(this%dbuff)
    deallocate(this%cauxcbc)
    call mem_deallocate(this%qauxcbc)
    call mem_deallocate(this%qleak)
    call mem_deallocate(this%qfw)
    call mem_deallocate(this%qout)
    call mem_deallocate(this%qsto)
    call mem_deallocate(this%qconst)
    deallocate(this%idxlocnode)
    deallocate(this%idxdglo)
    deallocate(this%idxoffdglo)
    deallocate(this%idxsymdglo)
    deallocate(this%idxsymoffdglo)
    deallocate(this%xoldpak)
    deallocate(this%cterm)
    !
    ! -- scalars
    call mem_deallocate(this%iprhed)
    call mem_deallocate(this%iheadout)
    call mem_deallocate(this%ibudgetout)
    call mem_deallocate(this%iflowingwells)
    call mem_deallocate(this%imawiss)
    call mem_deallocate(this%imawissopt)
    call mem_deallocate(this%nmawwells)
    call mem_deallocate(this%check_attr)
    call mem_deallocate(this%ishutoffcnt)
    call mem_deallocate(this%ieffradopt)
    call mem_deallocate(this%satomega)
    call mem_deallocate(this%bditems)
    call mem_deallocate(this%theta)
    call mem_deallocate(this%kappa)
    call mem_deallocate(this%cbcauxitems)
    !
    ! -- pointers to gwf variables
    nullify(this%gwfiss)
    !
    ! -- call standard BndType deallocate
    call this%BndType%bnd_da()
    !
    ! -- return
    return
  end subroutine maw_da

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(MawType), intent(inout) :: this
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
    end if
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'STRESS RATE'
    if(this%inamedbound == 1) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- return
    return
  end subroutine define_listlabel


  subroutine maw_set_pointers(this, neq, ibound, xnew, xold, flowja)
! ******************************************************************************
! set_pointers -- Set pointers to model arrays and variables so that a package
!                 has access to these things.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(MawType) :: this
    integer(I4B), pointer :: neq
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    real(DP), dimension(:), pointer, contiguous :: xnew
    real(DP), dimension(:), pointer, contiguous :: xold
    real(DP), dimension(:), pointer, contiguous :: flowja
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: istart, iend
! ------------------------------------------------------------------------------
    !
    ! -- call base BndType set_pointers
    call this%BndType%set_pointers(neq, ibound, xnew, xold, flowja)
    !
    ! -- Set the MAW pointers
    !
    ! -- set package pointers
    istart = this%dis%nodes + this%ioffset + 1
    iend = istart + this%nmawwells - 1
    this%iboundpak => this%ibound(istart:iend)
    this%xnewpak => this%xnew(istart:iend)
    allocate(this%xoldpak(this%nmawwells))
    allocate(this%cterm(this%maxbound))
    !
    ! -- initialize xnewpak
    do n = 1, this%nmawwells
      this%xnewpak(n) = DEP20
    end do
    !
    ! -- return
  end subroutine maw_set_pointers

  !
  ! -- Procedures related to observations (type-bound)
  logical function maw_obs_supported(this)
  ! ******************************************************************************
  ! maw_obs_supported
  !   -- Return true because MAW package supports observations.
  !   -- Overrides BndType%bnd_obs_supported()
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    class(MawType) :: this
  ! ------------------------------------------------------------------------------
    maw_obs_supported = .true.
    return
  end function maw_obs_supported


  subroutine maw_df_obs(this)
  ! ******************************************************************************
  ! maw_df_obs (implements bnd_df_obs)
  !   -- Store observation type supported by MAW package.
  !   -- Overrides BndType%bnd_df_obs
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    ! -- dummy
    class(MawType) :: this
    ! -- local
    integer(I4B) :: indx
  ! ------------------------------------------------------------------------------
    !
    ! -- Store obs type and assign procedure pointer
    !    for head observation type.
    call this%obs%StoreObsType('head', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => maw_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for frommvr observation type.
    call this%obs%StoreObsType('from-mvr', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => maw_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for conn-rate observation type.
    call this%obs%StoreObsType('maw', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => maw_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for rate observation type.
    call this%obs%StoreObsType('rate', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => maw_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for rate-to-mvr observation type.
    call this%obs%StoreObsType('rate-to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => maw_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for fw-rate observation type.
    call this%obs%StoreObsType('fw-rate', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => maw_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for rate-to-mvr observation type.
    call this%obs%StoreObsType('fw-to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => maw_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for storage observation type.
    call this%obs%StoreObsType('storage', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => maw_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for constant observation type.
    call this%obs%StoreObsType('constant', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => maw_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for cond observation type.
    call this%obs%StoreObsType('conductance', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => maw_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for fw-conductance observation type.
    call this%obs%StoreObsType('fw-conductance', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => maw_process_obsID
    !
    return
  end subroutine maw_df_obs


  subroutine maw_bd_obs(this)
    ! **************************************************************************
    ! maw_bd_obs
    !   -- Calculate observations this time step and call
    !      ObsType%SaveOneSimval for each MawType observation.
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    ! -- dummy
    class(MawType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, igwfnode, j, jj, n, nn
    real(DP) :: cmaw, hmaw, v
    real(DP) :: qfact
    character(len=200) :: errmsg
    type(ObserveType), pointer :: obsrv => null()
    !---------------------------------------------------------------------------
    !
    ! Calculate, save, and write simulated values for all MAW observations
    if (this%obs%npakobs > 0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        nn = size(obsrv%indxbnds)
        do j = 1, nn
          v = DNODATA
          jj = obsrv%indxbnds(j)
          select case (obsrv%ObsTypeId)
            case ('HEAD')
              if (this%iboundpak(jj) /= 0) then
                v = this%xnewpak(jj)
              end if
            case ('FROM-MVR')
              if (this%iboundpak(jj) /= 0) then
                if (this%imover == 1) then
                  v = this%pakmvrobj%get_qfrommvr(jj)
                end if
              end if
            case ('MAW')
              n = this%imap(jj)
              if (this%iboundpak(n) /= 0) then
                v = this%qleak(jj)
              end if
            case ('RATE')
              if (this%iboundpak(jj) /= 0) then
                v = this%ratesim(jj)
                if (v < DZERO .and. this%qout(jj) < DZERO) then
                  qfact = v / this%qout(jj)
                  if (this%imover == 1) then
                    v = v + this%pakmvrobj%get_qtomvr(jj) * qfact
                  end if
                end if
              end if
            case ('RATE-TO-MVR')
              if (this%iboundpak(jj) /= 0) then
                if (this%imover == 1) then
                  v = this%ratesim(jj)
                  qfact = DZERO
                  if (v < DZERO .and. this%qout(jj) < DZERO) then
                    qfact = v / this%qout(jj)
                  end if
                  v = this%pakmvrobj%get_qtomvr(jj) * qfact
                  if (v > DZERO) then
                    v = -v
                  end if
                end if
              end if
            case ('FW-RATE')
              if (this%iboundpak(jj) /= 0 .and. this%iflowingwells /= 0) then
                hmaw = this%xnewpak(jj)
                cmaw = this%fwcondsim(jj)
                v = cmaw * (this%fwelev(jj) - hmaw)
                if (v < DZERO .and. this%qout(jj) < DZERO) then
                  qfact = v / this%qout(jj)
                  if (this%imover == 1) then
                    v = v + this%pakmvrobj%get_qtomvr(jj) * qfact
                  end if
                end if
              end if
            case ('FW-TO-MVR')
              if (this%iboundpak(jj) /= 0 .and. this%iflowingwells /= 0) then
                if (this%imover == 1) then
                  hmaw = this%xnewpak(jj)
                  cmaw = this%fwcondsim(jj)
                  v = cmaw * (this%fwelev(jj) - hmaw)
                  qfact = DZERO
                  if (v < DZERO .and. this%qout(jj) < DZERO) then
                    qfact = v / this%qout(jj)
                  end if
                  v = this%pakmvrobj%get_qtomvr(jj) * qfact
                  if (v > DZERO) then
                    v = -v
                  end if
                end if
              end if
            case ('STORAGE')
              if (this%iboundpak(jj) /= 0 .and. this%imawissopt /= 1) then
                v = this%qsto(jj)
              end if
            case ('CONSTANT')
              if (this%iboundpak(jj) /= 0) then
                v = this%qconst(jj)
              end if
            case ('CONDUCTANCE')
              n = this%imap(jj)
              if (this%iboundpak(n) /= 0) then
                nn = jj - this%idxmawconn(n) + 1
                igwfnode = this%mawwells(n)%gwfnodes(nn)
                v = this%mawwells(n)%simcond(nn)
              end if
            case ('FW-CONDUCTANCE')
              if (this%iboundpak(jj) /= 0) then
                v = this%fwcondsim(jj)
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
    ! -- return
    return
  end subroutine maw_bd_obs


  subroutine maw_rp_obs(this)
    ! -- dummy
    class(MawType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, j, n, nn1, nn2
    integer(I4B) :: jj
    character(len=200) :: errmsg
    character(len=LENBOUNDNAME) :: bname
    logical :: jfound
    class(ObserveType),   pointer :: obsrv => null()
    ! --------------------------------------------------------------------------
    ! -- formats
10  format('Error: Boundary "',a,'" for observation "',a, &
           '" is invalid in package "',a,'"')
    !
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
          ! -- Observation maw is based on a boundary name.
          !    Iterate through all multi-aquifer wells to identify and store
          !    corresponding index in bound array.
          jfound = .false.
          if (obsrv%ObsTypeId=='MAW' .or.   &
               obsrv%ObsTypeId=='CONDUCTANCE') then
            do j = 1, this%nmawwells
              do jj = this%idxmawconn(j), this%idxmawconn(j+1) - 1
                if (this%boundname(jj) == bname) then
                  jfound = .true.
                  call ExpandArray(obsrv%indxbnds)
                  n = size(obsrv%indxbnds)
                  obsrv%indxbnds(n) = jj
                end if
              end do
            end do
          else
            do j = 1, this%nmawwells
              if (this%cmawname(j) == bname) then
                jfound = .true.
                call ExpandArray(obsrv%indxbnds)
                n = size(obsrv%indxbnds)
                obsrv%indxbnds(n) = j
              end if
            end do
          end if
          if (.not. jfound) then
            write(errmsg,10)trim(bname), trim(obsrv%Name), trim(this%name)
            call store_error(errmsg)
          end if
        end if
      else
        call ExpandArray(obsrv%indxbnds)
        n = size(obsrv%indxbnds)
        if (n == 1) then
          if (obsrv%ObsTypeId=='MAW' .or.   &
               obsrv%ObsTypeId=='CONDUCTANCE') then
            nn2 = obsrv%NodeNumber2
            j = this%idxmawconn(nn1) + nn2 - 1
            obsrv%indxbnds(1) = j
          else
            obsrv%indxbnds(1) = nn1
          end if
        else
          errmsg = 'Programming error in maw_rp_obs'
          call store_error(errmsg)
        end if
      end if
      !
      ! -- catch non-cumulative observation assigned to observation defined
      !    by a boundname that is assigned to more than one element
      if (obsrv%ObsTypeId == 'HEAD') then
        n = size(obsrv%indxbnds)
        if (n > 1) then
          write (errmsg, '(4x,a,4(1x,a))') &
            'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
            'for observation', trim(adjustl(obsrv%Name)), &
            ' must be assigned to a multi-aquifer well with a unique boundname.'
          call store_error(errmsg)
        end if
      end if
      !
      ! -- check that index values are valid
      if (obsrv%ObsTypeId=='MAW' .or.   &
          obsrv%ObsTypeId=='CONDUCTANCE') then
        do j = 1, size(obsrv%indxbnds)
          nn1 =  obsrv%indxbnds(j)
          n = this%imap(nn1)
          nn2 = nn1 - this%idxmawconn(n) + 1
          jj = this%idxmawconn(n+1) - this%idxmawconn(n)
          if (nn1 < 1 .or. nn1 > this%maxbound) then
            write (errmsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              ' multi-aquifer well connection number must be > 0 and <=', &
              jj, '(specified value is ', nn2, ')'
            call store_error(errmsg)
          end if
        end do
      else
        do j = 1, size(obsrv%indxbnds)
          nn1 =  obsrv%indxbnds(j)
          if (nn1 < 1 .or. nn1 > this%nmawwells) then
            write (errmsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              ' multi-aquifer well must be > 0 and <=', this%nmawwells, &
              '(specified value is ', nn1, ')'
            call store_error(errmsg)
          end if
        end do
      end if
    end do
    !
    ! -- check if any error were encountered
    if (count_errors() > 0) call ustop()
    !
    ! -- return
    return
  end subroutine maw_rp_obs


  !
  ! -- Procedures related to observations (NOT type-bound)
  subroutine maw_process_obsID(obsrv, dis, inunitobs, iout)
    ! -- This procedure is pointed to by ObsDataType%ProcesssIdPtr. It processes
    !    the ID string of an observation definition for MAW package observations.
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
    ! -- Extract multi-aquifer well number from strng and store it.
    !    If 1st item is not an integer(I4B), it should be a
    !    maw name--deal with it.
    icol = 1
    ! -- get multi-aquifer well number or boundary name
    call extract_idnum_or_bndname(strng, icol, istart, istop, nn1, bndname)
    if (nn1 == NAMEDBOUNDFLAG) then
      obsrv%FeatureName = bndname
    else
      if (obsrv%ObsTypeId=='MAW' .or. &
          obsrv%ObsTypeId=='CONDUCTANCE') then
        call extract_idnum_or_bndname(strng, icol, istart, istop, nn2, bndname)
        if (nn2 == NAMEDBOUNDFLAG) then
          obsrv%FeatureName = bndname
          ! -- reset nn1
          nn1 = nn2
        else
          obsrv%NodeNumber2 = nn2
        end if
      end if
    end if
    ! -- store multi-aquifer well number (NodeNumber)
    obsrv%NodeNumber = nn1
    !
    ! -- return
    return
  end subroutine maw_process_obsID

  !
  ! -- private MAW methods
  !
  subroutine maw_calculate_satcond(this, i, j, node)
    ! -- dummy
    class(MawType),intent(inout) :: this
    integer(I4B), intent(in) :: i
    integer(I4B), intent(in) :: j
    integer(I4B), intent(in) :: node
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: iTcontrastErr
    real(DP) :: c
    real(DP) :: k11
    real(DP) :: k22
    real(DP) :: sqrtk11k22
    real(DP) :: hks
    real(DP) :: area
    real(DP) :: eradius
    real(DP) :: topw
    real(DP) :: botw
    real(DP) :: tthkw
    real(DP) :: tthka
    real(DP) :: Tcontrast
    real(DP) :: skin
    real(DP) :: ravg
    real(DP) :: slen
    real(DP) :: pavg
    real(DP) :: gwfsat
    real(DP) :: gwftop
    real(DP) :: gwfbot
    real(DP) :: lc1
    real(DP) :: lc2
    real(DP) :: dx
    real(DP) :: dy
    real(DP) :: Txx
    real(DP) :: Tyy
    real(DP) :: T2pi
    real(DP) :: yx4
    real(DP) :: xy4
    ! -- formats
    ! ------------------------------------------------------------------------------
    !
    ! -- initialize conductance variables
    iTcontrastErr = 0
    lc1 = DZERO
    lc2 = DZERO
    !
    ! -- set K11 and K22
    k11 = this%gwfk11(node)
    if (this%gwfik22 == 0) then
      k22 = this%gwfk11(node)
    else
      k22 = this%gwfk22(node)
    end if
    sqrtk11k22 = sqrt(k11 * k22)
    !
    ! -- set gwftop, gwfbot, and gwfsat
    gwftop = this%dis%top(node)
    gwfbot = this%dis%bot(node)
    tthka  = gwftop - gwfbot
    gwfsat = this%gwfsat(node)
    !
    ! -- set top and bottom of well screen
    c = DZERO
    topw = this%mawwells(i)%topscrn(j)
    botw = this%mawwells(i)%botscrn(j)
    tthkw = topw - botw
    !
    ! -- scale screen thickness using gwfsat (for NPF Package THICKSTRT)
    if (gwftop == topw .and. gwfbot == botw) then
      if (this%icelltype(node) == 0) then
        tthkw = tthkw * gwfsat
        tthka = tthka * gwfsat
      end if
    end if
    !
    ! -- calculate the aquifer transmissivity (T2pi)
    T2pi = DTWOPI * tthka * sqrtk11k22
    !
    ! -- calculate effective radius
    if (this%dis%ndim == 3 .and. this%ieffradopt /= 0) then
      Txx = k11 * tthka
      Tyy = k22 * tthka
      dx = sqrt(this%dis%area(node))
      dy = dx
      yx4 = (Tyy/Txx)**DQUARTER
      xy4 = (Txx/Tyy)**DQUARTER
      eradius = 0.28_DP * ((yx4*dx)**DTWO + (xy4*dy)**DTWO)**DHALF / (yx4+xy4)
    else
      area = this%dis%area(node)
      eradius = sqrt(area / (DEIGHT * DPI))
    end if
    !
    ! -- conductance calculations
    ! -- Thiem equation (1) and cumulative Thiem and skin equations (3)
    if (this%ieqn(i) == 1 .or. this%ieqn(i) == 3) then
      lc1 = log(eradius / this%radius(i)) / T2pi
    end if
    !
    ! -- skin equation (2) and cumulative Thiem and skin equations (3)
    if (this%ieqn(i) == 2 .or. this%ieqn(i) == 3) then
      hks = this%mawwells(i)%hk(j)
      if (tthkw * hks > DZERO) then
        Tcontrast = (sqrtk11k22 * tthka) / (hks * tthkw)
        skin = (Tcontrast - DONE) *                                              &
                log(this%mawwells(i)%sradius(j) / this%radius(i))
        !
        ! -- trap invalid transmissvity contrast if using skin equation (2).
        !    Not trapped for cumulative Thiem and skin equations (3) 
        !    because the MNW2 package allowed this condition (for 
        !    backward compatibility with the MNW2 package for  
        !    MODFLOW-2005, MODFLOW-NWT, and MODFLOW-USG).
        if (Tcontrast <= 1 .and. this%ieqn(i) == 2) then
          iTcontrastErr = 1
          write(errmsg, '(a,g0,a,1x,i0,1x,a,1x,i0,a,4(1x,a))')                   &
            'Invalid calculated transmissivity contrast (', Tcontrast,           &
            ') for maw well', i, 'connection', j, '.', 'This happens when the',  &
            'skin transmissivity equals or exceeds the aquifer transmissivity.', &
            'Consider decreasing HK_SKIN for the connection or using the',       &
            'CUMULATIVE or MEAN conductance equations.'
          call store_error(errmsg)
        else
          lc2 = skin / T2pi
        end if
      end if
    end if
    ! -- conductance using screen elevations, hk, well radius, 
    !    and screen radius
    if (this%ieqn(i) == 4) then
      hks = this%mawwells(i)%hk(j)
      ravg = DHALF * (this%radius(i) + this%mawwells(i)%sradius(j))
      slen = this%mawwells(i)%sradius(j) - this%radius(i)
      pavg = DTWOPI * ravg
      c = hks * pavg * tthkw / slen
    end if
    !
    ! -- calculate final conductance for Theim (1), Skin (2), and 
    ! and cumulative Thiem and skin equations (3)
    if (this%ieqn(i) < 4) then
      if (lc1 + lc2 /= DZERO) then
        c = DONE / (lc1 + lc2)
      else
        c = -DNODATA
      end if
    end if
    !
    ! -- ensure that the conductance is not negative. Only write error message
    !    if error condition has not occured for skin calculations (LC2)
    if (c < DZERO .and. iTcontrastErr == 0) then
      write(errmsg, '(4x,a,g0,a,1x,i0,1x,a,1x,i0,a,4(1x,a))')                    &
        '****ERROR. INVALID CALCULATED NEGATIVE CONDUCTANCE (', c,               &
        ') for MAW WELL', i, 'CONNECTION', j, '.', 'THIS HAPPENS WHEN THE',      &
        'SKIN TRANSMISSIVITY EQUALS OR EXCEEDS THE AQUIFER TRANSMISSIVITY.',     &
        'CONSIDER DECREASING HK_SKIN FOR THE CONNECTION OR USING THE',           &
        'MEAN CONDUCTANCE EQUATION.'
      call store_error(errmsg)
    end if
    !
    ! -- set saturated conductance
    this%mawwells(i)%satcond(j) = c
    !
    ! -- return
    return
  end subroutine maw_calculate_satcond


  subroutine maw_calculate_saturation(this, i, j, node, sat)
    ! -- dummy
    class(MawType),intent(inout) :: this
    integer(I4B), intent(in) :: i
    integer(I4B), intent(in) :: j
    integer(I4B), intent(in) :: node
    real(DP), intent(inout) :: sat
    ! -- local
    real(DP) :: htmp
    real(DP) :: hwell
    real(DP) :: topw
    real(DP) :: botw
    ! -- formats
    ! ------------------------------------------------------------------------------
    !
    ! -- initialize saturation
    sat = DZERO
    !
    ! -- calculate current saturation for convertible cells
    if (this%icelltype(node) /= 0) then
      !
      ! -- set hwell
      hwell = this%xnewpak(i)
      ! -- set top and bottom of the well connection
      topw = this%mawwells(i)%topscrn(j)
      botw = this%mawwells(i)%botscrn(j)
      !
      ! -- calculate appropriate saturation
      if (this%inewton /= 1) then
        htmp = this%xnew(node)
        if (htmp < botw) htmp = botw
        if (hwell < botw) hwell = botw
        htmp = DHALF * (htmp + hwell)
      else
        htmp = this%xnew(node)
        if (hwell > htmp) htmp = hwell
        if (htmp < botw) htmp = botw
      end if
      ! -- calculate saturation
      sat = sQuadraticSaturation(topw, botw, htmp, this%satomega)
    else
      sat = DONE
    end if
    !
    ! -- return
    return
  end subroutine maw_calculate_saturation

  subroutine maw_calculate_wellq(this, n, hmaw, q)
! **************************************************************************
! maw_calculate_wellq-- Calculate well pumping rate based on constraints
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(MawType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: hmaw
    real(DP), intent(inout) :: q
    ! -- local
    real(DP) :: scale
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: rate
    real(DP) :: weight
    real(DP) :: dq
! --------------------------------------------------------------------------
    !
    ! -- Initialize q
    q = DZERO
    !
    ! -- Assign rate as the user-provided base pumping rate
    rate = this%rate(n)
    !
    ! -- Assign q differently depending on whether this is an extraction well
    !    (rate < 0) or an injection well (rate > 0).
    if (rate < DZERO) then
      !
      ! -- If well shut off is activated, then turn off well if necessary,
      !    or if shut off is not activated then check to see if rate scaling 
      !    is on.
      if (this%shutofflevel(n) /= DEP20) then
        call this%maw_calculate_qpot(n, q)
        if (q < DZERO) q = DZERO
        if (q > -rate) q = -rate

        if (this%ishutoffcnt == 1) then
          this%shutoffweight(n) = DONE
          this%shutoffdq(n) = DZERO
          this%shutoffqold(n) = q
        end if

        dq = q - this%shutoffqold(n)
        weight = this%shutoffweight(n)
        !
        ! -- for flip-flop condition, decrease factor
        if ( this%shutoffdq(n) * dq < DZERO ) then
          weight = this%theta * this%shutoffweight(n)
        !
        ! -- when change is of same sign, increase factor
        else
          weight = this%shutoffweight(n) + this%kappa
        end if
        if ( weight > DONE ) weight = DONE

        q = this%shutoffqold(n) + weight * dq

        this%shutoffqold(n) = q
        this%shutoffdq(n) = dq
        this%shutoffweight(n) = weight
        !
        ! -- If shutoffmin and shutoffmax are specified then apply 
        !    additional checks for when to shut off the well.
        if (this%shutoffmin(n) > DZERO) then
          if (hmaw < this%shutofflevel(n)) then
            !
            ! -- calculate adjusted well rate subject to constraints
            ! -- well is shutoff
            if (this%ishutoff(n) /= 0) then
              q = DZERO
            !
            ! --- well is not shut off
            else
              ! -- turn off well if q is less than the minimum rate and
              !    reset the ishutoff flag if at least on iteration 3
              if (q < this%shutoffmin(n)) then
                if (this%ishutoffcnt > 2) then
                  this%ishutoff(n) = 1
                end if
                q = DZERO
              !
              ! -- leave well on and use the specified rate
              !    or the potential rate
              end if
            end if
          !
          ! -- try to use the specified rate or the potential rate
          else
            if (q > this%shutoffmax(n)) then
              if (this%ishutoffcnt <= 2) then
                this%ishutoff(n) = 0
              end if
            end if
            if (this%ishutoff(n) /= 0) then
              q = DZERO
            end if
          end if
        end if

        if (q /= DZERO) q = -q

      else
        scale = DONE
        !
        ! -- Apply rate scaling by reducing pumpage when hmaw is less than the 
        !    sum of maw pump elevation (pumpelev) and the specified reduction 
        !    length.  The rate will go to zero as hmaw drops to the pump
        !    elevation.
        if (this%reduction_length(n) /= DEP20) then
          bt = this%pumpelev(n)
          tp = bt + this%reduction_length(n)
          scale = sQSaturation(tp, bt, hmaw)
        end if
        q = scale * rate
      end if
    !
    else
      !
      ! -- Handle the injection case (rate > 0) differently than extraction.
      q = rate
      if (this%shutofflevel(n) /= DEP20) then
        call this%maw_calculate_qpot(n, q)
        q = -q
        if (q < DZERO) q = DZERO
        if (q > rate) q = rate

        if (this%ishutoffcnt == 1) then
          this%shutoffweight(n) = DONE
          this%shutoffdq(n) = DZERO
          this%shutoffqold(n) = q
        end if

        dq = q - this%shutoffqold(n)
        weight = this%shutoffweight(n)
        !
        ! -- for flip-flop condition, decrease factor
        if ( this%shutoffdq(n) * dq < DZERO ) then
          weight = this%theta * this%shutoffweight(n)
        !
        ! -- when change is of same sign, increase factor
        else
          weight = this%shutoffweight(n) + this%kappa
        end if
        if ( weight > DONE ) weight = DONE
        
        q = this%shutoffqold(n) + weight * dq
        
        this%shutoffqold(n) = q
        this%shutoffdq(n) = dq
        this%shutoffweight(n) = weight
        
      else
        scale = DONE
        !
        ! -- Apply rate scaling for an injection well by reducting the 
        !    injection rate as hmaw rises above the pump elevation.  The rate
        !    will approach zero as hmaw approaches pumpelev + reduction_length.
        if (this%reduction_length(n) /= DEP20) then
          bt = this%pumpelev(n)
          tp = bt + this%reduction_length(n)
          scale = DONE - sQSaturation(tp, bt, hmaw)
        end if
        q = scale * rate
      end if
    end if
    !
    ! -- return
    return
  end subroutine maw_calculate_wellq

  subroutine maw_calculate_qpot(this, n, qnet)
! ******************************************************************************
! maw_calculate_qpot -- Calculate groundwater inflow to a maw well
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule,only:delt
    ! -- dummy
    class(MawType),intent(inout) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(inout) :: qnet
    ! -- local
    integer(I4B) :: j
    integer(I4B) :: igwfnode
    real(DP) :: bt
    real(DP) :: tp
    real(DP) :: scale
    real(DP) :: cfw
    real(DP) :: hdterm
    real(DP) :: sat
    real(DP) :: cmaw
    real(DP) :: hgwf
    real(DP) :: bmaw
    real(DP) :: htmp
    real(DP) :: hv
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- initialize qnet and htmp
    qnet = DZERO
    htmp = this%shutofflevel(n)
    !
    ! -- calculate discharge to flowing wells
    if (this%iflowingwells > 0) then
      if (this%fwcond(n) > DZERO) then
        bt = this%fwelev(n)
        tp = bt + this%fwrlen(n)
        scale = sQSaturation(tp, bt, htmp)
        cfw = scale * this%fwcond(n)
        this%ifwdischarge(n) = 0
        if (cfw > DZERO) then
          this%ifwdischarge(n) = 1
          this%xsto(n) = bt
        end if
        qnet = qnet + cfw * (bt - htmp)
      end if
    end if
    !
    ! -- calculate maw storage changes
    if (this%imawiss /= 1) then
      if (this%ifwdischarge(n) /= 1) then
        hdterm = this%xoldsto(n) - htmp
      else
        hdterm = this%xoldsto(n) - this%fwelev(n)
      end if
      qnet = qnet - (this%area(n) * hdterm / delt)
    end if
    !
    ! -- calculate inflow from aquifer
    do j = 1, this%ngwfnodes(n)
      igwfnode = this%mawwells(n)%gwfnodes(j)
      call this%maw_calculate_saturation(n, j, igwfnode, sat)
      cmaw = this%mawwells(n)%satcond(j) * sat
      hgwf = this%xnew(igwfnode)
      bmaw = this%mawwells(n)%botscrn(j)
      hv = htmp
      if (hv < bmaw) then
        hv = bmaw
      end if
      if (hgwf < bmaw) then
        hgwf = bmaw
      end if
      qnet = qnet + cmaw * (hgwf - hv)
    end do
    !
    ! -- return
    return
  end subroutine maw_calculate_qpot

  subroutine maw_cfupdate(this)
  ! ******************************************************************************
  ! maw_cfupdate -- Update MAW satcond and package rhs and hcof
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      class(MawType) :: this
      integer(I4B) :: j, n, node
      integer(I4B) :: ibnd
      real(DP) :: sat, cmaw, hmaw, bmaw
  ! ------------------------------------------------------------------------------
  !
  ! -- Return if no maw wells
      if(this%nbound.eq.0) return
  !
  ! -- Update shutoff count
      this%ishutoffcnt = this%ishutoffcnt + 1
  !
  ! -- Calculate hcof and rhs for each maw entry
      ibnd = 1
      do n = 1, this%nmawwells
        hmaw = this%xnewpak(n)
        do j = 1, this%ngwfnodes(n)
          node = this%nodelist(ibnd)
          this%hcof(ibnd) = DZERO
          this%rhs(ibnd) = DZERO
          !
          ! -- set bound, hcof, and rhs components
          call this%maw_calculate_saturation(n, j, node, sat)
          if (this%iboundpak(n) == 0) then
            cmaw = DZERO
          else
            cmaw = this%mawwells(n)%satcond(j) * sat
          end if
          this%mawwells(n)%simcond(j) = cmaw

          this%bound(2,ibnd) = cmaw

          bmaw = this%bound(3,ibnd)
          
          this%hcof(ibnd) = -cmaw
          !
          ! -- fill rhs
          if (hmaw < bmaw) then
            this%rhs(ibnd) = -cmaw * bmaw
          else
            this%rhs(ibnd) = -cmaw * hmaw
          end if
          !
          ! -- increment boundary number
          ibnd = ibnd + 1
        end do
      end do
      !
      ! -- Return
      return
  end subroutine maw_cfupdate



  subroutine maw_deallocate_well(this, n)
! ******************************************************************************
! maw_deallocate_well -- deallocate pointers for multi-aquifer well mawwells(n).
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(MawType) :: this
    integer(I4B), intent(in) :: n
    ! -- local
! ------------------------------------------------------------------------------
    !
    deallocate(this%mawwells(n)%gwfnodes)
    deallocate(this%mawwells(n)%satcond)
    deallocate(this%mawwells(n)%simcond)
    deallocate(this%mawwells(n)%topscrn)
    deallocate(this%mawwells(n)%botscrn)
    if (this%ieqn(n) == 2 .OR. this%ieqn(n) == 3 .OR.                            &
        this%ieqn(n) == 4) then
      deallocate(this%mawwells(n)%hk)
    end if
    if (this%ieqn(n) == 2 .OR. this%ieqn(n) == 3 .OR.                            &
        this%ieqn(n) == 4) then
      deallocate(this%mawwells(n)%sradius)
    end if
    !
    ! -- return
    return
  end subroutine maw_deallocate_well

  subroutine maw_setup_budobj(this)
! ******************************************************************************
! maw_setup_budobj -- Set up the budget object that stores all the maw flows
!   The terms listed here must correspond in number and order to the ones 
!   listed in the maw_fill_budobj routine.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(MawType) :: this
    ! -- local
    integer(I4B) :: nbudterm
    integer(I4B) :: n, j, n2
    real(DP) :: q
    integer(I4B) :: maxlist, naux
    integer(I4B) :: idx
    character(len=LENBUDTXT) :: text
    character(len=LENBUDTXT), dimension(1) :: auxtxt
! ------------------------------------------------------------------------------
    !
    ! -- Determine the number of maw budget terms. These are fixed for 
    !    the simulation and cannot change.  
    ! gwf rate [flowing_well] storage constant_flow [frommvr tomvr tomvrcf [tomvrfw]] [aux]
    nbudterm = 4
    if (this%iflowingwells > 0) nbudterm = nbudterm + 1
    if (this%imover == 1) then
      nbudterm = nbudterm + 3
      if (this%iflowingwells > 0) nbudterm = nbudterm + 1
    end if
    if (this%naux > 0) nbudterm = nbudterm + 1
    !
    ! -- set up budobj
    call budgetobject_cr(this%budobj, this%name)
    call this%budobj%budgetobject_df(this%nmawwells, nbudterm, 0, 0)
    idx = 0
    !
    ! -- Go through and set up each budget term
    !
    ! -- 
    text = '             GWF'
    idx = idx + 1
    maxlist = this%maxbound 
    naux = 1
    auxtxt(1) = '       FLOW-AREA'
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%name, &
                                             this%name_model, &
                                             this%name_model, &
                                             maxlist, .false., .true., &
                                             naux, auxtxt)
    call this%budobj%budterm(idx)%reset(this%maxbound)
    q = DZERO
    do n = 1, this%nmawwells
      do j = 1, this%ngwfnodes(n)
        n2 = this%mawwells(n)%gwfnodes(j)
        call this%budobj%budterm(idx)%update_term(n, n2, q)
      end do
    end do
    !
    ! -- 
    text = '            RATE'
    idx = idx + 1
    maxlist = this%nmawwells
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                              this%name_model, &
                                              this%name, &
                                              this%name_model, &
                                              this%name, &
                                              maxlist, .false., .false., &
                                              naux)
    !
    ! -- 
    if (this%iflowingwells > 0) then
      text = '         FW-RATE'
      idx = idx + 1
      maxlist = this%nmawwells
      naux = 0
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%name, &
                                               this%name_model, &
                                               this%name, &
                                               maxlist, .false., .false., &
                                               naux)
    end if
    !
    ! -- 
    text = '         STORAGE'
    idx = idx + 1
    maxlist = this%nmawwells 
    naux = 1
    auxtxt(1) = '          VOLUME'
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%name, &
                                             this%name_model, &
                                             this%name_model, &
                                             maxlist, .false., .true., &
                                             naux, auxtxt)
    !
    ! -- 
    text = '        CONSTANT'
    idx = idx + 1
    maxlist = this%nmawwells
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%name, &
                                             this%name_model, &
                                             this%name, &
                                             maxlist, .false., .false., &
                                             naux)
    !
    ! -- 
    if (this%imover == 1) then
      !
      ! -- 
      text = '        FROM-MVR'
      idx = idx + 1
      maxlist = this%nmawwells
      naux = 0
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%name, &
                                               this%name_model, &
                                               this%name, &
                                               maxlist, .false., .false., &
                                               naux)
      !
      ! -- 
      text = '     RATE-TO-MVR'
      idx = idx + 1
      maxlist = this%nmawwells
      naux = 0
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%name, &
                                               this%name_model, &
                                               this%name, &
                                               maxlist, .false., .false., &
                                               naux)
      !
      ! -- constant-head flow to mover
      text = ' CONSTANT-TO-MVR'
      idx = idx + 1
      maxlist = this%nmawwells
      naux = 0
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%name, &
                                               this%name_model, &
                                               this%name, &
                                               maxlist, .false., .false., &
                                               naux)
      !
      ! -- 
      if (this%iflowingwells > 0) then
        !
        ! -- 
        text = '  FW-RATE-TO-MVR'
        idx = idx + 1
        maxlist = this%nmawwells
        naux = 0
        call this%budobj%budterm(idx)%initialize(text, &
                                                 this%name_model, &
                                                 this%name, &
                                                 this%name_model, &
                                                 this%name, &
                                                 maxlist, .false., .false., &
                                                 naux)
      end if
    end if
    !
    ! -- 
    naux = this%naux
    if (naux > 0) then
      !
      ! -- 
      text = '       AUXILIARY'
      idx = idx + 1
      maxlist = this%maxbound
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%name, &
                                               this%name_model, &
                                               this%name, &
                                               maxlist, .false., .false., &
                                               naux, this%auxname)
    end if
    !
    ! -- if maw flow for each reach are written to the listing file
    if (this%iprflow /= 0) then
      call this%budobj%flowtable_df(this%iout)
    end if
    !
    ! -- return
    return
  end subroutine maw_setup_budobj

  subroutine maw_fill_budobj(this)
! ******************************************************************************
! maw_fill_budobj -- copy flow terms into this%budobj
!
! gwf rate [flowing_well] [storage] constant_flow [frommvr tomvr tomvrcf [tomvrfw]] [aux]
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(MawType) :: this
    ! -- local
    integer(I4B) :: naux
    integer(I4B) :: j, n, n2
    integer(I4B) :: idx
    integer(I4B) :: ibnd
    real(DP) :: q
    real(DP) :: tmaw
    real(DP) :: bmaw
    real(DP) :: sat
    real(DP) :: qfact
    real(DP) :: q2
    real(DP) :: b
    real(DP) :: v
    ! -- formats
! -----------------------------------------------------------------------------
    !
    ! -- initialize counter
    idx = 0
    !
    ! -- GWF (LEAKAGE) and connection surface area (aux)
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%maxbound)
    ibnd = 1
    do n = 1, this%nmawwells
      do j = 1, this%ngwfnodes(n)
        n2 = this%mawwells(n)%gwfnodes(j)
        tmaw = this%mawwells(n)%topscrn(j)
        bmaw = this%mawwells(n)%botscrn(j)
        call this%maw_calculate_saturation(n, j, n2, sat)
        this%qauxcbc(1) = DTWO * DPI * this%radius(n) * sat * (tmaw - bmaw)
        q = this%qleak(ibnd)
        call this%budobj%budterm(idx)%update_term(n, n2, q, this%qauxcbc)
        ibnd = ibnd + 1
      end do
    end do
    !
    ! -- RATE (WITHDRAWAL RATE)
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%nmawwells)
    do n = 1, this%nmawwells
      q = this%ratesim(n)
      !
      ! -- adjust if well rate is an outflow
      if (this%imover == 1 .and. q < DZERO) then
        qfact = DONE
        if (this%qout(n) < DZERO) then
          qfact = q / this%qout(n)
        end if
        q = q + qfact * this%pakmvrobj%get_qtomvr(n)
      end if
      call this%budobj%budterm(idx)%update_term(n, n, q)
    end do
    !
    ! -- FLOWING WELL
    if (this%iflowingwells > 0) then
      idx = idx + 1
      call this%budobj%budterm(idx)%reset(this%nmawwells)
      do n = 1, this%nmawwells
        q = this%qfw(n)
        if (this%imover == 1) then
          qfact = DONE
          ! -- adjust if well rate is an outflow
          if (this%qout(n) < DZERO) then
            qfact = q / this%qout(n)
          end if
          q = q + qfact * this%pakmvrobj%get_qtomvr(n)
        end if
        call this%budobj%budterm(idx)%update_term(n, n, q)
      end do
    end if
    !
    ! -- STORAGE (AND VOLUME AS AUX)
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%nmawwells)
    do n = 1, this%nmawwells
      b = this%xsto(n) - this%bot(n)
      if (b < DZERO) then
        b = DZERO
      end if
      v = this%area(n) * b
      if (this%imawissopt /= 1) then
        q = this%qsto(n)
      else
        q = DZERO
      end if
      this%qauxcbc(1) = v
      call this%budobj%budterm(idx)%update_term(n, n, q, this%qauxcbc)
    end do
    !
    ! -- CONSTANT FLOW
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%nmawwells)
    do n = 1, this%nmawwells
      q = this%qconst(n)
      !
      ! -- adjust if constant-flow rate is an outflow
      if (this%imover == 1 .and. q < DZERO) then
        qfact = DONE
        if (this%qout(n) < DZERO) then
          qfact = q / this%qout(n)
        end if
        q = q + qfact * this%pakmvrobj%get_qtomvr(n)
      end if
      call this%budobj%budterm(idx)%update_term(n, n, q)
    end do
    !
    ! -- MOVER
    if (this%imover == 1) then
      !
      ! -- FROM MOVER
      idx = idx + 1
      call this%budobj%budterm(idx)%reset(this%nmawwells)
      do n = 1, this%nmawwells
        if (this%iboundpak(n) == 0) then
          q = DZERO
        else
          q = this%pakmvrobj%get_qfrommvr(n)
        end if
        call this%budobj%budterm(idx)%update_term(n, n, q)
      end do
      !
      ! -- RATE TO MOVER
      idx = idx + 1
      call this%budobj%budterm(idx)%reset(this%nmawwells)
      do n = 1, this%nmawwells
        q = this%pakmvrobj%get_qtomvr(n)
        if (q > DZERO) then
          q = -q
          q2 = this%ratesim(n)
          !
          ! -- adjust TO MOVER if well rate is outflow
          if (q2 < DZERO) then
            qfact = q2 / this%qout(n)
            q = q * qfact
          else
            q = DZERO
          end if
        end if
        call this%budobj%budterm(idx)%update_term(n, n, q)
      end do
      !
      ! -- CONSTANT TO MOVER
      idx = idx + 1
      call this%budobj%budterm(idx)%reset(this%nmawwells)
      do n = 1, this%nmawwells
        q = this%pakmvrobj%get_qtomvr(n)
        if (q > DZERO) then
          q = -q
          q2 = this%qconst(n)
          ! -- adjust TO MOVER if well rate is outflow
          if (q2 < DZERO) then
            qfact = q2 / this%qout(n)
            q = q * qfact
          else
            q = DZERO
          end if
        end if
        call this%budobj%budterm(idx)%update_term(n, n, q)
      end do
      !
      ! -- FLOWING WELL TO MOVER
      if (this%iflowingwells > 0) then
        idx = idx + 1
        call this%budobj%budterm(idx)%reset(this%nmawwells)
        do n = 1, this%nmawwells
          q = this%pakmvrobj%get_qtomvr(n)
          if (q > DZERO) then
            q = -q
            q2 = this%ratesim(n)
            !
            ! -- adjust TO MOVER if well rate is outflow
            qfact = DONE
            if (this%qout(n) < DZERO) then
              qfact = this%qfw(n) / this%qout(n)
            end if
            q = q * qfact
          end if
          call this%budobj%budterm(idx)%update_term(n, n, q)
        end do
      end if
      
    end if
    !
    ! -- AUXILIARY VARIABLES
    naux = this%naux
    if (naux > 0) then
      idx = idx + 1
      call this%budobj%budterm(idx)%reset(this%nmawwells)
      do n = 1, this%nmawwells
        q = DZERO
        call this%budobj%budterm(idx)%update_term(n, n, q, this%auxvar(:, n))
      end do
    end if
    !
    ! --Terms are filled, now accumulate them for this time step
    call this%budobj%accumulate_terms()
    !
    ! -- return
    return
  end subroutine maw_fill_budobj

  subroutine maw_setup_tableobj(this)
! ******************************************************************************
! maw_setup_tableobj -- Set up the table object that is used to write the maw 
!                       head data. The terms listed here must correspond in  
!                       number and order to the ones written to the head table 
!                       in the maw_ot method.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH, LENBUDTXT
    ! -- dummy
    class(MawType) :: this
    ! -- local
    integer(I4B) :: nterms
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: text
! ------------------------------------------------------------------------------
    !
    ! -- setup well head table
    if (this%iprhed > 0) then
      !
      ! -- Determine the number of head table columns
      nterms = 2
      if (this%inamedbound == 1) nterms = nterms + 1
      !
      ! -- set up table title
      title = trim(adjustl(this%text)) // ' PACKAGE (' //                        &
              trim(adjustl(this%name)) //') HEADS FOR EACH CONTROL VOLUME'
      !
      ! -- set up head tableobj
      call table_cr(this%headtab, this%name, title)
      call this%headtab%table_df(this%nmawwells, nterms, this%iout,              &
                                 transient=.TRUE.)
      !
      ! -- Go through and set up table budget term
      if (this%inamedbound == 1) then
        text = 'NAME'
        call this%headtab%initialize_column(text, 20, alignment=TABLEFT)
      end if
      !
      ! -- reach number
      text = 'NUMBER'
      call this%headtab%initialize_column(text, 10, alignment=TABCENTER)
      !
      ! -- reach stage
      text = 'HEAD'
      call this%headtab%initialize_column(text, 12, alignment=TABCENTER)
    end if
    !
    ! -- return
    return
  end subroutine maw_setup_tableobj

end module MawModule

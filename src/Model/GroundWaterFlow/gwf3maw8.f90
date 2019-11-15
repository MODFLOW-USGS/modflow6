module mawmodule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, LENBOUNDNAME, LENTIMESERIESNAME,       &
  &                          DZERO, DEM6, DEM4, DEM2, DHALF, DP7, DP9, DONE,    &
  &                          DTWO, DPI, DEIGHT, DHUNDRED, DEP20,                &
  &                          NAMEDBOUNDFLAG, LENPACKAGENAME, LENAUXNAME,        &
  &                          LENFTYPE, DHNOFLO, DHDRY, DNODATA, MAXCHARLEN
  use SmoothingModule,  only: sQuadraticSaturation, sQSaturation, &
  &                           sQuadraticSaturationDerivative, sQSaturationDerivative
  use BndModule, only: BndType
  use BudgetModule, only : BudgetType

  use ObserveModule,        only: ObserveType
  use ObsModule, only: ObsType
  use InputOutputModule, only: get_node, URWORD, extract_idnum_or_bndname
  use BaseDisModule, only: DisBaseType
  use SimModule,        only: count_errors, store_error, store_error_unit, ustop
  use ArrayHandlersModule, only: ExpandArray
  use BlockParserModule,   only: BlockParserType
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_setptr,      &
                                 mem_deallocate
  !
  implicit none
  !
  character(len=LENFTYPE)       :: ftype = 'MAW'
  character(len=LENPACKAGENAME) :: text  = '             MAW'
  !
  type :: MawWellTSType
    character (len=LENTIMESERIESNAME), pointer :: name => null()
    real(DP), pointer :: value => null()
  end type MawWellTSType

  type :: MawWellType
    character (len=LENBOUNDNAME), pointer :: name => null()
    character (len=8), pointer :: status => null()
    integer(I4B), pointer :: ngwfnodes => null()
    integer(I4B), pointer :: ieqn => null()
    integer(I4B), pointer :: ishutoff => null()
    integer(I4B), pointer :: ifwdischarge => null()
    real(DP), pointer :: strt => null()
    real(DP), pointer :: radius => null()
    real(DP), pointer :: area => null()
    real(DP), pointer :: pumpelev => null()
    real(DP), pointer :: bot => null()
    real(DP), pointer :: ratesim => null()
    real(DP), pointer :: reduction_length => null()
    real(DP), pointer :: fwelev => null()
    real(DP), pointer :: fwcond => null()
    real(DP), pointer :: fwrlen => null()
    real(DP), pointer :: fwcondsim => null()
    real(DP), pointer :: xsto => null()
    real(DP), pointer :: xoldsto => null()
    real(DP), pointer :: shutoffmin => null()
    real(DP), pointer :: shutoffmax => null()
    real(DP), pointer :: shutofflevel => null()
    real(DP), pointer :: shutoffweight => null()
    real(DP), pointer :: shutoffdq => null()
    real(DP), pointer :: shutoffqold => null()
    ! -- vectors
    integer(I4B), dimension(:), pointer, contiguous :: gwfnodes => NULL()
    real(DP), dimension(:), pointer, contiguous :: sradius => NULL()
    real(DP), dimension(:), pointer, contiguous :: hk => NULL()
    real(DP), dimension(:), pointer, contiguous :: satcond => NULL()
    real(DP), dimension(:), pointer, contiguous :: simcond => NULL()
    real(DP), dimension(:), pointer, contiguous :: topscrn => NULL()
    real(DP), dimension(:), pointer, contiguous :: botscrn => NULL()
    ! -- time-series aware data
    ! -- aux data
    type (MawWellTSType), dimension(:), pointer, contiguous :: auxvar => null()
    ! -- pumping rate
    type(MawWellTSType), pointer :: rate => null()
    ! -- well head
    type(MawWellTSType), pointer :: head => null()
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
    ! -- for budgets
    integer(I4B), pointer :: bditems => NULL()
    !
    ! -- for underrelaxation of estimated well q if using shutoff
    real(DP), pointer :: theta => NULL()
    real(DP), pointer :: kappa => NULL()
    !
    ! -- derived types
    type(BudgetType), pointer :: budget => NULL()
    type(MawWellType), dimension(:), pointer, contiguous :: mawwells => NULL()
    !
    ! -- pointer to gwf iss and gwf hk
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
    ! -- imap vector
    integer(I4B), dimension(:), pointer, contiguous :: imap       => null()
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
    procedure :: maw_allocate_arrays
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
    procedure, private :: maw_allocate_well
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

  subroutine maw_allocate_arrays(this)
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
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_arrays()
    !
    ! -- allocate cmawname
    allocate(this%cmawname(this%nmawwells))
    !
    ! -- allocate idxmawconn
    call mem_allocate(this%idxmawconn, this%nmawwells+1, 'IDXMAWCONN', this%origin)
    !
    ! -- allocate imap
    call mem_allocate(this%imap, this%MAXBOUND, 'IMAP', this%origin)
    !
    ! -- initialize idxmawconn and imap
    do i = 1, this%nmawwells+1
      this%idxmawconn(i) = 0
    end do
    do i = 1, this%maxbound
      this%imap(i) = 0
    end do
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
    ! -- allocate qleak and qsto
    call mem_allocate(this%qleak, this%maxbound, 'QLEAK', this%origin)
    do i = 1, this%maxbound
      this%qleak(i) = DZERO
    end do
    if (this%iflowingwells /= 0) then
      call mem_allocate(this%qfw, this%nmawwells, 'QFW', this%origin)
    else
      call mem_allocate(this%qfw, 1, 'QFW', this%origin)
    end if
    call mem_allocate(this%qout, this%nmawwells, 'QOUT', this%origin)
    call mem_allocate(this%qsto, this%nmawwells, 'QSTO', this%origin)
    call mem_allocate(this%qconst, this%nmawwells, 'QCONST', this%origin)
    do i = 1, this%nmawwells
      if (this%iflowingwells /= 0) this%qfw(i) = DZERO
      this%qsto(i) = DZERO
      this%qconst(i) = DZERO
    end do
    !
    ! -- return
    return
  end subroutine maw_allocate_arrays

  subroutine maw_read_wells(this)
! ******************************************************************************
! pak1read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    ! -- dummy
    class(MawType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: text, keyword, cstr
    character(len=LINELENGTH) :: strttext
    character(len=LENBOUNDNAME) :: bndName, bndNameTemp
    character(len=9) :: cno
    character(len=50), dimension(:), allocatable :: caux
    integer(I4B) :: ival
    logical :: isfound, endOfBlock
    real(DP) :: rval
    integer(I4B) :: n
    integer(I4B) :: jj
    integer(I4B) :: iaux
    integer(I4B) :: itmp
    integer(I4B) :: ierr
    real(DP) :: endtim
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
    ! -- allocate pointers
    do n = 1, this%nmawwells
      call this%maw_allocate_well(n)
    enddo
    this%npakeq = this%nmawwells
    !
    ! -- allocate local storage for aux variables
    if (this%naux > 0) then
      allocate(caux(this%naux))
    end if
    !
    ! -- read maw well data
    ! -- get wells block
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr, &
      supportopenclose=.true.)
    !
    ! -- parse locations block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        ival = this%parser%GetInteger()
        n = ival

        if (n < 1 .or. n > this%nmawwells) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. IMAW MUST BE > 0 and <= ', this%nmawwells
          call store_error(errmsg)
          cycle
        end if
        
        ! -- increment nboundchk
        nboundchk(n) = nboundchk(n) + 1

        ! -- radius
        rval = this%parser%GetDouble()
        if (rval <= DZERO) then
          write(errmsg,'(4x,a,1x,i6,1x,a)') &
            '****ERROR. RADIUS FOR WELL', n, 'MUST BE GR5EATER THAN ZERO.'
          call store_error(errmsg)
          cycle
        end if
        this%mawwells(n)%radius = rval
        this%mawwells(n)%area = DPI * rval**DTWO
        ! -- well bottom
        this%mawwells(n)%bot = this%parser%GetDouble()
        ! -- strt
        call this%parser%GetString(strttext)
        ! -- ieqn
        call this%parser%GetStringCaps(keyword)
        if (keyword=='SPECIFIED') then
          this%mawwells(n)%ieqn = 0
        else if (keyword=='THEIM' .or. keyword=='THIEM') then
          this%mawwells(n)%ieqn = 1
        else if (keyword=='SKIN') then
          this%mawwells(n)%ieqn = 2
        else if (keyword=='CUMULATIVE') then
          this%mawwells(n)%ieqn = 3
        else if (keyword=='MEAN') then
          this%mawwells(n)%ieqn = 4
        else
          write(errmsg,'(4x,a,1x,i6,1x,a)') &
            '****ERROR. CONDEQN FOR WELL', n, &
            'MUST BE "CONDUCTANCE", "THIEM" "MEAN", OR "SKIN".'
        end if
        ! -- ngwnodes
        ival = this%parser%GetInteger()
        if (ival < 1) then
          write(errmsg,'(4x,a,1x,i6,1x,a)') &
            '****ERROR. NGWFNODES FOR WELL', n, 'MUST BE GREATER THAN ZERO'
          call store_error(errmsg)
        end if
        
        if (ival > 0) then
          this%mawwells(n)%ngwfnodes = ival
        end if

        ! -- allocate storage for connection data needed for the MAW well
        allocate(this%mawwells(n)%gwfnodes(this%mawwells(n)%ngwfnodes))
        allocate(this%mawwells(n)%satcond(this%mawwells(n)%ngwfnodes))
        allocate(this%mawwells(n)%simcond(this%mawwells(n)%ngwfnodes))
        allocate(this%mawwells(n)%topscrn(this%mawwells(n)%ngwfnodes))
        allocate(this%mawwells(n)%botscrn(this%mawwells(n)%ngwfnodes))
        if (this%mawwells(n)%ieqn==2 .OR. this%mawwells(n)%ieqn==3 .OR.       &
            this%mawwells(n)%ieqn==4) then
          allocate(this%mawwells(n)%hk(this%mawwells(n)%ngwfnodes))
        end if
        if (this%mawwells(n)%ieqn==2 .OR. this%mawwells(n)%ieqn==3 .OR.       &
            this%mawwells(n)%ieqn==4) then
          allocate(this%mawwells(n)%sradius(this%mawwells(n)%ngwfnodes))
        end if

        ! -- increment maxbound
        itmp = itmp + this%mawwells(n)%ngwfnodes
        !
        ! -- set default bndName
        write (cno,'(i9.9)') n
        bndName = 'MAWWELL' // cno

        ! -- get aux data
        do iaux = 1, this%naux
          call this%parser%GetString(caux(iaux))
        end do

        ! -- read well name
        this%mawwells(n)%name = bndName
        if (this%inamedbound /= 0) then
          call this%parser%GetStringCaps(bndNameTemp)
          if (bndNameTemp /= '') then
            this%mawwells(n)%name = bndNameTemp(1:16)
          endif
        else
          bndName = ''
        endif
        ! fill timeseries aware data
        jj = 1    ! For WELL_HEAD
        endtim = DZERO
        call read_single_value_or_time_series(strttext, &
                                              this%mawwells(n)%head%value, &
                                              this%mawwells(n)%head%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, 'HEAD', &
                                              bndName, this%parser%iuactive)
        this%mawwells(n)%strt = this%mawwells(n)%head%value
        if (this%mawwells(n)%strt < this%mawwells(n)%bot) then
          write(cstr, fmthdbot) this%mawwells(n)%strt, this%mawwells(n)%bot
          call this%maw_set_attribute_error(n, 'STRT', trim(cstr))
        end if

        ! -- fill aux data
        do iaux = 1, this%naux
          text = caux(iaux)
          jj = 1 !iaux
          call read_single_value_or_time_series(trim(adjustl(text)), &
                                                this%mawwells(n)%auxvar(iaux)%value, &
                                                this%mawwells(n)%auxvar(iaux)%name, &
                                                endtim,  &
                                                this%Name, 'AUX', this%TsManager, &
                                                this%iprpak, n, jj, &
                                                this%auxname(iaux), &
                                                bndName, this%parser%iuactive)
        end do
      end do

      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' PACKAGEDATA'
      !
      ! -- check for duplicate or missing wells
      do n = 1,  this%nmawwells
        if (nboundchk(n) == 0) then
          write(errmsg,'(a,1x,i0)')  'ERROR.  NO DATA SPECIFIED FOR MAW WELL', n
          call store_error(errmsg)
        else if (nboundchk(n) > 1) then
          write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                             &
            'ERROR.  DATA FOR MAW WELL', n, 'SPECIFIED', nboundchk(n), 'TIMES'
          call store_error(errmsg)
        end if
      end do
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
    this%MAXBOUND = itmp
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
    use SimModule, only: ustop, store_error, count_errors
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
      iachk(n+1) = iachk(n) +  this%mawwells(n)%ngwfnodes
    end do
    allocate(nboundchk(this%MAXBOUND))
    do n = 1, this%MAXBOUND
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
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. IMAW MUST BE > 0 and <= ', this%nmawwells
          call store_error(errmsg)
          cycle
        end if

        ! -- read connection number
        ival = this%parser%GetInteger()
        if (ival < 1 .or. ival > this%mawwells(n)%ngwfnodes) then
          write(errmsg,'(4x,a,1x,i4,1x,a,1x,i6)') &
            '****ERROR. JCONN FOR WELL ', n, 'MUST BE > 1 and <= ', this%mawwells(n)%ngwfnodes
          call store_error(errmsg)
          cycle
        end if
        
        ipos = iachk(n) + ival - 1
        nboundchk(ipos) = nboundchk(ipos) + 1
        
        j = ival
        ! -- read gwfnodes from the line
        call this%parser%GetCellid(this%dis%ndim, cellid)
        nn  = this%dis%noder_from_cellid(cellid, this%inunit, this%iout)
        topnn = this%dis%top(nn)
        botnn = this%dis%bot(nn)
        botw = this%mawwells(n)%bot
        ! -- set gwf node number for connection
        this%mawwells(n)%gwfnodes(j) = nn
        ! -- top of screen
        rval = this%parser%GetDouble()
        if (this%mawwells(n)%ieqn /= 4) then
          rval = topnn
        else
          if (rval > topnn) then
            rval = topnn
          end if
        end if
        this%mawwells(n)%topscrn(j)  = rval
        ! -- bottom of screen
        rval = this%parser%GetDouble()
        if (this%mawwells(n)%ieqn /= 4) then
          rval = botnn
        else
          if (rval < botnn) then
            rval = botnn
          end if
        end if
        this%mawwells(n)%botscrn(j)  = rval
        ! adjust the bottom of the well for all conductance approaches
        ! except for "mean"
        if (this%mawwells(n)%ieqn /= 4) then
          if (rval < botw) then
            botw = rval
            this%mawwells(n)%bot = rval
          end if
        end if
        ! -- hydraulic conductivity or conductance
        rval = this%parser%GetDouble()
        if (this%mawwells(n)%ieqn==0) then
          this%mawwells(n)%satcond(j)  = rval
        else if (this%mawwells(n)%ieqn==2 .OR. this%mawwells(n)%ieqn==3 .OR.  &
                 this%mawwells(n)%ieqn==4) then
          this%mawwells(n)%hk(j) = rval
        end if
        ! -- skin radius
        rval = this%parser%GetDouble()
        if (this%mawwells(n)%ieqn==2 .OR. this%mawwells(n)%ieqn==3 .OR.       &
            this%mawwells(n)%ieqn==4) then
          this%mawwells(n)%sradius(j) = rval
        end if
      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' CONNECTIONDATA'
      
      ipos = 0
      do n = 1, this%nmawwells
        do j = 1, this%mawwells(n)%ngwfnodes
          ipos = ipos + 1
          !
          ! -- check for missing or duplicate maw well connections
          if (nboundchk(ipos) == 0) then
            write(errmsg,'(a,1x,i0,1x,a,1x,i0)')                                &
              'ERROR.  NO DATA SPECIFIED FOR MAW WELL', n, 'CONNECTION', j
            call store_error(errmsg)
          else if (nboundchk(ipos) > 1) then
            write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a,1x,i0,1x,a)')                &
              'ERROR.  DATA FOR MAW WELL', n, 'CONNECTION', j,                  &
              'SPECIFIED', nboundchk(n), 'TIMES'
            call store_error(errmsg)
          end if
        end do
      end do
      !
      ! -- make sure that more than one connection per cell is only specified 
      !    wells using the mean conducance type
      do n = 1, this%nmawwells
        if (this%mawwells(n)%ieqn /= 4) then
          do j = 1, this%mawwells(n)%ngwfnodes
            nn = this%mawwells(n)%gwfnodes(j) 
            do jj = 1, this%mawwells(n)%ngwfnodes
              ! skip current maw node
              if (jj == j) then
                cycle
              end if
              nn2 =  this%mawwells(n)%gwfnodes(jj) 
              if (nn2 == nn) then
                call this%dis%noder_to_string(nn, nodestr)
                write(errmsg,'(a,1x,i0,1x,a,1x,i0,3(1x,a))')                    &
                  'ERROR.  ONLY ONE CONNECTION CAN BE SPECIFIED FOR MAW WELL',  &
                  n, 'CONNECTION', j, 'TO GWF CELL', trim(adjustl(nodestr)),   &
                  'UNLESS THE MEAN CONDEQN IS SPECIFIED'
                call store_error(errmsg)
              end if
            end do
          end do
        end if
      end do
    else
      call store_error('ERROR.  REQUIRED CONNECTIONDATA BLOCK NOT FOUND.')
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
    use SimModule, only: ustop, store_error, count_errors
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
    ierr = count_errors()
    if (ierr > 0) then
      call ustop()
    end if
    !
    ! -- read wells block
    call this%maw_read_wells()
    !
    ! -- read well_connections block
    call this%maw_read_well_connections()
    !
    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- return
    return
  end subroutine maw_read_dimensions


  subroutine maw_read_initial_attr(this)
! ******************************************************************************
! pak1read_dimensions -- Read the initial parameters for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors
    use MemoryManagerModule, only: mem_setptr
    use BudgetModule, only: budget_cr
    ! -- dummy
    class(MawType),intent(inout) :: this
    ! -- local
    integer(I4B) :: j, n
    integer(I4B) :: nn
    integer(I4B) :: inode
    integer(I4B) :: idx
    integer(I4B) :: ival
    real(DP) :: k11, k22
    character (len=10), dimension(0:4) :: ccond
    character (len=30) :: nodestr
    character (len=10) :: crskin, ckskin
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
    ! -- setup the maw budget
    call budget_cr(this%budget, this%origin)
    ival = this%bditems
    if (this%iflowingwells /= 1) ival = this%bditems - 1
    call this%budget%budget_df(ival, this%name, 'L**3')
    !
    ! -- initialize xnewpak
    do n = 1, this%nmawwells
      this%xnewpak(n) = this%mawwells(n)%strt
    end do
    !
    ! -- initialize status (iboundpak) of maw wells to active
    do n = 1, this%nmawwells
      if (this%mawwells(n)%status == 'CONSTANT') then
        this%iboundpak(n) = -1
      else if (this%mawwells(n)%status == 'INACTIVE') then
        this%iboundpak(n) = 0
      else if (this%mawwells(n)%status == 'ACTIVE ') then
        this%iboundpak(n) = 1
      end if
    end do
    !
    ! -- set idxmawconn and imap for each connection
    idx = 0
    this%idxmawconn(1) = 1
    do n = 1, this%nmawwells
      do j = 1, this%mawwells(n)%ngwfnodes
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
        this%cmawname(n) = this%mawwells(n)%name
        do j = 1, this%mawwells(n)%ngwfnodes
          idx = idx + 1
          this%boundname(idx) = this%mawwells(n)%name
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
        do j = 1, this%mawwells(n)%ngwfnodes
          idx = idx + 1
          this%boundname(idx) = this%mawwells(n)%name
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
      do j = 1, this%mawwells(n)%ngwfnodes
        if (this%mawwells(n)%ieqn /= 0) then
          inode = this%mawwells(n)%gwfnodes(j)
          call this%maw_calculate_satcond(n, j, inode)
        end if
      end do
    end do
    !
    ! -- write summary of static well data
    ! -- write well data
    write (this%iout,fmtwelln) '  WELL NO.', '    RADIUS', '      AREA', &
                               ' WELL BOT.', '      STRT', ' NGWFNODES', &
                               'CONDEQN   ', 'NAME            '
    do n = 1, this%nmawwells
      write (this%iout,fmtwelld) n, this%mawwells(n)%radius, this%mawwells(n)%area, &
                                 this%mawwells(n)%bot, this%mawwells(n)%strt, &
                                 this%mawwells(n)%ngwfnodes, &
                                 ccond(this%mawwells(n)%ieqn), &
                                 this%mawwells(n)%name
    end do
    ! -- write end line
    write (this%iout,fmtline)
    !
    ! -- write well connection data
    write (this%iout,fmtwellcn) '  WELL NO.', 'WELL CONN', &
                               'CELL                ',     &
                               '  TOP SCRN', '  BOT SCRN', &
                               ' SKIN RAD.', '    SKIN K', &
                               '       K11', '       K22', &
                               'WELL COND.'
    do n = 1, this%nmawwells
      do j = 1, this%mawwells(n)%ngwfnodes
        nn = this%mawwells(n)%gwfnodes(j)
        call this%dis%noder_to_string(nn, nodestr)
        crskin = '          '
        ckskin = '          '
        if (this%mawwells(n)%ieqn == 2 .or. &
            this%mawwells(n)%ieqn == 3 .or. &
            this%mawwells(n)%ieqn == 4) then
          write (crskin, '(G10.3)') this%mawwells(n)%sradius(j)
          write (ckskin, '(G10.3)') this%mawwells(n)%hk(j)
        end if
        k11 = this%gwfk11(nn)
        if(this%gwfik22 == 0) then
          k22 = this%gwfk11(nn)
        else
          k22 = this%gwfk22(nn)
        endif
        write (this%iout,fmtwellcd) n, j, nodestr, &
                                    this%mawwells(n)%topscrn(j), &
                                    this%mawwells(n)%botscrn(j), &
                                    crskin, ckskin, &
                                    k11, k22, this%mawwells(n)%satcond(j)
      end do
    end do
    !
    ! -- write end line
    write (this%iout,fmtline)
    !
    ! -- finished with pointer to gwf hydraulic conductivity
    this%gwfk11 => null()
    this%gwfk22 => null()
    this%gwfik22 => null()
    this%gwfsat => null()
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
    use TdisModule, only: kper, perlen, totimsav
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    use SimModule, only: ustop, store_error
    ! -- dummy
    class(MawType),intent(inout) :: this
    integer(I4B), intent(in) :: imaw
    character (len=*), intent(in) :: line
    ! -- local
    character(len=LINELENGTH) :: text, cstr
    character(len=LINELENGTH) :: caux
    character(len=LINELENGTH) :: keyword
    character(len=LINELENGTH) :: errmsg
    character(len=LENBOUNDNAME) :: bndName
    character(len=9) :: cmaw
    integer(I4B) :: ival, istart, istop
    integer(I4B) :: i0
    integer(I4B) :: lloc
    integer(I4B) :: ii
    integer(I4B) :: jj
    integer(I4B) :: idx
    integer(I4B) :: iaux
    real(DP) :: rval
    real(DP) :: endtim
    integer(I4B) :: istat
    character(len=MAXCHARLEN) :: ermsg, ermsgr
    ! -- formats
    character(len=*),parameter :: fmthdbot = &
      "('well head (',G0,') must be >= BOTTOM_ELEVATION (',G0, ').')"
! ------------------------------------------------------------------------------
    !
    ! -- Find time interval of current stress period.
    endtim = totimsav + perlen(kper)
    !
    ! -- Assign boundary name, if available
    !
    ! -- set default bndName
    write (cmaw,'(i9.9)') imaw
    !bndName = 'MAWWELL' // cmaw
    if (this%inamedbound==1) then
      idx = 0
      do ii = 1, imaw
        do jj = 1, this%mawwells(ii)%ngwfnodes
          idx = idx + 1
        end do
      end do
      bndName = this%boundname(idx)
    else
      bndName = ''
    endif
    !
    ! -- read line
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, this%iout, this%inunit)
    i0 = istart
    keyword = line(istart:istop)
    select case (line(istart:istop))
      case ('STATUS')
        call urword(line, lloc, istart, istop, 1, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        this%mawwells(imaw)%status = text(1:8)
        if (text == 'CONSTANT') then
          this%iboundpak(imaw) = -1
        else if (text == 'INACTIVE') then
          this%iboundpak(imaw) = 0
        else if (text == 'ACTIVE') then
          this%iboundpak(imaw) = 1
        else
          write(errmsg,'(4x,a,a)') &
            '****ERROR. UNKNOWN '//trim(this%text)//' MAW STATUS KEYWORD: ', &
            text
          call store_error(errmsg)
        end if
      case ('RATE')
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For RATE
        call read_single_value_or_time_series(text, &
                                              this%mawwells(imaw)%rate%value, &
                                              this%mawwells(imaw)%rate%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, imaw, jj, 'RATE', &
                                              bndName, this%inunit)
     case ('WELL_HEAD')
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For WELL_HEAD
        call read_single_value_or_time_series(text, &
                                              this%mawwells(imaw)%head%value, &
                                              this%mawwells(imaw)%head%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, imaw, jj, 'HEAD', &
                                              bndName, this%inunit)
        this%xnewpak(imaw) = this%mawwells(imaw)%head%value
        if (this%mawwells(imaw)%head%value < this%mawwells(imaw)%bot) then
          write(cstr, fmthdbot) this%mawwells(imaw)%head%value, this%mawwells(imaw)%bot
          call this%maw_set_attribute_error(imaw, 'WELL HEAD', trim(cstr))
        end if
      case ('FLOWING_WELL')
        call urword(line, lloc, istart, istop, 3, ival, rval, this%iout, this%inunit)
        this%mawwells(imaw)%fwelev = rval
        call urword(line, lloc, istart, istop, 3, ival, rval, this%iout, this%inunit)
        this%mawwells(imaw)%fwcond = rval
        call urword(line, lloc, istart, istop, 3, ival, rval, -this%iout, this%inunit)
        this%mawwells(imaw)%fwrlen = rval
      case ('RATE_SCALING')
        call urword(line, lloc, istart, istop, 3, ival, rval, this%iout, this%inunit)
        this%mawwells(imaw)%pumpelev = rval
        call urword(line, lloc, istart, istop, 3, ival, rval, this%iout, this%inunit)
        this%mawwells(imaw)%reduction_length = rval
        if (rval < DZERO) then
          call this%maw_set_attribute_error(imaw, trim(keyword), 'must be >= 0.')
        end if
      case ('HEAD_LIMIT')
        call urword(line, lloc, istart, istop, 1, ival, rval, this%iout, this%inunit)
        if (line(istart:istop) == 'OFF') then
          this%mawwells(imaw)%shutofflevel = DEP20
        else
          read (line(istart:istop), *,iostat=istat,iomsg=ermsgr) this%mawwells(imaw)%shutofflevel
          if (istat /= 0) then
            ermsg = 'Error reading HEAD_LIMIT value.'
            call store_error(ermsg)
            call store_error(ermsgr)
            call ustop()
          endif
        end if
      case ('SHUT_OFF')
        call urword(line, lloc, istart, istop, 3, ival, rval, this%iout, this%inunit)
        this%mawwells(imaw)%shutoffmin = rval
        call urword(line, lloc, istart, istop, 3, ival, rval, this%iout, this%inunit)
        this%mawwells(imaw)%shutoffmax = rval
      case ('AUXILIARY')
        call urword(line, lloc, istart, istop, 1, ival, rval, this%iout, this%inunit)
        caux = line(istart:istop)
        do iaux = 1, this%naux
          if (trim(adjustl(caux)) /= trim(adjustl(this%auxname(iaux)))) cycle
          call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
          text = line(istart:istop)
          jj = 1 !iaux
          call read_single_value_or_time_series(text, &
                                                this%mawwells(imaw)%auxvar(iaux)%value, &
                                                this%mawwells(imaw)%auxvar(iaux)%name, &
                                                endtim,  &
                                                this%Name, 'AUX', this%TsManager, &
                                                this%iprpak, imaw, jj, &
                                                this%auxname(iaux), bndName, &
                                                this%inunit)
          exit
        end do
      case default
        write(errmsg,'(4x,a,a)') &
          '****ERROR. UNKNOWN '//trim(this%text)//' MAW DATA KEYWORD: ', &
                                  line(istart:istop)
        call store_error(errmsg)
        call ustop()
      end select
    !
    ! -- write keyword data to output file
    if (this%iprpak /= 0) then
      write (this%iout, '(3x,i10,1x,a)') imaw, line(i0:istop)
    end if
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
      write(errmsg,'(4x,a,1x,a,1x,a,1x,i6,1x,a)') &
        '****ERROR.', keyword, ' for MAW well', imaw, 'has already been set.'
    else
      write(errmsg,'(4x,a,1x,a,1x,a,1x,i6,1x,a)') &
        '****ERROR.', keyword, ' for MAW well', imaw, msg
    end if
    call store_error(errmsg)
    ! -- return
    return
  end subroutine maw_set_attribute_error


  subroutine maw_check_attributes(this)
! ******************************************************************************
! maw_check_attributes -- Issue parameter errors for mawweslls(imaw)
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
      if (this%mawwells(n)%ngwfnodes < 1) then
        call this%maw_set_attribute_error(n, 'NGWFNODES', 'must be greater than 0.')
      end if
      ! -- CDL 2/5/2018 Moved to maw_set_stressperiod so it is only done if a
      !    new head is read in.
      !if (this%xnewpak(n) < this%mawwells(n)%bot) then
        !write(cstr, fmthdbot) this%xnewpak(n), this%mawwells(n)%bot
        !call this%maw_set_attribute_error(n, 'WELL HEAD', trim(cstr))
      !end if
      if (this%mawwells(n)%radius == DEP20) then
        call this%maw_set_attribute_error(n, 'RADIUS', 'has not been specified.')
      end if
      if (this%mawwells(n)%shutoffmin > DZERO) then
        if (this%mawwells(n)%shutoffmin >= this%mawwells(n)%shutoffmax) then
          call this%maw_set_attribute_error(n, 'SHUT_OFF', 'shutoffmax must be > shutoffmin.')
        end if
      end if
      do j = 1, this%mawwells(n)%ngwfnodes
        ! -- write gwfnode number
        write(cgwfnode,'(a,1x,i3,1x,a)') 'gwfnode(', j,')'
        if (this%mawwells(n)%botscrn(j) >= this%mawwells(n)%topscrn(j)) then
          call this%maw_set_attribute_error(n, 'SCREEN_TOP', 'screen bottom must be < screen top. '//trim(cgwfnode))
        end if
        if (this%mawwells(n)%ieqn==2 .OR. this%mawwells(n)%ieqn==3 .OR.       &
            this%mawwells(n)%ieqn==4) then
          if (this%mawwells(n)%sradius(j) > DZERO) then
            if (this%mawwells(n)%sradius(j) <= this%mawwells(n)%radius) then
              call this%maw_set_attribute_error(n, 'RADIUS_SKIN', 'skin radius must be >= well radius. '//trim(cgwfnode))
            end if
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
    use SimModule, only: store_error, ustop
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
      do j = 1, this%mawwells(n)%ngwfnodes
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
    use SimModule, only: store_error, ustop
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
      do ii = 1, this%mawwells(n)%ngwfnodes
        j =  this%mawwells(n)%gwfnodes(ii)
        jglo = j + moffset
        searchloop: do jj = iasln(iglo), iasln(iglo + 1) - 1
          if(jglo == jasln(jj)) then
            this%idxdglo(ipos) = iasln(iglo)
            this%idxoffdglo(ipos) = jj
            exit searchloop
          endif
        enddo searchloop
        ipos = ipos + 1
      end do
    end do
    ! -- maw contributions gwf portion of global matrix
    ipos = 1
    do n = 1, this%nmawwells
      do ii = 1, this%mawwells(n)%ngwfnodes
        iglo = this%mawwells(n)%gwfnodes(ii) + moffset
        jglo = moffset + this%dis%nodes + this%ioffset + n
        symsearchloop: do jj = iasln(iglo), iasln(iglo + 1) - 1
          if(jglo == jasln(jj)) then
            this%idxsymdglo(ipos) = iasln(iglo)
            this%idxsymoffdglo(ipos) = jj
            exit symsearchloop
          endif
        enddo symsearchloop
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
    use SimModule, only: ustop, store_error
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
    ! -- Allocate arrays in MAW and in package superclass
    call this%maw_allocate_arrays()
    !
    ! -- read optional initial package parameters
    call this%read_initial_attr()
    !
    ! -- setup pakmvrobj
    if (this%imover /= 0) then
      allocate(this%pakmvrobj)
      call this%pakmvrobj%ar(this%nmawwells, this%nmawwells, this%origin)
    endif
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
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(MawType),intent(inout) :: this
    ! -- local
    character (len=16) :: csteady
    character (len=10) :: chead, credlen, cmin, cmax
    integer(I4B) :: ierr
    integer(I4B) :: node, n
    integer(I4B) :: isofirst
    logical :: isfound, endOfBlock
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: imaw
    integer(I4B) :: ibnd
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
      "/20X,A16,1X,'MAW WELLS'," // &
      "//29X,'RATE DATA'," // &
      "/1X,65('-'),/1X,'  WELL NO.     STATUS       RATE SPEC. HEAD " // &
      " PUMP ELEV  RED. LEN.')"
    character(len=*), parameter :: fmtwelld = &
      "(1X,I10,1X,A10,1X,G10.3,1X,A10,G10.3,1X,A10)"
    character(len=*),parameter :: fmtfwh = &
      "(1X,//21X,'MULTI-AQUIFER WELL DATA'," // &
      "/21X,'FOR STRESS PERIOD',I6," // &
      "//25X,'FLOWING WELL DATA'," // &
      "/1X,65('-'),/12X,'  WELL NO.  ELEVATION   CONDUCT.  RED. LEN.')"
    character(len=*), parameter :: fmtfwd = &
      "(12X,I10,1X,3(G10.3,1X))"
    character(len=*),parameter :: fmtsoh = &
      "(1X,//21X,'MULTI-AQUIFER WELL DATA'," // &
      "/21X,'FOR STRESS PERIOD',I6," // &
      "//25X,'WELL SHUTOFF DATA'," // &
      "/1X,65('-'),/12X,'  WELL NO.  ELEVATION       MINQ       MAXQ')"
    character(len=*), parameter :: fmtsod = &
      "(12X,I10,1X,G10.3,1X,2(A10,1X))"
    character(len=*), parameter :: fmtline = &
      "(1X,65('-'),//)"
! ------------------------------------------------------------------------------
    !
    ! -- initialize flags
    isfirst = 1
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
      endif
    end if
    !
    ! -- Read data if ionper == kper
    if(this%ionper == kper) then

      this%check_attr = 1
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        if (isfirst /= 0) then
          isfirst = 0
          if (this%iprpak /= 0) then
            write(this%iout,'(/1x,a,1x,i6,/)')                                  &
              'READING '//trim(adjustl(this%text))//' DATA FOR PERIOD', kper
            write(this%iout,'(3x,a)') '  MAW WELL KEYWORD AND DATA'
            write(this%iout,'(3x,78("-"))')
          end if
        end if
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
      end do
      if (this%iprpak /= 0) then
        write(this%iout,'(/1x,a,1x,i6,/)')                                      &
          'END OF '//trim(adjustl(this%text))//' DATA FOR PERIOD', kper
      end if
    !
    else
      write(this%iout,fmtlsp) trim(this%filtyp)
    endif
    !
    !write summary of maw well stress period error messages
    ierr = count_errors()
    if (ierr > 0) then
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
        write (this%iout, fmtstdy) kper, csteady
        do n = 1, this%nmawwells
          chead = '    --    '
          if (this%iboundpak(n) < 0) then
            !write (chead,'(G10.3)') this%xnewpak(n)
            write (chead,'(G10.3)') this%mawwells(n)%head%value
          end if
          credlen = '    --    '
          if (this%mawwells(n)%reduction_length /= DEP20) then
            write (credlen,'(G10.3)') this%mawwells(n)%reduction_length
          end if
          write(this%iout,fmtwelld) n, this%mawwells(n)%status, &
                                    this%mawwells(n)%rate%value, chead, &
                                    this%mawwells(n)%pumpelev, &
                                    credlen
        end do
        write (this%iout, fmtline)

        if (this%iflowingwells /= 0) then
          write (this%iout, fmtfwh) kper
          do n = 1, this%nmawwells
            if (this%mawwells(n)%fwcond > DZERO) then
              write(this%iout,fmtfwd) n, this%mawwells(n)%fwelev, &
                                      this%mawwells(n)%fwcond, &
                                      this%mawwells(n)%fwrlen
            end if
          end do
          write (this%iout, fmtline)
        end if

        ! -- shutoff data
        isofirst = 1
        do n = 1, this%nmawwells
          if (this%mawwells(n)%shutofflevel /= DEP20) then
            if (isofirst /= 0) then
              isofirst = 0
              write (this%iout, fmtsoh) kper
            end if
            cmin = '    --    '
            cmax = '    --    '
            if (this%mawwells(n)%shutoffmin > DZERO) then
              write (cmin,'(G10.3)') this%mawwells(n)%shutoffmin
              write (cmax,'(G10.3)') this%mawwells(n)%shutoffmax
            end if
            write(this%iout,fmtsod) n, this%mawwells(n)%shutofflevel, &
                                    cmin, cmax
          end if
        end do
        if (isofirst /= 1) then
          write (this%iout, fmtline)
        end if
      end if
    end if
    !
    ! -- fill arrays
    ibnd = 1
    do n = 1, this%nmawwells
      do j = 1, this%mawwells(n)%ngwfnodes
        node = this%mawwells(n)%gwfnodes(j)
        this%nodelist(ibnd) = node

        this%bound(1,ibnd) = this%xnewpak(n)

        this%bound(2,ibnd) = this%mawwells(n)%satcond(j)

        this%bound(3,ibnd) = this%mawwells(n)%botscrn(j)

        if (this%iboundpak(n) > 0) then
          this%bound(4,ibnd) = this%mawwells(n)%rate%value
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
    integer(I4B) :: j, iaux, ibnd
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
        do j = 1, this%mawwells(n)%ngwfnodes
          do iaux = 1, this%naux
            this%auxvar(iaux, ibnd) = this%mawwells(n)%auxvar(iaux)%value
          end do
          ibnd = ibnd + 1
        end do
      end do
    end if
    !
    ! -- copy xnew into xold
    do n = 1, this%nmawwells
      this%xoldpak(n) = this%xnewpak(n)
      this%mawwells(n)%xoldsto = this%mawwells(n)%xsto
      if (this%iboundpak(n) < 0) then
        this%xnewpak(n) = this%mawwells(n)%head%value
      end if
    end do
    !
    !--use the appropriate xoldsto if intial heads are above the
    !  specified flowing well discharge elevation
    if (kper==1 .and. kstp==1) then
      do n = 1, this%nmawwells
        if (this%mawwells(n)%fwcond > DZERO) then
          if (this%mawwells(n)%xoldsto > this%mawwells(n)%fwelev) then
            this%mawwells(n)%xoldsto = this%mawwells(n)%fwelev
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
    endif
    !
    ! -- For each observation, push simulated value and corresponding
    !    simulation time from "current" to "preceding" and reset
    !    "current" value.
    call this%obs%obs_ad()
    !
    ! -- return
    return
  end subroutine maw_ad

  subroutine maw_cf(this)
  ! ******************************************************************************
  ! maw_cf -- Formulate the HCOF and RHS terms
  ! Subroutine: (1) skip if no multi-aquifer wells
  !             (2) calculate hcof and rhs
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    class(MawType) :: this
  ! ------------------------------------------------------------------------------
    !
    ! -- Calculate maw conductance and update package RHS and HCOF
    call this%maw_cfupdate()
    !
    ! -- pakmvrobj cf
    if(this%imover == 1) then
      call this%pakmvrobj%cf()
    endif
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
    endif
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    idx = 1
    do n = 1, this%nmawwells
      iloc = this%idxlocnode(n)
      ! -- update head value for constant head maw wells
      if (this%iboundpak(n) < 0) then
        this%xnewpak(n) = this%mawwells(n)%head%value
      end if
      hmaw = this%xnewpak(n)
      ! -- add pumping rate to active or constant maw well
      if (this%iboundpak(n) == 0) then
        this%mawwells(n)%ratesim = DZERO
      else
        call this%maw_calculate_wellq(n, hmaw, rate)
        this%mawwells(n)%ratesim = rate
        !write (1999,'(i5,5(g15.7))') this%ishutoffcnt, hmaw, rate, this%mawwells(n)%shutoffqold, &
        !                             this%mawwells(n)%shutoffdq, this%mawwells(n)%shutoffweight
        rhs(iloc) = rhs(iloc) - rate
        ! -- location of diagonal for maw row
        iposd = this%idxdglo(idx)
        ! -- add flowing well
        this%mawwells(n)%xsto = hmaw
        ratefw = DZERO
        if (this%iflowingwells > 0) then
          if (this%mawwells(n)%fwcond > DZERO) then
            bt = this%mawwells(n)%fwelev
            tp = bt + this%mawwells(n)%fwrlen
            scale = sQSaturation(tp, bt, hmaw)
            cfw = scale * this%mawwells(n)%fwcond
            this%mawwells(n)%ifwdischarge = 0
            if (cfw > DZERO) then
              this%mawwells(n)%ifwdischarge = 1
              this%mawwells(n)%xsto = bt
            end if
            this%mawwells(n)%fwcondsim = cfw
            amatsln(iposd) = amatsln(iposd) - cfw
            rhs(iloc) = rhs(iloc) - cfw * bt
            ratefw = cfw * (bt - hmaw)
          end if
        end if
        ! -- add maw storage changes
        if (this%imawiss /= 1) then
          if (this%mawwells(n)%ifwdischarge /= 1) then
            amatsln(iposd) = amatsln(iposd) - (this%mawwells(n)%area / delt)
            rhs(iloc) = rhs(iloc) - (this%mawwells(n)%area * this%mawwells(n)%xoldsto / delt)
          else
            cterm = this%mawwells(n)%xoldsto - this%mawwells(n)%fwelev
            rhs(iloc) = rhs(iloc) - (this%mawwells(n)%area * cterm / delt)
          end if
        end if
        !
        ! -- If mover is active, add receiver water to rhs and
        !    store available water (as positive value)
        if(this%imover == 1) then
          rhs(iloc) = rhs(iloc) - this%pakmvrobj%get_qfrommvr(n)
          call this%pakmvrobj%accumulate_qformvr(n, -rate)  !pumped water
          call this%pakmvrobj%accumulate_qformvr(n, -ratefw) !flowing water
        endif
        !
      endif
      do j = 1, this%mawwells(n)%ngwfnodes
        if (this%iboundpak(n) /= 0) then
          igwfnode = this%mawwells(n)%gwfnodes(j)
          call this%maw_calculate_saturation(n, j, igwfnode, sat)
          cmaw = this%mawwells(n)%satcond(j) * sat
          this%mawwells(n)%simcond(j) = cmaw

          bnode = this%dis%bot(igwfnode)
          bmaw = this%mawwells(n)%botscrn(j)
          ! -- calculate cterm - relative to gwf
          cterm = DZERO
          if (hmaw < bmaw) then
            cterm = cmaw * (bmaw - hmaw)
          end if
          ! -- add to maw row
          iposd = this%idxdglo(idx)
          iposoffd = this%idxoffdglo(idx)
          amatsln(iposd) = amatsln(iposd) - cmaw
          amatsln(iposoffd) = cmaw
          ! -- add correction term
          rhs(iloc) = rhs(iloc) + cterm
          ! -- add to gwf row for maw connection
          isymnode = this%mawwells(n)%gwfnodes(j)
          isymloc = ia(isymnode)
          ipossymd = this%idxsymdglo(idx)
          ipossymoffd = this%idxsymoffdglo(idx)
          amatsln(ipossymd) = amatsln(ipossymd) - cmaw
          amatsln(ipossymoffd) = cmaw
          ! -- add correction term
          rhs(isymnode) = rhs(isymnode) - cterm
        endif
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
      ! -- add pumping rate to active or constant maw well
      if (this%iboundpak(n) /= 0) then
        iposd = this%idxdglo(idx)
        scale = DONE
        drterm = DZERO
        rate = this%mawwells(n)%ratesim
        !--calculate final derivative for pumping rate
        call this%maw_calculate_wellq(n, hmaw+DEM4, rate2)
        drterm = (rate2 - rate) / DEM4
        !--fill amat and rhs with newton-raphson terms
        amatsln(iposd) = amatsln(iposd) + drterm
        rhs(iloc) = rhs(iloc) + drterm * hmaw
        ! -- add flowing well
        if (this%iflowingwells > 0) then
          if (this%mawwells(n)%fwcond > DZERO) then
            bt = this%mawwells(n)%fwelev
            tp = bt + this%mawwells(n)%fwrlen
            scale = sQSaturation(tp, bt, hmaw)
            cfw = scale * this%mawwells(n)%fwcond
            this%mawwells(n)%ifwdischarge = 0
            if (cfw > DZERO) this%mawwells(n)%ifwdischarge = 1
            this%mawwells(n)%fwcondsim = cfw
            rate = cfw * (bt - hmaw)
            rterm = -cfw * hmaw
            !--calculate derivative for flowing well
            if (hmaw < tp) then
              derv = sQSaturationDerivative(tp, bt, hmaw)
              drterm = -(cfw + this%mawwells(n)%fwcond * derv * (hmaw - bt))
              !--fill amat and rhs with newton-raphson terms
              amatsln(iposd) = amatsln(iposd) -                                &
                               this%mawwells(n)%fwcond * derv * (hmaw - bt)
              rhs(iloc) = rhs(iloc) - rterm + drterm * hmaw
            end if
          end if
        end if
        ! -- add maw storage changes
        if (this%imawiss /= 1) then
          if (this%mawwells(n)%ifwdischarge /= 1) then
            rate = this%mawwells(n)%area * hmaw / delt
            rterm = -rate
            !--calculate storage derivative
            drterm = -this%mawwells(n)%area / delt
            !--fill amat and rhs with storage components
            rhs(iloc) = rhs(iloc) - rterm + drterm * hmaw
          end if
        end if
      end if
      do j = 1, this%mawwells(n)%ngwfnodes
        if (this%iboundpak(n) /= 0) then
          igwfnode = this%mawwells(n)%gwfnodes(j)
          hgwf = this%xnew(igwfnode)
          ! -- calculate upstream weighted conductance
          call this%maw_calculate_saturation(n, j, igwfnode, sat)
          cmaw = this%mawwells(n)%satcond(j) * sat
          this%mawwells(n)%simcond(j) = cmaw
          ! -- set top and bottom of the screen
          tmaw = this%mawwells(n)%topscrn(j)
          bmaw = this%mawwells(n)%botscrn(j)
          ! -- add to maw row
          iposd = this%idxdglo(idx)
          iposoffd = this%idxoffdglo(idx)
          ! -- add to gwf row for maw connection
          isymnode = this%mawwells(n)%gwfnodes(j)
          isymloc = ia(isymnode)
          ipossymd = this%idxsymdglo(idx)
          ipossymoffd = this%idxsymoffdglo(idx)
          ! -- calculate newton corrections
          hups = hmaw
          if (hgwf > hups) hups = hgwf
          drterm = sQuadraticSaturationDerivative(tmaw, bmaw, hups, this%satomega)
          ! -- maw is upstream
          if (hmaw > hgwf) then
            term = drterm * this%mawwells(n)%satcond(j) * (hmaw - hgwf)
            rhs(iloc) = rhs(iloc) + term * hmaw
            rhs(isymnode) = rhs(isymnode) - term * hmaw
            amatsln(iposd) = amatsln(iposd) + term
            if (this%ibound(igwfnode) > 0) then
              amatsln(ipossymoffd) = amatsln(ipossymoffd) - term
            end if
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
        endif
        !
        ! -- increment maw connection counter
        idx = idx + 1
      end do
    end do
    !
    ! -- return
    return
  end subroutine maw_fn


  subroutine maw_nur(this, neqpak, x, xtemp, dx, inewtonur)
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
    ! -- local
    integer(I4B) :: n
    real(DP) :: botw
! ------------------------------------------------------------------------------

    !
    ! -- Newton-Raphson under-relaxation
    do n = 1, this%nmawwells
      if (this%iboundpak(n) < 1) cycle
      botw = this%mawwells(n)%bot
      ! -- only apply Newton-Raphson under-relaxation if
      !    solution head is below the bottom of the well
      if (x(n) < botw) then
        inewtonur = 1
        x(n) = xtemp(n)*(DONE-DP9) + botw*DP9
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
    real(DP) :: gwfratin, gwfratout
    real(DP) :: storatin, storatout
    real(DP) :: ratin, ratout
    real(DP) :: chrrate, chratin, chratout
    real(DP) :: fwratin, fwratout
    real(DP) :: mvrratin, mvrratout, mvrfwratout
    real(DP) :: ratsum
    integer(I4B) :: naux
    ! -- for budget
    integer(I4B) :: j, n
    integer(I4B) :: n2
    integer(I4B) :: igwfnode
    integer(I4B) :: ibnd
    real(DP) :: hmaw, hgwf
    real(DP) :: cfw
    real(DP) :: bmaw, cmaw
    real(DP) :: cterm
    real(DP) :: v
    real(DP) :: d
    real(DP) :: b
    real(DP) :: tmaw
    real(DP) :: sat
    real(DP) :: q
    real(DP) :: q2
    real(DP) :: qfact
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
    ! -- call base functionality in bnd_bd
    call this%BndType%bnd_bd(x, idvfl, icbcfl, ibudfl, icbcun, iprobslocal,    &
                             isuppress_output, model_budget, this%imap,        &
                             iadv=1)
    !
    ! -- maw budget routines (start by resetting)
    call this%budget%reset()
    !
    ! -- add to maw budget terms
    ! -- gwf flow
    gwfratin = DZERO
    gwfratout = DZERO
    storatin = DZERO
    storatout = DZERO
    ratin = DZERO
    ratout = DZERO
    chratin = DZERO
    chratout = DZERO
    fwratin = DZERO
    fwratout = DZERO
    mvrratin = DZERO
    mvrratout = DZERO
    mvrfwratout = DZERO
    ratsum = DZERO
    do n = 1, this%nmawwells
      this%qout(n) = DZERO
      this%qsto(n) = DZERO
      if (this%iflowingwells > 0) this%qfw(n) = DZERO
      if (this%iboundpak(n) == 0) cycle
      hmaw = this%xnewpak(n)
      ! -- add pumping rate to active maw well
      if (this%iboundpak(n) > 0) then
        rrate = this%mawwells(n)%ratesim
        if (rrate < DZERO) then
          !
          ! -- Flow is out of maw subtract rate from ratout.
          this%qout(n) = rrate
        end if
        ! -- add flowing well
        this%mawwells(n)%xsto = hmaw
        if (this%iflowingwells > 0) then
          if (this%mawwells(n)%fwcond > DZERO) then
            cfw = this%mawwells(n)%fwcondsim
            this%mawwells(n)%xsto = this%mawwells(n)%fwelev
            rrate = cfw * (this%mawwells(n)%fwelev - hmaw)
            this%qfw(n) = rrate
            this%qout(n) = this%qout(n) + rrate
          end if
        end if
        !
        ! -- add rate and fwrate terms to budget terms
        !
        ! -- adjust rate for mover
        rrate = this%mawwells(n)%ratesim
        if (this%imover == 1) then
          qfact = DZERO
          if (rrate < DZERO) then
            if (this%qout(n) < DZERO) then
              qfact = rrate / this%qout(n)
            end if
          end if
          rrate = rrate + qfact * this%pakmvrobj%get_qtomvr(n)
        end if
        !
        ! -- See if flow is into maw or out of maw.
        if (rrate < DZERO) then
          !
          ! -- Flow is out of maw subtract rate from ratout.
          ratout = ratout - rrate
        else
          !
          ! -- Flow is into maw; add rate to ratin.
          ratin = ratin + rrate
        end if
        !
        ! -- adjust flowing well rate for mover
        if (this%iflowingwells > 0) then
          rrate = this%qfw(n)
          if (this%imover == 1) then
            qfact = DZERO
            if (rrate < DZERO) then
              if (this%qout(n) < DZERO) then
                qfact = rrate / this%qout(n)
              end if
            end if
            rrate = rrate + qfact * this%pakmvrobj%get_qtomvr(n)
          end if
          !
          ! -- See if flowing well flow is into maw or out of maw.
          if (rrate < DZERO) then
            !
            ! -- Flow is out of maw subtract rate from ratout.
            fwratout = fwratout - rrate
          else
            !
            ! -- Flow is into maw; add rate to ratin.
            fwratin = fwratin + rrate
          end if
        end if
        ! -- add maw storage changes
        if (this%imawiss /= 1) then
          rrate = -this%mawwells(n)%area * (this%mawwells(n)%xsto - this%mawwells(n)%xoldsto) / delt
          this%qsto(n) = rrate
          !
          ! -- See if storage flow is into maw or out of maw.
          if(rrate < DZERO) then
            !
            ! -- Flow is out of maw subtract rate from ratout.
            storatout = storatout - rrate
          else
            !
            ! -- Flow is into maw; add rate to ratin.
            storatin = storatin + rrate
          endif
        end if
        !
        ! -- add mover terms
        if (this%imover == 1) then
          !
          ! -- from mover
          rrate = this%pakmvrobj%get_qfrommvr(n)
          mvrratin = mvrratin + rrate
          !
          ! -- to mover
          !
          ! -- rate
          q2 = this%mawwells(n)%ratesim
          if (q2 < DZERO) then
            rrate = this%pakmvrobj%get_qtomvr(n)
            qfact = DZERO
            if (this%qout(n) < DZERO) then
              qfact = q2 / this%qout(n)
            end if
            rrate = rrate * qfact
            mvrratout = mvrratout + rrate
          end if
          !
          ! -- fwrate
          if (this%iflowingwells > 0) then
            q2 = this%qfw(n)
            rrate = this%pakmvrobj%get_qtomvr(n)
            qfact = DZERO
            if (this%qout(n) < DZERO) then
              qfact = q2 / this%qout(n)
            end if
            rrate = rrate * qfact
            mvrfwratout = mvrfwratout + rrate
          end if
        end if
      end if
    end do
    !
    ! -- gwf flow and constant flow to maw
    ibnd = 1
    do n = 1, this%nmawwells
      rrate = DZERO
      chrrate = DZERO
      hmaw = this%xnewpak(n)
      do j = 1, this%mawwells(n)%ngwfnodes
        this%qleak(ibnd) = DZERO
        !if (this%iboundpak(n) == 0) cycle
        igwfnode = this%mawwells(n)%gwfnodes(j)
        hgwf = this%xnew(igwfnode)
        cmaw = this%mawwells(n)%simcond(j)

        bmaw = this%mawwells(n)%botscrn(j)
        ! -- calculate cterm - relative to gwf
        cterm = DZERO
        if (hmaw < bmaw) then
          cterm = cmaw * (bmaw - hmaw)
        end if
        rrate = -(cmaw * (hmaw - hgwf) + cterm)
        ratsum = ratsum + rrate
        this%qleak(ibnd) = rrate
        if (this%iboundpak(n) < 0) then
          chrrate = chrrate - rrate
        end if
        !
        ! -- See if flow is into maw or out of maw.
        if(rrate < DZERO) then
          !
          ! -- Flow is out of maw subtract rate from ratout.
          gwfratout = gwfratout - rrate
        else
          !
          ! -- Flow is into maw; add rate to ratin.
          gwfratin = gwfratin + rrate
        endif
        !
        ! -- See if flow is into maw or out of maw.
        if(chrrate < DZERO) then
          !
          ! -- Flow is out of maw subtract rate from ratout.
          chratout = chratout - chrrate
        else
          !
          ! -- Flow is into maw; add rate to ratin.
          chratin = chratin + chrrate
        endif
        ibnd = ibnd + 1
      end do
      if (this%iboundpak(n) < 0) then
        this%mawwells(n)%ratesim = -ratsum
      end if
      this%qconst(n) = chrrate
    end do
    !
    ! -- add calculated terms
    call this%budget%addentry(gwfratin, gwfratout, delt,  &
                              this%cmawbudget(1), isuppress_output)
    if (this%imover == 1) then
      call this%budget%addentry(mvrratin, DZERO, delt,  &
                                this%cmawbudget(6), isuppress_output)
    end if
    call this%budget%addentry(ratin, ratout, delt,  &
                              this%cmawbudget(2), isuppress_output)
    if (this%imover == 1) then
      call this%budget%addentry(DZERO, mvrratout, delt,  &
                                this%cmawbudget(7), isuppress_output)
    end if
    if (this%imawissopt /= 1) then
      call this%budget%addentry(storatin, storatout, delt,  &
                                this%cmawbudget(3), isuppress_output)
    end if
    call this%budget%addentry(chratin, chratout, delt,  &
                              this%cmawbudget(4), isuppress_output)
    if (this%iflowingwells /= 0) then
      call this%budget%addentry(fwratin, fwratout, delt,  &
                                this%cmawbudget(5), isuppress_output)
      if (this%imover == 1) then
        call this%budget%addentry(DZERO, mvrfwratout, delt,  &
                                  this%cmawbudget(8), isuppress_output)
      end if
    end if
    !
    ! -- For continuous observations, save simulated values.
    if (this%obs%npakobs > 0 .and. iprobs > 0) then
      call this%maw_bd_obs()
    endif
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
        d = v - this%mawwells(n)%bot
        if (this%iboundpak(n) < 1) then
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
    ! -- Set unit number for binary budget output
    ibinun = 0
    if(this%ibudgetout /= 0) then
      ibinun = this%ibudgetout
    end if
    if(icbcfl == 0) ibinun = 0
    if (isuppress_output /= 0) ibinun = 0
    !
    ! -- write maw binary budget output
    if (ibinun > 0) then
      ! GWF FLOW
      naux = this%cbcauxitems
      this%cauxcbc(1) = '       FLOW-AREA'
      call ubdsv06(kstp, kper, this%cmawbudget(1), this%name_model, this%name,  &
                   this%name_model, this%name_model,                            &
                   ibinun, naux, this%cauxcbc, this%maxbound, 1, 1,             &
                   this%maxbound, this%iout, delt, pertim, totim)
      ibnd = 1
      do n = 1, this%nmawwells
        do j = 1, this%mawwells(n)%ngwfnodes
          n2 = this%mawwells(n)%gwfnodes(j)
          tmaw = this%mawwells(n)%topscrn(j)
          bmaw = this%mawwells(n)%botscrn(j)
          call this%maw_calculate_saturation(n, j, n2, sat)
          ! -- fill qauxcbc
          ! -- connection surface area
          this%qauxcbc(1) = DTWO * DPI * this%mawwells(n)%radius * sat * (tmaw - bmaw)
          ! -- get leakage
          q = this%qleak(ibnd)
          call this%dis%record_mf6_list_entry(ibinun, n, n2, q, naux,       &
                                                  this%qauxcbc,                 &
                                                  olconv=.FALSE.)
          ibnd = ibnd + 1
        end do
      end do
      ! WELL RATE
      naux = 0
      call ubdsv06(kstp, kper, this%cmawbudget(2), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%nmawwells, 1, 1,           &
                   this%nmawwells, this%iout, delt, pertim, totim)
      do n = 1, this%nmawwells
        q = this%mawwells(n)%ratesim
        ! adjust if well rate is an outflow
        if (this%imover == 1 .and. q < DZERO) then
          qfact = DONE
          if (this%iflowingwells > 0) then
            if (this%qout(n) < DZERO) then
              qfact = q / this%qout(n)
            end if
          end if
          q = q + qfact * this%pakmvrobj%get_qtomvr(n)
        end if
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! FLOWING WELL
      if (this%iflowingwells > 0) then
        naux = 0
        call ubdsv06(kstp, kper, this%cmawbudget(5),                            &
                     this%name_model, this%name,                                &
                     this%name_model, this%name,                                &
                     ibinun, naux, this%auxname, this%nmawwells, 1, 1,          &
                     this%nmawwells, this%iout, delt, pertim, totim)
        do n = 1, this%nmawwells
          q = this%qfw(n)
          if (this%imover == 1) then
            qfact = DONE
            q2 = this%mawwells(n)%ratesim
            ! adjust if well rate is an outflow
            if (q2 < DZERO) then
              if (this%qout(n) < DZERO) then
                qfact = q / this%qout(n)
              end if
            end if
            q = q + qfact * this%pakmvrobj%get_qtomvr(n)
          end if
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                  this%auxvar(:,n),              &
                                                  olconv=.FALSE.,                &
                                                  olconv2=.FALSE.)
        end do
      end if
      ! STORAGE
      if (this%imawissopt /= 1) then
        naux = this%cbcauxitems
        this%cauxcbc(1) = 'VOLUME          '
        call ubdsv06(kstp, kper, this%cmawbudget(3),                            &
                     this%name_model, this%name,                                &
                     this%name_model, this%name,                                &
                     ibinun, naux, this%cauxcbc, this%nmawwells, 1, 1,          &
                     this%nmawwells, this%iout, delt, pertim, totim)
        do n = 1, this%nmawwells
          b = this%mawwells(n)%xsto - this%mawwells(n)%bot
          if (b < DZERO) then
            b = DZERO
          end if
          v = this%mawwells(n)%area * b
          q = this%qsto(n)
          this%qauxcbc(1) = v
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,        &
                                                  this%qauxcbc,                 &
                                                  olconv=.FALSE.,               &
                                                  olconv2=.FALSE.)
        end do
      end if
      ! CONSTANT FLOW
      naux = 0
      call ubdsv06(kstp, kper, this%cmawbudget(4), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%nmawwells, 1, 1,           &
                   this%nmawwells, this%iout, delt, pertim, totim)
      do n = 1, this%nmawwells
        q = this%qconst(n)
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! MOVER
      if (this%imover == 1) then
        ! FROM MOVER
        naux = 0
        call ubdsv06(kstp, kper, this%cmawbudget(6), this%name_model,          &
                     this%name, this%name_model, this%name,                    &
                     ibinun, naux, this%auxname,                               &
                     this%nmawwells, 1, 1,                                     &
                     this%nmawwells, this%iout, delt, pertim, totim)
        do n = 1, this%nmawwells
          if (this%iboundpak(n) == 0) then
            q = DZERO
          else
            q = this%pakmvrobj%get_qfrommvr(n)
          end if
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,       &
                                                  this%auxvar(:,n),            &
                                                  olconv=.FALSE.,              &
                                                  olconv2=.FALSE.)
        end do
        ! TO MOVER FROM WELL RATE
        naux = 0
        call ubdsv06(kstp, kper, this%cmawbudget(7), this%name_model,          &
                     this%name, this%name_model, this%name,                    &
                     ibinun, naux, this%auxname,                               &
                     this%nmawwells, 1, 1,                                     &
                     this%nmawwells, this%iout, delt, pertim, totim)
        do n = 1, this%nmawwells
          q = this%pakmvrobj%get_qtomvr(n)
          if (q > DZERO) then
            q = -q
            q2 = this%mawwells(n)%ratesim
            ! adjust TO MOVER if well rate is outflow
            if (q2 < DZERO) then
              qfact = q2 / this%qout(n)
              q = q * qfact
            else
              q = DZERO
            end if
          end if
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,       &
                                                  this%auxvar(:,n),            &
                                                  olconv=.FALSE.,              &
                                                  olconv2=.FALSE.)
        end do
        ! TO MOVER FROM FLOWING WELL
        if (this%iflowingwells > 0) then
          naux = 0
          call ubdsv06(kstp, kper, this%cmawbudget(8), this%name_model,         &
                       this%name, this%name_model, this%name,                   &
                       ibinun, naux, this%auxname,                              &
                       this%nmawwells, 1, 1,                                    &
                       this%nmawwells, this%iout, delt, pertim, totim)
          do n = 1, this%nmawwells
            q = this%pakmvrobj%get_qtomvr(n)
            if (q > DZERO) then
              q = -q
              q2 = this%mawwells(n)%ratesim
              ! adjust TO MOVER if well rate is outflow
              qfact = DONE
              if (this%qout(n) < DZERO) then
                qfact = this%qfw(n) / this%qout(n)
              end if
              q = q * qfact
            end if
            call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,      &
                                                    this%auxvar(:,n),           &
                                                    olconv=.FALSE.,             &
                                                    olconv2=.FALSE.)
          end do
        end if
      end if
      ! AUXILIARY VARIABLES
      naux = this%naux
      if (naux > 0) then
        call ubdsv06(kstp, kper, '       AUXILIARY', this%name_model, this%name,&
                     this%name_model, this%name,                                &
                     ibinun, naux, this%auxname, this%nmawwells, 1, 1,          &
                     this%nmawwells, this%iout, delt, pertim, totim)
        do n = 1, this%nmawwells
          q = DZERO
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,       &
                                                  this%auxvar(:,n),            &
                                                  olconv=.FALSE.,              &
                                                  olconv2=.FALSE.)
        end do
      end if


    end if
    !
    ! -- return
    return
  end subroutine maw_bd


  subroutine maw_ot(this, kstp, kper, iout, ihedfl, ibudfl)
    ! **************************************************************************
    ! pak1t -- Output package budget
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    !
    use InputOutputModule, only: UWWORD
    ! -- dummy
    class(MawType) :: this
    integer(I4B),intent(in) :: kstp
    integer(I4B),intent(in) :: kper
    integer(I4B),intent(in) :: iout
    integer(I4B),intent(in) :: ihedfl
    integer(I4B),intent(in) :: ibudfl
    ! -- locals
    character(len=LINELENGTH) :: line, linesep
    character(len=16) :: text
    integer(I4B) :: j
    integer(I4B) :: n
    integer(I4B) :: ibnd
    integer(I4B) :: iloc
    real(DP) :: q
    real(DP) :: qfact
    real(DP) :: qgwfin
    real(DP) :: qgwfout
    real(DP) :: qfrommvr
    real(DP) :: qrate
    real(DP) :: qfwrate
    real(DP) :: qratetomvr
    real(DP) :: qfwratetomvr
    real(DP) :: qsto
    real(DP) :: qconst
    real(DP) :: qin
    real(DP) :: qout
    real(DP) :: qerr
    real(DP) :: qavg
    real(DP) :: qpd
    ! format
 2000 FORMAT ( 1X, ///1X, A, A, A, '   PERIOD ', I6, '   STEP ', I8)
    ! --------------------------------------------------------------------------
    !
    ! -- write MAW heads to the listing file
    if (ihedfl /= 0 .and. this%iprhed /= 0) then
      write (iout, 2000) 'MULTI-AQUIFER WELL (', trim(this%name), ') HEAD', kper, kstp
      iloc = 1
      line = ''
      if (this%inamedbound==1) then
        call UWWORD(line, iloc, 16, 1, 'well', n, q, left=.TRUE.)
      end if
      call UWWORD(line, iloc, 6, 1, 'well', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'well', n, q, CENTER=.TRUE.)
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
      call UWWORD(line, iloc, 11, 1, 'head', n, q, CENTER=.TRUE.)
      ! -- write second line
      write(iout,'(1X,A)') line(1:iloc)
      write(iout,'(1X,A)') linesep(1:iloc)
      ! -- write data
      do n = 1, this%nmawwells
        iloc = 1
        line = ''
        if (this%inamedbound==1) then
          call UWWORD(line, iloc, 16, 1, this%mawwells(n)%name, n, q, left=.TRUE.)
        end if
        call UWWORD(line, iloc, 6, 2, text, n, q)
        call UWWORD(line, iloc, 11, 3, text, n, this%xnewpak(n))
        write(iout, '(1X,A)') line(1:iloc)
      end do
    end if
    !
    ! -- write MAW flows to the listing file
    if (ibudfl /= 0 .and. this%iprflow /= 0) then
      write (iout, 2000) 'MULTI-AQUIFER WELL (', trim(this%name), ') FLOWS', kper, kstp
      iloc = 1
      line = ''
      if (this%inamedbound==1) then
        call UWWORD(line, iloc, 16, 1, 'well', n, q, left=.TRUE.)
      end if
      call UWWORD(line, iloc, 6, 1, 'well', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'gwf', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'gwf', n, q, CENTER=.TRUE.)
      if (this%imover == 1) then
        call UWWORD(line, iloc, 11, 1, 'from', n, q, CENTER=.TRUE.)
      end if
      call UWWORD(line, iloc, 11, 1, 'well', n, q, CENTER=.TRUE.)
      if (this%imover == 1) then
        call UWWORD(line, iloc, 11, 1, 'rate', n, q, CENTER=.TRUE.)
      end if
      if (this%iflowingwells > 0) then
        call UWWORD(line, iloc, 11, 1, 'flowing', n, q, CENTER=.TRUE.)
        if (this%imover == 1) then
          call UWWORD(line, iloc, 11, 1, 'flowing', n, q, CENTER=.TRUE.)
        end if
      end if
      if (this%imawissopt /= 1) then
        call UWWORD(line, iloc, 11, 1, 'well', n, q, CENTER=.TRUE.)
      end if
      call UWWORD(line, iloc, 11, 1, 'constant', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'well', n, q, CENTER=.TRUE., SEP=' ')
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
      call UWWORD(line, iloc, 11, 1, 'in', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'out', n, q, CENTER=.TRUE.)
      if (this%imover == 1) then
        call UWWORD(line, iloc, 11, 1, 'mover', n, q, CENTER=.TRUE.)
      end if
      call UWWORD(line, iloc, 11, 1, 'rate', n, q, CENTER=.TRUE.)
      if (this%imover == 1) then
        call UWWORD(line, iloc, 11, 1, 'to mvr', n, q, CENTER=.TRUE.)
      end if
      if (this%iflowingwells > 0) then
        call UWWORD(line, iloc, 11, 1, 'rate', n, q, CENTER=.TRUE.)
        if (this%imover == 1) then
          call UWWORD(line, iloc, 11, 1, 'to mvr', n, q, CENTER=.TRUE.)
        end if
      end if
      if (this%imawissopt /= 1) then
        call UWWORD(line, iloc, 11, 1, 'storage', n, q, CENTER=.TRUE.)
      end if
      call UWWORD(line, iloc, 11, 1, 'flow', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'in - out', n, q, CENTER=.TRUE., SEP=' ')
      call UWWORD(line, iloc, 11, 1, 'difference', n, q, CENTER=.TRUE.)
      ! -- write second line
      write(iout,'(1X,A)') line(1:iloc)
      write(iout,'(1X,A)') linesep(1:iloc)
      !
      ibnd = 1
      do n = 1, this%nmawwells
        qgwfin = DZERO
        qgwfout = DZERO
        qfrommvr = DZERO
        qrate = DZERO
        qfwrate = DZERO
        qratetomvr = DZERO
        qfwratetomvr = DZERO
        qsto = DZERO
        qconst = DZERO
        qin = DZERO
        qout = DZERO
        qerr = DZERO
        qpd = DZERO
        qfact = DZERO
        do j = 1, this%mawwells(n)%ngwfnodes
          q = this%qleak(ibnd)
          if (q < DZERO) then
            qgwfout = qgwfout + q
          else
            qgwfin = qgwfin + q
          end if
          ibnd = ibnd + 1
        end do
        iloc = 1
        line = ''
        if (this%inamedbound==1) then
          call UWWORD(line, iloc, 16, 1, this%mawwells(n)%name, n, q, left=.TRUE.)
        end if
        call UWWORD(line, iloc, 6, 2, text, n, q)
        call UWWORD(line, iloc, 11, 3, text, n, qgwfin)
        call UWWORD(line, iloc, 11, 3, text, n, qgwfout)
        if (this%imover == 1) then
          if (this%iboundpak(n) /= 0) then
            qfrommvr = this%pakmvrobj%get_qfrommvr(n)
          end if
          call UWWORD(line, iloc, 11, 3, text, n, qfrommvr)
        end if
        if (this%iboundpak(n) < 0) then
          q = DZERO
        else
          q = this%mawwells(n)%ratesim
        end if
        if (q < DZERO .and. this%qout(n) < DZERO) then
          qfact = q / this%qout(n)
          if (this%imover == 1) then
            q = q + this%pakmvrobj%get_qtomvr(n) * qfact
          end if
        end if
        qrate = q
        call UWWORD(line, iloc, 11, 3, text, n, qrate)
        if (this%imover == 1) then
          qratetomvr = this%pakmvrobj%get_qtomvr(n) * qfact
          if (qratetomvr > DZERO) then
            qratetomvr = -qratetomvr
          end if
          call UWWORD(line, iloc, 11, 3, text, n, qratetomvr)
        end if
        if (this%iflowingwells > 0) then
          q = this%qfw(n)
          qfact = DONE
          if (q < DZERO .and. this%qout(n) < DZERO) then
            qfact = q / this%qout(n)
            if (this%imover == 1) then
              q = q + this%pakmvrobj%get_qtomvr(n) * qfact
            end if
          end if
          qfwrate = q
          call UWWORD(line, iloc, 11, 3, text, n, qfwrate)
          if (this%imover == 1) then
            qfwratetomvr = this%pakmvrobj%get_qtomvr(n) * qfact
            if (qfwratetomvr > DZERO) then
              qfwratetomvr = -qfwratetomvr
            end if
            call UWWORD(line, iloc, 11, 3, text, n, qfwratetomvr)
          end if
        end if
        if (this%imawissopt /= 1) then
          qsto = this%qsto(n)
          call UWWORD(line, iloc, 11, 3, text, n, qsto)
        end if
        qconst = this%qconst(n)
        call UWWORD(line, iloc, 11, 3, text, n, qconst)
        ! accumulate qin
        qin = qgwfin + qfrommvr
        qout = -qgwfout - qratetomvr - qfwratetomvr
        if (qrate < DZERO) then
          qout = qout - qrate
        else
          qin = qin + qrate
        end if
        if (qsto < DZERO) then
          qout = qout - qsto
        else
          qin = qin + qsto
        end if
        if (qconst < DZERO) then
          qout = qout - qconst
        else
          qin = qin + qconst
        end if
        qerr = qin - qout
        call UWWORD(line, iloc, 11, 3, text, n, qerr, SEP=' ')
        qavg = DHALF * (qin + qout)
        if (qavg > DZERO) then
          qpd = DHUNDRED * qerr / qavg
        end if
        call UWWORD(line, iloc, 11, 3, text, n, qpd)
        write(iout, '(1X,A)') line(1:iloc)
      end do
    end if
    !
    ! -- Output maw budget
    call this%budget%budget_ot(kstp, kper, iout)
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
    enddo
    deallocate(this%mawwells)
    !
    ! -- arrays
    deallocate(this%cmawname)
    deallocate(this%cmawbudget)
    call mem_deallocate(this%idxmawconn)
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
    ! -- objects
    call this%budget%budget_da()
    deallocate(this%budget)
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
    endif
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'STRESS RATE'
    if(this%inamedbound == 1) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    endif
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
                v = this%mawwells(jj)%ratesim
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
                  v = this%mawwells(jj)%ratesim
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
                cmaw = this%mawwells(jj)%fwcondsim
                v = cmaw * (this%mawwells(jj)%fwelev - hmaw)
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
                  cmaw = this%mawwells(jj)%fwcondsim
                  v = cmaw * (this%mawwells(jj)%fwelev - hmaw)
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
                v = this%mawwells(jj)%fwcondsim
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
    character(len=200) :: ermsg
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
            write(ermsg,10)trim(bname), trim(obsrv%Name), trim(this%name)
            call store_error(ermsg)
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
          ermsg = 'Programming error in maw_rp_obs'
          call store_error(ermsg)
        endif
      end if
      !
      ! -- catch non-cumulative observation assigned to observation defined
      !    by a boundname that is assigned to more than one element
      if (obsrv%ObsTypeId == 'HEAD') then
        n = size(obsrv%indxbnds)
        if (n > 1) then
          write (ermsg, '(4x,a,4(1x,a))') &
            'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
            'for observation', trim(adjustl(obsrv%Name)), &
            ' must be assigned to a multi-aquifer well with a unique boundname.'
          call store_error(ermsg)
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
            write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              ' multi-aquifer well connection number must be > 0 and <=', &
              jj, '(specified value is ', nn2, ')'
            call store_error(ermsg)
          end if
        end do
      else
        do j = 1, size(obsrv%indxbnds)
          nn1 =  obsrv%indxbnds(j)
          if (nn1 < 1 .or. nn1 > this%nmawwells) then
            write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              ' multi-aquifer well must be > 0 and <=', this%nmawwells, &
              '(specified value is ', nn1, ')'
            call store_error(ermsg)
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
    real(DP) :: skin
    real(DP) :: ravg
    real(DP) :: slen
    real(DP) :: pavg
    real(DP) :: gwfsat
    real(DP) :: gwftop
    real(DP) :: gwfbot
    real(DP) :: denom
    real(DP) :: lc1
    real(DP) :: lc2
    real(DP) :: dx
    real(DP) :: dy
    real(DP) :: Txx
    real(DP) :: Tyy
    real(DP) :: yx4
    real(DP) :: xy4
    ! -- formats
    ! ------------------------------------------------------------------------------
    !
    ! -- set K11 and K22
    k11 = this%gwfk11(node)
    if (this%gwfik22 == 0) then
      k22 = this%gwfk11(node)
    else
      k22 = this%gwfk22(node)
    endif
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
    ! -- calculate effective radius
    if (this%dis%ndim == 3 .and. this%ieffradopt /= 0) then
      Txx = k11 * tthka
      Tyy = k22 * tthka
      dx = sqrt(this%dis%area(node))
      dy = dx
      yx4 = (Tyy/Txx)**0.25D0
      xy4 = (Txx/Tyy)**0.25D0
      eradius = 0.28D0 *((yx4*dx)**2 +(xy4*dy)**2)**0.5D0 / (yx4+xy4)
    else
      area = this%dis%area(node)
      eradius = sqrt(area / (DEIGHT * DPI))
    end if
    !
    ! -- conductance calculated using Thiem equation
    if (this%mawwells(i)%ieqn == 1) then
      c = (DTWO * DPI * tthka * sqrtk11k22) / log(eradius / this%mawwells(i)%radius)
    ! -- conductance calculated using skin
    else if (this%mawwells(i)%ieqn == 2) then
      hks = this%mawwells(i)%hk(j)
      ! prevent division by zero
      if (tthkw * hks > DZERO) then
        skin = (((sqrtk11k22*tthka)/(hks*tthkw)) - DONE) * &
               log(this%mawwells(i)%sradius(j)/this%mawwells(i)%radius)
        c = (DTWO * DPI * tthka * sqrtk11k22) / skin
      end if
    ! -- conductance calculated using cumulative Thiem and skin equations
    else if (this%mawwells(i)%ieqn == 3) then
      ! calculate lc1
      lc1 = log(eradius / this%mawwells(i)%radius) / (DTWO * DPI * tthka * sqrtk11k22)
      ! calculate lc2
      hks = this%mawwells(i)%hk(j)
      ! prevent division by zero
      if (tthkw * hks > DZERO) then
        skin = (((sqrtk11k22*tthka)/(hks*tthkw)) - DONE) * &
               log(this%mawwells(i)%sradius(j)/this%mawwells(i)%radius)
        lc2 = skin / (DTWO * DPI * tthka * sqrtk11k22)
      else
        lc2 = DZERO
      end if
      ! calculate conductance
      denom = lc1 + lc2
      if (denom > DZERO) then
        c = DONE / denom
      end if
    ! -- conductance calculated using screen elevations, hk, well radius, and screen radius
    else if (this%mawwells(i)%ieqn == 4) then
      hks = this%mawwells(i)%hk(j)
      ravg = DHALF * (this%mawwells(i)%radius + this%mawwells(i)%sradius(j))
      slen = this%mawwells(i)%sradius(j) - this%mawwells(i)%radius
      pavg = DTWO * DPI * ravg
      c = hks * pavg * tthkw / slen
    end if
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
    rate = this%mawwells(n)%rate%value
    !
    ! -- Assign q differently depending on whether this is an extraction well
    !    (rate < 0) or an injection well (rate > 0).
    if (rate < DZERO) then
      !
      ! -- If well shut off is activated, then turn off well if necessary,
      !    or if shut off is not activated then check to see if rate scaling 
      !    is on.
      if (this%mawwells(n)%shutofflevel /= DEP20) then
        call this%maw_calculate_qpot(n, q)
        if (q < DZERO) q = DZERO
        if (q > -rate) q = -rate

        if (this%ishutoffcnt == 1) then
          this%mawwells(n)%shutoffweight = DONE
          this%mawwells(n)%shutoffdq = DZERO
          this%mawwells(n)%shutoffqold = q
        end if

        dq = q - this%mawwells(n)%shutoffqold
        weight = this%mawwells(n)%shutoffweight

        ! -- for flip-flop condition, decrease factor
        if ( this%mawwells(n)%shutoffdq*dq < DZERO ) then
          weight = this%theta * this%mawwells(n)%shutoffweight
        ! -- when change is of same sign, increase factor
        else
          weight = this%mawwells(n)%shutoffweight + this%kappa
        end if
        if ( weight > DONE ) weight = DONE

        q = this%mawwells(n)%shutoffqold + weight * dq

        this%mawwells(n)%shutoffqold = q
        this%mawwells(n)%shutoffdq = dq
        this%mawwells(n)%shutoffweight = weight

        !
        ! -- If shutoffmin and shutoffmax are specified then apply 
        !    additional checks for when to shut off the well.
        if (this%mawwells(n)%shutoffmin > DZERO) then
          if (hmaw < this%mawwells(n)%shutofflevel) then
            !
            ! -- calculate adjusted well rate subject to constraints
            ! -- well is shutoff
            if (this%mawwells(n)%ishutoff /= 0) then
              q = DZERO
            ! --- well is not shut off
            else
              ! -- turn off well if q is less than the minimum rate and
              !    reset the ishutoff flag if at least on iteration 3
              if (q < this%mawwells(n)%shutoffmin) then
                if (this%ishutoffcnt > 2) then
                  this%mawwells(n)%ishutoff = 1
                end if
                q = DZERO
              ! -- leave well on and use the specified rate
              !    or the potential rate
              end if
            end if
          ! -- try to use the specified rate or the potential rate
          else
            if (q > this%mawwells(n)%shutoffmax) then
              if (this%ishutoffcnt <= 2) then
                this%mawwells(n)%ishutoff = 0
              end if
            end if
            if (this%mawwells(n)%ishutoff /= 0) then
              q = DZERO
            end if
          end if
        end if

        if (q /= DZERO) q = -q

      else
        scale = DONE
        ! -- Apply rate scaling by reducing pumpage when hmaw is less than the 
        !    sum of maw pump elevation (pumpelev) and the specified reduction 
        !    length.  The rate will go to zero as hmaw drops to the pump
        !    elevation.
        if (this%mawwells(n)%reduction_length /= DEP20) then
          bt = this%mawwells(n)%pumpelev
          tp = bt + this%mawwells(n)%reduction_length
          scale = sQSaturation(tp, bt, hmaw)
        end if
        q = scale * rate
      end if
    !
    else
      !
      ! -- Handle the injection case (rate > 0) differently than extraction.
      q = rate
      if (this%mawwells(n)%shutofflevel /= DEP20) then
        call this%maw_calculate_qpot(n, q)
        q = -q
        if (q < DZERO) q = DZERO
        if (q > rate) q = rate

        if (this%ishutoffcnt == 1) then
          this%mawwells(n)%shutoffweight = DONE
          this%mawwells(n)%shutoffdq = DZERO
          this%mawwells(n)%shutoffqold = q
        end if

        dq = q - this%mawwells(n)%shutoffqold
        weight = this%mawwells(n)%shutoffweight

        ! -- for flip-flop condition, decrease factor
        if ( this%mawwells(n)%shutoffdq*dq < DZERO ) then
          weight = this%theta * this%mawwells(n)%shutoffweight
        ! -- when change is of same sign, increase factor
        else
          weight = this%mawwells(n)%shutoffweight + this%kappa
        end if
        if ( weight > DONE ) weight = DONE
        
        q = this%mawwells(n)%shutoffqold + weight * dq
        
        this%mawwells(n)%shutoffqold = q
        this%mawwells(n)%shutoffdq = dq
        this%mawwells(n)%shutoffweight = weight
        
      else
        scale = DONE
        ! -- Apply rate scaling for an injection well by reducting the 
        !    injection rate as hmaw rises above the pump elevation.  The rate
        !    will approach zero as hmaw approaches pumpelev + reduction_length.
        if (this%mawwells(n)%reduction_length /= DEP20) then
          bt = this%mawwells(n)%pumpelev
          tp = bt + this%mawwells(n)%reduction_length
          scale = DONE - sQSaturation(tp, bt, hmaw)
        endif
        q = scale * rate
      endif
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
    !--initialize qnet
    qnet = DZERO
   ! --
    htmp = this%mawwells(n)%shutofflevel
    ! -- calculate discharge to flowing wells
    if (this%iflowingwells > 0) then
      if (this%mawwells(n)%fwcond > DZERO) then
        bt = this%mawwells(n)%fwelev
        tp = bt + this%mawwells(n)%fwrlen
        scale = sQSaturation(tp, bt, htmp)
        cfw = scale * this%mawwells(n)%fwcond
        this%mawwells(n)%ifwdischarge = 0
        if (cfw > DZERO) then
          this%mawwells(n)%ifwdischarge = 1
          this%mawwells(n)%xsto = bt
        end if
        qnet = qnet + cfw * (bt - htmp)
      end if
    end if
    ! -- calculate maw storage changes
    if (this%imawiss /= 1) then
      if (this%mawwells(n)%ifwdischarge /= 1) then
        hdterm = this%mawwells(n)%xoldsto - htmp
      else
        hdterm = this%mawwells(n)%xoldsto - this%mawwells(n)%fwelev
      end if
      qnet = qnet - (this%mawwells(n)%area * hdterm / delt)
    end if
    ! -- calculate inflow from aquifer
    do j = 1, this%mawwells(n)%ngwfnodes
      igwfnode = this%mawwells(n)%gwfnodes(j)
      call this%maw_calculate_saturation(n, j, igwfnode, sat) !, hv)
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
        do j = 1, this%mawwells(n)%ngwfnodes
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
          endif
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


  subroutine maw_allocate_well(this, n)
! ******************************************************************************
! allocate_reach -- Allocate pointers for multi-aquifer well mawwells(n).
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(MawType) :: this
    integer(I4B), intent(in) :: n
    ! -- local
    character(len=LINELENGTH) :: ermsg
    character(len=10) :: cwel
    integer(I4B) :: iaux
! ------------------------------------------------------------------------------
    !
    ! -- make sure maw well has not been allocated
    if (associated(this%mawwells(n)%ieqn)) then
      write (cwel, '(i10)') n
      ermsg = 'multi-aquifer well ' // trim(cwel) // ' is already allocated'
      call store_error(ermsg)
      call ustop()
    end if
    ! -- allocate pointers
    !allocate(character (len=LENBOUNDNAME) :: this%mawwells(n)%name)
    allocate(this%mawwells(n)%name)
    allocate(this%mawwells(n)%status)
    allocate(this%mawwells(n)%ngwfnodes)
    allocate(this%mawwells(n)%ieqn)
    allocate(this%mawwells(n)%ishutoff)
    allocate(this%mawwells(n)%ifwdischarge)
    allocate(this%mawwells(n)%strt)
    allocate(this%mawwells(n)%radius)
    allocate(this%mawwells(n)%area)
    allocate(this%mawwells(n)%pumpelev)
    allocate(this%mawwells(n)%bot)
    allocate(this%mawwells(n)%ratesim)
    allocate(this%mawwells(n)%reduction_length)
    allocate(this%mawwells(n)%fwelev)
    allocate(this%mawwells(n)%fwcond)
    allocate(this%mawwells(n)%fwrlen)
    allocate(this%mawwells(n)%fwcondsim)
    allocate(this%mawwells(n)%xsto)
    allocate(this%mawwells(n)%xoldsto)
    allocate(this%mawwells(n)%shutoffmin)
    allocate(this%mawwells(n)%shutoffmax)
    allocate(this%mawwells(n)%shutofflevel)
    allocate(this%mawwells(n)%shutoffweight)
    allocate(this%mawwells(n)%shutoffdq)
    allocate(this%mawwells(n)%shutoffqold)
    ! -- timeseries aware data
    if (this%naux > 0) then
      allocate(this%mawwells(n)%auxvar(this%naux))
      do iaux = 1, this%naux
        allocate(this%mawwells(n)%auxvar(iaux)%name)
        allocate(this%mawwells(n)%auxvar(iaux)%value)
      end do
    end if
    allocate(this%mawwells(n)%rate)
    allocate(this%mawwells(n)%rate%name)
    allocate(this%mawwells(n)%rate%value)
    allocate(this%mawwells(n)%head)
    allocate(this%mawwells(n)%head%name)
    allocate(this%mawwells(n)%head%value)
    !
    ! -- initialize a few well variables
    this%mawwells(n)%name = ''
    this%mawwells(n)%status = 'ACTIVE'
    this%mawwells(n)%ngwfnodes = 0
    this%mawwells(n)%ieqn = 0
    this%mawwells(n)%ishutoff = 0
    this%mawwells(n)%ifwdischarge = 0
    this%mawwells(n)%strt = DEP20
    this%mawwells(n)%radius = DEP20
    this%mawwells(n)%area = DZERO
    this%mawwells(n)%pumpelev = DEP20
    this%mawwells(n)%bot = DEP20
    this%mawwells(n)%ratesim = DZERO
    this%mawwells(n)%reduction_length = DEP20
    this%mawwells(n)%fwelev = DZERO
    this%mawwells(n)%fwcond = DZERO
    this%mawwells(n)%fwrlen = DZERO
    this%mawwells(n)%fwcondsim = DZERO
    this%mawwells(n)%xsto = DZERO
    this%mawwells(n)%xoldsto = DZERO
    this%mawwells(n)%shutoffmin = DZERO
    this%mawwells(n)%shutoffmax = DZERO
    this%mawwells(n)%shutofflevel = DEP20
    this%mawwells(n)%shutoffweight = DONE
    this%mawwells(n)%shutoffdq = DONE
    this%mawwells(n)%shutoffqold = DONE
    ! -- timeseries aware data
    do iaux = 1, this%naux
      this%mawwells(n)%auxvar(iaux)%name = ''
      this%mawwells(n)%auxvar(iaux)%value = DZERO
    end do
    this%mawwells(n)%rate%name = ''
    this%mawwells(n)%rate%value = DZERO
    this%mawwells(n)%head%name = ''
    this%mawwells(n)%head%value = DZERO
    !
    ! -- return
    return
  end subroutine maw_allocate_well

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
    integer(I4B) :: iaux
! ------------------------------------------------------------------------------
    !
    deallocate(this%mawwells(n)%gwfnodes)
    deallocate(this%mawwells(n)%satcond)
    deallocate(this%mawwells(n)%simcond)
    deallocate(this%mawwells(n)%topscrn)
    deallocate(this%mawwells(n)%botscrn)
    if (this%mawwells(n)%ieqn==2 .OR. this%mawwells(n)%ieqn==3 .OR.           &
        this%mawwells(n)%ieqn==4) then
      deallocate(this%mawwells(n)%hk)
    end if
    if (this%mawwells(n)%ieqn==2 .OR. this%mawwells(n)%ieqn==3 .OR.           &
        this%mawwells(n)%ieqn==4) then
      deallocate(this%mawwells(n)%sradius)
    end if
    deallocate(this%mawwells(n)%name)
    deallocate(this%mawwells(n)%status)
    deallocate(this%mawwells(n)%ngwfnodes)
    deallocate(this%mawwells(n)%ieqn)
    deallocate(this%mawwells(n)%ishutoff)
    deallocate(this%mawwells(n)%ifwdischarge)
    deallocate(this%mawwells(n)%strt)
    deallocate(this%mawwells(n)%radius)
    deallocate(this%mawwells(n)%area)
    deallocate(this%mawwells(n)%pumpelev)
    deallocate(this%mawwells(n)%bot)
    deallocate(this%mawwells(n)%ratesim)
    deallocate(this%mawwells(n)%reduction_length)
    deallocate(this%mawwells(n)%fwelev)
    deallocate(this%mawwells(n)%fwcond)
    deallocate(this%mawwells(n)%fwrlen)
    deallocate(this%mawwells(n)%fwcondsim)
    deallocate(this%mawwells(n)%xsto)
    deallocate(this%mawwells(n)%xoldsto)
    deallocate(this%mawwells(n)%shutoffmin)
    deallocate(this%mawwells(n)%shutoffmax)
    deallocate(this%mawwells(n)%shutofflevel)
    deallocate(this%mawwells(n)%shutoffweight)
    deallocate(this%mawwells(n)%shutoffdq)
    deallocate(this%mawwells(n)%shutoffqold)
    ! -- timeseries aware data
    if (this%naux > 0) then
      do iaux = 1, this%naux
        deallocate(this%mawwells(n)%auxvar(iaux)%name)
        deallocate(this%mawwells(n)%auxvar(iaux)%value)
      end do
      deallocate(this%mawwells(n)%auxvar)
    end if
    deallocate(this%mawwells(n)%rate%name)
    deallocate(this%mawwells(n)%rate%value)
    deallocate(this%mawwells(n)%rate)
    deallocate(this%mawwells(n)%head%name)
    deallocate(this%mawwells(n)%head%value)
    deallocate(this%mawwells(n)%head)
    !
    ! -- return
    return
  end subroutine maw_deallocate_well


end module mawmodule

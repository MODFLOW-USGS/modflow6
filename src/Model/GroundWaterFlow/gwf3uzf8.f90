! -- Uzf module
module UzfModule
!
  use KindModule, only: DP, I4B
  use ArrayHandlersModule, only: ExpandArray
  use ConstantsModule, only: DZERO, DEM6, DEM4, DEM2, DEM1, DHALF,              &
                             DONE, DHUNDRED,                                    &
                             LINELENGTH, LENFTYPE, LENPACKAGENAME,              &
                             LENBOUNDNAME, LENBUDTXT, DNODATA,                  &
                             NAMEDBOUNDFLAG, MAXCHARLEN,                        &
                             DHNOFLO, DHDRY
  use MemoryTypeModule, only: MemoryTSType
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_setptr,      &
                                 mem_deallocate
  use SparseModule, only: sparsematrix
  use BndModule, only: BndType
  use UzfKinematicModule
  use BudgetModule,   only: BudgetType
  use BaseDisModule, only: DisBaseType
  use ObserveModule,        only: ObserveType
  use ObsModule, only: ObsType
  use InputOutputModule, only: URWORD, UWWORD
  use SimModule,        only: count_errors, store_error, ustop, &
                              store_error_unit
  use BlockParserModule, only: BlockParserType

  implicit none
  !
  character(len=LENFTYPE)       :: ftype = 'UZF'
  character(len=LENPACKAGENAME) :: text  = '       UZF CELLS' 

  private
  public :: uzf_create

  type, extends(BndType) :: UzfType
    ! output integers
    integer(I4B), pointer :: iprwcont => null()
    integer(I4B), pointer :: iwcontout => null()
    integer(I4B), pointer :: ibudgetout => null()
    !
    type(BudgetType), pointer                          :: budget      => null() !budget object
    integer(I4B), pointer                              :: bditems     => null() !number of budget items
    integer(I4B), pointer                              :: nbdtxt      => null() !number of budget text items
    character(len=LENBUDTXT), dimension(:), pointer    :: bdtxt       => null() !budget items written to cbc file
    type(UzfKinematicType), pointer                    :: uzfobj      => null() !uzf kinematic object
    type(UzfKinematicType), pointer                    :: uzfobjwork  => null() !uzf kinematic work object
    type(UzfKinematicType), pointer                    :: uzfobjbelow => null() !uzf kinematic object of underlying cell
    type(UzfKinematicType), pointer, dimension(:)      :: elements    => null() !array of all the kinematic uzf objects
    character(len=72), pointer                         :: nameuzf     => null() !cdl--(not sure.  Delete?)
    !
    ! -- pointer to gwf variables
    integer(I4B), pointer                      :: gwfiss      => null()
    real(DP), dimension(:), pointer            :: gwftop      => null()
    real(DP), dimension(:), pointer            :: gwfbot      => null()
    real(DP), dimension(:), pointer            :: gwfarea     => null()
    real(DP), dimension(:), pointer            :: gwfhcond    => null()
    !
    ! -- uzf data
    integer(I4B), pointer                       :: ntrail       => null()
    integer(I4B), pointer                       :: nsets        => null()
    integer(I4B), pointer                       :: nwav         => null()
    integer(I4B), pointer                       :: nodes        => null() !cdl--(this should probably be maxbound)
    integer(I4B), pointer                       :: nper         => null()
    integer(I4B), pointer                       :: nstp         => null()
    integer(I4B), pointer                       :: readflag     => null()
    integer(I4B), pointer                       :: outunitbud   => null()
    integer(I4B), pointer                       :: ietflag      => null()
    integer(I4B), pointer                       :: igwetflag    => null()
    integer(I4B), pointer                       :: iseepflag    => null()
    integer(I4B), pointer                       :: imaxcellcnt  => null()
    integer(I4B), dimension(:), pointer         :: mfcellid     => null()
    real(DP), dimension(:), pointer             :: appliedinf   => null()
    real(DP), dimension(:), pointer             :: rejinf       => null()
    real(DP), dimension(:), pointer             :: rejinf0      => null()
    real(DP), dimension(:), pointer             :: rejinftomvr  => null()
    real(DP), dimension(:), pointer             :: infiltration => null()
    real(DP), dimension(:), pointer             :: recharge     => null()
    real(DP), dimension(:), pointer             :: gwet         => null()
    real(DP), dimension(:), pointer             :: uzet         => null()
    real(DP), dimension(:), pointer             :: gwd          => null()
    real(DP), dimension(:), pointer             :: gwd0         => null()
    real(DP), dimension(:), pointer             :: gwdtomvr     => null()
    real(DP), dimension(:), pointer             :: rch          => null()
    real(DP), dimension(:), pointer             :: rch0         => null()
    real(DP), dimension(:), pointer             :: qsto         => null()
    integer(I4B), pointer                       :: iuzf2uzf     => null()
    !
    ! -- integer vectors
    integer(I4B), dimension(:), pointer, contiguous :: ia => null()
    integer(I4B), dimension(:), pointer, contiguous :: ja => null()
    !
    ! -- timeseries aware variables
    type (MemoryTSType), pointer, dimension(:) :: sinf => null()
    type (MemoryTSType), pointer, dimension(:) :: pet => null()
    type (MemoryTSType), pointer, dimension(:) :: extdp => null()
    type (MemoryTSType), pointer, dimension(:) :: extwc => null()
    type (MemoryTSType), pointer, dimension(:) :: ha => null()
    type (MemoryTSType), pointer, dimension(:) :: hroot => null()
    type (MemoryTSType), pointer, dimension(:) :: rootact => null()
    type (MemoryTSType), pointer, dimension(:) :: lauxvar => null()
    !
    ! -- convergence check
    integer(I4B), pointer  :: iconvchk    => null()
    real(DP), pointer      :: pdmax    => null()
    !
    ! formulate variables
    real(DP), dimension(:), pointer            :: deriv       => null()
    !
    ! budget variables
    real(DP), pointer                          :: totfluxtot  => null()
    real(DP), pointer                          :: infilsum    => null()
    real(DP), pointer                          :: rechsum     => null()
    real(DP), pointer                          :: delstorsum  => null()
    real(DP), pointer                          :: uzetsum     => null()
    real(DP), pointer                          :: vfluxsum    => null()
    integer(I4B), pointer                      :: issflag     => null()
    integer(I4B), pointer                      :: issflagold  => null()
    integer(I4B), pointer                      :: istocb      => null()
    !
    ! -- uzf cbc budget items
    integer(I4B), pointer :: cbcauxitems => NULL()
    character(len=16), dimension(:), pointer :: cauxcbc => NULL()
    real(DP), dimension(:), pointer, contiguous :: qauxcbc => null()
    !
    ! -- observations
    real(DP), dimension(:), pointer            :: obs_theta   => null()
    real(DP), dimension(:), pointer            :: obs_depth   => null()
    integer(I4B), dimension(:), pointer        :: obs_num     => null()

  contains

    procedure :: uzf_allocate_arrays
    procedure :: uzf_allocate_scalars
    procedure :: bnd_options => uzf_options
    procedure :: read_dimensions => uzf_readdimensions
    procedure :: bnd_ar => uzf_ar
    procedure :: bnd_rp => uzf_rp
    procedure :: bnd_ad => uzf_ad
    procedure :: bnd_cf => uzf_cf
    procedure :: bnd_cc => uzf_cc
    procedure :: bnd_bd => uzf_bd
    procedure :: bnd_ot => uzf_ot
    procedure :: bnd_fc => uzf_fc
    procedure :: bnd_fn => uzf_fn
    procedure :: bnd_da => uzf_da
    procedure :: define_listlabel
    !
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => uzf_obs_supported
    procedure, public :: bnd_df_obs => uzf_df_obs
    procedure, public :: bnd_rp_obs => uzf_rp_obs
    procedure, private :: uzf_bd_obs
    !
    ! -- methods specific for uzf
    procedure, private :: uzf_solve
    procedure, private :: read_cell_properties
    procedure, private :: print_cell_properties
    procedure, private :: uzcelloutput
    procedure, private :: findcellabove
    procedure, private :: check_cell_area

  end type UzfType

contains

  subroutine uzf_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! uzf_create -- Create a New UZF Package
! Subroutine: (1) create new-style package
!             (2) point packobj to the new package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B),intent(in) :: id
    integer(I4B),intent(in) :: ibcnum
    integer(I4B),intent(in) :: inunit
    integer(I4B),intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    ! -- local
    type(UzfType), pointer :: uzfobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(uzfobj)
    packobj => uzfobj
    !
    ! -- create name and origin
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call uzfobj%uzf_allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()
    !
    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 1
    packobj%iscloc = 0  ! not supported
    !
    ! -- return
    return
  end subroutine uzf_create

  subroutine uzf_ar(this)
! ******************************************************************************
! uzf_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use UzfKinematicModule, only: UzfKinematicType
    use MemoryManagerModule, only: mem_allocate, mem_setptr, mem_reallocate
    use BudgetModule, only: budget_cr
    ! -- dummy
    class(UzfType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, n
! ------------------------------------------------------------------------------
    !
    call this%obs%obs_ar()
    !
    ! -- Allocate arrays in package superclass
    call this%uzf_allocate_arrays()
    !
    ! -- Allocate UZF objects plus one extra for work array
    allocate(this%elements(this%nodes+1))
    do i = 1, this%nodes + 1
      allocate(this%elements(i))
    enddo
    !
    ! -- Initialize each UZF object
    do i = 1, this%nodes+1
        this%uzfobj => this%elements(i)
        call this%uzfobj%init(i,this%nwav)
    end do
    !
    ! -- Set pointers to GWF model arrays
    call mem_setptr(this%gwftop, 'TOP', trim(this%name_model)//' DIS')
    call mem_setptr(this%gwfbot, 'BOT', trim(this%name_model)//' DIS')
    call mem_setptr(this%gwfarea, 'AREA', trim(this%name_model)//' DIS')
    call mem_setptr(this%gwfhcond, 'CONDSAT', trim(this%name_model)//' NPF')
    call mem_setptr(this%gwfiss, 'ISS', trim(this%name_model))
!
!   --Read uzf cell properties and set values
    call this%read_cell_properties()
    !
    ! -- print cell data
    if (this%iprpak /= 0) then
      call this%print_cell_properties()
    end if
    !
    ! allocate space to store moisture content observations
    n = this%obs%npakobs
    if ( n > 0 ) then
      call mem_reallocate(this%obs_theta, n, 'OBS_THETA', this%origin)
      call mem_reallocate(this%obs_depth, n, 'OBS_DEPTH', this%origin)
      call mem_reallocate(this%obs_num, n, 'OBS_NUM', this%origin)
    end if
    !
    ! -- setup the budget
    call budget_cr(this%budget, this%origin)
    call this%budget%budget_df(this%bditems, this%name, 'L**3')
    !
    ! -- setup pakmvrobj
    if (this%imover /= 0) then
      allocate(this%pakmvrobj)
      call this%pakmvrobj%ar(this%maxbound, this%maxbound, this%origin)
    endif
    !
    ! -- return
    return
  end subroutine uzf_ar

  subroutine uzf_allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- allocate arrays used for mover
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    !use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(UzfType),   intent(inout) :: this
    ! -- local
    integer (I4B) :: i
    integer (I4B) :: j
    integer (I4B) :: ipos
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_arrays()
    !
    ! -- allocate uzf specific arrays
    call mem_allocate(this%mfcellid, this%nodes, 'MFCELLID', this%origin)
    call mem_allocate(this%appliedinf, this%nodes, 'APPLIEDINF', this%origin)
    call mem_allocate(this%rejinf, this%nodes, 'REJINF', this%origin)
    call mem_allocate(this%rejinf0, this%nodes, 'REJINF0', this%origin)
    call mem_allocate(this%rejinftomvr, this%nodes, 'REJINFTOMVR', this%origin)
    call mem_allocate(this%infiltration, this%nodes, 'INFILTRATION', this%origin)
    call mem_allocate(this%recharge, this%nodes, 'RECHARGE', this%origin)
    call mem_allocate(this%gwet, this%nodes, 'GWET', this%origin)
    call mem_allocate(this%uzet, this%nodes, 'UZET', this%origin)
    call mem_allocate(this%gwd, this%nodes, 'GWD', this%origin)
    call mem_allocate(this%gwd0, this%nodes, 'GWD0', this%origin)
    call mem_allocate(this%gwdtomvr, this%nodes, 'GWDTOMVR', this%origin)
    call mem_allocate(this%rch, this%nodes, 'RCH', this%origin)
    call mem_allocate(this%rch0, this%nodes, 'RCH0', this%origin)
    call mem_allocate(this%qsto, this%nodes, 'QSTO', this%origin)
    call mem_allocate(this%deriv, this%nodes, 'DERIV', this%origin)

    ! -- integer vectors
    call mem_allocate(this%ia, this%dis%nodes+1, 'IA', this%origin)
    call mem_allocate(this%ja, this%nodes, 'JA', this%origin)

    ! -- allocate timeseries aware variables
    call mem_allocate(this%sinf, this%nodes, 'SINF', this%origin)
    call mem_allocate(this%pet, this%nodes, 'PET', this%origin)
    call mem_allocate(this%extdp, this%nodes, 'EXDP', this%origin)
    call mem_allocate(this%extwc, this%nodes, 'EXTWC', this%origin)
    call mem_allocate(this%ha, this%nodes, 'HA', this%origin)
    call mem_allocate(this%hroot, this%nodes, 'HROOT', this%origin)
    call mem_allocate(this%rootact, this%nodes, 'ROOTACT', this%origin)
    call mem_allocate(this%lauxvar, this%naux*this%nodes, 'LAUXVAR', this%origin)


    do i = 1, this%nodes
      this%appliedinf(i) = DZERO
      this%recharge(i) = DZERO
      this%rejinf(i) = DZERO
      this%rejinf0(i) = DZERO
      this%rejinftomvr(i) = DZERO
      this%gwet(i) = DZERO
      this%uzet(i) = DZERO
      this%gwd(i) = DZERO
      this%gwd0(i) = DZERO
      this%gwdtomvr(i) = DZERO
      this%rch(i) = DZERO
      this%rch0(i) = DZERO
      this%qsto(i) = DZERO
      this%deriv(i) = DZERO
      ! -- timeseries aware variables
      this%sinf(i)%name = ''
      this%pet(i)%name = ''
      this%extdp(i)%name = ''
      this%extwc(i)%name = ''
      this%ha(i)%name = ''
      this%hroot(i)%name = ''
      this%rootact(i)%name = ''
      this%sinf(i)%value = DZERO
      this%pet(i)%value = DZERO
      this%extdp(i)%value = DZERO
      this%extwc(i)%value = DZERO
      this%ha(i)%value = DZERO
      this%hroot(i)%value = DZERO
      this%rootact(i)%value = DZERO
      do j = 1, this%naux
        ipos = (i - 1) * this%naux + j
        this%lauxvar(ipos)%name = ''
        if (this%iauxmultcol > 0 .and. j == this%iauxmultcol) then
          this%lauxvar(ipos)%value = DONE
        else
          this%lauxvar(ipos)%value = DZERO
        end if
      end do
    end do
    !
    ! -- allocate and initialize character array for budget text
    allocate(this%bdtxt(this%nbdtxt))
    this%bdtxt(1) = '         UZF-INF'
    this%bdtxt(2) = '       UZF-GWRCH'
    this%bdtxt(3) = '         UZF-GWD'
    this%bdtxt(4) = '        UZF-GWET'
    this%bdtxt(5) = '  UZF-GWD TO-MVR'
    !
    ! -- allocate character array for aux budget text
    allocate(this%cauxcbc(this%cbcauxitems))
    !
    ! -- allocate and initialize qauxcbc
    call mem_allocate(this%qauxcbc, this%cbcauxitems, 'QAUXCBC', this%origin)
    do i = 1, this%cbcauxitems
      this%qauxcbc(i) = DZERO
    end do
    !
    ! -- Allocate obs members
    call mem_allocate(this%obs_theta, 0, 'OBS_THETA', this%origin)
    call mem_allocate(this%obs_depth, 0, 'OBS_DEPTH', this%origin)
    call mem_allocate(this%obs_num, 0, 'OBS_NUM', this%origin)
    !
    ! -- return
    return
    end subroutine uzf_allocate_arrays
!

  subroutine uzf_options(this, option, found)
! ******************************************************************************
! uzf_options -- set options specific to UzfType
!
! uzf_options overrides BoundaryPackageType%child_class_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DZERO
    use OpenSpecModule, only: access, form
    use SimModule, only: ustop, store_error
    use InputOutputModule, only: urword, getunit, openfile
    implicit none
    ! -- dummy
    class(uzftype),   intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical,          intent(inout) :: found
    ! -- local
    character(len=MAXCHARLEN) :: fname, keyword
    real(DP) :: r
    ! -- formats
    character(len=*),parameter :: fmtnotfound= &
      "(4x, 'NO UZF OPTIONS WERE FOUND.')"
    character(len=*),parameter :: fmtet = &
      "(4x, 'ET WILL BE SIMULATED WITHIN UZ AND GW ZONES, WITH LINEAR ',  &
        'GWET IF OPTION NOT SPECIFIED OTHERWISE.')"
    character(len=*),parameter :: fmtgwetlin = &
      "(4x, 'GROUNDWATER ET FUNCTION WILL BE LINEAR.')"
    character(len=*),parameter :: fmtgwetsquare = &
      "(4x, 'GROUNDWATER ET FUNCTION WILL BE SQUARE WITH SMOOTHING.')"
    character(len=*),parameter :: fmtgwseepout = &
      "(4x, 'GROUNDWATER DISCHARGE TO LAND SURFACE WILL BE SIMULATED.')"
    character(len=*),parameter :: fmtuzetwc = &
      "(4x, 'UNSATURATED ET FUNCTION OF WATER CONTENT.')"
    character(len=*),parameter :: fmtuzetae = &
      "(4x, 'UNSATURATED ET FUNCTION OF AIR ENTRY PRESSURE.')"
    character(len=*),parameter :: fmtuznlay = &
      "(4x, 'UNSATURATED FLOW WILL BE SIMULATED SEPARATELY IN EACH LAYER.')"
    character(len=*),parameter :: fmtuzfbin = &
      "(4x, 'UZF ', 1x, a, 1x, ' WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
    character(len=*),parameter :: fmtuzfopt = &
      "(4x, 'UZF ', a, ' VALUE (',g15.7,') SPECIFIED.')"

! ------------------------------------------------------------------------------
    !
    !
    select case (option)
      !case ('PRINT_WATER-CONTENT')
      !  this%iprwcont = 1
      !  write(this%iout,'(4x,a)') trim(adjustl(this%text))// &
      !    ' WATERCONTENT WILL BE PRINTED TO LISTING FILE.'
      !  found = .true.
      !case('WATER-CONTENT')
      !  call this%parser%GetStringCaps(keyword)
      !  if (keyword == 'FILEOUT') then
      !    call this%parser%GetString(fname)
      !    this%iwcontout = getunit()
      !    call openfile(this%iwcontout, this%iout, fname, 'DATA(BINARY)',  &
      !                 form, access, 'REPLACE')
      !    write(this%iout,fmtuzfbin) 'WATERCONTENT', fname, this%iwcontout
      !    found = .true.
      !  else
      !    call store_error('OPTIONAL WATER-CONTENT KEYWORD MUST BE FOLLOWED BY FILEOUT')
      !  end if
      case('BUDGET')
        call this%parser%GetStringCaps(keyword)
        if (keyword == 'FILEOUT') then
          call this%parser%GetString(fname)
          this%ibudgetout = getunit()
          call openfile(this%ibudgetout, this%iout, fname, 'DATA(BINARY)',  &
                        form, access, 'REPLACE')
          write(this%iout,fmtuzfbin) 'BUDGET', fname, this%ibudgetout
          found = .true.
        else
          call store_error('OPTIONAL BUDGET KEYWORD MUST BE FOLLOWED BY FILEOUT')
        end if
      case('SIMULATE_ET')
        this%ietflag = 1    !default
        this%igwetflag = 0
        found = .true.
        write(this%iout, fmtet)
      case('LINEAR_GWET')
        this%igwetflag = 1
        found = .true.
        write(this%iout, fmtgwetlin)
      case('SQUARE_GWET')
        this%igwetflag = 2
        found = .true.
        write(this%iout, fmtgwetsquare)
      case('SIMULATE_GWSEEP')
        this%iseepflag = 1
        found = .true.
        write(this%iout, fmtgwseepout)
      case('UNSAT_ETWC')
        this%ietflag = 1
        found = .true.
        write(this%iout, fmtuzetwc)
      case('UNSAT_ETAE')
        this%ietflag = 2
        found = .true.
        write(this%iout, fmtuzetae)
      case('MOVER')
        this%imover = 1
        found = .true.
      !
      ! -- right now these are options that are available but may not be available in
      !    the release (or in documentation)
      case('DEV_NO_FINAL_CHECK')
        call this%parser%DevOpt()
        this%iconvchk = 0
        write(this%iout, '(4x,a)')                                             &
     &    'A FINAL CONVERGENCE CHECK OF THE CHANGE IN UZF RECHARGE ' //        &
     &    'WILL NOT BE MADE'
        found = .true.
      case('DEV_MAXIMUM_PERCENT_DIFFERENCE')
        call this%parser%DevOpt()
        r = this%parser%GetDouble()
        if (r > DZERO) then
          this%pdmax = r
          write(this%iout, fmtuzfopt) 'MAXIMUM_PERCENT_DIFFERENCE', this%pdmax
        else
          write(this%iout, fmtuzfopt) 'INVALID MAXIMUM_PERCENT_DIFFERENCE', r
          write(this%iout, fmtuzfopt) 'USING DEFAULT MAXIMUM_PERCENT_DIFFERENCE', this%pdmax
        end if
        found = .true.
     case default
    ! -- No options found
        found = .false.
    end select
    ! -- return
    return
  end subroutine uzf_options
!
  subroutine uzf_readdimensions(this)
! ******************************************************************************
! uzf_readdimensions -- set dimensions specific to UzfType
!
! uzf_readdimensions BoundaryPackageType%readdimensions
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error, count_errors
    class(uzftype),intent(inout) :: this
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- initialize dimensions to -1
    this%nodes= -1
    this%ntrail = 0
    this%nsets = 0
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
          case ('NUZFCELLS')
            this%nodes = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)')'NUZFCELLS = ', this%nodes
          case ('NTRAILWAVES')
            this%ntrail = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)')'NTRAILWAVES = ', this%ntrail
          case ('NWAVESETS')
            this%nsets = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)')'NTRAILSETS = ', this%nsets
          case default
            write(errmsg,'(4x,a,a)') &
              '****ERROR. UNKNOWN '//trim(this%text)//' DIMENSION: ', &
                                     trim(keyword)
            call store_error(errmsg)
            call ustop()
          end select
      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' DIMENSIONS'
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    ! -- increment maxbound
    this%maxbound = this%maxbound + this%nodes
    !
    ! -- verify dimensions were set
    if(this%nodes <= 0) then
      write(errmsg, '(1x,a)') &
        'ERROR.  NUZFCELLS WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    endif

    if(this%ntrail <= 0) then
      write(errmsg, '(1x,a)') &
        'ERROR.  NTRAILWAVES WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    if(this%nsets <= 0) then
      write(errmsg, '(1x,a)') &
        'ERROR.  NTRAILSETS WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    this%nwav = this%ntrail*this%nsets

  !! allocate variables
  !   call this%allocate_vars()
    !
    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
!
  end subroutine uzf_readdimensions

  subroutine uzf_rp(this)
! ******************************************************************************
! uzf_rp -- Read stress data
! Subroutine: (1) check if bc changes
!             (2) read new bc for stress period
!             (3) set kinematic variables to bc values
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kper, nper, perlen, totimsav
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error, count_errors
    use UzfKinematicModule, only: UzfKinematicType
    ! -- dummy
    class(UzfType), intent(inout) :: this
    ! -- local
    character(len=LENBOUNDNAME) :: bndName
    character(len=LENBOUNDNAME) :: cval
    integer (I4B) :: i
    integer (I4B) :: j
    integer (I4B) :: jj
    integer (I4B) :: ipos
    integer(I4B) :: ierr, ivertflag
    real (DP) :: endtim
    logical :: isfound, endOfBlock
    character(len=LINELENGTH) :: line, errmsg
    ! -- table output
    character (len=20) :: cellids, cellid
    character(len=LINELENGTH) :: linesep
    character(len=16) :: text
    integer(I4B) :: n
    integer(I4B) :: node
    integer(I4B) :: iloc
    real(DP) :: q
    !-- formats
    character(len=*),parameter :: fmtlsp = &
        "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
      character(len=*),parameter :: fmtblkerr = &
        "('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*), parameter :: fmtisvflow =                                &
        "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //  &
        "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*),parameter :: fmtflow =                                    &
        "(4x, 'FLOWS WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
! ------------------------------------------------------------------------------
    !
    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
    if(this%inunit == 0) return
    !
    ! -- Find time interval of current stress period.
    endtim = totimsav + perlen(kper)
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
    ! -- set steady-state flag based on gwfiss
    this%issflag = this%gwfiss
    !
    ! -- read data if ionper == kper
    if(this%ionper==kper) then
      !
      ! -- write header
      if (this%iprpak /= 0) then
        !
        ! -- set cell id based on discretization
        if (this%dis%ndim == 3) then
          cellids = '(LAYER,ROW,COLUMN)  '
        elseif (this%dis%ndim == 2) then
          cellids = '(LAYER,CELL2D)      '
        else
          cellids = '(NODE)              '
        end if
        write (this%iout, '(//3a)')                                                 &
          'UZF PACKAGE (', trim(adjustl(this%name)), ') STRESS PERIOD DATA'
        !<uzfno> <finf> <pet> <extdp> <extwc> <ha> <hroot> <rootact>
        iloc = 1
        line = ''
        if(this%inamedbound==1) then
          call UWWORD(line, iloc, 16, 1, 'name', n, q, left=.TRUE.)
        end if
        call UWWORD(line, iloc, 6, 1, 'no.', n, q, CENTER=.TRUE., sep=' ')
        call UWWORD(line, iloc, 20, 1, cellids, n, q, CENTER=.TRUE., sep=' ')
        call UWWORD(line, iloc, 11, 1, 'finf', n, q, CENTER=.TRUE., sep=' ')
        if (this%ietflag /= 0) then
          call UWWORD(line, iloc, 11, 1, 'pet', n, q, CENTER=.TRUE., sep=' ')
          call UWWORD(line, iloc, 11, 1, 'extdep', n, q, CENTER=.TRUE., sep=' ')
          call UWWORD(line, iloc, 11, 1, 'extwc', n, q, CENTER=.TRUE., sep=' ')
          if (this%ietflag == 2) then
            call UWWORD(line, iloc, 11, 1, 'ha', n, q, CENTER=.TRUE., sep=' ')
            call UWWORD(line, iloc, 11, 1, 'hroot', n, q, CENTER=.TRUE., sep=' ')
            call UWWORD(line, iloc, 11, 1, 'rootact', n, q, CENTER=.TRUE.)
          end if
        end if
        ! -- create line separator
        linesep = repeat('-', iloc)
        ! -- write header line and separator
        write(this%iout,'(1X,A)') line(1:iloc)
        write(this%iout,'(1X,A)') linesep(1:iloc)
      end if
      !
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        !
        ! -- check for valid uzf node
        i = this%parser%GetInteger()
        if (i < 1 .or. i > this%nodes) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. UZFNO MUST BE > 0 and <= ', this%nodes
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
        !
        ! -- setup pointers
        this%uzfobj => this%elements(i)
        ivertflag = this%uzfobj%ivertcon
        if ( ivertflag > 0 ) then
          this%uzfobjbelow => this%elements(ivertflag)
        else
          ! -- point to i so not null.  Does not use in this case.
          this%uzfobjbelow => this%elements(i)
        end if
        !
        !
        if (this%inamedbound > 0) then
          bndName = this%boundname(i)
        else
          bndName = ''
        end if
        !
        ! -- FINF
        call this%parser%GetStringCaps(cval)
        jj = 1    ! For SINF
        call read_single_value_or_time_series(cval, &
                                              this%sinf(i)%value, &
                                              this%sinf(i)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, i, jj, 'SINF', &
                                              bndName, this%inunit)
        !
        ! -- PET, EXTDP
        call this%parser%GetStringCaps(cval)
        jj = 1    ! For PET
        call read_single_value_or_time_series(cval, &
                                              this%pet(i)%value, &
                                              this%pet(i)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, i, jj, 'PET', &
                                              bndName, this%inunit)
        call this%parser%GetStringCaps(cval)
        jj = 1    ! For EXTDP
        call read_single_value_or_time_series(cval, &
                                              this%extdp(i)%value, &
                                              this%extdp(i)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, i, jj, 'EXTDP', &
                                              bndName, this%inunit)
        !
        ! -- ETWC
        call this%parser%GetStringCaps(cval)
        jj = 1    ! For EXTWC
        call read_single_value_or_time_series(cval, &
                                              this%extwc(i)%value, &
                                              this%extwc(i)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, i, jj, 'EXTWC', &
                                              bndName, this%inunit)
        !
        ! -- HA, HROOT, ROOTACT
        call this%parser%GetStringCaps(cval)
        jj = 1    ! For HA
        call read_single_value_or_time_series(cval, &
                                              this%ha(i)%value, &
                                              this%ha(i)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, i, jj, 'HA', &
                                              bndName, this%inunit)
        call this%parser%GetStringCaps(cval)
        jj = 1    ! For HROOT
        call read_single_value_or_time_series(cval, &
                                              this%hroot(i)%value, &
                                              this%hroot(i)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, i, jj, 'HROOT', &
                                              bndName, this%inunit)
        call this%parser%GetStringCaps(cval)
        jj = 1    ! For ROOTACT
        call read_single_value_or_time_series(cval, &
                                              this%rootact(i)%value, &
                                              this%rootact(i)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, i, jj, 'ROOTACT', &
                                              bndName, this%inunit)

        !
        ! -- read auxillary variables
        do j = 1, this%naux
          call this%parser%GetStringCaps(cval)
          ipos = (i - 1) * this%naux + j
          jj = 1
          call read_single_value_or_time_series(cval, &
                                                this%lauxvar(ipos)%value, &
                                                this%lauxvar(ipos)%name, &
                                                endtim,  &
                                                this%name, 'BND', this%TsManager, &
                                                this%iprpak, i, jj, &
                                                this%auxname(j), bndName, &
                                                this%inunit)
        end do
        !
        ! -- write line
        if (this%iprpak /= 0) then
          !
          ! -- get cellid
          node = this%mfcellid(i)
          if (node > 0) then
            call this%dis%noder_to_string(node, cellid)
          else
            cellid = 'none'
          end if
          !
          ! -- fill line
          !<uzfno> <finf> <pet> <extdp> <extwc> <ha> <hroot> <rootact>
          iloc = 1
          line = ''
          if(this%inamedbound==1) then
            call UWWORD(line, iloc, 16, 1, this%boundname(i), n, q, left=.TRUE.)
          end if
          call UWWORD(line, iloc, 6, 2, text, i, q, sep=' ')
          call UWWORD(line, iloc, 20, 1, cellid, n, q, left=.TRUE.)
          call UWWORD(line, iloc, 11, 3, text, i, this%sinf(i)%value, sep=' ')
          if (this%ietflag /= 0) then
            call UWWORD(line, iloc, 11, 3, text, i, this%pet(i)%value, sep=' ')
            call UWWORD(line, iloc, 11, 3, text, i, this%extdp(i)%value, sep=' ')
            call UWWORD(line, iloc, 11, 3, text, i, this%extwc(i)%value, sep=' ')
            if (this%ietflag == 2) then
              call UWWORD(line, iloc, 11, 3, text, i, this%ha(i)%value, sep=' ')
              call UWWORD(line, iloc, 11, 3, text, i, this%hroot(i)%value, sep=' ')
              call UWWORD(line, iloc, 11, 3, text, i, this%rootact(i)%value)
            end if
          end if
          ! -- write line
          write(this%iout,'(1X,A)') line(1:iloc)
        end if

      end do
      if (this%iprpak /= 0) then
        write(this%iout,'(1X,A)') linesep(1:iloc)
      end if

      write(this%iout,'(1x,a,1x,i6)')'END OF '//trim(adjustl(this%text)) //    &
        ' PERIOD', kper
    else
      write(this%iout,fmtlsp) trim(this%filtyp)
    endif
    !
    !write summary of uzf stress period error messages
    ierr = count_errors()
    if (ierr > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- set wave data for first stress period and second that follows SS
    if ( (this%issflag == 0 .AND. kper == 1) .or.                              &
      (kper == 2 .AND. this%issflagold == 1) ) then
      do i = 1, this%nodes
        this%uzfobj => this%elements(i)
        call this%uzfobj%setwaves(i)
      end do
    end if
    this%issflagold = this%issflag
    !call this%deallocate_vars()
    !
    ! -- return
    return
  end subroutine uzf_rp

  subroutine uzf_ad(this)
! ******************************************************************************
! uzf_ad -- Advance UZF Package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(UzfType) :: this
    ! -- locals
    integer (I4B) :: i
    integer (I4B) :: ivertflag
    integer (I4B) :: ipos
    real (DP) :: rval1, rval2, rval3
! ------------------------------------------------------------------------------
    !
    ! -- Advance the time series
    call this%TsManager%ad()
    !
    do i = 1, this%nodes
        this%uzfobj => this%elements(i)
        call this%uzfobj%advance()
    end do
    !
    ! -- update uzf objects with timeseries aware variables
    do i = 1, this%nodes
      !
      ! -- setup pointers
      this%uzfobj => this%elements(i)
      ivertflag = this%uzfobj%ivertcon
      if ( ivertflag > 0 ) then
        this%uzfobjbelow => this%elements(ivertflag)
      else
        ! -- point to iuzf so not null.  Does not use in this case.
        this%uzfobjbelow => this%elements(i)
      end if
      !
      ! -- recalculate uzfarea
      if (this%iauxmultcol > 0) then
        ipos = (i - 1) * this%naux + this%iauxmultcol
        rval1 = this%lauxvar(ipos)%value
        call this%uzfobj%setdatauzfarea(rval1)
      end if
      !
      ! -- FINF
      rval1 = this%sinf(i)%value
      call this%uzfobj%setdatafinf(rval1)
      !
      ! -- PET, EXTDP
      rval1 = this%pet(i)%value
      rval2 = this%extdp(i)%value
      call this%uzfobj%setdataet(this%uzfobjbelow, ivertflag, rval1, rval2)
      !
      ! -- ETWC
      rval1 = this%extwc(i)%value
      call this%uzfobj%setdataetwc(this%uzfobjbelow, ivertflag, rval1)
      !
      ! -- HA, HROOT, ROOTACT
      rval1 = this%ha(i)%value
      rval2 = this%hroot(i)%value
      rval3 = this%rootact(i)%value
      call this%uzfobj%setdataetha(this%uzfobjbelow, ivertflag, rval1,       &
                                    rval2, rval3)
    end do
    !
    ! -- check uzfarea
    if (this%iauxmultcol > 0) then
      call this%check_cell_area()
    end if
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
    ! -- Return
    return
  end subroutine uzf_ad

  subroutine uzf_cf(this)
! ******************************************************************************
! uzf_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip if no UZF cells
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(UzfType) :: this
    ! -- locals
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Return if no UZF cells
    if(this%nodes == 0) return
    !
    ! -- Store values at start of outer iteration to compare with calculated
    !    values for convergence check
    do n = 1, this%maxbound
      this%rejinf0(n) = this%rejinf(n)
      this%rch0(n) = this%rch(n)
      this%gwd0(n) = this%gwd(n)
    end do
    !
    ! -- pakmvrobj cf
    if(this%imover == 1) then
      call this%pakmvrobj%cf()
    endif
    !
    ! -- return
    return
  end subroutine uzf_cf

  subroutine uzf_fc(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! uzf_fc -- Copy rhs and hcof into solution rhs and amat
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i, n, ipos
! ------------------------------------------------------------------------------
    !
    ! -- pakmvrobj fc
    if(this%imover == 1) then
      call this%pakmvrobj%fc()
    endif
    !
    ! -- Solve UZF
    call this%uzf_solve()
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nodes
      n = this%nodelist(i)
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + this%hcof(i)
    enddo
    !
    ! -- return
    return
  end subroutine uzf_fc
!
  subroutine uzf_fn(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! uzf_fn -- Fill newton terms
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(UzfType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i, n
    integer(I4B) :: ipos
! --------------------------------------------------------------------------
    !
    ! -- Add derivative terms to rhs and amat
    do i = 1, this%nodes
      n = this%nodelist(i)
      ipos = ia(n)
      amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + this%deriv(i)
      rhs(n) = rhs(n) + this%deriv(i) * this%xnew(n)
    end do
    !
    ! -- return
    return
  end subroutine uzf_fn

  subroutine uzf_cc(this, iend, icnvg)
! **************************************************************************
! uzf_cc -- Final convergence check for package
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    use InputOutputModule, only: UWWORD
    ! -- dummy
    class(Uzftype), intent(inout) :: this
    integer(I4B), intent(in) :: iend
    integer(I4B), intent(inout) :: icnvg
    ! -- local
    character(len=LINELENGTH) :: line, linesep
    character(len=16) :: text
    integer(I4B) :: n
    integer(I4B) :: ifirst
    integer(I4B) :: iloc
    real(DP) :: r
    real(DP) :: drejinf
    real(DP) :: avgrejinf
    real(DP) :: pdrejinf
    real(DP) :: drch
    real(DP) :: avgrch
    real(DP) :: pdrch
    real(DP) :: dseep
    real(DP) :: avgseep
    real(DP) :: pdseep
    ! format
! --------------------------------------------------------------------------
    ifirst = 1
    if (this%iconvchk /= 0) then
      final_check: do n = 1, this%nodes
        drejinf = this%rejinf0(n) - this%rejinf(n)
        avgrejinf = DHALF * (this%rejinf0(n) + this%rejinf(n))
        pdrejinf = DZERO
        if (avgrejinf > DZERO) then
          pdrejinf = DHUNDRED * drejinf / avgrejinf
        end if
        drch = this%rch0(n) - this%rch(n)
        avgrch = DHALF * (this%rch0(n) + this%rch(n))
        pdrch = DZERO
        if (avgrch > DZERO) then
          pdrch = DHUNDRED * drch / avgrch
        end if
        avgseep = DZERO
        if (this%iseepflag == 1) then
          dseep = this%gwd0(n) - this%gwd(n)
          avgseep = DHALF * (this%gwd0(n) + this%gwd(n))
        end if
        pdseep = DZERO
        if (avgseep > DZERO) then
          pdseep = DHUNDRED * dseep / avgseep
        end if
        if (ABS(pdrejinf) > this%pdmax .or. ABS(pdrch) > this%pdmax .or. ABS(pdseep) > this%pdmax) then
          icnvg = 0
          ! write convergence check information if this is the last outer iteration
          if (iend == 1) then
            ! -- write header
            if (ifirst == 1) then
              ifirst = 0
              ! -- create first header line
              iloc = 1
              line = ''
              call UWWORD(line, iloc, 10, 1, 'uzf', n, r, CENTER=.TRUE., sep=' ')
              call UWWORD(line, iloc, 15, 1, 'rej infil', n, r, CENTER=.TRUE., sep=' ')
              call UWWORD(line, iloc, 15, 1, 'rej infil', n, r, CENTER=.TRUE., sep=' ')
              call UWWORD(line, iloc, 15, 1, 'gwf recharge', n, r, CENTER=.TRUE., sep=' ')
              call UWWORD(line, iloc, 15, 1, 'gwf recharge', n, r, CENTER=.TRUE., sep=' ')
              if (this%iseepflag == 1) then
                call UWWORD(line, iloc, 15, 1, 'gwf seepage', n, r, CENTER=.TRUE., sep=' ')
                call UWWORD(line, iloc, 15, 1, 'gwf seepage', n, r, CENTER=.TRUE., sep=' ')
              end if
              call UWWORD(line, iloc, 15, 1, 'pct difference', n, r, CENTER=.TRUE.)
              ! -- create line separator
              linesep = repeat('-', iloc)
              ! -- write first line
              write(this%iout,'(/1X,A)') 'UZF PACKAGE FAILED CONVERGENCE CRITERIA'
              write(this%iout,'(1X,A)') linesep(1:iloc)
              write(this%iout,'(1X,A)') line(1:iloc)
              ! -- create second header line
              iloc = 1
              line = ''
              call UWWORD(line, iloc, 10, 1, 'cell', n, r, CENTER=.TRUE., sep=' ')
              call UWWORD(line, iloc, 15, 1, 'difference', n, r, CENTER=.TRUE., sep=' ')
              call UWWORD(line, iloc, 15, 1, 'pct difference', n, r, CENTER=.TRUE., sep=' ')
              call UWWORD(line, iloc, 15, 1, 'difference', n, r, CENTER=.TRUE., sep=' ')
              call UWWORD(line, iloc, 15, 1, 'pct difference', n, r, CENTER=.TRUE., sep=' ')
              if (this%iseepflag == 1) then
                call UWWORD(line, iloc, 15, 1, 'difference', n, r, CENTER=.TRUE., sep=' ')
                call UWWORD(line, iloc, 15, 1, 'pct difference', n, r, CENTER=.TRUE., sep=' ')
              end if
              call UWWORD(line, iloc, 15, 1, 'criteria', n, r, CENTER=.TRUE.)
              ! -- write second line
              write(this%iout,'(1X,A)') line(1:iloc)
              write(this%iout,'(1X,A)') linesep(1:iloc)
            end if
            ! -- write data
            iloc = 1
            line = ''
            call UWWORD(line, iloc, 10, 2, text, n, r, sep=' ')
            call UWWORD(line, iloc, 15, 3, text, n, drejinf, sep=' ')
            call UWWORD(line, iloc, 15, 3, text, n, pdrejinf, sep=' ')
            call UWWORD(line, iloc, 15, 3, text, n, drch, sep=' ')
            call UWWORD(line, iloc, 15, 3, text, n, pdrch, sep=' ')
            if (this%iseepflag == 1) then
              call UWWORD(line, iloc, 15, 3, text, n, dseep, sep=' ')
              call UWWORD(line, iloc, 15, 3, text, n, pdseep, sep=' ')
            end if
            call UWWORD(line, iloc, 15, 3, text, n, this%pdmax)
            write(this%iout, '(1X,A)') line(1:iloc)
          else
            exit final_check
          end if
        end if
      end do final_check
      if (ifirst == 0) then
        write(this%iout,'(1X,A)') linesep(1:iloc)
      end if
    end if
    !
    ! -- return
    return
  end subroutine uzf_cc

  subroutine uzf_bd(this, x, idvfl, icbcfl, ibudfl, icbcun, iprobs,            &
                    isuppress_output, model_budget, imap, iadv)
! ******************************************************************************
! uzf_bd -- Calculate Volumetric Budget
! Note that the compact budget will always be used.
! Subroutine: (1) Process each package entry
!             (2) Write output
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper, delt, pertim, totim
    use ConstantsModule, only: LENBOUNDNAME, DZERO, DHNOFLO, DHDRY
    use BudgetModule, only: BudgetType
    use InputOutputModule, only: ulasav, ubdsv06
    ! -- dummy
    class(UzfType) :: this
    class(ObserveType),   pointer :: obsrv => null()
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
    integer(I4B) :: i, node, ibinun
    integer(I4B) :: ii
    integer(I4B) :: n, m, ivertflag, ierr
    integer(I4B) :: n1, n2
    integer(I4B) :: nlen
    real(DP) :: rfinf
    real(DP) :: rin,rout,rsto,ret,retgw,rgwseep,rvflux
    real(DP) :: rstoin
    real(DP) :: rstoout
    real(DP) :: hgwf,hgwflm1,ratin,ratout,rrate,rrech
    real(DP) :: trhsgwet,thcofgwet,gwet,derivgwet
    real(DP) :: qfrommvr, qformvr, qgwformvr, sumaet
    real(DP) :: qfinf
    real(DP) :: qrejinf
    real(DP) :: qrejinftomvr
    real(DP) :: qout
    real(DP) :: qfact
    real(DP) :: qtomvr
    real(DP) :: sqtomvr
    real(DP) :: q
    real(DP) :: rfrommvr
    real(DP) :: qseep
    real(DP) :: qseeptomvr
    real(DP) :: qgwet
    integer(I4B) :: ibdlbl, naux, numobs
    ! -- for observations
    integer(I4B) :: j
    character(len=LENBOUNDNAME) :: bname
    character(len=100) :: msg
    ! -- formats
    character(len=*), parameter :: fmttkk = &
      "(1X,/1X,A,'   PERIOD ',I0,'   STEP ',I0)"
    character(len=LENBUDTXT) :: aname(10)
    ! -- for table
    data aname(1)  /'    INFILTRATION'/
    data aname(2)  /'             GWF'/
    data aname(3)  /'         STORAGE'/
    data aname(4)  /'            UZET'/
    data aname(5)  /'        UZF-GWET'/
    data aname(6)  /'         UZF-GWD'/
    data aname(7)  /'SAT.-UNSAT. EXCH'/
    data aname(8)  /'         REJ-INF'/
    data aname(9)  /'  REJ-INF-TO-MVR'/
    data aname(10) /'        FROM-MVR'/
! ------------------------------------------------------------------------------
    !
    ! -- initialize accumulators
    ierr = 0
    rfinf = DZERO
    rin = DZERO
    rout = DZERO
    rrech = DZERO
    rsto = DZERO
    rstoin = DZERO
    rstoout = DZERO
    ret = DZERO
    retgw = DZERO
    rgwseep = DZERO
    rvflux = DZERO
    sumaet = DZERO
    qfinf = DZERO
    qfrommvr = DZERO
    qtomvr = DZERO
    qrejinf = DZERO
    qrejinftomvr = DZERO
    sqtomvr = DZERO
    rfrommvr = DZERO
    qseep = DZERO
    qseeptomvr = DZERO
    qgwet = DZERO
    !
    ! -- Budget for each UZF model (start by resetting)
    call this%budget%reset()
    !
    ! -- Go through and process each UZF cell
    do i = 1, this%nodes
      !
      ! -- Initialize variables
      n = this%nodelist(i)
      this%uzfobj => this%elements(i)
      ivertflag = this%uzfobj%ivertcon
      !
      ! Create pointer to object below
      if ( ivertflag > 0 ) then
        this%uzfobjbelow => this%elements(ivertflag)
      else
        ! -- point to i so not null.  Does not use in this case.
        this%uzfobjbelow => this%elements(i)
      end if
      !
      ! -- Skip if cell is not active
      if (this%ibound(n) < 1) cycle
      !
      ! -- Water mover added to infiltration
      qfrommvr = DZERO
      qformvr = DZERO
      if(this%imover == 1) then
        qfrommvr = this%pakmvrobj%get_qfrommvr(i)
        rfrommvr = rfrommvr + qfrommvr
      endif
      !
      hgwf = this%xnew(n)
      !
      m = n
      hgwflm1 = hgwf
      !
      ! -- Get obs information, check if there is obs in uzf cell
      numobs = 0
      do j = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(j)%obsrv
        if ( obsrv%intPak1 == i ) then
          numobs = numobs + 1
          this%obs_num(numobs) = j
          this%obs_depth(j) = obsrv%dblPak1
        end if
      end do
      !
      ! -- Call budget routine of the uzf kinematic object
      call this%uzfobj%budget(this%uzfobjbelow,i,this%totfluxtot,              &
                              rfinf,rin,rout,rsto,ret,retgw,rgwseep,rvflux,    &
                              this%ietflag,this%iseepflag,this%issflag,hgwf,   &
                              hgwflm1,this%gwfhcond(m),numobs,this%obs_num,    &
                              this%obs_depth,this%obs_theta,qfrommvr,qformvr,  &
                              qgwformvr,sumaet,ierr)
      if ( ierr > 0 ) then
        if ( ierr == 1 ) &
          msg = 'Error: UZF variable NWAVSETS needs to be increased.'
        call store_error(msg)
        call ustop()
      end if
      !
      ! -- Calculate gwet
      if (this%igwetflag > 0) then
        gwet = DZERO
        derivgwet = DZERO
        call this%uzfobj%simgwet(this%igwetflag, i, hgwf, trhsgwet, thcofgwet, &
                                 gwet, derivgwet)
        !retgw = retgw + trhsgwet + (thcofgwet * hgwf)
        retgw = retgw + this%gwet(i)
      end if
      !
      ! -- Calculate flows for cbc output and observations
      if ( hgwf > this%uzfobj%celbot ) then
        this%recharge(i) = this%uzfobj%totflux * this%uzfobj%uzfarea / delt
      else
        this%recharge(i) = this%uzfobjbelow%surflux * this%uzfobj%uzfarea
      end if

      this%rch(i) = this%uzfobj%totflux * this%uzfobj%uzfarea / delt

      this%appliedinf(i) = this%uzfobj%sinf * this%uzfobj%uzfarea
      this%infiltration(i) = this%uzfobj%surflux * this%uzfobj%uzfarea

      this%rejinf(i) = this%uzfobj%finf_rej * this%uzfobj%uzfarea

      qout = this%rejinf(i) + this%uzfobj%surfseep
      qtomvr = DZERO
      if (this%imover == 1) then
        qtomvr = this%pakmvrobj%get_qtomvr(i)
        sqtomvr = sqtomvr + qtomvr
      end if

      qfact = DZERO
      if (qout > DZERO) then
        qfact = this%rejinf(i) / qout
      end if
      q = this%rejinf(i)
      this%rejinftomvr(i) = qfact * qtomvr
      ! -- set rejected infiltration to the remainder
      q = q - this%rejinftomvr(i)
      ! -- values less than zero represent a volumetric error resulting
      !    from qtomvr being greater than water available to the mover
      if (q < DZERO) then
        q = DZERO
      end if
      this%rejinf(i) = q

      this%gwd(i) = this%uzfobj%surfseep
      qfact = DZERO
      if (qout > DZERO) then
        qfact = this%gwd(i) / qout
      end if
      q = this%gwd(i)
      this%gwdtomvr(i) = qfact * qtomvr
      ! -- set groundwater discharge to the remainder
      q = q - this%gwdtomvr(i)
      ! -- values less than zero represent a volumetric error resulting
      !    from qtomvr being greater than water available to the mover
      if (q < DZERO) then
        q = DZERO
      end if
      this%gwd(i) = q

      qfinf = qfinf + this%appliedinf(i)
      qrejinf = qrejinf + this%rejinf(i)
      qrejinftomvr = qrejinftomvr + this%rejinftomvr(i)

      qseep = qseep + this%gwd(i)
      qseeptomvr = qseeptomvr + this%gwdtomvr(i)

      this%gwet(i) = this%uzfobj%gwet
      this%uzet(i) = this%uzfobj%etact*this%uzfobj%uzfarea / delt
      this%qsto(i) = this%uzfobj%delstor / delt

      ! -- accumulate groundwater et
      qgwet = qgwet + this%gwet(i)

      if (this%qsto(i) < DZERO) then
        rstoin = rstoin - this%qsto(i)
      else
        rstoout = rstoout + this%qsto(i)
      end if
      !
      ! -- End of UZF cell loop
      !
    end do
    !
    ! -- For continuous observations, save simulated values.
    if (this%obs%npakobs > 0 .and. iprobs > 0) then
      call this%uzf_bd_obs
    endif
    !
    ! add cumulative flows to UZF budget
    this%infilsum = rin * delt
    this%rechsum = rout * delt
    rrech = rout
    this%delstorsum = rsto * delt
    this%uzetsum = ret * delt
    this%vfluxsum = rvflux

    call this%budget%addentry(qfinf, DZERO, delt, aname(1), isuppress_output)
    if (this%imover == 1) then
      call this%budget%addentry(rfrommvr, DZERO, delt, aname(10), isuppress_output)
    end if
    call this%budget%addentry(DZERO, qrejinf, delt, aname(8), isuppress_output)
    if (this%imover == 1) then
      call this%budget%addentry(DZERO, qrejinftomvr, delt, aname(9), isuppress_output)
    end if
    call this%budget%addentry(DZERO, rout, delt, aname(2), isuppress_output)
    if (this%ietflag /= 0) then
      call this%budget%addentry(DZERO, ret, delt, aname(4), isuppress_output)
    end if
    !
    !
    rin = DZERO
    rout = DZERO
    if(rsto < DZERO) then
      rin = -rsto
    else
      rout = rsto
    endif
    call this%budget%addentry(rstoin, rstoout, delt, aname(3), isuppress_output)
    !if ( isuppress_output == 0 ) call this%budget%budget_ot(kstp, kper, this%iout)

    !call this%uzf_bdsav(icbcfl, icbcun)

  ! output saturation or other variables for each cell
    call this%uzcelloutput(isuppress_output)
    !
    ! -- Clear accumulators and set flags
    ratin = dzero
    ratout = dzero
    rrate = dzero
    !iauxsv = 1  !always used compact budget
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
    if (isuppress_output /= 0) ibinun = 0
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if (ibinun /= 0 .or. ibudfl /= 0) then
      naux = this%naux
      !
      ! -- uzf-gwrch
      ibdlbl = 0
      if (ibinun /= 0) then
        call this%dis%record_srcdst_list_header(this%bdtxt(2), this%name_model, &
                    this%name_model, this%name_model, this%name, naux,          &
                    this%auxname, ibinun, this%nodes, this%iout)
      end if
      !
      ! -- Loop through each boundary calculating flow.
      do i = 1, this%nodes
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
        if (this%ibound(node) > 0) then
          !
          ! -- Calculate the flow rate into the cell.
          !rrate = this%hcof(i) * x(node) - this%rhs(i)
          rrate = this%rch(i)
          !
          ! -- Print the individual rates if requested(this%iprflow<0)
          if (ibudfl /= 0) then
            if (this%iprflow /= 0) then
              if (ibdlbl == 0) write(this%iout,fmttkk)                         &
                  this%bdtxt(2) // ' (' // trim(this%name) // ')', kper, kstp
              call this%dis%print_list_entry(i, node, rrate, this%iout,        &
                      bname)
              ibdlbl=1
            end if
          end if
        end if
        !
        ! -- If saving cell-by-cell flows in list, write flow
        if (ibinun /= 0) then
          n2 = i
          call this%dis%record_mf6_list_entry(ibinun, node, n2, rrate,         &
                                                  naux, this%auxvar(:,i),      &
                                                  olconv2=.FALSE.)
        end if
      end do
      !
      ! -- uzf-gwd
      if (this%iseepflag == 1) then
        ibdlbl = 0
        if (ibinun /= 0) then
          call this%dis%record_srcdst_list_header(this%bdtxt(3),               &
                      this%name_model,                                         &
                      this%name_model, this%name_model, this%name, naux,       &
                      this%auxname, ibinun, this%nodes, this%iout)
        end if
        !
        ! -- Loop through each boundary calculating flow.
        do i = 1, this%nodes
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
          if (this%ibound(node) > 0) then
            !
            ! -- Calculate the flow rate into the cell.
            rrate = -this%gwd(i)
            !
            ! -- Print the individual rates if requested(this%iprflow<0)
            if (ibudfl /= 0) then
              if (this%iprflow /= 0) then
                if (ibdlbl == 0) write(this%iout,fmttkk)                       &
                  this%bdtxt(3) // ' (' // trim(this%name) // ')', kper, kstp
                call this%dis%print_list_entry(i, node, rrate, this%iout, &
                        bname)
                ibdlbl=1
              end if
            end if
          end if
          !
          ! -- If saving cell-by-cell flows in list, write flow
          if (ibinun /= 0) then
            n2 = i
            call this%dis%record_mf6_list_entry(ibinun, node, n2, rrate,    &
                                                    naux, this%auxvar(:,i),     &
                                                    olconv2=.FALSE.)
          end if
        end do
        !
        ! -- uzf-gwd to mover
        if (this%imover == 1) then
          ibdlbl = 0
          if (ibinun /= 0) then
            call this%dis%record_srcdst_list_header(this%bdtxt(5),              &
                        this%name_model, this%name_model,                       &
                        this%name_model, this%name, naux,                       &
                        this%auxname, ibinun, this%nodes, this%iout)
          end if
          !
          ! -- Loop through each boundary calculating flow.
          do i = 1, this%nodes
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
            if (this%ibound(node) > 0) then
              !
              ! -- Calculate the flow rate into the cell.
              rrate = -this%gwdtomvr(i)
              !
              ! -- Print the individual rates if requested(this%iprflow<0)
              if (ibudfl /= 0) then
                if (this%iprflow /= 0) then
                  if (ibdlbl == 0) write(this%iout,fmttkk)                     &
                    this%bdtxt(5) // ' (' // trim(this%name) // ')', kper, kstp
                  call this%dis%print_list_entry(i, node, rrate, this%iout,    &
                          bname)
                  ibdlbl=1
                end if
              end if
            end if
            !
            ! -- If saving cell-by-cell flows in list, write flow
            if (ibinun /= 0) then
              n2 = i
              call this%dis%record_mf6_list_entry(ibinun, node, n2, rrate,  &
                                                      naux, this%auxvar(:,i),   &
                                                      olconv2=.FALSE.)
            end if
          end do
        end if
      end if
      ! -- uzf-evt
      if (this%ietflag /= 0) then
        ibdlbl = 0
        if (ibinun /= 0) then
          call this%dis%record_srcdst_list_header(this%bdtxt(4), this%name_model,&
                      this%name_model, this%name_model, this%name, naux,        &
                      this%auxname, ibinun, this%nodes, this%iout)
        end if
        !
        ! -- Loop through each boundary calculating flow.
        do i = 1, this%nodes
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
          if (this%ibound(node) > 0) then
            !
            ! -- Calculate the flow rate into the cell.
            rrate = -this%gwet(i)
            !
            ! -- Print the individual rates if requested(this%iprflow<0)
            if (ibudfl /= 0) then
              if (this%iprflow /= 0) then
                if (ibdlbl == 0) write(this%iout,fmttkk)                       &
                  this%bdtxt(4) // ' (' // trim(this%name) // ')', kper, kstp
                call this%dis%print_list_entry(i, node, rrate, this%iout, &
                        bname)
                ibdlbl=1
              end if
            end if
          end if
          !
          ! -- If saving cell-by-cell flows in list, write flow
          if (ibinun /= 0) then
            n2 = i
            call this%dis%record_mf6_list_entry(ibinun, node, n2, rrate,    &
                                                    naux, this%auxvar(:,i),     &
                                                    olconv2=.FALSE.)
          end if
        end do
      end if
    end if
    !
    ! -- Add the UZF rates to the model budget
    !uzf recharge
    ratin = rrech
    ratout = DZERO
    call model_budget%addentry(ratin, ratout, delt, this%bdtxt(2),                   &
                               isuppress_output, this%name)
    !groundwater discharge
    if (this%iseepflag == 1) then
      ratin = DZERO
      ratout = qseep !rgwseep
      call model_budget%addentry(ratin, ratout, delt, this%bdtxt(3),                 &
                                 isuppress_output, this%name)
      !groundwater discharge to mover
      if (this%imover == 1) then
        ratin = DZERO
        ratout = qseeptomvr
        call model_budget%addentry(ratin, ratout, delt, this%bdtxt(5),               &
                                   isuppress_output, this%name)
      end if
    end if
    !groundwater et
    if (this%igwetflag /= 0) then
      ratin = DZERO
      ratout = qgwet !retgw
      !ratout = DZERO
      !if (retgw > DZERO) then
      !  ratout = -retgw
      !end if
      call model_budget%addentry(ratin, ratout, delt, this%bdtxt(4),                 &
                                 isuppress_output, this%name)
    end if
    !
    ! -- set unit number for binary dependent variable output
    ibinun = 0
    if(this%iwcontout /= 0) then
      ibinun = this%iwcontout
    end if
    if(idvfl == 0) ibinun = 0
    if (isuppress_output /= 0) ibinun = 0
    !
    ! -- write uzf binary moisture-content output
    if (ibinun > 0) then
      ! here is where you add the code to write the simulated moisture content
      ! may want to write a cell-by-cell file with imeth=6 (see sfr and lake)
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
    ! -- write uzf binary budget output
    if (ibinun > 0) then
      ! FLOW JA FACE - uzf to uzf connections using outlets
      nlen = 0
      do n = 1, this%nodes
        !
        ! -- Initialize variables
        this%uzfobj => this%elements(n)
        ivertflag = this%uzfobj%ivertcon
        if ( ivertflag > 0 ) then
          nlen = nlen + 1
        end if
      end do
      if (nlen > 0) then
        naux = 0
        call ubdsv06(kstp, kper, '    FLOW-JA-FACE', this%name_model, this%name, &
                     this%name_model, this%name,                                 &
                     ibinun, naux, this%cauxcbc, nlen*2, 1, 1,                   &
                     nlen*2, this%iout, delt, pertim, totim)
        do n = 1, this%nodes
          !
          ! -- Initialize variables
          this%uzfobj => this%elements(n)
          ivertflag = this%uzfobj%ivertcon
          if ( ivertflag > 0 ) then
             q = this%uzfobj%surfluxbelow * this%uzfobj%uzfarea
             if (q > DZERO) then
               q = -q
             end if
             n1 = n
             n2 = ivertflag
             call this%dis%record_mf6_list_entry(ibinun, n1, n2, q, naux,     &
                                                    this%qauxcbc,                 &
                                                    olconv=.FALSE.,               &
                                                    olconv2=.FALSE.)
             q = this%uzfobj%surfluxbelow * this%uzfobj%uzfarea
             call this%dis%record_mf6_list_entry(ibinun, n2, n1, q, naux,     &
                                                    this%qauxcbc,                 &
                                                    olconv=.FALSE.,               &
                                                    olconv2=.FALSE.)

          end if
        end do
      end if
      ! GWF
      naux = this%cbcauxitems
      this%cauxcbc(1) = '       FLOW-AREA'
      call ubdsv06(kstp, kper, aname(2), this%name_model, this%name,            &
                   this%name_model, this%name_model,                            &
                   ibinun, naux, this%cauxcbc, this%nodes, 1, 1,                &
                   this%nodes, this%iout, delt, pertim, totim)
      do n = 1, this%nodes
        !
        ! -- Initialize variables
        this%uzfobj => this%elements(n)
        this%qauxcbc(1) = this%uzfobj%uzfarea
        n2 = this%mfcellid(n)
        q = -this%rch(n)
        call this%dis%record_mf6_list_entry(ibinun, n, n2, q, naux,         &
                                                this%qauxcbc,                   &
                                                olconv=.FALSE.)
      end do
      ! SPECIFIED INFILTRATION
      naux = 0
      call ubdsv06(kstp, kper, aname(1), this%name_model, this%name,            &
                   this%name_model, this%name,                                  &
                   ibinun, naux, this%cauxcbc, this%nodes, 1, 1,                &
                   this%nodes, this%iout, delt, pertim, totim)
      do n = 1, this%nodes
        q = this%appliedinf(n)
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,          &
                                                this%auxvar(:,n),               &
                                                olconv=.FALSE.,                 &
                                                olconv2=.FALSE.)
      end do
      ! REJECTED INFILTRATION
      naux = 0
      call ubdsv06(kstp, kper, aname(8), this%name_model, this%name,            &
                   this%name_model, this%name,                                  &
                   ibinun, naux, this%cauxcbc, this%nodes, 1, 1,                &
                   this%nodes, this%iout, delt, pertim, totim)
      do n = 1, this%nodes
        q = this%rejinf(n)
        if (q > DZERO) then
          q = -q
        end if
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! UNSATURATED EVT
      if (this%ietflag /= 0) then
        naux = 0
        call ubdsv06(kstp, kper, aname(4), this%name_model, this%name,          &
                     this%name_model, this%name,                                &
                     ibinun, naux, this%cauxcbc, this%nodes, 1, 1,              &
                     this%nodes, this%iout, delt, pertim, totim)
        do n = 1, this%nodes
          q = this%uzet(n)
          if (q > DZERO) then
            q = -q
          end if
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,        &
                                                  this%auxvar(:,n),             &
                                                  olconv=.FALSE.,               &
                                                  olconv2=.FALSE.)
        end do
      end if
      ! STORAGE
      naux = 0
      call ubdsv06(kstp, kper, aname(3), this%name_model, this%name,            &
                   this%name_model, this%name,                                  &
                   ibinun, naux, this%cauxcbc, this%nodes, 1, 1,                &
                   this%nodes, this%iout, delt, pertim, totim)
      do n = 1, this%nodes
        q = -this%qsto(n)
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,          &
                                                this%auxvar(:,n),               &
                                                olconv=.FALSE.,                 &
                                                olconv2=.FALSE.)
      end do
      ! MOVER
      if (this%imover == 1) then
        ! FROM MOVER
        naux = 0
        call ubdsv06(kstp, kper, aname(10), this%name_model,                    &
                     this%name, this%name_model, this%name,                     &
                     ibinun, naux, this%cauxcbc,                                &
                     this%nodes, 1, 1,                                          &
                     this%nodes, this%iout, delt, pertim, totim)
        do n = 1, this%nodes
          q = this%pakmvrobj%get_qfrommvr(n)
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,        &
                                                  this%auxvar(:,n),             &
                                                  olconv=.FALSE.,               &
                                                  olconv2=.FALSE.)
        end do
        ! TO MOVER
        naux = 0
        call ubdsv06(kstp, kper, aname(9), this%name_model,                     &
                     this%name, this%name_model, this%name,                     &
                     ibinun, naux, this%cauxcbc,                                &
                     this%nodes, 1, 1,                                          &
                     this%nodes, this%iout, delt, pertim, totim)
        do n = 1, this%nodes
          q = this%rejinftomvr(n)
          if (q > DZERO) then
            q = -q
          end if
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,        &
                                                  this%auxvar(:,n),             &
                                                  olconv=.FALSE.,               &
                                                  olconv2=.FALSE.)
        end do
      end if
      ! AUXILIARY VARIABLES
      naux = this%naux
      if (naux > 0) then
        call ubdsv06(kstp, kper, '       AUXILIARY', this%name_model, this%name,&
                     this%name_model, this%name,                                &
                     ibinun, naux, this%auxname, this%nodes, 1, 1,              &
                     this%nodes, this%iout, delt, pertim, totim)
        do n = 1, this%nodes
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
    !
    ! -- return
    return
  end subroutine uzf_bd

  subroutine uzf_ot(this, kstp, kper, iout, ihedfl, ibudfl)
! ******************************************************************************
! uzf_ot -- UZF package budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use InputOutputModule, only: UWWORD
    ! -- dummy
    class(UzfType) :: this
    integer(I4B),intent(in) :: kstp
    integer(I4B),intent(in) :: kper
    integer(I4B),intent(in) :: iout
    integer(I4B),intent(in) :: ihedfl
    integer(I4B),intent(in) :: ibudfl
    ! -- local
    character(len=LINELENGTH) :: line, linesep
    character(len=16) :: text
    integer(I4B) :: n
    integer(I4B) :: iloc
    integer(I4B) :: ivertflag
    real(DP) :: q
    real(DP) :: qin
    real(DP) :: qout
    real(DP) :: qerr
    real(DP) :: qavg
    real(DP) :: qpd
    ! -- format
 2000 FORMAT ( 1X, ///1X, A, A, A, '   PERIOD ', I6, '   STEP ', I8)
! ------------------------------------------------------------------------------
    !
    ! -- write uzf moisture content
    if (ihedfl /= 0 .and. this%iprwcont /= 0) then
      write (iout, 2000) 'UZF (', trim(this%name), ') WATER-CONTENT', kper, kstp
      ! add code to write moisture content
    end if
    !
    ! -- write uzf rates
    if (ibudfl /= 0 .and. this%iprflow /= 0) then
      write (iout, 2000) 'UZF (', trim(this%name), ') FLOWS', kper, kstp
       iloc = 1
       line = ''
       if(this%inamedbound==1) then
         call UWWORD(line, iloc, 16, 1, 'uzf', n, q, left=.TRUE.)
       end if
       call UWWORD(line, iloc, 6, 1, 'uzf', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'uzf', n, q, CENTER=.TRUE., sep=' ')
       if (this%iuzf2uzf == 1) then
         call UWWORD(line, iloc, 11, 1, 'uzf-uzf', n, q, CENTER=.TRUE., sep=' ')
       end if
       if (this%imover == 1) then
        call UWWORD(line, iloc, 11, 1, 'uzf', n, q, CENTER=.TRUE., sep=' ')
       end if
       call UWWORD(line, iloc, 11, 1, 'uzf', n, q, CENTER=.TRUE., sep=' ')
       if (this%imover == 1) then
         call UWWORD(line, iloc, 11, 1, 'uzf rej-inf', n, q, CENTER=.TRUE., sep=' ')
       end if
       if (this%ietflag /= 0) then
         call UWWORD(line, iloc, 11, 1, 'uzf', n, q, CENTER=.TRUE., sep=' ')
       end if
       call UWWORD(line, iloc, 11, 1, 'uzf', n, q, CENTER=.TRUE., sep=' ')
       if (this%iuzf2uzf == 1) then
         call UWWORD(line, iloc, 11, 1, 'uzf-uzf', n, q, CENTER=.TRUE., sep=' ')
       end if
       call UWWORD(line, iloc, 11, 1, 'uzf', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'uzf', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'percent', n, q, CENTER=.TRUE.)
       ! -- create line separator
       linesep = repeat('-', iloc)
       ! -- write first line
       write(iout,'(1X,A)') linesep(1:iloc)
       write(iout,'(1X,A)') line(1:iloc)
       ! -- create second header line
       iloc = 1
       line = ''
       if(this%inamedbound==1) then
         call UWWORD(line, iloc, 16, 1, 'name', n, q, left=.TRUE.)
       end if
       call UWWORD(line, iloc, 6, 1, 'no.', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'infilt.', n, q, CENTER=.TRUE., sep=' ')
       if (this%iuzf2uzf == 1) then
         call UWWORD(line, iloc, 11, 1, 'inflow', n, q, CENTER=.TRUE., sep=' ')
       end if
       if (this%imover == 1) then
         call UWWORD(line, iloc, 11, 1, 'from mvr', n, q, CENTER=.TRUE., sep=' ')
       end if
       call UWWORD(line, iloc, 11, 1, 'rej-inf', n, q, CENTER=.TRUE., sep=' ')
       if (this%imover == 1) then
         call UWWORD(line, iloc, 11, 1, 'to mvr', n, q, CENTER=.TRUE., sep=' ')
       end if
       if (this%ietflag /= 0) then
         call UWWORD(line, iloc, 11, 1, 'uzet', n, q, CENTER=.TRUE., sep=' ')
       end if
       call UWWORD(line, iloc, 11, 1, 'gwrch', n, q, CENTER=.TRUE., sep=' ')
       if (this%iuzf2uzf == 1) then
         call UWWORD(line, iloc, 11, 1, 'outflow', n, q, CENTER=.TRUE., sep=' ')
       end if
       call UWWORD(line, iloc, 11, 1, 'storage', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'in - out', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'difference', n, q, CENTER=.TRUE.)
       ! -- write second line
       write(iout,'(1X,A)') line(1:iloc)
       write(iout,'(1X,A)') linesep(1:iloc)
      ! write uzf rates for each uzf cell
      do n = 1, this%maxbound
        !
        ! -- reset accumulators
        qin = DZERO
        qout = DZERO
        !
        ! -- fill line
        iloc = 1
        line = ''
        if (this%inamedbound==1) then
          call UWWORD(line, iloc, 16, 1, this%boundname(n), n, q, left=.TRUE.)
        end if
        call UWWORD(line, iloc, 6, 2, text, n, q, CENTER=.TRUE., sep=' ')
        !
        ! -- specified infiltration
        q = this%appliedinf(n)
        qin = qin + q
        call UWWORD(line, iloc, 11, 3, text, n, q, sep=' ')
        !
        ! -- infiltration from cell above
        if (this%iuzf2uzf == 1) then
          q = DZERO
          this%uzfobj => this%elements(n)
          if (this%uzfobj%landflag == 0) then
            q = this%infiltration(n)
            qin = qin + q
          end if
          call UWWORD(line, iloc, 11, 3, text, n, q, sep=' ')
        end if
        !
        ! -- from mover
        if (this%imover == 1) then
          q = this%pakmvrobj%get_qfrommvr(n)
          qin = qin + q
          call UWWORD(line, iloc, 11, 3, text, n, q, sep=' ')
        end if
        !
        ! -- rejected infiltration
        q = this%rejinf(n)
        if (q > DZERO) then
          qout = qout + q
          q = -q
        end if
        call UWWORD(line, iloc, 11, 3, text, n, q, sep=' ')
        !
        ! -- rejected infiltration to mover
        if (this%imover == 1) then
          q = this%rejinftomvr(n)
          if (q > DZERO) then
            qout = qout + q
            q = -q
          end if
          call UWWORD(line, iloc, 11, 3, text, n, q, sep=' ')
        end if
        !
        ! -- unsaturated evapotranspiration
        if (this%ietflag /= 0) then
          q = this%uzet(n)
          if (q > DZERO) then
            qout = qout + q
            q = -q
          end if
          call UWWORD(line, iloc, 11, 3, text, n, q, sep=' ')
        end if
        !
        ! -- groundwater recharge
        q = this%rch(n)
        if (q > DZERO) then
          qout = qout + q
          q = -q
        end if
        call UWWORD(line, iloc, 11, 3, text, n, q, sep=' ')
        !
        ! -- uzf below
        if (this%iuzf2uzf == 1) then
          q = DZERO
          this%uzfobj => this%elements(n)
          ivertflag = this%uzfobj%ivertcon
          if ( ivertflag > 0 ) then
              q = this%uzfobj%surfluxbelow * this%uzfobj%uzfarea
              if (q > DZERO) then
                qout = qout + q
                q = -q
              end if
          end if
          call UWWORD(line, iloc, 11, 3, text, n, q, sep=' ')
        end if
        !
        ! -- storage
        q = this%qsto(n)
        if (q > DZERO) then
          qout = qout + q
        else
          qin = qin - q
        end if
        if (q /= DZERO) then
          q = -q
        end if
        call UWWORD(line, iloc, 11, 3, text, n, q, sep=' ')
        !
        ! -- calculate error
        qerr = qin - qout
        call UWWORD(line, iloc, 11, 3, text, n, qerr, sep=' ')
        !
        ! -- calculate percent difference
        qavg = DHALF * (qin + qout)
        if (qavg > DZERO) then
        end if
        qpd = DZERO
        if (qavg > DZERO) then
          qpd = DHUNDRED * qerr / qavg
        end if
        call UWWORD(line, iloc, 11, 3, text, n, qpd)
        !
        ! -- write line
        write(iout, '(1X,A)') line(1:iloc)
      end do
    end if
    !
    ! -- Output uzf budget
    call this%budget%budget_ot(kstp, kper, iout)
    !
    ! -- return
    return
  end subroutine uzf_ot

  subroutine uzf_solve(this)
! ******************************************************************************
! uzf_solve -- Formulate the HCOF and RHS terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only : delt
    ! -- dummy
    class(UzfType) :: this
    ! -- locals
    integer(I4B) :: i, ivertflag
    integer(I4B) :: n, m, ierr
    real(DP) :: trhs1, thcof1, trhs2, thcof2
    real(DP) :: hgwf, hgwflm1, cvv, uzderiv, gwet, derivgwet
    real(DP) :: qfrommvr, qformvr,sumaet
    character(len=100) :: msg
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    this%uzfobjwork => this%elements(this%nodes+1)
    ierr = 0
    sumaet = DZERO
    !
    ! -- Calculate hcof and rhs for each UZF entry
    do i = 1, this%nodes
      thcof1 = DZERO
      thcof2 = DZERO
      trhs1 = DZERO
      trhs2 = DZERO
      uzderiv = DZERO
      gwet = DZERO
      derivgwet = DZERO
      this%uzfobj => this%elements(i)
      ivertflag = this%uzfobj%ivertcon
      !
      ! Create pointer to object below
      if ( ivertflag > 0 ) then
        this%uzfobjbelow => this%elements(ivertflag)
      else
        ! -- point to i so not null.  Does not use in this case.
        this%uzfobjbelow => this%elements(i)
      end if
      !
      n = this%nodelist(i)
      if ( this%ibound(n) > 0 ) then
        !
        ! -- Water mover added to infiltration
        qfrommvr = DZERO
        qformvr = DZERO
        if(this%imover == 1) then
          qfrommvr = this%pakmvrobj%get_qfrommvr(i)
        endif
        !
        ! -- zero out hcof and rhs
        this%hcof(i) = DZERO
        this%rhs(i) = DZERO
        !
        hgwf = this%xnew(n)
        !
        m = n
        hgwflm1 = hgwf
        cvv = DZERO
        !
        ! -- solve for current uzf cell
        call this%uzfobj%formulate(this%uzfobjwork,this%uzfobjbelow,i,         &
                                    this%totfluxtot, this%ietflag,             &
                                    this%issflag,this%iseepflag,               &
                                    trhs1,thcof1,hgwf,hgwflm1,cvv,uzderiv,     &
                                    qfrommvr,qformvr,ierr,sumaet,ivertflag)
        if ( ierr > 0 ) then
            if ( ierr == 1 ) &
              msg = 'Error: UZF variable NWAVSETS needs to be increased '
            call store_error(msg)
            call ustop()
        end if
        if ( this%igwetflag > 0 )                                              &
          call this%uzfobj%simgwet(this%igwetflag,i,hgwf,trhs2,thcof2,gwet,    &
                                    derivgwet)
        this%deriv(i) = uzderiv + derivgwet
        !
        ! -- save current rejected infiltration, groundwater recharge, and
        !    groundwater discharge
        this%rejinf(i) = this%uzfobj%finf_rej * this%uzfobj%uzfarea
        this%rch(i) = this%uzfobj%totflux * this%uzfobj%uzfarea / delt
        this%gwd(i) = this%uzfobj%surfseep
        !
        ! -- add to hcof and rhs
        this%hcof(i) = thcof1 + thcof2
        this%rhs(i) = -trhs1 - trhs2
        !
        ! -- add spring discharge and rejected infiltration to mover
        if(this%imover == 1) then
          call this%pakmvrobj%accumulate_qformvr(i, qformvr)
        endif
      !
      end if
    end do
  end subroutine uzf_solve

!  subroutine uzf_bdsav(this, icbcfl, icbcun)
!! ******************************************************************************
!! uzf_bdsav -- Save budget terms
!! ******************************************************************************
!!
!!    SPECIFICATIONS:
!! ------------------------------------------------------------------------------
!    ! -- dummy
!    class(UzfType) :: this
!    integer(I4B), intent(in) :: icbcfl
!    integer(I4B), intent(in) :: icbcun
!    ! -- local
!    integer(I4B) :: ibinun
!    character(len=16), dimension(4) :: aname
!    integer(I4B) :: iprint, nvaluesp, nwidthp
!    character(len=1) :: cdatafmp=' ', editdesc=' '
!    real(DP) :: dinact
!    ! -- data
!    data aname(1) /'UZF INFILTRATION'/
!    data aname(2) /'    UZF RECHARGE'/
!    data aname(3) /' UZF GWDISCHARGE'/
!    data aname(4) /'        UZF GWET'/
!! ------------------------------------------------------------------------------
!    !
!    ! -- Set unit number for binary output
!    if(this%ipakcb < 0) then
!      ibinun = icbcun
!    elseif(this%ipakcb == 0) then
!      ibinun = 0
!    else
!      ibinun = this%ipakcb
!    endif
!    if(icbcfl == 0) ibinun = 0
!    !
!    ! -- Record the recharge/discharge rates if requested
!    ! -- langevin: this doesn't work.  We cannot use recordarray, because
!    !    it assumes that the array is of size dis%nodes.  In UZF, this%nodes
!    !    does not equal dis%nodes.  So we need to write each one of these
!    !    arrays (recharge, discharge, gwet) as a list.  For now, am commenting
!    !    out this section by hardwiring ibinun = 0.
!    ibinun = 0
!    if(ibinun /= 0) then
!      iprint = 0
!      dinact = DZERO
!      !
!      !! -- infiltration
!      !call this%dis%record_array(this%infiltration, this%iout, iprint,     &
!      !                               -ibinun, aname(1), cdatafmp, nvaluesp,    &
!      !                               nwidthp, editdesc, dinact)
!      !!
!      !! -- recharge
!      !call this%dis%record_array(this%recharge, this%iout, iprint,         &
!      !                               -ibinun, aname(2), cdatafmp, nvaluesp,    &
!      !                               nwidthp, editdesc, dinact)
!      !!
!      !! -- gw discharge
!      !call this%dis%record_array(this%rejinf, this%iout, iprint,        &
!      !                               -ibinun, aname(3), cdatafmp, nvaluesp,    &
!      !                               nwidthp, editdesc, dinact)
!      !!
!      !! -- gw ET
!      !call this%dis%record_array(this%gwet, this%iout, iprint,             &
!      !                               -ibinun, aname(4), cdatafmp, nvaluesp,    &
!      !                               nwidthp, editdesc, dinact)
!    endif
!    !
!    ! -- Return
!    return
!  end subroutine uzf_bdsav

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(UzfType), intent(inout) :: this
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

   subroutine findcellabove(this,n,nml)
    class(UzfType) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(inout) :: nml
    integer(I4B) :: m, ipos
! ------------------------------------------------------------------------------
!
    ! -- return nml = n if no cell is above it
    nml = n
    do ipos = this%dis%con%ia(n)+1, this%dis%con%ia(n+1)-1
      m = this%dis%con%ja(ipos)
      if(this%dis%con%ihc(ipos) /= 0) then
        if (n < m) then
          ! -- m is beneath n
        else
          nml = m  ! -- m is above n
          exit
        endif
      end if
    enddo
    return
   end subroutine findcellabove

   subroutine read_cell_properties(this)
! ******************************************************************************
! read_cell_properties -- Read UZF cell properties and set them for 
!                         UzfKinematic type.
! ******************************************************************************
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error, count_errors
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, cellid
    integer(I4B) :: ierr
    integer(I4B) :: i, n
    integer(I4B) :: j
    integer(I4B) :: ic
    integer(I4B) :: jcol
    logical :: isfound, endOfBlock
    integer(I4B) :: landflag
    integer(I4B) :: ivertcon
    real(DP) :: surfdep, vks, thtr, thts, thti, eps, hgwf
    integer(I4B), dimension(:), allocatable :: rowmaxnnz
    type(sparsematrix) :: sparse
    integer(I4B), dimension(:), allocatable :: nboundchk
! ------------------------------------------------------------------------------
!
    !
    ! -- allocate space for node counter and initilize
    allocate(rowmaxnnz(this%dis%nodes))
    do n = 1, this%dis%nodes
      rowmaxnnz(n) = 0
    end do
    !
    ! -- allocate space for local variables
    allocate(nboundchk(this%nodes))
    do n = 1, this%nodes
      nboundchk(n) = 0
    end do
    !
    ! -- initialize variables
    landflag = 0
    ivertcon = 0
    surfdep = DZERO
    vks = DZERO
    thtr = DZERO
    thts = DZERO
    thti = DZERO
    eps = DZERO
    hgwf = DZERO
    !
    ! -- get uzf properties block
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr, supportOpenClose=.true.)
    !
    ! -- parse locations block if detected
    if (isfound) then
      write(this%iout,'(/1x,3a)') 'PROCESSING ', trim(adjustl(this%text)),      &
                                  ' PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        !
        ! -- get uzf cell number
        i = this%parser%GetInteger()
    
        if (i < 1 .or. i > this%nodes) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. iuzno MUST BE > 0 and <= ', this%nodes
          call store_error(errmsg)
          cycle
        end if 
        !
        ! -- increment nboundchk
        nboundchk(i) = nboundchk(i) + 1
        
        ! -- store the reduced gwf nodenumber in mfcellid
        call this%parser%GetCellid(this%dis%ndim, cellid)
        ic = this%dis%noder_from_cellid(cellid,                                 &
                                        this%parser%iuactive, this%iout)
        this%mfcellid(i) = ic
        rowmaxnnz(ic) = rowmaxnnz(ic) + 1
        !
        ! -- landflag
        landflag = this%parser%GetInteger()
        if (landflag < 0 .OR. landflag > 1) then
          write(errmsg,'(4x,a,1x,i0,1x,a,1x,i0)') &
            '****ERROR. LANDFLAG FOR UZF CELL', i,                              &
            'MUST BE 0 or 1 - SPECIFIED VALUE =', landflag
          call store_error(errmsg)
        end if
        !
        ! -- ivertcon
        ivertcon = this%parser%GetInteger()
        if (ivertcon < 0 .OR. ivertcon > this%nodes) then
          write(errmsg,'(4x,a,1x,i0,1x,a,1x,i0)')                                          &
            '****ERROR. IVERTCON FOR UZF CELL', i,                                         &
            'MUST BE 0 or less than NUZFCELLS - SPECIFIED VALUE =', ivertcon
          call store_error(errmsg)
          ivertcon = 0
        end if
        !
        ! -- surfdep
        surfdep =  this%parser%GetDouble()
        if (surfdep <= DZERO) then   !need to check for cell thickness
          write(errmsg,'(4x,a,1x,i0,1x,a,1x,g0)')                                          &
            '****ERROR. SURFDEP FOR UZF CELL', i,                                          &
             'MUST BE > 0 - SPECIFIED VALUE =', surfdep
          call store_error(errmsg)
          surfdep = DZERO
        end if
        !
        ! -- vks
        vks = this%parser%GetDouble()
        if (vks <= DZERO) then
          write(errmsg,'(4x,a,1x,i0,1x,a,1x,g0)')                                          &
            '****ERROR. VKS FOR UZF CELL', i,                                              &
             'MUST BE > 0 - SPECIFIED VALUE =', vks
          call store_error(errmsg)
          vks = DONE
        end if
        !
        ! -- thtr
        thtr = this%parser%GetDouble()
        if (thtr <= DZERO) then
          write(errmsg,'(4x,a,1x,i0,1x,a,1x,g0)')                                          &
            '****ERROR. THTR FOR UZF CELL', i,                                             &
             'MUST BE > 0 - SPECIFIED VALUE =', thtr
          call store_error(errmsg)
          thtr = 0.1
        end if
        !
        ! -- thts
        thts = this%parser%GetDouble()
        if (thts <= thtr) then
          write(errmsg,'(4x,a,1x,i0,1x,a,1x,g0)')                                          &
            '****ERROR. THTS FOR UZF CELL', i,                                             &
             'MUST BE > THTR - SPECIFIED VALUE =', thts
          call store_error(errmsg)
          thts = 0.2
        end if
        !
        ! -- thti
        thti = this%parser%GetDouble()
        if (thti < thtr .OR. thti > thts) then
          write(errmsg,'(4x,a,1x,i0,1x,a,1x,g0)')                                          &
            '****ERROR. THTI FOR UZF CELL', i,                                             &
             'MUST BE >= THTR AND < THTS - SPECIFIED VALUE =', thti
          call store_error(errmsg)
          thti = 0.1
        end if
        !
        ! -- eps
        eps = this%parser%GetDouble()
        if (eps < 3.5 .OR. eps > 14) then
          write(errmsg,'(4x,a,1x,i0,1x,a,1x,g0)')                                          &
            '****ERROR. EPSILON FOR UZF CELL', i,                                          &
             'MUST BE BETWEEN 3.5 and 14.0 - SPECIFIED VALUE =', eps
          call store_error(errmsg)
          eps = 3.5
        end if
        !
        ! -- boundname
        if (this%inamedbound == 1) then
          call this%parser%GetStringCaps(this%boundname(i))
        endif
        n = this%mfcellid(i)
        this%nodelist(i) = n
        hgwf = this%xnew(n)
        this%uzfobj => this%elements(i)
        call this%uzfobj%setdata(i,this%gwfarea(n),this%gwftop(n),this%gwfbot(n), &
                                 surfdep,vks,thtr,thts,thti,eps,this%ntrail,      &
                                 landflag,ivertcon,hgwf)
        if (ivertcon > 0) then
          this%iuzf2uzf = 1
        end if
       !
      end do
    else
      call store_error('ERROR.  REQUIRED PACKAGEDATA BLOCK NOT FOUND.')
    end if
    !
    ! -- check for duplicate or missing uzf cells
    do i = 1, this%nodes
      if (nboundchk(i) == 0) then
        write(errmsg,'(a,1x,i0)')                                             &
          'ERROR.  NO DATA SPECIFIED FOR UZF CELL', i
        call store_error(errmsg)
      else if (nboundchk(i) > 1) then
        write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                             &
          'ERROR.  DATA FOR UZF CELL', i, 'SPECIFIED', nboundchk(i), 'TIMES'
        call store_error(errmsg)
      end if
    end do
    !
    ! -- write summary of UZF cell property error messages
    ierr = count_errors()
    if (ierr > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! --Initialize one more uzf object, which is used as a worker
    i = this%nodes + 1
    this%uzfobj => this%elements(i)
    n = this%mfcellid(i-1)
    hgwf = this%xnew(n)
    landflag = 0
    ivertcon = 0
    call this%uzfobj%setdata(i,this%gwfarea(n),this%gwftop(n),this%gwfbot(n), &
                             surfdep,vks,thtr,thts,thti,eps,this%ntrail,      &
                             landflag,ivertcon,hgwf)
    !
    ! -- setup sparse for connectivity used to identify multiple uzf cells per
    !    GWF model cell
    call sparse%init(this%dis%nodes, this%dis%nodes, rowmaxnnz)
    ! --
    do i = 1, this%nodes
      ic = this%mfcellid(i)
      call sparse%addconnection(ic, i, 1)
    end do
    !
    ! -- create ia and ja from sparse
    call sparse%filliaja(this%ia,this%ja,ierr)
    !
    ! -- set imaxcellcnt
    do i = 1, this%nodes
      jcol = 0
      do j = this%ia(i), this%ia(i+1) - 1
        jcol = jcol + 1
      end do
      if (jcol > this%imaxcellcnt) then
        this%imaxcellcnt = jcol
      end if
    end do
    !
    ! -- do an initial evaluation of the sum of uzfarea relative to the
    !    GWF cell area in the case that there is more than one UZF cell
    !    in a GWF cell and a auxmult value is not being applied to the
    !    calculate the UZF cell area from the GWF cell area.
    if (this%imaxcellcnt > 1 .and. this%iauxmultcol < 1) then
      call this%check_cell_area()
    end if
    !
    ! -- deallocate local variables
    deallocate(rowmaxnnz)
    deallocate(nboundchk)
    !
    ! -- return
    return
  end subroutine read_cell_properties

  subroutine print_cell_properties(this)
! ******************************************************************************
! print_cell_properties -- Read UZF cell properties and set them for 
!                          UzfKinematic type.
! ******************************************************************************
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType), intent(inout) :: this
    ! -- local
    character (len=20) :: cellids, cellid
    character(len=LINELENGTH) :: line, linesep
    character(len=16) :: text
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: node
    integer(I4B) :: iloc
    real(DP) :: q
! ------------------------------------------------------------------------------
!
    !
    ! -- set cell id based on discretization
    if (this%dis%ndim == 3) then
      cellids = '(LAYER,ROW,COLUMN)  '
    elseif (this%dis%ndim == 2) then
      cellids = '(LAYER,CELL2D)      '
    else
      cellids = '(NODE)              '
    end if
    write (this%iout, '(//3a)')                                                 &
      'UZF PACKAGE (', trim(adjustl(this%name)), ') CELL DATA'
    iloc = 1
    line = ''
    if(this%inamedbound==1) then
      call UWWORD(line, iloc, 16, 1, 'name', n, q, left=.TRUE.)
    end if
    call UWWORD(line, iloc, 6, 1, 'no.', n, q, CENTER=.TRUE., sep=' ')
    call UWWORD(line, iloc, 20, 1, cellids, n, q, CENTER=.TRUE., sep=' ')
    call UWWORD(line, iloc, 11, 1, 'landflag', n, q, CENTER=.TRUE., sep=' ')
    call UWWORD(line, iloc, 11, 1, 'ivertcon', n, q, CENTER=.TRUE., sep=' ')
    call UWWORD(line, iloc, 11, 1, 'surfdep', n, q, CENTER=.TRUE., sep=' ')
    call UWWORD(line, iloc, 11, 1, 'vks', n, q, CENTER=.TRUE., sep=' ')
    call UWWORD(line, iloc, 11, 1, 'thtr', n, q, CENTER=.TRUE., sep=' ')
    call UWWORD(line, iloc, 11, 1, 'thts', n, q, CENTER=.TRUE., sep=' ')
    call UWWORD(line, iloc, 11, 1, 'thti', n, q, CENTER=.TRUE., sep=' ')
    call UWWORD(line, iloc, 11, 1, 'eps', n, q, CENTER=.TRUE.)
    ! -- create line separator
    linesep = repeat('-', iloc)
    ! -- write header line and separator
    write(this%iout,'(1X,A)') line(1:iloc)
    write(this%iout,'(1X,A)') linesep(1:iloc)
    !
    ! -- write data for each cell
    do i = 1, this%nodes
      !
      ! -- Initialize variables
      this%uzfobj => this%elements(i)
      !
      ! -- get cellid
      node = this%mfcellid(i)
      if (node > 0) then
        call this%dis%noder_to_string(node, cellid)
      else
        cellid = 'none'
      end if
      !
      ! -- fill line
      iloc = 1
      line = ''
      if(this%inamedbound==1) then
        call UWWORD(line, iloc, 16, 1, this%boundname(i), n, q, left=.TRUE.)
      end if
      call UWWORD(line, iloc, 6, 2, text, i, q, sep=' ')
      call UWWORD(line, iloc, 20, 1, cellid, n, q, left=.TRUE.)
      call UWWORD(line, iloc, 11, 2, text, this%uzfobj%landflag, q, sep=' ')
      call UWWORD(line, iloc, 11, 2, text, this%uzfobj%ivertcon, q, sep=' ')
      call UWWORD(line, iloc, 11, 3, text, i, this%uzfobj%surfdep, sep=' ')
      call UWWORD(line, iloc, 11, 3, text, i, this%uzfobj%vks, sep=' ')
      call UWWORD(line, iloc, 11, 3, text, i, this%uzfobj%thtr, sep=' ')
      call UWWORD(line, iloc, 11, 3, text, i, this%uzfobj%thts, sep=' ')
      call UWWORD(line, iloc, 11, 3, text, i, this%uzfobj%thti, sep=' ')
      call UWWORD(line, iloc, 11, 3, text, i, this%uzfobj%eps)
      ! -- write line
      write(this%iout,'(1X,A)') line(1:iloc)
    end do
    !
    ! -- write separator
    write(this%iout,'(1X,A)') linesep(1:iloc)

    !
    ! -- return
    return
  end subroutine print_cell_properties

   subroutine check_cell_area(this)
! ******************************************************************************
! check_cell_area -- Check UZF cell areas.
! ******************************************************************************
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error, count_errors
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=16) :: cuzf
    character(len=20) :: cellid
    character(len=LINELENGTH) :: cuzfcells
    integer(I4B) :: ierr
    integer(I4B) :: i
    integer(I4B) :: i2
    integer(I4B) :: j
    integer(I4B) :: n
    integer(I4B) :: i0
    integer(I4B) :: i1
    real(DP) :: area
    real(DP) :: area2
    real(DP) :: sumarea
    real(DP) :: cellarea
    real(DP) :: d
! ------------------------------------------------------------------------------
!
    !
    ! -- check that the area of vertically connected uzf cells is the equal
    do i = 1, this%nodes
      !
      ! -- Initialize variables
      this%uzfobj => this%elements(i)
      i2 = this%uzfobj%ivertcon
      area = this%uzfobj%uzfarea
      !
      ! Create pointer to object below
      if ( i2 > 0 ) then
        this%uzfobjbelow => this%elements(i2)
        area2 = this%uzfobjbelow%uzfarea
        d = abs(area - area2)
        if (d > DEM6) then
          write(errmsg,'(4x,2(a,1x,g15.7,1x,a,1x,i6,1x))')                    &
            '****ERROR. UZF CELL AREA (', area, ') FOR CELL ', i,             &
            'DOES NOT EQUAL UZF CELL AREA (', area2, ') FOR CELL ', i2
          call store_error(errmsg)
        end if
      end if
    end do
    !
    ! -- check that the area of uzf cells in a GWF cell is less than or equal
    !    to the GWF cell area
    do n = 1, this%dis%nodes
      i0 = this%ia(n)
      i1 = this%ia(n+1)
      ! -- skip gwf cells with no UZF cells
      if ((i1 - i0) < 1) cycle
      sumarea = DZERO
      cellarea = DZERO
      cuzfcells = ''
      do j = i0, i1 - 1
        i = this%ja(j)
        write(cuzf,'(i0)') i
        cuzfcells = trim(adjustl(cuzfcells)) // ' ' // trim(adjustl(cuzf))
        this%uzfobj => this%elements(i)
        sumarea = sumarea + this%uzfobj%uzfarea
        cellarea = this%uzfobj%cellarea
      end do
      ! -- calculate the difference between the sum of UZF areas and GWF cell area
      d = abs(sumarea - cellarea)
      if (d > DEM6) then
        call this%dis%noder_to_string(n, cellid)
        write(errmsg,'(4x,a,1x,g15.7,1x,a,1x,g15.7,1x,a,1x,a,1x,a,a)')      &
          '****ERROR. TOTAL UZF CELL AREA (', sumarea,                      &
          ') EXCEEDS THE GWF CELL AREA (', cellarea, ') OF CELL', cellid,   &
          'WHICH INCLUDES UZF CELL(S): ', trim(adjustl(cuzfcells))
        call store_error(errmsg)
      end if
    end do
    !
    ! -- terminate if errors were encountered
    ierr = count_errors()
    if (ierr > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    ! -- return
    return
  end subroutine check_cell_area

  subroutine uzcelloutput(this, isuppress_output)
! ******************************************************************************
! write out cell by cell saturation.
! region beneath water table is considered 100% saturated.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(UzfType) :: this
    type(UzfKinematicType), pointer :: uzfobj
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    do i = 1, this%nodes
        uzfobj => this%elements(i)
    end do
  end subroutine uzcelloutput

  ! -- Procedures related to observations (type-bound)
  logical function uzf_obs_supported(this)
! ******************************************************************************
! uzf_obs_supported
!   -- Return true because uzf package supports observations.
!   -- Overrides BndType%bnd_obs_supported
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(UzfType) :: this
! ------------------------------------------------------------------------------
    uzf_obs_supported = .true.
    return
  end function uzf_obs_supported

  subroutine uzf_df_obs(this)
! ******************************************************************************
! uzf_df_obs (implements bnd_df_obs)
!   -- Store observation type supported by uzf package.
!   -- Overrides BndType%bnd_df_obs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType) :: this
    ! -- local
    integer(I4B) :: indx
  ! ------------------------------------------------------------------------------
    !
    ! -- Store obs type and assign procedure pointer
    !
    !    for recharge observation type.
    call this%obs%StoreObsType('uzf-gwrch', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => uzf_process_obsID
    !
    !    for discharge observation type.
    call this%obs%StoreObsType('uzf-gwd', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => uzf_process_obsID
    !
    !    for discharge observation type.
    call this%obs%StoreObsType('uzf-gwd-to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => uzf_process_obsID
    !
    !    for gwet observation type.
    call this%obs%StoreObsType('uzf-gwet', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => uzf_process_obsID
    !
    !    for infiltration observation type.
    call this%obs%StoreObsType('infiltration', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => uzf_process_obsID
    !
    !    for from mover observation type.
    call this%obs%StoreObsType('from-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => uzf_process_obsID
    !
    !    for rejected infiltration observation type.
    call this%obs%StoreObsType('rej-inf', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => uzf_process_obsID
    !
    !    for rejected infiltration to mover observation type.
    call this%obs%StoreObsType('rej-inf-to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => uzf_process_obsID
    !
    !    for uzet observation type.
    call this%obs%StoreObsType('uzet', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => uzf_process_obsID
    !
    !    for storage observation type.
    call this%obs%StoreObsType('storage', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => uzf_process_obsID
    !
    !    for net infiltration observation type.
    call this%obs%StoreObsType('net-infiltration', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => uzf_process_obsID
    !
    !    for water-content observation type.
    call this%obs%StoreObsType('water-content', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => uzf_process_obsID
    !
    ! -- return
    return
  end subroutine uzf_df_obs
!
  subroutine uzf_bd_obs(this)
    ! **************************************************************************
    ! uzf_bd_obs
    !   -- Calculate observations this time step and call
    !      ObsType%SaveOneSimval for each UzfType observation.
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    ! -- dummy
    class(UzfType) :: this
    ! -- local
    integer(I4B) :: i, ii, n, nn
    real(DP) :: v
    character(len=100) :: msg
    type(ObserveType), pointer :: obsrv => null()
    !---------------------------------------------------------------------------
    !
    ! Write simulated values for all uzf observations
    if (this%obs%npakobs>0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        nn = size(obsrv%indxbnds)
        do ii = 1, nn
          n = obsrv%indxbnds(ii)
          v = DNODATA
          select case (obsrv%ObsTypeId)
            case ('UZF-GWRCH')
              v = this%rch(n)
            case ('UZF-GWD')
              v = this%gwd(n)
              if (v > DZERO) then
                v = -v
              end if
            case ('UZF-GWD-TO-MVR')
              if (this%imover == 1) then
                v = this%gwdtomvr(n)
                if (v > DZERO) then
                  v = -v
                end if
              end if
            case ('UZF-GWET')
              if (this%igwetflag > 0) then
                v = this%gwet(n)
                if (v > DZERO) then
                  v = -v
                end if
              end if
            case ('INFILTRATION')
              v = this%appliedinf(n)
            case ('FROM-MVR')
              if (this%imover == 1) then
                v = this%pakmvrobj%get_qfrommvr(n)
              end if
            case ('REJ-INF')
              v = this%rejinf(n)
              if (v > DZERO) then
                v = -v
              end if
            case ('REJ-INF-TO-MVR')
              if (this%imover == 1) then
                v = this%rejinftomvr(n)
                if (v > DZERO) then
                  v = -v
                end if
              end if
            case ('UZET')
              if (this%ietflag /= 0) then
                v = this%uzet(n)
                if (v > DZERO) then
                  v = -v
                end if
              end if
            case ('STORAGE')
              v = -this%qsto(n)
            case ('NET-INFILTRATION')
              this%uzfobj => this%elements(n)
              v = this%infiltration(n)
            case ('WATER-CONTENT')
              v = this%obs_theta(i)  ! more than one obs per node
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
  end subroutine uzf_bd_obs
!
  subroutine uzf_rp_obs(this)
    ! -- dummy
    class(UzfType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, j, n, nn
    real(DP) :: obsdepth
    real(DP) :: dmax
    character(len=200) :: ermsg
    character(len=LENBOUNDNAME) :: bname
    class(ObserveType),   pointer :: obsrv => null()
    ! --------------------------------------------------------------------------
    ! -- formats
60  format('Error: Invalid node number in OBS input: ',i5)
70  format('Error: Invalid depth in OBS input: ',g15.7)
    !
    do i = 1, this%obs%npakobs
      obsrv => this%obs%pakobs(i)%obsrv
      ! -- indxbnds needs to be deallocated and reallocated (using
      !    ExpandArray) each stress period because list of boundaries
      !    can change each stress period.
      if (allocated(obsrv%indxbnds)) then
        deallocate(obsrv%indxbnds)
      endif
      !
      ! -- get node number 1
      nn = obsrv%NodeNumber
      if (nn == NAMEDBOUNDFLAG) then
        bname = obsrv%FeatureName
        ! -- Observation location(s) is(are) based on a boundary name.
        !    Iterate through all boundaries to identify and store
        !    corresponding index(indices) in bound array.
        do j = 1, this%nodes
          if (this%boundname(j) == bname) then
            !! In UZF, use of the same boundary name for multiple boundaries
            !! in an observation is not supported for obs type UZF-WATERCONTENT
            !if (obsrv%ObsTypeId=='WATER-CONTENT') then
            !  if (obsrv%BndFound) then
            !    ermsg = 'Duplicate names for multiple boundaries are not ' // &
            !            'supported for UZF observations of type ' // &
            !            '"UZF-WATERCONTENT". There are multiple' // &
            !            ' boundaries named "' // trim(bname) // &
            !            '" for observation: ' // &
            !            trim(obsrv%Name) // '.'
            !    call store_error(ermsg)
            !    call store_error_unit(this%inunit)
            !    call ustop()
            !  endif
            !endif
            obsrv%BndFound = .true.
            obsrv%CurrentTimeStepEndValue = DZERO
            call ExpandArray(obsrv%indxbnds)
            n = size(obsrv%indxbnds)
            obsrv%indxbnds(n) = j
            if (n==1) then
              ! Define intPak1 so that obs_theta is stored (for first uzf
              ! cell if multiple cells share the same boundname).
              obsrv%intPak1 = j
            endif
          endif
        enddo
      else
        ! -- get node number
        nn = obsrv%NodeNumber
        ! -- put nn (a value meaningful only to UZF) in intPak1
        obsrv%intPak1 = nn
        ! -- check that node number is valid; call store_error if not
        if (nn < 1 .or. nn > this%nodes) then
          write (ermsg, 60) nn
          call store_error(ermsg)
        else
          obsrv%BndFound = .true.
        endif
        obsrv%CurrentTimeStepEndValue = DZERO
        call ExpandArray(obsrv%indxbnds)
        n = size(obsrv%indxbnds)
        obsrv%indxbnds(n) = nn
      end if
      !
      ! -- catch non-cumulative observation assigned to observation defined
      !    by a boundname that is assigned to more than one element
      if (obsrv%ObsTypeId == 'WATER-CONTENT') then
        n = size(obsrv%indxbnds)
        if (n > 1) then
          write (ermsg, '(4x,a,4(1x,a))') &
            'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
            'for observation', trim(adjustl(obsrv%Name)), &
            ' must be assigned to a UZF cell with a unique boundname.'
          call store_error(ermsg)
        end if
        !
        ! -- check WATER-CONTENT depth
        obsdepth = obsrv%Obsdepth
        ! -- put obsdepth (a value meaningful only to UZF) in dblPak1
        obsrv%dblPak1 = obsdepth
        !
        ! -- determine maximum cell depth
        this%uzfobj => this%elements(n)
        dmax = this%uzfobj%celtop - this%uzfobj%celbot
        ! -- check that obs depth is valid; call store_error if not
        ! -- need to think about a way to put bounds on this depth
        if (obsdepth < DZERO .or. obsdepth > dmax) then
          write (ermsg, '(4x,a,4(1x,a),1x,g15.7,1x,a,1x,g15.7)') &
            'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
            'for observation', trim(adjustl(obsrv%Name)), &
            ' specified depth (', obsdepth, ') must be between 0. and ', dmax
          call store_error(ermsg)
        endif
      else
        do j = 1, size(obsrv%indxbnds)
          nn =  obsrv%indxbnds(j)
          if (nn < 1 .or. nn > this%maxbound) then
            write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              ' uzfno must be > 0 and <=', this%maxbound, &
              '(specified value is ', nn, ')'
            call store_error(ermsg)
          end if
        end do
      end if
  !    !
  !    select case (obsrv%ObsTypeId)
  !      case ('WATER-CONTENT')
  !        obsdepth = obsrv%Obsdepth
  !        ! -- put obsdepth (a value meaningful only to UZF) in dblPak1
  !        obsrv%dblPak1 = obsdepth
  !        ! -- check that obs depth is valid; call store_error if not
  !        ! -- need to think about a way to put bounds on this depth
  !        if (obsdepth < -999999.d0 .or. obsdepth > 999999.d0) then
  !          write (ermsg, 70) obsdepth
  !          call store_error(ermsg)
  !        endif
  !      case default
  !! left to check other types of observations
  !    end select
    end do
    if (count_errors() > 0) then
      call store_error_unit(this%inunit)
      call ustop()
    endif
    !
    return
  end subroutine uzf_rp_obs
  !
  ! -- Procedures related to observations (NOT type-bound)
  subroutine uzf_process_obsID(obsrv, dis, inunitobs, iout)
    ! -- This procedure is pointed to by ObsDataType%ProcesssIdPtr. It processes
    !    the ID string of an observation definition for UZF-package observations.
    ! -- dummy
    type(ObserveType),      intent(inout) :: obsrv
    class(DisBaseType), intent(in)    :: dis
    integer(I4B),            intent(in)    :: inunitobs
    integer(I4B),            intent(in)    :: iout
    ! -- local
    integer(I4B) :: n, nn
    real(DP) :: obsdepth
    integer(I4B) :: icol, istart, istop, istat
    real(DP) :: r
    character(len=LINELENGTH) :: strng
    ! formats
 30 format(i10)
    !
    strng = obsrv%IDstring
    ! -- Extract node number from strng and store it.
    !    If 1st item is not an integer(I4B), it should be a
    !    feature name--deal with it.
    icol = 1
    ! -- get node number
    call urword(strng, icol, istart, istop, 1, n, r, iout, inunitobs)
    read (strng(istart:istop), 30, iostat=istat) nn
    if (istat==0) then
      ! -- store uzf node number (NodeNumber)
      obsrv%NodeNumber = nn
    else
      ! Integer can't be read from strng; it's presumed to be a boundary
      ! name (already converted to uppercase)
      obsrv%FeatureName = strng(istart:istop)
      ! -- Observation may require summing rates from multiple boundaries,
      !    so assign NodeNumber as a value that indicates observation
      !    is for a named boundary or group of boundaries.
      obsrv%NodeNumber = NAMEDBOUNDFLAG
    endif
    !
    ! -- for soil water observation, store depth
    if (obsrv%ObsTypeId=='WATER-CONTENT' ) then
      call urword(strng, icol, istart, istop, 3, n, r, iout, inunitobs)
      obsdepth = r
      ! -- store observations depth
      obsrv%Obsdepth = obsdepth
    endif
    !
    return
    !
!300 continue
!    call store_error(ermsg)
!    call ustop()
  end subroutine uzf_process_obsID

  subroutine uzf_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules

    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(UzfType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- allocate uzf specific scalars
    call mem_allocate(this%iprwcont, 'IPRWCONT', this%origin)
    call mem_allocate(this%iwcontout, 'IWCONTOUT', this%origin)
    call mem_allocate(this%ibudgetout, 'IBUDGETOUT', this%origin)
    call mem_allocate(this%ntrail, 'NTRAIL', this%origin)
    call mem_allocate(this%nsets, 'NSETS', this%origin)
    call mem_allocate(this%nodes, 'NODES', this%origin)
    call mem_allocate(this%istocb, 'ISTOCB', this%origin)
    call mem_allocate(this%nwav, 'NWAV', this%origin)
    call mem_allocate(this%outunitbud, 'OUTUNITBUD', this%origin)
    call mem_allocate(this%totfluxtot, 'TOTFLUXTOT', this%origin)
    call mem_allocate(this%infilsum, 'INFILSUM', this%origin)
    call mem_allocate(this%uzetsum, 'UZETSUM', this%origin)
    call mem_allocate(this%rechsum, 'RECHSUM', this%origin)
    call mem_allocate(this%vfluxsum, 'VFLUXSUM', this%origin)
    call mem_allocate(this%delstorsum, 'DELSTORSUM', this%origin)
    call mem_allocate(this%bditems, 'BDITEMS', this%origin)
    call mem_allocate(this%nbdtxt, 'NBDTXT', this%origin)
    call mem_allocate(this%issflag, 'ISSFLAG', this%origin)
    call mem_allocate(this%issflagold, 'ISSFLAGOLD', this%origin)
    call mem_allocate(this%readflag, 'READFLAG', this%origin)
    call mem_allocate(this%iseepflag, 'ISEEPFLAG', this%origin)
    call mem_allocate(this%imaxcellcnt, 'IMAXCELLCNT', this%origin)
    call mem_allocate(this%ietflag, 'IETFLAG', this%origin)
    call mem_allocate(this%igwetflag, 'IGWETFLAG', this%origin)
    call mem_allocate(this%iuzf2uzf, 'IUZF2UZF', this%origin)
    call mem_allocate(this%cbcauxitems, 'CBCAUXITEMS', this%origin)

    call mem_allocate(this%iconvchk, 'ICONVCHK', this%origin)
    call mem_allocate(this%pdmax, 'PDMAX', this%origin)
    !
    ! -- initialize scalars
    this%iprwcont = 0
    this%iwcontout = 0
    this%ibudgetout = 0
    this%infilsum = DZERO
    this%uzetsum = DZERO
    this%rechsum = DZERO
    this%delstorsum = DZERO
    this%vfluxsum = DZERO
    this%istocb = 0
    this%bditems = 7
    this%nbdtxt = 5
    this%issflag = 0
    this%issflagold = 0
    this%ietflag = 0
    this%igwetflag = 0
    this%iseepflag = 0
    this%imaxcellcnt = 0
    this%iuzf2uzf = 0
    this%cbcauxitems = 1
    this%imover = 0
    !
    ! -- convergence check
    this%iconvchk = 1
    this%pdmax = DEM1
    !
    ! -- return
    return
  end subroutine uzf_allocate_scalars
!
  subroutine uzf_da(this)
! ******************************************************************************
! uzf_da -- Deallocate objects
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(UzfType) :: this
    ! -- locals
    integer (I4B) :: i
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- deallocate uzf objects
    do i = 1, this%nodes+1
        this%uzfobj => this%elements(i)
        call this%uzfobj%dealloc()
    end do
    nullify(this%uzfobj)
    nullify(this%uzfobjwork)
    nullify(this%uzfobjbelow)
    deallocate(this%elements)
    !
    ! -- budget object
    call this%budget%budget_da()
    deallocate(this%budget)
    !
    ! -- character arrays
    deallocate(this%bdtxt)
    deallocate(this%cauxcbc)
    !
    ! -- deallocate scalars
    call mem_deallocate(this%iprwcont)
    call mem_deallocate(this%iwcontout)
    call mem_deallocate(this%ibudgetout)
    call mem_deallocate(this%ntrail)
    call mem_deallocate(this%nsets)
    call mem_deallocate(this%nodes)
    call mem_deallocate(this%istocb)
    call mem_deallocate(this%nwav)
    call mem_deallocate(this%outunitbud)
    call mem_deallocate(this%totfluxtot)
    call mem_deallocate(this%infilsum)
    call mem_deallocate(this%uzetsum)
    call mem_deallocate(this%rechsum)
    call mem_deallocate(this%vfluxsum)
    call mem_deallocate(this%delstorsum)
    call mem_deallocate(this%bditems)
    call mem_deallocate(this%nbdtxt)
    call mem_deallocate(this%issflag)
    call mem_deallocate(this%issflagold)
    call mem_deallocate(this%readflag)
    call mem_deallocate(this%iseepflag)
    call mem_deallocate(this%imaxcellcnt)
    call mem_deallocate(this%ietflag)
    call mem_deallocate(this%igwetflag)
    call mem_deallocate(this%iuzf2uzf)
    call mem_deallocate(this%cbcauxitems)
    !
    ! -- convergence check
    call mem_deallocate(this%iconvchk)
    call mem_deallocate(this%pdmax)
    !
    ! -- deallocate arrays
    call mem_deallocate(this%mfcellid)
    call mem_deallocate(this%appliedinf)
    call mem_deallocate(this%rejinf)
    call mem_deallocate(this%rejinf0)
    call mem_deallocate(this%rejinftomvr)
    call mem_deallocate(this%infiltration)
    call mem_deallocate(this%recharge)
    call mem_deallocate(this%gwet)
    call mem_deallocate(this%uzet)
    call mem_deallocate(this%gwd)
    call mem_deallocate(this%gwd0)
    call mem_deallocate(this%gwdtomvr)
    call mem_deallocate(this%rch)
    call mem_deallocate(this%rch0)
    call mem_deallocate(this%qsto)
    call mem_deallocate(this%deriv)
    call mem_deallocate(this%qauxcbc)
    !
    ! -- deallocate integer arrays
    call mem_deallocate(this%ia)
    call mem_deallocate(this%ja)
    !
    ! -- deallocate timeseries aware variables
    call mem_deallocate(this%sinf)
    call mem_deallocate(this%pet)
    call mem_deallocate(this%extdp)
    call mem_deallocate(this%extwc)
    call mem_deallocate(this%ha)
    call mem_deallocate(this%hroot)
    call mem_deallocate(this%rootact)
    call mem_deallocate(this%lauxvar)
    !
    ! -- deallocate obs variables
    call mem_deallocate(this%obs_theta)
    call mem_deallocate(this%obs_depth)
    call mem_deallocate(this%obs_num)
    !
    ! -- Parent object
    call this%BndType%bnd_da()
    !
    ! -- Return
    return
  end subroutine uzf_da

end module UzfModule

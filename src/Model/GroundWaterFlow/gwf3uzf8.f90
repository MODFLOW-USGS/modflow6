! -- Uzf module
module UzfModule

  use KindModule, only: DP, I4B
  !use ArrayHandlersModule, only: ExpandArray
  use ConstantsModule, only: DZERO, DEM6, DEM4, DEM2, DEM1, DHALF,              &
                             DONE, DHUNDRED,                                    &
                             LINELENGTH, LENFTYPE, LENPACKAGENAME,              &
                             LENBOUNDNAME, LENBUDTXT, LENPAKLOC, DNODATA,       &
                             NAMEDBOUNDFLAG, MAXCHARLEN,                        &
                             DHNOFLO, DHDRY,                                    &
                             TABLEFT, TABCENTER, TABRIGHT,                      &
                             TABSTRING, TABUCSTRING, TABINTEGER, TABREAL
  use GenericUtilitiesModule, only: sim_message
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_setptr,      &
                                 mem_deallocate
  use MemoryHelperModule, only: create_mem_path
  use SparseModule, only: sparsematrix
  use BndModule, only: BndType
  use UzfCellGroupModule, only: UzfCellGroupType
  use BudgetObjectModule, only: BudgetObjectType, budgetobject_cr
  use BaseDisModule, only: DisBaseType
  use ObserveModule, only: ObserveType
  use ObsModule, only: ObsType
  use InputOutputModule, only: URWORD
  use SimVariablesModule, only: errmsg
  use SimModule, only: count_errors, store_error, ustop, store_error_unit
  use BlockParserModule, only: BlockParserType
  use TableModule, only: TableType, table_cr

  implicit none

  character(len=LENFTYPE)       :: ftype = 'UZF'
  character(len=LENPACKAGENAME) :: text  = '       UZF CELLS' 

  private
  public :: uzf_create
  public :: UzfType

  type, extends(BndType) :: UzfType
    ! output integers
    integer(I4B), pointer :: iprwcont => null()
    integer(I4B), pointer :: iwcontout => null()
    integer(I4B), pointer :: ibudgetout => null()
    integer(I4B), pointer :: ipakcsv => null()
    !
    type(BudgetObjectType), pointer                    :: budobj      => null()
    integer(I4B), pointer                              :: bditems     => null()  !number of budget items
    integer(I4B), pointer                              :: nbdtxt      => null()  !number of budget text items
    character(len=LENBUDTXT), dimension(:), pointer,                            &
                              contiguous               :: bdtxt       => null()  !budget items written to cbc file
    character(len=LENBOUNDNAME), dimension(:), pointer,                         &
                                 contiguous :: uzfname => null()
    !
    ! -- uzf table objects
    type(TableType), pointer                           :: pakcsvtab   => null()
    !
    ! -- uzf kinematic object
    type(UzfCellGroupType), pointer                    :: uzfobj      => null()
    type(UzfCellGroupType)                             :: uzfobjwork
    !
    ! -- pointer to gwf variables
    integer(I4B), pointer                                  :: gwfiss      => null()
    real(DP), dimension(:), pointer, contiguous            :: gwftop      => null()
    real(DP), dimension(:), pointer, contiguous            :: gwfbot      => null()
    real(DP), dimension(:), pointer, contiguous            :: gwfarea     => null()
    real(DP), dimension(:), pointer, contiguous            :: gwfhcond    => null()
    !
    ! -- uzf data
    integer(I4B), pointer                                   :: ntrail       => null()
    integer(I4B), pointer                                   :: nsets        => null()
    integer(I4B), pointer                                   :: nwav         => null()
    integer(I4B), pointer                                   :: nodes        => null()
    integer(I4B), pointer                                   :: nper         => null()
    integer(I4B), pointer                                   :: nstp         => null()
    integer(I4B), pointer                                   :: readflag     => null()
    integer(I4B), pointer                                   :: outunitbud   => null()
    integer(I4B), pointer                                   :: ietflag      => null()
    integer(I4B), pointer                                   :: igwetflag    => null()
    integer(I4B), pointer                                   :: iseepflag    => null()
    integer(I4B), pointer                                   :: imaxcellcnt  => null()
    integer(I4B), dimension(:), pointer, contiguous         :: igwfnode     => null()
    real(DP), dimension(:), pointer, contiguous             :: appliedinf   => null()
    real(DP), dimension(:), pointer, contiguous             :: rejinf       => null()
    real(DP), dimension(:), pointer, contiguous             :: rejinf0      => null()
    real(DP), dimension(:), pointer, contiguous             :: rejinftomvr  => null()
    real(DP), dimension(:), pointer, contiguous             :: infiltration => null()
    real(DP), dimension(:), pointer, contiguous             :: recharge     => null()
    real(DP), dimension(:), pointer, contiguous             :: gwet         => null()
    real(DP), dimension(:), pointer, contiguous             :: uzet         => null()
    real(DP), dimension(:), pointer, contiguous             :: gwd          => null()
    real(DP), dimension(:), pointer, contiguous             :: gwd0         => null()
    real(DP), dimension(:), pointer, contiguous             :: gwdtomvr     => null()
    real(DP), dimension(:), pointer, contiguous             :: rch          => null()
    real(DP), dimension(:), pointer, contiguous             :: rch0         => null()
    real(DP), dimension(:), pointer, contiguous             :: qsto         => null()
    integer(I4B), pointer                                   :: iuzf2uzf     => null()
    !
    ! -- integer vectors
    integer(I4B), dimension(:), pointer, contiguous :: ia => null()
    integer(I4B), dimension(:), pointer, contiguous :: ja => null()
    !
    ! -- timeseries aware variables
    real(DP), dimension(:), pointer, contiguous :: sinf => null()
    real(DP), dimension(:), pointer, contiguous :: pet => null()
    real(DP), dimension(:), pointer, contiguous :: extdp => null()
    real(DP), dimension(:), pointer, contiguous :: extwc => null()
    real(DP), dimension(:), pointer, contiguous :: ha => null()
    real(DP), dimension(:), pointer, contiguous :: hroot => null()
    real(DP), dimension(:), pointer, contiguous :: rootact => null()
    real(DP), dimension(:,:), pointer, contiguous :: uauxvar => null()
    !
    ! -- convergence check
    integer(I4B), pointer :: iconvchk => null()
    !
    ! formulate variables
    real(DP), dimension(:), pointer, contiguous :: deriv => null()
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
    character(len=16), dimension(:), pointer, contiguous :: cauxcbc => NULL()
    real(DP), dimension(:), pointer, contiguous :: qauxcbc => null()
    !
    ! -- observations
    real(DP), dimension(:), pointer, contiguous            :: obs_theta   => null()
    real(DP), dimension(:), pointer, contiguous            :: obs_depth   => null()
    integer(I4B), dimension(:), pointer, contiguous        :: obs_num     => null()

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
    procedure, private :: findcellabove
    procedure, private :: check_cell_area
    !
    ! -- budget
    procedure, private :: uzf_setup_budobj
    procedure, private :: uzf_fill_budobj
    
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
    ! -- create name and memory path
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
    packobj%ictMemPath = create_mem_path(namemodel,'NPF')
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
    use MemoryManagerModule, only: mem_allocate, mem_setptr, mem_reallocate
    ! -- dummy
    class(UzfType), intent(inout) :: this
    ! -- local
    integer(I4B) :: n, i
    real(DP) :: hgwf
! ------------------------------------------------------------------------------
    !
    call this%obs%obs_ar()
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_arrays()
    !
    ! -- set pointers now that data is available
    call mem_setptr(this%gwfhcond, 'CONDSAT', create_mem_path(this%name_model,'NPF'))
    call mem_setptr(this%gwfiss, 'ISS', create_mem_path(this%name_model))
    !
    ! -- set boundname for each connection
    if (this%inamedbound /= 0) then
      do n = 1, this%nodes
        this%boundname(n) = this%uzfname(n)
      end do
    endif
    !
    ! -- copy igwfnode into nodelist and set water table
    do i = 1, this%nodes
      this%nodelist(i) = this%igwfnode(i)
      n = this%igwfnode(i)
      hgwf = this%xnew(n)
      call this%uzfobj%sethead(i, hgwf)
    end do
    !
    ! allocate space to store moisture content observations
    n = this%obs%npakobs
    if ( n > 0 ) then
      call mem_reallocate(this%obs_theta, n, 'OBS_THETA', this%memoryPath)
      call mem_reallocate(this%obs_depth, n, 'OBS_DEPTH', this%memoryPath)
      call mem_reallocate(this%obs_num, n, 'OBS_NUM', this%memoryPath)
    end if
    !
    ! -- setup pakmvrobj
    if (this%imover /= 0) then
      allocate(this%pakmvrobj)
      call this%pakmvrobj%ar(this%maxbound, this%maxbound, this%memoryPath)
    endif
    !
    ! -- return
    return
  end subroutine uzf_ar

  subroutine uzf_allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- allocate arrays used for uzf
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(UzfType),   intent(inout) :: this
    ! -- local
    integer (I4B) :: i
    integer (I4B) :: j
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars (now done from AR)
    !call this%BndType%allocate_arrays()
    !
    ! -- allocate uzf specific arrays
    call mem_allocate(this%igwfnode, this%nodes, 'IGWFNODE', this%memoryPath)
    call mem_allocate(this%appliedinf, this%nodes, 'APPLIEDINF', this%memoryPath)
    call mem_allocate(this%rejinf, this%nodes, 'REJINF', this%memoryPath)
    call mem_allocate(this%rejinf0, this%nodes, 'REJINF0', this%memoryPath)
    call mem_allocate(this%rejinftomvr, this%nodes, 'REJINFTOMVR', this%memoryPath)
    call mem_allocate(this%infiltration, this%nodes, 'INFILTRATION', this%memoryPath)
    call mem_allocate(this%recharge, this%nodes, 'RECHARGE', this%memoryPath)
    call mem_allocate(this%gwet, this%nodes, 'GWET', this%memoryPath)
    call mem_allocate(this%uzet, this%nodes, 'UZET', this%memoryPath)
    call mem_allocate(this%gwd, this%nodes, 'GWD', this%memoryPath)
    call mem_allocate(this%gwd0, this%nodes, 'GWD0', this%memoryPath)
    call mem_allocate(this%gwdtomvr, this%nodes, 'GWDTOMVR', this%memoryPath)
    call mem_allocate(this%rch, this%nodes, 'RCH', this%memoryPath)
    call mem_allocate(this%rch0, this%nodes, 'RCH0', this%memoryPath)
    call mem_allocate(this%qsto, this%nodes, 'QSTO', this%memoryPath)
    call mem_allocate(this%deriv, this%nodes, 'DERIV', this%memoryPath)

    ! -- integer vectors
    call mem_allocate(this%ia, this%dis%nodes+1, 'IA', this%memoryPath)
    call mem_allocate(this%ja, this%nodes, 'JA', this%memoryPath)

    ! -- allocate timeseries aware variables
    call mem_allocate(this%sinf, this%nodes, 'SINF', this%memoryPath)
    call mem_allocate(this%pet, this%nodes, 'PET', this%memoryPath)
    call mem_allocate(this%extdp, this%nodes, 'EXDP', this%memoryPath)
    call mem_allocate(this%extwc, this%nodes, 'EXTWC', this%memoryPath)
    call mem_allocate(this%ha, this%nodes, 'HA', this%memoryPath)
    call mem_allocate(this%hroot, this%nodes, 'HROOT', this%memoryPath)
    call mem_allocate(this%rootact, this%nodes, 'ROOTACT', this%memoryPath)
    call mem_allocate(this%uauxvar, this%naux, this%nodes, 'UAUXVAR', this%memoryPath)
    
    ! -- initialize
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
      this%sinf(i) = DZERO
      this%pet(i) = DZERO
      this%extdp(i) = DZERO
      this%extwc(i) = DZERO
      this%ha(i) = DZERO
      this%hroot(i) = DZERO
      this%rootact(i) = DZERO
      do j = 1, this%naux
        if (this%iauxmultcol > 0 .and. j == this%iauxmultcol) then
          this%uauxvar(j, i) = DONE
        else
          this%uauxvar(j, i) = DZERO
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
    allocate(this%uzfname(this%nodes))
    !
    ! -- allocate and initialize qauxcbc
    call mem_allocate(this%qauxcbc, this%cbcauxitems, 'QAUXCBC', this%memoryPath)
    do i = 1, this%cbcauxitems
      this%qauxcbc(i) = DZERO
    end do
    !
    ! -- Allocate obs members
    call mem_allocate(this%obs_theta, 0, 'OBS_THETA', this%memoryPath)
    call mem_allocate(this%obs_depth, 0, 'OBS_DEPTH', this%memoryPath)
    call mem_allocate(this%obs_num, 0, 'OBS_NUM', this%memoryPath)
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
    use ConstantsModule, only: DZERO, MNORMAL
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
    ! -- formats
    character(len=*),parameter :: fmtnotfound= &
      "(4x, 'NO UZF OPTIONS WERE FOUND.')"
    character(len=*),parameter :: fmtet = &
      "(4x, 'ET WILL BE SIMULATED WITHIN UZ AND GW ZONES, WITH LINEAR ',  &
        &'GWET IF OPTION NOT SPECIFIED OTHERWISE.')"
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
      !                 form, access, 'REPLACE', mode_opt=MNORMAL)
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
          call openfile(this%ibudgetout, this%iout, fname, 'DATA(BINARY)',       &
                        form, access, 'REPLACE', mode_opt=MNORMAL)
          write(this%iout,fmtuzfbin) 'BUDGET', fname, this%ibudgetout
          found = .true.
        else
          call store_error('OPTIONAL BUDGET KEYWORD MUST BE FOLLOWED BY FILEOUT')
        end if
      case('PACKAGE_CONVERGENCE')
        call this%parser%GetStringCaps(keyword)
        if (keyword == 'FILEOUT') then
          call this%parser%GetString(fname)
          this%ipakcsv = getunit()
          call openfile(this%ipakcsv, this%iout, fname, 'CSV',                   &
                        filstat_opt='REPLACE', mode_opt=MNORMAL)
          write(this%iout,fmtuzfbin) 'PACKAGE_CONVERGENCE', fname, this%ipakcsv
          found = .true.
        else
          call store_error('OPTIONAL PACKAGE_CONVERGENCE KEYWORD MUST BE ' //    &
                           'FOLLOWED BY FILEOUT')
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
      !case('DEV_MAXIMUM_PERCENT_DIFFERENCE')
      !  call this%parser%DevOpt()
      !  r = this%parser%GetDouble()
      !  if (r > DZERO) then
      !    this%pdmax = r
      !    write(this%iout, fmtuzfopt) 'MAXIMUM_PERCENT_DIFFERENCE', this%pdmax
      !  else
      !    write(this%iout, fmtuzfopt) 'INVALID MAXIMUM_PERCENT_DIFFERENCE', r
      !    write(this%iout, fmtuzfopt) 'USING DEFAULT MAXIMUM_PERCENT_DIFFERENCE', this%pdmax
      !  end if
      !  found = .true.
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
    character(len=LINELENGTH) :: keyword
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
      write(this%iout,'(/1x,a)')                                                 &
        'PROCESSING ' // trim(adjustl(this%text)) // ' DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NUZFCELLS')
            this%nodes = this%parser%GetInteger()
            write(this%iout,'(4x,a,i0)') 'NUZFCELLS = ', this%nodes
          case ('NTRAILWAVES')
            this%ntrail = this%parser%GetInteger()
            write(this%iout,'(4x,a,i0)')  'NTRAILWAVES = ', this%ntrail
          case ('NWAVESETS')
            this%nsets = this%parser%GetInteger()
            write(this%iout,'(4x,a,i0)') 'NTRAILSETS = ', this%nsets
          case default
            write(errmsg,'(a,a)')                                                &
              'Unknown '// trim(this%text) // ' dimension: ', trim(keyword)
          end select
      end do
      write(this%iout,'(1x,a)')                                                  &
        'END OF ' // trim(adjustl(this%text)) // ' DIMENSIONS'
    else
      call store_error('Required dimensions block not found.')
    end if
    !
    ! -- increment maxbound
    this%maxbound = this%maxbound + this%nodes
    this%nbound = this%maxbound
    !
    ! -- verify dimensions were set
    if (this%nodes <= 0) then
      write(errmsg, '(a)')                                                       &
        'NUZFCELLS was not specified or was specified incorrectly.'
      call store_error(errmsg)
    end if

    if (this%ntrail <= 0) then
      write(errmsg, '(a)')                                                       &
        'NTRAILWAVES was not specified or was specified incorrectly.'
      call store_error(errmsg)
    end if
    !
    if (this%nsets <= 0) then
      write(errmsg, '(a)')                                                       &
        'NWAVESETS was not specified or was specified incorrectly.'
      call store_error(errmsg)
    end if
    !
    ! -- teminate if there are dimension errors
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- set the number of waves
    this%nwav = this%ntrail * this%nsets
    !
    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- Allocate arrays in package superclass
    call this%uzf_allocate_arrays()
    !
    ! -- initialize uzf group object
    allocate(this%uzfobj)
    call this%uzfobj%init(this%nodes, this%nwav, this%memoryPath)
    call this%uzfobjwork%init(1, this%nwav)
    !
    ! -- Set pointers to GWF model arrays
    call mem_setptr(this%gwftop, 'TOP', create_mem_path(this%name_model, 'DIS'))
    call mem_setptr(this%gwfbot, 'BOT', create_mem_path(this%name_model, 'DIS'))
    call mem_setptr(this%gwfarea, 'AREA', create_mem_path(this%name_model, 'DIS'))
    !
    !--Read uzf cell properties and set values
    call this%read_cell_properties()
    !
    ! -- print cell data
    if (this%iprpak /= 0) then
      call this%print_cell_properties()
    end if
    !
    ! -- setup the budget object
    call this%uzf_setup_budobj()
    !
    ! -- return
    return
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
    use TdisModule, only: kper, nper
    use TimeSeriesManagerModule, only: read_value_or_time_series_adv
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(UzfType), intent(inout) :: this
    ! -- local
    character(len=LENBOUNDNAME) :: bndName
    character(len=LINELENGTH) :: text
    character(len=LINELENGTH) :: line
    logical :: isfound
    logical :: endOfBlock
    integer (I4B) :: i
    integer (I4B) :: j
    integer (I4B) :: jj
    integer(I4B) :: ierr
    real(DP), pointer :: bndElem => null()
    ! -- table output
    character (len=20) :: cellid
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: tag
    integer(I4B) :: ntabrows
    integer(I4B) :: ntabcols
    integer(I4B) :: node
    !-- formats
    character(len=*),parameter :: fmtlsp = &
        "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
      character(len=*),parameter :: fmtblkerr = &
        "('Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*), parameter :: fmtisvflow =                                &
        "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //  &
        "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*),parameter :: fmtflow =                                    &
        "(4x, 'FLOWS WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
! ------------------------------------------------------------------------------
    !
    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
    if (this%inunit == 0) return
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
        ! -- setup inputtab tableobj
        !
        ! -- table dimensions
        ntabrows = 1
        ntabcols = 3
        if (this%ietflag /= 0) then
          ntabcols = ntabcols + 3
          if (this%ietflag == 2) then
            ntabcols = ntabcols + 3
          end if
        end if
        if (this%inamedbound == 1) then
          ntabcols = ntabcols + 1
        end if
        !
        ! -- initialize table and define columns
        title = trim(adjustl(this%text)) // ' PACKAGE (' //                        &
                trim(adjustl(this%packName)) //') DATA FOR PERIOD'
        write(title, '(a,1x,i6)') trim(adjustl(title)), kper
        call table_cr(this%inputtab, this%packName, title)
        call this%inputtab%table_df(ntabrows, ntabcols, this%iout,               &
                                    finalize=.FALSE.)
        tag = 'NUMBER'
        call this%inputtab%initialize_column(tag, 10)
        tag = 'CELLID'
        call this%inputtab%initialize_column(tag, 20, alignment=TABLEFT)
        tag = 'FINF'
        call this%inputtab%initialize_column(tag, 12)
        if (this%ietflag /= 0) then
          tag = 'PET'
          call this%inputtab%initialize_column(tag, 12)
          tag = 'EXTDEP'
          call this%inputtab%initialize_column(tag, 12)
          tag = 'EXTWC'
          call this%inputtab%initialize_column(tag, 12)
          if (this%ietflag == 2) then
            tag = 'HA'
            call this%inputtab%initialize_column(tag, 12)
            tag = 'HROOT'
            call this%inputtab%initialize_column(tag, 12)
            tag = 'ROOTACT'
            call this%inputtab%initialize_column(tag, 12)
          end if
        end if
        if (this%inamedbound == 1) then
          tag = 'BOUNDNAME'
          call this%inputtab%initialize_column(tag, LENBOUNDNAME, alignment=TABLEFT)
        end if
      end if
      !
      ! -- read the stress period data
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        !
        ! -- check for valid uzf node
        i = this%parser%GetInteger()
        if (i < 1 .or. i > this%nodes) then
          tag = trim(adjustl(this%text)) // ' PACKAGE (' //                      &
                trim(adjustl(this%packName)) //') DATA FOR PERIOD'
          write(tag, '(a,1x,i0)') trim(adjustl(tag)), kper
          
          write(errmsg,'(a,a,i0,1x,a,i0,a)')                                     &
            trim(adjustl(tag)), ': UZFNO ', i,                                   &
            'must be greater than 0 and less than or equal to ', this%nodes, '.'
          call store_error(errmsg)
          cycle
        end if
        !
        ! -- Setup boundname
        if (this%inamedbound > 0) then
          bndName = this%boundname(i)
        else
          bndName = ''
        end if
        !
        ! -- FINF
        call this%parser%GetStringCaps(text)
        jj = 1    ! For SINF
        bndElem => this%sinf(i)
        call read_value_or_time_series_adv(text, i, jj, bndElem, this%packName,      &
                                           'BND', this%tsManager, this%iprpak,   &
                                           'SINF')
        !
        ! -- PET
        call this%parser%GetStringCaps(text)
        jj = 1    ! For PET
        bndElem => this%pet(i)
        call read_value_or_time_series_adv(text, i, jj, bndElem, this%packName,      &
                                           'BND', this%tsManager, this%iprpak,   &
                                           'PET')
        !
        ! -- EXTD
        call this%parser%GetStringCaps(text)
        jj = 1    ! For EXTDP
        bndElem => this%extdp(i)
        call read_value_or_time_series_adv(text, i, jj, bndElem, this%packName,      &
                                           'BND', this%tsManager, this%iprpak,   &
                                           'EXTDP')
        !
        ! -- EXTWC
        call this%parser%GetStringCaps(text)
        jj = 1    ! For EXTWC
        bndElem => this%extwc(i)
        call read_value_or_time_series_adv(text, i, jj, bndElem, this%packName,      &
                                           'BND', this%tsManager, this%iprpak,   &
                                           'EXTWC')
        !
        ! -- HA
        call this%parser%GetStringCaps(text)
        jj = 1    ! For HA
        bndElem => this%ha(i)
        call read_value_or_time_series_adv(text, i, jj, bndElem, this%packName,      &
                                           'BND', this%tsManager, this%iprpak,   &
                                           'HA')
        !
        ! -- HROOT
        call this%parser%GetStringCaps(text)
        jj = 1    ! For HROOT
        bndElem => this%hroot(i)
        call read_value_or_time_series_adv(text, i, jj, bndElem, this%packName,      &
                                           'BND', this%tsManager, this%iprpak,   &
                                           'HROOT')
        !
        ! -- ROOTACT
        call this%parser%GetStringCaps(text)
        jj = 1    ! For ROOTACT
        bndElem => this%rootact(i)
        call read_value_or_time_series_adv(text, i, jj, bndElem, this%packName,      &
                                           'BND', this%tsManager, this%iprpak,   &
                                           'ROOTACT')
        !
        ! -- read auxillary variables
        do j = 1, this%naux
          call this%parser%GetStringCaps(text)
          bndElem => this%uauxvar(j, i)
          call read_value_or_time_series_adv(text, i, j, bndElem, this%packName,     &
                                             'AUX', this%tsManager, this%iprpak, &
                                             this%auxname(j))
        end do
        !
        ! -- write line
        if (this%iprpak /= 0) then
          !
          ! -- get cellid
          node = this%igwfnode(i)
          if (node > 0) then
            call this%dis%noder_to_string(node, cellid)
          else
            cellid = 'none'
          end if
          !
          ! -- write data to the table
          call this%inputtab%add_term(i)
          call this%inputtab%add_term(cellid)
          call this%inputtab%add_term(this%sinf(i))
          if (this%ietflag /= 0) then
            call this%inputtab%add_term(this%pet(i))
            call this%inputtab%add_term(this%extdp(i))
            call this%inputtab%add_term(this%extwc(i))
            if (this%ietflag == 2) then
              call this%inputtab%add_term(this%ha(i))
              call this%inputtab%add_term(this%hroot(i))
              call this%inputtab%add_term(this%rootact(i))
            end if
          end if
          if (this%inamedbound == 1) then
            call this%inputtab%add_term(this%boundname(i))
          end if
        end if

      end do
      !
      ! -- finalize the table
      if (this%iprpak /= 0) then
        call this%inputtab%finalize_table()
      end if
    !
    ! -- using stress period data from the previous stress period
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
    if ((this%issflag == 0 .AND. kper == 1) .or.                              &
      (kper == 2 .AND. this%issflagold == 1)) then
      do i = 1, this%nodes
        call this%uzfobj%setwaves(i)
      end do
    end if
    this%issflagold = this%issflag
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
    integer(I4B) :: i
    integer(I4B) :: ivertflag
    integer(I4B) :: n, iaux
    real (DP) :: rval1, rval2, rval3
! ------------------------------------------------------------------------------
    !
    ! -- Advance the time series
    call this%TsManager%ad()
    !
    ! -- update auxiliary variables by copying from the derived-type time
    !    series variable into the bndpackage auxvar variable so that this
    !    information is properly written to the GWF budget file
    if (this%naux > 0) then
      do n = 1, this%maxbound
        do iaux = 1, this%naux
          if (this%noupdateauxvar(iaux) /= 0) cycle
          this%auxvar(iaux, n) = this%uauxvar(iaux, n)
        end do
      end do
    end if
    !
    do i = 1, this%nodes
        call this%uzfobj%advance(i)
    end do
    !
    ! -- update uzf objects with timeseries aware variables
    do i = 1, this%nodes
      !
      ! -- Set ivertflag
      ivertflag = this%uzfobj%ivertcon(i)
      !
      ! -- recalculate uzfarea
      if (this%iauxmultcol > 0) then
        rval1 = this%uauxvar(this%iauxmultcol, i)
        call this%uzfobj%setdatauzfarea(i, rval1)
      end if
      !
      ! -- FINF
      rval1 = this%sinf(i)
      call this%uzfobj%setdatafinf(i, rval1)
      !
      ! -- PET, EXTDP
      rval1 = this%pet(i)
      rval2 = this%extdp(i)
      call this%uzfobj%setdataet(i, ivertflag, rval1, rval2)
      !
      ! -- ETWC
      rval1 = this%extwc(i)
      call this%uzfobj%setdataetwc(i, ivertflag, rval1)
      !
      ! -- HA, HROOT, ROOTACT
      rval1 = this%ha(i)
      rval2 = this%hroot(i)
      rval3 = this%rootact(i)
      call this%uzfobj%setdataetha(i, ivertflag, rval1, rval2, rval3)
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

  subroutine uzf_cf(this, reset_mover)
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
    logical, intent(in), optional :: reset_mover
    ! -- locals
    integer(I4B) :: n
    logical :: lrm
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
    lrm = .true.
    if (present(reset_mover)) lrm = reset_mover
    if(this%imover == 1 .and. lrm) then
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

  subroutine uzf_cc(this, innertot, kiter, iend, icnvgmod, cpak, ipak, dpak)
! **************************************************************************
! uzf_cc -- Final convergence check for package
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    use TdisModule, only: totim, kstp, kper, delt
    ! -- dummy
    class(Uzftype), intent(inout) :: this
    integer(I4B), intent(in) :: innertot
    integer(I4B), intent(in) :: kiter
    integer(I4B), intent(in) :: icnvgmod
    integer(I4B), intent(in) :: iend
    character(len=LENPAKLOC), intent(inout) :: cpak
    integer(I4B), intent(inout) :: ipak
    real(DP), intent(inout) :: dpak
    ! -- local
    character(len=LENPAKLOC) :: cloc
    character(len=LINELENGTH) :: tag
    integer(I4B) :: icheck
    integer(I4B) :: ipakfail
    integer(I4B) :: locdrejinfmax
    integer(I4B) :: locdrchmax
    integer(I4B) :: locdseepmax
    integer(I4B) :: ntabrows
    integer(I4B) :: ntabcols
    integer(I4B) :: n
    real(DP) :: qtolfact
    real(DP) :: drejinf
    real(DP) :: drejinfmax
    real(DP) :: drch
    real(DP) :: drchmax
    real(DP) :: dseep
    real(DP) :: dseepmax
    ! format
! --------------------------------------------------------------------------
    !
    ! -- initialize local variables
    icheck = this%iconvchk
    ipakfail = 0
    locdrejinfmax = 0
    locdrchmax = 0
    locdseepmax = 0
    drejinfmax = DZERO
    drchmax = DZERO
    dseepmax = DZERO
    !
    ! -- if not saving package convergence data on check convergence if
    !    the model is considered converged
    if (this%ipakcsv == 0) then
      if (icnvgmod == 0) then
        icheck = 0
      end if
    else
      !
      ! -- header for package csv
      if (.not. associated(this%pakcsvtab)) then
        !
        ! -- determine the number of columns and rows
        ntabrows = 1
        ntabcols = 9
        if (this%iseepflag == 1) then
          ntabcols = ntabcols + 2
        end if
        !
        ! -- setup table
        call table_cr(this%pakcsvtab, this%packName, '')
        call this%pakcsvtab%table_df(ntabrows, ntabcols, this%ipakcsv,           &
                                     lineseparator=.FALSE., separator=',',       &
                                     finalize=.FALSE.)
        !
        ! -- add columns to package csv
        tag = 'total_inner_iterations'
        call this%pakcsvtab%initialize_column(tag, 10, alignment=TABLEFT)
        tag = 'totim'
        call this%pakcsvtab%initialize_column(tag, 10, alignment=TABLEFT)
        tag = 'kper'
        call this%pakcsvtab%initialize_column(tag, 10, alignment=TABLEFT)
        tag = 'kstp'
        call this%pakcsvtab%initialize_column(tag, 10, alignment=TABLEFT)
        tag = 'nouter'
        call this%pakcsvtab%initialize_column(tag, 10, alignment=TABLEFT)
        tag = 'drejinfmax'
        call this%pakcsvtab%initialize_column(tag, 15, alignment=TABLEFT)
        tag = 'drejinfmax_loc'
        call this%pakcsvtab%initialize_column(tag, 15, alignment=TABLEFT)
        tag = 'drchmax'
        call this%pakcsvtab%initialize_column(tag, 15, alignment=TABLEFT)
        tag = 'drchmax_loc'
        call this%pakcsvtab%initialize_column(tag, 15, alignment=TABLEFT)
        if (this%iseepflag == 1) then
          tag = 'dseepmax'
          call this%pakcsvtab%initialize_column(tag, 15, alignment=TABLEFT)
          tag = 'dseepmax_loc'
          call this%pakcsvtab%initialize_column(tag, 15, alignment=TABLEFT)
        end if
      end if
    end if
    !
    ! -- perform package convergence check
    if (icheck /= 0) then
      final_check: do n = 1, this%nodes
        !
        ! -- set the Q to length factor
        qtolfact = delt / this%uzfobj%uzfarea(n)
        !
        ! -- rejected infiltration
        drejinf = qtolfact * (this%rejinf0(n) - this%rejinf(n))
        !
        ! -- groundwater recharge
        drch = qtolfact * (this%rch0(n) - this%rch(n))
        !
        ! -- groundwater seepage to the land surface
        dseep = DZERO
        if (this%iseepflag == 1) then
          dseep = qtolfact * (this%gwd0(n) - this%gwd(n))
        end if
        !
        ! -- evaluate magnitude of differences
        if (n == 1) then
          drejinfmax = drejinf
          locdrejinfmax = n
          drchmax = drch
          locdrchmax = n
          dseepmax = dseep
          locdseepmax = n
        else
          if (ABS(drejinf) > abs(drejinfmax)) then
            drejinfmax = drejinf
            locdrejinfmax = n
          end if
          if (ABS(drch) > abs(drchmax)) then
            drchmax = drch
            locdrchmax = n
          end if
          if (ABS(dseep) > abs(dseepmax)) then
            dseepmax = dseep
            locdseepmax = n
          end if
        end if
      end do final_check
      !
      ! -- set dpak and cpak
      if (ABS(drejinfmax) > abs(dpak)) then
        ipak = locdrejinfmax
        dpak = drejinfmax
        write(cloc, "(a,'-',a)") trim(this%packName), 'rejinf'
        cpak = trim(cloc)
      end if
      if (ABS(drchmax) > abs(dpak)) then
        ipak = locdrchmax
        dpak = drchmax
        write(cloc, "(a,'-',a)") trim(this%packName), 'rech'
        cpak = trim(cloc)
      end if
      if (this%iseepflag == 1) then
        if (ABS(dseepmax) > abs(dpak)) then
          ipak = locdseepmax
          dpak = dseepmax
          write(cloc, "(a,'-',a)") trim(this%packName), 'seep'
          cpak = trim(cloc)
        end if
      end if
      !
      ! -- write convergence data to package csv
      if (this%ipakcsv /= 0) then
        !
        ! -- write the data
        call this%pakcsvtab%add_term(innertot)
        call this%pakcsvtab%add_term(totim)
        call this%pakcsvtab%add_term(kper)
        call this%pakcsvtab%add_term(kstp)
        call this%pakcsvtab%add_term(kiter)
        call this%pakcsvtab%add_term(drejinfmax)
        call this%pakcsvtab%add_term(locdrejinfmax)
        call this%pakcsvtab%add_term(drchmax)
        call this%pakcsvtab%add_term(locdrchmax)
        if (this%iseepflag == 1) then
          call this%pakcsvtab%add_term(dseepmax)
          call this%pakcsvtab%add_term(locdseepmax)
        end if
        !
        ! -- finalize the package csv
        if (iend == 1) then
          call this%pakcsvtab%finalize_table()
        end if
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
    character(len=LINELENGTH) :: title
    character(len=20) :: nodestr
    integer(I4B) :: maxrows
    integer(I4B) :: nodeu
    integer(I4B) :: i, node, ibinun
    integer(I4B) :: n, m, ivertflag, ierr
    integer(I4B) :: n2
    real(DP) :: rfinf
    real(DP) :: rin,rout,rsto,ret,retgw,rgwseep,rvflux
    real(DP) :: hgwf,hgwflm1,ratin,ratout,rrate,rrech
    real(DP) :: trhsgwet,thcofgwet,derivgwet
    real(DP) :: qfrommvr, qformvr, qgwformvr
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
    real(DP) :: cvv
    integer(I4B) :: naux, numobs
    ! -- for observations
    integer(I4B) :: j
    character(len=LENBOUNDNAME) :: bname
    ! -- formats
    character(len=*), parameter :: fmttkk = &
      "(1X,/1X,A,'   PERIOD ',I0,'   STEP ',I0)"
! ------------------------------------------------------------------------------
    !
    ! -- initialize accumulators
    ierr = 0
    rfinf = DZERO
    rin = DZERO
    rout = DZERO
    rrech = DZERO
    rsto = DZERO
    ret = DZERO
    retgw = DZERO
    rgwseep = DZERO
    rvflux = DZERO
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
    this%uzfobj%pet = this%uzfobj%petmax
    !
    ! -- set kstp, kper, and maxrows
    maxrows = 0
    if (this%iprflow /= 0) then
      call this%outputtab%set_kstpkper(kstp, kper)
      do i = 1, this%nodes
        node = this%nodelist(i)
        if (this%ibound(node) > 0) then
          maxrows = maxrows + 1
        end if
      end do
      call this%outputtab%set_maxbound(maxrows)
    end if
    !
    ! -- Go through and process each UZF cell
    do i = 1, this%nodes
      !
      ! -- Initialize variables
      n = this%nodelist(i)
      ivertflag = this%uzfobj%ivertcon(i)
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
      ! -- for now set cvv = DZERO
      ! cvv = this%gwfhcond(m)
      cvv = DZERO
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
      call this%uzfobj%budget(ivertflag,i,this%totfluxtot,                     &
                              rfinf,rin,rout,rsto,ret,retgw,rgwseep,rvflux,    &
                              this%ietflag,this%iseepflag,this%issflag,hgwf,   &
                              hgwflm1,cvv,numobs,this%obs_num,                 &
                              this%obs_depth,this%obs_theta,qfrommvr,qformvr,  &
                              qgwformvr,ierr)
      if ( ierr > 0 ) then
        if ( ierr == 1 ) &
          errmsg = 'UZF variable NWAVESETS needs to be increased.'
        call store_error(errmsg)
        call ustop()
      end if 

      !
      ! -- Calculate gwet
      if (this%igwetflag > 0) then
        call this%uzfobj%setgwpet(i)
        derivgwet = DZERO
        call this%uzfobj%simgwet(this%igwetflag, i, hgwf, trhsgwet, thcofgwet, &
                                 derivgwet)
        retgw = retgw + this%uzfobj%gwet(i)
      end if
      !
      ! -- distribute PET to deeper cells
        if (this%ietflag > 0) then
          if (this%uzfobj%ivertcon(i) > 0) then
            call this%uzfobj%setbelowpet(i, ivertflag)
          end if
        end if
      !
      ! -- Calculate flows for cbc output and observations
      if (hgwf > this%uzfobj%celbot(i)) then
        this%recharge(i) = this%uzfobj%totflux(i) * this%uzfobj%uzfarea(i) / delt
      else
        if (ivertflag == 0) then
          this%recharge(i) = this%uzfobj%surflux(i) * this%uzfobj%uzfarea(i)
        else
          this%recharge(i) = this%uzfobj%surflux(ivertflag) * this%uzfobj%uzfarea(i)
        end if
      end if

      this%rch(i) = this%uzfobj%totflux(i) * this%uzfobj%uzfarea(i) / delt

      this%appliedinf(i) = this%uzfobj%sinf(i) * this%uzfobj%uzfarea(i)
      this%infiltration(i) = this%uzfobj%surflux(i) * this%uzfobj%uzfarea(i)

      this%rejinf(i) = this%uzfobj%finf_rej(i) * this%uzfobj%uzfarea(i)

      qout = this%rejinf(i) + this%uzfobj%surfseep(i)
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
      !
      ! -- set rejected infiltration to the remainder
      q = q - this%rejinftomvr(i)
      !
      ! -- values less than zero represent a volumetric error resulting
      !    from qtomvr being greater than water available to the mover
      if (q < DZERO) then
        q = DZERO
      end if
      this%rejinf(i) = q

      this%gwd(i) = this%uzfobj%surfseep(i)
      qfact = DZERO
      if (qout > DZERO) then
        qfact = this%gwd(i) / qout
      end if
      q = this%gwd(i)
      this%gwdtomvr(i) = qfact * qtomvr
      !
      ! -- set groundwater discharge to the remainder
      q = q - this%gwdtomvr(i)
      !
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

      this%gwet(i) = this%uzfobj%gwet(i)
      this%uzet(i) = this%uzfobj%etact(i) * this%uzfobj%uzfarea(i) / delt
      this%qsto(i) = this%uzfobj%delstor(i) / delt

      ! -- accumulate groundwater et
      qgwet = qgwet + this%gwet(i)

      !
      ! -- End of UZF cell loop
      !
    end do
    !
    ! -- For continuous observations, save simulated values.
    if (this%obs%npakobs > 0 .and. iprobs > 0) then
      call this%uzf_bd_obs()
    endif
    !
    ! add cumulative flows to UZF budget
    this%infilsum = rin * delt
    this%rechsum = rout * delt
    rrech = rout
    this%delstorsum = rsto * delt
    this%uzetsum = ret * delt
    this%vfluxsum = rvflux
    !
    !
    rin = DZERO
    rout = DZERO
    if(rsto < DZERO) then
      rin = -rsto
    else
      rout = rsto
    endif
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
      if (ibinun /= 0) then
        call this%dis%record_srcdst_list_header(this%bdtxt(2), this%name_model, &
                    this%name_model, this%name_model, this%packName, naux,          &
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
        ! -- reset table title
        if (this%iprflow /= 0) then
          title = trim(this%text) // ' PACKAGE (' // trim(this%packName) //          &
                  ') ' // trim(adjustl(this%bdtxt(2))) // ' FLOW RATES'
          call this%outputtab%set_title(title)
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
              !
              ! -- set nodestr and write outputtab table
              nodeu = this%dis%get_nodeuser(node)
              call this%dis%nodeu_to_string(nodeu, nodestr)
              call this%outputtab%print_list_entry(i, nodestr, rrate, bname)
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
        if (ibinun /= 0) then
          call this%dis%record_srcdst_list_header(this%bdtxt(3),               &
                      this%name_model,                                         &
                      this%name_model, this%name_model, this%packName, naux,       &
                      this%auxname, ibinun, this%nodes, this%iout)
        end if
        !
        ! -- reset table title
        if (this%iprflow /= 0) then
          title = trim(this%text) // ' PACKAGE (' // trim(this%packName) //          &
                  ') ' // trim(adjustl(this%bdtxt(4))) // ' FLOW RATES'
          call this%outputtab%set_title(title)
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
                !
                ! -- set nodestr and write outputtab table
                nodeu = this%dis%get_nodeuser(node)
                call this%dis%nodeu_to_string(nodeu, nodestr)
                call this%outputtab%print_list_entry(i, nodestr, rrate, bname)
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
          if (ibinun /= 0) then
            call this%dis%record_srcdst_list_header(this%bdtxt(5),              &
                        this%name_model, this%name_model,                       &
                        this%name_model, this%packName, naux,                       &
                        this%auxname, ibinun, this%nodes, this%iout)
          end if
          !
          ! -- reset table title
          if (this%iprflow /= 0) then
            title = trim(this%text) // ' PACKAGE (' // trim(this%packName) //        &
                    ') ' // trim(adjustl(this%bdtxt(5))) // ' FLOW RATES'
            call this%outputtab%set_title(title)
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
                  !
                  ! -- set nodestr and write outputtab table
                  nodeu = this%dis%get_nodeuser(node)
                  call this%dis%nodeu_to_string(nodeu, nodestr)
                  call this%outputtab%print_list_entry(i, nodestr, rrate, bname)
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
        if (ibinun /= 0) then
          call this%dis%record_srcdst_list_header(this%bdtxt(4), this%name_model,&
                      this%name_model, this%name_model, this%packName, naux,        &
                      this%auxname, ibinun, this%nodes, this%iout)
        end if
        !
        ! -- reset table title
        if (this%iprflow /= 0) then
          title = trim(this%text) // ' PACKAGE (' // trim(this%packName) //          &
                  ') ' // trim(adjustl(this%bdtxt(4))) // ' FLOW RATES'
          call this%outputtab%set_title(title)
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
                !
                ! -- set nodestr and write outputtab table
                nodeu = this%dis%get_nodeuser(node)
                call this%dis%nodeu_to_string(nodeu, nodestr)
                call this%outputtab%print_list_entry(i, nodestr, rrate, bname)
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
    !
    ! -- uzf recharge
    ratin = rrech
    ratout = DZERO
    call model_budget%addentry(ratin, ratout, delt, this%bdtxt(2),                   &
                               isuppress_output, this%packName)
    !
    ! -- groundwater discharge
    if (this%iseepflag == 1) then
      ratin = DZERO
      ratout = qseep !rgwseep
      call model_budget%addentry(ratin, ratout, delt, this%bdtxt(3),                 &
                                 isuppress_output, this%packName)
      !
      ! -- groundwater discharge to mover
      if (this%imover == 1) then
        ratin = DZERO
        ratout = qseeptomvr
        call model_budget%addentry(ratin, ratout, delt, this%bdtxt(5),               &
                                   isuppress_output, this%packName)
      end if
    end if
    !
    ! -- groundwater et
    if (this%igwetflag /= 0) then
      ratin = DZERO
      ratout = qgwet !retgw
      !ratout = DZERO
      !if (retgw > DZERO) then
      !  ratout = -retgw
      !end if
      call model_budget%addentry(ratin, ratout, delt, this%bdtxt(4),                 &
                                 isuppress_output, this%packName)
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
    ! -- fill the budget object
    call this%uzf_fill_budobj()
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
  end subroutine uzf_bd

  subroutine uzf_ot(this, kstp, kper, iout, ihedfl, ibudfl)
! ******************************************************************************
! uzf_ot -- UZF package budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType) :: this
    integer(I4B),intent(in) :: kstp
    integer(I4B),intent(in) :: kper
    integer(I4B),intent(in) :: iout
    integer(I4B),intent(in) :: ihedfl
    integer(I4B),intent(in) :: ibudfl
    ! -- local
    ! -- format
 2000 FORMAT ( 1X, ///1X, A, A, A, '   PERIOD ', I6, '   STEP ', I8)
! ------------------------------------------------------------------------------
    !
    ! -- write uzf moisture content
    if (ihedfl /= 0 .and. this%iprwcont /= 0) then
      write (iout, 2000) 'UZF (', trim(this%packName), ') WATER-CONTENT', kper, kstp
      ! add code to write moisture content
    end if
    !
    ! -- Output uzf flow table
    if (ibudfl /= 0 .and. this%iprflow /= 0) then
      call this%budobj%write_flowtable(this%dis, kstp, kper)
    end if
    !
    ! -- Output uzf budget
    call this%budobj%write_budtable(kstp, kper, iout)
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
    real(DP) :: hgwf, hgwflm1, cvv, uzderiv, derivgwet
    real(DP) :: qfrommvr, qformvr
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    ierr = 0
    this%uzfobj%pet = this%uzfobj%petmax
    !
    ! -- Calculate hcof and rhs for each UZF entry
    do i = 1, this%nodes
      thcof1 = DZERO
      thcof2 = DZERO
      trhs1 = DZERO
      trhs2 = DZERO
      uzderiv = DZERO
      derivgwet = DZERO
      ivertflag = this%uzfobj%ivertcon(i)
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
        call this%uzfobj%formulate(this%uzfobjwork, ivertflag, i,              &
                                    this%totfluxtot, this%ietflag,             &
                                    this%issflag,this%iseepflag,               &
                                    trhs1,thcof1,hgwf,hgwflm1,cvv,uzderiv,     &
                                    qfrommvr,qformvr,ierr,ivertflag)
        !
        ! -- terminate if an error condition has occurred
        if (ierr > 0) then
            if ( ierr == 1 ) &
              errmsg = 'UZF variable NWAVESETS needs to be increased.'
            call store_error(errmsg)
            call ustop()
        end if
        
        if ( this%igwetflag > 0 ) then
          call this%uzfobj%setgwpet(i)
          call this%uzfobj%simgwet(this%igwetflag,i,hgwf,trhs2,thcof2,         &
                                    derivgwet)
        end if
        !
        ! -- distribute PET to deeper cells
        if (this%ietflag > 0) then
          if (this%uzfobj%ivertcon(i) > 0) then
            call this%uzfobj%setbelowpet(i, ivertflag)
          end if
        end if
        
        this%deriv(i) = uzderiv + derivgwet
        !
        ! -- save current rejected infiltration, groundwater recharge, and
        !    groundwater discharge
        this%rejinf(i) = this%uzfobj%finf_rej(i) * this%uzfobj%uzfarea(i)
        this%rch(i) = this%uzfobj%totflux(i) * this%uzfobj%uzfarea(i) / delt
        this%gwd(i) = this%uzfobj%surfseep(i)
        !
        ! -- add to hcof and rhs
        this%hcof(i) = thcof1 + thcof2
        this%rhs(i) = -trhs1 + trhs2
        !
        ! -- add spring discharge and rejected infiltration to mover
        if(this%imover == 1) then
          call this%pakmvrobj%accumulate_qformvr(i, qformvr)
        endif
      !
      end if
    end do
    !
    ! -- return
    return
  end subroutine uzf_solve

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
!                         UzfCellGroup type.
! ******************************************************************************
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error, count_errors
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: cellid
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
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr, &
      supportOpenClose=.true.)
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
          write(errmsg,'(2(a,1x),i0,a)')                                            &
            'IUZNO must be greater than 0 and less than',                           &
            'or equal to', this%nodes , '.'
          call store_error(errmsg)
          cycle
        end if 
        !
        ! -- increment nboundchk
        nboundchk(i) = nboundchk(i) + 1
        !
        ! -- store the reduced gwf nodenumber in igwfnode
        call this%parser%GetCellid(this%dis%ndim, cellid)
        ic = this%dis%noder_from_cellid(cellid,                                 &
                                        this%parser%iuactive, this%iout)
        this%igwfnode(i) = ic
        rowmaxnnz(ic) = rowmaxnnz(ic) + 1
        !
        ! -- landflag
        landflag = this%parser%GetInteger()
        if (landflag < 0 .OR. landflag > 1) then
          write(errmsg,'(a,1x,i0,1x,a,1x,i0,a)')                                 &
            'LANDFLAG for uzf cell', i,                                          &
            'must be 0 or 1 (specified value is', landflag, ').'
          call store_error(errmsg)
        end if
        !
        ! -- ivertcon
        ivertcon = this%parser%GetInteger()
        if (ivertcon < 0 .OR. ivertcon > this%nodes) then
          write(errmsg,'(a,1x,i0,1x,a,1x,i0,a)')                                 &
            'IVERTCON for uzf cell', i,                                          &
            'must be 0 or less than NUZFCELLS (specified value is',              &
            ivertcon, ').'
          call store_error(errmsg)
        end if
        !
        ! -- surfdep
        surfdep =  this%parser%GetDouble()
        if (surfdep <= DZERO) then   !need to check for cell thickness
          write(errmsg,'(a,1x,i0,1x,a,1x,g0,a)')                                 &
            'SURFDEP for uzf cell', i,                                           &
            'must be greater than 0 (specified value is', surfdep, ').'
          call store_error(errmsg)
        end if
        if(surfdep >= this%GWFTOP(ic) - this%GWFBOT(ic)) then
          write(errmsg,'(a,1x,i0,1x,a)')                                         &
            'SURFDEP for uzf cell', i,                                           &
            'cannot be greater than the cell thickness.'
          call store_error(errmsg)
        end if
        !
        ! -- vks
        vks = this%parser%GetDouble()
        if (vks <= DZERO) then
          write(errmsg,'(a,1x,i0,1x,a,1x,g0,a)')                                 &
            'VKS for uzf cell', i,                                               &
            'must be greater than 0 (specified value ia', vks, ').'
          call store_error(errmsg)
        end if
        !
        ! -- thtr
        thtr = this%parser%GetDouble()
        if (thtr <= DZERO) then
          write(errmsg,'(a,1x,i0,1x,a,1x,g0,a)')                                 &
            'THTR for uzf cell', i,                                              &
             'must be greater than 0 (specified value is', thtr, ').'
          call store_error(errmsg)
        end if
        !
        ! -- thts
        thts = this%parser%GetDouble()
        if (thts <= thtr) then
          write(errmsg,'(a,1x,i0,1x,a,1x,g0,a)')                                 &
            'THTS for uzf cell', i,                                              &
            'must be greater than THTR (specified value is', thts, ').'
          call store_error(errmsg)
        end if
        !
        ! -- thti
        thti = this%parser%GetDouble()
        if (thti < thtr .OR. thti > thts) then
          write(errmsg,'(a,1x,i0,1x,a,1x,a,1x,g0,a)')                            &
            'THTI for uzf cell', i,                                              &
            'must be greater than or equal to THTR AND less than THTS',          &
            '(specified value is', thti, ').'
          call store_error(errmsg)
        end if
        !
        ! -- eps
        eps = this%parser%GetDouble()
        if (eps < 3.5 .OR. eps > 14) then
          write(errmsg,'(a,1x,i0,1x,a,1x,g0,a)')                                 &
            'EPSILON for uzf cell', i,                                           &
            'must be between 3.5 and 14.0 (specified value is', eps, ').'
          call store_error(errmsg)
        end if
        !
        ! -- boundname
        if (this%inamedbound == 1) then
          call this%parser%GetStringCaps(this%uzfname(i))
        endif
        !
        ! -- set data if there are no data errors
        if (count_errors() == 0) then
          n = this%igwfnode(i)
          call this%uzfobj%setdata(i, this%gwfarea(n), this%gwftop(n),           &
                                   this%gwfbot(n), surfdep, vks, thtr, thts,     &
                                   thti, eps, this%ntrail, landflag, ivertcon)
          if (ivertcon > 0) then
            this%iuzf2uzf = 1
          end if
        end if
       !
      end do
    else
      call store_error('Required packagedata block not found.')
    end if
    !
    ! -- check for duplicate or missing uzf cells
    do i = 1, this%nodes
      if (nboundchk(i) == 0) then
        write(errmsg,'(a,1x,i0,a)')                                              &
          'No data specified for uzf cell', i, '.'
        call store_error(errmsg)
      else if (nboundchk(i) > 1) then
        write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                                &
          'Data for uzf cell', i, 'specified', nboundchk(i), 'times.'
        call store_error(errmsg)
      end if
    end do
    !
    ! -- write summary of UZF cell property error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- setup sparse for connectivity used to identify multiple uzf cells per
    !    GWF model cell
    call sparse%init(this%dis%nodes, this%dis%nodes, rowmaxnnz)
    ! --
    do i = 1, this%nodes
      ic = this%igwfnode(i)
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
!                          UZFCellGroup type.
! ******************************************************************************
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType), intent(inout) :: this
    ! -- local
    character (len=20) :: cellid
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: tag
    integer(I4B) :: ntabrows
    integer(I4B) :: ntabcols
    integer(I4B) :: i
    integer(I4B) :: node
! ------------------------------------------------------------------------------
!
    !
    ! -- setup inputtab tableobj
    !
    ! -- table dimensions
    ntabrows = this%nodes
    ntabcols = 10
    if (this%inamedbound == 1) then
      ntabcols = ntabcols + 1
    end if
    !
    ! -- initialize table and define columns
    title = trim(adjustl(this%text)) // ' PACKAGE (' //                        &
            trim(adjustl(this%packName)) //') STATIC UZF CELL DATA'
    call table_cr(this%inputtab, this%packName, title)
    call this%inputtab%table_df(ntabrows, ntabcols, this%iout)
    tag = 'NUMBER'
    call this%inputtab%initialize_column(tag, 10)
    tag = 'CELLID'
    call this%inputtab%initialize_column(tag, 20, alignment=TABLEFT)
    tag = 'LANDFLAG'
    call this%inputtab%initialize_column(tag, 12)
    tag = 'IVERTCON'
    call this%inputtab%initialize_column(tag, 12)
    tag = 'SURFDEP'
    call this%inputtab%initialize_column(tag, 12)
    tag = 'VKS'
    call this%inputtab%initialize_column(tag, 12)
    tag = 'THTR'
    call this%inputtab%initialize_column(tag, 12)
    tag = 'THTS'
    call this%inputtab%initialize_column(tag, 12)
    tag = 'THTI'
    call this%inputtab%initialize_column(tag, 12)
    tag = 'EPS'
    call this%inputtab%initialize_column(tag, 12)
    if (this%inamedbound == 1) then
      tag = 'BOUNDNAME'
      call this%inputtab%initialize_column(tag, LENBOUNDNAME, alignment=TABLEFT)
    end if
    !
    ! -- write data for each cell
    do i = 1, this%nodes
      !
      ! -- get cellid
      node = this%igwfnode(i)
      if (node > 0) then
        call this%dis%noder_to_string(node, cellid)
      else
        cellid = 'none'
      end if
      !
      ! -- add data
      call this%inputtab%add_term(i)
      call this%inputtab%add_term(cellid)
      call this%inputtab%add_term(this%uzfobj%landflag(i))
      call this%inputtab%add_term(this%uzfobj%ivertcon(i))
      call this%inputtab%add_term(this%uzfobj%surfdep(i))
      call this%inputtab%add_term(this%uzfobj%vks(i))
      call this%inputtab%add_term(this%uzfobj%thtr(i))
      call this%inputtab%add_term(this%uzfobj%thts(i))
      call this%inputtab%add_term(this%uzfobj%thti(i))
      call this%inputtab%add_term(this%uzfobj%eps(i))
      if (this%inamedbound == 1) then
        call this%inputtab%add_term(this%uzfname(i))
      end if
    end do
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
    character(len=16) :: cuzf
    character(len=20) :: cellid
    character(len=LINELENGTH) :: cuzfcells
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
      i2 = this%uzfobj%ivertcon(i)
      area = this%uzfobj%uzfarea(i)
      !
      ! Create pointer to object below
      if (i2 > 0) then
        area2 = this%uzfobj%uzfarea(i2)
        d = abs(area - area2)
        if (d > DEM6) then
          write(errmsg,'(2(a,1x,g0,1x,a,1x,i0,1x),a)')                           &
            'UZF cell area (', area, ') for cell ', i,                           &
            'does not equal uzf cell area (', area2, ') for cell ', i2, '.'
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
        sumarea = sumarea + this%uzfobj%uzfarea(i)
        cellarea = this%uzfobj%cellarea(i)
      end do
      ! -- calculate the difference between the sum of UZF areas and GWF cell area
      d = abs(sumarea - cellarea)
      if (d > DEM6) then
        call this%dis%noder_to_string(n, cellid)
        write(errmsg,'(a,1x,g0,1x,a,1x,g0,1x,a,1x,a,1x,a,a,a)')                  &
          'Total uzf cell area (', sumarea,                                      &
          ') exceeds the gwf cell area (', cellarea, ') of cell', cellid,        &
          'which includes uzf cell(s): ', trim(adjustl(cuzfcells)), '.'
        call store_error(errmsg)
      end if
    end do
    !
    ! -- terminate if errors were encountered
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    ! -- return
    return
  end subroutine check_cell_area

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
    integer(I4B) :: i
    integer(I4B) :: ii
    integer(I4B) :: n
    real(DP) :: v
    type(ObserveType), pointer :: obsrv => null()
    !---------------------------------------------------------------------------
    !
    ! Write simulated values for all uzf observations
    if (this%obs%npakobs > 0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        do ii = 1, obsrv%indxbnds_count
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
              v = this%infiltration(n)
            case ('WATER-CONTENT')
              v = this%obs_theta(i)  ! more than one obs per node
            case default
              errmsg = 'Unrecognized observation type: ' // trim(obsrv%ObsTypeId)
              call store_error(errmsg)
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      end do
      !
      ! -- write summary of error messages
      if (count_errors() > 0) then
        call this%parser%StoreErrorUnit()
        call ustop()
      end if
    end if
    !
    ! -- return
    return
  end subroutine uzf_bd_obs
!
  subroutine uzf_rp_obs(this)
    use TdisModule, only: kper
    ! -- dummy
    class(UzfType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: n
    integer(I4B) :: nn
    real(DP) :: obsdepth
    real(DP) :: dmax
    character(len=LENBOUNDNAME) :: bname
    class(ObserveType),   pointer :: obsrv => null()
    ! --------------------------------------------------------------------------
    ! -- formats
60  format('Invalid node number in OBS input: ', i0)
    !
    ! -- process each package observation
    !    only done the first stress period since boundaries are fixed
    !    for the simulation
    if (kper == 1) then
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        !
        ! -- get node number 1
        nn = obsrv%NodeNumber
        if (nn == NAMEDBOUNDFLAG) then
          bname = obsrv%FeatureName
          !
          ! -- Observation location(s) is(are) based on a boundary name.
          !    Iterate through all boundaries to identify and store
          !    corresponding index(indices) in bound array.
          do j = 1, this%nodes
            if (this%boundname(j) == bname) then
              obsrv%BndFound = .true.
              obsrv%CurrentTimeStepEndValue = DZERO
              call obsrv%AddObsIndex(j)
              if (obsrv%indxbnds_count == 1) then
                !
                ! -- Define intPak1 so that obs_theta is stored (for first uzf
                !    cell if multiple cells share the same boundname).
                obsrv%intPak1 = j
              endif
            endif
          enddo
        else
          !
          ! -- get node number
          nn = obsrv%NodeNumber
          !
          ! -- put nn (a value meaningful only to UZF) in intPak1
          obsrv%intPak1 = nn
          ! -- check that node number is valid; call store_error if not
          if (nn < 1 .or. nn > this%nodes) then
            write (errmsg, 60) nn
            call store_error(errmsg)
          else
            obsrv%BndFound = .true.
          endif
          obsrv%CurrentTimeStepEndValue = DZERO
          call obsrv%AddObsIndex(nn)
        end if
        !
        ! -- catch non-cumulative observation assigned to observation defined
        !    by a boundname that is assigned to more than one element
        if (obsrv%ObsTypeId == 'WATER-CONTENT') then
          n = obsrv%indxbnds_count
          if (n > 1) then
            write (errmsg, '(a,3(1x,a))')                                        &
              trim(adjustl(obsrv%ObsTypeId)), 'for observation',                 &
              trim(adjustl(obsrv%Name)),                                         &
              'must be assigned to a UZF cell with a unique boundname.'
            call store_error(errmsg)
          end if
          !
          ! -- check WATER-CONTENT depth
          obsdepth = obsrv%Obsdepth
          !
          ! -- put obsdepth (a value meaningful only to UZF) in dblPak1
          obsrv%dblPak1 = obsdepth
          !
          ! -- determine maximum cell depth
          dmax = this%uzfobj%celtop(n) - this%uzfobj%celbot(n)
          ! -- check that obs depth is valid; call store_error if not
          ! -- need to think about a way to put bounds on this depth
          if (obsdepth < DZERO .or. obsdepth > dmax) then
            write (errmsg, '(a,3(1x,a),1x,g0,1x,a,1x,g0,a)')                       &
              trim(adjustl(obsrv%ObsTypeId)), 'for observation',                   &
              trim(adjustl(obsrv%Name)), 'specified depth (', obsdepth,            &
              ') must be between 0.0 and ', dmax, '.'
            call store_error(errmsg)
          endif
        else
          do j = 1, obsrv%indxbnds_count
            nn =  obsrv%indxbnds(j)
            if (nn < 1 .or. nn > this%maxbound) then
              write (errmsg, '(a,2(1x,a),1x,i0,1x,a,1x,i0,a)')                     &
                trim(adjustl(obsrv%ObsTypeId)), 'uzfno must be greater than 0 ',   &
                'and less than or equal to', this%maxbound,                        &
                '(specified value is ', nn, ').'
              call store_error(errmsg)
            end if
          end do
        end if
      end do
      !
      ! -- evaluate if there are any observation errors
      if (count_errors() > 0) then
        call store_error_unit(this%inunit)
        call ustop()
      end if
    end if
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
    call mem_allocate(this%iprwcont, 'IPRWCONT', this%memoryPath)
    call mem_allocate(this%iwcontout, 'IWCONTOUT', this%memoryPath)
    call mem_allocate(this%ibudgetout, 'IBUDGETOUT', this%memoryPath)
    call mem_allocate(this%ipakcsv, 'IPAKCSV', this%memoryPath)
    call mem_allocate(this%ntrail, 'NTRAIL', this%memoryPath)
    call mem_allocate(this%nsets, 'NSETS', this%memoryPath)
    call mem_allocate(this%nodes, 'NODES', this%memoryPath)
    call mem_allocate(this%istocb, 'ISTOCB', this%memoryPath)
    call mem_allocate(this%nwav, 'NWAV', this%memoryPath)
    call mem_allocate(this%outunitbud, 'OUTUNITBUD', this%memoryPath)
    call mem_allocate(this%totfluxtot, 'TOTFLUXTOT', this%memoryPath)
    call mem_allocate(this%infilsum, 'INFILSUM', this%memoryPath)
    call mem_allocate(this%uzetsum, 'UZETSUM', this%memoryPath)
    call mem_allocate(this%rechsum, 'RECHSUM', this%memoryPath)
    call mem_allocate(this%vfluxsum, 'VFLUXSUM', this%memoryPath)
    call mem_allocate(this%delstorsum, 'DELSTORSUM', this%memoryPath)
    call mem_allocate(this%bditems, 'BDITEMS', this%memoryPath)
    call mem_allocate(this%nbdtxt, 'NBDTXT', this%memoryPath)
    call mem_allocate(this%issflag, 'ISSFLAG', this%memoryPath)
    call mem_allocate(this%issflagold, 'ISSFLAGOLD', this%memoryPath)
    call mem_allocate(this%readflag, 'READFLAG', this%memoryPath)
    call mem_allocate(this%iseepflag, 'ISEEPFLAG', this%memoryPath)
    call mem_allocate(this%imaxcellcnt, 'IMAXCELLCNT', this%memoryPath)
    call mem_allocate(this%ietflag, 'IETFLAG', this%memoryPath)
    call mem_allocate(this%igwetflag, 'IGWETFLAG', this%memoryPath)
    call mem_allocate(this%iuzf2uzf, 'IUZF2UZF', this%memoryPath)
    call mem_allocate(this%cbcauxitems, 'CBCAUXITEMS', this%memoryPath)

    call mem_allocate(this%iconvchk, 'ICONVCHK', this%memoryPath)
    !
    ! -- initialize scalars
    this%iprwcont = 0
    this%iwcontout = 0
    this%ibudgetout = 0
    this%ipakcsv = 0
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
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- deallocate uzf objects
    call this%uzfobj%dealloc()
    call this%uzfobjwork%dealloc()

    nullify(this%uzfobj)
    call this%budobj%budgetobject_da()
    deallocate(this%budobj)
    nullify(this%budobj)
    !
    ! -- character arrays
    deallocate(this%bdtxt)
    deallocate(this%cauxcbc)
    deallocate(this%uzfname)
    !
    ! -- package csv table
    if (this%ipakcsv > 0) then
      call this%pakcsvtab%table_da()
      deallocate(this%pakcsvtab)
      nullify(this%pakcsvtab)
    end if
    !
    ! -- deallocate scalars
    call mem_deallocate(this%iprwcont)
    call mem_deallocate(this%iwcontout)
    call mem_deallocate(this%ibudgetout)
    call mem_deallocate(this%ipakcsv)
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
    !
    ! -- deallocate arrays
    call mem_deallocate(this%igwfnode)
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
    call mem_deallocate(this%uauxvar)
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

  subroutine uzf_setup_budobj(this)
! ******************************************************************************
! uzf_setup_budobj -- Set up the budget object that stores all the uzf flows
!   The terms listed here must correspond in number and order to the ones 
!   listed in the uzf_fill_budobj routine.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(UzfType) :: this
    ! -- local
    integer(I4B) :: nbudterm
    integer(I4B) :: maxlist, naux
    integer(I4B) :: idx
    integer(I4B) :: nlen
    integer(I4B) :: n, n1, n2
    integer(I4B) :: ivertflag
    real(DP) :: q
    character(len=LENBUDTXT) :: text
    character(len=LENBUDTXT), dimension(1) :: auxtxt
! ------------------------------------------------------------------------------
    !
    ! -- Determine the number of uzf to uzf connections
    nlen = 0
    do n = 1, this%nodes
      ivertflag = this%uzfobj%ivertcon(n)
      if (ivertflag > 0) then
        nlen = nlen + 1
      end if
    end do
    !
    ! -- Determine the number of uzf budget terms. These are fixed for 
    !    the simulation and cannot change.  This includes FLOW-JA-FACE
    !    so they can be written to the binary budget files, but these internal
    !    flows are not included as part of the budget table.
    nbudterm = 4
    if (nlen > 0) nbudterm = nbudterm + 1
    if (this%ietflag /= 0) nbudterm = nbudterm + 1
    if (this%imover == 1) nbudterm = nbudterm + 2
    if (this%naux > 0) nbudterm = nbudterm + 1
    !
    ! -- set up budobj
    call budgetobject_cr(this%budobj, this%packName)
    call this%budobj%budgetobject_df(this%maxbound, nbudterm, 0, 0)
    idx = 0
    !
    ! -- Go through and set up each budget term
    text = '    FLOW-JA-FACE'
    if (nlen > 0) then
      idx = idx + 1
      maxlist = nlen * 2
      naux = 1
      auxtxt(1) = '       FLOW-AREA'
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%packName, &
                                               this%name_model, &
                                               this%packName, &
                                               maxlist, .false., .false., &
                                               naux, auxtxt, ordered_id1=.false.)
      !
      ! -- store connectivity
      call this%budobj%budterm(idx)%reset(nlen * 2)
      q = DZERO
      do n = 1, this%nodes
        ivertflag = this%uzfobj%ivertcon(n)
        if (ivertflag > 0) then
          n1 = n
          n2 = ivertflag
          call this%budobj%budterm(idx)%update_term(n1, n2, q)
          call this%budobj%budterm(idx)%update_term(n2, n1, -q)
        end if
      end do
    end if
    !
    ! -- 
    text = '             GWF'
    idx = idx + 1
    maxlist = this%nodes 
    naux = 1
    auxtxt(1) = '       FLOW-AREA'
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%packName, &
                                             this%name_model, &
                                             this%name_model, &
                                             maxlist, .false., .true., &
                                             naux, auxtxt)
    call this%budobj%budterm(idx)%reset(this%nodes)
    q = DZERO
    do n = 1, this%nodes
      n2 = this%igwfnode(n)
      call this%budobj%budterm(idx)%update_term(n, n2, q)
    end do
    !
    ! -- 
    text = '    INFILTRATION'
    idx = idx + 1
    maxlist = this%nodes
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%packName, &
                                             this%name_model, &
                                             this%packName, &
                                             maxlist, .false., .false., &
                                             naux)
    !
    ! -- 
    text = '         REJ-INF'
    idx = idx + 1
    maxlist = this%nodes
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%packName, &
                                             this%name_model, &
                                             this%packName, &
                                             maxlist, .false., .false., &
                                             naux)
    !
    ! -- 
    text = '            UZET'
    if (this%ietflag /= 0) then
      idx = idx + 1
      maxlist = this%maxbound
      naux = 0
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%packName, &
                                               this%name_model, &
                                               this%packName, &
                                               maxlist, .false., .false., &
                                               naux)
    end if
    !
    ! -- 
    text = '         STORAGE'
    idx = idx + 1
    maxlist = this%nodes
    naux = 1
    auxtxt(1) = '          VOLUME'
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%packName, &
                                             this%name_model, &
                                             this%packName, &
                                             maxlist, .false., .false., &
                                             naux, auxtxt)
    !
    ! -- 
    if (this%imover == 1) then
      !
      ! -- 
      text = '        FROM-MVR'
      idx = idx + 1
      maxlist = this%nodes
      naux = 0
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%packName, &
                                               this%name_model, &
                                               this%packName, &
                                               maxlist, .false., .false., &
                                               naux)
      !
      ! -- 
      text = '  REJ-INF-TO-MVR'
      idx = idx + 1
      maxlist = this%nodes
      naux = 0
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%packName, &
                                               this%name_model, &
                                               this%packName, &
                                               maxlist, .false., .false., &
                                               naux)
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
                                               this%packName, &
                                               this%name_model, &
                                               this%packName, &
                                               maxlist, .false., .false., &
                                               naux, this%auxname)
    end if
    !
    ! -- if uzf flow for each reach are written to the listing file
    if (this%iprflow /= 0) then
      call this%budobj%flowtable_df(this%iout, cellids='GWF')
    end if
    !
    ! -- return
    return
  end subroutine uzf_setup_budobj

  subroutine uzf_fill_budobj(this)
! ******************************************************************************
! uzf_fill_budobj -- copy flow terms into this%budobj
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(UzfType) :: this
    ! -- local
    integer(I4B) :: naux
    integer(I4B) :: nlen
    integer(I4B) :: ivertflag
    integer(I4B) :: n, n1, n2
    integer(I4B) :: idx
    real(DP) :: q
    real(DP) :: a
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: thick
    real(DP) :: fm
    real(DP) :: v
    ! -- formats
! -----------------------------------------------------------------------------
    !
    ! -- initialize counter
    idx = 0
    !
    ! -- FLOW JA FACE
    nlen = 0
    do n = 1, this%nodes
      ivertflag = this%uzfobj%ivertcon(n)
      if ( ivertflag > 0 ) then
        nlen = nlen + 1
      end if
    end do
    if (nlen > 0) then
      idx = idx + 1
      call this%budobj%budterm(idx)%reset(nlen * 2)
      do n = 1, this%nodes
        ivertflag = this%uzfobj%ivertcon(n)
        if (ivertflag > 0) then
          a = this%uzfobj%uzfarea(n)
          q = this%uzfobj%surfluxbelow(n) * a
          this%qauxcbc(1) = a
          if (q > DZERO) then
            q = -q
          end if
          n1 = n
          n2 = ivertflag
          call this%budobj%budterm(idx)%update_term(n1, n2, q, this%qauxcbc)
          call this%budobj%budterm(idx)%update_term(n2, n1, -q, this%qauxcbc)
        end if
      end do
    end if
    !
    ! -- GWF (LEAKAGE)
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%nodes)
    do n = 1, this%nodes
      this%qauxcbc(1) = this%uzfobj%uzfarea(n)
      n2 = this%igwfnode(n)
      q = -this%rch(n)
      call this%budobj%budterm(idx)%update_term(n, n2, q, this%qauxcbc)
    end do
    !
    ! -- INFILTRATION
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%nodes)
    do n = 1, this%nodes
      q = this%appliedinf(n)
      call this%budobj%budterm(idx)%update_term(n, n, q)
    end do
    !
    ! -- REJECTED INFILTRATION
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%nodes)
    do n = 1, this%nodes
      q = this%rejinf(n)
      if (q > DZERO) then
        q = -q
      end if
      call this%budobj%budterm(idx)%update_term(n, n, q)
    end do
    !
    ! -- UNSATURATED EVT
    if (this%ietflag /= 0) then
      idx = idx + 1
      call this%budobj%budterm(idx)%reset(this%nodes)
      do n = 1, this%nodes
        q = this%uzet(n)
        if (q > DZERO) then
          q = -q
        end if
        call this%budobj%budterm(idx)%update_term(n, n, q)
      end do
    end if
    !
    ! -- STORAGE
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%nodes)
    do n = 1, this%nodes
      q = -this%qsto(n)
      top = this%uzfobj%celtop(n)
      bot = this%uzfobj%watab(n)
      thick = top - bot
      if (thick > DZERO) then
        fm = this%uzfobj%unsat_stor(n, thick)
        v = fm * this%uzfobj%uzfarea(n)
      else
        v = DZERO
      end if
      this%qauxcbc(1) = v
      call this%budobj%budterm(idx)%update_term(n, n, q, this%qauxcbc)
    end do
    !
    ! -- MOVER
    if (this%imover == 1) then
      !
      ! -- FROM MOVER
      idx = idx + 1
      call this%budobj%budterm(idx)%reset(this%nodes)
      do n = 1, this%nodes
        q = this%pakmvrobj%get_qfrommvr(n)
        call this%budobj%budterm(idx)%update_term(n, n, q)
      end do
      !
      ! -- REJ-INF-TO-MVR
      idx = idx + 1
      call this%budobj%budterm(idx)%reset(this%nodes)
      do n = 1, this%nodes
        q = this%rejinftomvr(n)
        if (q > DZERO) then
          q = -q
        end if
        call this%budobj%budterm(idx)%update_term(n, n, q)
      end do
      
    end if
    !
    ! -- AUXILIARY VARIABLES
    naux = this%naux
    if (naux > 0) then
      idx = idx + 1
      call this%budobj%budterm(idx)%reset(this%nodes)
      do n = 1, this%nodes
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
  end subroutine uzf_fill_budobj

end module UzfModule

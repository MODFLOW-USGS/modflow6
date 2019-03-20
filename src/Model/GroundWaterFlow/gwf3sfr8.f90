module SfrModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, LENBOUNDNAME, LENTIMESERIESNAME,      &
                             DZERO, DPREC, DEM30, DEM6, DEM5, DEM4, DEM2,      &
                             DHALF, DP6, DTWOTHIRDS, DP7, DP9, DP99, DP999,    &
                             DONE, D1P1, DFIVETHIRDS, DTWO, DPI, DEIGHT,       &
                             DHUNDRED, DEP20,                                  &
                             NAMEDBOUNDFLAG, LENBOUNDNAME, LENFTYPE,           &
                             LENPACKAGENAME, MAXCHARLEN,                       &
                             DHNOFLO, DHDRY, DNODATA
  use SmoothingModule,  only: sQuadraticSaturation, sQSaturation, &
                              sQuadraticSaturationDerivative, sQSaturationDerivative, &
                              sCubicSaturation, sChSmooth
  use BndModule, only: BndType
  use BudgetModule, only : BudgetType

  use ObserveModule,        only: ObserveType
  use ObsModule, only: ObsType
  use InputOutputModule, only: get_node, URWORD, extract_idnum_or_bndname
  use BaseDisModule, only: DisBaseType
  use SimModule,        only: count_errors, store_error, store_error_unit, ustop
  use SparseModule, only: sparsematrix
  use RectangularChGeometryModule,       only: RectangularChGeometryType
  use ArrayHandlersModule, only: ExpandArray
  use BlockParserModule,   only: BlockParserType
  !
  implicit none
  !
  character(len=LENFTYPE)       :: ftype = 'SFR'
  character(len=LENPACKAGENAME) :: text  = '             SFR'
  !
  ! -- timeseries type for
  type :: SfrTSType
    character (len=LENTIMESERIESNAME), pointer :: name => null()
    real(DP), pointer :: value => null()
  end type SfrTSType
  !
  type :: SfrDivType
    integer(I4B), pointer :: reach => null()
    integer(I4B), pointer :: iprior => null()
    character (len=10), pointer :: cprior => null()
    type (SfrTSType), pointer :: rate => null()
  end type SfrDivType
  !
  ! -- Streamflow Routing derived data type
  type :: SfrDataType
    character (len=8), pointer :: status => null()
    integer(I4B), pointer :: iboundpak => null()
    integer(I4B), pointer :: reach => null()
    integer(I4B), pointer :: igwfnode => null()
    integer(I4B), pointer :: igwftopnode => null()
    real(DP), pointer :: length => null()
    real(DP), pointer :: width => null()
    real(DP), pointer :: strtop => null()
    real(DP), pointer :: bthick => null()
    real(DP), pointer :: hk => null()
    real(DP), pointer :: slope => null()
    integer(I4B), pointer :: nconn => null()
    real(DP), pointer :: ustrf => null()
    real(DP), pointer :: ftotnd => null()
    ! -- diversion data
    integer(I4B), pointer :: ndiv => null()
    type (SfrDivType), dimension(:), pointer, contiguous :: diversion => null()
    ! -- aux data
    type (SfrTSType), dimension(:), pointer, contiguous :: auxvar => null()
    ! -- boundary data
    type (SfrTSType), pointer :: rough => null()
    type (SfrTSType), pointer :: rain => null()
    type (SfrTSType), pointer :: evap => null()
    type (SfrTSType), pointer :: inflow => null()
    type (SfrTSType), pointer :: runoff => null()
    type (SfrTSType), pointer :: sstage => null()
    ! -- dependent variables
    real(DP), pointer :: usflow => null()
    real(DP), pointer :: dsflow => null()
    real(DP), pointer :: depth => null()
    real(DP), pointer :: stage => null()
    real(DP), pointer :: gwflow => null()
    real(DP), pointer :: simevap => null()
    real(DP), pointer :: simrunoff => null()
    real(DP), pointer :: stage0 => null()
    real(DP), pointer :: usflow0 => null()
    ! -- arrays of data for reach
    integer(I4B), dimension(:), pointer, contiguous :: iconn => null()
    integer(I4B), dimension(:), pointer, contiguous :: idir => null()
    integer(I4B), dimension(:), pointer, contiguous :: idiv => null()
    ! -- double precision arrays for reach
    real(DP), dimension(:), pointer, contiguous :: qconn => null()
  end type SfrDataType
  !
  private
  public :: sfr_create
  !
  type, extends(BndType) :: SfrType
    ! -- scalars
    ! -- for budgets
    ! -- characters
    character(len=16), dimension(:), pointer, contiguous :: csfrbudget => NULL()
    character(len=16), dimension(:), pointer, contiguous :: cauxcbc => NULL()
    ! -- integers
    integer(I4B), pointer :: iprhed => null()
    integer(I4B), pointer :: istageout => null()
    integer(I4B), pointer :: ibudgetout => null()
    integer(I4B), pointer :: idiversions => null()
    integer(I4B), pointer :: nconn => NULL()
    integer(I4B), pointer :: maxsfrit => NULL()
    integer(I4B), pointer :: bditems => NULL()
    integer(I4B), pointer :: cbcauxitems => NULL()
    integer(I4B), pointer :: icheck => NULL()
    integer(I4B), pointer :: iconvchk => NULL()
    integer(I4B), pointer :: gwfiss => NULL()
    ! -- double precision
    real(DP), pointer :: unitconv => NULL()
    real(DP), pointer :: dmaxchg => NULL()
    real(DP), pointer :: deps => NULL()
    ! -- integer vectors
    integer(I4B), dimension(:), pointer, contiguous :: ia => null()
    integer(I4B), dimension(:), pointer, contiguous :: ja => null()
    ! -- double precision output vectors
    real(DP), dimension(:), pointer, contiguous :: qoutflow => null()
    real(DP), dimension(:), pointer, contiguous :: qextoutflow => null()
    real(DP), dimension(:), pointer, contiguous :: qauxcbc => null()
    real(DP), dimension(:), pointer, contiguous :: dbuff => null()
    ! -- derived types
    type(BudgetType), pointer :: budget => NULL()
    type(SfrDataType), dimension(:), pointer, contiguous :: reaches => NULL()
    type(sparsematrix), pointer :: sparse => null()
    type(RectangularChGeometryType), dimension(:), pointer,                     &
                                     contiguous :: geo => null()
    ! -- type bound procedures
    contains
    procedure :: sfr_allocate_scalars
    procedure :: sfr_allocate_arrays
    procedure :: bnd_options => sfr_options
    procedure :: read_dimensions => sfr_read_dimensions
    procedure :: set_pointers => sfr_set_pointers
    procedure :: bnd_ar => sfr_ar
    procedure :: bnd_rp => sfr_rp
    procedure :: bnd_ad => sfr_ad
    procedure :: bnd_cf => sfr_cf
    procedure :: bnd_fc => sfr_fc
    procedure :: bnd_fn => sfr_fn
    procedure :: bnd_cc => sfr_cc
    procedure :: bnd_bd => sfr_bd
    procedure :: bnd_ot => sfr_ot
    procedure :: bnd_da => sfr_da
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => sfr_obs_supported
    procedure, public :: bnd_df_obs => sfr_df_obs
    procedure, public :: bnd_rp_obs => sfr_rp_obs
    procedure, private :: sfr_bd_obs
    ! -- private procedures
    procedure, private :: allocate_reach
    procedure, private :: deallocate_reach
    procedure, private :: allocate_diversion
    procedure, private :: deallocate_diversion
    procedure, private :: sfr_set_stressperiod
    procedure, private :: sfr_solve
    procedure, private :: sfr_update_flows
    procedure, private :: sfr_calc_qgwf
    procedure, private :: sfr_calc_cond
    procedure, private :: sfr_calc_qman
    procedure, private :: sfr_calc_qd
    procedure, private :: sfr_calc_qsource
    procedure, private :: sfr_calc_div
    ! -- calculations
    procedure, private :: sfr_rectch_depth
    ! -- error checking
    procedure, private :: sfr_check_reaches
    procedure, private :: sfr_check_connections
    procedure, private :: sfr_check_diversions
    procedure, private :: sfr_check_ustrf
  end type SfrType

contains

  subroutine sfr_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! sfr_create -- Create a New Streamflow Routing Package
! Subroutine: (1) create new-style package
!             (2) point bndobj to the new package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B),intent(in) :: id
    integer(I4B),intent(in) :: ibcnum
    integer(I4B),intent(in) :: inunit
    integer(I4B),intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    ! -- local
    type(SfrType), pointer :: sfrobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(sfrobj)
    packobj => sfrobj
    !
    ! -- create name and origin
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call sfrobj%sfr_allocate_scalars()
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
  end subroutine sfr_create

  subroutine sfr_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(SfrType),   intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- allocate the object and assign values to object variables
    call mem_allocate(this%iprhed, 'IPRHED', this%origin)
    call mem_allocate(this%istageout, 'ISTAGEOUT', this%origin)
    call mem_allocate(this%ibudgetout, 'IBUDGETOUT', this%origin)
    call mem_allocate(this%idiversions, 'IDIVERSIONS', this%origin)
    call mem_allocate(this%maxsfrit, 'MAXSFRIT', this%origin)
    call mem_allocate(this%bditems, 'BDITEMS', this%origin)
    call mem_allocate(this%cbcauxitems, 'CBCAUXITEMS', this%origin)
    call mem_allocate(this%unitconv, 'UNITCONV', this%origin)
    call mem_allocate(this%dmaxchg, 'DMAXCHG', this%origin)
    call mem_allocate(this%deps, 'DEPS', this%origin)
    call mem_allocate(this%nconn, 'NCONN', this%origin)
    call mem_allocate(this%icheck, 'ICHECK', this%origin)
    call mem_allocate(this%iconvchk, 'ICONVCHK', this%origin)
    !
    ! -- set pointer to gwf iss
    call mem_setptr(this%gwfiss, 'ISS', trim(this%name_model))
    !
    ! -- Set values
    this%iprhed = 0
    this%istageout = 0
    this%ibudgetout = 0
    this%idiversions = 0
    this%maxsfrit = 100
    this%bditems = 8
    this%cbcauxitems = 1
    this%unitconv = DONE
    this%dmaxchg = DEM5
    this%deps = DP999 * this%dmaxchg
    !this%imover = 0
    this%nconn = 0
    this%icheck = 1
    this%iconvchk = 1
    !
    ! -- return
    return
  end subroutine sfr_allocate_scalars

  subroutine sfr_allocate_arrays(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(SfrType),   intent(inout) :: this
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_arrays()
    !
    ! -- allocate character array for budget text
    allocate(this%csfrbudget(this%bditems))
    !
    !
    !-- fill csfrbudget
    this%csfrbudget(1) = '        RAINFALL'
    this%csfrbudget(2) = '     EVAPORATION'
    this%csfrbudget(3) = '          RUNOFF'
    this%csfrbudget(4) = '      EXT-INFLOW'
    this%csfrbudget(5) = '             GWF'
    this%csfrbudget(6) = '     EXT-OUTFLOW'
    this%csfrbudget(7) = '        FROM-MVR'
    this%csfrbudget(8) = '          TO-MVR'
    !
    ! -- allocate and initialize budget output data
    call mem_allocate(this%qoutflow, this%maxbound, 'QOUTFLOW', this%origin)
    call mem_allocate(this%qextoutflow, this%maxbound, 'QEXTOUTFLOW', this%origin)
    do i = 1, this%maxbound
      this%qoutflow(i) = DZERO
      this%qextoutflow(i) = DZERO
    end do
    !
    ! -- allocate and initialize dbuff
    if (this%istageout > 0) then
      call mem_allocate(this%dbuff, this%maxbound, 'DBUFF', this%origin)
      do i = 1, this%maxbound
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
    !-- fill cauxcbc
    this%cauxcbc(1) = 'FLOW-AREA       '
    !
    ! -- return
    return
  end subroutine sfr_allocate_arrays

  subroutine sfr_read_dimensions(this)
! ******************************************************************************
! pak1read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(SfrType),intent(inout) :: this
    ! -- local
    character (len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- initialize dimensions to 0
    this%maxbound = 0
    !
    ! -- get dimensions block
    call this%parser%GetBlock('DIMENSIONS', isFound, ierr, &
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
          case ('NREACHES')
            this%maxbound = this%parser%GetInteger()
            write(this%iout,'(4x,a,i0)')'NREACHES = ', this%maxbound
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
    !
    ! -- verify dimensions were set
    if(this%maxbound < 1) then
      write(errmsg, '(1x,a)') &
        'ERROR.  NREACHES WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.'
      call store_error(errmsg)
    endif
    !
    ! -- write summary of error messages for block
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
  end subroutine sfr_read_dimensions

  subroutine sfr_options(this, option, found)
! ******************************************************************************
! rch_options -- set options specific to RchType
!
! rch_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DZERO
    use OpenSpecModule, only: access, form
    use SimModule, only: ustop, store_error
    use InputOutputModule, only: urword, getunit, openfile
    ! -- dummy
    class(SfrType),   intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical,          intent(inout) :: found
    ! -- local
    real(DP) :: r
    character(len=MAXCHARLEN) :: fname, keyword
    ! -- formats
    character(len=*),parameter :: fmtunitconv = &
      "(4x, 'UNIT CONVERSION VALUE (',g15.7,') SPECIFIED.')"
    character(len=*),parameter :: fmtiter = &
      "(4x, 'MAXIMUM SFR ITERATION VALUE (',i15,') SPECIFIED.')"
    character(len=*),parameter :: fmtdmaxchg = &
      "(4x, 'MAXIMUM DEPTH CHANGE VALUE (',g15.7,') SPECIFIED.')"
    character(len=*),parameter :: fmtsfrbin = &
      "(4x, 'SFR ', 1x, a, 1x, ' WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
! ------------------------------------------------------------------------------
    !
    ! -- Check for SFR options
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
          write(this%iout,fmtsfrbin) 'STAGE', fname, this%istageout
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
          write(this%iout,fmtsfrbin) 'BUDGET', fname, this%ibudgetout
          found = .true.
        else
          call store_error('OPTIONAL BUDGET KEYWORD MUST BE FOLLOWED BY FILEOUT')
        end if
      case('UNIT_CONVERSION')
        this%unitconv = this%parser%GetDouble()
        write(this%iout, fmtunitconv) this%unitconv
        found = .true.
      case('MAXIMUM_ITERATIONS')
        this%maxsfrit = this%parser%GetInteger()
        write(this%iout, fmtiter) this%maxsfrit
        found = .true.
      case('MAXIMUM_DEPTH_CHANGE')
        r = this%parser%GetDouble()
        this%dmaxchg = r
        this%deps = DP999 * r
        write(this%iout, fmtdmaxchg) this%dmaxchg
        found = .true.
      case('MOVER')
        this%imover = 1
        write(this%iout, '(4x,A)') 'MOVER OPTION ENABLED'
        found = .true.
      !
      ! -- right now these are options that are only available in the
      !    development version and are not included in the documentation.
      !    These options are only available when IDEVELOPMODE in
      !    constants module is set to 1
      case('DEV_NO_CHECK')
        call this%parser%DevOpt()
        this%icheck = 0
        write(this%iout, '(4x,A)') 'SFR CHECKS OF REACH GEOMETRY ' //         &
                                   'RELATIVE TO MODEL GRID AND ' //           &
                                   'REASONABLE PARAMETERS WILL NOT ' //       &
                                   'BE PERFORMED.'
        found = .true.
      case('DEV_NO_FINAL_CHECK')
        call this%parser%DevOpt()
        this%iconvchk = 0
        write(this%iout, '(4x,a)')                                             &
     &    'A FINAL CONVERGENCE CHECK OF THE CHANGE IN STREAM FLOW ROUTING ' // &
     &    'STAGES AND FLOWS WILL NOT BE MADE'
        found = .true.
      !
      ! -- no valid options found
      case default
        !
        ! -- No options found
        found = .false.
    end select
    !
    ! -- return
    return
  end subroutine sfr_options

  subroutine sfr_ar(this)
  ! ******************************************************************************
  ! sfr_ar -- Allocate and Read
  ! Subroutine: (1) create new-style package
  !             (2) point bndobj to the new package
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    use BudgetModule, only: budget_cr
    ! -- dummy
    class(SfrType),intent(inout) :: this
    ! -- local
    character (len=LINELENGTH) :: line, errmsg
    character(len=LINELENGTH) :: text, cellid, keyword
    character (len=10) :: cnum
    character (len=10) :: cval
    character(len=LENBOUNDNAME) :: bndName, bndNameTemp, manningname
    character(len=50), dimension(:), allocatable :: caux
    integer(I4B) :: j, n, ierr, ival
    integer(I4B) :: ipos
    integer(I4B) :: ndiv
    logical :: isfound, endOfBlock
    integer(I4B) :: i
    integer(I4B) :: jj
    integer(I4B) :: iaux
    integer(I4B) :: nja
    integer(I4B), dimension(:), pointer, contiguous :: rowmaxnnz => null()
    integer(I4B) :: idiv
    integer, allocatable, dimension(:) :: iachk
    integer, allocatable, dimension(:) :: nboundchk
    ! -- format
  ! ------------------------------------------------------------------------------
    !
    call this%obs%obs_ar()
    !
    ! -- Allocate arrays in package superclass
    call this%sfr_allocate_arrays()
    !
    ! -- addition
    !
    ! -- allocate space for sfr reach data
    allocate(this%reaches(this%maxbound))
    allocate(rowmaxnnz(this%maxbound))
    do i = 1, this%maxbound
      rowmaxnnz(i) = 0
    enddo 
    allocate(nboundchk(this%maxbound))
    do i = 1, this%maxbound
      nboundchk(i) = 0
    enddo 
    !
    ! -- allocate local storage for aux variables
    if (this%naux > 0) then
      allocate(caux(this%naux))
    end if
    !
    ! -- read reach data
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse reaches block if detected
    if (isfound) then
      nja = 0
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        ! -- read reach number
        n = this%parser%GetInteger()

        if (n < 1 .or. n > this%maxbound) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. REACH NUMBER (rno) MUST BE > 0 and <= ', this%maxbound
          call store_error(errmsg)
          cycle
        end if

        ! -- increment nboundchk
        nboundchk(n) = nboundchk(n) + 1

        ! -- allocate data for this reach
        call this%allocate_reach(n)
        ! -- set reach number
        this%reaches(n)%reach = n
        ! -- get model node number
        call this%parser%GetCellid(this%dis%ndim, cellid, flag_string=.true.)
        this%reaches(n)%igwfnode = this%dis%noder_from_cellid(cellid, &
                                   this%inunit, this%iout, flag_string=.true.)
        this%reaches(n)%igwftopnode = this%reaches(n)%igwfnode
        this%nodelist(n) = this%reaches(n)%igwfnode
        ! -- read the cellid string and determine if 'none' is specified
        if (this%reaches(n)%igwfnode < 1) then
          call this%parser%GetStringCaps(keyword)
          if (keyword .ne. 'NONE') then
            write (cnum, '(i0)') n
            errmsg = 'ERROR: cellid (' // trim(cellid) //                    &
                     ') for unconnected reach ' //  trim(cnum) // ' must be NONE'
            call store_error(errmsg)
          end if
        end if
        ! -- get reach length
        this%reaches(n)%length = this%parser%GetDouble()
        ! -- get reach width
        this%reaches(n)%width = this%parser%GetDouble()
        ! -- get reach slope
        this%reaches(n)%slope = this%parser%GetDouble()
        ! -- get reach stream bottom
        this%reaches(n)%strtop = this%parser%GetDouble()
        ! -- get reach bed thickness
        this%reaches(n)%bthick = this%parser%GetDouble()
        ! -- get reach bed hk
        this%reaches(n)%hk = this%parser%GetDouble()
        ! -- get reach roughness
        !this%reaches(n)%rough = this%parser%GetDouble()
        call this%parser%GetStringCaps(manningname)
        ! -- get number of connections for reach
        ival = this%parser%GetInteger()
        this%reaches(n)%nconn = ival
        this%nconn = this%nconn + ival
        if (ival > 0) then
          allocate(this%reaches(n)%iconn(ival))
          allocate(this%reaches(n)%idir(ival))
          allocate(this%reaches(n)%idiv(ival))
          allocate(this%reaches(n)%qconn(ival))
        else if (ival < 0) then
          ival = 0
        end if
        rowmaxnnz(n) = ival + 1
        nja = nja + ival + 1 !add the connections and the sfr reach
        ! -- get upstream fraction for reach
        this%reaches(n)%ustrf = this%parser%GetDouble()
        ! -- get number of diversions for reach
        ival = this%parser%GetInteger()
        this%reaches(n)%ndiv = ival
        if (ival > 0) then
          this%idiversions = 1
          call this%allocate_diversion(n, ival)
        else if (ival < 0) then
          ival = 0
        end if

        ! -- get aux data
        do iaux = 1, this%naux
          call this%parser%GetString(caux(iaux))
        end do

            ! -- set default bndName
        write (cnum,'(i10.10)') this%reaches(n)%reach
        bndName = 'Reach' // cnum

        ! -- get reach name
        if (this%inamedbound /= 0) then
          call this%parser%GetStringCaps(bndNameTemp)
          if (bndNameTemp /= '') then
            bndName = bndNameTemp(1:16)
          endif
          this%boundname(n) = bndName
        end if

        ! -- set Mannings
        text = manningname
        jj = 1 !iaux
        call read_single_value_or_time_series(text, &
                                              this%reaches(n)%rough%value, &
                                              this%reaches(n)%rough%name, &
                                              DZERO,  &
                                              this%Name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, &
                                              'MANNING', bndName, &
                                              this%parser%iuactive)


        ! -- get aux data
        do iaux = 1, this%naux
          text = caux(iaux)
          jj = 1 !iaux
          call read_single_value_or_time_series(text, &
                                                this%reaches(n)%auxvar(iaux)%value, &
                                                this%reaches(n)%auxvar(iaux)%name, &
                                                DZERO,  &
                                                this%Name, 'AUX', this%TsManager, &
                                                this%iprpak, n, jj, &
                                                this%auxname(iaux), bndName, &
                                                this%parser%iuactive)
        end do

        ! -- initialize sstage to the top of the reach
        !    this value would be used by simple routing reaches
        !    on kper = 1 and kstp = 1 if a stage is not specified
        !    on the status line for the reach
        this%reaches(n)%sstage%name = ''
        this%reaches(n)%sstage%value = this%reaches(n)%strtop

      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' PACKAGEDATA'
    else
      call store_error('ERROR.  REQUIRED PACKAGEDATA BLOCK NOT FOUND.')
    end if
    !
    ! -- Check to make sure that every reach is specified and that no reach
    !    is specified more than once.
    do i = 1, this%maxbound
      if (nboundchk(i) == 0) then
        write(errmsg, '(a, i0, a)') 'ERROR: INFORMATION FOR REACH ', i,        &
                                    ' NOT SPECIFIED IN PACKAGEDATA BLOCK.'
        call store_error(errmsg)
      else if (nboundchk(i) > 1) then
        write(errmsg, '(a, i0, i0)') 'ERROR: INFORMATION SPECIFIED ',          &
                                     nboundchk(i), ' TIMES FOR REACH ', i
        call store_error(errmsg)
      endif
    end do
    deallocate(nboundchk)
    !
    ! -- terminate if errors encountered in reach block
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- deallocate local storage for aux variables
    if (this%naux > 0) then
      deallocate(caux)
    end if
    !
    ! -- allocate and initialize local variables for reach connections
    allocate(nboundchk(this%maxbound))
    do n = 1, this%maxbound
      nboundchk(n) = 0
    end do
    !
    ! -- allocate space for connectivity
    allocate(this%sparse)
    !
    ! -- set up sparse
    
    call this%sparse%init(this%maxbound, this%maxbound, rowmaxnnz)
    !
    ! -- read connection data
    call this%parser%GetBlock('CONNECTIONDATA', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse reach connectivity block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' CONNECTIONDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        !
        ! -- get reach number
        n = this%parser%GetInteger()
        !
        ! -- check for error
        if(n < 1 .or. n > this%maxbound) then
          write(errmsg, '(a, i0)') 'SFR REACH LESS THAN ONE OR > NREACHES: ', n
          call store_error(errmsg)
          cycle
        endif
        !
        ! -- increment nboundchk
        if (this%reaches(n)%nconn > 0) then
          nboundchk(n) = nboundchk(n) + 1
        end if
        !
        ! -- add diagonal connection for reach
        call this%sparse%addconnection(n, n, 1)
        !
        ! -- fill off diagonals
        do i = 1, this%reaches(n)%nconn
          ival = this%parser%GetInteger()
          if (ival < 0) then
            this%reaches(n)%idir(i) = -1
            ival = abs(ival)
          elseif (ival == 0) then
            call store_error('Missing or zero connection reach in line:')
            call store_error(line)
          else
            this%reaches(n)%idir(i) = 1
          end if
          if (ival > this%maxbound) then
            call store_error('Reach number exceeds NREACHES in line:')
            call store_error(line)
          endif
          this%reaches(n)%iconn(i) = ival
          this%reaches(n)%idiv(i) = 0
          call this%sparse%addconnection(n, ival, 1)
        end do
      end do
      
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' CONNECTIONDATA'
      
      do n = 1, this%maxbound
        if (this%reaches(n)%nconn > 0) then
          !
          ! -- check for missing or duplicate sfr connections
          if (nboundchk(n) == 0) then
            write(errmsg,'(a,1x,i0)')                                             &
              'ERROR.  NO CONNECTION DATA SPECIFIED FOR REACH', n
            call store_error(errmsg)
          else if (nboundchk(n) > 1) then
            write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                             &
              'ERROR.  CONNECTION DATA FOR REACH', n,                             &
              'SPECIFIED', nboundchk(n), 'TIMES'
            call store_error(errmsg)
          end if
        end if
      end do
      
    else
      call store_error('ERROR.  REQUIRED CONNECTIONDATA BLOCK NOT FOUND.')
    end if
    !
    ! -- deallocate local storage for reach connections
    deallocate(nboundchk)
    !
    ! -- terminate if errors encountered in connectiondata block
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- allocate ia and ja for package
    allocate(this%ia(this%maxbound+1))
    allocate(this%ja(nja))
    !
    ! -- create ia and ja from sparse
    call this%sparse%filliaja(this%ia,this%ja,ierr)
    !
    ! -- deallocate temporary storage
    deallocate(rowmaxnnz)
    !
    ! -- destroy sparse
    call this%sparse%destroy()
    deallocate(this%sparse)
    !
    ! -- read diversions
    call this%parser%GetBlock('DIVERSIONS', isfound, ierr,                      &
                              supportOpenClose=.true.,                          &
                              blockRequired=.false.)
    !
    ! -- parse reach connectivity block if detected
    if (isfound) then
      if (this%idiversions /= 0) then
        write(this%iout,'(/1x,a)') 'PROCESSING ' // trim(adjustl(this%text)) // &
                                   ' DIVERSIONS'
        !
        ! -- allocate and initialize local variables for diversions
        ndiv = 0
        do n = 1, this%maxbound
          ndiv = ndiv + this%reaches(n)%ndiv
        end do
        allocate(iachk(this%maxbound+1))
        allocate(nboundchk(ndiv))
        iachk(1) = 1
        do n = 1, this%maxbound
          iachk(n+1) = iachk(n) + this%reaches(n)%ndiv
        end do
        do n = 1, ndiv
          nboundchk(n) = 0
        end do
        !
        ! -- read diversion data
        do
          call this%parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
          !
          ! -- get reach number
          n = this%parser%GetInteger()
          if (n < 1 .or. n > this%maxbound) then
            write (cnum, '(i0)') n
            errmsg = 'ERROR: reach number should be between 1 and ' //          &
                      trim(cnum) // '.'
            call store_error(errmsg)
            cycle
          end if
          !
          ! -- make sure reach has at least one diversion
          if (this%reaches(n)%ndiv < 1) then
            write (cnum, '(i0)') n
            errmsg = 'ERROR: diversions cannot be specified ' //                &
                     'for reach ' // trim(cnum)
            call store_error(errmsg)
            cycle
          end if
          !
          ! -- read diversion number
          ival = this%parser%GetInteger()
          if (ival < 1 .or. ival > this%reaches(n)%ndiv) then
            write (cnum, '(i0)') n
            errmsg = 'ERROR: reach  ' // trim(cnum)
            write (cnum, '(i0)') this%reaches(n)%ndiv
            errmsg = trim(errmsg) // ' diversion number should be between ' //  &
                     '1 and ' // trim(cnum) // '.'
            call store_error(errmsg)
            cycle
          end if
          
          ! -- increment nboundchk
          ipos = iachk(n) + ival - 1
          nboundchk(ipos) = nboundchk(ipos) + 1
          
          idiv = ival
          !
          ! -- get target reach for diversion
          ival = this%parser%GetInteger()
          if (ival < 1 .or. ival > this%maxbound) then
            write (cnum, '(i0)') ival
            errmsg = 'ERROR: diversion target reach number should be ' //       &
                     'between 1 and ' // trim(cnum) // '.'
            call store_error(errmsg)
            cycle
          end if
          this%reaches(n)%diversion(idiv)%reach = ival
          !
          ! -- get cprior
          call this%parser%GetStringCaps(cval)
          ival = -1
          select case (cval)
            case('UPTO')
              ival = 0
            case('THRESHOLD')
              ival = -1
            case('FRACTION')
              ival = -2
            case('EXCESS')
              ival = -3
            case default
              errmsg = 'ERROR: INVALID CPRIOR TYPE ' // trim(cval)
              call store_error(errmsg)
          end select
          this%reaches(n)%diversion(idiv)%cprior = cval
          this%reaches(n)%diversion(idiv)%iprior = ival

        end do
        
        write(this%iout,'(1x,a)') 'END OF ' // trim(adjustl(this%text)) //      &
                                  ' DIVERSIONS'
        
        do n = 1, this%maxbound
          do j = 1, this%reaches(n)%ndiv
            ipos = iachk(n) + j - 1
            !
            ! -- check for missing or duplicate reach diversions
            if (nboundchk(ipos) == 0) then
              write(errmsg,'(a,1x,i0,1x,a,1x,i0)')                              &
                'ERROR.  NO DATA SPECIFIED FOR REACH', n, 'DIVERSION', j
              call store_error(errmsg)
            else if (nboundchk(ipos) > 1) then
              write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a,1x,i0,1x,a)')              &
                'ERROR.  DATA FOR REACH', n, 'DIVERSION', j,                    &
                'SPECIFIED', nboundchk(ipos), 'TIMES'
              call store_error(errmsg)
            end if
          end do
        end do
        !
        ! -- deallocate local variables
        deallocate(iachk)
        deallocate(nboundchk)
      else
        !
        ! -- error condition
        write(errmsg,'(a,1x,a)') 'ERROR.  A DIVERSIONS BLOCK SHOULD NOT BE',    &
          'SPECIFIED IF DIVERSIONS ARE NOT SPECIFIED.'
          call store_error(errmsg)
      end if
    else
      if (this%idiversions /= 0) then
        call store_error('ERROR.  REQUIRED DIVERSIONS BLOCK NOT FOUND.')
      end if
    end if
    !
    ! -- write summary of package block error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- check the sfr data
    ! -- check the base sfr data
    call this%sfr_check_reaches()

    ! -- check the connection data
    call this%sfr_check_connections()

    ! -- check the diversion data
    if (this%idiversions /= 0) then
      call this%sfr_check_diversions()
    end if
    !
    ! -- calculate the total fraction of connected reaches that are
    !    not diversions
    call this%sfr_check_ustrf()
    !
    ! -- terminate if errors were detected in any of the static sfr data
    ierr = count_errors()
    if (ierr>0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- write header
    if (this%iprpak /= 0) then
      write (this%iout, '(//a)') 'SFR GEOMETRY DATA'
      write (this%iout, "(40('-'))")
    end if
    !
    ! -- build the rectangular geo type
    allocate(this%geo(this%maxbound))
    do n = 1, this%maxbound
      if(this%inamedbound==1) then
        bndName = this%boundname(n)
      else
        write (cnum,'(i10.0)') this%reaches(n)%reach
        bndName = 'Reach ' // trim(adjustl(cnum))
      end if
      call this%geo(n)%init(n, bndName, &
                            this%reaches(n)%width, &
                            this%reaches(n)%length)
      if (this%iprpak /= 0) then
        call this%geo(n)%print_attributes(this%iout)
      end if
    end do
    if (this%iprpak /= 0) then
      write (this%iout, "(40('-'))")
    end if
    !
    ! -- setup the sfr budget
    call budget_cr(this%budget, this%origin)
    ival = this%bditems
    call this%budget%budget_df(ival, this%name, 'L**3')
    !
    ! -- setup pakmvrobj
    if (this%imover /= 0) then
      allocate(this%pakmvrobj)
      call this%pakmvrobj%ar(this%maxbound, this%maxbound, this%origin)
    endif
    !
    ! -- return
    return
  end subroutine sfr_ar


  subroutine sfr_rp(this)
! ******************************************************************************
! sfr_rp -- Read and Prepare
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use TdisModule, only: kper, nper
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(SfrType),intent(inout) :: this
    ! -- local
    integer(I4B) :: ierr
    integer(I4B) :: n
    integer(I4B) :: ichkustrm
    logical :: isfound, endOfBlock
    integer(I4B) :: isfirst
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: errmsg
    ! -- formats
    character(len=*),parameter :: fmtblkerr = &
      "('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*),parameter :: fmtlsp = &
    &  "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
    character(len=*), parameter :: fmtnbd = &
      "(1X,/1X,'THE NUMBER OF ACTIVE ',A,'S (',I6, &
     &  ') IS GREATER THAN MAXIMUM(',I6,')')"
! ------------------------------------------------------------------------------
    !
    ! -- initialize flags
    ichkustrm =  0
    isfirst = 1
    !
    ! -- set nbound to maxbound
    this%nbound = this%maxbound
    !
    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
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
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
      endif
    end if
    !
    ! -- Read data if ionper == kper
    if(this%ionper==kper) then
      !
      ! -- read data
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        if (isfirst /= 0) then
          isfirst = 0
          if (this%iprpak /= 0) then
            write(this%iout,'(/1x,a,1x,i6,/)')                                  &
              'READING '//trim(adjustl(this%text))//                            &
              ' DATA FOR PERIOD', kper
            write(this%iout,'(3x,a)') '     REACH KEYWORD AND DATA'
            write(this%iout,'(3x,78("-"))')
          end if
        end if
        n = this%parser%GetInteger()
        if (n < 1 .or. n > this%maxbound) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. RNO MUST BE > 0 and <= ', this%maxbound
          call store_error(errmsg)
          cycle
        end if
        ! -- read data from the rest of the line
        call this%parser%GetRemainingLine(line)
        call this%sfr_set_stressperiod(n, line, ichkustrm)
      end do
      if (this%iprpak /= 0) then
        write(this%iout,'(/,1x,a,1x,i6,/)')                                     &
          'END OF '//trim(adjustl(this%text))//' DATA FOR PERIOD', kper
      end if
      !
      ! -- check upstream fraction values
      if (ichkustrm /= 0) then
        call this%sfr_check_ustrf()
      end if

    ! -- Reuse data from last stress period
    else
      write(this%iout,fmtlsp) trim(this%filtyp)
    endif
    !
    ! -- write summary of package block error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine sfr_rp

  subroutine sfr_ad(this)
! ******************************************************************************
! sfr_ad -- Add package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(SfrType) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Advance the time series manager
    call this%TsManager%ad()
    !
    ! -- reset upstream flow to zero and set specified stage
    do n = 1, this%maxbound
      this%reaches(n)%usflow = DZERO
      if (this%reaches(n)%iboundpak < 0) then
        this%reaches(n)%stage = this%reaches(n)%sstage%value
      end if
    end do
    !
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
  end subroutine sfr_ad

  subroutine sfr_cf(this)
  ! ******************************************************************************
  ! sfr_cf -- Formulate the HCOF and RHS terms
  ! Subroutine: (1) skip in no wells
  !             (2) calculate hcof and rhs
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      ! -- dummy variables
      class(SfrType) :: this
      ! -- local variables
      integer(I4B) :: n
      integer(I4B) :: igwfnode

  ! ------------------------------------------------------------------------------
    !
    ! -- Return if no sfr reaches
    if(this%nbound == 0) return
    !
    ! -- find highest active cell
    do n = 1, this%nbound
      igwfnode = this%reaches(n)%igwftopnode
      if (igwfnode > 0) then
        if (this%ibound(igwfnode) == 0) then
          call this%dis%highest_active(igwfnode, this%ibound)
        end if
      end if
      this%reaches(n)%igwfnode = igwfnode
      this%nodelist(n) = igwfnode
    end do
    !
    ! -- pakmvrobj cf
    if(this%imover == 1) then
      call this%pakmvrobj%cf()
    endif
    !
    ! -- return
    return
  end subroutine sfr_cf

  subroutine sfr_fc(this, rhs, ia, idxglo, amatsln)
  ! **************************************************************************
  ! sfr_fc -- Copy rhs and hcof into solution rhs and amat
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
    ! -- dummy
    class(SfrType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i, n
    integer(I4B) :: ipos
    integer(I4B) :: node
    real(DP) :: hgwf
    real(DP) :: v
    real(DP) :: hhcof
    real(DP) :: rrhs
! --------------------------------------------------------------------------
    !
    ! -- pakmvrobj fc
    if(this%imover == 1) then
      call this%pakmvrobj%fc()
    endif
    !
    ! -- solve for each sfr reach
    do n = 1, this%nbound
      node = this%reaches(n)%igwfnode
      if (node > 0) then
        hgwf = this%xnew(node)
      else
        hgwf = DEP20
      end if
      !
      ! -- save previous stage and upstream flow
      this%reaches(n)%stage0 = this%reaches(n)%stage
      this%reaches(n)%usflow0 = this%reaches(n)%usflow
      !
      ! -- solve for flow in swr
      if (this%reaches(n)%iboundpak /= 0) then
        call this%sfr_solve(n, hgwf, hhcof, rrhs)
      else
        this%reaches(n)%depth = DZERO
        this%reaches(n)%stage = this%reaches(n)%strtop
        v = DZERO
        call this%sfr_update_flows(n, v, v)
        hhcof = DZERO
        rrhs = DZERO
      end if
      this%hcof(n) = hhcof
      this%rhs(n) = rrhs
    end do
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      n = this%nodelist(i)
      if (n < 1) cycle
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + this%hcof(i)
    enddo
    !
    ! -- return
    return
  end subroutine sfr_fc

  subroutine sfr_fn(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! pak1fn -- Fill newton terms
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(SfrType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i, n
    integer(I4B) :: ipos
    real(DP) :: rterm, drterm
    real(DP) :: rhs1, hcof1, q1
    real(DP) :: q2
    real(DP) :: hgwf
! --------------------------------------------------------------------------
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      ! -- skip inactive reaches
      if (this%reaches(i)%iboundpak < 1) cycle
      ! -- skip if reach is not connected to gwf
      n = this%nodelist(i)
      if (n < 1) cycle
      ipos = ia(n)
      !rterm = this%hcof(i) * this%xnew(n) - this%rhs(i)
      rterm = this%hcof(i) * this%xnew(n)
      ! -- calculate perturbed head
      hgwf = this%xnew(n) + DEM4
      call this%sfr_solve(i, hgwf, hcof1, rhs1, update=.false.)
      q1 = rhs1 - hcof1 * hgwf
      ! -- calculate unperturbed head
      !hgwf = this%xnew(n)
      !call this%sfr_solve(i, hgwf, hcof2, rhs2)
      !q2 = rhs2 - hcof2 * hgwf
      q2 = this%rhs(i) - this%hcof(i) * this%xnew(n)
      ! -- calculate derivative
      drterm = (q2 - q1) / DEM4
      ! -- add terms to convert conductance formulation into
      !    newton-raphson formulation
      !amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + drterm
      amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + drterm - this%hcof(i)
      rhs(n) = rhs(n) - rterm + drterm * this%xnew(n)
    end do
    !
    ! -- return
    return
  end subroutine sfr_fn

  subroutine sfr_cc(this, iend, icnvg, hclose, rclose)
! **************************************************************************
! sfr_cc -- Final convergence check for package
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(SfrType), intent(inout) :: this
    integer(I4B), intent(in) :: iend
    integer(I4B), intent(inout) :: icnvg
    real(DP), intent(in) :: hclose
    real(DP), intent(in) :: rclose
    ! -- local
    character(len=15) :: cdhmax
    character(len=15) :: crmax
    integer(I4B) :: n
    integer(I4B) :: ifirst
    real(DP) :: dh
    real(DP) :: r
    ! format
02000 format(4x,'STREAMFLOW ROUTING PACKAGE FAILED CONVERGENCE CRITERIA',//,    &
             4x,a10,2(1x,a15),/,4x,74('-'))
02010 format(4x,i10,2(1x,G15.7))
02020 format(4x,74('-'))
02030 format('CONVERGENCE FAILED AS A RESULT OF STREAMFLOW ROUTING PACKAGE',    &
             1x,a)
! --------------------------------------------------------------------------
    ifirst = 1
    if (this%iconvchk /= 0) then
      final_check: do n = 1, this%maxbound
        if (this%reaches(n)%iboundpak == 0) cycle
        dh = this%reaches(n)%stage0 - this%reaches(n)%stage
        r = this%reaches(n)%usflow0 - this%reaches(n)%usflow
        if (ABS(dh) > hclose .or. ABS(r) > rclose) then
          icnvg = 0
          ! write convergence check information if this is the last outer iteration
          if (iend == 1) then
            if (ifirst == 1) then
              ifirst = 0
              write(*,2030) this%name
              write(this%iout, 2000) '     REACH',                              &
                 '        MAX. DH', '  MAX. RESIDUAL'
            end if
            cdhmax = '               '
            crmax = '               '
            if (ABS(dh) > hclose) then
              write(cdhmax, '(G15.7)') dh
            end if
            if (ABS(r) > rclose) then
              write(crmax, '(G15.7)') r
            end if
            write(this%iout,2010) n, cdhmax, crmax
          ! terminate check since no need to find more than one non-convergence
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
  end subroutine sfr_cc


  subroutine sfr_bd(this, x, idvfl, icbcfl, ibudfl, icbcun, iprobs,         &
                    isuppress_output, model_budget, imap, iadv)
! **************************************************************************
! bnd_bd -- Calculate Volumetric Budget
! Note that the compact budget will always be used.
! Subroutine: (1) Process each package entry
!             (2) Write output
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper, delt, pertim, totim
    use ConstantsModule, only: LENBOUNDNAME
    use InputOutputModule, only: ulasav, ubdsv06
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(SfrType) :: this
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
    integer(I4B) :: i
    integer(I4B) :: ibinun
    real(DP) :: rain_ratin, rain_ratout
    real(DP) :: evap_ratin, evap_ratout
    real(DP) :: runoff_ratin, runoff_ratout
    real(DP) :: extin_ratin, extin_ratout
    real(DP) :: qgwf_ratin, qgwf_ratout
    real(DP) :: extout_ratin, extout_ratout
    real(DP) :: qmvr_ratin, qmvr_ratout
    real(DP) :: qfrommvr, qtomvr
    real(DP) :: depth
    real(DP) :: a, ae
    real(DP) :: qi, qro, qr, qe, qgwf, qext
    ! -- for budget
    integer(I4B) :: n
    integer(I4B) :: n2
    integer(I4B) ::  ii
    integer(I4B) :: naux
    real(DP) :: q
    real(DP) :: qt
    real(DP) :: d
    real(DP) :: v
    real(DP) :: qoutflow
    ! -- for observations
    integer(I4B) :: iprobslocal
    ! -- formats
! --------------------------------------------------------------------------
    !
    ! -- Suppress saving of simulated values; they
    !    will be saved at end of this procedure.
    iprobslocal = 0
    !
    ! -- call base functionality in bnd_bd
    call this%BndType%bnd_bd(x, idvfl, icbcfl, ibudfl, icbcun, iprobslocal,    &
                             isuppress_output, model_budget, iadv=1)
    !
    ! -- sfr budget routines (start by resetting)
    call this%budget%reset()
    !
    ! -- initialize accumulators
    rain_ratin = DZERO
    rain_ratout = DZERO
    evap_ratin = DZERO
    evap_ratout = DZERO
    runoff_ratin = DZERO
    runoff_ratout = DZERO
    extin_ratin = DZERO
    extin_ratout = DZERO
    qgwf_ratin = DZERO
    qgwf_ratout = DZERO
    extout_ratin = DZERO
    extout_ratout = DZERO
    qmvr_ratin = DZERO
    qmvr_ratout = DZERO
    !
    ! -- sfr budget term calculations
    do n = 1, this%maxbound
      !
      ! -- rainfall and evaporation
      depth = this%reaches(n)%depth
      a = this%geo(n)%surface_area()
      ae = this%geo(n)%surface_area_wet(depth)
      qr = this%reaches(n)%rain%value * a
      !qe = -this%reaches(n)%evap%value * ae
      qe = -this%reaches(n)%simevap
      !
      ! -- inflow and runoff
      qi = this%reaches(n)%inflow%value
      qro = this%reaches(n)%runoff%value
      !
      ! -- mover
      qfrommvr = DZERO
      qtomvr = DZERO
      if (this%imover == 1) then
        qfrommvr = this%pakmvrobj%get_qfrommvr(n)
        qtomvr = this%pakmvrobj%get_qtomvr(n)
        if (qtomvr > DZERO) then
          qtomvr = -qtomvr
        end if
      endif
      !
      ! -- groundwater leakage
      qgwf = -this%reaches(n)%gwflow
      !
      ! -- external downstream stream flow
      qext = this%reaches(n)%dsflow
      qoutflow = DZERO
      if (qext > DZERO) then
        qext = -qext
      end if
      do i = 1, this%reaches(n)%nconn
        if (this%reaches(n)%idir(i) > 0) cycle
        qext = DZERO
        exit
      end do
      !
      ! -- adjust external downstream stream flow using qtomvr
      if (qext < DZERO) then
        if (qtomvr < DZERO) then
          qext = qext - qtomvr
        end if
      else
        qoutflow = this%reaches(n)%dsflow
        if (qoutflow > DZERO) then
          qoutflow = -qoutflow
        end if
      end if
      !
      ! -- set qextoutflow and qoutflow for cell by cell budget
      !    output and observations
      this%qextoutflow(n) = qext
      this%qoutflow(n) = qoutflow
      !
      ! -- accumulate terms
      if (qr < DZERO) then
        !
        ! -- Flow is out of sfr
        rain_ratout = rain_ratout - qr
      else
        !
        ! -- Flow is into sfr
        rain_ratin = rain_ratin + qr
      end if
      if (qe < DZERO) then
        !
        ! -- Flow is out of sfr
        evap_ratout = evap_ratout - qe
      else
        !
        ! -- Flow is into sfr
        evap_ratin = evap_ratin + qe
      end if
      if (qi < DZERO) then
        !
        ! -- Flow is out of sfr
        extin_ratout = extin_ratout - qi
      else
        !
        ! -- Flow is into sfr
        extin_ratin = extin_ratin + qi
      end if
      if (qro < DZERO) then
        !
        ! -- Flow is out of sfr
        runoff_ratout = runoff_ratout - qro
      else
        !
        ! -- Flow is into sfr
        runoff_ratin = runoff_ratin + qro
      end if
      if (qgwf < DZERO) then
        !
        ! -- Flow is out of sfr
        qgwf_ratout = qgwf_ratout - qgwf
      else
        !
        ! -- Flow is into sfr
        qgwf_ratin = qgwf_ratin + qgwf
      end if
      if (qext < DZERO) then
        !
        ! -- Flow is out of sfr
        extout_ratout = extout_ratout - qext
      else
        !
        ! -- Flow is into sfr
        extout_ratin = extout_ratin + qext
      end if
      if (qfrommvr < DZERO) then
        !
        ! -- Flow is out of sfr
        qmvr_ratout = qmvr_ratout - qfrommvr
      else
        !
        ! -- Flow is into sfr
        qmvr_ratin = qmvr_ratin + qfrommvr
      end if
      if (qtomvr < DZERO) then
        !
        ! -- Flow is out of sfr
        qmvr_ratout = qmvr_ratout - qtomvr
      else
        !
        ! -- Flow is into sfr
        qmvr_ratin = qmvr_ratin + qtomvr
      end if
    end do
    !
    ! -- add calculated terms
    call this%budget%addentry(extin_ratin, extin_ratout, delt,  &
                              this%csfrbudget(4), isuppress_output)
    if (this%imover == 1) then
      call this%budget%addentry(qmvr_ratin, DZERO, delt,  &
                              this%csfrbudget(7), isuppress_output)
    end if
    call this%budget%addentry(rain_ratin, rain_ratout, delt,  &
                              this%csfrbudget(1), isuppress_output)
    call this%budget%addentry(runoff_ratin, runoff_ratout, delt,  &
                              this%csfrbudget(3), isuppress_output)
    call this%budget%addentry(qgwf_ratin, qgwf_ratout, delt,  &
                              this%csfrbudget(5), isuppress_output)
    call this%budget%addentry(evap_ratin, evap_ratout, delt,  &
                              this%csfrbudget(2), isuppress_output)
    call this%budget%addentry(extout_ratin, extout_ratout, delt,  &
                              this%csfrbudget(6), isuppress_output)
    if (this%imover == 1) then
      call this%budget%addentry(DZERO, qmvr_ratout, delt,  &
                              this%csfrbudget(8), isuppress_output)
    end if
    !
    ! -- For continuous observations, save simulated values.
    if (this%obs%npakobs > 0 .and. iprobs > 0) then
      call this%sfr_bd_obs()
    end if
    !
    ! -- set unit number for binary dependent variable output
    ibinun = 0
    if(this%istageout /= 0) then
      ibinun = this%istageout
    end if
    if(idvfl == 0) ibinun = 0
    if (isuppress_output /= 0) ibinun = 0
    !
    ! -- write sfr binary output
    if (ibinun > 0) then
      do n = 1, this%maxbound
        d = this%reaches(n)%depth
        v = this%reaches(n)%stage
        if (this%reaches(n)%iboundpak < 1) then
          v = DHNOFLO
        else if (d == DZERO) then
          v = DHDRY
        end if
        this%dbuff(n) = v
      end do
      call ulasav(this%dbuff, '           STAGE', kstp, kper, pertim, totim,   &
                  this%maxbound, 1, 1, ibinun)
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
    ! -- write sfr binary budget output
    if (ibinun > 0) then
      ! FLOW JA FACE
      naux = this%cbcauxitems
      call ubdsv06(kstp, kper, '    FLOW-JA-FACE', this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%cauxcbc, this%nconn, 1, 1, this%nconn,   &
                   this%iout, delt, pertim, totim)
      do n = 1, this%maxbound
        do i = 1, this%reaches(n)%nconn
          n2 = this%reaches(n)%iconn(i)
          ! flow to downstream reaches
          if (this%reaches(n)%idir(i) < 0) then
            qt = this%reaches(n)%dsflow
            q = -this%reaches(n)%qconn(i)
          ! flow from upstream reaches
          else
            qt = this%reaches(n)%usflow
            do ii = 1, this%reaches(n2)%nconn
              if (this%reaches(n2)%idir(ii) > 0) cycle
              if (this%reaches(n2)%iconn(ii) /= n) cycle
              q = this%reaches(n2)%qconn(ii)
              exit
            end do
          end if
          ! calculate flow area
          call this%sfr_rectch_depth(n, qt, d)
          this%qauxcbc(1) = d * this%reaches(n)%width
          call this%dis%record_mf6_list_entry(ibinun, n, n2, q, naux,      &
                                                  this%qauxcbc,                &
                                                  olconv=.FALSE.,              &
                                                  olconv2=.FALSE.)
        end do
      end do
      ! LEAKAGE
      naux = this%cbcauxitems
      call ubdsv06(kstp, kper, this%csfrbudget(5), this%name_model, this%name, &
                   this%name_model, this%name_model,                           &
                   ibinun, naux, this%cauxcbc, this%maxbound, 1, 1,            &
                   this%maxbound, this%iout, delt, pertim, totim)
      do n = 1, this%maxbound
        ! -- fill qauxcbc
        ! -- reach volume
        this%qauxcbc(1) = this%reaches(n)%width * this%reaches(n)%length
        ! -- get leakage
        n2 = this%reaches(n)%igwfnode
        q = -this%reaches(n)%gwflow
        call this%dis%record_mf6_list_entry(ibinun, n, n2, q, naux,        &
                                                this%qauxcbc,                  &
                                                olconv=.FALSE.)
      end do
      ! INFLOW
      naux = 0
      call ubdsv06(kstp, kper, this%csfrbudget(4), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%maxbound, 1, 1,            &
                   this%maxbound, this%iout, delt, pertim, totim)
      do n = 1, this%maxbound
        q = this%reaches(n)%inflow%value
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! RAIN
      naux = 0
      call ubdsv06(kstp, kper, this%csfrbudget(1), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%maxbound, 1, 1,            &
                   this%maxbound, this%iout, delt, pertim, totim)
      do n = 1, this%maxbound
        a = this%geo(n)%surface_area()
        q = this%reaches(n)%rain%value * a
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! RUNOFF
      naux = 0
      call ubdsv06(kstp, kper, this%csfrbudget(3), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%maxbound, 1, 1,            &
                   this%maxbound, this%iout, delt, pertim, totim)
      do n = 1, this%maxbound
        !q = this%reaches(n)%runoff%value
        q = this%reaches(n)%simrunoff
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! EVAPORATION
      naux = 0
      call ubdsv06(kstp, kper, this%csfrbudget(2), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%maxbound, 1, 1,            &
                   this%maxbound, this%iout, delt, pertim, totim)
      do n = 1, this%maxbound
        ae = this%geo(n)%surface_area_wet(depth)
        !q = -this%reaches(n)%evap%value * ae
        q = -this%reaches(n)%simevap
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! EXTERNAL OUTFLOW
      naux = 0
      call ubdsv06(kstp, kper, this%csfrbudget(6), this%name_model, this%name, &
                   this%name_model, this%name,                                 &
                   ibinun, naux, this%auxname, this%maxbound, 1, 1,            &
                   this%maxbound, this%iout, delt, pertim, totim)
      do n = 1, this%maxbound
        q = this%reaches(n)%dsflow
        if (q > DZERO) q = -q
        do i = 1, this%reaches(n)%nconn
          if (this%reaches(n)%idir(i) > 0) cycle
          q = DZERO
          exit
        end do
        if (this%imover == 1) then
          q = q + this%pakmvrobj%get_qtomvr(n)
        end if
        call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,         &
                                                this%auxvar(:,n),              &
                                                olconv=.FALSE.,                &
                                                olconv2=.FALSE.)
      end do
      ! MOVER
      if (this%imover == 1) then
        ! FROM MOVER
        naux = 0
        call ubdsv06(kstp, kper, this%csfrbudget(7), this%name_model,          &
                     this%name, this%name_model, this%name,                    &
                     ibinun, naux, this%auxname,                               &
                     this%maxbound, 1, 1,                                      &
                     this%maxbound, this%iout, delt, pertim, totim)
        do n = 1, this%maxbound
          q = this%pakmvrobj%get_qfrommvr(n)
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,       &
                                                  this%auxvar(:,n),            &
                                                  olconv=.FALSE.,              &
                                                  olconv2=.FALSE.)
        end do
        ! TO MOVER
        naux = 0
        call ubdsv06(kstp, kper, this%csfrbudget(8), this%name_model,          &
                     this%name, this%name_model, this%name,                    &
                     ibinun, naux, this%auxname,                               &
                     this%maxbound, 1, 1,                                      &
                     this%maxbound, this%iout, delt, pertim, totim)
        do n = 1, this%maxbound
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
                     ibinun, naux, this%auxname, this%maxbound, 1, 1,           &
                     this%maxbound, this%iout, delt, pertim, totim)
        do n = 1, this%maxbound
          q = DZERO
          ! fill auxvar
          do i = 1, naux
            this%auxvar(i,n) = this%reaches(n)%auxvar(i)%value
          end do
          call this%dis%record_mf6_list_entry(ibinun, n, n, q, naux,       &
                                                  this%auxvar(:,n),            &
                                                  olconv=.FALSE.,              &
                                                  olconv2=.FALSE.)
        end do
      end if

    end if
    !
    !
    ! -- return
    return
  end subroutine sfr_bd


  subroutine sfr_ot(this, kstp, kper, iout, ihedfl, ibudfl)
    ! **************************************************************************
    ! pak1t -- Output package budget
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    use InputOutputModule, only: UWWORD
    ! -- dummy
    class(SfrType) :: this
    integer(I4B),intent(in) :: kstp
    integer(I4B),intent(in) :: kper
    integer(I4B),intent(in) :: iout
    integer(I4B),intent(in) :: ihedfl
    integer(I4B),intent(in) :: ibudfl
    ! -- locals
    character (len=20) :: cellids, cellid
    character(len=LINELENGTH) :: line, linesep
    character(len=16) :: text
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: node
    integer(I4B) :: iloc
    real(DP) :: hgwf
    real(DP) :: sbot
    real(DP) :: q
    real(DP) :: depth, stage, a, ae
    real(DP) :: qu, qr, qe, qi, qro, qgwf, qd, qext
    real(DP) :: w, cond, grad
    real(DP) :: qfrommvr, qtomvr
    real(DP) :: qin, qout, qerr, qavg, qpd
    ! format
 2000 FORMAT ( 1X, ///1X, A, A, A, '   PERIOD ', I6, '   STEP ', I8)
     ! --------------------------------------------------------------------------
     !
     ! -- set cell id based on discretization
     if (this%dis%ndim == 3) then
       cellids = '(LAYER,ROW,COLUMN)  '
     elseif (this%dis%ndim == 2) then
       cellids = '(LAYER,CELL2D)      '
     else
       cellids = '(NODE)              '
     end if
     !
     ! -- write sfr stage and depth
     if (ihedfl /= 0 .and. this%iprhed /= 0) then
       write (iout, 2000) 'SFR (', trim(this%name), ') STAGE', kper, kstp
      iloc = 1
      line = ''
      if(this%inamedbound==1) then
        call UWWORD(line, iloc, 16, 1, 'reach', n, q, left=.TRUE.)
      end if
      call UWWORD(line, iloc, 6, 1, 'reach', n, q, CENTER=.TRUE., sep=' ')
      call UWWORD(line, iloc, 20, 1, 'reach ', n, q, left=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'gwf', n, q, CENTER=.TRUE., sep=' ')
      call UWWORD(line, iloc, 11, 1, 'streambed', n, q, CENTER=.TRUE., sep=' ')
      call UWWORD(line, iloc, 11, 1, 'streambed', n, q, CENTER=.TRUE.)
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
      call UWWORD(line, iloc, 20, 1, cellids, n, q, left=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'stage', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'depth', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'width', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'head', n, q, CENTER=.TRUE., sep=' ')
      call UWWORD(line, iloc, 11, 1, 'conductance', n, q, CENTER=.TRUE., sep=' ')
      call UWWORD(line, iloc, 11, 1, 'gradient', n, q, CENTER=.TRUE.)
      ! -- write second line
      write(iout,'(1X,A)') line(1:iloc)
      write(iout,'(1X,A)') linesep(1:iloc)
      ! -- write data
      do n = 1, this%maxbound
        node = this%reaches(n)%igwfnode
        if (node > 0) then
          call this%dis%noder_to_string(node, cellid)
          hgwf = this%xnew(node)
        else
          cellid = 'none'
        end if
        iloc = 1
        line = ''
        if(this%inamedbound==1) then
          call UWWORD(line, iloc, 16, 1, this%boundname(n), n, q, left=.TRUE.)
        end if
        call UWWORD(line, iloc, 6, 2, text, n, q, sep=' ')
        call UWWORD(line, iloc, 20, 1, cellid, n, q, left=.TRUE.)
        depth = this%reaches(n)%depth
        stage = this%reaches(n)%stage
        w = this%geo(n)%top_width_wet(depth)
        call UWWORD(line, iloc, 11, 3, text, n, stage)
        call UWWORD(line, iloc, 11, 3, text, n, depth)
        call UWWORD(line, iloc, 11, 3, text, n, w)
        call this%sfr_calc_cond(n, depth, cond)
        if (node > 0) then
          sbot = this%reaches(n)%strtop - this%reaches(n)%bthick
          if (hgwf < sbot) then
            grad = stage - sbot
          else
            grad = stage - hgwf
          end if
          grad = grad / this%reaches(n)%bthick
          call UWWORD(line, iloc, 11, 3, text, n, hgwf, sep=' ')
          call UWWORD(line, iloc, 11, 3, text, n, cond, sep=' ')
          call UWWORD(line, iloc, 11, 3, text, n, grad)
        else
          call UWWORD(line, iloc, 11, 1, '--', n, q, center=.TRUE., sep=' ')
          call UWWORD(line, iloc, 11, 3, text, n, cond, sep=' ')
          call UWWORD(line, iloc, 11, 1, '--', n, q, center=.TRUE.)
        end if
        write(iout, '(1X,A)') line(1:iloc)
      end do
     end if
     !
     ! -- write sfr rates
     if (ibudfl /= 0 .and. this%iprflow /= 0) then
       write (iout, 2000) 'SFR (', trim(this%name), ') FLOWS', kper, kstp
       iloc = 1
       line = ''
       if(this%inamedbound==1) then
         call UWWORD(line, iloc, 16, 1, 'reach', n, q, left=.TRUE.)
       end if
       call UWWORD(line, iloc, 6, 1, 'reach', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 20, 1, 'reach ', n, q, left=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'external', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE., sep=' ')
       if (this%imover == 1) then
        call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE., sep=' ')
       end if
       call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'external', n, q, CENTER=.TRUE., sep=' ')
       if (this%imover == 1) then
        call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE., sep=' ')
       end if
       call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE., sep=' ')
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
       call UWWORD(line, iloc, 20, 1, cellids, n, q, left=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'inflow', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'inflow', n, q, CENTER=.TRUE., sep=' ')
       if (this%imover == 1) then
        call UWWORD(line, iloc, 11, 1, 'from mvr', n, q, CENTER=.TRUE., sep=' ')
       end if
       call UWWORD(line, iloc, 11, 1, 'rainfall', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'runoff', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'leakage', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'evaporation', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'outflow', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'outflow', n, q, CENTER=.TRUE., sep=' ')
       if (this%imover == 1) then
        call UWWORD(line, iloc, 11, 1, 'to mvr', n, q, CENTER=.TRUE., sep=' ')
       end if
       call UWWORD(line, iloc, 11, 1, 'in - out', n, q, CENTER=.TRUE., sep=' ')
       call UWWORD(line, iloc, 11, 1, 'difference', n, q, CENTER=.TRUE.)
       ! -- write second line
       write(iout,'(1X,A)') line(1:iloc)
       write(iout,'(1X,A)') linesep(1:iloc)
       ! -- write data
       do n = 1, this%maxbound
         depth = this%reaches(n)%depth
         stage = this%reaches(n)%stage
         node = this%reaches(n)%igwfnode
         if (node > 0) then
           call this%dis%noder_to_string(node, cellid)
         else
           cellid = 'none'
         end if
         a = this%geo(n)%surface_area()
         ae = this%geo(n)%surface_area_wet(depth)
         qu = this%reaches(n)%usflow
         qr = this%reaches(n)%rain%value * a
         qi =  this%reaches(n)%inflow%value
         !qro = this%reaches(n)%runoff%value
         qro = this%reaches(n)%simrunoff
         !qe = this%reaches(n)%evap%value * ae
         qe = this%reaches(n)%simevap
         if (qe > DZERO) then
           qe = -qe
         end if
         qgwf = this%reaches(n)%gwflow
         if (qgwf /= DZERO) then
           qgwf = -qgwf
         end if
         qext = this%reaches(n)%dsflow
         qd = DZERO
         do i = 1, this%reaches(n)%nconn
           if (this%reaches(n)%idir(i) > 0) cycle
           qd = qext
           qext = DZERO
           exit
         end do

         if (qd > DZERO) then
           qd = -qd
         end if

         if (qext > DZERO) then
           qext = -qext
         end if
         !
         ! -- mover term
         qfrommvr = DZERO
         qtomvr = DZERO
         if (this%imover == 1) then
           qfrommvr = this%pakmvrobj%get_qfrommvr(n)
           qtomvr = this%pakmvrobj%get_qtomvr(n)
           if (qd < DZERO) then
             qd = qd + qtomvr
           end if
           if (qext < DZERO) then
             qext = qext + qtomvr
           end if
           if (qtomvr > DZERO) then
             qtomvr = -qtomvr
           end if
         end if
         !
         ! -- calculate error
         qin = qi + qu + qfrommvr + qr + qro
         qout = -qe - qd - qext - qtomvr
         if (qgwf < DZERO) then
           qout = qout - qgwf
         else
           qin = qin + qgwf
         end if
         qerr = qin - qout
         qavg = DHALF * (qin + qout)
         qpd = DZERO
         if (qavg > DZERO) then
           qpd = DHUNDRED * qerr / qavg
         end if
         !
         !
         ! -- fill line
         iloc = 1
         line = ''
         if (this%inamedbound==1) then
           call UWWORD(line, iloc, 16, 1, this%boundname(n), n, q, left=.TRUE.)
         end if
         call UWWORD(line, iloc, 6, 2, text, n, q, CENTER=.TRUE., sep=' ')
         call UWWORD(line, iloc, 20, 1, cellid, n, q, left=.TRUE., sep=' ')
         call UWWORD(line, iloc, 11, 3, text, n, qi, sep=' ')
         call UWWORD(line, iloc, 11, 3, text, n, qu, sep=' ')
         if (this%imover == 1) then
           call UWWORD(line, iloc, 11, 3, text, n, qfrommvr, sep=' ')
         end if
         call UWWORD(line, iloc, 11, 3, text, n, qr, sep=' ')
         call UWWORD(line, iloc, 11, 3, text, n, qro, sep=' ')
         call UWWORD(line, iloc, 11, 3, text, n, qgwf, sep=' ')
         call UWWORD(line, iloc, 11, 3, text, n, qe, sep=' ')
         call UWWORD(line, iloc, 11, 3, text, n, qd, sep=' ')
         call UWWORD(line, iloc, 11, 3, text, n, qext, sep=' ')
         if (this%imover == 1) then
           call UWWORD(line, iloc, 11, 3, text, n, qtomvr, sep=' ')
         end if
         call UWWORD(line, iloc, 11, 3, text, n, qerr, sep=' ')
         call UWWORD(line, iloc, 11, 3, text, n, qpd)
         write(iout, '(1X,A)') line(1:iloc)
        end do
      end if
      !
      ! -- Output sfr budget
      call this%budget%budget_ot(kstp, kper, iout)
      !
      ! -- return
      return
  end subroutine sfr_ot

  subroutine sfr_da(this)
! ******************************************************************************
! sfr_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(SfrType) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- arrays
    call mem_deallocate(this%qoutflow)
    call mem_deallocate(this%qextoutflow)
    deallocate(this%csfrbudget)
    call mem_deallocate(this%dbuff)
    deallocate(this%cauxcbc)
    call mem_deallocate(this%qauxcbc)
    !
    ! -- deallocation diversions
    do n = 1, this%maxbound
      if (this%reaches(n)%ndiv > 0) then
        call this%deallocate_diversion(n)
      endif
    enddo
    !
    ! -- deallocate reaches
    do n = 1, this%maxbound
      call this%deallocate_reach(n)
    enddo
    deallocate(this%reaches)
    !
    ! -- ia ja
    deallocate(this%ia)
    deallocate(this%ja)
    !
    ! -- objects
    deallocate(this%geo)
    call this%budget%budget_da()
    deallocate(this%budget)
    !
    ! -- scalars
    call mem_deallocate(this%iprhed)
    call mem_deallocate(this%istageout)
    call mem_deallocate(this%ibudgetout)
    call mem_deallocate(this%idiversions)
    call mem_deallocate(this%maxsfrit)
    call mem_deallocate(this%bditems)
    call mem_deallocate(this%cbcauxitems)
    call mem_deallocate(this%unitconv)
    call mem_deallocate(this%dmaxchg)
    call mem_deallocate(this%deps)
    call mem_deallocate(this%nconn)
    call mem_deallocate(this%icheck)
    call mem_deallocate(this%iconvchk)
    nullify(this%gwfiss)
    !
    ! -- call BndType deallocate
    call this%BndType%bnd_da()
    !
    ! -- return
  end subroutine sfr_da

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(SfrType), intent(inout) :: this
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


  subroutine sfr_set_pointers(this, neq, ibound, xnew, xold, flowja)
! ******************************************************************************
! set_pointers -- Set pointers to model arrays and variables so that a package
!                 has access to these things.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(SfrType) :: this
    integer(I4B), pointer :: neq
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    real(DP), dimension(:), pointer, contiguous :: xnew
    real(DP), dimension(:), pointer, contiguous :: xold
    real(DP), dimension(:), pointer, contiguous :: flowja
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- call base BndType set_pointers
    call this%BndType%set_pointers(neq, ibound, xnew, xold, flowja)
    !
    ! -- return
  end subroutine sfr_set_pointers

  !
  ! -- Procedures related to observations (type-bound)
  logical function sfr_obs_supported(this)
  ! ******************************************************************************
  ! sfr_obs_supported
  !   -- Return true because sfr package supports observations.
  !   -- Overrides BndType%bnd_obs_supported()
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    class(SfrType) :: this
  ! ------------------------------------------------------------------------------
    sfr_obs_supported = .true.
    return
  end function sfr_obs_supported


  subroutine sfr_df_obs(this)
  ! ******************************************************************************
  ! sfr_df_obs (implements bnd_df_obs)
  !   -- Store observation type supported by sfr package.
  !   -- Overrides BndType%bnd_df_obs
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    ! -- dummy
    class(SfrType) :: this
    ! -- local
    integer(I4B) :: indx
  ! ------------------------------------------------------------------------------
    !
    ! -- Store obs type and assign procedure pointer
    !    for stage observation type.
    call this%obs%StoreObsType('stage', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for inflow observation type.
    call this%obs%StoreObsType('inflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for inflow observation type.
    call this%obs%StoreObsType('ext-inflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for rainfall observation type.
    call this%obs%StoreObsType('rainfall', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for runoff observation type.
    call this%obs%StoreObsType('runoff', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for evaporation observation type.
    call this%obs%StoreObsType('evaporation', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for outflow observation type.
    call this%obs%StoreObsType('outflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for ext-outflow observation type.
    call this%obs%StoreObsType('ext-outflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for sfr-frommvr observation type.
    call this%obs%StoreObsType('from-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for sfr observation type.
    call this%obs%StoreObsType('sfr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for upstream flow observation type.
    call this%obs%StoreObsType('upstream-flow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for downstream flow observation type.
    call this%obs%StoreObsType('downstream-flow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sfr_process_obsID
    !
    return
  end subroutine sfr_df_obs


  subroutine sfr_bd_obs(this)
    ! **************************************************************************
    ! sfr_bd_obs
    !   -- Calculate observations this time step and call
    !      ObsType%SaveOneSimval for each SfrType observation.
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    ! -- dummy
    class(SfrType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, j, n, nn
    real(DP) :: v
    character(len=100) :: msg
    type(ObserveType), pointer :: obsrv => null()
    !---------------------------------------------------------------------------
    !
    ! Write simulated values for all sfr observations
    if (this%obs%npakobs>0) then
      call this%obs%obs_bd_clear()
      do i=1 ,this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        nn = size(obsrv%indxbnds)
        do j = 1,nn
          n = obsrv%indxbnds(j)
          v = DZERO
          select case (obsrv%ObsTypeId)
            case ('STAGE')
              v = this%reaches(n)%stage
            case ('TO-MVR')
              v = DNODATA
              if (this%imover == 1) then
                v = this%pakmvrobj%get_qtomvr(n)
                if (v > DZERO) then
                  v = -v
                end if
              end if
            case ('FROM-MVR')
              v = DNODATA
              if (this%imover == 1) then
                v = this%pakmvrobj%get_qfrommvr(n)
              end if
            case ('EXT-INFLOW')
              v = this%reaches(n)%inflow%value
            case ('INFLOW')
              v = this%reaches(n)%usflow
            case ('OUTFLOW')
              v = this%qoutflow(n)
            case ('EXT-OUTFLOW')
              v = this%qextoutflow(n)
            case ('RAINFALL')
              v = this%reaches(n)%rain%value
            case ('RUNOFF')
              !v = this%reaches(n)%runoff%value
              v = this%reaches(n)%simrunoff
            case ('EVAPORATION')
              !v = this%reaches(n)%evap%value
              v = this%reaches(n)%simevap
            case ('SFR')
              v = this%reaches(n)%gwflow
            case ('UPSTREAM-FLOW')
              v = this%reaches(n)%usflow
              if (this%imover == 1) then
                v = v + this%pakmvrobj%get_qfrommvr(n)
              end if
            case ('DOWNSTREAM-FLOW')
              v = this%reaches(n)%dsflow
              if (v > DZERO) then
                v = -v
              end if
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
  end subroutine sfr_bd_obs


  subroutine sfr_rp_obs(this)
    ! -- dummy
    class(SfrType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, j, n, nn1
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
60  format('Error: Invalid node number in OBS input: ',i5)
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
          ! -- Observation location(s) is(are) based on a boundary name.
          !    Iterate through all boundaries to identify and store
          !    corresponding index(indices) in bound array.
          jfound = .false.
          do j = 1, this%maxbound
            if (this%boundname(j) == bname) then
              jfound = .true.
              call ExpandArray(obsrv%indxbnds)
              n = size(obsrv%indxbnds)
              obsrv%indxbnds(n) = j
            endif
          enddo
          if (.not. jfound) then
            write(ermsg,10)trim(bname), trim(obsrv%name), trim(this%name)
            call store_error(ermsg)
          endif
        else
          write (ermsg,30) trim(obsrv%name), trim(this%name)
          call store_error(ermsg)
        endif
      elseif (nn1 < 1 .or. nn1 > this%maxbound) then
        write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
          'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
          ' reach must be > 0 and <=', this%maxbound, &
          '(specified value is ', nn1, ')'
        call store_error(ermsg)
      else
        call ExpandArray(obsrv%indxbnds)
        n = size(obsrv%indxbnds)
        if (n == 1) then
          obsrv%indxbnds(1) = nn1
        else
          ermsg = 'Programming error in sfr_rp_obs'
          call store_error(ermsg)
        endif
      end if
      !
      ! -- catch non-cumulative observation assigned to observation defined
      !    by a boundname that is assigned to more than one element
      if (obsrv%ObsTypeId == 'STAGE') then
        nn1 = obsrv%NodeNumber
        if (nn1 == NAMEDBOUNDFLAG) then
          n = size(obsrv%indxbnds)
          if (n > 1) then
            write (ermsg, '(4x,a,4(1x,a))') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              'for observation', trim(adjustl(obsrv%Name)), &
              ' must be assigned to a reach with a unique boundname.'
            call store_error(ermsg)
          end if
        end if
      end if
      !
      ! -- check that node number 1 is valid; call store_error if not
      n = size(obsrv%indxbnds)
      do j = 1, n
        nn1 = obsrv%indxbnds(j)
        if (nn1 < 1 .or. nn1 > this%maxbound) then
          write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
            'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
            ' reach must be > 0 and <=', this%maxbound, &
            '(specified value is ', nn1, ')'
          call store_error(ermsg)
        end if
      end do
    end do
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    return
  end subroutine sfr_rp_obs


  !
  ! -- Procedures related to observations (NOT type-bound)
  subroutine sfr_process_obsID(obsrv, dis, inunitobs, iout)
    ! -- This procedure is pointed to by ObsDataType%ProcesssIdPtr. It processes
    !    the ID string of an observation definition for sfr-package observations.
    ! -- dummy
    type(ObserveType),      intent(inout) :: obsrv
    class(DisBaseType), intent(in)    :: dis
    integer(I4B),            intent(in)    :: inunitobs
    integer(I4B),            intent(in)    :: iout
    ! -- local
    integer(I4B) :: nn1
    integer(I4B) :: icol, istart, istop
    character(len=LINELENGTH) :: strng
    character(len=LENBOUNDNAME) :: bndname
    ! formats
 30 format(i10)
    !
    strng = obsrv%IDstring
    ! -- Extract reach number from strng and store it.
    !    If 1st item is not an integer(I4B), it should be a
    !    boundary name--deal with it.
    icol = 1
    ! -- get reach number or boundary name
    call extract_idnum_or_bndname(strng, icol, istart, istop, nn1, bndname)
    if (nn1 == NAMEDBOUNDFLAG) then
      obsrv%FeatureName = bndname
    endif
    ! -- store reach number (NodeNumber)
    obsrv%NodeNumber = nn1
    !
    return
  end subroutine sfr_process_obsID

  !
  ! -- private sfr methods
  !


  subroutine sfr_set_stressperiod(this, n, line, ichkustrm)
! ******************************************************************************
! sfr_set_stressperiod -- Set a stress period attribute for sfr reach n
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
    class(SfrType),intent(inout) :: this
    integer(I4B), intent(in) :: n
    character (len=*), intent(in) :: line
    integer(I4B), intent(inout) :: ichkustrm
    ! -- local
    character(len=10) :: cnum
    character(len=LINELENGTH) :: text
    character(len=LINELENGTH) :: caux
    character(len=LINELENGTH) :: keyword
    character(len=LINELENGTH) :: errmsg
    character(len=LENBOUNDNAME) :: bndName
    integer(I4B) :: ival, istart, istop, jj
    integer(I4B) :: i0
    integer(I4B) :: lloc
    integer(I4B) :: idiv
    integer(I4B) :: iaux
    real(DP) :: rval
    real(DP) :: endtim
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Find time interval of current stress period.
    endtim = totimsav + perlen(kper)
    !
    ! -- Assign boundary name
    if (this%inamedbound==1) then
      bndName = this%boundname(n)
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
        ichkustrm = 1
        call urword(line, lloc, istart, istop, 1, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        this%reaches(n)%status = text(1:8)
        if (text == 'INACTIVE') then
          this%reaches(n)%iboundpak = 0
        else if (text == 'ACTIVE') then
          this%reaches(n)%iboundpak = 1
        else if (text == 'SIMPLE') then
          this%reaches(n)%iboundpak = -1
        else
          write(errmsg,'(4x,a,a)') &
            '****ERROR. UNKNOWN '//trim(this%text)//' SFR STATUS KEYWORD: ', &
            text
          call store_error(errmsg)
        end if
      case ('MANNING')
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1  ! For 'MANNING'
        call read_single_value_or_time_series(text, &
                                              this%reaches(n)%rough%value, &
                                              this%reaches(n)%rough%name, &
                                              endtim,  &
                                              this%Name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, 'MANNING', &
                                              bndName, this%inunit)
      case ('STAGE')
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1  ! For 'STAGE'
        call read_single_value_or_time_series(text, &
                                              this%reaches(n)%sstage%value, &
                                              this%reaches(n)%sstage%name, &
                                              endtim,  &
                                              this%Name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, 'STAGE', &
                                              bndName, this%inunit)
      case ('RAINFALL')
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1  ! For 'RAIN'
        call read_single_value_or_time_series(text, &
                                              this%reaches(n)%rain%value, &
                                              this%reaches(n)%rain%name, &
                                              endtim,  &
                                              this%Name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, 'RAINFALL', &
                                              bndName, this%inunit)
      case ('EVAPORATION')
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 2  ! For 'EVAP'
        call read_single_value_or_time_series(text, &
                                              this%reaches(n)%evap%value, &
                                              this%reaches(n)%evap%name, &
                                              endtim,  &
                                              this%Name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, &
                                              'EVAPORATION', bndName, &
                                              this%inunit)
      case ('RUNOFF')
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 3  ! For 'RUNOFF'
        call read_single_value_or_time_series(text, &
                                              this%reaches(n)%runoff%value, &
                                              this%reaches(n)%runoff%name, &
                                              endtim,  &
                                              this%Name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, 'RUNOFF', &
                                              bndName, this%inunit)
      case ('INFLOW')
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 4  ! For 'INFLOW'
        call read_single_value_or_time_series(text, &
                                              this%reaches(n)%inflow%value, &
                                              this%reaches(n)%inflow%name, &
                                              endtim,  &
                                              this%Name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, 'INFLOW', &
                                              bndName, this%inunit)
      case ('DIVERSION')
        !
        ! -- make sure reach has at least one diversion
        if (this%reaches(n)%ndiv < 1) then
          write (cnum, '(i0)') n
          errmsg = 'ERROR: diversions cannot be specified for reach ' // trim(cnum)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
        !
        ! -- read diversion number
        call urword(line, lloc, istart, istop, 2, ival, rval, this%iout, this%inunit)
        if (ival < 1 .or. ival > this%reaches(n)%ndiv) then
          write (cnum, '(i0)') n
          errmsg = 'ERROR: reach  ' // trim(cnum)
          write (cnum, '(i0)') this%reaches(n)%ndiv
          errmsg = errmsg // ' diversion number should be between 1 and ' // trim(cnum) // '.'
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
        idiv = ival
        !
        ! -- read value
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 5   ! for 'DIVERSION'
        call read_single_value_or_time_series(text, &
                                              this%reaches(n)%diversion(idiv)%rate%value, &
                                              this%reaches(n)%diversion(idiv)%rate%name, &
                                              endtim,  &
                                              this%Name, 'BND', this%TsManager, &
                                              this%iprpak, n, jj, 'DIVERSION', &
                                              bndName, this%inunit)

      case ('UPSTREAM_FRACTION')
        ichkustrm = 1
        call urword(line, lloc, istart, istop, 3, ival, rval, this%iout, this%inunit)
        this%reaches(n)%ustrf = rval

      case ('AUXILIARY')
        call urword(line, lloc, istart, istop, 1, ival, rval, this%iout, this%inunit)
        caux = line(istart:istop)
        do iaux = 1, this%naux
          if (trim(adjustl(caux)) /= trim(adjustl(this%auxname(iaux)))) cycle
          call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
          text = line(istart:istop)
          jj = 1 !iaux
          call read_single_value_or_time_series(text, &
                                                this%reaches(n)%auxvar(iaux)%value, &
                                                this%reaches(n)%auxvar(iaux)%name, &
                                                endtim,  &
                                                this%Name, 'BND', this%TsManager, &
                                                this%iprpak, n, jj, &
                                                this%auxname(iaux), bndName, &
                                                this%inunit)
          exit
        end do

      case default
        write(errmsg,'(4x,a,a)') &
          '****ERROR. UNKNOWN '//trim(this%text)//' SFR DATA KEYWORD: ', &
                                  line(istart:istop)
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
        call ustop()
      end select
    !
    ! -- write keyword data to output file
    if (this%iprpak /= 0) then
      write (this%iout, '(3x,i10,1x,a)') n, line(i0:istop)
    end if
    !
    ! -- return
    return
  end subroutine sfr_set_stressperiod

  subroutine allocate_reach(this, n)
! ******************************************************************************
! allocate_reach -- Allocate pointers for reach(n).
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(SfrType) :: this
    integer(I4B), intent(in) :: n
    ! -- local
    character(len=LINELENGTH) :: ermsg
    character(len=10) :: crch
    integer(I4B) :: iaux
! ------------------------------------------------------------------------------
    !
    ! -- make sure reach has not been allocated
    if (associated(this%reaches(n)%reach)) then
      write (crch, '(i10)') n
      ermsg = 'reach ' // trim(crch) // ' is already allocated'
      call store_error(ermsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    ! -- allocate pointers
    allocate(this%reaches(n)%status)
    allocate(this%reaches(n)%iboundpak)
    allocate(this%reaches(n)%reach)
    allocate(this%reaches(n)%igwfnode)
    allocate(this%reaches(n)%igwftopnode)
    allocate(this%reaches(n)%length)
    allocate(this%reaches(n)%width)
    allocate(this%reaches(n)%strtop)
    allocate(this%reaches(n)%bthick)
    allocate(this%reaches(n)%hk)
    allocate(this%reaches(n)%slope)
    allocate(this%reaches(n)%nconn)
    allocate(this%reaches(n)%ustrf)
    allocate(this%reaches(n)%ftotnd)
    allocate(this%reaches(n)%ndiv)
    allocate(this%reaches(n)%rough)
    allocate(this%reaches(n)%rough%name)
    allocate(this%reaches(n)%rough%value)
    allocate(this%reaches(n)%rain)
    allocate(this%reaches(n)%rain%name)
    allocate(this%reaches(n)%rain%value)
    allocate(this%reaches(n)%evap)
    allocate(this%reaches(n)%evap%name)
    allocate(this%reaches(n)%evap%value)
    allocate(this%reaches(n)%inflow)
    allocate(this%reaches(n)%inflow%name)
    allocate(this%reaches(n)%inflow%value)
    allocate(this%reaches(n)%runoff)
    allocate(this%reaches(n)%runoff%name)
    allocate(this%reaches(n)%runoff%value)
    allocate(this%reaches(n)%sstage)
    allocate(this%reaches(n)%sstage%name)
    allocate(this%reaches(n)%sstage%value)
    if (this%naux > 0) then
      allocate(this%reaches(n)%auxvar(this%naux))
      do iaux = 1, this%naux
        allocate(this%reaches(n)%auxvar(iaux)%name)
        allocate(this%reaches(n)%auxvar(iaux)%value)
      end do
    end if
    allocate(this%reaches(n)%usflow)
    allocate(this%reaches(n)%dsflow)
    allocate(this%reaches(n)%depth)
    allocate(this%reaches(n)%stage)
    allocate(this%reaches(n)%gwflow)
    allocate(this%reaches(n)%simevap)
    allocate(this%reaches(n)%simrunoff)
    allocate(this%reaches(n)%stage0)
    allocate(this%reaches(n)%usflow0)
    !
    ! -- initialize a few items
    this%reaches(n)%status = 'ACTIVE'
    this%reaches(n)%iboundpak = 1
    this%reaches(n)%rough%name = ''
    this%reaches(n)%rain%name = ''
    this%reaches(n)%evap%name = ''
    this%reaches(n)%inflow%name = ''
    this%reaches(n)%runoff%name = ''
    this%reaches(n)%sstage%name = ''
    this%reaches(n)%rough%value = DZERO
    this%reaches(n)%rain%value = DZERO
    this%reaches(n)%evap%value = DZERO
    this%reaches(n)%inflow%value = DZERO
    this%reaches(n)%runoff%value = DZERO
    this%reaches(n)%sstage%value = DZERO
    do iaux = 1, this%naux
      this%reaches(n)%auxvar(iaux)%value = DZERO
    end do
    this%reaches(n)%usflow = DZERO
    this%reaches(n)%dsflow = DZERO
    this%reaches(n)%depth = DZERO
    this%reaches(n)%stage = DZERO
    this%reaches(n)%gwflow = DZERO
    this%reaches(n)%simevap = DZERO
    this%reaches(n)%simrunoff = DZERO
    this%reaches(n)%stage0 = DZERO
    this%reaches(n)%usflow0 = DZERO
    !
    ! -- return
    return
  end subroutine allocate_reach

  subroutine deallocate_reach(this, n)
! ******************************************************************************
! deallocate_reach -- Deallocate pointers for reach(n).
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(SfrType) :: this
    integer(I4B), intent(in) :: n
    ! -- local
    integer(I4B) :: iaux
! ------------------------------------------------------------------------------
    !
    ! -- connections
    if (this%reaches(n)%nconn > 0) then
      deallocate(this%reaches(n)%iconn)
      deallocate(this%reaches(n)%idir)
      deallocate(this%reaches(n)%idiv)
      deallocate(this%reaches(n)%qconn)
    endif
    !
    ! -- deallocate pointers
    deallocate(this%reaches(n)%status)
    deallocate(this%reaches(n)%iboundpak)
    deallocate(this%reaches(n)%reach)
    deallocate(this%reaches(n)%igwfnode)
    deallocate(this%reaches(n)%igwftopnode)
    deallocate(this%reaches(n)%length)
    deallocate(this%reaches(n)%width)
    deallocate(this%reaches(n)%strtop)
    deallocate(this%reaches(n)%bthick)
    deallocate(this%reaches(n)%hk)
    deallocate(this%reaches(n)%slope)
    deallocate(this%reaches(n)%nconn)
    deallocate(this%reaches(n)%ustrf)
    deallocate(this%reaches(n)%ftotnd)
    deallocate(this%reaches(n)%ndiv)
    deallocate(this%reaches(n)%rough%name)
    deallocate(this%reaches(n)%rough%value)
    deallocate(this%reaches(n)%rough)
    deallocate(this%reaches(n)%rain%name)
    deallocate(this%reaches(n)%rain%value)
    deallocate(this%reaches(n)%rain)
    deallocate(this%reaches(n)%evap%name)
    deallocate(this%reaches(n)%evap%value)
    deallocate(this%reaches(n)%evap)
    deallocate(this%reaches(n)%inflow%name)
    deallocate(this%reaches(n)%inflow%value)
    deallocate(this%reaches(n)%inflow)
    deallocate(this%reaches(n)%runoff%name)
    deallocate(this%reaches(n)%runoff%value)
    deallocate(this%reaches(n)%runoff)
    deallocate(this%reaches(n)%sstage%name)
    deallocate(this%reaches(n)%sstage%value)
    deallocate(this%reaches(n)%sstage)
    if (this%naux > 0) then
      do iaux = 1, this%naux
        deallocate(this%reaches(n)%auxvar(iaux)%name)
        deallocate(this%reaches(n)%auxvar(iaux)%value)
      end do
      deallocate(this%reaches(n)%auxvar)
    end if
    deallocate(this%reaches(n)%usflow)
    deallocate(this%reaches(n)%dsflow)
    deallocate(this%reaches(n)%depth)
    deallocate(this%reaches(n)%stage)
    deallocate(this%reaches(n)%gwflow)
    deallocate(this%reaches(n)%simevap)
    deallocate(this%reaches(n)%simrunoff)
    deallocate(this%reaches(n)%stage0)
    deallocate(this%reaches(n)%usflow0)
    !
    ! -- return
    return
  end subroutine deallocate_reach

  subroutine allocate_diversion(this, n, ndiv)
! ******************************************************************************
! allocate_diversion -- Allocate diversion pointers for reach(n).
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(SfrType) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: ndiv
    ! -- local
    character(len=LINELENGTH) :: ermsg
    character(len=10) :: crch
    integer(I4B) :: j
! ------------------------------------------------------------------------------
    !
    ! -- make sure reach has not been allocated
    if (associated(this%reaches(n)%diversion)) then
      write (crch, '(i10)') n
      ermsg = 'ERROR: reach ' // trim(adjustl(crch)) // &
     &        ' diversions are already allocated'
      call store_error(ermsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    ! -- allocate pointers
    allocate(this%reaches(n)%diversion(ndiv))
    do j = 1, ndiv
      allocate(this%reaches(n)%diversion(j)%reach)
      allocate(this%reaches(n)%diversion(j)%cprior)
      allocate(this%reaches(n)%diversion(j)%iprior)
      allocate(this%reaches(n)%diversion(j)%rate)
      allocate(this%reaches(n)%diversion(j)%rate%name)
      allocate(this%reaches(n)%diversion(j)%rate%value)
      ! -- initialize a few variables
      this%reaches(n)%diversion(j)%reach = 0
      this%reaches(n)%diversion(j)%cprior = ''
      this%reaches(n)%diversion(j)%iprior = 0
      this%reaches(n)%diversion(j)%rate%name = ''
      this%reaches(n)%diversion(j)%rate%value = DZERO
    end do
    !
    ! -- return
    return
  end subroutine allocate_diversion

  subroutine deallocate_diversion(this, n)
! ******************************************************************************
! deallocate_diversion
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(SfrType) :: this
    integer(I4B), intent(in) :: n
    ! -- local
    integer(I4B) :: j
! ------------------------------------------------------------------------------
    !
    ! -- make sure reach has not been allocated
    ! -- allocate pointers
    do j = 1, this%reaches(n)%ndiv
      deallocate(this%reaches(n)%diversion(j)%reach)
      deallocate(this%reaches(n)%diversion(j)%cprior)
      deallocate(this%reaches(n)%diversion(j)%iprior)
      deallocate(this%reaches(n)%diversion(j)%rate%name)
      deallocate(this%reaches(n)%diversion(j)%rate%value)
      deallocate(this%reaches(n)%diversion(j)%rate)
    end do
    deallocate(this%reaches(n)%diversion)
    !
    ! -- return
    return
  end subroutine deallocate_diversion

  subroutine sfr_solve(this, n, h, hcof, rhs, update)
  ! ******************************************************************************
  ! sfr_solve -- Solve continuity equation
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      class(SfrType) :: this
      integer(I4B), intent(in) :: n
      real(DP), intent(in) :: h
      real(DP), intent(inout) :: hcof
      real(DP), intent(inout) :: rhs
      logical, intent(in), optional :: update
      ! -- local
      logical :: lupdate
      integer(I4B) :: i, ii
      integer(I4B) :: n2
      integer(I4B) :: isolve
      integer(I4B) :: iic, iic2, iic3, iic4
      integer(I4B) :: ibflg
      real(DP) :: hgwf
      real(DP) :: qu, qi, qr, qe, qro, qmp, qsrc
      real(DP) :: qfrommvr
      real(DP) :: qgwf
      real(DP) :: qmpsrc
      real(DP) :: qc
      real(DP) :: qt
      real(DP) :: tp
      real(DP) :: bt
      real(DP) :: hsfr
      real(DP) :: cstr
      real(DP) :: qd
      real(DP) :: en1, en2
      real(DP) :: qen1
      real(DP) :: f1, f2
      real(DP) :: qgwf1, qgwf2, qgwfp, qgwfold
      real(DP) :: fhstr1, fhstr2
      real(DP) :: d1, d2, dpp, dx
      real(DP) :: q1, q2
      real(DP) :: derv
      real(DP) :: dlh, dlhold
      real(DP) :: fp
      real(DP) :: sat, sat1, sat2
      real(DP) :: err, errold
      real(DP) :: sumleak, sumrch
  ! ------------------------------------------------------------------------------
    !
    ! --
    if (present(update)) then
      lupdate = update
    else
      lupdate = .true.
    end if
    !
    ! -- calculate hgwf
    hgwf = h
    !
    !
    hcof = DZERO
    rhs = DZERO
    !
    ! -- initialize q1, q2, and qgwf
    q1 = DZERO
    q2 = DZERO
    qgwf = DZERO
    qgwfold = DZERO
    !
    ! -- calculate initial depth assuming a wide cross-section and ignore
    !    groundwater leakage
    ! -- calculate upstream flow
    qu = DZERO
    do i = 1, this%reaches(n)%nconn
      if (this%reaches(n)%idir(i) < 0) cycle
      n2 = this%reaches(n)%iconn(i)
      do ii = 1, this%reaches(n2)%nconn
        if (this%reaches(n2)%idir(ii) > 0) cycle
        if (this%reaches(n2)%iconn(ii) /= n) cycle
        qu = qu + this%reaches(n2)%qconn(ii)
      end do
    end do
    !qu = this%reaches(n)%usflow
    this%reaches(n)%usflow = qu
    ! -- calculate remaining terms
    qi = this%reaches(n)%inflow%value
    qr = this%reaches(n)%rain%value * this%reaches(n)%width * this%reaches(n)%length
    qe = this%reaches(n)%evap%value * this%reaches(n)%width * this%reaches(n)%length
    qro = this%reaches(n)%runoff%value
    !
    ! -- Water mover term; assume that it goes in at the upstream end of the reach
    qfrommvr = DZERO
    if(this%imover == 1) then
      qfrommvr = this%pakmvrobj%get_qfrommvr(n)
    endif
    !
    ! -- calculate sum of sources to the reach excluding groundwater leakage
    qc = qu + qi + qr - qe + qro + qfrommvr
    !
    ! -- adjust runoff or evaporation if sum of sources is negative
    if (qc < DZERO) then
      !
      ! -- calculate sources without et
      qt = qu + qi + qr + qro + qfrommvr
      !
      ! -- runoff exceeds sources of water for reach
      if (qt < DZERO) then
        qro = -(qu + qi + qr + qfrommvr)
        qe = DZERO
      !
      ! -- evaporation exceeds sources of water for reach
      else
        qe = qu + qi + qr + qro + qfrommvr
      end if
      qc = qu + qi + qr - qe + qro + qfrommvr
    end if
    !
    ! -- set simulated evaporation and runoff
    this%reaches(n)%simevap = qe
    this%reaches(n)%simrunoff = qro
    !
    ! -- calculate flow at the middle of the reach and excluding groundwater leakage
    qmp = qu + qi + qfrommvr + DHALF * (qr - qe + qro)
    qmpsrc = qmp
    !
    ! -- calculate stream depth at the midpoint
    if (this%reaches(n)%iboundpak > 0) then
      call this%sfr_rectch_depth(n, qmp, d1)
    else
      this%reaches(n)%stage = this%reaches(n)%sstage%value
      d1 = max(DZERO, this%reaches(n)%stage - this%reaches(n)%strtop)
    end if
    !
    ! -- calculate sources/sinks for reach excluding groundwater leakage
    call this%sfr_calc_qsource(n, d1, qsrc)
    !
    ! -- calculate initial reach stage, downstream flow, and groundwater leakage
    tp = this%reaches(n)%strtop
    bt = tp - this%reaches(n)%bthick
    hsfr = d1 + tp
    qd = MAX(qsrc, DZERO)
    qgwf = DZERO
    !
    ! -- calculate reach conductance for a unit depth of water
    !    if equal to zero will skip iterations
    call this%sfr_calc_cond(n, DONE, cstr)
    !
    ! -- set flag to skip iterations
    isolve = 1
    if (hsfr <= tp .and. hgwf <= tp) isolve = 0
    if (hgwf <= tp .and. qc < DEM30) isolve = 0
    if (cstr < DEM30) isolve = 0
    if (this%reaches(n)%iboundpak < 0) isolve = 0
    !
    ! -- iterate to achieve solution
    itersol: if (isolve /= 0) then
      !
      ! -- estimate initial end points
      en1 = DZERO
      if (d1 > DEM30) then
        if ((tp - hgwf) > DEM30) then
          en2 = DP9 * d1
        else
          en2 = D1P1 * d1 - (tp - hgwf)
        end if
      else if ((tp - hgwf) > DEM30) then
        en2 = DONE
      else
        en2 = DP99 * (hgwf - tp)
      end if
      !
      ! -- estimate flow at end points
      ! -- end point 1
      if (hgwf > tp) then
        qgwf1 = cstr * (tp - hgwf)
        qen1 = qmp - DHALF * qgwf1
      else
        qgwf1 = DZERO
        qen1 = qmpsrc
      end if
      if (hgwf > bt) then
        qgwf2 = cstr * (tp + en2 - hgwf)
      else
        qgwf2 = cstr * (tp + en2 - bt)
      end if
      if (qgwf2 > qsrc) qgwf2 = qsrc
      ! -- calculate two depths
      call this%sfr_rectch_depth(n, (qmpsrc-DHALF*qgwf1), d1)
      call this%sfr_rectch_depth(n, (qmpsrc-DHALF*qgwf2), d2)
      ! -- determine roots
      if (d1 > DEM30) then
        f1 = en1 - d1
      else
        en1 = DZERO
        f1 = en1 - DZERO
      end if
      if (d2 > DEM30) then
        f2 = en2 - d2
        if (f2 < DEM30) en2 = d2
      else
        d2 = DZERO
        f2 = en2 - DZERO
      end if
      !
      ! -- iterate to find a solution
      dpp = DHALF * (en1 + en2)
      dx = dpp
      iic = 0
      iic2 = 0
      iic3 = 0
      fhstr1 = DZERO
      fhstr2 = DZERO
      qgwfp = DZERO
      dlhold = DZERO
      do i = 1, this%maxsfrit
        ibflg = 0
        d1 = dpp
        d2 = d1 + DTWO * this%deps
        ! -- calculate q at midpoint at both end points
        call this%sfr_calc_qman(n, d1, q1)
        call this%sfr_calc_qman(n, d2, q2)
        ! -- calculate groundwater leakage at both end points
        call sChSmooth(d1, sat1, derv)
        call sChSmooth(d2, sat2, derv)
        if (hgwf > bt) then
          qgwf1 = sat1 * cstr * (d1 + tp - hgwf)
          qgwf2 = sat2 * cstr * (d2 + tp - hgwf)
        else
          qgwf1 = sat1 * cstr * (d1 + tp - bt)
          qgwf2 = sat2 * cstr * (d2 + tp - bt)
        end if
        !
        if (qgwf1 >= qsrc) then
          en2 = dpp
          dpp = DHALF * (en1 + en2)
          call sChSmooth(dpp, sat, derv)
          if (hgwf > bt) then
            qgwfp = sat * cstr * (dpp + tp - hgwf)
          else
            qgwfp = sat * cstr * (dpp + tp - bt)
          end if
          if (qgwfp > qsrc) qgwfp = qsrc
          call this%sfr_rectch_depth(n, (qmpsrc-DHALF*qgwfp), dx)
          ibflg = 1
        else
          fhstr1 = (qmpsrc-DHALF*qgwf1) - q1
          fhstr2 = (qmpsrc-DHALF*qgwf2) - q2
        end if
        !
        if (ibflg == 0) then
          derv = DZERO
          if (abs(d1-d2) > DZERO) then
            derv = (fhstr1-fhstr2) / (d1 - d2)
          end if
          if (abs(derv) > DEM30) then
            dlh = -fhstr1 / derv
          else
            dlh = DZERO
          end if
          dpp = d1 + dlh
          !
          ! -- updated depth outside of endpoints - use bisection instead
          if ((dpp >= en2) .or. (dpp <= en1)) then
            if (abs(dlh) > abs(dlhold) .or. dpp < DEM30) then
              ibflg = 1
              dpp = DHALF * (en1 + en2)
            end if
          end if
          !
          ! -- check for slow convergence
          ! -- set flags to determine if the Newton-Raphson method oscillates
          !    or if convergence is slow
          if (qgwf1*qgwfold < DEM30) then
            iic2 = iic2 + 1
          else
            iic2 = 0
          end if
          if (qgwf1 < DEM30) then
            iic3 = iic3 + 1
          else
            iic3 = 0
          end if
          if (dlh*dlhold < DEM30 .or. ABS(dlh) > ABS(dlhold)) then
            iic = iic + 1
          end if
          iic4 = 0
          if (iic3 > 7 .and. iic > 12) then
            iic4 = 1
          end if
          !
          ! -- switch to bisection when the Newton-Raphson method oscillates
          !    or when convergence is slow
          if (iic2 > 7 .or. iic > 12 .or. iic4 == 1) then
            ibflg = 1
            dpp = DHALF * (en1 + en2)
          end if
          !
          ! --
          call sChSmooth(dpp, sat, derv)
          if (hgwf > bt) then
            qgwfp = sat * cstr * (dpp + tp - hgwf)
          else
            qgwfp = sat * cstr * (dpp + tp - bt)
          end if
          if (qgwfp > qsrc) then
            qgwfp = qsrc
            if (abs(en1-en2) < this%dmaxchg*DEM6) then
              call this%sfr_rectch_depth(n, (qmpsrc-DHALF*qgwfp), dpp)
            end if
          end if
          call this%sfr_rectch_depth(n, (qmpsrc-DHALF*qgwfp), dx)
        end if
        !
        ! --
        fp = dpp - dx
        if (ibflg == 1) then
          dlh = fp
          ! -- change end points
          ! -- root is between f1 and fp
          if (f1*fp < DZERO) then
            en2 = dpp
            f2 = fp
          ! -- root is between fp and f2
          else
            en1 = dpp
            f1 = fp
          end if
          err = min(abs(fp), abs(en2-en1))
        else
          err = abs(dlh)
        end if
        if (err < this%dmaxchg) then
          d1 = dpp
          qgwf = qgwfp
          qd = qsrc - qgwf
          exit
        end if
        !
        ! -- save iterates
        errold = err
        dlhold = dlh
        if (ibflg == 1) then
          qgwfold = qgwfp
        else
          qgwfold = qgwf1
        end if
      !
      ! -- end of iteration
      end do
    end if itersol

    ! -- simple routing option or where depth = 0 and hgwf < bt
    !if (this%reaches(n)%iboundpak < 0) then
    if (isolve == 0) then
      call sChSmooth(d1, sat, derv)
      if (hgwf > bt) then
        qgwf = sat * cstr * (d1 + tp - hgwf)
      else
        qgwf = sat * cstr * (d1 + tp - bt)
      end if
      ! -- leakage exceeds inflow
      if (qgwf > qsrc) then
        d1 = DZERO
        call this%sfr_calc_qsource(n, d1, qsrc)
        qgwf = qsrc
      end if
      ! -- set qd
      qd = qsrc - qgwf
    end if

    ! -- update sfr stage
    hsfr = tp + d1

    ! -- update stored values
    if (lupdate) then
      !!
      !! -- save previous stage and upstream flow
      !this%reaches(n)%stage0 = this%reaches(n)%stage
      !this%reaches(n)%usflow0 = this%reaches(n)%usflow
      !
      ! -- save depth and calculate stage
      this%reaches(n)%depth = d1
      this%reaches(n)%stage = hsfr
      !
      call this%sfr_update_flows(n, qd, qgwf)
    end if
    !
    ! -- calculate sumleak and sumrch
    sumleak = DZERO
    sumrch = DZERO
    if (this%gwfiss == 0) then
      sumleak = qgwf
    else
      sumleak = qgwf
    end if
    if (hgwf < bt) then
      sumrch = qgwf
    end if
    !
    ! -- calculate hcof and rhs for MODFLOW
    call sChSmooth(d1, sat, derv)
    if (abs(sumleak) > DZERO) then
      ! -- stream leakage is not head dependent
      if (hgwf < bt) then
        rhs = rhs - sumrch
      ! -- stream leakage is head dependent
      else if ((sumleak-qsrc) < -DEM30) then
        if (this%gwfiss == 0) then
          rhs = rhs - sat * cstr * hsfr - sumrch
        else
          rhs = rhs - sat * cstr * hsfr
        end if
        hcof = -cstr
      ! -- place holder for UZF
      else
        if (this%gwfiss == 0) then
          rhs = rhs - sumleak - sumrch
        else
          rhs = rhs - sumleak
        end if
      end if
    ! -- add groundwater leakage
    else if (hgwf < bt) then
      rhs = rhs - sumrch
    end if
    !
    ! -- return
    return
  end subroutine sfr_solve

  subroutine sfr_update_flows(this, n, qd, qgwf)
  ! ******************************************************************************
  ! sfr_update_flows -- Update downstream and groundwater leakage terms for reach
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      class(SfrType), intent(inout) :: this
      integer(I4B), intent(in) :: n
      real(DP), intent(inout) :: qd
      real(DP), intent(in) :: qgwf
      ! -- local
      integer(I4B) :: i
      integer(I4B) :: n2
      real(DP) :: q2
      real(DP) :: f
  ! ------------------------------------------------------------------------------
    !
    ! -- update reach terms
    !
    ! -- save final downstream stream flow
    this%reaches(n)%dsflow = qd
    !
    ! -- save groundwater leakage
    this%reaches(n)%gwflow = qgwf
    !
    ! -- route downstream flow
    if (qd > DZERO) then
      !
      ! -- route water to diversions
      do i = 1, this%reaches(n)%nconn
        if (this%reaches(n)%idir(i) > 0) cycle
        if (this%reaches(n)%idiv(i) == 0) cycle
        call this%sfr_calc_div(n, this%reaches(n)%idiv(i), qd, q2)
        this%reaches(n)%qconn(i) = q2
      end do
      !
      ! -- Mover terms: store outflow after diversion loss
      !    as qformvr and reduce outflow (qd)
      !    by how much was actually sent to the mover
      if (this%imover == 1) then
        call this%pakmvrobj%accumulate_qformvr(n, qd)
        qd = MAX(qd - this%pakmvrobj%get_qtomvr(n), DZERO)
      endif
      !
      ! -- route remaining water to downstream reaches
      do i = 1, this%reaches(n)%nconn
        if (this%reaches(n)%idir(i) > 0) cycle
        if (this%reaches(n)%idiv(i) > 0) cycle
        n2 = this%reaches(n)%iconn(i)
        f = this%reaches(n2)%ustrf / this%reaches(n)%ftotnd
        this%reaches(n)%qconn(i) = qd * f
      end do
    else
      do i = 1, this%reaches(n)%nconn
        if (this%reaches(n)%idir(i) > 0) cycle
        this%reaches(n)%qconn(i) = DZERO
      end do
    end if
    !
    ! -- return
    return
  end subroutine sfr_update_flows

  subroutine sfr_calc_qd(this, n, depth, hgwf, qgwf, qd)
  ! ******************************************************************************
  ! sfr_calc_dq -- Calculate downstream flow for reach
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      class(SfrType) :: this
      integer(I4B), intent(in) :: n
      real(DP), intent(in) :: depth
      real(DP), intent(in) :: hgwf
      real(DP), intent(inout) :: qgwf
      real(DP), intent(inout) :: qd
      ! -- local
      real(DP) :: qsrc
  ! ------------------------------------------------------------------------------
    !
    ! -- initialize residual
    qd = DZERO
    !
    ! -- calculate total water sources excluding groundwater leakage
    call this%sfr_calc_qsource(n, depth, qsrc)
    !
    ! -- estimate groundwater leakage
    call this%sfr_calc_qgwf(n, depth, hgwf, qgwf)
    if (-qgwf > qsrc) qgwf = -qsrc
    !
    ! -- calculate down stream flow
    qd = qsrc + qgwf
    !
    ! -- limit downstream flow to a positive value
    if (qd < DEM30) qd = DZERO
    !
    ! -- return
    return
  end subroutine sfr_calc_qd

  subroutine sfr_calc_qsource(this, n, depth, qsrc)
  ! ******************************************************************************
  ! sfr_calc_qsource -- Calculate sum of sources for reach - excluding
  !                     reach leakage
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      class(SfrType) :: this
      integer(I4B), intent(in) :: n
      real(DP), intent(in) :: depth
      real(DP), intent(inout) :: qsrc
      ! -- local
      real(DP) :: qu, qi, qr, qe, qro, qfrommvr
      real(DP) :: qt
      real(DP) :: a, ae
  ! ------------------------------------------------------------------------------
    !
    ! -- initialize residual
    qsrc = DZERO
    !
    ! -- calculate flow terms
    qu = this%reaches(n)%usflow
    qi = this%reaches(n)%inflow%value
    qro = this%reaches(n)%runoff%value
    !
    ! -- calculate rainfall and evap
    a = this%geo(n)%surface_area()
    ae = this%geo(n)%surface_area_wet(depth)
    qr = this%reaches(n)%rain%value * a
    !qe = this%reaches(n)%evap%value * ae
    qe = this%reaches(n)%evap%value * a
    !
    ! -- calculate mover term
    qfrommvr = DZERO
    if (this%imover == 1) then
      qfrommvr = this%pakmvrobj%get_qfrommvr(n)
    endif
    !
    ! -- calculate down stream flow
    qsrc = qu + qi + qr - qe + qro + qfrommvr
    !
    ! -- adjust runoff or evaporation if sum of sources is negative
    if (qsrc < DZERO) then
      !
      ! -- calculate sources without et
      qt = qu + qi + qr + qro + qfrommvr
      !
      ! -- runoff exceeds sources of water for reach
      if (qt < DZERO) then
        qro = -(qu + qi + qr + qfrommvr)
        qe = DZERO
      !
      ! -- evaporation exceeds sources of water for reach
      else
        qe = qu + qi + qr + qro + qfrommvr
      end if
      qsrc = qu + qi + qr - qe + qro + qfrommvr
    end if
    !
    ! -- return
    return
  end subroutine sfr_calc_qsource


  subroutine sfr_calc_qman(this, n, depth, qman)
  ! ******************************************************************************
  ! sfr_calc_qman -- Calculate stream flow using Manning's equation
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      class(SfrType) :: this
      integer(I4B), intent(in) :: n
      real(DP), intent(in) :: depth
      real(DP), intent(inout) :: qman
      ! -- local
      real(DP) :: sat
      real(DP) :: derv
      real(DP) :: s, r, aw, wp, rh
  ! ------------------------------------------------------------------------------
    !
    ! -- initialize qman
    qman = DZERO
    !
    ! -- calculate terms for Manning's equation
    call sChSmooth(depth, sat, derv)
    s = this%reaches(n)%slope
    r = this%reaches(n)%rough%value
    aw = this%geo(n)%area_wet(depth)
    wp = this%geo(n)%perimeter_wet(depth)
    rh = DZERO
    if (wp > DZERO) then
      rh = aw / wp
    end if
    !
    ! -- calculate flow
    qman = sat * this%unitconv * aw * (rh**DTWOTHIRDS) * sqrt(s) / r
    !
    ! -- return
    return
  end subroutine sfr_calc_qman


  subroutine sfr_calc_qgwf(this, n, depth, hgwf, qgwf)
  ! ******************************************************************************
  ! sfr_calc_qgwf -- Calculate sfr-aquifer exchange (relative to sfr reach)
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      class(SfrType) :: this
      integer(I4B), intent(in) :: n
      real(DP), intent(in) :: depth
      real(DP), intent(in) :: hgwf
      real(DP), intent(inout) :: qgwf
      ! -- local
      integer(I4B) :: node
      real(DP) :: tp
      real(DP) :: bt
      real(DP) :: hsfr
      real(DP) :: htmp
      real(DP) :: cond
      real(DP) :: sat
      real(DP) :: derv
  ! ------------------------------------------------------------------------------
    !
    ! -- initialize qgwf
    qgwf = DZERO
    !
    ! -- skip sfr-aquifer exchange in external cells
    node = this%reaches(n)%igwfnode
    if (node < 1) return
    !
    ! -- skip sfr-aquifer exchange in inactive cells
    if (this%ibound(node) == 0) return
    !
    ! -- calculate saturation
    call sChSmooth(depth, sat, derv)
    !
    ! -- calculate conductance
    call this%sfr_calc_cond(n, depth, cond)
    !
    ! -- calculate groundwater leakage
    tp = this%reaches(n)%strtop
    bt = tp - this%reaches(n)%bthick
    hsfr = tp + depth
    htmp = hgwf
    if (htmp < bt) then
      htmp = bt
    end if
    qgwf = sat * cond * (htmp - hsfr)
    !
    ! -- return
    return
  end subroutine sfr_calc_qgwf

  subroutine sfr_calc_cond(this, n, depth, cond)
  ! ******************************************************************************
  ! sfr_calc_qgwf -- Calculate sfr-aquifer exchange
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      class(SfrType) :: this
      integer(I4B), intent(in) :: n
      real(DP), intent(in) :: depth
      real(DP), intent(inout) :: cond
      ! -- local
      integer(I4B) :: node
      real(DP) :: wp
  ! ------------------------------------------------------------------------------
    !
    ! -- initialize a few variables
    cond = DZERO
    node = this%reaches(n)%igwfnode
    if (node > 0) then
      if (this%ibound(this%reaches(n)%igwfnode) > 0) then
        wp = this%geo(n)%perimeter_wet(depth)
        cond = this%reaches(n)%hk * this%reaches(n)%length * wp / this%reaches(n)%bthick
      end if
    end if
    !
    ! -- return
    return
  end subroutine sfr_calc_cond


  subroutine sfr_calc_div(this, n, i, q, qd)
  ! ******************************************************************************
  ! sfr_calc_resid -- Calculate residual for reach
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      class(SfrType) :: this
      integer(I4B), intent(in) :: n
      integer(I4B), intent(in) :: i
      real(DP), intent(inout) :: q
      real(DP), intent(inout) :: qd
      ! -- local
      character (len=10) :: cp
      integer(I4B) :: n2
      !integer(I4B) :: ip
      real(DP) :: v
  ! ------------------------------------------------------------------------------
    !
    ! -- set local variables
    n2 = this%reaches(n)%diversion(i)%reach
    cp = this%reaches(n)%diversion(i)%cprior
    !ip = this%reaches(n)%diversion(i)%iprior
    v = this%reaches(n)%diversion(i)%rate%value
    !
    ! -- calculate diversion
    select case(cp)
      ! -- flood diversion
      !case (-3)
      case ('EXCESS')
        if (q < v) then
          v = DZERO
        else
          v = q - v
        end if
      ! -- diversion percentage
      !case (-2)
      case ('FRACTION')
        v = q * v
      ! -- STR priority algorithm
      !case (-1)
      case ('THRESHOLD')
        if (q < v) then
          v = DZERO
        end if
      ! -- specified diversion
      !case (0)
      case ('UPTO')
        if (v > q) then
          v = q
        end if
      case default
        v = DZERO
    end select
    !
    ! -- update upstream from for downstream reaches
    q = q - v
    qd = v
    !
    ! -- return
    return
  end subroutine sfr_calc_div

  subroutine sfr_rectch_depth(this, n, q1, d1)
    class(SfrType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: q1
    real(DP), intent(inout) :: d1
    ! -- local
    real(DP) :: w
    real(DP) :: s
    real(DP) :: r
    real(DP) :: qconst
    ! -- code
    ! -- calculate stream depth at the midpoint
    w = this%reaches(n)%width
    s = this%reaches(n)%slope
    r = this%reaches(n)%rough%value
    qconst = this%unitconv * w * sqrt(s) / r
    d1 = (q1 / qconst)**DP6
    if (d1 < DEM30) d1 = DZERO
    ! -- return
    return
  end subroutine sfr_rectch_depth


  subroutine sfr_check_reaches(this)
    class(SfrType) :: this
    ! -- local
    character (len= 5) :: crch
    character (len=10) :: cval
    character (len=30) :: nodestr
    character (len=LINELENGTH) :: ermsg
    integer(I4B) :: n, nn
    real(DP) :: btgwf, bt
    ! -- code
    !
    ! -- write header
    if (this%iprpak /= 0) then
      write (this%iout, '(//a)') 'SFR STATIC REACH DATA'
      write (this%iout, '(a)') '     REACH CELLID                        ' //   &
            '    LENGTH      WIDTH      SLOPE        TOP ' //                   &
            ' THICKNESS         HK  ROUGHNESS  USTR FRAC'
      write (this%iout, "(128('-'))")
    end if
    !
    ! -- check the reach data for simple errors
    do n = 1, this%maxbound
      write (crch, '(i5)') n
      nn = this%reaches(n)%igwfnode
      if (nn > 0) then
        btgwf = this%dis%bot(nn)
        call this%dis%noder_to_string(nn, nodestr)
      else
        nodestr = 'none'
      end if
      ! -- check reach length
      if (this%reaches(n)%length <= DZERO) then
        ermsg = 'ERROR: Reach ' // crch // ' length must be > 0.0'
        call store_error(ermsg)
      end if
      ! -- check reach width
      if (this%reaches(n)%width <= DZERO) then
        ermsg = 'ERROR: Reach ' // crch // ' width must be > 0.0'
        call store_error(ermsg)
      end if
      ! -- check reach slope
      if (this%reaches(n)%slope <= DZERO) then
        ermsg = 'ERROR: Reach ' // crch // ' slope must be > 0.0'
        call store_error(ermsg)
      end if
      ! -- check bed thickness and bed hk for reaches connected to GWF
      if (nn > 0) then
        bt = this%reaches(n)%strtop - this%reaches(n)%bthick
        if (bt <= btgwf .and. this%icheck /= 0) then
          write (cval,'(f10.4)') bt
          ermsg = 'ERROR: Reach ' // crch // ' bed bottom (rtp-rbth =' // cval
          ermsg = trim(adjustl(ermsg)) // ') must be > the bottom of cell (' // nodestr
          write (cval,'(f10.4)') btgwf
          ermsg = trim(adjustl(ermsg)) // '=' // cval // ').'
          call store_error(ermsg)
        end if
        if (this%reaches(n)%hk < DZERO) then
          ermsg = 'ERROR: Reach ' // crch // ' hk must be >= 0.0'
          call store_error(ermsg)
        end if
      end if
      ! -- check reach roughness
      if (this%reaches(n)%rough%value <= DZERO) then
        ermsg = 'ERROR: Reach ' // crch // " Manning's roughness coefficient must be > 0.0"
        call store_error(ermsg)
      end if
      ! -- check reach upstream fraction
      if (this%reaches(n)%ustrf < DZERO) then
        ermsg = 'ERROR: Reach ' // crch // " upstream fraction must be >= 0.0"
        call store_error(ermsg)
      end if
      ! -- write summary of reach information
      if (this%iprpak /= 0) then
        write (this%iout,'(i10,1x,a30,2(f10.4,1x),g10.3,1x,2(f10.4,1x),2(g10.3,1x),f10.4)') &
               n, nodestr,                                                      &
               this%reaches(n)%length, this%reaches(n)%width,                   &
               this%reaches(n)%slope, this%reaches(n)%strtop,                   &
               this%reaches(n)%bthick, this%reaches(n)%hk,                      &
               this%reaches(n)%rough%value, this%reaches(n)%ustrf
      end if
    end do
    if (this%iprpak /= 0) then
      write (this%iout, "(128('-'))")
    end if

    ! -- return
    return
  end subroutine sfr_check_reaches


  subroutine sfr_check_connections(this)
    class(SfrType) :: this
    ! -- local
    character (len= 5) :: crch
    character (len= 5) :: crch2
    character (len=LINELENGTH) :: ermsg
    character (len=LINELENGTH) :: line
    integer(I4B) :: n, nn, nc
    integer(I4B) :: i, ii
    integer(I4B) :: ifound
    integer(I4B) :: ierr
    ! -- code

    !
    ! -- check the reach connections for simple errors
    ! -- connection header
    line = 'REACH'
    do n = 1, 24
      write (crch, '(i5)') n
      line = trim(line) // crch
    end do
    if (this%iprpak /= 0) then
      write (this%iout, '(//a)') 'SFR REACH CONNECTION DATA'
      write (this%iout, '(59x,a)') 'CONNECTED REACH DATA'
      write (this%iout, '(a)') line
      write (this%iout, "(128('-'))")
    end if
    ! -- connection check
    do n = 1, this%maxbound
      write (crch, '(i5)') n
      line = crch
      eachconn: do i = 1, this%reaches(n)%nconn
        nn = this%reaches(n)%iconn(i)
        write (crch2, '(i5)') nn
        line = trim(line) // crch2
        ifound = 0
        connreach: do ii = 1, this%reaches(nn)%nconn
          nc = this%reaches(nn)%iconn(ii)
          if (nc == n) then
            !if (this%reaches(n)%idir(i) /= this%reaches(nn)%idir(ii)) then
            !  ifound = 1
            !end if
            ifound = 1
            exit connreach
          end if
        end do connreach
        if (ifound /= 1) then
          ermsg = 'ERROR: Reach ' // crch // ' is connected to ' // &
     &            'reach ' // crch2 // ' but reach ' // crch2 // &
     &            ' is not connected to reach ' // crch // '.'
          call store_error(ermsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
      end do eachconn
      ! write line to output file
      if (this%iprpak /= 0) then
        write (this%iout, '(a)') trim(line)
      end if
    end do
    if (this%iprpak /= 0) then
      write (this%iout, "(128('-'))")
    end if

    !
    ! -- check for incorrect connections between upstream connections
    ierr = 0
    do n = 1, this%maxbound
      write (crch, '(i5)') n
      eachconnv: do i = 1, this%reaches(n)%nconn
        ! -- skip downstream connections
        if (this%reaches(n)%idir(i) < 0) cycle eachconnv
        nn = this%reaches(n)%iconn(i)
        write (crch2, '(i5)') nn
        connreachv: do ii = 1, this%reaches(nn)%nconn
          ! -- skip downstream connections
          if (this%reaches(nn)%idir(ii) < 0) cycle connreachv
          nc = this%reaches(nn)%iconn(ii)
          ! if nc == n then that means reach n is an upstream connection for
          !  reach nn and reach nn is an upstream connection for reach n
          if (nc == n) then
            ierr = ierr + 1
            ermsg = 'ERROR: Reach ' // crch // ' is connected to ' //       &
                    'reach ' // crch2 // ' but streamflow from reach ' //   &
                    crch // ' to reach ' // crch2 // ' is not permitted.'
            call store_error(ermsg)
            exit connreachv
          end if
        end do connreachv
      end do eachconnv
    end do
    if (ierr > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- check that downstream reaches for a reach are
    !    the upstream reaches for the reach
    ! -- downstream connection header
    line = 'REACH'
    do n = 1, 24
      write (crch, '(i5)') n
      line = trim(line) // crch
    end do
    !
    ! -- write header for downstream connections
    if (this%iprpak /= 0) then
      write (this%iout, '(//a)') 'SFR DOWNSTREAM CONNECTIONS'
      write (this%iout, '(60x,a)') 'DOWNSTREAM REACHES'
      write (this%iout, '(a)') line
      write (this%iout, "(128('-'))")
    end if
    do n = 1, this%maxbound
      write (crch, '(i5)') n
      line = crch
      eachconnds: do i = 1, this%reaches(n)%nconn
        nn = this%reaches(n)%iconn(i)
        if (this%reaches(n)%idir(i) > 0) cycle eachconnds
        write (crch2, '(i5)') nn
        line = trim(line) // crch2
        ifound = 0
        connreachds: do ii = 1, this%reaches(nn)%nconn
          nc = this%reaches(nn)%iconn(ii)
          if (nc == n) then
            if (this%reaches(n)%idir(i) /= this%reaches(nn)%idir(ii)) then
              ifound = 1
            end if
            exit connreachds
          end if
        end do connreachds
        if (ifound /= 1) then
          ermsg = 'ERROR: Reach ' // crch // ' downstream connected reach is ' // &
     &            'reach ' // crch2 // ' but reach ' // crch // &
     &            ' is not the upstream connected reach for reach ' // crch2 // '.'
          call store_error(ermsg)
        end if
      end do eachconnds
      ! write line to output file
      if (this%iprpak /= 0) then
        write (this%iout, '(a)') trim(line)
      end if
    end do
    if (this%iprpak /= 0) then
      write (this%iout, "(128('-'))")
    end if
    !
    ! -- output upstream reaches for each reach
    ! -- upstream connection header
    line = 'REACH'
    do n = 1, 24
      write (crch, '(i5)') n
      line = trim(line) // crch
    end do
    if (this%iprpak /= 0) then
      write (this%iout, '(//a)') 'SFR UPSTREAM CONNECTIONS'
      write (this%iout, '(61x,a)') 'UPSTREAM REACHES'
      write (this%iout, '(a)') line
      write (this%iout, "(128('-'))")
    end if
    do n = 1, this%maxbound
      write (crch, '(i5)') n
      line = crch
      eachconnus: do i = 1, this%reaches(n)%nconn
        nn = this%reaches(n)%iconn(i)
        if (this%reaches(n)%idir(i) < 0) cycle eachconnus
        write (crch2, '(i5)') nn
        line = trim(line) // crch2
      end do eachconnus
      ! write line to output file
      if (this%iprpak /= 0) then
        write (this%iout, '(a)') trim(line)
      end if
    end do
    if (this%iprpak /= 0) then
      write (this%iout, "(128('-'))")
    end if

    ! -- return
    return
  end subroutine sfr_check_connections


  subroutine sfr_check_diversions(this)
    class(SfrType) :: this
    ! -- local
    character (len= 5) :: crch
    character (len= 5) :: cdiv
    character (len= 5) :: crch2
    character (len=10) :: cprior
    character (len=LINELENGTH) :: ermsg
    character (len=LINELENGTH) :: line
    integer(I4B) :: n, nn, nc
    integer(I4B) :: ii
    integer(I4B) :: idiv
    integer(I4B) :: ifound
    ! -- format
10  format('Diversion ',i0,' of reach ',i0,' is invalid or has not been defined.')
    ! -- code
    !
    ! -- write header
    if (this%iprpak /= 0) then
      write (this%iout, '(//a)') 'SFR DIVERSION DATA'
      write (this%iout, '(a)') '     REACH DIVERSION    REACH2 CPRIOR'
      write (this%iout, "(45('-'))")
    end if
    !
    ! -- check that diversion data are correct
    do n = 1, this%maxbound
      if (this%reaches(n)%ndiv < 1) cycle
      write (crch, '(i5)') n
      line = '     ' // crch
      do idiv = 1, this%reaches(n)%ndiv
        write (cdiv, '(i5)') idiv
        line = trim(line) // '     ' // cdiv
        !
        nn = this%reaches(n)%diversion(idiv)%reach
        write (crch2, '(i5)') nn
        line = trim(line) // '     ' // crch2
        ! -- make sure diversion reach is connected to current reach
        ifound = 0
        if (nn < 1 .or. nn > this%maxbound) then
          write(ermsg,10)idiv, n
          call store_error(ermsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        connreach: do ii = 1, this%reaches(nn)%nconn
          nc = this%reaches(nn)%iconn(ii)
          if (nc == n) then
            if (this%reaches(nn)%idir(ii) > 0) then
              ifound = 1
            end if
            exit connreach
          end if
        end do connreach
        if (ifound /= 1) then
          ermsg = 'ERROR: Reach ' // crch // ' is not a upstream reach for ' // &
     &            'reach ' // crch2 // ' as a result diversion ' // cdiv // ' from ' // &
     &            'reach ' // crch //' to reach ' // crch2 // ' is not possible. ' // &
     &            'Check reach connectivity.'
          call store_error(ermsg)
        end if
        ! -- iprior
        cprior = this%reaches(n)%diversion(idiv)%cprior
        line = trim(line) // ' ' // cprior
        !
        ! write final line to output file
        if (this%iprpak /= 0) then
          write (this%iout, '(a)') trim(line)
        end if
      end do
    end do
    if (this%iprpak /= 0) then
      write (this%iout, "(45('-'))")
    end if
    !
    ! -- return
    return
  end subroutine sfr_check_diversions


  subroutine sfr_check_ustrf(this)
    class(SfrType) :: this
    ! -- local
    logical :: ladd
    character (len=5) :: crch, crch2
    character (len=10) :: cval
    character (len=LINELENGTH) :: ermsg
    character (len=LINELENGTH) :: line
    integer(I4B) :: i, n
    integer(I4B) :: n2
    integer(I4B) :: idiv
    integer(I4B) :: ids
    real(DP) :: f
    real(DP) :: rval
    ! -- code
    !
    ! -- write header
    line = 'REACH'
    do n = 1, 8
      write (crch, '(i5)') n
      line = trim(line) // crch // '  FRACTION'
    end do
    if (this%iprpak /= 0) then
      write (this%iout, '(//a)') 'SFR UPSTREAM FRACTIONS'
      write (this%iout, '(47x,a)') 'CONNECTED REACHES UPSTREAM FRACTIONS'
      write (this%iout, '(a)') line
      write (this%iout, "(128('-'))")
    end if
    !
    ! -- calculate the total fraction of connected reaches that are
    !    not diversions and check that the sum of upstream fractions
    !    is equal to 1 for each reach
    do n = 1, this%maxbound
      ids = 0
      rval = DZERO
      f = DZERO
      write (crch, '(i5)') n
      line = crch
      eachconn: do i = 1, this%reaches(n)%nconn
        ! -- initialize downstream connection q
        this%reaches(n)%qconn(i) = DZERO
        ! -- skip upstream connections
        if (this%reaches(n)%idir(i) > 0) cycle eachconn
        n2 = this%reaches(n)%iconn(i)
        ! -- skip inactive downstream reaches
        if (this%reaches(n2)%iboundpak == 0) cycle eachconn
        write (crch2, '(i5)') n2
        ids = ids + 1
        ladd = .true.
        f = f + this%reaches(n2)%ustrf
        write (cval, '(f10.4)') this%reaches(n2)%ustrf
        line = trim(line) // crch2 // cval
        eachdiv: do idiv = 1, this%reaches(n)%ndiv
          if (this%reaches(n)%diversion(idiv)%reach == n2) then
            this%reaches(n)%idiv(i) = idiv
            ladd = .false.
            exit eachconn
          end if
        end do eachdiv
        if (ladd) then
          rval = rval + this%reaches(n2)%ustrf
        end if
      end do eachconn
      this%reaches(n)%ftotnd = rval
      !
      ! -- write upstream fractions
      if (this%iprpak /= 0) then
        write (this%iout, '(a)') line
      end if
      if (ids /= 0) then
        if (abs(f-DONE) > DEM6) then
          write (cval, '(f10.4)') f
          ermsg = 'ERROR: upstream fractions for reach ' // crch // ' not equal to one ('
          ermsg = trim(adjustl(ermsg)) // cval // '). Check reach connectivity.'
          call store_error(ermsg)
        end if
      end if
    end do
    if (this%iprpak /= 0) then
      write (this%iout, "(128('-'),//)")
    end if
    !
    ! -- return
    return
  end subroutine sfr_check_ustrf

  end module SfrModule

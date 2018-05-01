! This is the numerical solution module.

module NumericalSolutionModule
  use KindModule,              only: DP, I4B
  use TimerModule,             only: code_timer
  use ConstantsModule,         only: LINELENGTH, LENSOLUTIONNAME,              &
                                     DZERO, DEM20, DEM15, DEM6, DEM4,          &
                                     DEM3, DEM2, DEM1, DHALF,                  &
                                     DONE, DTHREE, DEP6, DEP20
  use VersionModule,           only: IDEVELOPMODE
  use BaseModelModule,         only: BaseModelType
  use BaseSolutionModule,      only: BaseSolutionType, AddBaseSolutionToList
  use ListModule,              only: ListType
  use ListsModule,             only: basesolutionlist
  use NumericalModelModule,    only: NumericalModelType,                       &
                                     AddNumericalModelToList,                  &
                                     GetNumericalModelFromList
  use NumericalExchangeModule, only: NumericalExchangeType,                    &
                                     AddNumericalExchangeToList,               &
                                     GetNumericalExchangeFromList
  use SparseModule,            only: sparsematrix
  use SimVariablesModule,      only: iout
  use BlockParserModule,       only: BlockParserType
  use IMSLinearModule

  implicit none
  private
  public :: solution_create

  type, extends(BaseSolutionType) :: NumericalSolutionType
    character(len=LINELENGTH)                            :: fname
    type(ListType)                                       :: modellist
    type(ListType)                                       :: exchangelist
    integer(I4B), pointer                                :: id
    integer(I4B), pointer                                :: iu
    real(DP), pointer                                    :: ttform
    real(DP), pointer                                    :: ttsoln
    integer(I4B), pointer                                :: neq => NULL()
    integer(I4B), pointer                                :: nja => NULL()
    integer(I4B), pointer, dimension(:), contiguous      :: ia => NULL()
    integer(I4B), pointer, dimension(:), contiguous      :: ja => NULL()
    real(DP), pointer, dimension(:), contiguous          :: amat => NULL()
    real(DP), pointer, dimension(:), contiguous          :: rhs => NULL()
    real(DP), pointer, dimension(:), contiguous          :: x => NULL()
    integer(I4B), pointer, dimension(:), contiguous      :: active => NULL()
    real(DP), pointer, dimension(:), contiguous          :: xtemp => NULL()
    type(BlockParserType) :: parser
    !
    !sparse matrix data
    real(DP), pointer                                    :: theta => NULL()
    real(DP), pointer                                    :: akappa => NULL()
    real(DP), pointer                                    :: gamma => NULL()
    real(DP), pointer                                    :: amomentum => NULL()
    real(DP), pointer                                    :: breduc => NULL()
    real(DP), pointer                                    :: btol => NULL()
    real(DP), pointer                                    :: res_lim => NULL()
    real(DP), pointer                                    :: hclose => NULL()
    real(DP), pointer                                    :: hiclose => NULL()
    real(DP), pointer                                    :: bigchold => NULL()
    real(DP), pointer                                    :: bigch => NULL()
    real(DP), pointer                                    :: relaxold => NULL()
    real(DP), pointer                                    :: res_prev => NULL()
    real(DP), pointer                                    :: res_new => NULL()
    real(DP), pointer                                    :: res_in  => NULL()
    integer(I4B), pointer                                :: ibcount => NULL()
    integer(I4B), pointer                                :: icnvg => NULL()
    integer(I4B), pointer                                :: mxiter => NULL()
    integer(I4B), pointer                                :: linmeth => NULL()
    integer(I4B), pointer                                :: nonmeth => NULL()
    integer(I4B), pointer                                :: numtrack => NULL()
    integer(I4B), pointer                                :: iprims => NULL()
    integer(I4B), pointer                                :: ibflag => NULL()
    integer(I4B), dimension(:,:), pointer                :: lrch => NULL()
    real(DP), dimension(:), pointer                      :: hncg => NULL()
    real(DP), dimension(:), pointer                      :: dxold => NULL()
    real(DP), dimension(:), pointer                      :: deold => NULL()
    real(DP), dimension(:), pointer                      :: wsave => NULL()
    real(DP), dimension(:), pointer                      :: hchold => NULL()
    ! summary
    character(len=31), pointer, dimension(:)             :: caccel => NULL()
    integer(I4B), pointer                                :: icsvout => NULL()
    integer(I4B), pointer                                :: nitermax => NULL()
    integer(I4B), pointer                                :: nitercnt => NULL()
    integer(I4B), pointer                                :: convnmod => NULL()
    integer(I4B), pointer, dimension(:), contiguous      :: convmodstart => NULL()
    integer(I4B), pointer, dimension(:), contiguous      :: locdv => NULL()
    integer(I4B), pointer, dimension(:), contiguous      :: locdr => NULL()
    integer(I4B), pointer, dimension(:), contiguous      :: itinner => NULL()
    integer(I4B), pointer, dimension(:,:), contiguous    :: convlocdv => NULL()
    integer(I4B), pointer, dimension(:,:), contiguous    :: convlocdr => NULL()
    real(DP), pointer, dimension(:), contiguous          :: dvmax => NULL()
    real(DP), pointer, dimension(:), contiguous          :: drmax => NULL()
    real(DP), pointer, dimension(:,:), contiguous        :: convdvmax => NULL()
    real(DP), pointer, dimension(:,:), contiguous        :: convdrmax => NULL()
    ! ptc
    integer(I4B), pointer                                :: iallowptc => NULL()
    integer(I4B), pointer                                :: iptcopt => NULL()
    integer(I4B), pointer                                :: iptcout => NULL()
    real(DP), pointer                                    :: l2norm0 => NULL()
    real(DP), pointer                                    :: ptcfact => NULL()
    real(DP), pointer                                    :: ptcdel => NULL()
    real(DP), pointer                                    :: ptcdel0 => NULL()
    real(DP), pointer                                    :: ptcexp => NULL()
    real(DP), pointer                                    :: ptcthresh => NULL()
    real(DP), pointer                                    :: ptcrat => NULL()
    !
    ! linear accelerator storage
    type(IMSLINEAR_DATA), POINTER                        :: imslinear => NULL()
    !
    ! sparse object
    type(sparsematrix)                                   :: sparse

  contains
    procedure :: sln_df
    procedure :: sln_ar
    procedure :: sln_rp
    procedure :: sln_ot
    procedure :: sln_ca
    procedure :: sln_fp
    procedure :: sln_da
    procedure :: addmodel
    procedure :: addexchange
    procedure :: slnassignexchanges
    procedure :: save

    procedure, private :: sln_connect
    procedure, private :: sln_reset
    procedure, private :: sln_ls
    procedure, private :: sln_setouter
    procedure, private :: sln_backtracking
    procedure, private :: sln_backtracking_xupdate
    procedure, private :: sln_l2norm
    procedure, private :: sln_maxval
    procedure, private :: sln_calcdx
    procedure, private :: sln_underrelax
    procedure, private :: sln_outer_check
    procedure, private :: sln_get_loc
    procedure, private :: sln_get_nodeu
    procedure, private :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: convergence_summary
    procedure, private :: csv_convergence_summary

  end type NumericalSolutionType

contains

  subroutine solution_create(filename, id)
! ******************************************************************************
! solution_create -- Create a New Solution
! Using the data in filename,  assign this new solution an id number and store
! the solution in the basesolutionlist.
! Subroutine: (1) allocate solution and assign id and name
!             (2) open the filename for later reading
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: iout
    use InputOutputModule,  only: getunit, openfile
    ! -- dummy
    character(len=*),intent(in) :: filename
    integer(I4B),intent(in) :: id
    ! -- local
    integer(I4B) :: inunit
    type(NumericalSolutionType), pointer :: solution => null()
    class(BaseSolutionType), pointer :: solbase => null()
    character(len=LENSOLUTIONNAME) :: solutionname
! ------------------------------------------------------------------------------
    !
    ! -- Create a new solution and add it to the basesolutionlist container
    allocate(solution)
    solbase => solution
    write(solutionname,'(a, i0)') 'SLN_', id
    call solution%allocate_scalars(solutionname)
    call AddBaseSolutionToList(basesolutionlist, solbase)
    !
    solution%id = id
    !
    ! -- Open solution input file for reading later after problem size is known
    !    Check to see if the file is already opened, which can happen when
    !    running in single model mode
    inquire(file=filename, number=inunit)

    if(inunit < 0) inunit = getunit()
    solution%iu = inunit
    write(iout,'(/a,a)') ' Creating solution: ', solution%name
    call openfile(solution%iu, iout, filename, 'IMS')
    !
    ! -- Initialize block parser
    call solution%parser%Initialize(solution%iu, iout)
    !
    ! -- return
    return
  end subroutine solution_create

  subroutine allocate_scalars(this, solutionname)
! ******************************************************************************
! allocate_scalars -- Allocate scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(NumericalSolutionType) :: this
    character(len=*), intent(in) :: solutionname
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- set value for solution name, which is a member of the base solution
    this%name = solutionname
    !
    ! -- allocate scalars
    call mem_allocate (this%id, 'ID', solutionname)
    call mem_allocate (this%iu, 'IU', solutionname)
    call mem_allocate (this%ttform, 'TTFORM', solutionname)
    call mem_allocate (this%ttsoln, 'TTSOLN', solutionname)
    call mem_allocate(this%neq, 'NEQ', solutionname)
    call mem_allocate(this%nja, 'NJA', solutionname)
    call mem_allocate (this%hclose, 'HCLOSE', solutionname)
    call mem_allocate (this%hiclose, 'HICLOSE', solutionname)
    call mem_allocate (this%bigchold, 'BIGCHOLD', solutionname)
    call mem_allocate (this%bigch, 'BIGCH', solutionname)
    call mem_allocate (this%relaxold, 'RELAXOLD', solutionname)
    call mem_allocate (this%res_prev, 'RES_PREV', solutionname)
    call mem_allocate (this%res_new, 'RES_NEW', solutionname)
    call mem_allocate (this%res_in, 'RES_IN', solutionname)
    call mem_allocate (this%ibcount, 'IBCOUNT', solutionname)
    call mem_allocate (this%icnvg, 'ICNVG', solutionname)
    call mem_allocate (this%mxiter, 'MXITER', solutionname)
    call mem_allocate (this%linmeth, 'LINMETH', solutionname)
    call mem_allocate (this%nonmeth, 'NONMETH', solutionname)
    call mem_allocate (this%iprims, 'IPRIMS', solutionname)
    call mem_allocate (this%theta, 'THETA', solutionname)
    call mem_allocate (this%akappa, 'AKAPPA', solutionname)
    call mem_allocate (this%gamma, 'GAMMA', solutionname)
    call mem_allocate (this%amomentum, 'AMOMENTUM', solutionname)
    call mem_allocate (this%breduc, 'BREDUC', solutionname)
    call mem_allocate (this%btol, 'BTOL', solutionname)
    call mem_allocate (this%res_lim, 'RES_LIM', solutionname)
    call mem_allocate (this%numtrack, 'NUMTRACK', solutionname)
    call mem_allocate (this%ibflag, 'IBFLAG', solutionname)
    call mem_allocate (this%icsvout, 'ICSVOUT', solutionname)
    call mem_allocate (this%nitermax, 'NITERMAX', solutionname)
    call mem_allocate (this%nitercnt, 'NITERCNT', solutionname)
    call mem_allocate(this%convnmod, 'CONVNMOD', solutionname)
    call mem_allocate (this%iallowptc, 'IALLOWPTC', solutionname)
    call mem_allocate (this%iptcopt, 'IPTCOPT', solutionname)
    call mem_allocate (this%iptcout, 'IPTCOUT', solutionname)
    call mem_allocate (this%l2norm0, 'L2NORM0', solutionname)
    call mem_allocate (this%ptcfact, 'PTCFACT', solutionname)
    call mem_allocate (this%ptcdel, 'PTCDEL', solutionname)
    call mem_allocate (this%ptcdel0, 'PTCDEL0', solutionname)
    call mem_allocate (this%ptcexp, 'PTCEXP', solutionname)
    call mem_allocate (this%ptcthresh, 'PTCTHRESH', solutionname)
    call mem_allocate (this%ptcrat, 'PTCRAT', solutionname)
    !
    ! -- initialize
    this%id = 0
    this%iu = 0
    this%ttform = DZERO
    this%ttsoln = DZERO
    this%neq = 0
    this%nja = 0
    this%hclose = DZERO
    this%hiclose = DZERO
    this%bigchold = DZERO
    this%bigch = DZERO
    this%relaxold = DZERO
    this%res_prev = DZERO
    this%res_in = DZERO
    this%ibcount = 0
    this%icnvg = 0
    this%mxiter = 0
    this%linmeth = 1
    this%nonmeth = 0
    this%iprims = 0
    this%theta = DZERO
    this%akappa = DZERO
    this%gamma = DZERO
    this%amomentum = DZERO
    this%breduc = DZERO
    this%btol = 0
    this%res_lim = DZERO
    this%numtrack = 0
    this%ibflag = 0
    this%icsvout = 0
    this%nitermax = 0
    this%nitercnt = 0
    this%convnmod = 0
    this%iallowptc = 1
    this%iptcopt = 0
    this%iptcout = 0
    this%l2norm0 = DZERO
    this%ptcfact = dem1
    this%ptcdel = DZERO
    this%ptcdel0 = DZERO
    this%ptcexp = done
    this%ptcthresh = DEM3
    this%ptcrat = DZERO
    !
    ! -- return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- Allocate arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(NumericalSolutionType) :: this
    ! -- local
    class(NumericalModelType), pointer :: mp
    integer(I4B) :: i
    integer(I4B) :: ieq
! ------------------------------------------------------------------------------
    !
    ! -- initialize the number of models in the solution
    this%convnmod = this%modellist%Count()
    !
    ! -- allocate arrays
    call mem_allocate(this%ia, this%neq + 1, 'IA', this%name)
    call mem_allocate(this%x, this%neq, 'X', this%name)
    call mem_allocate(this%rhs, this%neq, 'RHS', this%name)
    call mem_allocate(this%active, this%neq, 'IACTIVE', this%name)
    call mem_allocate(this%xtemp, this%neq, 'XTEMP', this%name)
    call mem_allocate(this%dxold, this%neq, 'DXOLD', this%name)
    call mem_allocate(this%hncg, 0, 'HNCG', this%name)
    call mem_allocate(this%lrch, 3, 0, 'LRCH', this%name)
    call mem_allocate(this%wsave, 0, 'WSAVE', this%name)
    call mem_allocate(this%hchold, 0, 'HCHOLD', this%name)
    call mem_allocate(this%deold, 0, 'DEOLD', this%name)
    call mem_allocate(this%convmodstart, this%convnmod+1, 'CONVMODSTART', this%name)
    call mem_allocate(this%locdv, this%convnmod, 'LOCDV', this%name)
    call mem_allocate(this%locdr, this%convnmod, 'LOCDR', this%name)
    call mem_allocate(this%itinner, 0, 'ITINNER', this%name)
    call mem_allocate(this%convlocdv, this%convnmod, 0, 'CONVLOCDV', this%name)
    call mem_allocate(this%convlocdr, this%convnmod, 0, 'CONVLOCDR', this%name)
    call mem_allocate(this%dvmax, this%convnmod, 'DVMAX', this%name)
    call mem_allocate(this%drmax, this%convnmod, 'DRMAX', this%name)
    call mem_allocate(this%convdvmax, this%convnmod, 0, 'CONVDVMAX', this%name)
    call mem_allocate(this%convdrmax, this%convnmod, 0, 'CONVDRMAX', this%name)
    !
    ! -- initialize
    do i = 1, this%neq
      this%x(i) = DZERO
      this%active(i) = 1 !default is active
    enddo
    !
    ! -- initialize convmodstart
    ieq = 1
    this%convmodstart(1) = ieq
    do i = 1, this%modellist%Count()
      mp => GetNumericalModelFromList(this%modellist, i)
      ieq = ieq + mp%neq
      this%convmodstart(i+1) = ieq
    end do
    !
    ! -- return
    return
  end subroutine allocate_arrays

  subroutine sln_df(this)
! ******************************************************************************
! sln_df -- Define the solution
! Must be called after the models and exchanges have been added to solution.
! Subroutine: (1) Allocate neq and nja
!             (2) Assign model offsets and solution ids
!             (3) Allocate and initialize the solution arrays
!             (4) Point each model's x and rhs arrays
!             (5) Initialize the sparsematrix instance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(NumericalSolutionType) :: this
    ! -- local
    class(NumericalModelType), pointer :: mp
    integer(I4B) :: i
    integer(I4B), allocatable, dimension(:) :: rowmaxnnz
! ------------------------------------------------------------------------------
    !
    ! -- calculate and set offsets
    do i = 1, this%modellist%Count()
      mp => GetNumericalModelFromList(this%modellist, i)
      call mp%set_idsoln(this%id)
      call mp%set_moffset(this%neq)
      this%neq = this%neq + mp%neq
    enddo
    !
    ! -- Allocate and initialize solution arrays
    call this%allocate_arrays()
    !
    ! -- Go through each model and point x, ibound, and rhs to solution
    do i = 1, this%modellist%Count()
      mp => GetNumericalModelFromList(this%modellist, i)
      call mp%set_xptr(this%x)
      call mp%set_rhsptr(this%rhs)
      call mp%set_iboundptr(this%active)
    enddo
    !
    ! -- Create the sparsematrix instance
    allocate(rowmaxnnz(this%neq))
    do i=1,this%neq
        rowmaxnnz(i)=4
    enddo
    call this%sparse%init(this%neq, this%neq, rowmaxnnz)
    deallocate(rowmaxnnz)
    !
    ! -- Assign connections, fill ia/ja, map connections
    call this%sln_connect()
    !
    ! -- return
    return
  end subroutine sln_df

  subroutine sln_ar(this)
! ******************************************************************************
! sln_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_reallocate
    use SimVariablesModule, only: iout
    use SimModule, only: ustop, store_error, count_errors
    use InputOutputModule, only: getunit, openfile
    ! -- dummy
    class(NumericalSolutionType) :: this
    ! -- local
    class(NumericalModelType), pointer :: mp
    class(NumericalExchangeType), pointer :: cp
    integer(I4B) :: i
    integer(I4B) :: im
    integer(I4B) :: ifdparam, mxvl, npp
    integer(I4B) :: imslinear
    character(len=linelength) :: errmsg, keyword, fname
    integer(I4B) :: isymflg=1
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    integer(I4B) :: ival
    real(DP) :: rval
    character(len=*),parameter :: fmtcsvout = &
      "(4x, 'CSV OUTPUT WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
    character(len=*),parameter :: fmtptcout = &
      "(4x, 'PTC OUTPUT WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
    character(len=*), parameter :: fmterrasym = &
      "(a,' **',a,'** PRODUCES AN ASYMMETRIC COEFFICIENT MATRIX, BUT THE       &
        &CONJUGATE GRADIENT METHOD WAS SELECTED. USE BICGSTAB INSTEAD. ')"
! ------------------------------------------------------------------------------
    !
    ! identify package and initialize.
    WRITE(IOUT,1) this%iu
00001 FORMAT(1X,/1X,'IMS -- ITERATIVE MODEL SOLUTION PACKAGE, VERSION 6',      &
    &  ', 4/28/2017',/,9X,'INPUT READ FROM UNIT',I5)
    !
    ! -- initialize
    i = 1
    ifdparam = 1
    npp = 0
    mxvl = 0
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(iout,'(/1x,a)')'PROCESSING IMS OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('PRINT_OPTION')
          call this%parser%GetStringCaps(keyword)
          if (keyword.eq.'NONE') then
            this%iprims = 0
          else if (keyword.eq.'SUMMARY') then
            this%iprims = 1
          else if (keyword.eq.'ALL') then
            this%iprims = 2
          else
            write(errmsg,'(4x,a,a)') 'IMS sln_ar: UNKNOWN IMS PRINT OPTION: ', &
              trim(keyword)
            call store_error(errmsg)
          end if
        case ('COMPLEXITY')
          call this%parser%GetStringCaps(keyword)
          if (keyword.eq.'SIMPLE') then
            ifdparam = 1
            WRITE(IOUT,21)
          else if (keyword.eq.'MODERATE') then
            ifdparam = 2
            WRITE(IOUT,23)
          else if (keyword.eq.'COMPLEX') then
            ifdparam = 3
            WRITE(IOUT,25)
          else
            write(errmsg,'(4x,a,a)')                                           &
              'IMS sln_ar: UNKNOWN IMS COMPLEXITY OPTION: ',                   &
              trim(keyword)
            call store_error(errmsg)
          end if
        case ('CSV_OUTPUT')
          call this%parser%GetStringCaps(keyword)
          if (keyword == 'FILEOUT') then
            call this%parser%GetString(fname)
            this%icsvout = getunit()
            call openfile(this%icsvout, iout, fname, 'CSV_OUTPUT',  &
                          filstat_opt='REPLACE')
            write(iout,fmtcsvout) trim(fname), this%icsvout
          else
            write(errmsg,'(4x,a)') 'IMS sln_ar: OPTIONAL CSV_OUTPUT ' //       &
              'KEYWORD MUST BE FOLLOWED BY FILEOUT'
            call store_error(errmsg)
          end if
        !
        ! -- right now these are options that are only available in the
        !    development version and are not included in the documentation.
        !    These options are only available when IDEVELOPMODE in
        !    constants module is set to 1
        case ('DEV_PTC')
          call this%parser%DevOpt()
          this%iallowptc = 1
          write(IOUT,'(1x,A)') 'PSEUDO-TRANSIENT CONTINUATION ENABLED'
        case ('DEV_NO_PTC')
          call this%parser%DevOpt()
          this%iallowptc = 0
          write(IOUT,'(1x,A)') 'PSEUDO-TRANSIENT CONTINUATION DISABLED'
        case('DEV_PTC_OUTPUT')
          call this%parser%DevOpt()
          this%iallowptc = 1
          call this%parser%GetStringCaps(keyword)
          if (keyword == 'FILEOUT') then
            call this%parser%GetString(fname)
            this%iptcout = getunit()
            call openfile(this%iptcout, iout, fname, 'PTC-OUT',                &
                          filstat_opt='REPLACE')
            write(iout,fmtptcout) trim(fname), this%iptcout
          else
            write(errmsg,'(4x,a)') 'IMS sln_ar: OPTIONAL PTC_OUTPUT ' //       &
              'KEYWORD MUST BE FOLLOWED BY FILEOUT'
            call store_error(errmsg)
          end if
        case ('DEV_PTC_OPTION')
          call this%parser%DevOpt()
          this%iallowptc = 1
          this%iptcopt = 1
          write(IOUT,'(1x,A)')                                                 &
            'PSEUDO-TRANSIENT CONTINUATION USES BNORM AND L2NORM TO ' //       &
            'SET INITIAL VALUE'
        case ('DEV_PTC_EXPONENT')
          call this%parser%DevOpt()
          rval = this%parser%GetDouble()
          if (rval < DZERO) then
            write(errmsg,'(4x,a)') 'IMS sln_ar: PTC_EXPONENT MUST BE > 0.'
            call store_error(errmsg)
          else
            this%iallowptc = 1
            this%ptcexp = rval
            write(IOUT,'(1x,A,1x,g15.7)')                                      &
              'PSEUDO-TRANSIENT CONTINUATION EXPONENT', this%ptcexp
          end if
        case ('DEV_PTC_THRESHOLD')
          call this%parser%DevOpt()
          rval = this%parser%GetDouble()
          if (rval < DZERO) then
            write(errmsg,'(4x,a)')'IMS sln_ar: PTC_THRESHOLD MUST BE > 0.'
            call store_error(errmsg)
          else
            this%iallowptc = 1
            this%ptcthresh = rval
            write(IOUT,'(1x,A,1x,g15.7)')                                      &
              'PSEUDO-TRANSIENT CONTINUATION THRESHOLD', this%ptcthresh
          end if
        case ('DEV_PTC_DEL0')
          call this%parser%DevOpt()
          rval = this%parser%GetDouble()
          if (rval < DZERO) then
            write(errmsg,'(4x,a)')'IMS sln_ar: PTC_DEL0 MUST BE > 0.'
            call store_error(errmsg)
          else
            this%iallowptc = 1
            this%ptcdel0 = rval
            write(IOUT,'(1x,A,1x,g15.7)')                                      &
              'PSEUDO-TRANSIENT CONTINUATION INITIAL TIMESTEP', this%ptcdel0
          end if
        case default
          write(errmsg,'(4x,a,a)') 'IMS sln_ar: UNKNOWN IMS OPTION: ',         &
            trim(keyword)
          call store_error(errmsg)
        end select
      end do
      write(iout,'(1x,a)')'END OF IMS OPTIONS'
    else
      write(iout,'(1x,a)')'NO IMS OPTION BLOCK DETECTED.'
    end if

00020 FORMAT(1X,'SPECIFIED OPTION:',/,                                         &
    &       1X,'SOLVER INPUT VALUES WILL BE USER-SPECIFIED')
00021 FORMAT(1X,'SIMPLE OPTION:',/,                                            &
    &       1X,'DEFAULT SOLVER INPUT VALUES FOR FAST SOLUTIONS')
00023 FORMAT(1X,'MODERATE OPTION:',/,1X,'DEFAULT SOLVER',                      &
    &          ' INPUT VALUES REFLECT MODERETELY NONLINEAR MODEL')
00025 FORMAT(1X,'COMPLEX OPTION:',/,1X,'DEFAULT SOLVER',                       &
    & ' INPUT VALUES REFLECT STRONGLY NONLINEAR MODEL')

    !-------READ NONLINEAR ITERATION PARAMETERS AND LINEAR SOLVER SELECTION INDEX
    ! -- set default nonlinear parameters
    call this%sln_setouter(ifdparam)
    !
    ! -- get NONLINEAR block
    call this%parser%GetBlock('NONLINEAR', isfound, ierr)
    !
    ! -- parse NONLINEAR block if detected
    if (isfound) then
      write(iout,'(/1x,a)')'PROCESSING IMS NONLINEAR'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        ! -- parse keyword
        select case (keyword)
        case ('OUTER_HCLOSE')
          this%hclose  = this%parser%GetDouble()
        case ('OUTER_MAXIMUM')
          this%mxiter  = this%parser%GetInteger()
        case ('UNDER_RELAXATION')
          call this%parser%GetStringCaps(keyword)
          ival = 0
          if (keyword == 'NONE') then
            ival = 0
          else if (keyword == 'SIMPLE') then
            ival = 1
          else if (keyword == 'COOLEY') then
            ival = 2
          else if (keyword == 'DBD') then
            ival = 3
          else
            write(errmsg,'(1x,a)') 'IMS sln_ar: UNKNOWN UNDER_RELAXATION SPECIFIED.'
            call store_error(errmsg)
          end if
          this%nonmeth = ival
        case ('LINEAR_SOLVER')
          call this%parser%GetStringCaps(keyword)
          ival = 1
          if (keyword.eq.'DEFAULT' .or.                                        &
              keyword.eq.'LINEAR') then
            ival = 1
          else
            write(errmsg,'(1x,a)') 'IMS sln_ar: UNKNOWN LINEAR_SOLVER SPECIFIED.'
            call store_error(errmsg)
          end if
          this%linmeth = ival
        case ('UNDER_RELAXATION_THETA')
          this%theta = this%parser%GetDouble()
        case ('UNDER_RELAXATION_KAPPA')
          this%akappa = this%parser%GetDouble()
        case ('UNDER_RELAXATION_GAMMA')
          this%gamma = this%parser%GetDouble()
        case ('UNDER_RELAXATION_MOMENTUM')
          this%amomentum  = this%parser%GetDouble()
        case ('BACKTRACKING_NUMBER')
          this%numtrack = this%parser%GetInteger()
          IF (this%numtrack > 0) this%ibflag = 1
        case ('BACKTRACKING_TOLERANCE')
          this%btol = this%parser%GetDouble()
        case ('BACKTRACKING_REDUCTION_FACTOR')
          this%breduc = this%parser%GetDouble()
        case ('BACKTRACKING_RESIDUAL_LIMIT')
          this%res_lim = this%parser%GetDouble()
        case default
          write(errmsg,'(4x,a,a)')'IMS sln_ar: UNKNOWN IMS NONLINEAR KEYWORD: ', &
            trim(keyword)
          call store_error(errmsg)
        end select
      end do
      write(iout,'(1x,a)') 'END OF IMS NONLINEAR DATA'
    else
      if (IFDPARAM.EQ.0) then
        write(errmsg,'(1x,a)') 'NO IMS NONLINEAR BLOCK DETECTED.'
        call store_error(errmsg)
      end if
    end if
    !
    IF ( THIS%THETA.LT.DEM3 ) this%theta = DEM3
    !
    ! -- backtracking should only be used if this%nonmeth > 0
    if (this%nonmeth < 1) then
      this%ibflag = 0
    end if
    !
    !-------ECHO INPUT OF NONLINEAR ITERATION PARAMETERS AND LINEAR SOLVER INDEX
    WRITE(IOUT,9002) this%hclose,this%mxiter,this%iprims,this%nonmeth,this%linmeth
    !
9002 FORMAT(1X,'OUTER ITERATION CONVERGENCE CRITERION     (HCLOSE) = ', E15.6, &
    &      /1X,'MAXIMUM NUMBER OF OUTER ITERATIONS        (MXITER) = ', I9,    &
    &      /1X,'SOLVER PRINTOUT INDEX                     (IPRIMS) = ',I9,     &
    &      /1X,'NONLINEAR ITERATION METHOD            (NONLINMETH) = ',I9,     &
    &      /1X,'LINEAR SOLUTION METHOD                   (LINMETH) = ',I9)
    !
    IF(THIS%NONMETH.NE.0)THEN
      WRITE(IOUT,9003) this%theta, this%akappa, this%gamma, this%amomentum,    &
                       this%numtrack
      IF(THIS%NUMTRACK.NE.0) WRITE(IOUT,9004) this%btol,this%breduc,this%res_lim
    ENDIF

9003 FORMAT(1X,'UNDER-RELAXATION WEIGHT REDUCTION FACTOR   (THETA) = ', E15.6, &
    &      /1X,'UNDER-RELAXATION WEIGHT INCREASE INCREMENT (KAPPA) = ', E15.6, &
    &      /1X,'UNDER-RELAXATION PREVIOUS HISTORY FACTOR   (GAMMA) = ', E15.6, &
    &      /1X,'UNDER-RELAXATIONMOMENTUM TERM          (AMOMENTUM) = ', E15.6, &
    &      /1X,'   MAXIMUM NUMBER OF BACKTRACKS         (NUMTRACK) = ',I9)
9004 FORMAT(1X,'BACKTRACKING TOLERANCE FACTOR               (BTOL) = ', E15.6, &
    &      /1X,'BACKTRACKING REDUCTION FACTOR             (BREDUC) = ', E15.6, &
    &      /1X,'BACKTRACKING RESIDUAL LIMIT              (RES_LIM) = ', E15.6)

    if(this%mxiter.le.0) then
      write (errmsg,'(a)') 'IMS sln_ar: OUTER ITERATION NUMBER MUST BE > 0.'
      call store_error(errmsg)
    END IF

    isymflg = 1
    if ( this%nonmeth.gt.0 )then
      WRITE(IOUT,*) '**UNDER-RELAXATION WILL BE USED***'
      WRITE(IOUT,*)
      isymflg = 0
    elseif ( this%nonmeth.eq.0 )then
      WRITE(IOUT,*) '***UNDER-RELAXATION WILL NOT BE USED***'
      WRITE(IOUT,*)
    ELSE
      WRITE(errmsg,'(a)') '***INCORRECT VALUE FOR VARIABLE NONMETH ',          &
        &                      'WAS SPECIFIED. CHECK INPUT.***'
      call store_error(errmsg)
    END IF
    ! call secondary subroutine to initialize and read linear solver parameters
    ! IMSLINEAR solver
    if ( this%linmeth==1 )then
      allocate(this%imslinear)
      WRITE(IOUT,*) '***IMS LINEAR SOLVER WILL BE USED***'
      call this%imslinear%imslinear_allocate(this%name, this%iu, IOUT,         &
                                             this%iprims, this%mxiter,         &
                                             ifdparam, imslinear,              &
                                             this%neq, this%nja, this%ia,      &
                                             this%ja, this%amat, this%rhs,     &
                                             this%x, this%nitermax)
      WRITE(IOUT,*)
      isymflg = 0
      if ( imslinear.eq.1 ) isymflg = 1
    ! incorrect linear solver flag
    ELSE
      WRITE(errmsg, *) '***INCORRECT VALUE FOR LINEAR SOLUTION ', &
        &                'METHOD SPECIFIED. CHECK INPUT.***'
      call store_error(errmsg)
    END IF

    !
    ! -- If CG, then go through each model and each exchange and check
    !    for asymmetry
    if (isymflg == 1) then
      !
      ! -- Models
      do i = 1, this%modellist%Count()
        mp => GetNumericalModelFromList(this%modellist, i)
        if (mp%get_iasym() /= 0) then
          write(errmsg, fmterrasym) 'MODEL', trim(adjustl(mp%name))
          call store_error(errmsg)
        endif
      enddo
      !
      ! -- Exchanges
      do i = 1, this%exchangelist%Count()
        cp => GetNumericalExchangeFromList(this%exchangelist, i)
        if (cp%get_iasym() /= 0) then
          write(errmsg, fmterrasym) 'EXCHANGE', trim(adjustl(cp%name))
          call store_error(errmsg)
        endif
      enddo
      !
    endif

    ! -- write summary of solver error messages
    ierr = count_errors()
    if (ierr>0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! reallocate space for nonlinear arrays and initialize
    call mem_reallocate(this%hncg, this%mxiter, 'HNCG', this%name)
    call mem_reallocate(this%lrch, 3, this%mxiter, 'LRCH', this%name)

    ! delta-bar-delta under-relaxation
    if(this%nonmeth.eq.3)then
      call mem_reallocate(this%wsave, this%neq, 'WSAVE', this%name)
      call mem_reallocate(this%hchold, this%neq, 'HCHOLD', this%name)
      call mem_reallocate(this%deold, this%neq, 'DEOLD', this%name)
      do i = 1, this%neq
        this%wsave(i) = DZERO
        this%hchold(i) = DZERO
        this%deold(i) = DZERO
      end do
    endif
    this%hncg = DZERO
    this%lrch = 0

    ! allocate space for saving solver convergence history
    if (this%iprims == 2) then
      this%nitermax = this%nitermax * this%mxiter
    else
      this%nitermax = 1
    end if

    allocate(this%caccel(this%nitermax))

    im = this%convnmod
    call mem_reallocate(this%itinner, this%nitermax, 'ITINNER',                &
                        trim(this%name))
    call mem_reallocate(this%convlocdv, im, this%nitermax, 'CONVLOCDV',        &
                        trim(this%name))
    call mem_reallocate(this%convlocdr, im, this%nitermax, 'CONVLOCDR',        &
                        trim(this%name))
    call mem_reallocate(this%convdvmax, im, this%nitermax, 'CONVDVMAX',        &
                        trim(this%name))
    call mem_reallocate(this%convdrmax, im, this%nitermax, 'CONVDRMAX',        &
                        trim(this%name))
    do i = 1, this%nitermax
      this%itinner(i) = 0
      do im = 1, this%convnmod
        this%convlocdv(im, i) = 0
        this%convlocdr(im, i) = 0
        this%convdvmax(im, i) = DZERO
        this%convdrmax(im, i) = DZERO
      end do
    end do
    !
    ! close ims input file
    call this%parser%Clear()
    !
    ! -- return
    return
  end subroutine sln_ar

  subroutine sln_rp(this)
! ******************************************************************************
! sln_rp -- Read and Prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: readnewdata
    ! -- dummy
    class(NumericalSolutionType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Check with TDIS on whether or not it is time to RP
    if (.not. readnewdata) return
    !
    ! -- return
    return
  end subroutine sln_rp

  subroutine sln_ot(this)
! ******************************************************************************
! sln_ot -- Output
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Nothing to do here
    !
    ! -- return
    return
  end subroutine sln_ot

  subroutine sln_fp(this)
! ******************************************************************************
! sln_fp -- Final processing
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Nothing to do here
    if (IDEVELOPMODE == 1) then
      write(this%imslinear%iout, '(//1x,a,1x,a,1x,a)')                         &
        'Solution', trim(adjustl(this%name)), 'summary'
      write(this%imslinear%iout, "(1x,70('-'))")
      write(this%imslinear%iout, '(1x,a,1x,g0,1x,a)')                          &
        'Total formulate time: ', this%ttform, 'seconds'
      write(this%imslinear%iout, '(1x,a,1x,g0,1x,a,/)')                        &
        'Total solution time:  ', this%ttsoln, 'seconds'
    end if
    !
    ! -- return
    return
  end subroutine sln_fp

  subroutine sln_da(this)
! ******************************************************************************
! sln_da -- Deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(NumericalSolutionType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- IMSLinearModule
    call this%imslinear%imslinear_da()
    deallocate(this%imslinear)
    !
    ! -- lists
    call this%modellist%Clear()
    call this%exchangelist%Clear()
    !
    ! -- character arrays
    deallocate(this%caccel)
    !
    ! -- arrays
    call mem_deallocate(this%ja)
    call mem_deallocate(this%amat)
    call mem_deallocate(this%ia)
    call mem_deallocate(this%x)
    call mem_deallocate(this%rhs)
    call mem_deallocate(this%active)
    call mem_deallocate(this%xtemp)
    call mem_deallocate(this%dxold)
    call mem_deallocate(this%hncg)
    call mem_deallocate(this%lrch)
    call mem_deallocate(this%wsave)
    call mem_deallocate(this%hchold)
    call mem_deallocate(this%deold)
    call mem_deallocate(this%convmodstart)
    call mem_deallocate(this%locdv)
    call mem_deallocate(this%locdr)
    call mem_deallocate(this%itinner)
    call mem_deallocate(this%convlocdv)
    call mem_deallocate(this%convlocdr)
    call mem_deallocate(this%dvmax)
    call mem_deallocate(this%drmax)
    call mem_deallocate(this%convdvmax)
    call mem_deallocate(this%convdrmax)
    !
    ! -- Scalars
    call mem_deallocate(this%id)
    call mem_deallocate(this%iu)
    call mem_deallocate(this%ttform)
    call mem_deallocate(this%ttsoln)
    call mem_deallocate(this%neq)
    call mem_deallocate(this%nja)
    call mem_deallocate(this%hclose)
    call mem_deallocate(this%hiclose)
    call mem_deallocate(this%bigchold)
    call mem_deallocate(this%bigch)
    call mem_deallocate(this%relaxold)
    call mem_deallocate(this%res_prev)
    call mem_deallocate(this%res_new)
    call mem_deallocate(this%res_in)
    call mem_deallocate(this%ibcount)
    call mem_deallocate(this%icnvg)
    call mem_deallocate(this%mxiter)
    call mem_deallocate(this%linmeth)
    call mem_deallocate(this%nonmeth)
    call mem_deallocate(this%iprims)
    call mem_deallocate(this%theta)
    call mem_deallocate(this%akappa)
    call mem_deallocate(this%gamma)
    call mem_deallocate(this%amomentum)
    call mem_deallocate(this%breduc)
    call mem_deallocate(this%btol)
    call mem_deallocate(this%res_lim)
    call mem_deallocate(this%numtrack)
    call mem_deallocate(this%ibflag)
    call mem_deallocate(this%icsvout)
    call mem_deallocate(this%nitermax)
    call mem_deallocate(this%nitercnt)
    call mem_deallocate(this%convnmod)
    call mem_deallocate(this%iallowptc)
    call mem_deallocate(this%iptcopt)
    call mem_deallocate(this%iptcout)
    call mem_deallocate(this%l2norm0)
    call mem_deallocate(this%ptcfact)
    call mem_deallocate(this%ptcdel)
    call mem_deallocate(this%ptcdel0)
    call mem_deallocate(this%ptcexp)
    call mem_deallocate(this%ptcthresh)
    call mem_deallocate(this%ptcrat)
    !
    ! -- return
    return
  end subroutine sln_da

  subroutine sln_ca(this, kstp, kper, kpicard, isgcnvg, isuppress_output)
! ******************************************************************************
! sln_ca -- Solve the models in this solution for kper and kstp.  If necessary
!           use subtiming to get to the end of the time step
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only:iout
    use TdisModule, only: subtiming_begin, subtiming_end, perlen, totimsav
    ! -- dummy
    class(NumericalSolutionType) :: this
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    integer(I4B), intent(in) :: kpicard
    integer(I4B), intent(inout) :: isgcnvg
    integer(I4B), intent(in) :: isuppress_output
    ! -- local
    class(NumericalModelType), pointer :: mp
    class(NumericalExchangeType), pointer :: cp
    character(len=16) :: cval
    character(len=34) :: strh
    integer(I4B) :: im, ic
    integer(I4B) :: kiter
    integer(I4B) :: iter
    integer(I4B) :: nsubtimes, nstm, isubtime
    integer(I4B) :: itertot
    integer(I4B) :: inewtonur
    integer(I4B) :: itestmat, n
    integer(I4B) :: i0, i1
    integer(I4B) :: iend
    integer(I4B) :: iptc
    integer(I4B) :: nodeu
    real(DP) :: ptcf
    real(DP) :: dt
    real(DP) :: totim
    real(DP) :: ttform
    real(DP) :: ttsoln
    real(DP) :: dxmax
    ! -- formats
    character(len=*), parameter :: fmtnocnvg =                                 &
      "(1X,'Solution ', i0, ' did not converge for stress period ', i0,        &
       ' and time step ', i0)"
 11 FORMAT(//1X,'OUTER ITERATION SUMMARY',/,1x,139('-'),/,                     &
        18x,'     OUTER     INNER BACKTRACK BACKTRACK        INCOMING        ',&
           'OUTGOING         MAXIMUM                    MAXIMUM CHANGE',/,     &
        18x,' ITERATION ITERATION      FLAG    NUMBER        RESIDUAL        ',&
           'RESIDUAL          CHANGE                    MODEL-(CELLID)',/,     &
          1x,139('-'))
 12 FORMAT(//1X,'OUTER ITERATION SUMMARY',/,1x,87('-'),/,                      &
         18x,'     OUTER     INNER         MAXIMUM                    ',       &
            'MAXIMUM CHANGE',/,                                                &
         18x,' ITERATION ITERATION          CHANGE                    ',       &
            'MODEL-(CELLID)',/,                                                &
         1x,87('-'))
! ------------------------------------------------------------------------------
    !
    ! -- write header for csv output
    if (kper == 1 .and. kstp == 1) then
      if (this%icsvout > 0) then
        write(this%icsvout, '(*(G0,:,","))', advance='NO')                     &
          'total_iterations', 'totim', 'kper', 'kstp', 'ksub', 'nouter',       &
          'ninner', 'solution_dvmax', 'solution_dvmax_model',                  &
          'solution_dvmax_node'
        if (this%iprims == 2) then
          write(this%icsvout, '(*(G0,:,","))', advance='NO')                   &
            '', 'solution_drmax', 'solution_drmax_model',                      &
            'solution_drmax_node', 'solution_alpha'
          if (this%imslinear%ilinmeth == 2) then
            write(this%icsvout, '(*(G0,:,","))', advance='NO')                 &
              '', 'solution_omega'
          end if
          ! -- check for more than one model
          if (this%convnmod > 1) then
            do im=1,this%modellist%Count()
              mp => GetNumericalModelFromList(this%modellist, im)
              write(this%icsvout, '(*(G0,:,","))', advance='NO')               &
                '', trim(adjustl(mp%name)) // '_dvmax',                        &
                trim(adjustl(mp%name)) // '_dvmax_node',                       &
                trim(adjustl(mp%name)) // '_drmax',                            &
                trim(adjustl(mp%name)) // '_drmax_node'
            end do
          end if
        end if
        write (this%icsvout,'(a)') ''
      end if
    end if
    !
    ! -- Find the number of sub-timesteps for each model and then use
    !    the largest one.
    nsubtimes = 1
    do im=1,this%modellist%Count()
      mp => GetNumericalModelFromList(this%modellist, im)
      nstm = mp%get_nsubtimes()
      if(nstm > nsubtimes) nsubtimes = nstm
    enddo
    !
    itertot = 0
    dt = perlen(kper) / REAL(nsubtimes, DP)
    totim = totimsav
    !
    do isubtime = 1, nsubtimes
      !
      ! -- update totim
      totim = totim + dt
      !
      ! -- Start subtiming
      call subtiming_begin(isubtime, nsubtimes, this%id)
      !
      ! -- Exchange advance
      do ic=1,this%exchangelist%Count()
        cp => GetNumericalExchangeFromList(this%exchangelist, ic)
        call cp%exg_ad(this%id, kpicard, isubtime)
      enddo
      !
      ! -- Model advance
      do im = 1, this%modellist%Count()
        mp => GetNumericalModelFromList(this%modellist, im)
        call mp%model_ad(kpicard, isubtime)
      enddo
      !
      ! -- determine if PTC will be used in any model
      n = 1
      do im = 1, this%modellist%Count()
        mp => GetNumericalModelFromList(this%modellist, im)
        call mp%model_ptcchk(iptc)
        iptc = iptc * this%iallowptc
        if (iptc /= 0) then
          if (n == 1) then
            write (iout, '(//)')
            n = 0
          end if
          write (iout, '(1x,a,1x,i0,1x,3a)')                                           &
            'PSEUDO-TRANSIENT CONTINUATION WILL BE APPLIED TO MODEL', im, '("',        &
            trim(adjustl(mp%name)), '") DURING THIS TIME STEP'
        end if
      enddo
      
      !
      ! -- Nonlinear iteration loop for this solution
      this%icnvg = 0
      outerloop: do kiter = 1, this%mxiter
        !
        ! --backtracking
        if (this%numtrack > 0) then
          if (kiter == 1) then
            ! -- write header for solver output
            if (this%iprims > 0) then
              write (iout,11)
            end if
          end if
          !
          ! -- call backtracking
          call this%sln_backtracking(mp, cp, kiter)
        else
          if (kiter == 1) then
            ! -- write header for solver output
            if (this%iprims > 0) then
              write (iout,12)
            end if
          end if
        end if
        !
        ! -- Set amat and rhs to zero
        call this%sln_reset()
        call code_timer(0, ttform, this%ttform)
        !
        ! -- Calculate the matrix terms for each exchange
        do ic=1,this%exchangelist%Count()
          cp => GetNumericalExchangeFromList(this%exchangelist, ic)
          call cp%exg_cf(kiter)
        enddo
        !
        ! -- Calculate the matrix terms for each model
        do im=1,this%modellist%Count()
          mp => GetNumericalModelFromList(this%modellist, im)
          call mp%model_cf(kiter)
        enddo
        !
        ! -- Add exchange coefficients to the solution
        do ic=1,this%exchangelist%Count()
          cp => GetNumericalExchangeFromList(this%exchangelist, ic)
          call cp%exg_fc(kiter, this%ia, this%amat, 1)
        enddo
        !
        ! -- Add model coefficients to the solution
        do im=1,this%modellist%Count()
          mp => GetNumericalModelFromList(this%modellist, im)
          call mp%model_fc(kiter, this%amat, this%nja, 1)
        enddo
        !
        ! -- Add exchange Newton-Raphson terms to solution
        do ic=1,this%exchangelist%Count()
          cp => GetNumericalExchangeFromList(this%exchangelist, ic)
          call cp%exg_nr(kiter, this%ia, this%amat)
        enddo
        !
        ! -- Calculate pseudo-transient continuation factor for each model
        iptc = 0
        ptcf = DZERO
        do im=1,this%modellist%Count()
          mp => GetNumericalModelFromList(this%modellist, im)
          call mp%model_ptc(kiter, this%neq, this%nja,                         &
                            this%ia, this%ja, this%x,                          &
                            this%rhs, this%amat,                               &
                            iptc, ptcf)
        end do
        !
        ! -- Add model Newton-Raphson terms to solution
        do im=1,this%modellist%Count()
          mp => GetNumericalModelFromList(this%modellist, im)
          call mp%model_nr(kiter, this%amat, this%nja, 1)
        enddo
        call code_timer(1, ttform, this%ttform)
        !
        ! -- linear solve
        call code_timer(0, ttsoln, this%ttsoln)
        CALL this%sln_ls(kiter,kstp,kper,iter,itertot,iptc,ptcf)
        call code_timer(1, ttsoln, this%ttsoln)
        !
        !-------------------------------------------------------
        itestmat = 0
        if(itestmat.eq.1)then
            open(99,file='sol_MF6.TXT')
        WRITE(99,*)'MATRIX SOLUTION FOLLOWS'
        WRITE(99,67)(n,this%x(N),N=1,this%NEQ)
67      FORMAT(10(I8,G15.4))
        close(99)
        stop
        endif
        !-------------------------------------------------------
        ! -- check convergence of solution
        call this%sln_outer_check(this%hncg(kiter), this%lrch(1,kiter))
        if (this%icnvg /= 0) then
          this%icnvg = 0
          if (abs(this%hncg(kiter)) <= this%hclose) this%icnvg = 1
        end if
        !
        ! -- Additional convergence check for pseudo-transient continuation
        !    term. Evaluate if the ptc value added to the diagonal has
        !    decayed sufficiently.
        if (iptc > 0) then
          if (this%icnvg /= 0) then
            if (this%ptcrat > this%ptcthresh) then
              this%icnvg = 0
              if (kiter == this%mxiter) then
                write(*,*) 'pseudo-transient continuation caused convergence failure'
              end if
            end if
          end if
        end if
        !
        ! -- Additional convergence check for exchanges
        do ic=1,this%exchangelist%Count()
          cp => GetNumericalExchangeFromList(this%exchangelist, ic)
          call cp%exg_cc(this%icnvg)
        enddo
        !
        ! -- additional convergence check for model packages
        if (this%icnvg == 1) then
          iend = 0
          if (kiter == this%mxiter) then
            iend = 1
          end if
          do im=1,this%modellist%Count()
            mp => GetNumericalModelFromList(this%modellist, im)
            call mp%model_cc(kiter, iend, this%icnvg)
          enddo
        end if
        !
        !--write maximum head change from linear solver to list file
        itertot = itertot + iter
        if (this%iprims > 0) then
          cval = 'Linear Solver   '
          call this%sln_get_loc(this%lrch(1,kiter), strh)
          if (this%numtrack > 0) then
            WRITE(IOUT,22) cval, kiter, iter, this%hncg(kiter),               &
                           adjustr(trim(strh))
          else
            WRITE(IOUT,23) cval, kiter, iter, this%hncg(kiter),               &
                           adjustr(trim(strh))
          end if
        end if
        !
        ! -- dampening
        if (this%icnvg /= 1) then 
          if (this%nonmeth > 0) then
            call this%sln_underrelax(kiter, this%hncg(kiter), this%neq,        &
                                     this%active, this%x, this%xtemp)
          else
            call this%sln_calcdx(this%neq, this%active,                        &
                                 this%x, this%xtemp, this%dxold)
          endif
          !
          ! --adjust heads if necessary
          inewtonur = 0
          do im=1,this%modellist%Count()
            mp => GetNumericalModelFromList(this%modellist, im)
            i0 = mp%moffset + 1
            i1 = i0 + mp%neq - 1
            call mp%model_nur(mp%neq, this%x(i0:i1), this%xtemp(i0:i1),        &
                              this%dxold(i0:i1), inewtonur)
          end do
          !
          ! --update maximum head change
          call this%sln_outer_check(this%hncg(kiter), this%lrch(1,kiter))
          if (inewtonur /= 0) then
            call this%sln_maxval(this%neq, this%dxold, dxmax)
            if (abs(dxmax) <= this%hclose .and.                                &
                abs(this%hncg(kiter)) <= this%hclose) then
              this%icnvg = 1
            end if
          end if
          !
          !--write maximum head change after under relaxation to list file
          !itertot = itertot + iter
          if (this%iprims > 0) then
            cval = 'Under-relaxation'
            call this%sln_get_loc(this%lrch(1,kiter), strh)
            if (this%numtrack > 0) then
             WRITE(IOUT,24) cval, kiter, this%hncg(kiter), adjustr(trim(strh))
            else
             WRITE(IOUT,25) cval, kiter, this%hncg(kiter), adjustr(trim(strh))
            end if
          end if
        end if
22    FORMAT(1X,A16,1X,I10,I10,53X,1PG15.6,A34)
23    FORMAT(1X,A16,1X,I10,I10,1X,1PG15.6,A34)
24    FORMAT(1X,A16,1X,I10,10X,53X,1PG15.6,A34)
25    FORMAT(1X,A16,1X,I10,10X,1X,1PG15.6,A34)
        !
        ! -- Write a message if convergence was not achieved
        if (kiter == this%mxiter) then
          write(iout, fmtnocnvg) this%id, kper, kstp
        end if
        !
        ! -- Exit outer iteration loop if converged
        if (this%icnvg == 1) then
          if (this%iprims > 0) then
            write(iout,1010) kiter, kstp, kper, itertot
          end if
          exit outerloop
        end if
        !
        ! -- End of outer iteration loop
      end do outerloop

01010 format(/1X,I0,' CALLS TO NUMERICAL SOLUTION ','IN TIME STEP ',I0,        &
            ' STRESS PERIOD ',I0,/1X,I0,' TOTAL ITERATIONS')
      !
      ! -- write inner iteration convergence summary
      if (this%iprims == 2) then
        !
        ! -- write summary for each model
        do im=1,this%modellist%Count()
          mp => GetNumericalModelFromList(this%modellist, im)
          call this%convergence_summary(mp%iout, im, itertot)
        end do
        !
        ! -- write summary for entire solution
        call this%convergence_summary(iout, this%convnmod+1, itertot)
      end if
      !
      ! -- write to csv file
      if (this%icsvout > 0) then
        if (this%iprims < 2) then
          !
          ! -- determine the total number of iterations at the end of this outer
          this%nitercnt = this%nitercnt + itertot
          !
          ! -- get model number and user node number
          call this%sln_get_nodeu(this%lrch(1,kiter), im, nodeu)
          !
          ! -- write line
          write(this%icsvout, '(*(G0,:,","))')                                 &
             this%nitercnt, totim, kper, kstp, isubtime, kiter, itertot,       &
             this%hncg(kiter), im, nodeu
        else
          call this%csv_convergence_summary(this%icsvout, totim, kper, kstp,   &
                                            isubtime, itertot)
        end if
      end if
      !
      !
      if (this%icnvg == 0) isgcnvg = 0
      !
      ! -- Calculate flow for each model
      do im=1,this%modellist%Count()
        mp => GetNumericalModelFromList(this%modellist, im)
        call mp%model_cq(this%icnvg, isuppress_output)
      enddo
      !
      ! -- Calculate flow for each exchange
      do ic = 1, this%exchangelist%Count()
        cp => GetNumericalExchangeFromList(this%exchangelist, ic)
        call cp%exg_cq(isgcnvg, isuppress_output, this%id)
      enddo
      !
      ! -- Budget terms for each model
      do im=1,this%modellist%Count()
        mp => GetNumericalModelFromList(this%modellist, im)
        call mp%model_bd(this%icnvg, isuppress_output)
      enddo
      !
      ! -- Budget terms for each exchange
      do ic = 1, this%exchangelist%Count()
        cp => GetNumericalExchangeFromList(this%exchangelist, ic)
        call cp%exg_bd(isgcnvg, isuppress_output, this%id)
      enddo
      !
      ! -- End of the sub-timestep loop
    enddo
    !
    ! -- end the subtiming
    call subtiming_end()
    !
    !
    ! -- Check if convergence for the exchange packages
    do ic = 1, this%exchangelist%Count()
      cp => GetNumericalExchangeFromList(this%exchangelist, ic)
      call cp%exg_cnvg(this%id, isgcnvg)
    enddo
    !
    ! -- return
    return
  end subroutine sln_ca

  subroutine convergence_summary(this, iu, im, itertot)
! ******************************************************************************
! convergence_summary -- Save convergence summary to a File
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only:getunit
    ! -- dummy
    class(NumericalSolutionType) :: this
    integer(I4B), intent(in) :: iu
    integer(I4B), intent(in) :: im
    integer(I4B), intent(in) :: itertot
    ! -- local
    character(len=34) :: strh, strr
    integer(I4B) :: i
    integer(I4B) :: i0
    integer(I4B) :: iouter
    integer(I4B) :: j
    integer(I4B) :: k
    integer(I4B) :: locdv
    integer(I4B) :: locdr
    real(DP) :: dv
    real(DP) :: dr
! ------------------------------------------------------------------------------
    iouter = 1
    write(iu,"(/,1x,A)") 'INNER ITERATION SUMMARY'
    write(iu,"(1x,128('-'))")
    write(iu,'(1x,3a)') '    TOTAL      OUTER     INNER',                      &
      '                    MAXIMUM CHANGE        MAXIMUM',                     &
      '                  MAXIMUM RESIDUAL        MAXIMUM'
    write(iu,'(1x,3a)') 'ITERATION  ITERATION ITERATION',                      &
      '                    MODEL-(CELLID)         CHANGE',                     &
      '                    MODEL-(CELLID)       RESIDUAL'
    write(iu,"(1x,128('-'))")
    i0 = 0
    do k = 1, itertot
      i = this%itinner(k)
      if (i <= i0) then
        iouter = iouter + 1
      end if
      if (im > this%convnmod) then
        dv = DZERO
        dr = DZERO
        do j = 1, this%convnmod
          if (ABS(this%convdvmax(j, k)) > ABS(dv)) then
            locdv = this%convlocdv(j, k)
            dv = this%convdvmax(j, k)
          end if
          if (ABS(this%convdrmax(j, k)) > ABS(dr)) then
            locdr = this%convlocdr(j, k)
            dr = this%convdrmax(j, k)
          end if
        end do
      else
        locdv = this%convlocdv(im, k)
        locdr = this%convlocdr(im, k)
        dv = this%convdvmax(im, k)
        dr = this%convdrmax(im, k)
      end if
      call this%sln_get_loc(locdv, strh)
      call this%sln_get_loc(locdr, strr)
      write(iu, '(1x,3i10,a34,g15.7,a34,g15.7)') k, iouter, i,                 &
                                                 adjustr(trim(strh)), dv,      &
                                                 adjustr(trim(strr)), dr
      i0 = i
    end do
    !
    ! -- write blank line
    if (im <= this%convnmod) then
      write(iu, '(a)') ''
    end if
    !
    ! -- return
    return
  end subroutine convergence_summary


  subroutine csv_convergence_summary(this, iu, totim, kper, kstp, isubtime,    &
                                     itertot)
! ******************************************************************************
! csv_convergence_summary -- Save convergence summary to a csv file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only:getunit
    ! -- dummy
    class(NumericalSolutionType) :: this
    integer(I4B), intent(in) :: iu
    real(DP), intent(in) :: totim
    integer(I4B), intent(in) :: kper
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: isubtime
    integer(I4B), intent(in) :: itertot
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: i0
    integer(I4B) :: iouter
    integer(I4B) :: im
    integer(I4B) :: j
    integer(I4B) :: k
    integer(I4B) :: locdv
    integer(I4B) :: locdr
    integer(I4B) :: nodeu
    real(DP) :: dv
    real(DP) :: dr
! ------------------------------------------------------------------------------
    iouter = 1
    i0 = 0
    do k = 1, itertot
      this%nitercnt = this%nitercnt + 1
      i = this%itinner(k)
      if (i <= i0) then
        iouter = iouter + 1
      end if
      write(iu, '(*(G0,:,","))', advance='NO')                                 &
        this%nitercnt, totim, kper, kstp, isubtime, iouter, i
      !
      ! -- solution summary
      dv = DZERO
      dr = DZERO
      do j = 1, this%convnmod
        if (ABS(this%convdvmax(j, k)) > ABS(dv)) then
          locdv = this%convlocdv(j, k)
          dv = this%convdvmax(j, k)
        end if
        if (ABS(this%convdrmax(j, k)) > ABS(dr)) then
          locdr = this%convlocdr(j, k)
          dr = this%convdrmax(j, k)
        end if
      end do
      !
      ! -- get model number and user node number for dv
      call this%sln_get_nodeu(locdv, im, nodeu)
      write(iu, '(*(G0,:,","))', advance='NO') '', dv, im, nodeu
      !
      ! -- get model number and user node number for dr
      call this%sln_get_nodeu(locdr, im, nodeu)
      write(iu, '(*(G0,:,","))', advance='NO') '', dr, im, nodeu
      !
      ! -- write acceleration parameters
      write(iu, '(*(G0,:,","))', advance='NO') '', trim(adjustl(this%caccel(k)))
      !
      ! -- write information for each model
      if (this%convnmod > 1) then
        do j = 1, this%convnmod
          locdv = this%convlocdv(j, k)
          dv = this%convdvmax(j, k)
          locdr = this%convlocdr(j, k)
          dr = this%convdrmax(j, k)
          !
          ! -- get model number and user node number for dv
          call this%sln_get_nodeu(locdv, im, nodeu)
          write(iu, '(*(G0,:,","))', advance='NO') '', dv, nodeu
          !
          ! -- get model number and user node number for dr
          call this%sln_get_nodeu(locdr, im, nodeu)
          write(iu, '(*(G0,:,","))', advance='NO') '', dr, nodeu
        end do
      end if
      !
      ! -- write line
      write (iu,'(a)') ''
      !
      ! -- update i0
      i0 = i
    end do
    !
    ! -- return
    return
  end subroutine csv_convergence_summary

  subroutine save(this, filename)
! ******************************************************************************
! save -- Save Solution Matrices to a File
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only:getunit
    ! -- dummy
    class(NumericalSolutionType) :: this
    character(len=*), intent(in) :: filename
    ! -- local
    integer(I4B) :: inunit
! ------------------------------------------------------------------------------
    !
    inunit = getunit()
    open(unit=inunit,file=filename,status='unknown')
    write(inunit,*) 'ia'
    write(inunit,*) this%ia
    write(inunit,*) 'ja'
    write(inunit,*) this%ja
    write(inunit,*) 'amat'
    write(inunit,*) this%amat
    write(inunit,*) 'rhs'
    write(inunit,*) this%rhs
    write(inunit,*) 'x'
    write(inunit,*) this%x
    close(inunit)
    !
    ! -- return
    return
  end subroutine save

  subroutine addmodel(this, mp)
! ******************************************************************************
! addmodel -- Add Model
! Subroutine: (1) add a model to this%modellist
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType) :: this
    class(BaseModelType), pointer, intent(in) :: mp
    ! -- local
    class(NumericalModelType), pointer :: m
! ------------------------------------------------------------------------------
    !
    select type(mp)
    class is (NumericalModelType)
      m => mp
      call AddNumericalModelToList(this%modellist, m)
    end select
    !
    ! -- return
    return
  end subroutine addmodel

  subroutine addexchange(this, exchange)
! ******************************************************************************
! addexchange -- Add exchange
! Subroutine: (1) add an exchange to this%exchangelist
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType) :: this
    class(NumericalExchangeType), pointer, intent(in) :: exchange
! ------------------------------------------------------------------------------
    !
    call AddNumericalExchangeToList(this%exchangelist, exchange)
    !
    ! -- return
    return
  end subroutine addexchange

  subroutine slnassignexchanges(this)
! ******************************************************************************
! slnassignexchanges -- Assign exchanges to this solution
! Subroutine: (1) assign the appropriate exchanges to this%exchangelist
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use BaseExchangeModule, only: BaseExchangeType, GetBaseExchangeFromList
    use ListsModule, only: baseexchangelist
    ! -- dummy
    class(NumericalSolutionType) :: this
    ! -- local
    class(BaseExchangeType), pointer :: cb
    class(NumericalExchangeType), pointer :: c
    integer(I4B) :: ic
! ------------------------------------------------------------------------------
    !
    ! -- Go through the list of exchange objects and if either model1 or model2
    !    are part of this solution, then include the exchange object as part of
    !    this solution.
    c => null()
    do ic=1,baseexchangelist%Count()
      cb => GetBaseExchangeFromList(baseexchangelist, ic)
      select type (cb)
      class is (NumericalExchangeType)
        c=>cb
      end select
      if(associated(c)) then
        if(c%m1%idsoln==this%id) then
          call this%addexchange(c)
          cycle
        elseif(c%m2%idsoln==this%id) then
          call this%addexchange(c)
          cycle
        endif
      endif
    enddo
    !
    ! -- return
    return
  end subroutine slnassignexchanges

  subroutine sln_connect(this)
! ******************************************************************************
! sln_connect -- Assign Connections
! Main workhorse method for solution.  This goes through all the models and all
! the connections and builds up the sparse matrix.
! Subroutine: (1) Add internal model connections,
!             (2) Add cross terms,
!             (3) Allocate solution arrays
!             (4) Create mapping arrays
!             (5) Fill cross term values if necessary
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(NumericalSolutionType) :: this
    ! -- local
    class(NumericalModelType), pointer :: mp
    class(NumericalExchangeType), pointer :: cp
    integer(I4B) :: im, ic, ierror
! ------------------------------------------------------------------------------
    !
    ! -- Add internal model connections to sparse
    do im=1,this%modellist%Count()
      mp => GetNumericalModelFromList(this%modellist, im)
      call mp%model_ac(this%sparse)
    enddo
    !
    ! -- Add the cross terms to sparse
    do ic=1,this%exchangelist%Count()
      cp => GetNumericalExchangeFromList(this%exchangelist, ic)
      call cp%exg_ac(this%sparse)
    enddo
    !
    ! -- The number of non-zero array values are now known so
    ! -- ia and ja can be created from sparse. then destroy sparse
    this%nja=this%sparse%nnz
    call mem_allocate(this%ja, this%nja, 'JA', this%name)
    call mem_allocate(this%amat, this%nja, 'AMAT', this%name)
    call this%sparse%sort()
    call this%sparse%filliaja(this%ia,this%ja,ierror)
    call this%sparse%destroy()
    !
    ! -- Create mapping arrays for each model.  Mapping assumes
    ! -- that each row has the diagonal in the first position,
    ! -- however, rows do not need to be sorted.
    do im=1,this%modellist%Count()
      mp => GetNumericalModelFromList(this%modellist, im)
      call mp%model_mc(this%ia, this%ja)
    enddo
    !
    ! -- Create arrays for mapping exchange connections to global solution
    do ic=1,this%exchangelist%Count()
      cp => GetNumericalExchangeFromList(this%exchangelist, ic)
      call cp%exg_mc(this%ia, this%ja)
    enddo
    !
    ! -- return
    return
  end subroutine sln_connect

  subroutine sln_reset(this)
! ******************************************************************************
! sln_reset -- Reset This Solution
! Reset this solution by setting amat and rhs to zero
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType) :: this
    ! -- local
    integer(I4B) :: i
    real(DP) :: zero = 0.d0
! ------------------------------------------------------------------------------
    !
    do i=1,this%nja
      this%amat(i) = zero
    enddo
    do i=1,this%neq
        this%rhs(i) = zero
    enddo
    !
    ! -- return
    return
  end subroutine sln_reset
!
  subroutine sln_ls(this, kiter, kstp, kper, in_iter, itersum, iptc, ptcf)
! ******************************************************************************
! perform residual reduction and newton linearization and
! prepare for sparse solver, and check convergence of nonlinearities
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    class(NumericalSolutionType), intent(inout) :: this
    integer(I4B), intent(in) :: kiter
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    integer(I4B), intent(inout) :: in_iter
    integer(I4B), intent(in) :: itersum
    integer(I4B), intent(inout) :: iptc
    real(DP), intent(in) :: ptcf
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: itestmat,i,i1,i2
    integer(I4B) :: iptct
    real(DP) :: adiag, diagval
    real(DP) :: l2norm
    real(DP) :: ptcval
    real(DP) :: diagmin
    real(DP) :: bnorm
    character(len=50) :: fname
    character(len=*), parameter :: fmtfname = "('mf6mat_', i0, '_', i0, &
      '_', i0, '_', i0, '.txt')"
! ------------------------------------------------------------------------------
    !
    ! -- take care of loose ends for all nodes before call to solver
    do n = 1, this%neq
      ! -- store x in temporary location
      this%xtemp(n) = this%x(n)
      ! -- set dirichlet boundary and no-flow condition
      if (this%active(n) <= 0) then
        this%amat(this%ia(n)) = DONE
        this%rhs(n) = this%x(n)
        i1 = this%ia(n) + 1
        i2 = this%ia(n + 1) - 1
        do i = i1, i2
          this%amat(i) = DZERO
        enddo
      else
        ! -- take care of zero row diagonal
        diagval = DONE
        adiag = abs(this%amat(this%ia(n)))
        if(adiag.lt.DEM15)then
          this%amat(this%ia(n)) = diagval
          this%rhs(n) = this%rhs(n) + this%x(n) * diagval
        endif
      endif
    end do
    ! -- pseudo transient continuation
    iptct = iptc * this%iallowptc
    if (iptct /= 0) then
      call this%sln_l2norm(this%neq, this%nja,                                 &
                           this%ia, this%ja, this%active,                      &
                           this%amat, this%rhs, this%x, l2norm)
      ! -- confirm that the l2norm exceeds previous l2norm
      !    if not, there is no need to add ptc terms
      if (kiter == 1) then
        if (kper > 1 .or. kstp > 1) then
          if (l2norm <= this%l2norm0) then
            iptc = 0
          end if
        end if
      end if
    end if
    iptct = iptc * this%iallowptc
    if (iptct /= 0) then
      if (kiter == 1) then
        if (this%iptcout > 0) then
          write(this%iptcout, '(A10,6(1x,A15),2(1x,A15))') 'OUTER ITER',       &
            '         PTCDEL', '        L2NORM0', '         L2NORM',           &
            '        RHSNORM', '       1/PTCDEL', '  DIAGONAL MIN.',           &
            ' RHSNORM/L2NORM', ' STOPPING CRIT.'
        end if
        if (this%ptcdel0 > DZERO) then
          this%ptcdel = this%ptcdel0
        else
          if (this%iptcopt == 0) then
            this%ptcdel = done / ptcf
          else
            bnorm = DZERO
            do n = 1, this%neq
              if (this%active(n).gt.0) then
                bnorm = bnorm + this%rhs(n) * this%rhs(n)
              end if
            end do
            bnorm = sqrt(bnorm)
            this%ptcdel = bnorm / l2norm
          end if
        end if
      else
        if (l2norm > DZERO) then
          this%ptcdel = this%ptcdel * (this%l2norm0 / l2norm)**this%ptcexp
        else
          this%ptcdel = DZERO
        end if
      end if
      if (this%ptcdel > DZERO) then
        ptcval = done / this%ptcdel
      else
        ptcval = done
      end if
      diagmin = DEP20
      bnorm = DZERO
      do n = 1, this%neq
        if (this%active(n).gt.0) then
          diagval = abs(this%amat(this%ia(n)))
          bnorm = bnorm + this%rhs(n) * this%rhs(n)
          if (diagval < diagmin) diagmin = diagval
          this%amat(this%ia(n)) = this%amat(this%ia(n)) - ptcval
          this%rhs(n) = this%rhs(n) - ptcval * this%x(n)
        end if
      end do
      bnorm = sqrt(bnorm)
      if (this%iptcout > 0) then
        write(this%iptcout, '(i10,6(1x,e15.7),2(1x,f15.6))')                   &
          kiter, this%ptcdel, this%l2norm0, l2norm, bnorm,                     &
          ptcval, diagmin, bnorm/l2norm, ptcval / diagmin
      end if
      this%l2norm0 = l2norm
    end if


  !-------------------------------------------------------
      itestmat = 0
      if(itestmat == 1) then
        write(fname, fmtfname) this%id, kper, kstp, kiter
        print *, 'Saving amat to: ', trim(adjustl(fname))
        open(99,file=trim(adjustl(fname)))
        WRITE(99,*)'NODE, RHS, AMAT FOLLOW'
        DO N=1,this%NEQ
          I1 = this%IA(N)
          I2 = this%IA(N+1)-1
          WRITE(99,'(*(G0,:,","))') N, this%RHS(N), (this%ja(i),i=i1,i2), &
                        (this%AMAT(I),I=I1,I2)
        ENDDO
66      FORMAT(I9,1X,G15.6,2X,100G15.6)
        close(99)
        !stop
      endif
  !-------------------------------------------------------
    !
    ! call appropriate linear solver
    ! call ims linear solver
    if (this%linmeth == 1) then
      call this%imslinear%imslinear_apply(this%icnvg, kstp, kiter, in_iter,    &
                                          this%nitermax,                       &
                                          this%convnmod, this%convmodstart,    &
                                          this%locdv, this%locdr,              &
                                          this%caccel, this%itinner,           &
                                          this%convlocdv, this%convlocdr,      &
                                          this%dvmax, this%drmax,              &
                                          this%convdvmax, this%convdrmax)
    end if
    !
    ! ptc finalize - set ratio of ptc value added to the diagonal and the
    !                minimum value on the diagonal. This value will be used
    !                to determine if the make sure the ptc value has decayed
    !                sufficiently
    if (iptct /= 0) then
      this%ptcrat = ptcval / diagmin
    end if
    !
    ! -- return
    return
  end subroutine sln_ls

  !
  subroutine sln_setouter(this, ifdparam)
! ******************************************************************************
! sln_setouter
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType), intent(inout) :: this
    integer(I4B), intent(in) :: ifdparam
! ------------------------------------------------------------------------------
    !
    ! -- simple option
    select case ( ifdparam )
    case ( 1 )
      this%hclose = dem3
      this%mxiter = 25
      this%nonmeth = 0
      this%theta = 1.0
      this%akappa = DZERO
      this%gamma = DZERO
      this%amomentum = DZERO
      this%numtrack = 0
      this%btol = DZERO
      this%breduc = DZERO
      this%res_lim = DZERO
    !
    ! -- moderate
    case ( 2 )
      this%hclose = dem2
      this%mxiter = 50
      this%nonmeth = 3
      this%theta = 0.9d0
      this%akappa = 0.0001d0
      this%gamma = DZERO
      this%amomentum = DZERO
      this%numtrack = 0
      this%btol = DZERO
      this%breduc = DZERO
      this%res_lim = DZERO
    !
    ! -- complex
    case ( 3 )
      this%hclose = dem1
      this%mxiter = 100
      this%nonmeth = 3
      this%theta = 0.8d0
      this%akappa = 0.0001d0
      this%gamma = DZERO
      this%amomentum = DZERO
      this%numtrack = 20
      this%btol = 1.05d0
      this%breduc = 0.1d0
      this%res_lim = 0.002d0
    end select
    !
    ! -- return
    return
  end subroutine sln_setouter

  subroutine sln_backtracking(this, mp, cp, kiter)
! ******************************************************************************
! sln_backtracking
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType), intent(inout) :: this
    class(NumericalModelType), pointer :: mp
    class(NumericalExchangeType), pointer :: cp
    integer(I4B), intent(in) :: kiter
    ! -- local
    character (len=16) :: cval
    integer(I4B) :: ic
    integer(I4B) :: im
    integer(I4B) :: nb
    integer(I4B) :: btflag
    integer(I4B) :: ibflag
    integer(I4B) :: ibtcnt
    real(DP) :: resin
! ------------------------------------------------------------------------------
    !
    ibflag = 0
    !
    ! -- refill amat and rhs with standard conductance
    ! -- Set amat and rhs to zero
    call this%sln_reset()
    !
    ! -- Calculate matrix coefficients (CF) for each exchange
    do ic=1,this%exchangelist%Count()
      cp => GetNumericalExchangeFromList(this%exchangelist, ic)
      call cp%exg_cf(kiter)
    end do
    !
    ! -- Calculate matrix coefficients (CF) for each model
    do im=1,this%modellist%Count()
      mp => GetNumericalModelFromList(this%modellist, im)
      call mp%model_cf(kiter)
    end do
    !
    ! -- Fill coefficients (FC) for each exchange
    do ic=1,this%exchangelist%Count()
      cp => GetNumericalExchangeFromList(this%exchangelist, ic)
      call cp%exg_fc(kiter, this%ia, this%amat, 0)
    end do
    !
    ! -- Fill coefficients (FC) for each model
    do im=1,this%modellist%Count()
      mp => GetNumericalModelFromList(this%modellist, im)
      call mp%model_fc(kiter, this%amat, this%nja, 0)
    end do
    !
    ! -- calculate initial l2 norm
    if (kiter == 1) then
      call this%sln_l2norm(this%neq, this%nja,                                 &
                            this%ia, this%ja, this%active,                     &
                            this%amat, this%rhs, this%x, this%res_prev)
      resin = this%res_prev
      ibflag = 0
    else
      call this%sln_l2norm(this%neq, this%nja,                                 &
                            this%ia, this%ja, this%active,                     &
                            this%amat, this%rhs, this%x, this%res_new)
      resin = this%res_new
    end if
    ibtcnt = 0
    if (kiter > 1) then
      if (this%res_new > this%res_prev * this%btol) then
        btloop: do nb = 1, this%numtrack
          !
          ! -- backtrack heads
          call this%sln_backtracking_xupdate(btflag)
          !
          ! -- head change less than hclose
          if (btflag == 0) then
            ibflag = 4
            exit btloop
          end if
          !
          ibtcnt = nb
          !
          ! -- Set amat and rhs to zero
          call this%sln_reset()
          !
          ! -- Calculate matrix coefficients (CF) for each exchange
          do ic=1,this%exchangelist%Count()
            cp => GetNumericalExchangeFromList(this%exchangelist, ic)
            call cp%exg_cf(kiter)
          end do
          !
          ! -- Calculate matrix coefficients (CF) for each model
          do im=1,this%modellist%Count()
            mp => GetNumericalModelFromList(this%modellist, im)
            call mp%model_cf(kiter)
          end do
          !
          ! -- Fill coefficients (FC) for each exchange
          do ic=1,this%exchangelist%Count()
            cp => GetNumericalExchangeFromList(this%exchangelist, ic)
            call cp%exg_fc(kiter, this%ia, this%amat, 0)
          end do
          !
          ! -- Fill coefficients (FC) for each model
          do im=1,this%modellist%Count()
            mp => GetNumericalModelFromList(this%modellist, im)
            call mp%model_fc(kiter, this%amat, this%nja, 0)
          end do
          !
          ! -- calculate updated l2norm
          call this%sln_l2norm(this%neq, this%nja,                             &
                               this%ia, this%ja, this%active,                  &
                                this%amat, this%rhs, this%x, this%res_new)
          !
          ! -- evaluate if back tracking can be terminated
          if (nb == this%numtrack) then
            ibflag = 2
            exit btloop
          end if
          if (this%res_new < this%res_prev * this%btol) then
              ibflag = 1
            exit btloop
          end if
          if (this%res_new < this%res_lim) then
            exit btloop
          end if
        end do btloop
      end if
      ! -- save new residual
      this%res_prev = this%res_new
    end if
    !
    ! -- write backtracking results
66  FORMAT(1X,A16,1X,I10,10X,I10,I10,1X,1PG15.6,1X,1PG15.6)
    WRITE(IOUT,66) 'Backtracking    ', kiter, ibflag, ibtcnt,                  &
                    resin, this%res_prev
    !
    ! -- return
    return
  end subroutine sln_backtracking

  subroutine sln_backtracking_xupdate(this, btflag)
! ******************************************************************************
! sln_backtracking_xupdate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType), intent(inout) :: this
    integer(I4B), intent(inout) :: btflag
    ! -- local
    integer(I4B) :: n
    real(DP) :: delx
    real(DP) :: absdelx
    real(DP) :: chmax
! ------------------------------------------------------------------------------
    !
    btflag = 0
    ! no backtracking if maximum change is less than closure so return
    chmax = 0.0
    do n=1, this%neq
      if (this%active(n) < 1) cycle
      delx = this%breduc*(this%x(n) - this%xtemp(n))
      absdelx = abs(delx)
      if(absdelx > chmax) chmax = absdelx
    end do
    ! perform backtracking if free of constraints and set counter and flag
    if (chmax >= this%hclose) then
      btflag = 1
      do n = 1, this%neq
        if (this%active(n) < 1) cycle
        delx = this%breduc*(this%x(n) - this%xtemp(n))
        this%x(n) = this%xtemp(n) + delx
      end do
    end if
    !
    ! -- return
    return
  end subroutine sln_backtracking_xupdate

  subroutine sln_l2norm(this, neq, nja, ia, ja, active, amat, rhs, x, resid)
! ******************************************************************************
! sln_l2norm
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType), intent(inout) :: this
    integer(I4B), intent(in) :: neq
    integer(I4B), intent(in) :: nja
    integer(I4B), dimension(neq+1), intent(in) :: ia
    integer(I4B), dimension(nja), intent(in) :: ja
    integer(I4B), dimension(neq), intent(in) :: active
    real(DP), dimension(nja), intent(in) :: amat
    real(DP), dimension(neq), intent(in) :: rhs
    real(DP), dimension(neq), intent(in) :: x
    real(DP), intent(inout) :: resid
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: j, jcol
    real(DP) :: rowsum
! ------------------------------------------------------------------------------
    !
    resid = DZERO
    do n = 1, neq
      if (active(n) > 0) then
        rowsum = DZERO
        do j = ia(n), ia(n+1)-1
          jcol = ja(j)
          rowsum = rowsum + amat(j) * x(jcol)
        end do
        ! compute mean square residual from q of each node
        resid = resid +  (rowsum - rhs(n))**2
      end if
    end do
    ! -- l2norm is the square root of the sum of the square of the residuals
    resid = sqrt(resid)
    !
    ! -- return
    return
  end subroutine sln_l2norm
  
  subroutine sln_maxval(this, neq, v, vnorm)
! ******************************************************************************
! sln_l2norm
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType), intent(inout) :: this
    integer(I4B), intent(in) :: neq
    real(DP), dimension(neq), intent(in) :: v
    real(DP), intent(inout) :: vnorm
    ! -- local
    integer(I4B) :: n
    real(DP) :: d
! ------------------------------------------------------------------------------ 
    vnorm = DZERO
    do n = 1, neq
      d = v(n)
      if (abs(d) > vnorm) then
        vnorm = d
      end if
    end do
    !
    ! -- return
    return
  end subroutine sln_maxval
  
  subroutine sln_calcdx(this, neq, active, x, xtemp, dx)
! ******************************************************************************
! sln_l2norm
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType), intent(inout) :: this
    integer(I4B), intent(in) :: neq
    integer(I4B), dimension(neq), intent(in) :: active
    real(DP), dimension(neq), intent(in) :: x
    real(DP), dimension(neq), intent(in) :: xtemp
    real(DP), dimension(neq), intent(inout) :: dx
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------ 
    do n = 1, neq
      ! -- skip inactive nodes
      if (active(n) < 1) then
        dx(n) = DZERO
      else
        dx(n) = x(n) - xtemp(n)
      end if
    end do
    !
    ! -- return
    return
  end subroutine sln_calcdx
  

  subroutine sln_underrelax(this, kiter, bigch, neq, active, x, xtemp)
! ******************************************************************************
! under relax using delta-bar-delta or cooley formula
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType), intent(inout) :: this
    integer(I4B), intent(in) :: kiter
    real(DP), intent(in) :: bigch
    integer(I4B), intent(in) :: neq
    integer(I4B), dimension(neq), intent(in) :: active
    real(DP), dimension(neq), intent(inout) :: x
    real(DP), dimension(neq), intent(in) :: xtemp
    ! -- local
    real(DP) :: ww, delx, relax, es, aes, amom
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- option for using simple dampening (as done by MODFLOW-2005 PCG)
    if (this%nonmeth == 1) then
      do n = 1, neq
        ! -- skip inactive nodes
        if (active(n) < 1) cycle
        !
        ! -- compute step-size (delta x)
        delx = x(n) - xtemp(n)
        this%dxold(n) = delx

        ! -- dampen head solution
        x(n) = xtemp(n) + this%gamma * delx
      end do
    !
    ! -- option for using cooley underrelaxation
    else if (this%nonmeth == 2) then
      if (kiter == 1) then
        relax = done
        this%relaxold = done
        this%bigch = bigch
        this%bigchold = bigch
      else
        ! -- compute relaxation factor
        es = this%bigch / (this%bigchold * this%relaxold)
        aes = abs(es)
        if (es.lt.-done) then
          relax = dhalf / aes
        else
          relax = (dthree + es) / (dthree + aes)
        end if
      end if
      this%relaxold = relax
      !
      ! -- modify cooley to use exponential average of past changes
      this%bigchold = (done - this%gamma) * this%bigch  + this%gamma *         &
                      this%bigchold
      ! -- this method does it right after newton - need to do it after
      !    underrelaxation and backtracking.
      !
      ! -- compute new head after under-relaxation
      if (relax.lt.done) then
        do n = 1, neq
          if (active(n) < 1) cycle
          delx = x(n) - xtemp(n)
          this%dxold(n) = delx
          x(n) = xtemp(n) + relax * delx
        end do
      end if
    !
    ! -- option for using delta-bar-delta scheme to under-relax for all equations
    else if (this%nonmeth == 3) then
      do n = 1, neq
        ! -- skip inactive nodes
        if (active(n) < 1) cycle
        !
        ! -- compute step-size (delta x) and initialize d-b-d parameters
        delx = x(n) - xtemp(n)

        if ( kiter.eq.1 ) then
          this%wsave(n) = DONE
          this%hchold(n) = DEM20
          this%deold(n) = DZERO
        end if
        !
        ! -- compute new relaxation term as per delta-bar-delta
        ww = this%wsave(n)

        ! for flip-flop condition, decrease factor
        if ( this%deold(n)*delx < DZERO ) then
          ww = this%theta * this%wsave(n)
          ! -- when change is of same sign, increase factor
        else
          ww = this%wsave(n) + this%akappa
        end if
        if ( ww.gt.done ) ww = done
        this%wsave(n) = ww

        ! -- compute exponential average of past changes in hchold
        if (kiter == 1) then
          ! -- this method does it right after newton
          ! -- need to do it after underrelaxation and backtracking.
          this%hchold(n) = delx
        else
          this%hchold(n) = (DONE - this%gamma) * delx +                        &
                           this%gamma * this%hchold(n)
        end if
        !
        ! -- store slope (change) term for next iteration
        this%deold(n) = delx
        this%dxold(n) = delx
        !
        ! -- compute accepted step-size and new head
        amom = DZERO
        if (kiter.gt.4) amom = this%amomentum
        delx = delx * ww + amom * this%hchold(n)
        x(n) = xtemp(n) + delx
      end do
      !
    end if
    !
    ! -- return
    return
  end subroutine sln_underrelax

  subroutine sln_outer_check(this, hncg, lrch)
! ******************************************************************************
! sln_outer_check
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType), intent(inout) :: this
    real(DP), intent(inout) :: hncg
    integer(I4B), intent(inout) :: lrch
    ! -- local
    integer(I4B) :: nb
    real(DP) :: bigch
    real(DP) :: abigch
    integer(I4B) :: n
    real(DP) :: hdif
    real(DP) :: ahdif
! ------------------------------------------------------------------------------
    !
    nb = 1
    bigch = DZERO
    abigch = DZERO
    do n = 1, this%neq
      if(this%active(n) < 1) cycle
      hdif = this%x(n) - this%xtemp(n)
      ahdif = abs(hdif)
      if (ahdif >= abigch) then
        bigch = hdif
        abigch = ahdif
        nb = n
      end if
    end do
    !
    !-----store maximum change value and location
    hncg = bigch
    lrch = nb
    !
    ! -- return
    return
  end subroutine sln_outer_check

  subroutine sln_get_loc(this, nodesln, str)
! ******************************************************************************
! sln_get_loc
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType), intent(inout) :: this
    integer(I4B), intent(in) :: nodesln
    character(len=*), intent(inout) :: str
    ! -- local
    class(NumericalModelType),pointer :: mp
    integer(I4B) :: i
    integer(I4B) :: istart
    integer(I4B) :: iend
    integer(I4B) :: noder
! ------------------------------------------------------------------------------
    !
    ! -- calculate and set offsets
    noder = 0
    do i = 1, this%modellist%Count()
      mp => GetNumericalModelFromList(this%modellist, i)
      call mp%get_mrange(istart, iend)
      if (nodesln >= istart .and. nodesln <= iend) then
        noder = nodesln - istart + 1
        call mp%get_mcellid(noder, str)
        exit
      end if
    end do
    !
    ! -- return
    return
  end subroutine sln_get_loc

  subroutine sln_get_nodeu(this, nodesln, im, nodeu)
! ******************************************************************************
! sln_get_nodeu
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalSolutionType), intent(inout) :: this
    integer(I4B), intent(in) :: nodesln
    integer(I4B), intent(inout) :: im
    integer(I4B), intent(inout) :: nodeu
    ! -- local
    class(NumericalModelType),pointer :: mp
    integer(I4B) :: i
    integer(I4B) :: istart
    integer(I4B) :: iend
    integer(I4B) :: noder
! ------------------------------------------------------------------------------
    !
    ! -- calculate and set offsets
    noder = 0
    do i = 1, this%modellist%Count()
      mp => GetNumericalModelFromList(this%modellist, i)
      call mp%get_mrange(istart, iend)
      if (nodesln >= istart .and. nodesln <= iend) then
        noder = nodesln - istart + 1
        call mp%get_mnodeu(noder, nodeu)
        im = i
        exit
      end if
    end do
    !
    ! -- return
    return
  end subroutine sln_get_nodeu

end module NumericalSolutionModule




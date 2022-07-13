module GwfDisModule

  use ArrayReadersModule, only: ReadArray
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, DHALF, DZERO
  use BaseDisModule, only: DisBaseType
  use InputOutputModule, only: get_node, URWORD, ulasav, ulaprufw, ubdsv1, &
                               ubdsv06
  use SimModule, only: count_errors, store_error, store_error_unit
  use BlockParserModule, only: BlockParserType
  use MemoryManagerModule, only: mem_allocate
  use TdisModule, only: kstp, kper, pertim, totim, delt

  implicit none
  private
  public dis_cr, dis_init_mem, GwfDisType

  type, extends(DisBaseType) :: GwfDisType
    integer(I4B), pointer :: nlay => null() ! number of layers
    integer(I4B), pointer :: nrow => null() ! number of rows
    integer(I4B), pointer :: ncol => null() ! number of columns
    real(DP), dimension(:), pointer, contiguous :: delr => null() ! spacing along a row
    real(DP), dimension(:), pointer, contiguous :: delc => null() ! spacing along a column
    real(DP), dimension(:, :), pointer, contiguous :: top2d => null() ! top elevations for each cell at top of model (ncol, nrow)
    real(DP), dimension(:, :, :), pointer, contiguous :: bot3d => null() ! bottom elevations for each cell (ncol, nrow, nlay)
    integer(I4B), dimension(:, :, :), pointer, contiguous :: idomain => null() ! idomain (ncol, nrow, nlay)
    real(DP), dimension(:, :, :), pointer :: botm => null() ! top and bottom elevations for each cell (ncol, nrow, nlay)
    real(DP), dimension(:), pointer, contiguous :: cellx => null() ! cell center x coordinate for column j
    real(DP), dimension(:), pointer, contiguous :: celly => null() ! cell center y coordinate for row i
  contains
    procedure :: dis_df => dis3d_df
    procedure :: dis_da => dis3d_da
    procedure :: get_cellxy => get_cellxy_dis3d
    procedure :: get_dis_type => get_dis_type
    procedure, public :: record_array
    procedure, public :: read_layer_array
    procedure, public :: record_srcdst_list_header
    procedure, public :: nlarray_to_nodelist
    ! -- helper functions
    procedure :: get_nodenumber_idx1
    procedure :: get_nodenumber_idx3
    procedure :: nodeu_to_string
    procedure :: nodeu_to_array
    procedure :: nodeu_from_string
    procedure :: nodeu_from_cellid
    procedure :: supports_layers
    procedure :: get_ncpl
    procedure :: connection_vector
    procedure :: connection_normal
    ! -- private
    procedure :: read_options
    procedure :: read_dimensions
    procedure :: read_mf6_griddata
    procedure :: grid_finalize
    procedure :: write_grb
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    !
    ! -- Read a node-sized model array (reduced or not)
    procedure :: read_int_array
    procedure :: read_dbl_array
  end type GwfDisType

contains

  subroutine dis_cr(dis, name_model, inunit, iout)
! ******************************************************************************
! dis_cr -- Create a new discretization 3d object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(DisBaseType), pointer :: dis
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwfDisType), pointer :: disnew
! ------------------------------------------------------------------------------
    allocate (disnew)
    dis => disnew
    call disnew%allocate_scalars(name_model)
    dis%inunit = inunit
    dis%iout = iout
    !
    ! -- Initialize block parser
    call dis%parser%Initialize(dis%inunit, dis%iout)
    !
    ! -- Return
    return
  end subroutine dis_cr

  subroutine dis_init_mem(dis, name_model, iout, nlay, nrow, ncol, &
                          delr, delc, top2d, bot3d, idomain)
! ******************************************************************************
! dis_init_mem -- Create a new discretization 3d object from memory
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(DisBaseType), pointer :: dis
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(in) :: nlay
    integer(I4B), intent(in) :: nrow
    integer(I4B), intent(in) :: ncol
    real(DP), dimension(:), pointer, contiguous, intent(in) :: delr
    real(DP), dimension(:), pointer, contiguous, intent(in) :: delc
    real(DP), dimension(:, :), pointer, contiguous, intent(in) :: top2d
    real(DP), dimension(:, :, :), pointer, contiguous, intent(in) :: bot3d
    integer(I4B), dimension(:, :, :), pointer, contiguous, intent(in), &
      optional :: idomain
    ! -- local
    type(GwfDisType), pointer :: disext
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: k
    integer(I4B) :: ival
    ! -- local
! ------------------------------------------------------------------------------
    allocate (disext)
    dis => disext
    call disext%allocate_scalars(name_model)
    dis%inunit = 0
    dis%iout = iout
    !
    ! -- set dimensions
    disext%nrow = nrow
    disext%ncol = ncol
    disext%nlay = nlay
    !
    ! -- calculate nodesuser
    disext%nodesuser = disext%nlay * disext%nrow * disext%ncol
    !
    ! -- Allocate delr, delc, and non-reduced vectors for dis
    call mem_allocate(disext%delr, disext%ncol, 'DELR', disext%memoryPath)
    call mem_allocate(disext%delc, disext%nrow, 'DELC', disext%memoryPath)
    call mem_allocate(disext%idomain, disext%ncol, disext%nrow, disext%nlay, &
                      'IDOMAIN', disext%memoryPath)
    call mem_allocate(disext%top2d, disext%ncol, disext%nrow, 'TOP2D', &
                      disext%memoryPath)
    call mem_allocate(disext%bot3d, disext%ncol, disext%nrow, disext%nlay, &
                      'BOT3D', disext%memoryPath)
    ! -- fill data
    do i = 1, disext%nrow
      disext%delc(i) = delc(i)
    end do
    do j = 1, disext%ncol
      disext%delr(j) = delr(j)
    end do
    do k = 1, disext%nlay
      do i = 1, disext%nrow
        do j = 1, disext%ncol
          if (k == 1) then
            disext%top2d(j, i) = top2d(j, i)
          end if
          disext%bot3d(j, i, k) = bot3d(j, i, k)
          if (present(idomain)) then
            ival = idomain(j, i, k)
          else
            ival = 1
          end if
          disext%idomain(j, i, k) = ival
        end do
      end do
    end do
    !
    ! -- Return
    return
  end subroutine dis_init_mem

  subroutine dis3d_df(this)
! ******************************************************************************
! read_from_file -- Allocate and read discretization information
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DNODATA
    ! -- dummy
    class(GwfDisType) :: this
    ! -- locals
! ------------------------------------------------------------------------------
    !
    ! -- read data from file
    if (this%inunit /= 0) then
      !
      ! -- Identify package
      write (this%iout, 1) this%inunit
1     format(1X, /1X, 'DIS -- STRUCTURED GRID DISCRETIZATION PACKAGE,', &
             ' VERSION 2 : 3/27/2014 - INPUT READ FROM UNIT ', I0, //)
      !
      ! -- Read options
      call this%read_options()
      !
      ! -- Read dimensions block
      call this%read_dimensions()
      !
      ! -- Read GRIDDATA block
      call this%read_mf6_griddata()
    end if
    !
    ! -- Final grid initialization
    call this%grid_finalize()
    !
    ! -- Return
    return
  end subroutine dis3d_df

  subroutine dis3d_da(this)
! ******************************************************************************
! dis3d_da -- Deallocate discretization data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwfDisType) :: this
    ! -- locals
! ------------------------------------------------------------------------------
    !
    ! -- DisBaseType deallocate
    call this%DisBaseType%dis_da()
    !
    ! -- Deallocate scalars
    call mem_deallocate(this%nlay)
    call mem_deallocate(this%nrow)
    call mem_deallocate(this%ncol)
    call mem_deallocate(this%delr)
    call mem_deallocate(this%delc)
    call mem_deallocate(this%cellx)
    call mem_deallocate(this%celly)
    !
    ! -- Deallocate Arrays
    call mem_deallocate(this%nodereduced)
    call mem_deallocate(this%nodeuser)
    call mem_deallocate(this%top2d)
    call mem_deallocate(this%bot3d)
    call mem_deallocate(this%idomain)
    !
    ! -- Return
    return
  end subroutine dis3d_da

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfDisType) :: this
    ! -- locals
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- set default options
    this%lenuni = 0
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING DISCRETIZATION OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('LENGTH_UNITS')
          call this%parser%GetStringCaps(keyword)
          if (keyword == 'FEET') then
            this%lenuni = 1
            write (this%iout, '(4x,a)') 'MODEL LENGTH UNIT IS FEET'
          elseif (keyword == 'METERS') then
            this%lenuni = 2
            write (this%iout, '(4x,a)') 'MODEL LENGTH UNIT IS METERS'
          elseif (keyword == 'CENTIMETERS') then
            this%lenuni = 3
            write (this%iout, '(4x,a)') 'MODEL LENGTH UNIT IS CENTIMETERS'
          else
            write (this%iout, '(4x,a)') 'UNKNOWN UNIT: ', trim(keyword)
            write (this%iout, '(4x,a)') 'SETTING TO: ', 'UNDEFINED'
          end if
        case ('NOGRB')
          write (this%iout, '(4x,a)') 'BINARY GRB FILE WILL NOT BE WRITTEN'
          this%writegrb = .false.
        case ('XORIGIN')
          this%xorigin = this%parser%GetDouble()
          write (this%iout, '(4x,a,1pg24.15)') 'XORIGIN SPECIFIED AS ', &
            this%xorigin
        case ('YORIGIN')
          this%yorigin = this%parser%GetDouble()
          write (this%iout, '(4x,a,1pg24.15)') 'YORIGIN SPECIFIED AS ', &
            this%yorigin
        case ('ANGROT')
          this%angrot = this%parser%GetDouble()
          write (this%iout, '(4x,a,1pg24.15)') 'ANGROT SPECIFIED AS ', &
            this%angrot
        case default
          write (errmsg, '(4x,a,a)') '****ERROR. UNKNOWN DIS OPTION: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF DISCRETIZATION OPTIONS'
    else
      write (this%iout, '(1x,a)') 'NO OPTION BLOCK DETECTED.'
    end if
    if (this%lenuni == 0) then
      write (this%iout, '(1x,a)') 'MODEL LENGTH UNIT IS UNDEFINED'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_dimensions(this)
! ******************************************************************************
! read_dimensions -- Read dimensions
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfDisType) :: this
    ! -- locals
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    integer(I4B) :: i, j, k
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get dimensions block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse dimensions block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING DISCRETIZATION DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('NLAY')
          this%nlay = this%parser%GetInteger()
          write (this%iout, '(4x,a,i7)') 'NLAY = ', this%nlay
        case ('NROW')
          this%nrow = this%parser%GetInteger()
          write (this%iout, '(4x,a,i7)') 'NROW = ', this%nrow
        case ('NCOL')
          this%ncol = this%parser%GetInteger()
          write (this%iout, '(4x,a,i7)') 'NCOL = ', this%ncol
        case default
          write (errmsg, '(4x,a,a)') '****ERROR. UNKNOWN DIS DIMENSION: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF DISCRETIZATION DIMENSIONS'
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- verify dimensions were set
    if (this%nlay < 1) then
      call store_error( &
        'ERROR.  NLAY WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.')
      call this%parser%StoreErrorUnit()
    end if
    if (this%nrow < 1) then
      call store_error( &
        'ERROR.  NROW WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.')
      call this%parser%StoreErrorUnit()
    end if
    if (this%ncol < 1) then
      call store_error( &
        'ERROR.  NCOL WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.')
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- calculate nodesuser
    this%nodesuser = this%nlay * this%nrow * this%ncol
    !
    ! -- Allocate delr, delc, and non-reduced vectors for dis
    call mem_allocate(this%delr, this%ncol, 'DELR', this%memoryPath)
    call mem_allocate(this%delc, this%nrow, 'DELC', this%memoryPath)
    call mem_allocate(this%idomain, this%ncol, this%nrow, this%nlay, 'IDOMAIN', &
                      this%memoryPath)
    call mem_allocate(this%top2d, this%ncol, this%nrow, 'TOP2D', this%memoryPath)
    call mem_allocate(this%bot3d, this%ncol, this%nrow, this%nlay, 'BOT3D', &
                      this%memoryPath)
    call mem_allocate(this%cellx, this%ncol, 'CELLX', this%memoryPath)
    call mem_allocate(this%celly, this%nrow, 'CELLY', this%memoryPath)
    !
    ! -- initialize all cells to be active (idomain = 1)
    do k = 1, this%nlay
      do i = 1, this%nrow
        do j = 1, this%ncol
          this%idomain(j, i, k) = 1
        end do
      end do
    end do
    !
    ! -- Return
    return
  end subroutine read_dimensions

  subroutine read_mf6_griddata(this)
! ******************************************************************************
! read_mf6_griddata -- Read griddata from a MODFLOW 6 ascii file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: count_errors, store_error
    use ConstantsModule, only: LINELENGTH, DZERO
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfDisType) :: this
    ! -- locals
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: n
    integer(I4B) :: nvals
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    integer(I4B), parameter :: nname = 5
    logical, dimension(nname) :: lname
    character(len=24), dimension(nname) :: aname
    character(len=300) :: ermsg
    ! -- formats
    ! -- data
    data aname(1)/'                    DELR'/
    data aname(2)/'                    DELC'/
    data aname(3)/'TOP ELEVATION OF LAYER 1'/
    data aname(4)/'  MODEL LAYER BOTTOM EL.'/
    data aname(5)/'                 IDOMAIN'/
! ------------------------------------------------------------------------------
    do n = 1, size(aname)
      lname(n) = .false.
    end do
    !
    ! -- Read GRIDDATA block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    lname(:) = .false.
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING GRIDDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('DELR')
          call ReadArray(this%parser%iuactive, this%delr, aname(1), &
                         this%ndim, this%ncol, this%iout, 0)
          lname(1) = .true.
        case ('DELC')
          call ReadArray(this%parser%iuactive, this%delc, aname(2), &
                         this%ndim, this%nrow, this%iout, 0)
          lname(2) = .true.
        case ('TOP')
          call ReadArray(this%parser%iuactive, this%top2d(:, :), aname(3), &
                         this%ndim, this%ncol, this%nrow, this%iout, 0)
          lname(3) = .true.
        case ('BOTM')
          call this%parser%GetStringCaps(keyword)
          if (keyword .EQ. 'LAYERED') then
            call ReadArray(this%parser%iuactive, this%bot3d(:, :, :), &
                           aname(4), this%ndim, this%ncol, this%nrow, &
                           this%nlay, this%iout, 1, this%nlay)
          else
            nvals = this%ncol * this%nrow * this%nlay
            call ReadArray(this%parser%iuactive, this%bot3d(:, :, :), &
                           aname(4), this%ndim, nvals, this%iout)
          end if
          lname(4) = .true.
        case ('IDOMAIN')
          call this%parser%GetStringCaps(keyword)
          if (keyword .EQ. 'LAYERED') then
            call ReadArray(this%parser%iuactive, this%idomain, aname(5), &
                           this%ndim, this%ncol, this%nrow, this%nlay, &
                           this%iout, 1, this%nlay)
          else
            call ReadArray(this%parser%iuactive, this%idomain, aname(5), &
                           this%ndim, this%nodesuser, 1, 1, this%iout, 0, 0)
          end if
          lname(5) = .true.
        case default
          write (ermsg, '(4x,a,a)') 'ERROR. UNKNOWN GRIDDATA TAG: ', &
            trim(keyword)
          call store_error(ermsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END PROCESSING GRIDDATA'
    else
      call store_error('ERROR.  REQUIRED GRIDDATA BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Verify all required items were read (IDOMAIN not required)
    do n = 1, nname - 1
      if (.not. lname(n)) then
        write (ermsg, '(1x,a,a)') &
          'ERROR.  REQUIRED INPUT WAS NOT SPECIFIED: ', aname(n)
        call store_error(ermsg)
      end if
    end do
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Return
    return
  end subroutine read_mf6_griddata

  subroutine grid_finalize(this)
! ******************************************************************************
! grid_finalize -- Finalize grid
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: count_errors, store_error
    use ConstantsModule, only: LINELENGTH, DZERO
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfDisType) :: this
    ! -- locals
    character(len=300) :: ermsg
    integer(I4B) :: n, i, j, k
    integer(I4B) :: node
    integer(I4B) :: noder
    integer(I4B) :: nrsize
    real(DP) :: top
    real(DP) :: dz
    ! -- formats
    character(len=*), parameter :: fmtdz = &
      "('ERROR. CELL (',i0,',',i0,',',i0,') THICKNESS <= 0. ', &
      &'TOP, BOT: ',2(1pg24.15))"
    character(len=*), parameter :: fmtnr = &
      "(/1x, 'THE SPECIFIED IDOMAIN RESULTS IN A REDUCED NUMBER OF CELLS.',&
      &/1x, 'NUMBER OF USER NODES: ',I0,&
      &/1X, 'NUMBER OF NODES IN SOLUTION: ', I0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- count active cells
    this%nodes = 0
    do k = 1, this%nlay
      do i = 1, this%nrow
        do j = 1, this%ncol
          if (this%idomain(j, i, k) > 0) this%nodes = this%nodes + 1
        end do
      end do
    end do
    !
    ! -- Check to make sure nodes is a valid number
    if (this%nodes == 0) then
      call store_error('ERROR.  MODEL DOES NOT HAVE ANY ACTIVE NODES.')
      call store_error('MAKE SURE IDOMAIN ARRAY HAS SOME VALUES GREATER &
        &THAN ZERO.')
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Check cell thicknesses
    n = 0
    do k = 1, this%nlay
      do i = 1, this%nrow
        do j = 1, this%ncol
          if (this%idomain(j, i, k) < 1) cycle
          if (k > 1) then
            top = this%bot3d(j, i, k - 1)
          else
            top = this%top2d(j, i)
          end if
          dz = top - this%bot3d(j, i, k)
          if (dz <= DZERO) then
            n = n + 1
            write (ermsg, fmt=fmtdz) k, i, j, top, this%bot3d(j, i, k)
            call store_error(ermsg)
          end if
        end do
      end do
    end do
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Write message if reduced grid
    if (this%nodes < this%nodesuser) then
      write (this%iout, fmtnr) this%nodesuser, this%nodes
    end if
    !
    ! -- Array size is now known, so allocate
    call this%allocate_arrays()
    !
    ! -- Fill the nodereduced array with the reduced nodenumber, or
    !    a negative number to indicate it is a pass-through cell, or
    !    a zero to indicate that the cell is excluded from the
    !    solution.
    if (this%nodes < this%nodesuser) then
      node = 1
      noder = 1
      do k = 1, this%nlay
        do i = 1, this%nrow
          do j = 1, this%ncol
            if (this%idomain(j, i, k) > 0) then
              this%nodereduced(node) = noder
              noder = noder + 1
            elseif (this%idomain(j, i, k) < 0) then
              this%nodereduced(node) = -1
            else
              this%nodereduced(node) = 0
            end if
            node = node + 1
          end do
        end do
      end do
    end if
    !
    ! -- allocate and fill nodeuser if a reduced grid
    if (this%nodes < this%nodesuser) then
      node = 1
      noder = 1
      do k = 1, this%nlay
        do i = 1, this%nrow
          do j = 1, this%ncol
            if (this%idomain(j, i, k) > 0) then
              this%nodeuser(noder) = node
              noder = noder + 1
            end if
            node = node + 1
          end do
        end do
      end do
    end if
    !
    ! -- Move top2d and botm3d into top and bot, and calculate area
    node = 0
    do k = 1, this%nlay
      do i = 1, this%nrow
        do j = 1, this%ncol
          node = node + 1
          noder = node
          if (this%nodes < this%nodesuser) noder = this%nodereduced(node)
          if (noder <= 0) cycle
          if (k > 1) then
            top = this%bot3d(j, i, k - 1)
          else
            top = this%top2d(j, i)
          end if
          this%top(noder) = top
          this%bot(noder) = this%bot3d(j, i, k)
          this%area(noder) = this%delr(j) * this%delc(i)
        end do
      end do
    end do
    !
    ! -- fill x,y coordinate arrays
    this%cellx(1) = DHALF * this%delr(1)
    this%celly(this%nrow) = DHALF * this%delc(this%nrow)
    do j = 2, this%ncol
      this%cellx(j) = this%cellx(j - 1) + DHALF * this%delr(j - 1) + &
                      DHALF * this%delr(j)
    end do
    ! -- row number increases in negative y direction:
    do i = this%nrow - 1, 1, -1
      this%celly(i) = this%celly(i + 1) + DHALF * this%delc(i + 1) + &
                      DHALF * this%delc(i)
    end do
    !
    ! -- create and fill the connections object
    nrsize = 0
    if (this%nodes < this%nodesuser) nrsize = this%nodes
    allocate (this%con)
    call this%con%disconnections(this%name_model, this%nodes, &
                                 this%ncol, this%nrow, this%nlay, &
                                 nrsize, this%delr, this%delc, &
                                 this%top, this%bot, this%nodereduced, &
                                 this%nodeuser)
    this%nja = this%con%nja
    this%njas = this%con%njas
    !
    ! -- Return
    return
  end subroutine grid_finalize

  subroutine write_grb(this, icelltype)
! ******************************************************************************
! write_grb -- Write the binary grid file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: getunit, openfile
    use OpenSpecModule, only: access, form
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwfDisType) :: this
    integer(I4B), dimension(:), intent(in) :: icelltype
    ! -- local
    integer(I4B) :: iunit, ntxt, ncpl
    integer(I4B), parameter :: lentxt = 100
    character(len=50) :: txthdr
    character(len=lentxt) :: txt
    character(len=LINELENGTH) :: fname
    character(len=*), parameter :: fmtgrdsave = &
      "(4X,'BINARY GRID INFORMATION WILL BE WRITTEN TO:', &
       &/,6X,'UNIT NUMBER: ', I0,/,6X, 'FILE NAME: ', A)"
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    ntxt = 16
    ncpl = this%nrow * this%ncol
    !
    ! -- Open the file
    inquire (unit=this%inunit, name=fname)
    fname = trim(fname)//'.grb'
    iunit = getunit()
    write (this%iout, fmtgrdsave) iunit, trim(adjustl(fname))
    call openfile(iunit, this%iout, trim(adjustl(fname)), 'DATA(BINARY)', &
                  form, access, 'REPLACE')
    !
    ! -- write header information
    write (txthdr, '(a)') 'GRID DIS'
    txthdr(50:50) = new_line('a')
    write (iunit) txthdr
    write (txthdr, '(a)') 'VERSION 1'
    txthdr(50:50) = new_line('a')
    write (iunit) txthdr
    write (txthdr, '(a, i0)') 'NTXT ', ntxt
    txthdr(50:50) = new_line('a')
    write (iunit) txthdr
    write (txthdr, '(a, i0)') 'LENTXT ', lentxt
    txthdr(50:50) = new_line('a')
    write (iunit) txthdr
    !
    ! -- write variable definitions
    write (txt, '(3a, i0)') 'NCELLS ', 'INTEGER ', 'NDIM 0 # ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'NLAY ', 'INTEGER ', 'NDIM 0 # ', this%nlay
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'NROW ', 'INTEGER ', 'NDIM 0 # ', this%nrow
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'NCOL ', 'INTEGER ', 'NDIM 0 # ', this%ncol
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'NJA ', 'INTEGER ', 'NDIM 0 # ', this%nja
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, 1pg24.15)') 'XORIGIN ', 'DOUBLE ', 'NDIM 0 # ', this%xorigin
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, 1pg24.15)') 'YORIGIN ', 'DOUBLE ', 'NDIM 0 # ', this%yorigin
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, 1pg24.15)') 'ANGROT ', 'DOUBLE ', 'NDIM 0 # ', this%angrot
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'DELR ', 'DOUBLE ', 'NDIM 1 ', this%ncol
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'DELC ', 'DOUBLE ', 'NDIM 1 ', this%nrow
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'TOP ', 'DOUBLE ', 'NDIM 1 ', ncpl
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'BOTM ', 'DOUBLE ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'IA ', 'INTEGER ', 'NDIM 1 ', this%nodesuser + 1
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'JA ', 'INTEGER ', 'NDIM 1 ', size(this%con%jausr)
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'IDOMAIN ', 'INTEGER ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'ICELLTYPE ', 'INTEGER ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    !
    ! -- write data
    write (iunit) this%nodesuser ! ncells
    write (iunit) this%nlay ! nlay
    write (iunit) this%nrow ! nrow
    write (iunit) this%ncol ! ncol
    write (iunit) this%nja ! nja
    write (iunit) this%xorigin ! xorigin
    write (iunit) this%yorigin ! yorigin
    write (iunit) this%angrot ! angrot
    write (iunit) this%delr ! delr
    write (iunit) this%delc ! delc
    write (iunit) this%top2d ! top2d
    write (iunit) this%bot3d ! bot3d
    write (iunit) this%con%iausr ! iausr
    write (iunit) this%con%jausr ! jausr
    write (iunit) this%idomain ! idomain
    write (iunit) icelltype ! icelltype
    !
    ! -- Close the file
    close (iunit)
    !
    ! -- return
    return
  end subroutine write_grb

  subroutine nodeu_to_string(this, nodeu, str)
! ******************************************************************************
! nodeu_to_string -- Convert user node number to a string in the form of
! (nodenumber) or (k,i,j)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use InputOutputModule, only: get_ijk
    implicit none
    class(GwfDisType) :: this
    integer(I4B), intent(in) :: nodeu
    character(len=*), intent(inout) :: str
    ! -- local
    integer(I4B) :: i, j, k
    character(len=10) :: kstr, istr, jstr
! ------------------------------------------------------------------------------
    !
    call get_ijk(nodeu, this%nrow, this%ncol, this%nlay, i, j, k)
    write (kstr, '(i10)') k
    write (istr, '(i10)') i
    write (jstr, '(i10)') j
    str = '('//trim(adjustl(kstr))//','// &
          trim(adjustl(istr))//','// &
          trim(adjustl(jstr))//')'
    !
    ! -- return
    return
  end subroutine nodeu_to_string

  subroutine nodeu_to_array(this, nodeu, arr)
! ******************************************************************************
! nodeu_to_array -- Convert user node number to cellid and fill array with
!                   (nodenumber) or (k,j) or (k,i,j)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use InputOutputModule, only: get_ijk
    implicit none
    class(GwfDisType) :: this
    integer(I4B), intent(in) :: nodeu
    integer(I4B), dimension(:), intent(inout) :: arr
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: isize
    integer(I4B) :: i, j, k
! ------------------------------------------------------------------------------
    !
    ! -- check the size of arr
    isize = size(arr)
    if (isize /= this%ndim) then
      write (errmsg, '(a,i0,a,i0,a)') &
        'Program error: nodeu_to_array size of array (', isize, &
        ') is not equal to the discretization dimension (', this%ndim, ')'
      call store_error(errmsg, terminate=.TRUE.)
    end if
    !
    ! -- get k, i, j
    call get_ijk(nodeu, this%nrow, this%ncol, this%nlay, i, j, k)
    !
    ! -- fill array
    arr(1) = k
    arr(2) = i
    arr(3) = j
    !
    ! -- return
    return
  end subroutine nodeu_to_array

  function get_nodenumber_idx1(this, nodeu, icheck) result(nodenumber)
! ******************************************************************************
! get_nodenumber -- Return a nodenumber from the user specified node number
!                   with an option to perform a check.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    ! -- return
    integer(I4B) :: nodenumber
    ! -- dummy
    class(GwfDisType), intent(in) :: this
    integer(I4B), intent(in) :: nodeu
    integer(I4B), intent(in) :: icheck
    ! -- local
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- check the node number if requested
    if (icheck /= 0) then
      !
      ! -- If within valid range, convert to reduced nodenumber
      if (nodeu < 1 .or. nodeu > this%nodesuser) then
        write (errmsg, '(a,i10)') &
          'Nodenumber less than 1 or greater than nodes:', nodeu
        call store_error(errmsg)
        nodenumber = 0
      else
        nodenumber = nodeu
        if (this%nodes < this%nodesuser) nodenumber = this%nodereduced(nodeu)
      end if
    else
      nodenumber = nodeu
      if (this%nodes < this%nodesuser) nodenumber = this%nodereduced(nodeu)
    end if
    !
    ! -- return
    return
  end function get_nodenumber_idx1

  function get_nodenumber_idx3(this, k, i, j, icheck) &
    result(nodenumber)
! ******************************************************************************
! get_nodenumber_idx3 -- Return a nodenumber from the user specified layer, row,
!                        and column with an option to perform a check.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use InputOutputModule, only: get_node
    implicit none
    ! -- return
    integer(I4B) :: nodenumber
    ! -- dummy
    class(GwfDisType), intent(in) :: this
    integer(I4B), intent(in) :: k, i, j
    integer(I4B), intent(in) :: icheck
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: nodeu
    ! formats
    character(len=*), parameter :: fmterr = &
      "('Error in structured-grid cell indices: layer = ',i0,', &
      &row = ',i0,', column = ',i0)"
! ------------------------------------------------------------------------------
    !
    nodeu = get_node(k, i, j, this%nlay, this%nrow, this%ncol)
    if (nodeu < 1) then
      write (errmsg, fmterr) k, i, j
      call store_error(errmsg, terminate=.TRUE.)
    end if
    nodenumber = nodeu
    if (this%nodes < this%nodesuser) nodenumber = this%nodereduced(nodeu)
    !
    ! -- check the node number if requested
    if (icheck /= 0) then
      !
      if (k < 1 .or. k > this%nlay) &
        call store_error('Layer less than one or greater than nlay')
      if (i < 1 .or. i > this%nrow) &
        call store_error('Row less than one or greater than nrow')
      if (j < 1 .or. j > this%ncol) &
        call store_error('Column less than one or greater than ncol')
      !
      ! -- Error if outside of range
      if (nodeu < 1 .or. nodeu > this%nodesuser) then
        write (errmsg, '(a,i10)') &
          'Nodenumber less than 1 or greater than nodes:', nodeu
        call store_error(errmsg)
      end if
    end if
    !
    ! -- return
    return
  end function get_nodenumber_idx3

  subroutine allocate_scalars(this, name_model)
! ******************************************************************************
! allocate_scalars -- Allocate and initialize scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfDisType) :: this
    character(len=*), intent(in) :: name_model
! ------------------------------------------------------------------------------
    !
    ! -- Allocate parent scalars
    call this%DisBaseType%allocate_scalars(name_model)
    !
    ! -- Allocate
    call mem_allocate(this%nlay, 'NLAY', this%memoryPath)
    call mem_allocate(this%nrow, 'NROW', this%memoryPath)
    call mem_allocate(this%ncol, 'NCOL', this%memoryPath)
    !
    ! -- Initialize
    this%nlay = 0
    this%nrow = 0
    this%ncol = 0
    this%ndim = 3
    !
    ! -- Return
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
    class(GwfDisType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Allocate arrays in DisBaseType (mshape, top, bot, area)
    call this%DisBaseType%allocate_arrays()
    !
    ! -- Allocate arrays for GwfDisType
    if (this%nodes < this%nodesuser) then
      call mem_allocate(this%nodeuser, this%nodes, 'NODEUSER', this%memoryPath)
      call mem_allocate(this%nodereduced, this%nodesuser, 'NODEREDUCED', &
                        this%memoryPath)
    else
      call mem_allocate(this%nodeuser, 1, 'NODEUSER', this%memoryPath)
      call mem_allocate(this%nodereduced, 1, 'NODEREDUCED', this%memoryPath)
    end if
    !
    ! -- Initialize
    this%mshape(1) = this%nlay
    this%mshape(2) = this%nrow
    this%mshape(3) = this%ncol
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  function nodeu_from_string(this, lloc, istart, istop, in, iout, line, &
                             flag_string, allow_zero) result(nodeu)
! ******************************************************************************
! nodeu_from_string -- Receive a string and convert the string to a user
!   nodenumber.  The model discretization is DIS; read layer, row, and column.
!   If flag_string argument is present and true, the first token in string
!   is allowed to be a string (e.g. boundary name). In this case, if a string
!   is encountered, return value as -2.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfDisType) :: this
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    integer(I4B), intent(in) :: in
    integer(I4B), intent(in) :: iout
    character(len=*), intent(inout) :: line
    logical, optional, intent(in) :: flag_string
    logical, optional, intent(in) :: allow_zero
    integer(I4B) :: nodeu
    ! -- local
    integer(I4B) :: k, i, j, nlay, nrow, ncol
    integer(I4B) :: lloclocal, ndum, istat, n
    real(DP) :: r
    character(len=LINELENGTH) :: ermsg, fname
! ------------------------------------------------------------------------------
    !
    if (present(flag_string)) then
      if (flag_string) then
        ! Check to see if first token in line can be read as an integer.
        lloclocal = lloc
        call urword(line, lloclocal, istart, istop, 1, ndum, r, iout, in)
        read (line(istart:istop), *, iostat=istat) n
        if (istat /= 0) then
          ! First token in line is not an integer; return flag to this effect.
          nodeu = -2
          return
        end if
      end if
    end if
    !
    nlay = this%mshape(1)
    nrow = this%mshape(2)
    ncol = this%mshape(3)
    !
    call urword(line, lloc, istart, istop, 2, k, r, iout, in)
    call urword(line, lloc, istart, istop, 2, i, r, iout, in)
    call urword(line, lloc, istart, istop, 2, j, r, iout, in)
    !
    if (k == 0 .and. i == 0 .and. j == 0) then
      if (present(allow_zero)) then
        if (allow_zero) then
          nodeu = 0
          return
        end if
      end if
    end if
    !
    if (k < 1 .or. k > nlay) then
      write (ermsg, *) ' Layer number in list is outside of the grid', k
      call store_error(ermsg)
    end if
    if (i < 1 .or. i > nrow) then
      write (ermsg, *) ' Row number in list is outside of the grid', i
      call store_error(ermsg)
    end if
    if (j < 1 .or. j > ncol) then
      write (ermsg, *) ' Column number in list is outside of the grid', j
      call store_error(ermsg)
    end if
    nodeu = get_node(k, i, j, nlay, nrow, ncol)
    !
    if (nodeu < 1 .or. nodeu > this%nodesuser) then
      write (ermsg, *) ' Node number in list is outside of the grid', nodeu
      call store_error(ermsg)
      inquire (unit=in, name=fname)
      call store_error('Error converting in file: ')
      call store_error(trim(adjustl(fname)))
      call store_error('Cell number cannot be determined in line: ')
      call store_error(trim(adjustl(line)))
      call store_error_unit(in)
    end if
    !
    ! -- return
    return

  end function nodeu_from_string

  function nodeu_from_cellid(this, cellid, inunit, iout, flag_string, &
                             allow_zero) result(nodeu)
! ******************************************************************************
! nodeu_from_cellid -- Receive cellid as a string and convert the string to a
!   user nodenumber.
!   If flag_string argument is present and true, the first token in string
!   is allowed to be a string (e.g. boundary name). In this case, if a string
!   is encountered, return value as -2.
!   If allow_zero argument is present and true, if all indices equal zero, the
!   result can be zero. If allow_zero is false, a zero in any index causes an
!   error.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    integer(I4B) :: nodeu
    ! -- dummy
    class(GwfDisType) :: this
    character(len=*), intent(inout) :: cellid
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    logical, optional, intent(in) :: flag_string
    logical, optional, intent(in) :: allow_zero
    ! -- local
    integer(I4B) :: lloclocal, istart, istop, ndum, n
    integer(I4B) :: k, i, j, nlay, nrow, ncol
    integer(I4B) :: istat
    real(DP) :: r
    character(len=LINELENGTH) :: ermsg, fname
! ------------------------------------------------------------------------------
    !
    if (present(flag_string)) then
      if (flag_string) then
        ! Check to see if first token in cellid can be read as an integer.
        lloclocal = 1
        call urword(cellid, lloclocal, istart, istop, 1, ndum, r, iout, inunit)
        read (cellid(istart:istop), *, iostat=istat) n
        if (istat /= 0) then
          ! First token in cellid is not an integer; return flag to this effect.
          nodeu = -2
          return
        end if
      end if
    end if
    !
    nlay = this%mshape(1)
    nrow = this%mshape(2)
    ncol = this%mshape(3)
    !
    lloclocal = 1
    call urword(cellid, lloclocal, istart, istop, 2, k, r, iout, inunit)
    call urword(cellid, lloclocal, istart, istop, 2, i, r, iout, inunit)
    call urword(cellid, lloclocal, istart, istop, 2, j, r, iout, inunit)
    !
    if (k == 0 .and. i == 0 .and. j == 0) then
      if (present(allow_zero)) then
        if (allow_zero) then
          nodeu = 0
          return
        end if
      end if
    end if
    !
    if (k < 1 .or. k > nlay) then
      write (ermsg, *) ' Layer number in list is outside of the grid', k
      call store_error(ermsg)
    end if
    if (i < 1 .or. i > nrow) then
      write (ermsg, *) ' Row number in list is outside of the grid', i
      call store_error(ermsg)
    end if
    if (j < 1 .or. j > ncol) then
      write (ermsg, *) ' Column number in list is outside of the grid', j
      call store_error(ermsg)
    end if
    nodeu = get_node(k, i, j, nlay, nrow, ncol)
    !
    if (nodeu < 1 .or. nodeu > this%nodesuser) then
      write (ermsg, *) ' Node number in list is outside of the grid', nodeu
      call store_error(ermsg)
      inquire (unit=inunit, name=fname)
      call store_error('Error converting in file: ')
      call store_error(trim(adjustl(fname)))
      call store_error('Cell number cannot be determined in cellid: ')
      call store_error(trim(adjustl(cellid)))
      call store_error_unit(inunit)
    end if
    !
    ! -- return
    return
  end function nodeu_from_cellid

  logical function supports_layers(this)
    implicit none
    ! -- dummy
    class(GwfDisType) :: this
    !
    supports_layers = .true.
    return
  end function supports_layers

  function get_ncpl(this)
! ******************************************************************************
! get_ncpl -- Return number of cells per layer.  This is nrow * ncol
!   for a DIS3D grid.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    integer(I4B) :: get_ncpl
    ! -- dummy
    class(GwfDisType) :: this
! ------------------------------------------------------------------------------
    !
    get_ncpl = this%nrow * this%ncol
    !
    ! -- Return
    return
  end function get_ncpl

  subroutine connection_normal(this, noden, nodem, ihc, xcomp, ycomp, zcomp, &
                               ipos)
! ******************************************************************************
! connection_normal -- calculate the normal vector components for reduced
!   nodenumber cell (noden) and its shared face with cell nodem.  ihc is the
!   horizontal connection flag.  Connection normal is a normal vector pointing
!   outward from the shared face between noden and nodem.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO, DONE
    use InputOutputModule, only: get_ijk
    ! -- dummy
    class(GwfDisType) :: this
    integer(I4B), intent(in) :: noden
    integer(I4B), intent(in) :: nodem
    integer(I4B), intent(in) :: ihc
    real(DP), intent(inout) :: xcomp
    real(DP), intent(inout) :: ycomp
    real(DP), intent(inout) :: zcomp
    integer(I4B), intent(in) :: ipos
    ! -- local
    integer(I4B) :: nodeu1, i1, j1, k1
    integer(I4B) :: nodeu2, i2, j2, k2
! ------------------------------------------------------------------------------
    !
    ! -- Set vector components based on ihc
    if (ihc == 0) then
      xcomp = DZERO
      ycomp = DZERO
      if (nodem < noden) then
        !
        ! -- nodem must be above noden, so upward connection
        zcomp = DONE
      else
        !
        ! -- nodem must be below noden, so downward connection
        zcomp = -DONE
      end if
    else
      xcomp = DZERO
      ycomp = DZERO
      zcomp = DZERO
      nodeu1 = this%get_nodeuser(noden)
      nodeu2 = this%get_nodeuser(nodem)
      call get_ijk(nodeu1, this%nrow, this%ncol, this%nlay, i1, j1, k1)
      call get_ijk(nodeu2, this%nrow, this%ncol, this%nlay, i2, j2, k2)
      if (i2 < i1) then ! back
        ycomp = DONE
      elseif (j2 < j1) then ! left
        xcomp = -DONE
      elseif (j2 > j1) then ! right
        xcomp = DONE
      else ! front
        ycomp = -DONE
      end if
      !
    end if
    !
    ! -- return
    return
  end subroutine connection_normal

  subroutine connection_vector(this, noden, nodem, nozee, satn, satm, ihc, &
                               xcomp, ycomp, zcomp, conlen)
! ******************************************************************************
! connection_vector -- calculate the unit vector components from reduced
!   nodenumber cell (noden) to its neighbor cell (nodem).  The saturation for
!   for these cells are also required so that the vertical position of the cell
!   cell centers can be calculated.  ihc is the horizontal flag.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO, DONE, DHALF
    use DisvGeom, only: line_unit_vector
    use InputOutputModule, only: get_ijk
    ! -- dummy
    class(GwfDisType) :: this
    integer(I4B), intent(in) :: noden
    integer(I4B), intent(in) :: nodem
    logical, intent(in) :: nozee
    real(DP), intent(in) :: satn
    real(DP), intent(in) :: satm
    integer(I4B), intent(in) :: ihc
    real(DP), intent(inout) :: xcomp
    real(DP), intent(inout) :: ycomp
    real(DP), intent(inout) :: zcomp
    real(DP), intent(inout) :: conlen
    ! -- local
    real(DP) :: z1, z2
    real(DP) :: x1, y1, x2, y2
    real(DP) :: ds
    integer(I4B) :: i1, i2, j1, j2, k1, k2
    integer(I4B) :: nodeu1, nodeu2, ipos
! ------------------------------------------------------------------------------
    !
    ! -- Set vector components based on ihc
    if (ihc == 0) then
      !
      ! -- vertical connection; set zcomp positive upward
      xcomp = DZERO
      ycomp = DZERO
      if (nodem < noden) then
        zcomp = DONE
      else
        zcomp = -DONE
      end if
      z1 = this%bot(noden) + DHALF * (this%top(noden) - this%bot(noden))
      z2 = this%bot(nodem) + DHALF * (this%top(nodem) - this%bot(nodem))
      conlen = abs(z2 - z1)
    else
      !
      if (nozee) then
        z1 = DZERO
        z2 = DZERO
      else
        z1 = this%bot(noden) + DHALF * satn * (this%top(noden) - this%bot(noden))
        z2 = this%bot(nodem) + DHALF * satm * (this%top(nodem) - this%bot(nodem))
      end if
      ipos = this%con%getjaindex(noden, nodem)
      ds = this%con%cl1(this%con%jas(ipos)) + this%con%cl2(this%con%jas(ipos))
      nodeu1 = this%get_nodeuser(noden)
      nodeu2 = this%get_nodeuser(nodem)
      call get_ijk(nodeu1, this%nrow, this%ncol, this%nlay, i1, j1, k1)
      call get_ijk(nodeu2, this%nrow, this%ncol, this%nlay, i2, j2, k2)
      x1 = DZERO
      x2 = DZERO
      y1 = DZERO
      y2 = DZERO
      if (i2 < i1) then ! back
        y2 = ds
      elseif (j2 < j1) then ! left
        x2 = -ds
      elseif (j2 > j1) then ! right
        x2 = ds
      else ! front
        y2 = -ds
      end if
      call line_unit_vector(x1, y1, z1, x2, y2, z2, xcomp, ycomp, zcomp, conlen)
    end if
    !
    ! -- return
    return
  end subroutine

  ! return x,y coordinate for a node
  subroutine get_cellxy_dis3d(this, node, xcell, ycell)
    use InputOutputModule, only: get_ijk
    class(GwfDisType), intent(in) :: this
    integer(I4B), intent(in) :: node ! the reduced node number
    real(DP), intent(out) :: xcell, ycell ! the x,y for the cell
    ! local
    integer(I4B) :: nodeuser, i, j, k

    nodeuser = this%get_nodeuser(node)
    call get_ijk(nodeuser, this%nrow, this%ncol, this%nlay, i, j, k)

    xcell = this%cellx(j)
    ycell = this%celly(i)

  end subroutine get_cellxy_dis3d

  ! return discretization type
  subroutine get_dis_type(this, dis_type)
    class(GwfDisType), intent(in) :: this
    character(len=*), intent(out) :: dis_type

    dis_type = "DIS"

  end subroutine get_dis_type

  subroutine read_int_array(this, line, lloc, istart, istop, iout, in, &
                            iarray, aname)
! ******************************************************************************
! read_int_array -- Read a GWF integer array
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: urword
    use SimModule, only: store_error
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfDisType), intent(inout) :: this
    character(len=*), intent(inout) :: line
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    integer(I4B), intent(in) :: in
    integer(I4B), intent(in) :: iout
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: iarray
    character(len=*), intent(in) :: aname
    ! -- local
    integer(I4B) :: ival
    real(DP) :: rval
    integer(I4B) :: nlay
    integer(I4B) :: nrow
    integer(I4B) :: ncol
    integer(I4B) :: nval
    integer(I4B), dimension(:), pointer, contiguous :: itemp
! ------------------------------------------------------------------------------
    !
    ! -- Point the temporary pointer array, which is passed to the reading
    !    subroutine.  The temporary array will point to ibuff if it is a
    !    reduced structured system, or to iarray if it is an unstructured
    !    model.
    nlay = this%mshape(1)
    nrow = this%mshape(2)
    ncol = this%mshape(3)
    !
    if (this%nodes < this%nodesuser) then
      nval = this%nodesuser
      itemp => this%ibuff
    else
      nval = this%nodes
      itemp => iarray
    end if
    !
    ! -- Read the array
    call urword(line, lloc, istart, istop, 1, ival, rval, iout, in)
    if (line(istart:istop) .EQ. 'LAYERED') then
      !
      ! -- Read layered input
      call ReadArray(in, itemp, aname, this%ndim, ncol, nrow, nlay, nval, &
                     iout, 1, nlay)
    else
      !
      ! -- Read unstructured input
      call ReadArray(in, itemp, aname, this%ndim, nval, iout, 0)
    end if
    !
    ! -- If reduced model, then need to copy from itemp(=>ibuff) to iarray
    if (this%nodes < this%nodesuser) then
      call this%fill_grid_array(itemp, iarray)
    end if
    !
    ! -- return
    return
  end subroutine read_int_array

  subroutine read_dbl_array(this, line, lloc, istart, istop, iout, in, &
                            darray, aname)
! ******************************************************************************
! read_dbl_array -- Read a GWF double precision array
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: urword
    use SimModule, only: store_error
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfDisType), intent(inout) :: this
    character(len=*), intent(inout) :: line
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    integer(I4B), intent(in) :: in
    integer(I4B), intent(in) :: iout
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: darray
    character(len=*), intent(in) :: aname
    ! -- local
    integer(I4B) :: ival
    real(DP) :: rval
    integer(I4B) :: nlay
    integer(I4B) :: nrow
    integer(I4B) :: ncol
    integer(I4B) :: nval
    real(DP), dimension(:), pointer, contiguous :: dtemp
! ------------------------------------------------------------------------------
    !
    ! -- Point the temporary pointer array, which is passed to the reading
    !    subroutine.  The temporary array will point to dbuff if it is a
    !    reduced structured system, or to darray if it is an unstructured
    !    model.
    nlay = this%mshape(1)
    nrow = this%mshape(2)
    ncol = this%mshape(3)
    !
    if (this%nodes < this%nodesuser) then
      nval = this%nodesuser
      dtemp => this%dbuff
    else
      nval = this%nodes
      dtemp => darray
    end if
    !
    ! -- Read the array
    call urword(line, lloc, istart, istop, 1, ival, rval, iout, in)
    if (line(istart:istop) .EQ. 'LAYERED') then
      !
      ! -- Read structured input
      call ReadArray(in, dtemp, aname, this%ndim, ncol, nrow, nlay, nval, &
                     iout, 1, nlay)
    else
      !
      ! -- Read unstructured input
      call ReadArray(in, dtemp, aname, this%ndim, nval, iout, 0)
    end if
    !
    ! -- If reduced model, then need to copy from dtemp(=>dbuff) to darray
    if (this%nodes < this%nodesuser) then
      call this%fill_grid_array(dtemp, darray)
    end if
    !
    ! -- return
    return
  end subroutine read_dbl_array

  subroutine read_layer_array(this, nodelist, darray, ncolbnd, maxbnd, &
                              icolbnd, aname, inunit, iout)
! ******************************************************************************
! read_layer_array -- Read a 2d double array into col icolbnd of darray.
!                     For cells that are outside of the active domain,
!                     do not copy the array value into darray.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: get_node
    ! -- dummy
    class(GwfDisType) :: this
    integer(I4B), intent(in) :: maxbnd
    integer(I4B), dimension(maxbnd) :: nodelist
    integer(I4B), intent(in) :: ncolbnd
    real(DP), dimension(ncolbnd, maxbnd), intent(inout) :: darray
    integer(I4B), intent(in) :: icolbnd
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: ir, ic, ncol, nrow, nlay, nval, ipos, nodeu
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    nlay = this%mshape(1)
    nrow = this%mshape(2)
    ncol = this%mshape(3)
    !
    ! -- Read the array
    nval = ncol * nrow
    call ReadArray(inunit, this%dbuff, aname, this%ndim, ncol, nrow, nlay, &
                   nval, iout, 0, 0)
    !
    ! -- Copy array into bound.  Note that this routine was substantially
    !    changed on 9/21/2021 to support changes to READASARRAYS input
    !    for recharge and evapotranspiration.  nodelist and bound are of
    !    size nrow * ncol and correspond directly to dbuff.
    ipos = 1
    do ir = 1, nrow
      do ic = 1, ncol
        nodeu = get_node(1, ir, ic, nlay, nrow, ncol)
        darray(icolbnd, ipos) = this%dbuff(nodeu)
        ipos = ipos + 1
        !
      end do
    end do
    !
    ! -- return
  end subroutine read_layer_array

  subroutine record_array(this, darray, iout, iprint, idataun, aname, &
                          cdatafmp, nvaluesp, nwidthp, editdesc, dinact)
! ******************************************************************************
! record_array -- Record a double precision array.  The array will be
!   printed to an external file and/or written to an unformatted external file
!   depending on the argument specifications.
! ******************************************************************************
!
!    SPECIFICATIONS:
!      darray is the double precision array to record
!      iout is the unit number for ascii output
!      iprint is a flag indicating whether or not to print the array
!      idataun is the unit number to which the array will be written in binary
!        form; if negative then do not write by layers, write entire array
!      aname is the text descriptor of the array
!      cdatafmp is the fortran format for writing the array
!      nvaluesp is the number of values per line for printing
!      nwidthp is the width of the number for printing
!      editdesc is the format type (I, G, F, S, E)
!      dinact is the double precision value to use for cells that are excluded
!        from the model domain
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfDisType), intent(inout) :: this
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: darray
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(in) :: iprint
    integer(I4B), intent(in) :: idataun
    character(len=*), intent(in) :: aname
    character(len=*), intent(in) :: cdatafmp
    integer(I4B), intent(in) :: nvaluesp
    integer(I4B), intent(in) :: nwidthp
    character(len=*), intent(in) :: editdesc
    real(DP), intent(in) :: dinact
    ! -- local
    integer(I4B) :: k, ifirst
    integer(I4B) :: nlay
    integer(I4B) :: nrow
    integer(I4B) :: ncol
    integer(I4B) :: nval
    integer(I4B) :: nodeu, noder
    integer(I4B) :: istart, istop
    real(DP), dimension(:), pointer, contiguous :: dtemp
    ! -- formats
    character(len=*), parameter :: fmthsv = &
      "(1X,/1X,a,' WILL BE SAVED ON UNIT ',I4, &
       &' AT END OF TIME STEP',I5,', STRESS PERIOD ',I4)"
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    nlay = this%mshape(1)
    nrow = this%mshape(2)
    ncol = this%mshape(3)
    !
    ! -- If this is a reduced model, then copy the values from darray into
    !    dtemp.
    if (this%nodes < this%nodesuser) then
      nval = this%nodes
      dtemp => this%dbuff
      do nodeu = 1, this%nodesuser
        noder = this%get_nodenumber(nodeu, 0)
        if (noder <= 0) then
          dtemp(nodeu) = dinact
          cycle
        end if
        dtemp(nodeu) = darray(noder)
      end do
    else
      nval = this%nodes
      dtemp => darray
    end if
    !
    ! -- Print to iout if iprint /= 0
    if (iprint /= 0) then
      istart = 1
      do k = 1, nlay
        istop = istart + nrow * ncol - 1
        call ulaprufw(ncol, nrow, kstp, kper, k, iout, dtemp(istart:istop), &
                      aname, cdatafmp, nvaluesp, nwidthp, editdesc)
        istart = istop + 1
      end do
    end if
    !
    ! -- Save array to an external file.
    if (idataun > 0) then
      ! -- write to binary file by layer
      ifirst = 1
      istart = 1
      do k = 1, nlay
        istop = istart + nrow * ncol - 1
        if (ifirst == 1) write (iout, fmthsv) &
          trim(adjustl(aname)), idataun, &
          kstp, kper
        ifirst = 0
        call ulasav(dtemp(istart:istop), aname, kstp, kper, &
                    pertim, totim, ncol, nrow, k, idataun)
        istart = istop + 1
      end do
    elseif (idataun < 0) then
      !
      ! -- write entire array as one record
      call ubdsv1(kstp, kper, aname, -idataun, dtemp, ncol, nrow, nlay, &
                  iout, delt, pertim, totim)
    end if
    !
    ! -- return
    return
  end subroutine record_array

  subroutine record_srcdst_list_header(this, text, textmodel, textpackage, &
                                       dstmodel, dstpackage, naux, auxtxt, &
                                       ibdchn, nlist, iout)
! ******************************************************************************
! record_srcdst_list_header -- Record list header for imeth=6
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfDisType) :: this
    character(len=16), intent(in) :: text
    character(len=16), intent(in) :: textmodel
    character(len=16), intent(in) :: textpackage
    character(len=16), intent(in) :: dstmodel
    character(len=16), intent(in) :: dstpackage
    integer(I4B), intent(in) :: naux
    character(len=16), dimension(:), intent(in) :: auxtxt
    integer(I4B), intent(in) :: ibdchn
    integer(I4B), intent(in) :: nlist
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: nlay, nrow, ncol
! ------------------------------------------------------------------------------
    !
    nlay = this%mshape(1)
    nrow = this%mshape(2)
    ncol = this%mshape(3)
    !
    ! -- Use ubdsv06 to write list header
    call ubdsv06(kstp, kper, text, textmodel, textpackage, dstmodel, dstpackage, &
                 ibdchn, naux, auxtxt, ncol, nrow, nlay, &
                 nlist, iout, delt, pertim, totim)
    !
    ! -- return
    return
  end subroutine record_srcdst_list_header

  subroutine nlarray_to_nodelist(this, nodelist, maxbnd, nbound, aname, &
                                 inunit, iout)
! ******************************************************************************
! nlarray_to_nodelist -- Read an integer array into nodelist. For structured
!                        model, integer array is layer number; for unstructured
!                        model, integer array is node number.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: get_node
    use SimModule, only: store_error
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfDisType) :: this
    integer(I4B), intent(in) :: maxbnd
    integer(I4B), dimension(maxbnd), intent(inout) :: nodelist
    integer(I4B), intent(inout) :: nbound
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: il, ir, ic, ncol, nrow, nlay, nval, nodeu, noder, ipos, ierr
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    nlay = this%mshape(1)
    nrow = this%mshape(2)
    ncol = this%mshape(3)
    !
    if (this%ndim > 1) then
      !
      nval = ncol * nrow
      call ReadArray(inunit, this%ibuff, aname, this%ndim, ncol, nrow, nlay, &
                     nval, iout, 0, 0)
      !
      ! -- Copy array into nodelist
      ipos = 1
      ierr = 0
      do ir = 1, nrow
        do ic = 1, ncol
          nodeu = get_node(1, ir, ic, nlay, nrow, ncol)
          il = this%ibuff(nodeu)
          if (il < 1 .or. il > nlay) then
            write (errmsg, *) 'INVALID LAYER NUMBER: ', il
            call store_error(errmsg, terminate=.TRUE.)
          end if
          nodeu = get_node(il, ir, ic, nlay, nrow, ncol)
          noder = this%get_nodenumber(nodeu, 0)
          if (ipos > maxbnd) then
            ierr = ipos
          else
            nodelist(ipos) = noder
          end if
          ipos = ipos + 1
        end do
      end do
      !
      ! -- Check for errors
      nbound = ipos - 1
      if (ierr > 0) then
        write (errmsg, *) 'MAXBOUND DIMENSION IS TOO SMALL.'
        call store_error(errmsg)
        write (errmsg, *) 'INCREASE MAXBOUND TO: ', ierr
        call store_error(errmsg, terminate=.TRUE.)
      end if
      !
      ! -- If nbound < maxbnd, then initialize nodelist to zero in this range
      if (nbound < maxbnd) then
        do ipos = nbound + 1, maxbnd
          nodelist(ipos) = 0
        end do
      end if
      !
    else
      !
      ! -- For unstructured, read nodelist directly, then check node numbers
      call ReadArray(inunit, nodelist, aname, this%ndim, maxbnd, iout, 0)
      do noder = 1, maxbnd
        if (noder < 1 .or. noder > this%nodes) then
          write (errmsg, *) 'INVALID NODE NUMBER: ', noder
          call store_error(errmsg, terminate=.TRUE.)
        end if
      end do
      nbound = maxbnd
      !
    end if
    !
    ! -- return
  end subroutine nlarray_to_nodelist

end module GwfDisModule

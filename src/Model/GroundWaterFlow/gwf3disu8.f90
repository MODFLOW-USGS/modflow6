module GwfDisuModule

  use ArrayReadersModule, only: ReadArray
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMEMPATH, LENVARNAME, &
                             DZERO, DONE
  use ConnectionsModule, only: iac_to_ia
  use InputOutputModule, only: URWORD, ulasav, ulaprufw, ubdsv1, ubdsv06
  use SimModule, only: count_errors, store_error, store_error_unit
  use SimVariablesModule, only: errmsg
  use BaseDisModule, only: DisBaseType
  use BlockParserModule, only: BlockParserType
  use MemoryManagerModule, only: mem_allocate
  use TdisModule, only: kstp, kper, pertim, totim, delt

  implicit none

  private
  public :: GwfDisuType
  public :: disu_cr
  public :: CastAsDisuType

  type, extends(DisBaseType) :: GwfDisuType
    integer(I4B), pointer :: njausr => null() ! user-specified nja size
    integer(I4B), pointer :: nvert => null() ! number of x,y vertices
    real(DP), pointer :: voffsettol => null() ! vertical offset tolerance
    real(DP), dimension(:, :), pointer, contiguous :: vertices => null() ! cell vertices stored as 2d array of x and y
    real(DP), dimension(:, :), pointer, contiguous :: cellxy => null() ! cell center stored as 2d array of x and y
    real(DP), dimension(:), pointer, contiguous :: top1d => null() ! (size:nodesuser) cell top elevation
    real(DP), dimension(:), pointer, contiguous :: bot1d => null() ! (size:nodesuser) cell bottom elevation
    real(DP), dimension(:), pointer, contiguous :: area1d => null() ! (size:nodesuser) cell area, in plan view
    integer(I4B), dimension(:), pointer, contiguous :: iainp => null() ! (size:nodesuser+1) user iac converted ia
    integer(I4B), dimension(:), pointer, contiguous :: jainp => null() ! (size:njausr) user-input ja array
    integer(I4B), dimension(:), pointer, contiguous :: ihcinp => null() ! (size:njausr) user-input ihc array
    real(DP), dimension(:), pointer, contiguous :: cl12inp => null() ! (size:njausr) user-input cl12 array
    real(DP), dimension(:), pointer, contiguous :: hwvainp => null() ! (size:njausr) user-input hwva array
    real(DP), dimension(:), pointer, contiguous :: angldegxinp => null() ! (size:njausr) user-input angldegx array
    integer(I4B), pointer :: iangledegx => null() ! =1 when angle information was present in input, 0 otherwise
    integer(I4B), dimension(:), pointer, contiguous :: iavert => null() ! cell vertex pointer ia array
    integer(I4B), dimension(:), pointer, contiguous :: javert => null() ! cell vertex pointer ja array
    integer(I4B), dimension(:), pointer, contiguous :: idomain => null() ! idomain (nodes)
    logical(LGP) :: readFromFile ! True, when DIS is read from file (almost always)
  contains
    procedure :: dis_df => disu_df
    procedure :: disu_load
    procedure :: dis_da => disu_da
    procedure :: get_dis_type => get_dis_type
    procedure :: disu_ck
    procedure :: grid_finalize
    procedure :: get_nodenumber_idx1
    procedure :: nodeu_to_string
    procedure :: nodeu_to_array
    procedure :: nodeu_from_string
    procedure :: nodeu_from_cellid
    procedure :: connection_normal
    procedure :: connection_vector
    procedure :: supports_layers
    procedure :: get_ncpl
    procedure, public :: record_array
    procedure, public :: record_srcdst_list_header
    ! -- private
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: allocate_arrays_mem
    procedure :: source_options
    procedure :: source_dimensions
    procedure :: source_griddata
    procedure :: source_connectivity
    procedure :: source_vertices
    procedure :: source_cell2d
    procedure :: log_options
    procedure :: log_dimensions
    procedure :: log_griddata
    procedure :: log_connectivity
    procedure :: define_cellverts
    procedure :: write_grb
    !
    ! -- Read a node-sized model array (reduced or not)
    procedure :: read_int_array
    procedure :: read_dbl_array
  end type GwfDisuType

contains

  subroutine disu_cr(dis, name_model, inunit, iout)
! ******************************************************************************
! disu_cr -- Create discretization object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use IdmMf6FileLoaderModule, only: input_load
    use ConstantsModule, only: LENPACKAGETYPE
    ! -- dummy
    class(DisBaseType), pointer :: dis
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    ! -- local
    type(GwfDisuType), pointer :: disnew
    character(len=*), parameter :: fmtheader = &
      "(1X, /1X, 'DISU -- UNSTRUCTURED GRID DISCRETIZATION PACKAGE,', &
      &' VERSION 2 : 3/27/2014 - INPUT READ FROM UNIT ', I0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- Create a new discretization object
    allocate (disnew)
    dis => disnew
    !
    ! -- Allocate scalars and assign data
    call dis%allocate_scalars(name_model)
    dis%inunit = inunit
    dis%iout = iout
    !
    ! -- if reading from file
    if (inunit > 0) then
      !
      ! -- Identify package
      if (iout > 0) then
        write (iout, fmtheader) inunit
      end if
      !
      ! -- initialize parser and load the disu input file
      call dis%parser%Initialize(inunit, iout)
      !
      ! -- Use the input data model routines to load the input data
      !    into memory
      call input_load(dis%parser, 'DISU6', 'GWF', 'DISU', name_model, 'DISU', &
                      [character(len=LENPACKAGETYPE) ::], iout)
      !
      ! -- load disu
      call disnew%disu_load()
    end if
    !
    ! -- Return
    return
  end subroutine disu_cr

  subroutine disu_load(this)
! ******************************************************************************
! disu_load -- transfer data into this discretization object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryHelperModule, only: create_mem_path
    ! -- dummy
    class(GwfDisuType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- source input data
    call this%source_options()
    call this%source_dimensions()
    call this%source_griddata()
    call this%source_connectivity()
    !
    ! -- If NVERT specified and greater than 0, then source VERTICES and CELL2D
    if (this%nvert > 0) then
      call this%source_vertices()
      call this%source_cell2d()
    else
      ! -- connection direction information cannot be calculated
      this%icondir = 0
    end if
    !
    ! -- Make some final disu checks on the non-reduced user-provided
    !    input
    call this%disu_ck()
    !
    ! -- Return
    return
  end subroutine disu_load

  subroutine disu_df(this)
! ******************************************************************************
! disu_df -- Read discretization information from DISU input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfDisuType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Finalize the grid by creating the connection object and reducing the
    !    grid using IDOMAIN, if necessary
    call this%grid_finalize()
    !
    ! -- Return
    return
  end subroutine disu_df

  subroutine grid_finalize(this)
! ******************************************************************************
! grid_finalize -- Finalize grid
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_reallocate
    ! -- dummy
    class(GwfDisuType) :: this
    ! -- locals
    integer(I4B) :: n
    integer(I4B) :: node
    integer(I4B) :: noder
    integer(I4B) :: nrsize
    ! -- formats
    character(len=*), parameter :: fmtdz = &
      "('CELL (',i0,',',i0,',',i0,') THICKNESS <= 0. ', &
      &'TOP, BOT: ',2(1pg24.15))"
    character(len=*), parameter :: fmtnr = &
      "(/1x, 'The specified IDOMAIN results in a reduced number of cells.',&
      &/1x, 'Number of user nodes: ',I0,&
      &/1X, 'Number of nodes in solution: ', I0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- count active cells
    this%nodes = 0
    do n = 1, this%nodesuser
      if (this%idomain(n) > 0) this%nodes = this%nodes + 1
    end do
    !
    ! -- Check to make sure nodes is a valid number
    if (this%nodes == 0) then
      call store_error('Model does not have any active nodes. &
                       &Ensure IDOMAIN array has some values greater &
                       &than zero.')
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
    !    solution. (negative idomain not supported for disu)
    if (this%nodes < this%nodesuser) then
      noder = 1
      do node = 1, this%nodesuser
        if (this%idomain(node) > 0) then
          this%nodereduced(node) = noder
          noder = noder + 1
        elseif (this%idomain(node) < 0) then
          this%nodereduced(node) = -1
        else
          this%nodereduced(node) = 0
        end if
      end do
    end if
    !
    ! -- Fill nodeuser if a reduced grid
    if (this%nodes < this%nodesuser) then
      noder = 1
      do node = 1, this%nodesuser
        if (this%idomain(node) > 0) then
          this%nodeuser(noder) = node
          noder = noder + 1
        end if
      end do
    end if
    !
    ! -- Move top1d, bot1d, and area1d into top, bot, and area
    do node = 1, this%nodesuser
      noder = node
      if (this%nodes < this%nodesuser) noder = this%nodereduced(node)
      if (noder <= 0) cycle
      this%top(noder) = this%top1d(node)
      this%bot(noder) = this%bot1d(node)
      this%area(noder) = this%area1d(node)
    end do
    !
    ! -- fill cell center coordinates
    if (this%nvert > 0) then
      do node = 1, this%nodesuser
        noder = node
        if (this%nodes < this%nodesuser) noder = this%nodereduced(node)
        if (noder <= 0) cycle
        this%xc(noder) = this%cellxy(1, node)
        this%yc(noder) = this%cellxy(2, node)
      end do
    else
      call mem_reallocate(this%xc, 0, 'XC', this%memoryPath)
      call mem_reallocate(this%yc, 0, 'YC', this%memoryPath)
    end if
    !
    ! -- create and fill the connections object
    nrsize = 0
    if (this%nodes < this%nodesuser) nrsize = this%nodes
    allocate (this%con)
    call this%con%disuconnections(this%name_model, this%nodes, &
                                  this%nodesuser, nrsize, &
                                  this%nodereduced, this%nodeuser, &
                                  this%iainp, this%jainp, &
                                  this%ihcinp, this%cl12inp, &
                                  this%hwvainp, this%angldegxinp, &
                                  this%iangledegx)
    this%nja = this%con%nja
    this%njas = this%con%njas
    !
    ! -- Return
    return
  end subroutine grid_finalize

  subroutine disu_ck(this)
! ******************************************************************************
! disu_ck -- Check the discretization information
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfDisuType) :: this
    ! -- local
    integer(I4B) :: n, m
    integer(I4B) :: ipos
    integer(I4B) :: ihc
    real(DP) :: dz
    ! -- formats
    character(len=*), parameter :: fmtidm = &
      &"('Invalid idomain value ', i0, ' specified for node ', i0)"
    character(len=*), parameter :: fmtdz = &
      &"('Cell ', i0, ' with thickness <= 0. Top, bot: ', 2(1pg24.15))"
    character(len=*), parameter :: fmtarea = &
      &"('Cell ', i0, ' with area <= 0. Area: ', 1(1pg24.15))"
    character(len=*), parameter :: fmtjan = &
      &"('Cell ', i0, ' must have its first connection be itself.  Found: ', i0)"
    character(len=*), parameter :: fmtjam = &
      &"('Cell ', i0, ' has invalid connection in JA.  Found: ', i0)"
    character(len=*), parameter :: fmterrmsg = &
      "('Top elevation (', 1pg15.6, ') for cell ', i0, ' is above bottom &
      &elevation (', 1pg15.6, ') for cell ', i0, '. Based on node numbering &
      &rules cell ', i0, ' must be below cell ', i0, '.')"
! ------------------------------------------------------------------------------
    !
    ! -- Check connectivity
    do n = 1, this%nodesuser
      !
      ! -- Ensure first connection is to itself, and
      !    that ja(ia(n)) is positive
      ipos = this%iainp(n)
      m = this%jainp(ipos)
      if (m < 0) then
        m = abs(m)
        this%jainp(ipos) = m
      end if
      if (n /= m) then
        write (errmsg, fmtjan) n, m
        call store_error(errmsg)
      end if
      !
      ! -- Check for valid node numbers in connected cells
      do ipos = this%iainp(n) + 1, this%iainp(n + 1) - 1
        m = this%jainp(ipos)
        if (m < 0 .or. m > this%nodesuser) then
          ! -- make sure first connection is to itself
          write (errmsg, fmtjam) n, m
          call store_error(errmsg)
        end if
      end do
    end do
    !
    ! -- terminate if errors found
    if (count_errors() > 0) then
      if (this%inunit > 0) then
        call store_error_unit(this%inunit)
      end if
    end if
    !
    ! -- Ensure idomain values are valid
    do n = 1, this%nodesuser
      if (this%idomain(n) > 1 .or. this%idomain(n) < 0) then
        write (errmsg, fmtidm) this%idomain(n), n
        call store_error(errmsg)
      end if
    end do
    !
    ! -- Check for zero and negative thickness and zero or negative areas
    !    for cells with idomain == 1
    do n = 1, this%nodesuser
      if (this%idomain(n) == 1) then
        dz = this%top1d(n) - this%bot1d(n)
        if (dz <= DZERO) then
          write (errmsg, fmt=fmtdz) n, this%top1d(n), this%bot1d(n)
          call store_error(errmsg)
        end if
        if (this%area1d(n) <= DZERO) then
          write (errmsg, fmt=fmtarea) n, this%area1d(n)
          call store_error(errmsg)
        end if
      end if
    end do
    !
    ! -- check to make sure voffsettol is >= 0
    if (this%voffsettol < DZERO) then
      write (errmsg, '(a, 1pg15.6)') &
        'Vertical offset tolerance must be greater than zero. Found ', &
        this%voffsettol
      call store_error(errmsg)
      if (this%inunit > 0) then
        call store_error_unit(this%inunit)
      end if
    end if
    !
    ! -- For cell n, ensure that underlying cells have tops less than
    !    or equal to the bottom of cell n
    do n = 1, this%nodesuser
      do ipos = this%iainp(n) + 1, this%iainp(n + 1) - 1
        m = this%jainp(ipos)
        ihc = this%ihcinp(ipos)
        if (ihc == 0 .and. m > n) then
          dz = this%top1d(m) - this%bot1d(n)
          if (dz > this%voffsettol) then
            write (errmsg, fmterrmsg) this%top1d(m), m, this%bot1d(n), n, m, n
            call store_error(errmsg)
          end if
        end if
      end do
    end do
    !
    ! -- terminate if errors found
    if (count_errors() > 0) then
      if (this%inunit > 0) then
        call store_error_unit(this%inunit)
      end if
    end if
    !
    ! -- Return
    return
  end subroutine disu_ck

  subroutine disu_da(this)
! ******************************************************************************
! disu_da -- Deallocate discretization object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(GwfDisuType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate idm memory
    call memorylist_remove(this%name_model, 'DISU', idm_context)
    call memorylist_remove(component=this%name_model, &
                           context=idm_context)
    !
    ! -- scalars
    call mem_deallocate(this%njausr)
    call mem_deallocate(this%nvert)
    call mem_deallocate(this%voffsettol)
    call mem_deallocate(this%iangledegx)
    !
    ! -- arrays
    if (this%readFromFile) then
      call mem_deallocate(this%top1d)
      call mem_deallocate(this%bot1d)
      call mem_deallocate(this%area1d)
      if (associated(this%iavert)) then
        call mem_deallocate(this%iavert)
        call mem_deallocate(this%javert)
      end if
      call mem_deallocate(this%vertices)
      call mem_deallocate(this%iainp)
      call mem_deallocate(this%jainp)
      call mem_deallocate(this%ihcinp)
      call mem_deallocate(this%cl12inp)
      call mem_deallocate(this%hwvainp)
      call mem_deallocate(this%angldegxinp)
    end if

    call mem_deallocate(this%idomain)
    call mem_deallocate(this%cellxy)

    call mem_deallocate(this%nodeuser)
    call mem_deallocate(this%nodereduced)
    !
    ! -- DisBaseType deallocate
    call this%DisBaseType%dis_da()
    !
    ! -- Return
    return
  end subroutine disu_da

  subroutine nodeu_to_string(this, nodeu, str)
! ******************************************************************************
! nodeu_to_string -- Convert user node number to a string in the form of
! (nodenumber) or (k,i,j)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfDisuType) :: this
    integer(I4B), intent(in) :: nodeu
    character(len=*), intent(inout) :: str
    ! -- local
    character(len=10) :: nstr
! ------------------------------------------------------------------------------
    !
    write (nstr, '(i0)') nodeu
    str = '('//trim(adjustl(nstr))//')'
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
    class(GwfDisuType) :: this
    integer(I4B), intent(in) :: nodeu
    integer(I4B), dimension(:), intent(inout) :: arr
    ! -- local
    integer(I4B) :: isize
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
    ! -- fill array
    arr(1) = nodeu
    !
    ! -- return
    return
  end subroutine nodeu_to_array

  !> @brief Write user options to list file
  !<
  subroutine log_options(this, found)
    use GwfDisuInputModule, only: GwfDisuParamFoundType
    class(GwfDisuType) :: this
    type(GwfDisuParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting Discretization Options'

    if (found%length_units) then
      write (this%iout, '(4x,a,i0)') 'Model length unit [0=UND, 1=FEET, &
      &2=METERS, 3=CENTIMETERS] set as ', this%lenuni
    end if

    if (found%nogrb) then
      write (this%iout, '(4x,a,i0)') 'Binary grid file [0=GRB, 1=NOGRB] &
        &set as ', this%nogrb
    end if

    if (found%xorigin) then
      write (this%iout, '(4x,a,G0)') 'XORIGIN = ', this%xorigin
    end if

    if (found%yorigin) then
      write (this%iout, '(4x,a,G0)') 'YORIGIN = ', this%yorigin
    end if

    if (found%angrot) then
      write (this%iout, '(4x,a,G0)') 'ANGROT = ', this%angrot
    end if

    if (found%voffsettol) then
      write (this%iout, '(4x,a,G0)') 'VERTICAL_OFFSET_TOLERANCE = ', &
        this%voffsettol
    end if

    write (this%iout, '(1x,a,/)') 'End Setting Discretization Options'

  end subroutine log_options

  !> @brief Copy options from IDM into package
  !<
  subroutine source_options(this)
! ******************************************************************************
! source_options -- source options from memory manager input path
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use KindModule, only: LGP
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use GwfDisuInputModule, only: GwfDisuParamFoundType
    ! -- dummy
    class(GwfDisuType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    character(len=LENVARNAME), dimension(3) :: lenunits = &
      &[character(len=LENVARNAME) :: 'FEET', 'METERS', 'CENTIMETERS']
    type(GwfDisuParamFoundType) :: found
! ------------------------------------------------------------------------------
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'DISU', idm_context)
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%lenuni, 'LENGTH_UNITS', idmMemoryPath, lenunits, &
                       found%length_units)
    call mem_set_value(this%nogrb, 'NOGRB', idmMemoryPath, found%nogrb)
    call mem_set_value(this%xorigin, 'XORIGIN', idmMemoryPath, found%xorigin)
    call mem_set_value(this%yorigin, 'YORIGIN', idmMemoryPath, found%yorigin)
    call mem_set_value(this%angrot, 'ANGROT', idmMemoryPath, found%angrot)
    call mem_set_value(this%voffsettol, 'VOFFSETTOL', idmMemoryPath, &
                       found%voffsettol)
    !
    ! -- log values to list file
    if (this%iout > 0) then
      call this%log_options(found)
    end if
    !
    ! -- Return
    return
  end subroutine source_options

  !> @brief Write dimensions to list file
  !<
  subroutine log_dimensions(this, found)
    use GwfDisuInputModule, only: GwfDisuParamFoundType
    class(GwfDisuType) :: this
    type(GwfDisuParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting Discretization Dimensions'

    if (found%nodes) then
      write (this%iout, '(4x,a,i0)') 'NODES = ', this%nodesuser
    end if

    if (found%nja) then
      write (this%iout, '(4x,a,i0)') 'NJA = ', this%njausr
    end if

    if (found%nvert) then
      write (this%iout, '(4x,a,i0)') 'NVERT = ', this%nvert
    end if

    write (this%iout, '(1x,a,/)') 'End Setting Discretization Dimensions'

  end subroutine log_dimensions

  !> @brief Copy dimensions from IDM into package
  !<
  subroutine source_dimensions(this)
! ******************************************************************************
! source_dimensions -- source dimensions from memory manager input path
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use KindModule, only: LGP
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use GwfDisuInputModule, only: GwfDisuParamFoundType
    ! -- dummy
    class(GwfDisuType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    integer(I4B) :: n
    type(GwfDisuParamFoundType) :: found
! ------------------------------------------------------------------------------
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'DISU', idm_context)
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%nodesuser, 'NODES', idmMemoryPath, found%nodes)
    call mem_set_value(this%njausr, 'NJA', idmMemoryPath, found%nja)
    call mem_set_value(this%nvert, 'NVERT', idmMemoryPath, found%nvert)
    !
    ! -- log simulation values
    if (this%iout > 0) then
      call this%log_dimensions(found)
    end if
    !
    ! -- verify dimensions were set
    if (this%nodesuser < 1) then
      call store_error( &
        'NODES was not specified or was specified incorrectly.')
    end if
    if (this%njausr < 1) then
      call store_error( &
        'NJA was not specified or was specified incorrectly.')
    end if
    !
    ! -- terminate if errors were detected
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- allocate vectors that are the size of nodesuser
    this%readFromFile = .true.
    call mem_allocate(this%top1d, this%nodesuser, 'TOP1D', this%memoryPath)
    call mem_allocate(this%bot1d, this%nodesuser, 'BOT1D', this%memoryPath)
    call mem_allocate(this%area1d, this%nodesuser, 'AREA1D', this%memoryPath)
    call mem_allocate(this%idomain, this%nodesuser, 'IDOMAIN', this%memoryPath)
    call mem_allocate(this%vertices, 2, this%nvert, 'VERTICES', this%memoryPath)
    call mem_allocate(this%iainp, this%nodesuser + 1, 'IAINP', this%memoryPath)
    call mem_allocate(this%jainp, this%njausr, 'JAINP', this%memoryPath)
    call mem_allocate(this%ihcinp, this%njausr, 'IHCINP', this%memoryPath)
    call mem_allocate(this%cl12inp, this%njausr, 'CL12INP', this%memoryPath)
    call mem_allocate(this%hwvainp, this%njausr, 'HWVAINP', this%memoryPath)
    call mem_allocate(this%angldegxinp, this%njausr, 'ANGLDEGXINP', &
                      this%memoryPath)
    if (this%nvert > 0) then
      call mem_allocate(this%cellxy, 2, this%nodesuser, 'CELLXY', this%memoryPath)
    else
      call mem_allocate(this%cellxy, 2, 0, 'CELLXY', this%memoryPath)
    end if
    !
    ! -- initialize all cells to be active (idomain = 1)
    do n = 1, this%nodesuser
      this%idomain(n) = 1
    end do
    !
    ! -- Return
    return
  end subroutine source_dimensions

  !> @brief Write griddata found to list file
  !<
  subroutine log_griddata(this, found)
    use GwfDisuInputModule, only: GwfDisuParamFoundType
    class(GwfDisuType) :: this
    type(GwfDisuParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting Discretization Griddata'

    if (found%top) then
      write (this%iout, '(4x,a)') 'TOP set from input file'
    end if

    if (found%bot) then
      write (this%iout, '(4x,a)') 'BOT set from input file'
    end if

    if (found%area) then
      write (this%iout, '(4x,a)') 'AREA set from input file'
    end if

    if (found%idomain) then
      write (this%iout, '(4x,a)') 'IDOMAIN set from input file'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting Discretization Griddata'

  end subroutine log_griddata

  subroutine source_griddata(this)
! ******************************************************************************
! source_griddata -- source griddata from memory manager input path
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use GwfDisuInputModule, only: GwfDisuParamFoundType
    ! -- dummy
    class(GwfDisuType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(GwfDisuParamFoundType) :: found
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'DISU', idm_context)
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%top1d, 'TOP', idmMemoryPath, found%top)
    call mem_set_value(this%bot1d, 'BOT', idmMemoryPath, found%bot)
    call mem_set_value(this%area1d, 'AREA', idmMemoryPath, found%area)
    call mem_set_value(this%idomain, 'IDOMAIN', idmMemoryPath, found%idomain)
    !
    ! -- log simulation values
    if (this%iout > 0) then
      call this%log_griddata(found)
    end if
    !
    ! -- Return
    return
  end subroutine source_griddata

  !> @brief Write griddata found to list file
  !<
  subroutine log_connectivity(this, found, iac)
    use GwfDisuInputModule, only: GwfDisuParamFoundType
    class(GwfDisuType) :: this
    type(GwfDisuParamFoundType), intent(in) :: found
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: iac

    write (this%iout, '(1x,a)') 'Setting Discretization Connectivity'

    if (associated(iac)) then
      write (this%iout, '(4x,a)') 'IAC set from input file'
    end if

    if (found%ja) then
      write (this%iout, '(4x,a)') 'JA set from input file'
    end if

    if (found%ihc) then
      write (this%iout, '(4x,a)') 'IHC set from input file'
    end if

    if (found%cl12) then
      write (this%iout, '(4x,a)') 'CL12 set from input file'
    end if

    if (found%hwva) then
      write (this%iout, '(4x,a)') 'HWVA set from input file'
    end if

    if (found%angldegx) then
      write (this%iout, '(4x,a)') 'ANGLDEGX set from input file'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting Discretization Connectivity'

  end subroutine log_connectivity

  subroutine source_connectivity(this)
! ******************************************************************************
! source_connectivity -- source connection data from memory manager input path
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use GwfDisuInputModule, only: GwfDisuParamFoundType
    ! -- dummy
    class(GwfDisuType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(GwfDisuParamFoundType) :: found
    integer(I4B), dimension(:), contiguous, pointer :: iac => null()
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'DISU', idm_context)
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%jainp, 'JA', idmMemoryPath, found%ja)
    call mem_set_value(this%ihcinp, 'IHC', idmMemoryPath, found%ihc)
    call mem_set_value(this%cl12inp, 'CL12', idmMemoryPath, found%cl12)
    call mem_set_value(this%hwvainp, 'HWVA', idmMemoryPath, found%hwva)
    call mem_set_value(this%angldegxinp, 'ANGLDEGX', idmMemoryPath, &
                       found%angldegx)
    !
    ! -- set pointer to iac input array
    call mem_setptr(iac, 'IAC', idmMemoryPath)
    !
    ! -- Convert iac to ia
    if (associated(iac)) call iac_to_ia(iac, this%iainp)
    !
    ! -- Set angldegx flag if found
    if (found%angldegx) this%iangledegx = 1
    !
    ! -- log simulation values
    if (this%iout > 0) then
      call this%log_connectivity(found, iac)
    end if
    !
    ! -- Return
    return
  end subroutine source_connectivity

  subroutine source_vertices(this)
! ******************************************************************************
! source_vertices -- source vertex data from memory manager input path
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(GwfDisuType) :: this
    ! -- local
    integer(I4B) :: i
    character(len=LENMEMPATH) :: idmMemoryPath
    real(DP), dimension(:), contiguous, pointer :: vert_x => null()
    real(DP), dimension(:), contiguous, pointer :: vert_y => null()
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'DISU', idm_context)
    !
    ! -- set pointers to memory manager input arrays
    call mem_setptr(vert_x, 'XV', idmMemoryPath)
    call mem_setptr(vert_y, 'YV', idmMemoryPath)
    !
    ! -- set vertices 2d array
    if (associated(vert_x) .and. associated(vert_y)) then
      do i = 1, this%nvert
        this%vertices(1, i) = vert_x(i)
        this%vertices(2, i) = vert_y(i)
      end do
    else
      call store_error('Required Vertex arrays not found.')
    end if
    !
    ! -- log
    if (this%iout > 0) then
      write (this%iout, '(1x,a)') 'Discretization Vertex data loaded'
    end if
    !
    ! -- Return
    return
  end subroutine source_vertices

  subroutine define_cellverts(this, icell2d, ncvert, icvert)
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(GwfDisuType) :: this
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: icell2d
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: ncvert
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: icvert
    ! -- locals
    type(sparsematrix) :: vert_spm
    integer(I4B) :: i, j, ierr
    integer(I4B) :: icv_idx, startvert, maxnnz = 5
! ------------------------------------------------------------------------------
    !
    ! -- initialize sparse matrix
    call vert_spm%init(this%nodesuser, this%nvert, maxnnz)
    !
    ! -- add sparse matrix connections from input memory paths
    icv_idx = 1
    do i = 1, this%nodesuser
      if (icell2d(i) /= i) call store_error('ICELL2D input sequence violation.')
      do j = 1, ncvert(i)
        call vert_spm%addconnection(i, icvert(icv_idx), 0)
        if (j == 1) then
          startvert = icvert(icv_idx)
        elseif (j == ncvert(i) .and. (icvert(icv_idx) /= startvert)) then
          call vert_spm%addconnection(i, startvert, 0)
        end if
        icv_idx = icv_idx + 1
      end do
    end do
    !
    ! -- allocate and fill iavert and javert
    call mem_allocate(this%iavert, this%nodesuser + 1, 'IAVERT', this%memoryPath)
    call mem_allocate(this%javert, vert_spm%nnz, 'JAVERT', this%memoryPath)
    call vert_spm%filliaja(this%iavert, this%javert, ierr)
    call vert_spm%destroy()
    !
    ! -- Return
    return
  end subroutine define_cellverts

  subroutine source_cell2d(this)
! ******************************************************************************
! source_cell2d -- source cell2d data from memory manager input path
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(GwfDisuType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    integer(I4B), dimension(:), contiguous, pointer :: icell2d => null()
    integer(I4B), dimension(:), contiguous, pointer :: ncvert => null()
    integer(I4B), dimension(:), contiguous, pointer :: icvert => null()
    real(DP), dimension(:), contiguous, pointer :: cell_x => null()
    real(DP), dimension(:), contiguous, pointer :: cell_y => null()
    integer(I4B) :: i
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'DISU', idm_context)
    !
    ! -- set pointers to input path ncvert and icvert
    call mem_setptr(icell2d, 'ICELL2D', idmMemoryPath)
    call mem_setptr(ncvert, 'NCVERT', idmMemoryPath)
    call mem_setptr(icvert, 'ICVERT', idmMemoryPath)
    !
    ! --
    if (associated(icell2d) .and. associated(ncvert) &
        .and. associated(icvert)) then
      call this%define_cellverts(icell2d, ncvert, icvert)
    else
      call store_error('Required cell vertex arrays not found.')
    end if
    !
    ! -- set pointers to cell center arrays
    call mem_setptr(cell_x, 'XC', idmMemoryPath)
    call mem_setptr(cell_y, 'YC', idmMemoryPath)
    !
    ! -- set cell centers
    if (associated(cell_x) .and. associated(cell_y)) then
      do i = 1, this%nodesuser
        this%cellxy(1, i) = cell_x(i)
        this%cellxy(2, i) = cell_y(i)
      end do
    else
      call store_error('Required cell center arrays not found.')
    end if
    !
    ! -- log
    if (this%iout > 0) then
      write (this%iout, '(1x,a)') 'Discretization Cell2d data loaded'
    end if
    !
    ! -- Return
    return
  end subroutine source_cell2d

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
    ! -- dummy
    class(GwfDisuType) :: this
    integer(I4B), dimension(:), intent(in) :: icelltype
    ! -- local
    integer(I4B) :: i, iunit, ntxt
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
    ntxt = 10
    if (this%nvert > 0) ntxt = ntxt + 5
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
    write (txthdr, '(a)') 'GRID DISU'
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
    write (txt, '(3a, i0)') 'NODES ', 'INTEGER ', 'NDIM 0 # ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'NJA ', 'INTEGER ', 'NDIM 0 # ', this%con%nja
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
    write (txt, '(3a, i0)') 'TOP ', 'DOUBLE ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'BOT ', 'DOUBLE ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'IA ', 'INTEGER ', 'NDIM 1 ', this%nodesuser + 1
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'JA ', 'INTEGER ', 'NDIM 1 ', this%con%nja
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'ICELLTYPE ', 'INTEGER ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    !
    ! -- if vertices have been read then write additional header information
    if (this%nvert > 0) then
      write (txt, '(3a, i0)') 'VERTICES ', 'DOUBLE ', 'NDIM 2 2 ', this%nvert
      txt(lentxt:lentxt) = new_line('a')
      write (iunit) txt
      write (txt, '(3a, i0)') 'CELLX ', 'DOUBLE ', 'NDIM 1 ', this%nodesuser
      txt(lentxt:lentxt) = new_line('a')
      write (iunit) txt
      write (txt, '(3a, i0)') 'CELLY ', 'DOUBLE ', 'NDIM 1 ', this%nodesuser
      txt(lentxt:lentxt) = new_line('a')
      write (iunit) txt
      write (txt, '(3a, i0)') 'IAVERT ', 'INTEGER ', 'NDIM 1 ', this%nodesuser + 1
      txt(lentxt:lentxt) = new_line('a')
      write (iunit) txt
      write (txt, '(3a, i0)') 'JAVERT ', 'INTEGER ', 'NDIM 1 ', size(this%javert)
      txt(lentxt:lentxt) = new_line('a')
      write (iunit) txt
    end if
    !
    ! -- write data
    write (iunit) this%nodesuser ! nodes
    write (iunit) this%nja ! nja
    write (iunit) this%xorigin ! xorigin
    write (iunit) this%yorigin ! yorigin
    write (iunit) this%angrot ! angrot
    write (iunit) this%top1d ! top
    write (iunit) this%bot1d ! bot
    write (iunit) this%con%iausr ! ia
    write (iunit) this%con%jausr ! ja
    write (iunit) icelltype ! icelltype
    !
    ! -- if vertices have been read then write additional data
    if (this%nvert > 0) then
      write (iunit) this%vertices ! vertices
      write (iunit) (this%cellxy(1, i), i=1, this%nodesuser) ! cellx
      write (iunit) (this%cellxy(2, i), i=1, this%nodesuser) ! celly
      write (iunit) this%iavert ! iavert
      write (iunit) this%javert ! javert
    end if
    !
    ! -- Close the file
    close (iunit)
    !
    ! -- return
    return
  end subroutine write_grb

  function get_nodenumber_idx1(this, nodeu, icheck) result(nodenumber)
! ******************************************************************************
! get_nodenumber -- Return a nodenumber from the user specified node number
!                   with an option to perform a check.  This subroutine
!                   can be overridden by child classes to perform mapping
!                   to a model node number
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfDisuType), intent(in) :: this
    integer(I4B), intent(in) :: nodeu
    integer(I4B), intent(in) :: icheck
    integer(I4B) :: nodenumber
! ------------------------------------------------------------------------------
    !
    if (icheck /= 0) then
      if (nodeu < 1 .or. nodeu > this%nodes) then
        write (errmsg, '(a,i10)') &
          'Nodenumber less than 1 or greater than nodes:', nodeu
        call store_error(errmsg)
      end if
    end if
    !
    ! -- set node number to passed in nodenumber since there is a one to one
    !    mapping for an unstructured grid
    if (this%nodes == this%nodesuser) then
      nodenumber = nodeu
    else
      nodenumber = this%nodereduced(nodeu)
    end if
    !
    ! -- return
    return
  end function get_nodenumber_idx1

  subroutine connection_normal(this, noden, nodem, ihc, xcomp, ycomp, zcomp, &
                               ipos)
! ******************************************************************************
! connection_normal -- calculate the normal vector components for reduced
!   nodenumber cell (noden) and its shared face with cell nodem.  ihc is the
!   horizontal connection flag.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfDisuType) :: this
    integer(I4B), intent(in) :: noden
    integer(I4B), intent(in) :: nodem
    integer(I4B), intent(in) :: ihc
    real(DP), intent(inout) :: xcomp
    real(DP), intent(inout) :: ycomp
    real(DP), intent(inout) :: zcomp
    integer(I4B), intent(in) :: ipos
    ! -- local
    real(DP) :: angle, dmult
! ------------------------------------------------------------------------------
    !
    ! -- Set vector components based on ihc
    if (ihc == 0) then
      !
      ! -- connection is vertical
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
      ! -- find from anglex, since anglex is symmetric, need to flip vector
      !    for lower triangle (nodem < noden)
      angle = this%con%anglex(this%con%jas(ipos))
      dmult = DONE
      if (nodem < noden) dmult = -DONE
      xcomp = cos(angle) * dmult
      ycomp = sin(angle) * dmult
      zcomp = DZERO
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
!   cell centers can be calculated.  ihc is the horizontal flag.  Also return
!   the straight-line connection length.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DHALF
    use DisvGeom, only: line_unit_vector
    ! -- dummy
    class(GwfDisuType) :: this
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
    real(DP) :: xn, xm, yn, ym, zn, zm
! ------------------------------------------------------------------------------
    !
    ! -- Terminate with error if requesting unit vector components for problems
    !    without cell data
    if (size(this%cellxy, 2) < 1) then
      write (errmsg, '(a)') &
        'Cannot calculate unit vector components for DISU grid if VERTEX '// &
        'data are not specified'
      call store_error(errmsg, terminate=.TRUE.)
    end if
    !
    ! -- get xy center coords
    xn = this%xc(noden)
    yn = this%yc(noden)
    xm = this%xc(nodem)
    ym = this%yc(nodem)
    !
    ! -- Set vector components based on ihc
    if (ihc == 0) then
      !
      ! -- vertical connection, calculate z as cell center elevation
      zn = this%bot(noden) + DHALF * (this%top(noden) - this%bot(noden))
      zm = this%bot(nodem) + DHALF * (this%top(nodem) - this%bot(nodem))
    else
      !
      ! -- horizontal connection, with possible z component due to cell offsets
      !    and/or water table conditions
      if (nozee) then
        zn = DZERO
        zm = DZERO
      else
        zn = this%bot(noden) + DHALF * satn * (this%top(noden) - this%bot(noden))
        zm = this%bot(nodem) + DHALF * satm * (this%top(nodem) - this%bot(nodem))
      end if
    end if
    !
    ! -- Use coords to find vector components and connection length
    call line_unit_vector(xn, yn, zn, xm, ym, zm, xcomp, ycomp, zcomp, &
                          conlen)
    !
    ! -- return
    return
  end subroutine connection_vector

  ! return discretization type
  subroutine get_dis_type(this, dis_type)
    class(GwfDisuType), intent(in) :: this
    character(len=*), intent(out) :: dis_type

    dis_type = "DISU"

  end subroutine get_dis_type

  subroutine allocate_scalars(this, name_model)
! ******************************************************************************
! allocate_scalars -- Allocate and initialize scalar variables in this class
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfDisuType) :: this
    character(len=*), intent(in) :: name_model
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Allocate parent scalars
    call this%DisBaseType%allocate_scalars(name_model)
    !
    ! -- Allocate variables for DISU
    call mem_allocate(this%njausr, 'NJAUSR', this%memoryPath)
    call mem_allocate(this%nvert, 'NVERT', this%memoryPath)
    call mem_allocate(this%voffsettol, 'VOFFSETTOL', this%memoryPath)
    call mem_allocate(this%iangledegx, 'IANGLEDEGX', this%memoryPath)
    !
    ! -- Set values
    this%ndim = 1
    this%njausr = 0
    this%nvert = 0
    this%voffsettol = DZERO
    this%iangledegx = 0
    this%readFromFile = .false.
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- Read discretization information from file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfDisuType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Allocate arrays in DisBaseType (mshape, top, bot, area)
    call this%DisBaseType%allocate_arrays()
    !
    ! -- Allocate arrays in DISU
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
    this%mshape(1) = this%nodesuser
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine allocate_arrays_mem(this)
    use MemoryManagerModule, only: mem_allocate
    class(GwfDisuType) :: this

    call mem_allocate(this%idomain, this%nodes, 'IDOMAIN', this%memoryPath)
    call mem_allocate(this%vertices, 2, this%nvert, 'VERTICES', this%memoryPath)
    call mem_allocate(this%cellxy, 2, this%nodes, 'CELLXY', this%memoryPath)

  end subroutine allocate_arrays_mem

  function nodeu_from_string(this, lloc, istart, istop, in, iout, line, &
                             flag_string, allow_zero) result(nodeu)
! ******************************************************************************
! nodeu_from_string -- Receive a string and convert the string to a user
!   nodenumber.  The model is unstructured; just read user nodenumber.
!   If flag_string argument is present and true, the first token in string
!   is allowed to be a string (e.g. boundary name). In this case, if a string
!   is encountered, return value as -2.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfDisuType) :: this
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
    integer(I4B) :: lloclocal, ndum, istat, n
    real(DP) :: r
    character(len=LINELENGTH) :: fname
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
    call urword(line, lloc, istart, istop, 2, nodeu, r, iout, in)
    !
    if (nodeu == 0) then
      if (present(allow_zero)) then
        if (allow_zero) then
          return
        end if
      end if
    end if
    !
    if (nodeu < 1 .or. nodeu > this%nodesuser) then
      write (errmsg, *) ' Node number in list is outside of the grid', nodeu
      call store_error(errmsg)
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
    class(GwfDisuType) :: this
    character(len=*), intent(inout) :: cellid
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    logical, optional, intent(in) :: flag_string
    logical, optional, intent(in) :: allow_zero
    ! -- local
    integer(I4B) :: lloclocal, istart, istop, ndum, n
    integer(I4B) :: istat
    real(DP) :: r
    character(len=LINELENGTH) :: fname
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
    lloclocal = 1
    call urword(cellid, lloclocal, istart, istop, 2, nodeu, r, iout, inunit)
    !
    if (nodeu == 0) then
      if (present(allow_zero)) then
        if (allow_zero) then
          return
        end if
      end if
    end if
    !
    if (nodeu < 1 .or. nodeu > this%nodesuser) then
      write (errmsg, *) ' Node number in list is outside of the grid', nodeu
      call store_error(errmsg)
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
    class(GwfDisuType) :: this
    !
    supports_layers = .false.
    return
  end function supports_layers

  function get_ncpl(this)
! ******************************************************************************
! get_ncpl -- Return number of cells per layer.  This is nodes
!   for a DISU grid, as there are no layers.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    integer(I4B) :: get_ncpl
    ! -- dummy
    class(GwfDisuType) :: this
! ------------------------------------------------------------------------------
    !
    get_ncpl = this%nodesuser
    !
    ! -- Return
    return
  end function get_ncpl

  subroutine read_int_array(this, line, lloc, istart, istop, iout, in, &
                            iarray, aname)
! ******************************************************************************
! read_int_array -- Read a GWF integer array
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfDisuType), intent(inout) :: this
    character(len=*), intent(inout) :: line
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    integer(I4B), intent(in) :: in
    integer(I4B), intent(in) :: iout
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: iarray
    character(len=*), intent(in) :: aname
    ! -- local
    integer(I4B) :: nval
    integer(I4B), dimension(:), pointer, contiguous :: itemp
! ------------------------------------------------------------------------------
    !
    ! -- Point the temporary pointer array, which is passed to the reading
    !    subroutine.  The temporary array will point to ibuff if it is a
    !    reduced structured system, or to iarray if it is an unstructured
    !    model.
    if (this%nodes < this%nodesuser) then
      nval = this%nodesuser
      itemp => this%ibuff
    else
      nval = this%nodes
      itemp => iarray
    end if
    !
    ! -- Read the array
    ! -- Read unstructured input
    call ReadArray(in, itemp, aname, this%ndim, nval, iout, 0)
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
    ! -- dummy
    class(GwfDisuType), intent(inout) :: this
    character(len=*), intent(inout) :: line
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    integer(I4B), intent(in) :: in
    integer(I4B), intent(in) :: iout
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: darray
    character(len=*), intent(in) :: aname
    ! -- local
    integer(I4B) :: nval
    real(DP), dimension(:), pointer, contiguous :: dtemp
! ------------------------------------------------------------------------------
    !
    ! -- Point the temporary pointer array, which is passed to the reading
    !    subroutine.  The temporary array will point to dbuff if it is a
    !    reduced structured system, or to darray if it is an unstructured
    !    model.
    if (this%nodes < this%nodesuser) then
      nval = this%nodesuser
      dtemp => this%dbuff
    else
      nval = this%nodes
      dtemp => darray
    end if
    !
    ! -- Read the array
    call ReadArray(in, dtemp, aname, this%ndim, nval, iout, 0)
    !
    ! -- If reduced model, then need to copy from dtemp(=>dbuff) to darray
    if (this%nodes < this%nodesuser) then
      call this%fill_grid_array(dtemp, darray)
    end if
    !
    ! -- return
    return
  end subroutine read_dbl_array

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
    class(GwfDisuType), intent(inout) :: this
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
    nlay = 1
    nrow = 1
    ncol = this%mshape(1)
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
    class(GwfDisuType) :: this
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
    nlay = 1
    nrow = 1
    ncol = this%mshape(1)
    !
    ! -- Use ubdsv06 to write list header
    call ubdsv06(kstp, kper, text, textmodel, textpackage, dstmodel, dstpackage, &
                 ibdchn, naux, auxtxt, ncol, nrow, nlay, &
                 nlist, iout, delt, pertim, totim)
    !
    ! -- return
    return
  end subroutine record_srcdst_list_header

  !> @brief Cast base to DISU
  !<
  function CastAsDisuType(dis) result(disu)
    class(*), pointer :: dis !< base pointer to DISU object
    class(GwfDisuType), pointer :: disu !< the resulting DISU pointer

    disu => null()
    select type (dis)
    class is (GwfDisuType)
      disu => dis
    end select

  end function CastAsDisuType

end module GwfDisuModule

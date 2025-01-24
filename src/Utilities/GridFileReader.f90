module GridFileReaderModule

  use KindModule
  use SimModule, only: store_error, store_error_unit
  use SimVariablesModule, only: errmsg
  use ConstantsModule, only: LINELENGTH
  use InputOutputModule, only: urword, upcase, openfile
  use Integer1dReaderModule, only: read_int1d
  use Integer2dReaderModule, only: read_int2d
  use Double1dReaderModule, only: read_dbl1d
  use Double2dReaderModule, only: read_dbl2d
  use HashTableModule, only: HashTableType, hash_table_cr, hash_table_da
  use ArrayHandlersModule, only: ExpandArray

  implicit none

  public :: GridFileReaderType

  type :: GridFileReaderType
    private
    integer(I4B), public :: inunit !< file unit
    ! header
    character(len=10), public :: grid_type !< DIS, DISV, DISU, etc
    integer(I4B), public :: version !< binary grid file format version
    integer(I4B) :: ntxt !< number of variables
    integer(I4B) :: lentxt !< header line length per variable
    ! index
    type(HashTableType), pointer :: dim !< map variable name to number of dims
    type(HashTableType), pointer :: pos !< map variable name to position in file
    type(HashTableType), pointer :: typ !< map variable name to type (1=int, 2=dbl)
    type(HashTableType), pointer :: shp_idx !< map variable name to index in shp
    integer(I4B), allocatable :: shp(:) !< flat array of variable shapes
    character(len=10), allocatable, public :: keys(:) !< variable names
  contains
    procedure, public :: initialize
    procedure, public :: finalize
    procedure, public :: read_int
    procedure, public :: read_dbl
    procedure, public :: read_ia
    procedure, public :: read_ja
    procedure, public :: read_grid_shape
    procedure, public :: read_idomain_dis
    procedure, public :: read_idomain_disv
    procedure, public :: read_idomain_disu
    procedure, public :: read_idomain_dis2d
    procedure, public :: read_idomain_disv2d
    procedure, public :: read_idomain_disv1d
    procedure, private :: read_header
    procedure, private :: read_header_meta
    procedure, private :: read_header_body
    ! procedure, private :: build_index
  end type GridFileReaderType

contains

  !> @Brief Initialize the grid file reader.
  subroutine initialize(this, iu)
    class(GridFileReaderType) :: this
    integer(I4B), intent(in) :: iu

    this%inunit = iu
    call hash_table_cr(this%dim)
    call hash_table_cr(this%pos)
    call hash_table_cr(this%typ)
    call hash_table_cr(this%shp_idx)
    allocate (this%shp(0))
    call this%read_header()
    ! call this%build_index()
  end subroutine initialize

  !> @brief Finalize the grid file reader.
  subroutine finalize(this)
    class(GridFileReaderType) :: this

    close (this%inunit)
    call hash_table_da(this%dim)
    call hash_table_da(this%pos)
    call hash_table_da(this%typ)
    call hash_table_da(this%shp_idx)
    deallocate (this%shp)
  end subroutine finalize

  !> @brief Read the file's self-describing header. Internal use only.
  subroutine read_header(this)
    class(GridFileReaderType) :: this
    call this%read_header_meta()
    call this%read_header_body()
  end subroutine read_header

  !> @brief Read self-describing metadata (first four lines). Internal use only.
  subroutine read_header_meta(this)
    ! dummy
    class(GridFileReaderType) :: this
    ! local
    character(len=50) :: line
    integer(I4B) :: lloc, istart, istop
    integer(I4B) :: ival
    real(DP) :: rval

    ! grid type
    read (this%inunit) line
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
    if (line(istart:istop) /= 'GRID') then
      call store_error('Binary grid file must begin with "GRID". '//&
                       &'Found: '//line(istart:istop))
      call store_error_unit(this%inunit)
    end if
    call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
    this%grid_type = line(istart:istop)
    call upcase(this%grid_type)

    ! version
    read (this%inunit) line
    lloc = 1
    call urword(line, lloc, istart, istop, 0, ival, rval, 0, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
    this%version = ival

    ! ntxt
    read (this%inunit) line
    lloc = 1
    call urword(line, lloc, istart, istop, 0, ival, rval, 0, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
    this%ntxt = ival

    ! lentxt
    read (this%inunit) line
    lloc = 1
    call urword(line, lloc, istart, istop, 0, ival, rval, 0, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
    this%lentxt = ival

  end subroutine read_header_meta

  !> @brief Read the header body section (text following first
  !< four "meta" lines) and build an index. Internal use only.
  subroutine read_header_body(this)
    ! dummy
    class(GridFileReaderType) :: this
    ! local
    character(len=:), allocatable :: body
    character(len=:), allocatable :: line
    character(len=10) :: key, dtype
    real(DP) :: rval
    integer(I4B) :: i, lloc, istart, istop, ival, pos
    integer(I4B) :: nvars, ndim, dim, ishp
    integer(I4B), allocatable :: shp(:)

    allocate (this%keys(this%ntxt))
    allocate (character(len=this%lentxt*this%ntxt) :: body)
    allocate (character(len=this%lentxt) :: line)

    nvars = 0
    read (this%inunit) body
    inquire (this%inunit, pos=pos)
    do i = 1, this%lentxt * this%ntxt, this%lentxt
      line = body(i:i + this%lentxt - 1)
      lloc = 1

      ! key
      lloc = 1
      call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
      key = line(istart:istop)
      call upcase(key)
      nvars = nvars + 1
      this%keys(nvars) = key

      ! type
      call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
      dtype = line(istart:istop)
      call upcase(dtype)
      if (dtype == "INTEGER") then
        call this%typ%add(key, 1)
      else if (dtype == "DOUBLE") then
        call this%typ%add(key, 2)
      end if

      ! dims
      call urword(line, lloc, istart, istop, 0, ival, rval, 0, 0)
      call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
      ndim = ival
      call this%dim%add(key, ndim)

      ! shape
      if (allocated(shp)) deallocate (shp)
      allocate (shp(ndim))
      if (ndim > 0) then
        do dim = 1, ndim
          call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
          shp(dim) = ival
        end do
        ishp = size(this%shp)
        call ExpandArray(this%shp, increment=ndim)
        this%shp(ishp + 1:ishp + ndim) = shp
        call this%shp_idx%add(key, ishp + 1)
      end if

      ! position
      call this%pos%add(key, pos)
      if (ndim == 0) then
        if (dtype == "INTEGER") then
          pos = pos + 4
        else if (dtype == "DOUBLE") then
          pos = pos + 8
        end if
      else
        if (dtype == "INTEGER") then
          pos = pos + (product(shp) * 4)
        else if (dtype == "DOUBLE") then
          pos = pos + (product(shp) * 8)
        end if
      end if
    end do

    rewind (this%inunit)
  end subroutine read_header_body

  !> @brief Read an integer from a grid file.
  function read_int(this, key) result(v)
    class(GridFileReaderType), intent(inout) :: this
    character(len=*), intent(in) :: key
    integer(I4B) :: v
    ! local
    integer(I4B) :: ndim, pos, typ

    ndim = this%dim%get(key)
    if (ndim > 0) then
      write (errmsg, '(a)') 'Variable '//trim(key)//' is not a scalar'
      call store_error(errmsg, terminate=.TRUE.)
    end if

    typ = this%typ%get(key)
    if (typ /= 1) then
      write (errmsg, '(a)') 'Variable '//trim(key)//' is not an integer'
      call store_error(errmsg, terminate=.TRUE.)
    end if

    pos = this%pos%get(key)
    read (this%inunit, pos=pos) v
    rewind (this%inunit)
  end function read_int

  !> @brief Read a double from a grid file.
  function read_dbl(this, key) result(v)
    class(GridFileReaderType), intent(inout) :: this
    character(len=*), intent(in) :: key
    real(DP) :: v
    ! local
    integer(I4B) :: ndim, pos, typ

    ndim = this%dim%get(key)
    if (ndim > 0) then
      write (errmsg, '(a)') 'Variable '//trim(key)//' is not a scalar'
      call store_error(errmsg, terminate=.TRUE.)
    end if

    typ = this%typ%get(key)
    if (typ /= 2) then
      write (errmsg, '(a)') 'Variable '//trim(key)//' is not a double'
      call store_error(errmsg, terminate=.TRUE.)
    end if

    pos = this%pos%get(key)
    read (this%inunit, pos=pos) v
    rewind (this%inunit)
  end function read_dbl

  !> @brief Read the grid shape from a grid file.
  function read_grid_shape(this) result(v)
    ! dummy
    class(GridFileReaderType) :: this
    integer(I4B), allocatable :: v(:)

    select case (this%grid_type)
    case ("DIS")
      allocate (v(3))
      v(1) = this%read_int("NLAY")
      v(2) = this%read_int("NROW")
      v(3) = this%read_int("NCOL")
    case ("DISV")
      allocate (v(2))
      v(1) = this%read_int("NLAY")
      v(2) = this%read_int("NCPL")
    case ("DISU")
      allocate (v(1))
      v(1) = this%read_int("NODES")
    end select
  end function read_grid_shape

  !> @brief Read ia array from a grid file.
  function read_ia(this) result(v)
    ! dummy
    class(GridFileReaderType) :: this
    integer(I4B), allocatable :: v(:)
    ! local
    character(len=10) :: key
    integer(I4B) :: idx, nvals, pos

    key = "IA"
    idx = this%shp_idx%get(key)
    pos = this%pos%get(key)
    nvals = this%shp(idx)
    allocate (v(nvals))
    read (this%inunit, pos=pos) v
    rewind (this%inunit)
  end function read_ia

  !> @brief Read ja array from a grid file.
  function read_ja(this) result(v)
    ! dummy
    class(GridFileReaderType) :: this
    integer(I4B), allocatable :: v(:)
    ! local
    character(len=10) :: key
    integer(I4B) :: idx, nvals, pos

    key = "JA"
    idx = this%shp_idx%get(key)
    pos = this%pos%get(key)
    nvals = this%shp(idx)
    allocate (v(nvals))
    read (this%inunit, pos=pos) v
    rewind (this%inunit)
  end function read_ja

  !> @brief Read idomain array from a DIS grid file.
  function read_idomain_dis(this) result(v)
    ! dummy
    class(GridFileReaderType) :: this
    integer(I4B), allocatable :: v(:, :, :)
    ! local
    character(len=10) :: key
    integer(I4B) :: idx, nvals, pos
    integer(I4B), allocatable :: tmp(:)

    if (this%grid_type /= "DIS") then
      write (errmsg, '(a)') 'Grid type is not DIS but '//this%grid_type
      call store_error(errmsg, terminate=.TRUE.)
    end if

    key = "IDOMAIN"
    idx = this%shp_idx%get(key)
    pos = this%pos%get(key)
    nvals = this%shp(idx)
    allocate (tmp(nvals))
    read (this%inunit, pos=pos) tmp
    v = reshape(tmp, [ &
                this%read_int("NCOL"), &
                this%read_int("NROW"), &
                this%read_int("NLAY") &
                ])
    deallocate (tmp)
    rewind (this%inunit)
  end function read_idomain_dis

  !> @brief Read idomain array from a DISV grid file.
  function read_idomain_disv(this) result(v)
    ! dummy
    class(GridFileReaderType) :: this
    integer(I4B), allocatable :: v(:, :)
    ! local
    character(len=10) :: key
    integer(I4B) :: idx, nvals, pos
    integer(I4B), allocatable :: tmp(:)

    if (this%grid_type /= "DISV") then
      write (errmsg, '(a)') 'Grid type is not DISV but '//this%grid_type
      call store_error(errmsg, terminate=.TRUE.)
    end if

    key = "IDOMAIN"
    idx = this%shp_idx%get(key)
    pos = this%pos%get(key)
    nvals = this%shp(idx)
    allocate (tmp(nvals))
    read (this%inunit, pos=pos) tmp
    v = reshape(tmp, [ &
                this%read_int("NCPL"), &
                this%read_int("NLAY") &
                ])
    deallocate (tmp)
    rewind (this%inunit)
  end function read_idomain_disv

  !> @brief Read idomain array from a DISU grid file.
  function read_idomain_disu(this) result(v)
    ! dummy
    class(GridFileReaderType) :: this
    integer(I4B), allocatable :: v(:)
    ! local
    character(len=10) :: key
    integer(I4B) :: idx, nvals, pos

    if (this%grid_type /= "DISU") then
      write (errmsg, '(a)') 'Grid type is not DISU but '//this%grid_type
      call store_error(errmsg, terminate=.TRUE.)
    end if

    key = "IDOMAIN"
    idx = this%shp_idx%get(key)
    pos = this%pos%get(key)
    nvals = this%shp(idx)
    allocate (v(nvals))
    read (this%inunit, pos=pos) v
    rewind (this%inunit)
  end function read_idomain_disu

  !> @brief Read idomain array from a DIS2D grid file.
  function read_idomain_dis2d(this) result(v)
    ! dummy
    class(GridFileReaderType) :: this
    integer(I4B), allocatable :: v(:, :)
    ! local
    character(len=10) :: key
    integer(I4B) :: idx, nvals, pos
    integer(I4B), allocatable :: tmp(:)

    if (this%grid_type /= "DIS2D") then
      write (errmsg, '(a)') 'Grid type is not DIS2D but '//this%grid_type
      call store_error(errmsg, terminate=.TRUE.)
    end if

    key = "IDOMAIN"
    idx = this%shp_idx%get(key)
    pos = this%pos%get(key)
    nvals = this%shp(idx)
    allocate (tmp(nvals))
    read (this%inunit, pos=pos) tmp
    v = reshape(tmp, [ &
                this%read_int("NCOL"), &
                this%read_int("NLAY") &
                ])
    deallocate (tmp)
    rewind (this%inunit)
  end function read_idomain_dis2d

  !> @brief Read idomain array from a DISV2D grid file.
  function read_idomain_disv2d(this) result(v)
    ! dummy
    class(GridFileReaderType) :: this
    integer(I4B), allocatable :: v(:)
    ! local
    character(len=10) :: key
    integer(I4B) :: idx, nvals, pos

    if (this%grid_type /= "DISV2D") then
      write (errmsg, '(a)') 'Grid type is not DISV2D but '//this%grid_type
      call store_error(errmsg, terminate=.TRUE.)
    end if

    key = "IDOMAIN"
    idx = this%shp_idx%get(key)
    pos = this%pos%get(key)
    nvals = this%shp(idx)
    allocate (v(nvals))
    read (this%inunit, pos=pos) v
    rewind (this%inunit)
  end function read_idomain_disv2d

  !> @brief Read idomain array from a DISV1D grid file.
  function read_idomain_disv1d(this) result(v)
    ! dummy
    class(GridFileReaderType) :: this
    integer(I4B), allocatable :: v(:)
    ! local
    character(len=10) :: key
    integer(I4B) :: idx, nvals, pos

    if (this%grid_type /= "DISV1D") then
      write (errmsg, '(a)') 'Grid type is not DISV1D but '//this%grid_type
      call store_error(errmsg, terminate=.TRUE.)
    end if

    key = "IDOMAIN"
    idx = this%shp_idx%get(key)
    pos = this%pos%get(key)
    nvals = this%shp(idx)
    allocate (v(nvals))
    read (this%inunit, pos=pos) v
    rewind (this%inunit)
  end function read_idomain_disv1d

end module GridFileReaderModule

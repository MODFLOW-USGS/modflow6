module GridFileReaderModule

  use KindModule
  use SimModule, only: store_error, store_error_unit
  use ConstantsModule, only: LINELENGTH
  use BaseDisModule, only: DisBaseType
  use DisModule, only: DisType
  use DisvModule, only: DisvType
  use DisuModule, only: DisuType
  use InputOutputModule, only: urword, read_line, upcase
  use LongLineReaderModule, only: LongLineReaderType

  implicit none

  public :: GridFileReaderType

  type :: GridFileReaderType
    private
    integer(I4B), public :: inunit
    character(len=10), public :: grid_type
    integer(I4B), public :: version
    integer(I4B) :: ntxt
    integer(I4B) :: lentxt
    type(LongLineReaderType) :: linereader
  contains
    procedure, public :: initialize
    procedure, public :: finalize
    procedure, public :: get_nodes
    generic, public :: get_idomain => &
      get_idomain_dis, get_idomain_disv, get_idomain_disu
    procedure, private :: get_idomain_dis
    procedure, private :: get_idomain_disv
    procedure, private :: get_idomain_disu
  end type GridFileReaderType

contains

  subroutine initialize(this, iu, iout)
    ! dummy
    class(GridFileReaderType) :: this
    integer(I4B), intent(in) :: iu
    integer(I4B), intent(in), optional :: iout
    ! local
    character(len=:), allocatable :: line
    integer(I4B) :: liout, lloc, istart, istop, ival, ierr
    real(DP) :: rval

    this%inunit = iu

    if (present(iout)) then
      liout = iout
    else
      liout = 0
    end if

    if (iout > 0) &
      write (iout, '(a)') &
      'Reading grid file text headers to determine variables.'

    ! grid type
    call this%linereader%rdcom(this%inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    this%grid_type = line(istart:istop)
    call upcase(this%grid_type)

    ! version
    call this%linereader%rdcom(this%inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    this%version = ival

    ! ntxt
    call this%linereader%rdcom(this%inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    this%ntxt = ival

    ! lentxt
    call this%linereader%rdcom(this%inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    this%lentxt = ival

    ! rewind file
    rewind (this%inunit)

  end subroutine initialize

  subroutine finalize(this)
    class(GridFileReaderType) :: this
    close (this%inunit)
  end subroutine finalize

  function get_nodes(this) result(nodes)
    class(GridFileReaderType) :: this
    integer(I4B) :: nodes
    ! local
    character(len=:), allocatable :: line, key
    integer(I4B) :: i, lloc, istart, istop, ival, ierr
    real(DP) :: rval

    ! skip 4 header lines
    do i = 1, 4
      call this%linereader%rdcom(this%inunit, 0, line, ierr)
    end do

    ! read remaining lines, find index
    ! of node count, and read value
    do i = 1, this%ntxt
      call this%linereader%rdcom(this%inunit, 0, line, ierr)
      lloc = 1
      call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
      key = line(istart:istop)
      if (key == "NCELLS") then
        call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
        call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
        call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
        call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
        call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
        nodes = ival
        exit
      end if
    end do

    ! rewind file
    rewind (this%inunit)

  end function get_nodes

  subroutine get_idomain_dis(this, idomain)
    class(GridFileReaderType) :: this
    integer(I4B), allocatable, intent(out) :: idomain(:, :, :)
    ! local
    character(len=:), allocatable :: line, key
    integer(I4B) :: i, idx, iostat, lloc, istart, istop, ival, ierr, dim
    integer(I4B) :: shp(3)
    real(DP) :: rval

    ! skip 4 header lines
    do i = 1, 4
      call this%linereader%rdcom(this%inunit, 0, line, ierr)
    end do

    ! read remaining lines, find index
    ! of idomain, and load its shape
    idx = 0
    do i = 1, this%ntxt
      call this%linereader%rdcom(this%inunit, 0, line, ierr)
      lloc = 1
      call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
      key = line(istart:istop)
      call upcase(key)
      if (key == "IDOMAIN") then
        idx = i
        call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
        call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
        call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
        do dim = 1, 3
          call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
          shp(dim) = ival
        end do
      end if
    end do

    ! skip to and read idomain array
    do i = 1, idx - 1
      read (this%inunit, iostat=iostat)
    end do
    allocate (idomain(shp(1), shp(2), shp(3)))
    read (this%inunit, iostat=iostat) idomain

    ! rewind file
    rewind (this%inunit)

  end subroutine get_idomain_dis

  subroutine get_idomain_disv(this, idomain)
    class(GridFileReaderType) :: this
    integer(I4B), allocatable, intent(out) :: idomain(:, :)
    ! local
    character(len=:), allocatable :: line, key
    integer(I4B) :: i, idx, iostat, lloc, istart, istop, ival, ierr, ndim, dim
    integer(I4B) :: shp(2)
    real(DP) :: rval

    ! skip 4 header lines
    do i = 1, 4
      call this%linereader%rdcom(this%inunit, 0, line, ierr)
    end do

    ! read remaining text lines, find index
    ! of idomain array, and load its shape
    idx = 0
    do i = 1, this%ntxt
      call this%linereader%rdcom(this%inunit, 0, line, ierr)
      lloc = 1
      call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
      key = line(istart:istop)
      call upcase(key)
      if (key == "IDOMAIN") then
        idx = i
        call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
        call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
        call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
        ndim = ival
        do dim = 1, ndim
          call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
          shp(dim) = ival
        end do
        if (ndim == 1) then
          shp(2) = ival
        end if
      end if
    end do

    ! skip to and read idomain array
    do i = 1, idx - 1
      read (this%inunit, iostat=iostat)
    end do
    allocate (idomain(shp(1), shp(2)))
    read (this%inunit, iostat=iostat) idomain

    ! rewind file
    rewind (this%inunit)

  end subroutine get_idomain_disv

  subroutine get_idomain_disu(this, idomain)
    class(GridFileReaderType) :: this
    integer(I4B), allocatable, intent(out) :: idomain(:)
    ! local
    character(len=:), allocatable :: line, key
    integer(I4B) :: i, idx, iostat, lloc, istart, istop, ival, ierr
    integer(I4B) :: shp
    real(DP) :: rval

    ! skip 4 header lines
    do i = 1, 4
      call this%linereader%rdcom(this%inunit, 0, line, ierr)
    end do

    ! read remaining text lines, find index
    ! of idomain array, and load its shape
    idx = 0
    do i = 1, this%ntxt
      call this%linereader%rdcom(this%inunit, 0, line, ierr)
      lloc = 1
      call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
      key = line(istart:istop)
      call upcase(key)
      if (key == "IDOMAIN") then
        idx = i
        call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
        call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
        call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
        shp = ival
      end if
    end do

    ! skip to and read idomain array
    do i = 1, idx - 1
      read (this%inunit, iostat=iostat)
    end do
    allocate (idomain(shp))
    read (this%inunit, iostat=iostat) idomain

    ! rewind file
    rewind (this%inunit)

  end subroutine get_idomain_disu

end module GridFileReaderModule

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

  private :: load_dis, load_disv, load_disu
  public :: load_grb

contains

  !> @brief Load a discretization from a binary grid file.
  function load_grb(inunit, iout) result(dis)
    ! dummy
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in), optional :: iout
    class(DisBaseType), pointer :: dis
    ! local
    type(LongLineReaderType) :: line_reader
    character(len=:), allocatable :: line
    integer(I4B) :: liout, lloc, istart, istop, ival, ierr
    real(DP) :: rval
    character(len=10) :: grid
    integer(I4B) :: version, ntxt, lentxt

    if (present(iout)) then
      liout = iout
    else
      liout = 0
    end if

    ! grid type
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    grid = line(istart:istop)
    call upcase(grid)

    ! version
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    version = ival

    ! ntxt
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    ntxt = ival

    ! lentxt
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    lentxt = ival

    ! load grid-specific data
    select case (grid)
    case ('DIS')
      dis => load_dis(inunit, liout)
    case ('DISV')
      dis => load_disv(inunit, liout)
    case ('DISU')
      dis => load_disu(inunit, liout)
    end select

    ! close file
    close (inunit)
  end function load_grb

  function load_dis(inunit, iout) result(dis)
    ! dummy
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in), optional :: iout
    type(DisType), pointer :: dis
    ! local
    type(LongLineReaderType) :: line_reader
    integer(I4B) :: liout

    if (present(iout)) then
      liout = iout
    else
      liout = 0
    end if

    ! 1. TODO read header text

  end function load_dis

  function load_disv(inunit, iout) result(disv)
    ! dummy
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in), optional :: iout
    type(DisvType), pointer :: disv
    ! local
    type(LongLineReaderType) :: line_reader
    character(len=:), allocatable :: line
    integer(I4B) :: liout, dim, lloc, istart, istop, ival, ierr
    real(DP) :: rval, xorigin, yorigin, angrot
    integer(I4B) :: ncells, nlay, ncpl, nvert, njavert, nja
    integer(I4B), allocatable :: shp(:)

    if (present(iout)) then
      liout = iout
    else
      liout = 0
    end if

    ! 1. read header text

    ! ncells integer ndim 0 # <ncells>
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    ncells = ival

    ! nlay integer ndim 0 # <nlay>
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    nlay = ival

    ! ncpl integer ndim 0 # <ncpl>
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    ncpl = ival

    ! nvert integer ndim 0 # <nvert>
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    nvert = ival

    ! njavert integer ndim 0 # <njavert>
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    njavert = ival

    ! nja integer ndim 0 # <nja>
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 2, ival, rval, liout, 0)
    nja = ival

    ! xorigin double ndim 0 # <xorigin>
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 3, ival, rval, liout, 0)
    xorigin = rval

    ! yorigin double ndim 0 # <yorigin>
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 3, ival, rval, liout, 0)
    yorigin = rval

    ! angrot double ndim 0 # <angrot>
    call line_reader%rdcom(inunit, liout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 1, ival, rval, liout, 0)
    call urword(line, lloc, istart, istop, 3, ival, rval, liout, 0)
    angrot = rval

    ! top

    ! botm

    ! vertices

    ! cellx

    ! celly

    ! iavert

    ! javert

    ! ia

    ! ja

    ! idomain

    ! icelltype

    ! 2. allocate
    allocate (disv)
    ! TODO allocate under fmi mempath? "floating" dis
    ! owned by fmi, rather than by a model as normal?

    ! 3. TODO read/populate arrays

  end function load_disv

  function load_disu(inunit, iout) result(disu)
    ! dummy
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in), optional :: iout
    type(DisuType), pointer :: disu
    ! local
    type(LongLineReaderType) :: line_reader
    integer(I4B) :: liout

    if (present(iout)) then
      liout = iout
    else
      liout = 0
    end if

    ! 1. TODO read header text

  end function load_disu

end module GridFileReaderModule

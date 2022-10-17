module Integer1dReaderModule

  use KindModule, only: DP, I4B, LGP
  use BlockParserModule, only: BlockParserType
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_unit
  use ArrayReadersModule, only: read_binary_header
  use ArrayReaderBaseModule, only: ArrayReaderBaseType

  implicit none
  private
  public :: read_int1d

  type, extends(ArrayReaderBaseType) :: Integer1dReaderType

    integer(I4B) :: constant_array_value = 1
    integer(I4B) :: factor = 1
    integer(I4B), dimension(:), contiguous, pointer :: int1d => null()

  contains

    procedure :: set_constant  ! must be overriden
    procedure :: fill_constant  ! must be overriden
    procedure :: read_ascii ! must be overriden
    procedure :: read_binary ! must be overriden
    procedure :: set_factor ! must be overriden
    procedure :: apply_factor ! must be overriden

  end type Integer1dReaderType

  contains

  subroutine read_int1d(parser, int1d, aname)
    ! -- dummy
    type(BlockParserType), intent(in), target :: parser
    integer(I4B), dimension(:), contiguous, target :: int1d
    character(len=*), intent(in) :: aname
    ! -- local
    type(Integer1dReaderType) :: this

    this%parser => parser
    this%int1d => int1d
    this%array_name = aname

    call this%read_array()

  end subroutine read_int1d

  subroutine set_constant(this)
    class(Integer1dReaderType) :: this
    this%constant_array_value = this%parser%GetInteger()
  end subroutine set_constant

  subroutine fill_constant(this)
    class(Integer1dReaderType) :: this
    integer(I4B) :: i
    do i = 1, size(this%int1d)
      this%int1d(i) = this%constant_array_value
    end do
  end subroutine fill_constant

  subroutine read_ascii(this)
    class(Integer1dReaderType) :: this
    integer(I4B) :: i
    integer(I4B) :: nvals
    integer(I4B) :: istat
    nvals = size(this%int1d)
    read (this%input_unit, *, iostat=istat, iomsg=errmsg) (this%int1d(i), i=1, nvals)
    if (istat /= 0) then
      errmsg = 'Error reading data for array ' // trim(this%array_name) // '.  ' // trim(errmsg)
      call store_error(errmsg)
      call store_error_unit(this%input_unit)
    end if
  end subroutine read_ascii

  subroutine read_binary(this)
    class(Integer1dReaderType) :: this
    integer(I4B) :: i
    integer(I4B) :: nvals
    integer(I4B) :: istat
    call read_binary_header(this%input_unit, this%iout, this%array_name, nvals)
    read (this%input_unit, iostat=istat, iomsg=errmsg) (this%int1d(i), i=1, nvals)
    if (istat /= 0) then
      errmsg = 'Error reading data for array ' // trim(this%array_name) // '.  ' // trim(errmsg)
      call store_error(errmsg)
      call store_error_unit(this%input_unit)
    end if
  end subroutine read_binary

  subroutine set_factor(this)
    class(Integer1dReaderType) :: this
    this%factor = this%parser%GetInteger()
  end subroutine set_factor

  subroutine apply_factor(this)
    class(Integer1dReaderType) :: this
    integer(I4B) :: i
    if (this%factor /= 0) then
      do i = 1, size(this%int1d)
        this%int1d(i) = this%int1d(i) * this%factor
      enddo
    end if
  end subroutine apply_factor


end module Integer1dReaderModule
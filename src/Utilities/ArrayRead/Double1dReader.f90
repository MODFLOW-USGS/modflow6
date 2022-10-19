module Double1dReaderModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DONE
  use BlockParserModule, only: BlockParserType
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_unit
  use ArrayReadersModule, only: read_binary_header
  use ArrayReaderBaseModule, only: ArrayReaderBaseType

  implicit none
  private
  public :: read_dbl1d
  public :: read_dbl1d_layered

  type, extends(ArrayReaderBaseType) :: Double1dReaderType

    real(DP) :: constant_array_value = DZERO
    real(DP) :: factor = DONE
    real(DP), dimension(:), contiguous, pointer :: dbl1d => null()

  contains

    procedure :: reset_reader
    procedure :: set_constant ! must be overriden
    procedure :: fill_constant ! must be overriden
    procedure :: read_ascii ! must be overriden
    procedure :: read_binary ! must be overriden
    procedure :: set_factor ! must be overriden
    procedure :: apply_factor ! must be overriden

  end type Double1dReaderType

contains

  subroutine read_dbl1d(parser, dbl1d, aname)
    ! -- dummy
    type(BlockParserType), intent(in), target :: parser
    real(DP), dimension(:), contiguous, target :: dbl1d
    character(len=*), intent(in) :: aname
    ! -- local
    type(Double1dReaderType) :: this

    this%parser => parser
    this%dbl1d => dbl1d
    this%array_name = aname

    call this%read_array()

  end subroutine read_dbl1d

  subroutine read_dbl1d_layered(parser, dbl1d, aname, nlay, layer_shape)
    use Double2dReaderModule, only: read_dbl2d
    ! -- dummy
    type(BlockParserType), intent(in), target :: parser
    real(DP), dimension(:), contiguous, target :: dbl1d
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: nlay
    integer(I4B), dimension(:), intent(in) :: layer_shape
    ! -- local
    integer(I4B) :: k
    integer(I4B) :: ncpl, nrow, ncol
    integer(I4B) :: index_start, index_stop
    real(DP), dimension(:, :), contiguous, pointer :: dbl2d_ptr

    ncpl = product(layer_shape)
    index_start = 1
    do k = 1, nlay
      index_stop = index_start + ncpl - 1
      if (size(layer_shape) == 2) then
        ncol = layer_shape(1)
        nrow = layer_shape(2)
        dbl2d_ptr(1:ncol, 1:nrow) => dbl1d(index_start:index_stop)
        call read_dbl2d(parser, dbl2d_ptr, aname)
      else
        call read_dbl1d(parser, dbl1d(index_start:index_stop), aname)
      end if
      index_start = index_stop + 1
    end do

  end subroutine read_dbl1d_layered

  subroutine reset_reader(this)
    class(Double1dReaderType) :: this
    call this%ArrayReaderBaseType%reset_reader()
    this%constant_array_value = DZERO
    this%factor = DONE
  end subroutine reset_reader

  subroutine set_constant(this)
    class(Double1dReaderType) :: this
    this%constant_array_value = this%parser%GetDouble()
  end subroutine set_constant

  subroutine fill_constant(this)
    class(Double1dReaderType) :: this
    integer(I4B) :: i
    do i = 1, size(this%dbl1d)
      this%dbl1d(i) = this%constant_array_value
    end do
  end subroutine fill_constant

  subroutine read_ascii(this)
    class(Double1dReaderType) :: this
    integer(I4B) :: i
    integer(I4B) :: istat
    read (this%input_unit, *, iostat=istat, iomsg=errmsg) &
      (this%dbl1d(i), i=1, size(this%dbl1d))
    if (istat /= 0) then
      errmsg = 'Error reading data for array '//trim(this%array_name)// &
               '.  '//trim(errmsg)
      call store_error(errmsg)
      call store_error_unit(this%input_unit)
    end if
  end subroutine read_ascii

  subroutine read_binary(this)
    class(Double1dReaderType) :: this
    integer(I4B) :: i
    integer(I4B) :: nvals
    integer(I4B) :: istat
    call read_binary_header(this%input_unit, this%iout, this%array_name, nvals)
    read (this%input_unit, iostat=istat, iomsg=errmsg) &
      (this%dbl1d(i), i=1, size(this%dbl1d))
    if (istat /= 0) then
      errmsg = 'Error reading data for array '//trim(this%array_name)// &
               '.  '//trim(errmsg)
      call store_error(errmsg)
      call store_error_unit(this%input_unit)
    end if
  end subroutine read_binary

  subroutine set_factor(this)
    class(Double1dReaderType) :: this
    this%factor = this%parser%GetDouble()
  end subroutine set_factor

  subroutine apply_factor(this)
    class(Double1dReaderType) :: this
    integer(I4B) :: i
    if (this%factor /= DZERO) then
      do i = 1, size(this%dbl1d)
        this%dbl1d(i) = this%dbl1d(i) * this%factor
      end do
    end if
  end subroutine apply_factor

end module Double1dReaderModule

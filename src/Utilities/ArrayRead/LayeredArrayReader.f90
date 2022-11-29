module LayeredArrayReaderModule

  use KindModule, only: DP, I4B, LGP
  use BlockParserModule, only: BlockParserType
  use Double1dReaderModule, only: read_dbl1d
  use Double2dReaderModule, only: read_dbl2d
  use Integer1dReaderModule, only: read_int1d
  use Integer2dReaderModule, only: read_int2d

  implicit none
  public :: read_dbl1d_layered
  public :: read_dbl2d_layered
  public :: read_dbl3d_layered
  public :: read_int1d_layered
  public :: read_int2d_layered
  public :: read_int3d_layered

contains

  subroutine read_dbl1d_layered(parser, dbl1d, aname, nlay, layer_shape)
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
    real(DP), dimension(:, :), contiguous, pointer :: dbl2d_ptr => null()

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
    nullify (dbl2d_ptr)

  end subroutine read_dbl1d_layered

  subroutine read_dbl2d_layered(parser, dbl2d, aname, nlay, layer_shape)
    ! -- dummy
    type(BlockParserType), intent(in), target :: parser
    real(DP), dimension(:, :), contiguous, target :: dbl2d
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: nlay
    integer(I4B), dimension(:), intent(in) :: layer_shape
    ! -- local
    integer(I4B) :: k
    integer(I4B) :: ncpl
    real(DP), dimension(:), contiguous, pointer :: dbl1d_ptr => null()

    ncpl = layer_shape(1)
    do k = 1, nlay
      dbl1d_ptr(1:ncpl) => dbl2d(1:ncpl, k)
      call read_dbl1d(parser, dbl1d_ptr, aname)
    end do
    nullify (dbl1d_ptr)

  end subroutine read_dbl2d_layered

  subroutine read_dbl3d_layered(parser, dbl3d, aname, nlay, layer_shape)
    ! -- dummy
    type(BlockParserType), intent(in), target :: parser
    real(DP), dimension(:, :, :), contiguous, target :: dbl3d
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: nlay
    integer(I4B), dimension(:), intent(in) :: layer_shape
    ! -- local
    integer(I4B) :: k
    integer(I4B) :: ncol, nrow
    real(DP), dimension(:, :), contiguous, pointer :: dbl2d_ptr => null()

    ncol = layer_shape(1)
    nrow = layer_shape(2)
    do k = 1, nlay
      dbl2d_ptr(1:ncol, 1:nrow) => dbl3d(:, :, k:k)
      call read_dbl2d(parser, dbl2d_ptr, aname)
    end do
    nullify (dbl2d_ptr)

  end subroutine read_dbl3d_layered

  subroutine read_int1d_layered(parser, int1d, aname, nlay, layer_shape)
    ! -- dummy
    type(BlockParserType), intent(in), target :: parser
    integer(I4B), dimension(:), contiguous, target :: int1d
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: nlay
    integer(I4B), dimension(:), intent(in) :: layer_shape
    ! -- local
    integer(I4B) :: k
    integer(I4B) :: ncpl, nrow, ncol
    integer(I4B) :: index_start, index_stop
    integer(I4B), dimension(:, :), contiguous, pointer :: int2d_ptr => null()

    ncpl = product(layer_shape)
    index_start = 1
    do k = 1, nlay
      index_stop = index_start + ncpl - 1
      if (size(layer_shape) == 2) then
        ncol = layer_shape(1)
        nrow = layer_shape(2)
        int2d_ptr(1:ncol, 1:nrow) => int1d(index_start:index_stop)
        call read_int2d(parser, int2d_ptr, aname)
      else
        call read_int1d(parser, int1d(index_start:index_stop), aname)
      end if
      index_start = index_stop + 1
    end do
    nullify (int2d_ptr)

  end subroutine read_int1d_layered

  subroutine read_int2d_layered(parser, int2d, aname, nlay, layer_shape)
    ! -- dummy
    type(BlockParserType), intent(in), target :: parser
    integer(I4B), dimension(:, :), contiguous, target :: int2d
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: nlay
    integer(I4B), dimension(:), intent(in) :: layer_shape
    ! -- local
    integer(I4B) :: k
    integer(I4B) :: ncpl
    integer(I4B), dimension(:), contiguous, pointer :: int1d_ptr => null()

    ncpl = layer_shape(1)
    do k = 1, nlay
      int1d_ptr(1:ncpl) => int2d(1:ncpl, k)
      call read_int1d(parser, int1d_ptr, aname)
    end do
    nullify (int1d_ptr)

  end subroutine read_int2d_layered

  subroutine read_int3d_layered(parser, int3d, aname, nlay, layer_shape)
    ! -- dummy
    type(BlockParserType), intent(in), target :: parser
    integer(I4B), dimension(:, :, :), contiguous, target :: int3d
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: nlay
    integer(I4B), dimension(:), intent(in) :: layer_shape
    ! -- local
    integer(I4B) :: k
    integer(I4B) :: ncol, nrow
    integer(I4B), dimension(:, :), contiguous, pointer :: int2d_ptr => null()

    ncol = layer_shape(1)
    nrow = layer_shape(2)
    do k = 1, nlay
      int2d_ptr(1:ncol, 1:nrow) => int3d(:, :, k:k)
      call read_int2d(parser, int2d_ptr, aname)
    end do
    nullify (int2d_ptr)

  end subroutine read_int3d_layered

end module LayeredArrayReaderModule

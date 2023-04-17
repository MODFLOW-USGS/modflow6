module IndexMapModule
  use KindModule, only: I4B
  use ArrayHandlersModule, only: ConcatArray
  implicit none
  private

  type, public :: IndexMapType
    integer(I4B), dimension(:), pointer, contiguous :: src_idx => null()
    integer(I4B), dimension(:), pointer, contiguous :: tgt_idx => null()
  contains
    procedure :: add => add_map
    procedure :: copy => copy_map
    procedure, private :: add_map
    procedure, private :: copy_map
  end type IndexMapType

  type, public :: IndexMapSgnType
    integer(I4B), dimension(:), pointer, contiguous :: src_idx => null()
    integer(I4B), dimension(:), pointer, contiguous :: tgt_idx => null()
    integer(I4B), dimension(:), pointer, contiguous :: sign => null()
  contains
    procedure :: add => add_signed_map
    procedure :: copy => copy_signed_map
    procedure, private :: add_signed_map
    procedure, private :: copy_signed_map
  end type IndexMapSgnType

contains

  subroutine add_map(this, map)
    class(IndexMapType) :: this
    class(IndexMapType) :: map

    call ConcatArray(this%src_idx, map%src_idx)
    call ConcatArray(this%tgt_idx, map%tgt_idx)

  end subroutine add_map

  subroutine copy_map(this, map)
    class(IndexMapType) :: this
    class(IndexMapType) :: map
    ! local
    integer(I4B) :: i

    allocate (this%src_idx(size(map%src_idx)))
    allocate (this%tgt_idx(size(map%tgt_idx)))
    do i = 1, size(map%src_idx)
      this%src_idx(i) = map%src_idx(i)
    end do
    do i = 1, size(map%tgt_idx)
      this%tgt_idx(i) = map%tgt_idx(i)
    end do

  end subroutine copy_map

  subroutine add_signed_map(this, signed_map)
    class(IndexMapSgnType) :: this
    class(IndexMapSgnType) :: signed_map

    call ConcatArray(this%src_idx, signed_map%src_idx)
    call ConcatArray(this%tgt_idx, signed_map%tgt_idx)
    call ConcatArray(this%sign, signed_map%sign)

  end subroutine add_signed_map

  subroutine copy_signed_map(this, signed_map)
    class(IndexMapSgnType) :: this
    class(IndexMapSgnType) :: signed_map
    ! local
    integer(I4B) :: i

    allocate (this%src_idx(size(signed_map%src_idx)))
    allocate (this%tgt_idx(size(signed_map%tgt_idx)))
    allocate (this%sign(size(signed_map%sign)))
    do i = 1, size(signed_map%src_idx)
      this%src_idx(i) = signed_map%src_idx(i)
    end do
    do i = 1, size(signed_map%tgt_idx)
      this%tgt_idx(i) = signed_map%tgt_idx(i)
    end do
    do i = 1, size(signed_map%sign)
      this%sign(i) = signed_map%sign(i)
    end do

  end subroutine copy_signed_map
end module IndexMapModule

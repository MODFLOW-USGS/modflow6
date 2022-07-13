module VectorIntModule
  use KindModule, only: I4B
  use SimModule, only: ustop
  use ArrayHandlersModule, only: ExpandArray
  implicit none
  private
  public :: VectorInt

  integer(I4B), parameter :: defaultInitialCapacity = 4

  ! This is a dynamic vector type for integers
  type :: VectorInt
    integer(I4B), private, allocatable :: values(:) ! the internal array for storage
    integer(I4B) :: size ! the number of elements (technically this stuff should be unsigned)
    integer(I4B) :: capacity ! the reserved storage
  contains
    procedure, pass(this) :: init ! allocate memory, init size and capacity
    procedure, pass(this) :: push_back ! adds an element at the end of the vector
    procedure, pass(this) :: at ! random access, unsafe, no bounds checking
    procedure, pass(this) :: at_safe ! random access with bounds checking
    procedure, pass(this) :: clear ! empties the vector, leaves memory unchanged
    procedure, pass(this) :: shrink_to_fit ! reduces the allocated memory to fit the actual vector size
    procedure, pass(this) :: destroy ! deletes the memory
    ! private
    procedure, private, pass(this) :: expand
  end type VectorInt

contains ! module routines

  subroutine init(this, capacity)
    class(VectorInt), intent(inout) :: this
    integer(I4B), intent(in), optional :: capacity ! the initial capacity, when given

    if (present(capacity)) then
      this%capacity = capacity
    else
      this%capacity = defaultInitialCapacity
    end if

    allocate (this%values(this%capacity))
    this%size = 0

  end subroutine init

  subroutine push_back(this, newValue)
    class(VectorInt), intent(inout) :: this
    integer(I4B) :: newValue
    ! check capacity
    if (this%size + 1 > this%capacity) then
      call this%expand()
    end if

    this%size = this%size + 1
    this%values(this%size) = newValue

  end subroutine push_back

  function at(this, idx) result(value)
    class(VectorInt), intent(inout) :: this
    integer(I4B), intent(in) :: idx
    integer(I4B) :: value

    value = this%values(idx)

  end function at

  function at_safe(this, idx) result(value)
    class(VectorInt), intent(inout) :: this
    integer(I4B), intent(in) :: idx
    integer(I4B) :: value

    if (idx > this%size) then
      write (*, *) 'VectorInt exception: access out of bounds, index ', idx, &
        ' exceeds actual size (', this%size, ')'
      call ustop()
    end if
    value = this%at(idx)

  end function at_safe

  subroutine clear(this)
    class(VectorInt), intent(inout) :: this

    ! really, this is all there is to it...
    this%size = 0

  end subroutine clear

  subroutine shrink_to_fit(this)
    class(VectorInt), intent(inout) :: this
    ! local
    integer(I4B), allocatable :: tempValues(:)
    integer(I4B) :: i, newSize

    if (this%size == this%capacity) then
      return
    end if

    ! store temp
    newSize = this%size
    allocate (tempValues(newSize))
    do i = 1, newSize
      tempValues(i) = this%values(i)
    end do

    ! reinit
    call this%destroy()
    call this%init(newSize)

    ! copy back
    do i = 1, newSize
      call this%push_back(tempValues(i))
    end do

  end subroutine shrink_to_fit

  subroutine destroy(this)
    class(VectorInt), intent(inout) :: this

    if (allocated(this%values)) then
      deallocate (this%values)
      this%size = 0
      this%capacity = 0
    else
      write (*, *) 'VectorInt exception: cannot delete an unallocated array'
      call ustop()
    end if

  end subroutine destroy

  ! expand the array with the given strategy, at
  ! least by 1
  subroutine expand(this)
    class(VectorInt), intent(inout) :: this
    integer(I4B) :: increment

    ! expansion strategy
    increment = this%capacity / 2 + 1
    call ExpandArray(this%values, increment)
    this%capacity = this%capacity + increment

  end subroutine expand

end module VectorIntModule

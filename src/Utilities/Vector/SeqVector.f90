module SeqVectorModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use VectorBaseModule
  implicit none
  private

  type, public, extends(VectorBaseType) :: SeqVectorType
    integer(I4B) :: size
    real(DP), dimension(:), pointer, contiguous :: array
  contains
    procedure :: create => sqv_create
    procedure :: destroy => sqv_destroy
    procedure :: get_array => sqv_get_array
    procedure :: get_ownership_range => sqv_get_ownership_range
    procedure :: get_size => sqv_get_size
    procedure :: zero_entries => sqv_zero_entries
    procedure :: print => sqv_print
  end type SeqVectorType

contains

  !> @brief Create a sequential vector: the classic MF6 verion
  !<
  subroutine sqv_create(this, n, name, mem_path)
    class(SeqVectorType) :: this !< this vector
    integer(I4B) :: n !< the nr. of elements in the vector
    character(len=*) :: name !< the variable name (for access through memory manager)
    character(len=*) :: mem_path !< memory path for storing the underlying memory items

    this%size = n
    call mem_allocate(this%array, n, name, mem_path)
    call this%zero_entries()

  end subroutine sqv_create

  !> @brief Clean up
  !<
  subroutine sqv_destroy(this)
    class(SeqVectorType) :: this !< this vector

    call mem_deallocate(this%array)

  end subroutine sqv_destroy

  !> @brief Get a pointer to the underlying data array
  !< for this vector
  function sqv_get_array(this) result(array)
    class(SeqVectorType) :: this !< this vector
    real(DP), dimension(:), pointer, contiguous :: array !< the underlying data array for this vector

    array => this%array

  end function sqv_get_array

  subroutine sqv_get_ownership_range(this, start, end)
    class(SeqVectorType) :: this !< this vector
    integer(I4B) :: start !< the index of the first element in the vector
    integer(I4B) :: end !< the index of the last element in the vector

    start = 1
    end = this%size

  end subroutine sqv_get_ownership_range

  function sqv_get_size(this) result(size)
    class(SeqVectorType) :: this !< this vector
    integer(I4B) :: size !< the vector size

    size = this%size

  end function sqv_get_size

  !> @brief set all elements to zero
  !<
  subroutine sqv_zero_entries(this)
    class(SeqVectorType) :: this !< this vector
    ! local
    integer(I4B) :: i

    do i = 1, this%size
      this%array(i) = DZERO
    end do

  end subroutine sqv_zero_entries

  subroutine sqv_print(this)
    class(SeqVectorType) :: this !< this vector

    write(*,*) this%array

  end subroutine sqv_print

end module SeqVectorModule
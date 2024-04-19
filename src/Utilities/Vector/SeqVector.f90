module SeqVectorModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use VectorBaseModule
  implicit none
  private

  type, public, extends(VectorBaseType) :: SeqVectorType
    integer(I4B), private :: size
    real(DP), dimension(:), pointer, contiguous :: array
  contains
    procedure :: create_mm => sqv_create_mm
    procedure :: create => sqv_create
    procedure :: destroy => sqv_destroy
    procedure :: get_array => sqv_get_array
    procedure :: get_ownership_range => sqv_get_ownership_range
    procedure :: get_size => sqv_get_size
    procedure :: get_value_local => sqv_get_value_local
    procedure :: zero_entries => sqv_zero_entries
    procedure :: set_value_local => sqv_set_value_local
    procedure :: axpy => sqv_axpy
    procedure :: norm2 => sqv_norm2
    procedure :: print => sqv_print
  end type SeqVectorType

contains

  !> @brief Create a sequential vector: the classic MF6 version,
  !< storing in the memory manager.
  subroutine sqv_create_mm(this, n, name, mem_path)
    class(SeqVectorType) :: this !< this vector
    integer(I4B) :: n !< the nr. of elements in the vector
    character(len=*) :: name !< the variable name (for access through memory manager)
    character(len=*) :: mem_path !< memory path for storing the underlying memory items

    this%size = n
    this%is_mem_managed = .true.
    call mem_allocate(this%array, n, name, mem_path)
    call this%zero_entries()

  end subroutine sqv_create_mm

  !> @brief Create a sequential vector: the classic MF6 version
  !<
  subroutine sqv_create(this, n)
    class(SeqVectorType) :: this !< this vector
    integer(I4B) :: n !< the nr. of elements in the vector

    this%size = n
    this%is_mem_managed = .false.
    allocate (this%array(n))
    call this%zero_entries()

  end subroutine sqv_create

  !> @brief Clean up
  !<
  subroutine sqv_destroy(this)
    class(SeqVectorType) :: this !< this vector

    if (this%is_mem_managed) then
      call mem_deallocate(this%array)
    else
      deallocate (this%array)
    end if

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

  !> @brief Get value at local index
  !<
  function sqv_get_value_local(this, idx) result(val)
    class(SeqVectorType) :: this !< this vector
    integer(I4B) :: idx !< the index in local numbering
    real(DP) :: val !< the value

    val = this%array(idx)

  end function sqv_get_value_local

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

  !> @brief Set vector value at local index
  !<
  subroutine sqv_set_value_local(this, idx, val)
    class(SeqVectorType) :: this !< this vector
    integer(I4B) :: idx !< the index in local numbering
    real(DP) :: val !< the value to set

    this%array(idx) = val

  end subroutine sqv_set_value_local

  !> @brief Caculcates AXPY: y = a*x + y
  !<
  subroutine sqv_axpy(this, alpha, vec_x)
    class(SeqVectorType) :: this !< this vector
    real(DP) :: alpha !< the factor
    class(VectorBaseType), pointer :: vec_x !< the vector to add
    ! local
    integer(I4B) :: i
    real(DP), dimension(:), pointer, contiguous :: x_array

    x_array => vec_x%get_array()
    do i = 1, this%size
      this%array(i) = alpha * x_array(i) + this%array(i)
    end do

  end subroutine sqv_axpy

  !> @brief Calculate the 2-norm
  !<
  function sqv_norm2(this) result(n2)
    class(SeqVectorType) :: this !< this vector
    real(DP) :: n2
    ! local
    integer(I4B) :: i

    n2 = DZERO
    do i = 1, this%size
      n2 = n2 + this%array(i)**2
    end do
    n2 = sqrt(n2)

  end function sqv_norm2

  subroutine sqv_print(this)
    class(SeqVectorType) :: this !< this vector

    write (*, *) this%array

  end subroutine sqv_print

end module SeqVectorModule

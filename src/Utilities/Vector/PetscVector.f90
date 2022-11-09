module PetscVectorModule
  use VectorBaseModule
  implicit none
  private

  type, extends(VectorBaseType) :: PetscVectorType
  contains
    procedure :: create => petsc_vec_create
    procedure :: destroy => petsc_vec_destroy
    procedure :: get_array => petsc_vec_get_array
    procedure :: zero_entries => petsc_vec_zero_entries
  end type PetscVectorType

contains


  !> @brief Create a PETSc vector
  !<
  subroutine petsc_vec_create(this, n, name, mem_path)
    class(PetscVectorType) :: this !< this vector
    integer(I4B) :: n !< the nr. of elements in the vector
    character(len=*) :: name !< the variable name (for access through memory manager)
    character(len=*) :: mem_path !< memory path for storing the underlying memory items

    call this%zero_entries()

  end subroutine petsc_vec_create

  !> @brief Clean up
  !<
  subroutine petsc_vec_destroy(this)
    class(PetscVectorType) :: this !< this vector

  end subroutine petsc_vec_destroy

  !> @brief Get a pointer to the underlying data array
  !< for this vector
  function petsc_vec_get_array(this) result(array)
    class(PetscVectorType) :: this !< this vector
    real(DP), dimension(:), pointer, contiguous :: array !< the underlying data array for this vector

  end function petsc_vec_get_array

  !> @brief set all elements to zero
  !<
  subroutine petsc_vec_zero_entries(this)
    class(PetscVectorType) :: this !< this vector

  end subroutine petsc_vec_zero_entries

end module PetscVectorModule
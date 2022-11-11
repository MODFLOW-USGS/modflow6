module PetscVectorModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use VectorBaseModule
  use KindModule, only: I4B, DP
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  implicit none
  private

  type, public, extends(VectorBaseType) :: PetscVectorType
    real(DP), dimension(:), pointer, contiguous :: array => null()
    Vec :: vec_impl
  contains
    procedure :: create => petsc_vec_create
    procedure :: destroy => petsc_vec_destroy
    procedure :: get_array => petsc_vec_get_array
    procedure :: zero_entries => petsc_vec_zero_entries
    procedure :: print => petsc_vec_print
  end type PetscVectorType

contains


  !> @brief Create a PETSc vector
  !<
  subroutine petsc_vec_create(this, n, name, mem_path)
    class(PetscVectorType) :: this !< this vector
    integer(I4B) :: n !< the nr. of elements in the vector
    character(len=*) :: name !< the variable name (for access through memory manager)
    character(len=*) :: mem_path !< memory path for storing the underlying memory items
    ! local
    PetscErrorCode :: ierr
 
    call mem_allocate(this%array, n, name, mem_path)
    call VecCreateSeqWithArray(PETSC_COMM_WORLD, 1, n, this%array, this%vec_impl, ierr)
    CHKERRQ(ierr)
    call this%zero_entries()

  end subroutine petsc_vec_create

  !> @brief Clean up
  !<
  subroutine petsc_vec_destroy(this)
    class(PetscVectorType) :: this !< this vector
    ! local
    PetscErrorCode :: ierr

    call VecDestroy(this%vec_impl, ierr)
    CHKERRQ(ierr)
    call mem_deallocate(this%array)

  end subroutine petsc_vec_destroy

  !> @brief Get a pointer to the underlying data array
  !< for this vector
  function petsc_vec_get_array(this) result(array)
    class(PetscVectorType) :: this !< this vector
    real(DP), dimension(:), pointer, contiguous :: array !< the underlying data array for this vector

    array => this%array

  end function petsc_vec_get_array

  !> @brief set all elements to zero
  !<
  subroutine petsc_vec_zero_entries(this)
    class(PetscVectorType) :: this !< this vector
    ! local
    PetscErrorCode :: ierr

    call VecZeroEntries(this%vec_impl, ierr)
    CHKERRQ(ierr)

  end subroutine petsc_vec_zero_entries

  subroutine petsc_vec_print(this)
    class(PetscVectorType) :: this !< this vector
    ! local
    PetscErrorCode :: ierr
    
    call VecView(this%vec_impl, PETSC_VIEWER_STDOUT_WORLD, ierr)
    CHKERRQ(ierr)

  end subroutine petsc_vec_print

end module PetscVectorModule
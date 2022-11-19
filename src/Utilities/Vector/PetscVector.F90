module PetscVectorModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use VectorBaseModule
  use KindModule, only: I4B, DP
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use SimVariablesModule, only: simulation_mode, num_ranks
  implicit none
  private

  type, public, extends(VectorBaseType) :: PetscVectorType
    real(DP), dimension(:), pointer, contiguous :: array => null()
    Vec :: vec_impl
  contains
    ! override
    procedure :: create => petsc_vec_create
    procedure :: destroy => petsc_vec_destroy
    procedure :: get_array => petsc_vec_get_array
    procedure :: get_ownership_range => petsc_vec_get_ownership_range
    procedure :: get_size => petsc_vec_get_size
    procedure :: zero_entries => petsc_vec_zero_entries
    procedure :: print => petsc_vec_print

    ! public
    procedure :: update => petsc_vec_update
    procedure :: reset => petsc_vec_reset
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
    if (simulation_mode == 'PARALLEL' .and. num_ranks > 1) then
      call VecCreateMPIWithArray(PETSC_COMM_WORLD, 1, n, PETSC_DECIDE, this%array, this%vec_impl, ierr)
    else
      call VecCreateSeqWithArray(PETSC_COMM_WORLD, 1, n, this%array, this%vec_impl, ierr)
    end if
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

  !> @brief Update state
  !<
  subroutine petsc_vec_update(this)
    class(PetscVectorType) :: this !< this vector
    ! local
    PetscErrorCode :: ierr

    call VecPlaceArray(this%vec_impl, this%array, ierr)
    CHKERRQ(ierr)

  end subroutine petsc_vec_update

  !> @brief Release arrays
  !<
  subroutine petsc_vec_reset(this)
    class(PetscVectorType) :: this !< this vector
    ! local
    PetscErrorCode :: ierr

    call VecResetArray(this%vec_impl, ierr)
    CHKERRQ(ierr)

  end subroutine petsc_vec_reset

  !> @brief Get a pointer to the underlying data array
  !< for this vector
  function petsc_vec_get_array(this) result(array)
    class(PetscVectorType) :: this !< this vector
    real(DP), dimension(:), pointer, contiguous :: array !< the underlying data array for this vector

    array => this%array

  end function petsc_vec_get_array

  subroutine petsc_vec_get_ownership_range(this, start, end)
    class(PetscVectorType) :: this !< this vector
    integer(I4B) :: start !< the index of the first element (owned by this process) in the global vector
    integer(I4B) :: end !< the index of the last element (owned by this process) in the global vector
    ! local
    PetscErrorCode :: ierr

    ! gets the range as [start, end) but 0-based
    call VecGetOwnershipRange(this%vec_impl, start, end, ierr)

    ! now we make it [start, end] and 1-based
    start = start + 1

    CHKERRQ(ierr)

  end subroutine petsc_vec_get_ownership_range

  function petsc_vec_get_size(this) result(size)
    class(PetscVectorType) :: this !< this vector
    integer(I4B) :: size !< the (global) vector size
    ! local
    PetscErrorCode :: ierr

    call VecGetSize(this%vec_impl, size, ierr)
    CHKERRQ(ierr)

  end function petsc_vec_get_size

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
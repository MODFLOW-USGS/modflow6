module PetscVectorModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use VectorBaseModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use SimVariablesModule, only: simulation_mode, nr_procs
  implicit none
  private

  type, public, extends(VectorBaseType) :: PetscVectorType
    real(DP), dimension(:), pointer, contiguous :: array => null()
    Vec :: vec_impl
  contains
    ! override
    procedure :: create_mm => petsc_vec_create_mm
    procedure :: create => petsc_vec_create
    procedure :: destroy => petsc_vec_destroy
    procedure :: get_array => petsc_vec_get_array
    procedure :: get_ownership_range => petsc_vec_get_ownership_range
    procedure :: get_size => petsc_vec_get_size
    procedure :: get_value_local => petsc_vec_get_value_local
    procedure :: zero_entries => petsc_vec_zero_entries
    procedure :: set_value_local => petsc_vec_set_value_local
    procedure :: axpy => petsc_vec_axpy
    procedure :: norm2 => petsc_vec_norm2
    procedure :: print => petsc_vec_print
  end type PetscVectorType

contains

  !> @brief Create a PETSc vector, with memory
  !< in the memory manager
  subroutine petsc_vec_create_mm(this, n, name, mem_path)
    class(PetscVectorType) :: this !< this vector
    integer(I4B) :: n !< the nr. of elements in the vector
    character(len=*) :: name !< the variable name (for access through memory manager)
    character(len=*) :: mem_path !< memory path for storing the underlying memory items
    ! local
    PetscErrorCode :: ierr

    this%is_mem_managed = .true.

    call mem_allocate(this%array, n, name, mem_path)
    if (simulation_mode == 'PARALLEL' .and. nr_procs > 1) then
      call VecCreateMPIWithArray(PETSC_COMM_WORLD, 1, n, PETSC_DECIDE, &
                                 this%array, this%vec_impl, ierr)
    else
      call VecCreateSeqWithArray(PETSC_COMM_WORLD, 1, n, this%array, &
                                 this%vec_impl, ierr)
    end if
    CHKERRQ(ierr)

    call this%zero_entries()

  end subroutine petsc_vec_create_mm

  !> @brief Create a PETSc vector, with memory
  !< in the memory manager
  subroutine petsc_vec_create(this, n)
    class(PetscVectorType) :: this !< this vector
    integer(I4B) :: n !< the nr. of elements in the vector
    ! local
    PetscErrorCode :: ierr

    this%is_mem_managed = .false.

    allocate (this%array(n))
    if (simulation_mode == 'PARALLEL' .and. nr_procs > 1) then
      call VecCreateMPIWithArray(PETSC_COMM_WORLD, 1, n, PETSC_DECIDE, &
                                 this%array, this%vec_impl, ierr)
    else
      call VecCreateSeqWithArray(PETSC_COMM_WORLD, 1, n, this%array, &
                                 this%vec_impl, ierr)
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
    if (this%is_mem_managed) then
      call mem_deallocate(this%array)
    else
      deallocate (this%array)
    end if

  end subroutine petsc_vec_destroy

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

  !> @brief Gets a value from the vector at the local index
  !<
  function petsc_vec_get_value_local(this, idx) result(val)
    class(PetscVectorType) :: this !< this vector
    integer(I4B) :: idx !< the index in local numbering
    real(DP) :: val !< the value

    val = this%array(idx)

  end function petsc_vec_get_value_local

  !> @brief set all elements to zero
  !<
  subroutine petsc_vec_zero_entries(this)
    class(PetscVectorType) :: this !< this vector
    ! local
    PetscErrorCode :: ierr

    call VecZeroEntries(this%vec_impl, ierr)
    CHKERRQ(ierr)
    call VecAssemblyBegin(this%vec_impl, ierr)
    CHKERRQ(ierr)
    call VecAssemblyEnd(this%vec_impl, ierr)
    CHKERRQ(ierr)

  end subroutine petsc_vec_zero_entries

  !> @brief Set vector value at local index
  !<
  subroutine petsc_vec_set_value_local(this, idx, val)
    class(PetscVectorType) :: this !< this vector
    integer(I4B) :: idx !< the index in local numbering
    real(DP) :: val !< the value to set

    this%array(idx) = val

  end subroutine petsc_vec_set_value_local

  !> @brief Calculate AXPY: y = a*x + y
  !<
  subroutine petsc_vec_axpy(this, alpha, vec_x)
    class(PetscVectorType) :: this !< this vector
    real(DP) :: alpha !< the factor
    class(VectorBaseType), pointer :: vec_x !< the vector to add
    ! local
    PetscErrorCode :: ierr
    class(PetscVectorType), pointer :: x

    x => null()
    select type (vec_x)
    class is (PetscVectorType)
      x => vec_x
    end select
    call VecAxpy(this%vec_impl, alpha, x%vec_impl, ierr)
    CHKERRQ(ierr)

  end subroutine petsc_vec_axpy

  !> @brief Calculate this vector's (global) 2-norm
  !<
  function petsc_vec_norm2(this) result(n2)
    class(PetscVectorType) :: this !< this vector
    real(DP) :: n2 !< the calculated 2-norm
    ! local
    PetscErrorCode :: ierr

    call VecNorm(this%vec_impl, NORM_2, n2, ierr)
    CHKERRQ(ierr)

  end function petsc_vec_norm2

  subroutine petsc_vec_print(this)
    class(PetscVectorType) :: this !< this vector
    ! local
    PetscErrorCode :: ierr

    call VecView(this%vec_impl, PETSC_VIEWER_STDOUT_WORLD, ierr)
    CHKERRQ(ierr)

  end subroutine petsc_vec_print

end module PetscVectorModule

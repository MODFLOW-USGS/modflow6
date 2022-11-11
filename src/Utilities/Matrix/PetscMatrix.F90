module PetscMatrixModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use KindModule, only: I4B, DP
  use SparseModule, only: sparsematrix
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use MatrixBaseModule
  use SparseMatrixModule
  use VectorBaseModule
  use PetscVectorModule
  implicit none
  private

  type, public, extends(MatrixBaseType) :: PetscMatrixType
    Mat :: petsc_mat_impl
    integer(I4B), dimension(:), pointer, contiguous :: ia_petsc
    integer(I4B), dimension(:), pointer, contiguous :: ja_petsc
    class(SparseMatrixType), pointer :: spm_impl
  contains
    procedure :: init => petsc_mat_init
    procedure :: destroy => petsc_mat_destroy
    procedure :: create_vector => petsc_mat_create_vector

    procedure :: get_value_pos => petsc_mat_get_value_pos
    procedure :: get_diag_value => petsc_mat_get_diag_value

    procedure :: set_diag_value => petsc_mat_set_diag_value
    procedure :: set_value_pos => petsc_mat_set_value_pos
    procedure :: add_value_pos => petsc_mat_add_value_pos
    procedure :: add_diag_value => petsc_mat_add_diag_value
    procedure :: zero_entries => petsc_mat_zero_entries
    procedure :: zero_row_offdiag => petsc_mat_zero_row_offdiag

    procedure :: get_first_col_pos => petsc_mat_get_first_col_pos
    procedure :: get_last_col_pos => petsc_mat_get_last_col_pos
    procedure :: get_column => petsc_mat_get_column
    procedure :: get_position => petsc_mat_get_position
    procedure :: get_position_diag => petsc_mat_get_position_diag

    procedure :: get_aij => petsc_mat_get_aij

  end type PetscMatrixType

contains

  subroutine petsc_mat_init(this, sparse, mem_path)
    class(PetscMatrixType) :: this
    type(sparsematrix) :: sparse
    character(len=*) :: mem_path
    ! local
    PetscErrorCode :: ierr

    this%memory_path = mem_path

    allocate (this%spm_impl)
    call this%spm_impl%init(sparse, mem_path)

    allocate (this%ia_petsc(size(this%spm_impl%ia)))
    allocate (this%ja_petsc(size(this%spm_impl%ja)))
    this%ia_petsc = this%spm_impl%ia - 1
    this%ja_petsc = this%spm_impl%ja - 1

    call MatCreateSeqAIJWithArrays(PETSC_COMM_WORLD, &
                                   this%spm_impl%nrow, this%spm_impl%ncol, &
                                   this%ia_petsc, this%ja_petsc, &
                                   this%spm_impl%amat, this%petsc_mat_impl, &
                                   ierr)
    CHKERRQ(ierr)

  end subroutine petsc_mat_init

  subroutine petsc_mat_destroy(this)
    class(PetscMatrixType) :: this
    ! local
    PetscErrorCode :: ierr

    call this%spm_impl%destroy()
    deallocate (this%spm_impl)

    deallocate (this%ia_petsc)
    deallocate (this%ja_petsc)

    call MatDestroy(this%petsc_mat_impl, ierr)
    CHKERRQ(ierr)

  end subroutine petsc_mat_destroy

  function petsc_mat_create_vector(this, n, name, mem_path) result(vec)
    class(PetscMatrixType) :: this
    integer(I4B) :: n !< the nr. of elements in the vector
    character(len=*) :: name !< the variable name (for access through memory manager)
    character(len=*) :: mem_path !< memory path for storing the underlying memory items
    class(VectorBaseType), pointer :: vec !< the vector object to return
    ! local
    class(PetscVectorType), pointer :: petsc_vec

    allocate (petsc_vec)
    call petsc_vec%create(n, name, mem_path)
    vec => petsc_vec

  end function petsc_mat_create_vector

  function petsc_mat_get_value_pos(this, ipos) result(value)
    class(PetscMatrixType) :: this
    integer(I4B) :: ipos
    real(DP) :: value

    value = this%spm_impl%get_value_pos(ipos)

  end function petsc_mat_get_value_pos

  function petsc_mat_get_diag_value(this, irow) result(diag_value)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    real(DP) :: diag_value

    diag_value = this%spm_impl%get_diag_value(irow)

  end function petsc_mat_get_diag_value

  subroutine petsc_mat_set_diag_value(this, irow, diag_value)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    real(DP) :: diag_value

    call this%spm_impl%set_diag_value(irow, diag_value)

  end subroutine petsc_mat_set_diag_value

  subroutine petsc_mat_set_value_pos(this, ipos, value)
    class(PetscMatrixType) :: this
    integer(I4B) :: ipos
    real(DP) :: value

    call this%spm_impl%set_value_pos(ipos, value)

  end subroutine petsc_mat_set_value_pos

  subroutine petsc_mat_add_value_pos(this, ipos, value)
    class(PetscMatrixType) :: this
    integer(I4B) :: ipos
    real(DP) :: value

    call this%spm_impl%add_value_pos(ipos, value)

  end subroutine petsc_mat_add_value_pos

  subroutine petsc_mat_add_diag_value(this, irow, value)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    real(DP) :: value

    call this%spm_impl%add_diag_value(irow, value)

  end subroutine petsc_mat_add_diag_value

  function petsc_mat_get_first_col_pos(this, irow) result(first_col_pos)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    integer(I4B) :: first_col_pos

    first_col_pos = this%spm_impl%get_first_col_pos(irow)

  end function petsc_mat_get_first_col_pos

  function petsc_mat_get_last_col_pos(this, irow) result(last_col_pos)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    integer(I4B) :: last_col_pos

    last_col_pos = this%spm_impl%get_last_col_pos(irow)

  end function petsc_mat_get_last_col_pos

  function petsc_mat_get_column(this, ipos) result(icol)
    class(PetscMatrixType) :: this
    integer(I4B) :: ipos
    integer(I4B) :: icol

    icol = this%spm_impl%get_column(ipos)

  end function petsc_mat_get_column

  !> @brief Return position index for (irow,icol) element
  !! in the matrix. This index can be used in other
  !! routines for direct access.
  !< Returns -1 when not found.
  function petsc_mat_get_position(this, irow, icol) result(ipos)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    integer(I4B) :: icol
    integer(I4B) :: ipos

    ipos = this%spm_impl%get_position(irow, icol)

  end function petsc_mat_get_position

  function petsc_mat_get_position_diag(this, irow) result(ipos_diag)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    integer(I4B) :: ipos_diag

    ipos_diag = this%spm_impl%get_position_diag(irow)

  end function petsc_mat_get_position_diag

  !> @brief Set all entries in the matrix to zero
  !<
  subroutine petsc_mat_zero_entries(this)
    class(PetscMatrixType) :: this

    call this%spm_impl%zero_entries()

  end subroutine petsc_mat_zero_entries

  !> @brief Set all off-diagonal entries in the matrix to zero
  !<
  subroutine petsc_mat_zero_row_offdiag(this, irow)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow

    call this%spm_impl%zero_row_offdiag(irow)

  end subroutine petsc_mat_zero_row_offdiag

  subroutine petsc_mat_get_aij(this, ia, ja, amat)
    class(PetscMatrixType) :: this
    integer(I4B), dimension(:), pointer, contiguous :: ia
    integer(I4B), dimension(:), pointer, contiguous :: ja
    real(DP), dimension(:), pointer, contiguous :: amat

    ia => this%spm_impl%ia
    ja => this%spm_impl%ja
    amat => this%spm_impl%amat

  end subroutine petsc_mat_get_aij

end module PetscMatrixModule
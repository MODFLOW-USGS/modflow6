module PetscMatrixModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO
  use SparseModule, only: sparsematrix
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use MatrixBaseModule
  use VectorBaseModule
  use PetscVectorModule
  implicit none
  private

  type, public, extends(MatrixBaseType) :: PetscMatrixType
    Mat :: mat
    integer(I4B), dimension(:), pointer, contiguous :: ia_petsc !< IA(CSR) for petsc, contains 0-based index values
    integer(I4B), dimension(:), pointer, contiguous :: ja_petsc !< JA(CSR) for petsc, contains 0-based index values
    real(DP), dimension(:), pointer, contiguous :: amat_petsc

    integer(I4B), dimension(:), pointer, contiguous :: ia_csr !< temp: until IMS and PETSc are properly separated (TODO_MJR)
    integer(I4B), dimension(:), pointer, contiguous :: ja_csr !< temp: until IMS and PETSc are properly separated (TODO_MJR)
  contains
    ! override
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

    ! private
    procedure, private :: petsc_mat_get_position

  end type PetscMatrixType

contains

  subroutine petsc_mat_init(this, sparse, mem_path)
    class(PetscMatrixType) :: this
    type(sparsematrix) :: sparse
    character(len=*) :: mem_path
    ! local
    PetscErrorCode :: ierr
    integer(I4B) :: i, ierror

    this%memory_path = mem_path

    allocate (this%ia_petsc(sparse%nrow + 1))
    allocate (this%ja_petsc(sparse%nnz))
    allocate (this%amat_petsc(sparse%nnz))

    ! temp:
    allocate (this%ia_csr(sparse%nrow + 1))
    allocate (this%ja_csr(sparse%nnz))

    call sparse%sort(with_csr = .true.) !< PETSc has full row sorting, MF6 had diagonal first and then sorted
    call sparse%filliaja(this%ia_petsc, this%ja_petsc, ierror)

    ! go to C indexing for PETSc internals
    do i = 1, sparse%nrow + 1
      this%ia_csr(i) = this%ia_petsc(i)
      this%ia_petsc(i) = this%ia_petsc(i) - 1
    end do
    do i = 1, sparse%nnz
      this%ja_csr(i) = this%ja_petsc(i)
      this%ja_petsc(i) = this%ja_petsc(i) - 1
    end do

    ! create PETSc matrix object
    call MatCreateSeqAIJWithArrays(PETSC_COMM_WORLD, &
                                   sparse%nrow, sparse%ncol, &
                                   this%ia_petsc, this%ja_petsc, &
                                   this%amat_petsc, this%mat, &
                                   ierr)
    CHKERRQ(ierr)

    call MatZeroEntries(this%mat, ierr)
    CHKERRQ(ierr)

  end subroutine petsc_mat_init

  subroutine petsc_mat_destroy(this)
    class(PetscMatrixType) :: this
    ! local
    PetscErrorCode :: ierr

    call MatDestroy(this%mat, ierr)
    CHKERRQ(ierr)

    deallocate (this%ia_petsc)
    deallocate (this%ja_petsc)
    deallocate (this%amat_petsc)

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

    value = this%amat_petsc(ipos)

  end function petsc_mat_get_value_pos

  function petsc_mat_get_diag_value(this, irow) result(diag_value)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    real(DP) :: diag_value
    ! local
    integer(I4B) :: idiag

    idiag = this%get_position_diag(irow)
    diag_value = this%amat_petsc(idiag)

  end function petsc_mat_get_diag_value

  subroutine petsc_mat_set_diag_value(this, irow, diag_value)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    real(DP) :: diag_value
    ! local
    integer(I4B) :: idiag

    idiag = this%get_position_diag(irow)
    this%amat_petsc(idiag) = diag_value

  end subroutine petsc_mat_set_diag_value

  subroutine petsc_mat_set_value_pos(this, ipos, value)
    class(PetscMatrixType) :: this
    integer(I4B) :: ipos
    real(DP) :: value

    this%amat_petsc(ipos) = value

  end subroutine petsc_mat_set_value_pos

  subroutine petsc_mat_add_value_pos(this, ipos, value)
    class(PetscMatrixType) :: this
    integer(I4B) :: ipos
    real(DP) :: value

    this%amat_petsc(ipos) = this%amat_petsc(ipos) + value

  end subroutine petsc_mat_add_value_pos

  subroutine petsc_mat_add_diag_value(this, irow, value)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    real(DP) :: value
    ! local
    integer(I4B) :: idiag

    idiag = this%get_position_diag(irow)
    this%amat_petsc(idiag) = this%amat_petsc(idiag) + value

  end subroutine petsc_mat_add_diag_value

  function petsc_mat_get_first_col_pos(this, irow) result(first_col_pos)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    integer(I4B) :: first_col_pos

    ! includes conversion to Fortran's 1-based
    first_col_pos = this%ia_petsc(irow) + 1

  end function petsc_mat_get_first_col_pos

  function petsc_mat_get_last_col_pos(this, irow) result(last_col_pos)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    integer(I4B) :: last_col_pos

    ! includes conversion to Fortran's 1-based
    last_col_pos = this%ia_petsc(irow + 1)

  end function petsc_mat_get_last_col_pos

  function petsc_mat_get_column(this, ipos) result(icol)
    class(PetscMatrixType) :: this
    integer(I4B) :: ipos
    integer(I4B) :: icol

    ! includes conversion to Fortran's 1-based
    icol = this%ja_petsc(ipos) + 1

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
    ! local
    integer(I4B) :: ipos_f

    ipos = -1

    ! includes conversion to Fortran's 1-based
    do ipos_f = this%ia_petsc(irow) + 1, this%ia_petsc(irow + 1)
      if (this%ja_petsc(ipos_f) + 1 == icol) then
        ipos = ipos_f
        return
      end if
    end do

  end function petsc_mat_get_position

  function petsc_mat_get_position_diag(this, irow) result(ipos_diag)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    integer(I4B) :: ipos_diag

    ipos_diag = this%petsc_mat_get_position(irow, irow)

  end function petsc_mat_get_position_diag

  !> @brief Set all entries in the matrix to zero
  !<
  subroutine petsc_mat_zero_entries(this)
    class(PetscMatrixType) :: this
    ! local
    PetscErrorCode :: ierr

    call MatZeroEntries(this%mat, ierr)
    CHKERRQ(ierr)    

  end subroutine petsc_mat_zero_entries

  !> @brief Set all off-diagonal entries in the matrix to zero
  !<
  subroutine petsc_mat_zero_row_offdiag(this, irow)
    class(PetscMatrixType) :: this
    integer(I4B) :: irow
    ! local
    integer(I4B) :: ipos, idiag

    idiag = this%get_position_diag(irow)
    do ipos = this%ia_petsc(irow) + 1, this%ia_petsc(irow + 1)
      if (ipos == idiag) cycle
      this%amat_petsc(ipos) = DZERO
    end do

  end subroutine petsc_mat_zero_row_offdiag

  subroutine petsc_mat_get_aij(this, ia, ja, amat)
    class(PetscMatrixType) :: this
    integer(I4B), dimension(:), pointer, contiguous :: ia
    integer(I4B), dimension(:), pointer, contiguous :: ja
    real(DP), dimension(:), pointer, contiguous :: amat
    
    ia => this%ia_csr
    ja => this%ja_csr
    amat => this%amat_petsc

  end subroutine petsc_mat_get_aij

end module PetscMatrixModule
module PetscImsPreconditionerModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENMEMPATH, DZERO, IZERO
  use PetscMatrixModule, only: PetscMatrixType
  use SparseMatrixModule, only: SparseMatrixType

  implicit none
  private

  public :: PcShellCtxType
  public :: pcshell_apply
  public :: pcshell_setup

  type :: PcShellCtxType
    character(len=LENMEMPATH) :: memory_path !< the memory path, taken from the system matrix
    class(PetscMatrixType), pointer :: system_matrix !< pointer to the petsc system matrix
    class(SparseMatrixType), pointer :: pc_matrix !< the preconditioner matrix in IMS compatible format
    integer(I4B), dimension(:), contiguous, pointer :: IW => null() !< integer work array
    real(DP), dimension(:), contiguous, pointer :: W => null() !< double precision work array
    real(DP) :: relax !< the MILU(T) relaxation factor
  contains
    procedure :: create => pctx_create
    procedure :: destroy => pctx_destroy
  end type

  interface
    subroutine PCShellGetContext(pc, ctx, ierr)
      import PcShellCtxType, tPC
      type(tPC) :: pc
      type(PcShellCtxType), pointer :: ctx
      integer :: ierr
    end subroutine
  end interface

contains

  !> @brief Create the preconditioner context from the system matrix
  !<
  subroutine pctx_create(this, matrix, relax_fct)
    use IMSLinearBaseModule, only: ims_base_pccrs
    use MemoryManagerModule, only: mem_allocate
    class(PcShellCtxType) :: this !< this instance
    class(PetscMatrixType), pointer :: matrix !< the linear system matrix
    real(DP) :: relax_fct !< the MILU(T) relaxation factor
    ! local
    integer(I4B) :: n, neq, nja
    integer(I4B), dimension(:), contiguous, pointer :: ia, ja
    real(DP), dimension(:), contiguous, pointer :: amat

    this%memory_path = matrix%memory_path
    this%relax = relax_fct

    this%system_matrix => matrix
    call matrix%get_aij_local(ia, ja, amat)

    neq = matrix%nrow
    nja = matrix%nnz_local
    allocate (this%pc_matrix)
    call this%pc_matrix%spm_init_empty(neq, nja, this%memory_path)

    call ims_base_pccrs(neq, nja, ia, ja, this%pc_matrix%ia, this%pc_matrix%ja)

    call mem_allocate(this%IW, matrix%nrow, 'IW', this%memory_path)
    call mem_allocate(this%W, matrix%nrow, 'W', this%memory_path)

    do n = 1, neq
      this%IW(n) = IZERO
      this%W(n) = DZERO
    end do

  end subroutine pctx_create

  !> @brief Cleanup the context
  !<
  subroutine pctx_destroy(this)
    use MemoryManagerModule, only: mem_deallocate
    class(PcShellCtxType) :: this

    call mem_deallocate(this%IW)
    call mem_deallocate(this%W)

    call this%pc_matrix%destroy()
    deallocate (this%pc_matrix)

    this%system_matrix => null()
    this%pc_matrix => null()

  end subroutine pctx_destroy

  !> @brief Apply shell preconditioner
  !< (this is not a type bound procedure)
  subroutine pcshell_apply(pc, x, y, ierr)
    use IMSLinearBaseModule, only: ims_base_ilu0a
    PC :: pc !< the shell preconditioner
    Vec :: x !< the input vector
    Vec :: y !< the output vector
    PetscErrorCode :: ierr !< PETSc error code
    ! local
    type(PcShellCtxType), pointer :: pc_ctx => null()
    class(SparseMatrixType), pointer :: pmat => null()
    real(DP), dimension(:), pointer :: local_x, local_y

    ! this applies an example jacobi pc,
    ! to be replaced by others (MILUT)
    call PCShellGetContext(pc, pc_ctx, ierr)
    CHKERRQ(ierr)

    call VecGetArrayReadF90(x, local_x, ierr)
    CHKERRQ(ierr)
    call VecGetArrayF90(y, local_y, ierr)
    CHKERRQ(ierr)

    pmat => pc_ctx%pc_matrix
    call ims_base_ilu0a(pmat%nja, pmat%nrow, pmat%amat, pmat%ia, pmat%ja, &
                        local_x, local_y)

    call VecRestoreArrayF90(x, local_x, ierr)
    CHKERRQ(ierr)
    call VecRestoreArrayF90(y, local_y, ierr)
    CHKERRQ(ierr)

  end subroutine pcshell_apply

  !> @brief Set up the custom preconditioner
  !< (this is not a type bound procedure)
  subroutine pcshell_setup(pc, ierr)
    use IMSLinearBaseModule, only: ims_base_pcilu0
    PC :: pc !< the shell preconditioner
    PetscErrorCode :: ierr !< PETSc error code
    ! local
    type(PcShellCtxType), pointer :: pc_ctx => null()
    class(SparseMatrixType), pointer :: pmat => null()
    integer(I4B) :: ipcflag = 0
    real(DP) :: delta
    integer(I4B), dimension(:), contiguous, pointer :: ia, ja
    real(DP), dimension(:), contiguous, pointer :: amat

    call PCShellGetContext(pc, pc_ctx, ierr)
    CHKERRQ(ierr)

    ! note the two different matrix types here:
    ! bridging between PETSc and IMS
    call pc_ctx%system_matrix%get_aij_local(ia, ja, amat)
    pmat => pc_ctx%pc_matrix
    ipcflag = 0
    delta = 0.0
    call ims_base_pcilu0(pc_ctx%system_matrix%nnz_local, &
                         pc_ctx%system_matrix%nrow, &
                         amat, ia, ja, &
                         pmat%amat, pmat%ia, pmat%ja, &
                         pc_ctx%IW, pc_ctx%W, pc_ctx%relax, &
                         ipcflag, delta)

  end subroutine pcshell_setup

end module PetscImsPreconditionerModule

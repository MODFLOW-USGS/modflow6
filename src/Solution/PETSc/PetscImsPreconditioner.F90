module PetscImsPreconditionerModule
#include <petsc/finclude/petscksp.h>
  use petscksp

  implicit none
  private

  public :: PcShellCtxType
  public :: pcshell_apply
  public :: pcshell_setup
  public :: pcshell_destroy

  type :: PcShellCtxType
    Vec :: diag
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

  !> @brief Apply shell preconditioner
  !<
  subroutine pcshell_apply(pc, x, y, ierr)
    PC :: pc !< the shell preconditioner
    Vec :: x !< the input vector
    Vec :: y !< the output vector
    PetscErrorCode :: ierr !< PETSc error code
    ! local
    type(PcShellCtxType), pointer :: pc_ctx => null()

    ! this applies an example jacobi pc,
    ! to be replaced by others (MILUT)
    call PCShellGetContext(pc, pc_ctx, ierr)
    CHKERRQ(ierr)
    call VecPointwiseMult(y, x, pc_ctx%diag, ierr)
    CHKERRQ(ierr)

  end subroutine pcshell_apply

  !> @brief Set up the custom preconditioner
  !<
  subroutine pcshell_setup(pc, ierr)
    PC :: pc !< the shell preconditioner
    PetscErrorCode :: ierr !< PETSc error code
    ! local
    Mat :: pmat
    type(PcShellCtxType), pointer :: pc_ctx => null()

    ! this currently sets up an example jacobi pc,
    ! to be replaced by others (MILUT)
    call PCShellGetContext(pc, pc_ctx, ierr)
    CHKERRQ(ierr)
    call PCGetOperators(pc, PETSC_NULL_MAT, pmat, ierr)
    CHKERRQ(ierr)
    call MatCreateVecs(pmat, pc_ctx%diag, PETSC_NULL_VEC, ierr)
    CHKERRQ(ierr)
    call MatGetDiagonal(pmat, pc_ctx%diag, ierr)
    CHKERRQ(ierr)
    call VecReciprocal(pc_ctx%diag, ierr)
    CHKERRQ(ierr)

  end subroutine pcshell_setup

  !> @brief Clean up
  !<
  subroutine pcshell_destroy(pc, ierr)
    PC :: pc !< the shell preconditioner
    PetscErrorCode :: ierr !< PETSc error code
    ! local

  end subroutine pcshell_destroy

end module PetscImsPreconditionerModule

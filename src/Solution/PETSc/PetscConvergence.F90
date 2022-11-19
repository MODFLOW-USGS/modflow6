module PetscConvergenceModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use KindModule, only: I4B, DP
  implicit none
  private

  public :: petsc_check_convergence
  public :: petsc_destroy_context
  
  type, public :: PetscContextType
    Vec :: x_old
    Vec :: delta_x
    real(DP) :: dvclose
  end type PetscContextType

contains

  subroutine petsc_check_convergence(ksp, n, rnorm, flag, petsc_context, ierr)
    KSP :: ksp !< Iterative context
    PetscInt :: n !< Iteration number
    PetscReal :: rnorm !< 2-norm (preconditioned) residual value
    KSPConvergedReason :: flag !< Converged reason
    class(PetscContextType), pointer :: petsc_context !< optional user-defined monitor context
    PetscErrorCode :: ierr !< error
    ! local
    PetscScalar :: alpha = -1.0
    real(DP) :: norm
    Vec :: x

    call KSPBuildSolution(ksp, PETSC_NULL_VEC, x, ierr)
    CHKERRQ(ierr)

    if (n == 0) then
      call VecCopy(x, petsc_context%x_old, ierr)
      CHKERRQ(ierr)
      flag = KSP_CONVERGED_ITERATING
      return
    end if

    call VecWAXPY(petsc_context%delta_x, alpha, x, petsc_context%x_old, ierr)
    CHKERRQ(ierr)

    call VecNorm(petsc_context%delta_x, NORM_INFINITY, norm, ierr)
    CHKERRQ(ierr)

    call VecCopy(x, petsc_context%x_old, ierr)
    CHKERRQ(ierr)
    
    if (norm < petsc_context%dvclose) then
      flag = KSP_CONVERGED_HAPPY_BREAKDOWN ! Converged
    else
      flag = KSP_CONVERGED_ITERATING ! Not yet converged
    end if

  end subroutine petsc_check_convergence

  subroutine petsc_destroy_context(petsc_context, ierr)
    class(PetscContextType), pointer :: petsc_context !< optional user-defined monitor context
    PetscErrorCode :: ierr                        !< error

    call VecDestroy(petsc_context%x_old, ierr)
    CHKERRQ(ierr)
    call VecDestroy(petsc_context%delta_x, ierr)
    CHKERRQ(ierr)
    deallocate(petsc_context)

  end subroutine petsc_destroy_context

end module PetscConvergenceModule
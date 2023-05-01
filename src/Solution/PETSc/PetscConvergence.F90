module PetscConvergenceModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use KindModule, only: I4B, DP
  use ListModule
  implicit none
  private

  public :: petsc_check_convergence
  public :: petsc_add_context
  public :: petsc_remove_context

  type, public :: PetscContextType
    Vec :: x_old
    Vec :: delta_x
    real(DP) :: dvclose
    integer(I4B) :: max_its
  end type PetscContextType

  type(ListType) :: ctx_list

contains

  !> @brief Add a context to the static list. The
  !! generated idx can then be used as a handle when
  !! calling 'KSPSetConvergenceTest'. Make sure to remove
  !< the context from this global list when done.
  subroutine petsc_add_context(ctx, idx)
    class(PetscContextType), pointer, intent(in) :: ctx
    integer(I4B), intent(out) :: idx
    ! local
    class(*), pointer :: obj_ptr

    obj_ptr => ctx
    call ctx_list%Add(obj_ptr)
    idx = ctx_list%Count()

  end subroutine petsc_add_context

  !> @brief This will clear the list with context pointers
  !<
  subroutine petsc_remove_context(ctx)
    class(PetscContextType), pointer, intent(in) :: ctx
    ! local
    integer(I4B) :: idx
    class(*), pointer :: obj_ptr

    obj_ptr => ctx
    idx = ctx_list%GetIndex(obj_ptr)
    call ctx_list%RemoveNode(idx, .false.)

  end subroutine petsc_remove_context

  !> @brief Routine to check the convergence. This is called
  !< from within PETSc.
  subroutine petsc_check_convergence(ksp, n, rnorm, flag, ctx_id, ierr)
    KSP :: ksp !< Iterative context
    PetscInt :: n !< Iteration number
    PetscReal :: rnorm !< 2-norm (preconditioned) residual value
    KSPConvergedReason :: flag !< Converged reason
    PetscInt :: ctx_id !< index into the static context list
    PetscErrorCode :: ierr !< error
    ! local
    PetscScalar, parameter :: min_one = -1.0
    real(DP) :: norm
    Vec :: x
    class(PetscContextType), pointer :: petsc_context
    class(*), pointer :: obj_ptr

    ! get the context from the list
    petsc_context => null()
    obj_ptr => ctx_list%GetItem(ctx_id)
    select type (obj_ptr)
    class is (PetscContextType)
      petsc_context => obj_ptr
    end select

    call KSPBuildSolution(ksp, PETSC_NULL_VEC, x, ierr)
    CHKERRQ(ierr)

    if (n == 0) then
      call VecCopy(x, petsc_context%x_old, ierr)
      CHKERRQ(ierr)
      flag = KSP_CONVERGED_ITERATING
      return
    end if

    call VecWAXPY(petsc_context%delta_x, min_one, x, petsc_context%x_old, ierr)
    CHKERRQ(ierr)

    call VecNorm(petsc_context%delta_x, NORM_INFINITY, norm, ierr)
    CHKERRQ(ierr)

    call VecCopy(x, petsc_context%x_old, ierr)
    CHKERRQ(ierr)

    if (norm < petsc_context%dvclose) then
      flag = KSP_CONVERGED_HAPPY_BREAKDOWN ! Converged
    else
      flag = KSP_CONVERGED_ITERATING ! Not yet converged
      if (n == petsc_context%max_its) then
        ! ran out of iterations before convergence
        ! has been reached
        flag = KSP_DIVERGED_ITS
      end if
    end if

  end subroutine petsc_check_convergence

end module PetscConvergenceModule

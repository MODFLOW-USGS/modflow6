module PetscConvergenceModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use KindModule, only: I4B, DP
  use ListModule
  use ConvergenceSummaryModule
  implicit none
  private

  public :: petsc_check_convergence
  public :: KSPSetConvergenceTest

  type, public :: PetscContextType
    Vec :: x_old
    Vec :: res_old
    Vec :: delta_x
    Vec :: delta_res
    real(DP) :: dvclose
    integer(I4B) :: max_its
    type(ConvergenceSummaryType), pointer :: cnvg_summary => null()
  contains
    procedure :: destroy
  end type PetscContextType

  ! passing our context into PETSc requires an explicit interface
  ! on KSPSetConvergenceTest, it is defined here:
  interface
    subroutine CnvgCheckFunc(ksp, n, rnorm, flag, context, ierr)
      import tKSP, PetscContextType
      type(tKSP) :: ksp
      PetscInt :: n
      PetscReal :: rnorm
      KSPConvergedReason :: flag
      class(PetscContextType), pointer :: context
      PetscErrorCode :: ierr
    end subroutine

    subroutine CnvgDestroyFunc(context, ierr)
      import PetscContextType
      class(PetscContextType), pointer :: context
      PetscErrorCode :: ierr
    end subroutine

    subroutine KSPSetConvergenceTest(ksp, check_convergence, context, &
                                     destroy, ierr)
      import tKSP, CnvgCheckFunc, PetscContextType, CnvgDestroyFunc
      type(tKSP) :: ksp
      procedure(CnvgCheckFunc) :: check_convergence
      class(PetscContextType), pointer :: context
      procedure(CnvgDestroyFunc) :: destroy
      PetscErrorCode :: ierr
    end subroutine
  end interface

contains

  !> @brief Routine to check the convergence. This is called
  !< from within PETSc.
  subroutine petsc_check_convergence(ksp, n, rnorm, flag, context, ierr)
    KSP :: ksp !< Iterative context
    PetscInt :: n !< Iteration number
    PetscReal :: rnorm !< 2-norm (preconditioned) residual value
    KSPConvergedReason :: flag !< Converged reason
    class(PetscContextType), pointer :: context !< context
    PetscErrorCode :: ierr !< error
    ! local
    PetscScalar, parameter :: min_one = -1.0
    PetscScalar, dimension(:), pointer :: local_dx, local_dr
    PetscScalar :: norm, dvmax_model, drmax_model
    PetscInt :: idx_dv, idx_dr
    Vec :: x, res
    type(ConvergenceSummaryType), pointer :: summary
    PetscInt :: iter_cnt
    PetscInt :: i, j, istart, iend

    summary => context%cnvg_summary

    ! NB: KSPBuildResidual needs to have its vector destroyed
    ! to avoid a memory leak, KSPBuildSolution doesn't...
    call KSPBuildSolution(ksp, PETSC_NULL_VEC, x, ierr)
    CHKERRQ(ierr)
    call KSPBuildResidual(ksp, PETSC_NULL_VEC, PETSC_NULL_VEC, res, ierr)
    CHKERRQ(ierr)

    ! n == 0 is before the iteration starts
    if (n == 0) then
      if (rnorm == 0.0) then
        ! exact solution found
        flag = KSP_CONVERGED_HAPPY_BREAKDOWN
      else
        call VecCopy(x, context%x_old, ierr)
        CHKERRQ(ierr)
        call VecCopy(res, context%res_old, ierr)
        CHKERRQ(ierr)
        flag = KSP_CONVERGED_ITERATING
      end if

      call VecDestroy(res, ierr)
      CHKERRQ(ierr)
      return
    end if

    ! increment iteration counter
    summary%iter_cnt = summary%iter_cnt + 1
    iter_cnt = summary%iter_cnt

    if (summary%nitermax > 1) then
      summary%itinner(iter_cnt) = n
      do i = 1, summary%convnmod
        summary%convdvmax(i, iter_cnt) = -huge(dvmax_model)
        summary%convlocdv(i, iter_cnt) = -1
        summary%convdrmax(i, iter_cnt) = -huge(drmax_model)
        summary%convlocdr(i, iter_cnt) = -1
      end do
    end if

    call VecWAXPY(context%delta_x, min_one, context%x_old, x, ierr)
    CHKERRQ(ierr)

    call VecWAXPY(context%delta_res, min_one, context%res_old, res, ierr)
    CHKERRQ(ierr)

    call VecNorm(context%delta_x, NORM_INFINITY, norm, ierr)
    CHKERRQ(ierr)

    call VecCopy(x, context%x_old, ierr)
    CHKERRQ(ierr)

    call VecCopy(res, context%res_old, ierr)
    CHKERRQ(ierr)

    call VecDestroy(res, ierr)
    CHKERRQ(ierr)

    ! get dv and dr per local model (readonly!)
    call VecGetArrayReadF90(context%delta_x, local_dx, ierr)
    CHKERRQ(ierr)
    call VecGetArrayReadF90(context%delta_res, local_dr, ierr)
    CHKERRQ(ierr)
    do i = 1, summary%convnmod
      ! reset
      dvmax_model = 0.0
      idx_dv = -1
      drmax_model = 0.0
      idx_dr = -1
      ! get first and last model index
      istart = summary%model_bounds(i)
      iend = summary%model_bounds(i + 1) - 1
      do j = istart, iend
        if (abs(local_dx(j)) > abs(dvmax_model)) then
          dvmax_model = local_dx(j)
          idx_dv = j
        end if
        if (abs(local_dr(j)) > abs(drmax_model)) then
          drmax_model = local_dr(j)
          idx_dr = j
        end if
      end do
      if (summary%nitermax > 1) then
        summary%convdvmax(i, iter_cnt) = dvmax_model
        summary%convlocdv(i, iter_cnt) = idx_dv
        summary%convdrmax(i, iter_cnt) = drmax_model
        summary%convlocdr(i, iter_cnt) = idx_dr
      end if
    end do
    call VecRestoreArrayF90(context%delta_x, local_dx, ierr)
    CHKERRQ(ierr)
    call VecRestoreArrayF90(context%delta_res, local_dr, ierr)
    CHKERRQ(ierr)

    if (norm < context%dvclose) then
      flag = KSP_CONVERGED_HAPPY_BREAKDOWN ! Converged
    else
      flag = KSP_CONVERGED_ITERATING ! Not yet converged
      if (n == context%max_its) then
        ! ran out of iterations before convergence
        ! has been reached
        flag = KSP_DIVERGED_ITS
      end if
    end if

  end subroutine petsc_check_convergence

  subroutine destroy(this)
    class(PetscContextType) :: this
    ! local
    integer(I4B) :: ierr

    call VecDestroy(this%x_old, ierr)
    CHKERRQ(ierr)
    call VecDestroy(this%res_old, ierr)
    CHKERRQ(ierr)
    call VecDestroy(this%delta_x, ierr)
    CHKERRQ(ierr)
    call VecDestroy(this%delta_res, ierr)
    CHKERRQ(ierr)

  end subroutine destroy

end module PetscConvergenceModule

module PetscConvergenceModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DPREC
  use ListModule
  use ConvergenceSummaryModule
  implicit none
  private

  public :: petsc_check_convergence
  public :: KSPSetConvergenceTest

  type, public :: PetscContextType
    Vec :: x_old
    Vec :: delta_x
    integer(I4B) :: icnvg_ims !< IMS convergence number: 1 => converged, -1 => forces next Picard iter
    integer(I4B) :: icnvgopt !< convergence option from IMS settings
    real(DP) :: dvclose !< dep. variable closure criterion
    real(DP) :: rclose !< residual closure criterion
    integer(I4B) :: max_its !< maximum number of inner iterations
    real(DP) :: rnorm_L2_init !< the initial L2 norm for (b - Ax)
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
  subroutine petsc_check_convergence(ksp, n, rnorm_L2, flag, context, ierr)
    KSP :: ksp !< Iterative context
    PetscInt :: n !< Iteration number
    PetscReal :: rnorm_L2 !< 2-norm (preconditioned) residual value
    KSPConvergedReason :: flag !< Converged reason
    class(PetscContextType), pointer :: context !< context
    PetscErrorCode :: ierr !< error
    ! local
    PetscReal, parameter :: min_one = -1.0
    PetscReal, dimension(:), pointer :: local_dx, local_res
    PetscReal :: xnorm_inf, rnorm_inf
    PetscReal :: dvmax_model, rmax_model
    PetscInt :: idx_dv, idx_r
    Vec :: x, res
    type(ConvergenceSummaryType), pointer :: summary
    PetscInt :: iter_cnt
    PetscInt :: i, j, istart, iend

    summary => context%cnvg_summary

    ! NB: KSPBuildResidual needs to have its vector destroyed
    ! to avoid a memory leak, KSPBuildSolution doesn't...
    call KSPBuildSolution(ksp, PETSC_NULL_VEC, x, ierr)
    CHKERRQ(ierr)

    ! n == 0 is before the iteration starts
    if (n == 0) then
      context%rnorm_L2_init = rnorm_L2
      if (rnorm_L2 < DPREC) then
        ! exact solution found
        flag = KSP_CONVERGED_HAPPY_BREAKDOWN
      else
        call VecCopy(x, context%x_old, ierr)
        CHKERRQ(ierr)
        flag = KSP_CONVERGED_ITERATING
      end if
      CHKERRQ(ierr)
      return
    end if

    call KSPBuildResidual(ksp, PETSC_NULL_VEC, PETSC_NULL_VEC, res, ierr)
    CHKERRQ(ierr)

    ! increment iteration counter
    summary%iter_cnt = summary%iter_cnt + 1
    iter_cnt = summary%iter_cnt

    if (summary%nitermax > 1) then
      summary%itinner(iter_cnt) = n
      do i = 1, summary%convnmod
        summary%convdvmax(i, iter_cnt) = -huge(dvmax_model)
        summary%convlocdv(i, iter_cnt) = -1
        summary%convrmax(i, iter_cnt) = -huge(rmax_model)
        summary%convlocr(i, iter_cnt) = -1
      end do
    end if

    call VecWAXPY(context%delta_x, min_one, context%x_old, x, ierr)
    CHKERRQ(ierr)

    call VecNorm(context%delta_x, NORM_INFINITY, xnorm_inf, ierr)
    CHKERRQ(ierr)

    rnorm_inf = 0.0
    if (context%icnvgopt == 0 .or. context%icnvgopt == 1) then
      call VecNorm(res, NORM_INFINITY, rnorm_inf, ierr)
      CHKERRQ(ierr)
    end if

    call VecCopy(x, context%x_old, ierr)
    CHKERRQ(ierr)

    ! get dv and dr per local model (readonly!)
    call VecGetArrayReadF90(context%delta_x, local_dx, ierr)
    CHKERRQ(ierr)
    call VecGetArrayReadF90(res, local_res, ierr)
    CHKERRQ(ierr)
    do i = 1, summary%convnmod
      ! reset
      dvmax_model = 0.0
      idx_dv = -1
      rmax_model = 0.0
      idx_r = -1
      ! get first and last model index
      istart = summary%model_bounds(i)
      iend = summary%model_bounds(i + 1) - 1
      do j = istart, iend
        if (abs(local_dx(j)) > abs(dvmax_model)) then
          dvmax_model = local_dx(j)
          idx_dv = j
        end if
        if (abs(local_res(j)) > abs(rmax_model)) then
          rmax_model = local_res(j)
          idx_r = j
        end if
      end do
      if (summary%nitermax > 1) then
        summary%convdvmax(i, iter_cnt) = dvmax_model
        summary%convlocdv(i, iter_cnt) = idx_dv
        summary%convrmax(i, iter_cnt) = rmax_model
        summary%convlocr(i, iter_cnt) = idx_r
      end if
    end do
    call VecRestoreArrayF90(context%delta_x, local_dx, ierr)
    CHKERRQ(ierr)
    call VecRestoreArrayF90(res, local_res, ierr)
    CHKERRQ(ierr)

    call VecDestroy(res, ierr)
    CHKERRQ(ierr)

    flag = apply_check(context, n, xnorm_inf, rnorm_inf, rnorm_L2)
    if (flag == KSP_CONVERGED_ITERATING) then
      ! not yet converged, max. iters reached? Then stop.
      if (n == context%max_its) then
        flag = KSP_DIVERGED_ITS
      end if
    end if
    if (rnorm_L2 < DPREC) then
      ! exact solution, set to 'converged'
      flag = KSP_CONVERGED_HAPPY_BREAKDOWN
    end if

  end subroutine petsc_check_convergence

  !> @brief Apply the IMS convergence check
  !<
  function apply_check(ctx, nit, dvmax, rnorm_inf, rnorm_L2) result(flag)
    use TdisModule, only: kstp
    use IMSLinearBaseModule, only: ims_base_testcnvg, ims_base_epfact
    class(PetscContextType) :: ctx
    integer(I4B) :: nit !< iteration number
    real(DP) :: dvmax !< infinity norm of dep. var. change
    real(DP) :: rnorm_inf !< infinity norm of residual change
    real(DP) :: rnorm_L2 !< L2-norm of residual change
    KSPConvergedReason :: flag !< the convergence status
    ! local
    real(DP) :: epfact
    real(DP) :: rcnvg

    ! Set to 'not converged'
    flag = KSP_CONVERGED_ITERATING
    ctx%icnvg_ims = 0

    epfact = ims_base_epfact(ctx%icnvgopt, kstp)

    if (ctx%icnvgopt == 2 .or. &
        ctx%icnvgopt == 3 .or. &
        ctx%icnvgopt == 4) then
      rcnvg = rnorm_L2
    else
      rcnvg = rnorm_inf
    end if
    call ims_base_testcnvg(ctx%icnvgopt, ctx%icnvg_ims, nit, &
                           dvmax, rcnvg, ctx%rnorm_L2_init, &
                           epfact, ctx%dvclose, ctx%rclose)

    if (ctx%icnvg_ims /= 0) then
      ! Set to 'converged'
      flag = KSP_CONVERGED_HAPPY_BREAKDOWN
    end if

  end function apply_check

  subroutine destroy(this)
    class(PetscContextType) :: this
    ! local
    integer(I4B) :: ierr

    call VecDestroy(this%x_old, ierr)
    CHKERRQ(ierr)
    call VecDestroy(this%delta_x, ierr)
    CHKERRQ(ierr)

  end subroutine destroy

end module PetscConvergenceModule

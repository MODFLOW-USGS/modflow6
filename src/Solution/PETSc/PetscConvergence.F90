module PetscConvergenceModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use KindModule, only: I4B, DP
  use ListModule
  use ConvergenceSummaryModule
  implicit none
  private

  public :: petsc_check_convergence
  public :: petsc_add_context
  public :: petsc_remove_context

  type, public :: PetscContextType
    Vec :: x_old
    Vec :: res_old
    Vec :: delta_x
    Vec :: delta_res
    real(DP) :: dvclose
    integer(I4B) :: max_its
    type(ConvergenceSummaryType), pointer :: cnvg_summary => null()
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
    PetscScalar, dimension(:), pointer :: local_dx, local_dr
    PetscScalar :: norm, dvmax_model, drmax_model
    PetscInt :: idx_dv, idx_dr
    Vec :: x, res
    class(PetscContextType), pointer :: petsc_ctx
    type(ConvergenceSummaryType), pointer :: summary
    class(*), pointer :: obj_ptr
    PetscInt :: iter_cnt
    PetscInt :: i, j, istart, iend

    ! get the context from the list
    petsc_ctx => null()
    obj_ptr => ctx_list%GetItem(ctx_id)
    select type (obj_ptr)
    class is (PetscContextType)
      petsc_ctx => obj_ptr
    end select

    summary => petsc_ctx%cnvg_summary

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
        call VecCopy(x, petsc_ctx%x_old, ierr)
        CHKERRQ(ierr)
        call VecCopy(res, petsc_ctx%res_old, ierr)
        CHKERRQ(ierr)
        flag = KSP_CONVERGED_ITERATING
      end if
      return
    end if

    ! increment iteration counter
    summary%iter_cnt = summary%iter_cnt + 1
    iter_cnt = summary%iter_cnt

    summary%itinner(iter_cnt) = n
    do i = 1, summary%convmod
      summary%convdvmax(i, iter_cnt) = -huge(dvmax_model)
      summary%convlocdv(i, iter_cnt) = -1
      summary%convdrmax(i, iter_cnt) = -huge(drmax_model)
      summary%convlocdr(i, iter_cnt) = -1
    end do

    call VecWAXPY(petsc_ctx%delta_x, min_one, petsc_ctx%x_old, x, ierr)
    CHKERRQ(ierr)

    call VecWAXPY(petsc_ctx%delta_res, min_one, petsc_ctx%res_old, res, ierr)
    CHKERRQ(ierr)

    call VecNorm(petsc_ctx%delta_x, NORM_INFINITY, norm, ierr)
    CHKERRQ(ierr)

    call VecCopy(x, petsc_ctx%x_old, ierr)
    CHKERRQ(ierr)

    call VecCopy(res, petsc_ctx%res_old, ierr)
    CHKERRQ(ierr)

    ! get dv and dr per local model
    call VecGetArrayF90(petsc_ctx%delta_x, local_dx, ierr)
    CHKERRQ(ierr)
    call VecGetArrayF90(petsc_ctx%delta_res, local_dr, ierr)
    CHKERRQ(ierr)
    do i = 1, summary%convmod
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
      summary%convdvmax(i, iter_cnt) = dvmax_model
      summary%convlocdv(i, iter_cnt) = idx_dv
      summary%convdrmax(i, iter_cnt) = drmax_model
      summary%convlocdr(i, iter_cnt) = idx_dr
    end do
    call VecRestoreArrayF90(x, local_dx, ierr)
    CHKERRQ(ierr)
    call VecRestoreArrayF90(x, local_dr, ierr)
    CHKERRQ(ierr)

    if (norm < petsc_ctx%dvclose) then
      flag = KSP_CONVERGED_HAPPY_BREAKDOWN ! Converged
    else
      flag = KSP_CONVERGED_ITERATING ! Not yet converged
      if (n == petsc_ctx%max_its) then
        ! ran out of iterations before convergence
        ! has been reached
        flag = KSP_DIVERGED_ITS
      end if
    end if

  end subroutine petsc_check_convergence

end module PetscConvergenceModule

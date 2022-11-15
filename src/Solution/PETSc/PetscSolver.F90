module PetscSolverModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use KindModule, only: I4B, DP
  use LinearSolverBaseModule
  use MatrixBaseModule
  use VectorBaseModule
  use PetscMatrixModule
  use PetscVectorModule
  use PetscConvergenceModule

  implicit none
  private

  public :: create_petsc_solver

  ! TODO_MJR: this should be universal
  integer(I4B), parameter :: LIN_ACCEL_CG = 1
  integer(I4B), parameter :: LIN_ACCEL_BCGS = 2

  type, public, extends(LinearSolverBaseType) :: PetscSolverType  
    KSP :: ksp_petsc
    Mat, pointer :: mat_petsc

    integer(I4B) :: lin_accel_type
    real(DP) :: dvclose
  contains
    procedure :: initialize => petsc_initialize 
    procedure :: solve => petsc_solve
    procedure :: get_result => petsc_get_result
    procedure :: destroy => petsc_destroy
    procedure :: create_matrix => petsc_create_matrix

    ! private
    procedure :: print_before
    procedure :: print_after
  end type PetscSolverType

contains

  function create_petsc_solver() result(solver)
    class(LinearSolverBaseType), pointer :: solver
    ! local
    class(PetscSolverType), pointer :: petsc_solver

    allocate(petsc_solver)
    solver => petsc_solver

  end function create_petsc_solver

  subroutine petsc_initialize(this, matrix, cfg)
    class(PetscSolverType) :: this
    class(MatrixBaseType), pointer :: matrix
    class(LinearSolverCfg) :: cfg
    ! local
    PetscErrorCode :: ierr
    !PC pc !< the preconditioner handle
    class(PetscContextType), pointer :: petsc_ctx => null() !< the context to use for convergence check

    this%mat_petsc => null()
    select type (pm => matrix)
    class is (PetscMatrixType)
      this%mat_petsc => pm%mat
    end select

    this%lin_accel_type = cfg%linear_accel_type
    this%dvclose = cfg%dvclose

    call KSPCreate(PETSC_COMM_WORLD, this%ksp_petsc, ierr)    
    CHKERRQ(ierr)
    if (this%lin_accel_type == LIN_ACCEL_CG) then
      call KSPSetType(this%ksp_petsc, KSPCG, ierr)
      CHKERRQ(ierr)
    else if (this%lin_accel_type == LIN_ACCEL_BCGS) then
      call KSPSetType(this%ksp_petsc, KSPBCGS, ierr)
      CHKERRQ(ierr)
    end if

    ! TODO_MJR: why does this need to be false, and not true...
    call KSPSetInitialGuessNonzero(this%ksp_petsc, .false., ierr)
    CHKERRQ(ierr)

    call KSPSetFromOptions(this%ksp_petsc, ierr)
    CHKERRQ(ierr)

    call KSPSetOperators(this%ksp_petsc, this%mat_petsc, this%mat_petsc, ierr)
    CHKERRQ(ierr)

    ! create context for custom convergence check
    allocate (petsc_ctx)
    petsc_ctx%dvclose = this%dvclose
    call KSPSetConvergenceTest(this%ksp_petsc, petsc_check_convergence, &
                               petsc_ctx, petsc_destroy_context, ierr)
    CHKERRQ(ierr)

  end subroutine petsc_initialize

  subroutine petsc_solve(this, kiter, rhs, x)
    class(PetscSolverType) :: this
    integer(I4B) :: kiter
    class(VectorBaseType), pointer :: rhs
    class(VectorBaseType), pointer :: x
    ! local
    PetscErrorCode :: ierr
    class(PetscVectorType), pointer :: rhs_petsc, x_petsc
    KSPConvergedReason :: icnvg
    ! for debugging:
    logical :: print_everything = .false.


    rhs_petsc => null()
    select type (rhs)
    class is (PetscVectorType)
      rhs_petsc => rhs
    end select

    x_petsc => null()
    select type (x)
    class is (PetscVectorType)
      x_petsc => x
    end select

    this%iteration_number = 0
    this%is_converged = 0

    ! print system
    if (print_everything) then
      call this%print_before(rhs_petsc, x_petsc, kiter)
    end if

    call KSPSolve(this%ksp_petsc, rhs_petsc%vec_impl, x_petsc%vec_impl, ierr)
    CHKERRQ(ierr)

    if (print_everything) then
      call this%print_after(x_petsc, kiter)
    end if

    call KSPGetIterationNumber(this%ksp_petsc, this%iteration_number, ierr)
    call KSPGetConvergedReason(this%ksp_petsc, icnvg, ierr)
    if (icnvg > 0) this%is_converged = 1

  end subroutine petsc_solve

  subroutine petsc_get_result(this)
    class(PetscSolverType) :: this
  end subroutine petsc_get_result

  subroutine petsc_destroy(this)
    class(PetscSolverType) :: this
    ! local
    PetscErrorCode :: ierr

    call KSPDestroy(this%ksp_petsc, ierr)
    CHKERRQ(ierr)

  end subroutine petsc_destroy

  function petsc_create_matrix(this) result(matrix)
    class(PetscSolverType) :: this
    class(MatrixBaseType), pointer :: matrix
    ! local
    class(PetscMatrixType), pointer :: petsc_matrix

    allocate (petsc_matrix)
    matrix => petsc_matrix

  end function petsc_create_matrix

  subroutine print_before(this, rhs, x, kiter)    
    use TdisModule, only: nper, kstp
    class(PetscSolverType) :: this
    class(PetscVectorType) :: rhs
    class(PetscVectorType) :: x
    integer(I4B) :: kiter
    ! local
    PetscViewer :: viewer
    character(len=24) :: filename
    PetscErrorCode :: ierr

    write(filename,'(a,i0,a,i0,a,i0,a)') 'amat_', nper, '_', kstp, '_', kiter, '.txt'
    call PetscViewerASCIIOpen(PETSC_COMM_WORLD, filename, viewer, ierr)
    CHKERRQ(ierr)
    call MatView(this%mat_petsc, viewer, ierr)        
    CHKERRQ(ierr)
    call PetscViewerDestroy(viewer,ierr)
    CHKERRQ(ierr)
    write(filename,'(a,i0,a,i0,a,i0,a)') 'rhs_', nper, '_', kstp, '_', kiter, '.txt'
    call PetscViewerASCIIOpen(PETSC_COMM_WORLD, filename, viewer, ierr)
    CHKERRQ(ierr)
    call VecView(rhs%vec_impl, viewer, ierr)        
    CHKERRQ(ierr)
    call PetscViewerDestroy(viewer,ierr)
    CHKERRQ(ierr)
    write(filename,'(a,i0,a,i0,a,i0,a)') 'xold_', nper, '_', kstp, '_', kiter, '.txt'
    call PetscViewerASCIIOpen(PETSC_COMM_WORLD, filename, viewer, ierr)
    CHKERRQ(ierr)
    call VecView(x%vec_impl, viewer, ierr)        
    CHKERRQ(ierr)
    call PetscViewerDestroy(viewer,ierr)
    CHKERRQ(ierr)

  end subroutine print_before

  subroutine print_after(this, x, kiter)    
    use TdisModule, only: nper, kstp
    class(PetscSolverType) :: this
    class(PetscVectorType) :: x
    integer(I4B) :: kiter
    ! local    
    PetscViewer :: viewer
    character(len=24) :: filename
    PetscErrorCode :: ierr

    write(filename,'(a,i0,a,i0,a,i0,a)') 'x_', nper, '_', kstp, '_', kiter, '.txt'
    call PetscViewerASCIIOpen(PETSC_COMM_WORLD, filename, viewer, ierr)
    CHKERRQ(ierr)
    call VecView(x%vec_impl, viewer, ierr)        
    CHKERRQ(ierr)
    call PetscViewerDestroy(viewer,ierr)
    CHKERRQ(ierr)

  end subroutine print_after

end module PetscSolverModule
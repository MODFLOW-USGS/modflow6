module PetscSolverModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  use KindModule, only: I4B, DP, LGP
  use LinearSolverBaseModule
  use MatrixBaseModule
  use VectorBaseModule
  use PetscMatrixModule
  use PetscVectorModule
  use PetscConvergenceModule
  use ConvergenceSummaryModule
  use SimVariablesModule, only: iout

  implicit none
  private

  public :: create_petsc_solver

  type, public, extends(LinearSolverBaseType) :: PetscSolverType
    KSP :: ksp_petsc
    class(PetscMatrixType), pointer :: matrix
    Mat, pointer :: mat_petsc
    Vec, pointer :: vec_residual

    real(DP) :: dvclose
    class(PetscContextType), pointer :: petsc_ctx
    integer(I4B) :: ctx_idx
    type(ConvergenceSummaryType), pointer :: convergence_summary => null()

  contains
    procedure :: initialize => petsc_initialize
    procedure :: solve => petsc_solve
    procedure :: get_result => petsc_get_result
    procedure :: print_summary => petsc_print_summary
    procedure :: destroy => petsc_destroy
    procedure :: create_matrix => petsc_create_matrix

    ! private
    procedure, private :: get_options
    procedure, private :: create_ksp
    procedure, private :: create_convergence_check
    procedure, private :: print_vec
  end type PetscSolverType

contains

  !> @brief Create a PETSc solver object
  !<
  function create_petsc_solver() result(solver)
    class(LinearSolverBaseType), pointer :: solver !< Uninitialized instance of the PETSc solver
    ! local
    class(PetscSolverType), pointer :: petsc_solver

    allocate (petsc_solver)
    allocate (petsc_solver%petsc_ctx)

    solver => petsc_solver

  end function create_petsc_solver

  !> @brief Initialize PETSc KSP solver with
  !<  options from the petsc database file
  subroutine petsc_initialize(this, matrix, convergence_summary)
    class(PetscSolverType) :: this !< This solver instance
    class(MatrixBaseType), pointer :: matrix !< The solution matrix as KSP operator
    type(ConvergenceSummaryType), pointer :: convergence_summary
    ! local
    PetscErrorCode :: ierr
    PetscInt :: major, minor, subminor, release
    character(len=128) :: petsc_version, release_str

    call PetscGetVersionNumber(major, minor, subminor, release, ierr)
    if (release == 1) then
      release_str = "(release)"
    else
      release_str = "(unofficial)"
    end if
    write (petsc_version, '(i0,a,i0,a,i0,a,a)') &
      major, ".", minor, ".", subminor, " ", trim(release_str)

    CHKERRQ(ierr)
    write (iout, '(1x,2a,/)') &
      "PETSc Linear Solver will be used: version ", petsc_version

    this%mat_petsc => null()
    select type (pm => matrix)
    class is (PetscMatrixType)
      this%matrix => pm
      this%mat_petsc => pm%mat
    end select

    allocate (this%vec_residual)
    call MatCreateVecs(this%mat_petsc, this%vec_residual, PETSC_NULL_VEC, ierr)
    CHKERRQ(ierr)

    ! get options from PETSc database file
    call this%get_options()

    ! create the solver object
    call this%create_ksp()

    ! Create custom convergence check
    call this%create_convergence_check(convergence_summary)

  end subroutine petsc_initialize

  !> @brief Get the PETSc options from the database
  !<
  subroutine get_options(this)
    class(PetscSolverType) :: this
    ! local
    PetscErrorCode :: ierr
    logical(LGP) :: found

    this%dvclose = 0.01_DP
    call PetscOptionsGetReal(PETSC_NULL_OPTIONS, PETSC_NULL_CHARACTER, &
                             '-dvclose', this%dvclose, found, ierr)
    CHKERRQ(ierr)

    this%nitermax = 100
    call PetscOptionsGetInt(PETSC_NULL_OPTIONS, PETSC_NULL_CHARACTER, &
                            '-nitermax', this%nitermax, found, ierr)
    CHKERRQ(ierr)

  end subroutine get_options

  !> @brief Create the PETSc KSP object
  !<
  subroutine create_ksp(this)
    class(PetscSolverType) :: this !< This solver instance
    ! local
    PetscErrorCode :: ierr

    call KSPCreate(PETSC_COMM_WORLD, this%ksp_petsc, ierr)
    CHKERRQ(ierr)

    call KSPSetInitialGuessNonzero(this%ksp_petsc, .true., ierr)
    CHKERRQ(ierr)

    call KSPSetFromOptions(this%ksp_petsc, ierr)
    CHKERRQ(ierr)

    call KSPSetOperators(this%ksp_petsc, this%mat_petsc, this%mat_petsc, ierr)
    CHKERRQ(ierr)

  end subroutine create_ksp

  !> @brief Create and assign a custom convergence
  !< check for this solver
  subroutine create_convergence_check(this, convergence_summary)
    class(PetscSolverType) :: this !< This solver instance
    type(ConvergenceSummaryType), pointer :: convergence_summary
    ! local
    PetscErrorCode :: ierr

    this%petsc_ctx%dvclose = this%dvclose
    this%petsc_ctx%max_its = this%nitermax
    this%petsc_ctx%cnvg_summary => convergence_summary
    call MatCreateVecs( &
      this%mat_petsc, this%petsc_ctx%x_old, PETSC_NULL_VEC, ierr)
    CHKERRQ(ierr)
    call MatCreateVecs( &
      this%mat_petsc, this%petsc_ctx%delta_x, PETSC_NULL_VEC, ierr)
    CHKERRQ(ierr)
    call MatCreateVecs( &
      this%mat_petsc, this%petsc_ctx%res_old, PETSC_NULL_VEC, ierr)
    CHKERRQ(ierr)
    call MatCreateVecs( &
      this%mat_petsc, this%petsc_ctx%delta_res, PETSC_NULL_VEC, ierr)
    CHKERRQ(ierr)
    call petsc_add_context(this%petsc_ctx, this%ctx_idx)

    call KSPSetConvergenceTest(this%ksp_petsc, petsc_check_convergence, &
                               this%ctx_idx, PETSC_NULL_FUNCTION, ierr)
    CHKERRQ(ierr)

  end subroutine create_convergence_check

  subroutine petsc_solve(this, kiter, rhs, x, cnvg_summary)
    class(PetscSolverType) :: this
    integer(I4B) :: kiter
    class(VectorBaseType), pointer :: rhs
    class(VectorBaseType), pointer :: x
    type(ConvergenceSummaryType) :: cnvg_summary
    ! local
    PetscErrorCode :: ierr
    class(PetscVectorType), pointer :: rhs_petsc, x_petsc
    KSPConvergedReason :: icnvg
    integer :: it_number

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
    if (kiter == 1) then
      this%petsc_ctx%cnvg_summary%iter_cnt = 0
    end if

    ! update matrix coefficients
    call this%matrix%update()
    call KSPSolve(this%ksp_petsc, rhs_petsc%vec_impl, x_petsc%vec_impl, ierr)
    CHKERRQ(ierr)

    call KSPGetIterationNumber(this%ksp_petsc, it_number, ierr)
    this%iteration_number = it_number
    call KSPGetConvergedReason(this%ksp_petsc, icnvg, ierr)
    if (icnvg > 0) this%is_converged = 1

  end subroutine petsc_solve

  subroutine petsc_get_result(this)
    class(PetscSolverType) :: this
  end subroutine petsc_get_result

  subroutine petsc_print_summary(this)
    class(PetscSolverType) :: this
    ! local
    character(len=128) :: ksp_type, pc_type, dvclose_str
    integer :: ierr
    PC :: pc

    call KSPGetType(this%ksp_petsc, ksp_type, ierr)
    CHKERRQ(ierr)
    call KSPGetPC(this%ksp_petsc, pc, ierr)
    CHKERRQ(ierr)
    call PCGetType(pc, pc_type, ierr)
    CHKERRQ(ierr)
    write (dvclose_str, '(e15.5)') this%dvclose

    write (iout, '(/,7x,a)') "PETSc linear solver settings: "
    write (iout, '(1x,a)') repeat('-', 66)
    write (iout, '(1x,a,a)') "Linear acceleration method:   ", ksp_type
    write (iout, '(1x,a,a)') "Preconditioner type:          ", pc_type
    write (iout, '(1x,a,i0)') "Maximum nr. of iterations:    ", this%nitermax
    write (iout, '(1x,a,a,/)') &
      "Dep. var. closure criterion:  ", adjustl(dvclose_str)

  end subroutine petsc_print_summary

  subroutine petsc_destroy(this)
    class(PetscSolverType) :: this
    ! local
    PetscErrorCode :: ierr

    call KSPDestroy(this%ksp_petsc, ierr)
    CHKERRQ(ierr)

    ! delete work vector
    call VecDestroy(this%vec_residual, ierr)
    CHKERRQ(ierr)
    deallocate (this%vec_residual)

    ! delete context
    call VecDestroy(this%petsc_ctx%delta_x, ierr)
    CHKERRQ(ierr)
    call VecDestroy(this%petsc_ctx%x_old, ierr)
    CHKERRQ(ierr)
    deallocate (this%petsc_ctx)

  end subroutine petsc_destroy

  function petsc_create_matrix(this) result(matrix)
    class(PetscSolverType) :: this
    class(MatrixBaseType), pointer :: matrix
    ! local
    class(PetscMatrixType), pointer :: petsc_matrix

    allocate (petsc_matrix)
    matrix => petsc_matrix

  end function petsc_create_matrix

  subroutine print_vec(this, vec, vec_name, kiter)
    use TdisModule, only: nper, kstp
    class(PetscSolverType) :: this
    class(PetscVectorType) :: vec
    character(len=*) :: vec_name
    integer(I4B) :: kiter
    ! local
    PetscViewer :: viewer
    character(len=24) :: filename
    PetscErrorCode :: ierr

    write (filename, '(2a,i0,a,i0,a,i0,a)') vec_name, '_', nper, &
      '_', kstp, '_', kiter, '.txt'
    call PetscViewerASCIIOpen(PETSC_COMM_WORLD, filename, viewer, ierr)
    CHKERRQ(ierr)
    call VecView(vec%vec_impl, viewer, ierr)
    CHKERRQ(ierr)
    call PetscViewerDestroy(viewer, ierr)
    CHKERRQ(ierr)

  end subroutine print_vec

end module PetscSolverModule

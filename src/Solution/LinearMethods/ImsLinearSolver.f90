module ImsLinearSolverModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENSOLUTIONNAME
  use LinearSolverBaseModule
  use MatrixBaseModule
  use VectorBaseModule
  use SparseMatrixModule
  use ImsLinearSettingsModule
  use ConvergenceSummaryModule

  implicit none
  private

  public :: create_ims_solver

  type, public, extends(LinearSolverBaseType) :: ImsLinearSolverType
  contains
    procedure :: initialize => ims_initialize
    procedure :: print_summary => ims_print_summary
    procedure :: solve => ims_solve
    procedure :: destroy => ims_destroy

    procedure :: create_matrix => ims_create_matrix
  end type

contains

  function create_ims_solver(sln_name) result(solver)
    class(LinearSolverBaseType), pointer :: solver
    character(len=LENSOLUTIONNAME) :: sln_name
    ! local
    class(ImsLinearSolverType), pointer :: ims_solver

    allocate (ims_solver)
    solver => ims_solver
    solver%name = sln_name

  end function create_ims_solver

  subroutine ims_initialize(this, matrix, linear_settings, convergence_summary)
    class(ImsLinearSolverType) :: this
    class(MatrixBaseType), pointer :: matrix
    type(ImsLinearSettingsType), pointer :: linear_settings
    type(ConvergenceSummaryType), pointer :: convergence_summary
  end subroutine ims_initialize

  subroutine ims_print_summary(this)
    class(ImsLinearSolverType) :: this
  end subroutine ims_print_summary

  subroutine ims_solve(this, kiter, rhs, x, cnvg_summary)
    class(ImsLinearSolverType) :: this
    integer(I4B) :: kiter
    class(VectorBaseType), pointer :: rhs
    class(VectorBaseType), pointer :: x
    type(ConvergenceSummaryType) :: cnvg_summary
  end subroutine ims_solve

  subroutine ims_destroy(this)
    class(ImsLinearSolverType) :: this
  end subroutine ims_destroy

  function ims_create_matrix(this) result(matrix)
    class(ImsLinearSolverType) :: this
    class(MatrixBaseType), pointer :: matrix
    ! local
    class(SparseMatrixType), pointer :: ims_matrix

    allocate (ims_matrix)
    matrix => ims_matrix

  end function ims_create_matrix

end module ImsLinearSolverModule

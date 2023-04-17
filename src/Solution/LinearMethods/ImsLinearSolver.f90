module ImsLinearSolverModule
  use KindModule, only: I4B, DP
  use LinearSolverBaseModule
  use MatrixBaseModule
  use VectorBaseModule
  use SparseMatrixModule
  implicit none
  private

  public :: create_ims_solver

  type, public, extends(LinearSolverBaseType) :: ImsLinearSolverType
  contains
    procedure :: initialize => ims_initialize
    procedure :: solve => ims_solve
    procedure :: get_result => ims_get_result
    procedure :: get_l2_norm => ims_get_l2_norm
    procedure :: destroy => ims_destroy

    procedure :: create_matrix => ims_create_matrix
  end type

contains

  function create_ims_solver() result(solver)
    class(LinearSolverBaseType), pointer :: solver
    ! local
    class(ImsLinearSolverType), pointer :: ims_solver

    allocate (ims_solver)
    solver => ims_solver

  end function create_ims_solver

  subroutine ims_initialize(this, matrix)
    class(ImsLinearSolverType) :: this
    class(MatrixBaseType), pointer :: matrix
  end subroutine ims_initialize

  subroutine ims_solve(this, kiter, rhs, x)
    class(ImsLinearSolverType) :: this
    integer(I4B) :: kiter
    class(VectorBaseType), pointer :: rhs
    class(VectorBaseType), pointer :: x
  end subroutine ims_solve

  subroutine ims_get_result(this)
    class(ImsLinearSolverType) :: this
  end subroutine ims_get_result

  function ims_get_l2_norm(this, x, rhs, active) result(l2norm)
    class(ImsLinearSolverType) :: this
    class(VectorBaseType), pointer :: x
    class(VectorBaseType), pointer :: rhs
    integer(I4B), dimension(:), pointer, contiguous :: active
    real(DP) :: l2norm
    l2norm = 0.0_DP
  end function ims_get_l2_norm

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

module ImsLinearSolverModule
  use LinearSolverBaseModule
  use MatrixBaseModule
  use SparseMatrixModule
  implicit none
  private

  public :: create_ims_solver

  type, public, extends(LinearSolverBaseType) :: ImsLinearSolverType
  contains
    procedure :: initialize => ims_initialize
    procedure :: configure => ims_configure
    procedure :: solve => ims_solve
    procedure :: get_result => ims_get_result
    procedure :: destroy => ims_destroy

    procedure :: create_matrix => ims_create_matrix
  end type

contains

function create_ims_solver() result(solver)
  class(LinearSolverBaseType), pointer :: solver
  ! local
  class(ImsLinearSolverType), pointer :: ims_solver

  allocate(ims_solver)
  solver => ims_solver

end function create_ims_solver

subroutine ims_initialize(this)
  class(ImsLinearSolverType) :: this
end subroutine ims_initialize

subroutine ims_configure(this)
  class(ImsLinearSolverType) :: this
end subroutine ims_configure

subroutine ims_solve(this)
  class(ImsLinearSolverType) :: this
end subroutine ims_solve

subroutine ims_get_result(this)
  class(ImsLinearSolverType) :: this
end subroutine ims_get_result

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
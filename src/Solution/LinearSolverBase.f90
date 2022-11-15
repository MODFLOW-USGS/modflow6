module LinearSolverBaseModule
  use KindModule, only: I4B, DP
  use MatrixBaseModule
  use VectorBaseModule
  implicit none
  private

  type, public :: LinearSolverCfg
    integer(I4B) :: linear_accel_type
    real(DP) :: dvclose
  end type LinearSolverCfg

  !> @brief Abstract type for linear solver
  !!
  !! This serves as the base type for our solvers:
  !! sequential, parallel, petsc, block solver, ...
  !<
  type, public, abstract :: LinearSolverBaseType
    integer(I4B) :: iteration_number
    integer(I4B) :: is_converged
  contains
    procedure(initialize_if), deferred :: initialize
    procedure(solve_if), deferred:: solve
    procedure(get_result_if), deferred :: get_result
    procedure(destroy_if), deferred :: destroy

    procedure(create_matrix_if), deferred :: create_matrix
  end type LinearSolverBaseType

  abstract interface
    subroutine initialize_if(this, matrix, cfg)
      import LinearSolverBaseType, MatrixBaseType, LinearSolverCfg
      class(LinearSolverBaseType) :: this
      class(MatrixBaseType), pointer :: matrix
      class(LinearSolverCfg) :: cfg
    end subroutine
    subroutine solve_if(this, kiter, rhs, x)
      import LinearSolverBaseType, I4B, VectorBaseType
      class(LinearSolverBaseType) :: this
      integer(I4B) :: kiter
      class(VectorBaseType), pointer :: rhs
      class(VectorBaseType), pointer :: x
    end subroutine
    subroutine get_result_if(this)
      import LinearSolverBaseType
      class(LinearSolverBaseType) :: this
    end subroutine
    subroutine destroy_if(this)
      import LinearSolverBaseType
      class(LinearSolverBaseType) :: this
    end subroutine
    function create_matrix_if(this) result(matrix)
      import LinearSolverBaseType, MatrixBaseType
      class(LinearSolverBaseType) :: this
      class(MatrixBaseType), pointer :: matrix
    end function
  end interface

end module LinearSolverBaseModule
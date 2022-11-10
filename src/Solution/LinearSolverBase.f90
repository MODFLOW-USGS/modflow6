module LinearSolverBaseModule
  use MatrixBaseModule
  implicit none
  private

  !> @brief Abstract type for linear solver
  !!
  !! This serves as the base type for our solvers:
  !! sequential, parallel, petsc, block solver, ...
  !<
  type, public, abstract :: LinearSolverBaseType
  contains
    procedure(initialize_if), deferred :: initialize
    procedure(configure_if), deferred :: configure
    procedure(solve_if), deferred:: solve
    procedure(get_result_if), deferred :: get_result
    procedure(destroy_if), deferred :: destroy

    procedure(create_matrix_if), deferred :: create_matrix
  end type LinearSolverBaseType

  abstract interface
    subroutine initialize_if(this)
      import LinearSolverBaseType
      class(LinearSolverBaseType) :: this
    end subroutine
    subroutine configure_if(this)
      import LinearSolverBaseType
      class(LinearSolverBaseType) :: this
    end subroutine
    subroutine solve_if(this)
      import LinearSolverBaseType
      class(LinearSolverBaseType) :: this
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
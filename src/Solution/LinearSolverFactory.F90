module LinearSolverFactory
  use SimModule, only: ustop
  use LinearSolverBaseModule
  use MatrixBaseModule
  use SparseMatrixModule
  use VectorBaseModule

#if defined(WITH_PETSC)
  use PetscSolverModule
#endif

  implicit none
  private

  public :: create_linear_solver

contains

  !> @brief Factory method to create the linear solver object
  !<
  function create_linear_solver(solver_mode) result(solver)
    character(len=*) :: solver_mode
    class(LinearSolverBaseType), pointer :: solver

    solver => null()

    if (solver_mode == 'SEQ') then
      solver => null() ! not yet...
      return
    else if (solver_mode == 'PAR') then
#if defined(WITH_PETSC)
      solver => create_petsc_solver()
#endif
    else
      call ustop('Unsupported solver mode: '//trim(solver_mode))
    end if

  end function create_linear_solver

end module LinearSolverFactory
module LinearSolverFactory
  use SimModule, only: ustop
  use LinearSolverBaseModule
  use MatrixBaseModule
  use SparseMatrixModule
  use VectorBaseModule

  use ImsLinearSolverModule
#if defined(__WITH_PETSC__)
  use PetscSolverModule, only: create_petsc_solver
#endif

  implicit none
  private

  public :: create_linear_solver

contains

  !> @brief Factory method to create the linear solver object
  !<
  function create_linear_solver(solver_mode, sln_name) result(solver)
    character(len=*) :: solver_mode
    character(len=*) :: sln_name
    class(LinearSolverBaseType), pointer :: solver

    solver => null()

    if (solver_mode == 'IMS') then
      solver => create_ims_solver(sln_name)
      return
#if defined(__WITH_PETSC__)
    else if (solver_mode == 'PETSC') then
      solver => create_petsc_solver(sln_name)
#endif
    else
      call ustop('Unsupported solver mode: '//trim(solver_mode))
    end if

  end function create_linear_solver

end module LinearSolverFactory

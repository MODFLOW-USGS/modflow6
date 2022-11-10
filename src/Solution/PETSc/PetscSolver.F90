module PetscSolverModule
  use LinearSolverBaseModule
  use MatrixBaseModule
  use PetscMatrixModule

  implicit none
  private

  public :: create_petsc_solver

  type, public, extends(LinearSolverBaseType) :: PetscSolverType
  contains
    procedure :: initialize => petsc_initialize
    procedure :: configure => petsc_configure    
    procedure :: solve => petsc_solve
    procedure :: get_result => petsc_get_result
    procedure :: destroy => petsc_destroy

    procedure :: create_matrix => petsc_create_matrix
  end type PetscSolverType

contains

  function create_petsc_solver() result(solver)
    class(LinearSolverBaseType), pointer :: solver
    ! local
    class(PetscSolverType), pointer :: petsc_solver

    allocate(petsc_solver)
    solver => petsc_solver

  end function create_petsc_solver

  subroutine petsc_initialize(this)
    class(PetscSolverType) :: this
  end subroutine petsc_initialize

  subroutine petsc_configure(this)
    class(PetscSolverType) :: this
  end subroutine petsc_configure

  subroutine petsc_solve(this)
    class(PetscSolverType) :: this
  end subroutine petsc_solve

  subroutine petsc_get_result(this)
    class(PetscSolverType) :: this
  end subroutine petsc_get_result

  subroutine petsc_destroy(this)
    class(PetscSolverType) :: this
  end subroutine petsc_destroy

  function petsc_create_matrix(this) result(matrix)
    class(PetscSolverType) :: this
    class(MatrixBaseType), pointer :: matrix
    ! local
    class(PetscMatrixType), pointer :: petsc_matrix

    allocate (petsc_matrix)
    matrix => petsc_matrix

  end function petsc_create_matrix

end module PetscSolverModule
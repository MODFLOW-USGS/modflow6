module ParallelSolutionModule
  use NumericalSolutionModule, only: NumericalSolutionType
  implicit none
  private

  public :: ParallelSolutionType

  type, extends(NumericalSolutionType) :: ParallelSolutionType
  end type ParallelSolutionType

end module ParallelSolutionModule

module SolutionFactoryModule
  use KindModule, only: I4B
  use SimModule, only: ustop
  use BaseSolutionModule
  use NumericalSolutionModule, only: NumericalSolutionType, &
                                     create_numerical_solution
  use ExplicitSolutionModule, only: ExplicitSolutionType, &
                                    create_explicit_solution
#if defined(__WITH_MPI__)
  use ParallelSolutionModule, only: ParallelSolutionType
#endif

  implicit none
  private

  public :: create_ims_solution
  public :: create_ems_solution

contains

  !> @brief Create an IMS solution of type NumericalSolution
  !! for serial runs or its sub-type ParallelSolution for
  !< parallel runs. Returns the base pointer.
  function create_ims_solution(sim_mode, filename, sol_id) result(base_sol)
    character(len=*) :: sim_mode
    character(len=*) :: filename
    integer(I4B) :: sol_id
    class(BaseSolutionType), pointer :: base_sol
    class(NumericalSolutionType), pointer :: num_sol => null()
#if defined(__WITH_MPI__)
    class(ParallelSolutionType), pointer :: par_sol => null()
#endif

    if (sim_mode == 'SEQUENTIAL') then
      allocate (num_sol)
#if defined(__WITH_MPI__)
    else if (sim_mode == 'PARALLEL') then
      allocate (par_sol)
      num_sol => par_sol
#endif
    else
      call ustop('Unsupported simulation mode for creating solution: '&
                 &//trim(sim_mode))
    end if

    call create_numerical_solution(num_sol, filename, sol_id)
    base_sol => num_sol

  end function create_ims_solution

  !> @brief Create an EMS solution of type ExplicitSolution
  !! for serial runs or its sub-type ParallelSolution for
  !< parallel runs. Returns the base pointer.
  function create_ems_solution(sim_mode, filename, sol_id) result(base_sol)
    character(len=*) :: sim_mode
    character(len=*) :: filename
    integer(I4B) :: sol_id
    class(BaseSolutionType), pointer :: base_sol
    class(ExplicitSolutionType), pointer :: exp_sol => null()
!#if defined(__WITH_MPI__)
!    class(ParallelSolutionType), pointer :: par_sol => null()
!#endif

    if (sim_mode == 'SEQUENTIAL') then
      allocate (exp_sol)
!#if defined(__WITH_MPI__)
!    else if (sim_mode == 'PARALLEL') then
!      allocate (par_sol)
!      exp_sol => par_sol
!#endif
    else
      call ustop('Unsupported simulation mode for creating solution: '&
                 &//trim(sim_mode))
    end if

    call create_explicit_solution(exp_sol, filename, sol_id)
    base_sol => exp_sol

  end function create_ems_solution

end module

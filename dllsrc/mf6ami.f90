! this module contains entry points for the mf6 dll to expose functionality
! that is _beyond_ the basic model interface: https://bmi-spec.readthedocs.io/en/latest/
module mf6ami
  use mf6core
  use KindModule
  use bmif, only: BMI_SUCCESS, BMI_FAILURE
  use iso_c_binding, only: c_int
  implicit none
 
  ! this is the counter for the outer iteration loop,
  ! it is initialized in prepare_iteration
  integer(I4B), pointer :: iterationCounter => null()
    
  contains
  
  function ami_prepare_timestep() result(bmi_status) bind(C, name="prepare_timestep")
  !DEC$ ATTRIBUTES DLLEXPORT :: ami_prepare_timestep
    integer(kind=c_int) :: bmi_status
      
    call prepareTimestep()    
    bmi_status = BMI_SUCCESS
    
  end function ami_prepare_timestep
    
  function ami_finalize_timestep() result(bmi_status) bind(C, name="finalize_timestep")
  !DEC$ ATTRIBUTES DLLEXPORT :: ami_finalize_timestep
    integer(kind=c_int) :: bmi_status
    ! local
    logical :: hasConverged
    
    hasConverged = finalizeTimestep()
    if (hasConverged) then
      bmi_status = BMI_SUCCESS
    else
      bmi_status = BMI_FAILURE
    end if
    
  end function ami_finalize_timestep
  
  ! returns the number of NumericalSolutions in the simulation. 
  ! It works if there is only one SolutionGroup used.
  function ami_get_subcomponent_count(count) result(bmi_status) bind(C, name="get_subcomponent_count")
  !DEC$ ATTRIBUTES DLLEXPORT :: ami_get_subcomponent_count
    use ListsModule, only: solutiongrouplist
    use SimVariablesModule, only: iout
    integer(kind=c_int) :: bmi_status
    integer(kind=c_int), intent(out) :: count
    ! local
    class(SolutionGroupType), pointer :: sgp
    
    ! TODO_MJR: this goes for all calls, move to init I guess...
    if (solutiongrouplist%Count() /= 1) then
      write(iout,*) 'multiple solution groups not supported'
      count = -1      
      bmi_status = BMI_FAILURE
      return
    end if 
    
    sgp => GetSolutionGroupFromList(solutiongrouplist, 1)    
    count = sgp%nsolutions    
    bmi_status = BMI_SUCCESS
    
  end function ami_get_subcomponent_count
    
  ! this prepares for running a single outer iteration on the 
  ! specific subcomponent (=NumericalSolution)
  function ami_prepare_iteration(subcomponent_idx) result(bmi_status) bind(C, name="prepare_iteration")
  !DEC$ ATTRIBUTES DLLEXPORT :: ami_prepare_iteration   
    use NumericalSolutionModule
    integer(kind=c_int) :: subcomponent_idx ! 1,2,...,ami_get_subcomponent_count()
    integer(kind=c_int) :: bmi_status
    ! local
    class(NumericalSolutionType), pointer :: ns
    
    ! get the numerical solution we are running
    ns => getSolution(subcomponent_idx)
    
    ! prepare with defaults, i.e. no subtiming and picard
    call ns%prepareIteration(1, 1)
    
    ! reset counter
    allocate(iterationCounter)
    iterationCounter = 0
    
    bmi_status = BMI_SUCCESS
    
  end function ami_prepare_iteration
  
  ! execute a single outer iteration on the specified 
  ! subcomponent (=NumericalSolution)
  function ami_do_iteration(subcomponent_idx, has_converged) result(bmi_status) bind(C, name="do_iteration")
  !DEC$ ATTRIBUTES DLLEXPORT :: ami_do_iteration   
    use NumericalSolutionModule
    integer(kind=c_int), intent(in) :: subcomponent_idx ! 1,2,...,ami_get_subcomponent_count()
    integer(kind=c_int), intent(out) :: has_converged
    integer(kind=c_int) :: bmi_status
    ! local
    class(NumericalSolutionType), pointer :: ns
    
    ! get the numerical solution we are running
    ns => getSolution(subcomponent_idx)
    
    ! execute the nth iteration    
    iterationCounter = iterationCounter + 1
    call ns%doIteration(iterationCounter)
        
    ! the following check is equivalent to that in NumericalSolution%sln_ca
    if (ns%icnvg == 1) then
      has_converged = 1
    else
      has_converged = 0
    end if
    
    bmi_status = BMI_SUCCESS
    
  end function ami_do_iteration
  
  ! after the convergence loop on the subcomponent is done,
  ! call this to report, clean up, etc.
  function ami_finalize_iteration(subcomponent_idx) result(bmi_status) bind(C, name="finalize_iteration")
  !DEC$ ATTRIBUTES DLLEXPORT :: ami_finalize_iteration   
    use NumericalSolutionModule
    integer(kind=c_int), intent(in) :: subcomponent_idx ! 1,2,...,ami_get_subcomponent_count()
    integer(kind=c_int) :: bmi_status
    ! local
    class(NumericalSolutionType), pointer :: ns
    integer(I4B) :: hasConverged
    
    ! get the numerical solution we are running
    ns => getSolution(subcomponent_idx)
    
    ! hasConverged is equivalent to the isgcnvg variable which is initialized to 1, 
    ! see the body of the picard loop in SolutionGroupType%sgp_ca
    hasConverged = 1
    
    ! finish up
    call ns%finalizeIteration(iterationCounter, hasConverged, 1, 0)
    
    ! check convergence on solution
    if (hasConverged == 1) then
      bmi_status = BMI_SUCCESS
    else
      bmi_status = BMI_FAILURE
    end if
    
    ! clear this for safety
    deallocate(iterationCounter)
    
  end function ami_finalize_iteration
  
  ! the subcomponent_idx runs from 1 to the nr of 
  ! solutions in the solution group
  function getSolution(subcomponent_idx) result(solution)
    use SolutionGroupModule
    use NumericalSolutionModule
    use ListsModule, only: basesolutionlist, solutiongrouplist
    integer(I4B), intent(in) :: subcomponent_idx
    class(NumericalSolutionType), pointer :: solution
    ! local
    class(SolutionGroupType), pointer :: sgp
    integer(I4B) :: solutionIdx
    
    ! this is equivalent to how it's done in sgp_ca
    sgp => GetSolutionGroupFromList(solutiongrouplist, 1)
    solutionIdx = sgp%idsolutions(subcomponent_idx)
    solution => GetNumericalSolutionFromList(basesolutionlist, solutionIdx)    
    
  end function getSolution
  
end module mf6ami
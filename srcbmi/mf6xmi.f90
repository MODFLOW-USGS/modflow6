! this module contains entry points for the mf6 dll to expose functionality
! that is _beyond_ the basic model interface: https://bmi-spec.readthedocs.io/en/latest/
module mf6xmi
  use Mf6CoreModule
  use KindModule
  use bmif, only: BMI_SUCCESS, BMI_FAILURE
  use iso_c_binding, only: c_int
  implicit none
 
  ! this is the counter for the outer iteration loop,
  ! it is initialized in prepare_iteration
  integer(I4B), pointer :: iterationCounter => null()
    
  contains
  
  function xmi_prepare_time_step(dt) result(bmi_status) bind(C, name="prepare_time_step")
  !DEC$ ATTRIBUTES DLLEXPORT :: xmi_prepare_time_step
    double precision, intent(in) :: dt ! we cannot set the timestep (yet), ignore for now
    integer(kind=c_int) :: bmi_status
      
    call Mf6PrepareTimestep()    
    bmi_status = BMI_SUCCESS
    
  end function xmi_prepare_time_step
    
  function xmi_do_time_step() result(bmi_status) bind(C, name="do_time_step")
  !DEC$ ATTRIBUTES DLLEXPORT :: xmi_do_time_step
    integer(kind=c_int) :: bmi_status
    
    call Mf6DoTimestep()
    bmi_status = BMI_SUCCESS
    
  end function xmi_do_time_step
  
  function xmi_finalize_time_step() result(bmi_status) bind(C, name="finalize_time_step")
  !DEC$ ATTRIBUTES DLLEXPORT :: xmi_finalize_time_step
    integer(kind=c_int) :: bmi_status
    ! local
    logical :: hasConverged
    
    hasConverged = Mf6FinalizeTimestep()
    if (hasConverged) then
      bmi_status = BMI_SUCCESS
    else
      bmi_status = BMI_FAILURE
    end if
    
  end function xmi_finalize_time_step
  
  ! returns the number of NumericalSolutions in the simulation. 
  ! It works if there is only one SolutionGroup used.
  function xmi_get_subcomponent_count(count) result(bmi_status) bind(C, name="get_subcomponent_count")
  !DEC$ ATTRIBUTES DLLEXPORT :: xmi_get_subcomponent_count
    use ListsModule, only: solutiongrouplist
    use SimVariablesModule, only: istdout
    integer(kind=c_int) :: bmi_status
    integer(kind=c_int), intent(out) :: count
    ! local
    class(SolutionGroupType), pointer :: sgp
    
    ! the following is true for all calls at this level (subcomponent)
    if (solutiongrouplist%Count() /= 1) then
      write(istdout,*) 'Error: BMI does not support the use of multiple solution groups'
      count = -1      
      bmi_status = BMI_FAILURE
      return
    end if 
    
    sgp => GetSolutionGroupFromList(solutiongrouplist, 1)
    count = sgp%nsolutions    
    bmi_status = BMI_SUCCESS
    
  end function xmi_get_subcomponent_count
    
  ! this prepares for running a loop over outer iterations 
  ! on the specific subcomponent (=NumericalSolution)
  function xmi_prepare_solve(subcomponent_idx) result(bmi_status) bind(C, name="prepare_solve")
  !DEC$ ATTRIBUTES DLLEXPORT :: xmi_prepare_solve   
    use ListsModule, only: solutiongrouplist
    use NumericalSolutionModule
    use SimVariablesModule, only: istdout
    integer(kind=c_int) :: subcomponent_idx ! 1,2,...,xmi_get_subcomponent_count()
    integer(kind=c_int) :: bmi_status
    ! local
    class(NumericalSolutionType), pointer :: ns
    
     ! people might not call 'xmi_get_subcomponent_count' first, so let's repeat this:
    if (solutiongrouplist%Count() /= 1) then
      write(istdout,*) 'Error: BMI does not support the use of multiple solution groups'
      bmi_status = BMI_FAILURE
      return
    end if 
    
    ! get the numerical solution we are running
    ns => getSolution(subcomponent_idx)
    
    ! *_ad (model, exg, sln)
    call ns%prepareSolve()
    
    ! reset counter
    allocate(iterationCounter)
    iterationCounter = 0
    
    bmi_status = BMI_SUCCESS
    
  end function xmi_prepare_solve
  
  ! execute a single outer iteration on the specified 
  ! subcomponent (=NumericalSolution)
  function xmi_solve(subcomponent_idx, has_converged) result(bmi_status) bind(C, name="solve")
  !DEC$ ATTRIBUTES DLLEXPORT :: xmi_solve  
    use NumericalSolutionModule
    integer(kind=c_int), intent(in) :: subcomponent_idx ! 1,2,...,xmi_get_subcomponent_count()
    integer(kind=c_int), intent(out) :: has_converged
    integer(kind=c_int) :: bmi_status
    ! local
    class(NumericalSolutionType), pointer :: ns
    
    ! get the numerical solution we are running
    ns => getSolution(subcomponent_idx)
    
    ! execute the nth iteration    
    iterationCounter = iterationCounter + 1
    call ns%solve(iterationCounter)
        
    ! the following check is equivalent to that in NumericalSolution%sln_ca
    if (ns%icnvg == 1) then
      has_converged = 1
    else
      has_converged = 0
    end if
    
    bmi_status = BMI_SUCCESS
    
  end function xmi_solve
  
  ! after the outer iteration loop on the subcomponent is exited,
  ! call this to report, clean up, etc.
  function xmi_finalize_solve(subcomponent_idx) result(bmi_status) bind(C, name="finalize_solve")
  !DEC$ ATTRIBUTES DLLEXPORT :: xmi_finalize_solve   
    use NumericalSolutionModule
    integer(kind=c_int), intent(in) :: subcomponent_idx ! 1,2,...,xmi_get_subcomponent_count()
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
    call ns%finalizeSolve(iterationCounter, hasConverged, 0)
    
    ! check convergence on solution
    if (hasConverged == 1) then
      bmi_status = BMI_SUCCESS
    else
      bmi_status = BMI_FAILURE
    end if
    
    ! clear this for safety
    deallocate(iterationCounter)
    
  end function xmi_finalize_solve
  
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

end module mf6xmi
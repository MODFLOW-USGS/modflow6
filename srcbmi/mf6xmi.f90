!> @brief This module contains the eXtended Model Interface
!!
!! In this module we expose functionality in the MODFLOW 6 shared library, 
!! that is _beyond_ the basic model interface: https://bmi-spec.readthedocs.io/en/latest/.
!! The main extensions are
!! 
!! - Controlling the kernel at a finer granularity, isolating the call to the linear solve 
!!   of the system. This way, the interface can be used to set up a non-linear coupling
!!   with, for example, an external unsaturated zone model.
!! - Expose the concept of subcomponents, which in case of MODFLOW 6 are 'Numerical Solution'
!!   objects, each of which represents a separate linear system to solve. An example here
!!   would be a transport model (GWT) coupled to a groundwater model (GWF).
!!
!! The common BMI control flow is
!!
!! ~~~{.py}
!! initialize()
!!
!! while t < t_end:
!!   update()
!!
!! finalize()
!! ~~~
!!
!! With the XMI you can now also use it as:
!!
!! ~~~{.py}
!! initialize()
!!
!! while(t < t_end):
!! 
!!   prepare_time_step()
!!   
!!   # modify some values here
!!  
!!   do_time_step()
!!    
!!   # and maybe something here as well
!!
!!   finalize_time_step()
!!
!! finalize()
!! ~~~
!!
!! Or, when you want to isolate the call to the linear solve, a
!! typical application could look like this:
!!
!! ~~~{.py}
!! initialize()
!!
!! while t < t_end:
!! 
!!   prepare_time_step()
!!   
!!   for i_sol in solutions:
!!
!!     prepare_solve(i_sol)
!!       
!!       while k_iter < max_iter:
!!
!!         # exchange coupled variables
!!         exchange_data()
!!
!!         # the MODFLOW linear solve:
!!         solve()
!!
!!         # maybe solve some other, external model here:
!!         solveExternalModel()
!!
!!         # and exchange back
!!         exchange_data()
!!         
!!         # check for convergence        
!!         convergence_check()
!!
!!     finalize_solve(i_sol)
!!
!!   finalize_time_step()
!!
!! finalize()
!! ~~~
!!
!! Note that the last example can only work when there is a single Solution Group defined. 
!! This will typically not be a problem, though, as applications with multiple Solution Groups
!! should be quite rare.
!<
module mf6xmi
  use mf6bmi
  use mf6bmiUtil
  use Mf6CoreModule
  use KindModule
  use bmif, only: BMI_SUCCESS, BMI_FAILURE
  use iso_c_binding, only: c_int, c_char  
  implicit none
 
  
  integer(I4B), pointer :: iterationCounter => null() !< the counter for the outer iteration loop, initialized in xmi_prepare_iteration()
    
  contains
  
  !> @brief Prepare a single time step
  !!
  !! The routine takes the time step \p dt as an argument. However, MODFLOW (currently)
  !! does not allow to alter this value after initialization, so it is ignored
  !! here.
  !! 
  !! @param[in]   dt              the time step
  !! @return      bmi_status      the BMI status code
  !<
  function xmi_prepare_time_step(dt) result(bmi_status) bind(C, name="prepare_time_step")
  !DEC$ ATTRIBUTES DLLEXPORT :: xmi_prepare_time_step
    double precision, intent(in) :: dt
    integer(kind=c_int) :: bmi_status
      
    call Mf6PrepareTimestep()    
    bmi_status = BMI_SUCCESS
    
  end function xmi_prepare_time_step
    
  !> @brief Perform a single time step
  !!
  !! It does so by looping over all solution groups, and calling
  !! the calculate function on all solutions in there.
  !! 
  !! @return      bmi_status      the BMI status code
  !<
  function xmi_do_time_step() result(bmi_status) bind(C, name="do_time_step")
  !DEC$ ATTRIBUTES DLLEXPORT :: xmi_do_time_step
    integer(kind=c_int) :: bmi_status
    
    call Mf6DoTimestep()
    bmi_status = BMI_SUCCESS
    
  end function xmi_do_time_step
  
  !> @brief Finalize the time step
  !!
  !! This will mostly write output and messages. It is essential
  !! to call this to finish the time step.
  !!
  !! @return      bmi_status      the BMI status code
  !<
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
  
  !> @brief This will get the number of Numerical Solutions in the simulation
  !!
  !! For most applications, this number will be equal to 1. Note that this part 
  !! of the XMI only works when the simulation is defined with a single
  !! Solution Group. (If you don't know what a Solution Group is, then
  !! you are most likely not using more than one...)
  !! 
  !! @param[out]  count           the number of solutions
  !! @return      bmi_status      the BMI status code    
  !<
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
    
  !> @brief Prepare for solving the system
  !!
  !! This preparation mostly consists of advancing the solutions, models, and exchanges 
  !! in the simulation. The index \p subcomponent_idx runs from 1 to the value returned 
  !! by xmi_get_subcomponent_count().
  !!
  !! @param[in]   subcomponent_idx    the index of the subcomponent (Numerical Solution)
  !! @return      bmi_status          the BMI status code  
  !<
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
  
  !> @brief Build and solve the linear system
  !!
  !! The solve is called on the Numerical Solution indicated by the value of \p subcomponent_idx,
  !! which runs from 1 to the value returned by xmi_get_subcomponent_count(). Before calling
  !! this, a matching call to xmi_prepare_solve() should be done.
  !!
  !! @param[in]   subcomponent_idx    the index of the subcomponent (Numerical Solution)
  !! @param[out]  has_converged       equal to 1 for convergence, 0 otherwise
  !! @return      bmi_status          the BMI status code
  !<
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
    
  !> @brief Finalize the solve of the system
  !!
  !! This will determine convergence, reports, calculate flows and budgets, and more... 
  !! It should always follow after a call to xmi_prepare_solve() and xmi_solve().
  !!
  !! @param[in]   subcomponent_idx    the index of the subcomponent (Numerical Solution)
  !! @return      bmi_status          the BMI status code
  !<
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
  
  !> @brief Get the full address string for a variable
  !!
  !! This routine constructs the full address string of a variable using the
  !! exact same logic as the internal memory manager. This routine
  !! should always be used when accessing a variable through the BMI as
  !! to assure compatibility with future versions of the library.
  !!
  !! @param[in]   c_component_name        the name of the component (a Model or Solution) as a C-string
  !! @param[in]   c_subcomponent_name     the name of the subcomponent (a Package) as a C-string (or an empty string '' when not applicable)
  !! @param[in]   c_var_name              the name of the variable as a C-string
  !! @param[out]  c_var_address           the full address of the variable as a C-string with length mf6bmiUtil::BMI_LENVARADDRESS
  !! @return      bmi_status              the BMI status code
  !<
  function get_var_address(c_component_name, c_subcomponent_name, &
                          c_var_name, c_var_address) &
                          result(bmi_status) bind(C, name="get_var_address")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_var_address
    use MemoryHelperModule, only: create_mem_path, create_mem_address
    use ConstantsModule, only: LENCOMPONENTNAME, LENVARNAME, LENMEMPATH, LENMEMADDRESS    
    character(kind=c_char), intent(in) :: c_component_name(*)
    character(kind=c_char), intent(in) :: c_subcomponent_name(*)
    character(kind=c_char), intent(in) :: c_var_name(*)
    character(kind=c_char), intent(out) :: c_var_address(BMI_LENVARADDRESS)
    integer(kind=c_int) :: bmi_status
 
    ! local
    character(len=LENCOMPONENTNAME) :: component_name
    character(len=LENCOMPONENTNAME) :: subcomponent_name
    character(len=LENVARNAME) :: variable_name
    character(len=LENMEMPATH) :: mem_path
    character(len=LENMEMADDRESS) :: mem_address

    ! convert to Fortran strings
    component_name = char_array_to_string(c_component_name, strlen(c_component_name)) 
    subcomponent_name = char_array_to_string(c_subcomponent_name, strlen(c_subcomponent_name))
    variable_name = char_array_to_string(c_var_name, strlen(c_var_name))

    ! create memory address
    if (subcomponent_name == '') then
      mem_path = create_mem_path(component_name)
    else
      mem_path = create_mem_path(component_name, subcomponent_name)
    end if
    mem_address = create_mem_address(mem_path, variable_name)

    ! convert to c string:
    c_var_address(1:len(trim(mem_address))+1) = string_to_char_array(trim(mem_address), len(trim(mem_address)))

    bmi_status = BMI_SUCCESS

  end function get_var_address

end module mf6xmi
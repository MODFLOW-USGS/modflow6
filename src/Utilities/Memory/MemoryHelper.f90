module MemoryHelperModule
  use ConstantsModule, only: LENMEMPATH, LENMEMSEPARATOR, LENMEMADDRESS

  implicit none
  private

  public :: create_mem_path, create_mem_address

  character(len=LENMEMSEPARATOR), parameter :: memPathSeparator = '/'   !<used to build up the memory address for the stored variables

contains

  !> @brief returns the path to the memory object
  !!
  !! Returns the path to the location in the memory manager where
  !! the variables for this (sub)component are stored, the 'memoryPath' 
  !!
  !! @param[in]   component       the name of the solution, model, or exchange
  !! @param[in]   subcomponent    the name of the package (optional)
  !! @return      memory_path      the path to the memory object
  !!
  !! NB: no need to trim the input parameters
  !<
  function create_mem_path(component, subcomponent) result(memory_path)
    character(len=*), intent(in) :: component
    character(len=*), intent(in), optional :: subcomponent
    character(len=LENMEMPATH) :: memory_path
    
    ! TODO_MJR: add check on lenghts, see check_var_name in mem mng
    
    if (present(subcomponent)) then
      memory_path = trim(component) // memPathSeparator // trim(subcomponent)    
    else
      memory_path = trim(component)
    end if
    
  end function create_mem_path

  !> @brief returns the address string of the memory object
  !!
  !! Returns the memory address, i.e. the full path plus name of the stored variable
  !!
  !! @param[in]   mem_path       the path to the memory object
  !! @param[in]   var_name       the name of the stored variable
  !! @return      mem_address    the full address string to the memory object
  !!
  !! NB: no need to trim the input parameters
  !<
  function create_mem_address(mem_path, var_name) result(mem_address)
    character(len=*), intent(in) :: mem_path
    character(len=*), intent(in) :: var_name
    character(len=LENMEMADDRESS) :: mem_address

    ! TODO_MJR: add check on lenghts, see check_var_name in mem mng

    mem_address = trim(mem_path) // memPathSeparator // trim(var_name) 

  end function create_mem_address


end module MemoryHelperModule
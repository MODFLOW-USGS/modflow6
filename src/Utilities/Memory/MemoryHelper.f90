module MemoryHelperModule
  use KindModule, only: I4B
  use ConstantsModule, only: LENMEMPATH, LENMEMSEPARATOR, LENMEMADDRESS, LENVARNAME, LENCOMPONENTNAME
  use SimModule, only: store_error, ustop
  use SimVariablesModule,     only: errmsg

  implicit none
  private

  public :: create_mem_path, create_mem_address, mem_check_length, memPathSeparator

  character(len=LENMEMSEPARATOR), parameter :: memPathSeparator = '/' !< used to build up the memory address for the stored variables

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
    
    call mem_check_length(component, LENCOMPONENTNAME, "solution/model/exchange")
    call mem_check_length(subcomponent, LENCOMPONENTNAME, "package")  
    
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

    call mem_check_length(mem_path, LENMEMPATH, "memory path")
    call mem_check_length(var_name, LENVARNAME, "variable")

    mem_address = trim(mem_path) // memPathSeparator // trim(var_name) 

  end function create_mem_address  

  !> @brief Generic routine to check the length of (parts of) the memory address
  !!
  !! @warning{if the length exceeds the maximum, a message is recorded 
  !! and the program will be stopped}
  !!
  !! @param[in]   name          the string to be checked
  !! @param[in]   max_length    the maximum length
  !! @param[in]   description   a descriptive string
  !!
  !! The description should describe the part of the address that is checked
  !! (variable, package, model, solution, exchange name) or the full memory path
  !! itself
  !<
  subroutine mem_check_length(name, max_length, description)
    character(len=*), intent(in) :: name
    integer(I4B), intent(in)     :: max_length
    character(len=*), intent(in) :: description
    
    if(len(name) > max_length) then
      write(errmsg, '(*(G0))')                                                   &
        'Fatal error in Memory Manager, length of ', description, ' must be ',   &
        max_length, ' characters or less: ', name

      ! -- store error and stop program execution
      call store_error(errmsg)
      call ustop()
    end if

  end subroutine mem_check_length

end module MemoryHelperModule
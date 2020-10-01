module MemorySetHandlerModule 
  use KindModule, only: I4B, LGP 
  use ListModule, only: ListType  
  use MemoryTypeModule, only: MemoryType
  use MemoryManagerModule, only: get_from_memorylist
  use ConstantsModule, only: LENMEMPATH, LENVARNAME

  implicit none
  private

  public :: set_handler_iface
  public :: mem_register_handler
  public :: on_memory_set

  type EventHandlerDataType
    procedure(set_handler_iface), pointer, nopass :: handler => null()
    class(*), pointer :: handlerContext => null()
  end type

  type(ListType) :: handler_list

  abstract interface
    subroutine set_handler_iface(owner)
      class(*), pointer :: owner
    end subroutine
  end interface

  contains

  !> @brief Register the event handler and context for this variable
  !!
  !! The event handler and its ctx are called whenever the trigger
  !! is given by calling @p on_set_memory(). This allows to handle 
  !! side effects, e.g. when a variable is from outside a class 
  !! (the context) such as happens with the BMI.
  !<
  subroutine mem_register_handler(var_name, mem_path, handler, ctx)
    character(len=LENVARNAME), intent(out) :: var_name  !< the variable name
    character(len=LENMEMPATH), intent(out) :: mem_path  !< the memory path
    procedure(set_handler_iface), pointer :: handler    !< called after memory is set
    class(*), pointer :: ctx                            !< the context with which the handler should be called
    ! local
    integer(I4B) :: handler_idx
    class(EventHandlerDataType), pointer :: handler_data => null()
    class(*), pointer :: handler_data_genptr
    type(MemoryType), pointer :: mt
    logical(LGP) :: found

    ! first store the handler data
    allocate(handler_data)
    handler_data%handler => handler
    handler_data%handlerContext => ctx

    handler_data_genptr => handler_data
    call handler_list%Add(handler_data_genptr)

    ! this is the index for the current handler
    handler_idx = handler_list%Count()

    ! now set it to the memory item
    mt => null()
    found = .false.
    call get_from_memorylist(var_name, mem_path, mt, found)
    mt%set_handler_idx = handler_idx

  end subroutine

  !> @brief Triggers the calling of the side effect handler for this variable
  !!
  !! The handler can be set by calling @p mem_register_handler()
  !<
  subroutine on_memory_set(var_name, mem_path)
    character(len=LENVARNAME), intent(out) :: var_name  !< the variable name
    character(len=LENMEMPATH), intent(out) :: mem_path  !< the memory path
    ! local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    class(*), pointer :: handler_data_genptr => null()
    class(EventHandlerDataType), pointer :: evt_handler_data => null()

    ! get the handler data and cast
    mt => null()
    found = .false.
    call get_from_memorylist(var_name, mem_path, mt, found)
    handler_data_genptr => handler_list%GetItem(mt%set_handler_idx)
    select type(handler_data_genptr)
    class is (EventHandlerDataType)
      evt_handler_data => handler_data_genptr
    end select
  
    ! call the function
    call evt_handler_data%handler(evt_handler_data%handlerContext)

  end subroutine

end module
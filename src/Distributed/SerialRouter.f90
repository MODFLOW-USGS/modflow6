module SerialRouterModule  
  use RouterBaseModule  
  use KindModule, only: I4B
  use VirtualSolutionModule
  implicit none
  private

  public :: create_serial_router

  type, public, extends(RouterBaseType) :: SerialRouterType
  contains
    procedure :: route => sr_route
  end type SerialRouterType

contains

  !> Factory method to create serial router
  !<
  function create_serial_router() result(router)
    class(RouterBaseType), pointer :: router
    ! local
    class(SerialRouterType), pointer :: serial_router

    allocate(serial_router)
    router => serial_router

  end function create_serial_router

  subroutine sr_route(this, virtual_sol, stage)
    class(SerialRouterType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage

  end subroutine sr_route

end module SerialRouterModule
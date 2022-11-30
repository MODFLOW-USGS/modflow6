module RouterBaseModule
  use KindModule, only: I4B
  use VirtualSolutionModule 
  implicit none
  private

  type, abstract, public :: RouterBaseType

  contains
    procedure(route_if), deferred :: route
  end type RouterBaseType

  abstract interface
    subroutine route_if(this, virtual_sol, stage)
      import RouterBaseType, VirtualSolutionType, I4B
      class(RouterBaseType) :: this
      type(VirtualSolutionType) :: virtual_sol
      integer(I4B) :: stage
    end subroutine route_if
  end interface

end module RouterBaseModule
module MeshConnectionModule
  use KindModule, only: I4B
  
  implicit none
  private
  
  type, public :: MeshConnection
    integer(I4B), dimension(:), pointer, contiguous :: localNodes => null()
    integer(I4B), dimension(:), pointer, contiguous :: connectedNodes => null()
  contains
    procedure, pass(this) :: construct
    procedure, pass(this) :: addConnection
    procedure, pass(this) :: getLocalNodes
    procedure, pass(this) :: getConnectedNodes
  end type
  
contains ! module procedures

  subroutine construct(this, nrOfConnections)
    class(MeshConnection), intent(in) :: this
    integer(I4B) :: nrOfConnections
    
    ! TODO_MJR
    
  end subroutine
  
  subroutine addConnection(this, n, m)
    class(MeshConnection), intent(in) :: this
    integer(I4B) :: n, m
    
    ! TODO_MJR
    
  end subroutine addConnection
    
  function getLocalNodes(this) result(nds)
    class(MeshConnection), intent(in) :: this
    integer(I4B), dimension(:), pointer :: nds
    
    ! TODO_MJR
    
  end function getLocalNodes
    
  function getConnectedNodes(this) result(connNds)
    class(MeshConnection), intent(in) :: this
    integer(I4B), dimension(:), pointer :: connNds
    
    ! TODO_MJR
    
  end function getConnectedNodes
  
end module MeshConnectionModule
module MeshConnectionModule
  use KindModule, only: I4B
  use ConstantsModule, only: LENORIGIN
  
  implicit none
  private
  
  type, public :: MeshConnectionType
    character(len=LENORIGIN) :: memOrigin
    integer(I4B), pointer :: nrOfConnections => null() ! TODO_MJR: probably we should call them 'links' or so, less confusing
    integer(I4B), dimension(:), pointer, contiguous :: localNodes => null()
    integer(I4B), dimension(:), pointer, contiguous :: connectedNodes => null()
  contains
    procedure, pass(this) :: construct
    procedure, private, pass(this) :: allocateScalars, allocateArrays
    procedure, pass(this) :: addConnection
  end type
  
contains ! module procedures

  ! note: constructing object allocates data structures
  subroutine construct(this, nConnections, connectionName)   
    class(MeshConnectionType), intent(inout) :: this
    integer(I4B) :: nConnections
    character(len=*) :: connectionName
        
    this%memOrigin = trim(connectionName)//'_MC'
    call this%allocateScalars()
    call this%allocateArrays(nConnections) 
    
    this%nrOfConnections = nConnections    
    this%localNodes = -1
    this%connectedNodes = -1
     
  end subroutine
  
  ! add connection between node n and m (global ids)
  subroutine addConnection(this, idx, n, m)
    class(MeshConnectionType), intent(in) :: this
    integer(I4B) :: n, m, idx
    
    this%localNodes(idx) = n
    this%connectedNodes(idx) = m
    
  end subroutine addConnection
  
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(MeshConnectionType), intent(in) :: this
      
    call mem_allocate(this%nrOfConnections, 'NRCONN', this%memOrigin)
    
  end subroutine allocateScalars
  
  subroutine allocateArrays(this, nConns)
    use MemoryManagerModule, only: mem_allocate
    class(MeshConnectionType), intent(in) :: this
    integer(I4B) :: nConns
    
    call mem_allocate(this%localNodes, nConns, 'LNODES', this%memOrigin)
    call mem_allocate(this%connectedNodes, nConns, 'CNODES', this%memOrigin)
    
  end subroutine allocateArrays
  
end module MeshConnectionModule
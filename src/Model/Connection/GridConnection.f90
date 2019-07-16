module GridConnectionModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENORIGIN
  use NumericalModelModule, only: NumericalModelType
  
  implicit none
  private
  
  ! link with node in other model
  ! TODO_MJR: not linkednode, but nodelink... rename
  type, public :: LinkedNodeType   
    integer(I4B)  :: ownIndex
    integer(I4B)  :: linkedIndex    ! linked node, local numbering
    integer(I4B)  :: connectionType ! 0 = vertical, 1 = horizontal, 2 = vertically staggered
    real(DP)      :: length1        ! connection length from n to face between n and m
    real(DP)      :: length2        ! connection length from m to face between n and m
    real(DP)      :: hwva           ! horizontal width (when connectionType == 1), or vertical area (when connectionType == 0)
    class(NumericalModelType), pointer :: connectedModel
    ! TODO_MJR: extend with nbrs-of-nbrs
  end type
  
  type, public :: GridConnectionType
    character(len=LENORIGIN) :: memOrigin
    integer(I4B), pointer :: nrOfLinks => null()
    type(LinkedNodeType), dimension(:), pointer, contiguous :: linkedNodes => null()  
  contains
    procedure, pass(this) :: construct
    procedure, private, pass(this) :: allocateScalars, allocateArrays
    procedure, pass(this) :: addLink
  end type
  
  contains ! module procedures

  ! note: constructing object allocates data structures
  subroutine construct(this, nLinks, connectionName)   
    class(GridConnectionType), intent(inout) :: this
    integer(I4B) :: nLinks ! reserves memory
    character(len=*) :: connectionName
        
    this%memOrigin = trim(connectionName)//'_MC'
    call this%allocateScalars()
    call this%allocateArrays(nLinks)
    
    ! TODO_MJR: to memorymanager?
    allocate(this%linkedNodes(nLinks))   
    
    this%nrOfLinks = 0
    
  end subroutine
  
  ! add connection between node n and m (local numbering)
  ! NB: total nr of connections should match reserved space
  subroutine addLink(this, own, linked, cl1, cl2, hwva, connType, nbrModel)
    class(GridConnectionType), intent(in)           :: this
    class(NumericalModelType), pointer, intent(in)  :: nbrModel   ! the neighbour
    integer(I4B) :: own, linked, connType                         ! node ids, connection type
    real(DP) :: cl1, cl2, hwva                                    ! connection lengths, hor. width or vert. area
            
    this%nrOfLinks = this%nrOfLinks + 1
    
    this%linkedNodes(this%nrOfLinks)%ownIndex = own   
    this%linkedNodes(this%nrOfLinks)%linkedIndex = linked
    this%linkedNodes(this%nrOfLinks)%length1 = cl1
    this%linkedNodes(this%nrOfLinks)%length2 = cl2
    this%linkedNodes(this%nrOfLinks)%hwva = hwva
    this%linkedNodes(this%nrOfLinks)%connectionType = connType
    this%linkedNodes(this%nrOfLinks)%connectedModel => nbrModel
    
  end subroutine addLink
  
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(GridConnectionType), intent(in) :: this
      
    call mem_allocate(this%nrOfLinks, 'NRLINKS', this%memOrigin)
    
  end subroutine allocateScalars
  
  subroutine allocateArrays(this, nConns)
    use MemoryManagerModule, only: mem_allocate
    class(GridConnectionType), intent(in) :: this
    integer(I4B) :: nConns
    
  end subroutine allocateArrays
  
end module GridConnectionModule
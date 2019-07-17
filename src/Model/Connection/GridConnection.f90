module GridConnectionModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENORIGIN
  use NumericalModelModule, only: NumericalModelType
  
  implicit none
  private
  
  type, public :: LinkGeometryType
    integer(I4B)  :: connectionType ! 0 = vertical, 1 = horizontal, 2 = vertically staggered
    real(DP)      :: length1        ! connection length from n to face between n and m
    real(DP)      :: length2        ! connection length from m to face between n and m
    real(DP)      :: hwva           ! horizontal width (when connectionType == 1), or vertical area (when connectionType == 0)
  end type
  
  type, public :: NodeLinkType
    integer(I4B) :: linkedIndex
    class(NumericalModelType), pointer :: linkedModel => null()
    integer(I4B) :: nrOfNbrs
    type(NodeLinkType), dimension(:), pointer :: neighbours => null() 
  end type  
  
  type, public :: GridConnectionType
    character(len=LENORIGIN) :: memOrigin
    integer(I4B), pointer :: nrOfLinks => null()
    integer(I4B), dimension(:), pointer :: ownIndices => null()
    type(NodeLinkType), dimension(:), pointer :: primaryLinks => null()
    type(LinkGeometryType), dimension(:), pointer :: linkGeometries => null()
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
    allocate(this%ownIndices(nLinks))
    allocate(this%primaryLinks(nLinks))
    allocate(this%linkGeometries(nLinks))  
    
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
    
    this%ownIndices(this%nrOfLinks) = own
    
    ! linked node (without neighbours)
    this%primaryLinks(this%nrOfLinks)%linkedIndex = linked
    this%primaryLinks(this%nrOfLinks)%linkedModel => nbrModel
    this%primaryLinks(this%nrOfLinks)%nrOfNbrs = 0
    
    ! construct link geometry
    this%linkGeometries(this%nrOfLinks)%connectionType = connType
    this%linkGeometries(this%nrOfLinks)%length1 = cl1
    this%linkGeometries(this%nrOfLinks)%length2 = cl2
    this%linkGeometries(this%nrOfLinks)%hwva = hwva
    
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
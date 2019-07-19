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
  
  type, public :: GlobalCellType
    integer(I4B) :: index
    class(NumericalModelType), pointer :: model => null()
  end type
  
  ! TODO_MJR: how about exchanges with (many) hanging nodes?
  ! for now stick to local neighbors only
  integer(I4B), parameter :: MaxNeighbors = 5
  
  ! a global cell as composite, we need it for XT3D
  type, public :: CellWithNbrsType
    type(GlobalCellType) :: cell
    integer(I4B) :: nrOfNbrs
    type(GlobalCellType), dimension(MaxNeighbors) :: neighbors
  end type
  
  type, private :: ModelWithNbrsType
      class(NumericalModelType), pointer :: model => null()
      integer(I4B) :: nrOfNbrs
      class(NumericalModelType), dimension(:), pointer :: neighbors => null()
  end type
  
  type, public :: GridConnectionType
    character(len=LENORIGIN) :: memOrigin
    type(NumericalModelType), pointer :: model => null()
    
    integer(I4B), pointer :: nrOfLinks => null()    
    type(LinkGeometryType), dimension(:), pointer :: linkGeometries => null()
    type(CellWithNbrsType), dimension(:), pointer :: localCells => null()
    type(CellWithNbrsType), dimension(:), pointer :: connectedCells => null()
    
    ! TODO_MJR: not sure yet about this
    integer(I4B), pointer :: nrOfConnectedModels => null()
    type(ModelWithNbrsType), dimension(:), pointer :: connectedModels => null()
    
  contains
    procedure, pass(this) :: construct
    procedure, private, pass(this) :: allocateScalars, allocateArrays
    procedure, pass(this) :: addLink
    procedure, pass(this) :: extendConnection
    procedure, pass(this) :: connectModels
  end type
  
  contains ! module procedures

  ! note: constructing object allocates data structures
  subroutine construct(this, model, nLinks, connectionName)
    class(GridConnectionType), intent(inout) :: this
    class(NumericalModelType), pointer, intent(in) :: model        
    integer(I4B) :: nLinks ! reserves memory
    character(len=*) :: connectionName
        
    this%model => model
    
    this%memOrigin = trim(connectionName)//'_MC'
    call this%allocateScalars()
    call this%allocateArrays(nLinks)
    
    ! TODO_MJR: to memorymanager?
    allocate(this%linkGeometries(nLinks))
    allocate(this%localCells(nLinks))
    allocate(this%connectedCells(nLinks))
    
    this%nrOfLinks = 0
    
  end subroutine
  
  ! add connection between node n and m (local numbering)
  ! NB: total nr of connections should match reserved space
  subroutine addLink(this, own, linked, cl1, cl2, hwva, connType, nbrModel)
    class(GridConnectionType), intent(in)           :: this
    class(NumericalModelType), pointer, intent(in)  :: nbrModel   ! the neighbor
    integer(I4B) :: own, linked, connType                         ! node ids, connection type
    real(DP) :: cl1, cl2, hwva                                    ! connection lengths, hor. width or vert. area
            
    this%nrOfLinks = this%nrOfLinks + 1    
    
    ! own node
    this%localCells(this%nrOfLinks)%cell%index = own
    this%localCells(this%nrOfLinks)%cell%model => this%model
    
    ! linked node
    this%connectedCells(this%nrOfLinks)%cell%index = linked
    this%connectedCells(this%nrOfLinks)%cell%model => nbrModel
    
    ! construct link geometry
    this%linkGeometries(this%nrOfLinks)%connectionType = connType
    this%linkGeometries(this%nrOfLinks)%length1 = cl1
    this%linkGeometries(this%nrOfLinks)%length2 = cl2
    this%linkGeometries(this%nrOfLinks)%hwva = hwva
    
  end subroutine addLink
  
  ! defines that two models of same type are connected through an
  ! exchange, need this for global topology
  subroutine connectModels(this, model1, model2)
    class(GridConnectionType), intent(in) :: this
    class(NumericalModelType), pointer :: model1, model2
    
        
  end subroutine
  
  ! build the connection topology to deal with neighbors-of-neighbors
  subroutine extendConnection(this)
    use ConnectionsModule, only: ConnectionsType
    class(GridConnectionType), intent(in) :: this    
    ! local
    integer(I4B) :: iLink, ipos
    integer(I4B) :: idx, nbrIdx, nbrCnt
    type(ConnectionsType), pointer :: connections => null()    
    type(NumericalModelType), pointer :: connectedModel => null()
        
    ! for all LOCAL cells
    connections => this%model%dis%con
    do iLink=1, this%nrOfLinks       
      ! find all local neighbors 
      nbrCnt = 0 
      idx = this%localCells(iLink)%cell%index
      do ipos=connections%ia(idx) + 1, connections%ia(idx+1) - 1        
        nbrIdx = connections%ja(ipos)
        this%localCells(iLink)%neighbors(nbrCnt+1)%index = nbrIdx
        this%localCells(iLink)%neighbors(nbrCnt+1)%model => this%model        
        nbrCnt = nbrCnt + 1        
      end do
      this%localCells(iLink)%nrOfNbrs = nbrCnt
    
      ! find all foreign neighbors
      ! TODO_MJR      
    end do
    
    ! for all CONNECTED cells
    do iLink=1, this%nrOfLinks
    
      idx = this%connectedCells(iLink)%cell%index
      connectedModel => this%connectedCells(iLink)%cell%model
      
      ! find all local neighbors
      nbrCnt = 0
      connections => connectedModel%dis%con
      do ipos=connections%ia(idx) + 1, connections%ia(idx+1) - 1        
        nbrIdx = connections%ja(ipos)
        this%connectedCells(iLink)%neighbors(nbrCnt+1)%index = nbrIdx
        this%connectedCells(iLink)%neighbors(nbrCnt+1)%model => connectedModel        
        nbrCnt = nbrCnt + 1        
      end do
      this%connectedCells(iLink)%nrOfNbrs = nbrCnt
            
      ! find all foreign neighbors
      ! TODO_MJR      
    end do
    
    connections => null()
    
  end subroutine extendConnection
 
  subroutine findNeighbors()
  
  end subroutine
  
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(GridConnectionType), intent(in) :: this
      
    call mem_allocate(this%nrOfLinks, 'NRLINKS', this%memOrigin)
    call mem_allocate(this%nrOfConnectedModels, 'NRCONMODELS', this%memorigin)
    
  end subroutine allocateScalars
  
  subroutine allocateArrays(this, nConns)
    use MemoryManagerModule, only: mem_allocate
    class(GridConnectionType), intent(in) :: this
    integer(I4B) :: nConns
    
  end subroutine allocateArrays
  
end module GridConnectionModule
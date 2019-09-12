module GridConnectionModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENORIGIN
  use NumericalModelModule, only: NumericalModelType
  use ConnectionsModule
  use SparseModule, only: sparsematrix
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
    type(CellWithNbrsType), dimension(:), allocatable :: neighbors
  end type
  
  type, private :: ModelWithNbrsType
      class(NumericalModelType), pointer :: model => null()
      integer(I4B) :: nrOfNbrs
      class(NumericalModelType), dimension(:), allocatable :: neighbors
  end type
  
  ! this class works with these steps:
  !
  ! 1: construct the basic instance to store the primary connections between grids
  ! 2: add those primary connections, typically from exchange file
  ! 3: add model topology with the proper depth (stencil dependent)
  ! 4: extend the connection, creating the full data structure and relations
  !
  type, public :: GridConnectionType
    character(len=LENORIGIN) :: memOrigin
    class(NumericalModelType), pointer :: model => null()
    
    integer(I4B), pointer :: nrOfLinks => null()  
    integer(I4B), pointer :: linkCapacity => null()
    type(LinkGeometryType), dimension(:), pointer :: linkGeometries => null()
    type(CellWithNbrsType), dimension(:), pointer :: localCells => null()
    type(CellWithNbrsType), dimension(:), pointer :: connectedCells => null()
    
    integer(I4B), pointer :: nrOfCells => null()                            ! the total number of cells which are connected
    type(GlobalCellType), dimension(:), pointer :: idxToGlobal => null()    ! a map from local to global coordinates
    type(ConnectionsType), pointer :: connections => null()                 ! sparse matrix with the connections
    
    ! TODO_MJR: not sure yet about this
    integer(I4B), pointer :: nrOfConnectedModels => null()
    type(ModelWithNbrsType), dimension(:), pointer :: connectedModels => null()
    
  contains
    procedure, pass(this) :: construct
    procedure, private, pass(this) :: allocateScalars, allocateArrays
    procedure, pass(this) :: addLink
    procedure, pass(this) :: extendConnection
    procedure, pass(this) :: connectModels
    
    procedure, private, pass(this) :: getNrOfCells
    procedure, private, pass(this) :: buildConnections
    procedure, private, pass(this) :: addNeighbors
    procedure, private, pass(this) :: addNeighborCell
  end type
  
  contains ! module procedures

  ! note: constructing object allocates data structures
  subroutine construct(this, model, nCapacity, connectionName)
    class(GridConnectionType), intent(inout) :: this
    class(NumericalModelType), pointer, intent(in) :: model        
    integer(I4B) :: nCapacity ! reserves memory
    character(len=*) :: connectionName
        
    this%model => model
    
    this%memOrigin = trim(connectionName)//'_MC'
    call this%allocateScalars()
    call this%allocateArrays(nCapacity)
    
    ! TODO_MJR: to memorymanager?
    allocate(this%linkGeometries(nCapacity))
    allocate(this%localCells(nCapacity))
    allocate(this%connectedCells(nCapacity))
    
    this%linkCapacity = nCapacity
    this%nrOfLinks = 0
    this%nrOfCells = 0
    
  end subroutine
  
  ! add connection between node n and m (local numbering)
  ! NB: total nr of connections should match reserved space
  subroutine addLink(this, own, linked, cl1, cl2, hwva, connType, nbrModel)
    class(GridConnectionType), intent(in)           :: this
    class(NumericalModelType), pointer, intent(in)  :: nbrModel   ! the neighbor
    integer(I4B) :: own, linked, connType                         ! node ids, connection type (0=vert,1=hor,2=vert.stagg.)
    real(DP) :: cl1, cl2, hwva                                    ! connection lengths, hor. width or vert. area
            
    this%nrOfLinks = this%nrOfLinks + 1    
    
    if (this%nrOfLinks > this%linkCapacity) then
      write(*,*) 'Error: no memory reserved for storing grid connection, skipping...'
      this%nrOfLinks = this%nrOfLinks - 1
      return
    end if
    
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
  subroutine extendConnection(this, localDepth, remoteDepth)    
    class(GridConnectionType), intent(in) :: this  
    integer(I4B) :: localDepth, remoteDepth ! to determine to which 
    ! local
    integer(I4B) :: i
     
    ! add neighbors
    do i = 1, this%nrOfLinks 
      call this%addNeighbors(this%localCells(i), localDepth, this%connectedCells(i)%cell)
      call this%addNeighbors(this%connectedCells(i), remoteDepth, this%localCells(i)%cell)
    end do
    
    ! now finalize the data structures
    this%nrOfCells = this%getNrOfCells()
    call this%buildConnections()
    
  end subroutine extendConnection
 
  ! returns total number of cells in the interface region,
  ! this varies with stencil size
  function getNrOfCells(this) result(ncells)
    class(GridConnectionType), intent(in) :: this
    integer(I4B) :: ncells
    
    !TODO_MJR: also for larger stencils
    ncells = size(this%localCells) + size(this%connectedCells)   
  end function
    
  ! routine for finding neighbors-of-neighbors, recursively
  recursive subroutine addNeighbors(this, cellNbrs, maxlvl, mask)
    use SimModule, only: ustop
    class(GridConnectionType), intent(in) :: this
    type(CellWithNbrsType), intent(inout) :: cellNbrs    
    integer(I4B), intent(inout)           :: maxlvl
    type(GlobalCellType), intent(in)      :: mask
    ! local
    integer(I4B) :: idx, nbrIdx, ipos, inbr
    type(ConnectionsType), pointer :: conn
    integer(I4B) :: level
    
    if (maxLvl < 1) then
      return
    end if
    level = maxLvl - 1
    
    conn => cellNbrs%cell%model%dis%con
    
    ! find neighbors local to this cell by looping through grid connections
    do ipos=conn%ia(cellNbrs%cell%index) + 1, conn%ia(cellNbrs%cell%index+1) - 1        
      nbrIdx = conn%ja(ipos)
      
      ! apply mask
      if (nbrIdx == mask%index .and. associated(cellNbrs%cell%model, mask%model)) then
        cycle
      end if
      
      call this%addNeighborCell(cellNbrs, nbrIdx, cellNbrs%cell%model)      
    end do
        
    ! TODO_MJR: find remote nbr
    
    ! now find nbr-of-nbr    
    do inbr=1, cellNbrs%nrOfNbrs
      call this%addNeighbors(cellNbrs%neighbors(inbr), level, cellNbrs%cell)
    end do
    
  end subroutine addNeighbors
  
  subroutine addNeighborCell(this, cellNbrs, newNbrIdx, nbrModel)
    use SimModule, only: ustop
    class(GridConnectionType), intent(in) :: this
    type(CellWithNbrsType), intent(inout) :: cellNbrs 
    integer(I4B), intent(in)              :: newNbrIdx
    class(NumericalModelType), pointer    :: nbrModel
    ! local
    integer(I4B) :: nbrCnt
    
    ! TODO_MJR: dynamic memory
    if (.not. allocated(cellNbrs%neighbors)) then
      allocate(cellNbrs%neighbors(MaxNeighbors))  
    end if
    
    nbrCnt = cellNbrs%nrOfNbrs
    if (nbrCnt + 1 > MaxNeighbors) then
       write(*,*) 'Error extending connections in GridConnection, max. nr. of neighbors exceeded: terminating...'
       call ustop()  
    end if
        
    cellNbrs%neighbors(nbrCnt + 1)%cell%index = newNbrIdx
    cellNbrs%neighbors(nbrCnt + 1)%cell%model => nbrModel  
    cellNbrs%nrOfNbrs = nbrCnt + 1
  end subroutine
  
  ! builds a sparse matrix holding all cell connections,
  ! with new indices, and stores the mapping to the global ids
  subroutine buildConnections(this)
    use SimModule, only: ustop
    class(GridConnectionType), intent(in) :: this       
    ! local
    integer(I4B) :: n, m, offset
    integer(I4B) :: isym, j
    integer(I4B) :: ierror
    integer(I4B), dimension(:), allocatable :: nnz
    type(SparseMatrix), pointer :: sparse 
    type(ConnectionsType), pointer :: conn
    
    
    allocate(this%idxToGlobal(this%nrOfCells))
    allocate(sparse)    
    
    allocate(nnz(this%nrOfCells))
    nnz = MaxNeighbors+1
    call sparse%init(this%nrOfCells, this%nrOfCells, nnz)
    
    ! diagonals
    do n = 1, this%nrOfCells
      call sparse%addconnection(n, n, 1)
    end do    
    
    ! internals TODO_MJR: do we need them at lowest order, why???
    
    ! exchanges
    offset = this%nrOfLinks
    do n = 1, this%nrOfLinks
      
      ! fill with local indices
      m = n + offset
      call sparse%addConnection(n, m, 1)
      call sparse%addConnection(m, n, 1)
      
      ! store mapping here
      this%idxToGlobal(n) = this%localCells(n)%cell
      this%idxToGlobal(m) = this%connectedCells(n)%cell
      
    end do
    
    ! create connections from sparse
    allocate(this%connections)
    conn => this%connections
    call conn%allocate_scalars(this%memorigin)
    conn%nodes = this%nrOfCells
    conn%nja = sparse%nnz
    conn%njas = (conn%nja -  conn%nodes) / 2
    call conn%allocate_arrays()
    
    call sparse%filliaja(conn%ia, conn%ja, ierror)  
    if (ierror /= 0) then
      write(*,*) 'Error filling ia/ja connections in GridConnection: terminating...'
      call ustop()
    end if
    
    call fillisym(conn%nodes, conn%nja, conn%ia, conn%ja, conn%isym)
    call filljas(conn%nodes, conn%nja, conn%ia, conn%ja, conn%isym, conn%jas)
    
    ! done with it
    call sparse%destroy()
    
    ! fill ihc, cl1, cl2, hwva, anglex (with size=njas) for all connections
    do n=1, conn%nodes
      do j=conn%ia(n)+1, conn%ia(n+1)-1
        m = conn%ja(j) 
        isym = conn%jas(j)
        if (m > n) then
          conn%cl1(isym) = this%linkGeometries(isym)%length1
        else
          conn%cl2(isym) = this%linkGeometries(isym)%length2
        end if
        conn%hwva(isym) = this%linkGeometries(isym)%hwva
        conn%ihc(isym) = this%linkGeometries(isym)%connectionType
        conn%anglex(isym) = 0.0 ! TODO_MJR: implement
      end do
    end do
    
  end subroutine buildConnections
  
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(GridConnectionType), intent(in) :: this
      
    call mem_allocate(this%nrOfLinks, 'NRLINKS', this%memOrigin)
    call mem_allocate(this%nrOfCells, 'NRCELLS', this%memOrigin)
    call mem_allocate(this%linkCapacity, 'LINKCAP', this%memOrigin)
    call mem_allocate(this%nrOfConnectedModels, 'NRCONMODELS', this%memorigin)
    
  end subroutine allocateScalars
  
  subroutine allocateArrays(this, nConns)
    use MemoryManagerModule, only: mem_allocate
    class(GridConnectionType), intent(in) :: this
    integer(I4B) :: nConns
    
  end subroutine allocateArrays  
  
end module GridConnectionModule
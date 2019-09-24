module GridConnectionModule
  use KindModule, only: I4B, DP
  use SimModule, only: ustop
  use ConstantsModule, only: LENORIGIN
  use ListModule, only: ListType
  use NumericalModelModule
  use NumericalExchangeModule
  use ConnectionsModule
  use SparseModule, only: sparsematrix
  implicit none
  private
  
  type, public :: GlobalCellType
    integer(I4B) :: index
    class(NumericalModelType), pointer :: model => null()
  end type
  
  ! TODO_MJR: how about exchanges with (many) hanging nodes?
  ! for now stick to local neighbors only
  integer(I4B), parameter :: MaxNeighbors = 4
  
  ! a global cell as composite, we need it for XT3D
  type, public :: CellWithNbrsType
    type(GlobalCellType) :: cell
    integer(I4B) :: nrOfNbrs
    type(CellWithNbrsType), dimension(:), allocatable :: neighbors
  end type
  
  type, private :: ModelWithNbrsType
      class(NumericalModelType), pointer :: model => null()
      integer(I4B) :: nrOfNbrs
      type(ModelWithNbrsType), dimension(:), allocatable :: neighbors
  end type
  
  ! --
  ! this class works as follows:
  !
  ! 1: construct the basic instance to store the primary connections between grids
  ! 2: add those primary connections, typically from exchange file
  ! 3: add model topology with the proper depth (stencil dependent)
  ! 4: extend the connection, creating the full data structure and relations
  ! 5: build the connections object, from which the grid for the interface model can be constructed
  ! --
  type, public :: GridConnectionType
    character(len=LENORIGIN) :: memOrigin
    class(NumericalModelType), pointer :: model => null()
    
    integer(I4B), pointer :: linkCapacity => null()
    
    ! --
    ! together with the stencil type, these data contain the
    ! full topology of the interface 
    integer(I4B), pointer :: nrOfBoundaryCells => null()
    type(CellWithNbrsType), dimension(:), pointer :: boundaryCells => null()
    type(CellWithNbrsType), dimension(:), pointer :: connectedCells => null()    
    type(ModelWithNbrsType), pointer :: modelWithNbrs => null() 
    type(ListType) ::  exchanges                                              ! all relevant exchanges for this connection (up to the specified depth)
    ! --
    
    integer(I4B), pointer :: nrOfCells => null()                              ! the total number of cells in the interface
    type(GlobalCellType), dimension(:), pointer :: idxToGlobal => null()      ! a map from interface index to global coordinate    
        
    ! --
    ! We have a global index, which is the row in the solution matrix,
    ! a regional index, which runs over all the models involved in this gridconnection,
    ! a local index, which is just local to each model,
    ! and finally, something called the interface index, which numbers the
    ! cells in the interface discretization.
    !
    ! The following data structure defines a map from the regional index to the interface index:
    integer(I4B), dimension(:), pointer               :: regionalToInterfaceIdxMap => null()
    type(ListType)                                    :: regionalModels                   ! the models that make up the interface
    integer(I4B), dimension(:), pointer               :: regionalModelOffset => null()    ! the new offset to compactify the range of indices 
    integer(I4B), pointer                             :: indexCount => null()             ! counts the number of cells in the interface
    ! --
    
    type(ConnectionsType), pointer :: connections => null()                 ! sparse matrix with the connections
    
    
  contains
    procedure, pass(this) :: construct
    procedure, private, pass(this) :: allocateScalars, allocateArrays
    procedure, pass(this) :: connectCell
    procedure, pass(this) :: addModelLink    
    procedure, pass(this) :: extendConnection
    ! private stuff
    procedure, private, pass(this) :: buildConnections
    procedure, private, pass(this) :: addNeighbors
    procedure, private, pass(this) :: addNeighborCell
    procedure, private, pass(this) :: addRemoteNeighbors
    procedure, private, pass(this) :: connectModels
    procedure, private, pass(this) :: addToRegionalModels
    procedure, private, pass(this) :: getRegionalModelOffset
    procedure, private, pass(this) :: getInterfaceIndex
    procedure, private, pass(this) :: getModelWithNbrs
    procedure, private, pass(this) :: getExchangeData
    procedure, private, pass(this) :: registerInterfaceCells
    procedure, private, pass(this) :: makePrimaryConnections
    procedure, private, pass(this) :: connectNeighborCells
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
    allocate(this%modelWithNbrs)
    allocate(this%boundaryCells(nCapacity))
    allocate(this%connectedCells(nCapacity))
    
    this%modelWithNbrs%model => model
    call this%addToRegionalModels(model)
    
    this%linkCapacity = nCapacity
    this%nrOfBoundaryCells = 0
  end subroutine
  
  ! TODO_MJR: refactor this, should accept numerical exchanges
  ! add connection between boundary cell and the remote cell
  ! as specified by the numerical exchange data (local numbering)
  ! NB: total nr of connections should match reserved space
  subroutine connectCell(this, idx1, model1, idx2, model2)
    class(GridConnectionType), intent(in) :: this
    integer(I4B)                          :: idx1, idx2  ! index of boundary cell
    class(NumericalModelType), pointer    :: model1, model2
            
    this%nrOfBoundaryCells = this%nrOfBoundaryCells + 1    
    if (this%nrOfBoundaryCells > this%linkCapacity) then
      write(*,*) 'Error: not enough memory reserved for storing grid connection, terminating...'
      call ustop()
    end if
    
    if (associated(model1, this%model)) then
      this%boundaryCells(this%nrOfBoundaryCells)%cell%index = idx1
      this%boundaryCells(this%nrOfBoundaryCells)%cell%model => this%model
      this%connectedCells(this%nrOfBoundaryCells)%cell%index = idx2
      this%connectedCells(this%nrOfBoundaryCells)%cell%model => model2
    else
      this%boundaryCells(this%nrOfBoundaryCells)%cell%index = idx2
      this%boundaryCells(this%nrOfBoundaryCells)%cell%model => this%model
      this%connectedCells(this%nrOfBoundaryCells)%cell%index = idx1
      this%connectedCells(this%nrOfBoundaryCells)%cell%model => model1
    end if
  
  end subroutine connectCell
  
  ! this is called for two models of same type that are connected through an
  ! exchange, need this for global topology
  ! NOTE: assumption here is only 1 exchange exists between any two models,
  ! can we do that??
  subroutine addModelLink(this, numEx, depth)
    class(GridConnectionType), intent(inout)  :: this
    class(NumericalExchangeType), pointer     :: numEx
    integer(I4B) :: depth
    ! local
        
    call this%connectModels(this%modelWithNbrs, numEx, depth)
    
  end subroutine addModelLink
  
  recursive subroutine connectModels(this, modelNbrs, numEx, depth)
    class(GridConnectionType), intent(inout)   :: this
    class(ModelWithNbrsType), intent(inout) :: modelNbrs
    class(NumericalExchangeType), pointer     :: numEx
    integer(I4B)                            :: depth
    ! local
    integer(I4B) :: inbr, newDepth
    class(NumericalModelType), pointer      :: neighborModel
    
    if (depth < 1) then
      return
    end if
    
    neighborModel => null()    
    
    ! is it a direct neighbor:
    if (associated(modelNbrs%model, numEx%m1)) then
      neighborModel => numEx%m2
    else if (associated(modelNbrs%model, numEx%m2)) then
      neighborModel => numEx%m1
    end if
    
    ! and/or maybe its connected to one of the neighbors:
    newDepth = depth - 1
    do inbr = 1, modelNbrs%nrOfNbrs
      call this%connectModels(modelNbrs%neighbors(inbr), numEx, newDepth)
    end do
    
    ! do not add until here, after the recursion, to prevent 
    ! back-and-forth connecting of models...
    if (associated(neighborModel)) then           
      if (.not. allocated(modelNbrs%neighbors)) then
        allocate(modelNbrs%neighbors(MaxNeighbors))
        modelNbrs%nrOfNbrs = 0
      end if
      modelNbrs%neighbors(modelNbrs%nrOfNbrs + 1)%model => neighborModel
      modelNbrs%nrOfNbrs = modelNbrs%nrOfNbrs + 1
      
      ! add to array of all neighbors
      call this%addToRegionalModels(neighborModel)
      
      ! add to list of exchanges
      call AddNumericalExchangeToList(this%exchanges, numEx)
      
    end if
    
  end subroutine connectModels
  
  subroutine addToRegionalModels(this, modelToAdd)
    class(GridConnectionType), intent(inout) :: this  
    class(NumericalModelType), pointer    :: modelToAdd
    ! local
    integer(I4B) :: im
    class(NumericalModelType), pointer    :: modelInList
    
    ! do we have it in there already?
    do im = 1, this%regionalModels%Count()
      modelInList => GetNumericalModelFromList(this%regionalModels, im)
      if (associated(modelToAdd, modelInList)) then
        return
      end if
    end do
    
    ! no? then add...
    call AddNumericalModelToList(this%regionalModels, modelToAdd)
    
  end subroutine addToRegionalModels

  ! build the connection topology to deal with neighbors-of-neighbors
  subroutine extendConnection(this, localDepth, remoteDepth)    
    class(GridConnectionType), intent(inout) :: this  
    integer(I4B) :: localDepth, remoteDepth
    ! local
    integer(I4B) :: icell
    integer(I4B) :: imod, regionSize, offset
    class(NumericalModelType), pointer :: numModel
   
    ! first add the neighbors for the interior, localOnly because 
    ! connections crossing model boundary will be added anyway
    do icell = 1, this%nrOfBoundaryCells
      call this%addNeighbors(this%boundaryCells(icell), localDepth, this%connectedCells(icell)%cell, local=.true.)
    end do
    ! and for the exterior
    do icell = 1, this%nrOfBoundaryCells
      call this%addNeighbors(this%connectedCells(icell), remoteDepth, this%boundaryCells(icell)%cell)
    end do
    
    ! set up mapping for the region (models participating in interface model grid)
    allocate(this%regionalModelOffset(this%regionalModels%Count()))
    regionSize = 0
    offset = 0
    do imod = 1, this%regionalModels%Count()
      numModel => GetNumericalModelFromList(this%regionalModels, imod)      
      regionSize = regionSize + numModel%dis%nodes 
      this%regionalModelOffset(imod) = offset     
      offset = offset + numModel%dis%nodes
    end do
    ! init to -1, meaning 'interface index was not assigned yet'
    allocate(this%regionalToInterfaceIdxMap(regionSize))
    this%regionalToInterfaceIdxMap = -1
    
    call this%buildConnections()
    
  end subroutine extendConnection
    
  ! routine for finding neighbors-of-neighbors, recursively
  recursive subroutine addNeighbors(this, cellNbrs, depth, mask, local)
    use SimModule, only: ustop
    class(GridConnectionType), intent(inout)  :: this
    type(CellWithNbrsType), intent(inout)     :: cellNbrs    
    integer(I4B), intent(inout)               :: depth
    type(GlobalCellType), optional            :: mask
    logical, optional                         :: local ! controls whether only local (within the same model) neighbors are added
    ! local
    integer(I4B)                              :: idx, nbrIdx, ipos, inbr, iexg
    type(ConnectionsType), pointer            :: conn
    integer(I4B)                              :: newDepth
    type(ModelWithNbrsType), pointer          :: modelWithNbrs
    logical                                   :: localOnly
    
    if (.not. present(local)) then
      localOnly = .false. ! default
    else
      localOnly = local
    end if
    
    ! if depth == 1, then we are not adding neighbors but use
    ! the boundary and connected cell only
    if (depth < 2) then
      return
    end if
    newDepth = depth - 1
    
    conn => cellNbrs%cell%model%dis%con
    
    ! find neighbors local to this cell by looping through grid connections
    do ipos=conn%ia(cellNbrs%cell%index) + 1, conn%ia(cellNbrs%cell%index+1) - 1        
      nbrIdx = conn%ja(ipos)      
      call this%addNeighborCell(cellNbrs, nbrIdx, cellNbrs%cell%model, mask)
    end do
        
    ! find and add remote nbr (from a different model)
    if (.not. localOnly) then
      call this%getModelWithNbrs(cellNbrs%cell%model, modelWithNbrs)
      call this%addRemoteNeighbors(cellNbrs, modelWithNbrs, mask)
    end if
    
    ! now find nbr-of-nbr    
    do inbr=1, cellNbrs%nrOfNbrs
      call this%addNeighbors(cellNbrs%neighbors(inbr), newDepth, cellNbrs%cell)
    end do
    
  end subroutine addNeighbors
  
  ! looks whether this models has any neighbors, then finds
  ! the exchange and from the n-m pairs in there, it adds 
  ! the neighboring cells
  subroutine addRemoteNeighbors(this, cellNbrs, modelWithNbrs, mask)
    class(GridConnectionType), intent(inout)      :: this
    type(CellWithNbrsType), intent(inout)         :: cellNbrs    
    type(ModelWithNbrsType), intent(in), pointer  :: modelWithNbrs
    type(GlobalCellType), optional                :: mask
    ! local
    integer(I4B) :: inbr, iexg
    type(NumericalExchangeType), pointer :: numEx
    
    do inbr = 1, modelWithNbrs%nrOfNbrs
      numEx => this%getExchangeData(cellNbrs%cell%model, modelWithNbrs%neighbors(inbr)%model)
      if (.not. associated(numEx)) then
        write(*,*) 'Error finding exchange data for models, should never happen: terminating...'
        call ustop()  
      end if
      
      ! loop over n-m links in the exchange
      if (associated(cellNbrs%cell%model, numEx%m1)) then
        do iexg = 1, numEx%nexg
          if (numEx%nodem1(iexg) == cellNbrs%cell%index) then
            ! we have a link, now add foreign neighbor
            call this%addNeighborCell(cellNbrs, numEx%nodem2(iexg), numEx%m2, mask)
          end if
        end do
      end if
      ! and the reverse
      if (associated(cellNbrs%cell%model, numEx%m2)) then
        do iexg = 1, numEx%nexg
          if (numEx%nodem2(iexg) == cellNbrs%cell%index) then
            ! we have a link, now add foreign neighbor
            call this%addNeighborCell(cellNbrs, numEx%nodem1(iexg), numEx%m1, mask)
          end if
        end do
      end if
      
    end do
    
  end subroutine addRemoteNeighbors
  
  ! returns the numerical exchange data for the pair of models
  function getExchangeData(this, model1, model2) result(numEx)
    class(GridConnectionType), intent(inout)        :: this
    class(NumericalModelType), pointer, intent(in)  :: model1, model2
    type(NumericalExchangeType), pointer            :: numEx
    ! local
    type(NumericalExchangeType), pointer            :: numExLocal    
    integer(I4B) :: i
    
    numEx => null()
        
    do i = 1, this%exchanges%Count()
      numExLocal => GetNumericalExchangeFromList(this%exchanges, i)
      if (associated(model1, numExLocal%m1) .and. associated(model2, numExLocal%m2)) then
        numEx => numExLocal
        exit
      end if
      if (associated(model1, numExLocal%m2) .and. associated(model2, numExLocal%m1)) then
        numEx => numExLocal
        exit
      end if
    end do
    
  end function getExchangeData
  
  subroutine getModelWithNbrs(this, model, modelWithNbr)
    class(GridConnectionType), intent(in)           :: this
    class(NumericalModelType), pointer, intent(in)  :: model
    type(ModelWithNbrsType), pointer, intent(out)   :: modelWithNbr
    ! local
    integer(I4B) :: i
    
    ! traverse the tree, currently two deep but this can be made recursive
    if (associated(model, this%modelWithNbrs%model)) then    
      modelWithNbr => this%modelWithNbrs
    else
      do i = 1, this%modelWithNbrs%nrOfNbrs
        if (associated(model, this%modelWithNbrs%neighbors(i)%model)) then  
          modelWithNbr => this%modelWithNbrs%neighbors(i)
        end if
      end do
    end if
    
  end subroutine getModelWithNbrs
  
  subroutine addNeighborCell(this, cellNbrs, newNbrIdx, nbrModel, mask)    
    class(GridConnectionType), intent(in) :: this
    type(CellWithNbrsType), intent(inout) :: cellNbrs 
    integer(I4B), intent(in)              :: newNbrIdx
    class(NumericalModelType), pointer    :: nbrModel
    type(GlobalCellType), optional        :: mask
    ! local
    integer(I4B) :: nbrCnt
    
    if (present(mask)) then
      if (newNbrIdx == mask%index .and. associated(nbrModel, mask%model)) then
        return
      end if
    end if
    
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
    use ArrayHandlersModule, only: ifind
    class(GridConnectionType), intent(inout) :: this 
    ! local
    integer(I4B) :: icell, ifaceIdx        
    integer(I4B), dimension(:), allocatable :: nnz
    type(SparseMatrix), pointer :: sparse     
    integer(I4B) :: ierror
    
    type(ConnectionsType), pointer :: conn, connOrig    
    integer(I4B) :: n, m, ipos, isym, iposOrig, isymOrig
    type(GlobalCellType), pointer :: ncell, mcell
    
    integer(I4B) :: inx, iexg, im, ivalAngldegx 
    integer(I4B) :: nOffset, mOffset, nIfaceIdx, mIfaceIdx
    class(NumericalExchangeType), pointer :: numEx
    class(NumericalModelType), pointer    :: model
    
    ! generate interface cell indices, recursively and build mapping. 
    ! Start with boundaryCells, this way the internal interface nodes 
    ! will be numbered contiguously
    this%indexCount = 0
    do icell = 1, this%nrOfBoundaryCells
      call this%registerInterfaceCells(this%boundaryCells(icell))
    end do
    do icell = 1, this%nrOfBoundaryCells
      call this%registerInterfaceCells(this%connectedCells(icell))
    end do
    this%nrOfCells = this%indexCount
    
    ! to map the interface index to global coordinates
    allocate(this%idxToGlobal(this%nrOfCells))
    
    ! create sparse, to temporarily hold connections
    allocate(sparse)
    allocate(nnz(this%nrOfCells))
    nnz = MaxNeighbors+1
    call sparse%init(this%nrOfCells, this%nrOfCells, nnz)
    
    ! now (recursively) add the connections for the boundary cells
    ! start with the primary connection (n-m from the exchanges)
    call this%makePrimaryConnections(sparse)   
    ! then into own domain
    do icell = 1, this%nrOfBoundaryCells
      call this%connectNeighborCells(this%boundaryCells(icell), sparse)
    end do
    ! and same for the neighbors of connected cells
    do icell = 1, this%nrOfBoundaryCells
      call this%connectNeighborCells(this%connectedCells(icell), sparse)
    end do
    
     ! create connections from sparse, and fill
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
    ! and done with it
    call sparse%destroy() 
    
    ! fill ihc, cl1, cl2, hwva, anglex (with size=njas) for all internal connections
    do n = 1, conn%nodes
      do ipos=conn%ia(n)+1, conn%ia(n+1)-1
        m = conn%ja(ipos)
        if (n > m) cycle
        
        isym = conn%jas(ipos)
        ncell => this%idxToGlobal(n)
        mcell => this%idxToGlobal(m)
        if (associated(ncell%model, mcell%model)) then
          ! within same model, straight copy
          connOrig => ncell%model%dis%con
          iposOrig = connOrig%getjaindex(ncell%index, mcell%index)          
          isymOrig = connOrig%jas(iposOrig)
          conn%cl1(isym) = connOrig%cl1(isymOrig)
          conn%cl2(isym) = connOrig%cl2(isymOrig)
          conn%hwva(isym) = connOrig%hwva(isymOrig)
          conn%ihc(isym) = connOrig%ihc(isymOrig)
          conn%anglex(isym) = connOrig%anglex(isymOrig)
        end if
      end do
    end do
    
    ! fill values for all exchanges, using symmetry
    do im = 1, this%regionalModels%Count()
      model => GetNumericalModelFromList(this%regionalModels, im)
      nOffset = this%getRegionalModelOffset(model)
      do inx = 1, this%exchanges%Count()
        numEx => GetNumericalExchangeFromList(this%exchanges, inx) 
        ! TODO_MJR: this is not good, shouldn't this be outside
        ! the GWF domain, in NumericalExchange directly?
        ivalAngldegx = ifind(numEx%auxname, 'ANGLDEGX')        
        if (associated(model, numEx%m1)) then
          mOffset = this%getRegionalModelOffset(numEx%m2)
          do iexg = 1, numEx%nexg
            nIfaceIdx = this%regionalToInterfaceIdxMap(noffset + numEx%nodem1(iexg))
            mIfaceIdx = this%regionalToInterfaceIdxMap(moffset + numEx%nodem2(iexg))
            ipos = conn%getjaindex(nIfaceIdx, mIfaceIdx)
            isym = conn%jas(ipos)
            conn%cl1(isym) = numEx%cl1(iexg)
            conn%cl2(isym) = numEx%cl2(iexg)
            conn%hwva(isym) = numEx%hwva(iexg)
            conn%ihc(isym) = numEx%ihc(iexg) 
            if (ivalAngldegx > 0) then
                conn%anglex(isym) = numEx%auxvar(ivalAngldegx,iexg)
            end if
          end do          
        end if
        
      end do
    end do
    
  end subroutine buildConnections 
  
  recursive subroutine registerInterfaceCells(this, cellWithNbrs)
    class(GridConnectionType), intent(inout) :: this
    type(CellWithNbrsType)                   :: cellWithNbrs
    ! local
    integer(I4B) :: offset, inbr
    integer(I4B) :: regionIdx  ! unique idx in the region (all connected models)
    integer(I4B) :: ifaceIdx   ! unique idx in the interface grid
    
    offset = this%getRegionalModelOffset(cellWithNbrs%cell%model)
    regionIdx = offset + cellWithNbrs%cell%index
    ifaceIdx = this%getInterfaceIndex(cellWithNbrs%cell)
    if (ifaceIdx == -1) then
      this%indexCount = this%indexCount + 1
      ifaceIdx = this%indexCount
      this%regionalToInterfaceIdxMap(regionIdx) = ifaceIdx
    end if   
    
    ! and also for its neighbors
    do inbr = 1, cellWithNbrs%nrOfNbrs
      call this%registerInterfaceCells(cellWithNbrs%neighbors(inbr))
    end do
      
  end subroutine registerInterfaceCells
  
  subroutine makePrimaryConnections(this, sparse)
    class(GridConnectionType), intent(inout)  :: this
    type(SparseMatrix), pointer               :: sparse
    ! local
    integer(I4B) :: icell
    integer(I4B) :: ifaceIdx, ifaceIdxNbr
    
    do icell = 1, this%nrOfBoundaryCells  
      ifaceIdx = this%getInterfaceIndex(this%boundaryCells(icell)%cell)
      ifaceIdxNbr = this%getInterfaceIndex(this%connectedCells(icell)%cell)
      
      ! set mapping
      this%idxToGlobal(ifaceIdx) = this%boundaryCells(icell)%cell
      this%idxToGlobal(ifaceIdxNbr) = this%connectedCells(icell)%cell
      
      ! add diagonals to sparse
      call sparse%addconnection(ifaceIdx, ifaceIdx, 1)
      call sparse%addconnection(ifaceIdxNbr, ifaceIdxNbr, 1)
      
      ! and cross terms
      call sparse%addconnection(ifaceIdx, ifaceIdxNbr, 1)
      call sparse%addconnection(ifaceIdxNbr, ifaceIdx, 1)
    end do
    
  end subroutine makePrimaryConnections
  
  recursive subroutine connectNeighborCells(this, cell, sparse)
    class(GridConnectionType), intent(inout)  :: this
    type(CellWithNbrsType)                    :: cell
    type(SparseMatrix), pointer               :: sparse
    ! local
    integer(I4B) :: ifaceIdx, ifaceIdxNbr    ! unique idx in the interface grid
    integer(I4B) :: inbr
    
    ifaceIdx = this%getInterfaceIndex(cell%cell)   
    do inbr = 1, cell%nrOfNbrs
      ifaceIdxNbr = this%getInterfaceIndex(cell%neighbors(inbr)%cell)      
      this%idxToGlobal(ifaceIdxNbr) = cell%neighbors(inbr)%cell      
      
      call sparse%addconnection(ifaceIdxNbr, ifaceIdxNbr, 1)
      call sparse%addconnection(ifaceIdx, ifaceIdxNbr, 1)
      call sparse%addconnection(ifaceIdxNbr, ifaceIdx, 1)
      
      ! recurse
      call this%connectNeighborCells(cell%neighbors(inbr), sparse)
    end do
    
  end subroutine connectNeighborCells
  
  ! helper routine to convert global cell to index in interface grid
  function getInterfaceIndex(this, cell) result(ifaceIdx)
    class(GridConnectionType), intent(inout)  :: this
    type(GlobalCellType), intent(in)          :: cell
    integer(I4B)                              :: ifaceIdx
    ! local
    integer(I4B) :: offset, regionIdx
    
    offset = this%getRegionalModelOffset(cell%model)
    regionIdx = offset + cell%index
    ifaceIdx = this%regionalToInterfaceIdxMap(regionIdx)
  end function getInterfaceIndex
  
  function getRegionalModelOffset(this, model) result(offset)
    class(GridConnectionType), intent(inout) :: this
    class(NumericalModelType), pointer       :: model
    integer(I4B)                             :: offset
    ! local
    integer(I4B) :: im
    class(NumericalModelType), pointer :: modelInList
    offset = 0
    do im = 1, this%regionalModels%Count()
       modelInList => GetNumericalModelFromList(this%regionalModels, im)
       if (associated(model, modelInList)) then
         offset = this%regionalModelOffset(im)
         return
       end if
    end do
    
  end function getRegionalModelOffset
  
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(GridConnectionType), intent(in) :: this
      
    call mem_allocate(this%linkCapacity, 'LINKCAP', this%memOrigin)
    call mem_allocate(this%nrOfBoundaryCells, 'NRBNDCELLS', this%memOrigin)
    call mem_allocate(this%indexCount, 'IDXCOUNT', this%memOrigin)
    call mem_allocate(this%nrOfCells, 'NRCELLS', this%memOrigin)
    
  end subroutine allocateScalars
  
  subroutine allocateArrays(this, nConns)
    use MemoryManagerModule, only: mem_allocate
    class(GridConnectionType), intent(in) :: this
    integer(I4B) :: nConns
    
  end subroutine allocateArrays  
  
end module GridConnectionModule
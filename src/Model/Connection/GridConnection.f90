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
      type(ModelWithNbrsType), dimension(:), allocatable :: neighbors
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
    
    integer(I4B), pointer :: linkCapacity => null()
    
    integer(I4B), pointer :: nrOfBoundaryCells => null()
    type(CellWithNbrsType), dimension(:), pointer :: boundaryCells => null()
    
    integer(I4B), pointer :: nrOfCells => null()                            ! the total number of cells which are connected
    type(GlobalCellType), dimension(:), pointer :: idxToGlobal => null()    ! a map from local to global coordinates
    
    type(ModelWithNbrsType), pointer :: modelWithNbrs => null() 
    type(ListType) ::  exchanges ! all relevant exchanges for this connection (up to the specified depth)
    
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
    
    type(ConnectionsType), pointer :: connections => null()                 ! sparse matrix with the connections
    
    
  contains
    procedure, pass(this) :: construct
    procedure, private, pass(this) :: allocateScalars, allocateArrays
    procedure, pass(this) :: setBoundaryCell
    procedure, pass(this) :: addModelLink    
    procedure, pass(this) :: extendConnection
    
    procedure, private, pass(this) :: buildConnections
    procedure, private, pass(this) :: addNeighbors
    procedure, private, pass(this) :: addNeighborCell
    procedure, private, pass(this) :: addRemoteNeighbors
    procedure, private, pass(this) :: connectModels
    procedure, private, pass(this) :: addToRegionalModels
    procedure, private, pass(this) :: getRegionalModelOffset
    procedure, private, pass(this) :: getModelWithNbrs
    procedure, private, pass(this) :: getExchangeData
    procedure, private, pass(this) :: registerInterfaceCells
    procedure, private, pass(this) :: connectInterfaceCells
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
    
    this%modelWithNbrs%model => model
    call this%addToRegionalModels(model)
    
    this%linkCapacity = nCapacity
    this%nrOfBoundaryCells = 0
  end subroutine
  
  ! add connection between node n and m (local numbering)
  ! NB: total nr of connections should match reserved space
  subroutine setBoundaryCell(this, boundaryIdx)
    class(GridConnectionType), intent(in) :: this
    integer(I4B)                          :: boundaryIdx  ! index of boundary cell
            
    this%nrOfBoundaryCells = this%nrOfBoundaryCells + 1    
    if (this%nrOfBoundaryCells > this%linkCapacity) then
      write(*,*) 'Error: not enough memory reserved for storing grid connection, terminating...'
      call ustop()
    end if
    
    this%boundaryCells(this%nrOfBoundaryCells)%cell%index = boundaryIdx
    this%boundaryCells(this%nrOfBoundaryCells)%cell%model => this%model
  end subroutine setBoundaryCell
  
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
  subroutine extendConnection(this, depth)    
    class(GridConnectionType), intent(inout) :: this  
    integer(I4B) :: depth ! depth == 1: nbrs, depth == 2: nbr-of-nbrs
    ! local
    integer(I4B) :: icell
    integer(I4B) :: imod, regionSize, offset
    class(NumericalModelType), pointer :: numModel
   
    ! add neighbors
    do icell = 1, this%nrOfBoundaryCells
      call this%addNeighbors(this%boundaryCells(icell), depth)
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
  recursive subroutine addNeighbors(this, cellNbrs, depth, mask)
    use SimModule, only: ustop
    class(GridConnectionType), intent(inout)  :: this
    type(CellWithNbrsType), intent(inout)     :: cellNbrs    
    integer(I4B), intent(inout)               :: depth
    type(GlobalCellType), optional            :: mask
    ! local
    integer(I4B)                              :: idx, nbrIdx, ipos, inbr, iexg
    type(ConnectionsType), pointer            :: conn
    integer(I4B)                              :: newDepth

    type(ModelWithNbrsType), pointer          :: modelWithNbrs
    
    if (depth < 1) then
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
    call this%getModelWithNbrs(cellNbrs%cell%model, modelWithNbrs)
    call this%addRemoteNeighbors(cellNbrs, modelWithNbrs, mask)
    
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
    class(GridConnectionType), intent(inout) :: this 
    ! local
    integer(I4B) :: icell
    integer(I4B), dimension(:), allocatable :: nnz
    type(SparseMatrix), pointer :: sparse 
    
    ! generate interface cell index for boundary cells, recursively
    ! and buildmapping
    this%indexCount = 0
    do icell = 1, this%nrOfBoundaryCells
      call this%registerInterfaceCells(this%boundaryCells(icell))
    end do
    this%nrOfCells = this%indexCount
    
    
    allocate(sparse)
    allocate(nnz(this%nrOfCells))
    nnz = MaxNeighbors+1
    call sparse%init(this%nrOfCells, this%nrOfCells, nnz)
    
    ! add the connections, recursively
    do icell = 1, this%nrOfBoundaryCells
      call this%connectInterfaceCells(this%boundaryCells(icell), sparse)
    end do
    
  end subroutine buildConnections
  
  recursive subroutine registerInterfaceCells(this, cell)
    class(GridConnectionType), intent(inout) :: this
    type(CellWithNbrsType)                   :: cell
    ! local
    integer(I4B) :: offset, inbr
    integer(I4B) :: regionIdx  ! unique idx in the region (all connected models)
    integer(I4B) :: ifaceIdx   ! unique idx in the interface grid
    
    offset = this%getRegionalModelOffset(cell%cell%model)
    regionIdx = offset + cell%cell%index
    ifaceIdx = this%regionalToInterfaceIdxMap(regionIdx)
    if (ifaceIdx == -1) then
      this%indexCount = this%indexCount + 1
      ifaceIdx = this%indexCount
      this%regionalToInterfaceIdxMap(regionIdx) = ifaceIdx
    end if   
    
    ! and also for its neighbors
    do inbr = 1, cell%nrOfNbrs
      call this%registerInterfaceCells(cell%neighbors(inbr))
    end do
      
  end subroutine registerInterfaceCells
   
  recursive subroutine connectInterfaceCells(this, cell, sparse)
    class(GridConnectionType), intent(inout)  :: this
    type(CellWithNbrsType)                    :: cell
    type(SparseMatrix), pointer               :: sparse
    ! local
    integer(I4B) :: regionIdx, regionIdxNbr  ! unique idx in the region (all connected models)
    integer(I4B) :: ifaceIdx, ifaceIdxNbr    ! unique idx in the interface grid
    integer(I4B) :: offset, inbr
    
    offset = this%getRegionalModelOffset(cell%cell%model)
    regionIdx = offset + cell%cell%index
    ifaceIdx = this%regionalToInterfaceIdxMap(regionIdx)
    
    call sparse%addconnection(ifaceIdx, ifaceIdx, 1)
    
    do inbr = 1, cell%nrOfNbrs
      offset = this%getRegionalModelOffset(cell%neighbors(inbr)%cell%model)
      regionIdxNbr = offset + cell%neighbors(inbr)%cell%index
      ifaceIdxNbr = this%regionalToInterfaceIdxMap(regionIdxNbr)
      
      call sparse%addconnection(ifaceIdx, ifaceIdxNbr, 1)
      call sparse%addconnection(ifaceIdxNbr, ifaceIdx, 1)
      
      ! recurse
      call this%connectInterfaceCells(cell%neighbors(inbr), sparse)
    end do
    
  end subroutine connectInterfaceCells
  
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
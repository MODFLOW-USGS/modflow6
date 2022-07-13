module GridConnectionModule
  use KindModule, only: I4B, DP, LGP
  use SimModule, only: ustop
  use ConstantsModule, only: LENMEMPATH, DZERO, DPIO180
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use MemoryHelperModule, only: create_mem_path
  use ListModule, only: ListType, isEqualIface, arePointersEqual
  use NumericalModelModule
  use GwfDisuModule
  use DisConnExchangeModule
  use CellWithNbrsModule
  use ConnectionsModule
  use SparseModule, only: sparsematrix
  implicit none
  private

  ! Initial nr of neighbors for sparse matrix allocation
  integer(I4B), parameter :: InitNrNeighbors = 7

  !> This class is used to construct the connections object for
  !! the interface model's spatial discretization/grid.
  !!
  !! It works as follows:
  !!
  !! 1: construct basic instance, allocate data structures
  !!    based on nr. of primary connections
  !! 2: add primary connections from exchanges
  !! 3: add secondary, tertiary, ... MODEL connections, depending
  !!    on the size of the computational stencil
  !! 4: extend the connection, creating the full data structure
  !!    and relations
  !! 5: build the connections object, from which the grid for
  !!    the interface model can be constructed
  !!
  !! A note on the different indices:
  !!
  !! We have
  !!
  !! - GLOBAL index, which technically labels the row in the solution matrix
  !! - REGIONAL index, running over all models that participate in the interface grid
  !! - LOCAL index, local to each model
  !! - INTERFACE index, numbering the cells in the interface grid
  !<
  type, public :: GridConnectionType

    character(len=LENMEMPATH) :: memoryPath
    integer(I4B) :: internalStencilDepth !< stencil size for the interior
    integer(I4B) :: exchangeStencilDepth !< stencil size at the interface

    class(NumericalModelType), pointer :: model => null() !< the model for which this grid connection exists

    integer(I4B), pointer :: nrOfBoundaryCells => null() !< nr of boundary cells with connection to another model
    type(CellWithNbrsType), dimension(:), pointer :: boundaryCells => null() !< cells on our side of the primary connections
    type(CellWithNbrsType), dimension(:), pointer :: connectedCells => null() !< cells on the neighbors side of the primary connection
    type(ListType) :: exchanges !< all relevant exchanges for this connection, up to the required depth

    integer, dimension(:), pointer :: primConnections => null() !< table mapping the index in the boundaryCells/connectedCells

    integer(I4B), pointer :: nrOfCells => null() !< the total number of cells in the interface
    type(GlobalCellType), dimension(:), pointer :: idxToGlobal => null() !< a map from interface index to global coordinate
    integer(I4B), dimension(:), pointer, contiguous :: idxToGlobalIdx => null() !< a (flat) map from interface index to global index,
                                                                                !! stored in mem. mgr. so can be used for debugging

    integer(I4B), dimension(:), pointer :: regionalToInterfaceIdxMap => null() !< (sparse) mapping from regional index to interface ixd
    type(ListType) :: regionalModels !< the models participating in the interface
    integer(I4B), dimension(:), pointer :: regionalModelOffset => null() !< the new offset to compactify the range of indices
    integer(I4B), pointer :: indexCount => null() !< counts the number of cells in the interface
    type(ConnectionsType), pointer :: connections => null() !< sparse matrix with the connections
    integer(I4B), dimension(:), pointer :: connectionMask => null() !< to mask out connections from the amat coefficient calculation

  contains
    ! public
    procedure, pass(this) :: construct
    procedure, private, pass(this) :: allocateScalars
    procedure, public, pass(this) :: destroy
    procedure, pass(this) :: connectCell
    procedure, pass(this) :: findModelNeighbors
    procedure, pass(this) :: extendConnection
    generic :: getInterfaceIndex => getInterfaceIndexByCell, &
      getInterfaceIndexByIndexModel

    procedure, pass(this) :: getDiscretization

    ! 'protected'
    procedure, pass(this) :: isPeriodic

    ! private routines
    procedure, private, pass(this) :: buildConnections
    procedure, private, pass(this) :: addNeighbors
    procedure, private, pass(this) :: addNeighborCell
    procedure, private, pass(this) :: addRemoteNeighbors
    procedure, private, pass(this) :: addModelNeighbors
    procedure, private, pass(this) :: addToRegionalModels
    procedure, private, pass(this) :: getRegionalModelOffset
    procedure, private, pass(this) :: getInterfaceIndexByCell
    procedure, private, pass(this) :: getInterfaceIndexByIndexModel
    procedure, private, pass(this) :: registerInterfaceCells
    procedure, private, pass(this) :: addToGlobalMap
    procedure, private, pass(this) :: compressGlobalMap
    procedure, private, pass(this) :: sortInterfaceGrid
    procedure, private, pass(this) :: makePrimaryConnections
    procedure, private, pass(this) :: connectNeighborCells
    procedure, private, pass(this) :: fillConnectionDataInternal
    procedure, private, pass(this) :: fillConnectionDataFromExchanges
    procedure, private, pass(this) :: createConnectionMask
    procedure, private, pass(this) :: maskInternalConnections
    procedure, private, pass(this) :: setMaskOnConnection
    procedure, private, pass(this) :: createLookupTable
  end type

contains

  !> @brief Construct the GridConnection and allocate
  !! the data structures for the primary connections
  !<
  subroutine construct(this, model, nrOfPrimaries, connectionName)
    class(GridConnectionType), intent(inout) :: this !> this instance
    class(NumericalModelType), pointer, intent(in) :: model !> the model for which the interface is constructed
    integer(I4B) :: nrOfPrimaries !> the number of primary connections between the two models
    character(len=*) :: connectionName !> the name, for memory management mostly
    ! local

    this%model => model
    this%memoryPath = create_mem_path(connectionName, 'GC')

    call this%allocateScalars()

    allocate (this%boundaryCells(nrOfPrimaries))
    allocate (this%connectedCells(nrOfPrimaries))
    allocate (this%primConnections(nrOfPrimaries))
    allocate (this%idxToGlobal(2 * nrOfPrimaries))

    call this%addToRegionalModels(model)

    this%nrOfBoundaryCells = 0

    this%internalStencilDepth = 1
    this%exchangeStencilDepth = 1

  end subroutine construct

  !> @brief Connect neighboring cells at the interface by
  !! storing them in the boundary cell and connected cell
  !! arrays
  !<
  subroutine connectCell(this, idx1, model1, idx2, model2)
    class(GridConnectionType), intent(in) :: this !< this grid connection
    integer(I4B) :: idx1 !< local index cell 1
    class(NumericalModelType), pointer :: model1 !< model of cell 1
    integer(I4B) :: idx2 !< local index cell 2
    class(NumericalModelType), pointer :: model2 !< model of cell 2

    this%nrOfBoundaryCells = this%nrOfBoundaryCells + 1
    if (this%nrOfBoundaryCells > size(this%boundaryCells)) then
      write (*, *) 'Error: nr of cell connections exceeds '// &
        'capacity in grid connection, terminating...'
      call ustop()
    end if

    if (associated(model1, this%model)) then
      this%boundaryCells(this%nrOfBoundaryCells)%cell%index = idx1
      this%boundaryCells(this%nrOfBoundaryCells)%cell%model => this%model

      this%connectedCells(this%nrOfBoundaryCells)%cell%index = idx2
      this%connectedCells(this%nrOfBoundaryCells)%cell%model => model2
    else if (associated(model2, this%model)) then
      this%boundaryCells(this%nrOfBoundaryCells)%cell%index = idx2
      this%boundaryCells(this%nrOfBoundaryCells)%cell%model => this%model

      this%connectedCells(this%nrOfBoundaryCells)%cell%index = idx1
      this%connectedCells(this%nrOfBoundaryCells)%cell%model => model1
    else
      write (*, *) 'Error: unable to connect cells outside the model'
      call ustop()
    end if

  end subroutine connectCell

  !> @brief Create the tree structure with all model nbrs, nbrs-of-nbrs,
  !< etc. for this model up to the specified depth
  subroutine findModelNeighbors(this, globalExchanges, depth)
    class(GridConnectionType), intent(inout) :: this !< this grid connection
    type(ListType), intent(inout) :: globalExchanges !< list with global exchanges
    integer(I4B) :: depth !< the maximal number of exchanges between
                          !! any two models in the topology

    call this%addModelNeighbors(this%model, globalExchanges, depth)

  end subroutine findModelNeighbors

  !> @brief Add neighbors and nbrs-of-nbrs to the model tree
  !<
  recursive subroutine addModelNeighbors(this, model, &
                                         globalExchanges, &
                                         depth, mask)
    class(GridConnectionType), intent(inout) :: this !< this grid connection
    class(NumericalModelType), pointer, intent(inout) :: model !< the model to add neighbors for
    type(ListType), intent(inout) :: globalExchanges !< list with all exchanges
    integer(I4B) :: depth !< the maximal number of exchanges between
    class(NumericalModelType), pointer, optional :: mask !< don't add this one a neighbor
    ! local
    integer(I4B) :: i, n
    class(DisConnExchangeType), pointer :: connEx
    class(NumericalModelType), pointer :: neighborModel
    class(NumericalModelType), pointer :: modelMask
    type(ListType) :: nbrModels
    class(*), pointer :: objPtr
    procedure(isEqualIface), pointer :: areEqualMethod

    if (.not. present(mask)) then
      modelMask => null()
    else
      modelMask => mask
    end if

    ! first find all direct neighbors of the model and add them,
    ! avoiding duplicates
    do i = 1, globalExchanges%Count()

      neighborModel => null()
      connEx => GetDisConnExchangeFromList(globalExchanges, i)
      if (associated(model, connEx%model1)) then
        neighborModel => connEx%model2
      else if (associated(model, connEx%model2)) then
        neighborModel => connEx%model1
      end if

      ! check if there is a neighbor, and it is not masked
      ! (to prevent back-and-forth connections)
      if (associated(neighborModel) .and. .not. &
          associated(neighborModel, modelMask)) then

        ! add to neighbors
        objPtr => neighborModel
        if (.not. nbrModels%ContainsObject(objPtr, areEqualMethod)) then
          call nbrModels%Add(objPtr)
        end if

        ! add to list of regional models
        call this%addToRegionalModels(neighborModel)

        ! add to list of relevant exchanges
        objPtr => connEx
        areEqualMethod => arePointersEqual
        if (.not. this%exchanges%ContainsObject(objPtr, areEqualMethod)) then
          call this%exchanges%Add(objPtr)
        end if

      end if
    end do

    ! now recurse on the neighbors up to the specified depth
    depth = depth - 1
    if (depth == 0) return

    do n = 1, nbrModels%Count()
      neighborModel => GetNumericalModelFromList(nbrModels, n)
      call this%addModelNeighbors(neighborModel, globalExchanges, depth, model)
    end do

    ! clear list
    call nbrModels%Clear(destroy=.false.)

  end subroutine addModelNeighbors

  !> @brief Add a model to a list of all regional models
  !<
  subroutine addToRegionalModels(this, modelToAdd)
    class(GridConnectionType), intent(inout) :: this !< this grid connection
    class(NumericalModelType), pointer :: modelToAdd !< the model to add to the region
    ! local
    class(*), pointer :: mPtr
    procedure(isEqualIface), pointer :: areEqualMethod

    mPtr => modelToAdd
    areEqualMethod => arePointersEqual
    if (.not. this%regionalModels%ContainsObject(mPtr, areEqualMethod)) then
      call AddNumericalModelToList(this%regionalModels, modelToAdd)
    end if

  end subroutine addToRegionalModels

  !> @brief Extend the connection topology to deal with
  !! higher levels of connectivity (neighbors-of-neighbors, etc.)
  !!
  !! The following steps are taken:
  !! 1. Recursively add interior neighbors (own model) up to the specified depth
  !! 2. Recursively add exterior neighbors
  !! 3. Allocate a (sparse) mapping table for the region
  !! 4. Build connection object for the interface grid, and the mask
  !<
  subroutine extendConnection(this)
    class(GridConnectionType), intent(inout) :: this !< this grid connection
    ! local
    integer(I4B) :: remoteDepth, localDepth
    integer(I4B) :: icell
    integer(I4B) :: imod, regionSize, offset
    class(NumericalModelType), pointer :: numModel

    ! we need (stencildepth-1) extra cells for the interior
    remoteDepth = this%exchangeStencilDepth
    localDepth = 2 * this%internalStencilDepth - 1
    if (localDepth < remoteDepth) then
      localDepth = remoteDepth
    end if

    ! first add the neighbors for the interior
    ! (possibly extending into other models)
    do icell = 1, this%nrOfBoundaryCells
      call this%addNeighbors(this%boundaryCells(icell), localDepth, &
                             this%connectedCells(icell)%cell, .true.)
    end do
    ! and for the exterior
    do icell = 1, this%nrOfBoundaryCells
      call this%addNeighbors(this%connectedCells(icell), remoteDepth, &
                             this%boundaryCells(icell)%cell, .false.)
    end do

    ! set up mapping for the region (models participating in interface model grid)
    allocate (this%regionalModelOffset(this%regionalModels%Count()))
    regionSize = 0
    offset = 0
    do imod = 1, this%regionalModels%Count()
      numModel => GetNumericalModelFromList(this%regionalModels, imod)
      regionSize = regionSize + numModel%dis%nodes
      this%regionalModelOffset(imod) = offset
      offset = offset + numModel%dis%nodes
    end do
    ! init to -1, meaning 'interface index was not assigned yet'
    allocate (this%regionalToInterfaceIdxMap(regionSize))
    this%regionalToInterfaceIdxMap = -1

    call this%buildConnections()

  end subroutine extendConnection

  !> @brief Builds a sparse matrix holding all cell connections,
  !< with new indices, and stores the mapping to the global ids
  subroutine buildConnections(this)
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    ! local
    integer(I4B) :: icell, iconn
    integer(I4B), dimension(:), allocatable :: nnz
    type(SparseMatrix), pointer :: sparse
    integer(I4B) :: ierror
    type(ConnectionsType), pointer :: conn

    ! Recursively generate interface cell indices, fill map to global cells,
    ! and add to region lookup table
    this%indexCount = 0
    do icell = 1, this%nrOfBoundaryCells
      call this%registerInterfaceCells(this%boundaryCells(icell))
    end do
    do icell = 1, this%nrOfBoundaryCells
      call this%registerInterfaceCells(this%connectedCells(icell))
    end do
    this%nrOfCells = this%indexCount

    ! compress lookup table
    call this%compressGlobalMap()

    ! sort interface indexes such that 'n > m' means 'n below m'
    call this%sortInterfaceGrid()

    ! allocate a map from interface index to global coordinates
    call mem_allocate(this%idxToGlobalIdx, this%nrOfCells, &
                      'IDXTOGLOBALIDX', this%memoryPath)

    ! create sparse data structure, to temporarily hold connections
    allocate (sparse)
    allocate (nnz(this%nrOfCells))
    nnz = InitNrNeighbors + 1
    call sparse%init(this%nrOfCells, this%nrOfCells, nnz)

    ! now (recursively) add connections to sparse, start with
    ! the primary connections (n-m from the exchange files)
    call this%makePrimaryConnections(sparse)
    ! then into own domain
    do icell = 1, this%nrOfBoundaryCells
      call this%connectNeighborCells(this%boundaryCells(icell), sparse)
    end do
    ! and same for the neighbors of connected cells
    do icell = 1, this%nrOfBoundaryCells
      call this%connectNeighborCells(this%connectedCells(icell), sparse)
    end do

    ! create connections object
    allocate (this%connections)
    conn => this%connections
    call conn%allocate_scalars(this%memoryPath)
    conn%nodes = this%nrOfCells
    conn%nja = sparse%nnz
    conn%njas = (conn%nja - conn%nodes) / 2
    call conn%allocate_arrays()
    do iconn = 1, conn%njas
      conn%anglex(iconn) = -999.
    end do

    ! fill connection from sparse
    call sparse%filliaja(conn%ia, conn%ja, ierror)
    if (ierror /= 0) then
      write (*, *) 'Error filling ia/ja in GridConnection: terminating...'
      call ustop()
    end if
    call fillisym(conn%nodes, conn%nja, conn%ia, conn%ja, conn%isym)
    call filljas(conn%nodes, conn%nja, conn%ia, conn%ja, conn%isym, conn%jas)
    call sparse%destroy()

    ! fill connection data (ihc, cl1, cl2, etc.) using data
    ! from models and exchanges
    call this%fillConnectionDataInternal()
    call this%fillConnectionDataFromExchanges()

    ! set the masks on connections
    call this%createConnectionMask()

    ! create lookup table(s)
    call this%createLookupTable()

  end subroutine buildConnections

  !< @brief Routine for finding neighbors-of-neighbors, recursively
  !<
  recursive subroutine addNeighbors(this, cellNbrs, depth, mask, interior)
    use SimModule, only: ustop
    class(GridConnectionType), intent(inout) :: this !< this grid connection
    type(CellWithNbrsType), intent(inout) :: cellNbrs !< cell to add to
    integer(I4B), intent(inout) :: depth !< current depth (typically decreases in recursion)
    type(GlobalCellType), optional :: mask !< mask to excluded back-and-forth connection between cells
    logical(LGP) :: interior !< when true, we are adding from the exchange back into the model
    ! local
    integer(I4B) :: nbrIdx, ipos, inbr
    type(ConnectionsType), pointer :: conn
    integer(I4B) :: newDepth

    ! if depth == 1, then we are not adding neighbors but use
    ! the boundary and connected cell only
    if (depth < 2) then
      return
    end if
    newDepth = depth - 1

    conn => cellNbrs%cell%model%dis%con

    ! find neighbors local to this cell by looping through grid connections
    do ipos = conn%ia(cellNbrs%cell%index) + 1, &
      conn%ia(cellNbrs%cell%index + 1) - 1
      nbrIdx = conn%ja(ipos)
      call this%addNeighborCell(cellNbrs, nbrIdx, cellNbrs%cell%model, mask)
    end do

    ! add remote nbr using the data from the exchanges
    call this%addRemoteNeighbors(cellNbrs, mask)

    ! now find nbr-of-nbr
    do inbr = 1, cellNbrs%nrOfNbrs

      ! are we leaving the model through another exchange?
      if (interior .and. associated(cellNbrs%cell%model, this%model)) then
        if (.not. associated(cellNbrs%neighbors(inbr)%cell%model, &
                             this%model)) then
          ! decrement by 1, because the connection we are crossing is not
          ! calculated by this interface
          newDepth = newDepth - 1
        end if
      end if
      ! and add neigbors with the new depth
      call this%addNeighbors(cellNbrs%neighbors(inbr), newDepth, &
                             cellNbrs%cell, interior)
    end do

  end subroutine addNeighbors

  !> @brief Add cell neighbors across models using the stored exchange
  !! data structures
  subroutine addRemoteNeighbors(this, cellNbrs, mask)
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    type(CellWithNbrsType), intent(inout) :: cellNbrs !< cell to add to
    type(GlobalCellType), optional :: mask !< a mask to exclude back-and-forth connections
    ! local
    integer(I4B) :: ix, iexg
    type(DisConnExchangeType), pointer :: connEx

    ! loop over all exchanges
    do ix = 1, this%exchanges%Count()
      connEx => GetDisConnExchangeFromList(this%exchanges, ix)

      ! loop over n-m links in the exchange
      if (associated(cellNbrs%cell%model, connEx%model1)) then
        do iexg = 1, connEx%nexg
          if (connEx%nodem1(iexg) == cellNbrs%cell%index) then
            ! we have a link, now add foreign neighbor
            call this%addNeighborCell(cellNbrs, connEx%nodem2(iexg), &
                                      connEx%model2, mask)
          end if
        end do
      end if
      ! and the reverse
      if (associated(cellNbrs%cell%model, connEx%model2)) then
        do iexg = 1, connEx%nexg
          if (connEx%nodem2(iexg) == cellNbrs%cell%index) then
            ! we have a link, now add foreign neighbor
            call this%addNeighborCell(cellNbrs, connEx%nodem1(iexg), &
                                      connEx%model1, mask)
          end if
        end do
      end if

    end do

  end subroutine addRemoteNeighbors

  !> @brief Add neighboring cell to tree structure
  !<
  subroutine addNeighborCell(this, cellNbrs, newNbrIdx, nbrModel, mask)
    class(GridConnectionType), intent(in) :: this !< this grid connection instance
    type(CellWithNbrsType), intent(inout) :: cellNbrs !< the root cell which to add to
    integer(I4B), intent(in) :: newNbrIdx !< the neigboring cell's index
    class(NumericalModelType), pointer :: nbrModel !< the model where the new neighbor lives
    type(GlobalCellType), optional :: mask !< don't add connections to this cell (optional)
    ! local

    if (present(mask)) then
      if (newNbrIdx == mask%index .and. associated(nbrModel, mask%model)) then
        return
      end if
    end if
    call cellNbrs%addNbrCell(newNbrIdx, nbrModel)

  end subroutine addNeighborCell

  !> @brief Recursively set interface cell indexes and
  !< add to the region-to-interface loopup table
  recursive subroutine registerInterfaceCells(this, cellWithNbrs)
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    type(CellWithNbrsType) :: cellWithNbrs !< the cell from where to start registering neighbors
    ! local
    integer(I4B) :: offset, inbr
    integer(I4B) :: regionIdx ! unique idx in the region (all connected models)
    integer(I4B) :: ifaceIdx ! unique idx in the interface grid

    offset = this%getRegionalModelOffset(cellWithNbrs%cell%model)
    regionIdx = offset + cellWithNbrs%cell%index
    ifaceIdx = this%getInterfaceIndex(cellWithNbrs%cell)
    if (ifaceIdx == -1) then
      this%indexCount = this%indexCount + 1
      ifaceIdx = this%indexCount
      call this%addToGlobalMap(ifaceIdx, cellWithNbrs%cell)
      this%regionalToInterfaceIdxMap(regionIdx) = ifaceIdx
    end if

    ! and also for its neighbors
    do inbr = 1, cellWithNbrs%nrOfNbrs
      call this%registerInterfaceCells(cellWithNbrs%neighbors(inbr))
    end do

  end subroutine registerInterfaceCells

  !> @brief Add entry to lookup table, inflating when necessary
  !<
  subroutine addToGlobalMap(this, ifaceIdx, cell)
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    integer(I4B), intent(in) :: ifaceIdx !< unique idx in the interface grid
    type(GlobalCellType), intent(in) :: cell !< the global cell
    ! local
    integer(I4B) :: i, currentSize, newSize
    type(GlobalCellType), dimension(:), pointer :: tempMap

    ! inflate?
    currentSize = size(this%idxToGlobal)
    if (ifaceIdx > currentSize) then
      newSize = nint(1.5 * currentSize)
      allocate (tempMap(newSize))
      do i = 1, currentSize
        tempMap(i) = this%idxToGlobal(i)
      end do

      deallocate (this%idxToGlobal)
      this%idxToGlobal => tempMap
    end if

    this%idxToGlobal(ifaceIdx) = cell

  end subroutine addToGlobalMap

  !> @brief Compress lookup table to get rid of unused entries
  !<
  subroutine compressGlobalMap(this)
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    ! local
    type(GlobalCellType), dimension(:), pointer :: tempMap

    if (size(this%idxToGlobal) > this%nrOfCells) then
      allocate (tempMap(this%nrOfCells))
      tempMap(1:this%nrOfCells) = this%idxToGlobal(1:this%nrOfCells)
      deallocate (this%idxToGlobal)
      allocate (this%idxToGlobal(this%nrOfCells))
      this%idxToGlobal(1:this%nrOfCells) = tempMap(1:this%nrOfCells)
      deallocate (tempMap)
    end if

  end subroutine compressGlobalMap

  !> @brief Soft cell ids in the interface grid such that
  !< id_1 < id_2 means that cell 1 lies above cell 2
  subroutine sortInterfaceGrid(this)
    use GridSorting, only: quickSortGrid
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    ! local
    integer(I4B), dimension(:), allocatable :: newToOldIdx
    integer(I4B), dimension(:), allocatable :: oldToNewIdx
    integer(I4B) :: idxOld
    integer(I4B) :: i
    type(GlobalCellType), dimension(:), allocatable :: sortedGlobalMap
    integer(I4B), dimension(:), allocatable :: sortedRegionMap

    ! sort based on coordinates
    newToOldIdx = (/(i, i=1, size(this%idxToGlobal))/)
    call quickSortGrid(newToOldIdx, size(newToOldIdx), this%idxToGlobal)

    ! and invert
    allocate (oldToNewIdx(size(newToOldIdx)))
    do i = 1, size(oldToNewIdx)
      oldToNewIdx(newToOldIdx(i)) = i
    end do

    ! reorder global table
    allocate (sortedGlobalMap(size(this%idxToGlobal)))
    do i = 1, size(newToOldIdx)
      sortedGlobalMap(i) = this%idxToGlobal(newToOldIdx(i))
    end do
    do i = 1, size(newToOldIdx)
      this%idxToGlobal(i) = sortedGlobalMap(i)
    end do
    deallocate (sortedGlobalMap)

    ! reorder regional lookup table
    allocate (sortedRegionMap(size(this%regionalToInterfaceIdxMap)))
    do i = 1, size(sortedRegionMap)
      if (this%regionalToInterfaceIdxMap(i) /= -1) then
        idxOld = this%regionalToInterfaceIdxMap(i)
        sortedRegionMap(i) = oldToNewIdx(idxOld)
      else
        sortedRegionMap(i) = -1
      end if
    end do
    do i = 1, size(sortedRegionMap)
      this%regionalToInterfaceIdxMap(i) = sortedRegionMap(i)
    end do
    deallocate (sortedRegionMap)

  end subroutine sortInterfaceGrid

  !> @brief Add primary connections to the sparse data structure
  !<
  subroutine makePrimaryConnections(this, sparse)
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    type(SparseMatrix), pointer :: sparse !< the sparse data structure to hold the connections
    ! local
    integer(I4B) :: icell
    integer(I4B) :: ifaceIdx, ifaceIdxNbr

    do icell = 1, this%nrOfBoundaryCells
      ifaceIdx = this%getInterfaceIndex(this%boundaryCells(icell)%cell)
      ifaceIdxNbr = this%getInterfaceIndex(this%connectedCells(icell)%cell)

      ! add diagonals to sparse
      call sparse%addconnection(ifaceIdx, ifaceIdx, 1)
      call sparse%addconnection(ifaceIdxNbr, ifaceIdxNbr, 1)

      ! and cross terms
      call sparse%addconnection(ifaceIdx, ifaceIdxNbr, 1)
      call sparse%addconnection(ifaceIdxNbr, ifaceIdx, 1)
    end do

  end subroutine makePrimaryConnections

  !> @brief Recursively add higher order connections (from
  !! cells neighoring the primarily connected cells) to the
  !< sparse data structure
  recursive subroutine connectNeighborCells(this, cell, sparse)
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    type(CellWithNbrsType) :: cell !< the cell whose connections is to be added
    type(SparseMatrix), pointer :: sparse !< the sparse data structure to hold the connections
    ! local
    integer(I4B) :: ifaceIdx, ifaceIdxNbr ! unique idx in the interface grid
    integer(I4B) :: inbr

    ifaceIdx = this%getInterfaceIndex(cell%cell)
    do inbr = 1, cell%nrOfNbrs
      ifaceIdxNbr = this%getInterfaceIndex(cell%neighbors(inbr)%cell)

      call sparse%addconnection(ifaceIdxNbr, ifaceIdxNbr, 1)
      call sparse%addconnection(ifaceIdx, ifaceIdxNbr, 1)
      call sparse%addconnection(ifaceIdxNbr, ifaceIdx, 1)

      ! recurse
      call this%connectNeighborCells(cell%neighbors(inbr), sparse)
    end do

  end subroutine connectNeighborCells

  !> @brief Fill connection data (ihc, cl1, ...) for
  !< connections between cells within the same model.
  subroutine fillConnectionDataInternal(this)
    use ConstantsModule, only: DPI, DTWOPI
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    ! local
    type(ConnectionsType), pointer :: conn, connOrig
    integer(I4B) :: n, m, ipos, isym, iposOrig, isymOrig
    type(GlobalCellType), pointer :: ncell, mcell

    conn => this%connections

    do n = 1, conn%nodes
      do ipos = conn%ia(n) + 1, conn%ia(n + 1) - 1
        m = conn%ja(ipos)
        if (n > m) cycle

        isym = conn%jas(ipos)
        ncell => this%idxToGlobal(n)
        mcell => this%idxToGlobal(m)
        if (associated(ncell%model, mcell%model)) then
          ! within same model, straight copy
          connOrig => ncell%model%dis%con
          iposOrig = connOrig%getjaindex(ncell%index, mcell%index)
          if (iposOrig == 0) then
            ! periodic boundary conditions can add connections between cells in
            ! the same model, but they are dealt with through the exchange data
            if (this%isPeriodic(ncell%index, mcell%index)) cycle

            ! this should not be possible
            write (*, *) 'Error: cannot find cell connection in model grid'
            call ustop()
          end if

          isymOrig = connOrig%jas(iposOrig)
          conn%hwva(isym) = connOrig%hwva(isymOrig)
          conn%ihc(isym) = connOrig%ihc(isymOrig)
          if (ncell%index < mcell%index) then
            conn%cl1(isym) = connOrig%cl1(isymOrig)
            conn%cl2(isym) = connOrig%cl2(isymOrig)
            conn%anglex(isym) = connOrig%anglex(isymOrig)
          else
            conn%cl1(isym) = connOrig%cl2(isymOrig)
            conn%cl2(isym) = connOrig%cl1(isymOrig)
            conn%anglex(isym) = mod(connOrig%anglex(isymOrig) + DPI, DTWOPI)
          end if
        end if
      end do
    end do
  end subroutine fillConnectionDataInternal

  !> @brief Fill connection data (ihc, cl1, ...) for
  !< all exchanges
  subroutine fillConnectionDataFromExchanges(this)
    use ConstantsModule, only: DPI, DTWOPI, DPIO180
    use ArrayHandlersModule, only: ifind
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    ! local
    integer(I4B) :: inx, iexg, ivalAngldegx
    integer(I4B) :: ipos, isym
    integer(I4B) :: nOffset, mOffset, nIfaceIdx, mIfaceIdx
    class(DisConnExchangeType), pointer :: connEx
    type(ConnectionsType), pointer :: conn

    conn => this%connections

    do inx = 1, this%exchanges%Count()
      connEx => GetDisConnExchangeFromList(this%exchanges, inx)

      ivalAngldegx = -1
      if (connEx%naux > 0) then
        ivalAngldegx = ifind(connEx%auxname, 'ANGLDEGX')
        if (ivalAngldegx > 0) then
          conn%ianglex = 1
        end if
      end if

      nOffset = this%getRegionalModelOffset(connEx%model1)
      mOffset = this%getRegionalModelOffset(connEx%model2)
      do iexg = 1, connEx%nexg
        nIfaceIdx = this%regionalToInterfaceIdxMap(noffset + connEx%nodem1(iexg))
        mIfaceIdx = this%regionalToInterfaceIdxMap(moffset + connEx%nodem2(iexg))
        ! not all nodes from the exchanges are part of the interface grid
        ! (think of exchanges between neigboring models, and their neighbors)
        if (nIFaceIdx == -1 .or. mIFaceIdx == -1) then
          cycle
        end if

        ipos = conn%getjaindex(nIfaceIdx, mIfaceIdx)
        ! (see prev. remark) sometimes the cells are in the interface grid,
        ! but the connection isn't. This can happen for leaf nodes of the grid.
        if (ipos == 0) then
          ! no match, safely cycle
          cycle
        end if
        isym = conn%jas(ipos)

        ! note: cl1 equals L_nm: the length from cell n to the shared
        ! face with cell m (and cl2 analogously for L_mn)
        if (nIfaceIdx < mIfaceIdx) then
          conn%cl1(isym) = connEx%cl1(iexg)
          conn%cl2(isym) = connEx%cl2(iexg)
          if (ivalAngldegx > 0) then
            conn%anglex(isym) = connEx%auxvar(ivalAngldegx, iexg) * DPIO180
          end if
        else
          conn%cl1(isym) = connEx%cl2(iexg)
          conn%cl2(isym) = connEx%cl1(iexg)
          if (ivalAngldegx > 0) then
            conn%anglex(isym) = mod(connEx%auxvar(ivalAngldegx, iexg) + &
                                    180.0_DP, 360.0_DP) * DPIO180
          end if
        end if
        conn%hwva(isym) = connEx%hwva(iexg)
        conn%ihc(isym) = connEx%ihc(iexg)

      end do
    end do

  end subroutine fillConnectionDataFromExchanges

  !> @brief Create the connection masks
  !!
  !! The level indicates the nr of connections away from
  !! the remote neighbor, the diagonal term holds the negated
  !! value of their nearest connection. We end with setting
  !< a normalized mask: 0 or 1
  subroutine createConnectionMask(this)
    class(GridConnectionType), intent(inout) :: this !< instance of this grid connection
    ! local
    integer(I4B) :: icell, inbr, n, ipos
    integer(I4B) :: level, newMask
    type(CellWithNbrsType), pointer :: cell, nbrCell

    ! set all masks to zero to begin with
    do ipos = 1, this%connections%nja
      call this%connections%set_mask(ipos, 0)
    end do

    ! remote connections remain masked
    ! now set mask for exchange connections (level == 1)
    level = 1
    do icell = 1, this%nrOfBoundaryCells
      call this%setMaskOnConnection(this%boundaryCells(icell), &
                                    this%connectedCells(icell), level)
      ! for cross-boundary connections, we need to apply the mask to both n-m and m-n,
      ! because if the upper triangular one is disabled, its transposed (lower triangular)
      ! counter part is skipped in the NPF calculation as well.
      call this%setMaskOnConnection(this%connectedCells(icell), &
                                    this%boundaryCells(icell), level)
    end do

    ! now extend mask recursively into the internal domain (level > 1)
    do icell = 1, this%nrOfBoundaryCells
      cell => this%boundaryCells(icell)
      do inbr = 1, cell%nrOfNbrs
        nbrCell => this%boundaryCells(icell)%neighbors(inbr)
        level = 2 ! this is incremented within the recursion
        call this%maskInternalConnections(this%boundaryCells(icell), &
                                          this%boundaryCells(icell)% &
                                          neighbors(inbr), level)
      end do
    end do

    ! set normalized mask:
    ! =1 for links with connectivity <= interior stencil depth
    ! =0 otherwise
    do n = 1, this%connections%nodes
      ! set diagonals to zero
      call this%connections%set_mask(this%connections%ia(n), 0)

      do ipos = this%connections%ia(n) + 1, this%connections%ia(n + 1) - 1
        newMask = 0
        if (this%connections%mask(ipos) > 0) then
          if (this%connections%mask(ipos) < this%internalStencilDepth + 1) then
            newMask = 1
          end if
        end if
        ! set mask on off-diag
        call this%connections%set_mask(ipos, newMask)
      end do
    end do

  end subroutine createConnectionMask

  !> @brief Create lookup tables for efficient access
  !< (this needs the connections object to be available)
  subroutine createLookupTable(this)
    use CsrUtilsModule, only: getCSRIndex
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    ! local
    integer(I4B) :: i, n1, n2, ipos

    do i = 1, this%nrOfBoundaryCells
      n1 = this%getInterfaceIndexByIndexModel(this%boundaryCells(i)%cell%index, &
                                              this%boundaryCells(i)%cell%model)
      n2 = this%getInterfaceIndexByIndexModel(this%connectedCells(i)%cell%index, &
                                              this%connectedCells(i)%cell%model)

      ipos = getCSRIndex(n1, n2, this%connections%ia, this%connections%ja)
      this%primConnections(i) = ipos
    end do

  end subroutine createLookupTable

  !> @brief Recursively mask connections, increasing the level as we go
  !<
  recursive subroutine maskInternalConnections(this, cell, nbrCell, level)
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    type(CellWithNbrsType), intent(inout) :: cell !< cell 1 in the connection to mask
    type(CellWithNbrsType), intent(inout) :: nbrCell !< cell 2 in the connection to mask
    integer(I4B), intent(in) :: level
    ! local
    integer(I4B) :: inbr, newLevel

    ! only set the mask for internal connections, leaving the
    ! others at 0
    if (associated(cell%cell%model, this%model) .and. &
        associated(nbrCell%cell%model, this%model)) then
      ! this will set a mask on both diagonal, and both cross terms
      call this%setMaskOnConnection(cell, nbrCell, level)
      call this%setMaskOnConnection(nbrCell, cell, level)
    end if

    ! recurse on nbrs-of-nbrs
    newLevel = level + 1
    do inbr = 1, nbrCell%nrOfNbrs
      call this%maskInternalConnections(nbrCell, &
                                        nbrCell%neighbors(inbr), &
                                        newLevel)
    end do

  end subroutine maskInternalConnections

  !> @brief Set a mask on the connection from a cell to its neighbor,
  !! (and not the transposed!) not overwriting the current level
  !< of a connection when it is smaller
  subroutine setMaskOnConnection(this, cell, nbrCell, level)
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    type(CellWithNbrsType), intent(inout) :: cell !< cell 1 in the connection
    type(CellWithNbrsType), intent(inout) :: nbrCell !< cell 2 in the connection
    integer(I4B), intent(in) :: level !< the level value to set the mask to
    ! local
    integer(I4B) :: ifaceIdx, ifaceIdxNbr
    integer(I4B) :: iposdiag, ipos
    integer(I4B) :: currentLevel

    ifaceIdx = this%getInterfaceIndex(cell%cell)
    ifaceIdxNbr = this%getInterfaceIndex(nbrCell%cell)

    ! diagonal
    iposdiag = this%connections%getjaindex(ifaceIdx, ifaceIdx)
    currentLevel = this%connections%mask(iposdiag)
    if (currentLevel == 0 .or. level < currentLevel) then
      call this%connections%set_mask(iposdiag, level)
    end if
    ! cross term
    ipos = this%connections%getjaindex(ifaceIdx, ifaceIdxNbr)
    currentLevel = this%connections%mask(ipos)
    if (currentLevel == 0 .or. level < currentLevel) then
      call this%connections%set_mask(ipos, level)
    end if

  end subroutine setMaskOnConnection

  !> @brief Get interface index from global cell
  !<
  function getInterfaceIndexByCell(this, cell) result(ifaceIdx)
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    type(GlobalCellType), intent(in) :: cell !< the global cell to get the interface index for
    integer(I4B) :: ifaceIdx !< the index in the interface model
    ! local
    integer(I4B) :: offset, regionIdx

    offset = this%getRegionalModelOffset(cell%model)
    regionIdx = offset + cell%index
    ifaceIdx = this%regionalToInterfaceIdxMap(regionIdx)
  end function getInterfaceIndexByCell

  !> @brief Get interface index from a model pointer and the local index
  !<
  function getInterfaceIndexByIndexModel(this, index, model) result(ifaceIdx)
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    integer(I4B) :: index !< the local cell index
    class(NumericalModelType), pointer :: model !< the cell's model
    integer(I4B) :: ifaceIdx !< the index in the interface model
    ! local
    integer(I4B) :: offset, regionIdx

    offset = this%getRegionalModelOffset(model)
    regionIdx = offset + index
    ifaceIdx = this%regionalToInterfaceIdxMap(regionIdx)
  end function getInterfaceIndexByIndexModel

  !> @brief Get the offset for a regional model
  !<
  function getRegionalModelOffset(this, model) result(offset)
    class(GridConnectionType), intent(inout) :: this !< this grid connection instance
    class(NumericalModelType), pointer :: model !< the model to get the offset for
    integer(I4B) :: offset !< the index offset in the regional domain
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

  !> @brief Allocate scalar data
  !<
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(GridConnectionType) :: this !< this grid connection instance

    call mem_allocate(this%nrOfBoundaryCells, 'NRBNDCELLS', this%memoryPath)
    call mem_allocate(this%indexCount, 'IDXCOUNT', this%memoryPath)
    call mem_allocate(this%nrOfCells, 'NRCELLS', this%memoryPath)

  end subroutine allocateScalars

  !> @brief Sets the discretization (DISU) after all
  !! preprocessing by this grid connection has been done,
  !< this comes after disu_cr
  subroutine getDiscretization(this, disu)
    use ConnectionsModule
    use SparseModule, only: sparsematrix
    class(GridConnectionType) :: this !< the grid connection
    class(GwfDisuType), pointer :: disu !< the target disu object
    ! local
    integer(I4B) :: icell, nrOfCells, idx
    type(NumericalModelType), pointer :: model
    real(DP) :: x, y, xglo, yglo

    ! the following is similar to dis_df
    nrOfCells = this%nrOfCells
    disu%nodes = nrOfCells
    disu%nodesuser = nrOfCells
    disu%nja = this%connections%nja

    call disu%allocate_arrays()
    ! these are otherwise allocated in dis%read_dimensions
    call disu%allocate_arrays_mem()

    ! fill data
    do icell = 1, nrOfCells
      idx = this%idxToGlobal(icell)%index
      model => this%idxToGlobal(icell)%model

      disu%top(icell) = model%dis%top(idx)
      disu%bot(icell) = model%dis%bot(idx)
      disu%area(icell) = model%dis%area(idx)
    end do

    ! grid connections follow from GridConnection:
    disu%con => this%connections
    disu%njas = disu%con%njas

    ! copy cell x,y
    do icell = 1, nrOfCells
      idx = this%idxToGlobal(icell)%index
      model => this%idxToGlobal(icell)%model
      call model%dis%get_cellxy(idx, x, y)

      ! we are merging grids with possibly (likely) different origins,
      ! transform:
      call model%dis%transform_xy(x, y, xglo, yglo)
      disu%cellxy(1, icell) = xglo
      disu%cellxy(2, icell) = yglo
    end do

    ! if vertices will be needed, it will look like this:
    !
    ! 1. determine total nr. of verts
    ! 2. allocate vertices list
    ! 3. create sparse
    ! 4. get vertex data per cell, add functions to base
    ! 5. add vertex (x,y) to list and connectivity to sparse
    ! 6. generate ia/ja from sparse

  end subroutine getDiscretization

  !> @brief Deallocate grid connection resources
  !<
  subroutine destroy(this)
    use MemoryManagerModule, only: mem_deallocate
    class(GridConnectionType) :: this !< this grid connection instance

    call mem_deallocate(this%nrOfBoundaryCells)
    call mem_deallocate(this%indexCount)
    call mem_deallocate(this%nrOfCells)

    ! arrays
    deallocate (this%idxToGlobal)
    deallocate (this%boundaryCells)
    deallocate (this%connectedCells)
    deallocate (this%primConnections)

    call mem_deallocate(this%idxToGlobalIdx)

  end subroutine destroy

  !> @brief Test if the connection between nodes within
  !< the same model is periodic
  function isPeriodic(this, n, m) result(periodic)
    class(GridConnectionType), intent(in) :: this !< this grid connection instance
    integer(I4B), intent(in) :: n !< first node of the connection
    integer(I4B), intent(in) :: m !< second node of the connection
    logical :: periodic !< true when periodic
    ! local
    integer(I4B) :: icell

    periodic = .false.
    do icell = 1, this%nrOfBoundaryCells
      if (.not. associated(this%boundaryCells(icell)%cell%model, &
                           this%connectedCells(icell)%cell%model)) cycle

      ! one way
      if (this%boundaryCells(icell)%cell%index == n) then
        if (this%connectedCells(icell)%cell%index == m) then
          periodic = .true.
          return
        end if
      end if
      ! or the other
      if (this%boundaryCells(icell)%cell%index == m) then
        if (this%connectedCells(icell)%cell%index == n) then
          periodic = .true.
          return
        end if
      end if

    end do

  end function

end module GridConnectionModule

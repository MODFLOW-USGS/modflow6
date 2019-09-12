! TODO: module description
module SpatialModelConnectionModule
  use KindModule, only: I4B
	use ModelConnectionModule
	use NumericalModelModule, only: NumericalModelType
  use NumericalExchangeModule, only: NumericalExchangeType, GetNumericalExchangeFromList
  use GridConnectionModule, only: GridConnectionType
  use ListModule, only: ListType
  
	implicit none
	private

	! Class to manage spatial connection of a model to one or more models of the same type.
	! Spatial connection here means that the model domains (spatial discretization) are adjacent
	! and connected.
	type, public, abstract, extends(ModelConnectionType) :: SpatialModelConnectionType
        
    ! aggregation, all exchanges which connect with our model
    type(ListType), pointer :: exchangeList => null()
    
    ! TODO_MJR: mem mgt of these guys:
    integer(I4B) :: nrOfConnections ! TODO_MJR: do we need this one?
    class(GridConnectionType), pointer :: gridConnection => null()    
    integer(I4B), dimension(:), pointer :: mapIdxToSln => null() ! maps local matrix (amat) to the global solution matrix
    
  contains
    procedure, pass(this) :: spatialConnection_ctor
    generic, public :: construct => spatialConnection_ctor
    procedure, pass(this) :: addExchange => addExchangeToSpatialConnection
    procedure, pass(this) :: mc_df => defineSpatialConnection 
    procedure, pass(this) :: mc_mc => mapCoefficients
    procedure, pass(this) :: mc_ac => addConnectionsToMatrix
    
    procedure, private, pass(this) :: addLinksToGridConnection
    procedure, private, pass(this) :: setGlobalNeighbours
    procedure, private, pass(this) :: getNrOfConnections
  end type SpatialModelConnectionType

contains ! module procedures
  
  subroutine spatialConnection_ctor(this, model, name)
    class(SpatialModelConnectionType), intent(inout) :: this
    class(NumericalModelType), intent(in), pointer :: model
    character(len=*), intent(in) :: name
    
    ! base props:
    this%name = name
    this%memoryOrigin = trim(this%name)
    this%owner => model
    
    this%nrOfConnections = 0
    
    allocate(this%exchangeList)
    allocate(this%gridConnection)    
    
  end subroutine spatialConnection_ctor
  
  subroutine addExchangeToSpatialConnection(this, exchange)
    class(SpatialModelConnectionType), intent(inout) :: this
	  class(NumericalExchangeType), pointer, intent(in) :: exchange
    ! local
    class(*), pointer :: exg
    
    exg => exchange
    call this%exchangeList%Add(exg)
    
  end subroutine addExchangeToSpatialConnection
  
  subroutine defineSpatialConnection(this)
    class(SpatialModelConnectionType), intent(inout) :: this    
    
    ! create the grid connection data structure
    this%nrOfConnections = this%getNrOfConnections()
    call this%gridConnection%construct(this%owner, this%nrOfConnections, this%name)
    call this%addLinksToGridConnection()
    
  end subroutine defineSpatialConnection
  
  ! create the mapping from local system matrix to global
  subroutine mapCoefficients(this, iasln, jasln)
    use SimModule, only: ustop
    use GridConnectionModule
    use ConnectionsModule, only: ConnectionsType
    class(SpatialModelConnectionType), intent(inout) :: this
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! local
    integer(I4B) :: mloc, nloc, mglob, nglob, j, csrIdx
    type(ConnectionsType), pointer :: conn => null()
    
    conn => this%gridConnection%connections
    
    allocate(this%mapIdxToSln(conn%nja))
    
    do mloc=1, conn%nodes
      do j=conn%ia(mloc), conn%ia(mloc+1)-1
        nloc = conn%ja(j) 
        
        mglob = this%gridConnection%idxToGlobal(mloc)%index + this%gridConnection%idxToGlobal(mloc)%model%moffset
        nglob = this%gridConnection%idxToGlobal(nloc)%index + this%gridConnection%idxToGlobal(nloc)%model%moffset 
        csrIdx = getCSRIndex(mglob, nglob, iasln, jasln)
        if (csrIdx == -1) then
          ! this should not be possible
          write(*,*) 'Error: cannot find cell connection in global system'
          call ustop()
        end if
        
        this%mapIdxToSln(j) = csrIdx       
      end do
    end do
    
  end subroutine mapCoefficients
  
  ! add connections to global matrix, does not fill in symmetric elements  
  subroutine addConnectionsToMatrix(this, sparse)
    use SparseModule, only:sparsematrix
    use GridConnectionModule
    class(SpatialModelConnectionType), intent(inout) :: this
    type(sparsematrix), intent(inout) :: sparse 
    
    ! local
    integer(I4B) :: i, nLinks
    integer(I4B) :: iglo, jglo ! global (solution matrix) indices
    
    type(GlobalCellType), dimension(:), pointer :: localCells => null()
    type(GlobalCellType), dimension(:), pointer :: connectedCells => null()
    
    localCells => this%gridConnection%localCells(:)%cell
    connectedCells => this%gridConnection%connectedCells(:)%cell
        
    nLinks = this%gridConnection%nrOfLinks     
    do i=1, nLinks
      iglo = localCells(i)%index + localCells(i)%model%moffset
      jglo = connectedCells(i)%index + connectedCells(i)%model%moffset
     
      ! add global numbers to sparse
      call sparse%addconnection(iglo, jglo, 1)
    end do
    
  end subroutine
  
  subroutine addLinksToGridConnection(this)
    class(SpatialModelConnectionType), intent(inout) :: this
    
    ! local
    integer(I4B) :: iex, iconn
    type(NumericalExchangeType), pointer :: numEx
    
    numEx => null()
    
    ! fill primary links, with local numbering: n => m or m <= n
    do iex=1, this%exchangeList%Count()
      numEx => GetNumericalExchangeFromList(this%exchangeList, iex)
      do iconn=1, numEx%nexg
        if (associated(numEx%m1, this%owner)) then          
          call this%gridConnection%addLink( numEx%nodem1(iconn),  &
                                            numEx%nodem2(iconn),  &
                                            numEx%cl1(iconn),     &
                                            numEx%cl2(iconn),     &
                                            numEx%hwva(iconn),    &
                                            numEx%ihc(iconn),     &
                                            numEx%m2) 
        else  
          ! then with nodes, lenghts, models reversed:
          call this%gridConnection%addLink( numEx%nodem2(iconn),  &
                                            numEx%nodem1(iconn),  &
                                            numEx%cl2(iconn),     &
                                            numEx%cl1(iconn),     &
                                            numEx%hwva(iconn),    &
                                            numEx%ihc(iconn),     &
                                            numEx%m1)       
        end if              
      end do
    end do
    
    ! here we scan for nbr-of-nbrs and create final data structures
    call this%gridConnection%extendConnection(0, 0)
    
  end subroutine addLinksToGridConnection
  
  ! subroutine uses the global exchanges from ModelConnection base,
  ! to set global neighbour information in GridConnection
  subroutine setGlobalNeighbours(this)
      class(SpatialModelConnectionType), intent(inout) :: this
      ! local   
      integer(I4B) :: i
      class(NumericalExchangeType), pointer :: numEx
      
      ! loop over all exchanges in solution with same conn. type
      do i=1, this%globalExchanges%Count()
          numEx => GetNumericalExchangeFromList(this%exchangelist, i)
          ! add connection between models
          call this%gridConnection%connectModels(numEx%m1, numEx%m2)
      end do     
      
   end subroutine
  
  ! count total nr. of connection between cells, from the exchanges
  function getNrOfConnections(this) result(nrConns)
    class(SpatialModelConnectionType), intent(inout) :: this
    integer(I4B) :: nrConns
    
    !local
    integer(I4B) :: iex
    type(NumericalExchangeType), pointer :: numEx
    
    nrConns = 0
    do iex = 1, this%exchangeList%Count()
      numEx => GetNumericalExchangeFromList(this%exchangeList, iex)
      nrConns = nrConns + numEx%nexg
    end do
    
  end function getNrOfConnections
  
  ! TODO_MJR: move this to generic place?
  ! return index for element i,j in CSR storage, and -1 when not there
  function getCSRIndex(i, j, ia, ja) result(csrIndex)
    integer(I4B) :: i, j                          ! the element to get the index for
    integer(I4B), dimension(:), intent(in) :: ia  ! csr ia
    integer(I4B), dimension(:), intent(in) :: ja  ! csr ja
    integer(I4B) :: csrIndex                 ! the resulting index
    ! local
    integer(I4B) :: idx
    
    csrIndex = -1
    do idx = ia(i), ia(i+1)-1
      if (ja(idx) == j) then
        csrIndex = idx
        return
      end if
    end do
    
  end function
  
end module SpatialModelConnectionModule

	
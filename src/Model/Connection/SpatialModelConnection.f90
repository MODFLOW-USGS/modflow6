! Module holding the definition of the SpatialModelConnectionType
module SpatialModelConnectionModule
  use KindModule, only: I4B
	use ModelConnectionModule
	use NumericalModelModule, only: NumericalModelType
  use NumericalExchangeModule, only: NumericalExchangeType, GetNumericalExchangeFromList
  use GridConnectionModule, only: GridConnectionType, GlobalCellType
  use ListModule, only: ListType
  
	implicit none
	private

	! Class to manage spatial connection of a model to one or more models of the same type.
	! Spatial connection here means that the model domains (spatial discretization) are adjacent
	! and connected via NumericalExchangeType object(s).
	type, public, abstract, extends(ModelConnectionType) :: SpatialModelConnectionType
        
    ! aggregation, all exchanges which directly connect with our model
    type(ListType), pointer :: exchangeList => null()
    
    integer(I4B) :: stencilDepth ! default = 1, xt3d = 2, ...
    
    ! TODO_MJR: mem mgt of these guys:
    integer(I4B) :: nrOfConnections
    class(GridConnectionType), pointer :: gridConnection => null()    
    integer(I4B), dimension(:), pointer :: mapIdxToSln => null() ! maps local matrix (amat) to the global solution matrix
    
  contains
    procedure, pass(this) :: spatialConnection_ctor
    generic, public :: construct => spatialConnection_ctor
    procedure, pass(this) :: addExchange => addExchangeToSpatialConnection
    procedure, pass(this) :: mc_df => defineSpatialConnection 
    procedure, pass(this) :: mc_mc => mapCoefficients
    procedure, pass(this) :: mc_ac => addConnectionsToMatrix
    ! private
    procedure, private, pass(this) :: setupGridConnection
    procedure, private, pass(this) :: setExchangeConnections
    procedure, private, pass(this) :: findModelNeighbors
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
    this%stencilDepth = 1
    
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
    call this%setupGridConnection()
    
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
    
    ! for readibility
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
  
  ! add connections to global matrix, c.f. exg_ac in NumericalExchange, 
  ! but now for all exchanges with this model and skipping over the 
  ! transposed elements
  subroutine addConnectionsToMatrix(this, sparse)
    use SparseModule, only:sparsematrix
    
    class(SpatialModelConnectionType), intent(inout) :: this
    type(sparsematrix), intent(inout) :: sparse 
    ! local
    integer(I4B) :: icell, iglo, jglo
    type(GlobalCellType), pointer :: ncell, mcell
        
    do icell = 1, this%gridConnection%nrOfBoundaryCells
      ncell => this%gridConnection%boundaryCells(icell)%cell
      mcell => this%gridConnection%connectedCells(icell)%cell
      iglo = ncell%index + ncell%model%moffset
      jglo = mcell%index + mcell%model%moffset
      call sparse%addconnection(iglo, jglo, 1)
    end do    
    
  end subroutine
  
  subroutine setupGridConnection(this)
    class(SpatialModelConnectionType), intent(inout) :: this
    ! local
    integer(I4B) :: localDepth, remoteDepth
    
    ! set boundary cells
    call this%setExchangeConnections()
    
    ! create topology of models
    call this%findModelNeighbors()
    
    ! now scan for nbr-of-nbrs and create final data structures
    ! we need (stencildepth-1) extra cells for the interior
    remoteDepth = this%stencilDepth
    localDepth = 2*this%stencilDepth - 1
    call this%gridConnection%extendConnection(localDepth, remoteDepth)
    
  end subroutine setupGridConnection
  
  ! set the primary links
  subroutine setExchangeConnections(this)
    class(SpatialModelConnectionType), intent(inout) :: this
    ! local
    integer(I4B) :: iex, iconn
    type(NumericalExchangeType), pointer :: numEx
    
    ! set boundary cells
    do iex=1, this%exchangeList%Count()
      numEx => GetNumericalExchangeFromList(this%exchangeList, iex)
      do iconn=1, numEx%nexg          
        call this%gridConnection%connectCell(numEx%nodem1(iconn), numEx%m1, numEx%nodem2(iconn), numEx%m2)
      end do
    end do
    
  end subroutine setExchangeConnections
  
  ! extends model topology to deal with cases where
  ! the stencil covers more than 2 models
  subroutine findModelNeighbors(this)
    class(SpatialModelConnectionType), intent(inout) :: this
    ! local   
    integer(I4B) :: i, depth
    class(NumericalExchangeType), pointer :: numEx
      
    ! loop over all exchanges in solution with same conn. type
    do i=1, this%globalExchanges%Count()
        numEx => GetNumericalExchangeFromList(this%globalExchanges, i)
        ! (possibly) add connection between models
        call this%gridConnection%addModelLink(numEx, this%stencilDepth)
    end do
      
   end subroutine findModelNeighbors
  
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

	
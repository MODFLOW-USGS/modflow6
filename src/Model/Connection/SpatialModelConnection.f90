! Module holding the definition of the SpatialModelConnectionType
module SpatialModelConnectionModule
  use KindModule, only: I4B, DP
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
    integer(I4B), pointer :: stencilDepth ! default = 1, xt3d = 2, ...
        
    ! the interface system doesn't live in a solution, so we need these
    integer(I4B), pointer                               :: neq => null()
    integer(I4B), pointer                               :: nja => null()
    integer(I4B), dimension(:), pointer, contiguous     :: ia => null()
    integer(I4B), dimension(:), pointer, contiguous     :: ja => null()
    real(DP), dimension(:), pointer, contiguous         :: amat => null()
    real(DP), dimension(:), pointer, contiguous         :: rhs => null()
    real(DP), dimension(:), pointer, contiguous         :: x => null()
    integer(I4B), dimension(:), pointer, contiguous     :: iactive => null()
        
    ! TODO_MJR: mem mgt of these guys:
    integer(I4B) :: nrOfConnections
    class(GridConnectionType), pointer :: gridConnection => null()    
    integer(I4B), dimension(:), pointer :: mapIdxToSln => null() ! maps local matrix (amat) to the global solution matrix
    
  contains
    procedure, pass(this) :: spatialConnection_ctor
    generic, public :: construct => spatialConnection_ctor
    procedure, pass(this) :: addExchange => addExchangeToSpatialConnection
    procedure, pass(this) :: mc_df => spatialcon_df 
    procedure, pass(this) :: mc_mc => spatialcon_mc
    procedure, pass(this) :: mc_ac => spatialcon_ac
    procedure, pass(this) :: spatialcon_df
    ! private
    procedure, private, pass(this) :: setupGridConnection
    procedure, private, pass(this) :: setExchangeConnections
    procedure, private, pass(this) :: findModelNeighbors
    procedure, private, pass(this) :: getNrOfConnections
    procedure, private, pass(this) :: allocateScalars
    procedure, private, pass(this) :: allocateArrays
    
  end type SpatialModelConnectionType

contains ! module procedures
  
  subroutine spatialConnection_ctor(this, model, name)
    class(SpatialModelConnectionType), intent(inout) :: this
    class(NumericalModelType), intent(in), pointer :: model
    character(len=*), intent(in) :: name
    integer(I4B) :: stencilDepth
    
    ! base props:
    this%name = name
    this%memoryOrigin = trim(this%name)
    this%owner => model
    this%iNewton = 0
    
    this%nrOfConnections = 0
    
    allocate(this%exchangeList)
    allocate(this%gridConnection)  
    
    call this%allocateScalars()
    
    this%stencilDepth = 1
        
  end subroutine spatialConnection_ctor
  
  subroutine addExchangeToSpatialConnection(this, exchange)
    class(SpatialModelConnectionType), intent(inout) :: this
	  class(NumericalExchangeType), pointer, intent(in) :: exchange
    ! local
    class(*), pointer :: exg
    
    exg => exchange
    call this%exchangeList%Add(exg)
    
  end subroutine addExchangeToSpatialConnection
  
  subroutine spatialcon_df(this)
    class(SpatialModelConnectionType), intent(inout) :: this    
    
    ! create the grid connection data structure
    this%nrOfConnections = this%getNrOfConnections()
    call this%gridConnection%construct(this%owner, this%nrOfConnections, this%stencilDepth, this%name)
    call this%setupGridConnection()
    
    this%neq = this%gridConnection%nrOfCells
    call this%allocateArrays()
    
  end subroutine spatialcon_df
  
  ! create the mapping from local system matrix to global
  subroutine spatialcon_mc(this, iasln, jasln)
    use SimModule, only: ustop
    use CsrUtilsModule, only: getCSRIndex
    use GridConnectionModule
    class(SpatialModelConnectionType), intent(inout) :: this
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! local
    integer(I4B) :: m, n, mglo, nglo, ipos, csrIdx
    
    allocate(this%mapIdxToSln(this%nja))
    
    do n = 1, this%neq
      do ipos = this%ia(n), this%ia(n+1)-1
        m = this%ja(ipos)        
        nglo = this%gridConnection%idxToGlobal(n)%index + this%gridConnection%idxToGlobal(n)%model%moffset
        mglo = this%gridConnection%idxToGlobal(m)%index + this%gridConnection%idxToGlobal(m)%model%moffset 
        csrIdx = getCSRIndex(nglo, mglo, iasln, jasln)
        if (csrIdx == -1) then
          ! this should not be possible
          write(*,*) 'Error: cannot find cell connection in global system'
          call ustop()
        end if
        
        this%mapIdxToSln(ipos) = csrIdx       
      end do
    end do
    
  end subroutine spatialcon_mc
  
  ! add connections to global matrix, c.f. exg_ac in NumericalExchange, 
  ! but now for all exchanges with this model and skipping over the 
  ! transposed elements
  subroutine spatialcon_ac(this, sparse)
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
    
  end subroutine spatialcon_ac
  
  subroutine setupGridConnection(this)
    class(SpatialModelConnectionType), intent(inout) :: this
    ! local
    
    ! set boundary cells
    call this%setExchangeConnections()
    
    ! create topology of models
    call this%findModelNeighbors()
    
    ! now scan for nbr-of-nbrs and create final data structures    
    call this%gridConnection%extendConnection()
    
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
        call this%gridConnection%addModelLink(numEx)
    end do
      
  end subroutine findModelNeighbors
  
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(SpatialModelConnectionType), intent(inout) :: this
  
    call mem_allocate(this%neq, 'NEQ', this%memoryOrigin)
    call mem_allocate(this%nja, 'NJA', this%memoryOrigin)
    call mem_allocate(this%stencilDepth, 'STENCILDEPTH', this%memoryOrigin)
    
  end subroutine allocateScalars
  
  subroutine allocateArrays(this)
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
    class(SpatialModelConnectionType), intent(inout) :: this
    ! local
    integer(I4B) :: i
    
    call mem_allocate(this%x, this%neq, 'X', this%memoryOrigin)
    call mem_allocate(this%rhs, this%neq, 'RHS', this%memoryOrigin)
    call mem_allocate(this%iactive, this%neq, 'IACTIVE', this%memoryOrigin)
    
    ! c.f. NumericalSolution
    do i = 1, this%neq
      this%x(i) = DZERO
      this%iactive(i) = 1 !default is active
    enddo
    
  end subroutine allocateArrays
  
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
  
end module SpatialModelConnectionModule

	
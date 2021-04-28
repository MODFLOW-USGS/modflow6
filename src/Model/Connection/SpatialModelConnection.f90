module SpatialModelConnectionModule
  use KindModule, only: I4B, DP, LGP
  use SparseModule, only:sparsematrix
  use NumericalModelModule, only: NumericalModelType
  use NumericalExchangeModule, only: NumericalExchangeType
  use DisConnExchangeModule, only: DisConnExchangeType, GetDisConnExchangeFromList
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use MemoryHelperModule, only: create_mem_path
  use GridConnectionModule, only: GridConnectionType, GlobalCellType
  use ListModule, only: ListType
  
  implicit none
  private
  public :: CastAsSpatialModelConnectionClass
  public :: AddSpatialModelConnectionToList
  public :: GetSpatialModelConnectionFromList

  !> Class to manage spatial connection of a model to one 
  !! or more models of the same type. Spatial connection here 
  !! means that the model domains (spatial discretization) are 
  !! adjacent and connected via DisConnExchangeType object(s).
  !! The connection itself is a Numerical Exchange as well, 
  !! and part of a Numerical Solution providing the amat and rhs
  !< values for the exchange.
  type, public, extends(NumericalExchangeType) :: SpatialModelConnectionType

    class(NumericalModelType), pointer  :: owner => null()            !< the model whose connection this is    
    integer(I4B), pointer               :: nrOfConnections => null()  !< total nr. of connected cells (primary)
    type(ListType), pointer             :: localExchanges => null()   !< aggregation, all exchanges which directly connect with our model
    type(ListType)                      :: globalExchanges            !< all exchanges in the same solution
    integer(I4B), pointer               :: intStencilDepth => null()  !< size of the computational stencil for the interior
                                                                      !! default = 1, xt3d = 2, tvd = ...
    integer(I4B), pointer               :: extStencilDepth => null()  !< size of the computational stencil at the interface
                                                                      !! default = 1, xt3d = 2, tvd = ...
    
    
    ! The following variables are equivalent to those in Numerical Solution:
    integer(I4B), pointer                               :: neq => null()      !< nr. of equations in matrix system
    integer(I4B), pointer                               :: nja => null()      !< nr. of nonzero matrix elements
    integer(I4B), dimension(:), pointer, contiguous     :: ia => null()       !< sparse indexing IA
    integer(I4B), dimension(:), pointer, contiguous     :: ja => null()       !< sparse indexing JA
    real(DP), dimension(:), pointer, contiguous         :: amat => null()     !< matrix coefficients
    real(DP), dimension(:), pointer, contiguous         :: rhs => null()      !< rhs of interface system
    real(DP), dimension(:), pointer, contiguous         :: x => null()        !< dependent variable of interface system
    integer(I4B), dimension(:), pointer, contiguous     :: active => null()   !< cell status (c.f. ibound) of interface system
        
    ! these are not in the memory manager
    class(GridConnectionType), pointer :: gridConnection => null() !< facility to build the interface grid connection structure
    integer(I4B), dimension(:), pointer :: mapIdxToSln => null()   !< mapping between interface matrix and the solution matrix
    
  contains
  
    ! public
    procedure, pass(this) :: spatialConnection_ctor
    generic :: construct => spatialConnection_ctor
    procedure, pass(this) :: addExchange => addExchangeToSpatialConnection

    ! partly overriding NumericalExchangeType:
    procedure, pass(this) :: exg_df => spatialcon_df
    procedure, pass(this) :: exg_ac => spatialcon_ac  
    procedure, pass(this) :: exg_mc => spatialcon_mc
    procedure, pass(this) :: exg_da => spatialcon_da

    procedure, pass(this) :: spatialcon_df 
    procedure, pass(this) :: spatialcon_ac  
    procedure, pass(this) :: spatialcon_mc
    procedure, pass(this) :: spatialcon_da

    ! protected
    procedure, pass(this) :: createCoefficientMatrix

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
    class(SpatialModelConnectionType) :: this
    class(NumericalModelType), intent(in), pointer :: model
    character(len=*), intent(in) :: name
    
    ! base props:
    this%name = name
    this%memoryPath = create_mem_path(this%name)
    this%owner => model
    
    allocate(this%localExchanges)
    allocate(this%gridConnection)
    call this%allocateScalars()

    this%intStencilDepth = 1
    this%extStencilDepth = 1
    this%nrOfConnections = 0
        
  end subroutine spatialConnection_ctor
  
  subroutine addExchangeToSpatialConnection(this, exchange)
    class(SpatialModelConnectionType) :: this
    class(DisConnExchangeType), pointer, intent(in) :: exchange
    ! local
    class(*), pointer :: exg

    exg => exchange
    call this%localExchanges%Add(exg)
    
  end subroutine addExchangeToSpatialConnection
  
  subroutine spatialcon_df(this)
    class(SpatialModelConnectionType) :: this
    
    ! create the grid connection data structure
    this%nrOfConnections = this%getNrOfConnections()
    call this%gridConnection%construct(this%owner, this%nrOfConnections, this%name)
    this%gridConnection%intStencilDepth = this%intStencilDepth
    this%gridConnection%extStencilDepth = this%extStencilDepth
    call this%setupGridConnection()
    
    this%neq = this%gridConnection%nrOfCells
    call this%allocateArrays()
    
  end subroutine spatialcon_df

  ! create the mapping from local system matrix to global
  subroutine spatialcon_mc(this, iasln, jasln)
    use SimModule, only: ustop
    use CsrUtilsModule, only: getCSRIndex
    use GridConnectionModule
    class(SpatialModelConnectionType) :: this
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
    class(SpatialModelConnectionType) :: this
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
  
  subroutine spatialcon_da(this)
    class(SpatialModelConnectionType) :: this
  
    call mem_deallocate(this%neq)
    call mem_deallocate(this%nja)
    call mem_deallocate(this%intStencilDepth)
    call mem_deallocate(this%extStencilDepth)
    call mem_deallocate(this%nrOfConnections)

    call mem_deallocate(this%ia)
    call mem_deallocate(this%ja)
    call mem_deallocate(this%amat)
    
    call mem_deallocate(this%x)
    call mem_deallocate(this%rhs)
    call mem_deallocate(this%active)
    
    call this%gridConnection%deallocate()
    deallocate(this%gridConnection)
    deallocate(this%mapIdxToSln)
  
  end subroutine spatialcon_da
  
  subroutine setupGridConnection(this)
    class(SpatialModelConnectionType) :: this
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
    class(SpatialModelConnectionType) :: this
    ! local
    integer(I4B) :: iex, iconn
    type(DisConnExchangeType), pointer :: connEx
    
    ! set boundary cells
    do iex=1, this%localExchanges%Count()
      connEx => GetDisConnExchangeFromList(this%localExchanges, iex)
      do iconn=1, connEx%nexg          
        call this%gridConnection%connectCell(connEx%nodem1(iconn), connEx%model1, connEx%nodem2(iconn), connEx%model2)
      end do
    end do
    
  end subroutine setExchangeConnections
  
  ! extends model topology to deal with cases where
  ! the stencil covers more than 2 models
  subroutine findModelNeighbors(this)
    class(SpatialModelConnectionType) :: this
    ! local   
    integer(I4B) :: i
    class(DisConnExchangeType), pointer :: connEx
      
    ! loop over all exchanges in solution with same conn. type
    do i=1, this%globalExchanges%Count()
        connEx => GetDisConnExchangeFromList(this%globalExchanges, i)
        ! (possibly) add connection between models
        call this%gridConnection%addModelLink(connEx)
    end do
      
  end subroutine findModelNeighbors
  
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(SpatialModelConnectionType) :: this
    
    call mem_allocate(this%neq, 'NEQ', this%memoryPath)
    call mem_allocate(this%nja, 'NJA', this%memoryPath)
    call mem_allocate(this%intStencilDepth, 'INTSTDEPTH', this%memoryPath)
    call mem_allocate(this%extStencilDepth, 'EXTSTDEPTH', this%memoryPath)
    call mem_allocate(this%nrOfConnections, 'NROFCONNS', this%memoryPath)
    
  end subroutine allocateScalars
  
  subroutine allocateArrays(this)
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
    class(SpatialModelConnectionType) :: this
    ! local
    integer(I4B) :: i
    
    call mem_allocate(this%x, this%neq, 'X', this%memoryPath)
    call mem_allocate(this%rhs, this%neq, 'RHS', this%memoryPath)
    call mem_allocate(this%active, this%neq, 'IACTIVE', this%memoryPath)
    
    ! c.f. NumericalSolution
    do i = 1, this%neq
      this%x(i) = DZERO
      this%active(i) = 1 !default is active
    enddo
    
  end subroutine allocateArrays
  
  ! count total nr. of connection between cells, from the exchanges
  function getNrOfConnections(this) result(nrConns)
    class(SpatialModelConnectionType) :: this
    integer(I4B) :: nrConns    
    !local
    integer(I4B) :: iex
    type(DisConnExchangeType), pointer :: connEx
    
    nrConns = 0
    do iex = 1, this%localExchanges%Count()
      connEx => GetDisConnExchangeFromList(this%localExchanges, iex)
      nrConns = nrConns + connEx%nexg
    end do
    
  end function getNrOfConnections
  
  !> @brief Create connection's ia/ja from sparse
  !<
  subroutine createCoefficientMatrix(this, sparse)
    use SimModule, only: ustop
    class(SpatialModelConnectionType) :: this
    type(sparsematrix), intent(inout) :: sparse
    ! local
    integer(I4B) :: ierror
        
    this%nja = sparse%nnz
    call mem_allocate(this%ia, this%neq + 1, 'IA', this%memoryPath)
    call mem_allocate(this%ja, this%nja, 'JA', this%memoryPath)
    call mem_allocate(this%amat, this%nja, 'AMAT', this%memoryPath)

    call sparse%sort()
    call sparse%filliaja(this%ia, this%ja, ierror)

    if (ierror /= 0) then
      write(*,*) 'Error: cannot fill ia/ja for model connection'
      call ustop()
    end if
    
  end subroutine createCoefficientMatrix

  function CastAsSpatialModelConnectionClass(obj) result (res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(SpatialModelConnectionType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (SpatialModelConnectionType)
      res => obj
    end select
    return
  end function CastAsSpatialModelConnectionClass

  subroutine AddSpatialModelConnectionToList(list, conn)
    implicit none
    ! -- dummy
    type(ListType),       intent(inout) :: list
    class(SpatialModelConnectionType), pointer, intent(in) :: conn
    ! -- local
    class(*), pointer :: obj
    !
    obj => conn
    call list%Add(obj)
    !
    return
  end subroutine AddSpatialModelConnectionToList

  function GetSpatialModelConnectionFromList(list, idx) result(res)
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: idx
    class(SpatialModelConnectionType), pointer :: res
    
    ! local
    class(*), pointer :: obj
    obj => list%GetItem(idx)
    res => CastAsSpatialModelConnectionClass(obj)
    !
    return
  end function GetSpatialModelConnectionFromList  
  
end module SpatialModelConnectionModule

	

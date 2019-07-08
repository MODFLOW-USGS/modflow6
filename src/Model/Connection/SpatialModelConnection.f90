! TODO: module description
module SpatialModelConnectionModule
  use KindModule, only: I4B
	use ModelConnectionModule
	use NumericalModelModule, only: NumericalModelType
  use NumericalExchangeModule, only: NumericalExchangeType, GetNumericalExchangeFromList
  use MeshConnectionModule, only: MeshConnectionType
  use ListModule, only: ListType
  
	implicit none
	private

	! Class to manage spatial connection of a model to one or more models of the same type.
	! Spatial connection here means that the model domains (spatial discretization) are adjacent
	! and connected.
	type, public, abstract, extends(ModelConnectionType) :: SpatialModelConnectionType
    
    integer(I4B) :: nrOfConnections ! TODO_MJR: do we need this one?
    type(ListType), pointer :: exchangeList => null()
    type(MeshConnectionType), pointer :: meshConnection => null()

  contains
    procedure, pass(this) :: spatialConnection_ctor
    generic, public :: construct => spatialConnection_ctor
    procedure, pass(this) :: addExchange => addExchangeToSpatialConnection
    procedure, pass(this) :: mc_df => defineSpatialConnection 
    procedure, pass(this) :: mc_ac => addConnectionsToMatrix
    procedure, private, pass(this) :: getNrOfConnections
  end type SpatialModelConnectionType

contains ! module procedures
  
  subroutine spatialConnection_ctor(this, model, name)
    class(SpatialModelConnectionType), intent(inout) :: this
    class(NumericalModelType), intent(in), pointer :: model
    character(len=*), intent(in) :: name
    
    this%name = name
    this%owner => model
    this%nrOfConnections = 0
    
    allocate(this%exchangeList)
    allocate(this%meshConnection)
    
    
  end subroutine spatialConnection_ctor
  
  subroutine addExchangeToSpatialConnection(this, exchange)
    class(SpatialModelConnectionType), intent(inout) :: this
	  class(NumericalExchangeType), pointer, intent(in) :: exchange
    ! local
    class(*), pointer :: exg
    
    ! assign exchange to unlim. polymorphic pointer 
    ! to able to add it to the list
    exg => exchange
    call this%exchangeList%Add(exg)
    
  end subroutine addExchangeToSpatialConnection
  
  subroutine defineSpatialConnection(this)
    class(SpatialModelConnectionType), intent(inout) :: this    
    
    ! create the mesh connection data structure
    this%nrOfConnections = this%getNrOfConnections()
    call this%meshConnection%construct(this%nrOfConnections, this%name)
    
  end subroutine defineSpatialConnection
  
  ! add connections to global matrix, does not fill in symmetric elements: i.e.,
  ! it needs to be called twice for two connected cells (elements m-n and n-m)
  subroutine addConnectionsToMatrix(this, sparse)
    use SparseModule, only:sparsematrix
    class(SpatialModelConnectionType), intent(inout) :: this
    type(sparsematrix), intent(inout) :: sparse 
    ! local
    integer(I4B) :: ic
    integer(I4B) :: iglo, jglo        ! global row (i) and column (j) numbers
    integer(I4B) :: offset, nbrOffset ! model offset in global solution matrix, for owner and neighbour
    integer(I4B) :: iex, iconn
    type(NumericalExchangeType), pointer :: numEx
    
    numEx => null()
    
    ! fill primary links, with global numbering: n => m or m <= n, but not both
    do iex=1, this%exchangeList%Count()
      numEx => GetNumericalExchangeFromList(this%exchangeList, iex)
      do iconn=1, numEx%nexg
        if (associated(numEx%m1, this%owner)) then
          iglo = numEx%nodem1(iconn) + this%owner%moffset
          jglo = numEx%nodem2(iconn) + numEx%m2%moffset
        else
          iglo = numEx%nodem2(iconn) + this%owner%moffset
          jglo = numEx%nodem1(iconn) + numEx%m1%moffset
        end if
        
        ! local admin
        call this%meshConnection%addConnection(iconn, iglo, jglo)
        
        ! add to sparse
        call sparse%addconnection(iglo, jglo, 1)
        
      end do
    end do
    
    offset = this%owner%moffset
    do ic=1, this%meshConnection%nrOfConnections     
      write(*,*) ic, ': ', this%meshConnection%localNodes(ic), this%meshConnection%connectedNodes(ic)
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
  
end module SpatialModelConnectionModule

	
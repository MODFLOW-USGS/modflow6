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
    procedure,pass(this)  :: addExchange => addExchangeToSpatialConnection
    procedure, pass(this) :: mc_df => defineSpatialConnection 
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
    ! local
    integer(I4B) :: iex
    type(NumericalExchangeType), pointer :: numEx
    
    numEx => null()
    
    ! create the mesh connection data structure
    this%nrOfConnections = this%getNrOfConnections()
    call this%meshConnection%construct(this%nrOfConnections, this%name)
    
    ! fill primary links: n <=> m
    do iex = 1, this%exchangeList%Count()
      numEx => GetNumericalExchangeFromList(this%exchangeList, iex)
      
    end do
    
  end subroutine defineSpatialConnection
   
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

	
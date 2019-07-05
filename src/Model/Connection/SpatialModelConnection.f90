! TODO: module description
module SpatialModelConnectionModule
	use ModelConnectionModule
	use NumericalModelModule, only: NumericalModelType
  use NumericalExchangeModule, only: NumericalExchangeType
  
	implicit none
	private

	! Class to manage spatial connection of a model to one or more models of the same type.
	! Spatial connection here means that the model domains (spatial discretization) are adjacent
	! and connected.
	type, public, abstract, extends(ModelConnectionType) :: SpatialModelConnectionType
  
    

  contains
    procedure, pass(this) :: spatialConnection_ctor
    generic, public :: construct => spatialConnection_ctor
    procedure,pass(this)  :: addExchange => addExchangeToSpatialConnection
    procedure, pass(this) :: mc_df => defineSpatialConnection 
  end type SpatialModelConnectionType

contains ! module procedures
  
  subroutine spatialConnection_ctor(this, model, name)
    class(SpatialModelConnectionType), intent(inout) :: this
    class(NumericalModelType), intent(in), pointer :: model
    character(len=*), intent(in) :: name
    
    this%name = name
    this%owner => model
    
  end subroutine spatialConnection_ctor
  
  subroutine addExchangeToSpatialConnection(this, exchange)
    class(SpatialModelConnectionType), intent(inout) :: this
	  class(NumericalExchangeType), pointer, intent(in) :: exchange
  
    ! TODO: create some logic here...
    
  end subroutine addExchangeToSpatialConnection
  
  subroutine defineSpatialConnection(this)
    class(SpatialModelConnectionType), intent(inout) :: this
  end subroutine defineSpatialConnection
  
end module SpatialModelConnectionModule

	
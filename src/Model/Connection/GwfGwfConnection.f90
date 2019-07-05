! TODO: module description
module GwfGwfConnectionModule
  use SpatialModelConnectionModule
  use GwfModule, only: GwfModelType
  
  implicit none
  private

  ! Connecting two groundwaterflow models in space
  type, public, extends(SpatialModelConnectionType) :: GwfGwfConnectionType
    
    type(GwfModelType), pointer :: gwfModel => null()
    
  contains 
    procedure, pass(this) :: gwfGwfConnection_ctor
	  generic, public :: construct => gwfGwfConnection_ctor
  end type GwfGwfConnectionType

contains

  subroutine gwfGwfConnection_ctor(this, model)
    use NumericalModelModule, only: NumericalModelType
    class(GwfGwfConnectionType), intent(inout)  :: this
    class(NumericalModelType), pointer          :: model ! note: this must be a GwfModelType
    
    ! first call base constructor
    call this%construct(model, 'GWF_GWF_CONN_'//model%name)
    
    ! construct GWF part here
    select type(model)
      type is(GwfModelType)
      this%gwfModel => model
    end select
      
  end subroutine gwfGwfConnection_ctor
    
end module GwfGwfConnectionModule

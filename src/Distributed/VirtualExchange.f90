module VirtualExchangeModule
  use VirtualBaseModule
  use ListModule
  implicit none
  private

  type, public :: VirtualExchangeType
    type(ListType) :: remote_data
    ! scalars
    type(VirtualIntType), pointer :: model1_id
    type(VirtualIntType), pointer :: model2_id
    type(VirtualIntType), pointer :: nexg => null()
    type(VirtualIntType), pointer :: naux => null()  
    type(VirtualIntType), pointer :: ianglex => null()
    ! arrays
    type(VirtualInt1dType), pointer :: nodem1 => null()
    type(VirtualInt1dType), pointer :: nodem2 => null()
    type(VirtualInt1dType), pointer :: ihc => null()
    type(VirtualDbl1dType), pointer :: cl1 => null()
    type(VirtualDbl1dType), pointer :: cl2 => null()
    type(VirtualDbl1dType), pointer :: hwva => null()
    type(VirtualDbl2dType), pointer :: auxvar => null()
  end type VirtualExchangeType

end module VirtualExchangeModule
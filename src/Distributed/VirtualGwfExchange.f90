module VirtualGwfExchangeModule
  use KindModule, only: I4B
  use VirtualDataContainerModule, only: VDC_GWFEXG_TYPE
  use VirtualExchangeModule
  use VirtualDataListsModule, only: virtual_exchange_list
  implicit none
  private

  public :: add_virtual_gwf_exchange

  type, public, extends(VirtualExchangeType) :: VirtualGwfExchangeType
  contains
    procedure :: create => vfx_create
    procedure :: destroy => vfx_destroy
  end type VirtualGwfExchangeType

contains

!> @brief Add a virtual GWF-GWF exchange to the simulation
!<
subroutine add_virtual_gwf_exchange(name, exchange_id, model1_id, model2_id)
  integer(I4B) :: exchange_id
  character(len=*) :: name
  integer(I4B) :: model1_id
  integer(I4B) :: model2_id
  ! local
  class(VirtualGwfExchangeType), pointer :: v_exg
  class(*), pointer :: obj_ptr

  allocate (v_exg)
  call v_exg%create(name, exchange_id, model1_id, model2_id)

  obj_ptr => v_exg
  call virtual_exchange_list%Add(obj_ptr)

end subroutine add_virtual_gwf_exchange

!> @brief Create a virtual GWF-GWF exchange
!<
subroutine vfx_create(this, name, exg_id, m1_id, m2_id)
  class(VirtualGwfExchangeType) :: this
  character(len=*) :: name
  integer(I4B) :: exg_id
  integer(I4B) :: m1_id
  integer(I4B) :: m2_id

  call this%VirtualExchangeType%create(name, exg_id, m1_id, m2_id)
  this%container_type = VDC_GWFEXG_TYPE
  
end subroutine vfx_create

subroutine vfx_destroy(this)
  class(VirtualGwfExchangeType) :: this

  call this%VirtualExchangeType%destroy()

end subroutine vfx_destroy

end module VirtualGwfExchangeModule
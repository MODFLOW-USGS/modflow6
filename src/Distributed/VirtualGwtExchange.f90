module VirtualGwtExchangeModule
  use KindModule, only: I4B
  use VirtualDataListsModule, only: virtual_exchange_list
  use VirtualExchangeModule
  implicit none
  private

  public :: add_virtual_gwt_exchange

  type, public, extends(VirtualExchangeType) :: VirtualGwtExchangeType
  contains
    procedure :: create => vtx_create
    procedure :: destroy => vtx_destroy
  end type VirtualGwtExchangeType

contains

!> @brief Add a virtual GWT-GWT exchange to the simulation
!<
subroutine add_virtual_gwt_exchange(name, exchange_id, model1_id, model2_id)
  character(len=*) :: name
  integer(I4B) :: exchange_id
  integer(I4B) :: model1_id
  integer(I4B) :: model2_id
  ! local
  class(VirtualGwtExchangeType), pointer :: v_exg
  class(*), pointer :: obj_ptr

  allocate (v_exg)
  call v_exg%create(name, exchange_id, model1_id, model2_id)
  
  obj_ptr => v_exg
  call virtual_exchange_list%Add(obj_ptr)

end subroutine add_virtual_gwt_exchange

!> @brief Create a virtual GWT-GWT exchange
!<
subroutine vtx_create(this, name, exg_id, m1_id, m2_id)
  class(VirtualGwtExchangeType) :: this
  character(len=*) :: name
  integer(I4B) :: exg_id
  integer(I4B) :: m1_id
  integer(I4B) :: m2_id

  call this%VirtualExchangeType%create(name, exg_id, m1_id, m2_id)

end subroutine vtx_create

subroutine vtx_destroy(this)
  class(VirtualGwtExchangeType) :: this

  call this%VirtualExchangeType%destroy()

end subroutine vtx_destroy

end module VirtualGwtExchangeModule
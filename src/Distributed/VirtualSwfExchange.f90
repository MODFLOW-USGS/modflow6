module VirtualSwfExchangeModule
  use KindModule, only: I4B
  use SimStagesModule
  use VirtualBaseModule
  use VirtualDataListsModule, only: virtual_exchange_list
  use VirtualExchangeModule
  implicit none
  private

  public :: register_virtual_swfswf

  type, public, extends(VirtualExchangeType) :: VirtualSwfExchangeType
    type(VirtualDbl1dType), pointer :: gwfsimvals => null()
  contains
    procedure :: destroy => vtx_destroy
    procedure :: prepare_stage => vtx_prepare_stage
    ! private
    procedure, private :: init_virtual_data
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
  end type VirtualSwfExchangeType

contains

  !> @brief Add a virtual SWF-SWF exchange to the simulation
  !<
  subroutine register_virtual_swfswf(name, exchange_id, &
                                     model1_id, model2_id)
    character(len=*) :: name
    integer(I4B) :: exchange_id
    integer(I4B) :: model1_id
    integer(I4B) :: model2_id
    ! noop
  end subroutine register_virtual_swfswf

  subroutine init_virtual_data(this)
    class(VirtualSwfExchangeType) :: this
    ! noop
  end subroutine init_virtual_data

  subroutine vtx_prepare_stage(this, stage)
    class(VirtualSwfExchangeType) :: this
    integer(I4B) :: stage
    ! noop
  end subroutine vtx_prepare_stage

  subroutine vtx_destroy(this)
    class(VirtualSwfExchangeType) :: this
    ! noop
  end subroutine vtx_destroy

  subroutine allocate_data(this)
    class(VirtualSwfExchangeType) :: this
    ! noop
  end subroutine allocate_data

  subroutine deallocate_data(this)
    class(VirtualSwfExchangeType) :: this
    ! noop
  end subroutine deallocate_data

end module VirtualSwfExchangeModule

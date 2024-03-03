module VirtualPrtExchangeModule
  use KindModule, only: I4B
  use SimStagesModule
  use VirtualBaseModule
  use VirtualDataListsModule, only: virtual_exchange_list
  use VirtualExchangeModule
  implicit none
  private

  public :: register_virtual_prtprt

  type, public, extends(VirtualExchangeType) :: VirtualPrtExchangeType
    type(VirtualDbl1dType), pointer :: gwfsimvals => null()
  contains
    procedure :: destroy => vtx_destroy
    procedure :: prepare_stage => vtx_prepare_stage
    ! private
    procedure, private :: init_virtual_data
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
  end type VirtualPrtExchangeType

contains

  !> @brief Add a virtual PRT-PRT exchange to the simulation
  !<
  subroutine register_virtual_prtprt(id, name, model1_id, model2_id)
    integer(I4B), intent(in) :: id
    character(len=*), intent(in) :: name
    integer(I4B), intent(in) :: model1_id
    integer(I4B), intent(in) :: model2_id
    ! noop
  end subroutine register_virtual_prtprt

  subroutine init_virtual_data(this)
    class(VirtualPrtExchangeType) :: this
    ! noop
  end subroutine init_virtual_data

  subroutine vtx_prepare_stage(this, stage)
    class(VirtualPrtExchangeType) :: this
    integer(I4B) :: stage
    ! noop
  end subroutine vtx_prepare_stage

  subroutine vtx_destroy(this)
    class(VirtualPrtExchangeType) :: this
    ! noop
  end subroutine vtx_destroy

  subroutine allocate_data(this)
    class(VirtualPrtExchangeType) :: this
    ! noop
  end subroutine allocate_data

  subroutine deallocate_data(this)
    class(VirtualPrtExchangeType) :: this
    ! noop
  end subroutine deallocate_data

end module VirtualPrtExchangeModule

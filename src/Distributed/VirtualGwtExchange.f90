module VirtualGwtExchangeModule
  use KindModule, only: I4B
  use SimStagesModule
  use VirtualBaseModule
  use VirtualDataListsModule, only: virtual_exchange_list
  use VirtualDataContainerModule, only: VDC_GWTEXG_TYPE
  use VirtualExchangeModule
  implicit none
  private

  public :: register_virtual_gwtgwt

  type, public, extends(VirtualExchangeType) :: VirtualGwtExchangeType
    type(VirtualDbl1dType), pointer :: gwfsimvals => null()
  contains
    procedure :: destroy => vtx_destroy
    procedure :: prepare_stage => vtx_prepare_stage
    ! private
    procedure, private :: init_virtual_data
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
  end type VirtualGwtExchangeType

contains

!> @brief Add a virtual GWT-GWT exchange to the simulation
!<
  subroutine register_virtual_gwtgwt(name, exchange_id, model1_id, model2_id)
    character(len=*) :: name
    integer(I4B) :: exchange_id
    integer(I4B) :: model1_id
    integer(I4B) :: model2_id
    ! local
    class(VirtualGwtExchangeType), pointer :: v_exg
    class(*), pointer :: obj_ptr

    allocate (v_exg)
    call v_exg%VirtualExchangeType%create(name, exchange_id, model1_id, model2_id)
    v_exg%container_type = VDC_GWTEXG_TYPE

    call v_exg%allocate_data()
    call v_exg%init_virtual_data()

    obj_ptr => v_exg
    call virtual_exchange_list%Add(obj_ptr)

  end subroutine register_virtual_gwtgwt

  subroutine init_virtual_data(this)
    class(VirtualGwtExchangeType) :: this

    call this%set(this%gwfsimvals%base(), 'GWFSIMVALS', '', MAP_ALL_TYPE)

  end subroutine init_virtual_data

  subroutine vtx_prepare_stage(this, stage)
    class(VirtualGwtExchangeType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: nexg

    ! prepare base exchange data items
    call this%VirtualExchangeType%prepare_stage(stage)

    if (stage == STG_BFR_CON_AR) then
      nexg = this%nexg%get()
      call this%map(this%gwfsimvals%base(), nexg, (/STG_BFR_EXG_AD/))
    end if

  end subroutine vtx_prepare_stage

  subroutine vtx_destroy(this)
    class(VirtualGwtExchangeType) :: this

    call this%VirtualExchangeType%destroy()
    call this%deallocate_data()

  end subroutine vtx_destroy

  subroutine allocate_data(this)
    class(VirtualGwtExchangeType) :: this

    allocate (this%gwfsimvals)

  end subroutine allocate_data

  subroutine deallocate_data(this)
    class(VirtualGwtExchangeType) :: this

    deallocate (this%gwfsimvals)

  end subroutine deallocate_data

end module VirtualGwtExchangeModule

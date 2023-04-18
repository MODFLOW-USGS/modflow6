module VirtualGweExchangeModule
  use KindModule, only: I4B
  use SimStagesModule
  use VirtualBaseModule
  use VirtualDataListsModule, only: virtual_exchange_list
  use VirtualDataContainerModule, only: VDC_GWEEXG_TYPE
  use VirtualExchangeModule
  implicit none
  private

  public :: add_virtual_gwe_exchange

  type, public, extends(VirtualExchangeType) :: VirtualGweExchangeType
    type(VirtualDbl1dType), pointer :: gwfsimvals => null()
  contains
    procedure :: create => vtx_create
    procedure :: destroy => vtx_destroy
    procedure :: prepare_stage => vtx_prepare_stage
    ! private
    procedure, private :: init_virtual_data
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
  end type VirtualGweExchangeType

contains

!> @brief Add a virtual GWE-GWE exchange to the simulation
!<
  subroutine add_virtual_gwe_exchange(name, exchange_id, model1_id, model2_id)
    character(len=*) :: name
    integer(I4B) :: exchange_id
    integer(I4B) :: model1_id
    integer(I4B) :: model2_id
    ! local
    class(VirtualGweExchangeType), pointer :: v_exg
    class(*), pointer :: obj_ptr

    allocate (v_exg)
    call v_exg%create(name, exchange_id, model1_id, model2_id)

    obj_ptr => v_exg
    call virtual_exchange_list%Add(obj_ptr)

  end subroutine add_virtual_gwe_exchange

!> @brief Create a virtual GWE-GWE exchange
!<
  subroutine vtx_create(this, name, exg_id, m1_id, m2_id)
    class(VirtualGweExchangeType) :: this
    character(len=*) :: name
    integer(I4B) :: exg_id
    integer(I4B) :: m1_id
    integer(I4B) :: m2_id

    ! create base
    call this%VirtualExchangeType%create(name, exg_id, m1_id, m2_id)
    this%container_type = VDC_GWEEXG_TYPE

    call this%allocate_data()
    call this%init_virtual_data()

  end subroutine vtx_create

  subroutine init_virtual_data(this)
    class(VirtualGweExchangeType) :: this

    call this%set(this%gwfsimvals%base(), 'GWFSIMVALS', '', MAP_ALL_TYPE)

  end subroutine init_virtual_data

  subroutine vtx_prepare_stage(this, stage)
    class(VirtualGweExchangeType) :: this
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
    class(VirtualGweExchangeType) :: this

    call this%VirtualExchangeType%destroy()
    call this%deallocate_data()

  end subroutine vtx_destroy

  subroutine allocate_data(this)
    class(VirtualGweExchangeType) :: this

    allocate (this%gwfsimvals)

  end subroutine allocate_data

  subroutine deallocate_data(this)
    class(VirtualGweExchangeType) :: this

    deallocate (this%gwfsimvals)

  end subroutine deallocate_data

end module VirtualGweExchangeModule

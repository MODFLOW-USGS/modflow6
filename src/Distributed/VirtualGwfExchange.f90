module VirtualGwfExchangeModule
  use KindModule, only: I4B, LGP
  use STLVecIntModule
  use SimStagesModule
  use VirtualBaseModule
  use VirtualDataContainerModule, only: VDC_GWFEXG_TYPE
  use VirtualExchangeModule
  use VirtualDataListsModule, only: virtual_exchange_list
  implicit none
  private

  public :: add_virtual_gwf_exchange

  !> TODO_MJR: prefix variables with mvr_
  !<
  type, public, extends(VirtualExchangeType) :: VirtualGwfExchangeType
    type(VirtualIntType), pointer :: inmvr => null()
    type(VirtualIntType), pointer :: maxmvr => null()
    type(VirtualDbl1dType), pointer :: qpactual_m1 => null()
    type(VirtualDbl1dType), pointer :: qpactual_m2 => null()
    type(VirtualInt1dType), pointer :: id_mapped_m1 => null()
    type(VirtualInt1dType), pointer :: id_mapped_m2 => null()
  contains
    procedure :: create => vfx_create
    procedure :: prepare_stage => vfx_prepare_stage
    procedure :: destroy => vfx_destroy
    procedure :: get_send_items => vfx_get_send_items
    procedure :: get_recv_items => vfx_get_recv_items
    ! private
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
    procedure, private :: init_virtual_data
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

    call this%allocate_data()
    call this%init_virtual_data()

  end subroutine vfx_create

  subroutine init_virtual_data(this)
    class(VirtualGwfExchangeType) :: this
    ! local
    logical(LGP) :: is_nodem1_local
    logical(LGP) :: is_nodem2_local

    is_nodem1_local = this%v_model1%is_local
    is_nodem2_local = this%v_model2%is_local
    call this%set(this%inmvr%base(), 'INMVR', '', MAP_ALL_TYPE)
    call this%set(this%maxmvr%base(), 'MAXMVR', 'MVR', MAP_ALL_TYPE)
    ! these follow locality of nodem1,2
    call this%set(this%qpactual_m1%base(), 'QPACTUAL_M1', 'MVR', &
                  MAP_ALL_TYPE, is_nodem1_local)
    call this%set(this%qpactual_m2%base(), 'QPACTUAL_M2', 'MVR', &
                  MAP_ALL_TYPE, is_nodem2_local)
    call this%set(this%id_mapped_m1%base(), 'ID_MAPPED_M1', 'MVR', &
                  MAP_ALL_TYPE, is_nodem1_local)
    call this%set(this%id_mapped_m2%base(), 'ID_MAPPED_M2', 'MVR', &
                  MAP_ALL_TYPE, is_nodem2_local)

  end subroutine init_virtual_data

  subroutine vfx_prepare_stage(this, stage)
    class(VirtualGwfExchangeType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: maxmvr

    ! prepare base exchange data items
    call this%VirtualExchangeType%prepare_stage(stage)

    if (stage == STG_AFT_EXG_DF) then

      call this%map(this%inmvr%base(), (/STG_AFT_EXG_DF/))

    else if (stage == STG_BFR_CON_AR) then

      ! only when MVR is active
      if (this%inmvr%get() > 0) then
        call this%map(this%maxmvr%base(), (/STG_BFR_CON_AR/))
      end if

    else if (stage == STG_AFT_CON_AR) then

      ! only when MVR is active
      if (this%inmvr%get() > 0) then
        maxmvr = this%maxmvr%get()
        call this%map(this%qpactual_m1%base(), maxmvr, (/STG_BFR_EXG_FC/))
        call this%map(this%qpactual_m2%base(), maxmvr, (/STG_BFR_EXG_FC/))
        call this%map(this%id_mapped_m1%base(), maxmvr, (/STG_AFT_CON_RP/))
        call this%map(this%id_mapped_m2%base(), maxmvr, (/STG_AFT_CON_RP/))
      end if

    end if

  end subroutine vfx_prepare_stage

  subroutine vfx_get_recv_items(this, stage, rank, virtual_items)
    class(VirtualGwfExchangeType) :: this
    integer(I4B) :: stage
    integer(I4B) :: rank
    type(STLVecInt) :: virtual_items
    ! local
    integer(I4B) :: qpactual_m1_idx, qpactual_m2_idx
    integer(I4B) :: id_mapped_m1_idx, id_mapped_m2_idx
    class(*), pointer :: vdi

    ! get base items to receive
    call this%VirtualExchangeType%get_recv_items(stage, rank, &
                                                 virtual_items)

    ! add more MVR items that follow nodem1/nodem2 pattern
    vdi => this%qpactual_m1
    qpactual_m1_idx = this%virtual_data_list%GetIndex(vdi)
    vdi => this%qpactual_m2
    qpactual_m2_idx = this%virtual_data_list%GetIndex(vdi)
    vdi => this%id_mapped_m1
    id_mapped_m1_idx = this%virtual_data_list%GetIndex(vdi)
    vdi => this%id_mapped_m2
    id_mapped_m2_idx = this%virtual_data_list%GetIndex(vdi)

    if (this%v_model1%is_local .and. &
        this%v_model2%orig_rank == rank) then
      ! this is our dual exchange on the other rank,
      ! only receive qpactual_m2
      if (this%qpactual_m2%check_stage(stage)) then
        call virtual_items%push_back(qpactual_m2_idx)
      end if
      if (this%id_mapped_m2%check_stage(stage)) then
        call virtual_items%push_back(id_mapped_m2_idx)
      end if
    else if (this%v_model2%is_local .and. &
             this%v_model1%orig_rank == rank) then
      ! the reverse case...
      if (this%qpactual_m1%check_stage(stage)) then
        call virtual_items%push_back(qpactual_m1_idx)
      end if
      if (this%id_mapped_m1%check_stage(stage)) then
        call virtual_items%push_back(id_mapped_m1_idx)
      end if
    end if

  end subroutine vfx_get_recv_items

  subroutine vfx_get_send_items(this, stage, rank, virtual_items)
    class(VirtualGwfExchangeType) :: this
    integer(I4B) :: stage
    integer(I4B) :: rank
    type(STLVecInt) :: virtual_items
    ! local
    integer(I4B) :: qpactual_m1_idx, qpactual_m2_idx
    integer(I4B) :: id_mapped_m1_idx, id_mapped_m2_idx
    class(*), pointer :: vdi

    ! get base items to send
    call this%VirtualExchangeType%get_send_items(stage, rank, &
                                                 virtual_items)

    ! add more MVR items that follow nodem1/nodem2 pattern
    vdi => this%qpactual_m1
    qpactual_m1_idx = this%virtual_data_list%GetIndex(vdi)
    vdi => this%qpactual_m2
    qpactual_m2_idx = this%virtual_data_list%GetIndex(vdi)
    vdi => this%id_mapped_m1
    id_mapped_m1_idx = this%virtual_data_list%GetIndex(vdi)
    vdi => this%id_mapped_m2
    id_mapped_m2_idx = this%virtual_data_list%GetIndex(vdi)

    if (this%v_model1%is_local .and. &
        this%v_model2%orig_rank == rank) then
      ! this is our dual exchange on the other rank,
      ! only add qpactual_m1
      if (this%qpactual_m1%check_stage(stage)) then
        call virtual_items%push_back(qpactual_m1_idx)
      end if
      if (this%id_mapped_m1%check_stage(stage)) then
        call virtual_items%push_back(id_mapped_m1_idx)
      end if
    else if (this%v_model2%is_local .and. &
             this%v_model1%orig_rank == rank) then
      ! the reverse case...
      if (this%qpactual_m2%check_stage(stage)) then
        call virtual_items%push_back(qpactual_m2_idx)
      end if
      if (this%id_mapped_m2%check_stage(stage)) then
        call virtual_items%push_back(id_mapped_m2_idx)
      end if
    end if

  end subroutine vfx_get_send_items

  subroutine allocate_data(this)
    class(VirtualGwfExchangeType) :: this

    allocate (this%inmvr)
    allocate (this%maxmvr)
    allocate (this%qpactual_m1)
    allocate (this%qpactual_m2)
    allocate (this%id_mapped_m1)
    allocate (this%id_mapped_m2)

  end subroutine allocate_data

  subroutine deallocate_data(this)
    class(VirtualGwfExchangeType) :: this

    deallocate (this%inmvr)
    deallocate (this%maxmvr)
    deallocate (this%qpactual_m1)
    deallocate (this%qpactual_m2)
    deallocate (this%id_mapped_m1)
    deallocate (this%id_mapped_m2)

  end subroutine deallocate_data

  subroutine vfx_destroy(this)
    class(VirtualGwfExchangeType) :: this

    call this%VirtualExchangeType%destroy()
    call this%deallocate_data()

  end subroutine vfx_destroy

end module VirtualGwfExchangeModule

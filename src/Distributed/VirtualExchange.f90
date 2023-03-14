module VirtualExchangeModule
  use VirtualBaseModule
  use VirtualDataContainerModule
  use VirtualModelModule, only: VirtualModelType, get_virtual_model
  use KindModule, only: I4B, LGP
  use ListModule, only: ListType
  use STLVecIntModule
  use ConstantsModule, only: LENEXCHANGENAME
  use SimStagesModule
  implicit none
  private

  public :: get_virtual_exchange
  public :: get_virtual_exchange_from_list
  private :: cast_as_virtual_exchange

  !> The Virtual Exchange is based on two Virtual Models
  !! and is therefore not always strictly local, or remote.
  !! We have to consider three different cases:
  !!
  !! 1) both virtual models are local
  !!
  !!  RECV: In this case this virtual data container will have 
  !!        no data items to receive from other processes. 
  !!  SEND: Whenever it is called to send its virtual data items
  !!        to other processes, it simply sends everything.
  !!
  !! 2) one model is local, one model is remote
  !!
  !!  Consequently, there is another exchange which
  !!  has the reverse, we call this our _dual_ exchange.
  !!
  !!  RECV: The sender is our dual exchange, and
  !!        we have all data except its list of
  !!        reduced model node numbers, either
  !!        this%nodem1 or this%nodem2. We receive
  !!        the missing array. Receiving from a sender
  !!        that is not the dual exchange cannot occur.
  !!
  !!  SEND: here we have to consider two cases
  !!   a) The receiver is our dual exchange, we return
  !!      the favor and send the list of model node
  !!      numbers that is present on this process, this
  !!      would be either this%nodem1 or this%nodem2
  !!   b) The receiver is not the dual exchange. And
  !!      here we will send everything.
  !!
  !! 3) both models are remote
  !!
  !!  RECV: we will receive everything. In case the source
  !!        exchange is fully local, i.e. type 1) above, we get
  !!        all the data at the first attempt. Otherwise, it will
  !!        take a second attempt before all the data is in.
  !!        (To allow for two attempts, the nodem1 and nodem2
  !!        arrays are registered to be synchronized at two
  !!        consecutive stages)
  !!
  !!  SEND: nothing to be sent.
  !<
  type, public, extends(VirtualDataContainerType) :: VirtualExchangeType
    class(VirtualModelType), pointer :: v_model1 => null()
    class(VirtualModelType), pointer :: v_model2 => null()
    ! scalars
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
  contains
    procedure :: create => vx_create
    procedure :: prepare_stage => vx_prepare_stage
    procedure :: get_send_items => vx_get_send_items
    procedure :: get_recv_items => vx_get_recv_items
    procedure :: destroy => vx_destroy
    ! private
    procedure, private :: create_virtual_fields
    procedure, private :: deallocate_data
  end type VirtualExchangeType

contains

  !> @brief Create the virtual exchange base
  !<
  subroutine vx_create(this, name, exg_id, m1_id, m2_id)
    class(VirtualExchangeType) :: this
    character(len=*) :: name
    integer(I4B) :: exg_id
    integer(I4B) :: m1_id
    integer(I4B) :: m2_id
    ! local
    logical(LGP) :: is_local

    this%v_model1 => get_virtual_model(m1_id)
    this%v_model2 => get_virtual_model(m2_id)

    ! - if both models are local, is_local = true
    ! - if both are remote, is_local = false
    ! - if only one of them is remote, is_local = true and only the
    ! remote nodem1/2 array will get its property is_local = false
    is_local = this%v_model1%is_local .or. this%v_model2%is_local
    call this%VirtualDataContainerType%vdc_create(name, exg_id, is_local)

    ! allocate fields
    call this%create_virtual_fields()

  end subroutine vx_create

  subroutine create_virtual_fields(this)
    class(VirtualExchangeType) :: this
    ! local
    logical(LGP) :: is_nodem1_local
    logical(LGP) :: is_nodem2_local

    ! exchanges can be hybrid with both local and remote fields
    is_nodem1_local = this%is_local .and. this%v_model1%is_local
    is_nodem2_local = this%is_local .and. this%v_model2%is_local

    allocate (this%nexg)
    call this%create_field(this%nexg%to_base(), 'NEXG', '')
    allocate (this%naux)
    call this%create_field(this%naux%to_base(), 'NAUX', '')
    allocate (this%ianglex)
    call this%create_field(this%ianglex%to_base(), 'IANGLEX', '')
    allocate (this%nodem1)
    call this%create_field(this%nodem1%to_base(), 'NODEM1', '', is_nodem1_local)
    allocate (this%nodem2)
    call this%create_field(this%nodem2%to_base(), 'NODEM2', '', is_nodem2_local)
    allocate (this%ihc)
    call this%create_field(this%ihc%to_base(), 'IHC', '')
    allocate (this%cl1)
    call this%create_field(this%cl1%to_base(), 'CL1', '')
    allocate (this%cl2)
    call this%create_field(this%cl2%to_base(), 'CL2', '')
    allocate (this%hwva)
    call this%create_field(this%hwva%to_base(), 'HWVA', '')
    allocate (this%auxvar)
    call this%create_field(this%auxvar%to_base(), 'AUXVAR', '')

  end subroutine create_virtual_fields

  subroutine vx_prepare_stage(this, stage)
    class(VirtualExchangeType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: nexg, naux

    if (stage == STG_AFTER_EXG_DF) then

      call this%map(this%nexg%to_base(), &
                    (/STG_AFTER_EXG_DF/), MAP_ALL_TYPE)
      call this%map(this%naux%to_base(), &
                    (/STG_AFTER_EXG_DF/), MAP_ALL_TYPE)
      call this%map(this%ianglex%to_base(), &
                    (/STG_AFTER_EXG_DF/), MAP_ALL_TYPE)

    else if (stage == STG_AFTER_CON_CR) then

      nexg = this%nexg%get()
      naux = this%naux%get()
      call this%map(this%nodem1%to_base(), nexg, &
                    (/STG_AFTER_CON_CR, STG_BEFORE_CON_DF/), &
                    MAP_ALL_TYPE)
      call this%map(this%nodem2%to_base(), nexg, &
                    (/STG_AFTER_CON_CR, STG_BEFORE_CON_DF/), &
                    MAP_ALL_TYPE)
      call this%map(this%ihc%to_base(), nexg, &
                    (/STG_AFTER_CON_CR/), MAP_ALL_TYPE)
      call this%map(this%cl1%to_base(), nexg, &
                    (/STG_AFTER_CON_CR/), MAP_ALL_TYPE)
      call this%map(this%cl2%to_base(), nexg, &
                    (/STG_AFTER_CON_CR/), MAP_ALL_TYPE)
      call this%map(this%hwva%to_base(), nexg, &
                    (/STG_AFTER_CON_CR/), MAP_ALL_TYPE)
      call this%map(this%auxvar%to_base(), naux, nexg, &
                    (/STG_AFTER_CON_CR/), MAP_ALL_TYPE)

    end if

  end subroutine vx_prepare_stage

  subroutine vx_get_recv_items(this, stage, rank, virtual_items)
    class(VirtualExchangeType) :: this
    integer(I4B) :: stage
    integer(I4B) :: rank
    type(STLVecInt) :: virtual_items
    ! local
    integer(I4B) :: nodem1_idx, nodem2_idx
    class(*), pointer :: vdi

    vdi => this%nodem1
    nodem1_idx = this%virtual_data_list%GetIndex(vdi)
    vdi => this%nodem2
    nodem2_idx = this%virtual_data_list%GetIndex(vdi)

    if (this%v_model1%is_local .and. &
        this%v_model2%orig_rank == rank) then
      ! this is our image exchange on the other rank,
      ! only receive nodem2
      if (this%nodem2%check_stage(stage)) then
        call virtual_items%push_back(nodem2_idx)
      end if
    else if (this%v_model2%is_local .and. &
             this%v_model1%orig_rank == rank) then
      ! the reverse case...
      if (this%nodem1%check_stage(stage)) then
        call virtual_items%push_back(nodem1_idx)
      end if
    else
      ! receive all using base
      call this%VirtualDataContainerType%get_recv_items(stage, rank, &
                                                        virtual_items)
    end if

  end subroutine vx_get_recv_items

  subroutine vx_get_send_items(this, stage, rank, virtual_items)
    class(VirtualExchangeType) :: this
    integer(I4B) :: stage
    integer(I4B) :: rank
    type(STLVecInt) :: virtual_items
    ! local
    integer(I4B) :: nodem1_idx, nodem2_idx
    class(*), pointer :: vdi

    vdi => this%nodem1
    nodem1_idx = this%virtual_data_list%GetIndex(vdi)
    vdi => this%nodem2
    nodem2_idx = this%virtual_data_list%GetIndex(vdi)
    if (this%v_model1%is_local .and. &
        this%v_model2%orig_rank == rank) then
      ! this is our image exchange on the other rank,
      ! only send nodem1
      if (this%nodem1%check_stage(stage)) then
        call virtual_items%push_back(nodem1_idx)
      end if
    else if (this%v_model2%is_local .and. &
             this%v_model1%orig_rank == rank) then
      ! the reverse case...
      if (this%nodem2%check_stage(stage)) then
        call virtual_items%push_back(nodem2_idx)
      end if
    else
      ! send all of it
      call this%VirtualDataContainerType%get_send_items(stage, rank, &
                                                        virtual_items)
    end if

  end subroutine vx_get_send_items

  subroutine vx_destroy(this)
    class(VirtualExchangeType) :: this

    call this%VirtualDataContainerType%destroy()
    call this%deallocate_data()

  end subroutine vx_destroy

  subroutine deallocate_data(this)
    class(VirtualExchangeType) :: this

    deallocate (this%nexg)
    deallocate (this%naux)
    deallocate (this%ianglex)
    deallocate (this%nodem1)
    deallocate (this%nodem2)
    deallocate (this%ihc)
    deallocate (this%cl1)
    deallocate (this%cl2)
    deallocate (this%hwva)
    deallocate (this%auxvar)

  end subroutine deallocate_data

  !> @brief Returs a virtual exchange with the specified id
  !< from the global list
  function get_virtual_exchange(exg_id) result(virtual_exg)
    use VirtualDataListsModule, only: virtual_exchange_list
    integer(I4B) :: exg_id
    class(VirtualExchangeType), pointer :: virtual_exg
    ! local
    integer(I4B) :: i
    class(*), pointer :: ve

    virtual_exg => null()
    do i = 1, virtual_exchange_list%Count()
      ve => virtual_exchange_list%GetItem(i)
      select type (ve)
      class is (VirtualExchangeType)
        if (ve%id == exg_id) then
          virtual_exg => ve
          return
        end if
      end select
    end do

  end function get_virtual_exchange

  function get_virtual_exchange_from_list(list, idx) result(virtual_exg)
    type(ListType) :: list
    integer(I4B) :: idx
    class(VirtualExchangeType), pointer :: virtual_exg
    ! local
    class(*), pointer :: obj_ptr

    obj_ptr => list%GetItem(idx)
    virtual_exg => cast_as_virtual_exchange(obj_ptr)

  end function get_virtual_exchange_from_list

  function cast_as_virtual_exchange(obj_ptr) result(virtual_exg)
    class(*), pointer :: obj_ptr
    class(VirtualExchangeType), pointer :: virtual_exg

    virtual_exg => null()
    select type (obj_ptr)
    class is (VirtualExchangeType)
      virtual_exg => obj_ptr
    end select

  end function cast_as_virtual_exchange

end module VirtualExchangeModule

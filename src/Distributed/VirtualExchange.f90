module VirtualExchangeModule
  use VirtualBaseModule
  use VirtualDataContainerModule
  use VirtualModelModule, only: VirtualModelType, get_virtual_model
  use KindModule, only: I4B, LGP
  use ListModule, only: ListType
  use ConstantsModule, only: LENEXCHANGENAME
  use SimStagesModule
  implicit none
  private

  public :: get_virtual_exchange
  public :: get_virtual_exchange_from_list
  private :: cast_as_virtual_exchange


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
    procedure :: destroy => vx_destroy
    procedure, private :: allocate_data
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
    logical(LGP) :: is_remote
    
    this%v_model1 => get_virtual_model(m1_id)
    this%v_model2 => get_virtual_model(m2_id)

    is_remote = this%v_model1%is_remote .and. this%v_model2%is_remote
    call this%VirtualDataContainerType%vdc_create(name, exg_id, is_remote)

    ! allocate fields
    call this%allocate_data()

  end subroutine vx_create

  subroutine vx_prepare_stage(this, stage)
    class(VirtualExchangeType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: nexg, naux

    if (stage == STG_INIT) then

      call this%map(this%nexg%to_base(), 'NEXG', '', (/STG_INIT/), MAP_ALL_TYPE)
      call this%map(this%naux%to_base(), 'NAUX', '', (/STG_INIT/), MAP_ALL_TYPE)
      call this%map(this%ianglex%to_base(), 'IANGLEX', '', (/STG_INIT/), MAP_ALL_TYPE)

    else if (stage == STG_BEFORE_DF) then

      nexg = this%nexg%get()
      naux = this%naux%get()
      call this%map(this%nodem1%to_base(), 'NODEM1', '', nexg, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%nodem2%to_base(), 'NODEM2', '', nexg, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%ihc%to_base(), 'IHC', '', nexg, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%cl1%to_base(), 'CL1', '', nexg, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%cl2%to_base(), 'CL2', '', nexg, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%hwva%to_base(), 'HWVA', '', nexg, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%auxvar%to_base(), 'AUXVAR', '', naux, nexg, (/STG_BEFORE_DF/), MAP_ALL_TYPE)

    end if

  end subroutine vx_prepare_stage

  subroutine allocate_data(this)
    class(VirtualExchangeType) :: this

    allocate (this%nexg)
    allocate (this%naux)  
    allocate (this%ianglex)    
    allocate (this%nodem1)
    allocate (this%nodem2)
    allocate (this%ihc)
    allocate (this%cl1)
    allocate (this%cl2)
    allocate (this%hwva)
    allocate (this%auxvar)

  end subroutine allocate_data

  subroutine vx_destroy(this)
    class(VirtualExchangeType) :: this

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
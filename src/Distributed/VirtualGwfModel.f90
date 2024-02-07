module VirtualGwfModelModule
  use KindModule, only: I4B
  use VirtualBaseModule
  use VirtualDataContainerModule, only: VDC_GWFMODEL_TYPE
  use VirtualModelModule
  use SimStagesModule
  use NumericalModelModule, only: NumericalModelType
  implicit none
  private

  public :: add_virtual_gwf_model

  type, public, extends(VirtualModelType) :: VirtualGwfModelType
    ! NPF
    type(VirtualIntType), pointer :: npf_iangle1 => null()
    type(VirtualIntType), pointer :: npf_iangle2 => null()
    type(VirtualIntType), pointer :: npf_iangle3 => null()
    type(VirtualIntType), pointer :: npf_iwetdry => null()
    type(VirtualIntType), pointer :: inbuy => null()
    type(VirtualInt1dType), pointer :: npf_icelltype => null()
    type(VirtualDbl1dType), pointer :: npf_k11 => null()
    type(VirtualDbl1dType), pointer :: npf_k22 => null()
    type(VirtualDbl1dType), pointer :: npf_k33 => null()
    type(VirtualDbl1dType), pointer :: npf_angle1 => null()
    type(VirtualDbl1dType), pointer :: npf_angle2 => null()
    type(VirtualDbl1dType), pointer :: npf_angle3 => null()
    type(VirtualDbl1dType), pointer :: npf_wetdry => null()
    type(VirtualDbl1dType), pointer :: buy_dense => null()
  contains
    ! public
    procedure :: create => vgwf_create
    procedure :: destroy => vgwf_destroy
    procedure :: prepare_stage => vgwf_prepare_stage
    ! private
    procedure, private :: init_virtual_data
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
  end type VirtualGwfModelType

contains

  !> @brief Add virtual GWF model
  !<
  subroutine add_virtual_gwf_model(model_id, model_name, model)
    use VirtualDataListsModule, only: virtual_model_list
    integer(I4B) :: model_id !< global model id
    character(len=*) :: model_name !< model name
    class(NumericalModelType), pointer :: model !< the actual model (can be null() when remote)
    ! local
    class(VirtualGwfModelType), pointer :: virtual_gwf_model
    class(*), pointer :: obj

    allocate (virtual_gwf_model)
    call virtual_gwf_model%create(model_name, model_id, model)

    obj => virtual_gwf_model
    call virtual_model_list%Add(obj)

  end subroutine add_virtual_gwf_model

  subroutine vgwf_create(this, name, id, model)
    class(VirtualGwfModelType) :: this
    character(len=*) :: name
    integer(I4B) :: id
    class(NumericalModelType), pointer :: model

    ! create base
    call this%VirtualModelType%create(name, id, model)
    this%container_type = VDC_GWFMODEL_TYPE

    call this%allocate_data()
    call this%init_virtual_data()

  end subroutine vgwf_create

  subroutine init_virtual_data(this)
    class(VirtualGwfModelType) :: this

    call this%set(this%npf_iangle1%base(), 'IANGLE1', 'NPF', MAP_ALL_TYPE)
    call this%set(this%npf_iangle2%base(), 'IANGLE2', 'NPF', MAP_ALL_TYPE)
    call this%set(this%npf_iangle3%base(), 'IANGLE3', 'NPF', MAP_ALL_TYPE)
    call this%set(this%npf_iwetdry%base(), 'IWETDRY', 'NPF', MAP_ALL_TYPE)
    call this%set(this%inbuy%base(), 'INBUY', '', MAP_ALL_TYPE)
    call this%set(this%npf_icelltype%base(), 'ICELLTYPE', 'NPF', MAP_NODE_TYPE)
    call this%set(this%npf_k11%base(), 'K11', 'NPF', MAP_NODE_TYPE)
    call this%set(this%npf_k22%base(), 'K22', 'NPF', MAP_NODE_TYPE)
    call this%set(this%npf_k33%base(), 'K33', 'NPF', MAP_NODE_TYPE)
    call this%set(this%npf_angle1%base(), 'ANGLE1', 'NPF', MAP_NODE_TYPE)
    call this%set(this%npf_angle2%base(), 'ANGLE2', 'NPF', MAP_NODE_TYPE)
    call this%set(this%npf_angle3%base(), 'ANGLE3', 'NPF', MAP_NODE_TYPE)
    call this%set(this%npf_wetdry%base(), 'WETDRY', 'NPF', MAP_NODE_TYPE)
    call this%set(this%buy_dense%base(), 'DENSE', 'BUY', MAP_NODE_TYPE)

  end subroutine init_virtual_data

  subroutine vgwf_prepare_stage(this, stage)
    class(VirtualGwfModelType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: nr_nodes

    ! prepare base (=numerical) model data items
    call this%VirtualModelType%prepare_stage(stage)

    if (stage == STG_AFT_MDL_DF) then

      call this%map(this%npf_iangle1%base(), (/STG_AFT_MDL_DF/))
      call this%map(this%npf_iangle2%base(), (/STG_AFT_MDL_DF/))
      call this%map(this%npf_iangle3%base(), (/STG_AFT_MDL_DF/))
      call this%map(this%npf_iwetdry%base(), (/STG_AFT_MDL_DF/))
      call this%map(this%inbuy%base(), (/STG_AFT_MDL_DF/))

    else if (stage == STG_BFR_CON_AR) then

      nr_nodes = this%element_maps(MAP_NODE_TYPE)%nr_virt_elems
      ! Num. model data
      call this%map(this%x%base(), nr_nodes, &
                    (/STG_BFR_CON_AR, STG_BFR_EXG_AD, STG_BFR_EXG_CF/))
      call this%map(this%ibound%base(), nr_nodes, &
                    (/STG_BFR_CON_AR, STG_BFR_EXG_AD, STG_BFR_EXG_CF/))
      call this%map(this%x_old%base(), nr_nodes, &
                    (/STG_BFR_EXG_AD, STG_BFR_EXG_CF/))
      ! NPF
      call this%map(this%npf_icelltype%base(), nr_nodes, (/STG_BFR_CON_AR/))
      call this%map(this%npf_k11%base(), nr_nodes, (/STG_BFR_CON_AR/))
      call this%map(this%npf_k22%base(), nr_nodes, (/STG_BFR_CON_AR/))
      call this%map(this%npf_k33%base(), nr_nodes, (/STG_BFR_CON_AR/))

      if (this%npf_iangle1%get() > 0) then
        call this%map(this%npf_angle1%base(), nr_nodes, (/STG_BFR_CON_AR/))
      else
        call this%map(this%npf_angle1%base(), 0, (/STG_NEVER/))
      end if
      if (this%npf_iangle2%get() > 0) then
        call this%map(this%npf_angle2%base(), nr_nodes, (/STG_BFR_CON_AR/))
      else
        call this%map(this%npf_angle2%base(), 0, (/STG_NEVER/))
      end if
      if (this%npf_iangle3%get() > 0) then
        call this%map(this%npf_angle3%base(), nr_nodes, (/STG_BFR_CON_AR/))
      else
        call this%map(this%npf_angle3%base(), 0, (/STG_NEVER/))
      end if
      if (this%npf_iwetdry%get() > 0) then
        call this%map(this%npf_wetdry%base(), nr_nodes, (/STG_BFR_CON_AR/))
      else
        call this%map(this%npf_wetdry%base(), 0, (/STG_NEVER/))
      end if

      if (this%inbuy%get() > 0) then
        call this%map(this%buy_dense%base(), nr_nodes, (/STG_BFR_EXG_CF/))
      else
        call this%map(this%buy_dense%base(), 0, (/STG_NEVER/))
      end if

    end if

  end subroutine vgwf_prepare_stage

  subroutine vgwf_destroy(this)
    class(VirtualGwfModelType) :: this

    call this%VirtualModelType%destroy()
    call this%deallocate_data()

  end subroutine vgwf_destroy

  subroutine allocate_data(this)
    class(VirtualGwfModelType) :: this

    allocate (this%npf_iangle1)
    allocate (this%npf_iangle2)
    allocate (this%npf_iangle3)
    allocate (this%npf_iwetdry)
    allocate (this%inbuy)
    allocate (this%npf_icelltype)
    allocate (this%npf_k11)
    allocate (this%npf_k22)
    allocate (this%npf_k33)
    allocate (this%npf_angle1)
    allocate (this%npf_angle2)
    allocate (this%npf_angle3)
    allocate (this%npf_wetdry)
    allocate (this%buy_dense)

  end subroutine allocate_data

  subroutine deallocate_data(this)
    class(VirtualGwfModelType) :: this

    deallocate (this%npf_iangle1)
    deallocate (this%npf_iangle2)
    deallocate (this%npf_iangle3)
    deallocate (this%npf_iwetdry)
    deallocate (this%inbuy)
    deallocate (this%npf_icelltype)
    deallocate (this%npf_k11)
    deallocate (this%npf_k22)
    deallocate (this%npf_k33)
    deallocate (this%npf_angle1)
    deallocate (this%npf_angle2)
    deallocate (this%npf_angle3)
    deallocate (this%npf_wetdry)
    deallocate (this%buy_dense)

  end subroutine deallocate_data

end module VirtualGwfModelModule

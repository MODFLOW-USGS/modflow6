module VirtualGwfModelModule
  use KindModule, only: I4B
  use VirtualBaseModule
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
    type(VirtualInt1dType), pointer :: npf_icelltype => null()
    type(VirtualDbl1dType), pointer :: npf_k11 => null()
    type(VirtualDbl1dType), pointer :: npf_k22 => null()
    type(VirtualDbl1dType), pointer :: npf_k33 => null()
    type(VirtualDbl1dType), pointer :: npf_angle1 => null()
    type(VirtualDbl1dType), pointer :: npf_angle2 => null()
    type(VirtualDbl1dType), pointer :: npf_angle3 => null()
    type(VirtualDbl1dType), pointer :: npf_wetdry => null()
  contains
    ! public
    procedure :: create => vgwf_create
    procedure :: destroy => vgwf_destroy
    procedure :: prepare_stage => vgwf_prepare_stage
    ! private
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
    
    ! allocate fields
    call this%allocate_data()

  end subroutine vgwf_create

  subroutine vgwf_prepare_stage(this, stage)
    class(VirtualGwfModelType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: nr_nodes

    ! prepare base (=numerical) model data items
    call this%VirtualModelType%prepare_stage(stage)

    if (stage == STG_AFTER_MDL_DF) then

      call this%map(this%npf_iangle1%to_base(), 'IANGLE1', 'NPF', (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)
      call this%map(this%npf_iangle2%to_base(), 'IANGLE2', 'NPF', (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)
      call this%map(this%npf_iangle3%to_base(), 'IANGLE3', 'NPF', (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)
      call this%map(this%npf_iwetdry%to_base(), 'IWETDRY', 'NPF', (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)

    else if (stage == STG_BEFORE_AR) then

      nr_nodes = 0 ! TODO_MJR: this should follow from the map
      ! Num. model data
      call this%map(this%x%to_base(), 'X', '', nr_nodes, (/STG_BEFORE_AR, STG_BEFORE_AD, STG_BEFORE_CF/), MAP_NODE_TYPE)
      call this%map(this%ibound%to_base(), 'IBOUND', '', nr_nodes, (/STG_BEFORE_AR, STG_BEFORE_AD, STG_BEFORE_CF/), MAP_NODE_TYPE)
      call this%map(this%x_old%to_base(), 'XOLD', '',  nr_nodes, (/STG_BEFORE_AD, STG_BEFORE_CF/), MAP_NODE_TYPE)
      ! NPF
      call this%map(this%npf_icelltype%to_base(), 'ICELLTYPE', 'NPF', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
      call this%map(this%npf_k11%to_base(), 'K11', 'NPF', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
      call this%map(this%npf_k22%to_base(), 'K22', 'NPF', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
      call this%map(this%npf_k33%to_base(), 'K33', 'NPF', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)    
      if (this%npf_iangle1%get() > 0) then
        call this%map(this%npf_angle1%to_base(), 'ANGLE1', 'NPF', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
      else
        call this%map(this%npf_angle1%to_base(), 'ANGLE1', 'NPF', 0, (/STG_NEVER/), MAP_NODE_TYPE)
      end if
      if (this%npf_iangle2%get() > 0) then
        call this%map(this%npf_angle2%to_base(), 'ANGLE2', 'NPF', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
      else
        call this%map(this%npf_angle2%to_base(), 'ANGLE2', 'NPF', 0, (/STG_NEVER/), MAP_NODE_TYPE)
      end if
      if (this%npf_iangle3%get() > 0) then
        call this%map(this%npf_angle3%to_base(), 'ANGLE3', 'NPF', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
      else
        call this%map(this%npf_angle3%to_base(), 'ANGLE3', 'NPF', 0, (/STG_NEVER/), MAP_NODE_TYPE)
      end if
      if (this%npf_iwetdry%get() > 0) then
        call this%map(this%npf_wetdry%to_base(), 'WETDRY', 'NPF', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
      else
        call this%map(this%npf_wetdry%to_base(), 'WETDRY', 'NPF', 0, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
      end if

    end if

  end subroutine vgwf_prepare_stage    
  
  subroutine allocate_data(this)
    class(VirtualGwfModelType) :: this
    
    allocate (this%npf_iangle1)
    allocate (this%npf_iangle2)
    allocate (this%npf_iangle3)
    allocate (this%npf_iwetdry)  
    allocate (this%npf_icelltype)
    allocate (this%npf_k11)
    allocate (this%npf_k22)
    allocate (this%npf_k33)
    allocate (this%npf_angle1)
    allocate (this%npf_angle2)
    allocate (this%npf_angle3)
    allocate (this%npf_wetdry)

  end subroutine allocate_data

  subroutine vgwf_destroy(this)
    class(VirtualGwfModelType) :: this
    
    call this%VirtualModelType%destroy()
    call this%deallocate_data()

  end subroutine vgwf_destroy

  subroutine deallocate_data(this)
    class(VirtualGwfModelType) :: this
    
    deallocate (this%npf_iangle1)
    deallocate (this%npf_iangle2)
    deallocate (this%npf_iangle3)
    deallocate (this%npf_iwetdry)  
    deallocate (this%npf_icelltype)
    deallocate (this%npf_k11)
    deallocate (this%npf_k22)
    deallocate (this%npf_k33)
    deallocate (this%npf_angle1)
    deallocate (this%npf_angle2)
    deallocate (this%npf_angle3)
    deallocate (this%npf_wetdry)

  end subroutine deallocate_data

end module VirtualGwfModelModule
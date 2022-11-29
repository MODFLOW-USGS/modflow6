module VirtualGwfModelModule
  use VirtualBaseModule
  use VirtualModelModule
  use SimStagesModule
  use NumericalModelModule, only: NumericalModelType
  implicit none
  private 

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
    procedure :: init_model_data => vgwf_init_model_data
    ! private
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
  end type VirtualGwfModelType

contains

  subroutine vgwf_create(this, model_name, model)
    class(VirtualGwfModelType) :: this
    character(len=*) :: model_name
    class(NumericalModelType), pointer :: model
    
    ! model can be null
    if (.not. associated(model)) this%is_remote = .true.

    ! create base
    call this%VirtualModelType%create(model_name, model)
    
    ! allocate fields
    call this%allocate_data()

    ! map virtual memory
    call this%map(this%npf_iangle1%to_base(), 'IANGLE1', 'NPF', (/STG_BEFORE_INIT/), MAP_ALL_TYPE)
    call this%map(this%npf_iangle2%to_base(), 'IANGLE2', 'NPF', (/STG_BEFORE_INIT/), MAP_ALL_TYPE)
    call this%map(this%npf_iangle3%to_base(), 'IANGLE3', 'NPF', (/STG_BEFORE_INIT/), MAP_ALL_TYPE)
    call this%map(this%npf_iwetdry%to_base(), 'IWETDRY', 'NPF', (/STG_BEFORE_INIT/), MAP_ALL_TYPE)

  end subroutine vgwf_create

  subroutine vgwf_init_model_data(this)
    class(VirtualGwfModelType) :: this
    ! local
    integer(I4B) :: nodes

    nodes = 0 ! TODO_MJR: this should follow from the map

    ! Num. model data
    call this%map(this%x%to_base(), 'X', '', nodes, (/STG_BEFORE_AR, STG_BEFORE_AD, STG_BEFORE_CF/), MAP_NODE_TYPE)
    call this%map(this%ibound%to_base(), 'IBOUND', '', nodes, (/STG_BEFORE_AR, STG_BEFORE_AD, STG_BEFORE_CF/), MAP_NODE_TYPE)
    call this%map(this%x_old%to_base(), 'XOLD', '',  nodes, (/STG_BEFORE_AD, STG_BEFORE_CF/), MAP_NODE_TYPE)

    ! NPF
    call this%map(this%npf_icelltype%to_base(), 'ICELLTYPE', 'NPF', nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
    call this%map(this%npf_k11%to_base(), 'K11', 'NPF', nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
    call this%map(this%npf_k22%to_base(), 'K22', 'NPF', nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
    call this%map(this%npf_k33%to_base(), 'K33', 'NPF', nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)    
    if (this%npf_iangle1%value > 0) then
      call this%map(this%npf_angle1%to_base(), 'ANGLE1', 'NPF', nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
    else
      call this%map(this%npf_angle1%to_base(), 'ANGLE1', 'NPF', 0, (/STG_NEVER/), MAP_NODE_TYPE)
    end if
    if (this%npf_iangle2%value > 0) then
      call this%map(this%npf_angle2%to_base(), 'ANGLE2', 'NPF', nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
    else
      call this%map(this%npf_angle2%to_base(), 'ANGLE2', 'NPF', 0, (/STG_NEVER/), MAP_NODE_TYPE)
    end if
    if (this%npf_iangle3%value > 0) then
      call this%map(this%npf_angle3%to_base(), 'ANGLE3', 'NPF', nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
    else
      call this%map(this%npf_angle3%to_base(), 'ANGLE3', 'NPF', 0, (/STG_NEVER/), MAP_NODE_TYPE)
    end if
    if (this%npf_iwetdry%value > 0) then
      call this%map(this%npf_wetdry%to_base(), 'WETDRY', 'NPF', nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
    else
      call this%map(this%npf_wetdry%to_base(), 'WETDRY', 'NPF', 0, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
    end if

  end subroutine vgwf_init_model_data
  
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
    
    call this%deallocate_data()

    call this%VirtualModelType%destroy()

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
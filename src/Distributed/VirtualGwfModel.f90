module VirtualGwfModelModule
  use VirtualBaseModule
  use VirtualModelModule
  use SimStagesModule
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
    procedure :: create_remote => create_virtual_gwf_remote
    procedure :: create_local => create_virtual_gwf_local
    procedure :: destroy => destroy_virtual_gwf
    ! private
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
  end type VirtualGwfModelType

contains

  subroutine create_virtual_gwf_remote(this, model_name)
    class(VirtualGwfModelType) :: this
    character(len=*) :: model_name
    
    ! create base
    call this%VirtualModelType%create_remote(model_name)
    
    ! allocate fields
    call this%allocate_data()

    ! map virtual memory to remote (first phase)
    call this%npf_iangle1%map('IANGLE1', 'NPF', model_name, (/STG_BEFORE_INIT/), MAP_ALL_TYPE)
    call this%add_to_list(this%npf_iangle1%to_base())
    call this%npf_iangle2%map('IANGLE2', 'NPF', model_name, (/STG_BEFORE_INIT/), MAP_ALL_TYPE)
    call this%add_to_list(this%npf_iangle2%to_base())
    call this%npf_iangle3%map('IANGLE3', 'NPF', model_name, (/STG_BEFORE_INIT/), MAP_ALL_TYPE)
    call this%add_to_list(this%npf_iangle3%to_base())
    call this%npf_iwetdry%map('IWETDRY', 'NPF', model_name, (/STG_BEFORE_INIT/), MAP_ALL_TYPE)
    call this%add_to_list(this%npf_iwetdry%to_base())

  end subroutine create_virtual_gwf_remote

  subroutine create_virtual_gwf_local(this, model_name)
    class(VirtualGwfModelType) :: this
    character(len=*) :: model_name

    ! create base
    call this%VirtualModelType%create_local(model_name)

    ! link gwf fields (all)
    call this%npf_iangle1%link('IANGLE1', 'NPF', model_name)
    call this%npf_iangle2%link('IANGLE2', 'NPF', model_name)
    call this%npf_iangle3%link('IANGLE3', 'NPF', model_name)
    call this%npf_iwetdry%link('IWETDRY', 'NPF', model_name)  
    call this%npf_icelltype%link('ICELLTYPE', 'NPF', model_name)
    call this%npf_k11%link('K11', 'NPF', model_name)
    call this%npf_k22%link('K22', 'NPF', model_name)
    call this%npf_k33%link('K33', 'NPF', model_name)
    call this%npf_angle1%link('ANGLE1', 'NPF', model_name)
    call this%npf_angle2%link('ANGLE2', 'NPF', model_name)
    call this%npf_angle3%link('ANGLE3', 'NPF', model_name)
    call this%npf_wetdry%link('WETDRY', 'NPF', model_name)

  end subroutine create_virtual_gwf_local
  
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

  subroutine destroy_virtual_gwf(this)
    class(VirtualGwfModelType) :: this

    call this%deallocate_data()

  end subroutine destroy_virtual_gwf

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
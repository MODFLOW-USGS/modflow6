module VirtualModelModule  
  use VirtualBaseModule
  use ConstantsModule, only: LENMODELNAME
  use SimStagesModule
  use ListModule
  implicit none
  private
  
  type, public :: VirtualModelType
    type(ListType) :: remote_data
    character(len=LENMODELNAME) :: model_name
    ! CON
    type(VirtualInt1dType), pointer :: con_ia => null()
    type(VirtualInt1dType), pointer :: con_ja => null()
    type(VirtualInt1dType), pointer :: con_jas => null()
    type(VirtualInt1dType), pointer :: con_ihc => null()
    type(VirtualDbl1dType), pointer :: con_hwva => null()
    type(VirtualDbl1dType), pointer :: con_cl1 => null()
    type(VirtualDbl1dType), pointer :: con_cl2 => null()
    type(VirtualDbl1dType), pointer :: con_anglex => null()
    ! DIS
    type(VirtualIntType), pointer :: dis_nodes => null()
    type(VirtualIntType), pointer :: dis_nja => null()
    type(VirtualIntType), pointer :: dis_njas => null()
    type(VirtualDblType), pointer :: dis_xorigin => null()
    type(VirtualDblType), pointer :: dis_yorigin => null()
    type(VirtualDblType), pointer :: dis_angrot => null()
    type(VirtualDbl1dType), pointer :: dis_xc => null()
    type(VirtualDbl1dType), pointer :: dis_yc => null()
    type(VirtualDbl1dType), pointer :: dis_top => null()
    type(VirtualDbl1dType), pointer :: dis_bot => null()
    type(VirtualDbl1dType), pointer :: dis_area => null()
    ! Numerical Model fields
    type(VirtualIntType), pointer :: moffset => null()
    type(VirtualDbl1dType), pointer :: x => null()
    type(VirtualDbl1dType), pointer :: x_old => null()
    type(VirtualDbl1dType), pointer :: ibound => null()
  contains
    ! public
    procedure :: create_remote => create_virtual_model_remote
    procedure :: create_local => create_virtual_model_local
    procedure :: destroy => destroy_virtual_model
    ! protected
    procedure :: add_to_list
    ! private
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
  end type VirtualModelType

contains

subroutine create_virtual_model_remote(this, model_name)
  class(VirtualModelType) :: this
  character(len=*) :: model_name

  this%model_name = model_name

  ! allocate fields
  call this%allocate_data()

  ! map virtual memory to remote (first phase)
  call this%moffset%map('MOFFSET', '', model_name, (/STG_BEFORE_AC/), MAP_ALL_TYPE)
  call this%add_to_list(this%moffset%to_base())
  call this%dis_nodes%map('NODES', 'DIS', model_name, (/STG_BEFORE_INIT/), MAP_ALL_TYPE)
  call this%add_to_list(this%dis_nodes%to_base())
  call this%dis_nja%map('NJA', 'DIS', model_name, (/STG_BEFORE_INIT/), MAP_ALL_TYPE)
  call this%add_to_list(this%dis_nja%to_base())
  call this%dis_njas%map('NJAS', 'DIS', model_name, (/STG_BEFORE_INIT/), MAP_ALL_TYPE)
  call this%add_to_list(this%dis_njas%to_base())

end subroutine create_virtual_model_remote

subroutine create_virtual_model_local(this, model_name)
  class(VirtualModelType) :: this
  character(len=*) :: model_name

  this%model_name = model_name

  ! allocate fields
  call this%allocate_data()
  
  ! link model fields
  call this%con_ia%link('MOFFSET', '', model_name)
  ! ...

end subroutine create_virtual_model_local

subroutine allocate_data(this)
  class(VirtualModelType) :: this
  
  allocate (this%moffset)
  ! ...

end subroutine allocate_data  

subroutine add_to_list(this, virtual_data)
  class(VirtualModelType) :: this
  class(VirtualDataType), pointer :: virtual_data
  ! local
  class(*), pointer :: vdata_ptr

  vdata_ptr => virtual_data
  call this%remote_data%Add(vdata_ptr)

end subroutine add_to_list

subroutine destroy_virtual_model(this)
  class(VirtualModelType) :: this

  call this%deallocate_data()

end subroutine destroy_virtual_model

subroutine deallocate_data(this)
  class(VirtualModelType) :: this

  deallocate (this%moffset)
  ! ...

end subroutine deallocate_data



end module VirtualModelModule
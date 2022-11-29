module VirtualModelModule  
  use VirtualBaseModule
  use VirtualDataContainerModule
  use ConstantsModule, only: LENMEMPATH
  use KindModule, only: LGP
  use SimStagesModule
  use NumericalModelModule, only: NumericalModelType
  implicit none
  private
  
  type, public, extends(VirtualDataContainerType) :: VirtualModelType    
    class(NumericalModelType), pointer :: local_model
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
    type(VirtualInt1dType), pointer :: ibound => null()
  contains
    ! public
    procedure :: create => vm_create
    procedure :: init_grid_data => vm_init_grid_data
    procedure :: init_model_data => vm_init_model_data
    procedure :: destroy => vm_destroy
    
    ! private
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
  end type VirtualModelType

contains

subroutine vm_create(this, model_name, model)
  class(VirtualModelType) :: this
  character(len=*) :: model_name
  class(NumericalModelType), pointer :: model

  this%name = model_name
  this%local_model => model

  ! allocate fields
  call this%allocate_data()

  ! map virtual memory (first phase)
  call this%map(this%moffset%to_base(), 'MOFFSET', '', (/STG_BEFORE_AC/), MAP_ALL_TYPE)
  call this%map(this%dis_nodes%to_base(), 'NODES', 'DIS', (/STG_BEFORE_AC/), MAP_ALL_TYPE)
  call this%map(this%dis_nja%to_base(), 'NJA', 'DIS', (/STG_BEFORE_AC/), MAP_ALL_TYPE)
  call this%map(this%dis_njas%to_base(), 'NJAS', 'DIS', (/STG_BEFORE_AC/), MAP_ALL_TYPE)

end subroutine vm_create

subroutine vm_init_grid_data(this)
  class(VirtualModelType) :: this
  ! local
  integer(I4B) :: nodes, nja, njas

  nodes = this%dis_nodes%value
  nja = this%dis_nja%value
  njas = this%dis_njas%value

  ! CON
  call this%map(this%dis_xorigin%to_base(), 'XORIGIN', 'DIS', (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%dis_yorigin%to_base(), 'YORIGIN', 'DIS', (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%dis_angrot%to_base(), 'ANGROT', 'DIS', (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%dis_xc%to_base(), 'XC', 'DIS', nodes, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%dis_yc%to_base(), 'YC', 'DIS', nodes, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%dis_top%to_base(), 'TOP', 'DIS', nodes, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%dis_bot%to_base(), 'BOT', 'DIS', nodes, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  ! DIS
  call this%map(this%con_ia%to_base(), 'IA', 'CON', nodes + 1, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%con_ja%to_base(), 'JA', 'CON', nja, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%con_jas%to_base(), 'JAS', 'CON', nja, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%con_ihc%to_base(), 'IHC', 'CON', njas, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%con_hwva%to_base(), 'HWVA', 'CON', njas, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%con_cl1%to_base(), 'CL1', 'CON', njas, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%con_cl2%to_base(), 'CL2', 'CON', njas, (/STG_BEFORE_DF/), MAP_ALL_TYPE)
  call this%map(this%con_anglex%to_base(), 'ANGLEX', 'CON', njas, (/STG_BEFORE_DF/), MAP_ALL_TYPE)

end subroutine vm_init_grid_data

subroutine vm_init_model_data(this)
  use SimModule, only: ustop
  class(VirtualModelType) :: this

  ! This should be overridden:
  ! derived types should decide what data 
  ! to map and which stages to sync
  write(*,*) 'Error: virtual model data not initialized'
  call ustop()

end subroutine vm_init_model_data

subroutine vm_destroy(this)
  class(VirtualModelType) :: this

  call this%deallocate_data()

  call this%VirtualDataContainerType%destroy()

end subroutine vm_destroy

subroutine allocate_data(this)
  class(VirtualModelType) :: this
  
  ! CON
  allocate (this%con_ia)
  allocate (this%con_ja)
  allocate (this%con_jas)
  allocate (this%con_ihc)
  allocate (this%con_hwva)
  allocate (this%con_cl1)
  allocate (this%con_cl2)
  allocate (this%con_anglex)
  ! DIS
  allocate (this%dis_nodes)
  allocate (this%dis_nja)
  allocate (this%dis_njas)
  allocate (this%dis_xorigin)
  allocate (this%dis_yorigin)
  allocate (this%dis_angrot)
  allocate (this%dis_xc)
  allocate (this%dis_yc)
  allocate (this%dis_top)
  allocate (this%dis_bot)
  allocate (this%dis_area)
  ! Numerical model
  allocate (this%moffset)
  allocate (this%x)
  allocate (this%x_old)
  allocate (this%ibound)

end subroutine allocate_data

subroutine deallocate_data(this)
  class(VirtualModelType) :: this

  ! TODO_MJR: add loop with deallocate for virtual mem items

  ! CON
  deallocate (this%con_ia)
  deallocate (this%con_ja)
  deallocate (this%con_jas)
  deallocate (this%con_ihc)
  deallocate (this%con_hwva)
  deallocate (this%con_cl1)
  deallocate (this%con_cl2)
  deallocate (this%con_anglex)
  ! DIS
  deallocate (this%dis_nodes)
  deallocate (this%dis_nja)
  deallocate (this%dis_njas)
  deallocate (this%dis_xorigin)
  deallocate (this%dis_yorigin)
  deallocate (this%dis_angrot)
  deallocate (this%dis_xc)
  deallocate (this%dis_yc)
  deallocate (this%dis_top)
  deallocate (this%dis_bot)
  deallocate (this%dis_area)
  ! Numerical model
  deallocate (this%moffset)
  deallocate (this%x)
  deallocate (this%x_old)
  deallocate (this%ibound)

end subroutine deallocate_data



end module VirtualModelModule
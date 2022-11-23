module VirtualGwtModelModule
  use VirtualBaseModule
  use VirtualModelModule
  implicit none
  private

  type, extends(VirtualModelType) :: VirtualGwtModelType
    ! DSP
    type(VirtualIntType), pointer :: dsp_idiffc => null()
    type(VirtualIntType), pointer :: dsp_idisp => null()
    type(VirtualDbl1dType), pointer :: dsp_diffc => null()
    type(VirtualDbl1dType), pointer :: dsp_alh => null()
    type(VirtualDbl1dType), pointer :: dsp_alv => null()
    type(VirtualDbl1dType), pointer :: dsp_ath1 => null()
    type(VirtualDbl1dType), pointer :: dsp_ath2 => null()
    type(VirtualDbl1dType), pointer :: dsp_atv => null()
    ! FMI
    type(VirtualDbl1dType), pointer :: fmi_gwfhead => null()
    type(VirtualDbl1dType), pointer :: fmi_gwfsat => null()
    type(VirtualDbl2dType), pointer :: fmi_gwfspdis => null()
    type(VirtualDbl1dType), pointer :: fmi_gwfflowja => null()
    ! MST
    type(VirtualDbl1dType), pointer :: mst_porosity => null()
    ! GWT Model fields
    type(VirtualIntType), pointer :: indsp => null()
    type(VirtualIntType), pointer :: inmst => null()
  contains
    ! public
    procedure :: create_remote => create_virtual_gwt_remote
    procedure :: create_local => create_virtual_gwt_local
    procedure :: destroy
    ! private

  end type VirtualGwtModelType

contains

subroutine create_virtual_gwt_remote(this, model_name)
  class(VirtualGwtModelType) :: this
  character(len=*) :: model_name

  ! create base
  call this%VirtualModelType%create_remote(model_name)

  ! allocate fields

  ! map virtual memory to remote (this is done in stages)

end subroutine create_virtual_gwt_remote

subroutine create_virtual_gwt_local(this, model_name)
  class(VirtualGwtModelType) :: this
  character(len=*) :: model_name

  ! create base
  call this%VirtualModelType%create_local(model_name)

  ! link gwt fields

end subroutine create_virtual_gwt_local

subroutine destroy(this)
  class(VirtualGwtModelType) :: this

end subroutine destroy


end module VirtualGwtModelModule
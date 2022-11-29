module VirtualGwtModelModule
  use VirtualBaseModule
  use VirtualModelModule
  use NumericalModelModule, only: NumericalModelType
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
    procedure :: create => vgwt_create
    procedure :: destroy => vgwt_destroy
    procedure :: init_model_data => vgwt_init_model_data
    ! private

  end type VirtualGwtModelType

contains

subroutine vgwt_create(this, model_name, model)
  class(VirtualGwtModelType) :: this
  character(len=*) :: model_name
  class(NumericalModelType), pointer :: model

  ! create base
  call this%VirtualModelType%create(model_name, model)

  ! allocate fields

  ! map virtual memory to remote (this is done in stages)

end subroutine vgwt_create

subroutine vgwt_init_model_data(this)
  class(VirtualGwtModelType) :: this
end subroutine vgwt_init_model_data

subroutine vgwt_destroy(this)
  class(VirtualGwtModelType) :: this

  call this%VirtualModelType%destroy()

end subroutine vgwt_destroy


end module VirtualGwtModelModule
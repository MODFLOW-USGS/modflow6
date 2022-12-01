module VirtualGwtModelModule
  use KindModule, only: I4B
  use VirtualBaseModule
  use VirtualModelModule
  use NumericalModelModule, only: NumericalModelType
  implicit none
  private

  public :: add_virtual_gwt_model

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
    procedure :: prepare_stage => vgwt_prepare_stage
    procedure :: destroy => vgwt_destroy
    ! private

  end type VirtualGwtModelType

contains

subroutine add_virtual_gwt_model(model_id, model_name, model)
  use VirtualDataListsModule, only: virtual_model_list
  integer(I4B) :: model_id !< global model id
  character(len=*) :: model_name !< model name
  class(NumericalModelType), pointer :: model !< the actual model (can be null() when remote)
  ! local
  class(VirtualGwtModelType), pointer :: virtual_gwt_model
  class(*), pointer :: obj

  allocate (virtual_gwt_model)
  call virtual_gwt_model%create(model_name, model_id, model)

  obj => virtual_gwt_model
  call virtual_model_list%Add(obj)
  
end subroutine add_virtual_gwt_model

subroutine vgwt_create(this, name, id, model)
  class(VirtualGwtModelType) :: this
  character(len=*) :: name
  integer(I4B) :: id
  class(NumericalModelType), pointer :: model

  ! create base
  call this%VirtualModelType%create(name, id, model)

  ! allocate fields

  ! map virtual memory to remote (this is done in stages)

end subroutine vgwt_create

subroutine vgwt_prepare_stage(this, stage)
  class(VirtualGwtModelType) :: this
  integer(I4B) :: stage

end subroutine vgwt_prepare_stage

subroutine vgwt_destroy(this)
  class(VirtualGwtModelType) :: this

  call this%VirtualModelType%destroy()

end subroutine vgwt_destroy


end module VirtualGwtModelModule
module VirtualGwtModelModule
  use KindModule, only: I4B
  use SimStagesModule
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
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
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
  call this%allocate_data()

end subroutine vgwt_create

subroutine vgwt_prepare_stage(this, stage)
  class(VirtualGwtModelType) :: this
  integer(I4B) :: stage
  ! local
  integer(I4B) :: nr_nodes, nr_conns

  ! prepare base (=numerical) model data items
  call this%VirtualModelType%prepare_stage(stage)

  ! TODO_MJR: get these from the map
  nr_nodes = 0
  nr_conns = 0

  if (stage == STG_AFTER_MDL_DF) then
    
    call this%map(this%dsp_idiffc%to_base(),'IDIFFC', 'DSP', (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)
    call this%map(this%dsp_idisp%to_base(), 'IDISP', 'DSP', (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)
    call this%map(this%indsp%to_base(), 'INDSP', '', (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)
    call this%map(this%inmst%to_base(), 'INMST', '', (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)

  else if (stage == STG_BEFORE_AR) then

    call this%map(this%x%to_base(), 'X', '', nr_nodes, (/STG_BEFORE_AR, STG_BEFORE_AD, STG_BEFORE_CF/), MAP_NODE_TYPE)
    call this%map(this%ibound%to_base(), 'IBOUND', '', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
    if (this%dsp_idiffc%get() > 0) then
      call this%map(this%dsp_diffc%to_base(), 'DIFFC', 'DSP', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
    end if
    if (this%dsp_idisp%get() > 0) then
      call this%map(this%dsp_alh%to_base(), 'ALH', 'DSP', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
      call this%map(this%dsp_alv%to_base(), 'ALV', 'DSP', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
      call this%map(this%dsp_ath1%to_base(), 'ATH1', 'DSP', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
      call this%map(this%dsp_ath2%to_base(), 'ATH2', 'DSP', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
      call this%map(this%dsp_atv%to_base(), 'ATV', 'DSP', nr_nodes, (/STG_BEFORE_AR/), MAP_NODE_TYPE)
    end if

    call this%map(this%fmi_gwfhead%to_base(), 'GWFHEAD', 'FMI', nr_nodes, (/STG_BEFORE_AD/), MAP_NODE_TYPE)
    call this%map(this%fmi_gwfsat%to_base(), 'GWFSAT', 'FMI', nr_nodes, (/STG_BEFORE_AD/), MAP_NODE_TYPE)
    call this%map(this%fmi_gwfspdis%to_base(), 'GWFSPDIS', 'FMI',  3, nr_nodes, (/STG_BEFORE_AD/), MAP_NODE_TYPE)
    call this%map(this%fmi_gwfflowja%to_base(), 'GWFFLOWJA', 'FMI', nr_conns, (/STG_BEFORE_AD/), MAP_NODE_TYPE)

    if (this%indsp%get() > 0 .and. this%inmst%get() > 0) then
      call this%map(this%mst_porosity%to_base(), 'POROSITY', 'MST', nr_nodes, (/STG_AFTER_AR/), MAP_NODE_TYPE)
    end if

  end if

end subroutine vgwt_prepare_stage

subroutine allocate_data(this)
  class(VirtualGwtModelType) :: this

  allocate (this%dsp_idiffc)
  allocate (this%dsp_idisp)
  allocate (this%dsp_diffc)
  allocate (this%dsp_alh)
  allocate (this%dsp_alv)
  allocate (this%dsp_ath1)
  allocate (this%dsp_ath2)
  allocate (this%dsp_atv)
  allocate (this%fmi_gwfhead)
  allocate (this%fmi_gwfsat)
  allocate (this%fmi_gwfspdis)
  allocate (this%fmi_gwfflowja)
  allocate (this%mst_porosity)
  allocate (this%indsp)
  allocate (this%inmst)

end subroutine allocate_data

subroutine deallocate_data(this)
  class(VirtualGwtModelType) :: this

  deallocate (this%dsp_idiffc)
  deallocate (this%dsp_idisp)
  deallocate (this%dsp_diffc)
  deallocate (this%dsp_alh)
  deallocate (this%dsp_alv)
  deallocate (this%dsp_ath1)
  deallocate (this%dsp_ath2)
  deallocate (this%dsp_atv)
  deallocate (this%fmi_gwfhead)
  deallocate (this%fmi_gwfsat)
  deallocate (this%fmi_gwfspdis)
  deallocate (this%fmi_gwfflowja)
  deallocate (this%mst_porosity)
  deallocate (this%indsp)
  deallocate (this%inmst)
  
end subroutine deallocate_data

subroutine vgwt_destroy(this)
  class(VirtualGwtModelType) :: this
  
  call this%VirtualModelType%destroy()
  call this%deallocate_data()

end subroutine vgwt_destroy


end module VirtualGwtModelModule
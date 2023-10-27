! ** Do Not Modify! MODFLOW 6 system generated file. **
module IdmGwtDfnSelectorModule

  use ConstantsModule, only: LENVARNAME
  use SimModule, only: store_error
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use GwtDisInputModule
  use GwtDisuInputModule
  use GwtDisvInputModule
  use GwtDspInputModule
  use GwtCncInputModule
  use GwtNamInputModule

  implicit none
  private
  public :: GwtParamFoundType
  public :: gwt_param_definitions
  public :: gwt_aggregate_definitions
  public :: gwt_block_definitions
  public :: gwt_idm_multi_package
  public :: gwt_idm_sfac_param
  public :: gwt_idm_integrated

  type GwtParamFoundType
    logical :: length_units = .false.
    logical :: nogrb = .false.
    logical :: xorigin = .false.
    logical :: yorigin = .false.
    logical :: angrot = .false.
    logical :: nlay = .false.
    logical :: nrow = .false.
    logical :: ncol = .false.
    logical :: delr = .false.
    logical :: delc = .false.
    logical :: top = .false.
    logical :: botm = .false.
    logical :: idomain = .false.
    logical :: voffsettol = .false.
    logical :: nodes = .false.
    logical :: nja = .false.
    logical :: nvert = .false.
    logical :: bot = .false.
    logical :: area = .false.
    logical :: iac = .false.
    logical :: ja = .false.
    logical :: ihc = .false.
    logical :: cl12 = .false.
    logical :: hwva = .false.
    logical :: angldegx = .false.
    logical :: iv = .false.
    logical :: xv = .false.
    logical :: yv = .false.
    logical :: icell2d = .false.
    logical :: xc = .false.
    logical :: yc = .false.
    logical :: ncvert = .false.
    logical :: icvert = .false.
    logical :: ncpl = .false.
    logical :: xt3d_off = .false.
    logical :: xt3d_rhs = .false.
    logical :: diffc = .false.
    logical :: alh = .false.
    logical :: alv = .false.
    logical :: ath1 = .false.
    logical :: ath2 = .false.
    logical :: atv = .false.
    logical :: auxiliary = .false.
    logical :: auxmultname = .false.
    logical :: boundnames = .false.
    logical :: iprflow = .false.
    logical :: ipakcb = .false.
    logical :: iprpak = .false.
    logical :: ts_filerecord = .false.
    logical :: ts6 = .false.
    logical :: filein = .false.
    logical :: ts6_filename = .false.
    logical :: obs_filerecord = .false.
    logical :: obs6 = .false.
    logical :: obs6_filename = .false.
    logical :: maxbound = .false.
    logical :: cellid = .false.
    logical :: tspvar = .false.
    logical :: auxvar = .false.
    logical :: boundname = .false.
    logical :: list = .false.
    logical :: print_input = .false.
    logical :: print_flows = .false.
    logical :: save_flows = .false.
    logical :: ftype = .false.
    logical :: fname = .false.
    logical :: pname = .false.
  end type GwtParamFoundType

contains

  subroutine set_param_pointer(input_dfn, input_dfn_target)
    type(InputParamDefinitionType), dimension(:), pointer :: input_dfn
    type(InputParamDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine set_param_pointer

  subroutine set_block_pointer(input_dfn, input_dfn_target)
    type(InputBlockDefinitionType), dimension(:), pointer :: input_dfn
    type(InputBlockDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine set_block_pointer

  function gwt_param_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('DIS')
      call set_param_pointer(input_definition, gwt_dis_param_definitions)
    case ('DISU')
      call set_param_pointer(input_definition, gwt_disu_param_definitions)
    case ('DISV')
      call set_param_pointer(input_definition, gwt_disv_param_definitions)
    case ('DSP')
      call set_param_pointer(input_definition, gwt_dsp_param_definitions)
    case ('CNC')
      call set_param_pointer(input_definition, gwt_cnc_param_definitions)
    case ('NAM')
      call set_param_pointer(input_definition, gwt_nam_param_definitions)
    case default
    end select
    return
  end function gwt_param_definitions

  function gwt_aggregate_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('DIS')
      call set_param_pointer(input_definition, gwt_dis_aggregate_definitions)
    case ('DISU')
      call set_param_pointer(input_definition, gwt_disu_aggregate_definitions)
    case ('DISV')
      call set_param_pointer(input_definition, gwt_disv_aggregate_definitions)
    case ('DSP')
      call set_param_pointer(input_definition, gwt_dsp_aggregate_definitions)
    case ('CNC')
      call set_param_pointer(input_definition, gwt_cnc_aggregate_definitions)
    case ('NAM')
      call set_param_pointer(input_definition, gwt_nam_aggregate_definitions)
    case default
    end select
    return
  end function gwt_aggregate_definitions

  function gwt_block_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('DIS')
      call set_block_pointer(input_definition, gwt_dis_block_definitions)
    case ('DISU')
      call set_block_pointer(input_definition, gwt_disu_block_definitions)
    case ('DISV')
      call set_block_pointer(input_definition, gwt_disv_block_definitions)
    case ('DSP')
      call set_block_pointer(input_definition, gwt_dsp_block_definitions)
    case ('CNC')
      call set_block_pointer(input_definition, gwt_cnc_block_definitions)
    case ('NAM')
      call set_block_pointer(input_definition, gwt_nam_block_definitions)
    case default
    end select
    return
  end function gwt_block_definitions

  function gwt_idm_multi_package(subcomponent) result(multi_package)
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (subcomponent)
    case ('DIS')
      multi_package = gwt_dis_multi_package
    case ('DISU')
      multi_package = gwt_disu_multi_package
    case ('DISV')
      multi_package = gwt_disv_multi_package
    case ('DSP')
      multi_package = gwt_dsp_multi_package
    case ('CNC')
      multi_package = gwt_cnc_multi_package
    case ('NAM')
      multi_package = gwt_nam_multi_package
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="GWT"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function gwt_idm_multi_package

  function gwt_idm_sfac_param(subcomponent) result(sfac_param)
    character(len=*), intent(in) :: subcomponent
    character(len=LENVARNAME) :: sfac_param
    select case (subcomponent)
    case ('DIS')
      sfac_param = gwt_dis_aux_sfac_param
    case ('DISU')
      sfac_param = gwt_disu_aux_sfac_param
    case ('DISV')
      sfac_param = gwt_disv_aux_sfac_param
    case ('DSP')
      sfac_param = gwt_dsp_aux_sfac_param
    case ('CNC')
      sfac_param = gwt_cnc_aux_sfac_param
    case ('NAM')
      sfac_param = gwt_nam_aux_sfac_param
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="GWT"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function gwt_idm_sfac_param

  function gwt_idm_integrated(subcomponent) result(integrated)
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (subcomponent)
    case ('DIS')
      integrated = .true.
    case ('DISU')
      integrated = .true.
    case ('DISV')
      integrated = .true.
    case ('DSP')
      integrated = .true.
    case ('CNC')
      integrated = .true.
    case ('NAM')
      integrated = .true.
    case default
    end select
    return
  end function gwt_idm_integrated

end module IdmGwtDfnSelectorModule

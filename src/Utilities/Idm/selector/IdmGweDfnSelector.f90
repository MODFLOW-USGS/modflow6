! ** Do Not Modify! MODFLOW 6 system generated file. **
module IdmGweDfnSelectorModule

  use ConstantsModule, only: LENVARNAME
  use SimModule, only: store_error
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use GweDisInputModule
  use GweDisuInputModule
  use GweDisvInputModule
  use GweDspInputModule
  use GweCtpInputModule
  use GweIcInputModule
  use GweNamInputModule

  implicit none
  private
  public :: gwe_param_definitions
  public :: gwe_aggregate_definitions
  public :: gwe_block_definitions
  public :: gwe_idm_multi_package
  public :: gwe_idm_integrated

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

  function gwe_param_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('DIS')
      call set_param_pointer(input_definition, gwe_dis_param_definitions)
    case ('DISU')
      call set_param_pointer(input_definition, gwe_disu_param_definitions)
    case ('DISV')
      call set_param_pointer(input_definition, gwe_disv_param_definitions)
    case ('DSP')
      call set_param_pointer(input_definition, gwe_dsp_param_definitions)
    case ('CTP')
      call set_param_pointer(input_definition, gwe_ctp_param_definitions)
    case ('IC')
      call set_param_pointer(input_definition, gwe_ic_param_definitions)
    case ('NAM')
      call set_param_pointer(input_definition, gwe_nam_param_definitions)
    case default
    end select
    return
  end function gwe_param_definitions

  function gwe_aggregate_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('DIS')
      call set_param_pointer(input_definition, gwe_dis_aggregate_definitions)
    case ('DISU')
      call set_param_pointer(input_definition, gwe_disu_aggregate_definitions)
    case ('DISV')
      call set_param_pointer(input_definition, gwe_disv_aggregate_definitions)
    case ('DSP')
      call set_param_pointer(input_definition, gwe_dsp_aggregate_definitions)
    case ('CTP')
      call set_param_pointer(input_definition, gwe_ctp_aggregate_definitions)
    case ('IC')
      call set_param_pointer(input_definition, gwe_ic_aggregate_definitions)
    case ('NAM')
      call set_param_pointer(input_definition, gwe_nam_aggregate_definitions)
    case default
    end select
    return
  end function gwe_aggregate_definitions

  function gwe_block_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('DIS')
      call set_block_pointer(input_definition, gwe_dis_block_definitions)
    case ('DISU')
      call set_block_pointer(input_definition, gwe_disu_block_definitions)
    case ('DISV')
      call set_block_pointer(input_definition, gwe_disv_block_definitions)
    case ('DSP')
      call set_block_pointer(input_definition, gwe_dsp_block_definitions)
    case ('CTP')
      call set_block_pointer(input_definition, gwe_ctp_block_definitions)
    case ('IC')
      call set_block_pointer(input_definition, gwe_ic_block_definitions)
    case ('NAM')
      call set_block_pointer(input_definition, gwe_nam_block_definitions)
    case default
    end select
    return
  end function gwe_block_definitions

  function gwe_idm_multi_package(subcomponent) result(multi_package)
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (subcomponent)
    case ('DIS')
      multi_package = gwe_dis_multi_package
    case ('DISU')
      multi_package = gwe_disu_multi_package
    case ('DISV')
      multi_package = gwe_disv_multi_package
    case ('DSP')
      multi_package = gwe_dsp_multi_package
    case ('CTP')
      multi_package = gwe_ctp_multi_package
    case ('IC')
      multi_package = gwe_ic_multi_package
    case ('NAM')
      multi_package = gwe_nam_multi_package
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="GWE"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function gwe_idm_multi_package

  function gwe_idm_integrated(subcomponent) result(integrated)
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
    case ('CTP')
      integrated = .true.
    case ('IC')
      integrated = .true.
    case ('NAM')
      integrated = .true.
    case default
    end select
    return
  end function gwe_idm_integrated

end module IdmGweDfnSelectorModule

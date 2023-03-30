! ** Do Not Modify! MODFLOW 6 system generated file. **
module IdmGwtDfnSelectorModule

  use SimModule, only: store_error
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use GwtDspInputModule, only: gwt_dsp_param_definitions, &
                               gwt_dsp_aggregate_definitions, &
                               gwt_dsp_block_definitions, &
                               gwt_dsp_multi_package

  implicit none
  private
  public :: gwt_param_definitions
  public :: gwt_aggregate_definitions
  public :: gwt_block_definitions
  public :: gwt_idm_multi_package
  public :: gwt_idm_integrated

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
    case ('DSP')
      call set_param_pointer(input_definition, gwt_dsp_param_definitions)
    case default
    end select
    return
  end function gwt_param_definitions

  function gwt_aggregate_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('DSP')
      call set_param_pointer(input_definition, gwt_dsp_aggregate_definitions)
    case default
    end select
    return
  end function gwt_aggregate_definitions

  function gwt_block_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('DSP')
      call set_block_pointer(input_definition, gwt_dsp_block_definitions)
    case default
    end select
    return
  end function gwt_block_definitions

  function gwt_idm_multi_package(subcomponent) result(multi_package)
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (subcomponent)
    case ('DSP')
      multi_package = gwt_dsp_multi_package
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="GWT"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function gwt_idm_multi_package

  function gwt_idm_integrated(subcomponent) result(integrated)
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (subcomponent)
    case ('DSP')
      integrated = .true.
    case default
    end select
    return
  end function gwt_idm_integrated

end module IdmGwtDfnSelectorModule

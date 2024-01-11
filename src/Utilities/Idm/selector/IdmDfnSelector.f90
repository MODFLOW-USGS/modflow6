! ** Do Not Modify! MODFLOW 6 system generated file. **
module IdmDfnSelectorModule

  use ConstantsModule, only: LENVARNAME
  use SimModule, only: store_error
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use IdmGwfDfnSelectorModule
  use IdmGwtDfnSelectorModule
  use IdmExgDfnSelectorModule
  use IdmSimDfnSelectorModule

  implicit none
  private
  public :: param_definitions
  public :: aggregate_definitions
  public :: block_definitions
  public :: idm_multi_package
  public :: idm_integrated
  public :: idm_component

contains

  function param_definitions(component, subcomponent) result(input_definition)
    character(len=*), intent(in) :: component
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (component)
    case ('GWF')
      input_definition => gwf_param_definitions(subcomponent)
    case ('GWT')
      input_definition => gwt_param_definitions(subcomponent)
    case ('EXG')
      input_definition => exg_param_definitions(subcomponent)
    case ('SIM')
      input_definition => sim_param_definitions(subcomponent)
    case default
    end select
    return
  end function param_definitions

  function aggregate_definitions(component, subcomponent) result(input_definition)
    character(len=*), intent(in) :: component
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (component)
    case ('GWF')
      input_definition => gwf_aggregate_definitions(subcomponent)
    case ('GWT')
      input_definition => gwt_aggregate_definitions(subcomponent)
    case ('EXG')
      input_definition => exg_aggregate_definitions(subcomponent)
    case ('SIM')
      input_definition => sim_aggregate_definitions(subcomponent)
    case default
    end select
    return
  end function aggregate_definitions

  function block_definitions(component, subcomponent) result(input_definition)
    character(len=*), intent(in) :: component
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (component)
    case ('GWF')
      input_definition => gwf_block_definitions(subcomponent)
    case ('GWT')
      input_definition => gwt_block_definitions(subcomponent)
    case ('EXG')
      input_definition => exg_block_definitions(subcomponent)
    case ('SIM')
      input_definition => sim_block_definitions(subcomponent)
    case default
    end select
    return
  end function block_definitions

  function idm_multi_package(component, subcomponent) result(multi_package)
    character(len=*), intent(in) :: component
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (component)
    case ('GWF')
      multi_package = gwf_idm_multi_package(subcomponent)
    case ('GWT')
      multi_package = gwt_idm_multi_package(subcomponent)
    case ('EXG')
      multi_package = exg_idm_multi_package(subcomponent)
    case ('SIM')
      multi_package = sim_idm_multi_package(subcomponent)
    case default
      call store_error('Idm selector component not found; '//&
                       &'component="'//trim(component)//&
                       &'", subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function idm_multi_package

  function idm_integrated(component, subcomponent) result(integrated)
    character(len=*), intent(in) :: component
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (component)
    case ('GWF')
      integrated = gwf_idm_integrated(subcomponent)
    case ('GWT')
      integrated = gwt_idm_integrated(subcomponent)
    case ('EXG')
      integrated = exg_idm_integrated(subcomponent)
    case ('SIM')
      integrated = sim_idm_integrated(subcomponent)
    case default
    end select
    return
  end function idm_integrated

  function idm_component(component) result(integrated)
    character(len=*), intent(in) :: component
    logical :: integrated
    integrated = .false.
    select case (component)
    case ('GWF')
      integrated = .true.
    case ('GWT')
      integrated = .true.
    case ('EXG')
      integrated = .true.
    case ('SIM')
      integrated = .true.
    case default
    end select
    return
  end function idm_component

end module IdmDfnSelectorModule

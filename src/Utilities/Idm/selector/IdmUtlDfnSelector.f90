! ** Do Not Modify! MODFLOW 6 system generated file. **
module IdmUtlDfnSelectorModule

  use ConstantsModule, only: LENVARNAME
  use SimModule, only: store_error
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use UtlTvkInputModule
  use UtlTvsInputModule

  implicit none
  private
  public :: utl_param_definitions
  public :: utl_aggregate_definitions
  public :: utl_block_definitions
  public :: utl_idm_multi_package
  public :: utl_idm_integrated

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

  function utl_param_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('TVK')
      call set_param_pointer(input_definition, utl_tvk_param_definitions)
    case ('TVS')
      call set_param_pointer(input_definition, utl_tvs_param_definitions)
    case default
    end select
    return
  end function utl_param_definitions

  function utl_aggregate_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('TVK')
      call set_param_pointer(input_definition, utl_tvk_aggregate_definitions)
    case ('TVS')
      call set_param_pointer(input_definition, utl_tvs_aggregate_definitions)
    case default
    end select
    return
  end function utl_aggregate_definitions

  function utl_block_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('TVK')
      call set_block_pointer(input_definition, utl_tvk_block_definitions)
    case ('TVS')
      call set_block_pointer(input_definition, utl_tvs_block_definitions)
    case default
    end select
    return
  end function utl_block_definitions

  function utl_idm_multi_package(subcomponent) result(multi_package)
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (subcomponent)
    case ('TVK')
      multi_package = utl_tvk_multi_package
    case ('TVS')
      multi_package = utl_tvs_multi_package
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="UTL"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function utl_idm_multi_package

  function utl_idm_integrated(subcomponent) result(integrated)
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (subcomponent)
    case ('TVK')
      integrated = .true.
    case ('TVS')
      integrated = .true.
    case default
    end select
    return
  end function utl_idm_integrated

end module IdmUtlDfnSelectorModule

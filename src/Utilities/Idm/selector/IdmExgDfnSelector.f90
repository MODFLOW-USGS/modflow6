! ** Do Not Modify! MODFLOW 6 system generated file. **
module IdmExgDfnSelectorModule

  use ConstantsModule, only: LENVARNAME
  use SimModule, only: store_error
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use ExgGwfgwfInputModule
  use ExgGwfgwtInputModule
  use ExgGwtgwtInputModule

  implicit none
  private
  public :: exg_param_definitions
  public :: exg_aggregate_definitions
  public :: exg_block_definitions
  public :: exg_idm_multi_package
  public :: exg_idm_integrated

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

  function exg_param_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('GWFGWF')
      call set_param_pointer(input_definition, exg_gwfgwf_param_definitions)
    case ('GWFGWT')
      call set_param_pointer(input_definition, exg_gwfgwt_param_definitions)
    case ('GWTGWT')
      call set_param_pointer(input_definition, exg_gwtgwt_param_definitions)
    case default
    end select
    return
  end function exg_param_definitions

  function exg_aggregate_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('GWFGWF')
      call set_param_pointer(input_definition, exg_gwfgwf_aggregate_definitions)
    case ('GWFGWT')
      call set_param_pointer(input_definition, exg_gwfgwt_aggregate_definitions)
    case ('GWTGWT')
      call set_param_pointer(input_definition, exg_gwtgwt_aggregate_definitions)
    case default
    end select
    return
  end function exg_aggregate_definitions

  function exg_block_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('GWFGWF')
      call set_block_pointer(input_definition, exg_gwfgwf_block_definitions)
    case ('GWFGWT')
      call set_block_pointer(input_definition, exg_gwfgwt_block_definitions)
    case ('GWTGWT')
      call set_block_pointer(input_definition, exg_gwtgwt_block_definitions)
    case default
    end select
    return
  end function exg_block_definitions

  function exg_idm_multi_package(subcomponent) result(multi_package)
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (subcomponent)
    case ('GWFGWF')
      multi_package = exg_gwfgwf_multi_package
    case ('GWFGWT')
      multi_package = exg_gwfgwt_multi_package
    case ('GWTGWT')
      multi_package = exg_gwtgwt_multi_package
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="EXG"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function exg_idm_multi_package

  function exg_idm_integrated(subcomponent) result(integrated)
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (subcomponent)
    case ('GWFGWF')
      integrated = .true.
    case ('GWFGWT')
      integrated = .true.
    case ('GWTGWT')
      integrated = .true.
    case default
    end select
    return
  end function exg_idm_integrated

end module IdmExgDfnSelectorModule

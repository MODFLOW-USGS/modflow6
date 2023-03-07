!> @brief This module contains the ModflowInputModule
!!
!! This module contains a helper object and function
!! for accessing the ModflowInput, which is a
!! description of the structure of a modflow input
!! file.
!!
!<
module ModflowInputModule

  use KindModule, only: I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LENCOMPONENTNAME, &
                             LENPACKAGETYPE
  use MemoryHelperModule, only: create_mem_path
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use InputDefinitionSelectorModule, only: block_definitions, &
                                           aggregate_definitions, &
                                           param_definitions
  use SimVariablesModule, only: idm_context

  implicit none
  private
  public :: ModflowInputType, getModflowInput

  !> @brief derived type for storing input definition for a file
  !!
  !! This derived type contains the information needed to read
  !! a specific modflow input file, including block definitions,
  !! aggregate definitions (structarrays), and individual
  !! parameter definitions.
  !!
  !<
  type ModflowInputType
    character(len=LENPACKAGETYPE) :: pkgtype
    character(len=LENCOMPONENTNAME) :: component_type
    character(len=LENCOMPONENTNAME) :: subcomponent_type
    character(len=LENCOMPONENTNAME) :: component_name
    character(len=LENCOMPONENTNAME) :: subcomponent_name
    character(len=LENMEMPATH) :: mempath
    character(len=LENMEMPATH) :: component
    type(InputBlockDefinitionType), dimension(:), pointer :: p_block_dfns
    type(InputParamDefinitionType), dimension(:), pointer :: p_aggregate_dfns
    type(InputParamDefinitionType), dimension(:), pointer :: p_param_dfns
  end type ModflowInputType

contains

  !> @brief function to return ModflowInputType
  !<
  function getModflowInput(pkgtype, component_type, &
                           subcomponent_type, component_name, subcomponent_name) &
    result(mf6_input)
    character(len=*), intent(in) :: pkgtype !< package type to load, such as DIS6, DISV6, NPF6
    character(len=*), intent(in) :: component_type !< component type, such as GWF or GWT
    character(len=*), intent(in) :: subcomponent_type !< subcomponent type, such as DIS or NPF
    character(len=*), intent(in) :: component_name !< component name, such as MYGWFMODEL
    character(len=*), intent(in) :: subcomponent_name !< subcomponent name, such as MYWELLPACKAGE
    type(ModflowInputType) :: mf6_input

    mf6_input%pkgtype = trim(pkgtype)
    mf6_input%component_type = trim(component_type)
    mf6_input%subcomponent_type = trim(subcomponent_type)
    mf6_input%component_name = trim(component_name)
    mf6_input%subcomponent_name = trim(subcomponent_name)

    mf6_input%mempath = create_mem_path(component_name, subcomponent_name, &
                                           idm_context)
    mf6_input%component = trim(component_type)//'/'//trim(subcomponent_type)

    mf6_input%p_block_dfns => block_definitions(mf6_input%component)
    mf6_input%p_aggregate_dfns => aggregate_definitions(mf6_input%component)
    mf6_input%p_param_dfns => param_definitions(mf6_input%component)
  end function getModflowInput

end module ModflowInputModule

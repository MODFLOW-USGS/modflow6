module IdmTypesModule

  use KindModule, only: I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LENCOMPONENTNAME, LENMEMSEPARATOR, &
                             LENFTYPE, MAXCHARLEN
  use MemoryHelperModule, only: create_mem_path
  use InputDefinitionModule, only:  InputParamDefinitionType, &
                                    InputBlockDefinitionType
  use InputDefinitionSelectorModule, only: block_definitions, &
                                           aggregate_definitions, &
                                           param_definitions
  use SimVariablesModule, only: idm_mempath_prefix

  implicit none
  private
  public :: ModflowInputType, ModflowInput

  type ModflowInputType
    character(len=LENFTYPE) :: file_type
    character(len=MAXCHARLEN) :: file_spec
    character(len=LENCOMPONENTNAME) :: component_type
    character(len=LENCOMPONENTNAME) :: subcomponent_type
    character(len=LENCOMPONENTNAME) :: component_name
    character(len=LENCOMPONENTNAME) :: subcomponent_name
    character(len=LENMEMPATH) :: memoryPath
    character(len=LENMEMPATH) :: component
    type(InputBlockDefinitionType), dimension(:), pointer :: p_block_dfns
    type(InputParamDefinitionType), dimension(:), pointer :: p_aggregate_dfns
    type(InputParamDefinitionType), dimension(:), pointer :: p_param_dfns
  end type ModflowInputType

  contains

  function ModflowInput(ftype, fspec, component_type, &
    subcomponent_type, component_name, subcomponent_name) result(mf6_input)
    character(len=*), intent(in) :: ftype
    character(len=*), intent(in) :: fspec
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    type(ModflowInputType), pointer :: mf6_input

    allocate(mf6_input)
    mf6_input%file_type = trim(ftype)
    mf6_input%file_spec = trim(fspec)
    mf6_input%component_type = trim(component_type)
    mf6_input%subcomponent_type = trim(subcomponent_type)
    mf6_input%component_name = trim(component_name)
    mf6_input%subcomponent_name = trim(subcomponent_name)

    mf6_input%memoryPath = create_mem_path(component_name, subcomponent_name, idm_mempath_prefix)
    mf6_input%component = trim(component_type)//'/'//trim(subcomponent_type)

    mf6_input%p_block_dfns => block_definitions(mf6_input%component)
    mf6_input%p_aggregate_dfns => aggregate_definitions(mf6_input%component)
    mf6_input%p_param_dfns => param_definitions(mf6_input%component)
  end function ModflowInput

end module IdmTypesModule

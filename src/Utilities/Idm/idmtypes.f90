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
    !integer(I4B) :: inunit
    character(len=LENCOMPONENTNAME) :: component_type
    character(len=LENCOMPONENTNAME) :: subcomponent_type
    character(len=LENCOMPONENTNAME) :: component_name
    character(len=LENCOMPONENTNAME) :: subcomponent_name
    !integer(I4B), dimension(:) :: mshape
    character(len=LENMEMPATH) :: memoryPath
    character(len=LENMEMPATH) :: component
    type(InputBlockDefinitionType), dimension(:), pointer :: p_block_dfns
    type(InputParamDefinitionType), dimension(:), pointer :: p_aggregate_dfns
    type(InputParamDefinitionType), dimension(:), pointer :: p_param_dfns
  end type ModflowInputType

  contains

  function ModflowInput(ftype, fspec, component_type, &
    subcomponent_type, component_name, subcomponent_name) result(pModflowInput)
    !character(len=LENFTYPE) :: ftype
    !character(len=MAXCHARLEN) :: fspec
    !character(len=LENCOMPONENTNAME) :: component_type
    !character(len=LENCOMPONENTNAME) :: subcomponent_type
    !character(len=LENCOMPONENTNAME) :: component_name
    !character(len=LENCOMPONENTNAME) :: subcomponent_name
    character(len=*), intent(in) :: ftype
    character(len=*), intent(in) :: fspec
    !integer(I4B), intent(in) :: inunit
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    type(ModflowInputType), pointer :: pModflowInput

    allocate(pModflowInput)
    pModflowInput%file_type = trim(ftype)
    pModflowInput%file_spec = trim(fspec)
    !pModflowInput%inunit = inunit
    pModflowInput%component_type = trim(component_type)
    pModflowInput%subcomponent_type = trim(subcomponent_type)
    pModflowInput%component_name = trim(component_name)
    pModflowInput%subcomponent_name = trim(subcomponent_name)

    pModflowInput%memoryPath = create_mem_path(trim(idm_mempath_prefix)//trim(component_name), trim(subcomponent_name))
    pModflowInput%component = trim(component_type)//'/'//trim(subcomponent_type)

    pModflowInput%p_block_dfns => block_definitions(pModflowInput%component)
    pModflowInput%p_aggregate_dfns => aggregate_definitions(pModflowInput%component)
    pModflowInput%p_param_dfns => param_definitions(pModflowInput%component)
  end function ModflowInput

end module IdmTypesModule

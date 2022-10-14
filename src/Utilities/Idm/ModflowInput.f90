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
                             LENPACKAGETYPE, LENFTYPE
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
    character(len=LENFTYPE) :: file_type
    character(len=LENCOMPONENTNAME) :: component_type
    character(len=LENCOMPONENTNAME) :: subcomponent_type
    character(len=LENCOMPONENTNAME) :: component_name
    character(len=LENCOMPONENTNAME) :: subcomponent_name
    character(len=LENMEMPATH) :: memoryPath
    character(len=LENMEMPATH) :: component
    character(len=LENPACKAGETYPE), allocatable, dimension(:) :: subpackages
    type(InputBlockDefinitionType), dimension(:), pointer :: p_block_dfns
    type(InputParamDefinitionType), dimension(:), pointer :: p_aggregate_dfns
    type(InputParamDefinitionType), dimension(:), pointer :: p_param_dfns
  contains
    procedure :: destroy
  end type ModflowInputType

contains

  !> @brief function to return ModflowInputType
  !<
  function getModflowInput(ftype, component_type, &
                           subcomponent_type, component_name, subcomponent_name, &
                           subpackages) &
    result(mf6_input)
    character(len=*), intent(in) :: ftype !< file type to load, such as DIS6, DISV6, NPF6
    character(len=*), intent(in) :: component_type !< component type, such as GWF or GWT
    character(len=*), intent(in) :: subcomponent_type !< subcomponent type, such as DIS or NPF
    character(len=*), intent(in) :: component_name !< component name, such as MYGWFMODEL
    character(len=*), intent(in) :: subcomponent_name !< subcomponent name, such as MYWELLPACKAGE
    character(len=*), dimension(:), intent(in) :: subpackages !< array of subpackage types, such as ["TVK6", "OBS6"]
    type(ModflowInputType) :: mf6_input

    mf6_input%file_type = trim(ftype)
    mf6_input%component_type = trim(component_type)
    mf6_input%subcomponent_type = trim(subcomponent_type)
    mf6_input%component_name = trim(component_name)
    mf6_input%subcomponent_name = trim(subcomponent_name)
    allocate (mf6_input%subpackages(size(subpackages)))
    mf6_input%subpackages = subpackages

    mf6_input%memoryPath = create_mem_path(component_name, subcomponent_name, &
                                           idm_context)
    mf6_input%component = trim(component_type)//'/'//trim(subcomponent_type)

    mf6_input%p_block_dfns => block_definitions(mf6_input%component)
    mf6_input%p_aggregate_dfns => aggregate_definitions(mf6_input%component)
    mf6_input%p_param_dfns => param_definitions(mf6_input%component)
  end function getModflowInput

  !> @brief function to release ModflowInputType allocated memory
  !<
  subroutine destroy(this)
    class(ModflowInputType) :: this !< ModflowInputType

    if (allocated(this%subpackages)) then
      deallocate (this%subpackages)
    end if
  end subroutine destroy

end module ModflowInputModule

module InputDefinitionSelectorModule

  use KindModule, only: I4B
  use SimVariablesModule, only: errmsg, warnmsg
  use SimModule, only: store_error, store_warning
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use GwfDisInputModule, only: gwf_dis_param_definitions, &
                               gwf_dis_aggregate_definitions, &
                               gwf_dis_block_definitions

  implicit none
  private
  public :: block_definitions
  public :: aggregate_definitions
  public :: param_definitions
  public :: get_param_definition_type
  public :: get_aggregate_definition_type

  contains

  function param_definitions(component) result(input_definition)
    character(len=*), intent(in) :: component 
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition

    select case (component)
    case ('GWF/DIS')
      call set_pointer(input_definition, gwf_dis_param_definitions)
    case default
      write(warnmsg, '(a,a)') 'IDM Unsupported input type: ', trim(component)
      call store_warning(warnmsg)
    end select

    return
  end function param_definitions

  function aggregate_definitions(component) result(input_definition)
    character(len=*), intent(in) :: component
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition

    select case (component)
    case ('GWF/DIS')
      call set_pointer(input_definition, gwf_dis_aggregate_definitions)
    case default
      write(warnmsg, '(a,a)') 'IDM Unsupported input type: ', trim(component)
      call store_warning(warnmsg)
    end select

    return
  end function aggregate_definitions

  function block_definitions(component) result(input_definition)
    character(len=*), intent(in) :: component
    type(InputBlockDefinitionType), dimension(:), pointer :: input_definition

    select case (component)
    case ('GWF/DIS')
      call set_block_pointer(input_definition, gwf_dis_block_definitions)
    case default
      write(warnmsg, '(a,a)') 'IDM Unsupported input type: ', trim(component)
      call store_warning(warnmsg)
    end select

    return
  end function block_definitions
  
  subroutine set_pointer(input_definition, input_definition_target)
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    type(InputParamDefinitionType), dimension(:), target :: input_definition_target
    input_definition => input_definition_target
  end subroutine set_pointer

  subroutine set_block_pointer(input_definition, input_definition_target)
    type(InputBlockDefinitionType), dimension(:), pointer :: input_definition
    type(InputBlockDefinitionType), dimension(:), target :: input_definition_target
    input_definition => input_definition_target
  end subroutine set_block_pointer

  function get_param_definition_type(input_definition_types, &
      component_type, subcomponent_type, tagname) result (idt)
    type(InputParamDefinitionType), dimension(:), intent(in), target :: input_definition_types
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: tagname
    type(InputParamDefinitionType), pointer :: idt
    type(InputParamDefinitionType), pointer :: tmp_ptr
    integer(I4B) :: i

    idt => null()
    do i = 1, size(input_definition_types)
      tmp_ptr => input_definition_types(i)
      if (tmp_ptr%component_type == component_type .and. &
          tmp_ptr%subcomponent_type == subcomponent_type .and. &
          tmp_ptr%tagname == tagname) then
        idt => input_definition_types(i)
        exit
      end if
    end do

    if (.not. associated(idt)) then
      write(errmsg, '(4x,a,a)') 'parameter definition not found: ', trim(tagname)
      call store_error(errmsg)
    end if

  end function get_param_definition_type

  function get_aggregate_definition_type(input_definition_types, &
      component_type, subcomponent_type, blockname) result (idt)
    type(InputParamDefinitionType), dimension(:), intent(in), target :: input_definition_types
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: blockname
    type(InputParamDefinitionType), pointer :: idt
    type(InputParamDefinitionType), pointer :: tmp_ptr
    integer(I4B) :: i

    idt => null()
    do i = 1, size(input_definition_types)
      tmp_ptr => input_definition_types(i)
      if (tmp_ptr%component_type == component_type .and. &
          tmp_ptr%subcomponent_type == subcomponent_type .and. &
          tmp_ptr%blockname == blockname) then
        idt => input_definition_types(i)
        exit
      end if
    end do

    if (.not. associated(idt)) then
      write(errmsg, '(4x,a,a)') 'aggregate definition not found: ', trim(blockname)
      call store_error(errmsg)
    end if
  end function get_aggregate_definition_type

end module InputDefinitionSelectorModule

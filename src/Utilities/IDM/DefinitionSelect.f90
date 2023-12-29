!> @brief This module contains the DefinitionSelectModule
!!
!! This module contains the routines for getting parameter
!! definitions, aggregate definitions, and block definitions
!! for the different package types.
!!
!<
module DefinitionSelectModule

  use KindModule, only: I4B
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType

  implicit none
  private
  public :: get_param_definition_type
  public :: get_aggregate_definition_type
  public :: split_record_definition

contains

  !> @brief Return parameter definition
  !<
  function get_param_definition_type(input_definition_types, &
                                     component_type, subcomponent_type, &
                                     blockname, tagname, filename) &
    result(idt)
    type(InputParamDefinitionType), dimension(:), intent(in), target :: &
      input_definition_types
    character(len=*), intent(in) :: component_type !< component type, such as GWF or GWT
    character(len=*), intent(in) :: subcomponent_type !< subcomponent type, such as DIS or NPF
    character(len=*), intent(in) :: blockname !< name of the block
    character(len=*), intent(in) :: tagname !< name of the input tag
    character(len=*), intent(in) :: filename !< input filename
    type(InputParamDefinitionType), pointer :: idt !< corresponding InputParameterDefinitionType for this tag
    type(InputParamDefinitionType), pointer :: tmp_ptr
    integer(I4B) :: i
    !
    idt => null()
    do i = 1, size(input_definition_types)
      tmp_ptr => input_definition_types(i)
      if (tmp_ptr%component_type == component_type .and. &
          tmp_ptr%subcomponent_type == subcomponent_type .and. &
          tmp_ptr%blockname == blockname .and. &
          tmp_ptr%tagname == tagname) then
        idt => input_definition_types(i)
        exit
      end if
    end do
    !
    if (.not. associated(idt)) then
      write (errmsg, '(a,a,a,a,a)') &
        'Input file tag not found: "', trim(tagname), &
        '" in block "', trim(blockname), &
        '".'
      call store_error(errmsg)
      call store_error_filename(filename)
    end if
    !
    ! -- return
    return
  end function get_param_definition_type

  !> @brief Return aggregate definition
  !<
  function get_aggregate_definition_type(input_definition_types, component_type, &
                                         subcomponent_type, blockname) result(idt)
    type(InputParamDefinitionType), dimension(:), intent(in), target :: &
      input_definition_types
    character(len=*), intent(in) :: component_type !< component type, such as GWF or GWT
    character(len=*), intent(in) :: subcomponent_type !< subcomponent type, such as DIS or NPF
    character(len=*), intent(in) :: blockname !< name of the block
    type(InputParamDefinitionType), pointer :: idt !< corresponding InputParameterDefinitionType for this block
    type(InputParamDefinitionType), pointer :: tmp_ptr
    integer(I4B) :: i
    !
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
    !
    if (.not. associated(idt)) then
      write (errmsg, '(a,a,a,a,a,a,a)') &
        'Idm aggregate definition not found: ', trim(blockname), &
        '. Component="', trim(component_type), &
        '", subcomponent="', trim(subcomponent_type), '".'
      call store_error(errmsg, .true.)
    end if
    !
    ! -- return
    return
  end function get_aggregate_definition_type

  !> @brief Return aggregate definition
  !!
  !! Split a component RECORD datatype definition whose second element matches
  !! tagname into an array of character tokens
  !<
  subroutine split_record_definition(input_definition_types, component_type, &
                                     subcomponent_type, tagname, nwords, words)
    use InputOutputModule, only: parseline
    type(InputParamDefinitionType), dimension(:), intent(in), target :: &
      input_definition_types
    character(len=*), intent(in) :: component_type !< component type, such as GWF or GWT
    character(len=*), intent(in) :: subcomponent_type !< subcomponent type, such as DIS or NPF
    character(len=*), intent(in) :: tagname !< name of the input tag
    integer(I4B), intent(inout) :: nwords
    character(len=40), dimension(:), allocatable, intent(inout) :: words
    type(InputParamDefinitionType), pointer :: tmp_ptr
    integer(I4B) :: i
    character(len=:), allocatable :: parse_str
    !
    ! -- initialize to deallocated
    if (allocated(words)) deallocate (words)
    !
    ! -- return all tokens of multi-record type that matches the first
    ! -- tag following the expected first token "RECORD"
    do i = 1, size(input_definition_types)
      !
      ! -- initialize
      nwords = 0
      !
      ! -- set ptr to current definition
      tmp_ptr => input_definition_types(i)
      !
      ! -- match for definition to split
      if (tmp_ptr%component_type == component_type .and. &
          tmp_ptr%subcomponent_type == subcomponent_type .and. &
          tmp_ptr%datatype(1:6) == 'RECORD') then
        !
        ! -- set split string
        parse_str = trim(input_definition_types(i)%datatype)//' '
        !
        ! -- split
        call parseline(parse_str, nwords, words)
        !
        ! -- check for match and manage memory
        if (nwords >= 2) then
          if (words(1) == 'RECORD' .and. words(2) == tagname) then
            if (allocated(parse_str)) deallocate (parse_str)
            exit
          end if
        end if
        !
        ! -- deallocate
        if (allocated(parse_str)) deallocate (parse_str)
        if (allocated(words)) deallocate (words)
        !
      end if
    end do
  end subroutine split_record_definition

end module DefinitionSelectModule

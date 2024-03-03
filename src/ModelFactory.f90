module ModelFactoryModule
  use KindModule, only: I4B
  use ConstantsModule, only: LINELENGTH, LENMODELNAME
  use SimModule, only: store_error, check_model_name
  use SimVariablesModule, only: model_names, model_loc_idx, &
                                proc_id, simulation_mode
  use CharacterStringModule, only: CharacterStringType
  use ModelRegistrarModule
  ! TODO BEGIN TEMPLATING
  use GwfModule, only: register_gwf
  use VirtualGwfModelModule, only: register_virtual_gwf
  use GwtModule, only: register_gwt
  use VirtualGwtModelModule, only: register_virtual_gwt
  use GweModule, only: register_gwe
  use VirtualGweModelModule, only: register_virtual_gwe
  use PrtModule, only: register_prt
  use VirtualPrtModelModule, only: register_virtual_prt
  use SwfModule, only: register_swf
  use VirtualSwfModelModule, only: register_virtual_swf
  ! TODO END TEMPLATING

  implicit none
  private
  public :: create_models

contains

  !> Create models
  subroutine create_models( &
    types, &
    names, &
    files)
    ! dummy
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: types
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: names
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: files
    ! local
    integer(I4B) :: global_model_id
    integer(I4B) :: model_id
    character(len=LINELENGTH) :: model_type
    character(len=LINELENGTH) :: model_name
    character(len=LINELENGTH) :: model_file
    character(len=LINELENGTH) :: errmsg
    procedure(register_actual_model), pointer :: register_actual
    procedure(register_virtual_model), pointer :: register_virtual

    model_id = 0
    do global_model_id = 1, size(types)
      model_type = types(global_model_id)
      model_name = names(global_model_id)
      model_file = files(global_model_id)

      ! make sure model name is valid
      call check_model_name(model_type, model_name)

      ! increment global model id
      model_names(global_model_id) = model_name(1:LENMODELNAME)
      model_loc_idx(global_model_id) = -1

      ! pick the registration procedures for this model type
      select case (model_type)
        ! TODO BEGIN TEMPLATING
      case ('GWF6')
        register_actual => register_gwf
        register_virtual => register_virtual_gwf
      case ('GWT6')
        register_actual => register_gwt
        register_virtual => register_virtual_gwt
      case ('GWE6')
        register_actual => register_gwe
        register_virtual => register_virtual_gwe
      case ('PRT6')
        register_actual => register_prt
        register_virtual => register_virtual_prt
      case ('SWF6')
        register_actual => register_swf
        register_virtual => register_virtual_swf
        ! TODO END TEMPLATING
      case default
        write (errmsg, '(a,a)') &
          'Unknown simulation model type: ', trim(model_type)
        call store_error(errmsg, terminate=.true.)
      end select

      ! register the model
      call register_model( &
        register_actual, &
        register_virtual, &
        global_model_id, &
        model_id, &
        model_names(global_model_id), &
        model_type, &
        model_file)
    end do

    ! sanity check
    if (simulation_mode == 'PARALLEL' .and. model_id == 0) then
      write (errmsg, '(a, i0)') &
        'No MODELS assigned to process ', proc_id
      call store_error(errmsg, terminate=.true.)
    end if

  end subroutine

end module ModelFactoryModule

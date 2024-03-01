module ModelFactoryModule
  use KindModule, only: I4B
  use ConstantsModule, only: LINELENGTH, LENMODELNAME
  use SimModule, only: check_model_name, store_error
  use SimVariablesModule, only: model_names, model_loc_idx, &
                                proc_id, simulation_mode
  use CharacterStringModule, only: CharacterStringType
  use ModelRegistrarModule

  implicit none
  private
  public :: create_models

contains

  !> Create models
  subroutine create_models(mtypes, mfnames, mnames)
    ! -- modules
    ! START TEMPLATING
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
    ! END TEMPLATING
    ! -- dummy
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: mtypes !< model types
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: mfnames !< model file names
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: mnames !< model names
    ! -- local
    integer(I4B) :: n, im
    character(len=LINELENGTH) :: errmsg, model_name, model_type, fname
    procedure(register_actual_model), pointer :: ram
    procedure(register_virtual_model), pointer :: rvm

    im = 0
    do n = 1, size(mtypes)
      ! -- attributes for this model
      model_type = mtypes(n)
      fname = mfnames(n)
      model_name = mnames(n)

      ! make sure model name is valid
      call check_model_name(model_type, model_name)

      ! increment global model id
      model_names(n) = model_name(1:LENMODELNAME)
      model_loc_idx(n) = -1

      ! -- pick the registration procedures for this model type
      !    todo: hashmap not select case? (mtype -> register_*)
      select case (model_type)
        ! START TEMPLATING
      case ('GWF6')
        ram => register_gwf
        rvm => register_virtual_gwf
      case ('GWT6')
        ram => register_gwt
        rvm => register_virtual_gwt
      case ('GWE6')
        ram => register_gwe
        rvm => register_virtual_gwe
      case ('PRT6')
        ram => register_prt
        rvm => register_virtual_prt
      case ('SWF6')
        ram => register_swf
        rvm => register_virtual_swf
        ! END TEMPLATING
      case default
        write (errmsg, '(a,a)') &
          'Unknown simulation model type: ', trim(model_type)
        call store_error(errmsg, terminate=.true.)
      end select

      ! -- register the model
      call register_model(ram, rvm, n, im, model_names(n), model_type, fname)
    end do

    ! -- sanity check
    if (simulation_mode == 'PARALLEL' .and. im == 0) then
      write (errmsg, '(a, i0)') &
        'No MODELS assigned to process ', proc_id
      call store_error(errmsg, terminate=.true.)
    end if

  end subroutine

end module ModelFactoryModule

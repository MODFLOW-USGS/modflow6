module ModelFactoryModule
  use KindModule, only: I4B
  use ConstantsModule, only: LENMEMPATH, LINELENGTH, LENMODELNAME
  use ListsModule, only: basemodellist
  use SimModule, only: store_error, count_errors
  use SimVariablesModule, only: iout, model_names, model_ranks, model_loc_idx, &
                                idm_context, proc_id, nr_procs, simulation_mode
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: mem_setptr, mem_allocate
  use CharacterStringModule, only: CharacterStringType

  implicit none
  private
  public :: create_models

contains

  subroutine add_gwf_model(n, im, fname)
    ! -- modules
    use NumericalModelModule, only: NumericalModelType, &
                                    GetNumericalModelFromList
    use GwfModule, only: gwf_cr
    use VirtualGwfModelModule, only: add_virtual_gwf_model
    ! -- dummy
    integer(I4B), intent(in) :: n
    integer(I4B), intent(inout) :: im
    character(len=*), intent(in) :: fname
    ! -- local
    class(NumericalModelType), pointer :: model

    model => null() ! can be null for remote models
    if (model_ranks(n) == proc_id) then
      im = im + 1
      write (iout, '(4x,2a,i0,a)') 'GWF6', ' model ', &
        n, ' will be created'
      call gwf_cr(fname, n, model_names(n))
      model => GetNumericalModelFromList(basemodellist, im)
      model_loc_idx(n) = im
    end if
    call add_virtual_gwf_model(n, model_names(n), model)

  end subroutine add_gwf_model

  subroutine add_gwt_model(n, im, fname)
    ! -- modules
    use NumericalModelModule, only: NumericalModelType, &
                                    GetNumericalModelFromList
    use GwtModule, only: gwt_cr
    use VirtualGwtModelModule, only: add_virtual_gwt_model
    ! -- dummy
    integer(I4B), intent(in) :: n
    integer(I4B), intent(inout) :: im
    character(len=*), intent(in) :: fname
    ! -- local
    class(NumericalModelType), pointer :: model

    model => null() ! can be null for remote models
    if (model_ranks(n) == proc_id) then
      im = im + 1
      write (iout, '(4x,2a,i0,a)') 'GWT6', ' model ', &
        n, ' will be created'
      call gwt_cr(fname, n, model_names(n))
      model => GetNumericalModelFromList(basemodellist, im)
      model_loc_idx(n) = im
    end if
    call add_virtual_gwt_model(n, model_names(n), model)

  end subroutine add_gwt_model

  subroutine add_gwe_model(n, im, fname)
    ! -- modules
    use NumericalModelModule, only: NumericalModelType, &
                                    GetNumericalModelFromList
    use GweModule, only: gwe_cr
    use VirtualGweModelModule, only: add_virtual_gwe_model
    ! -- dummy
    integer(I4B), intent(in) :: n
    integer(I4B), intent(inout) :: im
    character(len=*), intent(in) :: fname
    ! -- local
    class(NumericalModelType), pointer :: model

    model => null() ! can be null for remote models
    if (model_ranks(n) == proc_id) then
      im = im + 1
      write (iout, '(4x,2a,i0,a)') 'GWE6', ' model ', &
        n, ' will be created'
      call gwe_cr(fname, n, model_names(n))
      model => GetNumericalModelFromList(basemodellist, im)
      model_loc_idx(n) = im
    end if
    call add_virtual_gwe_model(n, model_names(n), model)

  end subroutine add_gwe_model

  subroutine add_prt_model(n, im, fname)
    ! -- modules
    use NumericalModelModule, only: NumericalModelType, &
                                    GetNumericalModelFromList
    use PrtModule, only: prt_cr
    use VirtualPrtModelModule, only: add_virtual_prt_model
    ! -- dummy
    integer(I4B), intent(in) :: n
    integer(I4B), intent(inout) :: im
    character(len=*), intent(in) :: fname
    ! -- local
    class(NumericalModelType), pointer :: model

    model => null() ! can be null for remote models
    if (model_ranks(n) == proc_id) then
      im = im + 1
      write (iout, '(4x,2a,i0,a)') 'PRT6', ' model ', &
        n, ' will be created'
      call prt_cr(fname, n, model_names(n))
      model => GetNumericalModelFromList(basemodellist, im)
      model_loc_idx(n) = im
    end if
    call add_virtual_prt_model(n, model_names(n), model)

  end subroutine add_prt_model

  subroutine add_swf_model(n, im, fname)
    ! -- modules
    use NumericalModelModule, only: NumericalModelType, &
                                    GetNumericalModelFromList
    use SwfModule, only: swf_cr
    use VirtualSwfModelModule, only: add_virtual_swf_model
    ! -- dummy
    integer(I4B), intent(in) :: n
    integer(I4B), intent(inout) :: im
    character(len=*), intent(in) :: fname
    ! -- local
    class(NumericalModelType), pointer :: model

    model => null() ! can be null for remote models
    if (model_ranks(n) == proc_id) then
      im = im + 1
      write (iout, '(4x,2a,i0,a)') 'SWF6', ' model ', &
        n, ' will be created'
      call swf_cr(fname, n, model_names(n))
      model => GetNumericalModelFromList(basemodellist, im)
      model_loc_idx(n) = im
    end if
    call add_virtual_swf_model(n, model_names(n), model)

  end subroutine add_swf_model

  !> @brief Check that the model name is valid
  !<
  subroutine check_model_name(mtype, mname)
    ! -- dummy
    character(len=*), intent(in) :: mtype
    character(len=*), intent(inout) :: mname
    ! -- local
    integer :: ilen
    integer :: i
    character(len=LINELENGTH) :: errmsg
    logical :: terminate = .true.
    !
    ilen = len_trim(mname)
    if (ilen > LENMODELNAME) then
      write (errmsg, '(a,a)') 'Invalid model name: ', trim(mname)
      call store_error(errmsg)
      write (errmsg, '(a,i0,a,i0)') &
        'Name length of ', ilen, ' exceeds maximum length of ', &
        LENMODELNAME
      call store_error(errmsg, terminate)
    end if
    do i = 1, ilen
      if (mname(i:i) == ' ') then
        write (errmsg, '(a,a)') 'Invalid model name: ', trim(mname)
        call store_error(errmsg)
        write (errmsg, '(a)') &
          'Model name cannot have spaces within it.'
        call store_error(errmsg, terminate)
      end if
    end do

  end subroutine check_model_name

  subroutine create_models(mtypes, mfnames, mnames)
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

    im = 0
    do n = 1, size(mtypes)
      ! -- attributes for this model
      model_type = mtypes(n)
      fname = mfnames(n)
      model_name = mnames(n)

      call check_model_name(model_type, model_name)

      ! increment global model id
      model_names(n) = model_name(1:LENMODELNAME)
      model_loc_idx(n) = -1

      ! -- add a new (local or global) model
      select case (model_type)
      case ('GWF6')
        call add_gwf_model(n, im, fname)
      case ('GWT6')
        call add_gwt_model(n, im, fname)
      case ('GWE6')
        call add_gwe_model(n, im, fname)
      case ('PRT6')
        call add_prt_model(n, im, fname)
      case ('SWF6')
        call add_swf_model(n, im, fname)
      case default
        write (errmsg, '(a,a)') &
          'Unknown simulation model type: ', trim(model_type)
        call store_error(errmsg, terminate=.true.)
      end select
    end do

    ! -- sanity check
    if (simulation_mode == 'PARALLEL' .and. im == 0) then
      write (errmsg, '(a, i0)') &
        'No MODELS assigned to process ', proc_id
      call store_error(errmsg, terminate=.true.)
    end if

  end subroutine

end module ModelFactoryModule

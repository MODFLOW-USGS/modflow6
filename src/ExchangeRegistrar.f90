module ExchangeRegistrarModule
  use KindModule, only: I4B, LGP
  use ConstantsModule, only: LINELENGTH
  use ListsModule, only: basemodellist
  use SimVariablesModule, only: iout, model_names, model_loc_idx
  use ArrayHandlersModule, only: ifind
  use SimModule, only: store_error

  implicit none
  private
  public :: register_exchange
  public :: register_actual_exchange
  public :: register_virtual_exchange

  interface
    subroutine register_actual_exchange( &
      exchange_id, &
      exchange_name, &
      exchange_file, &
      exchange_mempath, &
      model1_id, &
      model2_id)
      import I4B
      integer(I4B), intent(in) :: exchange_id
      character(len=*), intent(in) :: exchange_name
      character(len=*), intent(in) :: exchange_file
      character(len=*), intent(in) :: exchange_mempath
      integer(I4B), intent(in) :: model1_id
      integer(I4B), intent(in) :: model2_id
    end subroutine
  end interface

  interface
    subroutine register_virtual_exchange( &
      exchange_id, &
      exchange_name, &
      model1_id, &
      model2_id)
      import I4B
      integer(I4B), intent(in) :: exchange_id
      character(len=*), intent(in) :: exchange_name
      integer(I4B), intent(in) :: model1_id
      integer(I4B), intent(in) :: model2_id
    end subroutine
  end interface

contains

  subroutine register_exchange( &
    register_actual, &
    register_virtual, &
    exchange_id, &
    exchange_name, &
    exchange_type, &
    exchange_file, &
    exchange_mempath, &
    model1_name, &
    model2_name)
    ! dummy
    procedure(register_actual_exchange), pointer, intent(in) :: register_actual
    procedure(register_virtual_exchange), pointer, intent(in) :: register_virtual
    integer(I4B), intent(in) :: exchange_id
    character(len=*), intent(in) :: exchange_name
    character(len=*), intent(in) :: exchange_type
    character(len=*), intent(in) :: exchange_file
    character(len=*), intent(in) :: exchange_mempath
    character(len=*), intent(in) :: model1_name
    character(len=*), intent(in) :: model2_name
    ! local
    integer(I4B) :: model1_id
    integer(I4B) :: model2_id
    logical(LGP) :: both_local
    logical(LGP) :: both_remote
    logical(LGP) :: same_type
    character(len=LINELENGTH) :: errmsg
    ! formats
    character(len=*), parameter :: fmtmerr = "('Error in simulation control ', &
      &'file.  Could not find model: ', a)"

    ! find model index in list
    model1_id = ifind(model_names, model1_name)
    if (model1_id < 0) then
      write (errmsg, fmtmerr) trim(model1_name)
      call store_error(errmsg, terminate=.true.)
    end if
    model2_id = ifind(model_names, model2_name)
    if (model2_id < 0) then
      write (errmsg, fmtmerr) trim(model2_name)
      call store_error(errmsg, terminate=.true.)
    end if

    ! both models on other process? then don't create it here...
    both_remote = (model_loc_idx(model1_id) == -1 .and. &
                   model_loc_idx(model2_id) == -1)
    both_local = (model_loc_idx(model1_id) > 0 .and. &
                  model_loc_idx(model2_id) > 0)
    if (.not. both_remote) write (iout, '(4x,a,a,i0,a,i0,a,i0)') &
      trim(exchange_type), ' exchange ', exchange_id, &
      ' will be created to connect model ', model1_id, &
      ' with model ', model2_id

    ! check if models are of the same type
    same_type = exchange_name(1:3) == exchange_name(5:7)

    ! an actual exchange should be registered if the models
    ! are the same type and at least one is local, or if the
    ! models are not the same type and both are local
    if ((same_type .and. .not. both_remote) .or. &
        (.not. same_type .and. both_local)) &
      call register_actual( &
      exchange_id, &
      exchange_name, &
      exchange_file, &
      exchange_mempath, &
      model1_id, &
      model2_id)

    ! if models are of the same type, register virtual exchange
    if (same_type) &
      call register_virtual( &
      exchange_id, &
      exchange_name, &
      model1_id, &
      model2_id)

  end subroutine register_exchange

end module ExchangeRegistrarModule

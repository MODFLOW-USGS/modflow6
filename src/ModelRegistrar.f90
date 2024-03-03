!> @brief This module exposes a single higher-order routine whose purpose is to register a model with
!! the simulation. The routine accepts procedure pointers to model-scoped registration routines. This
!! could be done in ModelFactory (indeed, ModelFactory maps model acronyms to registration routines),
!! but we do registration here to keep ModelFactory minimal since editing template files is annoying.
module ModelRegistrarModule
  use KindModule, only: I4B
  use ConstantsModule, only: LENMODELNAME
  use ListsModule, only: basemodellist
  use SimVariablesModule, only: iout, proc_id, model_names, model_ranks, &
                                model_loc_idx, proc_id
  use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList

  implicit none
  private
  public :: register_model
  public :: register_actual_model
  public :: register_virtual_model

  interface
    subroutine register_actual_model(model_id, model_name, model_file)
      import I4B
      integer(I4B), intent(in) :: model_id
      character(len=*), intent(in) :: model_name
      character(len=*), intent(in) :: model_file
    end subroutine
  end interface

  interface
    subroutine register_virtual_model(model_id, model_name, model)
      import I4B
      import NumericalModelType
      integer(I4B), intent(in) :: model_id
      character(len=*), intent(in) :: model_name
      class(NumericalModelType), pointer, intent(inout) :: model
    end subroutine
  end interface

contains

  !> @brief Register a model with the simulation. This entails registering
  !! the actual model, as well as a virtual model if it supports parallel.
  subroutine register_model( &
    register_actual, &
    register_virtual, &
    global_model_id, &
    model_id, &
    model_name, &
    model_type, &
    model_file)
    ! -- dummy
    procedure(register_actual_model), pointer, intent(in) :: register_actual
    procedure(register_virtual_model), pointer, intent(in) :: register_virtual
    integer(I4B), intent(in) :: global_model_id
    integer(I4B), intent(inout) :: model_id
    character(len=*), intent(in) :: model_name
    character(len=*), intent(in) :: model_type
    character(len=*), intent(in) :: model_file
    ! -- local
    class(NumericalModelType), pointer :: model

    model => null() ! can be null for remote models
    if (model_ranks(global_model_id) == proc_id) then
      model_id = model_id + 1
      write (iout, '(4x,2a,i0,a)') trim(model_type), ' model ', &
        global_model_id, ' will be created'
      call register_actual(global_model_id, model_name, model_file)
      model => GetNumericalModelFromList(basemodellist, model_id)
      model_loc_idx(global_model_id) = model_id
    end if

    ! proc pointer will be null if the model doesn't support parallel
    if (associated(register_virtual)) &
      call register_virtual(global_model_id, model_name, model)

  end subroutine register_model

end module ModelRegistrarModule

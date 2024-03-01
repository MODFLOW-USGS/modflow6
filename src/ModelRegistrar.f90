!> @brief This module exposes a single higher-order routine whose purpose is to register a model with
!! the simulation. The routine accepts procedure pointers to model-scoped registration routines. This
!! could be done in ModelFactory (indeed, ModelFactory maps model acronyms to registration routines),
!! but we do registration here to keep ModelFactory minimal since editing template files is annoying.
module ModelRegistrarModule
  use KindModule, only: I4B
  use ListsModule, only: basemodellist
  use SimVariablesModule, only: iout, model_ranks, model_loc_idx, proc_id
  use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList

  implicit none
  private
  public :: register_model, register_actual_model, register_virtual_model

  interface
    subroutine register_actual_model(id, name, filename)
      import I4B
      integer(I4B), intent(in) :: id !< model id (listed in mfsim.nam)
      character(len=*), intent(in) :: name !< model name
      character(len=*), intent(in) :: filename !< input file
    end subroutine
  end interface

  interface
    subroutine register_virtual_model(id, name, model)
      import I4B
      import NumericalModelType
      integer(I4B), intent(in) :: id !< model id (listed in mfsim.nam)
      character(len=*), intent(in) :: name !< model name
      class(NumericalModelType), pointer, intent(inout) :: model !< the actual model (can be null() when remote)
    end subroutine
  end interface

contains

  !> @brief Register a model with the simulation. This entails registering
  !! the actual model, as well as a virtual model if it supports parallel.
  subroutine register_model(ram, rvm, n, id, mname, mtype, filename)
    ! -- dummy
    procedure(register_actual_model), pointer, intent(in) :: ram
    procedure(register_virtual_model), pointer, intent(in) :: rvm
    integer(I4B), intent(in) :: n
    integer(I4B), intent(inout) :: id !< model id (listed in mfsim.nam)
    character(len=*), intent(in) :: mname !< model name
    character(len=*), intent(in) :: mtype !< model type
    character(len=*), intent(in) :: filename !< input file
    ! -- local
    class(NumericalModelType), pointer :: model

    model => null() ! can be null for remote models
    if (model_ranks(n) == proc_id) then
      id = id + 1
      write (iout, '(4x,2a,i0,a)') trim(mtype), ' model ', &
        n, ' will be created'
      call ram(n, mname, filename)
      model => GetNumericalModelFromList(basemodellist, id)
      model_loc_idx(n) = id
    end if

    ! rvm will be null if the model doesn't support parallel
    if (associated(rvm)) call rvm(n, mname, model)

  end subroutine register_model

end module ModelRegistrarModule

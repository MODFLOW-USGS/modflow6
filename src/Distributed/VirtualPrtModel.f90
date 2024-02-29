module VirtualPrtModelModule
  use KindModule, only: I4B
  use SimStagesModule
  use VirtualBaseModule
  use VirtualModelModule
  use NumericalModelModule, only: NumericalModelType
  implicit none
  private

  public :: add_virtual_prt_model

  type, extends(VirtualModelType) :: VirtualPrtModelType
  contains
    ! public
    procedure :: create => vprt_create
    procedure :: prepare_stage => vprt_prepare_stage
    procedure :: destroy => vprt_destroy
    ! private
    procedure, private :: init_virtual_data
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
  end type VirtualPrtModelType

contains

  subroutine add_virtual_prt_model(model_id, model_name, model)
    integer(I4B) :: model_id !< global model id
    character(len=*) :: model_name !< model name
    class(NumericalModelType), pointer :: model !< the actual model (can be null() when remote)
    ! noop
  end subroutine add_virtual_prt_model

  subroutine vprt_create(this, name, id, model)
    class(VirtualPrtModelType) :: this
    character(len=*) :: name
    integer(I4B) :: id
    class(NumericalModelType), pointer :: model
    ! noop
  end subroutine vprt_create

  subroutine init_virtual_data(this)
    class(VirtualPrtModelType) :: this
    ! noop
  end subroutine init_virtual_data

  subroutine vprt_prepare_stage(this, stage)
    class(VirtualPrtModelType) :: this
    integer(I4B) :: stage
    ! noop
  end subroutine vprt_prepare_stage

  subroutine allocate_data(this)
    class(VirtualPrtModelType) :: this
    ! noop
  end subroutine allocate_data

  subroutine deallocate_data(this)
    class(VirtualPrtModelType) :: this
    ! noop
  end subroutine deallocate_data

  subroutine vprt_destroy(this)
    class(VirtualPrtModelType) :: this
    ! noop
  end subroutine vprt_destroy

end module VirtualPrtModelModule
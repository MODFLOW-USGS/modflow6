module VirtualSwfModelModule
  use KindModule, only: I4B
  use SimStagesModule
  use VirtualBaseModule
  use VirtualModelModule
  use NumericalModelModule, only: NumericalModelType
  implicit none
  private

  public :: add_virtual_swf_model

  type, extends(VirtualModelType) :: VirtualSwfModelType
  contains
    ! public
    procedure :: create => vswf_create
    procedure :: prepare_stage => vswf_prepare_stage
    procedure :: destroy => vswf_destroy
    ! private
    procedure, private :: init_virtual_data
    procedure, private :: allocate_data
    procedure, private :: deallocate_data
  end type VirtualSwfModelType

contains

  subroutine add_virtual_swf_model(model_id, model_name, model)
    integer(I4B) :: model_id !< global model id
    character(len=*) :: model_name !< model name
    class(NumericalModelType), pointer :: model !< the actual model (can be null() when remote)
    ! noop
  end subroutine add_virtual_swf_model

  subroutine vswf_create(this, name, id, model)
    class(VirtualSwfModelType) :: this
    character(len=*) :: name
    integer(I4B) :: id
    class(NumericalModelType), pointer :: model
    ! noop
  end subroutine vswf_create

  subroutine init_virtual_data(this)
    class(VirtualSwfModelType) :: this
    ! noop
  end subroutine init_virtual_data

  subroutine vswf_prepare_stage(this, stage)
    class(VirtualSwfModelType) :: this
    integer(I4B) :: stage
    ! noop
  end subroutine vswf_prepare_stage

  subroutine allocate_data(this)
    class(VirtualSwfModelType) :: this
    ! noop
  end subroutine allocate_data

  subroutine deallocate_data(this)
    class(VirtualSwfModelType) :: this
    ! noop
  end subroutine deallocate_data

  subroutine vswf_destroy(this)
    class(VirtualSwfModelType) :: this
    ! noop
  end subroutine vswf_destroy

end module VirtualSwfModelModule
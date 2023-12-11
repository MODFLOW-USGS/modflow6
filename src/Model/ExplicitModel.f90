!> @brief Explicit Model Module
!!
!! This module contains the Explicit Model, which is a parent
!! class for models that solve themselves.  Explicit models are
!! added to an Explicit Solution, which is simply a container
!! that scrolls through a list of explicit models and calls
!! methods in a prescribed sequence.
!!
!<
module ExplicitModelModule

  use KindModule, only: I4B
  use ListModule, only: ListType
  use BaseModelModule, only: BaseModelType
  use BaseDisModule, only: DisBaseType

  implicit none
  private
  public :: ExplicitModelType, &
            CastAsExplicitModelClass, &
            AddExplicitModelToList, &
            GetExplicitModelFromList

  !> @brief Base type for explicit models.
  type, extends(BaseModelType) :: ExplicitModelType
    type(ListType), pointer :: bndlist => null() !< array of boundary packages for this model
    class(DisBaseType), pointer :: dis => null() !< discretization object
  contains
    procedure :: model_ad
    procedure :: model_solve
    procedure :: model_cq
    procedure :: model_bd
    procedure :: model_da
    procedure :: allocate_scalars
  end type ExplicitModelType

contains

  !> @ brief Advance the model
  !<
  subroutine model_ad(this)
    class(ExplicitModelType) :: this
  end subroutine model_ad

  !> @ brief Solve the model
  !<
  subroutine model_solve(this)
    class(ExplicitModelType) :: this
  end subroutine model_solve

  !> @ brief Calculate model flows
  !<
  subroutine model_cq(this, icnvg, isuppress_output)
    class(ExplicitModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
  end subroutine model_cq

  !> @ brief Calculate model budget
  !<
  subroutine model_bd(this, icnvg, isuppress_output)
    class(ExplicitModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
  end subroutine model_bd

  !> @ brief Deallocate the model
  !<
  subroutine model_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    class(ExplicitModelType) :: this
    !
    ! -- derived types
    call this%bndlist%Clear()
    deallocate (this%bndlist)
    !
    ! -- BaseModelType
    call this%BaseModelType%model_da()
  end subroutine model_da

  !> @ brief Allocate model scalar variables
  !<
  subroutine allocate_scalars(this, modelname)
    use MemoryManagerModule, only: mem_allocate
    class(ExplicitModelType) :: this
    character(len=*), intent(in) :: modelname
    !
    ! -- allocate basetype members
    call this%BaseModelType%allocate_scalars(modelname)
    !
    ! -- allocate members from this type
    allocate (this%bndlist)
  end subroutine allocate_scalars

  !> @ brief Cast a generic object into an explicit model
  !<
  function CastAsExplicitModelClass(obj) result(res)
    class(*), pointer, intent(inout) :: obj
    class(ExplicitModelType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (ExplicitModelType)
      res => obj
    end select
  end function CastAsExplicitModelClass

  !> @ brief Add explicit model to a generic list
  !<
  subroutine AddExplicitModelToList(list, model)
    ! -- dummy
    type(ListType), intent(inout) :: list
    class(ExplicitModelType), pointer, intent(inout) :: model
    ! -- local
    class(*), pointer :: obj
    !
    obj => model
    call list%Add(obj)
  end subroutine AddExplicitModelToList

  !> @ brief Get generic object from list and return as explicit model
  !<
  function GetExplicitModelFromList(list, idx) result(res)
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: idx
    class(ExplicitModelType), pointer :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsExplicitModelClass(obj)
  end function GetExplicitModelFromList

end module ExplicitModelModule

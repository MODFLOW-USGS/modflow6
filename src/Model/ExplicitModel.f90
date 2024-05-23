!> @brief Models that solve themselves
module ExplicitModelModule

  use KindModule, only: I4B, DP
  use ConstantsModule, only: LINELENGTH
  use ListModule, only: ListType
  use BaseModelModule, only: BaseModelType
  use BaseDisModule, only: DisBaseType
  use MemoryManagerModule, only: mem_allocate, mem_deallocate

  implicit none
  private
  public :: ExplicitModelType, &
            CastAsExplicitModelClass, &
            AddExplicitModelToList, &
            GetExplicitModelFromList

  !> @brief Base type for models that solve themselves.
  !!
  !! An explicit solution simply scrolls through a list of explicit
  !! models and calls solution procedures in a prescribed sequence.
  !<
  type, extends(BaseModelType) :: ExplicitModelType
    character(len=LINELENGTH), pointer :: filename => null() !< input file name
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null() !< ibound
    type(ListType), pointer :: bndlist => null() !< array of boundary packages
    class(DisBaseType), pointer :: dis => null() !< discretization object
  contains
    ! -- Overridden methods
    procedure :: model_ad
    procedure :: model_solve
    procedure :: model_cq
    procedure :: model_bd
    procedure :: model_da
    ! -- Utility methods
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: set_idsoln
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
    class(ExplicitModelType) :: this

    ! -- deallocate scalars
    deallocate (this%filename)

    ! -- deallocate arrays
    call mem_deallocate(this%ibound)

    ! -- nullify pointers
    if (associated(this%ibound)) &
      call mem_deallocate(this%ibound, 'IBOUND', this%memoryPath)

    ! -- member derived types
    call this%bndlist%Clear()
    deallocate (this%bndlist)

    ! -- deallocate base type
    call this%BaseModelType%model_da()
  end subroutine model_da

  !> @ brief Allocate scalar variables
  !<
  subroutine allocate_scalars(this, modelname)
    class(ExplicitModelType) :: this
    character(len=*), intent(in) :: modelname

    call this%BaseModelType%allocate_scalars(modelname)
    allocate (this%bndlist)
    allocate (this%filename)
    this%filename = ''
  end subroutine allocate_scalars

  !> @brief Allocate array variables
  !<
  subroutine allocate_arrays(this)
    class(ExplicitModelType) :: this
    integer(I4B) :: i

    call mem_allocate(this%ibound, this%dis%nodes, 'IBOUND', this%memoryPath)
    do i = 1, this%dis%nodes
      this%ibound(i) = 1 ! active by default
    end do
  end subroutine allocate_arrays

  !> @ brief Cast a generic object into an explicit model
  !<
  function CastAsExplicitModelClass(obj) result(res)
    class(*), pointer, intent(inout) :: obj
    class(ExplicitModelType), pointer :: res

    res => null()
    if (.not. associated(obj)) return

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

    obj => list%GetItem(idx)
    res => CastAsExplicitModelClass(obj)
  end function GetExplicitModelFromList

  !> @brief Set the solution ID
  !<
  subroutine set_idsoln(this, id)
    class(ExplicitModelType) :: this
    integer(I4B), intent(in) :: id
    this%idsoln = id
  end subroutine set_idsoln

end module ExplicitModelModule

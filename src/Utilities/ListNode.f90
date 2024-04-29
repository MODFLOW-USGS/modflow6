module ListNodeModule
  implicit none
  private

  public :: ListNodeType

  type :: ListNodeType
    ! -- Public members
    type(ListNodeType), pointer, public :: nextNode => null()
    type(ListNodeType), pointer, public :: prevNode => null()
    class(*), pointer, public :: Value => null()
  contains
    ! -- Public procedure
    procedure, public :: GetItem
    procedure, public :: DeallocValue
  end type ListNodeType

contains
  ! -- Type-bound procedures for ListNodeType

  !> @brief Return a pointer to this node's value.
  function GetItem(this) result(valueObject)
    class(ListNodeType), intent(inout) :: this
    class(*), pointer :: valueObject
    valueObject => this%Value
  end function GetItem

  !> @brief Nullify (optionally deallocating) this node's value.
  subroutine DeallocValue(this, destroy)
    class(ListNodeType), intent(inout) :: this
    logical, intent(in), optional :: destroy

    if (associated(this%Value)) then
      if (present(destroy)) then
        if (destroy) then
          deallocate (this%Value)
        end if
      end if
      nullify (this%Value)
    end if
  end subroutine DeallocValue

end module

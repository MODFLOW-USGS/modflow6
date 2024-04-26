module ListIteratorModule
  use KindModule, only: I4B
  use IteratorModule, only: IteratorType
  use ListModule, only: ListType, ListNodeType

  implicit none
  private

  public :: ListIteratorType

  !> @brief An iterator used to iterate through a List
  !!
  !<
  type, extends(IteratorType) :: ListIteratorType
    type(ListType), pointer, private :: container => null() !< the List to iterate through
    type(ListNodeType), pointer, private :: current_node => null() !< the current node in the list the iterator is pointing at
  contains
    procedure :: has_next
    procedure :: next
    procedure :: value
  end type

  interface ListIteratorType
    module procedure Constructor
  end interface ListIteratorType

contains

  !> @brief Constructor to create a ListIterator
  !!
  !<
  function Constructor(container) result(iterator)
    ! -- dummy
    type(ListType), target :: container
    type(ListIteratorType) :: iterator

    iterator%container => container
    iterator%current_node => null()

  end function Constructor

  !> @brief Indicates if there is a next node in the iteration chain
  !!
  !<
  function has_next(this) result(res)
    ! -- dummy
    class(ListIteratorType) :: this
    type(logical) :: res

    if (associated(this%current_node)) then
      res = associated(this%current_node%nextNode)
    else
      res = associated(this%container%firstNode)
    end if

  end function

  !> @brief Increment the iterator to the next node
  !!
  !<
  subroutine next(this)
    ! -- dummy
    class(ListIteratorType) :: this

    if (associated(this%current_node)) then
      this%current_node => this%current_node%nextNode
    else
      this%current_node => this%container%firstNode
    end if
  end subroutine

  !> @brief Get the value the iterator is pointing at
  !!
  !<
  function value(this) result(res)
    ! -- dummy
    class(ListIteratorType) :: this
    class(*), pointer :: res

    if (associated(this%current_node)) then
      res => this%current_node%GetItem()
    else
      res => null()
    end if

  end function

end module ListIteratorModule

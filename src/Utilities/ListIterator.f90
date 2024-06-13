module ListIteratorModule
  use KindModule, only: I4B, LGP
  use IteratorModule, only: IteratorType
  use ListNodeModule, only: ListNodeType

  implicit none
  private

  public :: ListIteratorType

  !> @brief An iterator used to iterate through a List
  !!
  !<
  type, extends(IteratorType) :: ListIteratorType
    private
    type(ListNodeType), pointer :: first_node => null() !< the List to iterate through
    type(ListNodeType), pointer :: current_node => null() !< the current node in the list the iterator is pointing at
  contains
    procedure :: has_next
    procedure :: next
    procedure :: value
  end type

  interface ListIteratorType
    module procedure constructor
  end interface ListIteratorType

contains

  !> @brief Constructor to create a ListIterator
  !!
  !<
  function constructor(first_node) result(iterator)
    type(ListNodeType), pointer :: first_node
    type(ListIteratorType) :: iterator

    iterator%first_node => first_node
    iterator%current_node => null()

  end function constructor

  !> @brief Indicates if there is a next node in the iteration chain
  !!
  !<
  function has_next(this) result(res)
    class(ListIteratorType) :: this
    logical(LGP) :: res

    if (associated(this%current_node)) then
      res = associated(this%current_node%nextNode)
    else
      res = associated(this%first_node)
    end if

  end function

  !> @brief Increment the iterator to the next node
  !!
  !<
  subroutine next(this)
    class(ListIteratorType) :: this

    if (associated(this%current_node)) then
      this%current_node => this%current_node%nextNode
    else
      this%current_node => this%first_node
    end if
  end subroutine

  !> @brief Get the value the iterator is pointing at
  !!
  !<
  function value(this) result(res)
    class(ListIteratorType) :: this
    class(*), pointer :: res

    if (associated(this%current_node)) then
      res => this%current_node%GetItem()
    else
      res => null()
    end if

  end function

end module ListIteratorModule

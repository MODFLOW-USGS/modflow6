module KeyValueListIteratorModule
  use KindModule, only: I4B
  use KeyValueNodeModule, only: KeyValueNodeType
  use IteratorModule, only: IteratorType

  !> @brief An iterator used to iterate through a KeyValueList
  !!
  !<
  type, extends(IteratorType) :: KeyValueListIteratorType
    private
    type(KeyValueNodeType), pointer :: first_node => null() !< the KeyValueList to iterate through
    type(KeyValueNodeType), pointer :: current_node => null() !< the current node in the list the iterator is pointing to
  contains
    procedure :: has_next
    procedure :: next
    procedure :: value
  end type

  interface KeyValueListIteratorType
    module procedure constructor
  end interface KeyValueListIteratorType

contains

  !> @brief Constructor to create a KeyValueListIterator
  !!
  !<
  function constructor(first_node) Result(iterator)
    type(KeyValueNodeType), pointer, intent(in) :: first_node
    type(KeyValueListIteratorType) :: iterator

    iterator%first_node => first_node
    iterator%current_node => null()

  end function constructor

  !> @brief Indicates if there is a next node in the iteration chain
  !!
  !<
  function has_next(this) result(res)
    class(KeyValueListIteratorType) :: this
    type(logical) :: res

    if (associated(this%current_node)) then
      res = associated(this%current_node%next)
    else
      res = associated(this%first_node)
    end if

  end function

  !> @brief Increment the iterator to the next node
  !!
  !<
  subroutine next(this)
    class(KeyValueListIteratorType) :: this

    if (associated(this%current_node)) then
      this%current_node => this%current_node%next
    else
      this%current_node => this%first_node
    end if
  end subroutine

  !> @brief Get the value the iterator is pointing at
  !!
  !<
  function value(this) result(res)
    class(KeyValueListIteratorType) :: this
    class(*), pointer :: res

    if (associated(this%current_node)) then
      res => this%current_node%value
    else
      res => null()
    end if

  end function

end module KeyValueListIteratorModule

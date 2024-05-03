module KeyValueListModule
  use KindModule, only: I4B
  use IteratorModule, only: IteratorType
  use KeyValueListIteratorModule, only: KeyValueListIteratorType
  use KeyValueNodeModule, only: KeyValueNodeType
  use ConstantsModule, only: LENMEMADDRESS
  implicit none
  private

  public :: KeyValueListType

  !> @brief A list that stores items as a key-value pair
  !!
  !! Items in this list can be retrieved by using a key.
  !<
  type KeyValueListType
    private
    type(KeyValueNodeType), pointer :: first => null() !< first item in the list
    type(KeyValueNodeType), pointer :: last => null() !< first item in the list
    integer(I4B) :: cnt = 0 !< number of items in the list
  contains
    procedure :: iterator
    procedure :: add
    procedure :: get
    procedure :: count
    procedure :: clear
  end type KeyValueListType

contains

  function iterator(this) result(itr)
    ! -- dummy
    class(KeyValueListType) :: this
    class(IteratorType), allocatable :: itr

    itr = KeyValueListIteratorType(this%first)
  end function

  !> @brief Add a key-value pair to the list
  !!
  !! The list uses an 'append to end' approach for adding items
  !<
  subroutine add(this, key, val)
    ! -- dummy
    class(KeyValueListType) :: this
    character(len=*), intent(in) :: key
    class(*), pointer, intent(in) :: val
    ! -- local

    if (.not. associated(this%first)) then
      allocate (this%first)
      this%last => this%first
    else
      allocate (this%last%next)
      this%last => this%last%next
    end if

    allocate (this%last%key)
    this%last%key = key
    this%last%value => val
    this%last%next => null()

    this%cnt = this%cnt + 1

  end subroutine add

  !> @brief Get a value using a key
  !!
  !! If the key can't be found the return value will be a null pointer
  !<
  function get(this, key) result(val)
    ! -- dummy
    class(KeyValueListType) :: this
    character(len=*), intent(in) :: key
    class(*), pointer :: val
    ! -- local
    type(KeyValueNodeType), pointer :: node !< current node in the list

    val => null()
    node => this%first

    do while (associated(node))
      if (node%key == key) then
        val => node%value
        exit
      end if
      node => node%next
    end do

  end function

  !> @brief The nummer of items in the list
  !!
  !<
  function count(this) result(val)
    ! -- dummy
    class(KeyValueListType) :: this
    integer(I4B) :: val

    val = this%cnt
  end function

  !> @brief Clears the list
  !!
  !! clears all the nodes of the list
  !<
  subroutine clear(this)
    ! -- dummy
    class(KeyValueListType) :: this

    if (associated(this%first)) call clear_node(this%first)
    this%first => null()
    this%last => null()
    this%cnt = 0

  end subroutine

  !> @brief Clears the node
  !!
  !! Recursive method that clears the next node before clearing itself
  !<
  recursive subroutine clear_node(node)
    ! -- dummy
    type(KeyValueNodeType), pointer :: node

    if (associated(node)) then
      call clear_node(node%next)

      deallocate (node%key)

      nullify (node%key)
      nullify (node%value)
      nullify (node%next)

      deallocate (node)
    end if

  end subroutine clear_node

end module KeyValueListModule

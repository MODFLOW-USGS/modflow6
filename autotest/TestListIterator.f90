module TestListIterator
  use KindModule, only: I4B
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use ListNodeModule, only: ListNodeType
  use ListIteratorModule, only: ListIteratorType
  use IteratorModule, only: IteratorType

  implicit none
  private
  public :: collect_listiterator

contains

  subroutine collect_listiterator(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
                new_unittest("constructor", test_constructor), &
                new_unittest("iterate_through_list", test_iterate_through_list), &
                new_unittest("empty_list", test_empty_list) &
                ]
  end subroutine collect_listiterator

  !> @brief Test the initial state of the iterator
  !!
  !! When the iterator is created with a non-empty list:
  !! - it should indicate that it has a next value
  !! - it should return null when retrieving the current value
  !<
  subroutine test_constructor(error)
    type(error_type), allocatable, intent(out) :: error
    !- Locals
    class(IteratorType), allocatable :: itr
    type(ListNodeType), target :: firstNode
    type(ListNodeType), pointer :: firstNodePtr

    !- Arrange.
    firstNodePtr => firstNode

    !- Act.
    itr = ListIteratorType(firstNodePtr)

    !- Assert.
    call check(error,.not. associated(itr%value()))
    if (allocated(error)) return

    call check(error, itr%has_next())
    if (allocated(error)) return
  end subroutine test_constructor

  !> @brief Iterate through a list
  !!
  !! This test creates an iterator for a list of 3 nodes.
  !! It iterates though it and validates the expected values
  !<
  subroutine test_iterate_through_list(error)
    type(error_type), allocatable, intent(out) :: error
    !- Locals
    class(IteratorType), allocatable :: itr
    type(ListNodeType), pointer :: firstNodePtr

    type(ListNodeType), target :: firstNode
    type(ListNodeType), target :: secondNode
    type(ListNodeType), target :: thirdNode

    integer(I4B), target :: expected_value1 = 2
    integer(I4B), target :: expected_value2 = 6
    integer(I4B), target :: expected_value3 = 567
    integer(I4B) :: expected_values(3)

    integer(I4B) :: itr_count = 0
    integer(I4B), pointer :: value_ptr

    !- Arrange.
    expected_values = [expected_value1, expected_value2, expected_value3]

    firstNode%value => expected_value1
    firstNode%nextNode => secondNode

    secondNode%value => expected_value2
    secondNode%nextNode => thirdNode

    thirdNode%value => expected_value3
    thirdNode%nextNode => null()

    firstNodePtr => firstNode
    itr = ListIteratorType(firstNodePtr)

    !- Act.
    do while (itr%has_next())
      call itr%next()
      itr_count = itr_count + 1
      !- Assert.
      select type (val => itr%value())
      type is (integer(I4B))
        value_ptr => val

        call check(error, value_ptr == expected_values(itr_count))
        if (allocated(error)) return
      end select
    end do

  end subroutine test_iterate_through_list

  !> @brief Test the initial state of the iterator with an empty list
  !!
  !! When the iterator is created it with an empty list:
  !! - It should indicate that it has no next value
  !! - It should return null when retrieving the current value
  !<
  subroutine test_empty_list(error)
    type(error_type), allocatable, intent(out) :: error
    !- Locals
    class(IteratorType), allocatable :: itr
    type(ListNodeType), pointer :: firstNodePtr

    !- Arrange.
    firstNodePtr => null()

    !- Act.
    itr = ListIteratorType(firstNodePtr)

    !- Assert.
    call check(error,.not. itr%has_next())
    if (allocated(error)) return

    call check(error,.not. associated(itr%value()))
    if (allocated(error)) return

  end subroutine test_empty_list

end module TestListIterator

module ListModule
  use KindModule, only: DP, I4B
  use ErrorUtilModule, only: pstop
  use ConstantsModule, only: LINELENGTH
  use IteratorModule, only: IteratorType
  use ListIteratorModule, only: ListIteratorType
  use ListNodeModule, only: ListNodeType

  implicit none
  private
  public :: ListType, isEqualIface

  !> @brief A generic heterogeneous doubly-linked list.
  type :: ListType
    ! -- Public members
    type(ListNodeType), pointer, public :: firstNode => null()
    character(len=20), public :: name = ' '
    ! -- Private members
    type(ListNodeType), pointer, private :: lastNode => null()
    type(ListNodeType), pointer, private :: currentNode => null()
    integer(I4B), private :: currentNodeIndex = 0
    integer(I4B), private :: nodeCount = 0
  contains
    ! -- Public procedures
    procedure, public :: Iterator
    procedure, public :: Add
    procedure, public :: Clear
    procedure, public :: Count
    procedure, public :: ContainsObject
    procedure, public :: DeallocateBackward
    procedure, public :: GetIndex
    procedure, public :: GetNextItem
    procedure, public :: GetPreviousItem
    generic, public :: GetItem => get_item_by_index, get_current_item
    procedure, public :: InsertAfter
    procedure, public :: InsertBefore
    procedure, private :: Next
    procedure, private :: Previous
    procedure, public :: Reset
    generic, public :: RemoveNode => remove_node_by_index, remove_this_node
    ! -- Private procedures
    procedure, private :: get_current_item
    procedure, private :: get_item_by_index
    procedure, private :: get_node_by_index
    procedure, private :: remove_node_by_index
    procedure, private :: remove_this_node
    ! Finalization is not supported in gfortran (as of 4.10.0)
    !final :: clear_list
  end type ListType

  interface
    function isEqualIface(obj1, obj2) result(isEqual)
      class(*), pointer :: obj1, obj2
      logical :: isEqual
    end function
  end interface

contains

  function iterator(this) result(itr)
    class(ListType) :: this
    class(IteratorType), allocatable :: itr

    itr = ListIteratorType(this%firstNode)
  end function

  !> @brief Append the given item to the list
  subroutine Add(this, objptr)
    ! -- dummy variables
    class(ListType), intent(inout) :: this
    class(*), pointer, intent(inout) :: objptr
    !
    if (.not. associated(this%firstNode)) then
      allocate (this%firstNode)
      this%firstNode%Value => objptr
      this%firstNode%prevNode => null()
      this%lastNode => this%firstNode
    else
      allocate (this%lastNode%nextNode)
      this%lastNode%nextNode%prevNode => this%lastNode
      this%lastNode%nextNode%value => objptr
      this%lastNode => this%lastNode%nextNode
    end if
    this%nodeCount = this%nodeCount + 1
  end subroutine Add

  !> @brief Deallocate all items in list
  subroutine Clear(this, destroy)
    ! -- dummy variables
    class(ListType) :: this
    logical, intent(in), optional :: destroy
    ! -- local
    logical :: destroyLocal
    type(ListNodeType), pointer :: current => null()
    type(ListNodeType), pointer :: next => null()
    !
    destroyLocal = .false.
    if (present(destroy)) then
      destroyLocal = destroy
    end if
    !
    if (.not. associated(this%firstNode)) return
    ! -- The last node will be deallocated in the loop below.
    !    Just nullify the pointer to the last node to avoid
    !    having a dangling pointer. Also nullify currentNode.
    nullify (this%lastNode)
    nullify (this%currentNode)
    !
    current => this%firstNode
    do while (associated(current))
      ! -- Assign a pointer to the next node in the list
      next => current%nextNode
      ! -- Deallocate the object stored in the current node
      call current%DeallocValue(destroyLocal)
      ! -- Deallocate the current node
      deallocate (current)
      this%firstNode => next
      this%nodeCount = this%nodeCount - 1
      ! -- Advance to the next node
      current => next
    end do
    !
    call this%Reset()

  end subroutine Clear

  !> @brief Return number of nodes in list
  function Count(this)
    integer(I4B) :: Count
    class(ListType) :: this
    Count = this%nodeCount
  end function Count

  !> @brief Determine whether the list contains the given object.
  function ContainsObject(this, obj, isEqual) result(hasObj)
    class(ListType), intent(inout) :: this
    class(*), pointer :: obj
    procedure(isEqualIface), pointer, intent(in), optional :: isEqual
    logical :: hasObj
    ! local
    type(ListNodeType), pointer :: current => null()

    hasObj = .false.
    current => this%firstNode
    do while (associated(current))
      if (present(isEqual)) then
        if (isEqual(current%Value, obj)) then
          hasObj = .true.
          return
        end if
      else
        if (associated(current%Value, obj)) then
          hasObj = .true.
          return
        end if
      end if

      ! -- Advance to the next node
      current => current%nextNode
    end do

  end function

  !> @brief Deallocate fromNode and all previous nodes, and reassign firstNode.
  subroutine DeallocateBackward(this, fromNode)
    ! -- dummy
    class(ListType), target, intent(inout) :: this
    type(ListNodeType), pointer, intent(inout) :: fromNode
    ! -- local
    type(ListNodeType), pointer :: current => null()
    type(ListNodeType), pointer :: prev => null()
    !
    if (associated(fromNode)) then
      ! -- reassign firstNode
      if (associated(fromNode%nextNode)) then
        this%firstNode => fromNode%nextNode
      else
        this%firstNode => null()
      end if
      ! -- deallocate fromNode and all previous nodes
      current => fromNode
      do while (associated(current))
        prev => current%prevNode
        call current%DeallocValue(.true.)
        deallocate (current)
        this%nodeCount = this%nodeCount - 1
        current => prev
      end do
      fromNode => null()
    end if

  end subroutine DeallocateBackward

  !> @brief Get the index of the given item in the list.
  function GetIndex(this, obj) result(idx)
    class(ListType), target, intent(inout) :: this
    class(*), pointer :: obj
    integer(I4B) :: idx
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj_in_list

    idx = -1
    do i = 1, this%Count()
      obj_in_list => this%GetItem(i)
      if (associated(obj, obj_in_list)) then
        idx = i
        exit
      end if
    end do

  end function GetIndex

  !> @brief Get the next item in the list
  function GetNextItem(this) result(resultobj)
    class(ListType), target, intent(inout) :: this
    class(*), pointer :: resultobj
    call this%Next()
    resultobj => this%get_current_item()
  end function GetNextItem

  !> @brief Get the previous item in the list
  function GetPreviousItem(this) result(resultobj)
    class(ListType), target, intent(inout) :: this
    class(*), pointer :: resultobj
    call this%Previous()
    resultobj => this%get_current_item()
  end function GetPreviousItem

  !> @brief Insert the given item after the given index.
  subroutine InsertAfter(this, objptr, indx)
    ! -- dummy
    class(ListType), intent(inout) :: this
    class(*), pointer, intent(inout) :: objptr
    integer(I4B), intent(in) :: indx
    ! -- local
    integer(I4B) :: numnodes
    type(ListNodeType), pointer :: precedingNode => null()
    type(ListNodeType), pointer :: followingNode => null()
    type(ListNodeType), pointer :: newNode => null()
    !
    numnodes = this%Count()
    if (indx >= numnodes) then
      call this%Add(objptr)
    else
      precedingNode => this%get_node_by_index(indx)
      if (associated(precedingNode%nextNode)) then
        followingNode => precedingNode%nextNode
        allocate (newNode)
        newNode%Value => objptr
        newNode%nextNode => followingNode
        newNode%prevNode => precedingNode
        precedingNode%nextNode => newNode
        followingNode%prevNode => newNode
        this%nodeCount = this%nodeCount + 1
      else
        call pstop(1, 'Programming error in ListType%insert_after')
      end if
    end if

  end subroutine InsertAfter

  !> @brief Insert the given item before the given node.
  subroutine InsertBefore(this, objptr, targetNode)
    ! -- dummy
    class(ListType), intent(inout) :: this
    class(*), pointer, intent(inout) :: objptr
    type(ListNodeType), pointer, intent(inout) :: targetNode
    ! -- local
    type(ListNodeType), pointer :: newNode => null()
    !
    if (.not. associated(targetNode)) &
      call pstop(1, 'Programming error in ListType%InsertBefore')
    !
    ! Allocate a new list node and point its Value member to the object
    allocate (newNode)
    newNode%Value => objptr
    !
    ! Do the insertion
    newNode%nextNode => targetNode
    if (associated(targetNode%prevNode)) then
      ! Insert between two nodes
      targetNode%prevNode%nextNode => newNode
      newNode%prevNode => targetNode%prevNode
    else
      ! Insert before first node
      this%firstNode => newNode
      newNode%prevNode => null()
    end if
    targetNode%prevNode => newNode
    this%nodeCount = this%nodeCount + 1

  end subroutine InsertBefore

  !> @brief Move the list's current node pointer and index one node forwards.
  subroutine Next(this)
    class(ListType), target, intent(inout) :: this

    if (this%currentNodeIndex == 0) then
      if (associated(this%firstNode)) then
        this%currentNode => this%firstNode
        this%currentNodeIndex = 1
      else
        this%currentNode => null()
        this%currentNodeIndex = 0
      end if
    else
      if (associated(this%currentNode%nextNode)) then
        this%currentNode => this%currentNode%nextNode
        this%currentNodeIndex = this%currentNodeIndex + 1
      else
        this%currentNode => null()
        this%currentNodeIndex = 0
      end if
    end if
  end subroutine Next

  !> @brief Move the list's current node pointer and index one node backwards.
  subroutine Previous(this)
    class(ListType), target, intent(inout) :: this
    if (this%currentNodeIndex <= 1) then
      call this%Reset()
    else
      this%currentNode => this%currentNode%prevNode
      this%currentNodeIndex = this%currentNodeIndex - 1
    end if
  end subroutine Previous

  !> @brief Reset the list's current node pointer and index.
  subroutine Reset(this)
    class(ListType), target, intent(inout) :: this
    this%currentNode => null()
    this%currentNodeIndex = 0
  end subroutine Reset

  !> @brief Remove the node at the given index, optionally destroying its value.
  subroutine remove_node_by_index(this, i, destroyValue)
    ! -- dummy
    class(ListType), intent(inout) :: this
    integer(I4B), intent(in) :: i
    logical, intent(in) :: destroyValue
    ! -- local
    type(ListNodeType), pointer :: node
    !
    node => null()
    node => this%get_node_by_index(i)
    if (associated(node)) then
      call this%remove_this_node(node, destroyValue)
    end if

  end subroutine remove_node_by_index

  !> @brief Remove the given node, optionally destroying its value.
  subroutine remove_this_node(this, node, destroyValue)
    ! -- dummy
    class(ListType), intent(inout) :: this
    type(ListNodeType), pointer, intent(inout) :: node
    logical, intent(in) :: destroyValue
    ! -- local
    !
    logical :: first, last
    !
    first = .false.
    last = .false.
    if (associated(node)) then
      if (associated(node%prevNode)) then
        if (associated(node%nextNode)) then
          node%nextNode%prevNode => node%prevNode
        else
          node%prevNode%nextNode => null()
          this%lastNode => node%prevNode
        end if
      else
        first = .true.
      end if
      if (associated(node%nextNode)) then
        if (associated(node%prevNode)) then
          node%prevNode%nextNode => node%nextNode
        else
          node%nextNode%prevNode => null()
          this%firstNode => node%nextNode
        end if
      else
        last = .true.
      end if
      if (destroyValue) then
        call node%DeallocValue(destroyValue)
      end if
      deallocate (node)
      this%nodeCount = this%nodeCount - 1
      if (first .and. last) then
        this%firstNode => null()
        this%lastNode => null()
        this%currentNode => null()
      end if
      call this%Reset()
    end if

  end subroutine remove_this_node

  ! -- Private type-bound procedures for ListType

  !> @brief Get a pointer to the item at the current node.
  function get_current_item(this) result(resultobj)
    class(ListType), target, intent(inout) :: this
    ! result
    class(*), pointer :: resultobj
    !
    resultobj => null()
    if (associated(this%currentNode)) then
      resultobj => this%currentNode%Value
    end if
  end function get_current_item

  !> @brief Get a pointer to the item at the given index.
  function get_item_by_index(this, indx) result(resultobj)
    ! -- dummy
    class(ListType), intent(inout) :: this
    integer(I4B), intent(in) :: indx
    ! result
    class(*), pointer :: resultobj
    ! -- local
    integer(I4B) :: i
    !
    ! -- Initialize
    resultobj => null()
    !
    ! -- Ensure that this%currentNode is associated
    if (.not. associated(this%currentNode)) then
      this%currentNodeIndex = 0
    end if
    if (this%currentNodeIndex == 0) then
      if (associated(this%firstNode)) then
        this%currentNode => this%firstNode
        this%currentNodeIndex = 1
      end if
    end if
    !
    ! -- Check indx position relative to current node index
    i = 0
    if (indx < this%currentNodeIndex) then
      ! Start at beginning of list
      call this%Reset()
      if (associated(this%firstNode)) then
        this%currentNode => this%firstNode
        this%currentNodeIndex = 1
        i = 1
      end if
    else
      i = this%currentNodeIndex
    end if
    if (i == 0) return
    !
    ! -- If current node is requested node,
    !    assign pointer and return
    if (i == indx) then
      resultobj => this%currentNode%Value
      return
    end if
    !
    ! -- Iterate from current node to requested node
    do while (associated(this%currentNode%nextNode))
      this%currentNode => this%currentNode%nextNode
      this%currentNodeIndex = this%currentNodeIndex + 1
      if (this%currentNodeIndex == indx) then
        resultobj => this%currentNode%Value
        return
      end if
    end do
  end function get_item_by_index

  !> @brief Get the node at the given index
  function get_node_by_index(this, indx) result(resultnode)
    ! -- dummy
    class(ListType), intent(inout) :: this
    integer(I4B), intent(in) :: indx
    ! result
    type(ListNodeType), pointer :: resultnode
    ! -- local
    integer(I4B) :: i
    !
    ! -- Initialize
    resultnode => null()
    !
    ! -- Ensure that this%currentNode is associated
    if (this%currentNodeIndex == 0) then
      if (associated(this%firstNode)) then
        this%currentNode => this%firstNode
        this%currentNodeIndex = 1
      end if
    end if
    !
    ! -- Check indx position relative to current node index
    i = 0
    if (indx < this%currentNodeIndex) then
      ! Start at beginning of list
      call this%Reset()
      if (associated(this%firstNode)) then
        this%currentNode => this%firstNode
        this%currentNodeIndex = 1
        i = 1
      end if
    else
      i = this%currentNodeIndex
    end if
    if (i == 0) return
    !
    ! -- If current node is requested node,
    !    assign pointer and return
    if (i == indx) then
      resultnode => this%currentNode
      return
    end if
    !
    ! -- Iterate from current node to requested node
    do while (associated(this%currentNode%nextNode))
      this%currentNode => this%currentNode%nextNode
      this%currentNodeIndex = this%currentNodeIndex + 1
      if (this%currentNodeIndex == indx) then
        resultnode => this%currentNode
        return
      end if
    end do
  end function get_node_by_index

end module ListModule

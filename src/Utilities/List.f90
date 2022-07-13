module ListModule
  ! -- ListType implements a generic list.
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH
  use GenericUtilitiesModule, only: sim_message, stop_with_error
  private
  public :: ListType, ListNodeType, isEqualIface, arePointersEqual

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
    procedure, public :: Add
    procedure, public :: Clear
    procedure, public :: Count
    procedure, public :: ContainsObject
    procedure, public :: DeallocateBackward
    procedure, public :: GetNextItem
    procedure, public :: GetPreviousItem
    generic, public :: GetItem => get_item_by_index, get_current_item
    procedure, public :: InsertAfter
    procedure, public :: InsertBefore
    procedure, public :: Next
    procedure, public :: Previous
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

  type :: ListNodeType
    ! -- Public members
    type(ListNodeType), pointer, public :: nextNode => null()
    type(ListNodeType), pointer, public :: prevNode => null()
    ! -- Private members
    class(*), pointer, private :: Value => null()
  contains
    ! -- Public procedure
    procedure, public :: GetItem
    ! -- Private procedures
    procedure, private :: DeallocValue
  end type ListNodeType

  interface
    function isEqualIface(obj1, obj2) result(isEqual)
      class(*), pointer :: obj1, obj2
      logical :: isEqual
    end function
  end interface

contains

  ! -- Public type-bound procedures for ListType

  subroutine Add(this, objptr)
    implicit none
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
    return
  end subroutine Add

  subroutine Clear(this, destroy)
    ! **************************************************************************
    ! clear_list (finalizer)
    ! Deallocate all items in linked list
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    implicit none
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
    !
    return
  end subroutine Clear

  function Count(this)
    ! **************************************************************************
    ! Count
    ! Return number of nodes in linked list
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    implicit none
    ! -- return
    integer(I4B) :: Count
    ! -- dummy variables
    class(ListType) :: this
    !
    Count = this%nodeCount
    !
    return
  end function Count

  function ContainsObject(this, obj, isEqual) result(hasObj)
    class(ListType), intent(inout) :: this
    class(*), pointer :: obj
    procedure(isEqualIface), pointer, intent(in) :: isEqual
    logical :: hasObj
    ! local
    type(ListNodeType), pointer :: current => null()

    hasObj = .false.
    current => this%firstNode
    do while (associated(current))
      if (isEqual(current%Value, obj)) then
        hasObj = .true.
        return
      end if

      ! -- Advance to the next node
      current => current%nextNode
    end do

    ! this means there is no match
    return
  end function

  function arePointersEqual(obj1, obj2) result(areIdentical)
    class(*), pointer :: obj1, obj2
    logical :: areIdentical
    areIdentical = associated(obj1, obj2)
  end function arePointersEqual

  subroutine DeallocateBackward(this, fromNode)
    ! **************************************************************************
    ! DeallocateBackward
    ! Deallocate fromNode and all previous nodes in list; reassign firstNode.
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    implicit none
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
    !
    return
  end subroutine DeallocateBackward

  function GetNextItem(this) result(resultobj)
    implicit none
    class(ListType), target, intent(inout) :: this
    ! result
    class(*), pointer :: resultobj
    !
    call this%Next()
    resultobj => this%get_current_item()
    return
  end function GetNextItem

  function GetPreviousItem(this) result(resultobj)
    implicit none
    class(ListType), target, intent(inout) :: this
    ! result
    class(*), pointer :: resultobj
    !
    call this%Previous()
    resultobj => this%get_current_item()
    return
  end function GetPreviousItem

  subroutine InsertAfter(this, objptr, indx)
    implicit none
    ! -- dummy
    class(ListType), intent(inout) :: this
    class(*), pointer, intent(inout) :: objptr
    integer(I4B), intent(in) :: indx
    ! -- local
    character(len=LINELENGTH) :: line
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
        write (line, '(a)') 'Programming error in ListType%insert_after'
        call sim_message(line)
        call stop_with_error(1)
      end if
    end if
    !
    return
  end subroutine InsertAfter

  subroutine InsertBefore(this, objptr, targetNode)
    ! Insert an object into the list in front of a target node
    implicit none
    ! -- dummy
    class(ListType), intent(inout) :: this
    class(*), pointer, intent(inout) :: objptr
    type(ListNodeType), pointer, intent(inout) :: targetNode
    ! -- local
    type(ListNodeType), pointer :: newNode => null()
    !
    if (.not. associated(targetNode)) then
      stop 'Programming error, likely in call to ListType%InsertBefore'
    end if
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
    !
    return
  end subroutine InsertBefore

  subroutine Next(this)
    implicit none
    class(ListType), target, intent(inout) :: this
    !
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
    return
  end subroutine Next

  subroutine Previous(this)
    implicit none
    class(ListType), target, intent(inout) :: this
    !
    if (this%currentNodeIndex <= 1) then
      call this%Reset()
    else
      this%currentNode => this%currentNode%prevNode
      this%currentNodeIndex = this%currentNodeIndex - 1
    end if
    return
  end subroutine Previous

  subroutine Reset(this)
    implicit none
    class(ListType), target, intent(inout) :: this
    !
    this%currentNode => null()
    this%currentNodeIndex = 0
    return
  end subroutine Reset

  subroutine remove_node_by_index(this, i, destroyValue)
    implicit none
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
    !
    return
  end subroutine remove_node_by_index

  subroutine remove_this_node(this, node, destroyValue)
    implicit none
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
    !
    return
  end subroutine remove_this_node

  ! -- Private type-bound procedures for ListType

  function get_current_item(this) result(resultobj)
    implicit none
    class(ListType), target, intent(inout) :: this
    ! result
    class(*), pointer :: resultobj
    !
    resultobj => null()
    if (associated(this%currentNode)) then
      resultobj => this%currentNode%Value
    end if
    return
  end function get_current_item

  function get_item_by_index(this, indx) result(resultobj)
    ! **************************************************************************
    ! get_item_by_index (implements GetItem)
    ! Return object stored in ListNodeType%Value by index in list
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    implicit none
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
    return
  end function get_item_by_index

  function get_node_by_index(this, indx) result(resultnode)
    ! **************************************************************************
    ! get_item_by_index (implements GetItem)
    ! Return object stored in ListNodeType%Value by index in list
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    implicit none
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
    return
  end function get_node_by_index

  ! -- Type-bound procedures for ListNodeType

  function GetItem(this) result(valueObject)
    ! ************************************************************************
    ! Perform a pointer assignment of valueObject to the contents of
    ! this%Value
    ! ************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------
    implicit none
    class(ListNodeType), intent(inout) :: this
    class(*), pointer :: valueObject
    !
    valueObject => this%Value
    return
  end function GetItem

  subroutine DeallocValue(this, destroy)
    ! ************************************************************************
    ! Deallocate whatever is stored in the Value component of this node.
    ! ************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------
    implicit none
    class(ListNodeType), intent(inout) :: this
    logical, intent(in), optional :: destroy
    !
    if (associated(this%Value)) then
      if (present(destroy)) then
        if (destroy) then
          deallocate (this%Value)
        end if
      end if
      nullify (this%Value)
    end if
    return
  end subroutine DeallocValue

end module ListModule

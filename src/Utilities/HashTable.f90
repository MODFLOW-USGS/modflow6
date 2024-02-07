!> @brief A chaining hash map for integers.
!!
!! Convenient for use as an index into arrays of arbitrary
!! data type. The implementation is based on Arjen Markus'
!! dictionary in the Flibs collection of Fortran utilities.
!<
module HashTableModule

  use KindModule, only: DP, I4B

  implicit none

  private
  public HashTableType
  public hash_table_cr
  public hash_table_da

  integer, parameter, private :: HASH_SIZE = 4993
  integer, parameter, private :: MULTIPLIER = 31

  type :: NodeType
    character(len=:), allocatable :: key
    integer(I4B) :: value
    type(NodeType), pointer :: next => null()
  contains
    procedure :: add => list_add
  end type NodeType

  type :: BucketType
    type(NodeType), pointer :: list => null()
  end type BucketType

  type :: HashTableType
    private
    type(BucketType), pointer :: buckets(:) => null()
  contains
    procedure :: add => ht_add
    procedure :: get => ht_get
    procedure, private :: find_node
  end type HashTableType

contains

  !> @brief Create a hash table
  subroutine hash_table_cr(map)
    ! -- dummy
    type(HashTableType), pointer :: map
    ! -- local
    integer(I4B) :: i

    ! -- allocate
    allocate (map)
    allocate (map%buckets(HASH_SIZE))

    ! -- initialize nul buckets
    do i = 1, HASH_SIZE
      map%buckets(i)%list => null()
    end do

  end subroutine hash_table_cr

  !> @brief Deallocate the hash table
  subroutine hash_table_da(map)
    ! -- dummy
    type(HashTableType), pointer :: map
    ! -- local
    integer(I4B) :: i

    ! -- deallocate each bucket
    do i = 1, size(map%buckets)
      if (associated(map%buckets(i)%list)) then
        call list_da(map%buckets(i)%list)
      end if
    end do

    ! -- deallocate bucket array and hash table
    deallocate (map%buckets)
    deallocate (map)

  end subroutine hash_table_da

  !> @brief Associate the given key and value
  subroutine ht_add(this, k, v)
    ! -- dummy
    class(HashTableType) :: this
    character(len=*), intent(in) :: k
    integer(I4B), intent(in) :: v
    ! -- local
    type(NodeType), pointer :: node
    integer(I4B) :: h

    ! -- find the element corresponding to this key and replace index or
    !    get an unassociated elem that corresponds to this key
    node => this%find_node(k)

    ! -- replace index or create new entry
    if (associated(node)) then
      node%value = v
    else
      h = hash(trim(k))
      if (associated(this%buckets(h)%list)) then
        call this%buckets(h)%list%add(k, v)
      else
        call list_cr(this%buckets(h)%list, k, v)
      end if
    end if

  end subroutine ht_add

  !> @brief Find the node containing the given key
  function find_node(this, k) result(node)
    ! -- dummy
    class(HashTableType) :: this !< the hash map
    character(len=*), intent(in) :: k !< the key
    ! -- local
    type(NodeType), pointer :: node
    integer(I4B) :: h

    h = hash(trim(k))
    node => this%buckets(h)%list

    ! -- search bucket for node with matching key
    do while (associated(node))
      if (node%key == k) then
        exit
      else
        node => node%next
      end if
    end do

  end function find_node

  !> @brief Get the value for the given key if it exists, otherwise return zero.
  function ht_get(this, k) result(v)
    ! -- dummy
    class(HashTableType) :: this !< the hash map
    character(len=*), intent(in) :: k !< the key
    ! -- return
    integer(I4B) :: v
    ! -- local
    type(NodeType), pointer :: node

    node => this%find_node(k)
    if (associated(node)) then
      v = node%value
    else
      v = 0
    end if

  end function ht_get

  !> @brief Create a list with the given key/value pair
  subroutine list_cr(list, k, v)
    ! -- dummy
    type(NodeType), pointer :: list !< pointer to the list
    character(len=*), intent(in) :: k !< the first key
    integer(I4B), intent(in) :: v !< the first value

    allocate (list)
    list%next => null()
    list%key = k
    list%value = v

  end subroutine list_cr

  !> @brief Add a key/value pair to the list
  subroutine list_add(this, k, v)
    ! -- dummy
    class(NodeType) :: this !< the list
    character(len=*), intent(in) :: k !< the key
    integer(I4B), intent(in) :: v !< the value
    ! -- local
    type(NodeType), pointer :: next

    allocate (next)
    next%key = k
    next%value = v
    next%next => this%next
    this%next => next

  end subroutine list_add

  !> @brief Deallocate the list
  subroutine list_da(list)
    ! -- dummy
    type(NodeType), pointer, intent(in) :: list !< the list
    ! -- local
    type(NodeType), pointer :: curr
    type(NodeType), pointer :: node

    node => list
    do while (associated(node))
      curr => node
      node => curr%next
      deallocate (curr)
    end do

  end subroutine list_da

  !> @brief Map a character string to an integer
  function hash(k) result(h)
    ! -- dummy
    character(len=*), intent(in) :: k !< the key
    ! -- local
    integer(I4B) :: h
    integer(I4B) :: i

    h = 0
    do i = 1, len(k)
      h = modulo(MULTIPLIER * h + ichar(k(i:i)), HASH_SIZE)
    end do
    h = 1 + modulo(h - 1, HASH_SIZE)

  end function hash

end module HashTableModule

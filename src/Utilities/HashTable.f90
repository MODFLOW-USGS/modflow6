! HashTableType implements a hash table for storing integers,
! for use as an index for an array that could contain
! any data type.  This HashTableModule was designed using the
! dictionary implementation by Arjen Markus of the Flibs
! collection of Fortran utilities.  This hash table works
! like a dictionary.  There can be n number of character
! strings and each string will be assigned a unique number
! between 1 and n, allowing an efficient way to store a
! unique integer index with a character string.

module HashTableModule

  use KindModule, only: DP, I4B

  implicit none

  private
  public HashTableType
  public hash_table_cr  ! JLM: this could be made more OO, but it would probably break existing code
  public hash_table_da

  integer, parameter, private :: HASH_SIZE  = 4993
  integer, parameter, private :: MULTIPLIER = 31

  type :: ListDataType
    character(len=:), allocatable :: key
    integer(I4B) :: index
  end type ListDataType

  type :: ListType
    type(ListDataType) :: listdata
    type(ListType), pointer :: next => null()
  contains
    procedure :: add => listtype_add
  end type ListType

  type :: HashListType
    type(ListType), pointer :: list => null()
  end type HashListType

  type :: HashTableType
    private
    type(HashListType), dimension(:), pointer :: table => null()
  contains
    procedure :: add_entry
    procedure :: get_elem
    procedure :: get_index
  end type HashTableType

contains

  ! hash_table_cr -- public subroutine to create the hash table object
  subroutine hash_table_cr(ht)
    type(HashTableType), pointer :: ht
    ! -- local
    integer(I4B) :: i
    !
    allocate(ht)
    allocate(ht%table(HASH_SIZE))
    !
    ! -- nullify each list
    do i = 1, HASH_SIZE
      ht%table(i)%list => null()
    enddo
    !
    return
  end subroutine hash_table_cr

  ! hash_table_da -- public subroutine to deallocate the hash table object
  subroutine hash_table_da(ht)
    type(HashTableType), pointer :: ht
    ! -- local
    integer(I4B) :: i
    !
    ! -- deallocate the list for each hash
    do i = 1, size(ht%table)
      if ( associated(ht%table(i)%list)) then
        call listtype_da(ht%table(i)%list)
      endif
    enddo
    !
    ! -- deallocate the table and the hash table
    deallocate(ht%table)
    deallocate(ht)
    !
    return
  end subroutine hash_table_da

  ! add_entry -- hash table method to add a key/index entry
  subroutine add_entry(this, key, index)
    class(HashTableType) :: this
    character(len=*), intent(in) :: key
    integer(I4B), intent(in) :: index
    ! -- local
    type(ListType), pointer :: elem
    integer(I4B) :: ihash
    !
    ! -- find the element corresponding to this key and replace index or
    !    get an unassociated elem that corresponds to this key
    elem => this%get_elem(key)
    !
    ! -- replace index or create new entry
    if (associated(elem)) then
      elem%listdata%index = index
    else
      ihash = hashfunc(trim(key))
      if (associated(this%table(ihash)%list)) then
        call this%table(ihash)%list%add(key, index)
      else
        call listtype_cr(this%table(ihash)%list, key, index)
      end if
    end if
    !
    return
  end subroutine add_entry

  ! get_elem -- get the entry corresponding to this key
  function get_elem(this, key) result(elem)
    class(HashTableType) :: this
    character(len=*), intent(in) :: key
    ! -- local
    type(ListType), pointer :: elem
    integer(I4B) :: ihash
    !
    ihash = hashfunc(trim(key))
    elem => this%table(ihash)%list
    do while (associated(elem))
      if (elem%listdata%key == key) then
        exit
      else
        elem => elem%next
      end if
    enddo
    !
    return
  end function get_elem

  ! get_index -- get the integer index that corresponds to this hash.
  !   Return a zero if key does not exist in hash table.
  function get_index(this, key) result(index)
    class(HashTableType) :: this
    character(len=*), intent(in) :: key
    ! -- return
    integer(I4B) :: index
    ! -- local
    type(ListType), pointer :: elem
    !
    elem => this%get_elem(key)
    if (associated(elem)) then
      index = elem%listdata%index
    else
      index = 0
    endif
    !
    return
  end function get_index

  ! listtype_cr -- subroutine to create a list
  subroutine listtype_cr(list, key, index)
    type(ListType), pointer :: list
    character(len=*), intent(in) :: key
    integer(I4B), intent(in) :: index
    !
    allocate(list)
    list%next => null()
    list%listdata%key = key
    list%listdata%index = index
    !
    return
  end subroutine listtype_cr

  ! listtype_add -- method to add a key/index pair to a list
  subroutine listtype_add(this, key, index)
    class(ListType) :: this
    character(len=*), intent(in) :: key
    integer(I4B), intent(in) :: index
    ! -- local
    type(ListType), pointer :: next
    !
    allocate(next)
    next%listdata%key = key
    next%listdata%index = index
    next%next => this%next
    this%next => next
    !
    return
  end subroutine listtype_add

  ! listtype_da -- subroutine to deallocate a list
  subroutine listtype_da(list)
    type(ListType), pointer, intent(in) :: list
    ! -- local
    type(ListType), pointer  :: current
    type(ListType), pointer  :: elem
    !
    elem => list
    do while ( associated(elem) )
      current => elem
      elem => current%next
      deallocate(current)
    enddo
    !
    return
  end subroutine listtype_da

  ! hashfunc -- function to convert key into an integer hash number
  function hashfunc(key) result(ihash)
    character(len=*), intent(in) :: key
    ! -- local
    integer(I4B) :: ihash
    integer(I4B) :: i
    !
    ihash = 0
    do i = 1,len(key)
      ihash = modulo( MULTIPLIER * ihash + ichar(key(i:i)), HASH_SIZE)
    enddo
    ihash = 1 + modulo(ihash - 1, HASH_SIZE)
    !
    return
  end function hashfunc

  ! JLM: collisions?

end module HashTableModule

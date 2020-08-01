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
  public hash_table_cr
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
  
  subroutine hash_table_cr(ht)
! ******************************************************************************
! hash_table_cr -- public subroutine to create the hash table object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(HashTableType), pointer :: ht
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- allocate
    allocate(ht)
    allocate(ht%table(HASH_SIZE))
    !
    ! -- nullify each list
    do i = 1, HASH_SIZE
      ht%table(i)%list => null()
    enddo
    !
    ! -- return
    return
  end subroutine hash_table_cr
  
  subroutine hash_table_da(ht)
! ******************************************************************************
! hash_table_da -- public subroutine to deallocate the hash table object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(HashTableType), pointer :: ht
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- deallocate the list for each hash
    do i = 1, size(ht%table)
      if ( associated( ht%table(i)%list)) then
        call listtype_da(ht%table(i)%list)
      endif
    enddo
    !
    ! -- deallocate the table and the hash table
    deallocate(ht%table)
    deallocate(ht)
    !
    ! -- return
    return
  end subroutine hash_table_da
  
  subroutine add_entry(this, key, index)
! ******************************************************************************
! add_entry -- hash table method to add a key/index entry
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(HashTableType) :: this
    character(len=*), intent(in) :: key
    integer(I4B), intent(in) :: index
    ! -- local
    type(ListType), pointer :: elem
    integer(I4B) :: ihash
! ------------------------------------------------------------------------------
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
    ! -- return
    return
  end subroutine add_entry

  function get_elem(this, key) result(elem)
! ******************************************************************************
! get_elem -- get the entry corresponding to this key
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(HashTableType) :: this
    character(len=*), intent(in) :: key
    ! -- local
    type(ListType), pointer :: elem
    integer(I4B) :: ihash
! ------------------------------------------------------------------------------
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
    ! -- return
    return
  end function get_elem 
  
  function get_index(this, key) result(index)
! ******************************************************************************
! get_index -- get the integer index that corresponds to this hash.
!   Return a zero if key does not exist in hash table.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(HashTableType) :: this
    character(len=*), intent(in) :: key
    ! -- return
    integer(I4B) :: index
    ! -- local
    type(ListType), pointer :: elem
! ------------------------------------------------------------------------------
    elem => this%get_elem(key)
    if (associated(elem)) then
      index = elem%listdata%index
    else
      index = 0
    endif
    !
    ! -- return
    return
  end function get_index
  
  subroutine listtype_cr(list, key, index)
! ******************************************************************************
! listtype_cr -- subroutine to create a list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(ListType), pointer :: list
    character(len=*), intent(in) :: key
    integer(I4B), intent(in) :: index
! ------------------------------------------------------------------------------
    allocate(list)
    list%next => null()
    list%listdata%key = key
    list%listdata%index = index
    !
    ! -- return
    return
  end subroutine listtype_cr

  subroutine listtype_add(this, key, index)
! ******************************************************************************
! listtype_add -- method to add a key/index pair to a list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(ListType) :: this
    character(len=*), intent(in) :: key
    integer(I4B), intent(in) :: index
    ! -- local
    type(ListType), pointer :: next
! ------------------------------------------------------------------------------
    allocate(next)
    next%listdata%key = key
    next%listdata%index = index
    next%next => this%next
    this%next => next
    !
    ! -- return
    return
  end subroutine listtype_add

  subroutine listtype_da(list)
! ******************************************************************************
! listtype_da -- subroutine to deallocate a list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(ListType), pointer, intent(in) :: list
    ! -- local
    type(ListType), pointer  :: current
    type(ListType), pointer  :: elem
! ------------------------------------------------------------------------------
    elem => list
    do while ( associated(elem) )
      current => elem
      elem => current%next
      deallocate(current)
    enddo
    !
    ! -- return
    return
  end subroutine listtype_da

  function hashfunc(key) result(ihash)
! ******************************************************************************
! hashfunc -- function to convert key into an integer hash number
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: key
    ! -- local
    integer(I4B) :: ihash
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    ihash = 0
    do i = 1,len(key)
      ihash = modulo( MULTIPLIER * ihash + ichar(key(i:i)), HASH_SIZE)
    enddo
    ihash = 1 + modulo(ihash - 1, HASH_SIZE)
    !
    ! -- return
    return
  end function hashfunc

end module HashTableModule

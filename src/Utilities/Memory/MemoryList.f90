module MemoryListModule
  use ConstantsModule, only: LENMEMPATH, LENVARNAME
  use KindModule, only: DP, I4B
  use MemoryTypeModule, only: MemoryType
  use ListModule, only: ListType
  use IteratorModule, only: IteratorType
  use ListIteratorModule, only: ListIteratorType
  use MemoryContainerIteratorModule, only: MemoryContainerIteratorType
  
  private
  public :: MemoryListType

  type :: MemoryListType
    type(ListType), private :: list
  contains
    procedure :: iterator
    procedure :: add
    procedure :: get
    procedure :: count
    procedure :: clear
  end type MemoryListType

contains

  function iterator(this) result(itr)
    class(MemoryListType) :: this
    type(MemoryContainerIteratorType) :: itr

    class(IteratorType), allocatable :: container_iterator
    allocate(container_iterator, source=ListIteratorType(this%list))

    itr = MemoryContainerIteratorType(container_iterator)
  end function

  subroutine add(this, mt)
    class(MemoryListType) :: this
    type(MemoryType), pointer :: mt
    class(*), pointer :: obj => null()
    obj => mt
    call this%list%add(obj)
  end subroutine add

  function get(this, name, path) result(mt)
    ! -- dummy variables
    class(MemoryListType) :: this
    character(len=*) :: name
    character(len=*) :: path
    type(MemoryType), pointer :: mt
    ! -- local
    type(MemoryContainerIteratorType) :: itr

    itr = this%iterator()
    do while (itr%has_next())
      call itr%next()
      mt => itr%value()
      if (mt%name == name .and. mt%path == path) then
        return
      end if
    end do

    mt => null()

  end function get

  function count(this) result(nval)
    class(MemoryListType) :: this
    integer(I4B) :: nval
    nval = this%list%count()
    return
  end function count

  subroutine clear(this)
    class(MemoryListType) :: this
    call this%list%Clear()
  end subroutine clear

end module MemoryListModule

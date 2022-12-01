module MemoryListModule
  use KindModule, only: DP, I4B
  use MemoryTypeModule, only: MemoryType
  use ListModule, only: ListType
  private
  public :: MemoryListType

  type :: MemoryListType
    type(ListType), private :: list
  contains
    procedure :: add
    procedure :: get
    procedure :: count
    procedure :: clear
    procedure :: remove
  end type MemoryListType

contains

  subroutine add(this, mt)
    class(MemoryListType) :: this
    type(MemoryType), pointer :: mt
    class(*), pointer :: obj => null()
    obj => mt
    call this%list%add(obj)
  end subroutine add

  function get(this, ipos) result(res)
    class(MemoryListType) :: this
    integer(I4B), intent(in) :: ipos
    type(MemoryType), pointer :: res
    class(*), pointer :: obj => null()
    obj => this%list%getitem(ipos)
    select type (obj)
    type is (MemoryType)
      res => obj
    end select
    return
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

  subroutine remove(this, ipos, destroyValue)
    class(MemoryListType) :: this
    integer(I4B), intent(in) :: ipos
    logical, intent(in) :: destroyValue
    call this%list%RemoveNode(ipos, destroyValue)
  end subroutine remove

end module MemoryListModule

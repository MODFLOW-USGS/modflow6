module MemoryListModule
  use KindModule, only: I4B
  use MemoryTypeModule, only: MemoryType
  use PtrHashTableModule, only: PtrHashTableType
  use IteratorModule, only: IteratorType
  use MemoryContainerIteratorModule, only: MemoryContainerIteratorType
  use MemoryHelperModule, only: create_mem_address
  use ConstantsModule, only: LENMEMADDRESS

  private
  public :: MemoryListType

  type :: MemoryListType
    private
    type(PtrHashTableType), private :: container
  contains
    procedure :: iterator
    procedure :: add
    procedure :: get
    procedure :: count
    procedure :: clear
  end type MemoryListType

contains

  !> @brief An iterator used to iterate through a MemoryContainer
  !!
  !<
  function iterator(this) result(itr)
    ! -- dummy
    class(MemoryListType) :: this
    type(MemoryContainerIteratorType) :: itr

    itr = MemoryContainerIteratorType(this%container%Iterator())
  end function

  !> @brief Add a MemoryType to the container
  !!
  !! The MemoryType is stored together with a key for easy lookup
  !! The key is constructed using the memory type's path and name
  !<
  subroutine add(this, mt)
    ! -- dummy
    class(MemoryListType) :: this
    type(MemoryType), pointer, intent(in) :: mt
    ! -- local
    class(*), pointer :: obj => null()
    character(len=LENMEMADDRESS) :: key

    key = create_mem_address(mt%path, mt%name)
    obj => mt
    call this%container%add(key, obj)
  end subroutine add

  !> @brief Get a MemoryType using a key
  !!
  !! If the key can't be found the return value will be a null pointer
  !<
  function get(this, name, path) result(mt)
    ! -- dummy
    class(MemoryListType) :: this
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: path
    type(MemoryType), pointer :: mt
    ! -- local
    character(len=LENMEMADDRESS) :: key
    class(*), pointer :: obj

    key = create_mem_address(path, name)
    obj => this%container%get(key)

    select type (obj)
    type is (MemoryType)
      mt => obj
    class default
      mt => null()
    end select

  end function get

  !> @brief The nummer of items in the container
  !!
  !<
  function count(this) result(cnt)
    ! -- dummy
    class(MemoryListType) :: this
    integer(I4B) :: cnt

    cnt = this%container%count()
  end function count

  !> @brief Clears the memory container
  !!
  !<
  subroutine clear(this)
    ! -- dummy
    class(MemoryListType) :: this

    call this%container%clear()
  end subroutine clear

end module MemoryListModule

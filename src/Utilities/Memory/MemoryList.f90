module MemoryListModule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENCOMPONENTNAME
  use STLVecIntModule
  use MemoryTypeModule, only: MemoryType
  use MemoryHelperModule, only: split_mem_path
  use ListModule, only: ListType
  private
  public :: MemoryListType

  ! internal type to manage a faster lookup of memory items
  type, private :: NamedMemSegmentType
    character(len=LENCOMPONENTNAME) :: comp_name !< the name of the component for which this memory segment exists
    type(STLVecInt) :: mem_indexes !< all indexes into the main memory list, for this particular component
  end type NamedMemSegmentType

  type :: MemoryListType
    type(ListType), private :: segments !< a list with memory segments grouped by component name
    type(ListType), private :: list !< the full list of memory entries
  contains
    procedure :: add
    generic :: get => get_by_idx, get_by_address
    procedure :: count
    procedure :: clear
    procedure :: remove
    ! private
    procedure, private :: get_by_idx
    procedure, private :: get_by_address
    procedure, private :: get_mem_segment
  end type MemoryListType

contains

  subroutine add(this, mt)
    class(MemoryListType) :: this
    type(MemoryType), pointer :: mt
    ! local
    class(*), pointer :: obj => null()
    character(len=LENCOMPONENTNAME) :: comp_name
    character(len=LENCOMPONENTNAME) :: subcomp_name
    type(NamedMemSegmentType), pointer :: mem_segment

    ! add memory item
    obj => mt
    call this%list%add(obj)

    ! add item to lookup structure
    call split_mem_path(mt%path, comp_name, subcomp_name)
    mem_segment => this%get_mem_segment(comp_name)
    if (.not. associated(mem_segment)) then
      ! create and add
      allocate (mem_segment)
      mem_segment%comp_name = comp_name
      call mem_segment%mem_indexes%init()
      obj => mem_segment
      call this%segments%Add(obj)
    end if

    ! add index (of the last item)
    call mem_segment%mem_indexes%push_back(this%list%Count())

  end subroutine add

  !> @brief Fast, unsafe access by index into the list.
  !<
  function get_by_idx(this, ipos) result(res)
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
  end function get_by_idx

  !> @Brief Return the memory data item for the memory
  !! path and variable name. Returns null when not found.
  !! This method is faster than getting by list index because
  !< it will use the lookup data structure for components.
  function get_by_address(this, mem_path, var_name) result(mt)
    class(MemoryListType) :: this
    character(len=*) :: mem_path
    character(len=*) :: var_name
    type(MemoryType), pointer :: mt
    ! local
    integer(I4B) :: i, m_idx
    character(len=LENCOMPONENTNAME) :: comp_name
    character(len=LENCOMPONENTNAME) :: subcomp_name
    type(NamedMemSegmentType), pointer :: mem_segment
    type(MemoryType), pointer :: mt_try

    mt => null()
    mt_try => null()

    call split_mem_path(mem_path, comp_name, subcomp_name)
    mem_segment => this%get_mem_segment(comp_name)
    if (.not. associated(mem_segment)) then
      return
    end if

    do i = 1, mem_segment%mem_indexes%size
      m_idx = mem_segment%mem_indexes%at(i)
      mt_try => this%get(m_idx)
      if (mt_try%name == var_name .and. mt_try%path == mem_path) then
        mt => mt_try
        return
      end if
    end do

  end function get_by_address

  !> @Brief get the memory segment for this component.
  !< Returns null when not found.
  function get_mem_segment(this, comp_name) result(mem_segment)
    class(MemoryListType) :: this
    character(len=*) :: comp_name
    type(NamedMemSegmentType), pointer :: mem_segment
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj

    mem_segment => null()
    do i = 1, this%segments%Count()
      obj => this%segments%GetItem(i)
      select type (obj)
      class is (NamedMemSegmentType)
        if (obj%comp_name == comp_name) then
          mem_segment => obj
          return
        end if
      end select
    end do

  end function

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

module DistributedBaseModule
  use KindModule, only: I4B, LGP, DP
  use ConstantsModule, only: LENCOMPONENTNAME, LENMEMPATH
  use CharacterStringModule  
  use MemoryTypeModule, only: MemoryType
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: get_from_memorylist
  use ListModule
  use RemoteMemoryModule
  implicit none
  private

  character(len=7), public, parameter :: LOCAL_MEM_CTX = '__LOC__'
  
  type, public :: DistributedBaseType
    integer(I4B) :: id !< universal (global) identifier: id of the component
    character(len=LENCOMPONENTNAME) :: name !< component name
    character(len=3) :: macronym !< model acronym, e.g. GWT

    logical(LGP) :: is_local

    
    type(ListType) :: remote_mem_items
    integer(I4B), dimension(:), pointer :: src_map_node
    integer(I4B), dimension(:), pointer :: src_map_conn
  contains    
    procedure :: add_remote_mem
    generic :: load => load_intsclr, load_int1d, load_dblsclr, &
                       load_double1d, load_double2d, load_charstr1d
                       
    procedure :: get_rmt_mem
    procedure :: destroy

    ! private
    procedure, private :: load_intsclr
    procedure, private :: load_int1d
    procedure, private :: load_dblsclr
    procedure, private :: load_double1d
    procedure, private :: load_double2d
    procedure, private :: load_charstr1d
  end type DistributedBaseType

contains

subroutine add_remote_mem(this, var_name, component, subcomponent, stage, map_type)
  class(DistributedBaseType) :: this
  character(len=*) :: var_name
  character(len=*) :: component
  character(len=*) :: subcomponent
  integer(I4B) :: stage
  integer(I4B) :: map_type
  ! local
  character(len=LENMEMPATH) :: local_mem_path
  character(len=LENMEMPATH) :: remote_mem_path
  type(MemoryType), pointer :: local_mt
  logical(LGP) :: found
  class(RemoteMemoryType), pointer :: remote_mt
  class(*), pointer :: obj
  
  if (subcomponent == '') then    
    local_mem_path = create_mem_path(component, context=LOCAL_MEM_CTX)
    remote_mem_path = create_mem_path(component)
  else
    local_mem_path = create_mem_path(component, subcomponent, context=LOCAL_MEM_CTX)
    remote_mem_path = create_mem_path(component, subcomponent)
  end if

  call get_from_memorylist(var_name, local_mem_path, local_mt, found)

  allocate(remote_mt)
  remote_mt%var_name = var_name
  remote_mt%mem_path = remote_mem_path
  remote_mt%stage = stage
  remote_mt%map_type = map_type
  remote_mt%local_mt => local_mt

  obj => remote_mt
  call this%remote_mem_items%Add(obj)

end subroutine add_remote_mem

subroutine load_intsclr(this, intsclr, var_name, subcomp_name)
  class(DistributedBaseType) :: this
  integer(I4B), pointer :: intsclr
  character(len=*) :: var_name
  character(len=*), optional :: subcomp_name
  ! local
  type(MemoryType), pointer :: mt
  logical(LGP) :: found
  character(len=LENMEMPATH) :: mem_path

  if (present(subcomp_name)) then
    mem_path = create_mem_path(this%name, subcomp_name)
  else
    mem_path = create_mem_path(this%name)
  end if

  call get_from_memorylist(var_name, mem_path, mt, found)
  intsclr => mt%intsclr

end subroutine load_intsclr

subroutine load_int1d(this, aint1d, var_name, subcomp_name)
  class(DistributedBaseType) :: this
  integer(I4B), dimension(:), pointer, contiguous :: aint1d
  character(len=*) :: var_name
  character(len=*), optional :: subcomp_name
  ! local
  character(len=LENMEMPATH) :: mem_path
  type(MemoryType), pointer :: mt
  logical(LGP) :: found

  if (present(subcomp_name)) then
    mem_path = create_mem_path(this%name, subcomp_name)
  else
    mem_path = create_mem_path(this%name)
  end if

  call get_from_memorylist(var_name, mem_path, mt, found)
  aint1d => mt%aint1d

end subroutine load_int1d

subroutine load_dblsclr(this, dblsclr, var_name, subcomp_name)
  class(DistributedBaseType) :: this
  real(DP), pointer :: dblsclr
  character(len=*) :: var_name
  character(len=*), optional :: subcomp_name
  ! local
  type(MemoryType), pointer :: mt
  logical(LGP) :: found
  character(len=LENMEMPATH) :: mem_path

  if (present(subcomp_name)) then
    mem_path = create_mem_path(this%name, subcomp_name)
  else
    mem_path = create_mem_path(this%name)
  end if

  call get_from_memorylist(var_name, mem_path, mt, found)
  dblsclr => mt%dblsclr

end subroutine load_dblsclr

subroutine load_double1d(this, adbl1d, var_name, subcomp_name)
  class(DistributedBaseType) :: this
  real(DP), dimension(:), pointer, contiguous :: adbl1d
  character(len=*) :: var_name
  character(len=*), optional :: subcomp_name
  ! local
  character(len=LENMEMPATH) :: mem_path
  type(MemoryType), pointer :: mt
  logical(LGP) :: found

  if (present(subcomp_name)) then
    mem_path = create_mem_path(this%name, subcomp_name)
  else
    mem_path = create_mem_path(this%name)
  end if

  call get_from_memorylist(var_name, mem_path, mt, found)
  adbl1d => mt%adbl1d

end subroutine load_double1d

subroutine load_double2d(this, adbl2d, var_name, subcomp_name)
  class(DistributedBaseType) :: this
  real(DP), dimension(:,:), pointer, contiguous :: adbl2d
  character(len=*) :: var_name
  character(len=*), optional :: subcomp_name
  ! local
  character(len=LENMEMPATH) :: mem_path
  type(MemoryType), pointer :: mt
  logical(LGP) :: found

  if (present(subcomp_name)) then
    mem_path = create_mem_path(this%name, subcomp_name)
  else
    mem_path = create_mem_path(this%name)
  end if

  call get_from_memorylist(var_name, mem_path, mt, found)
  adbl2d => mt%adbl2d

end subroutine load_double2d

subroutine load_charstr1d(this, acharstr1d, var_name, subcomp_name)
  class(DistributedBaseType) :: this
  type(CharacterStringType), dimension(:), pointer :: acharstr1d
  character(len=*) :: var_name
  character(len=*), optional :: subcomp_name
  ! local
  character(len=LENMEMPATH) :: mem_path
  type(MemoryType), pointer :: mt
  logical(LGP) :: found

  if (present(subcomp_name)) then
    mem_path = create_mem_path(this%name, subcomp_name)
  else
    mem_path = create_mem_path(this%name)
  end if

  call get_from_memorylist(var_name, mem_path, mt, found)
  acharstr1d => mt%acharstr1d

end subroutine load_charstr1d

subroutine destroy(this)
  class(DistributedBaseType) :: this

  call this%remote_mem_items%Clear()
  
end subroutine destroy

function get_rmt_mem(this, idx) result(remote_mem)
  class(DistributedBaseType) :: this
  integer(I4B) :: idx
  class(RemoteMemoryType), pointer :: remote_mem
  class(*), pointer :: obj

  remote_mem => null()
  obj => this%remote_mem_items%GetItem(idx)
  select type(obj)
  class is (RemoteMemoryType)
    remote_mem => obj
  end select

end function get_rmt_mem

end module DistributedBaseModule
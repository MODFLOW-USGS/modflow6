module DistributedBaseModule
  use KindModule, only: I4B, LGP, DP
  use ConstantsModule, only: LENCOMPONENTNAME, LENMEMPATH
  use CharacterStringModule  
  use MemoryTypeModule, only: MemoryType
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: get_from_memorylist, mem_allocate
  use ListModule
  use RemoteMemoryModule
  implicit none
  private

  character(len=7), public, parameter :: LOCAL_MEM_CTX = '__LOC__'
  
  integer(I4B), public, parameter :: MAP_TYPE_NA = 1
  integer(I4B), public, parameter :: MAP_TYPE_NODE = 2
  integer(I4B), public, parameter :: MAP_TYPE_CONN = 3
  
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
    procedure, private :: get_local_mem_path
  end type DistributedBaseType

contains

subroutine add_remote_mem(this, var_name, component, subcomponent, stages, map_type)
  class(DistributedBaseType) :: this
  character(len=*) :: var_name
  character(len=*) :: component
  character(len=*) :: subcomponent
  integer(I4B), dimension(:) :: stages
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
  remote_mt%stages = stages
  remote_mt%map_type = map_type
  remote_mt%local_mt => local_mt

  obj => remote_mt
  call this%remote_mem_items%Add(obj)

end subroutine add_remote_mem

subroutine load_intsclr(this, intsclr, var_name, subcomp_name, sync_stages, map_type)
  class(DistributedBaseType) :: this
  integer(I4B), pointer :: intsclr
  character(len=*) :: var_name
  character(len=*) :: subcomp_name
  integer(I4B), dimension(:) :: sync_stages
  integer(I4B) :: map_type
  ! local
  type(MemoryType), pointer :: mt
  logical(LGP) :: found
  character(len=LENMEMPATH) :: mem_path

  mem_path = this%get_local_mem_path(subcomp_name)
  if (this%is_local) then
    call get_from_memorylist(var_name, mem_path, mt, found)
    intsclr => mt%intsclr
  else
    call mem_allocate(intsclr, var_name, mem_path)
    call this%add_remote_mem(var_name, this%name, subcomp_name, sync_stages, map_type)
  end if

end subroutine load_intsclr

subroutine load_int1d(this, aint1d, nrow, var_name, subcomp_name, sync_stages, map_type)
  class(DistributedBaseType) :: this
  integer(I4B), dimension(:), pointer, contiguous :: aint1d
  integer(I4B) :: nrow
  character(len=*) :: var_name
  character(len=*) :: subcomp_name
  integer(I4B), dimension(:) :: sync_stages
  integer(I4B) :: map_type
  ! local
  character(len=LENMEMPATH) :: mem_path
  type(MemoryType), pointer :: mt
  logical(LGP) :: found

  mem_path = this%get_local_mem_path(subcomp_name)
  if (this%is_local) then
    call get_from_memorylist(var_name, mem_path, mt, found)
    aint1d => mt%aint1d
  else
    call mem_allocate(aint1d, nrow, var_name, mem_path)
    call this%add_remote_mem(var_name, this%name, subcomp_name, sync_stages, map_type)
  end if

end subroutine load_int1d

subroutine load_dblsclr(this, dblsclr, var_name, subcomp_name, sync_stages, map_type)
  class(DistributedBaseType) :: this
  real(DP), pointer :: dblsclr
  character(len=*) :: var_name
  character(len=*) :: subcomp_name
  integer(I4B), dimension(:) :: sync_stages
  integer(I4B) :: map_type
  ! local
  type(MemoryType), pointer :: mt
  logical(LGP) :: found
  character(len=LENMEMPATH) :: mem_path

  mem_path = this%get_local_mem_path(subcomp_name)
  if (this%is_local) then
    call get_from_memorylist(var_name, mem_path, mt, found)
    dblsclr => mt%dblsclr
  else
    call mem_allocate(dblsclr, var_name, mem_path)
    call this%add_remote_mem(var_name, this%name, subcomp_name, sync_stages, map_type)
  end if

end subroutine load_dblsclr

subroutine load_double1d(this, adbl1d, nrow, var_name, subcomp_name, sync_stages, map_type)
  class(DistributedBaseType) :: this
  real(DP), dimension(:), pointer, contiguous :: adbl1d
  integer(I4B) :: nrow
  character(len=*) :: var_name
  character(len=*) :: subcomp_name
  integer(I4B), dimension(:) :: sync_stages
  integer(I4B) :: map_type
  ! local
  character(len=LENMEMPATH) :: mem_path
  type(MemoryType), pointer :: mt
  logical(LGP) :: found

  mem_path = this%get_local_mem_path(subcomp_name)
  if (this%is_local) then
    call get_from_memorylist(var_name, mem_path, mt, found)
    adbl1d => mt%adbl1d
  else
    call mem_allocate(adbl1d, nrow, var_name, mem_path)
    call this%add_remote_mem(var_name, this%name, subcomp_name, sync_stages, map_type)
  end if

end subroutine load_double1d

subroutine load_double2d(this, adbl2d, nrow, ncol, var_name, subcomp_name, sync_stages, map_type)
  class(DistributedBaseType) :: this
  real(DP), dimension(:,:), pointer, contiguous :: adbl2d
  integer(I4B) :: nrow
  integer(I4B) :: ncol
  character(len=*) :: var_name
  character(len=*) :: subcomp_name
  integer(I4B), dimension(:) :: sync_stages
  integer(I4B) :: map_type
  ! local
  character(len=LENMEMPATH) :: mem_path
  type(MemoryType), pointer :: mt
  logical(LGP) :: found

  mem_path = this%get_local_mem_path(subcomp_name)
  if (this%is_local) then
    call get_from_memorylist(var_name, mem_path, mt, found)
    adbl2d => mt%adbl2d
  else
    call mem_allocate(adbl2d, nrow, ncol, var_name, mem_path)
    call this%add_remote_mem(var_name, this%name, subcomp_name, sync_stages, map_type)
  end if

end subroutine load_double2d

subroutine load_charstr1d(this, acharstr1d, ilen, nrow, var_name, subcomp_name, sync_stages, map_type)
  class(DistributedBaseType) :: this
  type(CharacterStringType), dimension(:), pointer, contiguous :: acharstr1d
  integer(I4B) :: ilen
  integer(I4B) :: nrow
  character(len=*) :: var_name
  character(len=*) :: subcomp_name
  integer(I4B), dimension(:) :: sync_stages
  integer(I4B) :: map_type
  ! local
  character(len=LENMEMPATH) :: mem_path
  type(MemoryType), pointer :: mt
  logical(LGP) :: found

  mem_path = this%get_local_mem_path(subcomp_name)
  if (this%is_local) then
    call get_from_memorylist(var_name, mem_path, mt, found)
    acharstr1d => mt%acharstr1d
  else
    call mem_allocate(acharstr1d, ilen, nrow, var_name, mem_path)
    call this%add_remote_mem(var_name, this%name, subcomp_name, sync_stages, map_type)
  end if

end subroutine load_charstr1d

function get_local_mem_path(this, subcomp_name) result(loc_mem_path)
  class(DistributedBaseType) :: this
  character(len=*) :: subcomp_name
  character(len=LENMEMPATH) :: loc_mem_path

  if (this%is_local) then
    if (subcomp_name == '') then
      loc_mem_path = create_mem_path(this%name)
    else 
      loc_mem_path = create_mem_path(this%name, subcomp_name)
    end if
  else
    if (subcomp_name == '') then
      loc_mem_path = create_mem_path(this%name, LOCAL_MEM_CTX)
    else 
      loc_mem_path = create_mem_path(this%name, subcomp_name, LOCAL_MEM_CTX)
    end if
  end if

end function get_local_mem_path

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
module RemoteMemoryModule
  use KindModule, only: I4B
  use ConstantsModule, only: LENVARNAME, LENMEMPATH
  use MemoryTypeModule, only: MemoryType
  implicit none
  private
  
  type, public :: RemoteMemoryType
    character(len=LENVARNAME) :: var_name
    character(len=LENMEMPATH) :: mem_path
    integer(I4B) :: stage
    integer(I4B) :: map_type
    type(MemoryType), pointer :: local_mt
  end type RemoteMemoryType

end module RemoteMemoryModule
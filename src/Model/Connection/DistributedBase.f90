module DistributedBaseModule
  use KindModule, only: I4B, LGP, DP
  use ConstantsModule, only: LENCOMPONENTNAME, LENMEMPATH
  use CharacterStringModule
  use MemoryTypeModule, only: MemoryType
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: get_from_memorylist
  implicit none
  private

  type, public :: DistributedBaseType
    integer(I4B) :: id !< universal (global) identifier: id of the component
    character(len=LENCOMPONENTNAME) :: name !< component name
  contains  
    generic :: load => load_intsclr, load_int1d, load_dblsclr, &
                       load_double1d, load_double2d, load_charstr1d

    ! private
    procedure, private :: load_intsclr
    procedure, private :: load_int1d
    procedure, private :: load_dblsclr
    procedure, private :: load_double1d
    procedure, private :: load_double2d
    procedure, private :: load_charstr1d
  end type DistributedBaseType

contains

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

end module DistributedBaseModule
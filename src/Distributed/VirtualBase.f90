module VirtualBaseModule
  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: LENVARNAME, LENMEMPATH
  use MemoryTypeModule, only: MemoryType
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, &
                                 get_from_memorylist
  implicit none
  private

  !> This is a generic data structure to virtualize pieces
  !! of memory in 2 distinct ways:
  !!
  !!  1) Virtualize remote memory
  !!  This concerns memory residing on another process.
  !!  Typically, these pieces are subsets of certain model 
  !!  and exchange data and lookup tables are kept with the 
  !!  data to manage their mapping. The stage(s) at which
  !!  to synchronize the virtual memory is stored as well.
  !!
  !!  2) Virtualize local memory
  !!  In this case no virtual memory item is created, no
  !!  lookup tables and synchronization are necessary. 
  !!  The virtual memory item will be pointed to the
  !!  original memory location at the requested
  !!  synchronization stage.
  !<
  type, abstract, public :: VirtualDataType
    logical(LGP) :: is_remote !< is remote memory, when true
    character(len=LENVARNAME) :: remote_var_name !< remote variable name
    character(len=LENMEMPATH) :: remote_mem_path !< remote memory path
    integer(I4B), dimension(:), allocatable :: sync_stages !< stage(s) at which to synchronize
    integer(I4B) :: map_type !< the type of map
    integer(I4B), dimension(:), pointer, contiguous :: virtual_to_remote !< contiguous list which maps virtual index to remote
    integer(I4B), dimension(:), pointer, contiguous :: remote_to_virtual !< sparse list which maps remote index to virtual
    type(MemoryType), pointer :: virtual_mt
  contains
    procedure(allocate_vmem_if), deferred :: allocate_vmem
    procedure(deallocate_vmem_if), deferred :: deallocate_vmem
    procedure :: to_base => to_base_vmem
  end type

  integer(I4B), public, parameter :: MAP_ALL_TYPE = 1
  integer(I4B), public, parameter :: MAP_NODE_TYPE = 2
  integer(I4B), public, parameter :: MAP_CONN_TYPE = 3
  
  type, public, extends(VirtualDataType) :: VirtualIntType
    integer(I4B), pointer :: value
  contains
    procedure :: allocate_vmem => allocate_vmem_int
    procedure :: deallocate_vmem => deallocate_vmem_int
  end type

  type, public, extends(VirtualDataType) :: VirtualInt1dType
    integer(I4B), private, dimension(:), pointer, contiguous :: values
  contains
    procedure :: allocate_vmem => allocate_vmem_int1d
    procedure :: deallocate_vmem => deallocate_vmem_int1d
    procedure :: at => at_int1d
  end type

  type, public, extends(VirtualDataType) :: VirtualDblType
    real(DP), pointer :: value
  contains
    procedure :: allocate_vmem => allocate_vmem_dbl
    procedure :: deallocate_vmem => deallocate_vmem_dbl
  end type

  type, public, extends(VirtualDataType) :: VirtualDbl1dType
    real(DP), private, dimension(:), pointer, contiguous :: values
  contains
    procedure :: allocate_vmem => allocate_vmem_dbl1d
    procedure :: deallocate_vmem => deallocate_vmem_dbl1d
    procedure :: at => at_dbl1d
  end type

  type, public, extends(VirtualDataType) :: VirtualDbl2dType
    real(DP), private, dimension(:,:), pointer, contiguous :: values
  contains
    procedure :: allocate_vmem => allocate_vmem_dbl2D
    procedure :: deallocate_vmem => deallocate_vmem_dbl2D
    procedure :: at => at_dbl2d
  end type

  ! etc... 
  abstract interface
    subroutine allocate_vmem_if(this, var_name, mem_path, shape)
      import VirtualDataType, I4B
      class(VirtualDataType) :: this
      character(len=*) :: var_name
      character(len=*) :: mem_path
      integer(I4B), dimension(:) :: shape
    end subroutine allocate_vmem_if
    subroutine deallocate_vmem_if(this)
      import VirtualDataType
      class(VirtualDataType) :: this
    end subroutine deallocate_vmem_if
  end interface

contains

  function to_base_vmem(this) result(base_ptr)
    class(VirtualDataType), target :: this    
    class(VirtualDataType), pointer :: base_ptr

    base_ptr => this

  end function to_base_vmem

  subroutine allocate_vmem_int(this, var_name, mem_path, shape)
    class(VirtualIntType) :: this
    character(len=*) :: var_name
    character(len=*) :: mem_path
    integer(I4B), dimension(:) :: shape
    ! local
    integer(I4B), pointer :: intscl

    call mem_allocate(intscl, var_name, mem_path)

  end subroutine allocate_vmem_int

  subroutine deallocate_vmem_int(this)
    class(VirtualIntType) :: this

    call mem_deallocate(this%virtual_mt%intsclr)

  end subroutine deallocate_vmem_int

  subroutine allocate_vmem_int1d(this, var_name, mem_path, shape)
    class(VirtualInt1dType) :: this
    character(len=*) :: var_name
    character(len=*) :: mem_path
    integer(I4B), dimension(:) :: shape
    ! local
    integer(I4B), dimension(:), pointer, contiguous :: int1d

    call mem_allocate(int1d, shape(1), var_name, mem_path)

  end subroutine allocate_vmem_int1d

  subroutine deallocate_vmem_int1d(this)
    class(VirtualInt1dType) :: this

    call mem_deallocate(this%virtual_mt%aint1d)

  end subroutine deallocate_vmem_int1d

  subroutine allocate_vmem_dbl(this, var_name, mem_path, shape)
    class(VirtualDblType) :: this
    character(len=*) :: var_name
    character(len=*) :: mem_path
    integer(I4B), dimension(:) :: shape
    ! local
    real(DP), pointer :: dbl

    call mem_allocate(dbl, var_name, mem_path)

  end subroutine allocate_vmem_dbl

  subroutine deallocate_vmem_dbl(this)
    class(VirtualDblType) :: this

    call mem_deallocate(this%virtual_mt%dblsclr)

  end subroutine deallocate_vmem_dbl

  subroutine allocate_vmem_dbl1d(this, var_name, mem_path, shape)
    class(VirtualDbl1dType) :: this
    character(len=*) :: var_name
    character(len=*) :: mem_path
    integer(I4B), dimension(:) :: shape
    ! local
    real(DP), dimension(:), pointer, contiguous :: dbl1d

    call mem_allocate(dbl1d, shape(1), var_name, mem_path)

  end subroutine allocate_vmem_dbl1d

  subroutine deallocate_vmem_dbl1d(this)
    class(VirtualDbl1dType) :: this

    call mem_deallocate(this%virtual_mt%adbl1d)

  end subroutine deallocate_vmem_dbl1d

  subroutine allocate_vmem_dbl2d(this, var_name, mem_path, shape)
    class(VirtualDbl2dType) :: this
    character(len=*) :: var_name
    character(len=*) :: mem_path
    integer(I4B), dimension(:) :: shape
    ! local
    real(DP), dimension(:,:), pointer, contiguous :: dbl2d

    call mem_allocate(dbl2d, shape(1), shape(2), var_name, mem_path)

  end subroutine allocate_vmem_dbl2d

  subroutine deallocate_vmem_dbl2d(this)
    class(VirtualDbl2dType) :: this

    call mem_deallocate(this%virtual_mt%adbl2d)

  end subroutine deallocate_vmem_dbl2d

  function at_int1d(this, i_rmt) result(val)
    class(VirtualInt1dType) :: this
    integer(I4B) :: i_rmt
    integer(I4B) :: val
    ! local
    integer(I4B) :: i_vrt

    i_vrt = this%remote_to_virtual(i_rmt)
    val = this%virtual_mt%aint1d(i_vrt)

  end function at_int1d

  function at_dbl1d(this, i_rmt) result(val)
    class(VirtualDbl1dType) :: this
    integer(I4B) :: i_rmt
    real(DP) :: val
    ! local
    integer(I4B) :: i_vrt

    i_vrt = this%remote_to_virtual(i_rmt)
    val = this%virtual_mt%adbl1d(i_vrt)

  end function at_dbl1d

  function at_dbl2d(this, i_rmt, j_cmp) result(val)
    class(VirtualDbl2dType) :: this
    integer(I4B) :: i_rmt
    integer(I4B) :: j_cmp
    real(DP) :: val
    ! local
    integer(I4B) :: i_vrt

    i_vrt = this%remote_to_virtual(i_rmt)
    val = this%virtual_mt%adbl2d(j_cmp, i_vrt)

  end function at_dbl2d

end module VirtualBaseModule
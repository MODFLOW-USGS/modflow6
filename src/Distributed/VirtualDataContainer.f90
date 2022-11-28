module VirtualDataContainerModule
  use VirtualBaseModule
  use ListModule
  use KindModule, only: I4B, LGP
  use ConstantsModule, only: LENCOMPONENTNAME, LENMEMPATH
  use MemoryHelperModule, only: create_mem_path
  implicit none
  private

  type, public :: VirtualDataContainerType
    type(ListType) :: virtual_data_list
    logical(LGP) :: is_remote
    character(LENCOMPONENTNAME) :: name
  contains
    ! protected
    generic :: map => map_scalar, map_array1d, map_array2d
    ! private
    procedure, private :: map_scalar
    procedure, private :: map_array1d
    procedure, private :: map_array2d
    procedure, private :: map_internal    
    procedure, private :: add_to_list    
  end type VirtualDataContainerType

contains

  subroutine map_scalar(this, vdata, var_name, subcomp_name, stages, map_type)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: vdata
    character(len=*) :: var_name
    character(len=*) :: subcomp_name
    integer(I4B), dimension(:) :: stages
    integer(I4B) :: map_type

    call this%map_internal(vdata, var_name, subcomp_name, (/ 0 /), stages, map_type)

  end subroutine map_scalar

  subroutine map_array1d(this, vdata, var_name, subcomp_name, nrow, stages, map_type)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: vdata
    character(len=*) :: var_name
    character(len=*) :: subcomp_name
    integer(I4B) :: nrow
    integer(I4B), dimension(:) :: stages
    integer(I4B) :: map_type

    call this%map_internal(vdata, var_name, subcomp_name, (/ nrow /), stages, map_type)

  end subroutine map_array1d

  subroutine map_array2d(this, vdata, var_name, subcomp_name, ncol, nrow, stages, map_type)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: vdata
    character(len=*) :: var_name
    character(len=*) :: subcomp_name
    integer(I4B) :: ncol
    integer(I4B) :: nrow
    integer(I4B), dimension(:) :: stages
    integer(I4B) :: map_type

    call this%map_internal(vdata, var_name, subcomp_name, (/ ncol, nrow /), stages, map_type)

  end subroutine map_array2d

  subroutine map_internal(this, vdata, var_name, subcomp_name, shape, stages, map_type)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: vdata
    character(len=*) :: var_name
    character(len=*) :: subcomp_name
    integer(I4B), dimension(:) :: shape
    integer(I4B), dimension(:) :: stages
    integer(I4B) :: map_type
    ! local
    character(len=LENMEMPATH) :: virtual_mem_path
    logical(LGP) :: found

    vdata%remote_var_name = var_name
    if (subcomp_name == '') then
      vdata%remote_mem_path = create_mem_path(this%name)
    else
      vdata%remote_mem_path = create_mem_path(this%name, subcomp_name)
    end if
    vdata%sync_stages = stages
    vdata%map_type = map_type
    vdata%remote_to_virtual => null()
    vdata%virtual_to_remote => null()
    vdata%virtual_mt => null()

    if (this%is_remote) then
      ! create new virtual memory item
      if (subcomp_name == '') then
        virtual_mem_path = create_mem_path(this%name, context='__VIRTUAL__')
      else
        virtual_mem_path = create_mem_path(this%name, subcomp_name, context='__VIRTUAL__')
      end if
      call vdata%allocate_vmem(var_name, virtual_mem_path, shape)
      call get_from_memorylist(var_name, virtual_mem_path, vdata%virtual_mt, found)
    end if

    call this%add_to_list(vdata)

  end subroutine map_internal

  subroutine add_to_list(this, virtual_data)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: virtual_data
    ! local
    class(*), pointer :: vdata_ptr

    vdata_ptr => virtual_data
    call this%virtual_data_list%Add(vdata_ptr)

  end subroutine add_to_list

end module VirtualDataContainerModule
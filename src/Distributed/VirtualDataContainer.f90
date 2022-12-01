module VirtualDataContainerModule
  use VirtualBaseModule
  use ListModule
  use KindModule, only: I4B, LGP
  use ConstantsModule, only: LENCOMPONENTNAME, LENMEMPATH, LENCONTEXTNAME
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: get_from_memorylist
  implicit none
  private

  type, public :: VirtualDataContainerType
    type(ListType) :: virtual_data_list
    character(LENCOMPONENTNAME) :: name    
    integer(I4B) :: id
    character(LENCONTEXTNAME) :: vmem_ctx !< prefixes virtual memory located on remote processes
    logical(LGP) :: is_remote
  contains
    procedure :: vdc_create
    generic :: map => map_scalar, map_array1d, map_array2d
    procedure :: prepare_stage => vdc_prepare_stage
    procedure :: link_items => vdc_link_items
    procedure :: get_vrt_mem_path => vdc_get_vrt_mem_path
    procedure :: destroy => vdc_destroy
    ! private
    procedure, private :: map_scalar
    procedure, private :: map_array1d
    procedure, private :: map_array2d
    procedure, private :: map_internal    
    procedure, private :: add_to_list
  end type VirtualDataContainerType

contains

  subroutine vdc_create(this, name, id, is_remote)    
    use SimVariablesModule, only: proc_id
    class(VirtualDataContainerType) :: this
    character(len=*) :: name
    integer(I4B) :: id
    logical(LGP) :: is_remote

    this%name = name
    this%id = id
    this%is_remote = is_remote
    if (is_remote) then
      write(this%vmem_ctx, '(a,i0,a)') '__P', proc_id, '__'
    else
      this%vmem_ctx = 'undefined'
    end if

  end subroutine vdc_create

  subroutine vdc_prepare_stage(this, stage)
    use SimModule, only: ustop
    class(VirtualDataContainerType) :: this
    integer(I4B) :: stage

    write(*,*) 'Error: prepare_stage should be overridden'
    call ustop()

  end subroutine vdc_prepare_stage

  subroutine vdc_link_items(this, stage)
    class(VirtualDataContainerType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i
    class(*), pointer :: vdi

    do i = 1, this%virtual_data_list%Count()
      vdi => this%virtual_data_list%GetItem(i)
      select type (vdi)
        class is (VirtualDataType)
          if (vdi%check_stage(stage)) call vdi%link()
      end select
    end do

  end subroutine vdc_link_items

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

    vdata%is_remote = this%is_remote
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
      call vdata%vm_allocate(var_name, virtual_mem_path, shape)
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

  function vdc_get_vrt_mem_path(this, subcomp_name) result(vrt_path)
    class(VirtualDataContainerType) :: this
    character(len=*) :: subcomp_name
    character(len=LENMEMPATH) :: vrt_path
  
    if (this%is_remote) then
      ! remote so create memory with context: '__PXX__' for process XX
      if (subcomp_name == '') then
        vrt_path = create_mem_path(this%name, this%vmem_ctx)
      else 
        vrt_path = create_mem_path(this%name, subcomp_name, this%vmem_ctx)
      end if
    else
      if (subcomp_name == '') then
        vrt_path = create_mem_path(this%name)
      else
        vrt_path = create_mem_path(this%name, subcomp_name)
      end if
    end if
  
  end function vdc_get_vrt_mem_path

  subroutine vdc_destroy(this)
    class(VirtualDataContainerType) :: this
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj

    do i = 1, this%virtual_data_list%Count()
      obj => this%virtual_data_list%GetItem(i)
      select type (obj)
      class is (VirtualDataType)
        call obj%vm_deallocate()
      end select
    end do

  end subroutine vdc_destroy

end module VirtualDataContainerModule
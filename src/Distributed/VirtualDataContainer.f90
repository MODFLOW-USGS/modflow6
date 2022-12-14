module VirtualDataContainerModule
  use VirtualBaseModule
  use ListModule
  use KindModule, only: I4B, LGP
  use STLVecIntModule
  use ConstantsModule, only: LENCOMPONENTNAME, LENMEMPATH, LENCONTEXTNAME
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: get_from_memorylist
  implicit none
  private

  public :: get_vdc_from_list

  integer(I4B), public, parameter :: VDC_UNKNOWN_TYPE = 0
  integer(I4B), public, parameter :: VDC_GWFMODEL_TYPE = 1
  integer(I4B), public, parameter :: VDC_GWTMODEL_TYPE = 2
  integer(I4B), public, parameter :: VDC_GWFEXG_TYPE = 3
  integer(I4B), public, parameter :: VDC_GWTEXG_TYPE = 4
  integer(I4B), public, parameter :: VDC_GWFMVR_TYPE = 5
  integer(I4B), public, parameter :: VDC_GWTMVT_TYPE = 6
 
  type, public :: VdcPtrType
    class(VirtualDataContainerType), pointer :: ptr => null()
  end type VdcPtrType

  type, public :: VirtualDataContainerType   
    integer(I4B) :: id !< unique identifier matching with the real counterpart
    integer(I4B) :: container_type !< to identify the actual type of this container
    character(LENCOMPONENTNAME) :: name !< container name (model, exchange, ...) used in the memory path
    character(LENCONTEXTNAME) :: vmem_ctx !< prefixes virtual memory located on remote processes
    logical(LGP) :: is_local !< when true, the physical object resides on the same process. However,
                             !! some of its variables can still be remote
    logical(LGP) :: is_active !< when true, this container is being synchronized
    integer(I4B) :: orig_rank !< the global rank of the process which holds the physical data for this container
    
    type(ListType) :: virtual_data_list !< a list with all virtual data items for this container 
  contains
    procedure :: vdc_create
    generic :: map => map_scalar, map_array1d, map_array2d
    procedure :: prepare_stage => vdc_prepare_stage
    procedure :: link_items => vdc_link_items
    procedure :: get_vrt_mem_path => vdc_get_vrt_mem_path
    procedure :: destroy => vdc_destroy
    procedure :: set_orig_rank => vdc_set_orig_rank
    procedure :: get_send_items => vdc_get_send_items
    procedure :: get_recv_items => vdc_get_recv_items
    ! protected
    procedure :: create_field
    ! private
    procedure, private :: add_to_list
    procedure, private :: map_scalar
    procedure, private :: map_array1d
    procedure, private :: map_array2d
    procedure, private :: map_internal 
    procedure, private :: vdc_get_virtual_data
    procedure, private :: get_items_for_stage
  end type VirtualDataContainerType

contains

  subroutine vdc_create(this, name, id, is_local)
    class(VirtualDataContainerType) :: this
    character(len=*) :: name
    integer(I4B) :: id
    logical(LGP) :: is_local

    this%name = name
    this%id = id
    this%is_local = is_local
    this%vmem_ctx = 'undefined'
    this%orig_rank = 0
    this%is_active = .true.
    this%container_type = VDC_UNKNOWN_TYPE

  end subroutine vdc_create  

  subroutine create_field(this, field, var_name, subcmp_name, is_local)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: field
    character(len=*) :: var_name
    character(len=*) :: subcmp_name
    logical(LGP), optional :: is_local

    field%is_remote = .not. this%is_local
    if (present(is_local)) field%is_remote = .not. is_local
    field%var_name = var_name
    field%subcmp_name = subcmp_name
    if (subcmp_name == '') then
      field%mem_path = create_mem_path(this%name)
    else
      field%mem_path = create_mem_path(this%name, subcmp_name)
    end if
    field%virtual_to_remote => null()
    field%remote_to_virtual => null()
    field%virtual_mt => null()
    call this%add_to_list(field)

  end subroutine create_field  

  subroutine add_to_list(this, virtual_data)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: virtual_data
    ! local
    class(*), pointer :: vdata_ptr

    vdata_ptr => virtual_data
    call this%virtual_data_list%Add(vdata_ptr)

  end subroutine add_to_list

  subroutine vdc_prepare_stage(this, stage)
    use SimModule, only: ustop
    class(VirtualDataContainerType) :: this
    integer(I4B) :: stage

    write(*,*) 'Error: prepare_stage should be overridden'
    call ustop()

  end subroutine vdc_prepare_stage

  !> @brief Link all local data items to memory
  !<
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
          if (vdi%is_remote) cycle
          if (vdi%check_stage(stage)) call vdi%link()
      end select
    end do

  end subroutine vdc_link_items

  subroutine map_scalar(this, field, stages, map_type)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: field
    integer(I4B), dimension(:) :: stages
    integer(I4B) :: map_type

    call this%map_internal(field, (/ 0 /), stages, map_type)

  end subroutine map_scalar

  subroutine map_array1d(this, field, nrow, stages, map_type)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: field
    integer(I4B) :: nrow
    integer(I4B), dimension(:) :: stages
    integer(I4B) :: map_type

    call this%map_internal(field, (/ nrow /), stages, map_type)

  end subroutine map_array1d

  subroutine map_array2d(this, field, ncol, nrow, stages, map_type)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: field
    integer(I4B) :: ncol
    integer(I4B) :: nrow
    integer(I4B), dimension(:) :: stages
    integer(I4B) :: map_type

    call this%map_internal(field, (/ ncol, nrow /), stages, map_type)

  end subroutine map_array2d

  subroutine map_internal(this, field, shape, stages, map_type)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: field
    integer(I4B), dimension(:) :: shape
    integer(I4B), dimension(:) :: stages
    integer(I4B) :: map_type
    ! local
    character(len=LENMEMPATH) :: vmem_path
    logical(LGP) :: found
    
    field%sync_stages = stages
    field%map_type = map_type
    if (field%is_remote) then
      ! create new virtual memory item
      vmem_path = this%get_vrt_mem_path(field%var_name, field%subcmp_name)
      call field%vm_allocate(field%var_name, vmem_path, shape)
      call get_from_memorylist(field%var_name, vmem_path, field%virtual_mt, found)
    end if

  end subroutine map_internal

  !> @brief Get indexes of virtual data items to be
  !< sent for a given stage and rank
  subroutine vdc_get_send_items(this, stage, rank, virtual_items)
    class(VirtualDataContainerType) :: this
    integer(I4B) :: stage
    integer(I4B) :: rank
    type(STLVecInt) :: virtual_items

    call this%get_items_for_stage(stage, virtual_items)

  end subroutine vdc_get_send_items

  !> @brief Get indexes of virtual data items to be
  !< received for a given stage and rank
  subroutine vdc_get_recv_items(this, stage, rank, virtual_items)
    class(VirtualDataContainerType) :: this
    integer(I4B) :: stage
    integer(I4B) :: rank
    type(STLVecInt) :: virtual_items

    call this%get_items_for_stage(stage, virtual_items)

  end subroutine vdc_get_recv_items   

  subroutine get_items_for_stage(this, stage, virtual_items)
    class(VirtualDataContainerType) :: this
    integer(I4B) :: stage
    type(STLVecInt) :: virtual_items
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj_ptr

    do i = 1, this%virtual_data_list%Count()
      obj_ptr => this%virtual_data_list%GetItem(i)
      select type (obj_ptr)
        class is (VirtualDataType)
          if (.not. obj_ptr%check_stage(stage)) cycle
          call virtual_items%push_back(i)
      end select
    end do

  end subroutine get_items_for_stage

  !> @brief Get virtual memory path for a certain variable
  !<
  function vdc_get_vrt_mem_path(this, var_name, subcomp_name) result(vrt_path)
    class(VirtualDataContainerType) :: this
    character(len=*) :: var_name
    character(len=*) :: subcomp_name
    character(len=LENMEMPATH) :: vrt_path
    ! local
    class(VirtualDataType), pointer :: vdi
    
    vdi => this%vdc_get_virtual_data(var_name, subcomp_name)      
    if (vdi%is_remote) then
      if (subcomp_name == '') then
        vrt_path = create_mem_path(this%name, context=this%vmem_ctx)
      else 
        vrt_path = create_mem_path(this%name, subcomp_name, context=this%vmem_ctx)
      end if
    else
      if (subcomp_name == '') then
        vrt_path = create_mem_path(this%name)
      else
        vrt_path = create_mem_path(this%name, subcomp_name)
      end if
    end if

  end function vdc_get_vrt_mem_path

  function vdc_get_virtual_data(this, var_name, subcomp_name) result(virtual_data)
    use SimModule, only: ustop
    class(VirtualDataContainerType) :: this
    character(len=*) :: var_name
    character(len=*) :: subcomp_name
    class(VirtualDataType), pointer :: virtual_data
    ! local
    integer(I4B) :: i
    class(VirtualDataType), pointer :: vd

    virtual_data => null()
    do i = 1, this%virtual_data_list%Count()
      vd => get_virtual_data_from_list(this%virtual_data_list, i)
      if (vd%var_name == var_name .and. &
          vd%subcmp_name == subcomp_name) then
        virtual_data => vd
        return
      end if
    end do

    write(*,*) 'Error: unknown virtual variable ', var_name, ' ', subcomp_name
    call ustop()

  end function vdc_get_virtual_data

  subroutine vdc_destroy(this)
    class(VirtualDataContainerType) :: this
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj

    do i = 1, this%virtual_data_list%Count()
      obj => this%virtual_data_list%GetItem(i)
      select type (obj)
      class is (VirtualDataType)
        if (associated(obj%virtual_mt)) then
          call obj%vm_deallocate()
        end if
      end select
    end do
    call this%virtual_data_list%Clear()

  end subroutine vdc_destroy

  subroutine vdc_set_orig_rank(this, rank)
    class(VirtualDataContainerType) :: this
    integer(I4B) :: rank

    this%orig_rank = rank
    write(this%vmem_ctx, '(a,i0,a)') '__P', rank, '__'

  end subroutine vdc_set_orig_rank

  function get_vdc_from_list(list, idx) result(vdc)
    type(ListType) :: list
    integer(I4B) :: idx
    class(VirtualDataContainerType), pointer :: vdc
    ! local
    class(*), pointer :: obj_ptr

    vdc => null()
    obj_ptr => list%GetItem(idx)
    select type (obj_ptr)
      class is (VirtualDataContainerType)
        vdc => obj_ptr
    end select

  end function get_vdc_from_list

end module VirtualDataContainerModule
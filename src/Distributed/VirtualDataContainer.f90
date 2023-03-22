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
  public VDC_TYPE_TO_STR

  integer(I4B), public, parameter :: VDC_UNKNOWN_TYPE = 0
  integer(I4B), public, parameter :: VDC_GWFMODEL_TYPE = 1
  integer(I4B), public, parameter :: VDC_GWTMODEL_TYPE = 2
  integer(I4B), public, parameter :: VDC_GWFEXG_TYPE = 3
  integer(I4B), public, parameter :: VDC_GWTEXG_TYPE = 4
  integer(I4B), public, parameter :: VDC_GWFMVR_TYPE = 5
  integer(I4B), public, parameter :: VDC_GWTMVT_TYPE = 6

  !> @brief Wrapper for virtual data containers
  !!
  !! We can't have an array of pointers in Fortran, so we use
  !! this trick where we wrap the pointer and have an array
  !< of VdcPtrType instead.
  type, public :: VdcPtrType
    class(VirtualDataContainerType), pointer :: ptr => null()
  end type VdcPtrType

  type :: VdcElementMapType
    integer(I4B) :: nr_virt_elems !< the number of virtualized elements 
    integer(I4B), dimension(:), pointer, contiguous :: remote_elem_shift => null() !< array with 0-based remote indexes
    integer(I4B), dimension(:), pointer, contiguous :: remote_to_virtual => null() !< (sparse) array with local indexes
  end type VdcElementMapType

  !> @brief Container (list) of virtual data items.
  !!
  !! A virtual model or exchange derives from this base
  !! and can add the component-specific items to the list
  !! of virtual data items. As far as synchronization
  !! of virtual objects is concerned, all that is needed
  !< is the list of virtual data items in this container.
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
    type(VdcElementMapType), dimension(NR_VDC_ELEMENT_MAPS) :: element_maps !< a list with all element maps
  contains
    procedure :: vdc_create
    generic :: map => map_scalar, map_array1d, map_array2d
    procedure :: prepare_stage => vdc_prepare_stage
    procedure :: link_items => vdc_link_items
    procedure :: set_element_map => vdc_set_element_map
    procedure :: get_vrt_mem_path => vdc_get_vrt_mem_path
    procedure :: destroy => vdc_destroy
    procedure :: set_orig_rank => vdc_set_orig_rank
    procedure :: get_send_items => vdc_get_send_items
    procedure :: get_recv_items => vdc_get_recv_items
    procedure :: print_items
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
    ! local
    integer(I4B) :: i

    this%name = name
    this%id = id
    this%is_local = is_local
    this%vmem_ctx = 'undefined'
    this%orig_rank = 0
    this%is_active = .true.
    this%container_type = VDC_UNKNOWN_TYPE

    do i = 1, size(this%element_maps)
      this%element_maps(i)%nr_virt_elems = 0
      this%element_maps(i)%remote_elem_shift => null()
      this%element_maps(i)%remote_to_virtual => null()
    end do

  end subroutine vdc_create

  !> @brief Create virtual data item, without allocation,
  !< and add store it in this container.
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
    field%remote_elem_shift => null()
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

    write (*, *) 'Error: prepare_stage should be overridden'
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

  !> @brief Add the source indexes associated with map_id
  !! as a element map to this container, such that
  !< src_indexes(1:n) = (i_orig_1 - 1, ..., i_orig_n - 1)
  subroutine vdc_set_element_map(this, src_indexes, map_id)
    class(VirtualDataContainerType) :: this
    integer(I4B), dimension(:), pointer, contiguous :: src_indexes
    integer(I4B) :: map_id
    ! local
    integer(I4B) :: i, idx_remote, nmax

    this%element_maps(map_id)%nr_virt_elems = size(src_indexes)
    allocate (this%element_maps(map_id)%remote_elem_shift(size(src_indexes)))
    do i = 1, size(src_indexes)      
      this%element_maps(map_id)%remote_elem_shift(i) = src_indexes(i) - 1
    enddo

    nmax = maxval(src_indexes)    
    allocate (this%element_maps(map_id)%remote_to_virtual(nmax))
    do i = 1, nmax
      this%element_maps(map_id)%remote_to_virtual(i) = -1
    end do
    do i = 1, size(src_indexes)
      idx_remote = src_indexes(i)
      this%element_maps(map_id)%remote_to_virtual(idx_remote) = i
    end do

  end subroutine vdc_set_element_map

  subroutine map_scalar(this, field, stages, map_id)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: field
    integer(I4B), dimension(:) :: stages
    integer(I4B) :: map_id

    call this%map_internal(field, (/0/), stages, map_id)

  end subroutine map_scalar

  subroutine map_array1d(this, field, nrow, stages, map_id)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: field
    integer(I4B) :: nrow
    integer(I4B), dimension(:) :: stages
    integer(I4B) :: map_id

    call this%map_internal(field, (/nrow/), stages, map_id)

  end subroutine map_array1d

  subroutine map_array2d(this, field, ncol, nrow, stages, map_id)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: field
    integer(I4B) :: ncol
    integer(I4B) :: nrow
    integer(I4B), dimension(:) :: stages
    integer(I4B) :: map_id

    call this%map_internal(field, (/ncol, nrow/), stages, map_id)

  end subroutine map_array2d

  subroutine map_internal(this, field, shape, stages, map_id)
    class(VirtualDataContainerType) :: this
    class(VirtualDataType), pointer :: field
    integer(I4B), dimension(:) :: shape
    integer(I4B), dimension(:) :: stages
    integer(I4B) :: map_id
    ! local
    character(len=LENMEMPATH) :: vmem_path
    logical(LGP) :: found

    field%sync_stages = stages
    field%map_type = map_id
    if (field%is_remote) then
      ! create new virtual memory item
      vmem_path = this%get_vrt_mem_path(field%var_name, field%subcmp_name)
      call field%vm_allocate(field%var_name, vmem_path, shape)
      call get_from_memorylist(field%var_name, vmem_path, field%virtual_mt, found)
      if (map_id > 0) then
        field%remote_to_virtual => this%element_maps(map_id)%remote_to_virtual
        field%remote_elem_shift => this%element_maps(map_id)%remote_elem_shift
      end if
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

  subroutine print_items(this, imon, items)
    class(VirtualDataContainerType) :: this
    integer(I4B) :: imon
    type(STLVecInt) :: items
    ! local
    integer(I4B) :: i
    class(VirtualDataType), pointer :: vdi

    write (imon, *) "=====> items"
    do i = 1, items%size
      vdi => get_virtual_data_from_list(this%virtual_data_list, items%at(i))
      write (imon, *) vdi%var_name, ":", vdi%mem_path
    end do
    write (imon, *) "<===== items"

  end subroutine print_items

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

    write (*, *) 'Error: unknown virtual variable ', var_name, ' ', subcomp_name
    call ustop()

  end function vdc_get_virtual_data

  subroutine vdc_destroy(this)
    class(VirtualDataContainerType) :: this
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj

    do i = 1, size(this%element_maps)
      if (associated(this%element_maps(i)%remote_elem_shift)) then
        deallocate (this%element_maps(i)%remote_elem_shift)
      end if
    end do

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
    write (this%vmem_ctx, '(a,i0,a)') '__P', rank, '__'

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

  !> @ Converts a virtual container type to its string representation
  !<
  function VDC_TYPE_TO_STR(cntr_type) result(cntr_str)
    integer(I4B) :: cntr_type
    character(len=24) :: cntr_str

    if (cntr_type == VDC_UNKNOWN_TYPE) then; cntr_str = "unknown"
    else if (cntr_type == VDC_GWFMODEL_TYPE) then; cntr_str = "GWF Model"
    else if (cntr_type == VDC_GWTMODEL_TYPE) then; cntr_str = "GWT Model"
    else if (cntr_type == VDC_GWFEXG_TYPE) then; cntr_str = "GWF Exchange"
    else if (cntr_type == VDC_GWTEXG_TYPE) then; cntr_str = "GWT Exchange"
    else if (cntr_type == VDC_GWFMVR_TYPE) then; cntr_str = "GWF Mover"
    else if (cntr_type == VDC_GWTMVT_TYPE) then; cntr_str = "GWT Mover"
    else; cntr_str = "Undefined"
    end if

  end function VDC_TYPE_TO_STR

end module VirtualDataContainerModule

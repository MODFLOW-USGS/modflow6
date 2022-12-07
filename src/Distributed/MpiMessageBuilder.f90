module MpiMessageBuilderModule
  use KindModule, only: I4B
  use MemoryTypeModule, only: MemoryType
  use STLVecIntModule
  use VirtualBaseModule
  use VirtualDataListsModule, only: virtual_model_list, virtual_exchange_list
  use VirtualDataContainerModule
  use mpi
  implicit none
  private

  type, public :: VdcHeaderType
    integer(I4B) :: id
    integer(I4B) :: container_type
  end type

  type, public :: MpiMessageBuilderType
  contains
    procedure :: create_header_snd => mb_create_header_snd
    procedure :: create_header_rcv => mb_create_header_rcv
    procedure :: create_body_rcv => mb_create_body_rcv
    procedure :: create_body_snd => mb_create_body_snd
    ! private
    procedure, private :: create_vdc_snd_hdr
    procedure, private :: create_vdc_body
  end type

contains

  subroutine mb_create_header_snd(this, rank, stage, hdrs_snd_type)
    class(MpiMessageBuilderType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage    
    integer :: hdrs_snd_type
    ! local
    integer(I4B) :: i
    class(VirtualDataContainerType), pointer :: vdc
    integer :: ierr
    type(STLVecInt) :: model_idxs
    integer, dimension(:), allocatable :: blk_cnts, types
    integer(kind=MPI_ADDRESS_KIND), dimension(:), allocatable :: displs

    call model_idxs%init()

    ! determine which containers to include
    do i = 1, virtual_model_list%Count()
      vdc => get_vdc_from_list(virtual_model_list, i)
      if (vdc%is_active .and. vdc%orig_rank == rank) then
        call model_idxs%push_back(i)
      end if
    end do

    allocate(blk_cnts(model_idxs%size))
    allocate(types(model_idxs%size))
    allocate(displs(model_idxs%size))

    ! loop over included models
    do i = 1, model_idxs%size
      vdc => get_vdc_from_list(virtual_model_list, model_idxs%at(i))
      call MPI_Get_address(vdc%id, displs(i), ierr)
      blk_cnts(i) = 1
      types(i) = this%create_vdc_snd_hdr(vdc, stage)
    end do
    
    call MPI_Type_create_struct(model_idxs%size, blk_cnts, displs, &
                                types, hdrs_snd_type, ierr)

    call model_idxs%destroy()
    deallocate(blk_cnts)
    deallocate(types)
    deallocate(displs)

  end subroutine mb_create_header_snd

  subroutine mb_create_header_rcv(this, hdr_rcv_type)
    class(MpiMessageBuilderType) :: this
    integer :: hdr_rcv_type
    ! local
    integer :: ierr
    
    ! this will be for one data container, the mpi recv
    ! call will accept an array of them, no need to create
    ! an overarching contiguous type...
    call MPI_Type_contiguous(2, MPI_INTEGER, hdr_rcv_type, ierr)    

  end subroutine mb_create_header_rcv

  !> @brief Create the body to receive based on the headers
  !< that have been sent
  subroutine mb_create_body_rcv(this, rank, stage, body_rcv_type)
    class(MpiMessageBuilderType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage
    integer :: body_rcv_type
    ! local
    integer(I4B) :: i, nr_models
    class(VirtualDataContainerType), pointer :: vdc
    type(STLVecInt) :: model_idxs
    integer :: ierr
    integer, dimension(:), allocatable :: types
    integer(kind=MPI_ADDRESS_KIND), dimension(:), allocatable :: displs
    integer, dimension(:), allocatable :: blk_cnts

    call model_idxs%init()

    ! gather all models coming from this rank
    do i = 1, virtual_model_list%Count()
      vdc => get_vdc_from_list(virtual_model_list, i)
      if (vdc%is_active .and. vdc%orig_rank == rank) then
        call model_idxs%push_back(i)
      end if
    end do

    nr_models = model_idxs%size
    allocate (types(nr_models))
    allocate (displs(nr_models))
    allocate (blk_cnts(nr_models))

    ! loop over included models
    do i = 1, nr_models
      vdc => get_vdc_from_list(virtual_model_list, model_idxs%at(i))
      call MPI_Get_address(vdc%id, displs(i), ierr)
      types(i) = this%create_vdc_body(vdc, stage)
      blk_cnts(i) = 1
    end do

    ! create the list of virtual data containers to receive
    call MPI_Type_create_struct(model_idxs%size, blk_cnts, displs, types, body_rcv_type, ierr)

    call model_idxs%destroy()
    deallocate (types)
    deallocate (displs)
    deallocate (blk_cnts)

  end subroutine mb_create_body_rcv

  !> @brief Create the body to send based on the received headers
  !<
  subroutine mb_create_body_snd(this, rank, stage, headers, body_snd_type)
    class(MpiMessageBuilderType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage
    type(VdcHeaderType), dimension(:) :: headers
    integer :: body_snd_type
    ! local
    integer(I4B) :: i, nr_headers
    class(VirtualDataContainerType), pointer :: vdc
    integer :: ierr
    integer, dimension(:), allocatable :: types
    integer(kind=MPI_ADDRESS_KIND), dimension(:), allocatable :: displs
    integer, dimension(:), allocatable :: blk_cnts

    nr_headers = size(headers)
    allocate (types(nr_headers))
    allocate (displs(nr_headers))
    allocate (blk_cnts(nr_headers))

    do i = 1, nr_headers
      vdc => get_vdc_from_hdr(headers(i))
      call MPI_Get_address(vdc%id, displs(i), ierr)
      types(i) = this%create_vdc_body(vdc, stage)
      blk_cnts(i) = 1
    end do

    ! create the list of virtual data containers to receive
    call MPI_Type_create_struct(nr_headers, blk_cnts, displs, types, body_snd_type, ierr)

    deallocate (types)
    deallocate (displs)
    deallocate (blk_cnts)

  end subroutine mb_create_body_snd

  !> @brief Create send header for virtual data container, relative  
  !< to the field ...%id
  function create_vdc_snd_hdr(this, vdc, stage) result(new_type)
    class(MpiMessageBuilderType) :: this
    class(VirtualDataContainerType) :: vdc
    integer(I4B) :: stage
    integer :: new_type ! the created MPI datatype, uncommitted
    ! local
    integer :: ierr
    integer, dimension(2) :: blk_cnts
    integer(kind=MPI_ADDRESS_KIND), dimension(2) :: displs
    integer, dimension(2) :: types

    call MPI_Get_address(vdc%id, displs(1), ierr)
    types(1) = MPI_INTEGER
    blk_cnts(1) = 1
    call MPI_Get_address(vdc%container_type, displs(2), ierr)
    types(2) = MPI_INTEGER
    blk_cnts(2) = 1

    ! rebase to id field
    displs = displs - displs(1)
    call MPI_Type_create_struct(2, blk_cnts, displs, types, new_type, ierr)

  end function create_vdc_snd_hdr

  !> @brief Create data type for this container, relative
  !< to its id field. This is used for sending and receiving
  function create_vdc_body(this, vdc, stage) result(new_type)
    class(MpiMessageBuilderType) :: this
    class(VirtualDataContainerType), pointer :: vdc
    integer(I4B) :: stage
    integer :: new_type
    ! local
    integer(I4B) :: i 
    type(STLVecInt) :: items
    class(VirtualDataType), pointer :: vd
    integer :: ierr
    integer(kind=MPI_ADDRESS_KIND) :: offset
    integer, dimension(:), allocatable :: types
    integer(kind=MPI_ADDRESS_KIND), dimension(:), allocatable :: displs
    integer, dimension(:), allocatable :: blk_cnts

    call get_items_for_stage(vdc, stage, items)
    allocate (types(items%size))
    allocate (displs(items%size))
    allocate (blk_cnts(items%size))

    call MPI_Get_address(vdc%id, offset, ierr)

    do i = 1, items%size
      vd => get_virtual_data_from_list(vdc%virtual_data_list, i)
      call get_mpi_datatype(vd, displs(i), types(i))
      blk_cnts(i) = 1
      ! rebase w.r.t. id field
      displs(i) = displs(i) - offset
    end do

    call MPI_Type_create_struct(items%size, blk_cnts, displs, types, new_type, ierr)

    deallocate (types)
    deallocate (displs)
    deallocate (blk_cnts)

  end function create_vdc_body

  function get_vdc_from_hdr(header) result(vdc)
    type(VdcHeaderType) :: header
    class(VirtualDataContainerType), pointer :: vdc
    ! local
    integer(I4B) :: i

    vdc => null()
    if (header%container_type == VDC_GWFMODEL_TYPE .or. &
        header%container_type == VDC_GWTMODEL_TYPE) then
      do i = 1, virtual_model_list%Count()
        vdc => get_vdc_from_list(virtual_model_list, i)
        if (vdc%id == header%id) return
        vdc => null()
      end do
    else if (header%container_type == VDC_GWFEXG_TYPE .or. &
             header%container_type == VDC_GWTEXG_TYPE) then
      do i = 1, virtual_exchange_list%Count()
        vdc => get_vdc_from_list(virtual_exchange_list, i)
        if (vdc%id == header%id) return
        vdc => null()
      end do
    end if

  end function get_vdc_from_hdr

  subroutine get_mpi_datatype(virtual_data, el_displ, el_type)
    use SimModule, only: ustop    
    class(VirtualDataType), pointer :: virtual_data
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    type(MemoryType), pointer :: mt

    mt => virtual_data%virtual_mt
    if (associated(mt%intsclr)) then
      call get_mpitype_for_int(mt, el_displ, el_type)
    else if (associated(mt%aint1d)) then
      call get_mpitype_for_int1d(mt, el_displ, el_type)
    else if (associated(mt%aint2d)) then
      call get_mpitype_for_int2d(mt, el_displ, el_type)
    else if (associated(mt%aint3d)) then
      call get_mpitype_for_int3d(mt, el_displ, el_type)
    else if (associated(mt%dblsclr)) then
      call get_mpitype_for_dbl(mt, el_displ, el_type)
    else if (associated(mt%adbl1d)) then
      call get_mpitype_for_dbl1d(mt, el_displ, el_type)
    else if (associated(mt%adbl2d)) then
      call get_mpitype_for_dbl2d(mt, el_displ, el_type)
    else if (associated(mt%adbl3d)) then
      call get_mpitype_for_dbl3d(mt, el_displ, el_type)
    else
      write(*,*) 'unsupported datatype in MPI messaging for ', &
                 virtual_data%remote_var_name, virtual_data%remote_mem_path
      call ustop()
    end if

  end subroutine get_mpi_datatype

  subroutine get_mpitype_for_int(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%intsclr, el_displ, ierr)
    el_type = MPI_INT

  end subroutine get_mpitype_for_int

  subroutine get_mpitype_for_int1d(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%aint1d, el_displ, ierr)
    call MPI_Type_contiguous(mem%isize, MPI_INT, el_type, ierr)
    
  end subroutine get_mpitype_for_int1d

  subroutine get_mpitype_for_int2d(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%aint2d, el_displ, ierr)
    call MPI_Type_contiguous(mem%isize, MPI_INT, el_type, ierr)
    
  end subroutine get_mpitype_for_int2d

  subroutine get_mpitype_for_int3d(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%aint3d, el_displ, ierr)
    call MPI_Type_contiguous(mem%isize, MPI_INT, el_type, ierr)
    
  end subroutine get_mpitype_for_int3d

  subroutine get_mpitype_for_dbl(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%dblsclr, el_displ, ierr)
    el_type = MPI_DOUBLE_PRECISION
    
  end subroutine get_mpitype_for_dbl

  subroutine get_mpitype_for_dbl1d(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%adbl1d, el_displ, ierr)
    call MPI_Type_contiguous(mem%isize, MPI_DOUBLE_PRECISION, el_type, ierr)
    
  end subroutine get_mpitype_for_dbl1d

  subroutine get_mpitype_for_dbl2d(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%adbl2d, el_displ, ierr)
    call MPI_Type_contiguous(mem%isize, MPI_DOUBLE_PRECISION, el_type, ierr)
    
  end subroutine get_mpitype_for_dbl2d

  subroutine get_mpitype_for_dbl3d(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%adbl3d, el_displ, ierr)
    call MPI_Type_contiguous(mem%isize, MPI_DOUBLE_PRECISION, el_type, ierr)
    
  end subroutine get_mpitype_for_dbl3d

  !> @brief Get indexes of virtual data items in this
  !< container as a vector
  subroutine get_items_for_stage(vdc, stage, virtual_items)
    class(VirtualDataContainerType), pointer :: vdc
    integer(I4B) :: stage
    type(STLVecInt) :: virtual_items
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj_ptr

    call virtual_items%init()

    do i = 1, vdc%virtual_data_list%Count()
      obj_ptr => vdc%virtual_data_list%GetItem(i)
      select type (obj_ptr)
        class is (VirtualDataType)
          if (.not. obj_ptr%check_stage(stage)) cycle
          call virtual_items%push_back(i)
      end select
    end do

  end subroutine get_items_for_stage

end module MpiMessageBuilderModule
module MpiMessageBuilderModule
  use KindModule, only: I4B
  use MemoryTypeModule, only: MemoryType
  use STLVecIntModule
  use VirtualBaseModule
  use VirtualDataContainerModule
  use mpi
  implicit none
  private

  type, public :: VdcHeaderType
    integer(I4B) :: id
    integer(I4B) :: container_type
  end type

  type, public :: MpiMessageBuilderType
    type(VdcPtrType), dimension(:), pointer :: vdc_models => null() !< the models to be build the message for
    type(VdcPtrType), dimension(:), pointer :: vdc_exchanges => null() !< the exchanges to be build the message for
  contains
    procedure :: attach_data => mb_attach_data
    procedure :: release_data => mb_release_data
    procedure :: create_header_snd => mb_create_header_snd
    procedure :: create_header_rcv => mb_create_header_rcv
    procedure :: create_body_rcv => mb_create_body_rcv
    procedure :: create_body_snd => mb_create_body_snd
    ! private
    procedure, private :: get_vdc_from_hdr
    procedure, private :: create_vdc_snd_hdr
    procedure, private :: create_vdc_snd_body
    procedure, private :: create_vdc_rcv_body
    procedure, private :: create_vdc_body
  end type

contains

  subroutine mb_attach_data(this, vdc_models, vdc_exchanges)
    class(MpiMessageBuilderType) :: this
    type(VdcPtrType), dimension(:), pointer :: vdc_models
    type(VdcPtrType), dimension(:), pointer :: vdc_exchanges

    this%vdc_models => vdc_models
    this%vdc_exchanges => vdc_exchanges

  end subroutine mb_attach_data

  subroutine mb_release_data(this)
    class(MpiMessageBuilderType) :: this

    this%vdc_models => null()
    this%vdc_exchanges => null()

  end subroutine mb_release_data

  !> @brief Create the header data type to send to
  !! the remote process for this particular stage.
  !! From these data, the receiver can construct the
  !< body to send back to us.
  subroutine mb_create_header_snd(this, rank, stage, hdrs_snd_type)
    class(MpiMessageBuilderType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage
    integer :: hdrs_snd_type
    ! local
    integer(I4B) :: i, offset, nr_types
    class(VirtualDataContainerType), pointer :: vdc
    integer :: ierr
    type(STLVecInt) :: model_idxs, exg_idxs
    integer, dimension(:), allocatable :: blk_cnts, types
    integer(kind=MPI_ADDRESS_KIND), dimension(:), allocatable :: displs

    call model_idxs%init()
    call exg_idxs%init()

    ! determine which containers to include
    do i = 1, size(this%vdc_models)
      vdc => this%vdc_models(i)%ptr
      if (vdc%is_active .and. vdc%orig_rank == rank) then
        call model_idxs%push_back(i)
      end if
    end do
    do i = 1, size(this%vdc_exchanges)
      vdc => this%vdc_exchanges(i)%ptr
      if (vdc%is_active .and. vdc%orig_rank == rank) then
        call exg_idxs%push_back(i)
      end if
    end do

    nr_types = model_idxs%size + exg_idxs%size
    allocate (blk_cnts(nr_types))
    allocate (types(nr_types))
    allocate (displs(nr_types))

    ! loop over containers
    do i = 1, model_idxs%size
      vdc => this%vdc_models(model_idxs%at(i))%ptr
      call MPI_Get_address(vdc%id, displs(i), ierr)
      blk_cnts(i) = 1
      types(i) = this%create_vdc_snd_hdr(vdc, stage)
    end do
    offset = model_idxs%size
    do i = 1, exg_idxs%size
      vdc => this%vdc_exchanges(exg_idxs%at(i))%ptr
      call MPI_Get_address(vdc%id, displs(i + offset), ierr)
      blk_cnts(i + offset) = 1
      types(i + offset) = this%create_vdc_snd_hdr(vdc, stage)
    end do

    ! create a MPI data type for the headers to send
    call MPI_Type_create_struct(nr_types, blk_cnts, displs, types, &
                                hdrs_snd_type, ierr)
    call MPI_Type_commit(hdrs_snd_type, ierr)
    do i = 1, nr_types
      call MPI_Type_free(types(i), ierr)
    end do

    call model_idxs%destroy()
    call exg_idxs%destroy()

    deallocate (blk_cnts)
    deallocate (types)
    deallocate (displs)

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
    call MPI_Type_commit(hdr_rcv_type, ierr)

  end subroutine mb_create_header_rcv

  !> @brief Create the body to receive based on the headers
  !< that have been sent
  subroutine mb_create_body_rcv(this, rank, stage, body_rcv_type)
    class(MpiMessageBuilderType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage
    integer :: body_rcv_type
    ! local
    integer(I4B) :: i, nr_types, offset
    class(VirtualDataContainerType), pointer :: vdc
    type(STLVecInt) :: model_idxs, exg_idxs
    integer :: ierr
    integer, dimension(:), allocatable :: types
    integer(kind=MPI_ADDRESS_KIND), dimension(:), allocatable :: displs
    integer, dimension(:), allocatable :: blk_cnts

    call model_idxs%init()
    call exg_idxs%init()

    ! gather all containers from this rank
    do i = 1, size(this%vdc_models)
      vdc => this%vdc_models(i)%ptr
      if (vdc%is_active .and. vdc%orig_rank == rank) then
        call model_idxs%push_back(i)
      end if
    end do
    do i = 1, size(this%vdc_exchanges)
      vdc => this%vdc_exchanges(i)%ptr
      if (vdc%is_active .and. vdc%orig_rank == rank) then
        call exg_idxs%push_back(i)
      end if
    end do

    nr_types = model_idxs%size + exg_idxs%size
    allocate (types(nr_types))
    allocate (displs(nr_types))
    allocate (blk_cnts(nr_types))

    ! loop over included containers
    do i = 1, model_idxs%size
      vdc => this%vdc_models(model_idxs%at(i))%ptr
      call MPI_Get_address(vdc%id, displs(i), ierr)
      types(i) = this%create_vdc_rcv_body(vdc, rank, stage)
      blk_cnts(i) = 1
    end do
    offset = model_idxs%size
    do i = 1, exg_idxs%size
      vdc => this%vdc_exchanges(exg_idxs%at(i))%ptr
      call MPI_Get_address(vdc%id, displs(i + offset), ierr)
      blk_cnts(i + offset) = 1
      types(i + offset) = this%create_vdc_rcv_body(vdc, rank, stage)
    end do

    ! create a MPI data type for the virtual data containers to receive
    call MPI_Type_create_struct(nr_types, blk_cnts, displs, types, &
                                body_rcv_type, ierr)
    call MPI_Type_commit(body_rcv_type, ierr)
    do i = 1, nr_types
      call MPI_Type_free(types(i), ierr)
    end do

    call model_idxs%destroy()
    call exg_idxs%destroy()
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
      vdc => this%get_vdc_from_hdr(headers(i))
      call MPI_Get_address(vdc%id, displs(i), ierr)
      types(i) = this%create_vdc_snd_body(vdc, rank, stage)
      blk_cnts(i) = 1
    end do

    ! create the list of virtual data containers to receive
    call MPI_Type_create_struct(nr_headers, blk_cnts, displs, &
                                types, body_snd_type, ierr)
    call MPI_Type_commit(body_snd_type, ierr)
    do i = 1, nr_headers
      call MPI_Type_free(types(i), ierr)
    end do

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
    call MPI_Type_commit(new_type, ierr)

  end function create_vdc_snd_hdr

  function create_vdc_rcv_body(this, vdc, rank, stage) result(new_type)
    class(MpiMessageBuilderType) :: this
    class(VirtualDataContainerType), pointer :: vdc
    integer(I4B) :: rank
    integer(I4B) :: stage
    integer :: new_type
    ! local
    type(STLVecInt) :: virtual_items

    call virtual_items%init()
    call vdc%get_recv_items(stage, rank, virtual_items)
    new_type = this%create_vdc_body(vdc, virtual_items)
    call virtual_items%destroy()

  end function create_vdc_rcv_body

  function create_vdc_snd_body(this, vdc, rank, stage) result(new_type)
    class(MpiMessageBuilderType) :: this
    class(VirtualDataContainerType), pointer :: vdc
    integer(I4B) :: rank
    integer(I4B) :: stage
    integer :: new_type
    ! local
    type(STLVecInt) :: virtual_items

    call virtual_items%init()
    call vdc%get_send_items(stage, rank, virtual_items)
    new_type = this%create_vdc_body(vdc, virtual_items)
    call virtual_items%destroy()

  end function create_vdc_snd_body

  !> @brief Create data type for this container, relative
  !< to its id field. This is used for sending and receiving
  function create_vdc_body(this, vdc, items) result(new_type)
    class(MpiMessageBuilderType) :: this
    class(VirtualDataContainerType), pointer :: vdc
    type(STLVecInt) :: items
    integer :: new_type
    ! local
    integer(I4B) :: i
    class(VirtualDataType), pointer :: vd
    integer :: ierr
    integer(kind=MPI_ADDRESS_KIND) :: offset
    integer, dimension(:), allocatable :: types
    integer(kind=MPI_ADDRESS_KIND), dimension(:), allocatable :: displs
    integer, dimension(:), allocatable :: blk_cnts

    allocate (types(items%size))
    allocate (displs(items%size))
    allocate (blk_cnts(items%size))

    call MPI_Get_address(vdc%id, offset, ierr)

    do i = 1, items%size
      vd => get_virtual_data_from_list(vdc%virtual_data_list, items%at(i))
      call get_mpi_datatype(vd, displs(i), types(i))
      blk_cnts(i) = 1
      ! rebase w.r.t. id field
      displs(i) = displs(i) - offset
    end do

    call MPI_Type_create_struct(items%size, blk_cnts, displs, &
                                types, new_type, ierr)
    call MPI_Type_commit(new_type, ierr)

    do i = 1, items%size
      vd => get_virtual_data_from_list(vdc%virtual_data_list, items%at(i))
      call free_mpi_datatype(vd, types(i))
    end do

    deallocate (types)
    deallocate (displs)
    deallocate (blk_cnts)

  end function create_vdc_body

  function get_vdc_from_hdr(this, header) result(vdc)
    class(MpiMessageBuilderType) :: this
    type(VdcHeaderType) :: header
    class(VirtualDataContainerType), pointer :: vdc
    ! local
    integer(I4B) :: i

    vdc => null()
    if (header%container_type == VDC_GWFMODEL_TYPE .or. &
        header%container_type == VDC_GWTMODEL_TYPE) then
      do i = 1, size(this%vdc_models)
        vdc => this%vdc_models(i)%ptr
        if (vdc%id == header%id) return
        vdc => null()
      end do
    else if (header%container_type == VDC_GWFEXG_TYPE .or. &
             header%container_type == VDC_GWTEXG_TYPE) then
      do i = 1, size(this%vdc_exchanges)
        vdc => this%vdc_exchanges(i)%ptr
        if (vdc%id == header%id) return
        vdc => null()
      end do
    end if

  end function get_vdc_from_hdr

  !> @brief Local routine to get elemental mpi data types representing
  !! the virtual data items. Types are automatically committed unless
  !< they are primitives (e.g. MPI_INTEGER)
  subroutine get_mpi_datatype(virtual_data, el_displ, el_type)
    use SimModule, only: ustop
    use SimVariablesModule, only: proc_id
    class(VirtualDataType), pointer :: virtual_data
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    type(MemoryType), pointer :: mt

    mt => virtual_data%virtual_mt
    if (.not. associated(mt)) then
      write (*, *) 'not associated: ', virtual_data%var_name, proc_id
    end if
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
      write (*, *) 'unsupported datatype in MPI messaging for ', &
        virtual_data%var_name, virtual_data%mem_path
      call ustop()
    end if

  end subroutine get_mpi_datatype

  !> @brief Local routine to free elemental mpi data types representing
  !! the virtual data items. This can't be done generally, because some
  !< (scalar) types are primitive and freeing them causes nasty errors...
  subroutine free_mpi_datatype(virtual_data, el_type)
    class(VirtualDataType), pointer :: virtual_data
    integer :: el_type
    ! local
    type(MemoryType), pointer :: mt
    integer :: ierr

    mt => virtual_data%virtual_mt
    if (associated(mt%intsclr)) then
      ! type is MPI_INTEGER, don't free this!
      return
    else if (associated(mt%dblsclr)) then
      ! type is MPI_DOUBLE_PRECISION, don't free this!
      return
    else if (associated(mt%logicalsclr)) then
      ! type is MPI_LOGICAL, don't free this!
      return
    else
      ! all other types are freed here
      call MPI_Type_free(el_type, ierr)
      return
    end if

  end subroutine free_mpi_datatype

  subroutine get_mpitype_for_int(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%intsclr, el_displ, ierr)
    el_type = MPI_INTEGER
    ! no need to commit primitive type

  end subroutine get_mpitype_for_int

  subroutine get_mpitype_for_int1d(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%aint1d, el_displ, ierr)
    call MPI_Type_contiguous(mem%isize, MPI_INTEGER, el_type, ierr)
    call MPI_Type_commit(el_type, ierr)

  end subroutine get_mpitype_for_int1d

  subroutine get_mpitype_for_int2d(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%aint2d, el_displ, ierr)
    call MPI_Type_contiguous(mem%isize, MPI_INTEGER, el_type, ierr)
    call MPI_Type_commit(el_type, ierr)

  end subroutine get_mpitype_for_int2d

  subroutine get_mpitype_for_int3d(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%aint3d, el_displ, ierr)
    call MPI_Type_contiguous(mem%isize, MPI_INTEGER, el_type, ierr)
    call MPI_Type_commit(el_type, ierr)

  end subroutine get_mpitype_for_int3d

  subroutine get_mpitype_for_dbl(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%dblsclr, el_displ, ierr)
    el_type = MPI_DOUBLE_PRECISION
    ! no need to commit primitive type

  end subroutine get_mpitype_for_dbl

  subroutine get_mpitype_for_dbl1d(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%adbl1d, el_displ, ierr)
    call MPI_Type_contiguous(mem%isize, MPI_DOUBLE_PRECISION, el_type, ierr)
    call MPI_Type_commit(el_type, ierr)

  end subroutine get_mpitype_for_dbl1d

  subroutine get_mpitype_for_dbl2d(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%adbl2d, el_displ, ierr)
    call MPI_Type_contiguous(mem%isize, MPI_DOUBLE_PRECISION, el_type, ierr)
    call MPI_Type_commit(el_type, ierr)

  end subroutine get_mpitype_for_dbl2d

  subroutine get_mpitype_for_dbl3d(mem, el_displ, el_type)
    type(MemoryType), pointer :: mem
    integer(kind=MPI_ADDRESS_KIND) :: el_displ
    integer :: el_type
    ! local
    integer :: ierr

    call MPI_Get_address(mem%adbl3d, el_displ, ierr)
    call MPI_Type_contiguous(mem%isize, MPI_DOUBLE_PRECISION, el_type, ierr)
    call MPI_Type_commit(el_type, ierr)

  end subroutine get_mpitype_for_dbl3d

end module MpiMessageBuilderModule

module MpiRouterModule
  use RouterBaseModule
  use KindModule, only: I4B
  use STLVecIntModule
  use SimVariablesModule, only: proc_id
  use SimStagesModule, only: STG_TO_STR
  use VirtualDataListsModule, only: virtual_model_list, &
                                    virtual_exchange_list
  use VirtualDataContainerModule, only: VirtualDataContainerType, &
                                        get_vdc_from_list
  use VirtualExchangeModule, only: VirtualExchangeType, &
                                   get_virtual_exchange_from_list
  use VirtualSolutionModule
  use MpiMessageBuilderModule
  use MpiWorldModule
  use mpi
  implicit none
  private

  public :: create_mpi_router

  type, public, extends(RouterBaseType) :: MpiRouterType
    integer(I4B), dimension(:), pointer :: model_proc_ids
    type(STLVecInt) :: senders
    type(STLVecInt) :: receivers
    type(MpiMessageBuilderType) :: message_builder
    type(MpiWorldType), pointer :: mpi_world
  contains
    procedure :: initialize => mr_initialize
    procedure :: route_all => mr_route_all
    procedure :: route_sln => mr_route_sln
    procedure :: destroy => mr_destroy
    ! private
    procedure, private :: mr_update_senders
    procedure, private :: mr_update_receivers
  end type MpiRouterType

contains

  !> Factory method to create MPI router
  !<
  function create_mpi_router() result(router)
    class(RouterBaseType), pointer :: router
    ! local
    class(MpiRouterType), pointer :: mpi_router

    allocate (mpi_router)
    router => mpi_router

  end function create_mpi_router

  subroutine mr_initialize(this)
    class(MpiRouterType) :: this
    ! local
    integer :: ierr
    integer(I4B) :: i
    integer(I4B) :: nr_models, nr_exchanges
    class(VirtualDataContainerType), pointer :: vdc
    class(VirtualExchangeType), pointer :: vex
    
    ! get mpi world for our process
    this%mpi_world => get_mpi_world()

    ! init address list
    call this%senders%init()
    call this%receivers%init()

    ! find out where models are
    nr_models = virtual_model_list%Count()
    allocate (this%model_proc_ids(nr_models))
    
    do i = 1, nr_models
      vdc => get_vdc_from_list(virtual_model_list, i)
      if (vdc%is_remote) then
        this%model_proc_ids(i) = 0
      else
        this%model_proc_ids(i) = proc_id
      end if
    end do

    call MPI_Allreduce(MPI_IN_PLACE, this%model_proc_ids, nr_models, &
                       MPI_INTEGER, MPI_SUM, MF6_COMM_WORLD, ierr)

    do i = 1, nr_models
      vdc => get_vdc_from_list(virtual_model_list, i)
      call vdc%set_orig_rank(this%model_proc_ids(i))
    end do

    nr_exchanges = virtual_exchange_list%Count()
    do i = 1, nr_exchanges
      vex => get_virtual_exchange_from_list(virtual_exchange_list, i)
      ! TODO_MJR: we set it from model1, or from model2 when model1 is local.
      ! This is problematic because when a remote exchange resides
      ! on two distinct processes we cannot synchronize in one sweep...
      ! We need refactoring of Exchanges here, such that Exchange <=> rank
      ! is always 1-to-1.
      call vex%set_orig_rank(vex%v_model1%orig_rank)
      if (.not. vex%v_model1%is_remote) then
        call vex%set_orig_rank(vex%v_model2%orig_rank)
      end if
    end do

  end subroutine mr_initialize

  subroutine mr_route_all(this, stage)
    class(MpiRouterType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i, rnk
    integer :: ierr
    ! mpi handles
    integer, dimension(:), allocatable :: rcv_req
    integer, dimension(:), allocatable :: snd_req
    integer, dimension(:,:), allocatable :: rcv_stat
    integer, dimension(:,:), allocatable :: snd_stat
    ! message header
    integer(I4B) :: max_headers
    type(VdcHeaderType), dimension(:,:), allocatable :: headers
    integer, dimension(:), allocatable :: hdr_rcv_t
    integer, dimension(:), allocatable :: hdr_snd_t    
    integer, dimension(:), allocatable :: hdr_rcv_cnt
    ! message body
    integer, dimension(:), allocatable :: body_rcv_t
    integer, dimension(:), allocatable :: body_snd_t

    ! update address list
    call this%mr_update_senders()
    call this%mr_update_receivers() 

    ! allocate handles
    allocate (rcv_req(this%receivers%size))
    allocate (snd_req(this%senders%size))
    allocate (rcv_stat(MPI_STATUS_SIZE, this%receivers%size))
    allocate (snd_stat(MPI_STATUS_SIZE, this%senders%size))

    ! allocate header data
    max_headers = virtual_exchange_list%Count() + virtual_model_list%Count()    
    allocate (hdr_rcv_t(this%receivers%size))
    allocate (hdr_snd_t(this%senders%size))
    allocate (headers(max_headers, this%receivers%size))
    allocate (hdr_rcv_cnt(max_headers))

    ! allocate body data
    allocate (body_rcv_t(this%senders%size))
    allocate (body_snd_t(this%receivers%size))
    
    ! first receive headers for outward data  
    do i = 1, this%receivers%size
      rnk = this%receivers%at(i)
      call this%message_builder%create_header_rcv(hdr_rcv_t(i))
      call MPI_Type_commit(hdr_rcv_t(i), ierr)
      call MPI_Irecv(headers(:,i), max_headers, hdr_rcv_t(i), rnk, stage, MF6_COMM_WORLD, rcv_req(i), ierr)
      ! don't free mpi datatype, we need the count below
    end do

    ! send header for incoming data
    do i = 1, this%senders%size
      rnk = this%senders%at(i)
      call this%message_builder%create_header_snd(rnk, stage, hdr_snd_t(i))
      call MPI_Type_commit(hdr_snd_t(i), ierr)
      call MPI_Isend(MPI_BOTTOM, 1, hdr_snd_t(i), rnk, stage, MF6_COMM_WORLD, snd_req(i), ierr)
      call MPI_Type_free(hdr_snd_t(i), ierr)
    end do

    ! wait for exchange of all headers
    call MPI_WaitAll(this%receivers%size, rcv_req, rcv_stat, ierr)

    ! after WaitAll we can count incoming headers from statuses
    do i = 1, this%receivers%size
      call MPI_Get_count(rcv_stat(:,i), hdr_rcv_t(i), hdr_rcv_cnt(i), ierr)
      call MPI_Type_free(hdr_rcv_t(i), ierr)
    end do

    ! recv bodies
    do i = 1, this%senders%size
      rnk = this%senders%at(i)
      call this%message_builder%create_body_rcv(rnk, stage, body_rcv_t(i))
      call MPI_Irecv(MPI_BOTTOM, 1, body_rcv_t(i), rnk, stage, MF6_COMM_WORLD, snd_req(i), ierr)          
    end do

    ! send bodies
    do i = 1, this%receivers%size
      rnk = this%receivers%at(i)
      call this%message_builder%create_body_snd(rnk, stage, headers(1 : hdr_rcv_cnt(i), i), body_snd_t(i))
      call MPI_Isend(MPI_Bottom, 1, body_snd_t(i), rnk, stage, MF6_COMM_WORLD, rcv_req(i), ierr)      
    end do

    ! wait for exchange of all messages
    call MPI_WaitAll(this%senders%size, snd_req, snd_stat, ierr)

    ! clean up types
    do i = 1, this%senders%size
      call MPI_Type_free(body_rcv_t(i), ierr)  
    end do
    do i = 1, this%receivers%size
      call MPI_Type_free(body_snd_t(i), ierr)
    end do
    
    deallocate (rcv_req)
    deallocate (snd_req)
    deallocate (rcv_stat)
    deallocate (hdr_rcv_t)
    deallocate (hdr_snd_t)
    deallocate (headers)

  end subroutine mr_route_all

  subroutine mr_route_sln(this, virtual_sol, stage)
    class(MpiRouterType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage

    ! snd+rcv header

    ! snd+rcv maps

    ! snd+rcv data

    ! async. wait

  end subroutine mr_route_sln

  subroutine mr_update_senders(this)
    class(MpiRouterType) :: this
    ! local
    integer(I4B) :: i
    class(VirtualDataContainerType), pointer :: vdc

    call this%senders%clear()

    do i = 1, virtual_model_list%Count()
      vdc => get_vdc_from_list(virtual_model_list, i)
      if (vdc%is_remote .and. vdc%is_active) then
        call this%senders%push_back_unique(vdc%orig_rank)
      end if
    end do
    do i = 1, virtual_exchange_list%Count()
      vdc => get_vdc_from_list(virtual_exchange_list, i)
      if (vdc%is_remote .and. vdc%is_active) then
        call this%senders%push_back_unique(vdc%orig_rank)
      end if
    end do

  end subroutine mr_update_senders

  subroutine mr_update_receivers(this)
    class(MpiRouterType) :: this
    ! local
    integer(I4B) :: i

    call this%receivers%clear()

    ! assuming symmetry for now
    do i = 1, this%senders%size
      call this%receivers%push_back(this%senders%at(i))
    end do
    
  end subroutine mr_update_receivers

  subroutine mr_destroy(this)
    class(MpiRouterType) :: this

    call this%senders%destroy()
    call this%receivers%destroy()

    deallocate (this%model_proc_ids)

  end subroutine mr_destroy

end module MpiRouterModule
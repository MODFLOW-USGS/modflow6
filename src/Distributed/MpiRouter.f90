module MpiRouterModule
  use RouterBaseModule
  use KindModule, only: I4B
  use STLVecIntModule
  use SimVariablesModule, only: proc_id
  use SimStagesModule, only: STG_TO_STR
  use VirtualDataListsModule, only: virtual_model_list, &
                                    virtual_exchange_list
  use VirtualDataContainerModule, only: VirtualDataContainerType, &
                                        VdcPtrType, get_vdc_from_list
  use VirtualExchangeModule, only: VirtualExchangeType
  use VirtualSolutionModule
  use MpiMessageBuilderModule
  use MpiWorldModule
  use mpi
  implicit none
  private

  public :: create_mpi_router

  type, public, extends(RouterBaseType) :: MpiRouterType
    integer(I4B), dimension(:), pointer :: model_proc_ids
    type(STLVecInt) :: senders !< the process ids to receive data from
    type(STLVecInt) :: receivers !< the process ids to send data to
    type(VdcPtrType), dimension(:), pointer :: all_models => null() !< all virtual models from the global list
    type(VdcPtrType), dimension(:), pointer :: all_exchanges => null() !< all virtual exchanges from the global list
    type(VdcPtrType), dimension(:), pointer :: rte_models => null() !< the currently active models to be routed
    type(VdcPtrType), dimension(:), pointer :: rte_exchanges => null() !< the currently active exchanges to be routed
    type(MpiMessageBuilderType) :: message_builder
    type(MpiWorldType), pointer :: mpi_world => null()
  contains
    procedure :: initialize => mr_initialize
    procedure :: route_all => mr_route_all
    procedure :: route_sln => mr_route_sln
    procedure :: destroy => mr_destroy
    ! private
    procedure, private :: mr_update_senders
    procedure, private :: mr_update_senders_sln
    procedure, private :: mr_update_receivers
    procedure, private :: mr_update_receivers_sln
    procedure, private :: mr_route_active
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

    ! get mpi world for our process
    this%mpi_world => get_mpi_world()

    ! init address list
    call this%senders%init()
    call this%receivers%init()

    ! find out where models are
    nr_models = virtual_model_list%Count()
    nr_exchanges = virtual_exchange_list%Count()
    allocate (this%model_proc_ids(nr_models))
    allocate (this%all_models(nr_models))
    allocate (this%all_exchanges(nr_exchanges))

    do i = 1, nr_models
      vdc => get_vdc_from_list(virtual_model_list, i)
      this%all_models(i)%ptr => vdc
      if (vdc%is_local) then
        this%model_proc_ids(i) = proc_id
      else
        this%model_proc_ids(i) = 0
      end if
    end do

    call MPI_Allreduce(MPI_IN_PLACE, this%model_proc_ids, nr_models, &
                       MPI_INTEGER, MPI_SUM, MF6_COMM_WORLD, ierr)

    ! set the process id to the models and exchanges
    do i = 1, nr_models
      vdc => get_vdc_from_list(virtual_model_list, i)
      call vdc%set_orig_rank(this%model_proc_ids(i))
    end do

    do i = 1, nr_exchanges
      vdc => get_vdc_from_list(virtual_exchange_list, i)
      this%all_exchanges(i)%ptr => vdc
      select type (vex => vdc)
      class is (VirtualExchangeType)
        ! TODO_MJR: we set it from model1, or from model2 when model1 is local.
        ! This is problematic because when a remote exchange resides
        ! on two distinct processes we cannot synchronize in one sweep...
        ! We need refactoring of Exchanges here, such that Exchange <=> rank
        ! is always 1-to-1.
        call vex%set_orig_rank(vex%v_model1%orig_rank)
        if (vex%v_model1%is_local) then
          call vex%set_orig_rank(vex%v_model2%orig_rank)
        end if
      end select
    end do

  end subroutine mr_initialize

  !> @brief This will route all remote data from the
  !! global models and exchanges over MPI, for a
  !< given stage
  subroutine mr_route_all(this, stage)
    class(MpiRouterType) :: this
    integer(I4B) :: stage

    ! data to route
    this%rte_models => this%all_models
    this%rte_exchanges => this%all_exchanges
    call this%message_builder%attach_data(this%rte_models, &
                                          this%rte_exchanges)

    ! route all
    call this%mr_route_active(stage)

    ! release
    this%rte_models => null()
    this%rte_exchanges => null()
    call this%message_builder%release_data()

  end subroutine mr_route_all

  !> @brief This will route all remote data from models
  !! and exchanges in a particular solution over MPI,
  !< for a given stage
  subroutine mr_route_sln(this, virtual_sol, stage)
    class(MpiRouterType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage

    ! data to route
    this%rte_models => virtual_sol%models
    this%rte_exchanges => virtual_sol%exchanges
    call this%message_builder%attach_data(virtual_sol%models, &
                                          virtual_sol%exchanges)

    ! route for this solution
    call this%mr_route_active(stage)

    ! release
    this%rte_models => null()
    this%rte_exchanges => null()
    call this%message_builder%release_data()

  end subroutine mr_route_sln

  !> @brief Routes the models and exchanges
  !<
  subroutine mr_route_active(this, stage)
    class(MpiRouterType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i, rnk
    integer :: ierr
    ! mpi handles
    integer, dimension(:), allocatable :: rcv_req
    integer, dimension(:), allocatable :: snd_req
    integer, dimension(:, :), allocatable :: rcv_stat
    integer, dimension(:, :), allocatable :: snd_stat
    ! message header
    integer(I4B) :: max_headers
    type(VdcHeaderType), dimension(:, :), allocatable :: headers
    integer, dimension(:), allocatable :: hdr_rcv_t
    integer, dimension(:), allocatable :: hdr_snd_t
    integer, dimension(:), allocatable :: hdr_rcv_cnt
    ! message body
    integer, dimension(:), allocatable :: body_rcv_t
    integer, dimension(:), allocatable :: body_snd_t

    ! update adress list
    call this%mr_update_senders()
    call this%mr_update_receivers()

    ! allocate handles
    allocate (rcv_req(this%receivers%size))
    allocate (snd_req(this%senders%size))
    allocate (rcv_stat(MPI_STATUS_SIZE, this%receivers%size))
    allocate (snd_stat(MPI_STATUS_SIZE, this%senders%size))

    ! allocate header data
    max_headers = size(this%rte_models) + size(this%rte_exchanges)
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
      call MPI_Irecv(headers(:, i), max_headers, hdr_rcv_t(i), rnk, stage, &
                     MF6_COMM_WORLD, rcv_req(i), ierr)
      ! don't free mpi datatype, we need the count below
    end do

    ! send header for incoming data
    do i = 1, this%senders%size
      rnk = this%senders%at(i)
      call this%message_builder%create_header_snd(rnk, stage, hdr_snd_t(i))
      call MPI_Isend(MPI_BOTTOM, 1, hdr_snd_t(i), rnk, stage, &
                     MF6_COMM_WORLD, snd_req(i), ierr)
      call MPI_Type_free(hdr_snd_t(i), ierr)
    end do

    ! wait for exchange of all headers
    call MPI_WaitAll(this%receivers%size, rcv_req, rcv_stat, ierr)

    ! after WaitAll we can count incoming headers from statuses
    do i = 1, this%receivers%size
      call MPI_Get_count(rcv_stat(:, i), hdr_rcv_t(i), hdr_rcv_cnt(i), ierr)
      call MPI_Type_free(hdr_rcv_t(i), ierr)
    end do

    ! recv bodies
    do i = 1, this%senders%size
      rnk = this%senders%at(i)
      call this%message_builder%create_body_rcv(rnk, stage, body_rcv_t(i))
      call MPI_Irecv(MPI_BOTTOM, 1, body_rcv_t(i), rnk, stage, &
                     MF6_COMM_WORLD, snd_req(i), ierr)
    end do

    ! send bodies
    do i = 1, this%receivers%size
      rnk = this%receivers%at(i)
      call this%message_builder%create_body_snd( &
        rnk, stage, headers(1:hdr_rcv_cnt(i), i), body_snd_t(i))
      call MPI_Isend(MPI_Bottom, 1, body_snd_t(i), rnk, stage, &
                     MF6_COMM_WORLD, rcv_req(i), ierr)
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

  end subroutine mr_route_active

  subroutine mr_update_senders(this)
    class(MpiRouterType) :: this
    ! local
    integer(I4B) :: i
    class(VirtualDataContainerType), pointer :: vdc

    call this%senders%clear()

    do i = 1, size(this%rte_models)
      vdc => this%rte_models(i)%ptr
      if (.not. vdc%is_local .and. vdc%is_active) then
        call this%senders%push_back_unique(vdc%orig_rank)
      end if
    end do
    do i = 1, size(this%rte_exchanges)
      vdc => this%rte_exchanges(i)%ptr
      if (.not. vdc%is_local .and. vdc%is_active) then
        call this%senders%push_back_unique(vdc%orig_rank)
      end if
    end do

  end subroutine mr_update_senders

  subroutine mr_update_senders_sln(this, virtual_sol)
    class(MpiRouterType) :: this
    type(VirtualSolutionType) :: virtual_sol
    ! local
    integer(I4B) :: i
    class(VirtualDataContainerType), pointer :: vdc

    call this%senders%clear()

    do i = 1, size(virtual_sol%models)
      vdc => virtual_sol%models(i)%ptr
      if (.not. vdc%is_local .and. vdc%is_active) then
        call this%senders%push_back_unique(vdc%orig_rank)
      end if
    end do
    do i = 1, size(virtual_sol%exchanges)
      vdc => virtual_sol%exchanges(i)%ptr
      if (.not. vdc%is_local .and. vdc%is_active) then
        call this%senders%push_back_unique(vdc%orig_rank)
      end if
    end do

  end subroutine mr_update_senders_sln

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

  subroutine mr_update_receivers_sln(this, virtual_sol)
    class(MpiRouterType) :: this
    type(VirtualSolutionType) :: virtual_sol
    ! local
    integer(I4B) :: i

    call this%receivers%clear()

    ! assuming symmetry for now
    do i = 1, this%senders%size
      call this%receivers%push_back(this%senders%at(i))
    end do

  end subroutine mr_update_receivers_sln

  subroutine mr_destroy(this)
    class(MpiRouterType) :: this

    call this%senders%destroy()
    call this%receivers%destroy()

    deallocate (this%model_proc_ids)
    deallocate (this%all_models)
    deallocate (this%all_exchanges)

  end subroutine mr_destroy

end module MpiRouterModule

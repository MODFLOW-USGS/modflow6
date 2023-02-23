module VirtualDataManagerModule
  use KindModule, only: I4B
  use STLVecIntModule
  use VirtualDataListsModule, only: virtual_model_list, virtual_exchange_list
  use VirtualModelModule, only: get_virtual_model
  use VirtualExchangeModule, only: get_virtual_exchange
  use VirtualSolutionModule
  use VirtualDataContainerModule
  use RouterBaseModule
  use RouterFactoryModule, only: create_router
  use NumericalSolutionModule, only: NumericalSolutionType
  use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList
  use NumericalExchangeModule, only: NumericalExchangeType, &
                                     GetNumericalExchangeFromList
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                          get_smc_from_list
  implicit none
  private

  ! TODO_MJR: virtual solution should be a virtual data
  ! collection targeting a single remote address
  type, public :: VirtualDataManagerType
    integer(I4B) :: nr_solutions
    integer(I4B), dimension(:), allocatable :: solution_ids
    class(VirtualSolutionType), dimension(:), pointer :: virtual_solutions
    class(RouterBaseType), pointer :: router
  contains
    procedure :: create => vds_create
    procedure :: init => vds_init
    procedure :: add_solution => vds_add_solution
    procedure :: synchronize => vds_synchronize
    procedure :: synchronize_sln => vds_synchronize_sln
    procedure :: destroy

    ! private
    procedure, private :: vds_synchronize
    procedure, private :: vds_synchronize_sln
    procedure, private :: prepare_all
    procedure, private :: link_all
    procedure, private :: prepare_solution
    procedure, private :: update_solution
    procedure, private :: link
    procedure, private :: count_nr_solutions
  end type VirtualDataManagerType

contains

  !> @brief Initialize the virtual data store
  subroutine vds_create(this, sim_mode)
    class(VirtualDataManagerType) :: this
    character(len=*) :: sim_mode
    ! local
    integer(I4B) :: nr_sol

    nr_sol = this%count_nr_solutions()
    allocate (this%virtual_solutions(nr_sol))
    allocate (this%solution_ids(nr_sol))

    ! we use this one as a counter:
    this%nr_solutions = 0

    ! create a router, sequential or parallel
    this%router => create_router(sim_mode)

  end subroutine vds_create

  !> @brief Initialize internal components
  !<
  subroutine vds_init(this)
    class(VirtualDataManagerType) :: this

    call this%router%initialize()

  end subroutine

  !> @brief Add the models and exchanges from the passed solution
  !! to the virtual data structure. This can then be used
  !< to efficiently sync only this particular solution.
  subroutine vds_add_solution(this, num_sol)
    class(VirtualDataManagerType) :: this
    class(NumericalSolutionType), pointer :: num_sol
    ! local
    integer(I4B) :: i, im, ix, ihm, ihx
    class(VirtualSolutionType), pointer :: virt_sol
    class(NumericalModelType), pointer :: num_mod
    class(NumericalExchangeType), pointer :: num_exg
    class(SpatialModelConnectionType), pointer :: conn
    integer(I4B) :: model_id, exg_id
    type(STLVecInt) :: model_ids, exchange_ids
    class(VirtualDataContainerType), pointer :: vdc

    this%nr_solutions = this%nr_solutions + 1
    virt_sol => this%virtual_solutions(this%nr_solutions)

    call model_ids%init()
    call exchange_ids%init()

    ! build the virtual solution
    this%solution_ids(this%nr_solutions) = num_sol%id ! TODO_MJR: do we need this double bookkeeping?
    virt_sol%solution_id = num_sol%id

    ! let's start with adding all models and exchanges from the global list
    ! (we will make them inactive when they are remote and not part of
    ! our halo)
    do im = 1, num_sol%modellist%Count()
      num_mod => GetNumericalModelFromList(num_sol%modellist, im)
      call model_ids%push_back(num_mod%id)
    end do

    ! loop over exchanges in solution and get connections
    do ix = 1, num_sol%exchangelist%Count()
      conn => get_smc_from_list(num_sol%exchangelist, ix)
      if (.not. associated(conn)) then
        ! it's a classic exchange, add it
        num_exg => GetNumericalExchangeFromList(num_sol%exchangelist, ix)
        call exchange_ids%push_back_unique(num_exg%id)
        cycle
      end if

      ! it's an interface model based exchanged, get
      ! halo models and halo exchanges from connection
      do ihm = 1, conn%haloModels%size
        model_id = conn%haloModels%at(ihm)
        call model_ids%push_back_unique(model_id)
      end do
      do ihx = 1, conn%haloExchanges%size
        exg_id = conn%haloExchanges%at(ihx)
        call exchange_ids%push_back_unique(exg_id)
      end do

    end do

    allocate (virt_sol%models(model_ids%size))
    allocate (virt_sol%exchanges(exchange_ids%size))

    ! select virtual containers for models/exchanges
    do i = 1, model_ids%size
      vdc => get_virtual_model(model_ids%at(i))
      virt_sol%models(i)%ptr => vdc
    end do
    do i = 1, exchange_ids%size
      vdc => get_virtual_exchange(exchange_ids%at(i))
      virt_sol%exchanges(i)%ptr => vdc
    end do

    ! cleanup
    call model_ids%destroy()
    call exchange_ids%destroy()

  end subroutine vds_add_solution

  !> @brief Synchronize the full virtual data store for this stage
  !<
  subroutine vds_synchronize(this, stage)
    class(VirtualDataManagerType) :: this
    integer(I4B) :: stage

    call this%prepare_all(stage)
    call this%link_all(stage)
    call this%router%route_all(stage)

  end subroutine vds_synchronize

  subroutine prepare_all(this, stage)
    class(VirtualDataManagerType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i
    class(VirtualDataContainerType), pointer :: vdc

    ! prepare all virtual data for this stage
    do i = 1, virtual_model_list%Count()
      vdc => get_vdc_from_list(virtual_model_list, i)
      call vdc%prepare_stage(stage)
    end do
    do i = 1, virtual_exchange_list%Count()
      vdc => get_vdc_from_list(virtual_exchange_list, i)
      call vdc%prepare_stage(stage)
    end do

  end subroutine prepare_all

  subroutine link_all(this, stage)
    class(VirtualDataManagerType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i
    class(VirtualDataContainerType), pointer :: vdc

    ! link all local objects
    do i = 1, virtual_model_list%Count()
      vdc => get_vdc_from_list(virtual_model_list, i)
      call vdc%link_items(stage)
    end do
    do i = 1, virtual_exchange_list%Count()
      vdc => get_vdc_from_list(virtual_exchange_list, i)
      call vdc%link_items(stage)
    end do

  end subroutine link_all

  !> @brief Synchronize one particular solution for this stage
  !<
  subroutine vds_synchronize_sln(this, id_sln, stage)
    use ArrayHandlersModule, only: ifind
    class(VirtualDataManagerType) :: this
    integer(I4B) :: id_sln !< the id of the solution
    integer(I4B) :: stage
    ! local
    integer(I4B) :: sol_idx

    sol_idx = ifind(this%solution_ids, id_sln)
    call this%prepare_solution(this%virtual_solutions(sol_idx), stage)
    call this%update_solution(this%virtual_solutions(sol_idx), stage)

  end subroutine vds_synchronize_sln

  !> @brief Force the virtual data containers (models,
  !! exchanges, etc.) to schedule their virtual data
  !< items for synchronization
  subroutine prepare_solution(this, virtual_sol, stage)
    class(VirtualDataManagerType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i
    class(VirtualDataContainerType), pointer :: vdc

    do i = 1, size(virtual_sol%models)
      vdc => virtual_sol%models(i)%ptr
      call vdc%prepare_stage(stage)
    end do

    do i = 1, size(virtual_sol%exchanges)
      vdc => virtual_sol%exchanges(i)%ptr
      call vdc%prepare_stage(stage)
    end do

  end subroutine prepare_solution

  !> @brief Update this virtual solution in two steps:
  !!
  !!   1) link all items that are local
  !!   2) route data for items that are remote
  !<
  subroutine update_solution(this, virtual_sol, stage)
    class(VirtualDataManagerType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage

    ! local objects are linked
    call this%link(virtual_sol, stage)

    ! remote objects are routed
    call this%router%route_sln(virtual_sol, stage)

  end subroutine update_solution

  !> @brief Connect virtual memory items to their
  !< sources when they are local for this stage
  subroutine link(this, virtual_sol, stage)
    class(VirtualDataManagerType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i
    class(VirtualDataContainerType), pointer :: vdc

    do i = 1, size(virtual_sol%models)
      vdc => virtual_sol%models(i)%ptr
      call vdc%link_items(stage)
    end do

    do i = 1, size(virtual_sol%exchanges)
      vdc => virtual_sol%exchanges(i)%ptr
      call vdc%link_items(stage)
    end do

  end subroutine link

  !> @brief Returns the number of Numerical Solutions
  !< in this simulation
  function count_nr_solutions(this) result(count)
    use ListsModule, only: basesolutionlist
    use NumericalSolutionModule, only: NumericalSolutionType
    class(VirtualDataManagerType) :: this
    integer(I4B) :: count
    ! local
    integer(I4B) :: isol
    class(*), pointer :: sol

    ! count nr. of numerical solutions
    count = 0
    do isol = 1, basesolutionlist%Count()
      sol => basesolutionlist%GetItem(isol)
      select type (sol)
      class is (NumericalSolutionType)
        count = count + 1
      end select
    end do

  end function count_nr_solutions

  subroutine destroy(this)
    class(VirtualDataManagerType) :: this
    ! local
    integer(I4B) :: i
    class(VirtualDataContainerType), pointer :: vdc

    do i = 1, virtual_model_list%Count()
      vdc => get_vdc_from_list(virtual_model_list, i)
      call vdc%destroy()
    end do

    do i = 1, virtual_exchange_list%Count()
      vdc => get_vdc_from_list(virtual_exchange_list, i)
      call vdc%destroy()
    end do

    do i = 1, this%nr_solutions
      deallocate (this%virtual_solutions(i)%models)
      deallocate (this%virtual_solutions(i)%exchanges)
    end do
    deallocate (this%virtual_solutions)

  end subroutine destroy

end module VirtualDataManagerModule

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
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                          GetSpatialModelConnectionFromList
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
    allocate(this%virtual_solutions(nr_sol))
    allocate(this%solution_ids(nr_sol))
    
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

  subroutine vds_add_solution(this, num_sol)
    class(VirtualDataManagerType) :: this
    class(NumericalSolutionType), pointer :: num_sol
    ! local
    integer(I4B) :: i, ix, im, ihx
    class(VirtualSolutionType), pointer :: virt_sol
    class(SpatialModelConnectionType), pointer :: conn
    integer(I4B) :: model_id, exg_id
    type(STLVecInt) :: model_ids, exchange_ids
    class(*), pointer :: vdc

    this%nr_solutions = this%nr_solutions + 1
    virt_sol => this%virtual_solutions(this%nr_solutions)

    call model_ids%init()
    call exchange_ids%init()

    ! build the virtual solution       
    this%solution_ids(this%nr_solutions) = num_sol%id ! TODO_MJR: do we need this double bookkeeping?
    virt_sol%solution_id = num_sol%id

    ! loop over exchanges in solution and get connections
    do ix = 1, num_sol%exchangelist%Count()
      conn => GetSpatialModelConnectionFromList(num_sol%exchangelist, ix)
      if (.not. associated(conn)) cycle

      ! get halo models and halo exchanges from connection
      do im = 1, conn%haloModels%size
        model_id = conn%haloModels%at(im)
        call model_ids%push_back_unique(model_id)
      end do
      do ihx = 1, conn%haloExchanges%size
        exg_id = conn%haloExchanges%at(ihx)
        call exchange_ids%push_back_unique(exg_id)
      end do

    end do    

    ! select virtual containers for models/exchanges
    do i = 1, model_ids%size
      vdc => get_virtual_model(model_ids%at(i))
      call virt_sol%models%Add(vdc)
    end do
    do i = 1, exchange_ids%size
      vdc => get_virtual_exchange(exchange_ids%at(i))
      call virt_sol%exchanges%Add(vdc)
    end do

    ! cleanup
    call model_ids%destroy()
    call exchange_ids%destroy()

  end subroutine vds_add_solution

  !> @brief Synchronize the full virtual data store for this stage
  !<
  subroutine vds_synchronize(this, stage)
    use MpiWorldModule
    use VirtualModelModule
    use SimVariablesModule, only: proc_id
    use SimStagesModule
    class(VirtualDataManagerType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i
    integer(I4B) :: nodes
    class(VirtualModelType), pointer :: vm
    type(MpiWorldType), pointer :: mpi_world

    mpi_world => get_mpi_world()

    call this%prepare_all(stage)
    call this%link_all(stage)

    ! prepare all virtual data for this stage
    call mpi_world%begin_order()
    write(*,'(a,i0)') 'rank ', proc_id
    do i = 1, virtual_model_list%Count()
      vm => get_virtual_model_from_list(virtual_model_list, i)
      if (associated(vm%dis_nodes%virtual_mt)) nodes = vm%dis_nodes%get()
      write(*,*) 'after linking stage ', trim(STG_TO_STR(stage)), ', model ', vm%name, ': dis nodes = ', nodes
    end do
    call mpi_world%end_order()    

    call this%router%route_all(stage)

    call mpi_world%begin_order()
    write(*,'(a,i0)') 'rank ', proc_id
    do i = 1, virtual_model_list%Count()
      vm => get_virtual_model_from_list(virtual_model_list, i)
      if (associated(vm%dis_nodes%virtual_mt)) nodes = vm%dis_nodes%get()
      write(*,*) 'after routing stage ', trim(STG_TO_STR(stage)), ', model ', vm%name, ': dis nodes = ', nodes
    end do
    call mpi_world%end_order()

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
      if (.not. vdc%is_remote) then
        call vdc%link_items(stage)
      end if
    end do
    do i = 1, virtual_exchange_list%Count()
      vdc => get_vdc_from_list(virtual_exchange_list, i)
      if (.not. vdc%is_remote) then
        call vdc%link_items(stage)
      end if
    end do

  end subroutine link_all

  !> @brief Synchronize one particular solution for this stage  
  !<
  subroutine vds_synchronize_sln(this, id_sln, stage)
    class(VirtualDataManagerType) :: this
    integer(I4B) :: id_sln !< the id of the solution
    integer(I4B) :: stage
    ! local
    integer(I4B) :: sol_idx
    
    sol_idx = findloc(this%solution_ids, id_sln, dim=1)
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

    do i = 1, virtual_sol%models%Count()      
      vdc => get_vdc_from_list(virtual_sol%models, i)
      call vdc%prepare_stage(stage)
    end do

    do i = 1, virtual_sol%exchanges%Count()
      vdc => get_vdc_from_list(virtual_sol%exchanges, i)
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

    do i = 1, virtual_sol%models%Count()
      vdc => get_vdc_from_list(virtual_sol%models, i)
      if (.not. vdc%is_remote) then
        call vdc%link_items(stage)
      end if
    end do

    do i = 1, virtual_sol%exchanges%Count()
      vdc => get_vdc_from_list(virtual_sol%exchanges, i)
      if (.not. vdc%is_remote) then
        call vdc%link_items(stage)
      end if
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

    deallocate(this%virtual_solutions)

  end subroutine destroy

end module VirtualDataManagerModule
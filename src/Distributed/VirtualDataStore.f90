module VirtualDataStoreModule  
  use KindModule, only: I4B
  use STLVecIntModule
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

  type, public :: VirtualDataStoreType
    integer(I4B) :: nr_solutions
    integer(I4B), dimension(:), allocatable :: solution_ids
    class(VirtualSolutionType), dimension(:), pointer :: virtual_solutions
    class(RouterBaseType), pointer :: router
  contains
    procedure :: create => vds_create
    procedure :: add_solution => vds_add_solution
    generic :: synchronize => vds_synchronize, vds_synchronize_by_id
    procedure :: destroy

    ! private
    procedure, private :: vds_synchronize
    procedure, private :: vds_synchronize_by_id
    procedure, private :: sync_solution
    procedure, private :: prepare_solution
    procedure, private :: update_solution
    procedure, private :: link
    procedure, private :: count_nr_solutions
  end type VirtualDataStoreType

contains

  !> @brief Initialize the virtual data store
  subroutine vds_create(this, sim_mode)
    class(VirtualDataStoreType) :: this
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

  subroutine vds_add_solution(this, num_sol)
    class(VirtualDataStoreType) :: this
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

  !> @brief Synchronize the virtual data store for this stage
  !<
  subroutine vds_synchronize(this, stage)
    class(VirtualDataStoreType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i
    
    do i = 1, this%nr_solutions
      call this%sync_solution(i, stage)
    end do

  end subroutine vds_synchronize

  !> @brief Synchronize one particular solution for this stage  
  !<
  subroutine vds_synchronize_by_id(this, id_sln, stage)
    class(VirtualDataStoreType) :: this    
    integer(I4B) :: id_sln !< the id of the solution
    integer(I4B) :: stage
    ! local
    integer(I4B) :: sol_index
    
    sol_index = findloc(this%solution_ids, id_sln, dim=1)
    call this%sync_solution(sol_index, stage)    

  end subroutine vds_synchronize_by_id

  !> @brief Internal routine to sync a solution
  !<
  subroutine sync_solution(this, sol_idx, stage)
    class(VirtualDataStoreType) :: this    
    integer(I4B) :: sol_idx !< the index of the solution in our array of solution ids
    integer(I4B) :: stage

    call this%prepare_solution(this%virtual_solutions(sol_idx), stage)
    call this%update_solution(this%virtual_solutions(sol_idx), stage)

  end subroutine sync_solution

  !> @brief Force the virtual data containers (models,
  !! exchanges, etc.) to schedule their virtual data
  !< items for synchronization
  subroutine prepare_solution(this, virtual_sol, stage)
    class(VirtualDataStoreType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i
    class(*), pointer :: vdc

    do i = 1, virtual_sol%models%Count()
      vdc => virtual_sol%models%GetItem(i)
      select type (vdc)
        class is (VirtualDataContainerType)
          call vdc%prepare_stage(stage)
      end select
    end do

    do i = 1, virtual_sol%exchanges%Count()
      vdc => virtual_sol%exchanges%GetItem(i)
      select type (vdc)
        class is (VirtualDataContainerType)
          call vdc%prepare_stage(stage)
      end select
    end do

  end subroutine prepare_solution

  !> @brief Update this virtual solution in two steps:
  !!
  !!   1) link all items that are local
  !!   2) route data for items that are remote
  !<
  subroutine update_solution(this, virtual_sol, stage)
    class(VirtualDataStoreType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage

    ! local objects are linked
    call this%link(virtual_sol, stage)

    ! remote objects are routed
    call this%router%route(virtual_sol, stage)

  end subroutine update_solution

  !> @brief Connect virtual memory items to their  
  !< sources when they are local for this stage
  subroutine link(this, virtual_sol, stage)
    class(VirtualDataStoreType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i
    class(*), pointer :: vdc

    do i = 1, virtual_sol%models%Count()
      vdc => virtual_sol%models%GetItem(i)
      select type (vdc)
        class is (VirtualDataContainerType)
          if (.not. vdc%is_remote) then
            call vdc%link_items(stage)
          end if
      end select
    end do

    do i = 1, virtual_sol%exchanges%Count()
      vdc => virtual_sol%exchanges%GetItem(i)
      select type (vdc)
        class is (VirtualDataContainerType)
          if (.not. vdc%is_remote) then
            call vdc%link_items(stage)
          end if
      end select
    end do

  end subroutine link

  !> @brief Returns the number of Numerical Solutions 
  !< in this simulation
  function count_nr_solutions(this) result(count)
    use ListsModule, only: basesolutionlist
    use NumericalSolutionModule, only: NumericalSolutionType   
    class(VirtualDataStoreType) :: this
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
    class(VirtualDataStoreType) :: this

    ! TODO_MJR: clean virtual data lists

    deallocate(this%virtual_solutions)

  end subroutine destroy

end module VirtualDataStoreModule
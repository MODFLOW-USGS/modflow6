!> @brief Entry point for concept of distributed data (serial and parallel)
!!
!! This should hide all implementation from the main simulation code, with
!! the only elements leaking through being the subroutines in this module
!< and the concept of distributed models and exchanges.
module Mf6DistributedModule
  use KindModule, only: I4B
  use SimStagesModule
  use DistListsModule, only: distmodellist, distexchangelist
  use DistributedModelModule, only: DistributedModelType, get_dist_model
  use DistributedExchangeModule, only: DistributedExchangeType
  use NumericalSolutionModule, only: NumericalSolutionType
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                          GetSpatialModelConnectionFromList
  use ListsModule, only: basesolutionlist
  use BaseSolutionModule, only: BaseSolutionType, GetBaseSolutionFromList  
  use RouterModule, only: RouterType
  use MapperModule, only: MapperType  
  use InterfaceMapModule
  implicit none
  private
  
  type, public :: Mf6DistributedDataType

    type(RouterType) :: router
    type(MapperType) :: mapper

  contains

    procedure :: dd_init
    procedure :: dd_before_df
    procedure :: dd_after_df
    procedure :: dd_before_ar
    procedure :: dd_after_ar
    procedure :: dd_finalize
    
    procedure, private :: reduce_map

  end type Mf6DistributedDataType

  type(Mf6DistributedDataType), public :: mf6_dist_data !< this is global now, but could be part of 
                                                        !! a simulation object in the future

contains

  !> @brief Initialize the distributed simulation
  !<
  subroutine dd_init(this)
    class(Mf6DistributedDataType) :: this
    ! local
    integer(I4B) :: isol, nsol
    class(BaseSolutionType), pointer :: sol

    ! first count num. solutions
    nsol = 0
    do isol = 1, basesolutionlist%Count()
      sol => GetBaseSolutionFromList(basesolutionlist, isol)
      select type (sol)
      class is (NumericalSolutionType)
        nsol = nsol + 1
      end select
    end do

    call this%router%init(nsol)
    call this%mapper%init()

  end subroutine dd_init

  !> @brief Performs linking and synchronization before
  !< the DF on connections
  subroutine dd_before_df(this)
    class(Mf6DistributedDataType), target :: this
    ! local
    class(BaseSolutionType), pointer :: sol
    integer(I4B) ::isol

    do isol = 1, basesolutionlist%Count()
      sol => GetBaseSolutionFromList(basesolutionlist, isol)
      select type (sol)
      class is (NumericalSolutionType)
        call this%router%add_solution(sol)
        sol%synchronize => dd_solution_sync
        sol%synchronize_ctx => this
      end select
    end do

    call this%router%route(STG_BEFORE_INIT)
    call this%router%init_connectivity()
    
    call this%router%route(STG_BEFORE_DF)

  end subroutine dd_before_df

  !> @brief Called after DF on connections, which is when
  !! the interface model grid has been constructed
  !<
  subroutine dd_after_df(this)
    use ListsModule, only: baseconnectionlist
    class(Mf6DistributedDataType) :: this
    ! local
    integer(I4B) :: iconn
    class(SpatialModelConnectionType), pointer :: conn
  
    ! merge the interface maps to determine all nodes in the interface
    call this%router%create_interface_map()

    ! map the interface data
    do iconn = 1, baseconnectionlist%Count()
      conn => GetSpatialModelConnectionFromList(baseconnectionlist, iconn)
      
      ! for all remote models we need to remap into the reduced memory space
      call this%reduce_map(conn%interfaceMap, &
                           this%router%interface_maps(conn%owner%idsoln))            
      
      ! add the variables with the updated map
      call this%mapper%add_dist_vars(conn%owner%idsoln, &
                                conn%ifaceDistVars, &
                                conn%interfaceMap)
    end do

    ! use the final maps to initialize the data structures and,
    ! when remote, decompose the maps
    call this%router%init_interface()

  end subroutine dd_after_df
  
  subroutine reduce_map(this, map, merged_map)
    class(Mf6DistributedDataType) :: this
    type(InterfaceMapType) :: map ! map to reduce
    type(InterfaceMapType) :: merged_map ! full interface map (for a solution)
    ! local
    integer(I4B) :: im, i, m_idx
    integer(I4B) :: orig_idx, reduced_idx
    class(DistributedModelType), pointer :: dist_model

    ! reduction means remapping the source indexes 
    ! into the distributed model or exchange memory buffer
    do im = 1, map%nr_models
      ! only when remote model
      dist_model => get_dist_model(map%model_ids(im))
      if (dist_model%is_local) cycle

      m_idx = findloc(merged_map%model_ids, map%model_ids(im), dim=1)      
      do i = 1, size(map%node_map(im)%src_idx)
        orig_idx = map%node_map(im)%src_idx(i)        
        reduced_idx = findloc(merged_map%node_map(m_idx)%src_idx, orig_idx, dim=1)
        map%node_map(im)%src_idx(i) = reduced_idx
      end do
      do i = 1, size(map%connection_map(im)%src_idx)
        orig_idx = map%connection_map(im)%src_idx(i)
        reduced_idx = findloc(merged_map%connection_map(m_idx)%src_idx, orig_idx, dim=1)
        map%connection_map(im)%src_idx(i) = reduced_idx
      end do
    end do

  end subroutine reduce_map

  !> @brief Called before AR on connections
  !<
  subroutine dd_before_ar(this)
    class(Mf6DistributedDataType) :: this

    call this%router%route(STG_BEFORE_AR)
    call this%mapper%scatter(0, STG_BEFORE_AR)

  end subroutine dd_before_ar

  !> @brief Called after AR on connections
  !<
  subroutine dd_after_ar(this)
    class(Mf6DistributedDataType) :: this

    call this%router%route(STG_AFTER_AR)
    call this%mapper%scatter(0, STG_AFTER_AR)

  end subroutine dd_after_ar

  !> @brief Synchronizes from within numerical solution (delegate)
  !<
  subroutine dd_solution_sync(num_sol, stage, ctx)
    class(NumericalSolutionType) :: num_sol
    integer(I4B) :: stage
    class(*), pointer :: ctx

    select type (ctx)
    class is (Mf6DistributedDataType)
      call ctx%router%route(num_sol%id, stage)
      call ctx%mapper%scatter(num_sol%id, stage)
    end select

  end subroutine dd_solution_sync

  !> @brief clean up
  !<
  subroutine dd_finalize(this)
    use DistributedModelModule, only: GetDistModelFromList
    use DistributedExchangeModule, only: GetDistExchangeFromList
    class(Mf6DistributedDataType) :: this
    ! local
    integer(I4B) :: i
    class(DistributedModelType), pointer :: dist_mod
    class(DistributedExchangeType), pointer :: dist_exg

    call this%router%destroy()
    call this%mapper%destroy()

    do i = 1, distmodellist%Count()
      dist_mod => GetDistModelFromList(distmodellist, i)
      call dist_mod%destroy()
    end do
    do i = 1, distexchangelist%Count()
      dist_exg => GetDistExchangeFromList(distexchangelist, i)
      call dist_exg%deallocate()
    end do

    call distmodellist%Clear()
    call distexchangelist%Clear()

  end subroutine dd_finalize

end module Mf6DistributedModule

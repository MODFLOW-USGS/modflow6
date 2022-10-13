!> @brief Entry point for concept of distributed data (serial and parallel)
!!
!! This should hide all implementation from the main simulation code, with
!! the only elements leaking through being the subroutines in this module
!< and the concept of distributed models and exchanges.
module Mf6DistributedModule
  use KindModule, only: I4B
  use DistListsModule, only: distmodellist, distexchangelist
  use DistributedModelModule, only: DistributedModelType
  use DistributedExchangeModule, only: DistributedExchangeType
  use DistDataBuilderModule
  use DistVariableModule  
  use NumericalSolutionModule, only: NumericalSolutionType
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                          GetSpatialModelConnectionFromList
  use MapperModule, only: MapperType
  implicit none
  private

  public :: dd_init, dd_finalize  
  public :: dd_before_df, dd_after_df
  public :: dd_before_ar, dd_after_ar

  type(MapperType) :: mapper

contains

  !> @brief Initialize the distributed simulation
  !<
  subroutine dd_init()

    call mapper%init()

  end subroutine dd_init

  !> @brief Performs linking and synchronization before
  !< the DF on connections
  subroutine dd_before_df()
    use ListsModule, only: basesolutionlist
    use BaseSolutionModule, only: BaseSolutionType, GetBaseSolutionFromList
    ! local
    class(BaseSolutionType), pointer :: sol
    integer(I4B) ::isol
    type(DistDataBuilderType) :: dd_builder ! temporarily, to be replaced by factory method 
                                            ! getting either serial or parallel builder?!

    do isol = 1, basesolutionlist%Count()
      sol => GetBaseSolutionFromList(basesolutionlist, isol)
      select type (sol)
      class is (NumericalSolutionType)
        call dd_builder%connect_halo(sol)
        sol%synchronize => dd_solution_sync
      end select
    end do

  end subroutine dd_before_df

  !> @brief Called after DF on connections
  !<
  subroutine dd_after_df()
    use ListsModule, only: baseconnectionlist
    integer(I4B) :: iconn
    class(SpatialModelConnectionType), pointer :: conn

    ! map the variables for the interface grids
    do iconn = 1, baseconnectionlist%Count()
      conn => GetSpatialModelConnectionFromList(baseconnectionlist, iconn)
      call mapper%add_dist_vars(conn%owner%idsoln, &
                                conn%distVarList, &
                                conn%interfaceMap)
    end do

    ! interface grid is known
    ! - get the map: src_i => tgt_i
    ! - aggregate
    ! - split: map1: src_i => i, map2: i => tgt_i
    ! - pass map1 to router
    ! - pass map2 to interface filler

  end subroutine dd_after_df

  !> @brief Called before AR on connections
  !<
  subroutine dd_before_ar()

    call mapper%scatter(0, BEFORE_AR)

  end subroutine dd_before_ar

  !> @brief Called after AR on connections
  !<
  subroutine dd_after_ar()

    call mapper%scatter(0, AFTER_AR)

  end subroutine dd_after_ar

  !> @brief Synchronizes from within numerical solution (delegate)
  !<
  subroutine dd_solution_sync(num_sol, stage)
    class(NumericalSolutionType) :: num_sol
    integer(I4B) :: stage

    call mapper%scatter(num_sol%id, stage)

  end subroutine dd_solution_sync

  !> @brief clean up
  !<
  subroutine dd_finalize()
    use DistributedModelModule, only: GetDistModelFromList
    use DistributedExchangeModule, only: GetDistExchangeFromList
    integer(I4B) :: i
    class(DistributedModelType), pointer :: dist_mod
    class(DistributedExchangeType), pointer :: dist_exg

    call mapper%destroy()

    do i = 1, distmodellist%Count()
      dist_mod => GetDistModelFromList(distmodellist, i)
      call dist_mod%deallocate()
    end do
    do i = 1, distexchangelist%Count()
      dist_exg => GetDistExchangeFromList(distexchangelist, i)
      call dist_exg%deallocate()
    end do

    call distmodellist%Clear()
    call distexchangelist%Clear()

  end subroutine dd_finalize

end module Mf6DistributedModule

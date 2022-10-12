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
  implicit none
  private

  public :: dd_init, dd_finalize

contains

  !> @brief Initialize the distributed simulation
  !<
  subroutine dd_init()

  end subroutine dd_init

  !> @brief Performs linking and synchronization before
  !< the DF on connections
  subroutine dd_before_df()

  end subroutine dd_before_df

  !> @brief Called after DF on connections
  !<
  subroutine dd_after_df()

  end subroutine dd_after_df

  !> @brief Called before AR on connections
  !<
  subroutine dd_before_ar()

  end subroutine dd_before_ar

  !> @brief Called after AR on connections
  !<
  subroutine dd_after_ar()

  end subroutine dd_after_ar

  !> @brief Called before CF in a solution
  !<
  subroutine dd_before_cf(solution_id)
    integer(I4B) :: solution_id

  end subroutine dd_before_cf

  !> @brief Called after CF in a solution
  !<
  subroutine dd_after_cf(solution_id)
    integer(I4B) :: solution_id

  end subroutine dd_after_cf

  !> @brief Called before FC in a solution
  !<
  subroutine dd_before_fc(solution_id)
    integer(I4B) :: solution_id

  end subroutine dd_before_fc

  !> @brief Called after FC in a solution
  !<
  subroutine dd_after_fc(solution_id)
    integer(I4B) :: solution_id

  end subroutine dd_after_fc

  !> @brief clean up
  !<
  subroutine dd_finalize()
    use DistributedModelModule, only: GetDistModelFromList
    use DistributedExchangeModule, only: GetDistExchangeFromList
    integer(I4B) :: i
    class(DistributedModelType), pointer :: dist_mod
    class(DistributedExchangeType), pointer :: dist_exg

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

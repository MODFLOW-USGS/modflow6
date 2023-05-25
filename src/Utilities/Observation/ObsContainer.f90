!> @brief This module contains the derived type ObsContainerType
!!
!! This module contains the derived type ObsContainerType, which
!! contains a pointer to an object of type ObserveType.  Its purpose is
!! to allow ObserveType objects to be stored in an array.
!!
!<
module ObsContainerModule

  use KindModule, only: DP, I4B
  use ObserveModule, only: ObserveType

  implicit none

  private
  public :: ObsContainerType

  type :: ObsContainerType
    ! -- Public members
    class(ObserveType), pointer, public :: obsrv => null()
  end type ObsContainerType

end module ObsContainerModule

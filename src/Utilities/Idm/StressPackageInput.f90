!> @brief This module contains the StressPackageInputModule
!!
!! This module defines a type that manages input
!! context dynamic loading for a stress package.
!!
!! This type is not fully implemented.
!!
!<
module StressPackageInputModule

  use KindModule, only: DP, I4B, LGP
  use ModflowInputModule, only: ModflowInputType

  implicit none
  private
  public :: StressPackageInputType

  !> @brief StressPackageInputType, not fully implemented
  !<
  type :: StressPackageInputType
  contains
    procedure :: init => stresspkg_init
    procedure :: destroy => stresspkg_destroy
  end type StressPackageInputType

contains

  subroutine stresspkg_init(this, mf6_input, iout)
    ! -- modules
    ! -- dummy
    class(StressPackageInputType) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), intent(in) :: iout
    ! -- local
    !
    ! -- return
    return
  end subroutine stresspkg_init

  subroutine stresspkg_destroy(this)
    class(StressPackageInputType) :: this
    !
    ! -- return
    return
  end subroutine stresspkg_destroy

end module StressPackageInputModule

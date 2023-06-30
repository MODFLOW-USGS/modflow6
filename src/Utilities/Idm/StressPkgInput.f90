!> @brief This module contains the StressPkgInputModule
!!
!! This module defines a type that manages input
!! context dynamic loading for a stress package.
!!
!! This type is not fully implemented.
!!
!<
module StressPkgInputModule

  use KindModule, only: DP, I4B, LGP
  use ModflowInputModule, only: ModflowInputType

  implicit none
  private
  public :: StressPkgInputType

  !> @brief StressPkgInputType, not fully implemented
  !<
  type :: StressPkgInputType
  contains
    procedure :: init => stresspkg_init
    procedure :: destroy => stresspkg_destroy
  end type StressPkgInputType

contains

  subroutine stresspkg_init(this, mf6_input, iout)
    ! -- modules
    ! -- dummy
    class(StressPkgInputType) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), intent(in) :: iout
    ! -- local
    !
    ! -- return
    return
  end subroutine stresspkg_init

  subroutine stresspkg_destroy(this)
    class(StressPkgInputType) :: this
    !
    ! -- return
    return
  end subroutine stresspkg_destroy

end module StressPkgInputModule

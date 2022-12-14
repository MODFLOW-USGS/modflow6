module VirtualSolutionModule
  use KindModule, only: I4B
  use ListModule
  use VirtualDataContainerModule, only: VdcPtrType
  implicit none
  private

  !> This bundles all virtual data for a particular solution
  !< for convenience, it never owns any of it
  type, public :: VirtualSolutionType
    integer(I4B) :: solution_id = -1
    type(VdcPtrType), dimension(:), pointer :: models => null()
    type(VdcPtrType), dimension(:),  pointer :: exchanges => null()
    ! type(ListType) :: exchange_movers
    ! type(ListType) :: etc...
  end type VirtualSolutionType

end module VirtualSolutionModule
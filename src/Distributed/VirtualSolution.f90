module VirtualSolutionModule
  use KindModule, only: I4B
  use ListModule
  implicit none
  private

  type, public :: VirtualSolutionType
    integer(I4B) :: solution_id = -1
    type(ListType) :: models
    type(ListType) :: exchanges
    ! type(ListType) :: exchange_movers
    ! type(ListType) :: etc...
  end type VirtualSolutionType

end module VirtualSolutionModule
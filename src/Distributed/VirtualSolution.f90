module VirtualSolutionModule
  use KindModule, only: I4B
  use ListModule
  use VirtualDataContainerModule, only: VdcPtrType
  use InterfaceMapModule
  implicit none
  private

  !> This bundles all virtual data for a particular solution
  !< for convenience, it never owns any of it
  type, public :: VirtualSolutionType
    integer(I4B) :: solution_id = -1
    type(VdcPtrType), dimension(:), pointer :: models => null() !< the models as virtual data containers (wrapped)
    type(VdcPtrType), dimension(:), pointer :: exchanges => null() !< the exchanges as virtual data containers (wrapped)
    class(*), pointer :: numerical_solution => null() !< points back to the actual numerical solution
    type(InterfaceMapType), pointer :: interface_map => null() !< contains the aggregate interface map for the solution
                                                               !! NB: the aggregation is over multiple interface models
                                                               !! and there is no unique numbering there. The target
                                                               !! indexes should therefore be considered invalid.
  end type VirtualSolutionType

end module VirtualSolutionModule

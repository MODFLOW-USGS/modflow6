module GweCndOptionsModule
  use KindModule, only: I4B
  implicit none
  private

  !> @brief data structure (and helpers) for passing cnd option data
  !< into the package, as opposed to reading it from file
  type, public :: GweCndOptionsType
    integer(I4B) :: ixt3d !< flag indicating xt3d is active: 1 = enabled, 2 = rhs
  end type GweCndOptionsType

end module GweCndOptionsModule

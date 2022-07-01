module GwtDspOptionsModule
  use KindModule, only: I4B
  implicit none
  private

  !> @brief data structure (and helpers) for passing dsp option data
  !< into the package, as opposed to reading it from file
  type, public :: GwtDspOptionsType
    integer(I4B) :: ixt3d !< flag indicating xt3d is active: 1 = enabled, 2 = rhs
  end type GwtDspOptionsType

end module GwtDspOptionsModule

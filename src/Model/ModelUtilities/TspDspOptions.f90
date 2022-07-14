module TspDspOptionsModule
  use KindModule, only: I4B
  implicit none
  private

  !> @brief data structure (and helpers) for passing dsp option data
  !< into the package, as opposed to reading it from file
  type, public :: TspDspOptionsType
    integer(I4B) :: ixt3d !< flag indicating xt3d is active: 1 = enabled, 2 = rhs
  end type TspDspOptionsType

end module TspDspOptionsModule

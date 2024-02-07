module TspAdvOptionsModule
  use KindModule, only: I4B
  implicit none
  private

  type, public :: TspAdvOptionsType
    integer(I4B) :: iAdvScheme !< the advection scheme: 0 = up, 1 = central, 2 = TVD
  end type TspAdvOptionsType

end module TspAdvOptionsModule

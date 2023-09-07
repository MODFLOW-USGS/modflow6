!> @brief This module contains the base transport model type
!!
!! This module contains the base class for transport models.
!!
!<

module TransportModelModule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENFTYPE
  use SimVariablesModule, only: errmsg
  use NumericalModelModule, only: NumericalModelType

  implicit none

  private

  public :: TransportModelType

  type, extends(NumericalModelType) :: TransportModelType

  contains

  end type TransportModelType

end module TransportModelModule

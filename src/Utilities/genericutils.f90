!> @brief This module contains generic utilties
!!
!! This module contains generic utilities that have
!! limited dependencies.
!!
!<
module GenericUtilitiesModule
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: MAXCHARLEN, LENHUGELINE, &
                             DZERO, DPREC, DSAME, &
                             LINELENGTH, LENHUGELINE, VSUMMARY
  use SimVariablesModule, only: istdout, isim_level
  !
  implicit none

  private

  public :: is_same

contains

  !> @brief Function to determine if two reals are the same
  !!
  !! Function to evaluate if the difference between a and b are less than eps
  !! (i.e. a and b are the same).
  !!
  !<
  function is_same(a, b, eps) result(lvalue)
    ! -- return variable
    logical(LGP) :: lvalue !< boolean indicating if a and b are the same
    ! -- dummy variables
    real(DP), intent(in) :: a !< first number to evaluate
    real(DP), intent(in) :: b !< second number to evaluate
    real(DP), intent(in), optional :: eps !< optional maximum difference between a abd b (default=DSAME)
    ! -- local variables
    real(DP) :: epsloc
    real(DP) :: denom
    real(DP) :: rdiff
    !
    ! -- evaluate optioanl arguments
    if (present(eps)) then
      epsloc = eps
    else
      epsloc = DSAME
    end if
    lvalue = .FALSE.
    if (a == b) then
      lvalue = .TRUE.
    else
      if (abs(b) > abs(a)) then
        denom = b
      else
        denom = a
        if (abs(denom) == DZERO) then
          denom = DPREC
        end if
      end if
      rdiff = abs((a - b) / denom)
      if (rdiff <= epsloc) then
        lvalue = .TRUE.
      end if
    end if

  end function is_same

end module GenericUtilitiesModule

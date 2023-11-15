module MathUtilModule
  use KindModule, only: DP, I4B, LGP
  use ErrorUtilModule, only: pstop
  use ConstantsModule, only: MAXCHARLEN, LENHUGELINE, &
                             DZERO, DPREC, DSAME, &
                             LINELENGTH, LENHUGELINE, VSUMMARY

  implicit none
  private
  public :: mod_offset

  interface mod_offset
    module procedure :: mod_offset_int, mod_offset_dbl
  end interface mod_offset

contains

  !> @brief Modulo with offset for integer values.
  pure function mod_offset_int(a, n, d) result(mo)
    ! -- dummy
    integer(I4B), intent(in) :: a !< dividend
    integer(I4B), intent(in) :: n !< divisor
    integer(I4B), intent(in), optional :: d !< offset
    integer(I4B) :: mo
    ! -- local
    integer(I4B) :: ld

    if (present(d)) then
      ld = d
    else
      ld = 0
    end if
    mo = a - n * floor(real(a - ld) / n)
  end function mod_offset_int

  !> @brief Modulo with offset for double precision values.
  pure function mod_offset_dbl(a, n, d) result(mo)
    ! -- dummy
    real(DP), intent(in) :: a !< dividend
    real(DP), intent(in) :: n !< divisor
    real(DP), intent(in), optional :: d !< offset
    real(DP) :: mo
    ! -- local
    real(DP) :: ld

    if (present(d)) then
      ld = d
    else
      ld = 0
    end if
    mo = a - n * floor((a - ld) / n)
  end function mod_offset_dbl

end module MathUtilModule

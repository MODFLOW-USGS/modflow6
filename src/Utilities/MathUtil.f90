module MathUtilModule
  use KindModule, only: DP, I4B, LGP
  use ErrorUtilModule, only: pstop
  use ConstantsModule, only: MAXCHARLEN, LENHUGELINE, &
                             DZERO, DPREC, DSAME, &
                             LINELENGTH, LENHUGELINE, VSUMMARY

  implicit none
  private
  public :: is_same, is_close, mod_offset

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

  !> @brief Check if a real value is approximately equal to another.
  logical function is_same(a, b, eps)
    ! -- dummy
    real(DP), intent(in) :: a !< first real
    real(DP), intent(in) :: b !< second real
    real(DP), intent(in), optional :: eps !< relative tolerance (default=DSAME)
    ! -- local
    real(DP) :: epsloc
    real(DP) :: denom
    real(DP) :: rdiff

    ! -- evaluate optional arguments
    if (present(eps)) then
      epsloc = eps
    else
      epsloc = DSAME
    end if

    ! -- determine approximate equality
    is_same = .false.
    if (a == b) then
      is_same = .true.
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
        is_same = .true.
      end if
    end if
  end function is_same

  !> @brief Check if a real value is approximately equal to another.
  !!
  !! By default the determination is symmetric in a and b, as in
  !! Python's math.isclose, with relative difference scaled by a
  !! factor of the larger absolute value of a and b. The formula
  !! is: abs(a - b) <= max(rtol * max(abs(a), abs(b)), atol).
  !!
  !! If symmetric is set to false the test is asymmetric in a and
  !! b, with b taken to be the reference value, and the alternate
  !! formula (abs(a - b) <= (atol + rtol * abs(b))) is used. This
  !! is the approach taken by numpy.allclose.
  !!
  !! Defaults for rtol and atol are DSAME and DZERO, respectively.
  !! If a or b are near 0 (especially if either is 0), an absolute
  !! tolerance suitable for the particular case should be provided.
  !! For a justification of a zero absolute tolerance default see:
  !! https://peps.python.org/pep-0485/#absolute-tolerance-default
  !<
  logical function is_close(a, b, rtol, atol, symmetric)
    ! dummy
    real(DP), intent(in) :: a !< first real
    real(DP), intent(in) :: b !< second real (reference value if asymmetric)
    real(DP), intent(in), optional :: rtol !< relative tolerance (default=DSAME)
    real(DP), intent(in), optional :: atol !< absolute tolerance (default=DZERO)
    logical(LGP), intent(in), optional :: symmetric
    ! local
    real(DP) :: lrtol, latol
    logical(LGP) :: lsymmetric

    ! check for exact equality
    if (a == b) is_close = .true.

    ! process optional arguments
    if (.not. present(rtol)) then
      lrtol = DSAME
    else
      lrtol = rtol
    end if
    if (.not. present(atol)) then
      latol = DSAME
    else
      latol = atol
    end if
    if (.not. present(symmetric)) then
      lsymmetric = .true.
    else
      lsymmetric = symmetric
    end if

    ! approximate equality
    if (lsymmetric) then
      ! "weak" symmetric test, https://peps.python.org/pep-0485/#which-symmetric-test
      is_close = abs(a - b) <= max(lrtol * max(abs(a), abs(b)), latol)
    else
      ! asymmetric, https://numpy.org/doc/stable/reference/generated/numpy.isclose.html
      is_close = (abs(a - b) <= (latol + lrtol * abs(b)))
    end if
  end function is_close

end module MathUtilModule

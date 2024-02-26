!> @brief General-purpose hydrogeologic functions.
module HGeoUtilModule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE

  implicit none
  private
  public :: hyeff

contains

  !> @brief Calculate the effective horizontal hydraulic conductivity from an
  !! ellipse using a specified direction (unit vector vg1, vg2, vg3)
  !!
  !! k11 is the hydraulic conductivity of the major ellipse axis
  !! k22 is the hydraulic conductivity of first minor axis
  !! k33 is the hydraulic conductivity of the second minor axis
  !! ang1 is the counter-clockwise rotation (radians) of the ellipse in
  !!   the (x, y) plane
  !! ang2 is the rotation of the conductivity ellipsoid upward or
  !!   downward from the (x, y) plane
  !! ang3 is the rotation of the conductivity ellipsoid about the major
  !!   axis
  !! vg1, vg2, and vg3 are the components of a unit vector in model coordinates
  !!   in the direction of the connection between cell n and m
  !!iavgmeth is the averaging method.  If zero, then use harmonic averaging.
  !!   if one, then use arithmetic averaging.
  !<
  function hyeff(k11, k22, k33, ang1, ang2, ang3, vg1, vg2, vg3, &
                 iavgmeth) result(K)
    ! -- return
    real(DP) :: K
    ! -- dummy
    real(DP), intent(in) :: k11
    real(DP), intent(in) :: k22
    real(DP), intent(in) :: k33
    real(DP), intent(in) :: ang1
    real(DP), intent(in) :: ang2
    real(DP), intent(in) :: ang3
    real(DP), intent(in) :: vg1
    real(DP), intent(in) :: vg2
    real(DP), intent(in) :: vg3
    integer(I4B), intent(in) :: iavgmeth
    ! -- local
    real(DP) :: s1, s2, s3, c1, c2, c3
    real(DP), dimension(3, 3) :: r
    real(DP) :: ve1, ve2, ve3
    real(DP) :: denom, dnum, d1, d2, d3
    !
    ! -- Sin and cos of angles
    s1 = sin(ang1)
    c1 = cos(ang1)
    s2 = sin(ang2)
    c2 = cos(ang2)
    s3 = sin(ang3)
    c3 = cos(ang3)
    !
    ! -- Rotation matrix
    r(1, 1) = c1 * c2
    r(1, 2) = c1 * s2 * s3 - s1 * c3
    r(1, 3) = -c1 * s2 * c3 - s1 * s3
    r(2, 1) = s1 * c2
    r(2, 2) = s1 * s2 * s3 + c1 * c3
    r(2, 3) = -s1 * s2 * c3 + c1 * s3
    r(3, 1) = s2
    r(3, 2) = -c2 * s3
    r(3, 3) = c2 * c3
    !
    ! -- Unit vector in direction of n-m connection in a local coordinate
    !    system aligned with the ellipse axes
    ve1 = r(1, 1) * vg1 + r(2, 1) * vg2 + r(3, 1) * vg3
    ve2 = r(1, 2) * vg1 + r(2, 2) * vg2 + r(3, 2) * vg3
    ve3 = r(1, 3) * vg1 + r(2, 3) * vg2 + r(3, 3) * vg3
    !
    ! -- Effective hydraulic conductivity calculated using harmonic (1)
    !    or arithmetic (2) weighting
    K = DZERO
    if (iavgmeth == 0) then
      !
      ! -- Arithmetic weighting.  If principal direction corresponds exactly with
      !    unit vector then set to principal direction.  Otherwise weight it.
      dnum = DONE
      d1 = ve1**2
      d2 = ve2**2
      d3 = ve3**2
      if (ve1 /= DZERO) then
        dnum = dnum * k11
        d2 = d2 * k11
        d3 = d3 * k11
      end if
      if (ve2 /= DZERO) then
        dnum = dnum * k22
        d1 = d1 * k22
        d3 = d3 * k22
      end if
      if (ve3 /= DZERO) then
        dnum = dnum * k33
        d1 = d1 * k33
        d2 = d2 * k33
      end if
      denom = d1 + d2 + d3
      if (denom > DZERO) K = dnum / denom
    else if (iavgmeth == 1) then
      ! -- arithmetic
      K = ve1**2 * k11 + ve2**2 * k22 + ve3**2 * k33
    end if

  end function hyeff

end module HGeoUtilModule

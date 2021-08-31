!> @brief This module contains stateless sfr subroutines and functions
!!
!! This module contains the functions to calculate the wetted perimeter
!! and cross-sectional area for a reach cross-section that are used in
!! the streamflow routing (SFR) package. It also contains subroutines to 
!! calculate the wetted perimeter and cross-sectional area for each
!! line segment in the cross-section. This module does not depend on the 
!! SFR package.
!!
!<
module GwfSfrCrossSectionUtilsModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DHALF, DONE, DTWO

  implicit none
  private
  public :: get_saturated_topwidth
  public :: get_wetted_topwidth
  public :: get_wetted_perimeter
  public :: get_cross_section_area
  public :: SfrCrossSectionType

  type SfrCrossSectionType
    integer(I4B) :: npts                                        !< number of station elevation data for a reach
    real(DP), dimension(:), pointer, contiguous :: x => null()  !< cross-section station distances (x-distance)
    real(DP), dimension(:), pointer, contiguous :: z => null()  !< cross-section elevation data
  end type SfrCrossSectionType

contains

  !> @brief Calculate the saturated top width for a reach
  !!
  !! Function to calculate the saturated top width for a reach using the
  !! cross-section station data .
  !!
  !! @return      w               saturated top width
  !<
  function get_saturated_topwidth(npts, x) result(w)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts             !< number of station elevation data for a reach
    real(DP), dimension(npts), intent(in) :: x   !< cross-section station distances (x-distance)
    ! -- local variables
    real(DP) :: w
    !
    ! -- calculate the saturated top width
    w = x(npts) - x(1)
    !
    ! -- return
    return
  end function get_saturated_topwidth

  !> @brief Calculate the wetted top width for a reach
  !!
  !! Function to calculate the wetted top width for a reach using the
  !! cross-section station elevation data given a passed elevation.
  !!
  !! @return      w               wetted top width
  !<
  function get_wetted_topwidth(npts, x, z, d) result(w)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts             !< number of station elevation data for a reach
    real(DP), dimension(npts), intent(in) :: x   !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: z   !< cross-section elevation data
    real(DP), intent(in) :: d                    !< depth to evaluate cross-section
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: w
    real(DP), dimension(npts-1) :: widths
    !
    ! -- intitialize the wetted perimeter for the reach
    w = DZERO
    !
    ! -- calculate the wetted top width for each line segment
    call get_wetted_topwidths(npts, x, z, d, widths)
    !
    ! -- calculate the wetted top widths
    do n = 1, npts - 1
      w = w + widths(n)
    end do
    !
    ! -- return
    return
  end function get_wetted_topwidth
  
  !> @brief Calculate the wetted perimeter for a reach
  !!
  !! Function to calculate the wetted perimeter for a reach using the
  !! cross-section station elevation data given a passed elevation.
  !!
  !! @return      p               wetted perimeter
  !<
  function get_wetted_perimeter(npts, x, z, d) result(p)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts             !< number of station elevation data for a reach
    real(DP), dimension(npts), intent(in) :: x   !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: z   !< cross-section elevation data
    real(DP), intent(in) :: d                    !< depth to evaluate cross-section
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: p
    real(DP), dimension(npts-1) :: perimeters
    !
    ! -- intitialize the wetted perimeter for the reach
    p = DZERO
    !
    ! -- calculate the wetted perimeter for each line segment
    call get_wetted_perimeters(npts, x, z, d, perimeters)
    !
    ! -- calculate the wetted perimenter
    do n = 1, npts - 1
      p = p + perimeters(n)
    end do
    !
    ! -- return
    return
  end function get_wetted_perimeter

  !> @brief Calculate the cross-sectional area for a reach
  !!
  !! Function to calculate the cross-sectional area for a reach using 
  !! the cross-section station elevation data given a passed elevation.
  !!
  !! @return      a               cross-sectional area
  !<
  function get_cross_section_area(npts, x, z, d) result(a)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts             !< number of station elevation data for a reach
    real(DP), dimension(npts), intent(in) :: x   !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: z   !< cross-section elevation data
    real(DP), intent(in) :: d                    !< depth to evaluate cross-section
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: a
    real(DP), dimension(npts-1) :: areas
    !
    ! -- intitialize the area
    a = DZERO
    !
    ! -- calculate the cross-sectional area for each line segment
    call get_cross_section_areas(npts, x, z, d, areas)
    !
    ! -- calculate the cross-sectional area
    do n = 1, npts - 1
      a = a + areas(n)
    end do
    !
    ! -- return
    return
  end function get_cross_section_area

  ! -- private functions and subroutines

  !> @brief Calculate the wetted perimeters for each line segment
  !!
  !! Subroutine to calculate the wetted perimeter for each line segment
  !! that defines the reach using the cross-section station elevation 
  !! data given a passed elevation.
  !!
  !<
  subroutine get_wetted_perimeters(npts, x, z, d, p)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts             !< number of station elevation data for a reach
    real(DP), dimension(npts), intent(in) :: x   !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: z   !< cross-section elevation data
    real(DP), intent(in) :: d                    !< depth to evaluate cross-section
    real(DP), dimension(npts-1) :: p             !< wetted perimeter for each line segment
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: x0
    real(DP) :: x1
    real(DP) :: z0
    real(DP) :: z1
    real(DP) :: zmax
    real(DP) :: zmin
    real(DP) :: e
    real(DP) :: xlen
    real(DP) :: zlen
    !
    ! -- iterate over the station-elevation data
    do n = 1, npts - 1
      !
      ! -- initialize the wetted perimeter
      p(n) = DZERO
      !
      ! -- initialize station-elevation data for segment
      x0 = x(n)
      x1 = x(n+1)
      z0 = z(n)
      z1 = z(n+1)
      !
      ! -- get the start and end station position of the wetted segment
      call get_wetted_station(x0, x1, z0, z1, zmax, zmin, d)
      !
      ! -- calculate the elevation to evaluate
      e = zmin + d
      !
      ! -- calculate the wetted perimeter for the segment
      xlen = x1 - x0
      if (xlen > DZERO) then
        if (e > zmax) then
          zlen = zmax - zmin
        else
          zlen = e - zmin
        end if
        p(n) = sqrt(xlen**DTWO + zlen**DTWO)
      end if
    end do
    !
    ! -- return
    return
  end subroutine get_wetted_perimeters

  !> @brief Calculate the cross-sectional areas for each line segment
  !!
  !! Subroutine to calculate the cross-sectional area for each line segment
  !! that defines the reach using the cross-section station elevation 
  !! data given a passed elevation.
  !!
  !<
  subroutine get_cross_section_areas(npts, x, z, d, a)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts              !< number of station elevation data for a reach
    real(DP), dimension(npts), intent(in) :: x    !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: z    !< cross-section elevation data
    real(DP), intent(in) :: d                     !< depth to evaluate cross-section
    real(DP), dimension(npts-1) :: a              !< cross-sectional area for each line segment
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: x0
    real(DP) :: x1
    real(DP) :: z0
    real(DP) :: z1
    real(DP) :: zmax
    real(DP) :: zmin
    real(DP) :: e
    real(DP) :: xlen
    !
    ! -- iterate over the station-elevation data
    do n = 1, npts - 1
      !
      ! -- initialize the cross-sectional area
      a(n) = DZERO
      !
      ! -- initialize station-elevation data for segment
      x0 = x(n)
      x1 = x(n+1)
      z0 = z(n)
      z1 = z(n+1)
      !
      ! -- get the start and end station position of the wetted segment
      call get_wetted_station(x0, x1, z0, z1, zmax, zmin, d)
      !
      ! -- calculate the elevation to evaluate
      e = zmin + d
      !
      ! -- calculate the cross-sectional area for the segment
      xlen = x1 - x0
      if (xlen > DZERO) then
        !
        ! -- add the area above zmax
        if (e > zmax) then
          a(n) = xlen * (e - zmax)
        end if
        !
        ! -- add the area below zmax
        if (zmax /= zmin .and. e > zmin) then 
          a(n) = a(n) + DHALF * (e - zmin)
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine get_cross_section_areas

  !> @brief Calculate the wetted top widths for each line segment
  !!
  !! Subroutine to calculate the wetted top width for each line segment
  !! that defines the reach using the cross-section station elevation 
  !! data given a passed elevation.
  !!
  !<
  subroutine get_wetted_topwidths(npts, x, z, d, w)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts             !< number of station elevation data for a reach
    real(DP), dimension(npts), intent(in) :: x   !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: z   !< cross-section elevation data
    real(DP), intent(in) :: d                    !< depth to evaluate cross-section
    real(DP), dimension(npts-1) :: w             !< wetted top widths for each line segment
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: x0
    real(DP) :: x1
    real(DP) :: z0
    real(DP) :: z1
    real(DP) :: zmax
    real(DP) :: zmin
    !
    ! -- iterate over the station-elevation data
    do n = 1, npts - 1
      !
      ! -- initialize station-elevation data for segment
      x0 = x(n)
      x1 = x(n+1)
      z0 = z(n)
      z1 = z(n+1)
      !
      ! -- get the start and end station position of the wetted segment
      call get_wetted_station(x0, x1, z0, z1, zmax, zmin, d)
      !
      ! -- calculate the wetted top width for the segment
      w(n) = x1 - x0
    end do
    !
    ! -- return
    return
  end subroutine get_wetted_topwidths


  !> @brief Calculate the station values for the wetted portion of the cross-section 
  !!
  !! Subroutine to calculate the station values that define the extent of the 
  !! wetted portion of the cross section for a line segment. The left (x0) and 
  !! right (x1) station positions are altered if the passed elevation is less 
  !! than the maximum line segment elevation. If the line segment is dry the left 
  !! and right station are equal. Otherwise the wetted station values are equal 
  !! to the full line segment or smaller if the passed elevation is less than
  !! the maximum line segment elevation. 
  !!
  !<
  pure subroutine get_wetted_station(x0, x1, z0, z1, zmax, zmin, d)
    ! -- dummy variables
    real(DP), intent(inout) :: x0     !< left station position
    real(DP), intent(inout) :: x1     !< right station position
    real(DP), intent(in) :: z0        !< elevation at the left station
    real(DP), intent(in) :: z1        !< elevation at the right station
    real(DP), intent(inout) :: zmax   !< maximum elevation
    real(DP), intent(inout) :: zmin   !< minimum elevation
    real(DP), intent(in) :: d         !< depth to evaluate cross-section
    ! -- local variables
    real(DP) :: e
    real(DP) :: xlen
    real(DP) :: zlen
    real(DP) :: slope
    real(DP) :: dx
    real(DP) :: xt
    real(DP) :: xt0
    real(DP) :: xt1
    !
    ! -- calculate the minimum and maximum elevation
    zmin = min(z0, z1)
    zmax = max(z0, z1)
    !
    ! -- calculate the elevation to evaluate
    e = zmin + d
    !
    ! -- if e is less than or equal to the minimum value the 
    !    station length (xlen) is zero
    if (e <= zmin) then
      x1 = x0
    ! -- if e is between zmin and zmax station length is less
    !    than z1 - z0
    else if (e < zmax) then
      xlen = x1 - x0
      zlen = z1 - z0
      if (abs(zlen) > 0.) then
        slope = xlen / zlen
      else
        slope = DZERO
      end if
      if (z0 > z1) then
        dx = (e - z1) * slope 
        xt = x1 + dx
        xt0 = xt
        xt1 = x1
      else
        dx = (e - z0) * slope 
        xt = x0 + dx
        xt0 = x0
        xt1 = xt
      end if
      x0 = xt0
      x1 = xt1
    end if
    !
    ! -- return
    return
  end subroutine get_wetted_station


end module GwfSfrCrossSectionUtilsModule
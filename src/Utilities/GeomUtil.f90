module GeomUtilModule
  use KindModule, only: I4B, DP
  implicit none
  private
  public :: between, point_in_polygon
contains

  !> @brief Check if a value is between two other values (inclusive).
  logical function between(x, a, b)
    real(DP), intent(in) :: x, a, b
    between = (x >= a .and. x <= b .or. x <= a .and. x >= b)
  end function between

  !> @brief Check if a point is within a polygon.
  !! Vertices and edge points are considered in.
  !! Reference: https://stackoverflow.com/a/63436180/6514033
  logical function point_in_polygon(x, y, poly)
    real(DP), intent(in) :: x
    real(DP), intent(in) :: y
    real(DP), allocatable, intent(in) :: poly(:, :)
    integer(I4B) :: i, ii, num_verts
    real(DP) :: xa, xb, ya, yb, c = 0.0_DP

    point_in_polygon = .false.
    num_verts = size(poly, 1)
    xa = poly(1, 1)
    ya = poly(1, 2)

    do i = 0, num_verts + 1
      ii = mod(i, num_verts) + 1
      xb = poly(ii, 1)
      yb = poly(ii, 2)

      ! boundary cases
      if ((x == xa .and. y == ya) .or. (x == xb .and. y == yb)) then
        ! vertex point
        point_in_polygon = .true.
      else if (ya == yb .and. y == ya .and. between(x, xa, xb)) then
        ! horizontal edge
        point_in_polygon = .true.
        ! if within vertical range, cast a ray
      else if (between(y, ya, yb)) then
        if (y == ya .and. yb >= ya .or. y == yb .and. ya >= yb) then
          xa = xb
          ya = yb
          cycle
        end if
        ! cross product
        c = (xa - x) * (yb - y) - (xb - x) * (ya - y)
        ! boundary case
        if (c == 0) then
          point_in_polygon = .true.
          ! standard intersection
        else if ((ya < yb) .eqv. (c > 0)) then
          point_in_polygon = .not. point_in_polygon
        end if
      end if

      if (point_in_polygon) exit
      xa = xb
      ya = yb
    end do
  end function point_in_polygon

end module GeomUtilModule

module GeomUtilModule
  use KindModule, only: I4B, DP
  implicit none
  private
  public :: between, point_in_polygon, get_node, get_ijk, get_jk
contains

  !> @brief Check if a value is between two other values (inclusive).
  logical function between(x, a, b)
    real(DP), intent(in) :: x, a, b
    between = ((x >= a .and. x <= b) .or. (x <= a .and. x >= b))
  end function between

  !> @brief Check if a point is within a polygon.
  !! Vertices and edge points are considered in.
  !! Reference: https://stackoverflow.com/a/63436180/6514033
  logical function point_in_polygon(x, y, poly)
    ! dummy
    real(DP), intent(in) :: x !< x point coordinate
    real(DP), intent(in) :: y !< y point coordinate
    real(DP), allocatable, intent(in) :: poly(:, :) !< polygon vertices (column-major indexing)
    ! local
    integer(I4B) :: i, ii, num_verts
    real(DP) :: xa, xb, ya, yb, c = 0.0_DP

    point_in_polygon = .false.
    num_verts = size(poly, 2)
    xa = poly(1, num_verts)
    ya = poly(2, num_verts)

    do i = 0, num_verts - 1
      ii = mod(i, num_verts) + 1
      xb = poly(1, ii)
      yb = poly(2, ii)

      if ((x == xa .and. y == ya) .or. (x == xb .and. y == yb)) then
        ! on vertex
        point_in_polygon = .true.
        exit
      else if (ya == yb .and. y == ya .and. between(x, xa, xb)) then
        ! on horizontal edge
        point_in_polygon = .true.
        exit
      else if (between(y, ya, yb)) then
        if ((y == ya .and. yb >= ya) .or. (y == yb .and. ya >= yb)) then
          xa = xb
          ya = yb
          cycle
        end if
        ! cross product
        c = (xa - x) * (yb - y) - (xb - x) * (ya - y)
        if (c == 0) then
          ! on edge
          point_in_polygon = .true.
          exit
        else if ((ya < yb) .eqv. (c > 0)) then
          ! ray intersection
          point_in_polygon = .not. point_in_polygon
        end if
      end if

      xa = xb
      ya = yb
    end do
  end function point_in_polygon

  !> @brief Get node number, given layer, row, and column indices
  !! for a structured grid. If any argument is invalid return -1.
  function get_node(ilay, irow, icol, nlay, nrow, ncol)
    integer(I4B), intent(in) :: ilay, irow, icol, nlay, nrow, ncol
    integer(I4B) :: get_node

    if (nlay > 0 .and. nrow > 0 .and. ncol > 0 .and. &
        ilay > 0 .and. ilay <= nlay .and. &
        irow > 0 .and. irow <= nrow .and. &
        icol > 0 .and. icol <= ncol) then
      get_node = &
        icol + ncol * (irow - 1) + (ilay - 1) * nrow * ncol
    else
      get_node = -1
    end if
  end function get_node

  !> @brief Get row, column and layer indices from node number and grid
  !! dimensions. If nodenumber is invalid, irow, icol, and ilay are -1.
  subroutine get_ijk(nodenumber, nrow, ncol, nlay, irow, icol, ilay)
    ! -- dummy variables
    integer(I4B), intent(in) :: nodenumber
    integer(I4B), intent(in) :: nrow
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nlay
    integer(I4B), intent(out) :: irow
    integer(I4B), intent(out) :: icol
    integer(I4B), intent(out) :: ilay
    ! -- local variables
    integer(I4B) :: nodes
    integer(I4B) :: ij

    nodes = nlay * nrow * ncol
    if (nodenumber < 1 .or. nodenumber > nodes) then
      irow = -1
      icol = -1
      ilay = -1
    else
      ilay = (nodenumber - 1) / (ncol * nrow) + 1
      ij = nodenumber - (ilay - 1) * ncol * nrow
      irow = (ij - 1) / ncol + 1
      icol = ij - (irow - 1) * ncol
    end if
  end subroutine get_ijk

  !> @brief Get layer index and within-layer node index from node number
  !! and grid dimensions. If nodenumber is invalid, icpl and ilay are -1.
  subroutine get_jk(nodenumber, ncpl, nlay, icpl, ilay)
    ! -- dummy variables
    integer(I4B), intent(in) :: nodenumber
    integer(I4B), intent(in) :: ncpl
    integer(I4B), intent(in) :: nlay
    integer(I4B), intent(out) :: icpl
    integer(I4B), intent(out) :: ilay
    ! -- local variables
    integer(I4B) :: nodes

    nodes = ncpl * nlay
    if (nodenumber < 1 .or. nodenumber > nodes) then
      icpl = -1
      ilay = -1
    else
      ilay = (nodenumber - 1) / ncpl + 1
      icpl = nodenumber - (ilay - 1) * ncpl
    end if
  end subroutine get_jk

end module GeomUtilModule

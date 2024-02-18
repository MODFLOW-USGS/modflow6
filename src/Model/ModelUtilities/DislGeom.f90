module DislGeom

  use KindModule, only: DP, I4B
  implicit none
  private
  public :: calcdist, line_unit_vector

  contains

  !> @brief Calculate distance bewteen two vertices
  !<
  function calcdist(vertices, vert1, vert2) result(dist)
    ! -- dummy
    real(DP), dimension(:, :), intent(in) :: vertices
    integer(I4B), intent(in) :: vert1
    integer(I4B), intent(in) :: vert2
    real(DP) :: dist, xdist, ydist, zdist
    ! -- local
    !
    ! -- calc distance
    xdist = abs(vertices(1, vert1) - vertices(1, vert2))
    ydist = abs(vertices(2, vert1) - vertices(2, vert2))
    zdist = abs(vertices(3, vert1) - vertices(3, vert2))
    dist = sqrt(xdist * xdist + ydist * ydist + zdist * zdist)

    ! -- return
    return
  end function calcdist


  !> @brief Calculate distance bewteen two vertices
  !!
  !! Calculate the vector components (xcomp, ycomp, and zcomp)
  !! for a line defined by two points, (x0, y0, z0), (x1, y1, z1). Also 
  !! return the magnitude of the original vector, vmag.
  !!
  !<
  subroutine line_unit_vector(x0, y0, z0, x1, y1, z1, &
                              xcomp, ycomp, zcomp, vmag)
    real(DP), intent(in) :: x0
    real(DP), intent(in) :: y0
    real(DP), intent(in) :: z0
    real(DP), intent(in) :: x1
    real(DP), intent(in) :: y1
    real(DP), intent(in) :: z1
    real(DP), intent(out) :: xcomp
    real(DP), intent(out) :: ycomp
    real(DP), intent(out) :: zcomp
    real(DP) :: dx, dy, dz, vmag
    !
    dx = x1 - x0
    dy = y1 - y0
    dz = z1 - z0
    vmag = sqrt(dx**2 + dy**2 + dz**2)
    xcomp = dx / vmag
    ycomp = dy / vmag
    zcomp = dz / vmag
    return
  end subroutine line_unit_vector

end module DislGeom

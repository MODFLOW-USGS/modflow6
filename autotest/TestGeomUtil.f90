module TestGeomUtil
  use KindModule, only: I4B, DP
  use testdrive, only: error_type, unittest_type, new_unittest, check, test_failed
  use GeomUtilModule, only: point_in_polygon
  use ConstantsModule, only: LINELENGTH
  implicit none
  private
  public :: collect_geomutil

contains

  subroutine collect_geomutil(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("point_in_polygon_rect", &
                             test_point_in_polygon_rect) &
                ]
  end subroutine collect_geomutil

  subroutine test_point_in_polygon_rect(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP), allocatable :: poly(:, :)

    ! allocate and define polygon
    allocate (poly(4, 2))
    ! vertices in clockwise order
    poly(1, :) = (/0.0_DP, 0.0_DP/)
    poly(2, :) = (/0.0_DP, 1.0_DP/)
    poly(3, :) = (/1.0_DP, 1.0_DP/)
    poly(4, :) = (/1.0_DP, 0.0_DP/)

    ! points inside polygon
    call check(error, point_in_polygon(0.99_DP, 0.01_DP, poly))
    call check(error, point_in_polygon(0.5_DP, 0.5_DP, poly))
    call check(error, point_in_polygon(0.0001_DP, 0.9999_DP, poly))
    if (allocated(error)) return

    ! points outside polygon
    call check(error, (.not. point_in_polygon(0.5_DP, 1.00001_DP, poly)))
    call check(error, (.not. point_in_polygon(-0.5_DP, 34.0_DP, poly)))
    if (allocated(error)) return

    ! points on vertices
    call check(error, point_in_polygon(0.0_DP, 0.0_DP, poly))
    call check(error, point_in_polygon(1.0_DP, 0.0_DP, poly))
    call check(error, point_in_polygon(0.0_DP, 1.0_DP, poly))
    call check(error, point_in_polygon(1.0_DP, 1.0_DP, poly))
    if (allocated(error)) return

    ! points on faces
    call check(error, point_in_polygon(0.0_DP, 0.5_DP, poly))
    call check(error, point_in_polygon(0.5_DP, 0.0_DP, poly))
    call check(error, point_in_polygon(1.0_DP, 0.5_DP, poly))
    call check(error, point_in_polygon(0.5_DP, 1.0_DP, poly))
    if (allocated(error)) return

    ! vertices counter-clockwise
    poly(1, :) = (/0.0_DP, 0.0_DP/)
    poly(2, :) = (/1.0_DP, 0.0_DP/)
    poly(3, :) = (/1.0_DP, 1.0_DP/)
    poly(4, :) = (/0.0_DP, 1.0_DP/)

    ! points inside polygon
    call check(error, point_in_polygon(0.99_DP, 0.01_DP, poly))
    call check(error, point_in_polygon(0.5_DP, 0.5_DP, poly))
    call check(error, point_in_polygon(0.0001_DP, 0.9999_DP, poly))
    if (allocated(error)) return

    ! points outside polygon
    call check(error, (.not. point_in_polygon(0.5_DP, 1.00001_DP, poly)))
    call check(error, (.not. point_in_polygon(-0.5_DP, 34.0_DP, poly)))
    if (allocated(error)) return

    ! points on vertices
    call check(error, point_in_polygon(0.0_DP, 0.0_DP, poly))
    call check(error, point_in_polygon(1.0_DP, 0.0_DP, poly))
    call check(error, point_in_polygon(0.0_DP, 1.0_DP, poly))
    call check(error, point_in_polygon(1.0_DP, 1.0_DP, poly))
    if (allocated(error)) return

    ! points on faces
    call check(error, point_in_polygon(0.0_DP, 0.5_DP, poly))
    call check(error, point_in_polygon(0.5_DP, 0.0_DP, poly))
    call check(error, point_in_polygon(1.0_DP, 0.5_DP, poly))
    call check(error, point_in_polygon(0.5_DP, 1.0_DP, poly))
    if (allocated(error)) return

  end subroutine test_point_in_polygon_rect

end module TestGeomUtil

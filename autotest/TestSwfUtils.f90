module TestSwfUtils
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO, DHALF, DONE, DTWO, DNODATA, DTWOTHIRDS
  use testdrive, only : error_type, unittest_type, new_unittest, check, test_failed, to_string
  use SwfCxsUtilsModule, only: get_cross_section_area, &
                               get_wetted_perimeter, &
                               get_conveyance
  implicit none
  private
  public :: collect_swfutils
contains
  
  subroutine collect_swfutils(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("two_point_section", test_two_point_section), &
      new_unittest("three_point_section", test_three_point_section), &
      new_unittest("test_four_point_rectangular_section", test_four_point_rectangular_section) &
      ]
  end subroutine collect_swfutils

  subroutine test_two_point_section(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), parameter :: NPTS=2
    real(DP), dimension(NPTS) :: xfraction
    real(DP), dimension(NPTS) :: height
    real(DP), dimension(NPTS) :: cxs_rf
    real(DP) :: res
    real(DP) :: area
    real(DP) :: perimeter
    real(DP) :: conveyance
    real(DP) :: width
    real(DP) :: depth
    real(DP) :: rough

    xfraction = (/DZERO, DONE/)
    height = (/DZERO, DZERO/)
    cxs_rf = (/DONE, DNODATA/)
    width = 10.d0
    depth = 10.d0
    rough = 0.035d0

    ! confirm wetted perimeter calculation
    res = get_wetted_perimeter(NPTS, &
                               xfraction, &
                               height, &
                               width, depth)
    perimeter = res
    call check(error, perimeter == width, "Wetted perimeter correct")
    if (allocated(error)) then
      call test_failed(error, "Wetted perimeter incorrect")
      return
    end if

    ! confirm cross section area calculation
    res = get_cross_section_area(NPTS, &
                                 xfraction, &
                                 height, &
                                 width, depth)
    area = res
    call check(error, area == width * depth, "Cross section area correct")
    if (allocated(error)) then
      call test_failed(error, "Cross section area incorrect")
      return
    end if

    ! confirm composite conveyance calculation = A*R^(2/3)/n
    res = get_conveyance(NPTS, &
                         xfraction, &
                         height, cxs_rf, &
                         width, rough, depth)
    conveyance = res
    call check(error, &
               conveyance == area * (area / perimeter) ** (DTWOTHIRDS) / rough, &
               "Conveyance correct")
    if (allocated(error)) then
      call test_failed(error, "Conveyance incorrect")
      return
    end if
  end subroutine test_two_point_section

  subroutine test_three_point_section(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), parameter :: NPTS=3
    real(DP), dimension(NPTS) :: xfraction
    real(DP), dimension(NPTS) :: height
    real(DP), dimension(NPTS) :: cxs_rf
    real(DP) :: res
    real(DP) :: area
    real(DP) :: perimeter
    real(DP) :: conveyance
    real(DP) :: width
    real(DP) :: depth
    real(DP) :: rough
    real(DP) :: a
    real(DP) :: p
    real(DP) :: c

    xfraction = (/DZERO, DHALF, DONE/)
    height = (/DONE, DZERO, DONE/)
    cxs_rf = (/DONE, DONE, DNODATA/)
    width = 10.d0
    depth = 1.D0
    rough = 0.035d0

    ! confirm wetted perimeter calculation
    res = get_wetted_perimeter(NPTS, &
                               xfraction, &
                               height, &
                               width, depth)
    perimeter = res
    call check(error, &
               perimeter == DTWO * sqrt((width / DTWO) ** DTWO + depth ** DTWO), &
               "Wetted perimeter correct")
    if (allocated(error)) then
      call test_failed(error, "Wetted perimeter incorrect")
      return
    end if

    ! confirm cross section area calculation
    res = get_cross_section_area(NPTS, &
                                 xfraction, &
                                 height, &
                                 width, depth)
    area = res
    call check(error, area == DHALF * width * depth, "Cross section area correct")
    if (allocated(error)) then
      call test_failed(error, "Cross section area incorrect")
      return
    end if

    ! confirm conveyance calculation = A*R^(2/3)/n
    res = get_conveyance(NPTS, &
                         xfraction, &
                         height, cxs_rf, &
                         width, rough, depth)
    conveyance = res
    a = area / DTWO
    p = perimeter / DTWO
    c = DTWO * (a * ((a / p) ** DTWOTHIRDS) / rough)
    call check(error, &
               abs(conveyance - c) < 1.d-8, &
               "Swf util conveyance " // to_string(conveyance) // &
               " /= expected conveyance " // to_string(c))
    if (allocated(error)) then
      call test_failed(error, "Conveyance calculation failed")
      return
    end if
  end subroutine test_three_point_section

  subroutine test_four_point_rectangular_section(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), parameter :: NPTS=4
    real(DP), dimension(NPTS) :: xfraction
    real(DP), dimension(NPTS) :: height
    real(DP), dimension(NPTS) :: cxs_rf
    real(DP) :: res
    real(DP) :: area
    real(DP) :: perimeter
    real(DP) :: conveyance
    real(DP) :: width
    real(DP) :: depth
    real(DP) :: rough
    real(DP) :: a
    real(DP) :: p
    real(DP) :: c

    depth = DHALF

    xfraction = [DZERO, DZERO, DONE, DONE]
    height = [DONE, DZERO, DZERO, DONE]
    cxs_rf = [DONE, DONE, DONE, DNODATA]
    width = 10.d0
    rough = 0.035d0

    ! confirm wetted perimeter calculation
    res = get_wetted_perimeter(NPTS, &
                               xfraction, &
                               height, &
                               width, depth)
    perimeter = res
    p = DTWO * depth + width
    call check(error, &
               perimeter == p, &
               "Wetted perimeter " // to_string(perimeter) // &
               " /= expected value " // to_string(p))
    if (allocated(error)) then
      call test_failed(error, "Wetted perimeter fail")
      return
    end if

    ! confirm cross section area calculation
    res = get_cross_section_area(NPTS, &
                                 xfraction, &
                                 height, &
                                 width, depth)
    area = res
    a = depth * width
    call check(error, &
               area == a, &
               "Area " // to_string(area) // &
               " /= expected value " // to_string(a))
    if (allocated(error)) then
      call test_failed(error, "Cross section area incorrect")
      return
    end if

    ! confirm composite conveyance calculation = A/n*R^(2/3)
    res = get_conveyance(NPTS, &
                         xfraction, &
                         height, cxs_rf, &
                         width, rough, depth)
    conveyance = res
    a = area
    p = perimeter
    c = a / rough * (a / p) ** DTWOTHIRDS
    call check(error, &
               abs(conveyance - c) < 1.d-8, &
               "Conveyance " // to_string(conveyance) // &
               " /= expected value " // to_string(c))
    if (allocated(error)) then
      call test_failed(error, "Conveyance calculation failed")
      return
    end if

    ! test calculations when depth is above the highest
    ! cross section height.  In this case, the wetted perimeter
    ! does not increase when the water surface rises above the
    ! highest height point.
    depth = 1.5d0

        ! confirm wetted perimeter calculation
    res = get_wetted_perimeter(NPTS, &
                               xfraction, &
                               height, &
                               width, depth)
    perimeter = res
    p = DTWO * height(1) + width
    call check(error, &
               perimeter == p, &
               "Wetted perimeter " // to_string(perimeter) // &
               " /= expected value " // to_string(p))
    if (allocated(error)) then
      call test_failed(error, "Wetted perimeter fail")
      return
    end if

    ! confirm cross section area calculation
    res = get_cross_section_area(NPTS, &
                                 xfraction, &
                                 height, &
                                 width, depth)
    area = res
    a = depth * width
    call check(error, &
               area == a, &
               "Area " // to_string(area) // &
               " /= expected value " // to_string(a))
    if (allocated(error)) then
      call test_failed(error, "Cross section area incorrect")
      return
    end if

    ! confirm composite conveyance calculation = A/n*R^(2/3)
    res = get_conveyance(NPTS, &
                         xfraction, &
                         height, cxs_rf, &
                         width, rough, depth)
    conveyance = res
    a = area
    p = perimeter
    c = a / rough * (a / p) ** DTWOTHIRDS
    call check(error, &
               abs(conveyance - c) < 1.d-8, &
               "Conveyance " // to_string(conveyance) // &
               " /= expected value " // to_string(c))
    if (allocated(error)) then
      call test_failed(error, "Conveyance calculation failed")
      return
    end if
  end subroutine test_four_point_rectangular_section

end module TestSwfUtils
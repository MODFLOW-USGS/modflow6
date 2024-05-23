module TestSwfUtils
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO, DHALF, DONE, DTWO, DNODATA, DTWOTHIRDS
  use testdrive, only: error_type, unittest_type, new_unittest, check, &
                       test_failed, to_string
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
                new_unittest("test_four_point_rectangular_section", &
                             test_four_point_rectangular_section), &
                new_unittest("test_n_point_rectangular_section", &
                             test_n_point_rectangular_section) &
                ]
  end subroutine collect_swfutils

  subroutine test_two_point_section(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), parameter :: NPTS = 2
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
               conveyance == area * (area / perimeter)**(DTWOTHIRDS) / rough, &
               "Conveyance correct")
    if (allocated(error)) then
      call test_failed(error, "Conveyance incorrect")
      return
    end if
  end subroutine test_two_point_section

  subroutine test_three_point_section(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), parameter :: NPTS = 3
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
               perimeter == DTWO * sqrt((width / DTWO)**DTWO + depth**DTWO), &
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
    c = DTWO * (a * ((a / p)**DTWOTHIRDS) / rough)
    call check(error, &
               abs(conveyance - c) < 1.d-8, &
               "Swf util conveyance "//to_string(conveyance)// &
               " /= expected conveyance "//to_string(c))
    if (allocated(error)) then
      call test_failed(error, "Conveyance calculation failed")
      return
    end if
  end subroutine test_three_point_section

  subroutine test_four_point_rectangular_section(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), parameter :: NPTS = 4
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
               "Wetted perimeter "//to_string(perimeter)// &
               " /= expected value "//to_string(p))
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
               "Area "//to_string(area)// &
               " /= expected value "//to_string(a))
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
    c = a / rough * (a / p)**DTWOTHIRDS
    call check(error, &
               abs(conveyance - c) < 1.d-8, &
               "Conveyance "//to_string(conveyance)// &
               " /= expected value "//to_string(c))
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
               "Wetted perimeter "//to_string(perimeter)// &
               " /= expected value "//to_string(p))
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
               "Area "//to_string(area)// &
               " /= expected value "//to_string(a))
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
    c = a / rough * (a / p)**DTWOTHIRDS
    call check(error, &
               abs(conveyance - c) < 1.d-8, &
               "Conveyance "//to_string(conveyance)// &
               " /= expected value "//to_string(c))
    if (allocated(error)) then
      call test_failed(error, "Conveyance calculation failed")
      return
    end if
  end subroutine test_four_point_rectangular_section

  !> @brief Test n-point cross section calculations
  !!
  !! Use the 8-point cross section data from the
  !< Punxsutawney example of the HEC-HMS tutorial.
  subroutine test_n_point_rectangular_section(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), parameter :: NPTS = 8
    integer(I4B), parameter :: NDEPTHS = 12
    real(DP), dimension(NPTS) :: xfraction
    real(DP), dimension(NPTS) :: height
    real(DP), dimension(NPTS) :: cxs_rf
    real(DP), dimension(NDEPTHS) :: depths
    real(DP), dimension(NDEPTHS) :: area_expected
    real(DP), dimension(NDEPTHS) :: wp_expected
    real(DP), dimension(NDEPTHS) :: conveyance_expected
    integer(I4B) :: idepth
    real(DP) :: atol = 1.d-9
    real(DP) :: difference
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

    ! expected values
    area_expected = [ &
                    0.0000000000000000d0, &
                    14.756129265604915d0, &
                    37.883172712533252d0, &
                    63.615202573234768d0, &
                    91.952218847709474d0, &
                    139.22191462507655d0, &
                    231.01552353826017d0, &
                    325.38720871509929d0, &
                    422.33697015559397d0, &
                    521.86480785974402d0, &
                    623.97072182754971d0, &
                    728.53880232770473d0 &
                    ]
    wp_expected = [ &
                  0.0000000000000000d0, &
                  22.416013040258729d0, &
                  25.759029889993588d0, &
                  29.102046739728443d0, &
                  32.445063589463295d0, &
                  93.878846225107850d0, &
                  97.159078583084536d0, &
                  100.43931094106127d0, &
                  103.71954329903795d0, &
                  106.99977565701464d0, &
                  110.28000801499132d0, &
                  112.57661057753654d0 &
                  ]
    conveyance_expected = [ &
                          0.0000000000000000d0, &
                          285.75028108202571d0, &
                          1289.7912773134653d0, &
                          2876.8119843270820d0, &
                          5016.5410818181253d0, &
                          7835.1910481243458d0, &
                          12094.062268307061d0, &
                          17566.086627356817d0, &
                          24108.098792137902d0, &
                          31647.248549995347d0, &
                          40136.606753502856d0, &
                          49571.805574496953d0 &
                          ]

    xfraction = [0.d0, 6.10003658d0, 37.39941478d0, 41.09973177d0, &
                 61.39965862d0, 68.6021702d0, 96.299683d0, 105.19995123d0]
    height = [10.70013411d0, 4.70007315d0, 4.60009754d0, 0.d0, 0.6000061d0, &
              4.60009754d0, 5.d0, 10.70013411d0]
    cxs_rf = [0.09d0, 0.09d0, 0.04d0, 0.04d0, 0.04d0, 0.09d0, 0.09d0, 0.d0]
    depths = [0.d0, 1.d0, 2.d0, 3.d0, 4.d0, 5.d0, &
              6.d0, 7.d0, 8.d0, 9.d0, 10.d0, 11.d0]

    width = DONE
    rough = DONE

    do idepth = 1, NDEPTHS

      depth = depths(idepth)

      ! confirm cross section area calculation
      res = get_cross_section_area(NPTS, &
                                   xfraction, &
                                   height, &
                                   width, depth)
      area = res
      a = area_expected(idepth)
      difference = abs(area - a)
      call check(error, &
                 difference < atol, &
                 "Area "//to_string(area)// &
                 " /= expected value "//to_string(a)// &
                 " Difference is "//to_string(difference))
      if (allocated(error)) then
        call test_failed(error, "Cross section area incorrect")
        return
      end if

      ! confirm wetted perimeter calculation
      res = get_wetted_perimeter(NPTS, &
                                 xfraction, &
                                 height, &
                                 width, depth)
      perimeter = res
      p = wp_expected(idepth)
      difference = abs(perimeter - p)
      call check(error, &
                 difference < atol, &
                 "Wetted perimeter "//to_string(perimeter)// &
                 " /= expected value "//to_string(p)// &
                 " Difference is "//to_string(difference))
      if (allocated(error)) then
        call test_failed(error, "Wetted perimeter fail")
        return
      end if

      ! confirm composite conveyance calculation = A/n*R^(2/3)
      res = get_conveyance(NPTS, &
                           xfraction, &
                           height, cxs_rf, &
                           width, rough, depth)
      conveyance = res
      c = conveyance_expected(idepth)
      difference = abs(conveyance - c)
      call check(error, &
                 difference < atol, &
                 "Conveyance "//to_string(conveyance)// &
                 " /= expected value "//to_string(c)// &
                 " Difference is "//to_string(difference))
      if (allocated(error)) then
        call test_failed(error, "Conveyance calculation failed")
        return
      end if

    end do

  end subroutine test_n_point_rectangular_section

end module TestSwfUtils

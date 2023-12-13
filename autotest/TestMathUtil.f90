module TestMathUtil
  use KindModule, only: I4B, DP
  use testdrive, only: check, error_type, new_unittest, test_failed, &
                       to_string, unittest_type
  use MathUtilModule, only: is_close, mod_offset
  implicit none
  private
  public :: collect_mathutil

contains

  subroutine collect_mathutil(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("is_close_symmetric", test_is_close_symmetric), &
                new_unittest("is_close_symmetric_near_0", &
                             test_is_close_symmetric_near_0), &
                new_unittest("mod_offset", &
                             test_mod_offset) &
                ]
  end subroutine collect_mathutil

  subroutine test_mod_offset(error)
    type(error_type), allocatable, intent(out) :: error

    ! with no offset specified, should behave just like mod
    call check(error, mod_offset(2, 2) == 0)
    call check(error, mod_offset(2, 3) == 2)
    call check(error, mod_offset(2.0_DP, 2.0_DP) == 0.0_DP)
    call check(error, mod_offset(2.0_DP, 3.0_DP) == 2.0_DP)

    ! with offset d specified, if the result x = a mod n falls
    ! between 0 and n - 1, the new result x = a mod_d n falls
    ! between d and d + n - 1.
    call check(error, mod_offset(2, 3, -2) == -1)
    call check(error, mod_offset(2, 3, -1) == -1)
    call check(error, mod_offset(2, 3, 0) == 2)
    call check(error, mod_offset(2, 3, 1) == 2)
    call check(error, mod_offset(2, 3, 2) == 2)
    call check(error, mod_offset(2, 3, 3) == 5)
    call check(error, mod_offset(2, 3, 4) == 5)
    call check(error, mod_offset(2.0_DP, 3.0_DP, -1.0_DP) == -1.0_DP)
    call check(error, mod_offset(2.0_DP, 3.0_DP, 2.0_DP) == 2.0_DP)
    call check(error, mod_offset(2.0_DP, 3.0_DP, 3.0_DP) == 5.0_DP)
  end subroutine test_mod_offset

  subroutine test_is_close_symmetric(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP) :: a, b, rtol

    ! exact match
    a = 1.0_DP
    b = 1.0_DP
    call check(error, is_close(a, b), &
               "exp eq: a="//to_string(a)// &
               ", b="//to_string(b)// &
               ", eps=default")
    if (allocated(error)) return

    ! mismatch with default rtol
    b = 1.0001_DP
    call check(error, (.not. (is_close(a, b))), &
               "exp ne: a="//to_string(a)// &
               ", b="//to_string(b)// &
               ", eps=default")
    if (allocated(error)) return

    ! inexact match with large rtol
    rtol = 1d-2
    call check(error, is_close(a, b, rtol=rtol), &
               "exp eq: a="//to_string(a)// &
               ", b="//to_string(b)// &
               ", rtol="//to_string(rtol))
    if (allocated(error)) return

    ! mismatch when we reduce rtol
    rtol = 0.5d-5
    call check(error, (.not. is_close(a, b, rtol=rtol)), &
               "exp ne: a="//to_string(a)// &
               ", b="//to_string(b)// &
               ", rtol="//to_string(rtol))
    if (allocated(error)) return

    ! +/-0
    call check(error, is_close(0.0_DP, -0.0_DP), &
               "exp ne: a="//to_string(a)// &
               ", b="//to_string(b)// &
               ", eps=default")

  end subroutine test_is_close_symmetric

  subroutine test_is_close_symmetric_near_0(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP) :: a, b, rtol, atol

    a = 0.0_DP
    b = 0.0_DP
    call check(error, is_close(a, b), &
               "exp eq: a="//to_string(a)// &
               ", b="//to_string(b)// &
               ", rtol=default")
    if (allocated(error)) return

    b = 1d-4
    call check(error, (.not. is_close(a, b)), &
               "exp eq: a="//to_string(a)// &
               ", b="//to_string(b)// &
               ", rtol=default")
    if (allocated(error)) return

    rtol = 0.999_DP
    call check(error, &
               ! expect failure, see above
               (.not. is_close(a, b, rtol=rtol)), &
               "exp eq: a="//to_string(a)// &
               ", b="//to_string(b)// &
               ", rtol="//to_string(rtol))
    if (allocated(error)) return

    ! absolute comparison is appropriate when a and/or b are near or equal to 0
    b = 1d-4
    atol = 1d-3
    call check(error, is_close(a, b, atol=atol), &
               "exp eq: a="//to_string(a)// &
               ", b="//to_string(b)// &
               ", atol="//to_string(atol))
    if (allocated(error)) return

    ! make sure the absolute tolerance is applied
    b = 1d-4
    atol = 1d-5
    call check(error, (.not. is_close(a, b, atol=atol)), &
               "exp eq: a="//to_string(a)// &
               ", b="//to_string(b)// &
               ", atol="//to_string(atol))
    if (allocated(error)) return

  end subroutine test_is_close_symmetric_near_0

end module TestMathUtil

module TestMathUtil
  use KindModule, only: I4B, DP
  use testdrive, only: check, error_type, new_unittest, test_failed, &
                       to_string, unittest_type
  use MathUtilModule, only: mod_offset
  implicit none
  private
  public :: collect_mathutil

contains

  subroutine collect_mathutil(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
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

end module TestMathUtil

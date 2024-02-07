module TestTimeSelect
  use KindModule, only: I4B, DP, LGP
  use testdrive, only: check, error_type, new_unittest, test_failed, &
                       to_string, unittest_type
  use TimeSelectModule, only: TimeSelectType
  use ConstantsModule, only: LINELENGTH
  implicit none
  private
  public :: collect_timeselect

contains
  subroutine collect_timeselect(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("is_increasing", test_is_increasing), &
                new_unittest("slice", test_slice) &
                ]
  end subroutine collect_timeselect

  subroutine test_is_increasing(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeSelectType) :: ts

    call ts%expand(3)

    ! increasing
    ts%times = (/0.0_DP, 1.0_DP, 2.0_DP/)
    call check(error, ts%is_increasing())

    ! not decreasing
    ts%times = (/0.0_DP, 0.0_DP, 2.0_DP/)
    call check(error,.not. ts%is_increasing())

    ! decreasing
    ts%times = (/2.0_DP, 1.0_DP, 0.0_DP/)
    call check(error,.not. ts%is_increasing())
  end subroutine

  subroutine test_slice(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeSelectType) :: ts
    logical(LGP) :: changed

    call ts%expand(3)
    ts%times = (/0.0_DP, 1.0_DP, 2.0_DP/)
    call check( &
      error, &
      size(ts%times) == 3, &
      "expected size 3, got"//to_string(size(ts%times)))

    ! empty slice
    call ts%set_slice(1.1_DP, 1.9_DP)
    call check( &
      error, &
      ts%slice(1) == -1 .and. ts%slice(2) == -1, &
      "empty slice failed, got ["// &
      to_string(ts%slice(1))//","//to_string(ts%slice(2))//"]")

    ! single-item slice
    call ts%set_slice(0.5_DP, 1.5_DP)
    call check( &
      error, &
      ts%slice(1) == 2 .and. ts%slice(2) == 2, &
      "1-item slice failed, got ["// &
      to_string(ts%slice(1))//","//to_string(ts%slice(2))//"]")

    ! multi-item slice
    changed = .false.
    call ts%set_slice(0.5_DP, 2.5_DP, changed=changed)
    call check(error, changed)
    call check( &
      error, &
      ts%slice(1) == 2 .and. ts%slice(2) == 3, &
      "2-item slice failed, got ["// &
      to_string(ts%slice(1))//","//to_string(ts%slice(2))//"]")

    ! no-change
    call ts%set_slice(0.1_DP, 2.5_DP, changed=changed)
    call check(error,.not. changed)
    call check( &
      error, &
      ts%slice(1) == 2 .and. ts%slice(2) == 3, &
      "2-item slice failed, got ["// &
      to_string(ts%slice(1))//","//to_string(ts%slice(2))//"]")

    ! lower bound equal to a time value
    call ts%set_slice(0.0_DP, 2.5_DP)
    call check( &
      error, &
      ts%slice(1) == 1 .and. ts%slice(2) == 3, &
      "lb eq slice failed, got [" &
      //to_string(ts%slice(1))//","//to_string(ts%slice(2))//"]")

    ! upper bound equal to a time value
    call ts%set_slice(-0.5_DP, 2.0_DP)
    call check( &
      error, &
      ts%slice(1) == 1 .and. ts%slice(2) == 3, &
      "ub eq slice failed, got [" &
      //to_string(ts%slice(1))//","//to_string(ts%slice(2))//"]")

    ! both bounds equal to a time value
    call ts%set_slice(0.0_DP, 2.0_DP)
    call check( &
      error, &
      ts%slice(1) == 1 .and. ts%slice(2) == 3, &
      "lb ub eq slice failed, got [" &
      //to_string(ts%slice(1))//","//to_string(ts%slice(2))//"]")

  end subroutine test_slice
end module TestTimeSelect

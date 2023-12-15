module TestArrayHandlers
  use KindModule, only: I4B, DP, LGP
  use testdrive, only: error_type, unittest_type, new_unittest, check, &
                       test_failed, to_string
  use ArrayHandlersModule, only: ExpandArray, ExpandArray2D
  use ConstantsModule, only: LINELENGTH
  implicit none
  private
  public :: collect_arrayhandlers

contains

  subroutine collect_arrayhandlers(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("ExpandArray_int", &
                             test_ExpandArray_int), &
                new_unittest("ExpandArray_dbl", &
                             test_ExpandArray_dbl), &
                new_unittest("ExpandArray_logical", &
                             test_ExpandArray_logical), &
                new_unittest("ExpandArray_int_offset", &
                             test_ExpandArray_int_offset), &
                new_unittest("ExpandArray_dbl_offset", &
                             test_ExpandArray_dbl_offset), &
                new_unittest("ExpandArray_logical_offset", &
                             test_ExpandArray_logical_offset), &
                new_unittest("ExpandArray2D_int", test_ExpandArray2D_int), &
                new_unittest("ExpandArray2D_dbl", test_ExpandArray2D_dbl), &
                new_unittest("ExpandArray2D_int_offset", &
                             test_ExpandArray2D_int_offset), &
                new_unittest("ExpandArray2D_dbl_offset", &
                             test_ExpandArray2D_dbl_offset) &
                ]
  end subroutine collect_arrayhandlers

  !> @brief Test 1D int array expansion with default lower bound of 1
  subroutine test_ExpandArray_int(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), allocatable :: array(:)

    ! allocate array
    allocate (array(2))
    array(1) = 0
    array(2) = 1

    ! resize array and check new size and bounds
    call ExpandArray(array, 3)
    call check(error, size(array, 1) == 5, "1d int array resize failed")
    call check(error, lbound(array, 1) == 1, &
               "unexpected lower bound:"//to_string(lbound(array, 1)))
    call check(error, ubound(array, 1) == 5, &
               "unexpected upper bound:"//to_string(ubound(array, 1)))
    if (allocated(error)) return

    ! set new array elements and check new/old contents
    array(3) = 2
    array(4) = 3
    array(5) = 4
    call check(error, &
               array(1) == 0 .and. &
               array(2) == 1 .and. &
               array(3) == 2 .and. &
               array(4) == 3 .and. &
               array(5) == 4, &
               "1d int array repopulation failed")
    if (allocated(error)) return
    deallocate (array)
  end subroutine test_ExpandArray_int

  !> @brief Test 1D dbl array expansion with default lower bound of 1
  subroutine test_ExpandArray_dbl(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP), allocatable :: array(:)

    ! allocate array
    allocate (array(2))
    array(1) = 0.5_DP
    array(2) = 0.7_DP

    ! resize array and check new size and bounds
    call ExpandArray(array, 1)
    call check(error, size(array, 1) == 3, "1d dbl array resize failed")
    call check(error, lbound(array, 1) == 1, &
               "unexpected lower bound:"//to_string(lbound(array, 1)))
    call check(error, ubound(array, 1) == 3, &
               "unexpected upper bound:"//to_string(ubound(array, 1)))
    if (allocated(error)) return

    ! set new array element and check new/old contents
    array(3) = 0.1_DP
    call check(error, &
               array(1) == 0.5_DP .and. &
               array(2) == 0.7_DP .and. &
               array(3) == 0.1_DP, &
               "1d dbl array repopulation failed")
    if (allocated(error)) return
    deallocate (array)
  end subroutine test_ExpandArray_dbl

  !> @brief Test 1D logical array expansion with default lower bound of 1
  subroutine test_ExpandArray_logical(error)
    type(error_type), allocatable, intent(out) :: error
    logical(LGP), allocatable :: array(:)

    ! allocate array
    allocate (array(2))
    array(1) = .true.
    array(2) = .false.

    ! resize array and check new size and bounds
    call ExpandArray(array, 1)
    call check(error, size(array, 1) == 3, "1d logical array resize failed")
    call check(error, lbound(array, 1) == 1, &
               "unexpected lower bound:"//to_string(lbound(array, 1)))
    call check(error, ubound(array, 1) == 3, &
               "unexpected upper bound:"//to_string(ubound(array, 1)))
    if (allocated(error)) return

    ! set an element in the array and check new/old contents
    array(3) = .true.
    call check(error, &
               array(1) .and. &
               .not. array(2) .and. &
               array(3), &
               "1d logical array repopulation failed")
    if (allocated(error)) return
    deallocate (array)
  end subroutine test_ExpandArray_logical

  !> @brief Test 1D int array expansion with default lower bound of 1
  subroutine test_ExpandArray_int_offset(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), allocatable :: array(:)

    ! allocate array
    allocate (array(0:1))
    array(0) = 0
    array(1) = 1

    ! resize array and check new size and bounds
    call ExpandArray(array, 3)
    call check(error, size(array, 1) == 5, "1d int array resize failed")
    call check(error, lbound(array, 1) == 0, &
               "unexpected lower bound:"//to_string(lbound(array, 1)))
    call check(error, ubound(array, 1) == 4, &
               "unexpected upper bound:"//to_string(ubound(array, 1)))
    if (allocated(error)) return

    ! set new array elements and check new/old contents
    array(2) = 2
    array(3) = 3
    array(4) = 4
    call check(error, &
               array(0) == 0 .and. &
               array(1) == 1 .and. &
               array(2) == 2 .and. &
               array(3) == 3 .and. &
               array(4) == 4, &
               "1d int array repopulation failed")
    if (allocated(error)) return
    deallocate (array)

    ! allocate array with negative lower bound
    allocate (array(-1:0))
    array(-1) = -1
    array(0) = 0

    ! resize array and check new size and bounds
    call ExpandArray(array, 3)
    call check(error, size(array, 1) == 5, "1d int array resize failed")
    call check(error, lbound(array, 1) == -1, &
               "unexpected lower bound:"//to_string(lbound(array, 1)))
    call check(error, ubound(array, 1) == 3, &
               "unexpected upper bound:"//to_string(ubound(array, 1)))
    if (allocated(error)) return

    ! check contents
    array(1) = 1
    array(2) = 2
    array(3) = 3
    call check(error, &
               array(-1) == -1 .and. &
               array(0) == 0 .and. &
               array(1) == 1 .and. &
               array(2) == 2 .and. &
               array(3) == 3, &
               "1d int array repopulation failed")
    if (allocated(error)) return
    deallocate (array)
  end subroutine test_ExpandArray_int_offset

  !> @brief Test 1D dbl array expansion with lower bound /= 1
  subroutine test_ExpandArray_dbl_offset(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP), allocatable :: array(:)

    ! allocate array
    allocate (array(0:1))
    array(0) = 0.5_DP
    array(1) = 0.7_DP

    ! resize array and check new size and bounds
    call ExpandArray(array, 1)
    call check(error, size(array, 1) == 3, "1d dbl array resize failed")
    call check(error, lbound(array, 1) == 0, &
               "unexpected lower bound:"//to_string(lbound(array, 1)))
    call check(error, ubound(array, 1) == 2, &
               "unexpected upper bound:"//to_string(ubound(array, 1)))
    if (allocated(error)) return

    ! set new array element and check new/old contents
    array(2) = 0.1_DP
    call check(error, &
               array(0) == 0.5_DP .and. &
               array(1) == 0.7_DP .and. &
               array(2) == 0.1_DP, &
               "1d dbl array repopulation failed")
    if (allocated(error)) return
    deallocate (array)
  end subroutine test_ExpandArray_dbl_offset

  !> @brief Test 1D logical array expansion with lower bound /= 1
  subroutine test_ExpandArray_logical_offset(error)
    type(error_type), allocatable, intent(out) :: error
    logical(LGP), allocatable :: array(:)

    ! allocate array
    allocate (array(0:1))
    array(0) = .true.
    array(1) = .false.

    ! resize array and check new size and bounds
    call ExpandArray(array, 1)
    call check(error, size(array, 1) == 3, "1d logical array resize failed")
    call check(error, lbound(array, 1) == 0, &
               "unexpected lower bound:"//to_string(lbound(array, 1)))
    call check(error, ubound(array, 1) == 2, &
               "unexpected upper bound:"//to_string(ubound(array, 1)))
    if (allocated(error)) return

    ! set an element in the array and check new/old contents
    array(2) = .true.
    call check(error, &
               array(0) .and. &
               .not. array(1) .and. &
               array(2), &
               "1d logical array repopulation failed")
    if (allocated(error)) return
    deallocate (array)
  end subroutine test_ExpandArray_logical_offset

  !> @brief Test 2D int array expansion with default lower bound of 1
  subroutine test_ExpandArray2D_int(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), allocatable :: array(:, :)

    ! allocate array and check initial size
    allocate (array(2, 2))
    array(1, :) = (/1, 2/)
    array(2, :) = (/2, 3/)
    call check(error, size(array, 1) == 2 .and. size(array, 2) == 2)
    if (allocated(error)) return

    ! resize array and check new size and bounds
    call ExpandArray2D(array, 1, 1)
    call check(error, &
               size(array, 1) == 3 .and. size(array, 2) == 3, &
               "2d int array resize failed")
    call check(error, lbound(array, 1) == 1, &
               "unexpected lower bound (d1):"//to_string(lbound(array, 1)))
    call check(error, ubound(array, 1) == 3, &
               "unexpected upper bound (d1):"//to_string(ubound(array, 1)))
    call check(error, lbound(array, 2) == 1, &
               "unexpected lower bound (d2):"//to_string(lbound(array, 2)))
    call check(error, ubound(array, 2) == 3, &
               "unexpected upper bound (d2):"//to_string(ubound(array, 2)))
    if (allocated(error)) return

    ! add new array elements and check new/old contents
    array(3, :) = (/3, 4, 5/)
    call check(error, &
               array(1, 1) == 1 .and. &
               array(1, 2) == 2 .and. &
               ! can't guarantee unassigned item value
               ! array(1, 3) == 0 .and. &
               array(2, 1) == 2 .and. &
               array(2, 2) == 3 .and. &
               ! can't guarantee unassigned item value
               ! array(2, 3) == 0 .and. &
               array(3, 1) == 3 .and. &
               array(3, 2) == 4 .and. &
               array(3, 3) == 5, &
               "2d int array repopulation failed")
    deallocate (array)
  end subroutine test_ExpandArray2D_int

  !> @brief Test 2D dbl array expansion with default lower bound of 1
  subroutine test_ExpandArray2D_dbl(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP), allocatable :: array(:, :)

    ! allocate array and check initial size
    allocate (array(2, 2))
    array(1, :) = (/1.0_DP, 2.0_DP/)
    array(2, :) = (/2.0_DP, 3.0_DP/)
    call check(error, size(array, 1) == 2 .and. size(array, 2) == 2)
    if (allocated(error)) return

    ! resize array and check new size and bounds
    call ExpandArray2D(array, 1, 1)
    call check(error, &
               size(array, 1) == 3 .and. size(array, 2) == 3, &
               "2d dbl array resize failed")
    call check(error, lbound(array, 1) == 1, &
               "unexpected lower bound (d1):"//to_string(lbound(array, 1)))
    call check(error, ubound(array, 1) == 3, &
               "unexpected upper bound (d1):"//to_string(ubound(array, 1)))
    call check(error, lbound(array, 2) == 1, &
               "unexpected lower bound (d2):"//to_string(lbound(array, 2)))
    call check(error, ubound(array, 2) == 3, &
               "unexpected upper bound (d2):"//to_string(ubound(array, 2)))
    if (allocated(error)) return

    ! set new array elements and check new/old contents
    array(3, :) = (/3.0_DP, 4.0_DP, 5.0_DP/)
    call check(error, &
               array(1, 1) == 1.0_DP .and. &
               array(1, 2) == 2.0_DP .and. &
               ! can't guarantee unassigned item value
               ! array(1, 3) == 0.0_DP .and. &
               array(2, 1) == 2.0_DP .and. &
               array(2, 2) == 3.0_DP .and. &
               ! can't guarantee unassigned item value
               ! array(2, 3) == 0.0_DP .and. &
               array(3, 1) == 3.0_DP .and. &
               array(3, 2) == 4.0_DP .and. &
               array(3, 3) == 5.0_DP, &
               "2d dbl array repopulation failed")
    deallocate (array)
  end subroutine test_ExpandArray2D_dbl

  !> @brief Test 2D int array expansion when lower bound /= 1
  subroutine test_ExpandArray2D_int_offset(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), allocatable :: array(:, :)

    ! allocate array and check initial size
    allocate (array(0:1, 0:1))
    array(0, :) = (/1, 2/)
    array(1, :) = (/2, 3/)
    call check(error, size(array, 1) == 2 .and. size(array, 2) == 2)
    if (allocated(error)) return

    ! resize array and check new size
    call ExpandArray2D(array, 1, 1)
    call check(error, &
               size(array, 1) == 3 .and. size(array, 2) == 3, &
               "2d int array resize failed")
    call check(error, lbound(array, 1) == 0, &
               "unexpected lower bound (d1):"//to_string(lbound(array, 1)))
    call check(error, ubound(array, 1) == 2, &
               "unexpected upper bound (d1):"//to_string(ubound(array, 1)))
    call check(error, lbound(array, 2) == 0, &
               "unexpected lower bound (d2):"//to_string(lbound(array, 2)))
    call check(error, ubound(array, 2) == 2, &
               "unexpected upper bound (d2):"//to_string(ubound(array, 2)))
    if (allocated(error)) return

    ! add new array elements and check new/old contents
    array(2, :) = (/3, 4, 5/)
    call check(error, &
               array(0, 0) == 1 .and. &
               array(0, 1) == 2 .and. &
               ! can't guarantee unassigned item value
               ! array(1, 3) == 0 .and. &
               array(1, 0) == 2 .and. &
               array(1, 1) == 3 .and. &
               ! can't guarantee unassigned item value
               ! array(2, 3) == 0 .and. &
               array(2, 0) == 3 .and. &
               array(2, 1) == 4 .and. &
               array(2, 2) == 5, &
               "2d int array repopulation failed")
    deallocate (array)
  end subroutine test_ExpandArray2D_int_offset

  !> @brief Test 2D dbl array expansion when lower bound /= 1
  subroutine test_ExpandArray2D_dbl_offset(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP), allocatable :: array(:, :)

    ! allocate array and check initial size
    allocate (array(0:1, 0:1))
    array(0, :) = (/1.0_DP, 2.0_DP/)
    array(1, :) = (/2.0_DP, 3.0_DP/)
    call check(error, size(array, 1) == 2 .and. size(array, 2) == 2)
    if (allocated(error)) return

    ! resize array and check new size
    call ExpandArray2D(array, 1, 1)
    call check(error, &
               size(array, 1) == 3 .and. size(array, 2) == 3, &
               "2d dbl array resize failed")
    call check(error, lbound(array, 1) == 0, &
               "unexpected lower bound (d1):"//to_string(lbound(array, 1)))
    call check(error, ubound(array, 1) == 2, &
               "unexpected upper bound (d1):"//to_string(ubound(array, 1)))
    call check(error, lbound(array, 2) == 0, &
               "unexpected lower bound (d2):"//to_string(lbound(array, 2)))
    call check(error, ubound(array, 2) == 2, &
               "unexpected upper bound (d2):"//to_string(ubound(array, 2)))
    if (allocated(error)) return

    ! set new array elements and check new/old contents
    array(2, :) = (/3.0_DP, 4.0_DP, 5.0_DP/)
    call check(error, &
               array(0, 0) == 1.0_DP .and. &
               array(0, 1) == 2.0_DP .and. &
               ! can't guarantee unassigned item value
               ! array(1, 3) == 0.0_DP .and. &
               array(1, 0) == 2.0_DP .and. &
               array(1, 1) == 3.0_DP .and. &
               ! can't guarantee unassigned item value
               ! array(2, 3) == 0.0_DP .and. &
               array(2, 0) == 3.0_DP .and. &
               array(2, 1) == 4.0_DP .and. &
               array(2, 2) == 5.0_DP, &
               "2d dbl array repopulation failed")
    deallocate (array)
  end subroutine test_ExpandArray2D_dbl_offset
end module TestArrayHandlers

module TestArrayHandlers
  use KindModule, only: I4B, DP, LGP
  use testdrive, only: error_type, unittest_type, new_unittest, check, test_failed
  use ArrayHandlersModule, only: ExpandArray, ExpandArray2D
  use ConstantsModule, only: LINELENGTH
  implicit none
  private
  public :: collect_arrayhandlers

contains

  subroutine collect_arrayhandlers(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("ExpandArray_int", test_ExpandArray_int), &
                new_unittest("ExpandArray_dbl", test_ExpandArray_dbl), &
                new_unittest("ExpandArray_log", test_ExpandArray_log), &
                new_unittest("ExpandArray2D_int", test_ExpandArray2D_int), &
                new_unittest("ExpandArray2D_dbl", test_ExpandArray2D_dbl) &
                ]
  end subroutine collect_arrayhandlers

  subroutine test_ExpandArray_int(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), allocatable :: array(:)

    ! allocate array
    allocate (array(2))
    array(1) = 0
    array(2) = 1

    ! resize array
    call ExpandArray(array, 3)

    ! check that array has been resized
    call check(error, size(array, 1) == 5, "1d int array resize failed")
    if (allocated(error)) return

    ! set new array elements
    array(3) = 2
    array(4) = 3
    array(5) = 4

    ! check array contents
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

  subroutine test_ExpandArray_dbl(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP), allocatable :: array(:)

    ! allocate array
    allocate (array(2))
    array(1) = 0.5_DP
    array(2) = 0.7_DP

    ! resize array
    call ExpandArray(array, 1)

    ! check that array has been resized
    call check(error, size(array, 1) == 3, "1d dbl array resize failed")
    if (allocated(error)) return

    ! set new array element
    array(3) = 0.1_DP

    ! check array contents
    call check(error, &
               array(1) == 0.5_DP .and. &
               array(2) == 0.7_DP .and. &
               array(3) == 0.1_DP, &
               "1d dbl array repopulation failed")
    if (allocated(error)) return

    deallocate (array)

  end subroutine test_ExpandArray_dbl

  subroutine test_ExpandArray_log(error)
    type(error_type), allocatable, intent(out) :: error
    logical(LGP), allocatable :: array(:)

    ! allocate array
    allocate (array(2))
    array(1) = .true.
    array(2) = .false.

    ! resize array
    call ExpandArray(array, 1)

    ! check that array has been resized
    call check(error, size(array, 1) == 3, "1d logical array resize failed")
    if (allocated(error)) return

    ! set an element in the array
    array(3) = .true.

    ! check array contents
    call check(error, &
               array(1) .and. &
               .not. array(2) .and. &
               array(3), &
               "1d logical array repopulation failed")
    if (allocated(error)) return

    deallocate (array)

  end subroutine test_ExpandArray_log

  subroutine test_ExpandArray2D_int(error)
    type(error_type), allocatable, intent(out) :: error
    integer(I4B), allocatable :: array(:, :)

    ! allocate array
    allocate (array(2, 2))
    array(1, :) = (/1, 2/)
    array(2, :) = (/2, 3/)

    ! check initial array size
    call check(error, size(array, 1) == 2 .and. size(array, 2) == 2)
    if (allocated(error)) return

    ! resize array
    call ExpandArray2D(array, 1, 1)

    ! check that array has been resized
    call check(error, &
               size(array, 1) == 3 .and. size(array, 2) == 3, &
               "2d int array resize failed")
    if (allocated(error)) return

    ! add new array elements
    array(3, :) = (/3, 4, 5/)

    ! check array contents
    call check(error, &
               array(1, 1) == 1 .and. &
               array(1, 2) == 2 .and. &
               array(1, 3) == 0 .and. &
               array(2, 1) == 2 .and. &
               array(2, 2) == 3 .and. &
               array(2, 3) == 0 .and. &
               array(3, 1) == 3 .and. &
               array(3, 2) == 4 .and. &
               array(3, 3) == 5, &
               "2d int array repopulation failed")

    deallocate (array)

  end subroutine test_ExpandArray2D_int

  subroutine test_ExpandArray2D_dbl(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP), allocatable :: array(:, :)

    ! allocate array
    allocate (array(2, 2))
    array(1, :) = (/1.0_DP, 2.0_DP/)
    array(2, :) = (/2.0_DP, 3.0_DP/)

    ! check initial array size
    call check(error, size(array, 1) == 2 .and. size(array, 2) == 2)
    if (allocated(error)) return

    ! resize array
    call ExpandArray2D(array, 1, 1)

    ! check that array has been resized
    call check(error, &
               size(array, 1) == 3 .and. size(array, 2) == 3, &
               "2d dbl array resize failed")
    if (allocated(error)) return

    ! set new array elements
    array(3, :) = (/3.0_DP, 4.0_DP, 5.0_DP/)

    ! check array contents
    call check(error, &
               array(1, 1) == 1.0_DP .and. &
               array(1, 2) == 2.0_DP .and. &
               array(1, 3) == 0.0_DP .and. &
               array(2, 1) == 2.0_DP .and. &
               array(2, 2) == 3.0_DP .and. &
               array(2, 3) == 0.0_DP .and. &
               array(3, 1) == 3.0_DP .and. &
               array(3, 2) == 4.0_DP .and. &
               array(3, 3) == 5.0_DP, &
               "2d dbl array repopulation failed")

    deallocate (array)

  end subroutine test_ExpandArray2D_dbl
end module TestArrayHandlers

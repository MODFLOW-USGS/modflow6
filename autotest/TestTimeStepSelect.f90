module TestTimeStepSelect
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use TimeStepSelectModule, only: TimeStepSelectType
  use ConstantsModule, only: LINELENGTH

  implicit none
  private
  public :: collect_timestepselect

contains

  subroutine collect_timestepselect(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("first", test_first), &
                new_unittest("last", test_last), &
                new_unittest("all", test_all), &
                new_unittest("freq", test_freq), &
                new_unittest("step", test_step), &
                new_unittest("multiple", test_multiple) &
                ]
  end subroutine collect_timestepselect

  subroutine test_first(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeStepSelectType) :: steps
    character(len=LINELENGTH) :: line

    line = "FIRST"

    call steps%init()
    call steps%read(line)

    call check(error, steps%is_selected(1, .false.))
    if (allocated(error)) return

    call check(error, steps%is_selected(1, .true.))
    if (allocated(error)) return

    call check(error,.not. steps%is_selected(2, .false.))
    if (allocated(error)) return

  end subroutine test_first

  subroutine test_last(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeStepSelectType) :: steps
    character(len=LINELENGTH) :: line

    line = "LAST"

    call steps%init()
    call steps%read(line)

    call check(error,.not. steps%is_selected(1, .false.))
    if (allocated(error)) return

    call check(error, steps%is_selected(1, .true.))
    if (allocated(error)) return

  end subroutine test_last

  subroutine test_all(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeStepSelectType) :: steps
    character(len=LINELENGTH) :: line

    line = "ALL"

    call steps%init()
    call steps%read(line)

    call check(error, steps%is_selected(1, .true.))
    if (allocated(error)) return

    call check(error, steps%is_selected(1, .false.))
    if (allocated(error)) return

  end subroutine test_all

  subroutine test_freq(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeStepSelectType) :: steps
    character(len=LINELENGTH) :: line

    line = "FREQUENCY 2"

    call steps%init()
    call steps%read(line)

    call check(error,.not. steps%is_selected(1, .false.))
    if (allocated(error)) return

    call check(error, steps%is_selected(2, .false.))
    if (allocated(error)) return

    call check(error,.not. steps%is_selected(3, .false.))
    if (allocated(error)) return

    call check(error, steps%is_selected(4, .false.))
    if (allocated(error)) return

  end subroutine test_freq

  subroutine test_step(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeStepSelectType) :: steps
    character(len=LINELENGTH) :: line

    line = "STEPS 1"

    call steps%init()
    call steps%read(line)

    call check(error, steps%is_selected(1, .false.))
    if (allocated(error)) return

    call check(error,.not. steps%is_selected(2, .false.))
    if (allocated(error)) return

  end subroutine test_step

  subroutine test_multiple(error)
    type(error_type), allocatable, intent(out) :: error
    type(TimeStepSelectType) :: steps
    character(len=LINELENGTH) :: line

    call steps%init()

    line = "FIRST"
    call steps%read(line)

    line = "LAST"
    call steps%read(line)

    line = "STEPS 2"
    call steps%read(line)

    call check(error, steps%is_selected(1, .false.))
    if (allocated(error)) return

    call check(error, steps%is_selected(2, .false.))
    if (allocated(error)) return

    call check(error, steps%is_selected(3, .true.))
    if (allocated(error)) return

  end subroutine test_multiple

end module TestTimeStepSelect

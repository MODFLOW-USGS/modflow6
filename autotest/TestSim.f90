module TestSim
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use SimModule, only: store_error, store_warning, store_note, &
                       initial_message, count_errors, count_notes, &
                       count_warnings
  use ConstantsModule, only: LINELENGTH

  implicit none
  private
  public :: collect_sim

contains

  subroutine collect_sim(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("store_and_count", test_store_and_count) &
                ]
  end subroutine collect_sim

  subroutine test_store_and_count(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=LINELENGTH) :: ntemsg
    character(len=LINELENGTH) :: wrnmsg
    character(len=LINELENGTH) :: errmsg

    ! define messages
    ntemsg = "NOTE"
    wrnmsg = "WARNING"
    errmsg = "ERROR"

    ! initialize message arrays
    call initial_message()

    ! check no messages stored
    call check(error, count_errors() == 0)
    call check(error, count_warnings() == 0)
    call check(error, count_notes() == 0)
    if (allocated(error)) return

    ! todo store a note and check that it's stored
    call store_note(ntemsg)
    call check(error, count_notes() == 1)
    if (allocated(error)) return

    ! todo store a warning and check that it's stored
    call store_warning(wrnmsg)
    call check(error, count_warnings() == 1)
    if (allocated(error)) return

    ! store an error and check that it's stored
    call store_error(errmsg, terminate=.false.)
    call check(error, count_errors() == 1)
    if (allocated(error)) return

  end subroutine test_store_and_count

end module TestSim

module TestMessage
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use MessageModule, only: MessagesType
  use ConstantsModule, only: LINELENGTH

  implicit none
  private
  public :: collect_message

contains

  subroutine collect_message(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("init_and_count", test_init_and_count), &
                new_unittest("write_all", test_write_all) &
                ]
  end subroutine collect_message

  subroutine test_init_and_count(error)
    type(error_type), allocatable, intent(out) :: error
    type(MessagesType) :: messages
    messages = MessagesType()
    call messages%init()
    call check(error, messages%count() == 0)
  end subroutine test_init

  subroutine test_write_all(error)
    type(error_type), allocatable, intent(out) :: error
    type(MessagesType) :: messages
    messages = MessagesType()
    call messages%init()
    call messages%store("1")
    call messages%store("2")
    ! debug visually with e.g. `meson test --no-rebuild -C builddir --verbose Message`
    call messages%write_all()
  end subroutine test_write_all

end module TestMessage

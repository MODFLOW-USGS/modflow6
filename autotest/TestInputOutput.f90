module TestInputOutput
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use ConstantsModule, only: LINELENGTH
  ! use InputOutputModule, only: ???
  implicit none
  private
  public :: collect_inputoutput

contains

  subroutine collect_inputoutput(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    allocate (testsuite(0))
  end subroutine collect_inputoutput

end module TestInputOutput

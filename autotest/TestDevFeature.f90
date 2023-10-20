module TestDevFeature
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use DevFeatureModule, only: dev_feature
  use ConstantsModule, only: LINELENGTH
  use VersionModule, only: IDEVELOPMODE

  implicit none
  private
  public :: collect_dev_feature

contains

  subroutine collect_dev_feature(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                ! expect failure if in release mode, otherwise pass
                new_unittest("dev_feature", test_dev_feature, &
                             should_fail=(IDEVELOPMODE == 0)) &
                ]
  end subroutine collect_dev_feature

  subroutine test_dev_feature(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=LINELENGTH) :: errmsg
    call dev_feature(errmsg)
  end subroutine test_dev_feature

end module TestDevFeature

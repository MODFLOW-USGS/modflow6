! This module defines derived type ObsOutputListType.
!
! ObsOutputListType -- contains a list of ObsOutputType objects and
! methods needed for coordinating between an ObsType object and its
! ObsOutputType objects.  Like ObsOutputType, ObsOutputListType is
! needed only for processing continuous observations.
!-----------------------------------------------------------------------
module ObsOutputListModule

  use KindModule, only: DP, I4B
  use InputOutputModule, only: same_word
  use ListModule, only: ListType
  use ObsOutputModule, only: ObsOutputType, ConstructObsOutput, &
                             AddObsOutputToList, GetObsOutputFromList

  implicit none

  private
  public :: ObsOutputListType

  type ObsOutputListType
    ! -- Private members
    type(ListType), private :: ObsOutputs
  contains
    ! -- Public procedures
    procedure, public :: Add
    procedure, public :: ClearOutputLines
    procedure, public :: ContainsFile
    procedure, public :: Count
    procedure, public :: Get
    procedure, public :: WriteOutputLines
    procedure, public :: Clear
    procedure, public :: DeallocObsOutputList
  end type ObsOutputListType

contains

  subroutine ClearOutputLines(this)
! **************************************************************************
! ClearOutputLines -- clear the lineout member of all ObsOutputType objects
! in the list
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsOutputListType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, num
    type(ObsOutputType), pointer :: obsOutput => null()
    !
    num = this%Count()
    do i = 1, num
      obsOutput => this%Get(i)
      call obsOutput%ClearLineout()
    end do
    !
    return
  end subroutine ClearOutputLines

  function Count(this)
! **************************************************************************
! Count -- return the number of ObsOutputType objects in the list
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- return
    integer(I4B) :: count
    ! -- dummy
    class(ObsOutputListType), intent(inout) :: this
    !
    Count = this%ObsOutputs%Count()
    return
  end function Count

  logical function ContainsFile(this, fname)
! **************************************************************************
! ContainsFile -- return true if filename fname is included in list of
! ObsOutputType objects
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsOutputListType), intent(inout) :: this
    character(len=*), intent(in) :: fname
    ! -- local
    type(ObsOutputType), pointer :: obsOutput => null()
    integer(I4B) :: i, n
    !
    ContainsFile = .false.
    n = this%Count()
    loop1: do i = 1, n
      obsOutput => this%Get(i)
      if (same_word(obsOutput%filename, fname)) then
        ContainsFile = .true.
        exit loop1
      end if
    end do loop1
    return
  end function ContainsFile

  subroutine Add(this, fname, nunit)
! **************************************************************************
! Add -- construct a new ObsOutputType object with arguments assigned to
! its members, and add the new object to the list
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsOutputListType), intent(inout) :: this
    character(len=*), intent(in) :: fname
    integer(I4B), intent(in) :: nunit
    ! -- local
    type(ObsOutputType), pointer :: obsOutput => null()
    !
    call ConstructObsOutput(obsOutput, fname, nunit)
    call AddObsOutputToList(this%ObsOutputs, obsOutput)
    !
    return
  end subroutine Add

  subroutine WriteOutputLines(this)
! **************************************************************************
! WriteOutputLines -- iterate through list of ObsOutputType objects and,
! for each continuous observation, write the lineout member to the output
! file
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsOutputListType), intent(inout) :: this
    ! -- local
    type(ObsOutputType), pointer :: obsOutput => null()
    integer(I4B) :: i, num
    !
    num = this%Count()
    do i = 1, num
      obsOutput => this%Get(i)
      if (obsOutput%FormattedOutput) then
        call obsOutput%WriteLineout()
      end if
    end do
    !
    return
  end subroutine WriteOutputLines

  function Get(this, indx) result(obsOutput)
! **************************************************************************
! Get -- return the specified ObsOutputType object from the list
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsOutputListType), intent(inout) :: this
    integer(I4B), intent(in) :: indx
    ! result
    type(ObsOutputType), pointer :: obsOutput
    !
    obsOutput => GetObsOutputFromList(this%ObsOutputs, indx)
    return
  end function Get

  subroutine Clear(this)
! **************************************************************************
! Clear
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsOutputListType), intent(inout) :: this
    !
    call this%ObsOutputs%Clear()
    !
    return
  end subroutine Clear

  subroutine DeallocObsOutputList(this)
    implicit none
    ! -- dummy
    class(ObsOutputListType), intent(inout) :: this
    ! -- local
    integer :: i, n
    type(ObsOutputType), pointer :: obsoutput => null()
    !
    n = this%Count()
    do i = 1, n
      obsoutput => GetObsOutputFromList(this%ObsOutputs, i)
      !call obsoutput%DeallocObsOutput()
    end do
    !
    call this%ObsOutputs%Clear(.true.)
    !
    return
  end subroutine DeallocObsOutputList

end module ObsOutputListModule

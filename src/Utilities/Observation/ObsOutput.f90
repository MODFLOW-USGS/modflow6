! This module defines derived type ObsOutputType.
!
! ObsOutputType -- contains information and methods needed for writing
! a line of simulated values for observations to an output file.  Each
! block of type continuous in an observation file is
! associated with an ObsOutputType object. However, the methods are
! needed only for continuous observations.
!-----------------------------------------------------------------------
module ObsOutputModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENBIGLINE, LENOBSNAME
  use ListModule, only: ListType

  implicit none

  private
  public :: ObsOutputType, ConstructObsOutput, AddObsOutputToList, &
            GetObsOutputFromList

  type :: ObsOutputType
    ! -- Public members
    ! kind specified to ensure consistent binary output
    integer(kind=4), public :: nobs = 0
    integer(I4B), public :: nunit = 0
    character(len=500), public :: filename = ''
    !character(len=LENOBSNAME), allocatable, dimension(:), public :: obsnames
    character(len=LENOBSNAME), public :: header = ''
    character(len=LENOBSNAME), public :: lineout = ''
    logical, public :: FormattedOutput = .true.
  contains
    ! -- Public procedures
    procedure, public :: ClearLineout
    procedure, public :: WriteLineout
    !procedure, public :: DeallocObsOutput
  end type ObsOutputType

contains

  ! Procedures bound to ObsOutputType

  subroutine ClearLineout(this)
! **************************************************************************
! ClearLineout -- clear the lineout member
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsOutputType), intent(inout) :: this
    !
    this%lineout = ''
    return
  end subroutine ClearLineout

  subroutine WriteLineout(this)
! **************************************************************************
! WriteLineout -- write the lineout member to the output file
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsOutputType), intent(inout) :: this
    ! -- write a line return to end of observation output line 
    !    for this totim
    write(this%nunit,'(a)', advance='YES') ''
    !
    return
  end subroutine WriteLineout
  !
  !subroutine DeallocObsOutput(this)
  !  implicit none
  !  ! -- dummy
  !  class(ObsOutputType), intent(inout) :: this
  !  !
  !  if (allocated(this%obsnames)) then
  !    deallocate(this%obsnames)
  !  endif
  !  !
  !  return
  !end subroutine DeallocObsOutput

  ! Non-type-bound procedures

  function CastAsObsOutputType(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    type(ObsOutputType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    type is (ObsOutputType)
      res => obj
    class default
      continue
    end select
    return
  end function CastAsObsOutputType

  subroutine ConstructObsOutput(newObsOutput, fname, nunit)
! **************************************************************************
! ConstructObsOutput -- construct an ObsOutputType object and assign
! arguments to its members
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    type(ObsOutputType), pointer, intent(out) :: newObsOutput
    character(len=*), intent(in) :: fname
    integer(I4B), intent(in) :: nunit
    !
    allocate(newObsOutput)
    newObsOutput%filename = fname
    newObsOutput%nunit = nunit
    return
  end subroutine ConstructObsOutput

  subroutine AddObsOutputToList(list, obsOutput)
    implicit none
    ! -- dummy
    type(ListType),               intent(inout) :: list
    type(ObsOutputType), pointer, intent(inout) :: obsOutput
    ! -- local
    class(*), pointer :: obj
    !
    obj => obsOutput
    call list%Add(obj)
    !
    return
  end subroutine AddObsOutputToList

  function GetObsOutputFromList(list, idx) result (res)
    implicit none
    ! -- dummy
    type(ListType),   intent(inout) :: list
    integer(I4B),          intent(in)    :: idx
    type(ObsOutputType), pointer    :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsObsOutputType(obj)
    !
    return
  end function GetObsOutputFromList

end module ObsOutputModule

! This module defines two derived types:
!
! ObserveType -- is designed to contain all information and
!   functionality needed for one observation. ObserveType contains a
!   pointer to an ObsDataType object.
!
! ObsDataType -- is for storing package ID, observation type, and a
!   pointer to a subroutine that will be called to process the IDstring
!   provided in Obs input.  The ProcessIdPtr member of ObsDataType
!   requires a pointer to an ObserveType object.
!-----------------------------------------------------------------------
module ObserveModule

  use KindModule, only: DP, I4B
  use BaseDisModule,     only: DisBaseType
  use ConstantsModule,   only: LENBOUNDNAME, LENOBSNAME, LENOBSTYPE, &
                               MAXOBSTYPES, DNODATA
  use InputOutputModule, only: dclosetest, urword
  use ListModule,        only: ListType
  use SimModule,         only: store_warning, store_error, &
                               store_error_unit, ustop
  use TdisModule,        only: totim, totalsimtime

  implicit none

  private
  public :: ObserveType, ConstructObservation, ObsDataType, &
            AddObsToList, GetObsFromList

  type :: ObserveType
    ! -- Public members
    ! -- For all observations
    integer(I4B),                     public :: NodeNumber = 0
    integer(I4B),                     public :: UnitNumber = 0
    character(len=LENOBSNAME),        public :: Name = ''
    character(len=LENOBSTYPE),        public :: ObsTypeId = ''
    character(len=200),               public :: IDstring = ''
    character(len=LENBOUNDNAME),      public :: FeatureName = ''
    character(len=LENBOUNDNAME),      public :: FeatureName2 = ''
    ! -- members specific to NPF intercell-flow observations
    integer(I4B),                     public :: NodeNumber2 = 0
    integer(I4B),                     public :: JaIndex = -2
    ! -- members that can be used as needed by packages or models
    integer(I4B),                     public :: intPak1 = 0
    real(DP),                         public :: Obsdepth = 0.0d0
    real(DP),                         public :: dblPak1 = 0.0d0
    ! -- indxbnds is intended to hold indices of position(s) in bound
    !    array of boundaries included in the observation.
    integer(I4B), allocatable, dimension(:), public :: indxbnds
    ! Set FormattedOutput false if output unit is opened for unformatted i/o
    logical,                     public :: FormattedOutput = .true.
    logical,                     public :: BndFound = .false.
    real(DP),                    public :: CurrentTimeStepEndValue = 0.0d0
    real(DP),                    public :: CurrentTimeStepEndTime = 0.0d0
    ! -- Members specific to continuous observations
    integer(I4B),                     public :: indxObsOutput = -1
    ! -- Private members
    type(ObsDataType), pointer, private :: obsDatum => null()
  contains
    ! -- Public procedures
    procedure, public  :: ResetCurrent
    procedure, public  :: WriteTo
  end type ObserveType

  type :: ObsDataType
    ! -- Public members
    character(len=LENOBSTYPE),                public :: ObsTypeID = ''
    logical,                                  public :: Cumulative = .false.
    procedure(ProcessIdSub), nopass, pointer, public :: ProcessIdPtr => null()
  end type ObsDataType

  abstract interface
    subroutine ProcessIdSub(obsrv, dis, inunitobs, iout)
! **************************************************************************
! ProcessIdSub -- A procedure that implements this subroutine processes the
! user-provided IDstring, which identifies the grid location or model
! feature to be observed.
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
      use KindModule, only: DP, I4B
      import :: ObserveType
      import :: DisBaseType
      ! -- dummy
      type(ObserveType),  intent(inout) :: obsrv
      class(DisBaseType), intent(in)    :: dis
      integer(I4B),       intent(in)    :: inunitobs
      integer(I4B),       intent(in)    :: iout
    end subroutine ProcessIdSub
  end interface

contains

  ! Procedures bound to ObserveType

  subroutine ResetCurrent(this)
! **************************************************************************
! ResetCurrent -- Reset "current" value.
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObserveType), intent(inout) :: this
    !
    ! -- Reset current value to zero.
    this%CurrentTimeStepEndValue = 0.0d0
    return
  end subroutine ResetCurrent

  subroutine WriteTo(this, iout)
! **************************************************************************
! WriteTo -- Write information about this observation to table in list file.
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObserveType), intent(inout) :: this
    integer(I4B), intent(in) :: iout
    ! formats
    20 format(a,2x,a,a,t76,'All times',t89,'"',a,'"')
    !
    write(iout,20)this%Name, 'Continuous ', this%ObsTypeId, &
                  trim(this%IDstring)
    return
  end subroutine WriteTo

  ! Non-type-bound procedures

  subroutine ConstructObservation(newObservation, defLine, numunit, &
                                  formatted, indx, obsData, inunit)
! **************************************************************************
! ConstructObservation -- Construct and return an ObserveType object based
! on the contents of defLine.
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy variables
    type(ObserveType),    pointer :: newObservation
    character(len=*),  intent(in) :: defLine
    integer(I4B),      intent(in) :: numunit   ! Output unit number
    logical,           intent(in) :: formatted ! Formatted output?
    integer(I4B),      intent(in) :: indx      ! Index in ObsOutput array
    type(ObsDataType), dimension(:), pointer, intent(in) :: obsData
    integer(I4B),      intent(in) :: inunit
    ! -- local
    real(DP) :: r
    integer(I4B) :: i, icol, iout, istart, istop, ltrim, n
    ! --------------------------------------------------------------------------
    !
    ! -- initialize
    iout = 0
    icol = 1
    !
    ! -- Allocate an ObserveType object.
    allocate(newObservation)
    allocate(newObservation%indxbnds(0))
    !
    ! -- Define the contents of the ObservationSingleType object based on the
    !    contents of defLine.
    !
    ! -- Get observation name and store it
    call urword(defLine,icol,istart,istop,1,n,r,iout,inunit)
    newObservation%Name = defLine(istart:istop)
    !
    ! -- Get observation type, convert it to uppercase, and store it.
    call urword(defLine,icol,istart,istop,1,n,r,iout,inunit)
    newObservation%ObsTypeId = defLine(istart:istop)
    !
    ! -- Look up package ID for this observation type and store it
    do i=1,MAXOBSTYPES
      if (obsData(i)%ObsTypeID == newObservation%ObsTypeId) then
        newObservation%obsDatum => obsData(i)
        exit
      elseif (obsData(i)%ObsTypeID == '') then
        exit
      endif
    enddo
    !
    ! -- Remaining text is ID [and ID2]; store it
    ltrim = len_trim(defLine)
    call urword(defLine,icol,istart,istop,1,n,r,iout,inunit)
    newObservation%IDstring = (defLine(istart:ltrim))
    !
    ! Store UnitNumber, FormattedOutput, and IndxObsOutput
    newObservation%UnitNumber = numunit
    newObservation%FormattedOutput = formatted
    newObservation%IndxObsOutput = indx
    !
    return
  end subroutine ConstructObservation

  function CastAsObserveType(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    type(ObserveType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    type is (ObserveType)
      res => obj
    end select
    return
  end function CastAsObserveType

  subroutine AddObsToList(list, obs)
    implicit none
    ! -- dummy
    type(ListType),             intent(inout) :: list
    type(ObserveType), pointer, intent(inout) :: obs
    ! -- local
    class(*), pointer :: obj
    !
    obj => obs
    call list%Add(obj)
    !
    return
  end subroutine AddObsToList

  function GetObsFromList(list, idx) result (res)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B),        intent(in)    :: idx
    type(ObserveType), pointer    :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsObserveType(obj)
    !
    return
  end function GetObsFromList

end module ObserveModule

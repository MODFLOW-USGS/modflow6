module SfrSegmentModule

  use ListModule, only: ListType
  use SfrReachModule, only: SfrReachType, GetReachFromList

  private
  public :: SfrSegmentType, ConstructSfrSegment, CastAsSfrSegmentType, &
            GetSegmentFromList, AddSegmentToList

  type :: SfrSegmentType
    integer :: Segnum = 0
    integer :: Outseg = 0
    integer :: Iupseg = 0
    integer :: Iprior = -1
    integer :: LgrGrid = 0
    integer :: LgrSeg = 0
    real    :: Flow = 0.0
    real    :: Runoff = 0.0
    real    :: Etsw = 0.0
    real    :: Pptsw = 0.0
    real    :: RoughCh = 0.0
    real    :: Hc1fact = 0.0
    real    :: Hc2fact = 0.0
    real    :: ThickM1 = 0.0
    real    :: ThickM2 = 0.0
    real    :: ElevUp = 0.0
    real    :: ElevDn = 0.0
    real    :: Width1 = 0.0
    real    :: Width2 = 0.0
    real    :: Ustrf = 0.0
    real    :: SegLen = 0.0
    real    :: Depth1 = 0.0
    real    :: Depth2 = 0.0
    real    :: StageUp = 0.0
    real    :: StageDn = 0.0
    ! List of reaches
    type(ListType), pointer :: SegReaches => null()
  contains
    procedure, public :: AddReach
    procedure, public :: GetReach
    procedure, public :: Initialize
  end type SfrSegmentType

contains

  ! Procedures bound to type SfrSegmentType

  subroutine Initialize(this)
    implicit none
    ! dummy
    class(SfrSegmentType) :: this
    !
    allocate(this%SegReaches)
    !
    return
  end subroutine Initialize

  subroutine AddReach(this, reach)
    implicit none
    ! dummy
    class(SfrSegmentType) :: this
    type(SfrReachType), pointer :: reach
    ! local
    class(*), pointer :: obj => null()
    !
    obj => reach
    call this%SegReaches%Add(obj)
    !
    return
  end subroutine AddReach

  function GetReach(this, idx) result (res)
    implicit none
    ! dummy
    class(SfrSegmentType) :: this
    integer, intent(in) :: idx
    type(SfrReachType), pointer :: res
    !
    res => GetReachFromList(this%SegReaches, idx)
    !
    return
  end function GetReach

  ! Non-type-bound procedures

  subroutine ConstructSfrSegment(newSegment)
    implicit none
    ! dummy
    type(SfrSegmentType), pointer :: newSegment
    ! local
    !
    allocate(newSegment)
    call newSegment%Initialize()
    !
    return
  end subroutine ConstructSfrSegment

  function CastAsSfrSegmentType(obj) result (res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    type(SfrSegmentType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    type is (SfrSegmentType)
      res => obj
    end select
    return
  end function CastAsSfrSegmentType

  function GetSegmentFromList(segmentList, idx) result (res)
    implicit none
    ! dummy
    type(ListType) :: segmentList
    integer, intent(in) :: idx
    type(SfrSegmentType), pointer :: res
    ! local
    class(*), pointer :: obj => null()
    !
    obj => segmentList%GetItem(idx)
    res => CastAsSfrSegmentType(obj)
    !
    return
  end function GetSegmentFromList

  subroutine AddSegmentToList(segmentList, segmnt)
    implicit none
    ! dummy
    type(ListType), pointer :: segmentList
    type(SfrSegmentType), pointer :: segmnt
    ! local
    class(*), pointer :: obj => null()
    !
    obj => segmnt
    call segmentList%Add(obj)
    !
    return
  end subroutine AddSegmentToList

end module SfrSegmentModule



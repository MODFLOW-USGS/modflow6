module SfrReachModule

  use ArrayHandlersModule, only: ExpandArray
  use AuxiliaryModule, only: AuxiliaryType
  use ListModule, only: ListType
  use SfrDiversionModule, only: SfrDiversionType, ConstructSfrDiversion, &
                                AddDiversionToList, GetDiversionFromList

  private
  public :: SfrReachType, ConstructSfrReach, GetReachFromList, &
            AddReachToList

  type :: SfrReachType
    ! Reach-based input for SFR6
    integer, public :: rnopkg ! reach number in package
    integer, public :: rnoseg ! reach number in segment
    integer, public :: klay   ! layer
    integer, public :: irow   ! row
    integer, public :: jcol   ! column
    integer, public :: icalc = -9
    real, public    :: rlen   ! length
    real, public    :: rwid   ! width
    real, public    :: rgrd   ! gradient
    real, public    :: rtp    ! streambed top
    real, public    :: rbth   ! streambed thickness
    real, public    :: rbhk   ! streambed K
    real, public    :: man    ! Manning's n
    real, public    :: ustrf  ! upstream flow fraction
    real, public    :: stage = -9999.  ! specified stage at center of reach
    !
    ! Stress period input for SFR6
    real, public    :: inflow
    real, public    :: rainfall
    real, public    :: evap
    real, public    :: runoff
    integer, public :: inflow_iprn
    integer, public :: rainfall_iprn
    integer, public :: evap_iprn
    integer, public :: runoff_iprn
    !real, public    :: diversion
    !
    ! Segment-based (SFR2) stream network topology
    integer :: segnum  ! segment to which reach belongs
    integer :: ireach  ! reach number within segment
    !
    ! Array of connected reaches
    integer, allocatable, dimension(:) :: iconn
    !
    ! List of diversions
    type(ListType), pointer :: Diversions
    !
    ! Is this reach to be kept and written, considering idomain
    ! of parent is modified by child grids?
    logical, public :: KeepReachActive
    integer, public :: newReachNum
    !
  contains
    procedure, public :: AddConnection
    procedure, public :: AddDiversion
    procedure, public :: GetDiversion
    procedure, public :: Initialize
  end type SfrReachType

  real, parameter :: RZERO = 1.0e0

contains

  ! Procedures bound to type SfrReachType

  subroutine Initialize(this)
    implicit none
    ! dummy
    class(SfrReachType) :: this
    !
    allocate(this%Diversions)
    this%KeepReachActive = .true.
    this%newReachNum = -2
    this%inflow = 0.0
    this%rainfall = 0.0
    this%evap = 0.0
    this%runoff = 0.0
    this%inflow_iprn = 0
    this%rainfall_iprn = 0
    this%evap_iprn = 0
    this%runoff_iprn = 0
    !this%diversion  = 0.0
    this%ustrf = 0.0
    !
    return
  end subroutine Initialize

  subroutine AddConnection(this, icon)
    implicit none
    ! dummy
    class(SfrReachType) :: this
    integer, intent(in) :: icon
    ! local
    integer :: idx
    !
    call ExpandArray(this%iconn)
    idx = size(this%iconn)
    this%iconn(idx) = icon
    !
    return
  end subroutine AddConnection
  !
  subroutine AddDiversion(this, diversion)
    implicit none
    ! dummy
    class(SfrReachType) :: this
    type(SfrDiversionType), pointer :: diversion
    !
    call AddDiversionToList(this%Diversions, diversion)
    !
    return
  end subroutine AddDiversion

  function GetDiversion(this, idx) result(res)
    implicit none
    ! dummy
    class(SfrReachType) :: this
    integer, intent(in) :: idx
    type(SfrDiversionType), pointer :: res
    !
    res => GetDiversionFromList(this%Diversions, idx)
    !
    return
  end function GetDiversion

  ! Non-bound procedures

  subroutine ConstructSfrReach(newReach, SeqNum)
    implicit none
    ! dummy
    type(SfrReachType), pointer, intent(out) :: newReach
    integer, intent(inout) :: SeqNum
    !
    allocate(newReach)
    call newReach%Initialize()
    !
    ! Increment SeqNum and assign to rno member
    SeqNum = SeqNum + 1
    newReach%rnopkg = SeqNum
    newReach%newReachNum = SeqNum
    !
    ! Initialize other members
    newReach%klay = 0
    newReach%irow = 0
    newReach%jcol = 0
    newReach%rlen = RZERO
    newReach%rwid = RZERO
    newReach%rgrd = RZERO
    newReach%rtp = RZERO
    newReach%rbth = RZERO
    newReach%rbhk = RZERO
    newReach%man = RZERO
    newReach%ustrf = RZERO
    !
    return
  end subroutine ConstructSfrReach

  function CastAsSfrReachType(obj) result (res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    type(SfrReachType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    type is (SfrReachType)
      res => obj
    end select
    return
  end function CastAsSfrReachType

  function GetReachFromList(reachList, idx) result (res)
    implicit none
    ! dummy
    type(ListType), pointer :: reachList
    integer, intent(in) :: idx
    type(SfrReachType), pointer :: res
    ! local
    class(*), pointer :: obj => null()
    !
    obj => reachList%GetItem(idx)
    res => CastAsSfrReachType(obj)
    !
    return
  end function GetReachFromList

  subroutine AddReachToList(reachList, reach)
    implicit none
    ! dummy
    type(ListType), pointer :: reachList
    type(SfrReachType), pointer :: reach
    ! local
    class(*), pointer :: obj => null()
    !
    obj => reach
    call reachList%Add(obj)
    !
    return
  end subroutine AddReachToList

end module SfrReachModule

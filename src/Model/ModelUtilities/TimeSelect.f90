!> @brief Specify times for some event(s) to occur.
module TimeSelectModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DONE
  use ArrayHandlersModule, only: ExpandArray
  use ErrorUtilModule, only: pstop
  implicit none
  public :: TimeSelectType

  !> @brief Represents a series of instants at which some event should occur.
  !!
  !! Supports slicing e.g. to filter times within a given period & time step.
  !! Array storage can be expanded as needed. Note: array expansion must take
  !! place before slicing; whenever expand() is invoked, the slice is cleared.
  !! The series is assumed to strictly increase, is_increasing() checks this.
  !<
  type :: TimeSelectType
    real(DP), allocatable :: times(:)
    integer(I4B) :: slice(2)
  contains
    procedure :: destroy
    procedure :: expand
    procedure :: init
    procedure :: is_increasing
    procedure :: set_slice
  end type TimeSelectType

contains

  !> @brief Destroy the time selection object.
  subroutine destroy(this)
    class(TimeSelectType) :: this
    deallocate (this%times)
  end subroutine destroy

  !> @brief Expand capacity by the given amount. Resets the current slice.
  subroutine expand(this, increment)
    class(TimeSelectType) :: this
    integer(I4B), optional, intent(in) :: increment
    call ExpandArray(this%times, increment=increment)
    this%slice = (/1, size(this%times)/)
  end subroutine expand

  !> @brief Initialize or clear the time selection object.
  subroutine init(this)
    class(TimeSelectType) :: this
    if (.not. allocated(this%times)) allocate (this%times(0))
    this%slice = (/0, 0/)
  end subroutine

  !> @brief Determine if times strictly increase.
  !! Returns true if empty or not yet allocated.
  function is_increasing(this) result(inc)
    class(TimeSelectType) :: this
    logical(LGP) :: inc
    integer(I4B) :: i
    real(DP) :: l, t

    inc = .true.
    if (.not. allocated(this%times)) return
    do i = 1, size(this%times)
      t = this%times(i)
      if (i /= 1) then
        if (l >= t) then
          inc = .false.
          return
        end if
      end if
      l = t
    end do
  end function is_increasing

  !> @brief Slice the time selection between t0 and t1 (inclusive).
  !!
  !! Finds and stores the index of the first time at the same instant
  !! as or following the start time, and of the last time at the same
  !! instant as or preceding the end time. Allows filtering the times
  !! for e.g. a particular stress period and time step. Array indices
  !! are assumed to start at 1. If no times are found to fall within
  !! the slice (i.e. the slice falls entirely between two consecutive
  !! times or beyond the min/max range), indices are set to [-1, -1].
  !!
  !! The given start and end times are first checked against currently
  !! stored indices to avoid recalculating them if possible, allowing
  !! multiple consuming components (e.g., subdomain particle tracking
  !! solutions) to share the object efficiently, provided all proceed
  !! through stress periods and time steps in lockstep, i.e. they all
  !! solve any given period/step before any will proceed to the next.
  !<
  subroutine set_slice(this, t0, t1, changed)
    ! -- dummy
    class(TimeSelectType) :: this
    real(DP), intent(in) :: t0, t1
    logical(LGP), intent(inout), optional :: changed
    ! -- local
    integer(I4B) :: i, i0, i1
    integer(I4B) :: l, u, lp, up
    real(DP) :: t

    ! -- by default, need to iterate over all times
    i0 = 1
    i1 = size(this%times)

    ! -- if no times fall within the slice, set to [-1, -1]
    l = -1
    u = -1

    ! -- previous bounding indices
    lp = this%slice(1)
    up = this%slice(2)

    ! -- Check if we can reuse either the lower or upper bound.
    !    The lower doesn't need to change if it indexes the 1st
    !    time simultaneous with or later than the slice's start.
    !    The upper doesn't need to change if it indexes the last
    !    time before or simultaneous with the slice's end.
    if (lp > 0 .and. up > 0) then
      if (lp > 1) then
        if (this%times(lp - 1) < t0 .and. &
            this%times(lp) >= t0) then
          l = lp
          i0 = l
        end if
      end if
      if (up > 1 .and. up < i1) then
        if (this%times(up + 1) > t1 .and. &
            this%times(up) <= t1) then
          u = up
          i1 = u
        end if
      end if
      if (l == lp .and. u == up) then
        this%slice = (/l, u/)
        if (present(changed)) changed = .false.
        return
      end if
    end if

    ! -- recompute bounding indices if needed
    do i = i0, i1
      t = this%times(i)
      if (l < 0 .and. t >= t0 .and. t <= t1) l = i
      if (l > 0 .and. t <= t1) u = i
    end do
    this%slice = (/l, u/)
    if (present(changed)) changed = l /= lp .or. u /= up

  end subroutine

end module TimeSelectModule

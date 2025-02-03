!> @brief Particle tracking strategies
module MethodModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO
  use ErrorUtilModule, only: pstop
  use SubcellModule, only: SubcellType
  use ParticleModule
  use BaseDisModule, only: DisBaseType
  use PrtFmiModule, only: PrtFmiType
  use CellModule, only: CellType
  use CellDefnModule, only: CellDefnType
  use TrackControlModule, only: TrackControlType
  use TimeSelectModule, only: TimeSelectType
  use MathUtilModule, only: is_close
  implicit none

  private
  public :: MethodType

  !> @brief Base type for particle tracking methods.
  !!
  !! The PRT tracking algorithm invokes a "tracking method" for each
  !! domain. A domain can be a model, cell in a model, or subcell in
  !! a cell. Tracking proceeds recursively, delegating to a possibly
  !! arbitrary number of subdomains (currently, only the three above
  !! are recognized). A tracking method is responsible for advancing
  !! a particle through a domain, delegating to subdomains as needed
  !! depending on cell geometry (implementing the strategy pattern).
  !<
  type, abstract :: MethodType
    character(len=40), pointer, public :: name !< method name
    logical(LGP), public :: delegates !< whether the method delegates
    type(PrtFmiType), pointer, public :: fmi => null() !< ptr to fmi
    class(CellType), pointer, public :: cell => null() !< ptr to the current cell
    class(SubcellType), pointer, public :: subcell => null() !< ptr to the current subcell
    type(TrackControlType), pointer, public :: trackctl => null() !< ptr to track file control
    type(TimeSelectType), pointer, public :: tracktimes => null() !< ptr to user-defined tracking times
    integer(I4B), dimension(:), pointer, contiguous, public :: izone => null() !< pointer to zone numbers
    real(DP), dimension(:), pointer, contiguous, public :: flowja => null() !< pointer to intercell flows
    real(DP), dimension(:), pointer, contiguous, public :: porosity => null() !< pointer to aquifer porosity
    real(DP), dimension(:), pointer, contiguous, public :: retfactor => null() !< pointer to retardation factor
  contains
    ! Implemented in all subtypes
    procedure(apply), deferred :: apply
    procedure(deallocate), deferred :: deallocate
    ! Overridden in subtypes that delegate
    procedure :: pass
    procedure :: load
    ! Implemented here
    procedure :: init
    procedure :: save
    procedure :: track
    procedure :: try_pass
    procedure :: check
  end type MethodType

  abstract interface
    subroutine apply(this, particle, tmax)
      import DP
      import MethodType
      import ParticleType
      class(MethodType), intent(inout) :: this
      type(ParticleType), pointer, intent(inout) :: particle
      real(DP), intent(in) :: tmax
    end subroutine apply
    subroutine deallocate (this)
      import MethodType
      class(MethodType), intent(inout) :: this
    end subroutine deallocate
  end interface

contains

  subroutine init(this, fmi, cell, subcell, trackctl, tracktimes, &
                  izone, flowja, porosity, retfactor)
    class(MethodType), intent(inout) :: this
    type(PrtFmiType), intent(in), pointer, optional :: fmi
    class(CellType), intent(in), pointer, optional :: cell
    class(SubcellType), intent(in), pointer, optional :: subcell
    type(TrackControlType), intent(in), pointer, optional :: trackctl
    type(TimeSelectType), intent(in), pointer, optional :: tracktimes
    integer(I4B), intent(in), pointer, optional :: izone(:)
    real(DP), intent(in), pointer, optional :: flowja(:)
    real(DP), intent(in), pointer, optional :: porosity(:)
    real(DP), intent(in), pointer, optional :: retfactor(:)

    if (present(fmi)) this%fmi => fmi
    if (present(cell)) this%cell => cell
    if (present(subcell)) this%subcell => subcell
    if (present(trackctl)) this%trackctl => trackctl
    if (present(tracktimes)) this%tracktimes => tracktimes
    if (present(izone)) this%izone => izone
    if (present(flowja)) this%flowja => flowja
    if (present(porosity)) this%porosity => porosity
    if (present(retfactor)) this%retfactor => retfactor
  end subroutine init

  !> @brief Track the particle over domains of the given
  ! level until the particle terminates or tmax elapses.
  recursive subroutine track(this, particle, level, tmax)
    ! dummy
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer(I4B) :: level
    real(DP), intent(in) :: tmax
    ! local
    logical(LGP) :: advancing
    integer(I4B) :: nextlevel
    class(methodType), pointer :: submethod

    ! Advance the particle over subdomains
    advancing = .true.
    nextlevel = level + 1
    do while (advancing)
      call this%load(particle, nextlevel, submethod)
      call submethod%apply(particle, tmax)
      call this%try_pass(particle, nextlevel, advancing)
    end do
  end subroutine track

  !> @brief Try passing the particle to the next subdomain.
  subroutine try_pass(this, particle, nextlevel, advancing)
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer(I4B) :: nextlevel
    logical(LGP) :: advancing

    ! if the particle is done advancing, reset the domain boundary flag.
    if (.not. particle%advancing) then
      particle%iboundary = 0
      advancing = .false.
    else
      ! otherwise pass the particle to the next subdomain.
      ! if that leaves it on a boundary, stop advancing.
      call this%pass(particle)
      if (particle%iboundary(nextlevel - 1) .ne. 0) &
        advancing = .false.
    end if
  end subroutine try_pass

  !> @brief Load the subdomain tracking method (submethod).
  subroutine load(this, particle, next_level, submethod)
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer, intent(in) :: next_level
    class(MethodType), pointer, intent(inout) :: submethod
    call pstop(1, "load must be overridden")
  end subroutine load

  !> @brief Pass the particle to the next subdomain.
  subroutine pass(this, particle)
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    call pstop(1, "pass must be overridden")
  end subroutine pass

  !> @brief Save the particle's state to output files.
  subroutine save(this, particle, reason)
    use TdisModule, only: kper, kstp, totimc
    ! dummy
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer(I4B), intent(in) :: reason
    ! local
    integer(I4B) :: per, stp

    per = kper
    stp = kstp

    ! If tracking time falls exactly on a boundary between time steps,
    ! report the previous time step for this datum. This is to follow
    ! MP7's behavior, and because the particle will have been tracked
    ! up to this instant under the previous time step's conditions, so
    ! the time step we're about to start shouldn't get "credit" for it.
    if (particle%ttrack == totimc .and. (per > 1 .or. stp > 1)) then
      if (stp > 1) then
        stp = stp - 1
      else if (per > 1) then
        per = per - 1
        stp = 1
      end if
    end if

    call this%trackctl%save(particle, kper=per, kstp=stp, reason=reason)
  end subroutine save

  !> @brief Check reporting/terminating conditions before tracking.
  !!
  !! Check a number of conditions determining whether to continue
  !! tracking the particle or terminate it, as well as whether to
  !! record any output data as per selected reporting conditions.
  !<
  subroutine check(this, particle, cell_defn, tmax)
    ! modules
    use TdisModule, only: endofsimulation, totimc, totim
    use ParticleModule, only: TERM_WEAKSINK, TERM_NO_EXITS, &
                              TERM_STOPZONE, TERM_INACTIVE
    ! dummy
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    type(CellDefnType), pointer, intent(inout) :: cell_defn
    real(DP), intent(in) :: tmax
    ! local
    logical(LGP) :: dry_cell, dry_particle, no_exit_face, stop_zone, weak_sink
    integer(I4B) :: i
    real(DP) :: t, ttrackmax

    dry_cell = this%fmi%ibdgwfsat0(cell_defn%icell) == 0
    dry_particle = particle%z > cell_defn%top
    no_exit_face = cell_defn%inoexitface > 0
    stop_zone = cell_defn%izone > 0 .and. particle%istopzone == cell_defn%izone
    weak_sink = cell_defn%iweaksink > 0

    particle%izone = cell_defn%izone
    if (stop_zone) then
      particle%advancing = .false.
      particle%istatus = TERM_STOPZONE
      call this%save(particle, reason=3)
      return
    end if

    if (no_exit_face .and. .not. dry_cell) then
      particle%advancing = .false.
      particle%istatus = TERM_NO_EXITS
      call this%save(particle, reason=3)
      return
    end if

    if (weak_sink) then
      if (particle%istopweaksink > 0) then
        particle%advancing = .false.
        particle%istatus = TERM_WEAKSINK
        call this%save(particle, reason=3)
        return
      else
        call this%save(particle, reason=4)
      end if
    end if

    if (dry_cell) then
      if (particle%idrymeth == 0) then
        ! drop to cell bottom. handled by pass
        ! to bottom method, nothing to do here
        no_exit_face = .false.
      else if (particle%idrymeth == 1) then
        ! stop
        particle%advancing = .false.
        particle%istatus = TERM_INACTIVE
        call this%save(particle, reason=3)
        return
      else if (particle%idrymeth == 2) then
        ! stay
        particle%advancing = .false.
        no_exit_face = .false.

        ! we might report tracking times
        ! out of order here, but we want
        ! the particle termination event
        ! (if this is the last time step)
        ! to have the maximum tracking t,
        ! so we need to keep tabs on it.
        ttrackmax = totim

        ! update tracking time to time
        ! step end time and save record
        particle%ttrack = totim
        call this%save(particle, reason=2)

        ! record user tracking times
        call this%tracktimes%advance()
        if (this%tracktimes%any()) then
          do i = this%tracktimes%selection(1), this%tracktimes%selection(2)
            t = this%tracktimes%times(i)
            if (t < totimc) cycle
            if (t >= tmax) exit
            particle%ttrack = t
            call this%save(particle, reason=5)
            if (t > ttrackmax) ttrackmax = t
          end do
        end if

        ! terminate if last period/step
        if (endofsimulation) then
          particle%istatus = TERM_NO_EXITS
          particle%ttrack = ttrackmax
          call this%save(particle, reason=3)
          return
        end if
      end if
    else if (dry_particle .and. this%name /= "passtobottom") then
      if (particle%idrymeth == 0) then
        ! drop to water table
        particle%z = cell_defn%top
        call this%save(particle, reason=1)
      else if (particle%idrymeth == 1) then
        ! stop
        particle%advancing = .false.
        particle%istatus = TERM_INACTIVE
        call this%save(particle, reason=3)
        return
      else if (particle%idrymeth == 2) then
        ! stay
        particle%advancing = .false.
        no_exit_face = .false.

        ! we might report tracking times
        ! out of order here, but we want
        ! the particle termination event
        ! (if this is the last time step)
        ! to have the maximum tracking t,
        ! so we need to keep tabs on it.
        ttrackmax = totim

        ! update tracking time to time
        ! step end time and save record
        particle%ttrack = totim
        call this%save(particle, reason=2)

        ! record user tracking times
        call this%tracktimes%advance()
        if (this%tracktimes%any()) then
          do i = this%tracktimes%selection(1), this%tracktimes%selection(2)
            t = this%tracktimes%times(i)
            if (t < totimc) cycle
            if (t >= tmax) exit
            particle%ttrack = t
            call this%save(particle, reason=5)
            if (t > ttrackmax) ttrackmax = t
          end do
        end if
      end if
    end if

    if (no_exit_face) then
      particle%advancing = .false.
      particle%istatus = TERM_NO_EXITS
      call this%save(particle, reason=3)
      return
    end if

  end subroutine check

end module MethodModule

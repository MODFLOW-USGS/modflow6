!> @brief Particle tracking strategies
module MethodModule

  use KindModule, only: DP, I4B, LGP
  use ErrorUtilModule, only: pstop
  use SubcellModule, only: SubcellType
  use ParticleModule
  use BaseDisModule, only: DisBaseType
  use PrtFmiModule, only: PrtFmiType
  use CellModule, only: CellType
  use CellDefnModule, only: CellDefnType
  use TrackControlModule, only: TrackControlType
  use TimeSelectModule, only: TimeSelectType
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
    character(len=40), pointer, public :: type !< method name
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
    procedure :: update
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

  !> @brief Track particle through subdomains
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

    advancing = .true.
    nextlevel = level + 1
    do while (advancing)
      call this%load(particle, nextlevel, submethod)
      call submethod%apply(particle, tmax)
      call this%try_pass(particle, nextlevel, advancing)
    end do
  end subroutine track

  !> @brief Try passing the particle to the next subdomain
  subroutine try_pass(this, particle, nextlevel, advancing)
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer(I4B) :: nextlevel
    logical(LGP) :: advancing

    ! tracking submethod marked tracking complete?
    ! reset domain boundary flag and don't advance
    if (.not. particle%advancing) then
      particle%iboundary = 0
      advancing = .false.
    else
      ! otherwise pass particle to next subdomain
      ! and if it's on a boundary, stop advancing
      call this%pass(particle)
      if (particle%iboundary(nextlevel - 1) .ne. 0) &
        advancing = .false.
    end if
  end subroutine try_pass

  !> @brief Load subdomain tracking method (submethod)
  subroutine load(this, particle, next_level, submethod)
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer, intent(in) :: next_level
    class(MethodType), pointer, intent(inout) :: submethod
    call pstop(1, "load must be overridden")
  end subroutine load

  !> @brief Pass a particle to the next subdomain, internal use only
  subroutine pass(this, particle)
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    call pstop(1, "pass must be overridden")
  end subroutine pass

  !> @brief Save a particle's current state.
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

    ! Save the particle's state to any registered tracking output files
    call this%trackctl%save(particle, this%fmi%dis, &
                            kper=per, kstp=stp, &
                            reason=reason)
  end subroutine save

  !> @brief Update particle state and check termination conditions
  !!
  !! Update the particle's properties (e.g. advancing flag, zone number,
  !! status). If any termination conditions apply, the particle's status
  !! will be set to the appropriate termination value. If any reporting
  !! conditions apply, save particle state with the proper reason code.
  !<
  subroutine update(this, particle, cell_defn)
    ! dummy
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    type(CellDefnType), pointer, intent(inout) :: cell_defn

    particle%izone = cell_defn%izone
    if (cell_defn%izone .ne. 0) then
      if (particle%istopzone .eq. cell_defn%izone) then
        particle%advancing = .false.
        particle%istatus = 6
        call this%save(particle, reason=3) ! particle terminated
        return
      end if
    end if
    if (cell_defn%inoexitface .ne. 0) then
      particle%advancing = .false.
      particle%istatus = 5
      call this%save(particle, reason=3) ! particle terminated
      return
    end if
    if (cell_defn%iweaksink .ne. 0) then
      if (particle%istopweaksink .ne. 0) then
        particle%advancing = .false.
        particle%istatus = 3
        call this%save(particle, reason=3) ! particle terminated
        return
      else
        call this%save(particle, reason=4) ! particle exited weak sink
        return
      end if
    end if
  end subroutine update

end module MethodModule

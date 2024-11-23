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
    procedure :: prepare
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

    ! Save the particle's state to any registered tracking output files
    call this%trackctl%save(particle, kper=per, &
                            kstp=stp, reason=reason)
  end subroutine save

  !> @brief Prepare to apply the tracking method to the particle.
  !!
  !! Check a number of conditions determining whether to continue
  !! tracking the particle or stop tracking and move on. Check if
  !! any reporting conditions apply also, and save the particle's
  !! state if so.
  !<
  subroutine prepare(this, particle, cell_defn)
    ! dummy
    class(MethodType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    type(CellDefnType), pointer, intent(inout) :: cell_defn
    ! local
    logical(LGP) :: cell_dry
    logical(LGP) :: locn_dry
    integer(I4B) :: ic

    cell_dry = cell_defn%is_dry()
    locn_dry = particle%z > cell_defn%top

    ! dry
    if (cell_dry .or. locn_dry) then
      ! drop
      if (particle%idry == 0) then
        if (cell_dry) then
          ! if no active cell below particle's position, terminate
          ic = particle%idomain(2)
          call this%fmi%dis%highest_active(ic, this%fmi%ibound)
          if (this%fmi%ibound(ic) == 0) then
            particle%advancing = .false.
            particle%istatus = 7
            call this%save(particle, reason=3)
            return
          end if
          ! drop to cell below
          particle%z = cell_defn%bot
          particle%iboundary(2) = this%cell%defn%npolyverts + 2
          call this%save(particle, reason=1)
        else if (locn_dry) then
          ! drop to water table
          particle%z = cell_defn%top
          call this%save(particle, reason=1)
        end if
      else if (particle%idry == 1) then
        ! stop
        particle%advancing = .false.
        particle%istatus = 7
        call this%save(particle, reason=3)
        return
      else if (particle%idry == 2) then
        ! stay
        particle%advancing = .false.
        call this%save(particle, reason=6)
      end if
    end if

    ! stop zone
    if (cell_defn%izone .ne. 0) then
      if (particle%istopzone .eq. cell_defn%izone) then
        particle%advancing = .false.
        particle%istatus = 6
        call this%save(particle, reason=3)
        return
      end if
    end if

    ! cell with no exit face
    if (cell_defn%inoexitface .ne. 0) then
      particle%advancing = .false.
      particle%istatus = 5
      call this%save(particle, reason=3)
      return
    end if

    ! weak sink
    if (cell_defn%iweaksink .ne. 0) then
      if (particle%istopweaksink == 0) then
        call this%save(particle, reason=4)
      else
        particle%advancing = .false.
        particle%istatus = 3
        call this%save(particle, reason=3)
        return
      end if
    end if
  end subroutine prepare

end module MethodModule

module TrackModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DONE
  use ParticleModule, only: ParticleType

  implicit none

  private save_record
  public :: TrackFileType
  public :: TrackFileControlType

  !> @brief Output file containing all or some particle pathlines.
  !!
  !! Can be associated with a particle release point (PRP) package
  !! or with an entire model, and can be binary or comma-separated.
  !<
  type :: TrackFileType
    integer(I4B) :: iun = 0 !< file unit number
    logical(LGP) :: csv = .false. !< whether the file is binary or CSV
    integer(I4B) :: iprp = -1 !< -1 is model-level file, 0 is exchange PRP
  end type TrackFileType

  !> @brief Manages particle track (i.e. pathline) files.
  !!
  !! Optionally filters events ("ireason" codes, selectable in the PRT-OC pkg):
  !!
  !!    0: RELEASE: particle is released
  !!    1: TRANSIT: particle moves from cell to cell
  !!    2: TIMESTEP: timestep ends
  !!    3: TERMINATE: tracking stops for a particle
  !!    4: WEAKSINK: particle exits a weak sink
  !!    5: USERTIME: user-specified tracking time
  !!
  !! An arbitrary number of files can be managed. Internal arrays
  !! are resized as needed.
  !<
  type :: TrackFileControlType
    private
    type(TrackFileType), public, allocatable :: trackfiles(:) !< output files
    integer(I4B), public :: ntrackfiles !< number of output files
    logical(LGP), public :: trackrelease !< track release events
    logical(LGP), public :: tracktransit !< track cell-to-cell transitions
    logical(LGP), public :: tracktimestep !< track timestep ends
    logical(LGP), public :: trackterminate !< track termination events
    logical(LGP), public :: trackweaksink !< track weak sink exit events
    logical(LGP), public :: trackusertime !< track user-selected times
  contains
    procedure :: expand
    procedure, public :: init_track_file
    procedure, public :: save
    procedure, public :: set_track_events
  end type TrackFileControlType

  ! Track file header
  character(len=*), parameter, public :: TRACKHEADER = &
    'kper,kstp,imdl,iprp,irpt,ilay,icell,izone,&
    &istatus,ireason,trelease,t,x,y,z,name'

  ! Track file dtypes
  character(len=*), parameter, public :: TRACKDTYPES = &
    '<i4,<i4,<i4,<i4,<i4,<i4,<i4,<i4,&
    &<i4,<i4,<f8,<f8,<f8,<f8,<f8,|S40'

  ! Notes
  ! -----
  !
  ! Each particle's pathline consists of 1+ records reported as the particle
  ! is tracked over the model domain. Records are snapshots of the particle's
  ! state (e.g. tracking status, position) at a particular moment in time.
  !
  ! Particles have no ID property. Particles can be uniquely identified
  ! by composite key, i.e. combination of:
  !
  !   - imdl: originating model ID
  !   - iprp: originating PRP ID
  !   - irpt: particle release location ID
  !   - trelease: particle release time
  !
  ! Each record has an "ireason" property, which identifies the cause of
  ! the record. The user selects 1+ conditions or events for recording.
  ! Identical records (except "ireason") may be duplicated if multiple
  ! reporting conditions apply to particles at the same moment in time.
  ! Each "ireason" value corresponds to an OC "trackevent" option value:
  !
  !     0: particle released
  !     1: particle transitioned between cells
  !     2: current time step ended****
  !     3: particle terminated
  !     4: particle exited weak sink
  !     5: user-specified tracking time
  !
  ! Each record has an "istatus" property, which is the tracking status;
  ! e.g., awaiting release, active, terminated. A particle may terminate
  ! for several reasons. Status values greater than one imply termination.
  ! Particle status strictly increases over time, starting at zero:
  !
  !     0: pending release*
  !     1: active
  !     2: terminated at boundary face
  !     3: terminated in weak sink cell
  !     4: terminated in weak source cell**
  !     5: terminated in cell with no exit face
  !     6: terminated in cell with specified zone number
  !     7: terminated in inactive cell
  !     8: permanently unreleased***
  !     9: terminated for unknown reason*
  !
  ! PRT shares the same status enumeration as MODPATH 7. However, some
  ! don't apply to PRT; for instance, MODPATH 7 distinguishes forwards
  ! and backwards tracking, but status value 4 is not used by PRT.
  !
  ! --------------------------------------------------
  !   * is this necessary?
  !   ** unnecessary since PRT makes no distinction between forwards/backwards tracking
  !   *** e.g., released into an inactive cell, a stop zone cell, or a termination zone
  !   **** this may coincide with termination, in which case two events are reported

contains

  !> @brief Initialize a new track file
  subroutine init_track_file(this, iun, csv, iprp)
    ! -- dummy
    class(TrackFileControlType) :: this
    integer(I4B), intent(in) :: iun
    logical(LGP), intent(in), optional :: csv
    integer(I4B), intent(in), optional :: iprp
    ! -- local
    type(TrackFileType), pointer :: file

    ! -- allocate or expand array
    if (.not. allocated(this%trackfiles)) then
      allocate (this%trackfiles(1))
    else
      call this%expand(increment=1)
    end if

    ! -- setup new file
    allocate (file)
    file%iun = iun
    if (present(csv)) file%csv = csv
    if (present(iprp)) file%iprp = iprp

    ! -- update array and counter
    this%ntrackfiles = size(this%trackfiles)
    this%trackfiles(this%ntrackfiles) = file

  end subroutine init_track_file

  !> @brief Expand the trackfile array, internal use only
  subroutine expand(this, increment)
    ! -- dummy
    class(TrackFileControlType) :: this
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    integer(I4B) :: inclocal
    integer(I4B) :: isize
    integer(I4B) :: newsize
    type(TrackFileType), allocatable, dimension(:) :: temp

    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    end if

    ! -- increase size of array
    if (allocated(this%trackfiles)) then
      isize = size(this%trackfiles)
      newsize = isize + inclocal
      allocate (temp(newsize))
      temp(1:isize) = this%trackfiles
      deallocate (this%trackfiles)
      call move_alloc(temp, this%trackfiles)
    else
      allocate (this%trackfiles(inclocal))
    end if

  end subroutine expand

  !> @brief Save record to binary or CSV file, internal use only
  subroutine save_record(iun, particle, kper, kstp, reason, csv)
    ! -- dummy
    integer(I4B), intent(in) :: iun
    type(ParticleType), pointer, intent(in) :: particle
    integer(I4B), intent(in) :: kper
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: reason
    logical(LGP), intent(in) :: csv
    ! -- local
    real(DP) :: x
    real(DP) :: y
    real(DP) :: z
    integer(I4B) :: status

    ! -- Get model (global) coordinates
    call particle%get_model_coords(x, y, z)

    ! -- Get status
    if (particle%istatus .lt. 0) then
      status = 1
    else
      status = particle%istatus
    end if

    if (csv) then
      write (iun, '(*(G0,:,","))') &
        kper, &
        kstp, &
        particle%imdl, &
        particle%iprp, &
        particle%irpt, &
        particle%ilay, &
        particle%icu, &
        particle%izone, &
        status, &
        reason, &
        particle%trelease, &
        particle%ttrack, &
        x, &
        y, &
        z, &
        trim(adjustl(particle%name))
    else
      write (iun) &
        kper, &
        kstp, &
        particle%imdl, &
        particle%iprp, &
        particle%irpt, &
        particle%ilay, &
        particle%icu, &
        particle%izone, &
        status, &
        reason, &
        particle%trelease, &
        particle%ttrack, &
        x, &
        y, &
        z, &
        particle%name
    end if

  end subroutine

  !> @brief Save the particle's state to track output file(s).
  !!
  !! A record is saved to all enabled model-level files and to
  !! any PRP-level files with PRP index matching the particle's
  !! PRP index.
  !<
  subroutine save(this, particle, kper, kstp, reason, level)
    ! -- dummy
    class(TrackFileControlType), intent(inout) :: this
    type(ParticleType), pointer, intent(in) :: particle
    integer(I4B), intent(in) :: kper
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: reason
    integer(I4B), intent(in), optional :: level
    ! -- local
    integer(I4B) :: i
    type(TrackFileType) :: file

    ! -- Only save if reporting is enabled for specified event.
    if (.not. ((this%trackrelease .and. reason == 0) .or. &
               (this%tracktransit .and. reason == 1) .or. &
               (this%tracktimestep .and. reason == 2) .or. &
               (this%trackterminate .and. reason == 3) .or. &
               (this%trackweaksink .and. reason == 4) .or. &
               (this%trackusertime .and. reason == 5))) &
      return

    ! -- For now, only allow reporting from outside the tracking
    !    algorithm (e.g. release time), in which case level will
    !    not be provided, or if within the tracking solution, in
    !    subcells (level 3) only. This may change if the subcell
    !    ever delegates tracking to even smaller subcomponents.
    if (present(level)) then
      if (level .ne. 3) return
    end if

    ! -- Save to any enabled model-scoped or PRP-scoped files
    do i = 1, this%ntrackfiles
      file = this%trackfiles(i)
      if (file%iun > 0 .and. &
          (file%iprp == -1 .or. &
           file%iprp == particle%iprp)) &
        call save_record(file%iun, particle, &
                         kper, kstp, reason, csv=file%csv)
    end do

  end subroutine save

  !> @brief Configure particle events to track.
  !!
  !! Each tracking event corresponds to an "ireason" code
  !! as appears in each row of track output.
  !<
  subroutine set_track_events(this, &
                              release, &
                              transit, &
                              timestep, &
                              terminate, &
                              weaksink, &
                              usertime)
    class(TrackFileControlType) :: this
    logical(LGP), intent(in) :: release
    logical(LGP), intent(in) :: transit
    logical(LGP), intent(in) :: timestep
    logical(LGP), intent(in) :: terminate
    logical(LGP), intent(in) :: weaksink
    logical(LGP), intent(in) :: usertime
    this%trackrelease = release
    this%tracktransit = transit
    this%tracktimestep = timestep
    this%trackterminate = terminate
    this%trackweaksink = weaksink
    this%trackusertime = usertime
  end subroutine set_track_events

end module TrackModule

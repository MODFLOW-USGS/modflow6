module TrackFileModule
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DPIO180
  use ParticleModule, only: ParticleType
  use GeomUtilModule, only: transform

  implicit none
  public :: TrackFileType
  public :: save_record

  !> @brief Output file containing all or some particle pathlines.
  !!
  !! Can be associated with a particle release point (PRP) package
  !! or with an entire model, and can be binary or comma-separated.
  !!
  !! Each particle's pathline consists of 1+ records reported as the particle
  !! is tracked over the model domain. Records are snapshots of the particle's
  !! state (e.g. tracking status, position) at a particular moment in time.
  !!
  !! Particles have no ID property. Particles can be uniquely identified
  !! by composite key, i.e. combination of:
  !!
  !!   - imdl: originating model ID
  !!   - iprp: originating PRP ID
  !!   - irpt: particle release location ID
  !!   - trelease: particle release time
  !!
  !! Each record has an "ireason" property, which identifies the particle
  !! event. The user selects among particle output events to be reported.
  !! Records which are identical except for "ireason") may be written if
  !! multiple reporting conditions apply to the particle at a single time.
  !! Each "ireason" value corresponds to an OC "trackevent" option value:
  !!
  !!     0: particle released
  !!     1: particle transitioned between cells
  !!     2: current time step ended****
  !!     3: particle terminated
  !!     4: particle in weak sink
  !!     5: user-specified tracking time
  !!
  !! Each record has an "istatus" property, which is the tracking status;
  !! e.g., awaiting release, active, terminated. A particle may terminate
  !! for several reasons. Status values greater than one imply termination.
  !! Particle status strictly increases over time, starting at zero:
  !!
  !!     0: pending release (TODO is this necessary? will the user ever see it?)
  !!     1: active
  !!     2: terminated at boundary face
  !!     3: terminated in weak sink cell
  !!     4: (status code unused, see below)
  !!     5: terminated in cell with no exit face
  !!     6: terminated in cell with specified zone number
  !!     7: terminated in inactive cell
  !!     8: permanently unreleased
  !!     9: terminated in subcell with no exit face
  !!     10: terminated due to stop time or end of simulation
  !!
  !! Comparison to MODPATH 7
  !! -----------------------
  !!
  !! PRT istatus codes 0-3 and 5-8 correspond directly to MODPATH 7 status codes.
  !! Status code 4 does not apply to PRT because PRT does not distinguish forwards
  !! from backwards tracking. Status code 9 provides more specific, subcell-
  !! level information about a particle that terminated due to no exit face.
  !! Status code 10 distinguishes particles which have terminated due to timeout.
  !<
  type :: TrackFileType
    private
    integer(I4B), public :: iun = 0 !< file unit number
    logical(LGP), public :: csv = .false. !< whether the file is binary or CSV
    integer(I4B), public :: iprp = -1 !< -1 is model-level file, 0 is exchange PRP
  end type TrackFileType

  character(len=*), parameter, public :: TRACKHEADER = &
    'kper,kstp,imdl,iprp,irpt,ilay,icell,izone,&
    &istatus,ireason,trelease,t,x,y,z,name'

  character(len=*), parameter, public :: TRACKDTYPES = &
    '<i4,<i4,<i4,<i4,<i4,<i4,<i4,<i4,&
    &<i4,<i4,<f8,<f8,<f8,<f8,<f8,|S40'

contains

  !> @brief Save a particle track record to a binary or CSV file.
  subroutine save_record(iun, particle, kper, kstp, reason, csv)
    ! dummy
    integer(I4B), intent(in) :: iun
    type(ParticleType), pointer, intent(in) :: particle
    integer(I4B), intent(in) :: kper
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: reason
    logical(LGP), intent(in) :: csv
    ! local
    real(DP) :: x, y, z
    integer(I4B) :: status

    ! Convert from cell-local to model coordinates if needed
    call particle%get_model_coords(x, y, z)

    ! Set status
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
end module TrackFileModule

module ParticleModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DONE, LENMEMPATH, LENBOUNDNAME
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, &
                                 mem_reallocate
  implicit none

  private
  public :: ParticleType, ParticleStoreType, &
            create_particle, allocate_particle_store

  ! tracking levels (1: model, 2: cell, 3: subcell)
  integer, parameter, public :: levelmax = 4

  !> @brief A particle tracked by the PRT model.
  !!
  !! Record-type used mainly for convenience to shuffle
  !! data into and out of storage as tracking proceeds.
  !!
  !! Particle coordinates may be local to the cell or
  !! model coords. Routines are provided to convert a
  !! particle's model coordinates to/from cell-local
  !! coordinates for tracking through subdomains.
  !!
  !! Particles are identified by composite key, i.e.,
  !! combinations of properties imdl, iprp, irpt, and
  !! trelease. An optional label may be provided, but
  !! need not be unique
  !<
  type ParticleType
    private
    ! identity
    character(len=LENBOUNDNAME), public :: name = '' !< optional particle name
    integer(I4B), public :: imdl !< index of model the particle originated in
    integer(I4B), public :: iprp !< index of release package the particle is from
    integer(I4B), public :: irpt !< index of release point the particle is from
    integer(I4B), public :: ip !< index of particle in the particle list
    ! stop criteria
    integer(I4B), public :: istopweaksink !< weak sink option (0: do not stop, 1: stop)
    integer(I4B), public :: istopzone !< stop zone number
    ! state
    integer(I4B), allocatable, public :: idomain(:) !< tracking domain hierarchy
    integer(I4B), allocatable, public :: iboundary(:) !< tracking domain boundaries
    integer(I4B), public :: icu !< user cell (node) number
    integer(I4B), public :: ilay !< grid layer
    integer(I4B), public :: izone !< zone number
    integer(I4B), public :: istatus !< tracking status
    real(DP), public :: x !< x coordinate
    real(DP), public :: y !< y coordinate
    real(DP), public :: z !< z coordinate
    real(DP), public :: trelease !< release time
    real(DP), public :: tstop !< stop time
    real(DP), public :: ttrack !< time tracked so far
    real(DP), public :: xorigin !< x origin for coordinate transformation from model to local
    real(DP), public :: yorigin !< y origin for coordinate transformation from model to local
    real(DP), public :: zorigin !< z origin for coordinate transformation from model to local
    real(DP), public :: sinrot !< sine of rotation angle for coordinate transformation from model to local
    real(DP), public :: cosrot !< cosine of rotation angle for coordinate transformation from model to local
    real(DP), public :: extol !< tolerance for iterative solution of particle exit location and time in generalized Pollock's method
    logical(LGP), public :: transformed !< whether coordinates have been transformed from model to local
    logical(LGP), public :: advancing !< whether particle is still being tracked for current time step
    integer(I4B), public :: ifrctrn !< whether to force solving the particle with the ternary method
    integer(I4B), public :: iexmeth !< method for iterative solution of particle exit location and time in generalized Pollock's method
    integer(I4B), public :: iextend !< whether to extend tracking beyond the end of the simulation
    integer(I4B), public :: icoords !< coordinate system to use: 0 = model, 1: global
  contains
    procedure, public :: get_model_coords
    procedure, public :: load_particle
    procedure, public :: transform => transform_coords
  end type ParticleType

  !> @brief Structure of arrays to store particles.
  type ParticleStoreType
    private
    ! identity (composite key fields)
    character(len=LENBOUNDNAME), dimension(:), pointer, public, contiguous :: name !< optional particle label
    integer(I4B), dimension(:), pointer, public, contiguous :: imdl !< index of model particle originated in
    integer(I4B), dimension(:), pointer, public, contiguous :: iprp !< index of release package the particle originated in
    integer(I4B), dimension(:), pointer, public, contiguous :: irpt !< index of release point in the particle release package the particle originated in
    ! options
    integer(I4B), dimension(:), pointer, public, contiguous :: icoords !< coordinate system to use (0: global, 1: model)
    integer(I4B), dimension(:), pointer, public, contiguous :: iexmeth !< method for iterative solution of particle exit location and time in generalized Pollock's method
    integer(I4B), dimension(:), pointer, public, contiguous :: iextend !< whether to extend tracking beyond the end of the simulation
    integer(I4B), dimension(:), pointer, public, contiguous :: ifrctrn !< force ternary method
    integer(I4B), dimension(:), pointer, public, contiguous :: istopweaksink !< weak sink option: 0 = do not stop, 1 = stop
    integer(I4B), dimension(:), pointer, public, contiguous :: istopzone !< stop zone number
    real(DP), dimension(:), pointer, public, contiguous :: extol !< tolerance for iterative solution of particle exit location and time in generalized Pollock's method
    ! state
    integer(I4B), dimension(:, :), pointer, public, contiguous :: idomain !< domain hierarchy indices
    integer(I4B), dimension(:, :), pointer, public, contiguous :: iboundary !< domain boundary indices
    integer(I4B), dimension(:), pointer, public, contiguous :: icu !< cell number (user, not reduced)
    integer(I4B), dimension(:), pointer, public, contiguous :: ilay !< layer number
    integer(I4B), dimension(:), pointer, public, contiguous :: izone !< zone number
    integer(I4B), dimension(:), pointer, public, contiguous :: istatus !< tracking status
    real(DP), dimension(:), pointer, public, contiguous :: x !< x coordinate (model system)
    real(DP), dimension(:), pointer, public, contiguous :: y !< y coordinate (model system)
    real(DP), dimension(:), pointer, public, contiguous :: z !< z coordinate (model system)
    real(DP), dimension(:), pointer, public, contiguous :: trelease !< release time
    real(DP), dimension(:), pointer, public, contiguous :: tstop !< stop time
    real(DP), dimension(:), pointer, public, contiguous :: ttrack !< tracking time
  contains
    procedure, public :: deallocate
    procedure, public :: resize
    procedure, public :: save_particle
  end type ParticleStoreType

contains

  !> @brief Create a new particle
  subroutine create_particle(particle)
    type(ParticleType), pointer :: particle !< particle
    allocate (particle)
    allocate (particle%idomain(levelmax))
    allocate (particle%iboundary(levelmax))
  end subroutine create_particle

  !> @brief Create a new particle store
  subroutine allocate_particle_store(this, np, mempath)
    type(ParticleStoreType), pointer :: this !< store
    integer(I4B), intent(in) :: np !< number of particles
    character(*), intent(in) :: mempath !< path to memory

    allocate (this)
    call mem_allocate(this%imdl, np, 'PLIMDL', mempath)
    call mem_allocate(this%irpt, np, 'PLIRPT', mempath)
    call mem_allocate(this%iprp, np, 'PLIPRP', mempath)
    call mem_allocate(this%name, LENBOUNDNAME, np, 'PLNAME', mempath)
    call mem_allocate(this%icu, np, 'PLICU', mempath)
    call mem_allocate(this%ilay, np, 'PLILAY', mempath)
    call mem_allocate(this%izone, np, 'PLIZONE', mempath)
    call mem_allocate(this%istatus, np, 'PLISTATUS', mempath)
    call mem_allocate(this%x, np, 'PLX', mempath)
    call mem_allocate(this%y, np, 'PLY', mempath)
    call mem_allocate(this%z, np, 'PLZ', mempath)
    call mem_allocate(this%trelease, np, 'PLTRELEASE', mempath)
    call mem_allocate(this%tstop, np, 'PLTSTOP', mempath)
    call mem_allocate(this%ttrack, np, 'PLTTRACK', mempath)
    call mem_allocate(this%istopweaksink, np, 'PLISTOPWEAKSINK', mempath)
    call mem_allocate(this%istopzone, np, 'PLISTOPZONE', mempath)
    call mem_allocate(this%ifrctrn, np, 'PLIFRCTRN', mempath)
    call mem_allocate(this%iexmeth, np, 'PLIEXMETH', mempath)
    call mem_allocate(this%extol, np, 'PLEXTOL', mempath)
    call mem_allocate(this%iextend, np, 'PLIEXTEND', mempath)
    call mem_allocate(this%icoords, np, 'PLICOORDS', mempath)
    call mem_allocate(this%idomain, np, levelmax, 'PLIDOMAIN', mempath)
    call mem_allocate(this%iboundary, np, levelmax, 'PLIBOUNDARY', mempath)
  end subroutine allocate_particle_store

  !> @brief Deallocate particle arrays
  subroutine deallocate (this, mempath)
    class(ParticleStoreType), intent(inout) :: this !< store
    character(*), intent(in) :: mempath !< path to memory

    call mem_deallocate(this%imdl, 'PLIMDL', mempath)
    call mem_deallocate(this%iprp, 'PLIPRP', mempath)
    call mem_deallocate(this%irpt, 'PLIRPT', mempath)
    call mem_deallocate(this%name, 'PLNAME', mempath)
    call mem_deallocate(this%icu, 'PLICU', mempath)
    call mem_deallocate(this%ilay, 'PLILAY', mempath)
    call mem_deallocate(this%izone, 'PLIZONE', mempath)
    call mem_deallocate(this%istatus, 'PLISTATUS', mempath)
    call mem_deallocate(this%x, 'PLX', mempath)
    call mem_deallocate(this%y, 'PLY', mempath)
    call mem_deallocate(this%z, 'PLZ', mempath)
    call mem_deallocate(this%trelease, 'PLTRELEASE', mempath)
    call mem_deallocate(this%tstop, 'PLTSTOP', mempath)
    call mem_deallocate(this%ttrack, 'PLTTRACK', mempath)
    call mem_deallocate(this%istopweaksink, 'PLISTOPWEAKSINK', mempath)
    call mem_deallocate(this%istopzone, 'PLISTOPZONE', mempath)
    call mem_deallocate(this%ifrctrn, 'PLIFRCTRN', mempath)
    call mem_deallocate(this%iexmeth, 'PLIEXMETH', mempath)
    call mem_deallocate(this%extol, 'PLEXTOL', mempath)
    call mem_deallocate(this%iextend, 'PLIEXTEND', mempath)
    call mem_deallocate(this%icoords, 'PLICOORDS', mempath)
    call mem_deallocate(this%idomain, 'PLIDOMAIN', mempath)
    call mem_deallocate(this%iboundary, 'PLIBOUNDARY', mempath)
  end subroutine deallocate

  !> @brief Reallocate particle arrays
  subroutine resize(this, np, mempath)
    ! -- dummy
    class(ParticleStoreType), intent(inout) :: this !< particle store
    integer(I4B), intent(in) :: np !< number of particles
    character(*), intent(in) :: mempath !< path to memory

    ! resize arrays
    call mem_reallocate(this%imdl, np, 'PLIMDL', mempath)
    call mem_reallocate(this%iprp, np, 'PLIPRP', mempath)
    call mem_reallocate(this%irpt, np, 'PLIRPT', mempath)
    call mem_reallocate(this%name, LENBOUNDNAME, np, 'PLNAME', mempath)
    call mem_reallocate(this%icu, np, 'PLICU', mempath)
    call mem_reallocate(this%ilay, np, 'PLILAY', mempath)
    call mem_reallocate(this%izone, np, 'PLIZONE', mempath)
    call mem_reallocate(this%istatus, np, 'PLISTATUS', mempath)
    call mem_reallocate(this%x, np, 'PLX', mempath)
    call mem_reallocate(this%y, np, 'PLY', mempath)
    call mem_reallocate(this%z, np, 'PLZ', mempath)
    call mem_reallocate(this%trelease, np, 'PLTRELEASE', mempath)
    call mem_reallocate(this%tstop, np, 'PLTSTOP', mempath)
    call mem_reallocate(this%ttrack, np, 'PLTTRACK', mempath)
    call mem_reallocate(this%istopweaksink, np, 'PLISTOPWEAKSINK', mempath)
    call mem_reallocate(this%istopzone, np, 'PLISTOPZONE', mempath)
    call mem_reallocate(this%ifrctrn, np, 'PLIFRCTRN', mempath)
    call mem_reallocate(this%iexmeth, np, 'PLIEXMETH', mempath)
    call mem_reallocate(this%extol, np, 'PLEXTOL', mempath)
    call mem_reallocate(this%iextend, np, 'PLIEXTEND', mempath)
    call mem_reallocate(this%icoords, np, 'PLICOORDS', mempath)
    call mem_reallocate(this%idomain, np, levelmax, 'PLIDOMAIN', mempath)
    call mem_reallocate(this%iboundary, np, levelmax, 'PLIBOUNDARY', mempath)
  end subroutine resize

  !> @brief Load a particle from the particle store.
  !!
  !! This routine is used to initialize a particle for tracking.
  !! The advancing flag and coordinate transformation are reset.
  !<
  subroutine load_particle(this, store, imdl, iprp, ip)
    class(ParticleType), intent(inout) :: this !< particle
    type(ParticleStoreType), intent(in) :: store !< particle storage
    integer(I4B), intent(in) :: imdl !< index of model particle originated in
    integer(I4B), intent(in) :: iprp !< index of particle release package particle originated in
    integer(I4B), intent(in) :: ip !< index into the particle list

    call this%transform(reset=.true.)
    this%imdl = imdl
    this%iprp = iprp
    this%irpt = store%irpt(ip)
    this%ip = ip
    this%name = store%name(ip)
    this%istopweaksink = store%istopweaksink(ip)
    this%istopzone = store%istopzone(ip)
    this%icu = store%icu(ip)
    this%ilay = store%ilay(ip)
    this%izone = store%izone(ip)
    this%istatus = store%istatus(ip)
    this%x = store%x(ip)
    this%y = store%y(ip)
    this%z = store%z(ip)
    this%trelease = store%trelease(ip)
    this%tstop = store%tstop(ip)
    this%ttrack = store%ttrack(ip)
    this%advancing = .true.
    this%idomain(1:levelmax) = &
      store%idomain(ip, 1:levelmax)
    this%idomain(1) = imdl
    this%iboundary(1:levelmax) = &
      store%iboundary(ip, 1:levelmax)
    this%ifrctrn = store%ifrctrn(ip)
    this%iexmeth = store%iexmeth(ip)
    this%extol = store%extol(ip)
    this%iextend = store%iextend(ip)
    this%icoords = store%icoords(ip)
  end subroutine load_particle

  !> @brief Save a particle's state to the particle store
  subroutine save_particle(this, particle, ip)
    class(ParticleStoreType), intent(inout) :: this !< particle storage
    type(ParticleType), intent(in) :: particle !< particle
    integer(I4B), intent(in) :: ip !< particle index

    this%imdl(ip) = particle%imdl
    this%iprp(ip) = particle%iprp
    this%irpt(ip) = particle%irpt
    this%name(ip) = particle%name
    this%istopweaksink(ip) = particle%istopweaksink
    this%istopzone(ip) = particle%istopzone
    this%icu(ip) = particle%icu
    this%ilay(ip) = particle%ilay
    this%izone(ip) = particle%izone
    this%istatus(ip) = particle%istatus
    this%x(ip) = particle%x
    this%y(ip) = particle%y
    this%z(ip) = particle%z
    this%trelease(ip) = particle%trelease
    this%tstop(ip) = particle%tstop
    this%ttrack(ip) = particle%ttrack
    this%idomain( &
      ip, &
      1:levelmax) = &
      particle%idomain(1:levelmax)
    this%iboundary( &
      ip, &
      1:levelmax) = &
      particle%iboundary(1:levelmax)
    this%ifrctrn(ip) = particle%ifrctrn
    this%iexmeth(ip) = particle%iexmeth
    this%extol(ip) = particle%extol
    this%iextend(ip) = particle%iextend
    this%icoords(ip) = particle%icoords
  end subroutine save_particle

  !> @brief Apply the given global-to-local transformation to the particle.
  subroutine transform_coords(this, xorigin, yorigin, zorigin, &
                              sinrot, cosrot, invert, reset)
    use GeomUtilModule, only: transform, compose
    class(ParticleType), intent(inout) :: this !< particle
    real(DP), intent(in), optional :: xorigin !< x coordinate of origin
    real(DP), intent(in), optional :: yorigin !< y coordinate of origin
    real(DP), intent(in), optional :: zorigin !< z coordinate of origin
    real(DP), intent(in), optional :: sinrot !< sine of rotation angle
    real(DP), intent(in), optional :: cosrot !< cosine of rotation angle
    logical(LGP), intent(in), optional :: invert !< whether to invert
    logical(LGP), intent(in), optional :: reset !< whether to reset

    ! Reset if requested
    if (present(reset)) then
      if (reset) then
        this%xorigin = DZERO
        this%yorigin = DZERO
        this%zorigin = DZERO
        this%sinrot = DZERO
        this%cosrot = DONE
        this%transformed = .false.
        return
      end if
    end if

    ! Otherwise, transform coordinates
    call transform(this%x, this%y, this%z, &
                   this%x, this%y, this%z, &
                   xorigin, yorigin, zorigin, &
                   sinrot, cosrot, invert)

    ! Modify transformation from model coordinates to particle's new
    ! local coordinates by incorporating this latest transformation
    call compose(this%xorigin, this%yorigin, this%zorigin, &
                 this%sinrot, this%cosrot, &
                 xorigin, yorigin, zorigin, &
                 sinrot, cosrot, invert)

    ! Set isTransformed flag to true. Note that there is no check
    ! to see whether the modification brings the coordinates back
    ! to model coordinates (in which case the origin would be very
    ! close to zero and sinrot and cosrot would be very close to 0.
    ! and 1., respectively, allowing for roundoff error).
    this%transformed = .true.
  end subroutine transform_coords

  !> @brief Return the particle's model coordinates.
  subroutine get_model_coords(this, x, y, z)
    use GeomUtilModule, only: transform, compose
    class(ParticleType), intent(inout) :: this !< particle
    real(DP), intent(out) :: x !< x coordinate
    real(DP), intent(out) :: y !< y coordinate
    real(DP), intent(out) :: z !< z coordinate

    if (this%transformed) then
      ! Transform back from local to model coordinates
      call transform(this%x, this%y, this%z, x, y, z, &
                     this%xorigin, this%yorigin, this%zorigin, &
                     this%sinrot, this%cosrot, .true.)
    else
      ! Already in model coordinates
      x = this%x
      y = this%y
      z = this%z
    end if
  end subroutine get_model_coords

end module ParticleModule

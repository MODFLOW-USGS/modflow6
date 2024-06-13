module MethodSubcellTernaryModule
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DSAME, DHALF, DONE, DTWO, DONETHIRD
  use ErrorUtilModule, only: pstop
  use GeomUtilModule, only: clamp_bary, skew
  use MethodModule, only: MethodType
  use CellModule, only: CellType
  use SubcellModule, only: SubcellType
  use SubcellTriModule, only: SubcellTriType, create_subcell_tri
  use ParticleModule, only: ParticleType
  use TrackModule, only: TrackFileControlType
  use TernarySolveTrack, only: traverse_triangle, step_analytical, canonical
  use PrtFmiModule, only: PrtFmiType
  use BaseDisModule, only: DisBaseType
  implicit none

  private
  public :: MethodSubcellTernaryType
  public :: create_method_subcell_ternary

  !> @brief Ternary triangular subcell tracking method
  type, extends(MethodType) :: MethodSubcellTernaryType
    integer(I4B), public, pointer :: zeromethod
  contains
    procedure, public :: apply => apply_mst
    procedure, public :: deallocate
    procedure, private :: track_subcell
  end type MethodSubcellTernaryType

contains

  !> @brief Create a new ternary subcell method
  subroutine create_method_subcell_ternary(method)
    ! -- dummy
    type(MethodSubcellTernaryType), pointer :: method
    ! -- local
    type(SubcellTriType), pointer :: subcell

    allocate (method)
    call create_subcell_tri(subcell)
    method%subcell => subcell
    method%type => method%subcell%type
    method%delegates = .false.
  end subroutine create_method_subcell_ternary

  !> @brief Deallocate the ternary subcell method
  subroutine deallocate (this)
    class(MethodSubcellTernaryType), intent(inout) :: this
    deallocate (this%type)
  end subroutine deallocate

  !> @brief Apply the ternary subcell method
  subroutine apply_mst(this, particle, tmax)
    class(MethodSubcellTernaryType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    real(DP), intent(in) :: tmax

    select type (subcell => this%subcell)
    type is (SubcellTriType)
      call this%track_subcell(subcell, particle, tmax)
    end select
  end subroutine apply_mst

  !> @brief Track a particle across a triangular subcell using the ternary method
  subroutine track_subcell(this, subcell, particle, tmax)
    ! dummy
    class(MethodSubcellTernaryType), intent(inout) :: this
    class(SubcellTriType), intent(in) :: subcell
    type(ParticleType), pointer, intent(inout) :: particle
    real(DP), intent(in) :: tmax
    ! local
    integer(I4B) :: exitFace
    real(DP) :: x0
    real(DP) :: y0
    real(DP) :: x1
    real(DP) :: y1
    real(DP) :: x2
    real(DP) :: y2
    real(DP) :: v0x
    real(DP) :: v0y
    real(DP) :: v1x
    real(DP) :: v1y
    real(DP) :: v2x
    real(DP) :: v2y
    real(DP) :: xi
    real(DP) :: yi
    real(DP) :: zi
    real(DP) :: zirel
    real(DP) :: ztop
    real(DP) :: zbot
    real(DP) :: dz
    real(DP) :: rxx
    real(DP) :: rxy
    real(DP) :: ryx
    real(DP) :: ryy
    real(DP) :: sxx
    real(DP) :: sxy
    real(DP) :: syy
    real(DP) :: alp0
    real(DP) :: bet0
    real(DP) :: alp1
    real(DP) :: bet1
    real(DP) :: alp2
    real(DP) :: bet2
    real(DP) :: alpi
    real(DP) :: beti
    real(DP) :: gami
    real(DP) :: vzbot
    real(DP) :: vztop
    real(DP) :: vzi
    real(DP) :: vziodz
    real(DP) :: az
    real(DP) :: dtexitz
    real(DP) :: dt
    real(DP) :: t
    real(DP) :: t0
    real(DP) :: dtexitxy
    real(DP) :: texit
    real(DP) :: x
    real(DP) :: y
    real(DP) :: z
    integer(I4B) :: izstatus
    integer(I4B) :: itopbotexit
    integer(I4B) :: ntmax
    integer(I4B) :: isolv
    integer(I4B) :: itrifaceenter
    integer(I4B) :: itrifaceexit
    real(DP) :: tol
    real(DP) :: dtexit
    real(DP) :: alpexit
    real(DP) :: betexit
    integer(I4B) :: reason
    integer(I4B) :: i
    integer(I4B) :: tslice(2)

    ntmax = 10000
    isolv = particle%iexmethod
    tol = particle%extol
    reason = -1

    ! -- Set some local variables for convenience
    xi = particle%x
    yi = particle%y
    zi = particle%z
    x0 = subcell%x0
    y0 = subcell%y0
    x1 = subcell%x1
    y1 = subcell%y1
    x2 = subcell%x2
    y2 = subcell%y2
    v0x = subcell%v0x
    v0y = subcell%v0y
    v1x = subcell%v1x
    v1y = subcell%v1y
    v2x = subcell%v2x
    v2y = subcell%v2y
    zbot = subcell%zbot
    ztop = subcell%ztop
    dz = subcell%dz
    vzbot = subcell%vzbot
    vztop = subcell%vztop

    ! -- Transform coordinates to "canonical" configuration
    call canonical(x0, y0, x1, y1, x2, y2, &
                   v0x, v0y, v1x, v1y, v2x, v2y, &
                   xi, yi, &
                   rxx, rxy, ryx, ryy, &
                   sxx, sxy, syy, &
                   alp0, bet0, alp1, bet1, alp2, bet2, alpi, beti)

    ! -- Nudge particle if necessary so it begins within the subcell
    call clamp_bary(alpi, beti, gami)

    ! -- Do calculations related to analytical z solution, could possibly
    !    be done just once for each cell, todo: profile? potentially store
    !    on cell-level method? after initial release
    zirel = (zi - zbot) / dz
    call calculate_dt(vzbot, vztop, dz, zirel, vzi, &
                      az, dtexitz, izstatus, &
                      itopbotexit)
    vziodz = vzi / dz

    ! -- Traverse triangular subcell
    itrifaceenter = particle%iboundary(3) - 1
    if (itrifaceenter .eq. -1) itrifaceenter = 999
    call traverse_triangle(isolv, tol, &
                           dtexitxy, alpexit, betexit, &
                           itrifaceenter, itrifaceexit, &
                           alp1, bet1, alp2, bet2, alpi, beti)

    ! -- Subcell has no exit face, terminate the particle
    !    todo: after initial release, consider ramifications
    if ((itopbotexit .eq. 0) .and. (itrifaceexit .eq. 0)) then
      particle%istatus = 9
      call this%save(particle, reason=3)
      return
    end if

    ! -- Determine (earliest) exit face and corresponding travel time to exit
    if (itopbotexit .eq. 0) then
      ! -- Exits through triangle face first
      exitFace = itrifaceexit
      dtexit = dtexitxy
    else if (itrifaceexit .eq. 0) then
      ! -- Exits through top/bottom first
      exitFace = 45
      dtexit = dtexitz
    else if (dtexitz .lt. dtexitxy) then
      ! -- Exits through top/bottom first
      exitFace = 45
      dtexit = dtexitz
    else
      ! -- Exits through triangle face first
      exitFace = itrifaceexit
      dtexit = dtexitxy
    end if
    if (exitFace .eq. 45) then
      if (itopbotexit .eq. -1) then
        exitFace = 4
      else
        exitFace = 5
      end if
    end if

    ! -- Compute exit time, irrespective of tmax for now
    texit = particle%ttrack + dtexit
    t0 = particle%ttrack

    ! -- Select user tracking times to solve. If this is the last time step, include
    !    all times after it ends (as per MODPATH 7's extension behavior). Otherwise
    !    take the times within the current period and time step only.
    call this%tracktimes%try_advance()
    tslice = this%tracktimes%selection
    if (all(tslice > 0)) then
      do i = tslice(1), tslice(2)
        t = this%tracktimes%times(i)
        if (t < particle%ttrack .or. t >= texit .or. t >= tmax) cycle
        dt = t - t0
        call calculate_xyz_position(dt, rxx, rxy, ryx, ryy, sxx, sxy, syy, &
                                    izstatus, x0, y0, az, vzi, vzbot, &
                                    ztop, zbot, zi, x, y, z)
        particle%x = x
        particle%y = y
        particle%z = z
        particle%ttrack = t
        particle%istatus = 1
        call this%save(particle, reason=5)
      end do
    end if

    ! Compute final time, taking into account tmax, and set final
    ! particle status
    if (texit .gt. tmax) then
      ! -- The computed exit time is greater than the maximum time, so set
      ! -- final time for particle trajectory equal to maximum time.
      t = tmax
      dt = t - t0
      exitFace = 0
      particle%istatus = 1
      particle%advancing = .false.
      reason = 2 ! timestep end
    else
      ! -- The computed exit time is less than or equal to the maximum time,
      ! -- so set final time for particle trajectory equal to exit time.
      t = texit
      dt = dtexit
      reason = 1 ! cell transition
    end if

    call calculate_xyz_position(dt, rxx, rxy, ryx, ryy, sxx, sxy, syy, &
                                izstatus, x0, y0, az, vzi, vzbot, &
                                ztop, zbot, zi, x, y, z, exitface)

    ! -- Set final particle location in local (unscaled) subcell coordinates,
    ! -- final time for particle trajectory, and exit face
    particle%x = x
    particle%y = y
    particle%z = z
    particle%ttrack = t
    particle%iboundary(3) = exitFace

    ! -- Save particle track record
    call this%save(particle, reason=reason)
  end subroutine track_subcell

  !> @brief Do calculations related to analytical z solution
  !!
  !! This subroutine consists partly or entirely of code written by
  !! David W. Pollock of the USGS for MODPATH 7. The authors of the present
  !! code are responsible for its appropriate application in this context
  !! and for any modifications or errors.
  !<
  subroutine calculate_dt(v1, v2, dx, xL, v, dvdx, &
                          dt, status, itopbotexit)
    real(DP) :: v1
    real(DP) :: v2
    real(DP) :: dx
    real(DP) :: xL
    real(DP) :: v
    real(DP) :: dvdx
    real(DP) :: dt
    real(DP) :: v2a
    real(DP) :: v1a
    real(DP) :: dv
    real(DP) :: dva
    real(DP) :: vv
    real(DP) :: vvv
    real(DP) :: zro
    real(DP) :: zrom
    real(DP) :: x
    real(DP) :: tol
    real(DP) :: vr1
    real(DP) :: vr2
    real(DP) :: vr
    real(DP) :: v1v2
    integer(I4B) :: status
    integer(I4B) :: itopbotexit
    logical(LGP) :: noOutflow

    ! Initialize variables
    status = -1
    dt = 1.0d+20
    v2a = v2
    if (v2a .lt. DZERO) v2a = -v2a
    v1a = v1
    if (v1a .lt. DZERO) v1a = -v1a
    dv = v2 - v1
    dva = dv
    if (dva .lt. DZERO) dva = -dva

    ! Check for a uniform zero velocity in this direction.
    ! If so, set status = 2 and return (dt = 1.0d+20).
    tol = 1.0d-15
    if ((v2a .lt. tol) .and. (v1a .lt. tol)) then
      v = DZERO
      dvdx = DZERO
      status = 2
      itopbotexit = 0
      return
    end if

    ! Check for uniform non-zero velocity in this direction.
    ! If so, set compute dt using the constant velocity,
    ! set status = 1 and return.
    vv = v1a
    if (v2a .gt. vv) vv = v2a
    vvv = dva / vv
    if (vvv .lt. 1.0d-4) then
      zro = tol
      zrom = -zro
      v = v1
      x = xL * dx
      if (v1 .gt. zro) then
        dt = (dx - x) / v1
        itopbotexit = -2
      end if
      if (v1 .lt. zrom) then
        dt = -x / v1
        itopbotexit = -1
      end if
      dvdx = DZERO
      status = 1
      return
    end if

    ! Velocity has a linear variation.
    ! Compute velocity corresponding to particle position
    dvdx = dv / dx
    v = (DONE - xL) * v1 + xL * v2

    ! If flow is into the cell from both sides there is no outflow.
    ! In that case, set status = 3 and return
    noOutflow = .true.
    if (v1 .lt. DZERO) noOutflow = .false.
    if (v2 .gt. DZERO) noOutflow = .false.
    if (noOutflow) then
      status = 3
      itopbotexit = 0
      return
    end if

    ! If there is a divide in the cell for this flow direction, check to see if the
    ! particle is located exactly on the divide. If it is, move it very slightly to
    ! get it off the divide. This avoids possible numerical problems related to
    ! stagnation points.
    if ((v1 .le. DZERO) .and. (v2 .ge. DZERO)) then
      if (abs(v) .le. DZERO) then
        v = 1.0d-20
        if (v2 .le. DZERO) v = -v
      end if
    end if

    ! If there is a flow divide, find out what side of the divide the particle
    ! is on and set the value of vr appropriately to reflect that location.
    vr1 = v1 / v
    vr2 = v2 / v
    vr = vr1
    itopbotexit = -1
    if (vr .le. DZERO) then
      vr = vr2
      itopbotexit = -2
    end if

    ! Check if velocity is in the same direction throughout cell (i.e. no flow divide).
    ! Check if product v1*v2 > 0 then the velocity is in the same direction throughout
    ! the cell (i.e. no flow divide). If so, set vr to reflect appropriate direction.
    v1v2 = v1 * v2
    if (v1v2 .gt. DZERO) then
      if (v .gt. DZERO) then
        vr = vr2
        itopbotexit = -2
      end if
      if (v .lt. DZERO) then
        vr = vr1
        itopbotexit = -1
      end if
    end if

    ! Compute travel time to exit face. Return with status = 0
    dt = log(vr) / dvdx
    status = 0
  end subroutine calculate_dt

  !> @brief Calculate the particle's local unscaled xyz coordinates after dt.
  subroutine calculate_xyz_position(dt, rxx, rxy, ryx, ryy, sxx, sxy, syy, &
                                    izstatus, x0, y0, az, vzi, vzbot, &
                                    ztop, zbot, zi, x, y, z, exitFace)
    ! dummy
    real(DP) :: dt
    real(DP) :: rxx
    real(DP) :: rxy
    real(DP) :: ryx
    real(DP) :: ryy
    real(DP) :: sxx
    real(DP) :: sxy
    real(DP) :: syy
    integer(I4B) :: izstatus
    real(DP) :: x0
    real(DP) :: y0
    real(DP) :: az
    real(DP) :: vzi
    real(DP) :: vzbot
    real(DP) :: ztop
    real(DP) :: zbot
    real(DP) :: zi
    real(DP) :: x
    real(DP) :: y
    real(DP) :: z
    integer(I4B), optional :: exitFace
    ! local
    integer(I4B) :: lexitface
    real(DP) :: rot(2, 2), res(2), loc(2)
    real(DP) :: alp
    real(DP) :: bet

    ! process optional exit face argument
    if (present(exitface)) then
      lexitface = exitface
    else
      lexitface = 0
    end if

    ! calculate alpha and beta
    call step_analytical(dt, alp, bet)

    ! if exit face is known, set alpha or beta coordinate
    ! corresponding to the exit face exactly.
    if (lexitFace .eq. 1) then
      bet = DZERO
    else if (lexitFace .eq. 2) then
      alp = DONE - bet
    else if (lexitFace .eq. 3) then
      alp = DZERO
    end if

    ! if exit face is top or bottom, set z coordinate exactly.
    if (lexitFace .eq. 4) then
      z = zbot
    else if (lexitFace .eq. 5) then
      z = ztop
    else
      ! otherwise calculate z.
      if (izstatus .eq. 2) then
        ! -- vz uniformly zero
        z = zi
      else if (izstatus .eq. 1) then
        ! -- vz uniform, nonzero
        z = zi + vzi * dt
      else
        ! -- vz nonuniform
        z = zbot + (vzi * dexp(az * dt) - vzbot) / az
      end if
    end if

    ! transform (alp, beta) to (x, y)
    loc = (/alp, bet/)
    loc = skew(loc, (/sxx, sxy, syy/), invert=.true.)
    rot = reshape((/rxx, rxy, ryx, ryy/), shape(rot))
    res = matmul(rot, loc) ! rotate vector
    x = res(1) + x0
    y = res(2) + y0

  end subroutine calculate_xyz_position

end module MethodSubcellTernaryModule

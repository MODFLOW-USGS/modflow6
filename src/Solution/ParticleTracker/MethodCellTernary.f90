module MethodCellTernaryModule

  use KindModule, only: DP, I4B
  use ErrorUtilModule, only: pstop
  use MethodModule
  use MethodSubcellPoolModule
  use CellPolyModule
  use CellDefnModule
  use SubcellTriModule, only: SubcellTriType, create_subcell_tri
  use ParticleModule
  use TrackModule, only: TrackFileControlType
  implicit none

  private
  public :: MethodCellTernaryType
  public :: create_method_cell_ternary

  type, extends(MethodType) :: MethodCellTernaryType
    private
    real(DP), allocatable :: x_vert(:)
    real(DP), allocatable :: y_vert(:) !< cell vertex coordinates
    real(DP), allocatable :: vx_vert_polygon(:)
    real(DP), allocatable :: vy_vert_polygon(:) !< cell vertex velocities
    real(DP) :: xctr
    real(DP) :: yctr !< cell center coordinates
    real(DP) :: vxctr
    real(DP) :: vyctr !< cell center velocities
    real(DP) :: ztop
    real(DP) :: zbot !< cell top and bottom elevations
    real(DP) :: dz !< cell thickness
    real(DP) :: vztop
    real(DP) :: vzbot !< cell top and bottom velocities
    integer(I4B), public, pointer :: zeromethod
  contains
    procedure, public :: apply => apply_mct
    procedure, public :: destroy => destroy_mct
    procedure, public :: load => load_mct
    procedure, public :: load_subcell
    procedure, public :: pass => pass_mct
  end type MethodCellTernaryType

contains

  !> @brief Create a tracking method
  subroutine create_method_cell_ternary(method)
    ! -- dummy
    type(MethodCellTernaryType), pointer :: method
    ! -- local
    type(CellPolyType), pointer :: cell
    type(SubcellTriType), pointer :: subcell

    allocate (method)
    allocate (method%zeromethod)
    call create_cell_poly(cell)
    method%cell => cell
    method%type => method%cell%type
    method%delegates = .true.
    call create_subcell_tri(subcell)
    method%subcell => subcell
    method%zeromethod = 0
  end subroutine create_method_cell_ternary

  !> @brief Destroy the tracking method
  subroutine destroy_mct(this)
    class(MethodCellTernaryType), intent(inout) :: this
    deallocate (this%type)
  end subroutine destroy_mct

  !> @brief Load subcell into tracking method
  subroutine load_mct(this, particle, next_level, submethod)
    ! -- dummy
    class(MethodCellTernaryType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer(I4B), intent(in) :: next_level
    class(MethodType), pointer, intent(inout) :: submethod

    select type (subcell => this%subcell)
    type is (SubcellTriType)
      call this%load_subcell(particle, subcell)
    end select
    call method_subcell_tern%init( &
      cell=this%cell, &
      subcell=this%subcell, &
      trackfilectl=this%trackfilectl, &
      tracktimes=this%tracktimes)
    submethod => method_subcell_tern
    method_subcell_tern%zeromethod = this%zeromethod
  end subroutine load_mct

  !> @brief Pass particle to next subcell if there is one, or to the cell face
  subroutine pass_mct(this, particle)
    ! -- dummy
    class(MethodCellTernaryType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    ! local
    integer(I4B) :: isc
    integer(I4B) :: exitFace
    integer(I4B) :: inface
    integer(I4B) :: npolyverts

    exitFace = particle%iboundary(3)
    isc = particle%idomain(3)
    select type (cell => this%cell)
    type is (CellPolyType)
      npolyverts = cell%defn%npolyverts
    end select

    select case (exitFace)
    case (0)
      ! -- Subcell interior (cell interior)
      inface = -1
    case (1)
      ! -- Subcell face 1 (cell face)
      inface = isc
      if (inface .eq. 0) inface = npolyverts
    case (2)
      ! -- Subcell face --> next subcell in "cycle" (cell interior)
      isc = isc + 1
      if (isc .gt. npolyverts) isc = 1
      particle%idomain(3) = isc
      particle%iboundary(3) = 3
      inface = 0
    case (3)
      ! -- Subcell face --> preceding subcell in "cycle" (cell interior)
      isc = isc - 1
      if (isc .lt. 1) isc = npolyverts
      particle%idomain(3) = isc
      particle%iboundary(3) = 2
      inface = 0
    case (4)
      ! -- Subcell bottom (cell bottom)
      inface = npolyverts + 2
    case (5)
      ! -- Subcell top (cell top)
      inface = npolyverts + 3
    end select
    if (inface .eq. -1) then
      particle%iboundary(2) = 0
    else if (inface .eq. 0) then
      particle%iboundary(2) = 0
    else
      particle%iboundary(2) = inface
    end if
  end subroutine pass_mct

  !> @brief Apply the ternary method to a polygonal cell
  subroutine apply_mct(this, particle, tmax)
    use ConstantsModule, only: DZERO, DONE, DHALF
    use TdisModule, only: kper, kstp
    ! dummy
    class(MethodCellTernaryType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    real(DP), intent(in) :: tmax
    ! local
    integer(I4B) :: npolyverts
    integer(I4B) :: iv
    integer(I4B) :: ivp1
    integer(I4B) :: ivm1
    real(DP) :: retfactor
    real(DP) :: x0
    real(DP) :: y0
    real(DP) :: x1
    real(DP) :: y1
    real(DP) :: x2
    real(DP) :: y2
    real(DP) :: xsum
    real(DP) :: ysum
    real(DP) :: vxsum
    real(DP) :: vysum
    real(DP) :: flow0
    real(DP) :: flow1
    real(DP) :: v0x
    real(DP) :: v0y
    real(DP) :: d01x
    real(DP) :: d01y
    real(DP) :: d02x
    real(DP) :: d02y
    real(DP) :: det
    real(DP) :: area
    real(DP) :: term

    select type (cell => this%cell)
    type is (CellPolyType)
      ! -- Update particle state, checking whether any reporting or
      ! -- termination conditions apply
      call this%update(particle, cell%defn)

      ! -- Return early if particle is done advancing
      if (.not. particle%advancing) return

      ! -- If the particle is above the top of the cell (presumed water table)
      ! -- pass it vertically and instantaneously to the cell top and save the
      ! -- particle state to file
      if (particle%z > cell%defn%top) then
        particle%z = cell%defn%top
        call this%trackfilectl%save(particle, kper=kper, &
                                    kstp=kstp, reason=1) ! reason=1: cell transition
      end if

      npolyverts = cell%defn%npolyverts
      if (allocated(this%x_vert)) then
        deallocate (this%x_vert)
        deallocate (this%y_vert)
        deallocate (this%vx_vert_polygon)
        deallocate (this%vy_vert_polygon)
      end if
      allocate (this%x_vert(npolyverts))
      allocate (this%y_vert(npolyverts))
      allocate (this%vx_vert_polygon(npolyverts))
      allocate (this%vy_vert_polygon(npolyverts))

      xsum = DZERO
      ysum = DZERO
      vxsum = DZERO
      vysum = DZERO
      area = DZERO
      this%ztop = cell%defn%top
      this%zbot = cell%defn%bot
      this%dz = this%ztop - this%zbot
      do iv = 1, npolyverts
        ivp1 = iv + 1
        if (ivp1 .gt. npolyverts) ivp1 = 1
        ivm1 = iv - 1
        if (ivm1 .lt. 1) ivm1 = npolyverts
        x0 = cell%defn%polyvert(1, iv)
        y0 = cell%defn%polyvert(2, iv)
        x2 = cell%defn%polyvert(1, ivp1)
        y2 = cell%defn%polyvert(2, ivp1)
        x1 = cell%defn%polyvert(1, ivm1)
        y1 = cell%defn%polyvert(2, ivm1)
        term = DONE / (cell%defn%porosity * this%dz)
        flow0 = cell%defn%faceflow(iv) * term
        flow1 = cell%defn%faceflow(ivm1) * term
        d01x = x1 - x0 ! kluge note: do this more efficiently, not recomputing things so much???
        d01y = y1 - y0
        d02x = x2 - x0
        d02y = y2 - y0
        ! kluge note: can det ever be zero, like maybe for a 180-deg vertex???
        ! oodet = DONE/(d01y*d02x - d02y*d01x)
        ! velmult = particle%velmult
        ! kluge note: "flow" is volumetric (face) flow rate per unit thickness, divided by porosity
        ! v0x = -velmult*oodet*(d02x*flow1 + d01x*flow0)
        ! v0y = -velmult*oodet*(d02y*flow1 + d01y*flow0)   !
        det = d01y * d02x - d02y * d01x
        retfactor = cell%defn%retfactor
        ! kluge note: can det ever be zero, like maybe for a 180-deg vertex???
        ! term = velfactor/det
        ! kluge note: can det ever be zero, like maybe for a 180-deg vertex???
        term = DONE / (retfactor * det)
        ! kluge note: "flow" here is volumetric flow rate (MODFLOW face flow)
        v0x = -term * (d02x * flow1 + d01x * flow0)
        ! per unit thickness, divided by porosity
        v0y = -term * (d02y * flow1 + d01y * flow0)
        this%vx_vert_polygon(iv) = v0x
        this%vy_vert_polygon(iv) = v0y
        xsum = xsum + x0
        ysum = ysum + y0
        vxsum = vxsum + v0x
        vysum = vysum + v0y
        this%x_vert(iv) = x0
        this%y_vert(iv) = y0
        area = area + x0 * y1 - x1 * y0
      end do
      area = area * DHALF
      term = DONE / (retfactor * cell%defn%porosity * area)
      this%vzbot = cell%defn%faceflow(npolyverts + 2) * term
      this%vztop = -cell%defn%faceflow(npolyverts + 3) * term
      this%xctr = xsum / dble(npolyverts)
      this%yctr = ysum / dble(npolyverts)
      this%vxctr = vxsum / dble(npolyverts)
      this%vyctr = vysum / dble(npolyverts)

      ! -- Track across subcells
      call this%track(particle, 2, tmax) ! kluge, hardwired to level 2
    end select
  end subroutine apply_mct

  !> @brief Loads a triangular subcell from the polygonal cell
  subroutine load_subcell(this, particle, subcell)
    ! -- modules
    use ParticleModule, only: get_particle_id
    ! -- dummy
    class(MethodCellTernaryType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    class(SubcellTriType), intent(inout) :: subcell
    ! -- local
    integer(I4B) :: ic
    integer(I4B) :: isc
    integer(I4B) :: npolyverts
    integer(I4B) :: iv0
    integer(I4B) :: iv1
    integer(I4B) :: ipv0
    integer(I4B) :: ipv1
    integer(I4B) :: iv
    real(DP) :: x0
    real(DP) :: y0
    real(DP) :: x1
    real(DP) :: y1
    real(DP) :: x2
    real(DP) :: y2
    real(DP) :: x1rel
    real(DP) :: y1rel
    real(DP) :: x2rel
    real(DP) :: y2rel
    real(DP) :: xi
    real(DP) :: yi
    real(DP) :: di2
    real(DP) :: d02
    real(DP) :: d12
    real(DP) :: di1
    real(DP) :: d01
    real(DP) :: alphai
    real(DP) :: betai
    real(DP) :: betatol

    select type (cell => this%cell)
    type is (CellPolyType)
      ic = cell%defn%icell
      subcell%icell = ic
      isc = particle%idomain(3)
      npolyverts = cell%defn%npolyverts
      if (isc .le. 0) then
        xi = particle%x
        yi = particle%y
        do iv = 1, npolyverts
          iv0 = iv
          iv1 = iv + 1
          if (iv1 .gt. npolyverts) iv1 = 1
          ipv0 = iv0
          ipv1 = iv1
          x0 = this%x_vert(ipv0)
          y0 = this%y_vert(ipv0)
          x1 = this%x_vert(ipv1)
          y1 = this%y_vert(ipv1)
          x2 = this%xctr
          y2 = this%yctr
          x1rel = x1 - x0
          y1rel = y1 - y0
          x2rel = x2 - x0
          y2rel = y2 - y0
          di2 = xi * y2rel - yi * x2rel
          d02 = x0 * y2rel - y0 * x2rel
          d12 = x1rel * y2rel - y1rel * x2rel
          di1 = xi * y1rel - yi * x1rel
          d01 = x0 * y1rel - y0 * x1rel
          alphai = (di2 - d02) / d12
          betai = -(di1 - d01) / d12
          ! kluge note: can iboundary(2) be used to identify the subcell?
          betatol = -1e-7 ! kluge
          ! kluge note: think this handles points on triangle boundaries ok
          if ((alphai .ge. 0d0) .and. &
              (betai .ge. betatol) .and. &
              (alphai + betai .le. 1d0)) then
            isc = iv ! but maybe not!!!!!!!!!!!!
            exit ! kluge note: doesn't handle particle smack on cell center
          end if
        end do
        if (isc .le. 0) then
          print *, "error -- initial triangle not found for particle ", &
            get_particle_id(particle), " in cell ", ic
          call pstop(1)
        else
          ! subcellTri%isubcell = isc
          ! kluge note: as a matter of form, do we want to allow
          ! this subroutine to modify the particle???
          particle%idomain(3) = isc
        end if
      end if
      subcell%isubcell = isc

      ! -- Set coordinates and velocities at vertices of triangular subcell
      iv0 = isc
      iv1 = isc + 1
      if (iv1 .gt. npolyverts) iv1 = 1
      ipv0 = iv0
      ipv1 = iv1
      subcell%x0 = this%x_vert(ipv0)
      subcell%y0 = this%y_vert(ipv0)
      subcell%x1 = this%x_vert(ipv1)
      subcell%y1 = this%y_vert(ipv1)
      subcell%x2 = this%xctr
      subcell%y2 = this%yctr
      subcell%v0x = this%vx_vert_polygon(iv0)
      subcell%v0y = this%vy_vert_polygon(iv0)
      subcell%v1x = this%vx_vert_polygon(iv1)
      subcell%v1y = this%vy_vert_polygon(iv1)
      subcell%v2x = this%vxctr
      subcell%v2y = this%vyctr
      subcell%ztop = this%ztop
      subcell%zbot = this%zbot
      subcell%dz = this%dz
      subcell%vzbot = this%vzbot
      subcell%vztop = this%vztop
    end select
  end subroutine load_subcell

end module MethodCellTernaryModule

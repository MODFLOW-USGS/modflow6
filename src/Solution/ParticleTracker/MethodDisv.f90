module MethodDisvModule

  use KindModule, only: DP, I4B, LGP
  use ErrorUtilModule, only: pstop
  use ConstantsModule, only: DONE
  use MethodModule, only: MethodType, get_iatop
  use MethodCellPoolModule
  use CellDefnModule
  use CellPolyModule
  use ParticleModule
  use PrtFmiModule, only: PrtFmiType
  use GwfDisvModule, only: GwfDisvType
  use ArrayHandlersModule, only: ExpandArray
  use TrackModule, only: TrackFileControlType
  use GeomUtilModule, only: get_jk
  implicit none

  private
  public :: MethodDisvType
  public :: create_method_disv

  type, extends(MethodType) :: MethodDisvType
    integer(I4B), pointer :: zeromethod
  contains
    procedure, public :: apply => apply_disv ! applies the DISV-grid method
    procedure, public :: destroy ! destructor for the method
    procedure, public :: load => load_disv ! loads the cell method
    procedure, public :: load_cell_defn ! loads cell definition from the grid
    procedure, public :: map_neighbor ! maps a location on the cell face to the shared face of a neighbor
    procedure, public :: pass => pass_disv ! passes the particle to the next cell
    procedure, private :: get_npolyverts ! returns the number of polygon vertices for a cell in the grid
    procedure, private :: get_top ! returns top elevation based on index iatop
    procedure, private :: load_nbrs_to_defn ! loads face neighbors to a cell object
    procedure, private :: load_flags_to_defn ! loads 180-degree vertex indicator to a cell object
    procedure, private :: load_flows_to_defn ! loads flows to a cell object
    procedure, private :: load_boundary_flows_to_defn_rect ! adds BoundaryFlows from the grid to the faceflow array of a rectangular cell
    procedure, private :: load_boundary_flows_to_defn_rect_quad ! adds BoundaryFlows from the grid to the faceflow array of a rectangular-quad cell
    procedure, private :: load_boundary_flows_to_defn_poly ! adds BoundaryFlows from the grid to the faceflow array of a polygonal cell
  end type MethodDisvType

contains

  !> @brief Create a new vertex grid (DISV) tracking method
  subroutine create_method_disv(method)
    ! -- dummy
    type(MethodDisvType), pointer :: method
    ! -- local
    type(CellPolyType), pointer :: cell

    allocate (method)
    allocate (method%type)
    allocate (method%zeromethod)
    call create_cell_poly(cell)
    method%cell => cell
    method%type = "disv"
    method%delegates = .true.
    method%zeromethod = 0
  end subroutine create_method_disv

  !> @brief Destroy the tracking method
  subroutine destroy(this)
    class(MethodDisvType), intent(inout) :: this
    deallocate (this%type)
  end subroutine destroy

  !> @brief Load the cell and the tracking method
  subroutine load_disv(this, particle, next_level, submethod)
    use CellModule
    use CellRectModule
    use CellRectQuadModule
    use CellUtilModule
    ! -- dummy
    class(MethodDisvType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer(I4B), intent(in) :: next_level
    class(MethodType), pointer, intent(inout) :: submethod
    ! -- local
    integer(I4B) :: ic
    class(CellType), pointer :: base
    type(CellRectType), pointer :: rect
    type(CellRectQuadType), pointer :: quad

    select type (cell => this%cell)
    type is (CellPolyType)
      ! load cell definition
      ic = particle%idomain(next_level) ! kluge note: is cell number always known coming in?
      call this%load_cell_defn(ic, cell%defn)
      if (this%fmi%ibdgwfsat0(ic) == 0) then ! kluge note: use cellDefn%sat == DZERO here instead?
        ! -- Cell is active but dry, so select and initialize pass-to-bottom
        ! -- cell method and set cell method pointer
        call method_cell_ptb%init( &
          cell=this%cell, &
          trackfilectl=this%trackfilectl, &
          tracktimes=this%tracktimes)
        submethod => method_cell_ptb
      else
        ! -- Select and initialize cell method and set cell method pointer
        if (cell%defn%can_be_rect) then
          call cell_poly_to_rect(cell, rect)
          base => rect
          call method_cell_plck%init( &
            cell=base, &
            trackfilectl=this%trackfilectl, &
            tracktimes=this%tracktimes)
          submethod => method_cell_plck
        else if (cell%defn%can_be_quad) then
          call cell_poly_to_quad(cell, quad)
          base => quad
          call method_cell_quad%init( &
            cell=base, &
            trackfilectl=this%trackfilectl, &
            tracktimes=this%tracktimes)
          submethod => method_cell_quad
        else
          call method_cell_tern%init( &
            cell=this%cell, &
            trackfilectl=this%trackfilectl, &
            tracktimes=this%tracktimes)
          submethod => method_cell_tern
          method_cell_tern%zeromethod = this%zeromethod
        end if
      end if
    end select
  end subroutine load_disv

  !> @brief Pass a particle to the next cell, if there is one
  subroutine pass_disv(this, particle)
    ! -- modules
    use GwfDisvModule, only: GwfDisvType
    use TdisModule, only: kper, kstp
    ! -- dummy
    class(MethodDisvType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    ! -- local
    integer(I4B) :: inface
    integer(I4B) :: ipos
    integer(I4B) :: ic
    integer(I4B) :: icu
    integer(I4B) :: inbr
    integer(I4B) :: idiag
    integer(I4B) :: icpl
    integer(I4B) :: ilay
    real(DP) :: z

    inface = particle%iboundary(2)
    z = particle%z

    select type (cell => this%cell)
    type is (CellPolyType)
      select type (dis => this%fmi%dis)
      type is (GwfDisvType)
        inbr = cell%defn%facenbr(inface)
        if (inbr .eq. 0) then
          ! -- Exterior face; no neighbor to map to
          ! particle%idomain(1) = 0
          ! particle%idomain(2) = 0   ! kluge note: "has_exited" attribute instead???
          ! particle%idomain(1) = -abs(particle%idomain(1))   ! kluge???
          ! particle%idomain(2) = -abs(particle%idomain(2))   ! kluge???
          particle%istatus = 2 ! kluge note, todo: use -2 to check for transfer to another model???
          particle%advancing = .false.
          call this%trackfilectl%save(particle, kper=kper, &
                                      kstp=kstp, reason=3) ! reason=3: termination
          ! particle%iboundary(2) = -1
        else
          idiag = dis%con%ia(cell%defn%icell)
          ipos = idiag + inbr
          ic = dis%con%ja(ipos) ! kluge note, todo: use PRT model's DIS instead of fmi's??
          particle%idomain(2) = ic

          ! compute and set user node number and layer on particle
          icu = dis%get_nodeuser(ic)
          call get_jk(icu, dis%ncpl, dis%nlay, icpl, ilay)
          particle%icu = icu
          particle%ilay = ilay

          call this%map_neighbor(cell%defn, inface, z)
          particle%iboundary(2) = inface
          particle%idomain(3:) = 0
          particle%iboundary(3:) = 0
          particle%z = z
          ! -- Update cell-cell flows of particle mass.
          !    Every particle is currently assigned unit mass.
          ! -- leaving old cell
          this%flowja(ipos) = this%flowja(ipos) - DONE
          ! -- entering new cell
          this%flowja(dis%con%isym(ipos)) &
            = this%flowja(dis%con%isym(ipos)) + DONE
        end if
      end select
    end select
  end subroutine pass_disv

  !> @brief Map location on cell face to shared cell face of neighbor
  subroutine map_neighbor(this, defn, inface, z)
    ! dummy
    class(MethodDisvType), intent(inout) :: this
    type(CellDefnType), pointer, intent(inout) :: defn
    integer(I4B), intent(inout) :: inface
    double precision, intent(inout) :: z
    ! local
    integer(I4B) :: icin
    integer(I4B) :: npolyvertsin
    integer(I4B) :: ic
    integer(I4B) :: npolyverts
    integer(I4B) :: inbr
    integer(I4B) :: inbrnbr
    integer(I4B) :: j
    integer(I4B) :: m
    real(DP) :: zrel
    real(DP) :: topfrom
    real(DP) :: botfrom
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: sat
    type(CellDefnType), pointer :: cd

    ! -- Map to shared cell face of neighbor
    inbr = defn%facenbr(inface)
    if (inbr .eq. 0) then ! kluge note: redundant check
      ! -- Exterior face; no neighbor to map to
      inface = -1 ! kluge???
    else
      ! -- Load definition for neighbor cell (neighbor with shared face)
      icin = defn%icell
      j = this%fmi%dis%con%ia(icin)
      ic = this%fmi%dis%con%ja(j + inbr)
      call create_defn(cd)
      ! kluge note: really only need to load facenbr and npolyverts for this
      call this%load_cell_defn(ic, cd) ! kluge
      npolyvertsin = defn%npolyverts
      npolyverts = cd%npolyverts
      if (inface .eq. npolyvertsin + 2) then
        ! -- Exits through bot, enters through top
        inface = npolyverts + 3
      else if (inface .eq. npolyvertsin + 3) then
        ! -- Exits through top, enters through bot
        inface = npolyverts + 2
      else
        ! -- Exits and enters through shared polygon face
        j = this%fmi%dis%con%ia(ic)
        ! kluge note: use shared_edge in DisvGeom to find shared polygon face???
        do m = 1, npolyverts + 3
          inbrnbr = cd%facenbr(m)
          if (this%fmi%dis%con%ja(j + inbrnbr) .eq. icin) then
            inface = m
            exit
          end if
        end do
        ! -- Map z between cells
        topfrom = defn%top
        botfrom = defn%bot
        zrel = (z - botfrom) / (topfrom - botfrom)
        ! kluge note: use PRT model's DIS instead of fmi's???
        top = this%fmi%dis%top(ic)
        bot = this%fmi%dis%bot(ic)
        sat = this%fmi%gwfsat(ic)
        z = bot + zrel * sat * (top - bot)
      end if
      deallocate (cd)
    end if
  end subroutine map_neighbor

  !> @brief Apply the DISV-grid method
  subroutine apply_disv(this, particle, tmax)
    class(MethodDisvType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    real(DP), intent(in) :: tmax
    call this%track(particle, 1, tmax) ! kluge, hardwired to level 1
  end subroutine apply_disv

  !> @brief Return the number of polygon vertices for a cell in the grid
  function get_npolyverts(this, ic) result(npolyverts)
    ! -- dummy
    class(MethodDisvType), intent(inout) :: this
    integer(I4B), intent(in) :: ic
    ! -- local
    integer(I4B) :: icu
    integer(I4B) :: icu2d
    integer(I4B) :: ncpl
    ! -- result
    integer(I4B) :: npolyverts

    select type (dis => this%fmi%dis)
    type is (GwfDisvType)
      ncpl = dis%get_ncpl()
      icu = dis%get_nodeuser(ic)
      icu2d = icu - ((icu - 1) / ncpl) * ncpl ! kluge note: use MOD or MODULO???
      npolyverts = dis%iavert(icu2d + 1) - dis%iavert(icu2d) - 1
      if (npolyverts .le. 0) npolyverts = npolyverts + size(dis%javert) ! kluge???
    end select
  end function get_npolyverts

  !> @brief Get top elevation based on index iatop
  !! kluge note: not needed???
  function get_top(this, iatop) result(top)
    ! -- dummy
    class(MethodDisvType), intent(inout) :: this
    integer(I4B), intent(in) :: iatop
    ! -- result
    real(DP) :: top

    if (iatop .lt. 0) then
      top = this%fmi%dis%top(-iatop)
    else
      top = this%fmi%dis%bot(iatop)
    end if
  end function get_top

  !> @brief Load cell definition from the grid
  subroutine load_cell_defn(this, ic, defn)
    ! -- dummy
    class(MethodDisvType), intent(inout) :: this
    integer(I4B), intent(in) :: ic
    type(CellDefnType), pointer, intent(inout) :: defn
    ! -- local
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: sat

    ! -- Load basic cell properties
    defn%icell = ic
    defn%npolyverts = this%get_npolyverts(ic)
    defn%iatop = get_iatop(this%fmi%dis%get_ncpl(), &
                           this%fmi%dis%get_nodeuser(ic))
    top = this%fmi%dis%top(ic)
    bot = this%fmi%dis%bot(ic)
    sat = this%fmi%gwfsat(ic)
    top = bot + sat * (top - bot)
    defn%top = top
    defn%bot = bot
    defn%sat = sat
    defn%porosity = this%porosity(ic)
    defn%retfactor = this%retfactor(ic)
    defn%izone = this%izone(ic)

    ! -- Load polygon vertices
    call this%fmi%dis%get_polyverts( &
      defn%icell, &
      defn%polyvert, &
      closed=.true.)

    ! -- Load face neighbors
    call this%load_nbrs_to_defn(defn)

    ! -- Load 180-degree indicator
    call this%load_flags_to_defn(defn)

    ! -- Load flows (assumes face neighbors already loaded)
    call this%load_flows_to_defn(defn)
  end subroutine load_cell_defn

  !> @brief Loads face neighbors to cell definition from the grid
  !! Assumes cell index and number of vertices are already loaded.
  subroutine load_nbrs_to_defn(this, defn)
    ! -- dummy
    class(MethodDisvType), intent(inout) :: this
    type(CellDefnType), pointer, intent(inout) :: defn
    ! -- local
    integer(I4B) :: ic
    integer(I4B) :: npolyverts
    integer(I4B) :: ic1
    integer(I4B) :: ic2
    integer(I4B) :: icu1
    integer(I4B) :: icu2
    integer(I4B) :: j1
    integer(I4B) :: j2
    integer(I4B) :: k1
    integer(I4B) :: k2
    integer(I4B) :: iloc
    integer(I4B) :: ipos
    integer(I4B) :: istart1
    integer(I4B) :: istart2
    integer(I4B) :: istop1
    integer(I4B) :: istop2
    integer(I4B) :: iedgeface
    integer(I4B) :: ncpl

    ic = defn%icell
    npolyverts = defn%npolyverts

    ! -- allocate facenbr array
    call ExpandArray(defn%facenbr, npolyverts + 3)

    select type (dis => this%fmi%dis)
    type is (GwfDisvType)
      ! -- Load face neighbors.
      defn%facenbr = 0
      ic1 = ic
      icu1 = dis%get_nodeuser(ic1)
      ncpl = dis%get_ncpl()
      call get_jk(icu1, ncpl, dis%nlay, j1, k1)
      istart1 = dis%iavert(j1)
      istop1 = dis%iavert(j1 + 1) - 1
      do iloc = 1, dis%con%ia(ic1 + 1) - dis%con%ia(ic1) - 1
        ipos = dis%con%ia(ic1) + iloc
        if (dis%con%mask(ipos) == 0) cycle ! kluge note: need mask here???
        ic2 = dis%con%ja(ipos)
        icu2 = dis%get_nodeuser(ic2)
        call get_jk(icu2, ncpl, dis%nlay, j2, k2)
        istart2 = dis%iavert(j2)
        istop2 = dis%iavert(j2 + 1) - 1
        call shared_edgeface(dis%javert(istart1:istop1), &
                             dis%javert(istart2:istop2), &
                             iedgeface)
        if (iedgeface /= 0) then
          ! -- Edge (polygon) face neighbor
          defn%facenbr(iedgeface) = int(iloc, 1)
        else
          if (k2 > k1) then
            ! -- Bottom face neighbor
            defn%facenbr(npolyverts + 2) = int(iloc, 1)
          else if (k2 < k1) then
            ! -- Top face neighbor
            defn%facenbr(npolyverts + 3) = int(iloc, 1)
          else
            call pstop(1, "k2 should be <> k1, since no shared edge face")
          end if
        end if
      end do
    end select
    ! -- List of edge (polygon) faces wraps around
    defn%facenbr(npolyverts + 1) = defn%facenbr(1)

  end subroutine load_nbrs_to_defn

  !> @brief Find the edge face shared by two cells
  !!
  !! Find the shared edge face of cell1 shared by cell1 and cell2.
  !! isharedface will return with 0 if there is no shared edge
  !! face.  Proceed forward through ivlist1 and backward through
  !! ivlist2 as a clockwise face in cell1 must correspond to a
  !! counter clockwise face in cell2.
  !!
  !! kluge note: based on DisvGeom shared_edge
  !<
  subroutine shared_edgeface(ivlist1, ivlist2, iedgeface)
    integer(I4B), dimension(:) :: ivlist1
    integer(I4B), dimension(:) :: ivlist2
    integer(I4B), intent(out) :: iedgeface
    integer(I4B) :: nv1
    integer(I4B) :: nv2
    integer(I4B) :: il1
    integer(I4B) :: il2
    logical(LGP) :: found

    found = .false.
    nv1 = size(ivlist1)
    nv2 = size(ivlist2)
    iedgeface = 0
    outerloop: do il1 = 1, nv1 - 1
      do il2 = nv2, 2, -1
        if (ivlist1(il1) == ivlist2(il2) .and. &
            ivlist1(il1 + 1) == ivlist2(il2 - 1)) then
          found = .true.
          iedgeface = il1
          exit outerloop
        end if
      end do
      if (found) exit
    end do outerloop
  end subroutine shared_edgeface

  !> @brief Load flows into the cell definition.
  !! These include face flows and net distributed flows.
  !! Assumes cell index and number of vertices are already loaded.
  subroutine load_flows_to_defn(this, defn)
    ! -- dummy
    class(MethodDisvType), intent(inout) :: this
    type(CellDefnType), intent(inout) :: defn
    ! -- local
    integer(I4B) :: ic
    integer(I4B) :: npolyverts
    integer(I4B) :: m
    integer(I4B) :: n

    ic = defn%icell
    npolyverts = defn%npolyverts

    ! -- allocate faceflow array
    call ExpandArray(defn%faceflow, npolyverts + 3)

    ! -- Load face flows. Note that the faceflow array
    ! -- does not get reallocated if it is already allocated
    ! -- to a size greater than or equal to npolyverts+3.
    defn%faceflow = 0d0

    ! -- As with polygon nbrs, polygon face flows wrap around for
    ! -- convenience at position npolyverts+1, and bot and top flows
    ! -- are tacked on the end of the list
    do m = 1, npolyverts + 3
      n = defn%facenbr(m)
      if (n > 0) &
        defn%faceflow(m) = this%fmi%gwfflowja(this%fmi%dis%con%ia(ic) + n)
    end do
    call this%load_boundary_flows_to_defn_poly(defn)
    ! -- Set inoexitface flag
    defn%inoexitface = 1
    do m = 1, npolyverts + 3 ! kluge note: can be streamlined with above code
      if (defn%faceflow(m) < 0d0) defn%inoexitface = 0
    end do

    ! -- Add up net distributed flow
    defn%distflow = this%fmi%SourceFlows(ic) + this%fmi%SinkFlows(ic) + &
                    this%fmi%StorageFlows(ic)

    ! -- Set weak sink flag
    if (this%fmi%SinkFlows(ic) .ne. 0d0) then
      defn%iweaksink = 1
    else
      defn%iweaksink = 0
    end if

  end subroutine load_flows_to_defn

  !> @brief Load boundary flows from the grid into a rectangular cell.
  !! Assumes cell index and number of vertices are already loaded.
  subroutine load_boundary_flows_to_defn_rect(this, defn)
    ! -- dummy
    class(MethodDisvType), intent(inout) :: this
    type(CellDefnType), intent(inout) :: defn
    ! -- local
    integer(I4B) :: ic
    integer(I4B) :: npolyverts
    integer(I4B) :: ioffset

    ic = defn%icell
    npolyverts = defn%npolyverts

    ! kluge note - assignment of BoundaryFlows to faceflow below assumes vertex 1
    ! is at upper left of rectangular cell, and BoundaryFlows use old iface order
    ! ioffset = (ic - 1)*6
    ioffset = (ic - 1) * 10
    ! kluge note: should these be additive (seems so)???
    defn%faceflow(1) = defn%faceflow(1) + &
                       this%fmi%BoundaryFlows(ioffset + 4)
    defn%faceflow(2) = defn%faceflow(2) + &
                       this%fmi%BoundaryFlows(ioffset + 2)
    defn%faceflow(3) = defn%faceflow(3) + &
                       this%fmi%BoundaryFlows(ioffset + 3)
    defn%faceflow(4) = defn%faceflow(4) + &
                       this%fmi%BoundaryFlows(ioffset + 1)
    defn%faceflow(5) = defn%faceflow(1)
    defn%faceflow(6) = defn%faceflow(6) + &
                       this%fmi%BoundaryFlows(ioffset + 9)
    defn%faceflow(7) = defn%faceflow(7) + &
                       this%fmi%BoundaryFlows(ioffset + 10)

  end subroutine load_boundary_flows_to_defn_rect

  !> @brief Load boundary flows from the grid into rectangular quadcell.
  !! Assumes cell index and number of vertices are already loaded.
  subroutine load_boundary_flows_to_defn_rect_quad(this, defn)
    ! -- dummy
    class(MethodDisvType), intent(inout) :: this
    type(CellDefnType), intent(inout) :: defn
    ! -- local
    integer(I4B) :: ic
    integer(I4B) :: npolyverts
    integer(I4B) :: m
    integer(I4B) :: n
    integer(I4B) :: nn
    integer(I4B) :: ioffset
    integer(I4B) :: nbf
    integer(I4B) :: m1
    integer(I4B) :: m2
    integer(I4B) :: mdiff
    real(DP) :: qbf
    integer(I4B) :: irectvert(5) ! kluge

    ic = defn%icell
    npolyverts = defn%npolyverts

    ! kluge note - assignment of BoundaryFlows to faceflow below assumes vertex 1
    ! is at upper left of rectangular cell, and BoundaryFlows use old iface order
    ! ioffset = (ic - 1)*6
    ioffset = (ic - 1) * 10
    ! -- Polygon faces in positions 1 through npolyverts
    do n = 1, 4
      if (n .eq. 2) then
        nbf = 4
      else if (n .eq. 4) then
        nbf = 1
      else
        nbf = n
      end if
      qbf = this%fmi%BoundaryFlows(ioffset + nbf)
      nn = 0 ! kluge ...
      do m = 1, npolyverts
        if (.not. defn%ispv180(m)) then
          nn = nn + 1
          irectvert(nn) = m
        end if
      end do
      irectvert(5) = irectvert(1) ! ... kluge
      m1 = irectvert(n)
      m2 = irectvert(n + 1)
      if (m2 .lt. m1) m2 = m2 + npolyverts
      mdiff = m2 - m1
      if (mdiff .eq. 1) then
        ! -- Assign BoundaryFlow to corresponding polygon face
        defn%faceflow(m1) = defn%faceflow(m1) + qbf
      else
        ! -- Split BoundaryFlow between two faces on quad-refined edge
        qbf = 5d-1 * qbf
        defn%faceflow(m1) = defn%faceflow(m1) + qbf
        defn%faceflow(m1 + 1) = defn%faceflow(m1 + 1) + qbf
      end if
    end do
    ! -- Wrap around to 1 in position npolyverts+1
    m = npolyverts + 1
    defn%faceflow(m) = defn%faceflow(1)
    ! -- Bottom in position npolyverts+2
    m = m + 1
    defn%faceflow(m) = defn%faceflow(m) + &
                       this%fmi%BoundaryFlows(ioffset + 9)
    ! -- Top in position npolyverts+3
    m = m + 1
    defn%faceflow(m) = defn%faceflow(m) + &
                       this%fmi%BoundaryFlows(ioffset + 10)

  end subroutine load_boundary_flows_to_defn_rect_quad

  !> @brief Load boundary flows from the grid into a polygonal cell.
  !! Assumes cell index and number of vertices are already loaded.
  subroutine load_boundary_flows_to_defn_poly(this, defn)
    ! -- dummy
    class(MethodDisvType), intent(inout) :: this
    type(CellDefnType), intent(inout) :: defn
    ! -- local
    integer(I4B) :: ic
    integer(I4B) :: npolyverts
    integer(I4B) :: ioffset
    integer(I4B) :: iv

    ic = defn%icell
    npolyverts = defn%npolyverts

    ! kluge note: hardwired for max 8 polygon faces plus top and bottom for now
    ioffset = (ic - 1) * 10
    do iv = 1, npolyverts
      ! kluge note: should these be additive (seems so)???
      defn%faceflow(iv) = &
        defn%faceflow(iv) + &
        this%fmi%BoundaryFlows(ioffset + iv)
    end do
    defn%faceflow(npolyverts + 1) = defn%faceflow(1)
    defn%faceflow(npolyverts + 2) = &
      defn%faceflow(npolyverts + 2) + &
      this%fmi%BoundaryFlows(ioffset + 9)
    defn%faceflow(npolyverts + 3) = &
      defn%faceflow(npolyverts + 3) + &
      this%fmi%BoundaryFlows(ioffset + 10)

  end subroutine load_boundary_flows_to_defn_poly

  !> @brief Load 180-degree vertex indicator array and set flags
  !! indicating how cell can be represented (kluge: latter needed?).
  !! Assumes cell index and number of vertices are already loaded.
  subroutine load_flags_to_defn(this, defn) ! kluge note: rename???
    ! -- dummy
    class(MethodDisvType), intent(inout) :: this
    type(CellDefnType), pointer, intent(inout) :: defn
    ! -- local
    integer(I4B) :: npolyverts
    integer(I4B) :: m
    integer(I4B) :: m0
    integer(I4B) :: m1
    integer(I4B) :: m2
    integer(I4B) :: ic
    integer(I4B) :: num90
    integer(I4B) :: num180
    integer(I4B) :: numacute
    real(DP) :: x0
    real(DP) :: y0
    real(DP) :: x1
    real(DP) :: y1
    real(DP) :: x2
    real(DP) :: y2
    real(DP) :: epsang
    real(DP) :: epslen
    real(DP) :: s0x
    real(DP) :: s0y
    real(DP) :: &
      s0mag, s2x, s2y, s2mag, sinang, dotprod
    logical(LGP) last180

    ic = defn%icell
    npolyverts = defn%npolyverts

    ! -- allocate ispv180 array
    call ExpandArray(defn%ispv180, npolyverts + 1)

    ! -- Load 180-degree indicator.
    ! -- Also, set flags that indicate how cell can be represented.
    defn%ispv180(1:npolyverts + 1) = .false.
    defn%can_be_rect = .false.
    defn%can_be_quad = .false.
    epsang = 1d-3 ! kluge hardwire, and using one value for all angles
    epslen = 1d-3 ! kluge hardwire
    num90 = 0
    num180 = 0
    numacute = 0
    last180 = .false.
    ! kluge note: assumes non-self-intersecting polygon;
    ! no checks for self-intersection (e.g., star)
    do m = 1, npolyverts
      m1 = m
      if (m1 .eq. 1) then
        m0 = npolyverts
        m2 = 2
      else if (m1 .eq. npolyverts) then
        m0 = npolyverts - 1
        m2 = 1
      else
        m0 = m1 - 1
        m2 = m1 + 1
      end if
      x0 = defn%polyvert(1, m0)
      y0 = defn%polyvert(2, m0)
      x1 = defn%polyvert(1, m1)
      y1 = defn%polyvert(2, m1)
      x2 = defn%polyvert(1, m2)
      y2 = defn%polyvert(2, m2)
      s0x = x0 - x1
      s0y = y0 - y1
      s0mag = dsqrt(s0x * s0x + s0y * s0y)
      s2x = x2 - x1
      s2y = y2 - y1
      s2mag = dsqrt(s2x * s2x + s2y * s2y)
      sinang = (s0x * s2y - s0y * s2x) / (s0mag * s2mag)
      ! kluge note: is it better to check in terms of angle rather than sin{angle}???
      if (dabs(sinang) .lt. epsang) then
        dotprod = s0x * s2x + s0y * s2y
        if (dotprod .gt. 0d0) then
          print *, "Cell ", ic, " has a zero angle" ! kluge
          print *, "      (tolerance epsang = ", epsang, ")"
          call pstop(1)
        else
          if (last180) then
            print *, "Cell ", ic, &
              " has consecutive 180-deg angles - not supported" ! kluge
            print *, "      (tolerance epsang = ", epsang, ")"
            call pstop(1)
          else if (dabs((s2mag - s0mag) / max(s2mag, s0mag)) .gt. epslen) then
            print *, "Cell ", ic, &
              " has a non-bisecting 180-deg vertex - not supported" ! kluge
            print *, "      (tolerance epslen = ", epslen, ")"
            call pstop(1)
          end if
          ! kluge note: want to evaluate 180-deg vertex using one criterion implemented in
          ! one place (procedure) to avoid potential disparities between multiple checks
          num180 = num180 + 1
          last180 = .true.
          defn%ispv180(m) = .true.
        end if
      else if (sinang .gt. 0d0) then
        numacute = numacute + 1
        if (dabs(1d0 - sinang) .lt. epsang) num90 = num90 + 1
        last180 = .false.
      else
        print *, "Cell ", ic, &
          " has an obtuse angle and so is nonconvex" ! kluge
        print *, "      (tolerance epsang = ", epsang, ")"
        call pstop(1)
      end if
    end do
    if ((num90 .ne. 4) .and. (num180 .ne. 0)) then
      print *, "Cell ", ic, &
        " is a non-rectangle with a 180-deg angle - not supported" ! kluge
      print *, "      (tolerance epsang = ", epsang, ")"
      call pstop(1)
    end if
    ! -- List of 180-degree indicators wraps around for convenience
    defn%ispv180(npolyverts + 1) = defn%ispv180(1)
    !
    if (num90 .eq. 4) then
      if (num180 .eq. 0) then
        defn%can_be_rect = .true.
      else
        defn%can_be_quad = .true.
      end if
    end if

  end subroutine load_flags_to_defn

end module MethodDisvModule

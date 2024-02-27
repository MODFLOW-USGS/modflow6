module MethodDisModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DONE, DZERO
  use MethodModule, only: MethodType, get_iatop
  use MethodCellPoolModule
  use CellDefnModule
  use CellRectModule
  use ParticleModule
  use PrtFmiModule, only: PrtFmiType
  use DisModule, only: DisType
  use TrackModule, only: TrackFileControlType
  use GeomUtilModule, only: get_ijk, get_jk
  use ArrayHandlersModule, only: ExpandArray
  implicit none

  private
  public :: MethodDisType
  public :: create_method_dis

  type, extends(MethodType) :: MethodDisType
  contains
    procedure, public :: apply => apply_dis ! apply the method
    procedure, public :: destroy !< destructor for the method
    procedure, public :: load => load_dis ! load the method
    procedure :: load_cell_defn !< load cell definition from the grid
    procedure, public :: pass => pass_dis !< pass the particle to the next domain
    procedure, private :: get_top ! get cell top elevation
    procedure, private :: load_nbrs_to_defn ! load face neighbors
    procedure, private :: load_flows_to_defn ! loads face flows
    procedure, private :: load_boundary_flows_to_defn ! loads BoundaryFlows
  end type MethodDisType

contains

  !> @brief Create a new structured grid (DIS) tracking method
  subroutine create_method_dis(method)
    ! -- dummy
    type(MethodDisType), pointer :: method
    ! -- local
    type(CellRectType), pointer :: cell

    allocate (method)
    allocate (method%type)
    call create_cell_rect(cell)
    method%cell => cell
    method%type = "dis"
    method%delegates = .true.
  end subroutine create_method_dis

  !> @brief Destructor the tracking method
  subroutine destroy(this)
    class(MethodDisType), intent(inout) :: this
    deallocate (this%type)
  end subroutine destroy

  !> @brief Load the cell geometry and method (tracking strategy)
  subroutine load_dis(this, particle, next_level, submethod)
    ! -- dummy
    class(MethodDisType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer(I4B), intent(in) :: next_level
    class(MethodType), pointer, intent(inout) :: submethod
    ! -- local
    integer(I4B) :: ic
    integer(I4B) :: icu
    integer(I4B) :: irow
    integer(I4B) :: jcol
    integer(I4B) :: klay
    real(DP) :: areax
    real(DP) :: areay
    real(DP) :: areaz
    real(DP) :: dx
    real(DP) :: dy
    real(DP) :: dz
    real(DP) :: factor
    real(DP) :: term

    select type (cell => this%cell)
    type is (CellRectType)
      select type (dis => this%fmi%dis)
      type is (DisType)
        ic = particle%idomain(next_level)
        call this%load_cell_defn(ic, cell%defn)

        ! -- If cell is active but dry, select and initialize
        ! -- pass-to-bottom method and set cell method pointer
        if (this%fmi%ibdgwfsat0(ic) == 0) then ! kluge note: use cellDefn%sat == DZERO here instead?
          call method_cell_ptb%init( &
            cell=this%cell, &
            trackfilectl=this%trackfilectl, &
            tracktimes=this%tracktimes)
          submethod => method_cell_ptb
        else
          ! -- load rectangular cell (todo: refactor into separate routine)
          icu = dis%get_nodeuser(ic)
          call get_ijk(icu, dis%nrow, dis%ncol, dis%nlay, &
                       irow, jcol, klay)
          dx = dis%delr(jcol)
          dy = dis%delc(irow)
          dz = cell%defn%top - cell%defn%bot
          cell%dx = dx
          cell%dy = dy
          cell%dz = dz
          cell%sinrot = DZERO
          cell%cosrot = DONE
          cell%xOrigin = cell%defn%polyvert(1, 1) ! kluge note: could avoid using polyvert here
          cell%yOrigin = cell%defn%polyvert(2, 1)
          cell%zOrigin = cell%defn%bot
          cell%ipvOrigin = 1
          areax = dy * dz
          areay = dx * dz
          areaz = dx * dy
          factor = DONE / cell%defn%retfactor
          factor = factor / cell%defn%porosity
          term = factor / areax
          cell%vx1 = cell%defn%faceflow(1) * term
          cell%vx2 = -cell%defn%faceflow(3) * term
          term = factor / areay
          cell%vy1 = cell%defn%faceflow(4) * term
          cell%vy2 = -cell%defn%faceflow(2) * term
          term = factor / areaz
          cell%vz1 = cell%defn%faceflow(6) * term
          cell%vz2 = -cell%defn%faceflow(7) * term

          ! -- Select and initialize Pollock's method and set method pointer
          call method_cell_plck%init( &
            cell=this%cell, &
            trackfilectl=this%trackfilectl, &
            tracktimes=this%tracktimes)
          submethod => method_cell_plck
        end if
      end select
    end select
  end subroutine load_dis

  !> @brief Pass a particle to the next cell, if there is one
  subroutine pass_dis(this, particle)
    ! -- modules
    use TdisModule, only: kper, kstp
    ! -- dummy
    class(MethodDisType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    ! -- local
    integer(I4B) :: inface
    integer(I4B) :: ipos
    integer(I4B) :: ic
    integer(I4B) :: icu
    integer(I4B) :: inbr
    integer(I4B) :: idiag
    integer(I4B) :: ilay
    integer(I4B) :: irow
    integer(I4B) :: icol
    real(DP) :: z
    real(DP) :: zrel
    real(DP) :: topfrom
    real(DP) :: botfrom
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: sat

    inface = particle%iboundary(2)
    z = particle%z

    select type (cell => this%cell)
    type is (CellRectType)
      select type (dis => this%fmi%dis)
      type is (DisType)
        inbr = cell%defn%facenbr(inface)
        if (inbr .eq. 0) then
          ! -- Exterior face; no neighbor to map to
          ! particle%idomain(1) = 0
          ! particle%idomain(2) = 0      ! kluge note: set a "has_exited" attribute instead???
          ! particle%idomain(1) = -abs(particle%idomain(1))   ! kluge???
          ! particle%idomain(2) = -abs(particle%idomain(2))   ! kluge???
          particle%istatus = 2 ! kluge note: use -2 to allow check for transfer to another model???
          particle%advancing = .false.
          call this%trackfilectl%save(particle, kper=kper, &
                                      kstp=kstp, reason=3) ! reason=3: termination
          ! particle%iboundary(2) = -1
        else
          idiag = dis%con%ia(cell%defn%icell)
          ipos = idiag + inbr
          ic = dis%con%ja(ipos) ! kluge note: use PRT model's DIS instead of fmi's???
          particle%idomain(2) = ic

          ! compute and set user node number and layer on particle
          icu = dis%get_nodeuser(ic)
          call get_ijk(icu, dis%nrow, dis%ncol, dis%nlay, &
                       irow, icol, ilay)
          particle%icu = icu
          particle%ilay = ilay

          ! call this%mapToNbrCell(cellRect%cellDefn,inface,z)
          if (inface .eq. 1) then
            inface = 3
          else if (inface .eq. 2) then
            inface = 4
          else if (inface .eq. 3) then
            inface = 1
          else if (inface .eq. 4) then
            inface = 2
          else if (inface .eq. 6) then
            inface = 7
          else if (inface .eq. 7) then
            inface = 6
          end if
          particle%iboundary(2) = inface
          if (inface < 5) then
            ! -- Map z between cells
            topfrom = cell%defn%top
            botfrom = cell%defn%bot
            zrel = (z - botfrom) / (topfrom - botfrom)
            top = dis%top(ic) ! kluge note: use PRT model's DIS instead of fmi's???
            bot = dis%bot(ic)
            sat = this%fmi%gwfsat(ic)
            z = bot + zrel * sat * (top - bot)
          end if
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
  end subroutine pass_dis

  !> @brief Apply the method to a particle
  subroutine apply_dis(this, particle, tmax)
    class(MethodDisType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    real(DP), intent(in) :: tmax

    call this%track(particle, 1, tmax) ! kluge, hardwired to level 1
  end subroutine apply_dis

  !> @brief Returns a top elevation based on index iatop
  function get_top(this, iatop) result(top)
    class(MethodDisType), intent(inout) :: this
    integer, intent(in) :: iatop
    double precision :: top

    if (iatop .lt. 0) then
      top = this%fmi%dis%top(-iatop)
    else
      top = this%fmi%dis%bot(iatop)
    end if
  end function get_top

  !> @brief Loads cell definition from the grid
  subroutine load_cell_defn(this, ic, defn)
    ! -- dummy
    class(MethodDisType), intent(inout) :: this
    integer(I4B), intent(in) :: ic
    type(CellDefnType), pointer, intent(inout) :: defn

    select type (dis => this%fmi%dis)
    type is (DisType)
      ! -- Set basic cell properties
      defn%icell = ic
      defn%npolyverts = 4 ! rectangular cell always has 4 vertices
      defn%iatop = get_iatop(dis%get_ncpl(), &
                             dis%get_nodeuser(ic))
      defn%top = dis%bot(ic) + &
                 this%fmi%gwfsat(ic) * (dis%top(ic) - dis%bot(ic))
      defn%bot = dis%bot(ic)
      defn%sat = this%fmi%gwfsat(ic)
      defn%porosity = this%porosity(ic)
      defn%retfactor = this%retfactor(ic)
      defn%izone = this%izone(ic)
      defn%can_be_rect = .true.
      defn%can_be_quad = .false.

      ! -- Load cell polygon vertices
      call dis%get_polyverts( &
        defn%icell, &
        defn%polyvert, &
        closed=.true.)

      ! -- Load face neighbors
      call this%load_nbrs_to_defn(defn)

      ! -- Load 180 degree face indicators
      call ExpandArray(defn%ispv180, defn%npolyverts + 1)
      defn%ispv180(1:defn%npolyverts + 1) = .false.

      ! -- Load flows (assumes face neighbors already loaded)
      call this%load_flows_to_defn(defn)
    end select
  end subroutine load_cell_defn

  !> @brief Loads face neighbors to cell definition from the grid.
  !! Assumes cell index and number of vertices are already loaded.
  subroutine load_nbrs_to_defn(this, defn)
    ! -- dummy
    class(MethodDisType), intent(inout) :: this
    type(CellDefnType), pointer, intent(inout) :: defn
    ! -- local
    integer(I4B) :: ic1
    integer(I4B) :: ic2
    integer(I4B) :: icu1
    integer(I4B) :: icu2
    integer(I4B) :: j1
    integer(I4B) :: iloc
    integer(I4B) :: ipos
    integer(I4B) :: irow1
    integer(I4B) :: irow2
    integer(I4B) :: jcol1
    integer(I4B) :: jcol2
    integer(I4B) :: klay1
    integer(I4B) :: klay2
    integer(I4B) :: iedgeface

    ! -- Allocate facenbr array
    call ExpandArray(defn%facenbr, defn%npolyverts + 3)

    select type (dis => this%fmi%dis)
    type is (DisType)
      ! -- Load face neighbors
      defn%facenbr = 0
      ic1 = defn%icell
      icu1 = dis%get_nodeuser(ic1)
      call get_ijk(icu1, dis%nrow, dis%ncol, dis%nlay, &
                   irow1, jcol1, klay1)
      call get_jk(icu1, dis%get_ncpl(), dis%nlay, j1, klay1)
      do iloc = 1, dis%con%ia(ic1 + 1) - dis%con%ia(ic1) - 1
        ipos = dis%con%ia(ic1) + iloc
        if (dis%con%mask(ipos) == 0) cycle ! kluge note: need mask here???
        ic2 = dis%con%ja(ipos)
        icu2 = dis%get_nodeuser(ic2)
        call get_ijk(icu2, dis%nrow, dis%ncol, dis%nlay, &
                     irow2, jcol2, klay2)
        if (klay2 == klay1) then
          ! -- Edge (polygon) face neighbor
          if (irow2 > irow1) then
            ! Neighbor to the S
            iedgeface = 4 ! kluge note: make sure this numbering is consistent with numbering in cell method
          else if (jcol2 > jcol1) then
            ! Neighbor to the E
            iedgeface = 3
          else if (irow2 < irow1) then
            ! Neighbor to the N
            iedgeface = 2
          else
            ! Neighbor to the W
            iedgeface = 1
          end if
          defn%facenbr(iedgeface) = int(iloc, 1)
        else if (klay2 > klay1) then
          ! -- Bottom face neighbor
          defn%facenbr(defn%npolyverts + 2) = int(iloc, 1)
        else
          ! -- Top face neighbor
          defn%facenbr(defn%npolyverts + 3) = int(iloc, 1)
        end if
      end do
    end select
    ! -- List of edge (polygon) faces wraps around
    !    todo: why need to wrap around? no analog to "closing" a polygon?
    defn%facenbr(defn%npolyverts + 1) = defn%facenbr(1)
  end subroutine load_nbrs_to_defn

  !> @brief Load flows into the cell definition.
  !! These include face flows and net distributed flows.
  !! Assumes cell index and number of vertices are already loaded.
  subroutine load_flows_to_defn(this, defn)
    ! -- dummy
    class(MethodDisType), intent(inout) :: this
    type(CellDefnType), intent(inout) :: defn
    ! -- local
    integer(I4B) :: ic
    integer(I4B) :: m
    integer(I4B) :: n
    integer(I4B) :: npolyverts

    ic = defn%icell
    npolyverts = defn%npolyverts

    ! -- allocate faceflow array
    call ExpandArray(defn%faceflow, npolyverts + 3)

    ! -- Load face flows. Note that the faceflow array
    ! -- does not get reallocated if it is already allocated
    ! -- to a size greater than or equal to npolyverts+3.
    defn%faceflow = 0d0 ! kluge note: eventually use DZERO for 0d0 throughout
    ! -- As with polygon nbrs, polygon face flows wrap around for
    ! -- convenience at position npolyverts+1, and bot and top flows
    ! -- are tacked on the end of the list
    do m = 1, npolyverts + 3
      n = defn%facenbr(m)
      if (n > 0) &
        defn%faceflow(m) = this%fmi%gwfflowja(this%fmi%dis%con%ia(ic) + n)
      ! if (cellDefn%faceflow(m) < 0d0) defn%inoexitface = 0
    end do
    ! -- Add BoundaryFlows to face flows
    call this%load_boundary_flows_to_defn(defn)
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

  !> @brief Add boundary flows to the cell definition faceflow array.
  !! Assumes cell index and number of vertices are already loaded.
  subroutine load_boundary_flows_to_defn(this, defn)
    ! -- dummy
    class(MethodDisType), intent(inout) :: this
    type(CellDefnType), intent(inout) :: defn
    ! -- local
    integer(I4B) :: ioffset

    ioffset = (defn%icell - 1) * 10
    defn%faceflow(1) = defn%faceflow(1) + &
                       this%fmi%BoundaryFlows(ioffset + 1) ! kluge note: should these be additive (seems so)???
    defn%faceflow(2) = defn%faceflow(2) + &
                       this%fmi%BoundaryFlows(ioffset + 2)
    defn%faceflow(3) = defn%faceflow(3) + &
                       this%fmi%BoundaryFlows(ioffset + 3)
    defn%faceflow(4) = defn%faceflow(4) + &
                       this%fmi%BoundaryFlows(ioffset + 4)
    defn%faceflow(5) = defn%faceflow(1)
    defn%faceflow(6) = defn%faceflow(6) + &
                       this%fmi%BoundaryFlows(ioffset + 9)
    defn%faceflow(7) = defn%faceflow(7) + &
                       this%fmi%BoundaryFlows(ioffset + 10)
  end subroutine load_boundary_flows_to_defn

end module MethodDisModule

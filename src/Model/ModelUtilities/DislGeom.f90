module DislGeom

  use KindModule, only: DP, I4B
  use SimModule, only: store_error, store_error_unit, ustop
  implicit none
  private
  public :: DislGeomType
  public :: calcdist, partialdist, line_unit_vector

  type DislGeomType
    integer(I4B) :: nodesuser
    logical :: reduced
    integer(I4B) :: nodes ! number of reduced nodes;
    real(DP), pointer, dimension(:) :: cellfdc => null()
    integer(I4B), pointer, dimension(:) :: iavert => null()
    integer(I4B), pointer, dimension(:) :: javert => null()
    integer(I4B), pointer, dimension(:) :: iavertcells => null()
    integer(I4B), pointer, dimension(:) :: javertcells => null()
    real(DP), pointer, dimension(:, :) :: vertices => null()
    real(DP), pointer, dimension(:, :) :: cellcenters => null()
    integer(I4B), pointer, dimension(:, :) :: centerverts => null()
    integer(I4B), pointer, dimension(:) :: nodereduced => null()
    integer(I4B), pointer, dimension(:) :: nodeuser => null()
  contains
    procedure :: init
    procedure :: cprops
    procedure :: connection_vector
    procedure :: shared_vertex
    procedure :: disttocenter
    procedure :: containscenter
    procedure :: vertccdist
  end type DislGeomType

contains

  subroutine init(this, nodesuser, nodes, cellfdc, iavert, javert, &
                  iavertcells, javertcells, vertices, cellcenters, &
                  centerverts, nodereduced, nodeuser)
    class(DislGeomType) :: this
    integer(I4B), intent(in) :: nodesuser
    integer(I4B), intent(in) :: nodes
    real(DP), dimension(nodes), target :: cellfdc
    integer(I4B), dimension(:), target :: iavert
    integer(I4B), dimension(:), target :: javert
    integer(I4B), dimension(:), target :: iavertcells
    integer(I4B), dimension(:), target :: javertcells
    real(DP), dimension(:, :), target :: vertices
    real(DP), dimension(:, :), target :: cellcenters
    integer(I4B), dimension(:, :), target :: centerverts
    integer(I4B), dimension(:), target :: nodereduced
    integer(I4B), dimension(:), target :: nodeuser
    ! -- local
    this%nodes = nodes
    this%nodesuser = nodesuser
    this%cellfdc => cellfdc
    this%iavert => iavert
    this%javert => javert
    this%iavertcells => iavertcells
    this%javertcells => javertcells

    this%vertices => vertices
    this%cellcenters => cellcenters
    this%centerverts => centerverts
    this%nodereduced => nodereduced
    this%nodeuser => nodeuser
    if (nodes < nodesuser) then
      this%reduced = .true.
    else
      this%reduced = .false.
    end if
  end subroutine init

  subroutine cprops(this, cell1, cell2, hwva, cl1, cl2)
    ! -- module
    use ConstantsModule, only: DZERO, DHALF, DONE
    class(DislGeomType) :: this
    integer(I4B), intent(in) :: cell1
    integer(I4B), intent(in) :: cell2
    real(DP), intent(out) :: hwva
    real(DP), intent(out) :: cl1
    real(DP), intent(out) :: cl2
    ! -- local
    integer(I4B) :: shared_vert
    character(len=300) :: ermsg
    character(len=*), parameter :: fmtvert = &
                          "('CELLS ', I0, ' AND ', i0, ' &
                          &DO NOT SHARE A VERTEX ')"

    ! find shared vertex
    call this%shared_vertex(cell1, cell2, shared_vert)
    if (shared_vert == 0) then
      write (ermsg, fmtvert) cell1, cell2
      call store_error(ermsg)
      call ustop()
    end if

    ! cl1 - distance from center of pipe 1 to vertex
    cl1 = this%disttocenter(cell1, shared_vert)

    ! cl2 - distance from center of pipe 2 to connection
    cl2 = this%disttocenter(cell2, shared_vert)

    ! hwva - smallest cross sectional area?

    return
  end subroutine cprops

  subroutine shared_vertex(this, cell1, cell2, shared_vert)
    ! return the shared vertex of two cells
    ! -- module
    class(DislGeomType) :: this
    integer(I4B), intent(in) :: cell1
    integer(I4B), intent(in) :: cell2
    integer(I4B), intent(out) :: shared_vert
    ! -- local
    integer(I4B) :: n, v, test1, test3, test4, test5
    !
    ! find shared vertex
    shared_vert = 0
    ! loop through all vertices in cell1
    outer: do v = this%iavert(cell1), this%iavert(cell1 + 1) - 1
      ! loop through all cells that share vertex v
      test1 = this%javert(v)
      test3 = this%iavertcells(this%javert(v))
      test4 = this%iavertcells(this%javert(v) + 1)
      do n = this%iavertcells(this%javert(v)), &
        this%iavertcells(this%javert(v) + 1) - 1
        ! if cell2 has shared vertex v
        test5 = this%javertcells(n)
        if (cell2 == this%javertcells(n)) then
          ! save shared vertex and exit
          shared_vert = this%javert(v)
          exit outer
        end if
      end do
    end do outer
    return
  end subroutine shared_vertex

  subroutine connection_vector(this, cell1, cell2, nozee, xcomp, &
                               ycomp, zcomp, conlen)
    ! return the x y and z components of a unit vector that points
    ! from the center of this to the center of cell2, and the
    ! straight-line connection length
    ! -- module
    use ConstantsModule, only: DZERO, DHALF, DONE
    ! -- dummy
    class(DislGeomType) :: this
    integer(I4B), intent(in) :: cell1
    integer(I4B), intent(in) :: cell2
    logical, intent(in) :: nozee
    real(DP), intent(out) :: xcomp
    real(DP), intent(out) :: ycomp
    real(DP), intent(out) :: zcomp
    real(DP), intent(out) :: conlen
    ! -- local
    real(DP) :: x1, y1, z1, x2, y2, z2
    !
    x1 = this%cellcenters(1, cell1)
    y1 = this%cellcenters(2, cell1)
    x2 = this%cellcenters(1, cell2)
    y2 = this%cellcenters(2, cell2)
    if (nozee) then
      z1 = DZERO
      z2 = DZERO
    else
      z1 = this%cellcenters(3, cell1)
      z2 = this%cellcenters(3, cell2)
    end if
    call line_unit_vector(x1, y1, z1, x2, y2, z2, xcomp, ycomp, zcomp, &
                          conlen)
    return
  end subroutine connection_vector

  function disttocenter(this, nodenum, vertnum) result(dist)
! ******************************************************************************
! disttocenter -- Return distance from cell vertex to cell center
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(DislGeomType) :: this
    integer(I4B), intent(in) :: nodenum
    integer(I4B), intent(in) :: vertnum
    real(DP) :: dist
    integer(I4B) :: j
    logical :: cellcenter, vertex
    character(len=300) :: ermsg
    character(len=*), parameter :: novert = &
                                   "('VERTEX ', I0, ' NOT IN NODE ', i0, '.')"
    character(len=*), parameter :: cellcent = &
                                   "('CELL CENTER NOT FOUND FOR &
                                   &NODE ', I0, '.')"
! ------------------------------------------------------------------------------
    ! initialize
    j = this%iavert(nodenum)
    if (this%javert(j) == vertnum) then
      vertex = .true.
      if (this%containscenter(nodenum, this%javert(j), &
                              this%javert(j + 1)) .eqv. .true.) then
        cellcenter = .true.
        dist = this%vertccdist(nodenum, this%javert(j))
        return
      else
        cellcenter = .false.
        dist = calcdist(this%vertices, this%javert(j), this%javert(j + 1))
      end if
    else
      vertex = .false.
      if (this%containscenter(nodenum, this%javert(j), &
                              this%javert(j + 1)) .eqv. .true.) then
        cellcenter = .true.
        dist = this%vertccdist(nodenum, this%javert(j + 1))
      else
        cellcenter = .false.
        dist = 0.0
      end if
    end if

    ! loop through cell's vertices
    loop: do j = this%iavert(nodenum) + 1, this%iavert(nodenum + 1) - 1
      if (this%javert(j) == vertnum) then
        vertex = .true.
        if (cellcenter .eqv. .true.) then
          exit loop
        else
          if (this%containscenter(nodenum, this%javert(j), &
                                  this%javert(j + 1)) .eqv. .true.) then
            cellcenter = .true.
            ! add the distance from this vertex to the cell center
            dist = dist + this%vertccdist(nodenum, this%javert(j))
            exit loop
          else
            ! add the distance between this vertex and the next
            dist = dist + calcdist(this%vertices, this%javert(j), &
                                   this%javert(j + 1))
          end if
        end if
      else if (this%containscenter(nodenum, this%javert(j), &
                                   this%javert(j + 1)) .eqv. .true.) then
        cellcenter = .true.
        if (vertex .eqv. .true.) then
          ! add the remaining distance from the current vertex to the cell center
          dist = dist + this%vertccdist(nodenum, this%javert(j))
          exit loop
        else
          ! add the distance from the cell center to the next vertex
          dist = dist + this%vertccdist(nodenum, this%javert(j + 1))
        end if
      else if ((vertex .eqv. .true.) .or. (cellcenter .eqv. .true.)) then
        ! add the distance between this vertex and the next
        dist = dist + calcdist(this%vertices, this%javert(j), &
                               this%javert(j + 1))
      end if
    end do loop

    if (vertex .eqv. .false.) then
      write (ermsg, novert) vertnum, nodenum
      call store_error(ermsg)
      call ustop()
    else if (cellcenter .eqv. .false.) then
      write (ermsg, cellcent) nodenum
      call store_error(ermsg)
      call ustop()
    end if

    return
  end function disttocenter

  function containscenter(this, nodenum, vert1, vert2) result(l)
! ******************************************************************************
! containscenter -- check to see if cell center is contained at or
!                   between vert1 and vert2

! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(DislGeomType) :: this
    integer(I4B), intent(in) :: nodenum
    integer(I4B), intent(in) :: vert1
    integer(I4B), intent(in) :: vert2
    logical :: l
! ------------------------------------------------------------------------------
    if (this%centerverts(2, nodenum) == 0) then
      ! cell center is at a vertex
      if ((this%centerverts(1, nodenum) == vert1) .or. &
          (this%centerverts(1, nodenum) == vert2)) then
        l = .true.
      else
        l = .false.
      end if
    else
      ! cell center is between two vertices
      if (((this%centerverts(1, nodenum) == vert1) .or. &
           (this%centerverts(1, nodenum) == vert2)) .and. &
          ((this%centerverts(2, nodenum) == vert1) .or. &
           (this%centerverts(2, nodenum) == vert2))) then
        l = .true.
      else
        l = .false.
      end if
    end if

    return
  end function containscenter

  function vertccdist(this, node, vertex) result(dist)
! ******************************************************************************
! vertccdist -- Return the distance between a cell vertex and that cell's center.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DislGeomType), intent(in) :: this
    integer(I4B), intent(in) :: node
    integer(I4B), intent(in) :: vertex
    real(DP) :: dist
    real(DP) :: xdist, ydist, zdist
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- calc distance
    xdist = abs(this%vertices(1, vertex) - this%cellcenters(1, node))
    ydist = abs(this%vertices(2, vertex) - this%cellcenters(2, node))
    zdist = abs(this%vertices(3, vertex) - this%cellcenters(3, node))
    dist = sqrt(xdist * xdist + ydist * ydist + zdist * zdist)

    ! -- return
    return
  end function vertccdist

  function calcdist(vertices, vert1, vert2) result(dist)
! ******************************************************************************
! calcdist -- Return the distance between two vertices.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:, :), intent(in) :: vertices
    integer(I4B), intent(in) :: vert1
    integer(I4B), intent(in) :: vert2
    real(DP) :: dist, xdist, ydist, zdist
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- calc distance
    xdist = abs(vertices(1, vert1) - vertices(1, vert2))
    ydist = abs(vertices(2, vert1) - vertices(2, vert2))
    zdist = abs(vertices(3, vert1) - vertices(3, vert2))
    dist = sqrt(xdist * xdist + ydist * ydist + zdist * zdist)

    ! -- return
    return
  end function calcdist

  function partialdist(coord1, coord2, percent) result(dist)
! ******************************************************************************
! partialdist -- Return the distance between two vertices.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DONE
    ! -- dummy
    real(DP), intent(in) :: coord1
    real(DP), intent(in) :: coord2
    real(DP) :: percent
    real(DP) :: dist
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- calc distance
    if (coord1 > coord2) then
      dist = coord2 + (coord1 - coord2) * (DONE - percent)
    else
      dist = coord1 + (coord2 - coord1) * percent
    end if

    ! -- return
    return
  end function partialdist

  subroutine line_unit_vector(x0, y0, z0, x1, y1, z1, &
                              xcomp, ycomp, zcomp, vmag)
! ******************************************************************************
! line_unit_vector -- Calculate the vector components (xcomp, ycomp, and zcomp)
!   for a line defined by two points, (x0, y0, z0), (x1, y1, z1). Also return
!   the magnitude of the original vector, vmag.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    real(DP), intent(in) :: x0
    real(DP), intent(in) :: y0
    real(DP), intent(in) :: z0
    real(DP), intent(in) :: x1
    real(DP), intent(in) :: y1
    real(DP), intent(in) :: z1
    real(DP), intent(out) :: xcomp
    real(DP), intent(out) :: ycomp
    real(DP), intent(out) :: zcomp
    real(DP) :: dx, dy, dz, vmag
! ------------------------------------------------------------------------------
    dx = x1 - x0
    dy = y1 - y0
    dz = z1 - z0
    vmag = sqrt(dx**2 + dy**2 + dz**2)
    xcomp = dx / vmag
    ycomp = dy / vmag
    zcomp = dz / vmag
    return
  end subroutine line_unit_vector

end module DislGeom

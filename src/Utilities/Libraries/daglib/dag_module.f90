!> @brief DAG Module
!<
module dag_module

  implicit none

  private

  type :: vertex
    ! -- A vertex of a directed acyclic graph (DAG)
    private
    integer, dimension(:), allocatable :: edges !< these are the vertices that this vertex depends on
    integer :: ivertex = 0 !< vertex number
    logical :: checking = .false. !< used for toposort
    logical :: marked = .false. !< used for toposort
    character(len=:), allocatable :: label !< used for diagraph
    character(len=:), allocatable :: attributes !< used for diagraph

  contains

    generic :: set_edges => set_edge_vector, add_edge
    procedure :: set_edge_vector
    procedure :: add_edge
  end type vertex

  type, public :: dag
    ! -- A directed acyclic graph (DAG)
    private
    integer :: n = 0 !< number of `vertices`
    type(vertex), dimension(:), allocatable :: vertices !< the vertices in the DAG.

  contains

    procedure, public :: set_vertices => dag_set_vertices
    procedure, public :: set_edges => dag_set_edges
    procedure, public :: set_vertex_info => dag_set_vertex_info
    procedure, public :: toposort => dag_toposort
    procedure, public :: generate_digraph => dag_generate_digraph
    procedure, public :: generate_dependency_matrix => &
      dag_generate_dependency_matrix
    procedure, public :: save_digraph => dag_save_digraph
    procedure, public :: get_edges => dag_get_edges
    procedure, public :: get_dependencies => dag_get_dependencies
    procedure, public :: destroy => dag_destroy
  end type dag

contains

  !> @brief Destroy the `dag`
  !<
  subroutine dag_destroy(me)
    ! -- dummy
    class(dag), intent(inout) :: me
    !
    me%n = 0
    if (allocated(me%vertices)) deallocate (me%vertices)
    !
  end subroutine dag_destroy

  !> @brief Specify the edge indices for this vertex
  !<
  subroutine set_edge_vector(me, edges)
    ! -- dummy
    class(vertex), intent(inout) :: me
    integer, dimension(:), intent(in) :: edges
    ! -- local
    integer :: i
    !
    if (allocated(me%edges)) then
      do i = 1, size(edges)
        call me%add_edge(edges(i))
      end do
    else
      ! -- Not checking for uniqueness here
      allocate (me%edges(size(edges)))
      me%edges = edges
    end if
    !
  end subroutine set_edge_vector

  !> @brief Add an edge index for this vertex
  !<
  subroutine add_edge(me, edge)
    ! -- dummy
    class(vertex), intent(inout) :: me
    integer, intent(in) :: edge
    !
    if (allocated(me%edges)) then
      if (.not. any(edge == me%edges)) then
        ! -- Auto LHS allocation
        me%edges = [me%edges, edge]
      end if
    else
      allocate (me%edges(1))
      me%edges = [edge]
    end if
    !
  end subroutine add_edge

  !> @brief Get the edges for the vertex (all the the vertices that this
  !! vertex depends on)
  !<
  pure function dag_get_edges(me, ivertex) result(edges)
    implicit none
    ! -- dummy
    class(dag), intent(in) :: me
    integer, intent(in) :: ivertex
    ! -- return
    integer, dimension(:), allocatable :: edges
    !
    if (ivertex > 0 .and. ivertex <= me%n) then
      ! -- Auto LHS allocation
      edges = me%vertices(ivertex)%edges
    end if
    !
  end function dag_get_edges

  !> @brief Get all the vertices that depend on this vertex
  !<
  pure function dag_get_dependencies(me, ivertex) result(dep)
    implicit none
    ! -- dummy
    class(dag), intent(in) :: me
    integer, intent(in) :: ivertex
    ! -- return
    integer, dimension(:), allocatable :: dep !< the set of all vertices than depend on `ivertex`
    ! -- local
    integer :: i !< vertex counter
    !
    if (ivertex > 0 .and. ivertex <= me%n) then
      !
      ! -- Have to check all the vertices:
      do i = 1, me%n
        if (allocated(me%vertices(i)%edges)) then
          if (any(me%vertices(i)%edges == ivertex)) then
            if (allocated(dep)) then
              dep = [dep, i] ! auto LHS allocation
            else
              dep = [i] ! auto LHS allocation
            end if
          end if
        end if
      end do
    end if
    !
  end function dag_get_dependencies

  !> @brief Set the number of vertices in the dag
  !<
  subroutine dag_set_vertices(me, nvertices)
    ! -- dummy
    class(dag), intent(inout) :: me
    integer, intent(in) :: nvertices
    ! -- local
    integer :: i
    !
    me%n = nvertices
    allocate (me%vertices(nvertices))
    me%vertices%ivertex = [(i, i=1, nvertices)]
    !
  end subroutine dag_set_vertices

  !> @brief Set info about a vertex in a dag
  !<
  subroutine dag_set_vertex_info(me, ivertex, label, attributes)
    ! -- dummy
    class(dag), intent(inout) :: me
    integer, intent(in) :: ivertex !< vertex number
    character(len=*), intent(in), optional :: label !< if a label is not set, then the integer vertex number is used.
    character(len=*), intent(in), optional :: attributes !< other attributes when saving as a diagraph.
    !
    if (present(label)) then
      me%vertices(ivertex)%label = label
    else
      ! -- Just use the vertex number
      me%vertices(ivertex)%label = integer_to_string(ivertex)
    end if
    !
    if (present(attributes)) then
      me%vertices(ivertex)%attributes = attributes
    end if
    !
  end subroutine dag_set_vertex_info

  !> @brief Set the edges for a vertex in a dag
  !<
  subroutine dag_set_edges(me, ivertex, edges)
    ! -- dummy
    class(dag), intent(inout) :: me
    integer, intent(in) :: ivertex !< vertex number
    integer, dimension(:), intent(in) :: edges
    !
    call me%vertices(ivertex)%set_edges(edges)
    !
  end subroutine dag_set_edges

  !> @brief Main toposort routine
  !<
  subroutine dag_toposort(me, order, istat)
    ! -- dummy
    class(dag), intent(inout) :: me
    integer, dimension(:), allocatable, intent(out) :: order !< the toposort order
    integer, intent(out) :: istat !< Status flag:
                                  !!
                                  !! * 0 if no errors
                                  !! * -1 if circular dependency
                                  !!  (in this case, `order` will not be allocated)
    ! -- local
    integer :: i, iorder
    !
    if (me%n == 0) return
    !
    allocate (order(me%n))
    !
    iorder = 0 !< index in order array
    istat = 0 !< no errors so far
    do i = 1, me%n
      if (.not. me%vertices(i)%marked) call dfs(me%vertices(i))
      if (istat == -1) exit
    end do
    !
    if (istat == -1) deallocate (order)
    !
  contains
    !
    ! -- Depth-first graph traversal
    recursive subroutine dfs(v)
      ! -- dummy
      type(vertex), intent(inout) :: v
      ! -- local
      integer :: j
      !
      if (istat == -1) return
      !
      if (v%checking) then
        ! -- Error: circular dependency
        istat = -1
      else
        if (.not. v%marked) then
          v%checking = .true.
          if (allocated(v%edges)) then
            do j = 1, size(v%edges)
              call dfs(me%vertices(v%edges(j)))
              if (istat == -1) return
            end do
          end if
          v%checking = .false.
          v%marked = .true.
          iorder = iorder + 1
          order(iorder) = v%ivertex
        end if
      end if
      !
    end subroutine dfs
    !
  end subroutine dag_toposort

  !> @brief Generate a Graphviz digraph structure for the DAG
  !!
  !! Example:
  !!  * To convert this to a PDF using `dot`: `dot -Tpdf -o test.pdf test.dot`,
  !!    where `test.dot` is `str` written to a file.
  !<
  function dag_generate_digraph(me, rankdir, dpi, edge) result(str)
    implicit none
    ! -- dummy
    class(dag), intent(in) :: me
    character(len=*), intent(in), optional :: rankdir !< right to left orientation (e.g. 'RL')
    integer, intent(in), optional :: dpi !< resolution (e.g. 300)
    character(len=*), intent(in), optional :: edge !< right to left orientation (e.g. 'forward' or 'back)
    ! -- return
    character(len=:), allocatable :: str
    ! -- local
    integer :: i, j !< counter
    integer :: n_edges !< number of edges
    character(len=:), allocatable :: attributes, label
    logical :: has_label, has_attributes
    !
    character(len=*), parameter :: tab = '  ' !< for indenting
    character(len=*), parameter :: newline = new_line(' ') !< line break character
    !
    if (me%n == 0) return
    !
    str = 'digraph G {'//newline//newline
    if (present(rankdir)) &
      str = str//tab//'rankdir='//rankdir//newline//newline
    if (present(dpi)) &
      str = str//tab//'graph [ dpi = '//integer_to_string(dpi)// &
            ' ]'//newline//newline
    if (present(edge)) &
      str = str//tab//'edge [ dir = "'//trim(adjustl(edge))// &
            '" ]'//newline//newline
    !
    ! -- Define the vertices:
    do i = 1, me%n
      has_label = allocated(me%vertices(i)%label)
      has_attributes = allocated(me%vertices(i)%attributes)
      if (has_label) label = 'label="'//trim(adjustl(me%vertices(i)%label))//'"'
      if (has_label .and. has_attributes) then
        attributes = '['//trim(adjustl(me%vertices(i)%attributes))// &
                     ','//label//']'
      elseif (has_label .and. .not. has_attributes) then
        attributes = '['//label//']'
      elseif (.not. has_label .and. has_attributes) then
        attributes = '['//trim(adjustl(me%vertices(i)%attributes))//']'
      else ! neither
        attributes = ''
      end if
      str = str//tab//integer_to_string(i)//' '//attributes//newline
      if (i == me%n) str = str//newline
    end do
    !
    ! -- Define the dependencies:
    do i = 1, me%n
      if (allocated(me%vertices(i)%edges)) then
        n_edges = size(me%vertices(i)%edges)
        str = str//tab//integer_to_string(i)//' -> '
        do j = 1, n_edges
          ! -- Comma-separated list:
          str = str//integer_to_string(me%vertices(i)%edges(j))
          if (n_edges > 1 .and. j < n_edges) str = str//','
        end do
        str = str//';'//newline
      end if
    end do
    !
    str = str//newline//'}'
    !
  end function dag_generate_digraph

  !> @brief Generate the dependency matrix for the DAG
  !!
  !! This is an \(n \times n \) matrix with elements \(A_{ij}\),
  !! such that \(A_{ij}\) is true if vertex \(i\) depends on vertex \(j\).
  !<
  subroutine dag_generate_dependency_matrix(me, mat)
    implicit none
    ! -- dummy
    class(dag), intent(in) :: me
    logical, dimension(:, :), intent(out), allocatable :: mat !< dependency matrix
    ! -- local
    integer :: i
    !
    if (me%n > 0) then
      !
      allocate (mat(me%n, me%n))
      mat = .false.
      !
      do i = 1, me%n
        if (allocated(me%vertices(i)%edges)) then
          mat(i, me%vertices(i)%edges) = .true.
        end if
      end do
      !
    end if
    !
  end subroutine dag_generate_dependency_matrix

  !> @brief Generate a Graphviz digraph structure for the DAG and write it to
  !! a file
  !<
  subroutine dag_save_digraph(me, filename, rankdir, dpi, edge)
    implicit none
    ! -- dummy
    class(dag), intent(in) :: me
    character(len=*), intent(in), optional :: filename !< file name for diagraph
    character(len=*), intent(in), optional :: rankdir !< right to left orientation (e.g. 'RL')
    integer, intent(in), optional :: dpi !< resolution (e.g. 300)
    character(len=*), intent(in), optional :: edge !< right to left orientation (e.g. 'forward' or 'back)
    ! -- local
    integer :: iunit, istat
    character(len=:), allocatable :: diagraph
    !
    diagraph = me%generate_digraph(rankdir, dpi, edge)
    !
    open (newunit=iunit, file=filename, status='REPLACE', iostat=istat)
    !
    if (istat == 0) then
      write (iunit, fmt='(A)', iostat=istat) diagraph
    else
      write (*, *) 'error opening '//trim(filename)
    end if
    !
    close (iunit, iostat=istat)
    !
  end subroutine dag_save_digraph

  !> @brief Integer to allocatable string
  pure function integer_to_string(i) result(s)
    implicit none
    ! -- dummy
    integer, intent(in) :: i
    ! -- return
    character(len=:), allocatable :: s
    ! -- local
    integer :: istat
    allocate (character(len=64) :: s) !< should be big enough
    !
    write (s, fmt='(ss,I0)', iostat=istat) i
    if (istat == 0) then
      s = trim(adjustl(s))
    else
      s = '***'
    end if
    !
  end function integer_to_string

end module dag_module

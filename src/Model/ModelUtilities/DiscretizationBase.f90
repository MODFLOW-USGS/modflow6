module BaseDisModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LENMODELNAME, LENAUXNAME, LINELENGTH, &
                             DZERO, LENMEMPATH, DPIO180
  use SmoothingModule, only: sQuadraticSaturation
  use ConnectionsModule, only: ConnectionsType
  use InputOutputModule, only: URWORD, ubdsv1
  use SimVariablesModule, only: errmsg
  use SimModule, only: count_errors, store_error, &
                       store_error_unit
  use BlockParserModule, only: BlockParserType
  use MemoryManagerModule, only: mem_allocate
  use MemoryHelperModule, only: create_mem_path
  use TdisModule, only: kstp, kper, pertim, totim, delt
  use TimeSeriesManagerModule, only: TimeSeriesManagerType
  use MatrixBaseModule

  implicit none

  private
  public :: DisBaseType
  public :: dis_transform_xy
  public :: dis_da

  ! Abstract base type for grid discretizations.
  type, abstract :: DisBaseType

    character(len=LENMEMPATH) :: memoryPath !< path for memory allocation
    character(len=LENMEMPATH) :: input_mempath = '' !< input context mempath
    character(len=LENMODELNAME), pointer :: name_model => null() !< name of the model
    character(len=LINELENGTH), pointer :: input_fname => null() !< input file name
    integer(I4B), pointer :: inunit => null() !< unit number for input file
    integer(I4B), pointer :: iout => null() !< unit number for output file
    integer(I4B), pointer :: nodes => null() !< number of nodes in solution
    integer(I4B), pointer :: nodesuser => null() !< number of user nodes (same as nodes for disu grid)
    integer(I4B), pointer :: nja => null() !< number of connections plus number of nodes
    integer(I4B), pointer :: njas => null() !< (nja-nodes)/2
    integer(I4B), pointer :: lenuni => null() !< length unit
    integer(I4B), pointer :: ndim => null() !< number of spatial model dimensions (1 for disu grid)
    integer(I4B), pointer :: icondir => null() !< flag indicating if grid has enough info to calculate connection vectors
    integer(I4B), pointer :: nogrb => null() !< don't write binary grid file
    real(DP), dimension(:), pointer, contiguous :: xc => null() !< x-coordinate of the cell center
    real(DP), dimension(:), pointer, contiguous :: yc => null() !< y-coordinate of the cell center
    real(DP), pointer :: yorigin => null() !< y-position of the lower-left grid corner (default is 0.)
    real(DP), pointer :: xorigin => null() !< x-position of the lower-left grid corner (default is 0.)
    real(DP), pointer :: angrot => null() !< counter-clockwise rotation angle of the lower-left corner (default is 0.0)
    integer(I4B), dimension(:), pointer, contiguous :: mshape => null() !< shape of the model; (nodes) for DisBaseType
    real(DP), dimension(:), pointer, contiguous :: top => null() !< (size:nodes) cell top elevation
    real(DP), dimension(:), pointer, contiguous :: bot => null() !< (size:nodes) cell bottom elevation
    real(DP), dimension(:), pointer, contiguous :: area => null() !< (size:nodes) cell area, in plan view
    type(ConnectionsType), pointer :: con => null() !< connections object
    type(BlockParserType) :: parser !< object to read blocks
    real(DP), dimension(:), pointer, contiguous :: dbuff => null() !< helper double array of size nodesuser
    integer(I4B), dimension(:), pointer, contiguous :: ibuff => null() !< helper int array of size nodesuser
    integer(I4B), dimension(:), pointer, contiguous :: nodereduced => null() !< (size:nodesuser)contains reduced nodenumber (size 0 if not reduced); -1 means vertical pass through, 0 is idomain = 0
    integer(I4B), dimension(:), pointer, contiguous :: nodeuser => null() !< (size:nodes) given a reduced nodenumber, provide the user nodenumber (size 0 if not reduced)
  contains

    ! lifecycle procedures
    procedure(dis_df), deferred :: dis_df
    procedure :: dis_ac
    procedure :: dis_mc
    procedure :: dis_ar
    procedure :: dis_da

    ! allocate scalar and array variables
    procedure :: allocate_scalars_default
    procedure :: allocate_scalars => allocate_scalars_default
    procedure :: allocate_arrays_default
    procedure :: allocate_arrays => allocate_arrays_default

    ! get_nodenumber is an overloaded integer function that will always
    ! return the reduced nodenumber.  For all grids, get_nodenumber can
    ! be passed the user nodenumber.  For some other grids, it can also
    ! be passed an index.  For dis3d the index is k, i, j, and for
    ! disv the index is k, n.
    generic :: get_nodenumber => get_nodenumber_idx1, &
      get_nodenumber_idx2, &
      get_nodenumber_idx3
    procedure :: get_nodenumber_idx1
    procedure :: get_nodenumber_idx2
    procedure :: get_nodenumber_idx3

    ! conversion between user and reduced node numbers, and utilities
    ! to parse user node numbers from lines of text and cellid strings
    procedure :: get_nodeuser ! todo rename to noder_from_nodeu?
    procedure :: noder_from_string
    procedure :: noder_from_cellid
    procedure :: noder_to_string
    procedure :: noder_to_array
    procedure(nodeu_from_string), deferred :: nodeu_from_string
    procedure(nodeu_from_cellid), deferred :: nodeu_from_cellid
    procedure(nodeu_to_string), deferred :: nodeu_to_string
    procedure(nodeu_to_array), deferred :: nodeu_to_array
    procedure(nlarray_to_nodelist), deferred :: nlarray_to_nodelist

    ! calculate normal and unit vector components for shared faces
    procedure(connection_normal), deferred :: connection_normal
    procedure(connection_vector), deferred :: connection_vector

    ! basic discretization info
    procedure(get_dis_type), deferred :: get_dis_type
    procedure(supports_layers), deferred :: supports_layers

    ! cell-related information
    procedure(get_ncpl), deferred :: get_ncpl
    procedure :: get_cell_volume
    procedure :: get_area
    procedure :: get_area_factor
    procedure :: highest_active ! todo rename to get_highest_active?

    ! write a binary grid file
    procedure(write_grb), deferred :: write_grb

    ! generic procedure to read an array from a line of text
    generic :: read_grid_array => read_int_array, read_dbl_array
    procedure :: read_int_array
    procedure :: read_dbl_array

    ! read a 2D double array from file
    procedure(read_layer_array), deferred, public :: read_layer_array

    ! generic procedure to fill arrays indexed by reduced node number
    procedure :: fill_int_array
    procedure :: fill_dbl_array
    generic :: fill_grid_array => fill_int_array, fill_dbl_array

    ! read from input files and write to output files
    procedure :: read_list
    procedure(record_srcdst_list_header), deferred :: record_srcdst_list_header
    procedure, private :: record_srcdst_list_entry
    procedure(record_array), deferred :: record_array
    procedure :: record_connection_array
    generic :: record_mf6_list_entry => record_srcdst_list_entry

  end type DisBaseType

  abstract interface
    !> @brief Get the discretization type.
    subroutine get_dis_type(this, dis_type)
      import DisBaseType
      class(DisBaseType), intent(in) :: this
      character(len=*), intent(out) :: dis_type
    end subroutine get_dis_type
  end interface

  abstract interface
    !> @brief Define the discretization.
    subroutine dis_df(this)
      import DisBaseType
      class(DisBaseType) :: this
    end subroutine dis_df
  end interface

  abstract interface
    !> @brief Write binary grid file. Called from AR procedure.
    subroutine write_grb(this, icelltype)
      import DisBaseType
      import I4B
      class(DisBaseType) :: this
      integer(I4B), dimension(:), intent(in) :: icelltype
    end subroutine write_grb
  end interface

  abstract interface
    !> @brief Convert user node number to a string in the form of (nodenumber) or (k,i,j).
    subroutine nodeu_to_string(this, nodeu, str)
      import DisBaseType
      import I4B
      class(DisBaseType) :: this
      integer(I4B), intent(in) :: nodeu
      character(len=*), intent(inout) :: str
    end subroutine nodeu_to_string
  end interface

  abstract interface
    !> @brief Convert user node number to  array with format (nodenumber) or (k,j) or (k,i,j).
    subroutine nodeu_to_array(this, nodeu, arr)
      import DisBaseType
      import I4B
      class(DisBaseType) :: this
      integer(I4B), intent(in) :: nodeu
      integer(I4B), dimension(:), intent(inout) :: arr
    end subroutine nodeu_to_array
  end interface

  abstract interface
    !> @brief Calculate normal vector components for shared cell faces.
    !!
    !! Reduced nodenumber cell is noden and its shared face with is cell nodem.
    !! ihc is the horizontal connection flag.
    subroutine connection_normal(this, noden, nodem, ihc, xcomp, ycomp, zcomp, &
                                 ipos)
      import DisBaseType
      import I4B
      import DP
      class(DisBaseType) :: this
      integer(I4B), intent(in) :: noden
      integer(I4B), intent(in) :: nodem
      integer(I4B), intent(in) :: ihc
      real(DP), intent(inout) :: xcomp
      real(DP), intent(inout) :: ycomp
      real(DP), intent(inout) :: zcomp
      integer(I4B), intent(in) :: ipos
    end subroutine connection_normal
  end interface

  abstract interface
    !> @brief Calculate unit vector components for shared cell faces.
    !!
    !! Reduced nodenumber cell is noden and its neighbor cell is nodem.
    !! Saturations for these cells are also required so the vertical
    !! position of the cell centers can be calculated. ihc is the
    !! horizontal flag. Also return the straight-line connection length.
    subroutine connection_vector(this, noden, nodem, nozee, satn, satm, ihc, &
                                 xcomp, ycomp, zcomp, conlen)
      import DisBaseType
      import I4B
      import DP
      class(DisBaseType) :: this
      integer(I4B), intent(in) :: noden
      integer(I4B), intent(in) :: nodem
      logical, intent(in) :: nozee
      real(DP), intent(in) :: satn
      real(DP), intent(in) :: satm
      integer(I4B), intent(in) :: ihc
      real(DP), intent(inout) :: xcomp
      real(DP), intent(inout) :: ycomp
      real(DP), intent(inout) :: zcomp
      real(DP), intent(inout) :: conlen
    end subroutine connection_vector
  end interface

  abstract interface
    !> @brief Parse a user nodenumber from a line of text.
    !!
    !! The model is unstructured; just read user nodenumber.
    !! If flag_string argument is present and true, the first token in string
    !! is allowed to be a string (e.g. boundary name). In this case, if a string
    !! is encountered, return value as -2.
    function nodeu_from_string(this, lloc, istart, istop, in, iout, line, &
                               flag_string, allow_zero) result(nodeu)
      import DisBaseType
      import I4B
      class(DisBaseType) :: this
      integer(I4B), intent(inout) :: lloc
      integer(I4B), intent(inout) :: istart
      integer(I4B), intent(inout) :: istop
      integer(I4B), intent(in) :: in
      integer(I4B), intent(in) :: iout
      character(len=*), intent(inout) :: line
      logical, optional, intent(in) :: flag_string
      logical, optional, intent(in) :: allow_zero
      integer(I4B) :: nodeu
    end function nodeu_from_string
  end interface

  abstract interface
    !> @brief Parse a user nodenumber from a cell ID string.
    !!
    !! If flag_string argument is present and true, the first token in string
    !! is allowed to be a string (e.g. boundary name). In this case, if a string
    !! is encountered, return value as -2.
    !! If allow_zero argument is present and true, if all indices equal zero, the
    !! result can be zero. If allow_zero is false, a zero in any index causes an
    !! error.
    function nodeu_from_cellid(this, cellid, inunit, iout, flag_string, &
                               allow_zero) result(nodeu)
      import DisBaseType
      import I4B
      class(DisBaseType) :: this
      character(len=*), intent(inout) :: cellid
      integer(I4B), intent(in) :: inunit
      integer(I4B), intent(in) :: iout
      logical, optional, intent(in) :: flag_string
      logical, optional, intent(in) :: allow_zero
      integer(I4B) :: nodeu
    end function nodeu_from_cellid
  end interface

  abstract interface
    !> @brief Indicates whether the discretization supports layers.
    logical function supports_layers(this)
      import DisBaseType
      class(DisBaseType) :: this
    end function supports_layers
  end interface

  abstract interface
    !> @brief Get the number of cells per layer (nodes for DISU, since no layers).
    function get_ncpl(this)
      import DisBaseType
      import I4B
      integer(I4B) :: get_ncpl
      class(DisBaseType) :: this
    end function get_ncpl
  end interface

  abstract interface
    !> @brief Read a 2d double array into col icolbnd of darray.
    !! For cells that are outside of the active domain,
    !! do not copy the array value into darray.
    subroutine read_layer_array(this, nodelist, darray, ncolbnd, maxbnd, &
                                icolbnd, aname, inunit, iout)
      import DisBaseType
      import I4B
      import DP
      class(DisBaseType) :: this
      integer(I4B), intent(in) :: ncolbnd
      integer(I4B), intent(in) :: maxbnd
      integer(I4B), dimension(maxbnd) :: nodelist
      real(DP), dimension(ncolbnd, maxbnd), intent(inout) :: darray
      integer(I4B), intent(in) :: icolbnd
      character(len=*), intent(in) :: aname
      integer(I4B), intent(in) :: inunit
      integer(I4B), intent(in) :: iout
    end subroutine read_layer_array
  end interface

  abstract interface
    !> @brief Record a double precision array.
    !!
    !! The array will be printed to an external file and/or written to an
    !! unformatted external file depending on the argument specifications.
    !!
    !! SPECIFICATIONS:
    !!   darray is the double precision array to record
    !!   iout is the unit number for ascii output
    !!   iprint is a flag indicating whether or not to print the array
    !!   idataun is the unit number to which the array will be written in binary
    !!     form; if negative then do not write by layers, write entire array
    !!   aname is the text descriptor of the array
    !!   cdatafmp is the fortran format for writing the array
    !!   nvaluesp is the number of values per line for printing
    !!   nwidthp is the width of the number for printing
    !!   editdesc is the format type (I, G, F, S, E)
    !!   dinact is the double precision value to use for cells that are excluded
    !!     from the model domain
    subroutine record_array(this, darray, iout, iprint, idataun, aname, &
                            cdatafmp, nvaluesp, nwidthp, editdesc, dinact)
      import DisBaseType
      import I4B
      import DP
      class(DisBaseType), intent(inout) :: this
      real(DP), dimension(:), pointer, contiguous, intent(inout) :: darray
      integer(I4B), intent(in) :: iout
      integer(I4B), intent(in) :: iprint
      integer(I4B), intent(in) :: idataun
      character(len=*), intent(in) :: aname
      character(len=*), intent(in) :: cdatafmp
      integer(I4B), intent(in) :: nvaluesp
      integer(I4B), intent(in) :: nwidthp
      character(len=*), intent(in) :: editdesc
      real(DP), intent(in) :: dinact
    end subroutine record_array
  end interface

  abstract interface
    !> @brief Record list header for imeth=6
    subroutine record_srcdst_list_header(this, text, textmodel, textpackage, &
                                         dstmodel, dstpackage, naux, auxtxt, &
                                         ibdchn, nlist, iout)
      import DisBaseType
      import I4B
      class(DisBaseType) :: this
      character(len=16), intent(in) :: text
      character(len=16), intent(in) :: textmodel
      character(len=16), intent(in) :: textpackage
      character(len=16), intent(in) :: dstmodel
      character(len=16), intent(in) :: dstpackage
      integer(I4B), intent(in) :: naux
      character(len=16), dimension(:), intent(in) :: auxtxt
      integer(I4B), intent(in) :: ibdchn
      integer(I4B), intent(in) :: nlist
      integer(I4B), intent(in) :: iout
    end subroutine record_srcdst_list_header
  end interface

  abstract interface
    !> @brief Convert an integer array to nodelist.
    !! For structured model, integer array is layer number; for unstructured
    !! model, integer array is node number.
    subroutine nlarray_to_nodelist(this, darray, nodelist, maxbnd, nbound, aname)
      import DisBaseType
      import I4B
      class(DisBaseType) :: this
      integer(I4B), intent(in) :: maxbnd
      integer(I4B), dimension(:), pointer, contiguous :: darray
      integer(I4B), dimension(maxbnd), intent(inout) :: nodelist
      integer(I4B), intent(inout) :: nbound
      character(len=*), intent(in) :: aname
    end subroutine nlarray_to_nodelist
  end interface

contains

  !> @brief Add connections to sparse based on cell connectivity.
  subroutine dis_ac(this, moffset, sparse)
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: moffset
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    integer(I4B) :: i, j, ipos, iglo, jglo
    !
    do i = 1, this%nodes
      do ipos = this%con%ia(i), this%con%ia(i + 1) - 1
        j = this%con%ja(ipos)
        iglo = i + moffset
        jglo = j + moffset
        call sparse%addconnection(iglo, jglo, 1)
      end do
    end do
  end subroutine dis_ac

  !> @brief Map cell connection positions in numerical solution coefficient matrix.
  subroutine dis_mc(this, moffset, idxglo, matrix_sln)
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: moffset
    integer(I4B), dimension(:), intent(inout) :: idxglo
    class(MatrixBaseType), pointer :: matrix_sln
    ! -- local
    integer(I4B) :: i, j, ipos, iglo, jglo
    !
    do i = 1, this%nodes
      iglo = i + moffset
      do ipos = this%con%ia(i), this%con%ia(i + 1) - 1
        j = this%con%ja(ipos)
        jglo = j + moffset
        idxglo(ipos) = matrix_sln%get_position(iglo, jglo)
      end do
    end do
  end subroutine dis_mc

  !> @brief Allocate and read.
  subroutine dis_ar(this, icelltype)
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), dimension(:), intent(in) :: icelltype
    ! -- local
    integer(I4B), dimension(:), allocatable :: ict
    integer(I4B) :: nu, nr
    !
    ! -- Expand icelltype to full grid; fill with 0 if cell is excluded
    allocate (ict(this%nodesuser))
    do nu = 1, this%nodesuser
      nr = this%get_nodenumber(nu, 0)
      if (nr > 0) then
        ict(nu) = icelltype(nr)
      else
        ict(nu) = 0
      end if
    end do
    !
    if (this%nogrb == 0) call this%write_grb(ict)
  end subroutine dis_ar

  !> @brief Deallocate discretization object.
  subroutine dis_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(DisBaseType) :: this
    !
    ! -- Strings
    deallocate (this%name_model)
    deallocate (this%input_fname)
    !
    ! -- Scalars
    call mem_deallocate(this%inunit)
    call mem_deallocate(this%iout)
    call mem_deallocate(this%nodes)
    call mem_deallocate(this%nodesuser)
    call mem_deallocate(this%ndim)
    call mem_deallocate(this%icondir)
    call mem_deallocate(this%nogrb)
    call mem_deallocate(this%xorigin)
    call mem_deallocate(this%yorigin)
    call mem_deallocate(this%angrot)
    call mem_deallocate(this%nja)
    call mem_deallocate(this%njas)
    call mem_deallocate(this%lenuni)
    !
    ! -- Arrays
    call mem_deallocate(this%mshape)
    call mem_deallocate(this%xc)
    call mem_deallocate(this%yc)
    call mem_deallocate(this%top)
    call mem_deallocate(this%bot)
    call mem_deallocate(this%area)
    call mem_deallocate(this%dbuff)
    call mem_deallocate(this%ibuff)
    !
    ! -- Connections
    call this%con%con_da()
    deallocate (this%con)
  end subroutine dis_da

  !> @brief Get user nodenumber from reduced node number.
  function get_nodeuser(this, noder) result(nodenumber)
    ! -- return
    integer(I4B) :: nodenumber
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: noder
    !
    if (this%nodes < this%nodesuser) then
      nodenumber = this%nodeuser(noder)
    else
      nodenumber = noder
    end if
  end function get_nodeuser

  !> @brief Get nodenumber from the user specified node number.
  !! Overridde in child classes to map to a model node number,
  !! optionally performing a check.
  function get_nodenumber_idx1(this, nodeu, icheck) result(nodenumber)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    ! -- dummy
    class(DisBaseType), intent(in) :: this
    integer(I4B), intent(in) :: nodeu
    integer(I4B), intent(in) :: icheck
    ! -- local
    integer(I4B) :: nodenumber
    !
    nodenumber = 0
    call store_error('Program error: get_nodenumber_idx1 not implemented.', &
                     terminate=.TRUE.)
  end function get_nodenumber_idx1

  !> @brief Override in child classes to map to a model node number.
  function get_nodenumber_idx2(this, k, j, icheck) result(nodenumber)
    use SimModule, only: store_error
    ! -- dummy
    class(DisBaseType), intent(in) :: this
    integer(I4B), intent(in) :: k, j
    integer(I4B), intent(in) :: icheck
    integer(I4B) :: nodenumber
    !
    nodenumber = 0
    call store_error('Program error: get_nodenumber_idx2 not implemented.', &
                     terminate=.TRUE.)
  end function get_nodenumber_idx2

  !> @brief This function will not be invoked for an unstructured
  !! model, but it may be from a Discretization3dType model.
  function get_nodenumber_idx3(this, k, i, j, icheck) result(nodenumber)
    use SimModule, only: store_error
    ! -- dummy
    class(DisBaseType), intent(in) :: this
    integer(I4B), intent(in) :: k, i, j
    integer(I4B), intent(in) :: icheck
    integer(I4B) :: nodenumber
    !
    nodenumber = 0
    call store_error('Program error: get_nodenumber_idx3 not implemented.', &
                     terminate=.TRUE.)
  end function get_nodenumber_idx3

  !> @brief Transform local to global coords using xorigin, yorigin, angrot.
  subroutine dis_transform_xy(x, y, xorigin, yorigin, angrot, xglo, yglo)
    real(DP), intent(in) :: x !< the cell-x coordinate to transform
    real(DP), intent(in) :: y !< the cell-y coordinate to transform
    real(DP), intent(in) :: xorigin !< the cell-y coordinate to transform
    real(DP), intent(in) :: yorigin !< the cell-y coordinate to transform
    real(DP), intent(in) :: angrot !< the cell-y coordinate to transform
    real(DP), intent(out) :: xglo !< the global cell-x coordinate
    real(DP), intent(out) :: yglo !< the global cell-y coordinate
    ! local
    real(DP) :: ang

    xglo = x
    yglo = y

    ! first _rotate_ to 'real world'
    ang = angrot * DPIO180
    if (ang /= DZERO) then
      xglo = x * cos(ang) - y * sin(ang)
      yglo = x * sin(ang) + y * cos(ang)
    end if

    ! then _translate_
    xglo = xglo + xorigin
    yglo = yglo + yorigin
  end subroutine dis_transform_xy

  !> @brief Allocate and initialize scalar variables.
  subroutine allocate_scalars_default(this, name_model, input_mempath)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use MemoryManagerExtModule, only: mem_set_value
    ! -- dummy
    class(DisBaseType) :: this
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: input_mempath
    logical(LGP) :: found
    !
    ! -- Create memory path
    this%memoryPath = create_mem_path(name_model, 'DIS')
    !
    ! -- Allocate
    allocate (this%name_model)
    allocate (this%input_fname)
    !
    call mem_allocate(this%inunit, 'INUNIT', this%memoryPath)
    call mem_allocate(this%iout, 'IOUT', this%memoryPath)
    call mem_allocate(this%nodes, 'NODES', this%memoryPath)
    call mem_allocate(this%nodesuser, 'NODESUSER', this%memoryPath)
    call mem_allocate(this%ndim, 'NDIM', this%memoryPath)
    call mem_allocate(this%icondir, 'ICONDIR', this%memoryPath)
    call mem_allocate(this%nogrb, 'NOGRB', this%memoryPath)
    call mem_allocate(this%xorigin, 'XORIGIN', this%memoryPath)
    call mem_allocate(this%yorigin, 'YORIGIN', this%memoryPath)
    call mem_allocate(this%angrot, 'ANGROT', this%memoryPath)
    call mem_allocate(this%nja, 'NJA', this%memoryPath)
    call mem_allocate(this%njas, 'NJAS', this%memoryPath)
    call mem_allocate(this%lenuni, 'LENUNI', this%memoryPath)
    !
    ! -- Initialize
    this%name_model = name_model
    this%input_mempath = input_mempath
    this%input_fname = ''
    this%inunit = 0
    this%iout = 0
    this%nodes = 0
    this%nodesuser = 0
    this%ndim = 1
    this%icondir = 1
    this%nogrb = 0
    this%xorigin = DZERO
    this%yorigin = DZERO
    this%angrot = DZERO
    this%nja = 0
    this%njas = 0
    this%lenuni = 0
    !
    ! -- update input filename
    call mem_set_value(this%input_fname, 'INPUT_FNAME', &
                       this%input_mempath, found)
  end subroutine allocate_scalars_default

  !> @brief Allocate and initialize array variables.
  subroutine allocate_arrays_default(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(DisBaseType) :: this
    integer :: isize
    !
    ! -- Allocate
    call mem_allocate(this%mshape, this%ndim, 'MSHAPE', this%memoryPath)
    call mem_allocate(this%xc, this%nodes, 'XC', this%memoryPath)
    call mem_allocate(this%yc, this%nodes, 'YC', this%memoryPath)
    call mem_allocate(this%top, this%nodes, 'TOP', this%memoryPath)
    call mem_allocate(this%bot, this%nodes, 'BOT', this%memoryPath)
    call mem_allocate(this%area, this%nodes, 'AREA', this%memoryPath)
    !
    ! -- Initialize
    this%mshape(1) = this%nodes
    !
    ! -- Determine size of buff memory
    if (this%nodes < this%nodesuser) then
      isize = this%nodesuser
    else
      isize = this%nodes
    end if
    !
    ! -- Allocate the arrays
    call mem_allocate(this%dbuff, isize, 'DBUFF', this%name_model)
    call mem_allocate(this%ibuff, isize, 'IBUFF', this%name_model)
  end subroutine allocate_arrays_default

  !> @brief Parse a reduced nodenumber from a line of text.
  !!
  !! The model is unstructured; just read user nodenumber.
  !! If flag_string argument is present and true, the first token in string
  !! is allowed to be a string (e.g. boundary name). In this case, if a string
  !! is encountered, return value as -2.
  function noder_from_string(this, lloc, istart, istop, in, iout, line, &
                             flag_string) result(noder)
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    integer(I4B), intent(in) :: in
    integer(I4B), intent(in) :: iout
    character(len=*), intent(inout) :: line
    logical, optional, intent(in) :: flag_string
    integer(I4B) :: noder
    ! -- local
    integer(I4B) :: nodeu
    character(len=LINELENGTH) :: nodestr
    logical :: flag_string_local
    !
    if (present(flag_string)) then
      flag_string_local = flag_string
    else
      flag_string_local = .false.
    end if
    nodeu = this%nodeu_from_string(lloc, istart, istop, in, iout, line, &
                                   flag_string_local)
    !
    ! -- Convert user-based nodenumber to reduced node number
    if (nodeu > 0) then
      noder = this%get_nodenumber(nodeu, 0)
    else
      noder = nodeu
    end if
    if (noder <= 0 .and. .not. flag_string_local) then
      call this%nodeu_to_string(nodeu, nodestr)
      write (errmsg, *) &
        ' Cell is outside active grid domain: '// &
        trim(adjustl(nodestr))
      call store_error(errmsg)
    end if
  end function noder_from_string

  !> @brief Parse a reduced nodenumber from a cell ID string.
  !!
  !! If flag_string argument is present and true, the first token in string
  !! is allowed to be a string (e.g. boundary name). In this case, if a string
  !! is encountered, return value as -2.
  !! If allow_zero argument is present and true, if all indices equal zero, the
  !! result can be zero. If allow_zero is false, a zero in any index is an error.
  function noder_from_cellid(this, cellid, inunit, iout, flag_string, &
                             allow_zero) result(noder)
    integer(I4B) :: noder
    ! -- dummy
    class(DisBaseType) :: this
    character(len=*), intent(inout) :: cellid
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    logical, optional, intent(in) :: flag_string
    logical, optional, intent(in) :: allow_zero
    ! -- local
    integer(I4B) :: nodeu
    logical :: allowzerolocal
    character(len=LINELENGTH) :: nodestr
    logical :: flag_string_local
    !
    if (present(flag_string)) then
      flag_string_local = flag_string
    else
      flag_string_local = .false.
    end if
    if (present(allow_zero)) then
      allowzerolocal = allow_zero
    else
      allowzerolocal = .false.
    end if
    !
    nodeu = this%nodeu_from_cellid(cellid, inunit, iout, flag_string_local, &
                                   allowzerolocal)
    !
    ! -- Convert user-based nodenumber to reduced node number
    if (nodeu > 0) then
      noder = this%get_nodenumber(nodeu, 0)
    else
      noder = nodeu
    end if
    if (noder <= 0 .and. .not. flag_string_local) then
      call this%nodeu_to_string(nodeu, nodestr)
      write (errmsg, *) &
        ' Cell is outside active grid domain: '// &
        trim(adjustl(nodestr))
      call store_error(errmsg)
    end if
  end function noder_from_cellid

  !> @brief Get the volume of cell n based on x value passed.
  function get_cell_volume(this, n, x)
    ! -- modules
    ! -- return
    real(DP) :: get_cell_volume
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: x
    ! -- local
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: sat
    real(DP) :: thick
    !
    get_cell_volume = DZERO
    tp = this%top(n)
    bt = this%bot(n)
    sat = sQuadraticSaturation(tp, bt, x)
    thick = (tp - bt) * sat
    get_cell_volume = this%area(n) * thick
  end function get_cell_volume

  !> @brief Read an integer array from a line of text.
  subroutine read_int_array(this, line, lloc, istart, istop, iout, in, &
                            iarray, aname)
    ! -- dummy
    class(DisBaseType), intent(inout) :: this
    character(len=*), intent(inout) :: line
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    integer(I4B), intent(in) :: in
    integer(I4B), intent(in) :: iout
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: iarray
    character(len=*), intent(in) :: aname
    !
    ! -- store error
    errmsg = 'Programmer error: read_int_array needs to be overridden &
            &in any DIS type that extends DisBaseType'
    call store_error(errmsg, terminate=.TRUE.)
  end subroutine read_int_array

  !> @brief Read a double precision array from a line of text.
  subroutine read_dbl_array(this, line, lloc, istart, istop, iout, in, &
                            darray, aname)
    ! -- dummy
    class(DisBaseType), intent(inout) :: this
    character(len=*), intent(inout) :: line
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    integer(I4B), intent(in) :: in
    integer(I4B), intent(in) :: iout
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: darray
    character(len=*), intent(in) :: aname
    !
    ! -- str=ore error message
    errmsg = 'Programmer error: read_dbl_array needs to be overridden &
            &in any DIS type that extends DisBaseType'
    call store_error(errmsg, terminate=.TRUE.)
  end subroutine read_dbl_array

  !> @brief Fill an integer array indexed by reduced node number.
  subroutine fill_int_array(this, ibuff1, ibuff2)
    ! -- dummy
    class(DisBaseType), intent(inout) :: this
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: ibuff1
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: ibuff2
    ! -- local
    integer(I4B) :: nodeu
    integer(I4B) :: noder
    !
    do nodeu = 1, this%nodesuser
      noder = this%get_nodenumber(nodeu, 0)
      if (noder <= 0) cycle
      ibuff2(noder) = ibuff1(nodeu)
    end do
  end subroutine fill_int_array

  !> @brief Fill a double precision array indexed by reduced node number.
  subroutine fill_dbl_array(this, buff1, buff2)
    ! -- dummy
    class(DisBaseType), intent(inout) :: this
    real(DP), dimension(:), pointer, contiguous, intent(in) :: buff1
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: buff2
    ! -- local
    integer(I4B) :: nodeu
    integer(I4B) :: noder
    !
    do nodeu = 1, this%nodesuser
      noder = this%get_nodenumber(nodeu, 0)
      if (noder <= 0) cycle
      buff2(noder) = buff1(nodeu)
    end do
  end subroutine fill_dbl_array

  !> @brief Read a list using the list reader object.
  !!
  !! Convert user node numbers to reduced numbers.
  !! Terminate if any nodenumbers are within an inactive domain.
  !! Set up time series and multiply by iauxmultcol if it exists.
  !! Write the list to iout if iprpak is set.
  subroutine read_list(this, line_reader, in, iout, iprpak, nlist, &
                       inamedbound, iauxmultcol, nodelist, rlist, auxvar, &
                       auxname, boundname, label, pkgname, tsManager, iscloc, &
                       indxconvertflux)
    ! -- modules
    use ConstantsModule, only: LENBOUNDNAME, LINELENGTH
    use LongLineReaderModule, only: LongLineReaderType
    use ListReaderModule, only: ListReaderType
    use SimModule, only: store_error, store_error_unit, count_errors
    use InputOutputModule, only: urword
    use TimeSeriesLinkModule, only: TimeSeriesLinkType
    use TimeSeriesManagerModule, only: read_value_or_time_series
    ! -- dummy
    class(DisBaseType) :: this
    type(LongLineReaderType), intent(inout) :: line_reader
    integer(I4B), intent(in) :: in
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(in) :: iprpak
    integer(I4B), intent(inout) :: nlist
    integer(I4B), intent(in) :: inamedbound
    integer(I4B), intent(in) :: iauxmultcol
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: rlist
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: auxvar
    character(len=LENAUXNAME), dimension(:), intent(inout) :: auxname
    character(len=LENBOUNDNAME), dimension(:), pointer, contiguous, &
      intent(inout) :: boundname
    character(len=*), intent(in) :: label
    character(len=*), intent(in) :: pkgName
    type(TimeSeriesManagerType) :: tsManager
    integer(I4B), intent(in) :: iscloc
    integer(I4B), intent(in), optional :: indxconvertflux
    ! -- local
    integer(I4B) :: l
    integer(I4B) :: nodeu, noder
    character(len=LINELENGTH) :: nodestr
    integer(I4B) :: ii, jj
    real(DP), pointer :: bndElem => null()
    type(ListReaderType) :: lstrdobj
    type(TimeSeriesLinkType), pointer :: tsLinkBnd => null()
    type(TimeSeriesLinkType), pointer :: tsLinkAux => null()
    !
    ! -- Read the list
    call lstrdobj%read_list(line_reader, in, iout, nlist, inamedbound, &
                            this%mshape, nodelist, rlist, auxvar, auxname, &
                            boundname, label)
    !
    ! -- Go through all locations where a text string was found instead of
    !    a double precision value and make time-series links to rlist
    if (lstrdobj%ntxtrlist > 0) then
      do l = 1, lstrdobj%ntxtrlist
        ii = lstrdobj%idxtxtrow(l)
        jj = lstrdobj%idxtxtcol(l)
        tsLinkBnd => NULL()
        bndElem => rlist(jj, ii)
        call read_value_or_time_series(lstrdobj%txtrlist(l), ii, jj, bndElem, &
                                       pkgName, 'BND', tsManager, iprpak, &
                                       tsLinkBnd)
        if (associated(tsLinkBnd)) then
          !
          ! -- If iauxmultcol is active and this column is the column
          !    to be scaled, then assign tsLinkBnd%RMultiplier to auxvar
          !    multiplier
          if (iauxmultcol > 0 .and. jj == iscloc) then
            tsLinkBnd%RMultiplier => auxvar(iauxmultcol, ii)
          end if
          !
          ! -- If boundaries are named, save the name in the link
          if (lstrdobj%inamedbound == 1) then
            tsLinkBnd%BndName = lstrdobj%boundname(tsLinkBnd%IRow)
          end if
          !
          ! -- if the value is a flux and needs to be converted to a flow
          !    then set the tsLinkBnd appropriately
          if (present(indxconvertflux)) then
            if (indxconvertflux == jj) then
              tsLinkBnd%convertflux = .true.
              nodeu = nodelist(ii)
              noder = this%get_nodenumber(nodeu, 0)
              tsLinkBnd%CellArea = this%get_area(noder)
            end if
          end if
          !
        end if
      end do
    end if
    !
    ! -- Make time-series substitutions for auxvar
    if (lstrdobj%ntxtauxvar > 0) then
      do l = 1, lstrdobj%ntxtauxvar
        ii = lstrdobj%idxtxtauxrow(l)
        jj = lstrdobj%idxtxtauxcol(l)
        tsLinkAux => NULL()
        bndElem => auxvar(jj, ii)
        call read_value_or_time_series(lstrdobj%txtauxvar(l), ii, jj, bndElem, &
                                       pkgName, 'AUX', tsManager, iprpak, &
                                       tslinkAux)
        if (lstrdobj%inamedbound == 1) then
          if (associated(tsLinkAux)) then
            tsLinkAux%BndName = lstrdobj%boundname(tsLinkAux%IRow)
          end if
        end if
      end do
    end if
    !
    ! -- Multiply rlist by the multiplier column in auxvar
    if (iauxmultcol > 0) then
      do l = 1, nlist
        rlist(iscloc, l) = rlist(iscloc, l) * auxvar(iauxmultcol, l)
      end do
    end if
    !
    ! -- Write the list to iout if requested
    if (iprpak /= 0) then
      call lstrdobj%write_list()
    end if
    !
    ! -- Convert user nodenumbers to reduced nodenumbers, if necessary.
    !    Conversion to reduced nodenumbers must be done last, after the
    !    list is written so that correct indices are written to the list.
    if (this%nodes < this%nodesuser) then
      do l = 1, nlist
        nodeu = nodelist(l)
        noder = this%get_nodenumber(nodeu, 0)
        if (noder <= 0) then
          call this%nodeu_to_string(nodeu, nodestr)
          write (errmsg, *) &
            ' Cell is outside active grid domain: '// &
            trim(adjustl(nodestr))
          call store_error(errmsg)
        end if
        nodelist(l) = noder
      end do
      !
      ! -- Check for errors and terminate if encountered
      if (count_errors() > 0) then
        write (errmsg, *) count_errors(), ' errors encountered.'
        call store_error(errmsg)
        call store_error_unit(in)
      end if
    end if
  end subroutine read_list

  !> @brief Record a connection-based double precision array.
  subroutine record_connection_array(this, flowja, ibinun, iout)
    ! -- dummy
    class(DisBaseType) :: this
    real(DP), dimension(:), intent(in) :: flowja
    integer(I4B), intent(in) :: ibinun
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=16), dimension(1) :: text
    ! -- data
    data text(1)/'    FLOW-JA-FACE'/
    !
    ! -- write full ja array
    call ubdsv1(kstp, kper, text(1), ibinun, flowja, size(flowja), 1, 1, &
                iout, delt, pertim, totim)
  end subroutine record_connection_array

  !> @brief Convert reduced node number to string in form (nodenumber) or (k,i,j).
  subroutine noder_to_string(this, noder, str)
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: noder
    character(len=*), intent(inout) :: str
    ! -- local
    integer(I4B) :: nodeu
    !
    nodeu = this%get_nodeuser(noder)
    call this%nodeu_to_string(nodeu, str)
  end subroutine noder_to_string

  !> @brief Convert reduced node number to cellid and fill array with (nodenumber)
  !! or (k,j) or (k,i,j)
  subroutine noder_to_array(this, noder, arr)
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: noder
    integer(I4B), dimension(:), intent(inout) :: arr
    ! -- local
    integer(I4B) :: nodeu
    !
    nodeu = this%get_nodeuser(noder)
    call this%nodeu_to_array(nodeu, arr)
  end subroutine noder_to_array

  !> @brief Record list header
  subroutine record_srcdst_list_entry(this, ibdchn, noder, noder2, q, &
                                      naux, aux, olconv, olconv2)
    ! -- modules
    use InputOutputModule, only: ubdsvd
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: ibdchn
    integer(I4B), intent(in) :: noder
    integer(I4B), intent(in) :: noder2
    real(DP), intent(in) :: q
    integer(I4B), intent(in) :: naux
    real(DP), dimension(naux), intent(in) :: aux
    logical, optional, intent(in) :: olconv
    logical, optional, intent(in) :: olconv2
    ! -- local
    logical :: lconv
    logical :: lconv2
    integer(I4B) :: nodeu
    integer(I4B) :: nodeu2
    !
    ! -- Use ubdsvb to write list header
    if (present(olconv)) then
      lconv = olconv
    else
      lconv = .TRUE.
    end if
    if (lconv) then
      nodeu = this%get_nodeuser(noder)
    else
      nodeu = noder
    end if
    if (present(olconv2)) then
      lconv2 = olconv2
    else
      lconv2 = .TRUE.
    end if
    if (lconv2) then
      nodeu2 = this%get_nodeuser(noder2)
    else
      nodeu2 = noder2
    end if
    call ubdsvd(ibdchn, nodeu, nodeu2, q, naux, aux)
  end subroutine record_srcdst_list_entry

  !> @brief Find the first highest active cell beneath cell n.
  subroutine highest_active(this, n, ibound)
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(inout) :: n
    integer(I4B), dimension(:), intent(in) :: ibound
    ! -- locals
    integer(I4B) :: m, ii, iis
    logical done, bottomcell
    !
    ! -- Loop through connected cells until the highest active one (including a
    !    constant head cell) is found.  Return that cell as n.
    done = .false.
    do while (.not. done)
      bottomcell = .true.
      cloop: do ii = this%con%ia(n) + 1, this%con%ia(n + 1) - 1
        m = this%con%ja(ii)
        iis = this%con%jas(ii)
        if (this%con%ihc(iis) == 0 .and. m > n) then
          !
          ! -- this cannot be a bottom cell
          bottomcell = .false.
          !
          ! -- vertical down
          if (ibound(m) /= 0) then
            n = m
            done = .true.
            exit cloop
          else
            n = m
            exit cloop
          end if
        end if
      end do cloop
      if (bottomcell) done = .true.
    end do
  end subroutine highest_active

  !> @brief Get the cell area for this node.
  function get_area(this, node) result(area)
    real(DP) :: area
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: node
    !
    ! -- Return the cell area
    area = this%area(node)
  end function get_area

  !> @brief Calculate the area factor for the cell connection
  !!
  !! Function calculates the area factor for the cell connection. The sum of
  !! all area factors for all cell connections to overlying or underlying
  !! cells cells will be 1.
  !!
  !! TODO: confirm that this works for cells that are only partially covered
  !!       by overlying or underlying cells.
  function get_area_factor(this, node, idx_conn) result(area_factor)
    real(DP) :: area_factor !< connection cell area factor
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: node !< cell node number
    integer(I4B), intent(in) :: idx_conn !< connection index
    ! -- local
    real(DP) :: area_node
    real(DP) :: area_conn
    !
    ! -- calculate the cell area fraction
    area_node = this%area(node)
    area_conn = this%con%hwva(idx_conn)
    !
    ! -- return the cell area factor
    area_factor = area_conn / area_node
  end function get_area_factor

end module BaseDisModule

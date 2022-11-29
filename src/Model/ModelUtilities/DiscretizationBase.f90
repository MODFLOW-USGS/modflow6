module BaseDisModule

  use KindModule, only: DP, I4B
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

  implicit none

  private
  public :: DisBaseType
  public :: dis_transform_xy

  type :: DisBaseType
    character(len=LENMEMPATH) :: memoryPath !< path for memory allocation
    character(len=LENMODELNAME), pointer :: name_model => null() !< name of the model
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
    procedure :: dis_df
    procedure :: dis_ac
    procedure :: dis_mc
    procedure :: dis_ar
    procedure :: dis_da
    ! -- helper functions
    !
    ! -- get_nodenumber is an overloaded integer function that will always
    !    return the reduced nodenumber.  For all grids, get_nodenumber can
    !    be passed the user nodenumber.  For some other grids, it can also
    !    be passed an index.  For dis3d the index is k, i, j, and for
    !    disv the index is k, n.
    generic :: get_nodenumber => get_nodenumber_idx1, &
      get_nodenumber_idx2, &
      get_nodenumber_idx3
    procedure :: get_nodenumber_idx1
    procedure :: get_nodenumber_idx2
    procedure :: get_nodenumber_idx3
    procedure :: get_nodeuser
    procedure :: nodeu_to_string
    procedure :: nodeu_to_array
    procedure :: nodeu_from_string
    procedure :: nodeu_from_cellid
    procedure :: noder_from_string
    procedure :: noder_from_cellid
    procedure :: connection_normal
    procedure :: connection_vector
    procedure :: get_dis_type
    procedure :: supports_layers
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: get_ncpl
    procedure :: get_cell_volume
    procedure :: write_grb
    !
    procedure :: read_int_array
    procedure :: read_dbl_array
    generic, public :: read_grid_array => read_int_array, read_dbl_array
    procedure, public :: read_layer_array
    procedure :: fill_int_array
    procedure :: fill_dbl_array
    generic, public :: fill_grid_array => fill_int_array, fill_dbl_array
    procedure, public :: read_list
    !
    procedure, public :: record_array
    procedure, public :: record_connection_array
    procedure, public :: noder_to_string
    procedure, public :: noder_to_array
    procedure, public :: record_srcdst_list_header
    procedure, private :: record_srcdst_list_entry
    generic, public :: record_mf6_list_entry => record_srcdst_list_entry
    procedure, public :: nlarray_to_nodelist
    procedure, public :: highest_active
    procedure, public :: get_area

  end type DisBaseType

contains

  subroutine dis_df(this)
! ******************************************************************************
! dis_df -- Read discretization information from DISU input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DisBaseType) :: this
! ------------------------------------------------------------------------------
    !
    call store_error('Program error: DisBaseType method dis_df not &
                     &implemented.', terminate=.TRUE.)
    !
    ! -- Return
    return
  end subroutine dis_df

  subroutine dis_ac(this, moffset, sparse)
! ******************************************************************************
! dis_ac -- Add connections to sparse based on cell connectivity
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: moffset
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    integer(I4B) :: i, j, ipos, iglo, jglo
! ------------------------------------------------------------------------------
    !
    do i = 1, this%nodes
      do ipos = this%con%ia(i), this%con%ia(i + 1) - 1
        j = this%con%ja(ipos)
        iglo = i + moffset
        jglo = j + moffset
        call sparse%addconnection(iglo, jglo, 1)
      end do
    end do
    !
    ! -- Return
    return
  end subroutine dis_ac

  subroutine dis_mc(this, moffset, idxglo, iasln, jasln)
! ******************************************************************************
! dis_mc -- Map the positions of cell connections in the numerical solution
!   coefficient matrix.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: moffset
    integer(I4B), dimension(:), intent(inout) :: idxglo
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
    integer(I4B) :: i, j, ipos, ipossln, iglo, jglo
! ------------------------------------------------------------------------------
    !
    do i = 1, this%nodes
      iglo = i + moffset
      do ipos = this%con%ia(i), this%con%ia(i + 1) - 1
        j = this%con%ja(ipos)
        jglo = j + moffset
        searchloop: do ipossln = iasln(iglo), iasln(iglo + 1) - 1
          if (jglo == jasln(ipossln)) then
            idxglo(ipos) = ipossln
            exit searchloop
          end if
        end do searchloop
      end do
    end do
    !
    ! -- Return
    return
  end subroutine dis_mc

  subroutine dis_ar(this, icelltype)
! ******************************************************************************
! dis_ar -- Called from AR procedure.  Only task is to write binary grid file.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), dimension(:), intent(in) :: icelltype
    ! -- local
    integer(I4B), dimension(:), allocatable :: ict
    integer(I4B) :: nu, nr
! ------------------------------------------------------------------------------
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
    !
    ! -- Return
    return
  end subroutine dis_ar

  subroutine write_grb(this, icelltype)
! ******************************************************************************
! write_grb -- Called from AR procedure.  Only task is to write binary grid file.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), dimension(:), intent(in) :: icelltype
    ! -- local
! ------------------------------------------------------------------------------
    !
    !
    call store_error('Program error: DisBaseType method write_grb not &
                     &implemented.', terminate=.TRUE.)
    !
    ! -- Return
    return
  end subroutine write_grb

  subroutine dis_da(this)
! ******************************************************************************
! dis_da -- Deallocate discretization object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(DisBaseType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Strings
    deallocate (this%name_model)
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
    !
    ! -- Return
    return
  end subroutine dis_da

  subroutine nodeu_to_string(this, nodeu, str)
! ******************************************************************************
! nodeu_to_string -- Convert user node number to a string in the form of
! (nodenumber) or (k,i,j)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: nodeu
    character(len=*), intent(inout) :: str
    ! -- local
! ------------------------------------------------------------------------------
    !
    call store_error('Program error: DisBaseType method nodeu_to_string not &
                     &implemented.', terminate=.TRUE.)
    !
    ! -- return
    return
  end subroutine nodeu_to_string

  subroutine nodeu_to_array(this, nodeu, arr)
! ******************************************************************************
! nodeu_to_array -- Convert user node number to cellid and fill array with
!                   (nodenumber) or (k,j) or (k,i,j)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: nodeu
    integer(I4B), dimension(:), intent(inout) :: arr
    ! -- local
! ------------------------------------------------------------------------------
    !
    call store_error('Program error: DisBaseType method nodeu_to_array not &
                     &implemented.', terminate=.TRUE.)
    !
    ! -- return
    return
  end subroutine nodeu_to_array

  function get_nodeuser(this, noder) result(nodenumber)
! ******************************************************************************
! get_nodeuser -- Return the user nodenumber from the reduced node number
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    integer(I4B) :: nodenumber
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: noder
! ------------------------------------------------------------------------------
    !
    if (this%nodes < this%nodesuser) then
      nodenumber = this%nodeuser(noder)
    else
      nodenumber = noder
    end if
    !
    ! -- return
    return
  end function get_nodeuser

  function get_nodenumber_idx1(this, nodeu, icheck) result(nodenumber)
! ******************************************************************************
! get_nodenumber -- Return a nodenumber from the user specified node number
!                   with an option to perform a check.  This subroutine
!                   can be overridden by child classes to perform mapping
!                   to a model node number
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    ! -- dummy
    class(DisBaseType), intent(in) :: this
    integer(I4B), intent(in) :: nodeu
    integer(I4B), intent(in) :: icheck
    ! -- local
    integer(I4B) :: nodenumber
! ------------------------------------------------------------------------------
    !
    nodenumber = 0
    call store_error('Program error: get_nodenumber_idx1 not implemented.', &
                     terminate=.TRUE.)
    !
    ! -- return
    return
  end function get_nodenumber_idx1

  function get_nodenumber_idx2(this, k, j, icheck) result(nodenumber)
! ******************************************************************************
! get_nodenumber_idx2 -- This function should never be called.  It must be
!   overridden by a child class.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SimModule, only: store_error
    ! -- dummy
    class(DisBaseType), intent(in) :: this
    integer(I4B), intent(in) :: k, j
    integer(I4B), intent(in) :: icheck
    integer(I4B) :: nodenumber
! ------------------------------------------------------------------------------
    !
    nodenumber = 0
    call store_error('Program error: get_nodenumber_idx2 not implemented.', &
                     terminate=.TRUE.)
    !
    ! -- Return
    return
  end function get_nodenumber_idx2

  function get_nodenumber_idx3(this, k, i, j, icheck) result(nodenumber)
! ******************************************************************************
! get_nodenumber_idx3 -- This function will not be invoked for an unstructured
! model, but it may be from a Discretization3dType model.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SimModule, only: store_error
    ! -- dummy
    class(DisBaseType), intent(in) :: this
    integer(I4B), intent(in) :: k, i, j
    integer(I4B), intent(in) :: icheck
    integer(I4B) :: nodenumber
! ------------------------------------------------------------------------------
    !
    nodenumber = 0
    call store_error('Program error: get_nodenumber_idx3 not implemented.', &
                     terminate=.TRUE.)
    !
    ! -- Return
    return
  end function get_nodenumber_idx3

  subroutine connection_normal(this, noden, nodem, ihc, xcomp, ycomp, zcomp, &
                               ipos)
! ******************************************************************************
! connection_normal -- calculate the normal vector components for reduced
!   nodenumber cell (noden) and its shared face with cell nodem.  ihc is the
!   horizontal connection flag.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: noden
    integer(I4B), intent(in) :: nodem
    integer(I4B), intent(in) :: ihc
    real(DP), intent(inout) :: xcomp
    real(DP), intent(inout) :: ycomp
    real(DP), intent(inout) :: zcomp
    integer(I4B), intent(in) :: ipos
! ------------------------------------------------------------------------------
    !
    call store_error('Program error: connection_normal not implemented.', &
                     terminate=.TRUE.)
    !
    ! -- return
    return
  end subroutine connection_normal

  subroutine connection_vector(this, noden, nodem, nozee, satn, satm, ihc, &
                               xcomp, ycomp, zcomp, conlen)
! ******************************************************************************
! connection_vector -- calculate the unit vector components from reduced
!   nodenumber cell (noden) to its neighbor cell (nodem).  The saturation for
!   for these cells are also required so that the vertical position of the cell
!   cell centers can be calculated.  ihc is the horizontal flag.  Also return
!   the straight-line connection length.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error
    ! -- dummy
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
    ! -- local
! ------------------------------------------------------------------------------
    !
    call store_error('Program error: connection_vector not implemented.', &
                     terminate=.TRUE.)
    !
    ! -- return
    return
  end subroutine connection_vector

  !> @brief get the x,y for a node transformed into
  !! 'global coordinates' using xorigin, yorigin, angrot,
  !< analogously to how flopy does this.
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

  !> @brief return discretization type
  !<
  subroutine get_dis_type(this, dis_type)
    class(DisBaseType), intent(in) :: this
    character(len=*), intent(out) :: dis_type

    ! suppress warning
    dis_type = "Not implemented"

    call store_error('Program error: get_dis_type not implemented.', &
                     terminate=.TRUE.)

  end subroutine get_dis_type

  subroutine allocate_scalars(this, name_model)
! ******************************************************************************
! allocate_scalars -- Allocate and initialize scalar variables in this class
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(DisBaseType) :: this
    character(len=*), intent(in) :: name_model
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Create memory path
    this%memoryPath = create_mem_path(name_model, 'DIS')
    !
    ! -- Allocate
    allocate (this%name_model)
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
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- Read discretization information from file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(DisBaseType) :: this
    integer :: isize
! ------------------------------------------------------------------------------
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
    call mem_allocate(this%dbuff, isize, 'DBUFF', this%name_model) ! TODO_MJR: is this correct??
    call mem_allocate(this%ibuff, isize, 'IBUFF', this%name_model)
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  function nodeu_from_string(this, lloc, istart, istop, in, iout, line, &
                             flag_string, allow_zero) result(nodeu)
! ******************************************************************************
! nodeu_from_string -- Receive a string and convert the string to a user
!   nodenumber.  The model is unstructured; just read user nodenumber.
!   If flag_string argument is present and true, the first token in string
!   is allowed to be a string (e.g. boundary name). In this case, if a string
!   is encountered, return value as -2.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
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
    ! -- local
! ------------------------------------------------------------------------------
    !
    !
    nodeu = 0
    call store_error('Program error: DisBaseType method nodeu_from_string &
                     &not implemented.', terminate=.TRUE.)
    !
    ! -- return
    return
  end function nodeu_from_string

  function nodeu_from_cellid(this, cellid, inunit, iout, flag_string, &
                             allow_zero) result(nodeu)
! ******************************************************************************
! nodeu_from_cellid -- Receive cellid as a string and convert the string to a
!   user nodenumber.
!   If flag_string argument is present and true, the first token in string
!   is allowed to be a string (e.g. boundary name). In this case, if a string
!   is encountered, return value as -2.
!   If allow_zero argument is present and true, if all indices equal zero, the
!   result can be zero. If allow_zero is false, a zero in any index causes an
!   error.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DisBaseType) :: this
    character(len=*), intent(inout) :: cellid
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    logical, optional, intent(in) :: flag_string
    logical, optional, intent(in) :: allow_zero
    integer(I4B) :: nodeu
! ------------------------------------------------------------------------------
    !
    nodeu = 0
    call store_error('Program error: DisBaseType method nodeu_from_cellid &
                      &not implemented.', terminate=.TRUE.)
    !
    ! -- return
    return
  end function nodeu_from_cellid

  function noder_from_string(this, lloc, istart, istop, in, iout, line, &
                             flag_string) result(noder)
! ******************************************************************************
! noder_from_string -- Receive a string and convert the string to a reduced
!   nodenumber.  The model is unstructured; just read user nodenumber.
!   If flag_string argument is present and true, the first token in string
!   is allowed to be a string (e.g. boundary name). In this case, if a string
!   is encountered, return value as -2.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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
! ------------------------------------------------------------------------------
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
    !
    ! -- return
    return
  end function noder_from_string

  function noder_from_cellid(this, cellid, inunit, iout, flag_string, &
                             allow_zero) result(noder)
! ******************************************************************************
! noder_from_cellid -- Receive cellid as a string and convert it to a reduced
!   nodenumber.
!   If flag_string argument is present and true, the first token in string
!   is allowed to be a string (e.g. boundary name). In this case, if a string
!   is encountered, return value as -2.
!   If allow_zero argument is present and true, if all indices equal zero, the
!   result can be zero. If allow_zero is false, a zero in any index causes an
!   error.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
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
! ------------------------------------------------------------------------------
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
    !
    ! -- return
    return
  end function noder_from_cellid

  logical function supports_layers(this)
! ******************************************************************************
! supports_layers
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DisBaseType) :: this
! ------------------------------------------------------------------------------
    !
    !
    supports_layers = .false.
    call store_error('Program error: DisBaseType method supports_layers not &
                     &implemented.', terminate=.TRUE.)
    return
  end function supports_layers

  function get_ncpl(this)
! ******************************************************************************
! get_ncpl -- Return number of cells per layer.  This is nodes
!   for a DISU grid, as there are no layers.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    integer(I4B) :: get_ncpl
    ! -- dummy
    class(DisBaseType) :: this
! ------------------------------------------------------------------------------
    !
    !
    get_ncpl = 0
    call store_error('Program error: DisBaseType method get_ncpl not &
                     &implemented.', terminate=.TRUE.)
    !
    ! -- Return
    return
  end function get_ncpl

  function get_cell_volume(this, n, x)
! ******************************************************************************
! get_cell_volume -- Return volume of cell n based on x value passed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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
! ------------------------------------------------------------------------------
    !
    get_cell_volume = DZERO
    tp = this%top(n)
    bt = this%bot(n)
    sat = sQuadraticSaturation(tp, bt, x)
    thick = (tp - bt) * sat
    get_cell_volume = this%area(n) * thick
    !
    ! -- Return
    return
  end function get_cell_volume

  subroutine read_int_array(this, line, lloc, istart, istop, iout, in, &
                            iarray, aname)
! ******************************************************************************
! read_int_array -- Read a GWF integer array
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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
    !
    ! -- return
    return
  end subroutine read_int_array

  subroutine read_dbl_array(this, line, lloc, istart, istop, iout, in, &
                            darray, aname)
! ******************************************************************************
! read_dbl_array -- Read a GWF double precision array
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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
    !
    ! -- return
    return
  end subroutine read_dbl_array

  subroutine fill_int_array(this, ibuff1, ibuff2)
! ******************************************************************************
! fill_dbl_array -- Fill a GWF integer array
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DisBaseType), intent(inout) :: this
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: ibuff1
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: ibuff2
    ! -- local
    integer(I4B) :: nodeu
    integer(I4B) :: noder
! ------------------------------------------------------------------------------
    do nodeu = 1, this%nodesuser
      noder = this%get_nodenumber(nodeu, 0)
      if (noder <= 0) cycle
      ibuff2(noder) = ibuff1(nodeu)
    end do
    !
    ! -- return
    return
  end subroutine fill_int_array

  subroutine fill_dbl_array(this, buff1, buff2)
! ******************************************************************************
! fill_dbl_array -- Fill a GWF double precision array
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DisBaseType), intent(inout) :: this
    real(DP), dimension(:), pointer, contiguous, intent(in) :: buff1
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: buff2
    ! -- local
    integer(I4B) :: nodeu
    integer(I4B) :: noder
! ------------------------------------------------------------------------------
    do nodeu = 1, this%nodesuser
      noder = this%get_nodenumber(nodeu, 0)
      if (noder <= 0) cycle
      buff2(noder) = buff1(nodeu)
    end do
    !
    ! -- return
    return
  end subroutine fill_dbl_array

  subroutine read_list(this, in, iout, iprpak, nlist, inamedbound, &
                       iauxmultcol, nodelist, rlist, auxvar, auxname, &
                       boundname, label, pkgname, tsManager, iscloc, &
                       indxconvertflux)
! ******************************************************************************
! read_list -- Read a list using the list reader object.
!              Convert user node numbers to reduced numbers.
!              Terminate if any nodenumbers are within an inactive domain.
!              Set up time series and multiply by iauxmultcol if it exists.
!              Write the list to iout if iprpak is set.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBOUNDNAME, LINELENGTH
    use ListReaderModule, only: ListReaderType
    use SimModule, only: store_error, store_error_unit, count_errors
    use InputOutputModule, only: urword
    use TimeSeriesLinkModule, only: TimeSeriesLinkType
    use TimeSeriesManagerModule, only: read_value_or_time_series
    ! -- dummy
    class(DisBaseType) :: this
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
    !character(len=:), dimension(:), pointer, contiguous, intent(inout) :: auxname
    !character(len=:), dimension(:), pointer, contiguous, intent(inout) :: boundname
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
! ------------------------------------------------------------------------------
    !
    ! -- Read the list
    call lstrdobj%read_list(in, iout, nlist, inamedbound, this%mshape, &
                            nodelist, rlist, auxvar, auxname, boundname, label)
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
    !
    ! -- return
  end subroutine read_list

  subroutine read_layer_array(this, nodelist, darray, ncolbnd, maxbnd, &
                              icolbnd, aname, inunit, iout)
! ******************************************************************************
! read_layer_array -- Read a 2d double array into col icolbnd of darray.
!                     For cells that are outside of the active domain,
!                     do not copy the array value into darray.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: ncolbnd
    integer(I4B), intent(in) :: maxbnd
    integer(I4B), dimension(maxbnd) :: nodelist
    real(DP), dimension(ncolbnd, maxbnd), intent(inout) :: darray
    integer(I4B), intent(in) :: icolbnd
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    !
    !
    errmsg = 'Programmer error: read_layer_array needs to be overridden &
            &in any DIS type that extends DisBaseType'
    call store_error(errmsg, terminate=.TRUE.)
    !
    ! -- return
  end subroutine read_layer_array

  subroutine record_array(this, darray, iout, iprint, idataun, aname, &
                          cdatafmp, nvaluesp, nwidthp, editdesc, dinact)
! ******************************************************************************
! record_array -- Record a double precision array.  The array will be
!   printed to an external file and/or written to an unformatted external file
!   depending on the argument specifications.
! ******************************************************************************
!
!    SPECIFICATIONS:
!      darray is the double precision array to record
!      iout is the unit number for ascii output
!      iprint is a flag indicating whether or not to print the array
!      idataun is the unit number to which the array will be written in binary
!        form; if negative then do not write by layers, write entire array
!      aname is the text descriptor of the array
!      cdatafmp is the fortran format for writing the array
!      nvaluesp is the number of values per line for printing
!      nwidthp is the width of the number for printing
!      editdesc is the format type (I, G, F, S, E)
!      dinact is the double precision value to use for cells that are excluded
!        from the model domain
! ------------------------------------------------------------------------------
    ! -- dummy
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
    !
    ! --
    errmsg = 'Programmer error: record_array needs to be overridden &
            &in any DIS type that extends DisBaseType'
    call store_error(errmsg, terminate=.TRUE.)
    !
  end subroutine record_array

  subroutine record_connection_array(this, flowja, ibinun, iout)
! ******************************************************************************
! record_connection_array -- Record a connection-based double precision
! array for either a structured or an unstructured grid.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DisBaseType) :: this
    real(DP), dimension(:), intent(in) :: flowja
    integer(I4B), intent(in) :: ibinun
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=16), dimension(1) :: text
    ! -- data
    data text(1)/'    FLOW-JA-FACE'/
! ------------------------------------------------------------------------------
    !
    ! -- write full ja array
    call ubdsv1(kstp, kper, text(1), ibinun, flowja, size(flowja), 1, 1, &
                iout, delt, pertim, totim)
    !
    ! -- return
    return
  end subroutine record_connection_array

  subroutine noder_to_string(this, noder, str)
! ******************************************************************************
! noder_to_string -- Convert reduced node number to a string in the form of
! (nodenumber) or (k,i,j)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: noder
    character(len=*), intent(inout) :: str
    ! -- local
    integer(I4B) :: nodeu
! ------------------------------------------------------------------------------
    !
    nodeu = this%get_nodeuser(noder)
    call this%nodeu_to_string(nodeu, str)
    !
    ! -- return
    return
  end subroutine noder_to_string

  subroutine noder_to_array(this, noder, arr)
! ******************************************************************************
! noder_to_array -- Convert reduced node number to cellid and fill array with
!                   (nodenumber) or (k,j) or (k,i,j)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: noder
    integer(I4B), dimension(:), intent(inout) :: arr
    ! -- local
    integer(I4B) :: nodeu
! ------------------------------------------------------------------------------
    !
    nodeu = this%get_nodeuser(noder)
    call this%nodeu_to_array(nodeu, arr)
    !
    ! -- return
    return
  end subroutine noder_to_array

  subroutine record_srcdst_list_header(this, text, textmodel, textpackage, &
                                       dstmodel, dstpackage, naux, auxtxt, &
                                       ibdchn, nlist, iout)
! ******************************************************************************
! record_srcdst_list_header -- Record list header for imeth=6
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
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
    !
    ! --
    errmsg = 'Programmer error: record_srcdst_list_header needs to be &
            &overridden in any DIS type that extends DisBaseType'
    call store_error(errmsg, terminate=.TRUE.)
    !
    ! -- return
    return
  end subroutine record_srcdst_list_header

  subroutine record_srcdst_list_entry(this, ibdchn, noder, noder2, q, &
                                      naux, aux, olconv, olconv2)
! ******************************************************************************
! record_srcdst_list_header -- Record list header
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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
! ------------------------------------------------------------------------------
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
    !
    ! -- return
    return
  end subroutine record_srcdst_list_entry

  subroutine nlarray_to_nodelist(this, nodelist, maxbnd, nbound, aname, &
                                 inunit, iout)
! ******************************************************************************
! nlarray_to_nodelist -- Read an integer array into nodelist. For structured
!                        model, integer array is layer number; for unstructured
!                        model, integer array is node number.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: maxbnd
    integer(I4B), dimension(maxbnd), intent(inout) :: nodelist
    integer(I4B), intent(inout) :: nbound
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    !
    ! --
    errmsg = 'Programmer error: nlarray_to_nodelist needs to be &
            &overridden in any DIS type that extends DisBaseType'
    call store_error(errmsg, terminate=.TRUE.)
    !
    ! -- return
    return
  end subroutine nlarray_to_nodelist

  subroutine highest_active(this, n, ibound)
! ******************************************************************************
! highest_active -- Find the first highest active cell beneath cell n
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(inout) :: n
    integer(I4B), dimension(:), intent(in) :: ibound
    ! -- locals
    integer(I4B) :: m, ii, iis
    logical done, bottomcell
! ------------------------------------------------------------------------------
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
    !
    ! -- return
    return
  end subroutine highest_active

  function get_area(this, node) result(area)
! ******************************************************************************
! get_area -- Return the cell area for this node
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: area
    ! -- dummy
    class(DisBaseType) :: this
    integer(I4B), intent(in) :: node
! ------------------------------------------------------------------------------
    !
    ! -- Return the cell area
    area = this%area(node)
    !
    ! -- return
    return
  end function get_area

end module BaseDisModule

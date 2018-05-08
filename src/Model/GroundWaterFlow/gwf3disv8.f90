module GwfDisvModule

  use ArrayReadersModule, only: ReadArray
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH
  use BaseDisModule, only: DisBaseType
  use InputOutputModule, only: get_node, URWORD, ulasav, ulaprufw, ubdsv1, &
                               ubdsv06
  use SimModule, only: count_errors, store_error, store_error_unit, ustop
  use DisvGeom, only: DisvGeomType
  use BlockParserModule, only: BlockParserType
  use MemoryManagerModule, only: mem_allocate
  use TdisModule,          only: kstp, kper, pertim, totim, delt

  implicit none
  private
  public disv_cr, GwfDisvType

  type, extends(DisBaseType) :: GwfDisvType
    integer(I4B), pointer                          :: nlay        => null()     ! number of layers
    integer(I4B), pointer                          :: ncpl        => null()     ! number of cells per layer
    integer(I4B), pointer                          :: nvert       => null()     ! number of x,y vertices
    integer(I4B), dimension(:), pointer            :: nodereduced => null()     ! (size:nodesuser)contains reduced nodenumber (size 0 if not reduced); -1 means vertical pass through, 0 is idomain = 0
    integer(I4B), dimension(:), pointer            :: nodeuser    => null()     ! (size:nodes) given a reduced nodenumber, provide the user nodenumber (size 0 if not reduced)
    real(DP), dimension(:,:), pointer              :: vertices    => null()     ! cell vertices stored as 2d array of x and y
    real(DP), dimension(:,:), pointer              :: cellxy      => null()     ! cell center stored as 2d array of x and y
    integer(I4B), dimension(:), pointer            :: iavert      => null()     ! cell vertex pointer ia array
    integer(I4B), dimension(:), pointer            :: javert      => null()     ! cell vertex pointer ja array
    real(DP), dimension(:, :, :), pointer          :: botm        => null()     ! top and bottom elevations for each cell (ncpl, 1, 0:nlay)
    integer(I4B), dimension(:, :, :), pointer      :: idomain     => null()     ! idomain (ncpl, 1, nlay)
    type(DisvGeomType)                             :: cell1                     ! cell object used to calculate geometric properties
    type(DisvGeomType)                             :: cell2                     ! cell object used to calculate geometric properties
  contains
    procedure :: dis_df => disv_df
    procedure :: dis_da => disv_da
    procedure, public :: record_array
    procedure, public :: read_layer_array
    procedure, public :: record_srcdst_list_header
    procedure, public :: nlarray_to_nodelist
    ! -- helper functions
    procedure :: get_nodenumber_idx1
    procedure :: get_nodenumber_idx2
    procedure :: get_nodeuser
    procedure :: nodeu_to_string
    procedure :: nodeu_from_string
    procedure :: nodeu_from_cellid
    procedure :: connection_normal
    procedure :: connection_vector
    procedure :: supports_layers
    procedure :: get_ncpl
    ! -- private
    procedure :: read_options
    procedure :: read_dimensions
    procedure :: read_vertices
    procedure :: read_cell2d
    procedure :: read_griddata
    procedure :: connect
    procedure :: write_grb
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: get_cell2d_area
    !
    procedure :: read_int_array
    procedure :: read_dbl_array
    !
  end type GwfDisvType

  contains

  subroutine disv_cr(dis, name_model, inunit, iout)
! ******************************************************************************
! disv_cr -- Create a new discretization by vertices object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(DisBaseType), pointer :: dis
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwfDisvType), pointer :: disnew
! ------------------------------------------------------------------------------
    allocate(disnew)
    dis => disnew
    call disnew%allocate_scalars(name_model)
    dis%inunit = inunit
    dis%iout = iout
    !
    ! -- Initialize block parser
    call dis%parser%Initialize(dis%inunit, dis%iout)
    !
    ! -- Return
    return
  end subroutine disv_cr

  subroutine disv_df(this)
! ******************************************************************************
! read_from_file -- Allocate and read discretization information
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfDisvType) :: this
    ! -- locals
! ------------------------------------------------------------------------------
    !
    ! -- Identify
    write(this%iout,1) this%inunit
  1 format(1X,/1X,'DISV -- VERTEX GRID DISCRETIZATION PACKAGE,',            &
                  ' VERSION 1 : 12/23/2015 - INPUT READ FROM UNIT ',I0,//)
    !
    ! -- Read options
    call this%read_options()
    !
    ! -- Read dimensions block
    call this%read_dimensions()
    !
    ! -- Read GRIDDATA block
    call this%read_griddata()
    !
    ! -- Read VERTICES block
    call this%read_vertices()
    !
    ! -- Read CELL2D block
    call this%read_cell2d()
    !
    ! -- Build connections
    call this%connect()
    !
    ! -- Create two cell objects that can be used for geometric processing
    call this%cell1%init(this%nlay, this%ncpl, this%nodes, this%top, this%bot, &
                    this%iavert, this%javert, this%vertices, this%cellxy,      &
                    this%nodereduced, this%nodeuser)
    call this%cell2%init(this%nlay, this%ncpl, this%nodes, this%top, this%bot, &
                    this%iavert, this%javert, this%vertices, this%cellxy,      &
                    this%nodereduced, this%nodeuser)
    !
    ! -- Return
    return
  end subroutine disv_df

  subroutine disv_da(this)
! ******************************************************************************
! disv_da -- Deallocate discretization data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwfDisvType) :: this
    ! -- locals
! ------------------------------------------------------------------------------
    !
    ! -- DisBaseType deallocate
    call this%DisBaseType%dis_da()
    !
    ! -- Deallocate scalars
    call mem_deallocate(this%nlay)
    call mem_deallocate(this%ncpl)
    call mem_deallocate(this%nvert)
    !
    ! -- Deallocate Arrays
    call mem_deallocate(this%nodereduced)
    call mem_deallocate(this%nodeuser)
    call mem_deallocate(this%vertices)
    call mem_deallocate(this%cellxy)
    call mem_deallocate(this%iavert)
    call mem_deallocate(this%javert)
    deallocate(this%botm)
    deallocate(this%idomain)
    !
    ! -- Return
    return
  end subroutine disv_da

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfDisvType) :: this
    ! -- locals
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- set default options
      this%lenuni = 0
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(/,1x,a)')'PROCESSING DISCRETIZATION OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('LENGTH_UNITS')
            call this%parser%GetStringCaps(keyword)
            if(keyword=='FEET') then
              this%lenuni = 1
              write(this%iout,'(4x,a)') 'MODEL LENGTH UNIT IS FEET'
            elseif(keyword=='METERS') then
              this%lenuni = 2
              write(this%iout,'(4x,a)') 'MODEL LENGTH UNIT IS METERS'
            elseif(keyword=='CENTIMETERS') then
              this%lenuni = 3
              write(this%iout,'(4x,a)') 'MODEL LENGTH UNIT IS CENTIMETERS'
            else
              write(this%iout,'(4x,a)')'UNKNOWN UNIT: ',trim(keyword)
              write(this%iout,'(4x,a)')'SETTING TO: ','UNDEFINED'
            endif
          case('NOGRB')
            write(this%iout,'(4x,a)') 'BINARY GRB FILE WILL NOT BE WRITTEN'
            this%writegrb = .false.
          case('XORIGIN')
            this%xorigin = this%parser%GetDouble()
            write(this%iout,'(4x,a,1pg24.15)') 'XORIGIN SPECIFIED AS ',        &
                                              this%xorigin
          case('YORIGIN')
            this%yorigin = this%parser%GetDouble()
            write(this%iout,'(4x,a,1pg24.15)') 'YORIGIN SPECIFIED AS ',        &
                        this%yorigin
          case('ANGROT')
            this%angrot = this%parser%GetDouble()
            write(this%iout,'(4x,a,1pg24.15)') 'ANGROT SPECIFIED AS ',         &
              this%angrot
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN DIS OPTION: ',         &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
    else
      write(this%iout,'(1x,a)')'NO DISV OPTION BLOCK DETECTED.'
    end if
    if(this%lenuni==0) write(this%iout,'(3x,a)') 'MODEL LENGTH UNIT IS UNDEFINED'
    if(isfound) then
      write(this%iout,'(1x,a)')'END OF DISCRETIZATION OPTIONS'
    endif
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_dimensions(this)
! ******************************************************************************
! read_dimensions -- Read dimensions
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule,  only: LINELENGTH
    ! -- dummy
    class(GwfDisvType) :: this
    ! -- locals
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get dimensions block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse dimensions block if detected
    if (isfound) then
      write(this%iout,'(/,1x,a)')'PROCESSING DISCRETIZATION DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NLAY')
            this%nlay = this%parser%GetInteger()
            write(this%iout,'(3x,a,i0)')'NLAY = ', this%nlay
          case ('NCPL')
            this%ncpl  = this%parser%GetInteger()
            write(this%iout,'(3x,a,i0)')'NCPL = ', this%ncpl
          case ('NVERT')
            this%nvert = this%parser%GetInteger()
            write(this%iout,'(3x,a,i0)')'NVERT = ', this%nvert
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN DIS DIMENSION: ',      &
                                      trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- verify dimensions were set
    if(this%nlay < 1) then
      call store_error( &
          'ERROR.  NLAY WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.')
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    if(this%ncpl < 1) then
      call store_error( &
          'ERROR.  NCPL WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.')
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    if(this%nvert < 1) then
      call store_error( &
          'ERROR.  NVERT WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.')
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    write(this%iout,'(1x,a)')'END OF DISCRETIZATION DIMENSIONS'
    !
    ! -- Calculate nodesuser
    this%nodesuser = this%nlay * this%ncpl
    !
    ! -- Return
    return
  end subroutine read_dimensions

  subroutine read_griddata(this)
! ******************************************************************************
! read_griddata -- Read cell information (TOP, BOTM, IDOMAIN)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: ustop, count_errors, store_error
    use ConstantsModule,   only: LINELENGTH, DZERO
    ! -- dummy
    class(GwfDisvType) :: this
    ! -- locals
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: n, node, noder, j, k
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    real(DP) :: dz
    integer(I4B), parameter :: nname = 3
    logical, dimension(nname) :: lname
    character(len=24),dimension(nname) :: aname
    character(len=300) :: ermsg
    ! -- formats
    character(len=*), parameter :: fmtdz = &
      "('ERROR. CELL (',i0,',',i0,') THICKNESS <= 0. ', " //             &
      "'TOP, BOT: ',2(1pg24.15))"
    character(len=*), parameter :: fmtnr = &
      "(/1x, 'THE SPECIFIED IDOMAIN RESULTS IN A REDUCED NUMBER OF CELLS.'," // &
      "/1x, 'NUMBER OF USER NODES: ',I7," // &
      "/1X, 'NUMBER OF NODES IN SOLUTION: ', I7, //)"
    ! -- data
    data aname(1) /'TOP ELEVATION OF LAYER 1'/
    data aname(2) /'  MODEL LAYER BOTTOM EL.'/
    data aname(3) /'                 IDOMAIN'/
! ------------------------------------------------------------------------------
    !
    ! -- Allocate botm here (cannot use mem manager because starts at 0)
    allocate(this%botm(this%ncpl, 1, 0:this%nlay))
    allocate(this%idomain(this%ncpl, 1, this%nlay))
    !
    ! --Read DISDATA block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    lname(:) = .false.
    if(isfound) then
      write(this%iout,'(/,1x,a)')'PROCESSING GRIDDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('TOP')
            call ReadArray(this%parser%iuactive, this%botm(:, :, 0),          &
                            aname(1), this%ndim, this%ncpl, 1, this%iout, 0)
            lname(1) = .true.
          case ('BOTM')
            call this%parser%GetStringCaps(keyword)
            if (keyword.EQ.'LAYERED') then
              call ReadArray(this%parser%iuactive,                            &
                              this%botm(:,:,1:this%nlay), aname(2), this%ndim, &
                              this%ncpl, 1, this%nlay, this%iout, 1, this%nlay)
            else
              call ReadArray(this%parser%iuactive,                            &
                              this%botm(:, :, 1:this%nlay), aname(2),          &
                              this%ndim, this%nodesuser, 1, 1, this%iout, 0, 0)
            end if
            lname(2) = .true.
          case ('IDOMAIN')
            call this%parser%GetStringCaps(keyword)
            if (keyword.EQ.'LAYERED') then
              call ReadArray(this%parser%iuactive, this%idomain, aname(3),    &
                              this%ndim, this%ncpl, 1, this%nlay, this%iout,   &
                              1, this%nlay)
            else
              call ReadArray(this%parser%iuactive, this%idomain, aname(3),    &
                              this%ndim, this%nodesuser, 1, 1, this%iout,      &
                              0, 0)
            end if
            lname(3) = .true.
          case default
            write(ermsg,'(4x,a,a)')'ERROR. UNKNOWN GRIDDATA TAG: ',            &
                                     trim(keyword)
            call store_error(ermsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END PROCESSING GRIDDATA'
    else
      call store_error('ERROR.  REQUIRED GRIDDATA BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Verify all required items were read (IDOMAIN not required)
    do n = 1, nname - 1
      if(.not. lname(n)) then
        write(ermsg,'(1x,a,a)') &
          'ERROR.  REQUIRED INPUT WAS NOT SPECIFIED: ',aname(n)
        call store_error(ermsg)
      endif
    enddo
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- If IDOMAIN was not read, then set all values to 1, otherwise
    !    count active cells
    if(.not. lname(3)) then
      do k = 1, this%nlay
        do j = 1, this%ncpl
          this%idomain(j, 1, k) = 1
        enddo
      enddo
      this%nodes = this%nodesuser
    else
      this%nodes = 0
      do k = 1, this%nlay
        do j = 1, this%ncpl
          if(this%idomain(j, 1, k) > 0) this%nodes = this%nodes + 1
        enddo
      enddo
    endif
    !
    ! -- Check cell thicknesses
    do k = 1, this%nlay
      do j = 1, this%ncpl
        if (this%idomain(j, 1, k) == 0) cycle
        if (this%idomain(j, 1, k) > 0) then
          dz = this%botm(j, 1, k - 1) - this%botm(j, 1, k)
          if (dz <= DZERO) then
            write(ermsg, fmt=fmtdz) k, j, this%botm(j, 1, k - 1),              &
                                    this%botm(j, 1, k)
            call store_error(ermsg)
          endif
        endif
      enddo
    enddo
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Write message if reduced grid
    if(this%nodes < this%nodesuser) then
      write(this%iout, fmtnr) this%nodesuser, this%nodes
    endif
    !
    ! -- Array size is now known, so allocate
    call this%allocate_arrays()
    !
    ! -- Fill the nodereduced array with the reduced nodenumber, or
    !    a negative number to indicate it is a pass-through cell, or
    !    a zero to indicate that the cell is excluded from the
    !    solution.
    if(this%nodes < this%nodesuser) then
      node = 1
      noder = 1
      do k = 1, this%nlay
        do j = 1, this%ncpl
          if(this%idomain(j, 1, k) > 0) then
            this%nodereduced(node) = noder
            noder = noder + 1
          elseif(this%idomain(j, 1, k) < 0) then
            this%nodereduced(node) = -1
          else
            this%nodereduced(node) = 0
          endif
          node = node + 1
        enddo
      enddo
    endif
    !
    ! -- allocate and fill nodeuser if a reduced grid
    if(this%nodes < this%nodesuser) then
      node = 1
      noder = 1
      do k = 1, this%nlay
        do j = 1, this%ncpl
          if(this%idomain(j, 1, k) > 0) then
            this%nodeuser(noder) = node
            noder = noder + 1
          endif
          node = node + 1
        enddo
      enddo
    endif
    !
    ! -- Move botm into top and bot, and calculate area
    node = 0
    do k = 1, this%nlay
      do j = 1, this%ncpl
        node = node + 1
        noder = node
        if(this%nodes < this%nodesuser) noder = this%nodereduced(node)
        if(noder <= 0) cycle
        this%top(noder) = this%botm(j, 1, k - 1)
        this%bot(noder) = this%botm(j, 1, k)
      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine read_griddata

  subroutine read_vertices(this)
! ******************************************************************************
! read_vertices -- Read data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: ustop, count_errors, store_error
    use ConstantsModule,   only: LINELENGTH, DZERO
    ! -- dummy
    class(GwfDisvType) :: this
    integer(I4B) :: i
    integer(I4B) :: ierr, ival
    logical :: isfound, endOfBlock
    real(DP) :: xmin, xmax, ymin, ymax
    character(len=300) :: ermsg
    ! -- formats
    character(len=*), parameter :: fmtvnum = &
      "('ERROR. VERTEX NUMBER NOT CONSECUTIVE.  LOOKING FOR ',i0," //           &
      "' BUT FOUND ', i0)"
    character(len=*), parameter :: fmtnvert = &
      "(3x, 'SUCCESSFULLY READ ',i0,' (X,Y) COORDINATES')"
    character(len=*), parameter :: fmtcoord = &
      "(3x, a,' COORDINATE = ', 1(1pg24.15))"
! ------------------------------------------------------------------------------
    !
    ! -- Calculates nodesuser
    this%nodesuser = this%nlay * this%ncpl
    !
    ! --Read DISDATA block
    call this%parser%GetBlock('VERTICES', isfound, ierr, &
                              supportOpenClose=.true.)
    if(isfound) then
      write(this%iout,'(/,1x,a)') 'PROCESSING VERTICES'
      do i = 1, this%nvert
        call this%parser%GetNextLine(endOfBlock)
        !
        ! -- vertex number
        ival = this%parser%GetInteger()
        if(ival /= i) then
          write(ermsg, fmtvnum) i, ival
          call store_error(ermsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        !
        ! -- x
        this%vertices(1, i) = this%parser%GetDouble()
        !
        ! -- y
        this%vertices(2, i) = this%parser%GetDouble()
        !
        ! -- set min/max coords
        if(i == 1) then
          xmin = this%vertices(1, i)
          xmax = xmin
          ymin = this%vertices(2, i)
          ymax = ymin
        else
          xmin = min(xmin, this%vertices(1, i))
          xmax = max(xmax, this%vertices(1, i))
          ymin = min(ymin, this%vertices(2, i))
          ymax = max(ymax, this%vertices(2, i))
        endif
      enddo
      !
      ! -- Terminate the block
      call this%parser%terminateblock()
    else
      call store_error('ERROR.  REQUIRED VERTICES BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Write information
    write(this%iout, fmtnvert) this%nvert
    write(this%iout, fmtcoord) 'MINIMUM X', xmin
    write(this%iout, fmtcoord) 'MAXIMUM X', xmax
    write(this%iout, fmtcoord) 'MINIMUM Y', ymin
    write(this%iout, fmtcoord) 'MAXIMUM Y', ymax
    write(this%iout,'(1x,a)')'END PROCESSING VERTICES'
    !
    ! -- Return
    return
  end subroutine read_vertices

  subroutine read_cell2d(this)
! ******************************************************************************
! read_cell2d -- Read information describing the two dimensional (x, y)
!   configuration of each cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: ustop, count_errors, store_error
    use ConstantsModule,   only: LINELENGTH, DZERO
    use InputOutputModule, only: urword
    use SparseModule, only: sparsematrix
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfDisvType) :: this
    integer(I4B) :: i, j, ivert, ivert1, ncvert
    integer(I4B) :: ierr, ival
    logical :: isfound, endOfBlock
    integer(I4B) :: maxvert, maxvertcell, iuext
    real(DP) :: xmin, xmax, ymin, ymax
    character(len=300) :: ermsg
    integer(I4B), dimension(:), allocatable :: maxnnz
    type(sparsematrix) :: vertspm
    ! -- formats
    character(len=*), parameter :: fmtcnum = &
      "('ERROR. CELL NUMBER NOT CONSECUTIVE.  LOOKING FOR ',i0," //           &
      "' BUT FOUND ', i0)"
    character(len=*), parameter :: fmtncpl = &
      "(3x, 'SUCCESSFULLY READ ',i0,' CELL2D INFORMATION ENTRIES')"
    character(len=*), parameter :: fmtcoord = &
      "(3x, a,' CELL CENTER = ', 1(1pg24.15))"
    character(len=*), parameter :: fmtmaxvert = &
      "(3x, 'MAXIMUM NUMBER OF CELL2D VERTICES IS ',i0,' FOR CELL ', i0)"
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    maxvert = 0
    maxvertcell = 0
    !
    ! -- Initialize estimate of the max number of vertices for each cell
    !    (using 5 as default) and initialize the sparse matrix, which will
    !    temporarily store the vertex numbers for each cell.  This will
    !    be converted to iavert and javert after all cell vertices have
    !    been read.
    allocate(maxnnz(this%ncpl))
    do i = 1, this%ncpl
      maxnnz(i) = 5
    enddo
    call vertspm%init(this%ncpl, this%nvert, maxnnz)
    !
    ! --Read CELL2D block
    call this%parser%GetBlock('CELL2D', isfound, ierr, supportOpenClose=.true.)
    if(isfound) then
      write(this%iout,'(/,1x,a)') 'PROCESSING CELL2D'
      do i = 1, this%ncpl
        call this%parser%GetNextLine(endOfBlock)
        !
        ! -- cell number
        ival = this%parser%GetInteger()
        if(ival /= i) then
          write(ermsg, fmtcnum) i, ival
          call store_error(ermsg)
          call store_error_unit(iuext)
          call ustop()
        endif
        !
        ! -- Cell x center
        this%cellxy(1, i) = this%parser%GetDouble()
        !
        ! -- Cell y center
        this%cellxy(2, i) = this%parser%GetDouble()
        !
        ! -- Number of vertices for this cell
        ncvert = this%parser%GetInteger()
        if(ncvert > maxvert) then
          maxvert = ncvert
          maxvertcell = i
        endif
        !
        ! -- Read each vertex number, and then close the polygon if
        !    the last vertex does not equal the first vertex
        do j = 1, ncvert
          ivert = this%parser%GetInteger()
          call vertspm%addconnection(i, ivert, 0)
          !
          ! -- If necessary, repeat the last vertex in order to close the cell
          if(j == 1) then
            ivert1 = ivert
          elseif(j == ncvert) then
            if(ivert1 /= ivert) then
              call vertspm%addconnection(i, ivert1, 0)
            endif
          endif
        enddo
        !
        ! -- set min/max coords
        if(i == 1) then
          xmin = this%cellxy(1, i)
          xmax = xmin
          ymin = this%cellxy(2, i)
          ymax = ymin
        else
          xmin = min(xmin, this%cellxy(1, i))
          xmax = max(xmax, this%cellxy(1, i))
          ymin = min(ymin, this%cellxy(2, i))
          ymax = max(ymax, this%cellxy(2, i))
        endif
      enddo
      !
      ! -- Terminate the block
      call this%parser%terminateblock()
    else
      call store_error('ERROR.  REQUIRED CELL2D BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Convert vertspm into ia/ja form
    call mem_allocate(this%iavert, this%ncpl+1, 'IAVERT', this%origin)
    call mem_allocate(this%javert, vertspm%nnz, 'JAVERT', this%origin)
    call vertspm%filliaja(this%iavert, this%javert, ierr)
    call vertspm%destroy()
    !
    ! -- Write information
    write(this%iout, fmtncpl) this%ncpl
    write(this%iout, fmtcoord) 'MINIMUM X', xmin
    write(this%iout, fmtcoord) 'MAXIMUM X', xmax
    write(this%iout, fmtcoord) 'MINIMUM Y', ymin
    write(this%iout, fmtcoord) 'MAXIMUM Y', ymax
    write(this%iout, fmtmaxvert) maxvert, maxvertcell
    write(this%iout,'(1x,a)')'END PROCESSING VERTICES'
    !
    ! -- Return
    return
  end subroutine read_cell2d

  subroutine connect(this)
! ******************************************************************************
! connect -- Build grid connections
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfDisvType) :: this
    ! -- local
    integer(I4B) :: j, k
    integer(I4B) :: noder, nrsize
    real(DP) :: area
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- Assign the cell area
    do j = 1, this%ncpl
      area = this%get_cell2d_area(j)
      do k = 1, this%nlay
        noder = this%get_nodenumber(k, j, 0)
        if(noder > 0) this%area(noder) = area
      enddo
      if (area < 0) then
        write(errmsg, '(a,i0)') 'ERROR. CELL2D AREA LESS THAN ZERO FOR CELL ', j
        call store_error(errmsg)
      endif
    enddo
    !
    ! -- check for errors
    if(count_errors() > 0) then
      write(errmsg, '(a)') 'CELL VERTICES MUST BE LISTED IN CLOCKWISE ORDER. '
      call store_error(errmsg)
      call store_error_unit(this%inunit)
      call ustop()
    endif
    !
    ! -- create and fill the connections object
    nrsize = 0
    if(this%nodes < this%nodesuser) nrsize = this%nodes
    allocate(this%con)
    call this%con%disvconnections(this%name_model, this%nodes,                 &
                                  this%ncpl, this%nlay, nrsize,                &
                                  this%nvert, this%vertices, this%iavert,      &
                                  this%javert, this%cellxy, this%area,         &
                                  this%top, this%bot,                          &
                                  this%nodereduced, this%nodeuser)
    this%nja = this%con%nja
    this%njas = this%con%njas
    !
    !
    ! -- return
    return
  end subroutine connect

  subroutine write_grb(this, icelltype)
! ******************************************************************************
! write_grb -- Write the binary grid file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: getunit, openfile
    use OpenSpecModule, only: access, form
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwfDisvType) :: this
    integer(I4B), dimension(:), intent(in) :: icelltype
    ! -- local
    integer(I4B) :: iunit, i, ntxt
    integer(I4B), parameter :: lentxt = 100
    character(len=50) :: txthdr
    character(len=lentxt) :: txt
    character(len=LINELENGTH) :: fname
    character(len=*),parameter :: fmtgrdsave = &
      "(4X,'BINARY GRID INFORMATION WILL BE WRITTEN TO:',                      &
       &/,6X,'UNIT NUMBER: ', I0,/,6X, 'FILE NAME: ', A)"
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    ntxt = 20
    !
    ! -- Open the file
    inquire(unit=this%inunit, name=fname)
    fname = trim(fname) // '.grb'
    iunit = getunit()
    write(this%iout, fmtgrdsave) iunit, trim(adjustl(fname))
    call openfile(iunit, this%iout, trim(adjustl(fname)), 'DATA(BINARY)',      &
                  form, access, 'REPLACE')
    !
    ! -- write header information
    write(txthdr, '(a)') 'GRID DISV'
    txthdr(50:50) = new_line('a')
    write(iunit) txthdr
    write(txthdr, '(a)') 'VERSION 1'
    txthdr(50:50) = new_line('a')
    write(iunit) txthdr
    write(txthdr, '(a, i0)') 'NTXT ', ntxt
    txthdr(50:50) = new_line('a')
    write(iunit) txthdr
    write(txthdr, '(a, i0)') 'LENTXT ', lentxt
    txthdr(50:50) = new_line('a')
    write(iunit) txthdr
    !
    ! -- write variable definitions
    write(txt, '(3a, i0)') 'NCELLS ', 'INTEGER ', 'NDIM 0 # ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'NLAY ', 'INTEGER ', 'NDIM 0 # ', this%nlay
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'NCPL ', 'INTEGER ', 'NDIM 0 # ', this%ncpl
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'NVERT ', 'INTEGER ', 'NDIM 0 # ', this%nvert
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'NJAVERT ', 'INTEGER ', 'NDIM 0 # ', size(this%javert)
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'NJA ', 'INTEGER ', 'NDIM 0 # ', this%con%nja
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, 1pg25.15e3)') 'XORIGIN ', 'DOUBLE ', 'NDIM 0 # ', this%xorigin
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, 1pg25.15e3)') 'YORIGIN ', 'DOUBLE ', 'NDIM 0 # ', this%yorigin
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, 1pg25.15e3)') 'ANGROT ', 'DOUBLE ', 'NDIM 0 # ', this%angrot
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'TOP ', 'DOUBLE ', 'NDIM 1 ', this%ncpl
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'BOTM ', 'DOUBLE ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'VERTICES ', 'DOUBLE ', 'NDIM 2 2 ', this%nvert
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'CELLX ', 'DOUBLE ', 'NDIM 1 ', this%ncpl
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'CELLY ', 'DOUBLE ', 'NDIM 1 ', this%ncpl
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'IAVERT ', 'INTEGER ', 'NDIM 1 ', this%ncpl + 1
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'JAVERT ', 'INTEGER ', 'NDIM 1 ', size(this%javert)
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'IA ', 'INTEGER ', 'NDIM 1 ', this%nodesuser + 1
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'JA ', 'INTEGER ', 'NDIM 1 ', size(this%con%jausr)
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'IDOMAIN ', 'INTEGER ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'ICELLTYPE ', 'INTEGER ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    !
    ! -- write data
    write(iunit) this%nodesuser                                                 ! ncells
    write(iunit) this%nlay                                                      ! nlay
    write(iunit) this%ncpl                                                      ! ncpl
    write(iunit) this%nvert                                                     ! nvert
    write(iunit) size(this%javert)                                              ! njavert
    write(iunit) this%nja                                                       ! nja
    write(iunit) this%xorigin                                                   ! xorigin
    write(iunit) this%yorigin                                                   ! yorigin
    write(iunit) this%angrot                                                    ! angrot
    write(iunit) this%botm(:, :, 0)                                             ! top
    write(iunit) this%botm(:, :, 1:)                                            ! botm
    write(iunit) this%vertices                                                  ! vertices
    write(iunit) (this%cellxy(1, i), i = 1, this%ncpl)                          ! cellx
    write(iunit) (this%cellxy(2, i), i = 1, this%ncpl)                          ! celly
    write(iunit) this%iavert                                                    ! iavert
    write(iunit) this%javert                                                    ! javert
    write(iunit) this%con%iausr                                                 ! iausr
    write(iunit) this%con%jausr                                                 ! jausr
    write(iunit) this%idomain                                                   ! idomain
    write(iunit) icelltype                                                      ! icelltype
    !
    ! -- Close the file
    close(iunit)
    !
    ! -- return
    return
  end subroutine write_grb

  subroutine nodeu_to_string(this, nodeu, str)
! ******************************************************************************
! nodeu_to_string -- Convert user node number to a string in the form of
! (nodenumber) or (k,j)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: get_ijk
    ! -- dummy
    class(GwfDisvType) :: this
    integer(I4B), intent(in) :: nodeu
    character(len=*), intent(inout) :: str
    ! -- local
    integer(I4B) :: i, j, k
    character(len=10) :: kstr, jstr
! ------------------------------------------------------------------------------
    !
    call get_ijk(nodeu, 1, this%ncpl, this%nlay, i, j, k)
    write(kstr, '(i10)') k
    write(jstr, '(i10)') j
    str = '(' // trim(adjustl(kstr)) // ',' // &
                 trim(adjustl(jstr)) // ')'
    !
    ! -- return
    return
  end subroutine nodeu_to_string

  function get_nodenumber_idx1(this, nodeu, icheck) result(nodenumber)
! ******************************************************************************
! get_nodenumber -- Return a nodenumber from the user specified node number
!                   with an option to perform a check.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    ! -- return
    integer(I4B) :: nodenumber
    ! -- dummy
    class(GwfDisvType), intent(in) :: this
    integer(I4B), intent(in) :: nodeu
    integer(I4B), intent(in) :: icheck
    ! -- local
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- check the node number if requested
    if(icheck /= 0) then
      !
      ! -- If within valid range, convert to reduced nodenumber
      if(nodeu < 1 .or. nodeu > this%nodesuser) then
        write(errmsg, '(a,i10)') &
          'Nodenumber less than 1 or greater than nodes:', nodeu
        call store_error(errmsg)
        nodenumber = 0
      else
        nodenumber = nodeu
        if(this%nodes < this%nodesuser) nodenumber = this%nodereduced(nodeu)
      endif
    else
      nodenumber = nodeu
      if(this%nodes < this%nodesuser) nodenumber = this%nodereduced(nodeu)
    endif
    !
    ! -- return
    return
  end function get_nodenumber_idx1

  function get_nodenumber_idx2(this, k, j, icheck)                     &
    result(nodenumber)
! ******************************************************************************
! get_nodenumber_idx2 -- Return a nodenumber from the user specified layer and
!                       column with an option to perform a check.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use InputOutputModule, only: get_node
    implicit none
    ! -- return
    integer(I4B) :: nodenumber
    ! -- dummy
    class(GwfDisvType), intent(in) :: this
    integer(I4B), intent(in) :: k, j
    integer(I4B), intent(in) :: icheck
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: nodeu
    ! formats
    character(len=*), parameter :: fmterr = &
    "('Error in disv grid cell indices: layer = ',i0,', node = ',i0)"
! ------------------------------------------------------------------------------
    !
    nodeu = get_node(k, 1, j, this%nlay, 1, this%ncpl)
    if (nodeu < 1) then
      write(errmsg, fmterr) k, j
      call store_error(errmsg)
      call ustop()
    endif
    nodenumber = nodeu
    if(this%nodes < this%nodesuser) nodenumber = this%nodereduced(nodeu)
    !
    ! -- check the node number if requested
    if(icheck /= 0) then
      !
      if(k < 1 .or. k > this%nlay) &
        call store_error('Layer less than one or greater than nlay')
      if(j < 1 .or. j > this%ncpl) &
        call store_error('Node number less than one or greater than ncpl')
      !
      ! -- Error if outside of range
      if(nodeu < 1 .or. nodeu > this%nodesuser) then
        write(errmsg, '(a,i10)') &
          'Nodenumber less than 1 or greater than nodes:', nodeu
        call store_error(errmsg)
      endif
    endif
    !
    ! -- return
    return
  end function get_nodenumber_idx2

  function get_nodeuser(this, noder) &
    result(nodenumber)
! ******************************************************************************
! get_nodeuser -- Return the user nodenumber from the reduced node number
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- return
    integer(I4B) :: nodenumber
    class(GwfDisvType) :: this
    integer(I4B), intent(in) :: noder
! ------------------------------------------------------------------------------
    !
    if(this%nodes < this%nodesuser) then
      nodenumber = this%nodeuser(noder)
    else
      nodenumber = noder
    endif
    !
    ! -- return
    return
    end function get_nodeuser

!  subroutine connection_normal(this, noden, nodem, ihc, xcomp, ycomp, zcomp)
!! ******************************************************************************
!! connection_normal -- calculate the normal vector components for reduced
!!   nodenumber cell (noden) and its shared face with cell nodem.  ihc is the
!!   horizontal connection flag.
!! ******************************************************************************
!!
!!    SPECIFICATIONS:
!! ------------------------------------------------------------------------------
!    ! -- modules
!    use ConstantsModule, only: DZERO, DONE
!    ! -- dummy
!    class(GwfDisvType) :: this
!    integer(I4B), intent(in) :: noden
!    integer(I4B), intent(in) :: nodem
!    integer(I4B), intent(in) :: ihc
!    real(DP), intent(inout) :: xcomp
!    real(DP), intent(inout) :: ycomp
!    real(DP), intent(inout) :: zcomp
!    ! -- local
!! ------------------------------------------------------------------------------
!    !
!    ! -- Set cell1 and cell2 to nodes nodered and mred
!    call this%cell1%set_nodered(noden)
!    call this%cell2%set_nodered(nodem)
!    !
!    ! -- Set vector components based on ihc
!    if(ihc == 0) then
!      xcomp = DZERO
!      ycomp = DZERO
!      if(nodem < noden) then
!        !
!        ! -- nodem must be above noden, so upward connection
!        zcomp = DONE
!      else
!        !
!        ! -- nodem must be below noden, so downward connection
!        zcomp = -DONE
!      endif
!    else
!      call this%cell1%edge_normal(this%cell2, xcomp, ycomp)
!      zcomp = DZERO
!    endif
!    !
!    ! -- return
!    return
!  end subroutine connection_normal

  subroutine connection_normal(this, noden, nodem, ihc, xcomp, ycomp, zcomp,   &
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
    use ConstantsModule, only: DONE, DZERO
    use SimModule, only: ustop, store_error
    ! -- dummy
    class(GwfDisvType) :: this
    integer(I4B), intent(in) :: noden
    integer(I4B), intent(in) :: nodem
    integer(I4B), intent(in) :: ihc
    real(DP), intent(inout) :: xcomp
    real(DP), intent(inout) :: ycomp
    real(DP), intent(inout) :: zcomp
    integer(I4B), intent(in) :: ipos
    ! -- local
    !integer(I4B) :: ipos
    !integer(I4B) :: ncell3d, mcell3d
    real(DP) :: angle, dmult
! ------------------------------------------------------------------------------
    !
    ! -- Set vector components based on ihc
    if(ihc == 0) then
      xcomp = DZERO
      ycomp = DZERO
      if(nodem < noden) then
        !
        ! -- nodem must be above noden, so upward connection
        zcomp = DONE
      else
        !
        ! -- nodem must be below noden, so downward connection
        zcomp = -DONE
      endif
    else
      ! -- find from anglex, since anglex is symmetric, need to flip vector
      !    for lower triangle (nodem < noden)
      !ipos = this%con%getjaindex(noden, nodem)
      angle = this%con%anglex(this%con%jas(ipos))
      dmult = DONE
      if (nodem < noden) dmult = -DONE
      xcomp = cos(angle) * dmult
      ycomp = sin(angle) * dmult
      zcomp = DZERO
    endif
    !
    ! -- return
    return
  end subroutine connection_normal

!  subroutine connection_vector(this, noden, nodem, nozee, satn, satm, ihc,   &
!                               xcomp, ycomp, zcomp, conlen)
!! ******************************************************************************
!! connection_vector -- calculate the unit vector components from reduced
!!   nodenumber cell (noden) to its neighbor cell (nodem).  The saturation for
!!   for these cells are also required so that the vertical position of the cell
!!   cell centers can be calculated.  ihc is the horizontal flag.  Also return
!!   the straight-line connection length.
!! ******************************************************************************
!!
!!    SPECIFICATIONS:
!! ------------------------------------------------------------------------------
!    ! -- modules
!    use ConstantsModule, only: DZERO, DONE, DHALF
!    ! -- dummy
!    class(GwfDisvType) :: this
!    integer(I4B), intent(in) :: noden
!    integer(I4B), intent(in) :: nodem
!    logical, intent(in) :: nozee
!    real(DP), intent(in) :: satn
!    real(DP), intent(in) :: satm
!    integer(I4B), intent(in) :: ihc
!    real(DP), intent(inout) :: xcomp
!    real(DP), intent(inout) :: ycomp
!    real(DP), intent(inout) :: zcomp
!    real(DP), intent(inout) :: conlen
!    ! -- local
!    real(DP) :: zn, zm
!! ------------------------------------------------------------------------------
!    !
!    ! -- Set cell1 and cell2 to nodes noden and nodem
!    call this%cell1%set_nodered(noden)
!    call this%cell2%set_nodered(nodem)
!    !
!    ! -- Set vector components based on ihc
!    if(ihc == 0) then
!      !
!      ! -- vertical connection; set zcomp positive upward
!      xcomp = DZERO
!      ycomp = DZERO
!      if(nodem < noden) then
!        zcomp = DONE
!      else
!        zcomp = -DONE
!      endif
!      !
!      ! -- Cell centers are calculated without consideration for saturation.
!      !    This routine only used by XT3D at the moment, and so the NPF
!      !    options for vertical conductance do not need to be supported
!      !    here.
!      zn = this%cell1%bot + DHALF * (this%cell1%top - this%cell1%bot)
!      zm = this%cell2%bot + DHALF * (this%cell2%top - this%cell2%bot)
!      conlen = abs(zm - zn)
!    else
!      !
!      ! -- horizontal connection, with possible z component due to cell offsets
!      !    and/or water table conditions
!      call this%cell1%connection_vector(this%cell2, nozee, satn, satm, xcomp,  &
!        ycomp, zcomp, conlen)
!    endif
!    !
!    ! -- return
!    return
!  end subroutine connection_vector

  subroutine connection_vector(this, noden, nodem, nozee, satn, satm, ihc,   &
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
    use ConstantsModule, only: DZERO, DONE, DHALF
    use SimModule, only: ustop, store_error
    use InputOutputModule, only: get_jk
    use DisvGeom, only: line_unit_vector
    ! -- dummy
    class(GwfDisvType) :: this
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
    integer(I4B) :: nodeu, ncell2d, mcell2d, k
    real(DP) :: xn, xm, yn, ym, zn, zm
! ------------------------------------------------------------------------------
    !
    ! -- Set vector components based on ihc
    if(ihc == 0) then
      !
      ! -- vertical connection; set zcomp positive upward
      xcomp = DZERO
      ycomp = DZERO
      if(nodem < noden) then
        zcomp = DONE
      else
        zcomp = -DONE
      endif
      zn = this%bot(noden) + DHALF * (this%top(noden) - this%bot(noden))
      zm = this%bot(nodem) + DHALF * (this%top(nodem) - this%bot(nodem))
      conlen = abs(zm - zn)
    else
      !
      ! -- horizontal connection, with possible z component due to cell offsets
      !    and/or water table conditions
      if (nozee) then
        zn = DZERO
        zm = DZERO
      else
        zn = this%bot(noden) + DHALF * satn * (this%top(noden) - this%bot(noden))
        zm = this%bot(nodem) + DHALF * satm * (this%top(nodem) - this%bot(nodem))
      endif
      nodeu = this%get_nodeuser(noden)
      call get_jk(nodeu, this%ncpl, this%nlay, ncell2d, k)
      nodeu = this%get_nodeuser(nodem)
      call get_jk(nodeu, this%ncpl, this%nlay, mcell2d, k)
      xn = this%cellxy(1, ncell2d)
      yn = this%cellxy(2, ncell2d)
      xm = this%cellxy(1, mcell2d)
      ym = this%cellxy(2, mcell2d)
      call line_unit_vector(xn, yn, zn, xm, ym, zm, xcomp, ycomp, zcomp,       &
                            conlen)
    endif
    !
    ! -- return
    return
  end subroutine connection_vector

  subroutine allocate_scalars(this, name_model)
! ******************************************************************************
! allocate_scalars -- Allocate and initialize scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfDisvType) :: this
    character(len=*), intent(in) :: name_model
! ------------------------------------------------------------------------------
    !
    ! -- Allocate parent scalars
    call this%DisBaseType%allocate_scalars(name_model)
    !
    ! -- Allocate
    call mem_allocate(this%nlay, 'NLAY', this%origin)
    call mem_allocate(this%ncpl, 'NCPL', this%origin)
    call mem_allocate(this%nvert, 'NVERT', this%origin)
    !
    ! -- Initialize
    this%nlay = 0
    this%ncpl = 0
    this%nvert = 0
    this%ndim = 2
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- Allocate arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfDisvType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Allocate arrays in DisBaseType (mshape, top, bot, area)
    call this%DisBaseType%allocate_arrays()
    !
    ! -- Allocate arrays for GwfDisvType
    if(this%nodes < this%nodesuser) then
      call mem_allocate(this%nodeuser, this%nodes, 'NODEUSER', this%origin)
      call mem_allocate(this%nodereduced, this%nodesuser, 'NODEREDUCED',       &
                        this%origin)
    else
      call mem_allocate(this%nodeuser, 1, 'NODEUSER', this%origin)
      call mem_allocate(this%nodereduced, 1, 'NODEREDUCED', this%origin)
    endif
    !
    ! -- Allocate vertices array
    call mem_allocate(this%vertices, 2, this%nvert, 'VERTICES', this%origin)
    call mem_allocate(this%cellxy, 2, this%ncpl, 'CELLXY', this%origin)
    !
    ! -- Initialize
    this%mshape(1) = this%nlay
    this%mshape(2) = this%ncpl
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  function get_cell2d_area(this, icell2d) result(area)
! ******************************************************************************
! get_cell2d_area -- Calculate and return the signed area of the cell.  A
!   negative area means the points are in counter clockwise orientation.
!   a = 1/2 *[(x1*y2 + x2*y3 + x3*y4 + ... + xn*y1) -
!             (x2*y1 + x3*y2 + x4*y3 + ... + x1*yn)]
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    use ConstantsModule, only: DZERO, DHALF, DONE
    ! -- dummy
    class(GwfDisvType) :: this
    integer(I4B), intent(in) :: icell2d
    ! -- return
    real(DP) :: area
    ! -- local
    integer(I4B) :: ivert
    integer(I4B) :: nvert
    integer(I4B) :: icount
    real(DP) :: x
    real(DP) :: y
! ------------------------------------------------------------------------------
    !
    area = DZERO
    nvert = this%iavert(icell2d + 1) - this%iavert(icell2d)
    icount = 1
    do ivert = this%iavert(icell2d), this%iavert(icell2d + 1) - 1
      x = this%vertices(1, this%javert(ivert))
      if(icount < nvert) then
        y = this%vertices(2, this%javert(ivert + 1))
      else
        y = this%vertices(2, this%javert(this%iavert(icell2d)))
      endif
      area = area + x * y
      icount = icount + 1
    enddo
    !
    icount = 1
    do ivert = this%iavert(icell2d), this%iavert(icell2d + 1) - 1
      y = this%vertices(2, this%javert(ivert))
      if(icount < nvert) then
        x = this%vertices(1, this%javert(ivert + 1))
      else
        x = this%vertices(1, this%javert(this%iavert(icell2d)))
      endif
      area = area - x * y
      icount = icount + 1
    enddo
    !
    area = -DONE * area * DHALF
    !
    ! -- return
    return
  end function get_cell2d_area

  function nodeu_from_string(this, lloc, istart, istop, in, iout, line, &
                             flag_string, allow_zero) result(nodeu)
! ******************************************************************************
! nodeu_from_string -- Receive a string and convert the string to a user
!   nodenumber.  The model discretization is DISV; read layer and cell number.
!   If flag_string argument is present and true, the first token in string
!   is allowed to be a string (e.g. boundary name). In this case, if a string
!   is encountered, return value as -2.
! ******************************************************************************
    implicit none
    ! -- dummy
    class(GwfDisvType)                  :: this
    integer(I4B),           intent(inout) :: lloc
    integer(I4B),           intent(inout) :: istart
    integer(I4B),           intent(inout) :: istop
    integer(I4B),           intent(in)    :: in
    integer(I4B),           intent(in)    :: iout
    character(len=*),  intent(inout) :: line
    logical, optional, intent(in)    :: flag_string
    logical, optional, intent(in)    :: allow_zero
    integer(I4B)                     :: nodeu
    ! -- local
    integer(I4B) :: j, k, nlay, nrow, ncpl
    integer(I4B) :: lloclocal, ndum, istat, n
    real(DP) :: r
    character(len=LINELENGTH) :: ermsg, fname
    !
    if (present(flag_string)) then
      if (flag_string) then
        ! Check to see if first token in line can be read as an integer.
        lloclocal = lloc
        call urword(line, lloclocal, istart, istop, 1, ndum, r, iout, in)
        read(line(istart:istop),*,iostat=istat)n
        if (istat /= 0) then
          ! First token in line is not an integer; return flag to this effect.
          nodeu = -2
          return
        endif
      endif
    endif
    !
    nlay = this%mshape(1)
    nrow = 1
    ncpl = this%mshape(2)
    !
    call urword(line, lloc, istart, istop, 2, k, r, iout, in)
    call urword(line, lloc, istart, istop, 2, j, r, iout, in)
    !
    if (k == 0 .and. j == 0) then
      if (present(allow_zero)) then
        if (allow_zero) then
          nodeu = 0
          return
        endif
      endif
    endif
    !
    if(k < 1 .or. k > nlay) then
        write(ermsg, *) ' Layer number in list is outside of the grid', k
        call store_error(ermsg)
    end if
    if(j < 1 .or. j > ncpl) then
        write(ermsg, *) ' Cell2d number in list is outside of the grid', j
        call store_error(ermsg)
    end if
    nodeu = get_node(k, 1, j, nlay, nrow, ncpl)
    !
    if(nodeu < 1 .or. nodeu > this%nodesuser) then
      write(ermsg, *) ' Node number in list is outside of the grid', nodeu
      call store_error(ermsg)
      inquire(unit=in, name=fname)
      call store_error('Error converting in file: ')
      call store_error(trim(adjustl(fname)))
      call store_error('Cell number cannot be determined in line: ')
      call store_error(trim(adjustl(line)))
      call store_error_unit(in)
      call ustop()
    end if
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
    implicit none
    ! -- return
    integer(I4B) :: nodeu
    ! -- dummy
    class(GwfDisvType)               :: this
    character(len=*),  intent(inout) :: cellid
    integer(I4B),           intent(in)    :: inunit
    integer(I4B),           intent(in)    :: iout
    logical, optional, intent(in)    :: flag_string
    logical, optional, intent(in)    :: allow_zero
    ! -- local
    integer(I4B) :: j, k, nlay, nrow, ncpl
    integer(I4B) :: lloclocal, ndum, istat, n
    integer(I4B) :: istart, istop
    real(DP) :: r
    character(len=LINELENGTH) :: ermsg, fname
    !
    if (present(flag_string)) then
      if (flag_string) then
        ! Check to see if first token in cellid can be read as an integer.
        lloclocal = 1
        call urword(cellid, lloclocal, istart, istop, 1, ndum, r, iout, inunit)
        read(cellid(istart:istop),*,iostat=istat)n
        if (istat /= 0) then
          ! First token in cellid is not an integer; return flag to this effect.
          nodeu = -2
          return
        endif
      endif
    endif
    !
    nlay = this%mshape(1)
    nrow = 1
    ncpl = this%mshape(2)
    !
    lloclocal = 1
    call urword(cellid, lloclocal, istart, istop, 2, k, r, iout, inunit)
    call urword(cellid, lloclocal, istart, istop, 2, j, r, iout, inunit)
    !
    if (k == 0 .and. j == 0) then
      if (present(allow_zero)) then
        if (allow_zero) then
          nodeu = 0
          return
        endif
      endif
    endif
    !
    if(k < 1 .or. k > nlay) then
        write(ermsg, *) ' Layer number in list is outside of the grid', k
        call store_error(ermsg)
    end if
    if(j < 1 .or. j > ncpl) then
        write(ermsg, *) ' Cell2d number in list is outside of the grid', j
        call store_error(ermsg)
    end if
    nodeu = get_node(k, 1, j, nlay, nrow, ncpl)
    !
    if(nodeu < 1 .or. nodeu > this%nodesuser) then
      write(ermsg, *) ' Node number in list is outside of the grid', nodeu
      call store_error(ermsg)
      inquire(unit=inunit, name=fname)
      call store_error('Error converting in file: ')
      call store_error(trim(adjustl(fname)))
      call store_error('Cell number cannot be determined in cellid: ')
      call store_error(trim(adjustl(cellid)))
      call store_error_unit(inunit)
      call ustop()
    end if
    !
    ! -- return
    return
  end function nodeu_from_cellid

  logical function supports_layers(this)
    implicit none
    ! -- dummy
    class(GwfDisvType) :: this
    !
    supports_layers = .true.
    return
  end function supports_layers

  function get_ncpl(this)
! ******************************************************************************
! get_ncpl -- Return number of cells per layer.  This is ncpl
!   for a DISV grid.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    integer(I4B) :: get_ncpl
    ! -- dummy
    class(GwfDisvType) :: this
! ------------------------------------------------------------------------------
    !
    get_ncpl = this%ncpl
    !
    ! -- Return
    return
  end function get_ncpl

  subroutine read_int_array(this, line, lloc, istart, istop, iout, in, &
                            iarray, aname)
! ******************************************************************************
! read_int_array -- Read a GWF integer array
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: urword
    use SimModule, only: store_error, ustop
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfDisvType), intent(inout)                  :: this
    character(len=*), intent(inout)                    :: line
    integer(I4B), intent(inout)                        :: lloc
    integer(I4B), intent(inout)                        :: istart
    integer(I4B), intent(inout)                        :: istop
    integer(I4B), intent(in)                           :: in
    integer(I4B), intent(in)                           :: iout
    integer(I4B), dimension(:), pointer, intent(inout) :: iarray
    character(len=*), intent(in)                       :: aname
    ! -- local
    integer(I4B) :: ival
    real(DP) :: rval
    integer(I4B) :: nlay
    integer(I4B) :: nrow
    integer(I4B) :: ncol
    integer(I4B) :: nval
    integer(I4B) :: nodeu, noder
    integer(I4B), dimension(:), pointer :: itemp
! ------------------------------------------------------------------------------
    !
    ! -- Point the temporary pointer array, which is passed to the reading
    !    subroutine.  The temporary array will point to ibuff if it is a
    !    reduced structured system, or to iarray if it is an unstructured
    !    model.
    nlay = this%mshape(1)
    nrow = 1
    ncol = this%mshape(2)
    !
    if(this%nodes < this%nodesuser) then
      nval = this%nodesuser
      itemp => this%ibuff
    else
      nval = this%nodes
      itemp => iarray
    endif
    !
    ! -- Read the array
    call urword(line, lloc, istart, istop, 1, ival, rval, iout, in)
    if (line(istart:istop).EQ.'LAYERED') then
      !
      ! -- Read layered input
      call ReadArray(in, itemp, aname, this%ndim, ncol, nrow, nlay, nval, &
                      iout, 1, nlay)
    else
      !
      ! -- Read unstructured input
      call ReadArray(in, itemp, aname, this%ndim, nval, iout, 0)
    end if
    !
    ! -- If reduced model, then need to copy from itemp(=>ibuff) to iarray
    if(this%nodes <  this%nodesuser) then
      do nodeu = 1, this%nodesuser
        noder = this%get_nodenumber(nodeu, 0)
        if(noder <= 0) cycle
        iarray(noder) = itemp(nodeu)
      enddo
    endif
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
    ! -- modules
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfDisvType), intent(inout)              :: this
    character(len=*), intent(inout)                :: line
    integer(I4B), intent(inout)                    :: lloc
    integer(I4B), intent(inout)                    :: istart
    integer(I4B), intent(inout)                    :: istop
    integer(I4B), intent(in)                       :: in
    integer(I4B), intent(in)                       :: iout
    real(DP), dimension(:), pointer, intent(inout) :: darray
    character(len=*), intent(in)                   :: aname
    ! -- local
    integer(I4B) :: ival
    real(DP) :: rval
    integer(I4B) :: nlay
    integer(I4B) :: nrow
    integer(I4B) :: ncol
    integer(I4B) :: nval
    integer(I4B) :: nodeu, noder
    real(DP), dimension(:), pointer :: dtemp
! ------------------------------------------------------------------------------
    !
    ! -- Point the temporary pointer array, which is passed to the reading
    !    subroutine.  The temporary array will point to dbuff if it is a
    !    reduced structured system, or to darray if it is an unstructured
    !    model.
    nlay = this%mshape(1)
    nrow = 1
    ncol = this%mshape(2)
    !
    if(this%nodes < this%nodesuser) then
      nval = this%nodesuser
      dtemp => this%dbuff
    else
      nval = this%nodes
      dtemp => darray
    endif
    !
    ! -- Read the array
    call urword(line, lloc, istart, istop, 1, ival, rval, iout, in)
    if (line(istart:istop).EQ.'LAYERED') then
      !
      ! -- Read structured input
      call ReadArray(in, dtemp, aname, this%ndim, ncol, nrow, nlay, nval, &
                      iout, 1, nlay)
    else
      !
      ! -- Read unstructured input
      call ReadArray(in, dtemp, aname, this%ndim, nval, iout, 0)
    end if
    !
    ! -- If reduced model, then need to copy from dtemp(=>dbuff) to darray
    if(this%nodes <  this%nodesuser) then
      do nodeu = 1, this%nodesuser
        noder = this%get_nodenumber(nodeu, 0)
        if(noder <= 0) cycle
        darray(noder) = dtemp(nodeu)
      enddo
    endif
    !
    ! -- return
    return
  end subroutine read_dbl_array

  subroutine read_layer_array(this, nodelist, darray, ncolbnd, maxbnd,     &
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
    use InputOutputModule, only: get_node
    ! -- dummy
    class(GwfDisvType) :: this
    integer(I4B), intent(in) :: ncolbnd
    integer(I4B), intent(in) :: maxbnd
    integer(I4B), dimension(maxbnd) :: nodelist
    real(DP), dimension(ncolbnd, maxbnd), intent(inout) :: darray
    integer(I4B), intent(in) :: icolbnd
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: il, ir, ic, ncol, nrow, nlay, nval, ipos, noder, nodeu
    logical :: found
! ------------------------------------------------------------------------------
    !
    nlay = this%mshape(1)
    nrow = 1
    ncol = this%mshape(2)
    !
    ! -- Read the array
    nval = ncol * nrow
    call ReadArray(inunit, this%dbuff, aname, this%ndim, nval, iout, 0)
    !
    ! -- Copy array into bound
    ipos = 1
    do ir = 1, nrow
      columnloop: do ic = 1, ncol
        !
        ! -- look down through all layers and see if nodeu == nodelist(ipos)
        !    cycle if not, because node must be inactive or pass through
        found = .false.
        layerloop: do il = 1, nlay
          nodeu = get_node(il, ir, ic, nlay, nrow, ncol)
          noder = this%get_nodenumber(nodeu, 0)
          if(noder == 0) cycle layerloop
          if(noder == nodelist(ipos)) then
            found = .true.
            exit layerloop
          endif
        enddo layerloop
        if(.not. found) cycle columnloop
        !
        ! -- Assign the array value to darray
        nodeu = get_node(1, ir, ic, nlay, nrow, ncol)
        darray(icolbnd, ipos) = this%dbuff(nodeu)
        ipos = ipos + 1
        !
      enddo columnloop
    enddo
    !
    ! -- return
  end subroutine read_layer_array

  subroutine record_array(this, darray, iout, iprint, idataun, aname,     &
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
    ! -- modules
    ! -- dummy
    class(GwfDisvType), intent(inout)              :: this
    real(DP), dimension(:), pointer, intent(inout) :: darray
    integer(I4B), intent(in)                       :: iout
    integer(I4B), intent(in)                       :: iprint
    integer(I4B), intent(in)                       :: idataun
    character(len=*), intent(in)                   :: aname
    character(len=*), intent(in)                   :: cdatafmp
    integer(I4B), intent(in)                       :: nvaluesp
    integer(I4B), intent(in)                       :: nwidthp
    character(len=*), intent(in)                   :: editdesc
    real(DP), intent(in)                           :: dinact
    ! -- local
    integer(I4B) :: k, ifirst
    integer(I4B) :: nlay
    integer(I4B) :: nrow
    integer(I4B) :: ncol
    integer(I4B) :: nval
    integer(I4B) :: nodeu, noder
    integer(I4B) :: istart, istop
    real(DP), dimension(:), pointer :: dtemp
    ! -- formats
    character(len=*),parameter :: fmthsv = &
      "(1X,/1X,a,' WILL BE SAVED ON UNIT ',I4, &
       &' AT END OF TIME STEP',I5,', STRESS PERIOD ',I4)"
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    nlay = this%mshape(1)
    nrow = 1
    ncol = this%mshape(2)
    !
    ! -- If this is a reduced model, then copy the values from darray into
    !    dtemp.
    if(this%nodes < this%nodesuser) then
      nval = this%nodes
      dtemp => this%dbuff
      do nodeu = 1, this%nodesuser
        noder = this%get_nodenumber(nodeu, 0)
        if(noder <= 0) then
          dtemp(nodeu) = dinact
          cycle
        endif
        dtemp(nodeu) = darray(noder)
      enddo
    else
      nval = this%nodes
      dtemp => darray
    endif
    !
    ! -- Print to iout if iprint /= 0
    if(iprint /= 0) then
      istart = 1
      do k = 1, nlay
        istop = istart + nrow * ncol - 1
        call ulaprufw(ncol, nrow, kstp, kper, k, iout, dtemp(istart:istop),  &
                      aname, cdatafmp, nvaluesp, nwidthp, editdesc)
        istart = istop + 1
      enddo
    endif
    !
    ! -- Save array to an external file.
    if(idataun > 0) then
      ! -- write to binary file by layer
      ifirst = 1
      istart = 1
      do k=1, nlay
        istop = istart + nrow * ncol - 1
        if(ifirst == 1) write(iout, fmthsv)                                    &
                            trim(adjustl(aname)), idataun,                     &
                            kstp, kper
        ifirst = 0
        call ulasav(dtemp(istart:istop), aname, kstp, kper,                    &
                    pertim, totim, ncol, nrow, k, idataun)
        istart = istop + 1
      enddo
    elseif(idataun < 0) then
      !
      ! -- write entire array as one record
      call ubdsv1(kstp, kper, aname, -idataun, dtemp, ncol, nrow, nlay,        &
                  iout, delt, pertim, totim)
    endif
    !
    ! -- return
    return
  end subroutine record_array

  subroutine record_srcdst_list_header(this, text, textmodel, textpackage,      &
                                       dstmodel, dstpackage, naux, auxtxt,      &
                                       ibdchn, nlist, iout)
! ******************************************************************************
! record_srcdst_list_header -- Record list header for imeth=6
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfDisvType) :: this
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
    ! -- local
    integer(I4B) :: nlay, nrow, ncol
! ------------------------------------------------------------------------------
    !
    nlay = this%mshape(1)
    nrow = 1
    ncol = this%mshape(2)
    !
    ! -- Use ubdsv06 to write list header
    call ubdsv06(kstp, kper, text, textmodel, textpackage, dstmodel, dstpackage,&
                 ibdchn, naux, auxtxt, ncol, nrow, nlay,                        &
                 nlist, iout, delt, pertim, totim)
    !
    ! -- return
    return
  end subroutine record_srcdst_list_header

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
    use InputOutputModule, only: get_node
    use SimModule, only: ustop, store_error
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfDisvType) :: this
    integer(I4B), intent(in) :: maxbnd
    integer(I4B), dimension(maxbnd), intent(inout) :: nodelist
    integer(I4B), intent(inout) :: nbound
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: il, ir, ic, ncol, nrow, nlay, nval, nodeu, noder, ipos, ierr
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    nlay = this%mshape(1)
    nrow = 1
    ncol = this%mshape(2)
    !
    nval = ncol * nrow
    call ReadArray(inunit, this%ibuff, aname, this%ndim, nval, iout, 0)
    !
    ! -- Copy array into nodelist
    ipos = 1
    ierr = 0
    do ir = 1, nrow
      do ic = 1, ncol
        nodeu = get_node(1, ir, ic, nlay, nrow, ncol)
        il = this%ibuff(nodeu)
        if(il < 1 .or. il > nlay) then
          write(errmsg, *) 'ERROR.  INVALID LAYER NUMBER: ', il
          call store_error(errmsg)
          call ustop()
        endif
        nodeu = get_node(il, ir, ic, nlay, nrow, ncol)
        noder = this%get_nodenumber(nodeu, 0)
        if(noder > 0) then
          if(ipos > maxbnd) then
            ierr = ipos
          else
            nodelist(ipos) = noder
          endif
          ipos = ipos + 1
        endif
      enddo
    enddo
    !
    ! -- Check for errors
    nbound = ipos - 1
    if(ierr > 0) then
      write(errmsg, *) 'ERROR. MAXBOUND DIMENSION IS TOO SMALL.'
      call store_error(errmsg)
      write(errmsg, *) 'INCREASE MAXBOUND TO: ', ierr
      call store_error(errmsg)
      call ustop()
    endif
    !
    ! -- If nbound < maxbnd, then initialize nodelist to zero in this range
    if(nbound < maxbnd) then
      do ipos = nbound+1, maxbnd
        nodelist(ipos) = 0
      enddo
    endif
    !
    ! -- return
  end subroutine nlarray_to_nodelist

end module GwfDisvModule

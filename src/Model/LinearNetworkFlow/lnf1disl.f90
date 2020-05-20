module LnfDislModule

  use ArrayReadersModule, only: ReadArray
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH
  use BaseDisModule, only: DisBaseType
  use InputOutputModule, only: get_node, URWORD, ulasav, ulaprufw, ubdsv1, &
                               ubdsv06
  use SimModule, only: count_errors, store_error, store_error_unit, ustop
  use BlockParserModule, only: BlockParserType
  use MemoryManagerModule, only: mem_allocate
  use TdisModule,          only: kstp, kper, pertim, totim, delt
  use DislGeom, only: calcdist, partialdist
  use BaseGeometryModule, only: GeometryBaseType

  implicit none
  private
  public disl_cr, disl_init_mem, LnfDislType, GeometryContainer

  type :: GeometryContainer
    class(GeometryBaseType), pointer :: obj
  end type GeometryContainer

  type, extends(DisBaseType) :: LnfDislType
    integer(I4B), pointer :: nvert => null()                                     ! number of x,y vertices
    integer(I4B), pointer :: nsupportedgeoms => null()                           ! number of supported geometries
    integer(I4B), pointer :: nactivegeoms => null()                              ! number of active geometries
    real(DP), dimension(:,:), pointer, contiguous :: vertices => null()          ! cell vertices stored as 3d array of x, y, and z
    real(DP), dimension(:,:), pointer, contiguous :: cellcenters => null()       ! cell centers stored as 3d array of x, y, and z
    integer(I4B), dimension(:,:), pointer, contiguous :: centerverts => null()   ! vertex at cell center or vertices cell center is between
    real(DP), dimension(:), pointer, contiguous :: cellfdc => null()             ! fdc stored as array
    integer(I4B), dimension(:), pointer, contiguous :: iavert => null()          ! cell vertex pointer ia array
    integer(I4B), dimension(:), pointer, contiguous :: javert => null()          ! cell vertex pointer ja array
    integer(I4B), dimension(:), pointer, contiguous :: iavertcells => null()     ! vertex to cells ia array
    integer(I4B), dimension(:), pointer, contiguous :: javertcells => null()     ! vertex to cells ja array
    integer(I4B), dimension(:), pointer, contiguous :: idomain  => null()        ! idomain (nodes)
    integer(I4B), dimension(:), pointer, contiguous :: iageom  => null()         ! cell geometry pointer ia array (nodes))
    integer(I4B), dimension(:), pointer, contiguous :: iageocellnum  => null()   ! cell geometry number ia array (nodes))
    type(GeometryContainer), allocatable, dimension(:) :: jametries              ! active geometry classes ja array
  contains
    procedure :: dis_df => disl_df
    procedure :: dis_da => disl_da
    procedure :: get_cellxy => get_cellxy_disl
    procedure, public :: register_geometry
    procedure, public :: record_array
    procedure, public :: record_srcdst_list_header
    ! -- helper functions
    procedure :: get_nodenumber_idx1
    procedure :: nodeu_to_string
    procedure :: nodeu_from_string
    procedure :: nodeu_from_cellid
    procedure :: connection_normal
    procedure :: connection_vector
    procedure :: get_cellxyz_disl
    ! -- private
    procedure :: read_options
    procedure :: read_dimensions
    procedure :: read_vertices
    procedure :: read_cell1d
    procedure :: read_mf6_griddata
    procedure :: grid_finalize
    procedure :: connect
    procedure :: write_grb
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    !
    procedure :: read_int_array
    procedure :: read_dbl_array
    !
  end type LnfDislType

  contains

  subroutine disl_cr(dis, name_model, inunit, nsupportedgeoms, iout)
! ******************************************************************************
! disl_cr -- Create a new discretization by vertices object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(LnfDislType), pointer :: dis
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: nsupportedgeoms
    integer(I4B), intent(in) :: iout
    type(LnfDislType), pointer :: disnew
! ------------------------------------------------------------------------------
    allocate(disnew)
    dis => disnew
    call disnew%allocate_scalars(name_model)
    dis%inunit = inunit
    dis%iout = iout
    disnew%nsupportedgeoms = nsupportedgeoms
    !
    ! -- Initialize block parser
    call dis%parser%Initialize(dis%inunit, dis%iout)
    !
    ! -- Return
    return
  end subroutine disl_cr

  subroutine disl_init_mem(dis, name_model, iout, nnodes, vertices, cellfdc, idomain)
! ******************************************************************************
! dis_init_mem -- Create a new discretization by vertices object from memory
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(LnfDislType), pointer :: dis
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(in) :: nnodes
    integer(I4B), dimension(:, :), pointer, contiguous, intent(in) :: vertices
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: cellfdc
    integer(I4B), dimension(:), pointer, contiguous, intent(in), optional :: idomain
    ! -- local
    type(LnfDislType), pointer :: disext
    integer(I4B) :: n
    integer(I4B) :: j
    integer(I4B) :: k
    integer(I4B) :: ival
    ! -- local
! ------------------------------------------------------------------------------
    allocate(disext)
    dis => disext
    call disext%allocate_scalars(name_model)
    dis%inunit = 0
    dis%iout = iout
    !
    ! -- Calculate nodesuser
    disext%nodesuser = nnodes
    !
    ! -- Allocate non-reduced vectors for disl
    call mem_allocate(disext%idomain, disext%nodesuser, 'IDOMAIN', disext%origin)
    !
    ! -- Allocate vertices array
    call mem_allocate(disext%cellcenters, 3, disext%nodesuser, 'CELLCENTERS', disext%origin)
    call mem_allocate(disext%centerverts, 2, disext%nodesuser, 'CENTERVERTS', disext%origin)
    call mem_allocate(disext%vertices, 3, disext%nvert, 'VERTICES', disext%origin)
    call mem_allocate(disext%cellfdc, disext%nodesuser, 'CELLFDC', disext%origin)
    ! -- Allocate geometries array
    call mem_allocate(disext%iageom, disext%nodesuser, 'IAGEOM', disext%origin)
    call mem_allocate(disext%iageocellnum, disext%nodesuser, 'IAGEOCELLNUM', disext%origin)
    allocate(disext%jametries(disext%nsupportedgeoms))
    !call mem_allocate(disext%jametries, disext%nsupportedgeoms, 'JAMETRIES', disext%origin)

    !
    ! -- fill data
    do k = 1, disext%nodesuser
      if (present(idomain)) then
        ival = idomain(k)
      else
        ival = 1
      end if
      disext%iageom(k) = 0
      disext%iageocellnum(k) = 0
      disext%idomain(k) = ival
    end do
    do n = 1, disext%nvert
      do j = 1, 3
        disext%vertices(j, n) = vertices(j, n)
      end do
    end do
    do n = 1, disext%nodesuser
      disext%cellfdc(n) = cellfdc(n)
    end do
    !
    ! -- Return
    return
  end subroutine disl_init_mem

  subroutine register_geometry(this, geopkg)
! ******************************************************************************
! register_geometry -- registers active geometry package with disl
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    use ConstantsModule, only: LINELENGTH
    class(LnfDislType) :: this
    class(GeometryBaseType), target, intent(in) :: geopkg
    ! -- locals
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: k

! ------------------------------------------------------------------------------
    ! save active geometry package in array
    this%nactivegeoms = this%nactivegeoms + 1
    this%jametries(this%nactivegeoms)%obj => geopkg
    ! update node to geometry ia lookup
    do k = 1, size(geopkg%nodelist)
      if (this%iageom(geopkg%nodelist(k)) /= 0) then
        ! more than one geometry points to the same node
        write(errmsg,'(4x,a,i0,a)')'****ERROR. NODE ', geopkg%nodelist(k), &
              ' HAS MULTIPLE GEOMETRIES.'
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
        call ustop()
      end if
      this%iageom(geopkg%nodelist(k)) = this%nactivegeoms
      this%iageocellnum(geopkg%nodelist(k)) = k
    end do
    !
    ! -- Return
    return
  end subroutine register_geometry

  subroutine disl_df(this)
! ******************************************************************************
! read_from_file -- Allocate and read discretization information
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LnfDislType) :: this
    ! -- locals
! ------------------------------------------------------------------------------
    !
    ! -- read data from file
    if (this%inunit /= 0) then
      !
      ! -- Identify package
      write(this%iout,1) this%inunit
  1   format(1X,/1X,'disl -- VERTEX GRID DISCRETIZATION PACKAGE,',               &
                    ' VERSION 1 : 12/23/2015 - INPUT READ FROM UNIT ',I0,//)
      !
      ! -- Read options
      call this%read_options()
      !
      ! -- Read dimensions block
      call this%read_dimensions()
      !
      ! -- Read GRIDDATA block
      call this%read_mf6_griddata()
      !
      ! -- Read VERTICES block
      call this%read_vertices()
      !
      ! -- Read CELL2D block
      call this%read_cell1d()
    end if
    !
    ! -- Final grid initialization
    call this%grid_finalize()
    !
    ! -- Return
    return
  end subroutine disl_df

  subroutine disl_da(this)
! ******************************************************************************
! disl_da -- Deallocate discretization data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(LnfDislType) :: this
    ! -- locals
! ------------------------------------------------------------------------------
    !
    ! -- DisBaseType deallocate
    call this%DisBaseType%dis_da()
    !
    ! -- Deallocate scalars
    call mem_deallocate(this%nvert)
    !
    ! -- Deallocate Arrays
    call mem_deallocate(this%nsupportedgeoms)
    call mem_deallocate(this%nactivegeoms)
    call mem_deallocate(this%nodereduced)
    call mem_deallocate(this%nodeuser)
    call mem_deallocate(this%cellcenters)
    call mem_deallocate(this%centerverts)
    call mem_deallocate(this%vertices)
    call mem_deallocate(this%cellfdc)
    call mem_deallocate(this%iavert)
    call mem_deallocate(this%javert)
    call mem_deallocate(this%iavertcells)
    call mem_deallocate(this%javertcells)
    call mem_deallocate(this%idomain)
    call mem_deallocate(this%iageom)
    call mem_deallocate(this%iageocellnum)
    
    deallocate(this%jametries)
    !
    ! -- Return
    return
  end subroutine disl_da

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(LnfDislType) :: this
    ! -- locals
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
      supportOpenClose=.true., blockRequired=.false.)
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
      write(this%iout,'(1x,a)')'NO disl OPTION BLOCK DETECTED.'
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
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule,  only: LINELENGTH
    ! -- dummy
    class(LnfDislType) :: this
    ! -- locals
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    integer(I4B) :: j
    integer(I4B) :: k
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
          case ('NODES')
            this%nodesuser = this%parser%GetInteger()
            write(this%iout,'(3x,a,i0)')'NODES = ', this%nodesuser
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
    if(this%nodesuser < 1) then
      call store_error( &
          'ERROR.  NODES WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.')
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
    ! -- Allocate non-reduced vectors for disl
    call mem_allocate(this%idomain, this%nodesuser, 'IDOMAIN', this%origin)
    !
    ! -- Allocate vertices array
    call mem_allocate(this%vertices, 3, this%nvert, 'VERTICES', this%origin)
    call mem_allocate(this%cellfdc, this%nodesuser, 'CELLFDC', this%origin)
    call mem_allocate(this%cellcenters, 3, this%nodesuser, 'CELLCENTERS', this%origin)
    call mem_allocate(this%centerverts, 2, this%nodesuser, 'CENTERVERTS', this%origin)
    call mem_allocate(this%iageom, this%nodesuser, 'IAGEOM', this%origin)
    call mem_allocate(this%iageocellnum, this%nodesuser, 'IAGEOCELLNUM', this%origin) 
    allocate(this%jametries(this%nsupportedgeoms))

    !
    ! -- initialize all cells to be active (idomain = 1)
    do k = 1, this%nodesuser
      this%idomain(k) = 1
      this%iageom(k) = 0
      this%iageocellnum(k) = 0
    end do
    !
    ! -- Return
    return
  end subroutine read_dimensions

  subroutine read_mf6_griddata(this)
! ******************************************************************************
! read_mf6_griddata -- Read grid data from a MODFLOW 6 ascii file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: ustop, count_errors, store_error
    use ConstantsModule,   only: LINELENGTH, DZERO
    ! -- dummy
    class(LnfDislType) :: this
    ! -- locals
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: n
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    integer(I4B), parameter :: nname = 1
    logical, dimension(nname) :: lname
    character(len=24),dimension(nname) :: aname
    character(len=300) :: ermsg
    ! -- formats
    character(len=*), parameter :: fmtdz = &
      "('ERROR. CELL (',i0,',',i0,') THICKNESS <= 0. ', " //             &
      "'TOP, BOT: ',2(1pg24.15))"
    character(len=*), parameter :: fmtnr = &
      "(/1x, 'THE SPECIFIED IDOMAIN RESULTS IN A REDUCED NUMBER OF CELLS.'," // &
      "/1x, 'NUMBER OF USER NODES: ',I0," // &
      "/1X, 'NUMBER OF NODES IN SOLUTION: ', I0, //)"
    ! -- data
    data aname(1) /'                 IDOMAIN'/
! ------------------------------------------------------------------------------
    !
    ! --Read GRIDDATA block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    lname(:) = .false.
    if(isfound) then
      write(this%iout,'(/,1x,a)')'PROCESSING GRIDDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('IDOMAIN')
            call this%parser%GetStringCaps(keyword)
            call ReadArray(this%parser%iuactive, this%idomain, aname(1),    &
                           this%ndim, this%nodesuser, this%iout, 0)
            lname(1) = .true.
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
    ! -- Return
    return
  end subroutine read_mf6_griddata

  subroutine grid_finalize(this)
! ******************************************************************************
! grid_finalize -- Finalize grid
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: ustop, count_errors, store_error
    use ConstantsModule,   only: LINELENGTH, DZERO, DONE
    ! -- dummy
    class(LnfDislType) :: this
    ! -- locals
    integer(I4B) :: node, noder, j, k, n
    real(DP) :: nodelen, curlen, seglen
    real(DP) :: cendist, segpercent
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
! ------------------------------------------------------------------------------
    !
    ! -- count active cells
    this%nodes = 0
    do k = 1, this%nodesuser
      if(this%idomain(k) > 0) this%nodes = this%nodes + 1
    enddo
    !
    ! -- Check to make sure nodes is a valid number
    if (this%nodes == 0) then
      call store_error('ERROR.  MODEL DOES NOT HAVE ANY ACTIVE NODES.')
      call store_error('MAKE SURE IDOMAIN ARRAY HAS SOME VALUES GREATER &
        &THAN ZERO.')
      call this%parser%StoreErrorUnit()
      call ustop()
    end if

    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
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
      do k = 1, this%nodesuser
        if(this%idomain(k) > 0) then
          this%nodereduced(node) = noder
          noder = noder + 1
        elseif(this%idomain(k) < 0) then
          this%nodereduced(node) = -1
        else
          this%nodereduced(node) = 0
        endif
        node = node + 1
      enddo
    endif
    !
    ! -- allocate and fill nodeuser if a reduced grid
    if(this%nodes < this%nodesuser) then
      node = 1
      noder = 1
      do k = 1, this%nodesuser
        if(this%idomain(k) > 0) then
          this%nodeuser(noder) = node
          noder = noder + 1
        endif
          node = node + 1
      enddo
    endif

    ! calculate and fill cell center array
    do k = 1, this%nodesuser
      ! calculate node length
      nodelen = DZERO
      do j = this%iavert(k), this%iavert(k+1) - 2
        nodelen = nodelen + calcdist(this%vertices, this%javert(j), &
          this%javert(j+1))
      end do
      ! calculate distance from start of node to cell center
      cendist = nodelen * this%cellfdc(k)
      ! calculate cell center location
      curlen = DZERO
      ! loop through cell's vertices
      inner: do j = this%iavert(k), this%iavert(k+1) - 2
        seglen = calcdist(this%vertices, this%javert(j), this%javert(j+1))
        ! if cell center between vertex k and k+1
        if (seglen + curlen >= cendist) then
            ! calculate cell center locations
            segpercent = (cendist - curlen) / seglen
            this%cellcenters(1, k) = partialdist(this%vertices(1,   &
              this%javert(j)), this%vertices(1, this%javert(j+1)),  &
              segpercent)
            this%cellcenters(2, k) = partialdist(this%vertices(2,   &
              this%javert(j)), this%vertices(2, this%javert(j+1)),  &
              segpercent)
            this%cellcenters(3, k) = partialdist(this%vertices(3,   &
              this%javert(j)), this%vertices(3, this%javert(j+1)),  &
              segpercent)
            ! record vertices that cell center is between
            if (abs(segpercent - DONE) < 0.00001) then
              this%centerverts(1, k) = this%javert(j+1)
              this%centerverts(2, k) = 0
            else if (abs(segpercent - DZERO) < 0.00001) then
              this%centerverts(1, k) = this%javert(j)
              this%centerverts(2, k) = 0
            else
              this%centerverts(1, k) = this%javert(j)
              this%centerverts(2, k) = this%javert(j+1)
            end if
            exit inner
        end if
        curlen = curlen + seglen
      end do inner
    end do

    !write(*, '(a)') 'cellcenters'
    !do n = 1, this%nodesuser
    !  write(*, '(1(1pg24.15), 1(1pg24.15), 1(1pg24.15))') &
    !    this%cellcenters(1, n), this%cellcenters(2, n), this%cellcenters(3, n)
    !end do

    !write(*, '(a)') 'centerverts'
    !do n = 1, this%nodesuser
    !  !write(*, '(I4)') this%centerverts(1, n)
    !  !write(*, '(I4)') this%centerverts(2, n)
    !  write(*, '(I4, I4)') &
    !    this%centerverts(1, n), this%centerverts(2, n)
    !end do

    !
    ! -- Build connections
    call this%connect()
    ! -- Return
    return
  end subroutine grid_finalize

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
    class(LnfDislType) :: this
    integer(I4B) :: i
    integer(I4B) :: ierr, ival
    logical :: isfound, endOfBlock
    real(DP) :: xmin, xmax, ymin, ymax, zmin, zmax
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
        ! -- z
        this%vertices(3, i) = this%parser%GetDouble()
        !
        ! -- set min/max coords
        if(i == 1) then
          xmin = this%vertices(1, i)
          xmax = xmin
          ymin = this%vertices(2, i)
          ymax = ymin
          zmin = this%vertices(3, i)
          zmax = zmin
        else
          xmin = min(xmin, this%vertices(1, i))
          xmax = max(xmax, this%vertices(1, i))
          ymin = min(ymin, this%vertices(2, i))
          ymax = max(ymax, this%vertices(2, i))
          zmin = min(zmin, this%vertices(3, i))
          zmax = max(zmax, this%vertices(3, i))
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
    write(this%iout, fmtcoord) 'MINIMUM Z', zmin
    write(this%iout, fmtcoord) 'MAXIMUM Z', zmax
    write(this%iout,'(1x,a)')'END PROCESSING VERTICES'
    !
    ! -- Return
    return
  end subroutine read_vertices

  subroutine read_cell1d(this)
! ******************************************************************************
! read_cell1d -- Read information describing the two dimensional (x, y)
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
    class(LnfDislType) :: this
    integer(I4B) :: i, j, k, ivert, ivert1, ncvert
    integer(I4B) :: ierr, ival, icurcell
    logical :: isfound, endOfBlock
    integer(I4B) :: maxvert, maxvertcell, iuext
    real(DP) :: xmin, xmax, ymin, ymax
    character(len=300) :: ermsg
    integer(I4B), dimension(:), allocatable :: maxnnz
    integer(I4B), dimension(:), pointer, contiguous :: vnumcells => null()
    type(sparsematrix) :: vertspm
    ! -- formats
    character(len=*), parameter :: fmtvert = &
      "('ERROR. IAVERTCELLS DOES NOT CONTAIN THE CORRECT NUMBER OF '" //   &
      "' CONNECTIONS FOR VERTEX ', i0)"
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
    allocate(maxnnz(this%nodesuser))
    do i = 1, this%nodesuser
      maxnnz(i) = 5
    enddo
    call vertspm%init(this%nodesuser, this%nvert, maxnnz)
    !
    ! --Read CELL2D block
    call this%parser%GetBlock('CELL1D', isfound, ierr, supportOpenClose=.true.)
    if(isfound) then
      write(this%iout,'(/,1x,a)') 'PROCESSING CELL1D'
      do i = 1, this%nodesuser
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
        ! -- Fractional distance to cell center
        this%cellfdc(i) = this%parser%GetDouble()
        !
        ! -- Number of vertices for this cell
        ncvert = this%parser%GetInteger()
        if(ncvert > maxvert) then
          maxvert = ncvert
          maxvertcell = i
        endif
        !
        ! -- Read each vertex number
        do j = 1, ncvert
          ivert = this%parser%GetInteger()
          call vertspm%addconnection(i, ivert, 0)
        enddo
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
    call mem_allocate(this%iavert, this%nodesuser+1, 'IAVERT', this%origin)
    call mem_allocate(this%javert, vertspm%nnz, 'JAVERT', this%origin)
    call vertspm%filliaja(this%iavert, this%javert, ierr)

    ! allocate vertex to cellids map
    call mem_allocate(this%iavertcells, this%nvert+1, 'IAVERTCELLS', this%origin)
    call mem_allocate(this%javertcells, vertspm%nnz, 'JAVERTCELLS', this%origin)
    ! calculate number of cell connections for each vertex
    allocate(vnumcells(this%nvert))
    do j = 1, this%nvert
      vnumcells(j) = 0
    end do
    do j = 1, vertspm%nnz
      vnumcells(this%javert(j)) = vnumcells(this%javert(j)) + 1
    end do
    ! build iavertcells
    this%iavertcells(1) = 1
    do j = 2, this%nvert
      this%iavertcells(j) = this%iavertcells(j-1) + vnumcells(j-1)
    end do
    this%iavertcells(this%nvert+1) = vertspm%nnz + 1
    ! initialize javertcells
    do j = 1, vertspm%nnz
      this%javertcells(j) = 0
    end do
    ! build javertcells
    icurcell = 1
    do j = 1, vertspm%nnz
      if (this%iavert(icurcell+1) == j) then
        icurcell = icurcell + 1
      end if
      isfound = .FALSE.
      inner: do k = this%iavertcells(this%javert(j)), this%iavertcells(this%javert(j)+1) - 1
        if (this%javertcells(k) == 0) then
          ! fill the first available index and exit
          this%javertcells(k) = icurcell
          isfound = .TRUE.
          exit inner
        end if
      end do inner
      if (.not. isfound) then
        write(ermsg, fmtvert) j
        call store_error(ermsg)
        call store_error_unit(iuext)
        call ustop()
      endif
    end do

    ! clean up
    deallocate(vnumcells)
    call vertspm%destroy()

    !
    ! -- Write information
    write(this%iout, fmtncpl) this%nodesuser
    write(this%iout, fmtmaxvert) maxvert, maxvertcell
    write(this%iout,'(1x,a)')'END PROCESSING VERTICES'

    !
    ! -- Return
    return
  end subroutine read_cell1d
  
  subroutine connect(this)
! ******************************************************************************
! connect -- Build grid connections
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(LnfDislType) :: this
    ! -- local
    integer(I4B) :: j, k
    integer(I4B) :: noder, nrsize
    real(DP) :: area
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- create and fill the connections object
    nrsize = 0
    if(this%nodes < this%nodesuser) nrsize = this%nodes
    allocate(this%con)
    ! SRP TODO: connections need geometry info
    call this%con%dislconnections(this%name_model, this%nodes, this%nodesuser, &
                                  nrsize, this%nvert, this%vertices,           &
                                  this%iavert, this%javert, this%iavertcells,  &
                                  this%javertcells, this%cellcenters,          &
                                  this%centerverts, this%cellfdc,              &
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
    class(LnfDislType) :: this
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
    write(txthdr, '(a)') 'GRID DISL'
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
    write(txt, '(3a, i0)') 'VERTICES ', 'DOUBLE ', 'NDIM 2 2 ', this%nvert
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'CELLFDC ', 'DOUBLE ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'IAVERT ', 'INTEGER ', 'NDIM 1 ', this%nodesuser + 1
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'JAVERT ', 'INTEGER ', 'NDIM 1 ', size(this%javert)
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'IAVERTCELLS ', 'INTEGER ', 'NDIM 1 ', size(this%iavertcells)
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'JAVERTCELLS ', 'INTEGER ', 'NDIM 1 ', size(this%javertcells)
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
    write(iunit) this%nvert                                                     ! nvert
    write(iunit) size(this%javert)                                              ! njavert
    write(iunit) this%nja                                                       ! nja
    write(iunit) this%xorigin                                                   ! xorigin
    write(iunit) this%yorigin                                                   ! yorigin
    write(iunit) this%angrot                                                    ! angrot
    write(iunit) this%vertices                                                  ! vertices
    write(iunit) (this%cellfdc(i), i = 1, this%nodesuser)                       ! cellfdc
    write(iunit) this%iavert                                                    ! iavert
    write(iunit) this%javert                                                    ! javert
    write(iunit) this%iavertcells                                               ! iavert
    write(iunit) this%javertcells                                               ! javert
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
    class(LnfDislType) :: this
    integer(I4B), intent(in) :: nodeu
    character(len=*), intent(inout) :: str
    ! -- local
    character(len=10) :: unstr
    integer(I4B) :: i, j, k
! ------------------------------------------------------------------------------
    !
    write(unstr, '(i10)') nodeu
    str = '(' // trim(adjustl(unstr)) // ')'
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
    class(LnfDislType), intent(in) :: this
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
    class(LnfDislType) :: this
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
    class(LnfDislType) :: this
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
    ! get cell centers
    call this%get_cellxyz_disl(noden, xn, yn, zn)
    call this%get_cellxyz_disl(nodem, xm, ym, zm)
    if (nozee) then
      zn = DZERO
      zm = DZERO
    end if

    ! Set vector components based on cell centers
    call line_unit_vector(xn, yn, zn, xm, ym, zm, xcomp, ycomp, zcomp,       &
                          conlen)
    !
    ! -- return
    return
    end subroutine connection_vector

    ! return x,y coordinate for a node
  subroutine get_cellxy_disl(this, node, xcell, ycell)
    use InputOutputModule, only: get_jk
    class(LnfDislType), intent(in)  :: this
    integer(I4B), intent(in)        :: node         ! the reduced node number
    real(DP), intent(out)           :: xcell, ycell ! the x,y for the cell
    ! local
    real(DP) :: zcell

    call this%get_cellxyz_disl(node, xcell, ycell, zcell)
  end subroutine get_cellxy_disl

  ! return x,y,z coordinate for a node
  subroutine get_cellxyz_disl(this, node, xcell, ycell, zcell)
    use ConstantsModule, only: DZERO, DONE, DHALF
    use InputOutputModule, only: get_jk
    class(LnfDislType), intent(in)  :: this
    integer(I4B), intent(in)        :: node                ! the reduced node number
    real(DP), intent(out)           :: xcell, ycell, zcell ! the x,y for the cell
    ! local
    integer(I4B) :: nodeuser, ncell2d, k, numseg, ivert, segnum
    real(DP) :: lnflen, lnfcendist, curdist, distsegone, distsegtwo
    real(DP) :: disttot, xdist, ydist, zdist
    real, dimension (:), allocatable :: lnfsegdist

    nodeuser = this%get_nodeuser(node)
    numseg = this%iavert(nodeuser + 1) - 1 - this%iavert(nodeuser)
    allocate(lnfsegdist(numseg))
    lnflen = DZERO
    do ivert = this%iavert(nodeuser), this%iavert(nodeuser + 1) - 1
      ! calculate linear distance between each set of vertices
      xdist = abs(this%vertices(1, ivert) - this%vertices(1, ivert + 1))
      ydist = abs(this%vertices(2, ivert) - this%vertices(2, ivert + 1))
      zdist = abs(this%vertices(3, ivert) - this%vertices(3, ivert + 1))
      lnfsegdist(this%iavert(nodeuser) + 1) = sqrt(xdist * xdist + ydist * ydist &
                                                   + zdist * zdist)
      ! calculate total distance
      lnflen = lnflen + lnfsegdist(this%iavert(nodeuser) + 1)
    end do

    ! calculate cellfdc percent of the total distance
    lnfcendist = lnflen * this%cellfdc(nodeuser)

    ! find segment that contains midpoint and midpoint location
    curdist = DZERO
    do segnum = 1, numseg
      curdist = curdist + lnfsegdist(segnum)
      if(curdist > lnfcendist) then
        distsegtwo = curdist - lnfcendist
        distsegone = lnfsegdist(segnum) - distsegtwo
        xcell = this%vertices(1, this%iavert(nodeuser) + segnum) *      &
                (distsegtwo / lnfsegdist(segnum)) -                     &
                 this%vertices(1, this%iavert(nodeuser) + segnum - 1) * &
                (distsegone / lnfsegdist(segnum))
        ycell = this%vertices(2, this%iavert(nodeuser) + segnum) *      &
                (distsegtwo / lnfsegdist(segnum)) -                     &
                 this%vertices(2, this%iavert(nodeuser) + segnum - 1) * &
                (distsegone / lnfsegdist(segnum))
        zcell = this%vertices(3, this%iavert(nodeuser) + segnum) *      &
                (distsegtwo / lnfsegdist(segnum)) -                     &
                 this%vertices(3, this%iavert(nodeuser) + segnum - 1) * &
                (distsegone / lnfsegdist(segnum))
        exit
      end if
    end do

    deallocate(lnfsegdist)
  end subroutine get_cellxyz_disl

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
    class(LnfDislType) :: this
    character(len=*), intent(in) :: name_model
! ------------------------------------------------------------------------------
    !
    ! -- Allocate parent scalars
    call this%DisBaseType%allocate_scalars(name_model)
    !
    ! -- Allocate
    call mem_allocate(this%nvert, 'NVERT', this%origin)
    call mem_allocate(this%nsupportedgeoms, 'NSUPPORTEDGEOMS', this%origin)
    call mem_allocate(this%nactivegeoms, 'NACTIVEGEOMS', this%origin)
    !
    ! -- Initialize
    this%nvert = 0
    this%nactivegeoms = 0
    this%ndim = 1
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
    class(LnfDislType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Allocate arrays in DisBaseType (mshape, top, bot, area)
    call this%DisBaseType%allocate_arrays()
    !
    ! -- Allocate arrays for LnfDislType
    if(this%nodes < this%nodesuser) then
      call mem_allocate(this%nodeuser, this%nodes, 'NODEUSER', this%origin)
      call mem_allocate(this%nodereduced, this%nodesuser, 'NODEREDUCED',       &
                        this%origin)
    else
      call mem_allocate(this%nodeuser, 1, 'NODEUSER', this%origin)
      call mem_allocate(this%nodereduced, 1, 'NODEREDUCED', this%origin)
    endif
    !
    ! -- Initialize
    this%mshape(1) = this%nodesuser
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  function nodeu_from_string(this, lloc, istart, istop, in, iout, line, &
                             flag_string, allow_zero) result(nodeu)
! ******************************************************************************
! nodeu_from_string -- Receive a string and convert the string to a user
!   nodenumber.  The model discretization is DISL; read cell number.
!   If flag_string argument is present and true, the first token in string
!   is allowed to be a string (e.g. boundary name). In this case, if a string
!   is encountered, return value as -2.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LnfDislType) :: this
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    integer(I4B), intent(in) :: in
    integer(I4B), intent(in) :: iout
    character(len=*),  intent(inout) :: line
    logical, optional, intent(in) :: flag_string
    logical, optional, intent(in) :: allow_zero
    integer(I4B) :: nodeu
    ! -- local
    integer(I4B) :: lloclocal, ndum, istat, n
    real(DP) :: r
    character(len=LINELENGTH) :: ermsg, fname
! ------------------------------------------------------------------------------
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
    call urword(line, lloc, istart, istop, 2, nodeu, r, iout, in)
    !
    if (nodeu == 0) then
      if (present(allow_zero)) then
        if (allow_zero) then
          return
        endif
      endif
    endif
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
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    integer(I4B) :: nodeu
    ! -- dummy
    class(LnfDislType) :: this
    character(len=*), intent(inout) :: cellid
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    logical, optional, intent(in) :: flag_string
    logical, optional, intent(in) :: allow_zero
    ! -- local
    integer(I4B) :: lloclocal, istart, istop, ndum, n
    integer(I4B) :: istat
    real(DP) :: r
    character(len=LINELENGTH) :: ermsg, fname
! ------------------------------------------------------------------------------
    !
    if (present(flag_string)) then
      if (flag_string) then
        ! Check to see if first token in cellid can be read as an integer.
        lloclocal = 1
        call urword(cellid, lloclocal, istart, istop, 1, ndum, r, iout, inunit)
        read(cellid(istart:istop), *, iostat=istat) n
        if (istat /= 0) then
          ! First token in cellid is not an integer; return flag to this effect.
          nodeu = -2
          return
        endif
      endif
    endif
    !
    lloclocal = 1
    call urword(cellid, lloclocal, istart, istop, 2, nodeu, r, iout, inunit)
    !
    if (nodeu == 0) then
      if (present(allow_zero)) then
        if (allow_zero) then
          return
        endif
      endif
    endif
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
    class(LnfDislType), intent(inout)                  :: this
    character(len=*), intent(inout)                    :: line
    integer(I4B), intent(inout)                        :: lloc
    integer(I4B), intent(inout)                        :: istart
    integer(I4B), intent(inout)                        :: istop
    integer(I4B), intent(in)                           :: in
    integer(I4B), intent(in)                           :: iout
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: iarray
    character(len=*), intent(in)                       :: aname
    ! -- local
    integer(I4B) :: nval
    integer(I4B), dimension(:), pointer, contiguous :: itemp
! ------------------------------------------------------------------------------
    !
    ! -- Point the temporary pointer array, which is passed to the reading
    !    subroutine.  The temporary array will point to ibuff if it is a
    !    reduced structured system, or to iarray if it is an unstructured
    !    model.
    if(this%nodes < this%nodesuser) then
      nval = this%nodesuser
      itemp => this%ibuff
    else
      nval = this%nodes
      itemp => iarray
    endif
    !
    ! -- Read the array
    ! -- Read unstructured input
    call ReadArray(in, itemp, aname, this%ndim, nval, iout, 0)
    !
    ! -- If reduced model, then need to copy from itemp(=>ibuff) to iarray
    if(this%nodes <  this%nodesuser) then
      call this%fill_grid_array(itemp, iarray)
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
    class(LnfDislType), intent(inout)              :: this
    character(len=*), intent(inout)                :: line
    integer(I4B), intent(inout)                    :: lloc
    integer(I4B), intent(inout)                    :: istart
    integer(I4B), intent(inout)                    :: istop
    integer(I4B), intent(in)                       :: in
    integer(I4B), intent(in)                       :: iout
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: darray
    character(len=*), intent(in)                   :: aname
    ! -- local
    integer(I4B) :: nval
    real(DP), dimension(:), pointer, contiguous :: dtemp
! ------------------------------------------------------------------------------
    !
    ! -- Point the temporary pointer array, which is passed to the reading
    !    subroutine.  The temporary array will point to dbuff if it is a
    !    reduced structured system, or to darray if it is an unstructured
    !    model.
    if(this%nodes < this%nodesuser) then
      nval = this%nodesuser
      dtemp => this%dbuff
    else
      nval = this%nodes
      dtemp => darray
    endif
    !
    ! -- Read the array
    call ReadArray(in, dtemp, aname, this%ndim, nval, iout, 0)
    !
    ! -- If reduced model, then need to copy from dtemp(=>dbuff) to darray
    if(this%nodes <  this%nodesuser) then
      call this%fill_grid_array(dtemp, darray)
    endif
    !
    ! -- return
    return
  end subroutine read_dbl_array

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
    class(LnfDislType), intent(inout)              :: this
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: darray
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
    real(DP), dimension(:), pointer, contiguous :: dtemp
    ! -- formats
    character(len=*),parameter :: fmthsv = &
      "(1X,/1X,a,' WILL BE SAVED ON UNIT ',I4, &
       &' AT END OF TIME STEP',I5,', STRESS PERIOD ',I4)"
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    nlay = 1
    nrow = 1
    ncol = this%mshape(1)
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
    class(LnfDislType) :: this
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
    nlay = 1
    nrow = 1
    ncol = this%mshape(1)
    !
    ! -- Use ubdsv06 to write list header
    call ubdsv06(kstp, kper, text, textmodel, textpackage, dstmodel, dstpackage,&
                 ibdchn, naux, auxtxt, ncol, nrow, nlay,                        &
                 nlist, iout, delt, pertim, totim)
    !
    ! -- return
    return
  end subroutine record_srcdst_list_header

end module LnfDislModule

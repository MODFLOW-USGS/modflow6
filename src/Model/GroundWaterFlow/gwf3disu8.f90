module GwfDisuModule

  use ArrayReadersModule, only: ReadArray
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENMODELNAME, LENORIGIN, LINELENGTH
  use ConnectionsModule, only: ConnectionsType
  use InputOutputModule, only: URWORD, ulasav, ulaprufw, ubdsv1, ubdsv06
  use SimModule, only: count_errors, store_error, store_error_unit, ustop
  use BaseDisModule, only: DisBaseType
  use BlockParserModule, only: BlockParserType
  use MemoryManagerModule, only: mem_allocate
  use TdisModule,          only: kstp, kper, pertim, totim, delt

  implicit none

  private
  public :: GwfDisuType
  public :: disu_cr

  type, extends(DisBaseType) :: GwfDisuType
    integer(I4B), pointer                              :: nvert       => null() ! number of x,y vertices
    real(DP), dimension(:,:), pointer                  :: vertices    => null() ! cell vertices stored as 2d array of x and y
    real(DP), dimension(:,:), pointer                  :: cellxy      => null() ! cell center stored as 2d array of x and y
    integer(I4B), dimension(:), pointer                :: iavert      => null() ! cell vertex pointer ia array
    integer(I4B), dimension(:), pointer                :: javert      => null() ! cell vertex pointer ja array
  contains
    procedure :: dis_df => disu_df
    procedure :: dis_da => disu_da
    procedure :: get_nodenumber_idx1
    procedure :: get_nodeuser
    procedure :: nodeu_to_string
    procedure :: nodeu_from_string
    procedure :: nodeu_from_cellid
    procedure :: connection_normal
    procedure :: connection_vector
    procedure :: supports_layers
    procedure :: get_ncpl
    procedure, public :: record_array
    procedure, public :: read_layer_array
    procedure, public :: record_srcdst_list_header
    procedure, public :: nlarray_to_nodelist
    ! -- private
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: read_options
    procedure :: read_dimensions
    procedure :: read_data
    procedure :: read_vertices
    procedure :: read_cell2d
    procedure :: write_grb
    !
    ! -- Read a node-sized model array (reduced or not)
    procedure :: read_int_array
    procedure :: read_dbl_array
  end type GwfDisuType

  contains

  subroutine disu_cr(dis, name_model, inunit, iout)
! ******************************************************************************
! disu_cr -- Create discretization object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DisBaseType), pointer :: dis
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    ! -- local
    type(GwfDisuType), pointer :: disnew
! ------------------------------------------------------------------------------
    !
    ! -- Create a new discretization object
    allocate(disnew)
    dis => disnew
    !
    ! -- Allocate scalars and assign data
    call dis%allocate_scalars(name_model)
    dis%inunit = inunit
    dis%iout = iout
    !
    ! -- Initialize block parser
    call dis%parser%Initialize(dis%inunit, dis%iout)
    !
    ! -- Return
    return
  end subroutine disu_cr

  subroutine disu_df(this)
! ******************************************************************************
! disu_df -- Read discretization information from DISU input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfDisuType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Identify
    write(this%iout,1) this%inunit
  1 format(1X,/1X,'DISU -- UNSTRUCTURED GRID DISCRETIZATION PACKAGE,',         &
                  ' VERSION 2 : 3/27/2014 - INPUT READ FROM UNIT ',I0,//)
    !
    call this%read_options()
    call this%read_dimensions()
    call this%allocate_arrays()
    call this%read_data()
    !
    ! -- Create and fill the connections object
    allocate(this%con)
    call this%con%read_from_block(this%name_model, this%nodes, this%nja,       &
                                  this%inunit, this%iout)
    this%njas = this%con%njas
    !
    ! -- If NVERT specified and greater than 0, then read VERTICES and CELL2D
    if(this%nvert > 0) then
      call this%read_vertices()
      call this%read_cell2d()
    else
      ! -- connection direction information cannot be calculated
      this%icondir = 0
    endif
    !
    ! -- Return
    return
  end subroutine disu_df

  subroutine disu_da(this)
! ******************************************************************************
! disu_da -- Deallocate discretization object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwfDisuType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- DisBaseType deallocate
    call this%DisBaseType%dis_da()
    !
    ! -- Return
    return
  end subroutine disu_da

  subroutine nodeu_to_string(this, nodeu, str)
! ******************************************************************************
! nodeu_to_string -- Convert user node number to a string in the form of
! (nodenumber) or (k,i,j)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfDisuType) :: this
    integer(I4B), intent(in) :: nodeu
    character(len=*), intent(inout) :: str
    ! -- local
    character(len=10) :: nstr
! ------------------------------------------------------------------------------
    !
    write(nstr, '(i0)') nodeu
    str = '(' // trim(adjustl(nstr)) // ')'
    !
    ! -- return
    return
  end subroutine nodeu_to_string

  integer(I4B) function get_nodeuser(this, noder) &
    result(nodenumber)
! ******************************************************************************
! get_nodeuser -- Return the user nodenumber from the reduced node number
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfDisuType) :: this
    integer(I4B), intent(in) :: noder
! ------------------------------------------------------------------------------
    !
    nodenumber = noder
    !
    ! -- return
    return
  end function get_nodeuser

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read discretization options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, count_errors, store_error
    implicit none
    class(GwfDisuType) :: this
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr, nerr
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
      write(this%iout,'(1x,a)')'PROCESSING DISCRETIZATION OPTIONS'
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
        end select
      end do
      write(this%iout,'(1x,a)')'END OF DISCRETIZATION OPTIONS'
    else
      write(this%iout,'(1x,a)')'NO OPTION BLOCK DETECTED.'
    end if
    if(this%lenuni==0) write(this%iout,'(1x,a)') 'MODEL LENGTH UNIT IS UNDEFINED'
    !
    nerr = count_errors()
    if(nerr > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_dimensions(this)
! ******************************************************************************
! read_dimensions -- Read discretization information from file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, count_errors, store_error
    implicit none
    class(GwfDisuType) :: this
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- Initialize dimensions
    this%nodes = -1
    this%nja = -1
    !
    ! -- get options block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING DISCRETIZATION DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NODES')
            this%nodes =  this%parser%GetInteger()
            write(this%iout,'(4x,a,i0)') 'NODES = ', this%nodes
          case ('NJA')
            this%nja = this%parser%GetInteger()
            write(this%iout,'(4x,a,i0)') 'NJA   = ', this%nja
          case ('NVERT')
            this%nvert = this%parser%GetInteger()
            write(this%iout,'(3x,a,i0)') 'NVERT = ', this%nvert
            write(this%iout,'(3x,a)') 'VERTICES AND CELL2D BLOCKS WILL ' //    &
              'BE READ BELOW. '
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN DIS DIMENSION: ',      &
                                      trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF DISCRETIZATION OPTIONS'
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Set nodesuser to nodes
    this%nodesuser = this%nodes
    !
    ! -- verify dimensions were set
    if(this%nodes < 1) then
      call store_error( &
          'ERROR.  NODES WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.')
      call ustop()
    endif
    if(this%nja < 1) then
      call store_error( &
          'ERROR.  NJA WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.')
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine read_dimensions

  subroutine read_data(this)
! ******************************************************************************
! read_data -- Read discretization data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: LINELENGTH, DZERO
    use SimModule, only: ustop, count_errors, store_error, store_error_unit
    ! -- dummy
    class(GwfDisuType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: n
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    real(DP) :: dz
    integer(I4B), parameter :: nname = 3
    logical,dimension(nname) :: lname
    character(len=24),dimension(nname) :: aname(nname)
    ! -- formats
    character(len=*), parameter :: fmtdz = &
      "('ERROR. CELL ', i0, ' WITH THICKNESS <= 0. TOP, BOT: ', 2(1pg24.15))"
    character(len=*), parameter :: fmtarea = &
      "('ERROR. CELL ', i0, ' WITH AREA <= 0. AREA: ', 1(1pg24.15))"
    ! -- data
    data aname(1) /'                     TOP'/
    data aname(2) /'                     BOT'/
    data aname(3) /'                    AREA'/
! ------------------------------------------------------------------------------
    !
    ! -- get disdata block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    lname(:) = .false.
    if(isfound) then
      write(this%iout,'(1x,a)')'PROCESSING GRIDDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('TOP')
            call ReadArray(this%parser%iuactive, this%top, aname(1), &
                            this%ndim, this%nodes, this%iout, 0)
            lname(1) = .true.
          case ('BOT')
            call ReadArray(this%parser%iuactive, this%bot, aname(2), &
                            this%ndim, this%nodes, this%iout, 0)
            lname(2) = .true.
          case ('AREA')
            call ReadArray(this%parser%iuactive, this%area, aname(3), &
                            this%ndim, this%nodes, this%iout, 0)
            lname(3) = .true.
          case default
            write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN GRIDDATA TAG: ', &
                                     trim(keyword)
            call store_error(errmsg)
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
    ! -- verify all items were read
    do n = 1, nname
      if(.not. lname(n)) then
        write(errmsg,'(1x,a,a)') &
          'ERROR.  REQUIRED INPUT WAS NOT SPECIFIED: ', aname(n)
        call store_error(errmsg)
      endif
    enddo
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Check for zero and negative thickness and zero or negative areas
    do n = 1, this%nodes
      dz = this%top(n) - this%bot(n)
      if (dz <= DZERO) then
        write(errmsg, fmt=fmtdz) n, this%top(n), this%bot(n)
        call store_error(errmsg)
      endif
      if (this%area(n) <= DZERO) then
        write(errmsg, fmt=fmtarea) n, this%area(n)
        call store_error(errmsg)
      endif
    enddo
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine read_data

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
    class(GwfDisuType) :: this
    character(len=LINELENGTH) :: line
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
    ! -- dummy
    class(GwfDisuType) :: this
    character(len=LINELENGTH) :: line
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
    allocate(maxnnz(this%nodes))
    do i = 1, this%nodes
      maxnnz(i) = 5
    enddo
    call vertspm%init(this%nodes, this%nvert, maxnnz)
    !
    ! --Read CELL2D block
    call this%parser%GetBlock('CELL2D', isfound, ierr, supportOpenClose=.true.)
    if(isfound) then
      write(this%iout,'(/,1x,a)') 'PROCESSING CELL2D'
      do i = 1, this%nodes
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
    allocate(this%iavert(this%nodes+1))
    allocate(this%javert(vertspm%nnz))
    call vertspm%filliaja(this%iavert, this%javert, ierr)
    call vertspm%destroy()
    !
    ! -- Write information
    write(this%iout, fmtncpl) this%nodes
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
    class(GwfDisuType) :: this
    integer(I4B), dimension(:), intent(in) :: icelltype
    ! -- local
    integer(I4B) :: i, iunit, ntxt
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
    ntxt = 10
    if (this%nvert > 0) ntxt = ntxt + 5
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
    write(txthdr, '(a)') 'GRID DISU'
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
    write(txt, '(3a, i0)') 'NODES ', 'INTEGER ', 'NDIM 0 # ', this%nodes
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'NJA ', 'INTEGER ', 'NDIM 0 # ', this%con%nja
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, 1pg24.15)') 'XORIGIN ', 'DOUBLE ', 'NDIM 0 # ', this%xorigin
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, 1pg24.15)') 'YORIGIN ', 'DOUBLE ', 'NDIM 0 # ', this%yorigin
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, 1pg24.15)') 'ANGROT ', 'DOUBLE ', 'NDIM 0 # ', this%angrot
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'TOP ', 'DOUBLE ', 'NDIM 1 ', this%nodes
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'BOT ', 'DOUBLE ', 'NDIM 1 ', this%nodes
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'IA ', 'INTEGER ', 'NDIM 1 ', this%nodes + 1
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'JA ', 'INTEGER ', 'NDIM 1 ', this%con%nja
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'ICELLTYPE ', 'INTEGER ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    !
    ! -- if vertices have been read then write additional header information
    if (this%nvert > 0) then
      write(txt, '(3a, i0)') 'VERTICES ', 'DOUBLE ', 'NDIM 2 2 ', this%nvert
      txt(lentxt:lentxt) = new_line('a')
      write(iunit) txt
      write(txt, '(3a, i0)') 'CELLX ', 'DOUBLE ', 'NDIM 1 ', this%nodes
      txt(lentxt:lentxt) = new_line('a')
      write(iunit) txt
      write(txt, '(3a, i0)') 'CELLY ', 'DOUBLE ', 'NDIM 1 ', this%nodes
      txt(lentxt:lentxt) = new_line('a')
      write(iunit) txt
      write(txt, '(3a, i0)') 'IAVERT ', 'INTEGER ', 'NDIM 1 ', this%nodes + 1
      txt(lentxt:lentxt) = new_line('a')
      write(iunit) txt
      write(txt, '(3a, i0)') 'JAVERT ', 'INTEGER ', 'NDIM 1 ', size(this%javert)
      txt(lentxt:lentxt) = new_line('a')
      write(iunit) txt
    endif
    !
    ! -- write data
    write(iunit) this%nodes                                                     ! nodes
    write(iunit) this%nja                                                       ! nja
    write(iunit) this%xorigin                                                   ! xorigin
    write(iunit) this%yorigin                                                   ! yorigin
    write(iunit) this%angrot                                                    ! angrot
    write(iunit) this%top                                                       ! top
    write(iunit) this%bot                                                       ! bot
    write(iunit) this%con%ia                                                    ! ia
    write(iunit) this%con%ja                                                    ! ja
    write(iunit) icelltype                                                      ! icelltype
    !
    ! -- if vertices have been read then write additional data
    if (this%nvert > 0) then
      write(iunit) this%vertices                                                ! vertices
      write(iunit) (this%cellxy(1, i), i = 1, this%nodes)                       ! cellx
      write(iunit) (this%cellxy(2, i), i = 1, this%nodes)                       ! celly
      write(iunit) this%iavert                                                  ! iavert
      write(iunit) this%javert                                                  ! javert
    endif
    !
    ! -- Close the file
    close(iunit)
    !
    ! -- return
    return
  end subroutine write_grb

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
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    implicit none
    class(GwfDisuType), intent(in) :: this
    integer(I4B), intent(in) :: nodeu
    integer(I4B), intent(in) :: icheck
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: nodenumber
! ------------------------------------------------------------------------------
    !
    if(icheck /= 0) then
      if(nodeu < 1 .or. nodeu > this%nodes) then
        write(errmsg, '(a,i10)') &
          'Nodenumber less than 1 or greater than nodes:', nodeu
        call store_error(errmsg)
      endif
    endif
    !
    ! -- set node number to passed in nodenumber since there is a one to one
    !    mapping for an unstructured grid
    nodenumber = nodeu
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
    class(GwfDisuType) :: this
    integer(I4B), intent(in) :: noden
    integer(I4B), intent(in) :: nodem
    integer(I4B), intent(in) :: ihc
    real(DP), intent(inout) :: xcomp
    real(DP), intent(inout) :: ycomp
    real(DP), intent(inout) :: zcomp
    integer(I4B), intent(in) :: ipos
    ! -- local
    !integer(I4B) :: ipos
    real(DP) :: angle, dmult
! ------------------------------------------------------------------------------
    !
    ! -- Set vector components based on ihc
    if(ihc == 0) then
      !
      ! -- connection is vertical
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
    use DisvGeom, only: line_unit_vector
    ! -- dummy
    class(GwfDisuType) :: this
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
    real(DP) :: xn, xm, yn, ym, zn, zm
! ------------------------------------------------------------------------------
    !
    ! -- Find xy coords
    xn = this%cellxy(1, noden)
    yn = this%cellxy(2, noden)
    xm = this%cellxy(1, nodem)
    ym = this%cellxy(2, nodem)
    !
    ! -- Set vector components based on ihc
    if(ihc == 0) then
      !
      ! -- vertical connection, calculate z as cell center elevation
      zn = this%bot(noden) + DHALF * (this%top(noden) - this%bot(noden))
      zm = this%bot(nodem) + DHALF * (this%top(nodem) - this%bot(nodem))
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
    endif
    !
    ! -- Use coords to find vector components and connection length
    call line_unit_vector(xn, yn, zn, xm, ym, zm, xcomp, ycomp, zcomp,         &
                          conlen)
    !
    ! -- return
    return
  end subroutine connection_vector

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
    class(GwfDisuType) :: this
    character(len=*), intent(in) :: name_model
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Allocate parent scalars
    call this%DisBaseType%allocate_scalars(name_model)
    !
    ! -- Allocate variables for DISU
    call mem_allocate(this%nvert, 'NVERT', this%origin)
    !
    ! -- Set values
    this%ndim = 1
    this%nvert = 0
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
    class(GwfDisuType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Allocate arrays in DisBaseType (mshape, top, bot, area)
    call this%DisBaseType%allocate_arrays()
    !
    ! -- Allocate arrays in DISU
    call mem_allocate(this%vertices, 2, this%nvert, 'VERTICES', this%origin)
    if(this%nvert > 0) then
      call mem_allocate(this%cellxy, 2, this%nodes, 'CELLXY', this%origin)
    else
      call mem_allocate(this%cellxy, 2, 0, 'CELLXY', this%origin)
    endif
    !
    ! -- Initialize
    this%mshape(1) = this%nodes
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
    class(GwfDisuType)               :: this
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
    class(GwfDisuType)               :: this
    character(len=*),  intent(inout) :: cellid
    integer(I4B),           intent(in)    :: inunit
    integer(I4B),           intent(in)    :: iout
    logical, optional, intent(in)    :: flag_string
    logical, optional, intent(in)    :: allow_zero
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
        read(cellid(istart:istop),*,iostat=istat)n
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

  logical function supports_layers(this)
    implicit none
    ! -- dummy
    class(GwfDisuType) :: this
    !
    supports_layers = .false.
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
    class(GwfDisuType) :: this
! ------------------------------------------------------------------------------
    !
    get_ncpl = this%nodes
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
    class(GwfDisuType), intent(inout)                  :: this
    character(len=*), intent(inout)                    :: line
    integer(I4B), intent(inout)                        :: lloc
    integer(I4B), intent(inout)                        :: istart
    integer(I4B), intent(inout)                        :: istop
    integer(I4B), intent(in)                           :: in
    integer(I4B), intent(in)                           :: iout
    integer(I4B), dimension(:), pointer, intent(inout) :: iarray
    character(len=*), intent(in)                       :: aname
    ! -- local
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
    nlay = 1
    nrow = 1
    ncol = this%nodes
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
    ! -- Read unstructured input
    call ReadArray(in, itemp, aname, this%ndim, nval, iout, 0)
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
    class(GwfDisuType), intent(inout)              :: this
    character(len=*), intent(inout)                :: line
    integer(I4B), intent(inout)                    :: lloc
    integer(I4B), intent(inout)                    :: istart
    integer(I4B), intent(inout)                    :: istop
    integer(I4B), intent(in)                       :: in
    integer(I4B), intent(in)                       :: iout
    real(DP), dimension(:), pointer, intent(inout) :: darray
    character(len=*), intent(in)                   :: aname
    ! -- local
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
    nlay = 1
    nrow = 1
    ncol = this%nodes
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
    ! -- Read structured input
    call ReadArray(in, dtemp, aname, this%ndim, nval, iout, 0)
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
    class(GwfDisuType) :: this
    integer(I4B), intent(in) :: maxbnd
    integer(I4B), intent(in) :: ncolbnd
    integer(I4B), dimension(maxbnd) :: nodelist
    real(DP), dimension(ncolbnd, maxbnd), intent(inout) :: darray
    integer(I4B), intent(in) :: icolbnd
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: ipos
! ------------------------------------------------------------------------------
    !
    ! -- Read unstructured and then copy into darray
    call ReadArray(inunit, this%dbuff, aname, this%ndim, maxbnd, iout, 0)
    do ipos = 1, maxbnd
      darray(icolbnd, ipos) = this%dbuff(ipos)
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
    class(GwfDisuType), intent(inout)              :: this
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
    integer(I4B) :: istart, istop
    real(DP), dimension(:), pointer :: dtemp
    ! -- formats
    character(len=*),parameter :: fmthsv = &
      "(1X,/1X,a,' WILL BE SAVED ON UNIT ',I4, &
       &' AT END OF TIME STEP',I5,', STRESS PERIOD ',I4)"
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    nlay = 1
    nrow = 1
    ncol = this%nodes
    !
    nval = this%nodes
    dtemp => darray
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
    class(GwfDisuType) :: this
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
    ncol = this%nodes
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
    class(GwfDisuType) :: this
    integer(I4B), intent(in) :: maxbnd
    integer(I4B), dimension(maxbnd), intent(inout) :: nodelist
    integer(I4B), intent(inout) :: nbound
    character(len=*), intent(in) :: aname
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: noder
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- For unstructured, read nodelist directly, then check node numbers
    call ReadArray(inunit, nodelist, aname, this%ndim, maxbnd, iout, 0)
    do noder = 1, maxbnd
      if(noder < 1 .or. noder > this%nodes) then
        write(errmsg, *) 'ERROR.  INVALID NODE NUMBER: ', noder
        call store_error(errmsg)
        call ustop()
      endif
    enddo
    nbound = maxbnd
    !
    ! -- return
  end subroutine nlarray_to_nodelist

end module GwfDisuModule

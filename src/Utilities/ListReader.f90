! -- Generic List Reader Module
module ListReaderModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, LENBOUNDNAME, LENTIMESERIESNAME, &
                             LENAUXNAME, LENLISTLABEL, DONE
  use SimModule,       only: store_error_unit
  implicit none
  private
  public ListReaderType
  
  type :: ListReaderType
    integer(I4B) :: in = 0                                                       ! unit number of file containing control record
    integer(I4B) :: inlist = 0                                                   ! unit number of file from which list will be read
    integer(I4B) :: iout = 0                                                     ! unit number to output messages
    integer(I4B) :: inamedbound = 0                                              ! flag indicating boundary names are to be read
    integer(I4B) :: ierr = 0                                                     ! error flag
    integer(I4B) :: nlist = 0                                                    ! number of entries in list.  -1 indicates number will be automatically determined
    integer(I4B) :: ibinary = 0                                                  ! flag indicating to read binary list
    integer(I4B) :: istart = 0                                                   ! string starting location
    integer(I4B) :: istop = 0                                                    ! string ending location
    integer(I4B) :: lloc = 0                                                     ! entry number in line
    integer(I4B) :: iclose = 0                                                   ! flag indicating whether or not to close file
    integer(I4B) :: ndim = 0                                                     ! number of dimensions in model
    integer(I4B) :: ntxtrlist = 0                                                ! number of text entries found in rlist
    integer(I4B) :: ntxtauxvar = 0                                               ! number of text entries found in auxvar
    character(len=LENLISTLABEL) :: label = ''                                    ! label for printing list
    character(len=:), allocatable, private :: line                               ! current line
    integer(I4B), dimension(:), pointer, contiguous :: mshape => null()          ! pointer to model shape
    integer(I4B), dimension(:), pointer, contiguous :: nodelist => null()        ! pointer to nodelist
    real(DP), dimension(:, :), pointer, contiguous :: rlist => null()            ! pointer to rlist
    real(DP), dimension(:, :), pointer, contiguous :: auxvar  => null()          ! pointer to auxvar
    character(len=16), dimension(:), pointer :: auxname => null()                ! pointer to aux names
    character(len=LENBOUNDNAME), dimension(:), pointer,                         &
                                 contiguous :: boundname => null()               ! pointer to boundname
    integer(I4B), dimension(:), allocatable :: idxtxtrow                         ! row locations of text in rlist
    integer(I4B), dimension(:), allocatable :: idxtxtcol                         ! col locations of text in rlist
    integer(I4B), dimension(:), allocatable :: idxtxtauxrow                      ! row locations of text in auxvar
    integer(I4B), dimension(:), allocatable :: idxtxtauxcol                      ! col locations of text in auxvar
    character(len=LENTIMESERIESNAME), dimension(:), allocatable :: txtrlist      ! text found in rlist
    character(len=LENTIMESERIESNAME), dimension(:), allocatable :: txtauxvar     ! text found in auxvar
  contains
    procedure          :: read_list
    procedure          :: write_list
    procedure, private :: read_control_record
    procedure, private :: read_data
    procedure, private :: set_openclose
    procedure, private :: read_ascii
    procedure, private :: read_binary
  end type ListReaderType
  
  contains
  
  subroutine read_list(this, in, iout, nlist, inamedbound, mshape, nodelist,   &
                       rlist, auxvar, auxname, boundname, label)
! ******************************************************************************
! init -- Initialize the reader
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBOUNDNAME
    ! -- dummy
    class(ListReaderType) :: this
    integer(I4B), intent(in) :: in
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(inout) :: nlist
    integer(I4B), intent(in) :: inamedbound
    integer(I4B), dimension(:), intent(in), contiguous, pointer :: mshape
    integer(I4B), dimension(:), intent(inout), contiguous, pointer :: nodelist
    real(DP), dimension(:, :), intent(inout), contiguous, pointer :: rlist
    real(DP), dimension(:, :), intent(inout), contiguous, pointer :: auxvar
    character(len=LENAUXNAME), dimension(:), intent(inout), target :: auxname
    character(len=LENBOUNDNAME), dimension(:), pointer, contiguous, intent(inout) :: boundname
    character(len=LENLISTLABEL), intent(in) :: label
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Copy variables
    this%in = in
    this%iout = iout
    this%nlist = nlist
    this%inamedbound = inamedbound
    this%ndim = size(mshape)
    this%label = label
    !
    ! -- Set pointers
    this%mshape => mshape
    this%nodelist => nodelist
    this%rlist => rlist
    this%auxvar => auxvar
    this%auxname => auxname
    this%boundname => boundname
    !
    ! -- Allocate arrays for storing text and text locations
    if(.not. allocated(this%idxtxtrow)) allocate(this%idxtxtrow(0))
    if(.not. allocated(this%idxtxtcol)) allocate(this%idxtxtcol(0))   
    if(.not. allocated(this%idxtxtauxrow)) allocate(this%idxtxtauxrow(0))
    if(.not. allocated(this%idxtxtauxcol)) allocate(this%idxtxtauxcol(0))
    if(.not. allocated(this%txtrlist)) allocate(this%txtrlist(0))
    if(.not. allocated(this%txtauxvar)) allocate(this%txtauxvar(0))
    !
    ! -- Read control record
    call this%read_control_record()
    !
    ! -- Read data
    call this%read_data()
    !
    ! -- Set nlist for return
    nlist = this%nlist
    !
    ! -- return
    return
  end subroutine read_list
  
  subroutine read_control_record(this)
! ******************************************************************************
! read_control_record -- Check for a control record, and parse if found
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: u9rdcom, urword
    ! -- dummy
    class(ListReaderType) :: this
    ! -- local
    integer(I4B) :: idum
    real(DP) :: r
    ! -- formats
    character(len=*), parameter :: fmtlsf = &
      "(1X,'LIST SCALING FACTOR=',1PG12.5)"
! ------------------------------------------------------------------------------
    !
    ! -- Set default values, which may be changed by control record
    this%inlist = this%in
    this%iclose = 0
    this%ibinary = 0
    !
    ! -- Read to the first non-commented line
    call u9rdcom(this%in, this%iout, this%line, this%ierr)
    this%lloc = 1
    call urword(this%line, this%lloc, this%istart, this%istop, 1, idum, r,     &
                this%iout, this%in)
    !
    ! -- Parse record
    select case(this%line(this%istart:this%istop))
    case('OPEN/CLOSE')
      call this%set_openclose()
    end select
    !
    ! -- return
    return
  end subroutine read_control_record
  
  subroutine set_openclose(this)
! ******************************************************************************
! set_openclose -- set up for open/close file
!
!   OPEN/CLOSE fname [(BINARY)]
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: u9rdcom, urword, openfile
    use OpenSpecModule, only: form, access
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    ! -- dummy
    class(ListReaderType) :: this
    ! -- local
    integer(I4B) :: idum, itmp
    real(DP) :: r
    logical :: exists
    integer(I4B) :: nunopn = 99
    character(len=LINELENGTH) :: fname
    character(len=LINELENGTH) :: errmsg
    ! -- formats
    character(len=*), parameter :: fmtocne = &
      "('Specified OPEN/CLOSE file ',(A),' does not exist')"
    character(len=*), parameter :: fmtobf = &
      "(1X,/1X,'OPENING BINARY FILE ON UNIT ',I0,':',/1X,A)"
    character(len=*), parameter :: fmtobfnlist = &
      "(1X, 'TO READ ', I0, ' RECORDS.')"
    character(len=*), parameter :: fmtofnlist = &
      "(1x,'TO READ ', I0, ' RECORDS.')"
    character(len=*), parameter :: fmtof = &
      "(1X,/1X,'OPENING FILE ON UNIT ',I0,':',/1X,A)"
! ------------------------------------------------------------------------------
    !
    ! -- get filename
    call urword(this%line, this%lloc, this%istart, this%istop, 0, idum, r,     &
                this%iout, this%in)
    fname = this%line(this%istart:this%istop)
    !
    ! -- check to see if file OPEN/CLOSE file exists
    inquire(file=fname, exist=exists)
    if (.not. exists) then
      write(errmsg, fmtocne) this%line(this%istart:this%istop)
      call store_error(errmsg)
      call store_error('Specified OPEN/CLOSE file does not exist')
      call store_error_unit(this%in)
    endif
    !
    ! -- Check for (BINARY) keyword
    call urword(this%line, this%lloc, this%istart, this%istop, 1, idum, r,     &
                this%iout, this%in)
    if(this%line(this%istart:this%istop) == '(BINARY)') this%ibinary = 1
    !
    ! -- Open the file depending on ibinary flag
    this%inlist = nunopn
    if(this%ibinary == 1) then
      itmp = this%iout
      if(this%iout > 0) then
        itmp = 0
        write(this%iout, fmtobf) this%inlist, trim(adjustl(fname))
        if(this%nlist > 0) write(this%iout, fmtobfnlist) this%nlist
      endif
      call openfile(this%inlist, itmp, fname, 'OPEN/CLOSE', fmtarg_opt=form,   &
                    accarg_opt=access)
    else
      itmp = this%iout
      if(this%iout > 0) then
        itmp = 0
        write(this%iout, fmtof) this%inlist, trim(adjustl(fname))
        if(this%nlist > 0) write(this%iout, fmtofnlist) this%nlist
      endif
      call openfile(this%inlist, itmp, fname, 'OPEN/CLOSE')
    end if
    !
    ! -- Set iclose to 1 because it is open/close, to indicate that the
    !    file needs to be closed after the list is read
    this%iclose = 1
    !
    ! -- Read the first line from inlist to be consistent with how the list is
    !    read when it is included in the package input file
    if(this%ibinary /= 1) call u9rdcom(this%inlist, this%iout, this%line,      &
                                       this%ierr)
    !
    ! -- return
    return
  end subroutine set_openclose
  
  subroutine read_data(this)
! ******************************************************************************
! read_data -- read the data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(ListReaderType) :: this
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Read the list
    if(this%ibinary == 1) then
      call this%read_binary()
    else
      call this%read_ascii()
    endif
    !
    ! -- if open/close, then close file
    if(this%iclose == 1) then
      close(this%inlist)
    endif
    ! -- return
    return
  end subroutine read_data
  
  subroutine read_binary(this)
! ******************************************************************************
! read_binary -- read the data from a binary file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH, LENBIGLINE
    use InputOutputModule, only: get_node
    use SimModule, only: store_error
    ! -- dummy
    class(ListReaderType) :: this
    ! -- local
    integer(I4B) :: mxlist, ldim, naux, nod, ii, jj
    character(len=LINELENGTH) :: fname
    character(len=LENBIGLINE) :: errmsg
    integer(I4B), dimension(:), allocatable :: cellid
    ! -- formats
    character(len=*), parameter :: fmtmxlsterronly = &
      "('ERROR READING LIST FROM FILE: '," // &
       "a,' ON UNIT: ',I0," // &
       "' THE NUMBER OF RECORDS ENCOUNTERED EXCEEDS THE MAXIMUM NUMBER " // &
       "OF RECORDS.  TRY INCREASING MAXBOUND FOR THIS LIST." // &
       "  NUMBER OF RECORDS: ',I0,' MAXBOUND: ',I0)"
    character(len=*), parameter :: fmtlsterronly = &
      "('ERROR READING LIST FROM FILE: '," // &
       "1x,a,1x,' ON UNIT: ',I0)"
! ------------------------------------------------------------------------------
    !
    ! -- determine array sizes
    mxlist = size(this%rlist, 2)
    ldim = size(this%rlist, 1)
    naux = size(this%auxvar, 1)
    !
    ! -- Allocate arrays
    allocate(cellid(this%ndim))
    !
    ii = 1
    readloop: do
      !
      ! -- read layer, row, col, or cell number
      read(this%inlist, iostat=this%ierr) cellid

      ! -- If not end of record, then store nodenumber, else
      !    calculate lstend and nlist, and exit readloop
      select case(this%ierr)
      case(0)
        !
        ! -- Check range
        if(ii > mxlist) then
          inquire(unit=this%inlist, name=fname)
          write(errmsg, fmtmxlsterronly) fname, this%inlist, ii, mxlist
          call store_error(errmsg, terminate=.TRUE.)
        endif
        !
        ! -- Store node number and read the remainder of the record
        if(this%ndim == 1) then
          nod = cellid(1)
        elseif(this%ndim == 2) then
          nod = get_node(cellid(1), 1, cellid(2),                              &
                         this%mshape(1), 1, this%mshape(2))
        else
          nod = get_node(cellid(1), cellid(2), cellid(3),                      &
                         this%mshape(1), this%mshape(2), this%mshape(3))
        endif
        this%nodelist(ii) = nod
        read(this%inlist, iostat=this%ierr) (this%rlist(jj,ii),jj=1,ldim),     &
                                            (this%auxvar(ii,jj),jj=1,naux)
        if(this%ierr /= 0) then
          inquire(unit=this%inlist, name=fname)
          write(errmsg, fmtlsterronly) trim(adjustl(fname)), this%inlist
          call store_error(errmsg, terminate=.TRUE.)
        endif
        !
      case(:-1)
        !
        ! -- End of record was encountered
        this%nlist = ii - 1
        exit readloop
        !
      case(1:)
        !
        ! -- Error
        inquire(unit=this%inlist, name=fname)
        write(errmsg, fmtlsterronly) trim(adjustl(fname)), this%inlist
        call store_error(errmsg, terminate=.TRUE.)
        !
      end select
      !
      ! -- If nlist is known, then exit when nlist values have been read
      if(this%nlist > 0) then
        if(ii == this%nlist) exit readloop
      endif
      !
      ! -- increment ii
      ii = ii + 1
      !
    enddo readloop
    !
    ! -- return
    return
  end subroutine read_binary
  
  subroutine read_ascii(this)
! ******************************************************************************
! read_ascii -- read the data from an ascii file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBOUNDNAME, LINELENGTH, DZERO
    use InputOutputModule, only: u9rdcom, urword, get_node
    use SimModule, only: store_error, count_errors
    use ArrayHandlersModule, only: ExpandArray
    ! -- dummy
    class(ListReaderType) :: this
    ! -- local  
    integer(I4B) :: mxlist, ldim, naux
    integer(I4B) :: ii, jj, idum, nod, istat, increment
    real(DP) :: r
    integer(I4B), dimension(:), allocatable :: cellid
    character(len=LINELENGTH) :: fname
    character(len=LINELENGTH) :: errmsg
    ! -- formats
    character(len=*), parameter :: fmtmxlsterronly = &
      "('***ERROR READING LIST. &
       &THE NUMBER OF RECORDS ENCOUNTERED EXCEEDS THE MAXIMUM NUMBER " // &
       "OF RECORDS.  TRY INCREASING MAXBOUND FOR THIS LIST." // &
       "  NUMBER OF RECORDS: ',I0,' MAXBOUND: ',I0)"
! ------------------------------------------------------------------------------
    !
    ! -- determine array sizes
    mxlist = size(this%rlist, 2)
    ldim = size(this%rlist, 1)
    naux = size(this%auxvar, 1)
    this%ntxtrlist = 0
    this%ntxtauxvar = 0
    !
    ! -- Allocate arrays
    allocate(cellid(this%ndim))
    !
    ii = 1
    readloop: do
      !
      ! -- First line was already read, so don't read again
      if(ii /= 1) call u9rdcom(this%inlist, 0, this%line, this%ierr)
      !
      ! -- If this is an unknown-length list, then check for END.
      !    If found, then backspace, set nlist, and exit readloop.
      if(this%nlist < 0) then
        this%lloc = 1
        call urword(this%line, this%lloc, this%istart, this%istop, 1, idum, r, &
                    this%iout, this%inlist)
        if(this%line(this%istart:this%istop) == 'END' .or. this%ierr < 0) then
          ! If ierr < 0, backspace was already performed in u9rdcom, so only
          ! need to backspace if END was found.
          if (this%ierr == 0) then
            backspace(this%inlist)
          endif
          this%nlist = ii - 1
          exit readloop
        endif
      endif
      !
      ! -- Check range
      if(ii > mxlist) then
        inquire(unit=this%inlist, name=fname)
        write(errmsg, fmtmxlsterronly) ii, mxlist
        call store_error(errmsg)
        errmsg = 'Error occurred reading line: ' // trim(this%line)
        call store_error(errmsg)
        call store_error_unit(this%inlist)
      endif
      !
      ! -- Read layer, row, column or cell number and assign to nodelist
      this%lloc = 1
      if(this%ndim == 3) then
        !
        ! -- Grid is structured; read layer, row, column
        call urword(this%line, this%lloc, this%istart, this%istop, 2,          &
                    cellid(1), r, this%iout, this%inlist)
        call urword(this%line, this%lloc, this%istart, this%istop, 2,          &
                    cellid(2), r, this%iout, this%inlist)
        call urword(this%line, this%lloc, this%istart, this%istop, 2,          &
                    cellid(3), r, this%iout, this%inlist)
        !
        ! -- Check for illegal grid location
        if(cellid(1) < 1 .or. cellid(1) > this%mshape(1)) then
            write(errmsg, *) ' Layer number in list is outside of the grid',   &
                             cellid(1)
            call store_error(errmsg)
        end if
        if(cellid(2) < 1 .or. cellid(2) > this%mshape(2)) then
            write(errmsg, *) ' Row number in list is outside of the grid',     &
                             cellid(2)
            call store_error(errmsg)
        end if
        if(cellid(3) < 1 .or. cellid(3) > this%mshape(3)) then
            write(errmsg, *) ' Column number in list is outside of the grid',  &
                             cellid(3)
            call store_error(errmsg)
        end if
        !
        ! -- Calculate nodenumber and put in nodelist
        nod = get_node(cellid(1), cellid(2), cellid(3),                        &
                       this%mshape(1), this%mshape(2), this%mshape(3))
      elseif(this%ndim == 2) then
        !
        ! -- Grid is disv
        call urword(this%line, this%lloc, this%istart, this%istop, 2,          &
                    cellid(1), r, this%iout, this%inlist)
        call urword(this%line, this%lloc, this%istart, this%istop, 2,          &
                    cellid(2), r, this%iout, this%inlist)
        !
        ! -- Check for illegal grid location
        if(cellid(1) < 1 .or. cellid(1) > this%mshape(1)) then
            write(errmsg, *) ' Layer number in list is outside of the grid',   &
                             cellid(1)
            call store_error(errmsg)
        end if
        if(cellid(2) < 1 .or. cellid(2) > this%mshape(2)) then
            write(errmsg, *) ' Cell2d number in list is outside of the grid',  &
                             cellid(2)
            call store_error(errmsg)
        end if
        !
        ! -- Calculate nodenumber and put in nodelist
        nod = get_node(cellid(1), 1, cellid(2),                                &
                       this%mshape(1), 1, this%mshape(2))
      else
        !
        ! -- Grid is unstructured; read layer and celld2d number
        call urword(this%line, this%lloc, this%istart, this%istop, 2, nod, r,  &
                    this%iout, this%inlist)
        if(nod < 1 .or. nod > this%mshape(1)) then
            write(errmsg, *) ' Node number in list is outside of the grid', nod
            call store_error(errmsg)
        end if
        !
      endif
      !
      ! -- Assign nod to nodelist
      this%nodelist(ii) = nod
      !
      ! -- Read rlist
      do jj = 1, ldim
        call urword(this%line, this%lloc, this%istart, this%istop, 0, idum,    &
                    r, this%iout, this%inlist)
        read(this%line(this%istart:this%istop), *, iostat=istat) r
        !
        ! -- If a double precision value, then store in rlist, otherwise store
        !    the text name and location
        if (istat == 0) then
          this%rlist(jj, ii) = r
        else
          this%rlist(jj, ii) = DZERO
          this%ntxtrlist = this%ntxtrlist + 1
          if(this%ntxtrlist > size(this%txtrlist)) then
            increment = int(size(this%txtrlist) * 0.2)
            increment = max(100, increment)
            call ExpandArray(this%txtrlist, increment)
            call ExpandArray(this%idxtxtrow, increment)
            call ExpandArray(this%idxtxtcol, increment)
          endif
          this%txtrlist(this%ntxtrlist) = this%line(this%istart:this%istop)
          this%idxtxtrow(this%ntxtrlist) = ii
          this%idxtxtcol(this%ntxtrlist) = jj
        endif
        !
      enddo
      !
      ! -- Read auxvar
      do jj = 1, naux
        call urword(this%line, this%lloc, this%istart, this%istop, 0, idum,    &
                    r, this%iout, this%inlist)
        read(this%line(this%istart:this%istop), *, iostat=istat) r
        !
        ! -- If a double precision value, then store in auxvar, otherwise store
        !    the text name and location
        if (istat == 0) then
          this%auxvar(jj, ii) = r
        else
          this%auxvar(jj, ii) = DZERO
          this%ntxtauxvar = this%ntxtauxvar + 1
          if(this%ntxtauxvar > size(this%txtauxvar)) then
            increment = int(size(this%txtauxvar) * 0.2)
            increment = max(100, increment)
            call ExpandArray(this%txtauxvar, increment)
            call ExpandArray(this%idxtxtauxrow, increment)
            call ExpandArray(this%idxtxtauxcol, increment)
          endif
          this%txtauxvar(this%ntxtauxvar) = this%line(this%istart:this%istop)
          this%idxtxtauxrow(this%ntxtauxvar) = ii
          this%idxtxtauxcol(this%ntxtauxvar) = jj
        endif
        !
      enddo
      !
      ! -- Read the boundary names (only supported for ascii input)
      if (this%inamedbound > 0) then
        call urword(this%line, this%lloc, this%istart, this%istop, 1, idum, r, &
                    this%iout, this%inlist)
        this%boundname(ii) = this%line(this%istart:this%istop)
      endif
      !
      ! -- If nlist is known, then exit when nlist values have been read
      if(this%nlist > 0) then
        if(ii == this%nlist) exit readloop
      endif
      !
      ! -- increment ii row counter
      ii = ii + 1
      !
    enddo readloop
    !
    ! -- Stop if errors were detected
    if(count_errors() > 0) then
      call store_error_unit(this%inlist)
    endif
    !
    ! -- return
    return
  end subroutine read_ascii
  
  subroutine write_list(this)
! ******************************************************************************
! write_list -- Write input data to a list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH, LENBOUNDNAME,                         &
                               TABLEFT, TABCENTER
    use InputOutputModule, only: ulstlb, get_ijk
    use TableModule, only: TableType, table_cr
    ! -- dummy
    class(ListReaderType) :: this
    ! -- local
    character(len=10) :: cpos
    character(len=LINELENGTH) :: tag
    character(len=LINELENGTH), allocatable, dimension(:) :: words
    integer(I4B) :: ntabrows
    integer(I4B) :: ntabcols
    integer(I4B) :: ipos
    integer(I4B) :: ii, jj, i, j, k, nod
    integer(I4B) :: ldim
    integer(I4B) :: naux
    type(TableType), pointer :: inputtab => null()
    ! -- formats
    character(len=LINELENGTH) :: fmtlstbn
! ------------------------------------------------------------------------------
    !
    ! -- Determine sizes
    ldim = size(this%rlist, 1)
    naux = size(this%auxvar, 1)
    !
    ! -- dimension table
    ntabrows = this%nlist
    !
    ! -- start building format statement to parse this%label, which
    !    contains the column headers (except for boundname and auxnames)
    ipos = index(this%label, 'NO.')
    if (ipos /= 0) then
      write(cpos,'(i10)') ipos + 3
      fmtlstbn = '(a' // trim(adjustl(cpos))
    else
      fmtlstbn = '(a7'
    end if
    ! -- sequence number, layer, row, and column.
    if(size(this%mshape) == 3) then
      ntabcols = 4
      fmtlstbn = trim(fmtlstbn) // ',a7,a7,a7'
    !
    ! -- sequence number, layer, and cell2d.
    else if(size(this%mshape) == 2) then
      ntabcols = 3
      fmtlstbn = trim(fmtlstbn) // ',a7,a7'
    !
    ! -- sequence number and node.
    else
      ntabcols = 2
      fmtlstbn = trim(fmtlstbn) // ',a7'
    end if
    !
    ! -- Add fields for non-optional real values
    ntabcols = ntabcols + ldim
    do i = 1, ldim
      fmtlstbn = trim(fmtlstbn) // ',a16'
    end do
    !
    ! -- Add field for boundary name
    if (this%inamedbound == 1) then
      ntabcols = ntabcols + 1
      fmtlstbn = trim(fmtlstbn) // ',a16'
    end if
    !
    ! -- Add fields for auxiliary variables
    ntabcols = ntabcols + naux
    do i = 1, naux
      fmtlstbn = trim(fmtlstbn) // ',a16'
    end do
    fmtlstbn = trim(fmtlstbn) // ')'
    !
    ! -- allocate words
    allocate(words(ntabcols))
    !
    ! -- parse this%label into words
    read(this%label, fmtlstbn) (words(i), i=1, ntabcols)
    !
    ! -- initialize the input table object
    call table_cr(inputtab, ' ', ' ')
    call inputtab%table_df(ntabrows, ntabcols, this%iout)
    !
    ! -- add the columns
    ipos = 1
    call inputtab%initialize_column(words(ipos), 10, alignment=TABCENTER)
    !
    ! -- discretization
    do i = 1, size(this%mshape)
      ipos = ipos + 1
      call inputtab%initialize_column(words(ipos), 7, alignment=TABCENTER)
    end do
    !
    ! -- non-optional variables
    do i = 1, ldim
      ipos = ipos + 1
      call inputtab%initialize_column(words(ipos), 16, alignment=TABCENTER)
    end do
    !
    ! -- boundname
    if (this%inamedbound == 1) then
      ipos = ipos + 1
      tag = 'BOUNDNAME'
      call inputtab%initialize_column(tag, LENBOUNDNAME, alignment=TABLEFT)
    end if
    !
    ! -- aux variables
    do i = 1, naux
      call inputtab%initialize_column(this%auxname(i), 16, alignment=TABCENTER)
    end do
    !
    ! -- Write the table
    do ii = 1, this%nlist
      call inputtab%add_term(ii)
      !
      ! -- discretization
      if (size(this%mshape) == 3) then
        nod = this%nodelist(ii)
        call get_ijk(nod, this%mshape(2), this%mshape(3), this%mshape(1),        &
                     i, j, k)
        call inputtab%add_term(k)
        call inputtab%add_term(i)
        call inputtab%add_term(j)
      else if (size(this%mshape) == 2) then
        nod = this%nodelist(ii)
        call get_ijk(nod, 1, this%mshape(2), this%mshape(1), i, j, k)
        call inputtab%add_term(k)
        call inputtab%add_term(j)
      else
        nod = this%nodelist(ii)
        call inputtab%add_term(nod)
      end if
      !
      ! -- non-optional variables
      do jj = 1, ldim
        call inputtab%add_term(this%rlist(jj,ii))
      end do
      !
      ! -- boundname
      if (this%inamedbound == 1) then
        call inputtab%add_term(this%boundname(ii))
      end if
      !
      ! -- aux variables
      do jj = 1, naux
        call inputtab%add_term(this%auxvar(jj,ii))
      end do
    end do    
    !
    ! -- deallocate the local variables
    call inputtab%table_da()
    deallocate(inputtab)
    nullify(inputtab)
    deallocate(words)
    !
    ! -- return
    return
  end subroutine write_list
  
end module ListReaderModule

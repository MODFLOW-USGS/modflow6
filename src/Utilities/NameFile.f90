module NameFileModule

  use KindModule, only: DP, I4B
  use InputOutputModule,   only: ParseLine, openfile, getunit
  use ConstantsModule,     only: LINELENGTH, LENPACKAGENAME
  use ArrayHandlersModule, only: ExpandArray, remove_character
  use IunitModule,         only: IunitType
  use BlockParserModule,   only: BlockParserType
  implicit none
  private
  public :: NameFileType

  type :: NameFileType
    character(len=LINELENGTH) :: filename
    logical :: opened_listfile = .false.
    character(len=LINELENGTH), dimension(:), allocatable :: opts
    character(len=LINELENGTH), dimension(:), allocatable :: input_files
    character(len=LINELENGTH), dimension(:), allocatable :: output_files
    type(IunitType) :: iunit_obj
    type(BlockParserType) :: parser
  contains
    procedure :: init                  => namefile_init
    procedure :: add_cunit             => namefile_add_cunit
    procedure :: openlistfile          => namefile_openlistfile
    procedure :: openfiles             => namefile_openfiles
    procedure :: get_unitnumber        => namefile_get_unitnumber
    procedure :: get_nval_for_row      => namefile_get_nval_for_row
    procedure :: get_unitnumber_rowcol => namefile_get_unitnumber_rowcol
    procedure :: get_pakname           => namefile_get_pakname
    procedure :: get_output_filename
    procedure :: get_input_filename
  end type NameFileType

  contains

  subroutine namefile_init(this, filename, iout)
! ******************************************************************************
! namefile_init -- initialize the namefile object using the filename.  if iout
!   is non-zero, then the block information will be written to iout.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error, ustop
    ! -- dummy
    class(NameFileType) :: this
    character(len=*), intent(in) :: filename
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LINELENGTH) :: errmsg, line
    integer(I4B) :: i, ierr, inunit, n
    logical :: isFound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtfname =                                  &
      "(1x, 'NON-COMMENTED ENTRIES FOUND IN ', /,                              &
       &4X, 'BLOCK: ', a, /,                                                   &
       &4X, 'FILE: ', a)"
    character(len=*), parameter :: fmtbeg = "(/, 1x, A)"
    character(len=*), parameter :: fmtline = "(2x, a)"
    character(len=*), parameter :: fmtend = "(1x, A, /)"
! ------------------------------------------------------------------------------
    !
    ! -- Store filename and initialize variables
    this%filename = filename
    allocate(this%opts(0))
    allocate(this%input_files(0))
    allocate(this%output_files(0))
    !
    ! -- Open the name file and initialize the block parser
    inunit = getunit()
    call openfile(inunit, iout, filename, 'NAM', filstat_opt='OLD')
    call this%parser%Initialize(inunit, iout)
    !
    ! -- Read and set the options
    call this%parser%GetBlock('OPTIONS', isFound, ierr, blockRequired=.false.)
    if(isFound) then
      ! Populate this%opts
      n = 0
      getopts: do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit getopts
        call this%parser%GetCurrentLine(line)
        call ExpandArray(this%opts)
        n = n + 1
        this%opts(n) = adjustl(line)
      enddo getopts
      !
      if(iout > 0) then
        write(iout, fmtfname) 'OPTIONS', trim(adjustl(filename))
        write(iout, fmtbeg) 'BEGIN OPTIONS'
        do i=1,n
          write(iout, fmtline) trim(adjustl(this%opts(i)))
        enddo
        write(iout, fmtend) 'END OPTIONS'
      endif
    else
      if(iout > 0) then
        write(iout, '(/, A, /)') 'NO VALID OPTIONS BLOCK DETECTED'
      endif
    endif
    !
    ! -- Read and set the input_files
    call this%parser%GetBlock('PACKAGES', isFound, ierr)
    if(isFound) then
      ! Populate this%input_files
      n = 0
      getpaks: do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit getpaks
        call this%parser%GetCurrentLine(line)
        call ExpandArray(this%input_files)
        n = n + 1
        this%input_files(n) = adjustl(line)
      enddo getpaks
      !
      if(iout > 0) then
        write(iout, fmtfname) 'PACKAGES', trim(adjustl(filename))
        write(iout, fmtbeg) 'BEGIN PACKAGES'
        do i=1,n
          write(iout, fmtline) trim(adjustl(this%input_files(i)))
        enddo
        write(iout, fmtend) 'END PACKAGES'
      endif
    else
      write(errmsg, '(a, a)') 'Error reading PACKAGES from file: ',         &
                               trim(adjustl(filename))
      call store_error(errmsg)
      call ustop()
    endif
    !
    ! -- return
    return
  end subroutine namefile_init

  subroutine namefile_add_cunit(this, niunit, cunit)
! ******************************************************************************
! namefile_add_cunit -- attach the cunit array to the iunit object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NameFileType) :: this
    integer(I4B), intent(in) :: niunit
    character(len=*), dimension(niunit), intent(in) :: cunit
! ------------------------------------------------------------------------------
    !
    call this%iunit_obj%allocate(niunit, cunit)
    !
    ! -- return
    return
  end subroutine namefile_add_cunit

  subroutine get_input_filename(this, ftype, fname, found)
! ******************************************************************************
! get_input_filename -- get the filename from namefilearray for the specified
!   ftype.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use InputOutputModule, only: upcase
    ! -- dummy
    class(NameFileType) :: this
    character(len=*), intent(in) :: ftype
    character(len=*), intent(inout) :: fname
    logical, intent(inout) :: found
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: nwords
    character(len=len(ftype)) :: ftype_copy
    character(len=LINELENGTH), allocatable, dimension(:) :: words
! ------------------------------------------------------------------------------
    !
    fname = ' '
    found = .false.
    ftype_copy = ftype
    call upcase(ftype_copy)
    findloop: do i = 1, size(this%input_files)
      call ParseLine(this%input_files(i), nwords, words)
      call upcase(words(1))
      if(words(1) == ftype_copy) then
        fname = words(2)
        found = .true.
        exit findloop
      endif
    enddo findloop
    !
    ! -- return
    return
  end subroutine get_input_filename

  subroutine get_output_filename(this, ftype, fname, found)
! ******************************************************************************
! get_output_filename -- get the filename from namefilearray for the specified
!   ftype.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use InputOutputModule, only: upcase
    ! -- dummy
    class(NameFileType) :: this
    character(len=*), intent(in) :: ftype
    character(len=*), intent(inout) :: fname
    logical, intent(inout) :: found
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: nwords
    character(len=len(ftype)) :: ftype_copy
    character(len=LINELENGTH), allocatable, dimension(:) :: words
! ------------------------------------------------------------------------------
    !
    fname = ' '
    found = .false.
    ftype_copy = ftype
    call upcase(ftype_copy)
    findloop: do i = 1, size(this%output_files)
      call ParseLine(this%output_files(i), nwords, words)
      call upcase(words(1))
      if(words(1) == ftype_copy) then
        fname = words(2)
        found = .true.
        exit findloop
      endif
    enddo findloop
    !
    ! -- return
    return
  end subroutine get_output_filename

  subroutine namefile_openlistfile(this, iout)
! ******************************************************************************
! namefile_openlistfile -- Open the list file and set iout.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error, ustop
    use InputOutputModule, only: getunit, upcase
    ! -- dummy
    class(NameFileType) :: this
    integer(I4B), intent(inout) :: iout
    ! -- local
    logical :: found
    character(len=LINELENGTH) :: fname
    integer(I4B) :: i, istart, istop
    integer(I4B) :: nwords
    integer(I4B) :: ipos
    character(len=LINELENGTH), allocatable, dimension(:) :: words
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Go through the options and see if LIST was specified
    found = .false.
    ipos = 0
    findloop: do i = 1, size(this%opts)
      call ParseLine(this%opts(i), nwords, words)
      call upcase(words(1))
      if(words(1) == 'LIST') then
        fname = words(2)
        ipos = i
        found = .true.
        exit findloop
      endif
    enddo findloop
    !
    ! -- remove list file from options list
    if (ipos > 0) then
      call remove_character(this%opts, ipos)
    end if
    !
    ! -- If LIST was not found, then set name of list file by replacing the
    !    namefile extension with '.lst'  If no extension then add to end
    !    of namefile name.
    if (.not. found) then
      fname = ' '
      istart = 0
      istop = len_trim(this%filename)
      do i = istop, 1, -1
        if (this%filename(i:i) == '.') then
          istart = i
          exit
        endif
      enddo
      if (istart == 0) istart = istop + 1
      fname = this%filename(1:istart)
      istop = istart + 3
      fname(istart:istop) = '.lst'
    endif
    !
    ! -- Open the list file
    iout = getunit()
    call openfile(iout, 0, trim(fname), 'LIST', filstat_opt='REPLACE')
    this%opened_listfile = .true.
    !
    ! -- return
    return
  end subroutine namefile_openlistfile

  subroutine namefile_openfiles(this, iout)
! ******************************************************************************
! namefile_openfiles -- Open the files.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error, ustop
    use InputOutputModule, only: getunit
    ! -- dummy
    class(NameFileType) :: this
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=20) :: ftype, accarg, fmtarg, filstat
    integer(I4B) :: i, inunit, nwords
    character(len=LINELENGTH), allocatable, dimension(:) :: words
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Open the input_files
    do i = 1, size(this%input_files)
      !
      ! -- Parse the line and set defaults
      call ParseLine(this%input_files(i), nwords, words)
      ftype = words(1)
      accarg = 'SEQUENTIAL'
      fmtarg = 'FORMATTED'
      filstat = 'OLD'
      !
      ! -- Get a free unit number
      inunit = getunit()
      !
      ! -- Skip LIST if already opened
      select case(ftype)
        !case('DATA(BINARY)')
        !  fmtarg = form
        !  accarg = access
        !case('DATA')
        !  continue
        case default
          call this%iunit_obj%addfile(ftype, inunit, i, this%filename)
      end select
      !
      ! -- Open the file
      call openfile(inunit, iout, trim(adjustl(words(2))),                     &
                    ftype, fmtarg, accarg, filstat)
    enddo
    !
    ! -- return
    return
  end subroutine namefile_openfiles

  subroutine namefile_get_unitnumber(this, ftype, inunit, iremove)
! ******************************************************************************
! namefile_get_unitnumber -- Assign the unit number for the ftype to inunit.
!   If iremove > 0, then remove this file from iunit_obj.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(NameFileType) :: this
    character(len=*), intent(in) :: ftype
    integer(I4B), intent(inout) :: inunit
    integer(I4B), intent(in) :: iremove
! ------------------------------------------------------------------------------
    !
    call this%iunit_obj%getunitnumber(ftype, inunit, iremove)
    !
    ! -- return
    return
  end subroutine namefile_get_unitnumber

  function namefile_get_nval_for_row(this, irow) result(nval)
! ******************************************************************************
! namefile_get_nval_for_row -- Get the number of entries for the cunit type in
!   row irow.  For example, return the number of well packages that were
!   read from the name file.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    integer(I4B) :: nval
    class(NameFileType) :: this
    integer(I4B), intent(in) :: irow
! ------------------------------------------------------------------------------
    !
    nval = this%iunit_obj%iunit(irow)%nval
    !
    ! -- return
    return
  end function namefile_get_nval_for_row

  function namefile_get_unitnumber_rowcol(this, irow, jcol)            &
          result(iu)
! ******************************************************************************
! namefile_get_unitnumber_rowcol -- Get the unit number for entries in
!   cunit(irow) and columns (icol).  For example, return the unit number for
!   the first, second, or third well package.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    integer(I4B) :: iu
    class(NameFileType) :: this
    integer(I4B), intent(in) :: irow
    integer(I4B), intent(in) :: jcol
! ------------------------------------------------------------------------------
    !
    iu = this%iunit_obj%iunit(irow)%iunit(jcol)
    !
    ! -- return
    return
  end function namefile_get_unitnumber_rowcol

  subroutine namefile_get_pakname(this, irow, jcol, pakname)
! ******************************************************************************
! namefile_get_pakname -- Assign the unit number for the ftype to inunit.
!   If iremove > 0, then remove this file from iunit_obj.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error, ustop
    use InputOutputModule, only: upcase
    ! -- dummy
    class(NameFileType) :: this
    integer(I4B), intent(in) :: irow
    integer(I4B), intent(in) :: jcol
    character(len=*), intent(inout) :: pakname
    ! -- local
    integer(I4B) :: ilen, ipos, nwords
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH), allocatable, dimension(:) :: words
! ------------------------------------------------------------------------------
    !
    ipos = this%iunit_obj%iunit(irow)%ipos(jcol)
    call ParseLine(this%input_files(ipos), nwords, words, &
                   filename=this%filename)
    pakname = ''
    if (nwords > 2) then
      ilen = len(trim(adjustl(words(3))))
      if(ilen > LENPACKAGENAME) then
        write(errmsg, "(a, i0, a)")                                      &
                        'ERROR.  PACKAGENAME MUST NOT BE GREATER THAN ', &
                        LENPACKAGENAME, ' CHARACTERS.'
        call store_error(errmsg)
        call store_error(trim(this%input_files(ipos)))
        write(errmsg, '(a, a)') 'Error in PACKAGES block in file: ',  &
                               trim(adjustl(this%filename))
        call store_error(errmsg)
        call ustop()
      endif
      pakname = trim(adjustl(words(3)))
      call upcase(pakname)
    endif
    !
    ! -- return
    return
  end subroutine namefile_get_pakname


end module NameFileModule

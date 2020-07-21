module BlockParserModule
  
  use KindModule,         only: DP, I4B
  use ConstantsModule,    only: LENHUGELINE, LINELENGTH, MAXCHARLEN
  use VersionModule,      only: IDEVELOPMODE
  use InputOutputModule,  only: uget_block, uget_any_block, uterminate_block, &
                                u9rdcom, urword, upcase
  use SimModule,          only: store_error, store_error_unit, ustop
  use SimVariablesModule, only: errmsg
  
  implicit none
  
  private
  public :: BlockParserType
  
  type :: BlockParserType
    integer(I4B), public  :: iuactive  ! not used internally, so can be public
    integer(I4B), private :: inunit
    integer(I4B), private :: iuext
    integer(I4B), private :: iout
    integer(I4B), private :: linesRead
    integer(I4B), private :: lloc
    character(len=LINELENGTH), private :: blockName
    character(len=LINELENGTH), private :: blockNameFound
    character(len=LENHUGELINE), private :: laststring
    character(len=:), allocatable, private :: line
  contains
    procedure, public :: Initialize
    procedure, public :: Clear
    procedure, public :: GetBlock
    procedure, public :: GetCellid
    procedure, public :: GetCurrentLine
    procedure, public :: GetDouble
    procedure, public :: GetInteger
    procedure, public :: GetLinesRead
    procedure, public :: GetNextLine
    procedure, public :: GetRemainingLine
    procedure, public :: terminateblock
    procedure, public :: GetString
    procedure, public :: GetStringCaps
    procedure, public :: StoreErrorUnit
    procedure, public :: GetUnit
    procedure, public :: DevOpt
    procedure, private :: ReadScalarError
  end type BlockParserType
  
contains

  subroutine Initialize(this, inunit, iout)
! ******************************************************************************
! Initialize -- initialize the block parser
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- initialize values
    this%inunit = inunit
    this%iuext = inunit
    this%iuactive = inunit
    this%iout = iout
    this%blockName = ''
    this%linesRead = 0
    !
    ! -- return
    return
  end subroutine Initialize
  
  subroutine Clear(this)
! ******************************************************************************
! clear -- Close file(s) and clear member variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    ! -- local
    logical :: lop
! ------------------------------------------------------------------------------
    !
    ! Close any connected files
    if (this%inunit > 0) then
      inquire(unit=this%inunit, opened=lop)
      if (lop) then
        close(this%inunit)
      endif
    endif
    !
    if (this%iuext /= this%inunit .and. this%iuext > 0) then
      inquire(unit=this%iuext, opened=lop)
      if (lop) then
        close(this%iuext)
      endif
    endif
    !
    ! Clear all member variables
    this%inunit = 0
    this%iuext = 0
    this%iuactive = 0
    this%iout = 0
    this%lloc = 0
    this%linesRead = 0
    this%blockName = ''
    this%line = ''
    !
    ! -- return
    return
  end subroutine Clear
  
  subroutine GetBlock(this, blockName, isFound, ierr, supportOpenClose, &
                      blockRequired, blockNameFound)
! ******************************************************************************
! GetBlock -- read until blockname is found
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    character(len=*), intent(in) :: blockName
    logical, intent(out) :: isFound
    integer(I4B), intent(out) :: ierr
    logical, intent(in), optional :: supportOpenClose ! default false
    logical, intent(in), optional :: blockRequired    ! default true
    character(len=*), intent(inout), optional :: blockNameFound
    ! -- local
    logical :: continueRead, supportOpenCloseLocal, blockRequiredLocal
! ------------------------------------------------------------------------------
    !
    if (present(supportOpenClose)) then
      supportOpenCloseLocal = supportOpenClose
    else
      supportOpenCloseLocal = .false.
    endif
    !
    if (present(blockRequired)) then
      blockRequiredLocal = blockRequired
    else
      blockRequiredLocal = .true.
    endif
    continueRead = blockRequiredLocal
    this%blockName = blockName
    this%blockNameFound = ''
    !
    if (blockName == '*') then
      call uget_any_block(this%inunit, this%iout, isFound, this%lloc, &
                          this%line, blockNameFound, this%iuext)
      if (isFound) then
        this%blockNameFound = blockNameFound
        ierr = 0
      else
        ierr = 1
      endif
    else
      call uget_block(this%inunit, this%iout, this%blockName, ierr, isFound, &
                      this%lloc, this%line, this%iuext, continueRead, &
                      supportOpenCloseLocal)
      if (isFound) this%blockNameFound = this%blockName
    endif
    this%iuactive = this%iuext
    this%linesRead = 0
    !
    ! -- return
    return
  end subroutine GetBlock

  subroutine GetNextLine(this, endOfBlock)
! ******************************************************************************
! GetNextLine -- read the next line
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    logical, intent(out) :: endOfBlock
    ! -- local
    integer(I4B) :: ierr, ival
    integer(I4B) :: istart, istop
    real(DP) :: rval
    character(len=10) :: key
    logical :: lineread
! ------------------------------------------------------------------------------
    !
    endOfBlock = .false.
    ierr = 0
    lineread = .false.
    loop1: do
      if (lineread) exit loop1
      call u9rdcom(this%iuext, this%iout, this%line, ierr)
      this%lloc = 1
      call urword(this%line, this%lloc, istart, istop, 0, ival, rval, &
                  this%iout, this%iuext)
      key = this%line(istart:istop)
      call upcase(key)
      if (key == 'END' .or. key == 'BEGIN') then
        call uterminate_block(this%inunit, this%iout, key, &
                              this%blockNameFound, this%lloc, this%line, &
                              ierr, this%iuext)
        this%iuactive = this%iuext
        endOfBlock = .true.
        lineread = .true.
      elseif (key == '') then
        ! End of file reached. 
        ! If this is an OPEN/CLOSE file, close the file and read the next 
        ! line from this%inunit.
        if (this%iuext /= this%inunit) then
          close(this%iuext)
          this%iuext = this%inunit
          this%iuactive = this%inunit
        else
          errmsg = 'Unexpected end of file reached.'
          call store_error(errmsg)
          call this%StoreErrorUnit()
          call ustop()
        endif
      else
        this%lloc = 1
        this%linesRead = this%linesRead + 1
        lineread = .true.
      endif
    enddo loop1
    !
    ! -- return
    return
  end subroutine GetNextLine

  function GetInteger(this) result(i)
! ******************************************************************************
! GetInteger -- return an integer
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    integer(I4B) :: i
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    ! -- local
    integer(I4B) :: istart, istop
    real(DP) :: rval
! ------------------------------------------------------------------------------
    !
    call urword(this%line, this%lloc, istart, istop, 2, i, rval, &
                this%iout, this%iuext)
    !
    ! -- Make sure variable was read before end of line
    if (istart == istop .and. istop == len(this%line)) then
      call this%ReadScalarError('INTEGER')
    endif
    !
    ! -- return
    return
  end function GetInteger
  
  function GetLinesRead(this) result(nlines)
! ******************************************************************************
! GetLinesRead -- return number of lines that have been read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    integer(I4B) :: nlines
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    nlines =  this%linesRead
    !
    return
  end function GetLinesRead
  
  function GetDouble(this) result(r)
! ******************************************************************************
! GetDouble -- return a double precision floating point number
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: R
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    ! -- local
    integer(I4B) :: istart, istop
    integer(I4B) :: ival
! ------------------------------------------------------------------------------
    !
    call urword(this%line, this%lloc, istart, istop, 3, ival, r, &
                this%iout, this%iuext)
    !
    ! -- Make sure variable was read before end of line
    if (istart == istop .and. istop == len(this%line)) then
      call this%ReadScalarError('DOUBLE PRECISION')
    endif
    !
    ! -- return
    return
  end function GetDouble
  
  subroutine ReadScalarError(this, vartype)
! ******************************************************************************
! ReadScalarError -- issue unable to read error
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    character(len=*), intent(in) :: vartype
    ! -- local
    character(len=MAXCHARLEN-100) :: linetemp
! ------------------------------------------------------------------------------
    !
    ! -- use linetemp as line may be longer than MAXCHARLEN
    linetemp = this%line
    !
    ! -- write the message
    write(errmsg, '(3a)') 'Error in block ', trim(this%blockName), '.'
    write(errmsg, '(4a)')                                                        &
      trim(errmsg), ' Could not read variable of type ', trim(vartype),          &
                    " from the following line: '"
    write(errmsg, '(3a)')                                                        &
      trim(errmsg), trim(adjustl(this%line)), "'."
    call store_error(errmsg)
    call this%StoreErrorUnit()
    call ustop()
    !
    ! -- return
    return
  end subroutine ReadScalarError
  
  subroutine GetString(this, string, convertToUpper)
! ******************************************************************************
! GetString -- return a string and optionally convert to upper case
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    character(len=*), intent(out) :: string
    logical, optional, intent(in) :: convertToUpper ! default false
    ! -- local
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: ival
    integer(I4B) :: ncode
    real(DP) :: rval
! ------------------------------------------------------------------------------
    !
    if (present(convertToUpper)) then
      if (convertToUpper) then
        ncode = 1
      else
        ncode = 0
      endif
    else
      ncode = 0
    endif
    !
    call urword(this%line, this%lloc, istart, istop, ncode, &
                ival, rval, this%iout, this%iuext)
    string = this%line(istart:istop)
    this%laststring = this%line(istart:istop)
    !
    ! -- return
    return
  end subroutine GetString

  subroutine GetStringCaps(this, string)
! ******************************************************************************
! GetStringCaps -- convert string to caps
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    character(len=*),       intent(out)   :: string
! ------------------------------------------------------------------------------
    !
    call this%GetString(string, convertToUpper=.true.)
    !
    return
  end subroutine GetStringCaps

  subroutine GetRemainingLine(this, line)
! ******************************************************************************
! GetRemainingLine -- get the rest of the line
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    character(len=:), allocatable, intent(out) :: line
    ! -- local
    integer(I4B) :: lastpos
    integer(I4B) :: newlinelen
! ------------------------------------------------------------------------------
    !
    lastpos = len_trim(this%line)
    newlinelen = lastpos - this%lloc + 2
    newlinelen = max(newlinelen, 1)
    allocate(character(len=newlinelen) :: line)
    line(:) = this%line(this%lloc:lastpos) 
    line(newlinelen:newlinelen) = ' '
    !
    ! -- return
    return
  end subroutine GetRemainingLine
  
  subroutine terminateblock(this)
! ******************************************************************************
! terminateblock -- ensure block is closed with end
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    ! -- local
    logical :: endofblock
! ------------------------------------------------------------------------------
    !
    call this%GetNextLine(endofblock)
    if (.not. endofblock) then
      errmsg = "LOOKING FOR 'END " // trim(this%blockname) //                    &
               "'.  FOUND: " // "'" // trim(this%line) // "'."
      call store_error(errmsg)
      call this%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- return
    return
  end subroutine terminateblock

  subroutine GetCellid(this, ndim, cellid, flag_string)
! ******************************************************************************
! GetCellid -- get a cellid
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    integer(I4B),           intent(in)    :: ndim
    character(len=*),       intent(out)   :: cellid
    logical,         optional, intent(in) :: flag_string
    ! -- local
    integer(I4B) :: i, j, lloc, istart, istop, ival, istat
    real(DP) :: rval
    character(len=10) :: cint
    character(len=100) :: firsttoken
! ------------------------------------------------------------------------------
    !
    if (present(flag_string)) then
      lloc = this%lloc
      call urword(this%line, lloc, istart, istop, 0, ival, rval, this%iout, &
                  this%iuext)
      firsttoken = this%line(istart:istop)
      read(firsttoken,*,iostat=istat) ival
      if (istat > 0) then
        call upcase(firsttoken)
        cellid = firsttoken
        return
      endif
    endif
    !
    cellid = ''
    do i=1,ndim
      j = this%GetInteger()
      write(cint,'(i0)') j
      if (i == 1) then
        cellid = cint
      else
        cellid = trim(cellid) // ' ' // cint
      endif
    enddo
    !
    ! -- return
    return
  end subroutine GetCellid

  subroutine GetCurrentLine(this, line)
! ******************************************************************************
! GetCurrentLine -- get the current line
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    character(len=*),       intent(out)   :: line
! ------------------------------------------------------------------------------
    !
    line = this%line
    !
    ! -- return
    return
  end subroutine GetCurrentLine

  subroutine StoreErrorUnit(this)
! ******************************************************************************
! StoreErrorUnit -- store the unit number for which the read error occurred
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    call store_error_unit(this%iuext)
    !
    ! -- return
    return
  end subroutine StoreErrorUnit

  function GetUnit(this) result(i)
! ******************************************************************************
! GetUnit -- get unit number for the block parser
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    integer(I4B) :: i
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    i = this%iuext
    !
    return
  end function GetUnit

  subroutine DevOpt(this)
! ******************************************************************************
! DevOpt -- development option.  This subroutine will cause the program to
!   terminate with an error if the IDEVELOPMODE flag is set to 1.  This
!   is used to allow develop options to be on for development testing but
!   not for the public release.  For the public release, IDEVELOPMODE is set
!   to zero.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BlockParserType), intent(inout) :: this
    ! -- local
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- If release mode (not develop mode), then option not available.
    !    Terminate with an error.
    if (IDEVELOPMODE == 0) then
      errmsg = "Invalid keyword '" // trim(this%laststring) // &
               "' detected in block '" // trim(this%blockname) // "'."
      call store_error(errmsg)
      call this%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine DevOpt

end module BlockParserModule

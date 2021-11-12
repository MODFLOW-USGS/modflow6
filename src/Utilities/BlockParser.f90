!> @brief This module contains block parser methods
!!
!! This module contains the generic block parser type and methods that are
!! used to parse MODFLOW 6 block data.
!!
!<
module BlockParserModule
  
  use KindModule,         only: DP, I4B
  use ConstantsModule,    only: LENHUGELINE, LINELENGTH, MAXCHARLEN
  use VersionModule,      only: IDEVELOPMODE
  use InputOutputModule,  only: uget_block, uget_any_block, uterminate_block, &
                                u9rdcom, urword, upcase
  use SimModule,          only: store_error, store_error_unit
  use SimVariablesModule, only: errmsg
  
  implicit none
  
  private
  public :: BlockParserType
  
  type :: BlockParserType
    integer(I4B), public  :: iuactive                     !< flag indicating if a file unit is active, variable is not used internally
    integer(I4B), private :: inunit                       !< file unit number
    integer(I4B), private :: iuext                        !< external file unit number
    integer(I4B), private :: iout                         !< listing file unit number
    integer(I4B), private :: linesRead                    !< number of lines read
    integer(I4B), private :: lloc                         !< line location counter
    character(len=LINELENGTH), private :: blockName       !< block name
    character(len=LINELENGTH), private :: blockNameFound  !< block name found
    character(len=LENHUGELINE), private :: laststring     !< last string read
    character(len=:), allocatable, private :: line        !< current line
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

  !> @ brief Initialize the block parser
  !!
  !! Method to initialize the block parser.
  !!
  !<
  subroutine Initialize(this, inunit, iout)
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this  !< BlockParserType object
    integer(I4B), intent(in) :: inunit             !< input file unit number
    integer(I4B), intent(in) :: iout               !< listing file unit number
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
  
  !> @ brief Close the block parser
  !!
  !! Method to clear the block parser, which closes file(s) and clears member
  !! variables.
  !!
  !<
  subroutine Clear(this)
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this  !< BlockParserType object
    ! -- local variables
    logical :: lop
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
    deallocate(this%line)
    !
    ! -- return
    return
  end subroutine Clear
  
  !> @ brief Get block
  !!
  !! Method to get the block from a file. The file is read until the blockname
  !! is found.
  !!
  !<
  subroutine GetBlock(this, blockName, isFound, ierr, supportOpenClose, &
                      blockRequired, blockNameFound)
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this                !< BlockParserType object
    character(len=*), intent(in) :: blockName                    !< block name to search for
    logical, intent(out) :: isFound                              !< boolean indicating if the block name was found
    integer(I4B), intent(out) :: ierr                            !< return error code, 0 indicates block was found
    logical, intent(in), optional :: supportOpenClose            !< boolean indicating if the block supports open/close, default false
    logical, intent(in), optional :: blockRequired               !< boolean indicating if the block is required, default true
    character(len=*), intent(inout), optional :: blockNameFound  !< optional return value of block name found
    ! -- local variables
    logical :: continueRead
    logical :: supportOpenCloseLocal
    logical :: blockRequiredLocal
    !
    ! -- process optional variables
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

  !> @ brief Get the next line
  !!
  !! Method to get the next line from a file.
  !!
  !<
  subroutine GetNextLine(this, endOfBlock)
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this  !< BlockParserType object
    logical, intent(out) :: endOfBlock             !< boolean indicating if the end of the block was read
    ! -- local variables
    integer(I4B) :: ierr
    integer(I4B) :: ival
    integer(I4B) :: istart
    integer(I4B) :: istop
    real(DP) :: rval
    character(len=10) :: key
    logical :: lineread
    !
    ! -- initialize local variables
    endOfBlock = .false.
    ierr = 0
    lineread = .false.
    !
    ! -- read next line
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

  !> @ brief Get a integer
  !!
  !! Function to get a integer from the current line.
  !!
  !<
  function GetInteger(this) result(i)
    ! -- return variable
    integer(I4B) :: i                               !< integer variable
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this   !< BlockParserType object
    ! -- local variables
    integer(I4B) :: istart
    integer(I4B) :: istop
    real(DP) :: rval
    !
    ! -- get integer using urword
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
  
  !> @ brief Get the number of lines read
  !!
  !! Function to get the number of lines read from the current block.
  !!
  !<
  function GetLinesRead(this) result(nlines)
    ! -- return variable
    integer(I4B) :: nlines                           !< number of lines read
    ! -- dummy variable
    class(BlockParserType), intent(inout) :: this    !< BlockParserType object
    !
    ! -- number of lines read
    nlines =  this%linesRead
    !
    ! -- return
    return
  end function GetLinesRead
  
  !> @ brief Get a double precision real
  !!
  !! Function to get adouble precision floating point number from 
  !! the current line.
  !!
  !<
  function GetDouble(this) result(r)
    ! -- return variable
    real(DP) :: r                                  !< double precision real variable
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this  !< BlockParserType object
    ! -- local variables
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: ival
    !
    ! -- get double precision real using urword
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
  
  !> @ brief Issue a read error
  !!
  !! Method to issue an unable to read error.
  !!
  !<
  subroutine ReadScalarError(this, vartype)
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this    !< BlockParserType object
    character(len=*), intent(in) :: vartype          !< string of variable type
    ! -- local variables
    character(len=MAXCHARLEN-100) :: linetemp
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
    !
    ! -- return
    return
  end subroutine ReadScalarError
  
  !> @ brief Get a string
  !!
  !! Method to get a string from the current line and optionally convert it
  !! to upper case.
  !!
  !<
  subroutine GetString(this, string, convertToUpper)
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this   !< BlockParserType object
    character(len=*), intent(out) :: string         !< string
    logical, optional, intent(in) :: convertToUpper !< boolean indicating if the string should be converted to upper case, default false
    ! -- local variables
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: ival
    integer(I4B) :: ncode
    real(DP) :: rval
    !
    ! -- process optional variables
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

  !> @ brief Get an upper case string
  !!
  !! Method to get a string from the current line and convert it
  !! to upper case.
  !!
  !<
  subroutine GetStringCaps(this, string)
    ! -- dummy  variables
    class(BlockParserType), intent(inout) :: this      !< BlockParserType object
    character(len=*),       intent(out)   :: string    !< upper case string
    !
    ! -- call base GetString method with convertToUpper variable
    call this%GetString(string, convertToUpper=.true.)
    !
    ! -- return
    return
  end subroutine GetStringCaps

  !> @ brief Get the rest of a line
  !!
  !! Method to get the rest of the line from the current line.
  !!
  !<
  subroutine GetRemainingLine(this, line)
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this        !< BlockParserType object
    character(len=:), allocatable, intent(out) :: line   !< remainder of the line
    ! -- local variables
    integer(I4B) :: lastpos
    integer(I4B) :: newlinelen
    !
    ! -- get the rest of the line
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
  
  !> @ brief Ensure that the block is closed
  !!
  !! Method to ensure that the block is closed with an "end".
  !!
  !<
  subroutine terminateblock(this)
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this  !< BlockParserType object
    ! -- local variables
    logical :: endofblock
    !
    ! -- look for block termination
    call this%GetNextLine(endofblock)
    if (.not. endofblock) then
      errmsg = "LOOKING FOR 'END " // trim(this%blockname) //                    &
               "'.  FOUND: " // "'" // trim(this%line) // "'."
      call store_error(errmsg)
      call this%StoreErrorUnit()
    endif
    !
    ! -- return
    return
  end subroutine terminateblock

  !> @ brief Get a cellid
  !!
  !! Method to get a cellid from a line.
  !!
  !<
  subroutine GetCellid(this, ndim, cellid, flag_string)
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this    !< BlockParserType object
    integer(I4B), intent(in) :: ndim                 !< number of dimensions (1, 2, or 3)
    character(len=*), intent(out) :: cellid          !< cell =id
    logical, optional, intent(in) :: flag_string     !< boolean indicating id cellid is a string
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: lloc
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: ival
    integer(I4B) :: istat
    real(DP) :: rval
    character(len=10) :: cint
    character(len=100) :: firsttoken
    !
    ! -- process optional variables
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

  !> @ brief Get the current line
  !!
  !! Method to get the current line.
  !!
  !<
  subroutine GetCurrentLine(this, line)
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this  !< BlockParserType object
    character(len=*), intent(out)   :: line        !< current line
    !
    ! -- get the current line
    line = this%line
    !
    ! -- return
    return
  end subroutine GetCurrentLine

  !> @ brief Store the unit number
  !!
  !! Method to store the unit number for the file that caused a read error.
  !! Default is to terminate the simulation when this method is called.
  !!
  !<
  subroutine StoreErrorUnit(this, terminate)
    ! -- dummy variable
    class(BlockParserType), intent(inout) :: this   !< BlockParserType object
    logical, intent(in), optional :: terminate      !< boolean indicating if the simulation should be terminated
    ! -- loacl variables
    logical :: lterminate
    !
    ! -- process optional variables
    if (present(terminate)) then
      lterminate = terminate
    else
      lterminate = .TRUE.
    end if
  !
    ! -- store error unit
    call store_error_unit(this%iuext, terminate=lterminate)
    !
    ! -- return
    return
  end subroutine StoreErrorUnit

  !> @ brief Get the unit number
  !!
  !! Function to get the unit number for the block parser.
  !!
  !<
  function GetUnit(this) result(i)
    ! -- return variable
    integer(I4B) :: i                              !< unit number for the block parser
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this  !< BlockParserType object
    !
    ! -- block parser unit number
    i = this%iuext
    !
    ! -- return
    return
  end function GetUnit

  !> @ brief Development option
  !!
  !! Method that will cause the program to terminate with an error if the 
  !! IDEVELOPMODE flag is set to 1.  This is used to allow develop options
  !! to be specified for development testing but not for the public release.
  !! For the public release, IDEVELOPMODE is set to zero.
  !!
  !<
  subroutine DevOpt(this)
    ! -- dummy variables
    class(BlockParserType), intent(inout) :: this  !< BlockParserType object
    !
    ! -- If release mode (not develop mode), then option not available.
    !    Terminate with an error.
    if (IDEVELOPMODE == 0) then
      errmsg = "Invalid keyword '" // trim(this%laststring) // &
               "' detected in block '" // trim(this%blockname) // "'."
      call store_error(errmsg)
      call this%StoreErrorUnit()
    endif
    !
    ! -- Return
    return
  end subroutine DevOpt

end module BlockParserModule

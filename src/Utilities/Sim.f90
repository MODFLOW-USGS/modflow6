module SimModule
  
  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: MAXCHARLEN, LINELENGTH,                      &
                                    DONE,                                        &
                                    IUSTART, IULAST,                             &
                                    VSUMMARY, VALL, VDEBUG
  use SimVariablesModule,     only: istdout, iout, isim_level, ireturnerr,       &
                                    iforcestop, iunext
  use GenericUtilitiesModule, only: sim_message, stop_with_error

  implicit none

  private
  public :: count_errors
  public :: store_error
  public :: ustop
  public :: converge_reset
  public :: converge_check
  public :: final_message
  public :: store_warning
  public :: store_note
  public :: count_warnings
  public :: count_notes
  public :: store_error_unit
  public :: store_error_filename
  public :: print_notes
  public :: maxerrors
  
  character(len=MAXCHARLEN), allocatable, dimension(:) :: sim_errors
  character(len=MAXCHARLEN), allocatable, dimension(:) :: sim_warnings
  character(len=MAXCHARLEN), allocatable, dimension(:) :: sim_notes
  integer(I4B), allocatable, dimension(:) :: icount_errors
  integer(I4B), allocatable, dimension(:) :: icount_warnings
  integer(I4B), allocatable, dimension(:) :: icount_notes
  integer(I4B) :: lenerrors = 0
  integer(I4B) :: nerrors = 0
  integer(I4B) :: maxerrors = 1000
  integer(I4B) :: maxerrors_exceeded = 0
  integer(I4B) :: lenwarnings = 0
  integer(I4B) :: nwarnings = 0
  integer(I4B) :: maxwarnings = 1000
  integer(I4B) :: maxwarnings_exceeded = 0
  integer(I4B) :: maxnotes = 1000
  integer(I4B) :: maxnotes_exceeded = 0
  integer(I4B) :: lennotes = 0
  integer(I4B) :: nnotes = 0
  integer(I4B) :: inc_errors = 100
  integer(I4B) :: inc_warnings = 100
  integer(I4B) :: inc_notes = 100

contains

function count_errors()
! ******************************************************************************
! Return error count
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- return
  integer(I4B) :: count_errors
! ------------------------------------------------------------------------------
  if (allocated(sim_errors)) then
    count_errors = nerrors
  else
    count_errors = 0
  endif
  return
end function count_errors

function count_warnings()
! ******************************************************************************
! Return warning count
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- return
  integer(I4B) :: count_warnings
! ------------------------------------------------------------------------------
  if (allocated(sim_warnings)) then
    count_warnings = nwarnings
  else
    count_warnings = 0
  endif
  return
end function count_warnings

function count_notes()
! ******************************************************************************
! Return notes count
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- return
  integer(I4B) :: count_notes
! ------------------------------------------------------------------------------
  if (allocated(sim_notes)) then
    count_notes = nnotes
  else
    count_notes = 0
  endif
  return
end function count_notes

subroutine store_error(errmsg, count)
! ******************************************************************************
! Store an error message for printing at end of simulation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  use ArrayHandlersModule, only: ExpandArray
  ! -- dummy
  character(len=*), intent(in) :: errmsg
  logical, intent(in), optional :: count
  ! -- local
  logical :: inc_count
  logical :: inc_array
  integer(I4B) :: i
  integer(I4B) :: j
! ------------------------------------------------------------------------------
  !
  ! -- process optional variables
  if (present(count)) then
    inc_count = count
  else
    inc_count = .true.
  end if
  !
  ! -- determine if the sim_errors should be expanded
  inc_array = .TRUE.
  if (allocated(sim_errors)) then
    if (count_errors() < size(sim_errors)) then
      inc_array = .FALSE.
    end if
  end if
  !
  ! -- resize sim_errors
  if (inc_array) then
    call ExpandArray(sim_errors, increment=inc_errors)
    call ExpandArray(icount_errors, increment=inc_errors)
    inc_errors = inc_errors * 1.1
  end if
  !
  ! -- store this error
  i = lenerrors + 1
  if (i <= maxerrors) then
    lenerrors = i
    if (inc_count) then
      j = nerrors + 1
      nerrors = j
    else
      j = 0
    end if
    sim_errors(i) = errmsg
    icount_errors(i) = j
  else
    maxerrors_exceeded = maxerrors_exceeded + 1
  end if
  !
  ! -- return
  return
end subroutine store_error

subroutine store_error_unit(iunit)
! ******************************************************************************
! Convert iunit to file name and indicate error reading from this file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- dummy
  integer(I4B), intent(in) :: iunit
  ! -- local
  character(len=LINELENGTH) :: fname
  character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
  !
  inquire(unit=iunit, name=fname)
  write(errmsg,'(3a)')                                                           &
    "ERROR OCCURRED WHILE READING FILE '", trim(adjustl(fname)), "'"
  call store_error(errmsg, count=.false.)
  !
  return
end subroutine store_error_unit

subroutine store_error_filename(filename)
! ******************************************************************************
! Indicate error reading from this file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- dummy
  character(len=*), intent(in) :: filename
  ! -- local
  character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
  !
  write(errmsg,'(3a)')                                                           &
    "ERROR OCCURRED WHILE READING FILE '", trim(adjustl(filename)), "'"
  call store_error(errmsg, count=.false.)
  !
  return
end subroutine store_error_filename

subroutine store_warning(warnmsg, count)
! ******************************************************************************
! Store a warning message for printing at end of simulation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  use ArrayHandlersModule, only: ExpandArray
  ! -- dummy
  character(len=*), intent(in) :: warnmsg
  logical, intent(in), optional :: count
  ! -- local
  logical :: inc_count
  logical :: inc_array
  integer(I4B) :: i
  integer(I4B) :: j
! ------------------------------------------------------------------------------
  !
  ! -- process optional variables
  if (present(count)) then
    inc_count = count
  else
    inc_count = .true.
  end if
  !
  ! -- determine if the sim_warnings should be expanded
  inc_array = .TRUE.
  if (allocated(sim_warnings)) then
    if (count_warnings() < size(sim_warnings)) then
      inc_array = .FALSE.
    end if
  end if
  !
  ! -- resize sim_warnings
  if (inc_array) then
    call ExpandArray(sim_warnings, increment=inc_warnings)
    call ExpandArray(icount_warnings, increment=inc_warnings)
    inc_warnings = inc_warnings * 1.1
  end if
  !
  ! -- store this warning
  i = lenwarnings + 1
  if (i <= maxwarnings) then
    lenwarnings = i
    if (inc_count) then
      j = nwarnings + 1
      nwarnings = j
    else
      j = 0
    end if
    sim_warnings(i) = warnmsg
    icount_warnings(i) = j
  else
    maxwarnings_exceeded = maxwarnings_exceeded + 1
  end if
  !
  ! -- return
  return
end subroutine store_warning

subroutine store_note(note, count)
! ******************************************************************************
! Store a note for printing at end of simulation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  use ArrayHandlersModule, only: ExpandArray
  ! -- dummy
  character(len=*), intent(in) :: note
  logical, intent(in), optional :: count
  ! -- local
  logical :: inc_count
  logical :: inc_array
  integer(I4B) :: i
  integer(I4B) :: j
! ------------------------------------------------------------------------------
  !
  ! -- process optional variables
  if (present(count)) then
    inc_count = count
  else
    inc_count = .true.
  end if
  !
  ! -- determine if the sim_notes should be expanded
  inc_array = .TRUE.
  if (allocated(sim_notes)) then
    if (count_notes() < size(sim_notes)) then
      inc_array = .FALSE.
    end if
  end if
  !
  ! -- resize sim_notes
  if (inc_array) then
    call ExpandArray(sim_notes, increment=inc_notes)
    inc_notes = inc_notes * 1.1
  end if
  !
  ! -- store this note
  i = lennotes + 1
  if (i <= maxnotes) then
    lennotes = i
    if (inc_count) then
      j = nnotes + 1
      nnotes = j
    else
      j = 0
    end if
    sim_notes(i) = note
    icount_notes(i) = j
  else
    maxnotes_exceeded = maxnotes_exceeded + 1
  end if
  !
  ! -- return
  return
end subroutine store_note

logical function print_errors()
! ******************************************************************************
! Print all error messages that have been stored
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- local
  character(len=LINELENGTH) :: errmsg
  integer(I4B) :: i
  integer(I4B) :: isize
  integer(I4B) :: ifmt
  integer(I4B) :: icnt
  real(DP) :: rval
  ! -- formats
  character(len=*), parameter :: stdfmt = "(/,'ERROR REPORT:',/)"
! ------------------------------------------------------------------------------
  !
  print_errors = .false.
  if (allocated(sim_errors)) then
    isize = count_errors()
    if (isize > 0) then
      print_errors = .true.
      rval = real(isize, DP)
      ifmt = modulo(log10(rval), DONE) + 1
      if (iout > 0) then
        call sim_message('', iunit=iout, fmt=stdfmt)
      end if
      call sim_message('', fmt=stdfmt)
      !do i = 1, isize
      do i = 1, lenerrors
        icnt = icount_errors(i)
        call write_message(sim_errors(i), icnt=icnt, ifmt=ifmt)
        if (iout > 0) then
          call write_message(sim_errors(i), iunit=iout, icnt=icnt, ifmt=ifmt)
        end if
      end do
      !
      ! -- write the number of additional errors
      if (maxerrors_exceeded > 0) then
        write(errmsg, '(i0,1x,a)')                                               &
          maxerrors_exceeded, 'additional errors detected but not printed.'
        call write_message(trim(errmsg))
        if (iout > 0) then
          call write_message(trim(errmsg), iout)
        end if
      end if
    end if
  endif
  !
  ! -- return
  return
end function print_errors

logical function print_warnings()
! ******************************************************************************
! Print all warning messages that have been stored
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- local
  character(len=LINELENGTH) :: msg
  integer(I4B) :: i
  integer(I4B) :: isize
  integer(I4B) :: ifmt
  integer(I4B) :: icnt
  real(DP) :: rval
  ! -- formats
  character(len=*), parameter :: stdfmt = "(/,'WARNINGS:',/)"
! ------------------------------------------------------------------------------
  !
  ! -- initialize
  print_warnings = .false.
  !
  ! -- print warnings
  if (allocated(sim_warnings)) then
    isize = count_warnings()
    if (isize > 0) then
      print_warnings = .true.
      rval = real(isize, DP)
      ifmt = modulo(log10(rval), DONE) + 1
      if (iout > 0) then
        call sim_message('', fmt=stdfmt, iunit=iout)
      end if
      call sim_message('', fmt=stdfmt)
      !do i = 1, isize
      do i = 1, lenwarnings
        icnt = icount_warnings(i)
        call write_message(sim_warnings(i), icnt=icnt, ifmt=ifmt)
        if (iout > 0) then
          call write_message(sim_warnings(i), iunit=iout, icnt=icnt, ifmt=ifmt)
        end if
      end do
    end if
    !
    ! -- write number of additional warnings
    if (maxwarnings_exceeded > 0) then
      write(msg, '(i0,1x,a)')                                                    &
        maxwarnings_exceeded, 'additional warnings detected but not printed.'
      call write_message(trim(msg))
      if (iout > 0) then
        call write_message(trim(msg), iout)
      end if
    end if
  end if
  !
  ! -- return
  return
end function print_warnings

subroutine print_notes(numberlist)
! ******************************************************************************
! Print all notes that have been stored
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- dummy
  logical, intent(in), optional :: numberlist
  ! -- local
  character(len=LINELENGTH) :: msg
  character(len=MAXCHARLEN+10) :: noteplus
  integer(I4B) :: i
  integer(I4B) :: isize
  logical :: numlist
  ! -- formats
   character(len=*), parameter :: fmtnotes = "(/,'NOTES:')"
   character(len=*), parameter :: fmta = "(i0,'. ',a)" 
   character(len=*), parameter :: fmtb = '(a)' 
! ------------------------------------------------------------------------------
  !
  ! -- process optional variables
  if (present(numberlist)) then
    numlist = numberlist
  else
    numlist = .true.
  endif
  !
  if (allocated(sim_notes)) then
    isize = count_notes()
    if (isize>0) then
      if (iout>0) then
        call sim_message('', fmt=fmtnotes, iunit=iout)
      end if
      call sim_message('', fmt=fmtnotes)
      do i=1, isize
        if (numlist) then
          write(noteplus,fmta) i, trim(sim_notes(i))
        else
          write(noteplus,fmtb) trim(sim_notes(i))
        endif
        call write_message(noteplus)
        if (iout > 0) then
          call write_message(noteplus, iout)
        end if
      enddo
    endif
    !
    ! -- write number of additional notes
    if (maxnotes_exceeded > 0) then
      write(msg, '(i0,1x,a)')                                                    &
        maxnotes_exceeded, 'additional notes detected but not printed.'
      call write_message(trim(msg))
      if (iout > 0) then
        call write_message(trim(msg), iout)
      end if
    end if
  endif
  !
  ! -- return
  return
end subroutine print_notes

subroutine write_message(message, iunit, error, skipbefore, skipafter,           &
                         icnt, ifmt)
! ******************************************************************************
! Subroutine write_message formats and writes a message.
!
! -- Arguments are as follows:
!       MESSAGE      : message to be written
!       IUNIT        : the unit number to which the message is written
!       ERROR        : if true precede message with "Error"
!       SKIPBEFORE   : number of empty lines before message
!       SKIPAFTER    : number of empty lines after message
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- dummy
  character (len=*), intent(in)           :: message
  integer(I4B),      intent(in), optional :: iunit
  logical,           intent(in), optional :: error
  integer(I4B),      intent(in), optional :: skipbefore
  integer(I4B),      intent(in), optional :: skipafter
  integer(I4B),      intent(in), optional :: icnt
  integer(I4B),      intent(in), optional :: ifmt
  ! -- local
  character(len=MAXCHARLEN) :: amessage
  character(len=20)         :: ablank
  character(len=16)         :: cfmt
  character(len=10)         :: cval
  integer(I4B)              :: jend
  integer(I4B)              :: nblc
  integer(I4B)              :: junit
  integer(I4B)              :: leadblank
  integer(I4B)              :: itake
  integer(I4B)              :: ipos
  integer(I4B)              :: i
  integer(I4B)              :: j
! ------------------------------------------------------------------------------
  !
  amessage = message
  if (amessage == ' ') return
  !
  ! -- ensure that there is at least one blank space at the start of amessage
  if (amessage(1:1) /= ' ') then
    amessage = ' ' // trim(amessage)
  end if
  !
  ! -- initialize local variables
  junit = istdout
  ablank = ' '
  itake = 0
  j = 0
  !
  ! -- process optional dummy variables
  !    set the unit number
  if(present(iunit))then
    if (iunit > 0) then
      junit = iunit
    end if
  end if
  !
  ! -- add blank lines before writing amessage
  if(present(skipbefore))then
    do i = 1, skipbefore
      call sim_message('', iunit=junit)
    end do
  end if
  !
  ! -- prepend amessage with 'Error:' string, if necessary
  if(present(error))then
    if (error) then
      !
      ! -- evaluate if amessage already includes 'ERROR:' or 'Error:' string
      ipos = index(amessage, 'ERROR:')
      if (ipos < 1) then
        ipos = index(amessage, 'Error:')
      end if
      !
      ! -- prepend amessage with 'Error:' string
      if (ipos < 1) then
        nblc = len_trim(amessage)
        amessage = adjustr(amessage(1:nblc+8))
        if (nblc+8 < len(amessage)) then
          amessage(nblc+9:) = ' '
        end if
        amessage(1:8) = ' Error: '
      end if
    end if
  end if
  !
  ! -- prepend amessage with counter
  if (present(icnt) .and. present(ifmt)) then
    write(cfmt, '(A,I0,A)') '(1X,I', ifmt, ',".")'
    !
    ! -- update amessage, if required
    if (icnt > 0) then
      write(cval, cfmt) icnt
      ipos = len_trim(cval)
    else
      write(cfmt, '(A,I0,A)') '(1X,A', ifmt, ',1X)'
      write(cval, cfmt) '*'
      call sim_message('', iunit=junit)
      ipos = ifmt + 2
    end if
    nblc = len_trim(amessage)
    amessage = adjustr(amessage(1:nblc+ipos))
    if (nblc+ipos < len(amessage)) then
      amessage(nblc+ipos+1:) = ' '
    end if
    amessage(1:ipos) = cval(1:ipos)
    leadblank = ipos - 1
  !
  ! -- determine the default number of leading blanks
  else
    do i = 1, 20
      if (amessage(i:i).ne.' ') exit
    end do
    leadblank = i - 1
  end if
  !!
  !! -- determine the number of leading blanks
  !do i = 1, 20
  !  if (amessage(i:i).ne.' ') exit
  !end do
  !leadblank = i - 1
  !
  ! -- calculate the final length of the message after modification
  nblc = len_trim(amessage)
  !
  ! -- parse the amessage into multiple lines
5 continue
  jend = j + 78 - itake
  if (jend >= nblc) go to 100
  do i = jend, j+1, -1
    if (amessage(i:i).eq.' ') then
      if (itake.eq.0) then
        call sim_message(amessage(j+1:i), iunit=junit)
        itake = 2 + leadblank
      else
        call sim_message(ablank(1:leadblank+2)//amessage(j+1:i), iunit=junit)
      end if
      j = i
      go to 5
    end if
  end do
  if (itake == 0)then
    call sim_message(amessage(j+1:jend), iunit=junit)
    itake = 2 + leadblank
  else
    call sim_message(ablank(1:leadblank+2)//amessage(j+1:jend), iunit=junit)
  end if
  j = jend
  go to 5
  !
  ! -- last piece of amessage to write to a line
100 continue
  jend = nblc
  if (itake == 0)then
    call sim_message(amessage(j+1:jend), iunit=junit)
  else
    call sim_message(ablank(1:leadblank+2)//amessage(j+1:jend), iunit=junit)
  end if
  !
  ! -- add blank lines at the end of amessage
  if(present(skipafter))then
    do i = 1, skipafter
      call sim_message('', iunit=junit)
    end do
  endif
  !
  ! -- return
  return
end subroutine write_message

! -- this subroutine prints final messages and then stops with the active
!    error code.
subroutine ustop(stopmess, ioutlocal)
! ******************************************************************************
! Stop program, with option to print message before stopping.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- dummy
  character, optional, intent(in) :: stopmess*(*)
  integer(I4B), optional, intent(in) :: ioutlocal
  
  !---------------------------------------------------------------------------
  !
  ! -- print the final message
  call print_final_message(stopmess, ioutlocal)
  !
  ! -- return appropriate error codes when terminating the program
  call stop_with_error(ireturnerr)
  
end subroutine ustop

subroutine print_final_message(stopmess, ioutlocal)
! ******************************************************************************
! Print a final message and close all open files
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- dummy
  character, optional, intent(in) :: stopmess*(*)
  integer(I4B),   optional, intent(in) :: ioutlocal  
  ! -- local
  character(len=*), parameter :: fmt = '(1x,a)'
  character(len=*), parameter :: msg = 'Stopping due to error(s)'
  logical :: errorfound  
  logical :: warningfound
  !---------------------------------------------------------------------------
  call print_notes()
  warningfound =  print_warnings()
  errorfound = print_errors()
  if (present(stopmess)) then
    if (stopmess.ne.' ') then
      call sim_message(stopmess, fmt=fmt, iunit=iout)
      call sim_message(stopmess, fmt=fmt)
      if (present(ioutlocal)) then
        if (ioutlocal > 0 .and. ioutlocal /= iout) then
          write(ioutlocal,fmt) trim(stopmess)
          close (ioutlocal)
        endif
      endif
    endif
  endif
  !
  ! -- determine if error condition occurred
  if (errorfound) then
    ireturnerr = 2
    if (iout > 0) then
      call sim_message(stopmess, fmt=fmt, iunit=iout)
    end if
    call sim_message(stopmess, fmt=fmt)
    
    if (present(ioutlocal)) then
      if (ioutlocal > 0 .and. ioutlocal /= iout) write(ioutlocal,fmt) msg
    endif
  endif
  !
  ! -- close all open files
  call sim_closefiles()
  !
  ! -- return
  return
  
end subroutine print_final_message

subroutine converge_reset()
! ******************************************************************************
! converge_reset
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: isimcnvg
! ------------------------------------------------------------------------------
    !
    isimcnvg = 1
    !
    ! -- Return
    return
  end subroutine converge_reset

  subroutine converge_check(hasConverged)
! ******************************************************************************
! convergence check
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: isimcnvg, numnoconverge, isimcontinue
    ! -- dummy
    logical, intent(inout) :: hasConverged
    ! -- format
    character(len=*), parameter :: fmtfail =                                   &
      "(1x, 'Simulation convergence failure.',                                 &
       &' Simulation will terminate after output and deallocation.')"
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    hasConverged = .true.
    !
    ! -- Count number of failures
    if(isimcnvg == 0) then
      numnoconverge = numnoconverge + 1
    end if
    !
    ! -- Continue if 'CONTINUE' specified in simulation control file
    if(isimcontinue == 1) then
      if(isimcnvg == 0) then
        isimcnvg = 1
      endif
    endif
    !
    ! --
    if(isimcnvg == 0) then
      !write(iout, fmtfail)
      call sim_message('', fmt=fmtfail, iunit=iout)
      hasConverged = .false.
    endif
    !
    ! -- Return
    return
  end subroutine converge_check

  subroutine final_message()
! ******************************************************************************
! Create the appropriate final message and terminate the program
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    use SimVariablesModule, only: isimcnvg, numnoconverge, ireturnerr
    ! -- local
    character(len=LINELENGTH) :: line
    ! -- formats
    character(len=*), parameter :: fmtnocnvg =                                 &
      "(1x, 'Simulation convergence failure occurred ', i0, ' time(s).')"
! ------------------------------------------------------------------------------
    !
    ! -- Write message if any nonconvergence
    if(numnoconverge > 0) then
      write(line, fmtnocnvg) numnoconverge
      call sim_message(line, iunit=iout)
      call sim_message(line)
    endif
    !
    if(isimcnvg == 0) then
      ireturnerr = 1
      call print_final_message('Premature termination of simulation.', iout)
    else
      call print_final_message('Normal termination of simulation.', iout)
    endif
    !
    ! -- Return or halt
    if (iforcestop == 1) then
      call stop_with_error(ireturnerr)
    end if
    
  end subroutine final_message
  
  subroutine sim_closefiles()
! ******************************************************************************
! Close all opened files.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    implicit none
    ! -- dummy
    ! -- local
    integer(I4B) :: i
    logical :: opened
! ------------------------------------------------------------------------------
    !
    ! -- close all open file units
    do i = iustart, iunext - 1
      !
      ! -- determine if file unit i is open
      inquire(unit=i, opened=opened)
      !
      ! -- skip file units that are no longer open
      if(.not. opened) then
        cycle
      end if
      !
      ! -- close file unit i
      close(i)
    end do
    !
    ! -- return
    return
  end subroutine sim_closefiles
  
end module SimModule

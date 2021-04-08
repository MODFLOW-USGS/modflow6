module SimModule
  
  use KindModule,             only: DP, I4B
  use DefinedMacros,          only: get_os
  use ConstantsModule,        only: MAXCHARLEN, LINELENGTH,                      &
                                    DONE,                                        &
                                    IUSTART, IULAST,                             &
                                    VSUMMARY, VALL, VDEBUG,                      &
                                    OSWIN, OSUNDEF
  use SimVariablesModule,     only: istdout, iout, isim_level, ireturnerr,       &
                                    iforcestop, iunext
  use GenericUtilitiesModule, only: sim_message, stop_with_error
  use MessageModule,          only: MessageType

  implicit none

  private
  public :: count_errors
  public :: store_error
  public :: ustop
  public :: converge_reset
  public :: converge_check
  public :: final_message
  public :: store_warning
  public :: deprecation_warning
  public :: store_note
  public :: count_warnings
  public :: count_notes
  public :: store_error_unit
  public :: store_error_filename
  public :: MaxErrors
  
  type(MessageType) :: sim_errors
  type(MessageType) :: sim_uniterrors
  type(MessageType) :: sim_warnings
  type(MessageType) :: sim_notes

contains

function count_errors() result(ncount)
! ******************************************************************************
! Return error count
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- return
  integer(I4B) :: ncount
! ------------------------------------------------------------------------------
  ncount = sim_errors%count_message()
  return
end function count_errors

function count_warnings() result(ncount)
! ******************************************************************************
! Return warning count
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- return
  integer(I4B) :: ncount
! ------------------------------------------------------------------------------
  ncount = sim_warnings%count_message()
  return
end function count_warnings

function count_notes() result(ncount)
! ******************************************************************************
! Return notes count
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- return
  integer(I4B) :: ncount
! ------------------------------------------------------------------------------
  ncount = sim_notes%count_message()
  return
end function count_notes

subroutine MaxErrors(imax)
! ******************************************************************************
! Set the maximum number of errors saved
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- dummy
  integer(I4B), intent(in) :: imax
  ! -- local
! ------------------------------------------------------------------------------
  !
  ! -- set the maximum number of error messages that will be saved
  call sim_errors%set_max_message(imax)
  !
  ! -- return
  return
end subroutine MaxErrors

subroutine store_error(errmsg)
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
  ! -- local
! ------------------------------------------------------------------------------
  !
  ! -- store error
  call sim_errors%store_message(errmsg)
  !
  ! -- return
  return
end subroutine store_error

subroutine get_filename(iunit, fname)
! ******************************************************************************
! Get filename from unit number. If the INQUIRE function returns the full
! path (for example, the INTEL compiler) then the returned file name (fname)  
! is limited to the filename without the path.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- dummy
  integer(I4B), intent(in) :: iunit
  character(len=*), intent(inout) :: fname
  ! -- local
  integer(I4B) :: ipos
  integer(I4B) :: ios
  integer(I4B) :: ilen
! ------------------------------------------------------------------------------
  !
  ! -- get file name from unit number
  inquire(unit=iunit, name=fname)
  !
  ! -- determine the operating system
  ios = get_os()
  !
  ! -- extract filename from full path, if present
  !    forward slash on linux, unix, and osx
  if (ios /= OSWIN) then
    ipos = index(fname, '/', back=.TRUE.)
  end if
  !
  ! -- check for backslash on windows or undefined os and 
  !    forward slashes were not found
  if (ios == OSWIN .or. ios == OSUNDEF) then
    if (ipos < 1) then
      ipos = index(fname, '\', back=.TRUE.)
    end if
  end if
  !
  ! -- exclude the path from the file name
  if (ipos > 0) then
    ilen = len_trim(fname)
    write(fname, '(a)') fname(ipos+1:ilen) // ' '
  end if
  !
  ! -- return
  return
end subroutine get_filename

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
  ! -- store error unit
  inquire(unit=iunit, name=fname)
  write(errmsg,'(3a)')                                                           &
    "ERROR OCCURRED WHILE READING FILE '", trim(adjustl(fname)), "'"
  call sim_uniterrors%store_message(errmsg)
  !
  ! -- return
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
  ! -- store error unit
  write(errmsg,'(3a)')                                                           &
    "ERROR OCCURRED WHILE READING FILE '", trim(adjustl(filename)), "'"
  call sim_uniterrors%store_message(errmsg)
  !
  ! -- return
  return
end subroutine store_error_filename

subroutine store_warning(warnmsg, substring)
! ******************************************************************************
! Store a warning message for printing at end of simulation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  ! -- dummy
  character(len=*), intent(in) :: warnmsg
  character(len=*), intent(in), optional :: substring
! ------------------------------------------------------------------------------
  !
  ! -- store warning
  if (present(substring)) then
    call sim_warnings%store_message(warnmsg, substring)
  else
    call sim_warnings%store_message(warnmsg)
  end if
  !
  ! -- return
  return
end subroutine store_warning

subroutine deprecation_warning(cblock, cvar, cver, endmsg, iunit)
! ******************************************************************************
! Store a warning message for deprecated variables and printing at the 
! end of simulation
!
! -- Arguments are as follows:
!       CBLOCK       : block name
!       CVAR         : variable name
!       CVER         : version when variable was deprecated  
!       ENDMSG       : optional user defined message to append at the end of 
!                      the deprecation warning
!       IUNIT        : optional input file unit number with the deprecated 
!                      variable
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  use ArrayHandlersModule, only: ExpandArray
  ! -- dummy
  character(len=*), intent(in) :: cblock
  character(len=*), intent(in) :: cvar
  character(len=*), intent(in) :: cver
  character(len=*), intent(in), optional :: endmsg
  integer(I4B), intent(in), optional :: iunit
  ! -- local
  character(len=MAXCHARLEN) :: message
  character(len=LINELENGTH) :: fname
! ------------------------------------------------------------------------------
  !
  ! -- build message
  write(message,'(a)')                                                           &
    trim(cblock) // " BLOCK VARIABLE '" // trim(cvar) // "'"
  if (present(iunit)) then
    call get_filename(iunit, fname)
    write(message,'(a,1x,3a)')                                                   &
      trim(message), "IN FILE '", trim(fname), "'"
  end if 
  write(message,'(a)')                                                           &
    trim(message) // ' WAS DEPRECATED IN VERSION ' // trim(cver) // '.'
  if (present(endmsg)) then
    write(message,'(a,1x,2a)') trim(message), trim(endmsg), '.'
  end if
  !
  ! -- store warning
  call sim_warnings%store_message(message)
  !
  ! -- return
  return
end subroutine deprecation_warning

subroutine store_note(note)
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
  ! -- local
! ------------------------------------------------------------------------------
  !
  ! -- store note
  call sim_notes%store_message(note)
  !
  ! -- return
  return
end subroutine store_note

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
  !---------------------------------------------------------------------------
  !
  ! -- print the accumulated messages
  call sim_notes%print_message('NOTES:', 'note(s)',                              &
                               iunit=iout, level=VALL)
  call sim_warnings%print_message('WARNING REPORT:', 'warning(s)',               &
                                  iunit=iout, level=VALL)
  call sim_errors%print_message('ERROR REPORT:', 'error(s)', iunit=iout)
  call sim_uniterrors%print_message('UNIT ERROR REPORT:',                        &
                                    'file unit error(s)', iunit=iout)
  !
  ! -- write a stop message, if one is passed 
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
  ! -- determine if an error condition has occurred
  if (sim_errors%count_message() > 0) then
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
    ! -- destroy messages
    call sim_errors%deallocate_message()
    call sim_uniterrors%deallocate_message()
    call sim_warnings%deallocate_message()
    call sim_notes%deallocate_message()
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

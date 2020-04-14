module SimModule
  
  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: MAXCHARLEN, LINELENGTH,                      &
                                    DONE,                                        &
                                    IUSTART, IULAST,                             &
                                    VSUMMARY, VALL, VDEBUG
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
  call sim_errors%count_message(count_errors)
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
  call sim_warnings%count_message(count_warnings)
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
  call sim_notes%count_message(count_notes)
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
  call sim_errors%store_message(errmsg)
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
  !call sim_uniterrors%store_message(errmsg, count=.false.)
  call sim_uniterrors%store_message(errmsg)
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
  !call sim_uniterrors%store_message(errmsg, count=.false.)
  call sim_uniterrors%store_message(errmsg)
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
! ------------------------------------------------------------------------------
  !
  ! -- process optional variables
  if (present(count)) then
    inc_count = count
  else
    inc_count = .true.
  end if
  call sim_warnings%store_message(warnmsg, count=inc_count)
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
! ------------------------------------------------------------------------------
  !
  ! -- process optional variables
  if (present(count)) then
    inc_count = count
  else
    inc_count = .true.
  end if
  call sim_notes%store_message(note, count=inc_count)
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
  !logical :: errorfound  
  !logical :: warningfound
  integer :: ierr
  !---------------------------------------------------------------------------
  !call print_notes()
  !warningfound =  print_warnings()
  !errorfound = print_errors()
  call sim_notes%print_message('NOTES:', 'notes', iunit=iout)
  call sim_warnings%print_message('WARNING REPORT:', 'warnings', iunit=iout)
  call sim_errors%print_message('ERROR REPORT:', 'errors', iunit=iout)
  call sim_uniterrors%print_message('UNIT ERROR REPORT:', 'file unit errors',    &
                                    iunit=iout)
  !
  !
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
  !if (errorfound) then
  call sim_errors%count_message(ierr)
  if (ierr > 0) then
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

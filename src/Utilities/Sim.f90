module SimModule
  use KindModule,         only: DP, I4B
  use ConstantsModule,    only: MAXCHARLEN,LINELENGTH, ISTDOUT
  use SimVariablesModule, only: iout
  implicit none
  private
  public :: count_errors, iverbose, sim_message, store_error, ustop,  &
            converge_reset, converge_check, final_message, store_warning,      &
            store_note, count_warnings, count_notes, store_error_unit, &
            store_error_filename, print_notes
  integer(I4B) :: iverbose=0 !0: print nothing
                             !1: print first level subroutine information
  character(len=MAXCHARLEN), allocatable, dimension(:) :: sim_errors
  character(len=MAXCHARLEN), allocatable, dimension(:) :: sim_warnings
  character(len=MAXCHARLEN), allocatable, dimension(:) :: sim_notes
  integer(I4B) :: nerrors = 0
  integer(I4B) :: nwarnings = 0
  integer(I4B) :: nnotes = 0
  integer(I4B) :: inc_errors = 100
  integer(I4B) :: inc_warnings = 100
  integer(I4B) :: inc_notes = 100


contains

function count_errors()
  implicit none
  integer(I4B) :: count_errors
  if (allocated(sim_errors)) then
    !count_errors = size(sim_errors)
    count_errors = nerrors
  else
    count_errors = 0
  endif
  return
end function count_errors

function count_warnings()
  implicit none
  integer(I4B) :: count_warnings
  if (allocated(sim_warnings)) then
    !count_warnings = size(sim_warnings)
    count_warnings = nwarnings
  else
    count_warnings = 0
  endif
  return
end function count_warnings

function count_notes()
  implicit none
  integer(I4B) :: count_notes
  if (allocated(sim_notes)) then
    !count_notes = size(sim_notes)
    count_notes = nnotes
  else
    count_notes = 0
  endif
  return
end function count_notes

subroutine store_error(errmsg)
  ! **************************************************************************
  ! Store an error message for printing at end of simulation
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  use ArrayHandlersModule, only: ExpandArray
  implicit none
  ! -- dummy
  character(len=*), intent(in) :: errmsg
  ! -- local
  logical :: inc_array
  integer(I4B) :: i
  !
  inc_array = .TRUE.
  if (allocated(sim_errors)) then
    if (count_errors() < size(sim_errors)) then
      inc_array = .FALSE.
    end if
  end if
  if (inc_array) then
    call ExpandArray(sim_errors, increment=inc_errors)
    inc_errors = inc_errors * 1.1
  end if
  i = count_errors() + 1
  nerrors = i
  sim_errors(i) = errmsg
  !
  return
end subroutine store_error

subroutine store_error_unit(iunit)
  ! **************************************************************************
  ! Convert iunit to file name and indicate error reading from this file
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- dummy
  integer(I4B), intent(in) :: iunit
  ! -- local
  character(len=LINELENGTH) :: fname
  !
  inquire(unit=iunit, name=fname)
  call store_error('ERROR OCCURRED WHILE READING FILE: ')
  call store_error(trim(adjustl(fname)))
  !
  return
end subroutine store_error_unit

subroutine store_error_filename(filename)
  ! **************************************************************************
  ! Indicate error reading from this file
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- dummy
  character(len=*), intent(in) :: filename
  ! -- local
  !
  call store_error('ERROR OCCURRED WHILE READING FILE: ')
  call store_error(trim(adjustl(filename)))
  !
  return
end subroutine store_error_filename

subroutine store_warning(warnmsg)
  ! **************************************************************************
  ! Store a warning message for printing at end of simulation
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  use ArrayHandlersModule, only: ExpandArray
  implicit none
  ! -- dummy
  character(len=*), intent(in) :: warnmsg
  ! -- local
  logical :: inc_array
  integer(I4B) :: i
  !
  inc_array = .TRUE.
  if (allocated(sim_warnings)) then
    if (count_warnings() < size(sim_warnings)) then
      inc_array = .FALSE.
    end if
  end if
  if (inc_array) then
    call ExpandArray(sim_warnings, increment=inc_warnings)
    inc_warnings = inc_warnings * 1.1
  end if
  i = count_warnings() + 1
  nwarnings = i
  sim_warnings(i) = warnmsg
  !
  return
end subroutine store_warning

subroutine store_note(note)
  ! **************************************************************************
  ! Store a note for printing at end of simulation
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  use ArrayHandlersModule, only: ExpandArray
  implicit none
  ! -- dummy
  character(len=*), intent(in) :: note
  ! -- local
  logical :: inc_array
  integer(I4B) :: i
  !
  inc_array = .TRUE.
  if (allocated(sim_notes)) then
    if (count_notes() < size(sim_notes)) then
      inc_array = .FALSE.
    end if
  end if
  if (inc_array) then
    call ExpandArray(sim_notes, increment=inc_notes)
    inc_notes = inc_notes * 1.1
  end if
  i = count_notes() + 1
  nnotes = i
  sim_notes(i) = note
  !
  return
end subroutine store_note

logical function print_errors()
  ! **************************************************************************
  ! Print all error messages that have been stored
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- local
  integer(I4B) :: i, isize
  ! -- formats
10 format(/,'ERROR REPORT:',/)
  !
  print_errors = .false.
  if (allocated(sim_errors)) then
    isize = count_errors()
    if (isize>0) then
      print_errors = .true.
      if (iout > 0) write(iout,10)
      write(*,10)
      do i=1,isize
        call write_message(sim_errors(i))
        if (iout > 0) call write_message(sim_errors(i),iout)
      enddo
    endif
  endif
  !
  return
end function print_errors

subroutine print_warnings()
  ! **************************************************************************
  ! Print all warning messages that have been stored
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- local
  integer(I4B) :: i, isize
  ! -- formats
10 format(/,'WARNINGS:',/)
  !
  if (allocated(sim_warnings)) then
    isize = count_warnings()
    if (isize>0) then
      if (iout>0) write(iout,10)
      write(*,10)
      do i=1,isize
        call write_message(sim_warnings(i))
        if (iout>0) call write_message(sim_warnings(i),iout)
      enddo
    endif
    write(*,'()')
    if (iout>0) write(iout,'()')
  endif
  !
  return
end subroutine print_warnings

subroutine print_notes(numberlist)
  ! **************************************************************************
  ! Print all notes that have been stored
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  implicit none
  ! -- dummy
  logical, intent(in), optional :: numberlist
  ! -- local
  integer(I4B) :: i, isize
  character(len=MAXCHARLEN+10) :: noteplus
  logical :: numlist
  ! -- formats
10 format(/,'NOTES:')
20 format(i0,'. ',a)
30 format(a)
  !
  if (present(numberlist)) then
    numlist = numberlist
  else
    numlist = .true.
  endif
  !
  if (allocated(sim_notes)) then
    isize = count_notes()
    if (isize>0) then
      if (iout>0) write(iout,10)
      write(*,10)
      do i=1,isize
        if (numlist) then
          write(noteplus,20)i,trim(sim_notes(i))
        else
          write(noteplus,30)trim(sim_notes(i))
        endif
        call write_message(noteplus)
        if (iout>0) call write_message(noteplus,iout)
      enddo
    endif
  endif
  !
  return
end subroutine print_notes

subroutine write_message(message,iunit,error,leadspace,endspace)
  ! -- Subroutine write_message formats and writes a message.
  !
  ! -- Arguments are as follows:
  !       MESSAGE      : message to be written
  !       IUNIT        : the unit number to which the message is written
  !       ERROR        : if true precede message with "Error"
  !       LEADSPACE    : if true precede message with blank line
  !       ENDSPACE     : if true follow message by blank line
  !
  implicit none
  ! -- dummy
  character (len=*), intent(in) :: message
  integer(I4B),      intent(in), optional :: iunit
  logical,           intent(in), optional :: error
  logical,           intent(in), optional :: leadspace
  logical,           intent(in), optional :: endspace
  ! -- local
  integer(I4B)              :: jend, i, nblc, junit, leadblank
  integer(I4B)              :: itake, j
  character(len=20)         :: ablank
  character(len=MAXCHARLEN) :: amessage
  !
  amessage = message
  if (amessage==' ') return
  if (amessage(1:1).ne.' ') amessage=' ' // trim(amessage)
  ablank=' '
  itake=0
  j=0
  if(present(iunit))then
    junit = iunit
  else
    junit = ISTDOUT
  end if
  if(present(leadspace))then
    if(leadspace) then
      if (junit>0) then
        write(junit,*)
      else
        write(*,*)
      endif
    endif
  endif
  if(present(error))then
    if(error)then
      nblc=len_trim(amessage)
      amessage=adjustr(amessage(1:nblc+8))
      if(nblc+8.lt.len(amessage)) amessage(nblc+9:)=' '
      amessage(1:8)=' Error: '
    end if
  end if
  !
  do i=1,20
    if(amessage(i:i).ne.' ')exit
  end do
  leadblank=i-1
  nblc=len_trim(amessage)
  !
5 continue
  jend=j+78-itake
  if(jend.ge.nblc) go to 100
  do i=jend,j+1,-1
    if(amessage(i:i).eq.' ') then
      if(itake.eq.0) then
        if (junit>0) then
          write(junit,'(a)',err=200) amessage(j+1:i)
        else
          write(*,'(a)',err=200) amessage(j+1:i)
        endif
        itake=2+leadblank
      else
        if (junit>0) then
          write(junit,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:i)
        else
          write(*,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:i)
        endif
      end if
      j=i
      go to 5
    end if
  end do
  if(itake.eq.0)then
    if (junit>0) then
      write(junit,'(a)',err=200) amessage(j+1:jend)
    else
      write(*,'(a)',err=200) amessage(j+1:jend)
    endif
    itake=2+leadblank
  else
    if (junit>0) then
      write(junit,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:jend)
    else
      write(*,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:jend)
    endif
  end if
  j=jend
  go to 5
  !
100 continue
  jend=nblc
  if(itake.eq.0)then
    if (junit>0) then
      write(junit,'(a)',err=200) amessage(j+1:jend)
    else
      write(*,'(a)',err=200) amessage(j+1:jend)
    endif
  else
    if (junit>0) then
      write(junit,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:jend)
    else
      write(*,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:jend)
    endif
  end if
  !
  if(present(endspace))then
    if(endspace) then
      if (junit>0) then
        write(junit,*)
      else
        write(*,*)
      endif
    endif
  end if
  return
  !
200 continue
  call ustop()
  !
end subroutine write_message

subroutine sim_message(iv,message)
! -- iv is the verbosity level of this message
! --  (1) means primary subroutine for simulation, exchange, model,
! --      solution, package, etc.
! -- message is a character string message to write
  implicit none
  integer(I4B),intent(in) :: iv
  character(len=*),intent(in) :: message
  if(iv<=iverbose) then
    write(iout,'(a)') message
  endif
end subroutine sim_message

subroutine ustop(stopmess,ioutlocal)
  ! **************************************************************************
  ! Stop program, with option to print message before stopping.
  ! **************************************************************************
  !
  !    SPECIFICATIONS:
  ! --------------------------------------------------------------------------
  ! -- dummy
  implicit none
  character, optional, intent(in) :: stopmess*(*)
  integer(I4B),   optional, intent(in) :: ioutlocal
  ! -- local
  character(len=*), parameter :: fmt = '(1x,a)'
  character(len=*), parameter :: msg = 'Stopping due to error(s)'
  logical :: errorfound
  !---------------------------------------------------------------------------
  call print_notes()
  call print_warnings()
  errorfound = print_errors()
  if (present(stopmess)) then
    if (stopmess.ne.' ') then
      write(*,fmt) stopmess
      write(iout,fmt) stopmess
      if (present(ioutlocal)) then
        if (ioutlocal > 0 .and. ioutlocal .ne. iout) then
          write(ioutlocal,fmt) trim(stopmess)
          close(ioutlocal)
        endif
      endif
    endif
  endif
  !
  if (errorfound) then
    write(*,fmt) msg
    if (iout > 0) write(iout,fmt) msg
    if (present(ioutlocal)) then
      if (ioutlocal > 0 .and. ioutlocal /= iout) write(ioutlocal,fmt) msg
    endif
  endif
  !
  close(iout)
  stop
end subroutine ustop

  subroutine converge_reset()
! ******************************************************************************
! converge_reset
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SimVariablesModule, only: isimcnvg
! ------------------------------------------------------------------------------
    !
    isimcnvg = 1
    !
    ! -- Return
    return
  end subroutine converge_reset

  subroutine converge_check(exit_tsloop)
! ******************************************************************************
! converge_check
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: isimcnvg, numnoconverge, isimcontinue
    ! -- dummy
    logical, intent(inout) :: exit_tsloop
    ! -- format
    character(len=*), parameter :: fmtfail =                                   &
      "(1x, 'Simulation convergence failure.',                                 &
       ' Simulation will terminate after output and deallocation.')"
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    exit_tsloop = .false.
    !
    ! -- Count number of failures
    if(isimcnvg == 0) numnoconverge = numnoconverge + 1
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
      write(iout, fmtfail)
      exit_tsloop = .true.
    endif
    !
    ! -- Return
    return
  end subroutine converge_check

  subroutine final_message()
! ******************************************************************************
! final_message
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: isimcnvg, numnoconverge
    ! -- formats
    character(len=*), parameter :: fmtnocnvg =                                 &
      "(1x, 'Simulation convergence failure occurred ', i0, ' time(s).')"
! ------------------------------------------------------------------------------
    !
    ! -- Write message if any nonconvergence
    if(numnoconverge > 0) then
      write(*, fmtnocnvg) numnoconverge
      write(iout, fmtnocnvg) numnoconverge
    endif
    !
    if(isimcnvg == 0) then
      call ustop('Premature termination of simulation.', iout)
    else
      call ustop('Normal termination of simulation.', iout)
    endif
    !
    ! -- Return
    return
  end subroutine final_message

end module SimModule

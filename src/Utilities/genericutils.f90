!> @brief This module contains generic utilties
!!
!! This module contains generic utilities that have
!! limited dependencies.
!!
!<
module GenericUtilitiesModule
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: MAXCHARLEN, LENHUGELINE, &
                             DZERO, DPREC, DSAME, &
                             LINELENGTH, LENHUGELINE, VSUMMARY
  use SimVariablesModule, only: istdout, isim_level
  !
  implicit none

  private

  public :: sim_message
  public :: write_message
  public :: write_centered
  public :: is_same
  public :: stop_with_error

contains

  !> @brief Write simulation message
  !!
  !!  Subroutine to print message to user specified iunit or STDOUT based on level.
  !!
  !<
  subroutine sim_message(message, iunit, fmt, level, &
                         skipbefore, skipafter, advance)
    ! -- dummy variables
    character(len=*), intent(in) :: message !< message to write to iunit
    integer(I4B), intent(in), optional :: iunit !< optional file unit to write the message to (default=stdout)
    character(len=*), intent(in), optional :: fmt !< optional format to write the message (default='(a)')
    integer(I4B), intent(in), optional :: level !< optional level for the message (default=summary)
    integer(I4B), intent(in), optional :: skipbefore !< optional number of empty lines before message (default=0)
    integer(I4B), intent(in), optional :: skipafter !< optional number of empty lines after message (default=0)
    logical(LGP), intent(in), optional :: advance !< optional boolean indicating if advancing output (default is .TRUE.)
    ! -- local variables
    character(len=3) :: cadvance
    integer(I4B) :: i
    integer(I4B) :: ilen
    integer(I4B) :: iu
    integer(I4B) :: ilevel
    character(len=LENHUGELINE) :: simfmt
    character(len=*), parameter :: stdfmt = '(a)'
    character(len=*), parameter :: emptyfmt = '()'
    !
    ! -- initialize local variables
    ilen = len_trim(message)
    !
    ! -- process optional dummy variables
    if (present(iunit)) then
      iu = iunit
    else
      iu = istdout
    end if
    if (present(fmt)) then
      simfmt = fmt
    else
      if (ilen > 0) then
        simfmt = stdfmt
      else
        simfmt = emptyfmt
      end if
    end if
    if (present(level)) then
      ilevel = level
    else
      ilevel = VSUMMARY
    end if
    if (present(advance)) then
      if (advance) then
        cadvance = 'YES'
      else
        cadvance = 'NO'
      end if
    else
      cadvance = 'YES'
    end if
    !
    ! -- write empty line before message
    if (present(skipbefore)) then
      do i = 1, skipbefore
        write (iu, *)
      end do
    end if
    !
    ! -- write message if the level of the message is less than
    !    or equal the isim_level for the simulation
    if (ilevel <= isim_level) then
      if (ilen > 0) then
        write (iu, trim(simfmt), advance=cadvance) message(1:ilen)
      else
        write (iu, trim(simfmt), advance=cadvance)
      end if
    end if
    !
    ! -- write empty line after message
    if (present(skipafter)) then
      do i = 1, skipafter
        write (iu, *)
      end do
    end if
    !
    ! -- return
    return
  end subroutine sim_message

  !> @brief Write messages
  !!
  !!  Subroutine that formats and writes a single message that
  !!  may exceeed 78 characters in length. Messages longer than
  !!  78 characters are written across multiple lines. When a
  !!  counter is passed in subsequent lines are indented.
  !!
  !<
  subroutine write_message(message, icount, iwidth, iunit, level, &
                           skipbefore, skipafter)
    ! -- dummy variables
    character(len=*), intent(in) :: message !< message to be written
    integer(I4B), intent(in), optional :: icount !< counter to prepended to the message
    integer(I4B), intent(in), optional :: iwidth !< maximum width of the prepended counter
    integer(I4B), intent(in), optional :: iunit !< the unit number to which the message is written
    integer(I4B), intent(in), optional :: level !< level of message (VSUMMARY, VALL, VDEBUG)
    integer(I4B), intent(in), optional :: skipbefore !< optional number of empty lines before message (default=0)
    integer(I4B), intent(in), optional :: skipafter !< optional number of empty lines after message (default=0)
    ! -- local variables
    integer(I4B), parameter :: len_line = 78
    character(len=LENHUGELINE) :: amessage
    character(len=len_line) :: line
    character(len=16) :: cfmt
    character(len=10) :: counter
    character(len=5) :: fmt_first
    character(len=20) :: fmt_cont
    logical(LGP) :: include_counter
    integer(I4B) :: isb
    integer(I4B) :: isa
    integer(I4B) :: jend
    integer(I4B) :: len_str1
    integer(I4B) :: len_str2
    integer(I4B) :: len_message
    integer(I4B) :: junit
    integer(I4B) :: ilevel
    integer(I4B) :: i
    integer(I4B) :: j
    !
    ! -- return if no message is passed
    if (len_trim(message) < 1) then
      return
    end if
    !
    ! -- initialize local variables
    amessage = message
    counter = ''
    fmt_first = '(A)'
    fmt_cont = '(A)'
    len_str1 = 0
    len_str2 = len_line
    include_counter = .FALSE.
    junit = istdout
    j = 0
    !
    ! -- process optional dummy variables
    ! -- set the unit number
    if (present(iunit)) then
      if (iunit > 0) then
        junit = iunit
      end if
    end if
    !
    ! -- set the message level
    if (present(level)) then
      ilevel = level
    else
      ilevel = VSUMMARY
    end if
    !
    ! -- set skip before
    if (present(skipbefore)) then
      isb = skipbefore
    else
      isb = 0
    end if
    !
    ! -- set skip after
    if (present(skipafter)) then
      isa = skipafter
    else
      isa = 0
    end if
    !
    ! -- create the counter to prepend to the start of the message,
    !    formats, and variables used to create strings
    if (present(iwidth) .and. present(icount)) then
      include_counter = .TRUE.
      ! -- write counter
      write (cfmt, '(A,I0,A)') '(1x,i', iwidth, ',".",1x)'
      write (counter, cfmt) icount
      ! -- calculate the length of the first and second string on a line
      len_str1 = len(trim(counter)) + 1
      len_str2 = len_line - len_str1
      ! -- write format for the continuation lines
      write (fmt_cont, '(a,i0,a)') &
        '(', len(trim(counter)) + 1, 'x,a)'
    end if
    !
    ! -- calculate the length of the message
    len_message = len_trim(amessage)
    !
    ! -- parse the amessage into multiple lines
5   continue
    jend = j + len_str2
    if (jend >= len_message) go to 100
    do i = jend, j + 1, -1
      if (amessage(i:i) .eq. ' ') then
        if (j == 0) then
          if (include_counter) then
            line = counter(1:len_str1)//amessage(j + 1:i)
          else
            line = amessage(j + 1:i)
          end if
          call sim_message(line, iunit=junit, &
                           fmt=fmt_first, level=ilevel, &
                           skipbefore=isb)
        else
          line = adjustl(amessage(j + 1:i))
          call sim_message(line, iunit=junit, &
                           fmt=fmt_cont, level=ilevel)
        end if
        j = i
        go to 5
      end if
    end do
    if (j == 0) then
      if (include_counter) then
        line = counter(1:len_str1)//amessage(j + 1:jend)
      else
        line = amessage(j + 1:jend)
      end if
      call sim_message(line, iunit=junit, &
                       fmt=fmt_first, level=ilevel, &
                       skipbefore=isb)
    else
      line = amessage(j + 1:jend)
      call sim_message(line, iunit=junit, &
                       fmt=fmt_cont, level=ilevel)
    end if
    j = jend
    go to 5
    !
    ! -- last piece of amessage to write to a line
100 continue
    jend = len_message
    if (j == 0) then
      if (include_counter) then
        line = counter(1:len_str1)//amessage(j + 1:jend)
      else
        line = amessage(j + 1:jend)
      end if
      call sim_message(line, iunit=junit, &
                       fmt=fmt_first, level=ilevel, &
                       skipbefore=isb, skipafter=isa)
    else
      line = amessage(j + 1:jend)
      call sim_message(line, iunit=junit, fmt=fmt_cont, &
                       level=ilevel, &
                       skipafter=isa)
    end if
    !
    ! -- return
    return
  end subroutine write_message

  !> @brief Write centered text
  !!
  !! Subroutine to write text to unit iunit centered in width defined by linelen.
  !! Left-pad with blanks as needed.
  !!
  !<
  subroutine write_centered(text, linelen, iunit)
    ! -- dummy variables
    character(len=*), intent(in) :: text !< message to write to iunit
    integer(I4B), intent(in) :: linelen !< length of line to center text in
    integer(I4B), intent(in), optional :: iunit !< optional file unit to write text (default=stdout)
    ! -- local variables
    character(len=linelen) :: line
    character(len=linelen) :: blank
    integer(I4B) :: iu
    integer(I4B) :: len_message
    integer(I4B) :: jend
    integer(I4B) :: ipad
    integer(I4B) :: i
    integer(I4B) :: j
    !
    ! -- process optional parameters
    if (present(iunit)) then
      iu = iunit
    else
      iu = istdout
    end if
    !
    ! -- process text
    if (iu > 0) then
      !
      ! -- initialize local variables
      blank = ''
      len_message = len_trim(adjustl(text))
      j = 0
      !
      ! -- parse the amessage into multiple lines
5     continue
      jend = j + linelen
      if (jend >= len_message) go to 100
      do i = jend, j + 1, -1
        if (text(i:i) .eq. ' ') then
          line = text(j + 1:i)
          ipad = ((linelen - len_trim(line)) / 2)
          call sim_message(blank(1:ipad)//line, iunit=iu)
          j = i
          go to 5
        end if
      end do
      line = text(j + 1:jend)
      ipad = ((linelen - len_trim(line)) / 2)
      call sim_message(blank(1:ipad)//line, iunit=iu)
      j = jend
      go to 5
      !
      ! -- last piece of amessage to write to a line
100   continue
      jend = len_message
      line = text(j + 1:jend)
      ipad = ((linelen - len_trim(line)) / 2)
      call sim_message(blank(1:ipad)//line, iunit=iu)
    end if
    !
    ! -- return
    return
  end subroutine write_centered

  !> @brief Function to determine if two reals are the same
  !!
  !! Function to evaluate if the difference between a and b are less than eps
  !! (i.e. a and b are the same).
  !!
  !<
  function is_same(a, b, eps) result(lvalue)
    ! -- return variable
    logical(LGP) :: lvalue !< boolean indicating if a and b are the same
    ! -- dummy variables
    real(DP), intent(in) :: a !< first number to evaluate
    real(DP), intent(in) :: b !< second number to evaluate
    real(DP), intent(in), optional :: eps !< optional maximum difference between a abd b (default=DSAME)
    ! -- local variables
    real(DP) :: epsloc
    real(DP) :: denom
    real(DP) :: rdiff
    !
    ! -- evaluate optioanl arguments
    if (present(eps)) then
      epsloc = eps
    else
      epsloc = DSAME
    end if
    lvalue = .FALSE.
    if (a == b) then
      lvalue = .TRUE.
    else
      if (abs(b) > abs(a)) then
        denom = b
      else
        denom = a
        if (abs(denom) == DZERO) then
          denom = DPREC
        end if
      end if
      rdiff = abs((a - b) / denom)
      if (rdiff <= epsloc) then
        lvalue = .TRUE.
      end if
    end if
    !
    ! -- return
    return
  end function is_same

  !> @brief Subroutine to stop the program
  !!
  !! Subroutine to stop the program and issue the correct return code.
  !!
  !<
  subroutine stop_with_error(ierr)
    ! -- dummy variables
    integer(I4B), intent(in), optional :: ierr !< optional error code to return (default=0)
    ! -- local variables
    integer(I4B) :: ireturn_err
    !
    ! -- process optional dummy variables
    if (present(ierr)) then
      ireturn_err = ierr
    else
      ireturn_err = 0
    end if

    ! -- return the correct return code
    call exit(ireturn_err)

  end subroutine stop_with_error

end module GenericUtilitiesModule

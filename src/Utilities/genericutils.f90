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
  subroutine sim_message(message, iunit, fmt, level,                             &
                         skipbefore, skipafter, advance)
    ! -- dummy variables
    character(len=*), intent(in) :: message              !< message to write to iunit
    integer(I4B), intent(in), optional :: iunit          !< optional file unit to write the message to (default=stdout)
    character(len=*), intent(in), optional :: fmt        !< optional format to write the message (default='(a)')
    integer(I4B), intent(in), optional :: level          !< optional level for the message (default=summary)
    integer(I4B), intent(in), optional :: skipbefore     !< optional number of empty lines before message (default=0)
    integer(I4B), intent(in), optional :: skipafter      !< optional number of empty lines after message (default=0)
    logical(LGP), intent(in), optional :: advance        !< optional boolean indicating if advancing output (default is .TRUE.)
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
        write(iu, *)
      end do
    end if
    !
    ! -- write message if the level of the message is less than
    !    or equal the isim_level for the simulation
    if (ilevel <= isim_level) then
      if (ilen > 0) then
        write(iu, trim(simfmt), advance=cadvance) message(1:ilen)
      else
        write(iu, trim(simfmt), advance=cadvance)
      end if
    end if
    !
    ! -- write empty line after message
    if (present(skipafter)) then
      do i = 1, skipafter
        write(iu, *)
      end do
    end if
    !
    ! -- return
    return
  end subroutine sim_message

  !> @brief Write messages
  !!
  !!  Subroutine that formats and writes a single message.
  !!
  !<
  subroutine write_message(message, icount, iwidth, iunit, level)
    ! -- dummy variables
    character (len=*), intent(in)           :: message   !< message to be written
    integer(I4B),      intent(in)           :: icount    !< counter to prepended to the message  
    integer(I4B),      intent(in)           :: iwidth    !< maximum width of the prepended counter
    integer(I4B),      intent(in), optional :: iunit     !< the unit number to which the message is written
    integer(I4B),      intent(in), optional :: level     !< level of message (VSUMMARY, VALL, VDEBUG)
    ! -- local variables
    character(len=LENHUGELINE) :: amessage
    character(len=20)          :: ablank
    character(len=16)          :: cfmt
    character(len=10)          :: cval
    integer(I4B)               :: jend
    integer(I4B)               :: nblc
    integer(I4B)               :: junit
    integer(I4B)               :: ilevel
    integer(I4B)               :: leadblank
    integer(I4B)               :: itake
    integer(I4B)               :: ipos
    integer(I4B)               :: i
    integer(I4B)               :: j
    !
    ! -- return if no message is passed
    if (len_trim(message) < 1) then
      return
    end if
    !
    ! -- initialize local variables
    amessage = message
    junit = istdout
    ablank = ' '
    itake = 0
    j = 0
    !
    ! -- ensure that there is at least one blank space at the start of amessage
    if (amessage(1:1) /= ' ') then
      amessage = ' ' // trim(amessage)
    end if
    !
    ! -- process optional dummy variables
    ! -- set the unit number
    if(present(iunit))then
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
    ! -- create the counter to prepend to amessage
    if (iwidth > 0) then
      write(cfmt, '(A,I0,A)') '(1X,I', iwidth, ',".")'
      write(cval, cfmt) icount
      ipos = len_trim(cval)
    else
      cval = ''
      ipos = 2
    end if
    !
    ! -- prepend amessage with the counter
    nblc = len_trim(amessage)
    amessage = adjustr(amessage(1:nblc+ipos))
    if (nblc+ipos < len(amessage)) then
      amessage(nblc+ipos+1:) = ' '
    end if
    amessage(1:ipos) = cval(1:ipos)
    !
    ! -- set the number of leading blanks
    leadblank = ipos - 1
    !
    ! -- calculate the final length of the message after modification
    nblc = len_trim(amessage)
    !
    ! -- parse the amessage into multiple lines
5   continue
    jend = j + 78 - itake
    if (jend >= nblc) go to 100
    do i = jend, j+1, -1
      if (amessage(i:i).eq.' ') then
        if (itake.eq.0) then
          call sim_message(amessage(j+1:i), iunit=junit, level=ilevel)
          itake = 2 + leadblank
        else
          call sim_message(ablank(1:leadblank+2)//amessage(j+1:i),               &
                           iunit=junit, level=ilevel)
        end if
        j = i
        go to 5
      end if
    end do
    if (itake == 0)then
      call sim_message(amessage(j+1:jend), iunit=junit, level=ilevel)
      itake = 2 + leadblank
    else
      call sim_message(ablank(1:leadblank+2)//amessage(j+1:jend),                &
                       iunit=junit, level=ilevel)
    end if
    j = jend
    go to 5
    !
    ! -- last piece of amessage to write to a line
100 continue
    jend = nblc
    if (itake == 0)then
      call sim_message(amessage(j+1:jend), iunit=junit, level=ilevel)
    else
      if (leadblank > 0) then
        call sim_message(ablank(1:leadblank+2)//amessage(j+1:jend), &
                         iunit=junit, level=ilevel)
      else
        call sim_message(amessage(j+1:jend), &
                         iunit=junit, level=ilevel)
      end if
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
    character(len=*), intent(in) :: text            !< message to write to iunit
    integer(I4B), intent(in) :: linelen             !< length of line to center text in
    integer(I4B), intent(in), optional :: iunit     !< optional file unit to write text (default=stdout)
    ! -- local variables
    character(len=LINELENGTH) :: newline
    character(len=LINELENGTH) :: textleft
    integer(I4B) :: iu
    integer(I4B) :: loc1
    integer(I4B) :: loc2
    integer(I4B) :: lentext
    integer(I4B) :: nspaces
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
      textleft = adjustl(text)
      lentext = len_trim(textleft)
      nspaces = linelen - lentext
      loc1 = (nspaces / 2) + 1
      loc2 = loc1 + lentext - 1
      newline = ' '
      newline(loc1:loc2) = textleft
      !
      ! -- write processed text to iu
      write(iu,'(a)') trim(newline)
    end if
    !
    ! -- retirn
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
    logical(LGP) :: lvalue                 !< boolean indicating if a and b are the same
    ! -- dummy variables
    real(DP), intent(in)   :: a            !< first number to evaluate 
    real(DP), intent(in)   :: b            !< second number to evaluate
    real(DP), intent(in), optional :: eps  !< optional maximum difference between a abd b (default=DSAME)
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
    endif
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
      rdiff = abs( (a - b) / denom )
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
    integer(I4B), intent(in), optional :: ierr  !< optional error code to return (default=0)
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
    select case (ireturn_err)
      case (0)
        stop
      case (1)
        stop 1
      case (2)
        stop 2
      case (138)
        stop 138
      case default
        stop 999
    end select
  
  end subroutine stop_with_error  

  end module GenericUtilitiesModule
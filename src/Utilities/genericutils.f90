module GenericUtilitiesModule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DPREC, DSAME,                                &
                             LINELENGTH, LENHUGELINE, VSUMMARY
  use SimVariablesModule, only: istdout, isim_level
  !
  implicit none 
  
  private
  
  public :: sim_message
  public :: write_centered
  public :: is_same
  public :: stop_with_error

  contains

  subroutine sim_message(message, iunit, fmt, level,                             &
                         skipbefore, skipafter, advance)
  ! ******************************************************************************
  ! Print message to user specified iunit or STDOUT based on level.
  !
  ! -- Arguments are as follows:
  !     message               : message to write to iunit
  !     iunit      (optional) : file unit to write the message to (default=stdout)
  !     fmt        (optional) : format to write the message (default='(a)')
  !     level      (optional) : level for the message (default=summary)
  !     skipbefore (optional) : number of empty lines before message
  !     skipafter  (optional) : number of empty lines after message
  !     advance    (optional) : advancing output (default is .TRUE.)
  !
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    ! -- modules
    implicit none
    ! -- dummy
    character(len=*), intent(in) :: message
    integer(I4B), intent(in), optional :: iunit
    character(len=*), intent(in), optional :: fmt
    integer(I4B), intent(in), optional :: level
    integer(I4B), intent(in), optional :: skipbefore
    integer(I4B), intent(in), optional :: skipafter
    logical, intent(in), optional :: advance
    ! -- local
    character(len=3) :: cadvance
    integer(I4B) :: i
    integer(I4B) :: ilen
    integer(I4B) :: iu
    integer(I4B) :: ilevel
    character(len=LENHUGELINE) :: simfmt
    character(len=*), parameter :: stdfmt = '(a)'
    character(len=*), parameter :: emptyfmt = '()'
  ! ------------------------------------------------------------------------------
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

  subroutine write_centered(text, linelen, iunit)
  ! ******************************************************************************
  ! Write text to unit iunit centered in width defined by linelen. Left-pad with
  ! blanks as needed.
  !
  ! -- Arguments are as follows:
  !     text               : message to write to iunit
  !     linelen            : length of line to center text in
  !     iunit   (optional) : file unit to write text (default stdout)
  !
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: text
    integer(I4B), intent(in) :: linelen
    integer(I4B), intent(in), optional :: iunit
    ! -- local
    character(len=LINELENGTH) :: newline
    character(len=LINELENGTH) :: textleft
    integer(I4B) :: iu
    integer(I4B) :: loc1
    integer(I4B) :: loc2
    integer(I4B) :: lentext
    integer(I4B) :: nspaces
    ! -- code
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
  
  function is_same(a, b, eps) result(lvalue)
  ! ******************************************************************************
  ! Evaluate if the difference between a and b are less than eps 
  ! (i.e. a and b are the same).
  !
  ! -- Arguments are as follows:
  !     a              : first number to evaluate 
  !     b              : second number to evaluate
  !     eps (optional) : maximum difference between a abd b (default DSAME)
  !
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    ! -- return variable
    logical :: lvalue
    ! -- dummy arguments
    real(DP), intent(in)   :: a
    real(DP), intent(in)   :: b
    real(DP), intent(in), optional :: eps
    ! -- local definitions 
    real(DP) :: epsloc
    real(DP) :: denom
    real(DP) :: rdiff
    ! -- parameters
    ! -- functions
    ! -- code
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

  subroutine stop_with_error(ierr)
  ! ******************************************************************************
  ! Stop the program and issue the correct return code 
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    integer(I4B), intent(in), optional :: ierr
    ! -- local
    integer(I4B) :: ireturn_err
  !-------------------------------------------------------------------------------
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
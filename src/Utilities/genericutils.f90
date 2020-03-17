module GenericUtilitiesModule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DPREC, DSAME,                                &
                             LINELENGTH, LENHUGELINE, VSUMMARY
  use SimVariablesModule, only: istdout, isim_level
  !
  implicit none 
  
  private
  
  public :: sim_message
  public :: is_same
  public :: stop_with_error

  contains

  subroutine sim_message(message, iunit, fmt, level)
  ! ******************************************************************************
  ! Print message to user specified iunit or STDOUT based on level.
  !
  ! -- Arguments are as follows:
  !     message            : message to write to iunit
  !     iunit   (optional) : file unit to write the message to (default=istdout)
  !     fmt     (optional) : format to write the message (default='(a)')
  !     level   (optional) : level for the message (default=summary)
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
    ! -- local
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
    !
    ! -- write message if the level of the message is less than
    !    or equal the isim_level for the simulation
    if (ilevel <= isim_level) then
      if (ilen > 0) then
        write(iu, trim(simfmt)) message(1:ilen)
      else
        write(iu, trim(simfmt))
      end if
    end if
    !
    ! -- return
    return
  end subroutine sim_message
  
  function is_same(a, b, eps) result(lvalue)
    ! -- return
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
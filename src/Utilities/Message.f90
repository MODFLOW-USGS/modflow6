! Comprehensive table object that stores all of the 
! intercell flows, and the inflows and the outflows for 
! an advanced package.
module MessageModule
  
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LINELENGTH, MAXCHARLEN, DONE
  use GenericUtilitiesModule, only: sim_message  
  use SimVariablesModule,     only: istdout
  use ArrayHandlersModule, only: ExpandArray  
  
  implicit none
  
  public :: MessageType
  
  type :: MessageType

    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: name
    integer(I4B) :: nmessage = 0
    integer(I4B) :: ncount = 0
    integer(I4B) :: max_message = 1000
    integer(I4B) :: max_exceeded = 0
    integer(I4B) :: inc_message = 100
    character(len=MAXCHARLEN), allocatable, dimension(:) :: message
    integer(I4B), allocatable, dimension(:) :: icount
    
    contains
  
    procedure :: count_message
    procedure :: set_max_message
    procedure :: store_message
    procedure :: print_message

  end type MessageType
  
  contains
  
    subroutine count_message(this, ncount)
    ! ******************************************************************************
    ! Return message count
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
      ! -- modules
      ! -- return variable
      ! -- dummy
      class(MessageType) :: this
      integer(I4B), intent(inout) :: ncount
    ! ------------------------------------------------------------------------------
      if (allocated(this%message)) then
        ncount = this%ncount
      else
        ncount = 0
      end if
      !
      ! -- return
      return
    end subroutine count_message
    
    subroutine set_max_message(this, imax)
    ! ******************************************************************************
    ! Set maximum number of messages stored
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
      ! -- modules
      ! -- return variable
      ! -- dummy
      class(MessageType) :: this
      integer(I4B), intent(in) :: imax
    ! ------------------------------------------------------------------------------
      this%max_message = imax
      !
      ! -- return
      return
    end subroutine set_max_message
    
    subroutine store_message(this, msg, count)
    ! ******************************************************************************
    ! Store an error message for printing at end of simulation
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
      ! -- modules
      ! -- dummy
      class(MessageType) :: this
      character(len=*), intent(in) :: msg
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
        inc_count = .TRUE.
      end if
      !
      ! -- determine if messages should be expanded
      inc_array = .TRUE.
      if (allocated(this%icount)) then
        i = this%nmessage
        if (i < size(this%icount)) then
          inc_array = .FALSE.
        end if
      end if
      !
      ! -- resize message and icount
      if (inc_array) then
        call ExpandArray(this%message, increment=this%inc_message)
        call ExpandArray(this%icount, increment=this%inc_message)
        this%inc_message = this%inc_message * 1.1
      end if
      !
      ! -- store this message and calculate ncount
      i = this%nmessage + 1
      if (i <= this%max_message) then
        this%nmessage = i
        if (inc_count) then
          j = this%ncount + 1
          this%ncount = j
        else
          j = 0
        end if
        this%message(i) = msg
        this%icount(i) = j
      else
        this%max_exceeded = this%max_exceeded + 1
      end if
      !
      ! -- return
      return
    end subroutine store_message
    
    subroutine print_message(this, title, name, iunit)
    ! ******************************************************************************
    ! Print all messages that have been stored
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
      ! -- modules
      ! -- dummy
      class(MessageType) :: this
      character(len=*), intent(in) :: title
      character(len=*), intent(in) :: name
      integer(I4B), intent(in), optional :: iunit
      ! -- local
      character(len=LINELENGTH) :: errmsg
      integer(I4B) :: iu
      integer(I4B) :: i
      integer(I4B) :: isize
      integer(I4B) :: iwidth
      real(DP) :: rval
      ! -- formats
      character(len=*), parameter :: stdfmt = "(/,A,/)"
    ! ------------------------------------------------------------------------------
      !
      ! -- process optional variables
      if (present(iunit)) then
        iu = iunit
      else
        iu = 0
      end if
      !
      if (allocated(this%message)) then
        isize = this%nmessage
        if (isize > 0) then
          !
          ! -- calculate the maximum width of the prepended string
          !    for the counter
          rval = real(isize, DP)
          iwidth = modulo(log10(rval), DONE) + 1
          !
          ! -- write title for message
          if (iu > 0) then
            call sim_message(title, iunit=iu, fmt=stdfmt)
          end if
          call sim_message(title, fmt=stdfmt)
          !
          ! -- write each message
          do i = 1, isize
            call write_message(this%message(i), this%icount(i), iwidth=iwidth)
            if (iu > 0) then
              call write_message(this%message(i), this%icount(i), iwidth=iwidth, iunit=iu)
            end if
          end do
          !
          ! -- write the number of additional messages
          if (this%max_exceeded > 0) then
            write(errmsg, '(i0,3(1x,a))')                                        &
              this%max_exceeded, 'additional', trim(name),                       &
              'detected but not printed.'
            call sim_message(trim(errmsg), fmt='(/,1x,a)')
            if (iu > 0) then
              call sim_message(trim(errmsg), iunit=iu, fmt='(/,1x,a)')
            end if
          end if
        end if
      end if
      !
      ! -- return
      return
    end subroutine print_message

    subroutine write_message(message, icount, iwidth, iunit)
  ! ******************************************************************************
  ! Subroutine write_message formats and writes a message.
  !
  ! -- Arguments are as follows:
  !       MESSAGE      : message to be written
  !       ICOUNT       : counter for message. If ICOUNT==0 then no counter is
  !                      prepended to the first line  
  !       IWIDTH       : maximum width of the prepended counter
  !       IUNIT        : the unit number to which the message is written
  !
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    ! -- dummy
    character (len=*), intent(in)           :: message
    integer(I4B),      intent(in)           :: icount
    integer(I4B),      intent(in)           :: iwidth
    integer(I4B),      intent(in), optional :: iunit
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
    ! -- prepend amessage with counter
    write(cfmt, '(A,I0,A)') '(1X,I', iwidth, ',".")'
    !
    ! -- update amessage with the counter (or '*')
    if (icount > 0) then
      write(cval, cfmt) icount
      ipos = len_trim(cval)
    else
      write(cfmt, '(A,I0,A)') '(1X,A', iwidth, ',1X)'
      write(cval, cfmt) '*'
      ipos = iwidth + 2
    end if
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
    ! -- add blank line if an uncounted message
    if (icount < 1) then
      call sim_message('', iunit=junit)
    end if
    !
    ! -- parse the amessage into multiple lines
5   continue
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
    ! -- return
    return
  end subroutine write_message

end module MessageModule

!> @brief This module contains the PrintSaveManagerModule
!!
!! This module defines the PrintSaveManagerType, which can be used
!! to determine when something should be printed and/or saved. The
!! object should be initiated with the following call:
!!   call psm_obj%init()
!!
!! The set method will configure the members based on the following
!! keywords when set is called as follows:
!!   call psm_obj%set(nstp, line)
!! where line may be in the following form:
!!   PRINT ALL
!!   PRINT STEPS 1 4 5 6
!!   PRINT FIRST
!!   PRINT LAST
!!   PRINT FREQUENCY 4
!!   SAVE ALL
!!   SAVE STEPS 1 4 5 6
!!   SAVE FIRST
!!   SAVE LAST
!!   SAVE FREQUENCY 4
!!
!! Based on the keywords, the object can be called with
!!   psm_obj%time_to_print(kstp, kper)
!!   psm_obj%time_to_save(kstp, kper)
!! to return a logical flag indicating whether or not it
!! it is time to print or time to save
!!
!<
module PrintSaveManagerModule

  use KindModule, only: DP, I4B, LGP
  use ArrayHandlersModule, only: expandarray
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use InputOutputModule, only: urword

  implicit none
  private
  public :: PrintSaveManagerType

  !> @ brief PrintSaveManagerType
  !!
  !!  Object for storing information and determining whether or
  !!  not data should be printed to a list file or saved to disk.
  !<
  type :: PrintSaveManagerType
    integer(I4B), allocatable, dimension(:) :: kstp_list_print
    integer(I4B), allocatable, dimension(:) :: kstp_list_save
    integer(I4B) :: ifreq_print
    integer(I4B) :: ifreq_save
    logical :: print_first
    logical :: save_first
    logical :: print_last
    logical :: save_last
    logical :: print_all
    logical :: save_all
    logical :: save_detected
    logical :: print_detected
  contains
    procedure :: init
    procedure :: rp
    procedure :: kstp_to_print
    procedure :: kstp_to_save
  end type PrintSaveManagerType

contains

  !> @ brief Initialize PrintSaveManager
  !!
  !!  Initializes variables of a PrintSaveManagerType
  !!
  !<
  subroutine init(this)
    ! -- dummy
    class(PrintSaveManagerType) :: this !< psm object to initialize
    !
    ! -- Initialize members to their defaults
    if (allocated(this%kstp_list_print)) deallocate (this%kstp_list_print)
    if (allocated(this%kstp_list_save)) deallocate (this%kstp_list_save)
    allocate (this%kstp_list_print(0))
    allocate (this%kstp_list_save(0))
    this%ifreq_print = 0
    this%ifreq_save = 0
    this%save_first = .false.
    this%save_last = .false.
    this%save_all = .false.
    this%print_first = .false.
    this%print_last = .false.
    this%print_all = .false.
    this%save_detected = .false.
    this%print_detected = .false.
    !
    ! -- return
    return
  end subroutine init

  !> @ brief Read and prepare for PrintSaveManager
  !!
  !!  Parse information in the line and assign settings for the
  !!  PrintSaveManagerType.
  !!
  !<
  subroutine rp(this, linein, iout)
    ! -- dummy
    class(PrintSaveManagerType) :: this !< psm object
    character(len=*), intent(in) :: linein !< character line of information
    integer(I4B), intent(in) :: iout !< unit number of output file
    ! -- local
    character(len=len(linein)) :: line
    logical lp, ls
    integer(I4B) :: n
    integer(I4B) :: lloc, istart, istop, ival
    real(DP) :: rval
    ! -- formats
    character(len=*), parameter :: fmt_steps = &
      &"(6x,'THE FOLLOWING STEPS WILL BE ',A,': ',50(I0,' '))"
    character(len=*), parameter :: fmt_freq = &
      &"(6x,'THE FOLLOWING FREQUENCY WILL BE ',A,': ',I0)"
    !
    ! -- Set the values based on line
    ! -- Get keyword to use in assignment
    line(:) = linein(:)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
    !
    ! -- set dimension for print or save
    lp = .false.
    ls = .false.
    select case (line(istart:istop))
    case ('PRINT')
      lp = .true.
    case ('SAVE')
      ls = .true.
    case default
      write (errmsg, '(2a)') &
        'Looking for PRINT or SAVE. Found:', trim(adjustl(line))
      call store_error(errmsg, terminate=.TRUE.)
    end select
    !
    ! -- set member variables
    this%save_detected = ls
    this%print_detected = lp
    !
    ! -- set the steps to print or save
    call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
    select case (line(istart:istop))
    case ('ALL')
      if (lp) then
        this%print_all = .true.
        if (iout > 0) write (iout, "(6x,a)") 'ALL TIME STEPS WILL BE PRINTED'
      end if
      if (ls) then
        this%save_all = .true.
        if (iout > 0) write (iout, "(6x,a)") 'ALL TIME STEPS WILL BE SAVED'
      end if
    case ('STEPS')
      listsearch: do
        call urword(line, lloc, istart, istop, 2, ival, rval, -1, 0)
        if (ival > 0) then
          if (lp) then
            n = size(this%kstp_list_print)
            call expandarray(this%kstp_list_print)
            this%kstp_list_print(n + 1) = ival
          end if
          if (ls) then
            n = size(this%kstp_list_save)
            call expandarray(this%kstp_list_save)
            this%kstp_list_save(n + 1) = ival
          end if
          cycle listsearch
        end if
        exit listsearch
      end do listsearch
      if (iout > 0) then
        if (lp) write (iout, fmt_steps) 'PRINTED', this%kstp_list_print
        if (ls) write (iout, fmt_steps) 'SAVED', this%kstp_list_save
      end if
    case ('FREQUENCY')
      call urword(line, lloc, istart, istop, 2, ival, rval, -1, 0)
      if (lp) this%ifreq_print = ival
      if (ls) this%ifreq_save = ival
      if (iout > 0) then
        if (lp) write (iout, fmt_freq) 'PRINTED', this%ifreq_print
        if (ls) write (iout, fmt_freq) 'SAVED', this%ifreq_save
      end if
    case ('FIRST')
      if (lp) then
        this%print_first = .true.
        if (iout > 0) write (iout, "(6x,a)") 'THE FIRST TIME STEP WILL BE PRINTED'
      end if
      if (ls) then
        this%save_first = .true.
        if (iout > 0) write (iout, "(6x,a)") 'THE FIRST TIME STEP WILL BE SAVED'
      end if
    case ('LAST')
      if (lp) then
        this%print_last = .true.
        if (iout > 0) write (iout, "(6x,a)") 'THE LAST TIME STEP WILL BE PRINTED'
      end if
      if (ls) then
        this%save_last = .true.
        if (iout > 0) write (iout, "(6x,a)") 'THE LAST TIME STEP WILL BE SAVED'
      end if
    case default
      write (errmsg, '(2a)') &
        'Looking for ALL, STEPS, FIRST, LAST, OR FREQUENCY. Found: ', &
        trim(adjustl(line))
      call store_error(errmsg, terminate=.TRUE.)
    end select
    !
    ! -- return
    return
  end subroutine rp

  !> @ brief Determine if it is time to print the data
  !!
  !!  Determine if data should be printed based on kstp and endofperiod
  !!
  !<
  logical function kstp_to_print(this, kstp, endofperiod)
    ! -- dummy
    class(PrintSaveManagerType) :: this !< psm object
    integer(I4B), intent(in) :: kstp !< current time step
    logical(LGP), intent(in) :: endofperiod !< flag indicating end of stress period
    ! -- local
    integer(I4B) :: i, n
    !
    kstp_to_print = .false.
    if (this%print_all) kstp_to_print = .true.
    if (kstp == 1 .and. this%print_first) kstp_to_print = .true.
    if (endofperiod .and. this%print_last) kstp_to_print = .true.
    if (this%ifreq_print > 0) then
      if (mod(kstp, this%ifreq_print) == 0) kstp_to_print = .true.
    end if
    n = size(this%kstp_list_print)
    if (n > 0) then
      do i = 1, n
        if (kstp == this%kstp_list_print(i)) then
          kstp_to_print = .true.
          exit
        end if
      end do
    end if
    !
    ! -- Return
    return
  end function kstp_to_print

  !> @ brief Determine if it is time to save the data
  !!
  !!  Determine if data should be saved based on kstp and endofperiod
  !!
  !<
  logical function kstp_to_save(this, kstp, endofperiod)
    ! -- dummy
    class(PrintSaveManagerType) :: this !< psm object
    integer(I4B), intent(in) :: kstp !< current time step
    logical(LGP), intent(in) :: endofperiod !< flag indicating end of stress period
    ! -- local
    integer(I4B) :: i, n
    !
    kstp_to_save = .false.
    if (this%save_all) kstp_to_save = .true.
    if (kstp == 1 .and. this%save_first) kstp_to_save = .true.
    if (endofperiod .and. this%save_last) kstp_to_save = .true.
    if (this%ifreq_save > 0) then
      if (mod(kstp, this%ifreq_save) == 0) kstp_to_save = .true.
    end if
    n = size(this%kstp_list_save)
    if (n > 0) then
      do i = 1, n
        if (kstp == this%kstp_list_save(i)) then
          kstp_to_save = .true.
          exit
        end if
      end do
    end if
    !
    ! -- Return
    return
  end function kstp_to_save

end module PrintSaveManagerModule

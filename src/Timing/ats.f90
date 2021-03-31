! todo:
!   state advance/restore gwt
!   state advance/restore gwf packages
!   check ats input and echo
!   check ats not active for steady state?
!   code comments
!   store iboundold for rewetting case
!   recalculate time series variables for time step retry
!   add ATS options to solutions, packages, and models?
module AdaptiveTimeStepModule
 
  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: iout
  use SimModule, only: ustop, store_error, count_errors
  use BlockParserModule, only: BlockParserType
  use ConstantsModule, only: DZERO, DONE, LINELENGTH, DNODATA

  implicit none
  private
  public :: isAdaptivePeriod
  public :: ats_set_delt
  public :: ats_period_message
  public :: ats_set_endofperiod
  public :: ats_submit_delt
  public :: ats_reset_delt
  public :: ats_cr
  public :: ats_da

  integer(I4B), pointer                            :: nper => null()       !< set equal to nper
  integer(I4B), pointer                            :: maxats => null()     !< number of ats entries
  real(DP), public, pointer                        :: dtstable => null()   !< delt value required for stability
  integer(I4B), dimension(:), pointer, contiguous  :: kperats => null()    !< array of stress period numbers to apply ats
  real(DP), dimension(:), pointer, contiguous      :: dt0 => null()        !< input array of initial time step sizes
  real(DP), dimension(:), pointer, contiguous      :: dtmin => null()      !< input array of minimum time step sizes
  real(DP), dimension(:), pointer, contiguous      :: dtmax => null()      !< input array of maximum time step sizes
  real(DP), dimension(:), pointer, contiguous      :: dtadj => null()      !< input array of time step factors for shortening or increasing
  real(DP), dimension(:), pointer, contiguous      :: dtfailadj => null()  !< input array of time step factors for shortening due to nonconvergence
  type(BlockParserType)                            :: parser               !< block parser for reading input file
 
  contains
  
  function isAdaptivePeriod(kper) result(lv)
    integer(I4B), intent(in) :: kper
    logical(LGP) :: lv
    lv = .false.
    if (associated(kperats)) then
      if (kperats(kper) > 0) then
        lv = .true.
      end if
    end if
    return
  end function isAdaptivePeriod
  
  subroutine ats_submit_delt(kstp, kper, dt, sloc, idir)
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    real(DP), intent(in) :: dt
    character(len=*), intent(in) :: sloc
    integer(I4B), intent(in), optional :: idir
    ! -- local
    integer(I4B) :: n
    real(DP) :: tsfact
    real(DP) :: dt_temp
    character(len=*), parameter :: fmtdtsubmit =                               &
      &"(1x, 'ATS: ', A,' submitted a preferred time step size of ', G15.7)"
    
    if (isAdaptivePeriod(kper)) then
      n = kperats(kper)
      tsfact = dtadj(n)
      if (tsfact > DONE) then
        !
        ! -- if idir is present, then dt is a length that should be adjusted
        !    (divided by or multiplied by) by dtadj.  If idir is not present
        !    then dt is the submitted time step.
        if (present(idir)) then
          dt_temp = DZERO
          if (idir == -1) then
            dt_temp = dt / tsfact
          else if (idir == 1) then
            dt_temp = dt * tsfact
          end if
        else
          dt_temp = dt
        end if
        if (kstp > 1 .and. dt_temp > DZERO) then
          write(iout, fmtdtsubmit) trim(adjustl(sloc)), dt_temp
        end if
        if (dt_temp > DZERO .and. dt_temp < dtstable) then
          ! -- Reset dtstable to a smaller value
          dtstable = dt_temp
        end if
      end if
    end if
    return
  end subroutine ats_submit_delt
  
  subroutine ats_reset_delt(kstp, kper, lastStepFailed, delt, finishedTrying)
    ! -- modules
    ! -- dummy
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    integer(I4B), intent(in) :: lastStepFailed
    real(DP), intent(inout) :: delt
    logical, intent(inout) :: finishedTrying
    ! -- local
    integer(I4B) :: n
    real(DP) :: delt_temp
    real(DP) :: tsfact
    ! -- formats
    character(len=*),parameter :: fmttsi =                                     &
      "(1X, 'Failed solution for step ', i0, ' and period ', i0, &
      &' will be retried using time step of ', G15.7)"
    
    if (isAdaptivePeriod(kper)) then
      if (lastStepFailed /= 0) then
    
        delt_temp = delt
        n = kperats(kper)
        tsfact = dtfailadj(n)
        if (tsfact > DONE) then
          delt_temp = delt / tsfact
          if (delt_temp >= dtmin(n)) then
            finishedTrying = .false.
            delt = delt_temp
            write(iout, fmttsi) kstp, kper, delt
          end if
        end if
        
      end if
    end if
    return
  end subroutine ats_reset_delt
  
  subroutine ats_cr(inunit, nper_tdis)
! ******************************************************************************
! ats_cr -- create ats
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: nper_tdis
    ! -- local
    ! -- formats
    character(len=*),parameter :: fmtheader = &
        "(1X,/1X,'ATS -- ADAPTIVE TIME STEP PACKAGE,',   /                  &
        &' VERSION 1 : 03/18/2021 - INPUT READ FROM UNIT ',I0)"
! ------------------------------------------------------------------------------
    !
    ! -- Allocate the scalar variables
    call ats_allocate_scalars()
    !
    ! -- Identify package
    write(iout, fmtheader) inunit
    !
    ! -- Initialize block parser
    call parser%initialize(inunit, iout)
    !
    ! -- Read options
    call ats_read_options()
    !
    ! -- store tdis nper in nper
    nper = nper_tdis
    !
    ! -- Read dimensions and then allocate arrays
    call ats_read_dimensions()
    call ats_allocate_arrays()
    !
    ! -- Read timing
    call ats_read_timing()
    !
    ! -- Close the file
    call parser%Clear()
    !
    ! -- return
    return
  end subroutine ats_cr

  subroutine ats_allocate_scalars()
! ******************************************************************************
! ats_allocate_scalars -- allocate scalar variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
! ------------------------------------------------------------------------------
    !
    ! -- memory manager variables
    call mem_allocate(nper, 'NPER', 'ATS')
    call mem_allocate(maxats, 'MAXATS', 'ATS')
    call mem_allocate(dtstable, 'DTSTABLE', 'ATS')
    !
    ! -- Initialize variables
    nper = 0
    maxats = 0
    dtstable = DNODATA
    !
    ! -- return
    return
  end subroutine ats_allocate_scalars
    
  subroutine ats_allocate_arrays()
! ******************************************************************************
! ats_allocate_arrays -- Allocate ats arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    call mem_allocate(kperats, nper, 'KPERATS', 'ATS')
    call mem_allocate(dt0, maxats, 'DT0', 'ATS')
    call mem_allocate(dtmin, maxats, 'DTMIN', 'ATS')
    call mem_allocate(dtmax, maxats, 'DTMAX', 'ATS')
    call mem_allocate(dtadj, maxats, 'DTADJ', 'ATS')
    call mem_allocate(dtfailadj, maxats, 'DTFAILADJ', 'ATS')
    !
    ! -- initialize kperats
    do n = 1, nper
      kperats(n) = 0
    end do
    !
    ! -- initialize
    do n = 1, maxats
      dt0(n) = DZERO
      dtmin(n) = DZERO
      dtmax(n) = DZERO
      dtadj(n) = DZERO
      dtfailadj(n) = DZERO
    end do
    !
    ! -- return
    return
  end subroutine ats_allocate_arrays

  subroutine ats_da()
! ******************************************************************************
! ats_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_deallocate
! ------------------------------------------------------------------------------
    !
    ! -- Scalars
    call mem_deallocate(nper)
    call mem_deallocate(maxats)
    call mem_deallocate(dtstable)
    !
    ! -- Arrays
    call mem_deallocate(kperats)
    call mem_deallocate(dt0)
    call mem_deallocate(dtmin)
    call mem_deallocate(dtmax)
    call mem_deallocate(dtadj)
    call mem_deallocate(dtfailadj)
    !
    ! -- Return
    return
  end subroutine ats_da


  subroutine ats_read_options()
! ******************************************************************************
! ats_read_options -- Read the options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call parser%GetBlock('OPTIONS', isfound, ierr, &
      supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(iout,'(1x,a)')'PROCESSING ATS OPTIONS'
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
        case default
          write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN ATS OPTION: ',        &
                                    trim(keyword)
          call store_error(errmsg)
          call parser%StoreErrorUnit()
          call ustop()
        end select
      end do
      write(iout,'(1x,a)') 'END OF ATS OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine ats_read_options
    
  subroutine ats_read_dimensions()
! ******************************************************************************
! ats_read_dimensions -- Read dimension MAXATS
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtmaxats = &
      "(1X,I0,' ADAPTIVE TIME STEP RECORDS(S) WILL FOLLOW IN PERIODDATA')"
! ------------------------------------------------------------------------------
    !
    ! -- get DIMENSIONS block
    call parser%GetBlock('DIMENSIONS', isfound, ierr, &
      supportOpenClose=.true.)
    !
    ! -- parse block if detected
    if (isfound) then
      write(iout,'(1x,a)')'PROCESSING ATS DIMENSIONS'
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          case ('MAXATS')
            maxats = parser%GetInteger()
            write(iout, fmtmaxats) maxats
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN ATS DIMENSION: ',     &
                                      trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(iout,'(1x,a)') 'END OF ATS DIMENSIONS'
    else
      write(errmsg,'(1x,a)')'ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.'
      call store_error(errmsg)
      call parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Return
    return
  end subroutine ats_read_dimensions
        
  subroutine ats_read_timing()
! ******************************************************************************
! ats_read_timing -- Read timing information
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: ierr
    integer(I4B) :: kkper
    integer(I4B) :: n
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtheader =                                 &
      "(1X,//1X,'STRESS PERIOD     LENGTH       TIME STEPS',                   &
        &'     MULTIPLIER FOR DELT',/1X,76('-'))"
    character(len=*), parameter :: fmtrow =                                    &
      "(1X,I8,1PG21.7,I7,0PF25.3)"
! ------------------------------------------------------------------------------
    !
    ! -- get PERIODDATA block
    call parser%GetBlock('PERIODDATA', isfound, ierr, &
      supportOpenClose=.true.)
    !
    ! -- parse block if detected
    if (isfound) then
      write(iout,'(1x,a)')'PROCESSING ATS PERIODDATA'
      write(iout, fmtheader)
      n = 1
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        kkper = parser%GetInteger()
        if (kperats(kkper) == 1) then
          write(errmsg,'(1x,a)') 'Found duplicate ATS entry for period ', kkper
          call store_error(errmsg)
        end if
        !
        ! -- set kperats to index value for dt0, tminats, tmaxats
        kperats(kkper) = n
        !
        ! -- fill the ats data arrays
        dt0(n) = parser%GetDouble()
        dtmin(n) = parser%GetDouble()
        dtmax(n) = parser%GetDouble()
        dtadj(n) = parser%GetDouble()   !todo: ensure value is 0, 1, or greater than 1
        dtfailadj(n) = parser%GetDouble()  !todo: ensure value is 0, 1, or greater than 1
        n = n + 1
      enddo
      !
      ! -- Check timing information
      !call check_ats_timing(nper, perlen, nstp, tsmult)
      !call parser%terminateblock()
      !
      ! -- Check for errors
      if(count_errors() > 0) then
        call parser%StoreErrorUnit()
        call ustop()
      endif
      write(iout,'(1x,a)') 'END OF ATS PERIODDATA'
    else
      write(errmsg,'(1x,a)')'ERROR.  REQUIRED PERIODDATA BLOCK NOT FOUND.'
      call store_error(errmsg)
      call parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Return
    return
  end subroutine ats_read_timing
  
  subroutine ats_period_message(kper)
    integer(I4B), intent(in) :: kper
    integer(I4B) :: n
    character(len=*),parameter :: fmtspts =                                    &
    "(28X,'ATS IS OVERRIDING TIME STEPPING FOR THIS PERIOD',/                  &
      &28X,'INITIAL TIME STEP SIZE                 (DT0) = ',G15.7,/           &
      &28X,'MINIMUM TIME STEP SIZE               (DTMIN) = ',G15.7,/           &
      &28X,'MAXIMUM TIME STEP SIZE               (DTMAX) = ',G15.7,/           &
      &28X,'MULTIPLIER/DIVIDER FOR TIME STEP     (DTADJ) = ',G15.7,/           &
      &28X,'DIVIDER FOR FAILED TIME STEP     (DTFAILADJ) = ',G15.7,/           &
      &)"
    n = kperats(kper)
    write(iout, fmtspts) dt0(n), dtmin(n), dtmax(n), dtadj(n), dtfailadj(n)
      
  end subroutine ats_period_message
  
  subroutine ats_set_delt(kstp, kper, pertim, perlencurrent, delt)
    ! -- modules
    ! -- dummy
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    real(DP), intent(inout) :: pertim
    real(DP), intent(in) :: perlencurrent
    real(DP), intent(inout) :: delt
    ! -- local
    integer(I4B) :: n
    real(DP) :: tstart
    ! -- formats
    character(len=*), parameter :: fmtdt =                               &
      &"(1x, 'ATS: time step set to ', G15.7, ' for step ', i0,          &
      &' and period ', i0)"
    !
    ! -- initialize the record position (n) for this stress period
    n = kperats(kper)
    !
    ! -- set tstart to the end of the last time step.
    tstart = pertim
    !
    ! -- Calculate delt
    !
    ! -- Setup new stress period if kstp is 1
    if(kstp == 1) then
      !
      ! -- Assign first value of delt for this stress period
      if (dt0(n) /= DZERO) then
        delt = dt0(n)
      else
        ! leave delt the way it was
      end if
    else
      !
      ! -- Assign delt based on stability
      if (dtstable /= DNODATA) then
        delt = dtstable
        dtstable = DNODATA
      end if
    end if
    !
    ! -- Ensure  tsmin < delt < tsmax
    if (delt < dtmin(n)) then
      delt = dtmin(n)
    end if
    if (delt > dtmax(n)) then
      delt = dtmax(n)
    end if
    !
    ! -- Cut timestep down to meet end of period
    if (tstart + delt > perlencurrent - dtmin(n)) then
      delt = perlencurrent - tstart
    end if
    !
    ! -- Write time step size information
    write(iout, fmtdt) delt, kstp, kper
    !
    return
 end subroutine ats_set_delt
                          
 subroutine ats_set_endofperiod(kper, pertim, perlencurrent, endofperiod)
    integer(I4B), intent(in) :: kper
    real(DP), intent(inout) :: pertim
    real(DP), intent(in) :: perlencurrent
    logical(LGP), intent(inout) :: endofperiod
    ! -- local
    integer(I4B) :: n
    !
    ! -- End of stress period and/or simulation?  
    !    todo: need more reliable check
    n = kperats(kper)
    if (abs(pertim - perlencurrent) < dtmin(n)) then
      endofperiod = .true.
    end if
 
 end subroutine ats_set_endofperiod
      
   
end module AdaptiveTimeStepModule
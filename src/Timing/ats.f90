! todo:
!   - fail if ats is active for a steady state period
!   - read ats info for numerical solution
!   - handle solver failure
!       - Set failed flag (this needs to be available to tdis_pu)
!       - Do not call budget routines or write results
!       - Do not increment kstp
!       - Set readnewdata to .false.
!       - Reset pertim/totim back to previous values
!       - PrepareSolve should not advance routines
!       - Reset all dependent values to old values (hnew = hold)
!       - Cut down size of delt
!       - If cut down size is < tsminats, then solution should be marked as not converged
!       - 
!   - write time step history to csv file
!   - time series

module AdaptiveTimeStepModule

  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: iout
  use SimModule, only: ustop, store_error, count_errors
  use BlockParserModule, only: BlockParserType
  use ConstantsModule, only: DZERO, LINELENGTH

  implicit none
  private
  public :: ats_cr
  public :: ats_ar
  public :: ats_tu
  public :: ats_da

  integer(I4B), pointer                            :: nperats => null()    !< set equal to nper
  integer(I4B), pointer                            :: maxats => null()     !< number of ats entries
  real(DP), dimension(:), pointer, contiguous      :: deltats => null()    !< array of initial time step sizes
  real(DP), dimension(:), pointer, contiguous      :: tsminats => null()   !< array of minimum time step sizes
  real(DP), dimension(:), pointer, contiguous      :: tsmaxats => null()   !< array of maximum time step sizes
  real(DP), dimension(:), pointer, contiguous      :: tsfactats => null()  !< array of time step factors for shortening or increasing
  type(BlockParserType)                            :: parser               !< block parser for reading input file

  contains

  subroutine ats_cr(fname)
! ******************************************************************************
! ats_cr -- create ats
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: getunit, openfile
    ! -- dummy
    character(len=*), intent(in) :: fname
    ! -- local
    integer(I4B) :: inunit
    ! -- formats
    character(len=*),parameter :: fmtheader = &
        "(1X,/1X,'ATS -- ADAPTIVE TIME STEP PACKAGE,',   /                  &
        &' VERSION 1 : 02/09/2021 - INPUT READ FROM UNIT ',I0)"
! ------------------------------------------------------------------------------
    !
    ! -- Allocate the scalar variables
    call ats_allocate_scalars()
    !
    ! -- Get a unit number for ats and open the file if it is not opened
    inquire(file=fname, number=inunit)
    if(inunit < 0) then
      inunit = getunit()
      call openfile(inunit, iout, fname, 'ATS')
    endif
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
    ! -- return
    return
  end subroutine ats_cr

  subroutine ats_ar(nper, kperats)
! ******************************************************************************
! ats_ar -- allocate and read ats
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    integer(I4B), intent(in) :: nper
    integer(I4B), dimension(:), contiguous, intent(inout) :: kperats
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- store tdis nper in nperats
    nperats = nper
    !
    ! -- Read dimensions and then allocate arrays
    call ats_read_dimensions()
    call ats_allocate_arrays()
    !
    ! -- Read timing
    call ats_read_timing(kperats)
    !
    ! -- Close the file
    call parser%Clear()
    !
    ! -- return
    return
  end subroutine ats_ar

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
    call mem_allocate(nperats, 'NPERATS', 'ATS')
    call mem_allocate(maxats, 'MAXATS', 'ATS')
    !
    ! -- Initialize variables
    nperats = 0
    maxats = 0
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
    call mem_allocate(deltats, maxats, 'DELTATS', 'ATS')
    call mem_allocate(tsminats, maxats, 'TSMINATS', 'ATS')
    call mem_allocate(tsmaxats, maxats, 'TSMAXATS', 'ATS')
    call mem_allocate(tsfactats, maxats, 'TSFACTATS', 'ATS')
    !
    ! -- initialize
    do n = 1, maxats
      deltats(n) = DZERO
      tsminats(n) = DZERO
      tsmaxats(n) = DZERO
      tsfactats(n) = DZERO
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
    call mem_deallocate(nperats)
    call mem_deallocate(maxats)
    !
    ! -- Arrays
    call mem_deallocate(deltats)
    call mem_deallocate(tsminats)
    call mem_deallocate(tsmaxats)
    call mem_deallocate(tsfactats)
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
        
  subroutine ats_read_timing(kperats)
! ******************************************************************************
! ats_read_timing -- Read timing information
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:), contiguous, intent(inout) :: kperats
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
        ! -- set kperats to index value for deltats, tminats, tmaxats
        kperats(kkper) = n
        !
        ! -- fill the ats data arrays
        deltats(n) = parser%GetDouble()
        tsminats(n) = parser%GetDouble()
        tsmaxats(n) = parser%GetDouble()
        !tsfactats(n) = parser%GetDouble()
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
      
  subroutine ats_tu(endofperiod, endofsimulation, readnewdata, &
                    kstp, kper, perlen, nstp, &
                    tsmult, delt, dtstable, pertim, pertimsav, totim, totimsav, &
                    totalsimtime, totimc, &
                    kperats, latsfailed, atskeeptrying)
! ******************************************************************************
! ats_tu -- Time update
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DONE, DZERO, MNORMAL, MVALIDATE, DNODATA
    ! -- dummy
    logical(LGP), intent(inout) :: endofperiod
    logical(LGP), intent(inout) :: endofsimulation
    logical(LGP), intent(inout) :: readnewdata
    integer(I4B), intent(inout) :: kstp
    integer(I4B), intent(inout) :: kper
    integer(I4B), dimension(:), intent(inout) :: nstp
    real(DP), dimension(:), intent(inout) :: perlen
    real(DP), dimension(:), intent(inout) :: tsmult
    real(DP), intent(inout) :: delt
    real(DP), intent(inout) :: dtstable
    real(DP), intent(inout) :: pertim
    real(DP), intent(inout) :: pertimsav
    real(DP), intent(inout) :: totim
    real(DP), intent(inout) :: totimsav
    real(DP), intent(inout) :: totalsimtime
    real(DP), intent(inout) :: totimc
    integer(I4B), dimension(:), contiguous, intent(in) :: kperats
    logical(LGP), intent(in) :: latsfailed
    logical(LGP), intent(inout) :: atskeeptrying
    ! -- local
    integer(I4B) :: n
    ! -- formats
    character(len=*),parameter :: fmttsi =                                     &
      "(1X, 'ATS: INITIAL TIME STEP SIZE =', G15.7)"
    character(len=*), parameter :: fmtdelt =                                   &
      &"(1x, 'ATS: TIME STEP SIZE SET TO ', G15.7)"
! ------------------------------------------------------------------------------
    !
    ! -- initialize the record position (n) for this stress period
    n = kperats(kper)
    !
    ! -- If the last step failed, then reset
    atskeeptrying = .true.
    if (latsfailed) then
      pertim = pertimsav
      totim = totimsav
      delt = delt / 2.d0   ! chop it down (make this an input variable)
      print *, 'ats_tu redoing time step with size ', delt
      !
      ! -- If the length of the next retry would be less than tsmin, then
      !    this must be the last ats retry
      if (delt / 2.d0 < tsminats(n)) atskeeptrying = .false.
    else
      !
      ! -- Setup new stress period if kstp is 1
      if(kstp == 1) then
        !
        ! -- Assign first value of delt for this stress period
        if (deltats(n) /= DZERO) then
          delt = deltats(n)
        end if
        !
        ! -- Print length of first time step
        write(iout, fmttsi) delt
        !
        ! -- Initialize pertim (Elapsed time within stress period)
        pertim = DZERO
        !
        ! -- Clear flag that indicates last time step of a stress period
        endofperiod = .false.
        !
      endif
      !
      ! -- Assign delt based on stability
      if (kstp > 1) then
        if (dtstable /= DNODATA) then
          delt = dtstable
        end if
      end if
    end if
    !
    ! -- Restrict delt
    if (delt < tsminats(n)) then
      delt = tsminats(n)
    end if
    if (delt > tsmaxats(n)) then
      delt = tsmaxats(n)
    end if
    !
    ! -- Cut timestep down to meet end of period
    if (pertim + delt > perlen(kper)) then
      delt = perlen(kper) - pertim
    end if
    !
    ! -- Write time step length to mfsim.lst
    write(iout, fmtdelt) delt
    !
    ! -- Store totim and pertim, which are times at end of previous time step
    totimsav = totim
    pertimsav = pertim
    totimc = totim
    !
    ! -- Update totim and pertim
    totim = totimsav + delt
    pertim = pertimsav + delt
    !
    ! -- End of stress period and/or simulation?  THIS NEEDS TO BE FIXED
    if (abs(pertim - perlen(kper)) < 1.d-5) then
      endofperiod = .true.
    else
      endofperiod = .false.
    end if
    if (endofperiod .and. kper==nperats) then
      endofsimulation = .true.
      totim = totalsimtime  
    end if
    !
    ! -- return
    return
  end subroutine ats_tu
    
        

end module AdaptiveTimeStepModule
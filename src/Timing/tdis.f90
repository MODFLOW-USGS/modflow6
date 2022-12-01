!stress periods and time stepping is handled by these routines
!convert this to a derived type?  May not be necessary since only
!one of them is needed.

module TdisModule

  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: iout
  use BlockParserModule, only: BlockParserType
  use ConstantsModule, only: LINELENGTH, LENDATETIME, VALL
  !
  implicit none
  !
  private
  public :: tdis_cr
  public :: tdis_set_counters
  public :: tdis_set_timestep
  public :: tdis_delt_reset
  public :: tdis_ot
  public :: tdis_da
  !
  integer(I4B), public, pointer :: nper => null() !< number of stress period
  integer(I4B), public, pointer :: itmuni => null() !< flag indicating time units
  integer(I4B), public, pointer :: kper => null() !< current stress period number
  integer(I4B), public, pointer :: kstp => null() !< current time step number
  integer(I4B), public, pointer :: inats => null() !< flag indicating ats active for simulation
  logical(LGP), public, pointer :: readnewdata => null() !< flag indicating time to read new data
  logical(LGP), public, pointer :: endofperiod => null() !< flag indicating end of stress period
  logical(LGP), public, pointer :: endofsimulation => null() !< flag indicating end of simulation
  real(DP), public, pointer :: delt => null() !< length of the current time step
  real(DP), public, pointer :: pertim => null() !< time relative to start of stress period
  real(DP), public, pointer :: totim => null() !< time relative to start of simulation
  real(DP), public, pointer :: totimc => null() !< simulation time at start of time step
  real(DP), public, pointer :: deltsav => null() !< saved value for delt, used for subtiming
  real(DP), public, pointer :: totimsav => null() !< saved value for totim, used for subtiming
  real(DP), public, pointer :: pertimsav => null() !< saved value for pertim, used for subtiming
  real(DP), public, pointer :: totalsimtime => null() !< time at end of simulation
  real(DP), public, dimension(:), pointer, contiguous :: perlen => null() !< length of each stress period
  integer(I4B), public, dimension(:), pointer, contiguous :: nstp => null() !< number of time steps in each stress period
  real(DP), public, dimension(:), pointer, contiguous :: tsmult => null() !< time step multiplier for each stress period
  character(len=LENDATETIME), pointer :: datetime0 => null() !< starting date and time for the simulation
  !
  type(BlockParserType), private :: parser

contains

  subroutine tdis_cr(fname)
! ******************************************************************************
! tdis_cr -- create temporal discretization.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: getunit, openfile
    use ConstantsModule, only: LINELENGTH, DZERO
    use AdaptiveTimeStepModule, only: ats_cr
    ! -- dummy
    character(len=*), intent(in) :: fname
    ! -- local
    integer(I4B) :: inunit
    ! -- formats
    character(len=*), parameter :: fmtheader = &
     "(1X,/1X,'TDIS -- TEMPORAL DISCRETIZATION PACKAGE,',   /                  &
      &' VERSION 1 : 11/13/2014 - INPUT READ FROM UNIT ',I4)"
! ------------------------------------------------------------------------------
    !
    ! -- Allocate the scalar variables
    call tdis_allocate_scalars()
    !
    ! -- Get a unit number for tdis and open the file if it is not opened
    inquire (file=fname, number=inunit)
    if (inunit < 0) then
      inunit = getunit()
      call openfile(inunit, iout, fname, 'TDIS')
    end if
    !
    ! -- Identify package
    write (iout, fmtheader) inunit
    !
    ! -- Initialize block parser
    call parser%Initialize(inunit, iout)
    !
    ! -- Read options
    call tdis_read_options()
    !
    ! -- Read dimensions and then allocate arrays
    call tdis_read_dimensions()
    call tdis_allocate_arrays()
    !
    ! -- Read timing
    call tdis_read_timing()
    !
    ! -- Close the file
    call parser%Clear()
    !
    if (inats > 0) then
      call ats_cr(inats, nper)
    end if
    !
    ! -- return
    return
  end subroutine tdis_cr

  subroutine tdis_set_counters()
! ******************************************************************************
! tdis_set_counters -- Set kstp and kper
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DONE, DZERO, MNORMAL, MVALIDATE, DNODATA
    use SimVariablesModule, only: isim_mode
    use GenericUtilitiesModule, only: sim_message
    use AdaptiveTimeStepModule, only: isAdaptivePeriod, dtstable, &
                                      ats_period_message
    ! -- local
    character(len=LINELENGTH) :: line
    character(len=4) :: cpref
    character(len=10) :: cend
    ! -- formats
    character(len=*), parameter :: fmtspts = &
      &"(a, 'Solving:  Stress period: ',i5,4x, 'Time step: ',i5,4x, a)"
    character(len=*), parameter :: fmtvspts = &
      &"(' Validating:  Stress period: ',i5,4x,'Time step: ',i5,4x)"
    character(len=*), parameter :: fmtspi = &
      "('1',/28X,'STRESS PERIOD NO. ',I0,', LENGTH =',G15.7,/ &
      &28X,47('-'))"
    character(len=*), parameter :: fmtspits = &
      "(28X,'NUMBER OF TIME STEPS = ',I0,/ &
      &28X,'MULTIPLIER FOR DELT =',F10.3)"
! ------------------------------------------------------------------------------
    !
    ! -- Initialize variables for this step
    if (inats > 0) dtstable = DNODATA
    readnewdata = .false.
    cpref = '    '
    cend = ''
    !
    ! -- Increment kstp and kper
    if (endofperiod) then
      kstp = 1
      kper = kper + 1
      readnewdata = .true.
    else
      kstp = kstp + 1
    end if
    !
    ! -- Print stress period and time step to console
    select case (isim_mode)
    case (MVALIDATE)
      write (line, fmtvspts) kper, kstp
    case (MNORMAL)
      write (line, fmtspts) cpref, kper, kstp, trim(cend)
    end select
    call sim_message(line, level=VALL)
    call sim_message(line, iunit=iout, skipbefore=1, skipafter=1)
    !
    ! -- Write message if first time step
    if (kstp == 1) then
      write (iout, fmtspi) kper, perlen(kper)
      if (isAdaptivePeriod(kper)) then
        call ats_period_message(kper)
      else
        write (iout, fmtspits) nstp(kper), tsmult(kper)
      end if
    end if
    !
    ! -- return
    return
  end subroutine tdis_set_counters

  subroutine tdis_set_timestep()
! ******************************************************************************
! tdis_set_timestep -- Set time step length
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DONE, DZERO
    use AdaptiveTimeStepModule, only: isAdaptivePeriod, &
                                      ats_set_delt, &
                                      ats_set_endofperiod
    ! -- local
    logical(LGP) :: adaptivePeriod
    ! -- format
    character(len=*), parameter :: fmttsi = &
                                   "(28X,'INITIAL TIME STEP SIZE =',G15.7)"
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    adaptivePeriod = isAdaptivePeriod(kper)
    if (kstp == 1) then
      pertim = DZERO
    end if
    !
    ! -- Set delt
    if (adaptivePeriod) then
      call ats_set_delt(kstp, kper, pertim, perlen(kper), delt)
    else
      call tdis_set_delt()
      if (kstp == 1) then
        write (iout, fmttsi) delt
      end if
    end if
    !
    ! -- Advance timers and update totim and pertim based on delt
    totimsav = totim
    pertimsav = pertim
    totimc = totimsav
    totim = totimsav + delt
    pertim = pertimsav + delt
    !
    ! -- Set end of period indicator
    endofperiod = .false.
    if (adaptivePeriod) then
      call ats_set_endofperiod(kper, pertim, perlen(kper), endofperiod)
    else
      if (kstp == nstp(kper)) then
        endofperiod = .true.
      end if
    end if
    if (endofperiod) then
      pertim = perlen(kper)
    end if
    !
    ! -- Set end of simulation indicator
    if (endofperiod .and. kper == nper) then
      endofsimulation = .true.
      totim = totalsimtime
    end if
    !
    ! -- return
    return
  end subroutine tdis_set_timestep

  subroutine tdis_delt_reset(deltnew)
! ******************************************************************************
! tdis_delt_reset -- reset delt and update timing variables and indicators.
!   This routine is called when a timestep fails to converge, and so it is
!   retried using a smaller time step (deltnew).
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DONE, DZERO
    use AdaptiveTimeStepModule, only: isAdaptivePeriod, &
                                      ats_set_delt, &
                                      ats_set_endofperiod
    ! -- dummy
    real(DP), intent(in) :: deltnew
    ! -- local
    logical(LGP) :: adaptivePeriod
! ------------------------------------------------------------------------------
    !
    ! -- Set values
    adaptivePeriod = isAdaptivePeriod(kper)
    delt = deltnew
    totim = totimsav + delt
    pertim = pertimsav + delt
    !
    ! -- Set end of period indicator
    endofperiod = .false.
    if (adaptivePeriod) then
      call ats_set_endofperiod(kper, pertim, perlen(kper), endofperiod)
    else
      if (kstp == nstp(kper)) then
        endofperiod = .true.
      end if
    end if
    !
    ! -- Set end of simulation indicator
    if (endofperiod .and. kper == nper) then
      endofsimulation = .true.
      totim = totalsimtime
    end if
    !
    ! -- return
    return
  end subroutine tdis_delt_reset

  subroutine tdis_set_delt()
! ******************************************************************************
! tdis_set_delt -- Set time step length
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DONE
    ! -- local
! ------------------------------------------------------------------------------
    !
    if (kstp == 1) then
      delt = perlen(kper) / float(nstp(kper))
      if (tsmult(kper) /= DONE) &
        delt = perlen(kper) * (DONE - tsmult(kper)) / &
               (DONE - tsmult(kper)**nstp(kper))
    else
      delt = tsmult(kper) * delt
    end if
    !
    ! -- return
    return
  end subroutine tdis_set_delt

!  subroutine tdis_set_delt_std()
!! ******************************************************************************
!! tdis_tu_std -- Standard non-adaptive time update
!! ******************************************************************************
!!
!!    SPECIFICATIONS:
!! ------------------------------------------------------------------------------
!    ! -- modules
!    use ConstantsModule, only: DONE, DZERO
!    ! -- formats
!    character(len=*),parameter :: fmttsi = &
!      "(28X,'INITIAL TIME STEP SIZE =',G15.7)"
!! ------------------------------------------------------------------------------
!    !
!    ! -- Setup new stress period if kstp is 1
!    if(kstp == 1) then
!      !
!      ! -- Calculate the first value of delt for this stress period
!      delt = perlen(kper) / float(nstp(kper))
!      if(tsmult(kper) /= DONE) &
!          delt = perlen(kper) * (DONE-tsmult(kper)) / &
!              (DONE - tsmult(kper) ** nstp(kper))
!      !
!      ! -- Print length of first time step
!      write(iout, fmttsi) delt
!      !
!      ! -- Initialize pertim (Elapsed time within stress period)
!      pertim = DZERO
!      !
!      ! -- Clear flag that indicates last time step of a stress period
!      endofperiod = .false.
!    endif
!    !
!    ! -- Calculate delt for kstp > 1
!    if (kstp /= 1) then
!      delt = tsmult(kper) * delt
!    end if
!    !
!    ! -- Store totim and pertim, which are times at end of previous time step
!    totimsav = totim
!    pertimsav = pertim
!    totimc = totim
!    !
!    ! -- Update totim and pertim
!    totim = totimsav + delt
!    pertim = pertimsav + delt
!    !
!    ! -- End of stress period and/or simulation?
!    if (kstp == nstp(kper)) then
!      endofperiod = .true.
!    end if
!    if (endofperiod .and. kper==nper) then
!      endofsimulation = .true.
!      totim = totalsimtime
!    end if
!    !
!    ! -- return
!    return
!  end subroutine tdis_set_delt_std

  subroutine tdis_ot(iout)
! ******************************************************************************
!     PRINT SIMULATION TIME
! ******************************************************************************
!
!        SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), intent(in) :: iout
    ! -- local
    real(DP) :: zero, cnv, delsec, totsec, persec, sixty, hrday, dayyr, &
                delmn, delhr, totmn, tothr, totdy, totyr, permn, perhr, &
                perdy, peryr, deldy, delyr
! ------------------------------------------------------------------------------
    WRITE (IOUT, 199) KSTP, KPER
199 FORMAT(1X, ///9X, 'TIME SUMMARY AT END OF TIME STEP', I5, &
     &     ' IN STRESS PERIOD ', I4)
!C
!C1------USE TIME UNIT INDICATOR TO GET FACTOR TO CONVERT TO SECONDS.
    ZERO = 0.d0
    CNV = ZERO
    IF (ITMUNI .EQ. 1) CNV = 1.
    IF (ITMUNI .EQ. 2) CNV = 60.
    IF (ITMUNI .EQ. 3) CNV = 3600.
    IF (ITMUNI .EQ. 4) CNV = 86400.
    IF (ITMUNI .EQ. 5) CNV = 31557600.
!C
!C2------IF FACTOR=0 THEN TIME UNITS ARE NON-STANDARD.
    IF (CNV .NE. ZERO) GO TO 100
!C
!C2A-----PRINT TIMES IN NON-STANDARD TIME UNITS.
    WRITE (IOUT, 301) DELT, PERTIM, TOTIM
301 FORMAT(21X, '     TIME STEP LENGTH =', G15.6 / &
     &       21X, '   STRESS PERIOD TIME =', G15.6 / &
     &       21X, 'TOTAL SIMULATION TIME =', G15.6)
!C
!C2B-----RETURN
    RETURN
!C
!C3------CALCULATE LENGTH OF TIME STEP & ELAPSED TIMES IN SECONDS.
100 DELSEC = CNV * DELT
    TOTSEC = CNV * TOTIM
    PERSEC = CNV * PERTIM
!C
!C4------CALCULATE TIMES IN MINUTES,HOURS,DAYS AND YEARS.
    SIXTY = 60.
    HRDAY = 24.
    DAYYR = 365.25
    DELMN = DELSEC / SIXTY
    DELHR = DELMN / SIXTY
    DELDY = DELHR / HRDAY
    DELYR = DELDY / DAYYR
    TOTMN = TOTSEC / SIXTY
    TOTHR = TOTMN / SIXTY
    TOTDY = TOTHR / HRDAY
    TOTYR = TOTDY / DAYYR
    PERMN = PERSEC / SIXTY
    PERHR = PERMN / SIXTY
    PERDY = PERHR / HRDAY
    PERYR = PERDY / DAYYR
!C
!C5------PRINT TIME STEP LENGTH AND ELAPSED TIMES IN ALL TIME UNITS.
    WRITE (IOUT, 200)
200 FORMAT(19X, ' SECONDS     MINUTES      HOURS', 7X, &
     &    'DAYS        YEARS'/20X, 59('-'))
    write (IOUT, 201) DELSEC, DELMN, DELHR, DELDY, DELYR
201 FORMAT(1X, '  TIME STEP LENGTH', 1P, 5G12.5)
    WRITE (IOUT, 202) PERSEC, PERMN, PERHR, PERDY, PERYR
202 FORMAT(1X, 'STRESS PERIOD TIME', 1P, 5G12.5)
    WRITE (IOUT, 203) TOTSEC, TOTMN, TOTHR, TOTDY, TOTYR
203 FORMAT(1X, '        TOTAL TIME', 1P, 5G12.5,/)
!C
!C6------RETURN
    RETURN
  END subroutine tdis_ot

  subroutine tdis_da()
! ******************************************************************************
! tdis_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_deallocate
    use AdaptiveTimeStepModule, only: ats_da
! ------------------------------------------------------------------------------
    ! -- ats
    if (inats > 0) call ats_da()
    !
    ! -- Scalars
    call mem_deallocate(nper)
    call mem_deallocate(itmuni)
    call mem_deallocate(kper)
    call mem_deallocate(kstp)
    call mem_deallocate(inats)
    call mem_deallocate(readnewdata)
    call mem_deallocate(endofperiod)
    call mem_deallocate(endofsimulation)
    call mem_deallocate(delt)
    call mem_deallocate(pertim)
    call mem_deallocate(totim)
    call mem_deallocate(totimc)
    call mem_deallocate(deltsav)
    call mem_deallocate(totimsav)
    call mem_deallocate(pertimsav)
    call mem_deallocate(totalsimtime)
    !
    ! -- strings
    deallocate (datetime0)
    !
    ! -- Arrays
    call mem_deallocate(perlen)
    call mem_deallocate(nstp)
    call mem_deallocate(tsmult)
    !
    ! -- Return
    return
  end subroutine tdis_da

  subroutine tdis_read_options()
! ******************************************************************************
! tdis_read_options -- Read the options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    use InputOutputModule, only: GetUnit, openfile
    ! -- dummy
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword, fname
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    logical :: undspec
    ! -- formats
    character(len=*), parameter :: fmtitmuni = &
      &"(4x,'SIMULATION TIME UNIT IS ',A)"
    character(len=*), parameter :: fmtdatetime0 = &
      &"(4x,'SIMULATION STARTING DATE AND TIME IS ',A)"
    !data
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    itmuni = 0
    undspec = .false.
    !
    ! -- get options block
    call parser%GetBlock('OPTIONS', isfound, ierr, &
                         supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (iout, '(1x,a)') 'PROCESSING TDIS OPTIONS'
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
        case ('TIME_UNITS')
          call parser%GetStringCaps(keyword)
          select case (keyword)
          case ('UNDEFINED')
            itmuni = 0
            write (iout, fmtitmuni) 'UNDEFINED'
            undspec = .true.
          case ('SECONDS')
            itmuni = 1
            write (iout, fmtitmuni) 'SECONDS'
          case ('MINUTES')
            itmuni = 2
            write (iout, fmtitmuni) 'MINUTES'
          case ('HOURS')
            itmuni = 3
            write (iout, fmtitmuni) 'HOURS'
          case ('DAYS')
            itmuni = 4
            write (iout, fmtitmuni) 'DAYS'
          case ('YEARS')
            itmuni = 5
            write (iout, fmtitmuni) 'YEARS'
          case default
            write (errmsg, '(4x,a,a)') '****ERROR. UNKNOWN TIME_UNITS: ', &
              trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
          end select
        case ('START_DATE_TIME')
          call parser%GetString(datetime0)
          write (iout, fmtdatetime0) datetime0
        case ('ATS6')
          call parser%GetStringCaps(keyword)
          if (trim(adjustl(keyword)) /= 'FILEIN') then
            errmsg = 'ATS6 keyword must be followed by "FILEIN" '// &
                     'then by filename.'
            call store_error(errmsg)
          end if
          call parser%GetString(fname)
          inats = GetUnit()
          call openfile(inats, iout, fname, 'ATS')
        case default
          write (errmsg, '(4x,a,a)') '****ERROR. UNKNOWN TDIS OPTION: ', &
            trim(keyword)
          call store_error(errmsg)
          call parser%StoreErrorUnit()
        end select
      end do
      write (iout, '(1x,a)') 'END OF TDIS OPTIONS'
    end if
    !
    ! -- Set to itmuni to undefined if not specified
    if (itmuni == 0) then
      if (.not. undspec) then
        write (iout, fmtitmuni) 'UNDEFINED'
      end if
    end if
    !
    ! -- Return
    return
  end subroutine tdis_read_options

  subroutine tdis_allocate_scalars()
! ******************************************************************************
! tdis_read_dimensions -- Read dimension NPER
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
! ------------------------------------------------------------------------------
    !
    ! -- memory manager variables
    call mem_allocate(nper, 'NPER', 'TDIS')
    call mem_allocate(itmuni, 'ITMUNI', 'TDIS')
    call mem_allocate(kper, 'KPER', 'TDIS')
    call mem_allocate(kstp, 'KSTP', 'TDIS')
    call mem_allocate(inats, 'INATS', 'TDIS')
    call mem_allocate(readnewdata, 'READNEWDATA', 'TDIS')
    call mem_allocate(endofperiod, 'ENDOFPERIOD', 'TDIS')
    call mem_allocate(endofsimulation, 'ENDOFSIMULATION', 'TDIS')
    call mem_allocate(delt, 'DELT', 'TDIS')
    call mem_allocate(pertim, 'PERTIM', 'TDIS')
    call mem_allocate(totim, 'TOTIM', 'TDIS')
    call mem_allocate(totimc, 'TOTIMC', 'TDIS')
    call mem_allocate(deltsav, 'DELTSAV', 'TDIS')
    call mem_allocate(totimsav, 'TOTIMSAV', 'TDIS')
    call mem_allocate(pertimsav, 'PERTIMSAV', 'TDIS')
    call mem_allocate(totalsimtime, 'TOTALSIMTIME', 'TDIS')
    !
    ! -- strings
    allocate (datetime0)
    !
    ! -- Initialize variables
    nper = 0
    itmuni = 0
    kper = 0
    kstp = 0
    inats = 0
    readnewdata = .true.
    endofperiod = .true.
    endofsimulation = .false.
    delt = DZERO
    pertim = DZERO
    totim = DZERO
    totimc = DZERO
    deltsav = DZERO
    totimsav = DZERO
    pertimsav = DZERO
    totalsimtime = DZERO
    datetime0 = ''
    !
    ! -- return
    return
  end subroutine tdis_allocate_scalars

  subroutine tdis_allocate_arrays()
! ******************************************************************************
! tdis_allocate_arrays -- Allocate tdis arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
! ------------------------------------------------------------------------------
    !
    call mem_allocate(perlen, nper, 'PERLEN', 'TDIS')
    call mem_allocate(nstp, nper, 'NSTP', 'TDIS')
    call mem_allocate(tsmult, nper, 'TSMULT', 'TDIS')
    !
    ! -- return
    return
  end subroutine tdis_allocate_arrays

  subroutine tdis_read_dimensions()
! ******************************************************************************
! tdis_read_dimensions -- Read dimension NPER
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    ! -- dummy
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtnper = &
                                   "(1X,I4,' STRESS PERIOD(S) IN SIMULATION')"
    !data
! ------------------------------------------------------------------------------
    !
    ! -- get DIMENSIONS block
    call parser%GetBlock('DIMENSIONS', isfound, ierr, &
                         supportOpenClose=.true.)
    !
    ! -- parse block if detected
    if (isfound) then
      write (iout, '(1x,a)') 'PROCESSING TDIS DIMENSIONS'
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
        case ('NPER')
          nper = parser%GetInteger()
          write (iout, fmtnper) nper
        case default
          write (errmsg, '(4x,a,a)') '****ERROR. UNKNOWN TDIS DIMENSION: ', &
            trim(keyword)
          call store_error(errmsg)
          call parser%StoreErrorUnit()
        end select
      end do
      write (iout, '(1x,a)') 'END OF TDIS DIMENSIONS'
    else
      write (errmsg, '(1x,a)') 'ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.'
      call store_error(errmsg)
      call parser%StoreErrorUnit()
    end if
    !
    ! -- Return
    return
  end subroutine tdis_read_dimensions

  subroutine tdis_read_timing()
! ******************************************************************************
! tdis_read_timing -- Read timing information
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH, DZERO
    use SimModule, only: store_error, count_errors
    ! -- dummy
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: ierr
    integer(I4B) :: n
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtheader = &
      "(1X,//1X,'STRESS PERIOD     LENGTH       TIME STEPS', &
       &'     MULTIPLIER FOR DELT',/1X,76('-'))"
    character(len=*), parameter :: fmtrow = &
                                   "(1X,I8,1PG21.7,I7,0PF25.3)"
! ------------------------------------------------------------------------------
    !
    ! -- get PERIODDATA block
    call parser%GetBlock('PERIODDATA', isfound, ierr, &
                         supportOpenClose=.true.)
    !
    ! -- parse block if detected
    if (isfound) then
      write (iout, '(1x,a)') 'PROCESSING TDIS PERIODDATA'
      write (iout, fmtheader)
      do n = 1, nper
        call parser%GetNextLine(endOfBlock)
        perlen(n) = parser%GetDouble()
        nstp(n) = parser%GetInteger()
        tsmult(n) = parser%GetDouble()
        write (iout, fmtrow) n, perlen(n), nstp(n), tsmult(n)
        totalsimtime = totalsimtime + perlen(n)
      end do
      !
      ! -- Check timing information
      call check_tdis_timing(nper, perlen, nstp, tsmult)
      call parser%terminateblock()
      !
      ! -- Check for errors
      if (count_errors() > 0) then
        call parser%StoreErrorUnit()
      end if
      write (iout, '(1x,a)') 'END OF TDIS PERIODDATA'
    else
      write (errmsg, '(1x,a)') 'ERROR.  REQUIRED PERIODDATA BLOCK NOT FOUND.'
      call store_error(errmsg)
      call parser%StoreErrorUnit()
    end if
    !
    ! -- Return
    return
  end subroutine tdis_read_timing

  subroutine check_tdis_timing(nper, perlen, nstp, tsmult)
! ******************************************************************************
! check_tdis_timing -- Check the tdis timing information.  Return back to
!   tdis_read_timing if an error condition is found and let the ustop
!   routine be called there instead so the StoreErrorUnit routine can be
!   called to assign the correct file name.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH, DZERO, DONE
    use SimModule, only: store_error, count_errors
    ! -- dummy
    integer(I4B), intent(in) :: nper
    real(DP), dimension(:), contiguous, intent(in) :: perlen
    integer(I4B), dimension(:), contiguous, intent(in) :: nstp
    real(DP), dimension(:), contiguous, intent(in) :: tsmult
    ! -- local
    integer(I4B) :: kper, kstp
    real(DP) :: tstart, tend, dt
    character(len=LINELENGTH) :: errmsg
    ! -- formats
    character(len=*), parameter :: fmtpwarn = &
      "(1X,/1X,'PERLEN IS ZERO FOR STRESS PERIOD ', I0, &
      &'. PERLEN MUST NOT BE ZERO FOR TRANSIENT PERIODS.')"
    character(len=*), parameter :: fmtsperror = &
      &"(A,' FOR STRESS PERIOD ', I0)"
    character(len=*), parameter :: fmtdterror = &
      "('TIME STEP LENGTH OF ', G0, ' IS TOO SMALL IN PERIOD ', I0, &
      &' AND TIME STEP ', I0)"
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    tstart = DZERO
    !
    ! -- Go through and check each stress period
    do kper = 1, nper
      !
      ! -- Error if nstp less than or equal to zero
      if (nstp(kper) <= 0) then
        write (errmsg, fmtsperror) 'NUMBER OF TIME STEPS LESS THAN ONE ', kper
        call store_error(errmsg)
        return
      end if
      !
      ! -- Warn if perlen is zero
      if (perlen(kper) == DZERO) then
        write (iout, fmtpwarn) kper
        return
      end if
      !
      ! -- Error if tsmult is less than zero
      if (tsmult(kper) <= DZERO) then
        write (errmsg, fmtsperror) 'TSMULT MUST BE GREATER THAN 0.0 ', kper
        call store_error(errmsg)
        return
      end if
      !
      ! -- Error if negative period length
      if (perlen(kper) < DZERO) then
        write (errmsg, fmtsperror) 'PERLEN CANNOT BE LESS THAN 0.0 ', kper
        call store_error(errmsg)
        return
      end if
      !
      ! -- Go through all time step lengths and make sure they are valid
      do kstp = 1, nstp(kper)
        if (kstp == 1) then
          dt = perlen(kper) / float(nstp(kper))
          if (tsmult(kper) /= DONE) &
            dt = perlen(kper) * (DONE - tsmult(kper)) / &
                 (DONE - tsmult(kper)**nstp(kper))
        else
          dt = dt * tsmult(kper)
        end if
        tend = tstart + dt
        !
        ! -- Error condition if tstart == tend
        if (tstart == tend) then
          write (errmsg, fmtdterror) dt, kper, kstp
          call store_error(errmsg)
          return
        end if
      end do
      !
      ! -- reset tstart = tend
      tstart = tend
      !
    end do
    ! -- Return
    return
  end subroutine check_tdis_timing

end module TdisModule


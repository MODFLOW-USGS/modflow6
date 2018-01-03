!stress periods and time stepping is handled by these routines
!convert this to a derived type?  May not be necessary since only
!one of them is needed.

  module TdisModule

  use KindModule, only: DP, I4B
  use SimVariablesModule, only: iout
  use BlockParserModule, only: BlockParserType
  use ConstantsModule, only: LENDATETIME
  !
  implicit none
  !
  private
  public :: subtiming_begin
  public :: subtiming_end
  public :: tdis_cr
  public :: tdis_tu
  public :: tdis_ot
  public :: tdis_da
  !
  integer(I4B),          public, pointer       :: nper                          !number of stress period
  integer(I4B),          public, pointer       :: itmuni                        !flag indicating time units
  integer(I4B),          public, pointer       :: kper                          !current stress period number
  integer(I4B),          public, pointer       :: kstp                          !current time step number
  logical,          public, pointer            :: readnewdata                   !flag indicating time to read new data
  logical,          public, pointer            :: endofperiod             !flag indicating end of stress period
  logical,          public, pointer            :: endofsimulation               !flag indicating end of simulation
  real(DP), public, pointer                    :: delt                          !length of the current time step
  real(DP), public, pointer                    :: pertim                        !time relative to start of stress period
  real(DP), public, pointer                    :: totim                         !time relative to start of simulation
  real(DP), public, pointer                    :: totimc                        !simulation time at start of time step
  real(DP), public, pointer                    :: deltsav                       !saved value for delt, used for subtiming
  real(DP), public, pointer                    :: totimsav                      !saved value for totim, used for subtiming
  real(DP), public, pointer                    :: pertimsav                     !saved value for pertim, used for subtiming
  real(DP), public, pointer                    :: totalsimtime                  !time at end of simulation
  real(DP), public, dimension(:), pointer      :: perlen                        !length of each stress period
  integer(I4B), public, dimension(:), pointer  :: nstp                          !number of time steps in each stress period
  real(DP), public, dimension(:), pointer      :: tsmult                        !time step multiplier for each stress period
  character(len=LENDATETIME), pointer          :: datetime0                     !starting date and time for the simulation
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
    ! -- dummy
    character(len=*),intent(in) :: fname
    ! -- local
    integer(I4B) :: inunit
    ! -- formats
    character(len=*),parameter :: fmtheader = &
     "(1X,/1X,'TDIS -- TEMPORAL DISCRETIZATION PACKAGE,',   /                  &
      ' VERSION 1 : 11/13/2014 - INPUT READ FROM UNIT ',I4)"
! ------------------------------------------------------------------------------
    !
    ! -- Allocate the scalar variables
    call tdis_allocate_scalars()
    !
    ! -- Get a unit number for tdis and open the file if it is not opened
    inquire(file=fname, number=inunit)
    if(inunit < 0) then
      inunit = getunit()
      call openfile(inunit, iout, fname, 'TDIS')
    endif
    !
    ! -- Identify package
    write(iout, fmtheader) inunit
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
    ! -- return
    return
  end subroutine tdis_cr

  subroutine tdis_tu()
! ******************************************************************************
! tdis_tu -- Time Update.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DONE, DZERO, ISTDOUT
    ! -- local
    ! -- formats
    character(len=*),parameter :: fmtspi =                                     &
      "('1',/28X,'STRESS PERIOD NO. ',I4,', LENGTH =',G15.7,/                  &
       28X,47('-'),//                                                          &
       30X,'NUMBER OF TIME STEPS =',I6,//                                      &
       31X,'MULTIPLIER FOR DELT =',F10.3)"
    character(len=*),parameter :: fmttsi =                                     &
      "(1X,/28X,'INITIAL TIME STEP SIZE =',G15.7)"
    character(len=*),parameter :: fmtspts =                                    &
      "(' Solving:  Stress period: ',i5,4x,'Time step: ',i5,4x)"
! ------------------------------------------------------------------------------
    !
    ! -- Increment kstp and kper
    if(endofperiod) then
      kstp = 1
      kper = kper + 1
    else
      kstp = kstp + 1
    endif
    !
    ! -- Set readnewdata to .false. and change if new stress period
    readnewdata = .false.
    !
    ! -- Setup new stress period if kstp is 1
    if(kstp == 1) then
      !
      ! -- Write stress period information to simulation list file
      write(iout,fmtspi) kper, perlen(kper), nstp(kper), tsmult(kper)
      !
      ! -- Calculate the first value of delt for this stress period
      delt = perlen(kper) / float(nstp(kper))
      if(tsmult(kper) /= DONE)                                                 &
          delt = perlen(kper) * (DONE-tsmult(kper)) /                          &
              (DONE - tsmult(kper) ** nstp(kper))
      !
      ! -- Print length of first time step
       write (iout, fmttsi) delt
      !
      ! -- Initialize pertim (Elapsed time within stress period)
      pertim = DZERO
      !
      ! -- Clear flag that indicates last time step of a stress period
      endofperiod = .false.
      !
      ! -- Read new data
      readnewdata = .true.
      !
    endif
    !
    ! -- Calculate delt for kstp > 1
    if(kstp /= 1) delt = tsmult(kper) * delt
    !
    ! -- Print stress period and time step to console
    write(ISTDOUT, fmtspts) kper, kstp
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
    ! -- End of stress period and/or simulation?
    if(kstp == nstp(kper)) endofperiod = .true.
    if(endofperiod .and. kper==nper) endofsimulation = .true.
    !
    ! -- return
    return
  end subroutine tdis_tu

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
  real(DP) :: zero,cnv,delsec,totsec,persec,sixty,hrday,dayyr,         &
                      delmn,delhr,totmn,tothr,totdy,totyr,permn,perhr,perdy,   &
                      peryr,deldy,delyr
! ------------------------------------------------------------------------------
      WRITE(IOUT,199) KSTP,KPER
  199 FORMAT(1X,///9X,'TIME SUMMARY AT END OF TIME STEP',I5, &
     &     ' IN STRESS PERIOD ',I4)
!C
!C1------USE TIME UNIT INDICATOR TO GET FACTOR TO CONVERT TO SECONDS.
      ZERO=0.d0
      CNV=ZERO
      IF(ITMUNI.EQ.1) CNV=1.
      IF(ITMUNI.EQ.2) CNV=60.
      IF(ITMUNI.EQ.3) CNV=3600.
      IF(ITMUNI.EQ.4) CNV=86400.
      IF(ITMUNI.EQ.5) CNV=31557600.
!C
!C2------IF FACTOR=0 THEN TIME UNITS ARE NON-STANDARD.
      IF(CNV.NE.ZERO) GO TO 100
!C
!C2A-----PRINT TIMES IN NON-STANDARD TIME UNITS.
      WRITE(IOUT,301) DELT,PERTIM,TOTIM
  301 FORMAT(21X,'     TIME STEP LENGTH =',G15.6/ &
     &       21X,'   STRESS PERIOD TIME =',G15.6/ &
     &       21X,'TOTAL SIMULATION TIME =',G15.6)
!C
!C2B-----RETURN
      RETURN
!C
!C3------CALCULATE LENGTH OF TIME STEP & ELAPSED TIMES IN SECONDS.
  100 DELSEC=CNV*DELT
      TOTSEC=CNV*TOTIM
      PERSEC=CNV*PERTIM
!C
!C4------CALCULATE TIMES IN MINUTES,HOURS,DAYS AND YEARS.
      SIXTY=60.
      HRDAY=24.
      DAYYR=365.25
      DELMN=DELSEC/SIXTY
      DELHR=DELMN/SIXTY
      DELDY=DELHR/HRDAY
      DELYR=DELDY/DAYYR
      TOTMN=TOTSEC/SIXTY
      TOTHR=TOTMN/SIXTY
      TOTDY=TOTHR/HRDAY
      TOTYR=TOTDY/DAYYR
      PERMN=PERSEC/SIXTY
      PERHR=PERMN/SIXTY
      PERDY=PERHR/HRDAY
      PERYR=PERDY/DAYYR
!C
!C5------PRINT TIME STEP LENGTH AND ELAPSED TIMES IN ALL TIME UNITS.
      WRITE(IOUT,200)
  200 FORMAT(19X,' SECONDS     MINUTES      HOURS',7X, &
     &    'DAYS        YEARS'/20X,59('-'))
      WRITE (IOUT,201) DELSEC,DELMN,DELHR,DELDY,DELYR
  201 FORMAT(1X,'  TIME STEP LENGTH',1P,5G12.5)
      WRITE(IOUT,202) PERSEC,PERMN,PERHR,PERDY,PERYR
  202 FORMAT(1X,'STRESS PERIOD TIME',1P,5G12.5)
      WRITE(IOUT,203) TOTSEC,TOTMN,TOTHR,TOTDY,TOTYR
  203 FORMAT(1X,'        TOTAL TIME',1P,5G12.5)
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
! ------------------------------------------------------------------------------
    !
    ! -- Scalars
    call mem_deallocate(nper)
    call mem_deallocate(itmuni)
    call mem_deallocate(kper)
    call mem_deallocate(kstp)
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
    deallocate(datetime0)
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
    use SimModule, only: ustop, store_error
    ! -- dummy
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    logical :: undspec
    ! -- formats
    character(len=*), parameter :: fmtitmuni = &
      "(4x,'SIMULATION TIME UNIT IS ',A)"
    character(len=*), parameter :: fmtdatetime0 = &
      "(4x,'SIMULATION STARTING DATE AND TIME IS ',A)"
    !data
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    itmuni = 0
    undspec = .false.
    !
    ! -- get options block
    call parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(iout,'(1x,a)')'PROCESSING TDIS OPTIONS'
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
        case ('TIME_UNITS')
          call parser%GetStringCaps(keyword)
          select case (keyword)
          case('UNDEFINED')
            itmuni = 0
            write(iout, fmtitmuni) 'UNDEFINED'
            undspec = .true.
          case('SECONDS')
            itmuni = 1
            write(iout, fmtitmuni) 'SECONDS'
          case('MINUTES')
            itmuni = 2
            write(iout, fmtitmuni) 'MINUTES'
          case('HOURS')
            itmuni = 3
            write(iout, fmtitmuni) 'HOURS'
          case('DAYS')
            itmuni = 4
            write(iout, fmtitmuni) 'DAYS'
          case('YEARS')
            itmuni = 5
            write(iout, fmtitmuni) 'YEARS'
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN TIME_UNITS: ',       &
                                      trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
            call ustop()
          end select
        case ('START_DATE_TIME')
          call parser%GetString(datetime0)
          write(iout, fmtdatetime0) datetime0
        case default
          write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN TDIS OPTION: ',        &
                                    trim(keyword)
          call store_error(errmsg)
          call parser%StoreErrorUnit()
          call ustop()
        end select
      end do
      write(iout,'(1x,a)') 'END OF TDIS OPTIONS'
    end if
    !
    ! -- Set to itmuni to undefined if not specified
    if(itmuni == 0) then
      if(.not. undspec) then
        write(iout, fmtitmuni) 'UNDEFINED'
      endif
    endif
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
    allocate(datetime0)
    !
    ! -- Initialize variables
    nper = 0
    itmuni = 0
    kper = 0
    kstp = 0
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
    use SimModule, only: ustop, store_error
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
    call parser%GetBlock('DIMENSIONS', isfound, ierr)
    !
    ! -- parse block if detected
    if (isfound) then
      write(iout,'(1x,a)')'PROCESSING TDIS DIMENSIONS'
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NPER')
            nper = parser%GetInteger()
            write(iout, fmtnper) nper
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN TDIS DIMENSION: ',     &
                                     trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(iout,'(1x,a)') 'END OF TDIS DIMENSIONS'
    else
      write(errmsg,'(1x,a)')'ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.'
      call store_error(errmsg)
      call parser%StoreErrorUnit()
      call ustop()
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
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    ! -- local
    character(len=LINELENGTH) :: line, errmsg
    integer(I4B) :: ierr
    integer(I4B) :: n
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtheader =                                 &
      "(1X,//1X,'STRESS PERIOD     LENGTH       TIME STEPS',                   &
                '     MULTIPLIER FOR DELT',/1X,76('-'))"
    character(len=*), parameter :: fmtrow =                                    &
      "(1X,I8,1PG21.7,I7,0PF25.3)"
    character(len=*), parameter :: fmtpwarn =                                  &
      "(1X,/1X,                                                                &
        'WARNING: PERLEN MUST NOT BE 0.0 FOR TRANSIENT STRESS PERIODS')"
    !data
! ------------------------------------------------------------------------------
    !
    ! -- get PERIODDATA block
    call parser%GetBlock('PERIODDATA', isfound, ierr)
    !
    ! -- parse block if detected
    if (isfound) then
      write(iout,'(1x,a)')'PROCESSING TDIS PERIODDATA'
      write(iout, fmtheader)
      do n = 1, nper
        call parser%GetNextLine(endOfBlock)
        perlen(n) = parser%GetDouble()
        nstp(n) = parser%GetInteger()
        tsmult(n) = parser%GetDouble()
        write (iout, fmtrow) n, perlen(n), nstp(n), tsmult(n)
        !
        !-----stop if nstp le 0, perlen eq 0 for transient stress periods,
        !-----tsmult le 0, or perlen lt 0..
        if(nstp(n) <= 0) then
           call store_error( &
             'THERE MUST BE AT LEAST ONE TIME STEP IN EVERY STRESS PERIOD')
        end if
        if(perlen(n) == dzero) then
           write(iout, fmtpwarn)
        end if
        if(tsmult(n) <= dzero) then
           call store_error( &
            'TSMULT MUST BE GREATER THAN 0.0')
        end if
        if(perlen(n).lt.dzero) then
           call store_error( &
            'PERLEN CANNOT BE LESS THAN 0.0 FOR ANY STRESS PERIOD')
        end if
        totalsimtime = totalsimtime + perlen(n)
      enddo
      call parser%GetNextLine(endOfBlock)
      if (.not. endOfBlock) then
        write(errmsg, '(a)') 'END PERIODDATA not detected. Instead found:'
        call store_error(errmsg)
        call parser%GetCurrentLine(line)
        write(errmsg, '(a)') trim(line)
        call store_error(errmsg)
      endif
      !
      ! -- Check for errors
      if(count_errors() > 0) then
        call parser%StoreErrorUnit()
        call ustop()
      endif
      write(iout,'(1x,a)') 'END OF TDIS PERIODDATA'
    else
      write(errmsg,'(1x,a)')'ERROR.  REQUIRED PERIODDATA BLOCK NOT FOUND.'
      call store_error(errmsg)
      call parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Return
    return
  end subroutine tdis_read_timing

  subroutine subtiming_begin(isubtime, nsubtimes, idsolution)
! ******************************************************************************
! subtiming_begin -- start subtiming
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    integer(I4B), intent(in) :: isubtime
    integer(I4B), intent(in) :: nsubtimes
    integer(I4B), intent(in) :: idsolution
    ! -- formats
    character(len=*), parameter :: fmtsub = "(a, i0, a, a, i0, a, i0, a)"
    character(len=*), parameter :: fmtdelt = "(a, i0, a, 1pg15.6)"
! ------------------------------------------------------------------------------
    !
    ! -- Save and calculate delt if first subtimestep
    if(isubtime == 1) then
      deltsav = delt
      delt = delt / nsubtimes
    else
      totimc = totimc + delt
    endif
    !
    ! -- Write message
    if(nsubtimes > 1) then
      write(iout, fmtsub) 'SOLUTION ID (', idsolution, '): ', &
                     'SUB-TIMESTEP ', isubtime, ' OF ', nsubtimes, ' TOTAL'
      write(iout, fmtdelt) 'SOLUTION ID (', idsolution, '): DELT = ', delt
    endif
    !
    ! -- return
    return
  end subroutine subtiming_begin

  subroutine subtiming_end()
! ******************************************************************************
! subtiming_end -- start subtiming
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !
    ! -- Reset delt to what it was prior to subtiming
    delt = deltsav
    !
    ! -- return
    return
  end subroutine subtiming_end

end module TdisModule


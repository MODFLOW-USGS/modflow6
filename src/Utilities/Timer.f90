module TimerModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, DZERO
  use MessageModule, only: write_message
  implicit none
  private
  public :: print_start_time
  public :: elapsed_time
  public :: code_timer
  integer(I4B), dimension(8) :: ibdt

contains

  subroutine print_start_time()
! ******************************************************************************
!  Start simulation timer
! ******************************************************************************
!
!        SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    ! -- local
    character(len=LINELENGTH) :: line
    integer(I4B) :: i
    ! -- format
    character(len=*), parameter :: fmtdt = &
      "(1X,'Run start date and time (yyyy/mm/dd hh:mm:ss): ', &
      &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)"
! ------------------------------------------------------------------------------
    !
    ! -- Get current date and time, assign to IBDT, and write to screen
    call date_and_time(values=ibdt)
    write (line, fmtdt) (ibdt(i), i=1, 3), (ibdt(i), i=5, 7)
    call write_message(line, skipafter=1)
    !
    ! -- return
    return
  end subroutine print_start_time

  SUBROUTINE elapsed_time(iout, iprtim)
! ******************************************************************************
!     Get end time and calculate elapsed time
! ******************************************************************************
!
!        SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(i4b), intent(in) :: iout
    integer(I4B), intent(in) :: iprtim
    ! -- local
    character(len=LINELENGTH) :: line
    INTEGER(I4B) :: IEDT(8), IDPM(12)
    integer(I4B) :: NSPD
    integer(I4B) :: i
    integer(I4B) :: ndays, leap, ibd, ied, mb, me, nm, mc, m
    integer(I4B) :: nhours, nmins, nsecs, msecs, nrsecs
    real(DP) :: elsec, rsecs
    DATA IDPM/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/ ! Days per month
    DATA NSPD/86400/ ! Seconds per day
    ! -- format
    character(len=*), parameter :: fmtdt = &
      "(1X,'Run end date and time (yyyy/mm/dd hh:mm:ss): ', &
      &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)"
! ------------------------------------------------------------------------------
!
!     Get current date and time, assign to IEDT, and write.
    CALL DATE_AND_TIME(VALUES=IEDT)
    !
    ! -- write elapsed time to stdout
    write (line, fmtdt) (IEDT(I), I=1, 3), (IEDT(I), I=5, 7)
    call write_message(line, skipbefore=1)
    !
    ! -- write elapsted time to iout
    IF (IPRTIM .GT. 0) THEN
      call write_message(line, iunit=iout, skipbefore=1)
    END IF
!
!     Calculate elapsed time in days and seconds
    NDAYS = 0
    LEAP = 0
    IF (MOD(IEDT(1), 4) .EQ. 0) LEAP = 1
    IBD = IBDT(3) ! BEGIN DAY
    IED = IEDT(3) ! END DAY
!     FIND DAYS
    IF (IBDT(2) .NE. IEDT(2)) THEN
!       MONTHS DIFFER
      MB = IBDT(2) ! BEGIN MONTH
      ME = IEDT(2) ! END MONTH
      NM = ME - MB + 1 ! NUMBER OF MONTHS TO LOOK AT
      IF (MB .GT. ME) NM = NM + 12
      MC = MB - 1
      DO M = 1, NM
        MC = MC + 1 ! MC IS CURRENT MONTH
        IF (MC .EQ. 13) MC = 1
        IF (MC .EQ. MB) THEN
          NDAYS = NDAYS + IDPM(MC) - IBD
          IF (MC .EQ. 2) NDAYS = NDAYS + LEAP
        ELSEIF (MC .EQ. ME) THEN
          NDAYS = NDAYS + IED
        ELSE
          NDAYS = NDAYS + IDPM(MC)
          IF (MC .EQ. 2) NDAYS = NDAYS + LEAP
        END IF
      END DO
    ELSEIF (IBD .LT. IED) THEN
!       START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
      NDAYS = IED - IBD
    END IF
    ELSEC = NDAYS * NSPD
!
!     ADD OR SUBTRACT SECONDS
    ELSEC = ELSEC + (IEDT(5) - IBDT(5)) * 3600.0
    ELSEC = ELSEC + (IEDT(6) - IBDT(6)) * 60.0
    ELSEC = ELSEC + (IEDT(7) - IBDT(7))
    ELSEC = ELSEC + (IEDT(8) - IBDT(8)) * 0.001
!
!     CONVERT SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
    NDAYS = INT(ELSEC / NSPD)
    RSECS = MOD(ELSEC, 86400.0_DP)
    NHOURS = INT(RSECS / 3600.0)
    RSECS = MOD(RSECS, 3600.0_DP)
    NMINS = INT(RSECS / 60.0)
    RSECS = MOD(RSECS, 60.0_DP)
    NSECS = INT(RSECS)
    RSECS = MOD(RSECS, 1.0_DP)
    MSECS = NINT(RSECS * 1000.0)
    NRSECS = NSECS
    IF (RSECS .GE. 0.5) NRSECS = NRSECS + 1
!
!     Write elapsed time to screen
    IF (NDAYS .GT. 0) THEN
      WRITE (line, 1010) NDAYS, NHOURS, NMINS, NRSECS
1010  FORMAT(1X, 'Elapsed run time: ', I3, ' Days, ', I2, ' Hours, ', I2, &
             ' Minutes, ', I2, ' Seconds')
    ELSEIF (NHOURS .GT. 0) THEN
      WRITE (line, 1020) NHOURS, NMINS, NRSECS
1020  FORMAT(1X, 'Elapsed run time: ', I2, ' Hours, ', I2, &
             ' Minutes, ', I2, ' Seconds')
    ELSEIF (NMINS .GT. 0) THEN
      WRITE (line, 1030) NMINS, NSECS, MSECS
1030  FORMAT(1X, 'Elapsed run time: ', I2, ' Minutes, ', &
             I2, '.', I3.3, ' Seconds')
    ELSE
      WRITE (line, 1040) NSECS, MSECS
1040  FORMAT(1X, 'Elapsed run time: ', I2, '.', I3.3, ' Seconds')
    END IF
    call write_message(line, skipafter=1)
!
!     Write times to file if requested
    IF (IPRTIM .GT. 0) THEN
      IF (NDAYS .GT. 0) THEN
        WRITE (IOUT, 1010) NDAYS, NHOURS, NMINS, NRSECS
      ELSEIF (NHOURS .GT. 0) THEN
        WRITE (IOUT, 1020) NHOURS, NMINS, NRSECS
      ELSEIF (NMINS .GT. 0) THEN
        WRITE (IOUT, 1030) NMINS, NSECS, MSECS
      ELSE
        WRITE (IOUT, 1040) NSECS, MSECS
      END IF
    END IF
!
    RETURN
  END SUBROUTINE elapsed_time

!
!-------TIMER FOR SUBROUTINES
  SUBROUTINE code_timer(it, t1, ts)
! ******************************************************************************
!     Get end time and calculate elapsed time
! ******************************************************************************
!
!        SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    INTEGER(I4B), INTENT(IN) :: it
    REAL(DP), INTENT(INOUT) :: t1
    REAL(DP), INTENT(INOUT) :: ts
    ! -- local
    REAL(DP) :: dt
! ------------------------------------------------------------------------------
    !
    IF (IT == 0) THEN
      call CPU_TIME(t1)
    ELSE
      call CPU_TIME(dt)
      ts = ts + dt - t1
    END IF
    !
    ! -- RETURN
    RETURN
  END SUBROUTINE code_timer

end module TimerModule

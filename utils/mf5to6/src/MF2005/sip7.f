         MODULE SIPMODULE
         INTEGER, SAVE, POINTER          ::NPARM,IPCALC,IPRSIP
         integer, save, pointer          :: mxitersip
         REAL,    SAVE, POINTER          ::HCLOSE,ACCL
         REAL,    SAVE, DIMENSION(:),       POINTER   ::W
         REAL,    SAVE, DIMENSION(:,:,:),   POINTER   ::EL
         REAL,    SAVE, DIMENSION(:,:,:),   POINTER   ::FL
         REAL,    SAVE, DIMENSION(:,:,:),   POINTER   ::GL
         REAL,    SAVE, DIMENSION(:,:,:),   POINTER   ::V
         REAL,    SAVE, DIMENSION(:),       POINTER   ::HDCG
         INTEGER, SAVE, DIMENSION(:,:),     POINTER   ::LRCH
       TYPE SIPTYPE
         INTEGER,       POINTER          ::NPARM,IPCALC,IPRSIP
         integer,       pointer          :: mxitersip
         REAL,          POINTER          ::HCLOSE,ACCL
         REAL,          DIMENSION(:),       POINTER   ::W
         REAL,          DIMENSION(:,:,:),   POINTER   ::EL
         REAL,          DIMENSION(:,:,:),   POINTER   ::FL
         REAL,          DIMENSION(:,:,:),   POINTER   ::GL
         REAL,          DIMENSION(:,:,:),   POINTER   ::V
         REAL,          DIMENSION(:),       POINTER   ::HDCG
         INTEGER,       DIMENSION(:,:),     POINTER   ::LRCH
       END TYPE
       TYPE(SIPTYPE),SAVE  ::SIPDAT(10)
      END MODULE SIPMODULE

!***********************************************************************

      SUBROUTINE SIP7AR(IN,MXITER,IGRID)
C     ******************************************************************
C     ALLOCATE STORAGE FOR SIP ARRAYS AND READ SIP DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY: IOUT,NCOL,NROW,NLAY
      USE SIPMODULE, ONLY: NPARM,IPCALC,IPRSIP,HCLOSE,ACCL,W,EL,FL,GL,
     1                     V,HDCG,LRCH,mxitersip
      use utl7module, only: URDCOM, URWORD
C
      CHARACTER*200 LINE
      double precision :: r
C     ------------------------------------------------------------------
      ALLOCATE(NPARM,IPCALC,IPRSIP,HCLOSE,ACCL)
      allocate(mxitersip)
C
C1------PRINT A MESSAGE IDENTIFYING SIP PACKAGE
      WRITE(IOUT,1)IN
    1 FORMAT(1X,
     1   /1X,'SIP -- STRONGLY-IMPLICIT PROCEDURE SOLUTION PACKAGE',
     2   /20X,'VERSION 7, 5/2/2005',' INPUT READ FROM UNIT ',I4)
C
C2------READ AND PRINT COMMENTS, MXITER, AND NPARM
      CALL URDCOM(IN,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXITER,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPARM,R,IOUT,IN)
      mxitersip = mxiter
      WRITE(IOUT,3) MXITER,NPARM
    3 FORMAT(1X,'MAXIMUM OF',I4,' ITERATIONS ALLOWED FOR CLOSURE'/
     1       1X,I2,' ITERATION PARAMETERS')
C
C3------ALLOCATE SPACE FOR THE SIP ARRAYS
      ALLOCATE(EL(NCOL,NROW,NLAY))
      ALLOCATE(FL(NCOL,NROW,NLAY))
      ALLOCATE(GL(NCOL,NROW,NLAY))
      ALLOCATE(V(NCOL,NROW,NLAY))
      ALLOCATE(W(NPARM))
      ALLOCATE(LRCH(3,MXITER))
      ALLOCATE(HDCG(MXITER))
C
C4------READ ACCL,HCLOSE,WSEED,IPCALC,IPRSIP
      READ(IN,*) ACCL,HCLOSE,IPCALC,WSEED,IPRSIP
      ZERO=0.
      IF(ACCL.EQ.ZERO) ACCL=1.
      IF(IPRSIP.LE.0)IPRSIP=999
C
C5------PRINT DATA VALUES JUST READ
      WRITE(IOUT,100)
  100 FORMAT(1X,/10X,'SOLUTION BY THE STRONGLY IMPLICIT PROCEDURE'
     1   /10X,43('-'))
      WRITE(IOUT,115) MXITER
  115 FORMAT(1X,'MAXIMUM ITERATIONS ALLOWED FOR CLOSURE =',I9)
      WRITE(IOUT,120) ACCL
  120 FORMAT(1X,16X,'ACCELERATION PARAMETER =',G15.5)
      WRITE(IOUT,125) HCLOSE
  125 FORMAT(1X,5X,'HEAD CHANGE CRITERION FOR CLOSURE =',E15.5)
      WRITE(IOUT,130) IPRSIP
  130 FORMAT(1X,5X,'SIP HEAD CHANGE PRINTOUT INTERVAL =',I9)
C
C6------CHECK IF SPECIFIED VALUE OF WSEED SHOULD BE USED OR IF
C6------SEED SHOULD BE CALCULATED
      IF(IPCALC.NE.0) THEN
C
C6A-----CALCULATE SEED & ITERATION PARAMETERS PRIOR TO 1ST ITERATION
         WRITE(IOUT,140)
  140    FORMAT(1X,/5X,'CALCULATE ITERATION PARAMETERS FROM MODEL',
     1   ' CALCULATED WSEED')
      ELSE
C
C6B-----USE SPECIFIED VALUE OF WSEED
C6B-----CALCULATE AND PRINT ITERATION PARAMETERS
  150    ONE=1.
         P1=-ONE
         P2=NPARM-1
         DO 160 I=1,NPARM
         P1=P1+ONE
  160    W(I)=ONE-WSEED**(P1/P2)
         WRITE(IOUT,161) NPARM,WSEED,(W(J),J=1,NPARM)
  161    FORMAT(1X,/1X,I5,' ITERATION PARAMETERS CALCULATED FROM',
     1     ' SPECIFIED WSEED =',F11.8,' :'//(1X,5E13.6))
      END IF
C
C7------RETURN
      CALL SIP7PSV(IGRID)
      RETURN
      END SUBROUTINE SIP7AR

!***********************************************************************

      SUBROUTINE SIP7DA(IGRID)
C  Deallocate SIP DATA
      USE SIPMODULE
C
      CALL SIP7PNT(IGRID)
        DEALLOCATE(NPARM,IPCALC,IPRSIP,HCLOSE,ACCL)
        DEALLOCATE(EL)
        DEALLOCATE(FL)
        DEALLOCATE(GL)
        DEALLOCATE(V)
        DEALLOCATE(W)
        DEALLOCATE(LRCH)
        DEALLOCATE(HDCG)
        deallocate(mxitersip)
C
      RETURN
      END SUBROUTINE SIP7DA

!***********************************************************************

      SUBROUTINE SIP7PNT(IGRID)
C  Set pointers to SIP data for a grid
      USE SIPMODULE
C
      NPARM=>SIPDAT(IGRID)%NPARM
      IPCALC=>SIPDAT(IGRID)%IPCALC
      IPRSIP=>SIPDAT(IGRID)%IPRSIP
      HCLOSE=>SIPDAT(IGRID)%HCLOSE
      ACCL=>SIPDAT(IGRID)%ACCL
      EL=>SIPDAT(IGRID)%EL
      FL=>SIPDAT(IGRID)%FL
      GL=>SIPDAT(IGRID)%GL
      V=>SIPDAT(IGRID)%V
      W=>SIPDAT(IGRID)%W
      LRCH=>SIPDAT(IGRID)%LRCH
      HDCG=>SIPDAT(IGRID)%HDCG
      mxitersip => SIPDAT(igrid)%mxitersip
C
      RETURN
      END SUBROUTINE SIP7PNT

!***********************************************************************

      SUBROUTINE SIP7PSV(IGRID)
C  Save pointers to SIP data
      USE SIPMODULE
C
      SIPDAT(IGRID)%NPARM=>NPARM
      SIPDAT(IGRID)%IPCALC=>IPCALC
      SIPDAT(IGRID)%IPRSIP=>IPRSIP
      SIPDAT(IGRID)%HCLOSE=>HCLOSE
      SIPDAT(IGRID)%ACCL=>ACCL
      SIPDAT(IGRID)%EL=>EL
      SIPDAT(IGRID)%FL=>FL
      SIPDAT(IGRID)%GL=>GL
      SIPDAT(IGRID)%V=>V
      SIPDAT(IGRID)%W=>W
      SIPDAT(IGRID)%LRCH=>LRCH
      SIPDAT(IGRID)%HDCG=>HDCG
      sipdat(igrid)%mxitersip => mxitersip
C
      RETURN
      END SUBROUTINE SIP7PSV

!***********************************************************************

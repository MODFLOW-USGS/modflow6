
      MODULE GWFEVTMODULE
        INTEGER, SAVE, POINTER                 ::NEVTOP,IEVTCB
        INTEGER, SAVE, POINTER                 ::NPEVT,IEVTPF
        double precision,  SAVE,   DIMENSION(:,:),  POINTER      ::EVTR
        double precision,  SAVE,   DIMENSION(:,:),  POINTER      ::EXDP
        double precision,  SAVE,   DIMENSION(:,:),  POINTER      ::SURF
        INTEGER, SAVE,   DIMENSION(:,:),  POINTER      ::IEVT
      TYPE GWFEVTTYPE
        INTEGER,  POINTER                 ::NEVTOP,IEVTCB
        INTEGER,  POINTER                 ::NPEVT,IEVTPF
        double precision,       DIMENSION(:,:),  POINTER      ::EVTR
        double precision,       DIMENSION(:,:),  POINTER      ::EXDP
        double precision,       DIMENSION(:,:),  POINTER      ::SURF
        INTEGER,    DIMENSION(:,:),  POINTER      ::IEVT
      END TYPE
      TYPE(GWFEVTTYPE), SAVE ::GWFEVTDAT(10)
      
      contains

      SUBROUTINE GWF2EVT7DA(IGRID)
C  Deallocate EVT MEMORY
C
        DEALLOCATE(GWFEVTDAT(IGRID)%NEVTOP)
        DEALLOCATE(GWFEVTDAT(IGRID)%IEVTCB)
        DEALLOCATE(GWFEVTDAT(IGRID)%NPEVT)
        DEALLOCATE(GWFEVTDAT(IGRID)%IEVTPF)
        DEALLOCATE(GWFEVTDAT(IGRID)%EVTR)
        DEALLOCATE(GWFEVTDAT(IGRID)%EXDP)
        DEALLOCATE(GWFEVTDAT(IGRID)%SURF)
        DEALLOCATE(GWFEVTDAT(IGRID)%IEVT)
C
      RETURN
      END SUBROUTINE GWF2EVT7DA

C*******************************************************************************

      SUBROUTINE SGWF2EVT7PNT(IGRID)
C  Set pointers to EVT data for grid.
C
        NEVTOP=>GWFEVTDAT(IGRID)%NEVTOP
        IEVTCB=>GWFEVTDAT(IGRID)%IEVTCB
        NPEVT=>GWFEVTDAT(IGRID)%NPEVT
        IEVTPF=>GWFEVTDAT(IGRID)%IEVTPF
        EVTR=>GWFEVTDAT(IGRID)%EVTR
        EXDP=>GWFEVTDAT(IGRID)%EXDP
        SURF=>GWFEVTDAT(IGRID)%SURF
        IEVT=>GWFEVTDAT(IGRID)%IEVT
C
      RETURN
      END SUBROUTINE SGWF2EVT7PNT

C*******************************************************************************

      SUBROUTINE SGWF2EVT7PSV(IGRID)
C  Save pointers to EVT data for grid.
C
        GWFEVTDAT(IGRID)%NEVTOP=>NEVTOP
        GWFEVTDAT(IGRID)%IEVTCB=>IEVTCB
        GWFEVTDAT(IGRID)%NPEVT=>NPEVT
        GWFEVTDAT(IGRID)%IEVTPF=>IEVTPF
        GWFEVTDAT(IGRID)%EVTR=>EVTR
        GWFEVTDAT(IGRID)%EXDP=>EXDP
        GWFEVTDAT(IGRID)%SURF=>SURF
        GWFEVTDAT(IGRID)%IEVT=>IEVT
C
      RETURN
      END SUBROUTINE SGWF2EVT7PSV
      
      END MODULE GWFEVTMODULE

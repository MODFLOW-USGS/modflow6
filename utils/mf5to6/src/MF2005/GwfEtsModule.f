      MODULE GWFETSMODULE
        INTEGER,SAVE,POINTER :: NETSOP => null()
        INTEGER,SAVE,POINTER :: IETSCB => null()
        INTEGER,SAVE,POINTER :: NPETS => null()
        INTEGER,SAVE,POINTER :: IETSPF => null()
        INTEGER,SAVE,POINTER :: NETSEG => null()
        INTEGER,      SAVE, DIMENSION(:,:),   POINTER ::IETS
        double precision,         SAVE, DIMENSION(:,:),   POINTER ::ETSR
        double precision,         SAVE, DIMENSION(:,:),   POINTER ::ETSX
        double precision,         SAVE, DIMENSION(:,:),   POINTER ::ETSS
        double precision,         SAVE, DIMENSION(:,:,:), POINTER ::PXDP
        double precision,         SAVE, DIMENSION(:,:,:), POINTER ::PETM
      TYPE GWFETSTYPE
        INTEGER, POINTER   ::NETSOP,IETSCB,NPETS,IETSPF,NETSEG
        INTEGER,       DIMENSION(:,:),   POINTER ::IETS
        double precision,          DIMENSION(:,:),   POINTER ::ETSR
        double precision,          DIMENSION(:,:),   POINTER ::ETSX
        double precision,          DIMENSION(:,:),   POINTER ::ETSS
        double precision,          DIMENSION(:,:,:), POINTER ::PXDP
        double precision,          DIMENSION(:,:,:), POINTER ::PETM
      END TYPE
      TYPE(GWFETSTYPE), SAVE :: GWFETSDAT(10)
      
      contains
      
      SUBROUTINE GWF2ETS7DA(IGRID)
C  Deallocate ETS MEMORY
C
        CALL SGWF2ETS7PNT(IGRID)
        DEALLOCATE(NETSOP)
        DEALLOCATE(IETSCB)
        DEALLOCATE(NPETS)
        DEALLOCATE(IETSPF)
        DEALLOCATE(NETSEG)
        DEALLOCATE(IETS)
        DEALLOCATE(ETSR)
        DEALLOCATE(ETSX)
        DEALLOCATE(ETSS)
        DEALLOCATE(PXDP)
        DEALLOCATE(PETM)
C
      RETURN
      END SUBROUTINE GWF2ETS7DA

C*******************************************************************************

      SUBROUTINE SGWF2ETS7PNT(IGRID)
C  Change ETS data to a different grid.
C
        NETSOP=>GWFETSDAT(IGRID)%NETSOP
        IETSCB=>GWFETSDAT(IGRID)%IETSCB
        NPETS=>GWFETSDAT(IGRID)%NPETS
        IETSPF=>GWFETSDAT(IGRID)%IETSPF
        NETSEG=>GWFETSDAT(IGRID)%NETSEG
        IETS=>GWFETSDAT(IGRID)%IETS
        ETSR=>GWFETSDAT(IGRID)%ETSR
        ETSX=>GWFETSDAT(IGRID)%ETSX
        ETSS=>GWFETSDAT(IGRID)%ETSS
        PXDP=>GWFETSDAT(IGRID)%PXDP
        PETM=>GWFETSDAT(IGRID)%PETM
C
      RETURN
      END SUBROUTINE SGWF2ETS7PNT

C*******************************************************************************

      SUBROUTINE SGWF2ETS7PSV(IGRID)
C  Save ETS data for a grid.
C
        GWFETSDAT(IGRID)%NETSOP=>NETSOP
        GWFETSDAT(IGRID)%IETSCB=>IETSCB
        GWFETSDAT(IGRID)%NPETS=>NPETS
        GWFETSDAT(IGRID)%IETSPF=>IETSPF
        GWFETSDAT(IGRID)%NETSEG=>NETSEG
        GWFETSDAT(IGRID)%IETS=>IETS
        GWFETSDAT(IGRID)%ETSR=>ETSR
        GWFETSDAT(IGRID)%ETSX=>ETSX
        GWFETSDAT(IGRID)%ETSS=>ETSS
        GWFETSDAT(IGRID)%PXDP=>PXDP
        GWFETSDAT(IGRID)%PETM=>PETM
C
      RETURN
      END SUBROUTINE SGWF2ETS7PSV
      
      END MODULE GWFETSMODULE



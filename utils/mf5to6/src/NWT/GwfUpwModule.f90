      MODULE GWFUPWMODULE
      IMPLICIT NONE
      DOUBLE PRECISION, PARAMETER :: HEPS = 1.0E-7
      DOUBLE PRECISION, PARAMETER :: CLOSEZERO = 1.0E-15
      DOUBLE PRECISION,PARAMETER :: BIG = 1.0D20
      DOUBLE PRECISION,PARAMETER :: SMALL = 1.0D-5
      DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER :: Sn, So
      INTEGER, SAVE,   POINTER :: Iuupw
! Cell property data
        INTEGER, SAVE,   POINTER ::IUPWCB,IWDFLG,IWETIT,IHDWET,IPHDRY
        INTEGER, SAVE,   POINTER ::ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC
        REAL,    SAVE,   POINTER ::WETFCT
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYTYPUPW
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYAVG
        double precision,    SAVE,   POINTER, DIMENSION(:)     ::CHANI
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYVKAUPW
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYWET
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYSTRT
        INTEGER, SAVE,   POINTER, DIMENSION(:,:)   ::LAYFLG
        INTEGER, SAVE,    DIMENSION(:,:,:), POINTER ::IBOUND2
        double precision,SAVE,POINTER, DIMENSION(:,:,:) ::VKAUPW
        double precision,    SAVE,   POINTER, DIMENSION(:,:,:) ::VKCB
        double precision,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC1
        double precision,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC2UPW
        double precision,SAVE,POINTER, DIMENSION(:,:,:) ::HANI
        double precision,    SAVE,   POINTER, DIMENSION(:,:,:) ::WETDRY
        double precision,SAVE,POINTER, DIMENSION(:,:,:) ::HKUPW
      TYPE GWFUPWTYPE
        INTEGER, POINTER :: Iuupw
! Cell property data
        INTEGER, POINTER ::IUPWCB,IWDFLG,IWETIT,IHDWET,IPHDRY
        INTEGER, POINTER ::ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC
        REAL, POINTER    ::WETFCT
        DOUBLE PRECISION, DIMENSION(:), POINTER :: Sn, So
        INTEGER,   POINTER, DIMENSION(:)     ::LAYTYPUPW
        INTEGER,   POINTER, DIMENSION(:)     ::LAYAVG
        double precision,      POINTER, DIMENSION(:)     ::CHANI
        INTEGER,   POINTER, DIMENSION(:)     ::LAYVKAUPW
        INTEGER,   POINTER, DIMENSION(:)     ::LAYWET
        INTEGER,   POINTER, DIMENSION(:)     ::LAYSTRT
        INTEGER,   POINTER, DIMENSION(:,:)   ::LAYFLG
        INTEGER,   POINTER, DIMENSION(:,:,:) ::IBOUND2
        double precision,POINTER,DIMENSION(:,:,:) ::VKAUPW
        double precision,      POINTER, DIMENSION(:,:,:) ::VKCB
        double precision,      POINTER, DIMENSION(:,:,:) ::SC1
        double precision,      POINTER, DIMENSION(:,:,:) ::SC2UPW
        double precision,POINTER,DIMENSION(:,:,:) ::HANI
        double precision,      POINTER, DIMENSION(:,:,:) ::WETDRY
        double precision,POINTER,DIMENSION(:,:,:) ::HKUPW
      END TYPE GWFUPWTYPE
      TYPE (GWFUPWTYPE) , SAVE::Gwfupwdat(10)
      END MODULE GWFUPWMODULE


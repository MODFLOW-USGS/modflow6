! Code in this file is from MODFLOW-NWT ver. 1.0.9, file gwf2wel7_NWT.f
      MODULE GWFWELMODULE
        INTEGER,SAVE,POINTER  ::NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL
        INTEGER,SAVE,POINTER  ::NPWEL,IWELPB,NNPWEL,IRDPSI
        CHARACTER(LEN=16),SAVE, DIMENSION(:),   POINTER     ::WELAUX
        double precision, SAVE, DIMENSION(:,:), POINTER     ::WELL
        double precision, SAVE,                 POINTER     ::PSIRAMP
        INTEGER,          SAVE,                 POINTER     ::IUNITRAMP
      TYPE GWFWELTYPE
        INTEGER,POINTER  ::NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL
        INTEGER,POINTER  ::NPWEL,IWELPB,NNPWEL,IRDPSI
        CHARACTER(LEN=16), DIMENSION(:),   POINTER     ::WELAUX
        double precision,  DIMENSION(:,:), POINTER     ::WELL
        double precision,                  POINTER     ::PSIRAMP
        INTEGER,                           POINTER     ::IUNITRAMP
      END TYPE
      TYPE(GWFWELTYPE), SAVE:: GWFWELDAT(10)
      END MODULE GWFWELMODULE

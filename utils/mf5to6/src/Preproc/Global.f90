MODULE GLOBAL
  use ConstantsModule, only: MAXCHARLEN
  ! scalars
  INTEGER,                   SAVE, POINTER :: NCOL, NROW, NLAY, NPER, NBOTM, NCNFBD
  INTEGER,                   SAVE, POINTER :: ITMUNI, LENUNI, IXSEC, ITRSS, INBAS
  INTEGER,                   SAVE, POINTER :: IFREFM, NODES, IOUT, MXITER
  double precision,          save, pointer :: constantdelr
  double precision,          save, pointer :: constantdelc
  character(len=MAXCHARLEN), save, pointer :: cbcfilename
  ! arrays
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: IUNIT(:)
  DOUBLE PRECISION, SAVE, DIMENSION(:,:,:), POINTER :: HNEW
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: LBOTM
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: LAYCBD
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: LAYHDT
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: LAYHDS
  double precision, SAVE, DIMENSION(:),     POINTER :: PERLEN
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: NSTP
  double precision, SAVE, DIMENSION(:),     POINTER :: TSMULT
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: ISSFLG
  double precision, SAVE, DIMENSION(:),     POINTER :: DELR
  double precision, SAVE, DIMENSION(:),     POINTER :: DELC
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: BOTM
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: HOLD
  INTEGER, SAVE,    DIMENSION(:,:,:),       POINTER :: IBOUND
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: CR
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: CC
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: CV
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: HCOF
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: RHS
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: BUFF
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: STRT
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: DDREF

  TYPE GLOBALTYPE
    ! scalars
    INTEGER,                   POINTER :: NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD
    INTEGER,                   POINTER :: ITMUNI,LENUNI,IXSEC,ITRSS,INBAS
    INTEGER,                   POINTER :: IFREFM,NODES,IOUT,MXITER
    double precision,          pointer :: constantdelr
    double precision,          pointer :: constantdelc
    character(len=MAXCHARLEN), pointer :: cbcfilename
    ! arrays
    INTEGER,          DIMENSION(:),     POINTER ::IUNIT
    DOUBLE PRECISION, DIMENSION(:,:,:), POINTER ::HNEW
    INTEGER,          DIMENSION(:),     POINTER ::LBOTM
    INTEGER,          DIMENSION(:),     POINTER ::LAYCBD
    INTEGER,          DIMENSION(:),     POINTER ::LAYHDT
    INTEGER,          DIMENSION(:),     POINTER ::LAYHDS
    double precision, DIMENSION(:),     POINTER ::PERLEN
    INTEGER,          DIMENSION(:),     POINTER ::NSTP
    double precision, DIMENSION(:),     POINTER ::TSMULT
    INTEGER,          DIMENSION(:),     POINTER ::ISSFLG
    double precision, DIMENSION(:),     POINTER ::DELR
    double precision, DIMENSION(:),     POINTER ::DELC
    double precision, DIMENSION(:,:,:), POINTER ::BOTM
    double precision, DIMENSION(:,:,:), POINTER ::HOLD
    INTEGER,          DIMENSION(:,:,:), POINTER ::IBOUND
    double precision, DIMENSION(:,:,:), POINTER ::CR
    double precision, DIMENSION(:,:,:), POINTER ::CC
    double precision, DIMENSION(:,:,:), POINTER ::CV
    double precision, DIMENSION(:,:,:), POINTER ::HCOF
    double precision, DIMENSION(:,:,:), POINTER ::RHS
    double precision, DIMENSION(:,:,:), POINTER ::BUFF
    double precision, DIMENSION(:,:,:), POINTER ::STRT
    double precision, DIMENSION(:,:,:), POINTER ::DDREF
  END TYPE GLOBALTYPE

  TYPE(GLOBALTYPE),SAVE ::GLOBALDAT(10)
  
contains

  subroutine AllocateGlobalScalars()
    implicit none
    !
    allocate(NCOL, NROW, NLAY, NPER, NBOTM, NCNFBD)
    allocate(ITMUNI, LENUNI, IXSEC, ITRSS, INBAS)
    allocate(IFREFM, NODES, IOUT, MXITER)
    allocate(constantdelc, constantdelr, cbcfilename)
    !
    return
  end subroutine AllocateGlobalScalars

END MODULE GLOBAL

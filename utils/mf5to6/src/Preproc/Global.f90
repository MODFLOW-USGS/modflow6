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
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: IUNIT(:) => null()
  DOUBLE PRECISION, SAVE, DIMENSION(:,:,:), POINTER :: HNEW => null()
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: LBOTM => null()
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: LAYCBD => null()
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: LAYHDT => null()
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: LAYHDS => null()
  double precision, SAVE, DIMENSION(:),     POINTER :: PERLEN => null()
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: NSTP => null()
  double precision, SAVE, DIMENSION(:),     POINTER :: TSMULT => null()
  INTEGER, SAVE,    DIMENSION(:),           POINTER :: ISSFLG => null()
  double precision, SAVE, DIMENSION(:),     POINTER :: DELR => null()
  double precision, SAVE, DIMENSION(:),     POINTER :: DELC => null()
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: BOTM => null()
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: HOLD => null()
  INTEGER, SAVE,    DIMENSION(:,:,:),       POINTER :: IBOUND => null()
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: CR => null()
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: CC => null()
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: CV => null()
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: HCOF => null()
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: RHS => null()
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: BUFF => null()
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: STRT => null()
  double precision, SAVE, DIMENSION(:,:,:), POINTER :: DDREF => null()

  TYPE GLOBALTYPE
    ! scalars
    INTEGER,                   POINTER :: NCOL => null()
    INTEGER,                   POINTER :: NROW => null()
    INTEGER,                   POINTER :: NLAY => null()
    INTEGER,                   POINTER :: NPER => null()
    INTEGER,                   POINTER :: NBOTM => null()
    INTEGER,                   POINTER :: NCNFBD => null()
    INTEGER,                   POINTER :: ITMUNI => null()
    INTEGER,                   POINTER :: LENUNI => null()
    INTEGER,                   POINTER :: IXSEC => null()
    INTEGER,                   POINTER :: ITRSS => null()
    INTEGER,                   POINTER :: INBAS => null()
    INTEGER,                   POINTER :: IFREFM => null()
    INTEGER,                   POINTER :: NODES => null()
    INTEGER,                   POINTER :: IOUT => null()
    INTEGER,                   POINTER :: MXITER => null()
    double precision,          pointer :: constantdelr => null()
    double precision,          pointer :: constantdelc => null()
    character(len=MAXCHARLEN), pointer :: cbcfilename => null()
    ! arrays
    INTEGER,          DIMENSION(:),     POINTER ::IUNIT => null()
    DOUBLE PRECISION, DIMENSION(:,:,:), POINTER ::HNEW => null()
    INTEGER,          DIMENSION(:),     POINTER ::LBOTM => null()
    INTEGER,          DIMENSION(:),     POINTER ::LAYCBD => null()
    INTEGER,          DIMENSION(:),     POINTER ::LAYHDT => null()
    INTEGER,          DIMENSION(:),     POINTER ::LAYHDS => null()
    double precision, DIMENSION(:),     POINTER ::PERLEN => null()
    INTEGER,          DIMENSION(:),     POINTER ::NSTP => null()
    double precision, DIMENSION(:),     POINTER ::TSMULT => null()
    INTEGER,          DIMENSION(:),     POINTER ::ISSFLG => null()
    double precision, DIMENSION(:),     POINTER ::DELR => null()
    double precision, DIMENSION(:),     POINTER ::DELC => null()
    double precision, DIMENSION(:,:,:), POINTER ::BOTM => null()
    double precision, DIMENSION(:,:,:), POINTER ::HOLD => null()
    INTEGER,          DIMENSION(:,:,:), POINTER ::IBOUND => null()
    double precision, DIMENSION(:,:,:), POINTER ::CR => null()
    double precision, DIMENSION(:,:,:), POINTER ::CC => null()
    double precision, DIMENSION(:,:,:), POINTER ::CV => null()
    double precision, DIMENSION(:,:,:), POINTER ::HCOF => null()
    double precision, DIMENSION(:,:,:), POINTER ::RHS => null()
    double precision, DIMENSION(:,:,:), POINTER ::BUFF => null()
    double precision, DIMENSION(:,:,:), POINTER ::STRT => null()
    double precision, DIMENSION(:,:,:), POINTER ::DDREF => null()
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
    ! -- initialize IOUT
    iout = 0
    return
  end subroutine AllocateGlobalScalars

END MODULE GLOBAL

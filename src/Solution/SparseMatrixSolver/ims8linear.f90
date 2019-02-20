  MODULE IMSLinearModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, LENSOLUTIONNAME,                      &
                             IZERO, DZERO, DPREC, DSAME,                       &
                             DEM8, DEM6, DEM5, DEM4, DEM3, DEM2, DEM1,         &
                             DHALF, DONE, DTWO
  use GenericUtilities, only: IS_SAME
  use IMSReorderingModule, only: ims_genrcm, ims_odrv, ims_dperm, ims_vperm
  use BlockParserModule, only: BlockParserType

  IMPLICIT NONE
  private
  
  TYPE, PUBLIC :: IMSLINEAR_DATA
    CHARACTER (LEN=20) :: ORIGIN
    integer(I4B), POINTER :: iout => NULL()
    integer(I4B), POINTER :: IPRIMS => NULL()
    integer(I4B), POINTER :: ILINMETH => NULL()
    integer(I4B), POINTER :: ITER1 => NULL()
    integer(I4B), POINTER :: IPC => NULL()
    integer(I4B), POINTER :: ISCL => NULL()
    integer(I4B), POINTER :: IORD => NULL()
    integer(I4B), POINTER :: NORTH => NULL()
    integer(I4B), POINTER :: ICNVGOPT => NULL()
    integer(I4B), POINTER :: IACPC => NULL()
    integer(I4B), POINTER :: NITERC => NULL()
    integer(I4B), POINTER :: NIABCGS => NULL()
    integer(I4B), POINTER :: NIAPC => NULL()
    integer(I4B), POINTER :: NJAPC => NULL()
    real(DP), POINTER :: HCLOSE => NULL()
    real(DP), POINTER :: RCLOSE => NULL()
    real(DP), POINTER :: RELAX => NULL()
    real(DP), POINTER :: EPFACT => NULL()
    real(DP), POINTER :: L2NORM0 => NULL()
    ! ILUT VARIABLES
    integer(I4B), POINTER :: LEVEL => NULL()
    real(DP), POINTER :: DROPTOL => NULL()
    integer(I4B), POINTER :: NJLU => NULL()
    integer(I4B), POINTER :: NJW => NULL()
    integer(I4B), POINTER :: NWLU => NULL()
    ! POINTERS TO SOLUTION VARIABLES
    integer(I4B), POINTER :: NEQ => NULL()
    integer(I4B), POINTER :: NJA => NULL()
    integer(I4B), dimension(:), pointer, contiguous :: IA => NULL()
    integer(I4B), dimension(:), pointer, contiguous :: JA => NULL()
    real(DP), dimension(:), pointer, contiguous :: AMAT => NULL()
    real(DP), dimension(:), pointer, contiguous :: RHS => NULL()
    real(DP), dimension(:), pointer, contiguous :: X => NULL()
    ! VECTORS
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: DSCALE => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: DSCALE2 => NULL()
    integer(I4B), POINTER,DIMENSION(:),CONTIGUOUS :: IAPC => NULL()
    integer(I4B), POINTER,DIMENSION(:),CONTIGUOUS :: JAPC => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: APC => NULL()
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: LORDER => NULL()
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: IORDER => NULL()
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: IARO => NULL()
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: JARO => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: ARO => NULL()
    ! WORKING ARRAYS
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: IW => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: W => NULL()
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: ID => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: D => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: P => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: Q => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: Z => NULL()
    ! BICGSTAB WORKING ARRAYS
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: T => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: V => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: DHAT => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: PHAT => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: QHAT => NULL()
    ! POINTERS FOR USE WITH BOTH ORIGINAL AND RCM ORDERINGS
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: IA0 => NULL()
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: JA0 => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: A0 => NULL()
    ! ILUT WORKING ARRAYS
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: JLU => NULL()
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: JW => NULL()
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: WLU => NULL()
    
    ! PROCEDURES (METHODS)
    CONTAINS
      PROCEDURE :: IMSLINEAR_ALLOCATE => IMSLINEAR_AR
      procedure :: imslinear_summary
      PROCEDURE :: IMSLINEAR_APPLY => IMSLINEAR_AP
      procedure :: IMSLINEAR_DA
      procedure, private :: allocate_scalars
      ! -- PRIVATE PROCEDURES
      PROCEDURE, PRIVATE :: SET_IMSLINEAR_INPUT
  END TYPE IMSLINEAR_DATA
  
  type(BlockParserType), private :: parser

  
  CONTAINS
    SUBROUTINE IMSLINEAR_AR(THIS, NAME, IN, IOUT, IPRIMS, MXITER, IFDPARAM, &
                            IMSLINEARM, NEQ, NJA, IA, JA, AMAT, RHS, X,     &
                            NINNER, LFINDBLOCK)
!     ******************************************************************
!     ALLOCATE STORAGE FOR PCG ARRAYS AND READ IMSLINEAR DATA
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      use MemoryManagerModule, only: mem_allocate
      use SimModule, only: ustop, store_error, count_errors
      !IMPLICIT NONE
!     + + + DUMMY VARIABLES + + +
      CLASS(IMSLINEAR_DATA), INTENT(INOUT) :: THIS
      CHARACTER (LEN=LENSOLUTIONNAME), INTENT(IN) :: NAME
      integer(I4B), INTENT(IN) :: IN
      integer(I4B), INTENT(IN) :: IOUT
      integer(I4B), TARGET, INTENT(IN) :: IPRIMS
      integer(I4B), INTENT(IN) :: MXITER
      integer(I4B), INTENT(IN) :: IFDPARAM
      integer(I4B), INTENT(INOUT) :: IMSLINEARM
      integer(I4B), TARGET, INTENT(IN) :: NEQ
      integer(I4B), TARGET, INTENT(IN) :: NJA
      integer(I4B), DIMENSION(NEQ+1), TARGET, INTENT(IN) :: IA
      integer(I4B), DIMENSION(NJA), TARGET, INTENT(IN) :: JA
      real(DP), DIMENSION(NJA), TARGET, INTENT(IN) :: AMAT
      real(DP), DIMENSION(NEQ), TARGET, INTENT(INOUT) :: RHS
      real(DP), DIMENSION(NEQ), TARGET, INTENT(INOUT) :: X
      integer(I4B), TARGET, INTENT(INOUT) :: NINNER
      integer(I4B), INTENT(IN), OPTIONAL :: LFINDBLOCK
!     + + + LOCAL VARIABLES + + +
      LOGICAL :: lreaddata
      character(len=LINELENGTH) :: errmsg, keyword
      !CHARACTER (LEN= 10) :: clin(0:2)
      !CHARACTER (LEN= 31) :: clintit(0:2)
      !CHARACTER (LEN= 20) :: cipc(0:4)
      !CHARACTER (LEN= 20) :: cscale(0:2)
      !CHARACTER (LEN= 25) :: corder(0:2)
      !CHARACTER (LEN= 16), DIMENSION(0:4) :: ccnvgopt
      !CHARACTER (LEN= 15) :: clevel, cdroptol 
      integer(I4B) :: i, n
      integer(I4B) :: i0
      integer(I4B) :: iscllen, iolen
      integer(I4B) :: ierr
      real(DP) :: r
      logical :: isfound, endOfBlock
      integer(I4B) :: ijlu
      integer(I4B) :: ijw
      integer(I4B) :: iwlu
      integer(I4B) :: iwk
!     + + + PARAMETERS + + +
!     + + + OUTPUT FORMATS + + +
!------------------------------------------------------------------
!
!-------SET LREADDATA
      IF (PRESENT(LFINDBLOCK)) THEN
        IF (LFINDBLOCK < 1) THEN
          lreaddata = .FALSE.
        ELSE
          lreaddata = .TRUE.
        END IF
      ELSE
        lreaddata = .TRUE.
      END IF
!
!-------DEFINE NAME
      THIS%ORIGIN = TRIM(NAME) // ' IMSLINEAR'
!
!-------SET POINTERS TO SOLUTION STORAGE
      THIS%IPRIMS => IPRIMS
      THIS%NEQ => NEQ
      THIS%NJA => NJA
      THIS%IA => IA
      THIS%JA => JA
      THIS%AMAT => AMAT
      THIS%RHS => RHS
      THIS%X => X
!-------ALLOCATE SCALAR VARIABLES
      call this%allocate_scalars()
!
!-------initialize iout
      this%iout = iout
!
!-------DEFAULT VALUES
      THIS%IORD = 0
      THIS%ISCL = 0
      THIS%IPC = 0
      THIS%LEVEL = 0
      
      !clevel = ''
      !cdroptol = ''
!
!-------TRANSFER COMMON VARIABLES FROM IMS TO IMSLINEAR
      THIS%ILINMETH = 0
      
      THIS%IACPC = 0
      THIS%RELAX = DZERO !0.97
      
      THIS%DROPTOL = DZERO
      
      THIS%NORTH = 0

      THIS%ICNVGOPT = 0
!
!-------PRINT A MESSAGE IDENTIFYING IMSLINEAR SOLVER PACKAGE
      WRITE (iout,2000)
02000 FORMAT (1X,/1X,'IMSLINEAR -- UNSTRUCTURED LINEAR SOLUTION',               &
     &        ' PACKAGE, VERSION 8, 04/28/2017')
!
!-------SET DEFAULT IMSLINEAR PARAMETERS
      CALL THIS%SET_IMSLINEAR_INPUT(IFDPARAM)
      NINNER = this%iter1
!
!-------Initialize block parser
      call parser%Initialize(in, iout)
!
! -- get IMSLINEAR block
      if (lreaddata) then
        call parser%GetBlock('LINEAR', isfound, ierr)
      else
        isfound = .FALSE.
      end if
!
! -- parse IMSLINEAR block if detected
      if (isfound) then
        write(iout,'(/1x,a)')'PROCESSING LINEAR DATA'
        do
          call parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
          call parser%GetStringCaps(keyword)
          ! -- parse keyword
          select case (keyword)
            case ('INNER_HCLOSE')
              this%hclose = parser%GetDouble()
            case ('INNER_RCLOSE')
              this%rclose = parser%GetDouble()
              ! -- look for additional key words
              call parser%GetStringCaps(keyword)
              if (keyword == 'STRICT') then
                THIS%ICNVGOPT = 1
              else if (keyword == 'L2NORM_RCLOSE') then
                THIS%ICNVGOPT = 2
              else if (keyword == 'RELATIVE_RCLOSE') then
                THIS%ICNVGOPT = 3
              else if (keyword == 'L2NORM_RELATIVE_RCLOSE') then
                THIS%ICNVGOPT = 4
              end if
            case ('INNER_MAXIMUM')
              i = parser%GetInteger()
              this%iter1 = i
              NINNER = i
            case ('LINEAR_ACCELERATION')
              call parser%GetStringCaps(keyword)
              if (keyword.eq.'CG') then
                THIS%ILINMETH = 1
              else if (keyword.eq.'BICGSTAB') then
                THIS%ILINMETH = 2
              else
                THIS%ILINMETH = 0
                write(errmsg,'(4x,a,a)') &
     &            '****ERROR. UNKNOWN IMSLINEAR LINEAR_ACCELERATION METHOD: ', &
     &            trim(keyword)
                call store_error(errmsg)
              end if
            case ('SCALING_METHOD')
              call parser%GetStringCaps(keyword)
              i = 0
              if (keyword.eq.'NONE') then
                i = 0
              else if (keyword.eq.'DIAGONAL') then
                i = 1
              else if (keyword.eq.'L2NORM') then
                i = 2
              else
                write(errmsg,'(4x,a,a)') &
     &            '****ERROR. UNKNOWN IMSLINEAR SCALING_METHOD: ', &
     &            trim(keyword)
                call store_error(errmsg)
              end if
              THIS%ISCL = i
            case ('RED_BLACK_ORDERING')
              i = 0
            case ('REORDERING_METHOD')
              call parser%GetStringCaps(keyword)
              i = 0
              if (keyword == 'NONE') then
                i = 0
              else if (keyword == 'RCM') then
                i = 1
              else if (keyword == 'MD') then
                i = 2
              else
                write(errmsg,'(4x,a,a)') &
     &            '****ERROR. UNKNOWN IMSLINEAR REORDERING_METHOD: ', &
                  trim(keyword)
                call store_error(errmsg)
              end if
              THIS%IORD = i
            case ('NUMBER_ORTHOGONALIZATIONS')
              this%north = parser%GetInteger()
            case ('RELAXATION_FACTOR')
              this%relax = parser%GetDouble()
            case ('PRECONDITIONER_LEVELS')
              i = parser%GetInteger()
              this%level = i
              if (i < 0) then
                write(errmsg,'(4x,a,a)') &
     &            '****ERROR. PRECONDITIONER_LEVELS: ', &
     &            'MUST BE GREATER THAN OR EQUAL TO ZERO'
                call store_error(errmsg)
              end if
              !write (clevel, '(i15)') i
            case ('PRECONDITIONER_DROP_TOLERANCE')
              r = parser%GetDouble()
              THIS%DROPTOL = r
              if (r < DZERO) then
                write(errmsg,'(4x,a,a)') &
     &            '****ERROR. PRECONDITIONER_DROP_TOLERANCE: ', &
     &            'MUST BE GREATER THAN OR EQUAL TO ZERO'
                call store_error(errmsg)
              end if
              !write (cdroptol, '(e15.5)') r
            case default
              write(errmsg,'(4x,a,a)') &
     &          '****WARNING. UNKNOWN IMSLINEAR KEYWORD: ', &
     &          trim(keyword)
              call store_error(errmsg)
          end select
        end do
        write(iout,'(1x,a)') 'END OF LINEAR DATA'
      else
        if (IFDPARAM ==  0) THEN
          write(errmsg,'(1x,a)') 'NO LINEAR BLOCK DETECTED.'
          call store_error(errmsg)
        end if
      end if
      
      IMSLINEARM = THIS%ILINMETH
!
!-------DETERMINE PRECONDITIONER
      IF (THIS%LEVEL > 0 .OR. THIS%DROPTOL > DZERO) THEN
        THIS%IPC = 3
      ELSE
        THIS%IPC = 1
      END IF
      IF (THIS%RELAX > DZERO) THEN
        THIS%IPC = THIS%IPC + 1
      END IF
!
!-------ERROR CHECKING FOR OPTIONS
      IF (THIS%ISCL < 0 ) THIS%ISCL = 0
      IF (THIS%ISCL > 2 ) THEN
        WRITE( errmsg,'(A)' ) 'IMSLINEAR7AR: ISCL MUST BE .LE. 2'
        call store_error(errmsg)
      END IF
      IF (THIS%IORD < 0 ) THIS%IORD = 0
      IF (THIS%IORD > 2) THEN
        WRITE( errmsg,'(A)' ) 'IMSLINEAR7AR: IORD MUST BE .LE. 2'
        call store_error(errmsg)
      END IF
      IF (THIS%NORTH < 0) THEN
        WRITE( errmsg,'(A)' ) 'IMSLINEAR7AR: NORTH MUST .GE. 0'
        call store_error(errmsg)
      END IF
      IF (THIS%RCLOSE ==  DZERO) THEN
        IF (THIS%ICNVGOPT /= 3) THEN
          WRITE( errmsg,'(A)' ) 'IMSLINEAR7AR: RCLOSE MUST .NE. 0.0'
          call store_error(errmsg)
        END IF
      END IF
      IF (THIS%RELAX < DZERO) THEN
        WRITE( errmsg,'(A)' ) 'IMSLINEAR7AR: RELAX MUST BE .GE. 0.0'
        call store_error(errmsg)
      END IF
      IF (THIS%RELAX > DONE) THEN
        WRITE( errmsg,'(A)' ) 'IMSLINEAR7AR: RELAX MUST BE .LE. 1.0'
        call store_error(errmsg)
      END IF

      if (count_errors() > 0) then
        call parser%StoreErrorUnit()
        call ustop()
      endif
!
!-------INITIALIZE IMSLINEAR VARIABLES
      THIS%NITERC = 0
!
!-------ALLOCATE AND INITIALIZE MEMORY FOR IMSLINEAR
      iscllen  = 1
      IF (THIS%ISCL.NE.0 ) iscllen  = NEQ
      CALL mem_allocate(THIS%DSCALE, iscllen, 'DSCALE', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%DSCALE2, iscllen, 'DSCALE2', TRIM(THIS%ORIGIN))
      
!-------ALLOCATE MEMORY FOR PRECONDITIONING MATRIX
      ijlu      = 1
      ijw       = 1
      iwlu      = 1
      ! -- ILU0 AND MILU0
      THIS%NIAPC = THIS%NEQ
      THIS%NJAPC = THIS%NJA
      ! -- ILUT AND MILUT
      IF (THIS%IPC ==  3 .OR. THIS%IPC ==  4) THEN
        THIS%NIAPC = THIS%NEQ
        IF (THIS%LEVEL > 0) THEN
          iwk      = THIS%NEQ * (THIS%LEVEL * 2 + 1)
        ELSE
          iwk = 0
          DO n = 1, NEQ
            i = IA(n+1) - IA(n)
            IF (i > iwk) THEN
              iwk = i
            END IF
          END DO
          iwk      = THIS%NEQ * iwk
        END IF
        THIS%NJAPC = iwk
        ijlu       = iwk
        ijw        = 2 * THIS%NEQ
        iwlu       = THIS%NEQ + 1
      END IF
      THIS%NJLU = ijlu
      THIS%NJW  = ijw
      THIS%NWLU = iwlu
!-------ALLOCATE BASE PRECONDITIONER VECTORS
      CALL mem_allocate(THIS%IAPC, THIS%NIAPC+1, 'IAPC', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%JAPC, THIS%NJAPC, 'JAPC', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%APC, THIS%NJAPC, 'APC', TRIM(THIS%ORIGIN))
!-------ALLOCATE MEMORY FOR ILU0 AND MILU0 NON-ZERO ROW ENTRY VECTOR
      CALL mem_allocate(THIS%IW, THIS%NIAPC, 'IW', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%W, THIS%NIAPC, 'W', TRIM(THIS%ORIGIN))
!-------ALLOCATE MEMORY FOR ILUT VECTORS
      CALL mem_allocate(THIS%JLU, ijlu, 'JLU', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%JW, ijw, 'JW', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%WLU, iwlu, 'WLU', TRIM(THIS%ORIGIN))
!-------GENERATE IAPC AND JAPC FOR ILU0 AND MILU0
      IF (THIS%IPC ==  1 .OR. THIS%IPC ==  2) THEN
        CALL IMSLINEARSUB_PCCRS(THIS%NEQ,THIS%NJA,THIS%IA,THIS%JA,              &
                                THIS%IAPC,THIS%JAPC)
      END IF
!-------ALLOCATE SPACE FOR PERMUTATION VECTOR
      i0     = 1
      iolen  = 1
      IF (THIS%IORD.NE.0) THEN
        i0     = THIS%NEQ
        iolen  = THIS%NJA
      END IF
      CALL mem_allocate(THIS%LORDER, i0, 'LORDER', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%IORDER, i0, 'IORDER', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%IARO, i0+1, 'IARO', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%JARO, iolen, 'JARO', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%ARO, iolen, 'ARO', TRIM(THIS%ORIGIN))
!-------ALLOCATE WORKING VECTORS FOR IMSLINEAR SOLVER
      CALL mem_allocate(THIS%ID, THIS%NEQ, 'ID', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%D, THIS%NEQ, 'D', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%P, THIS%NEQ, 'P', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%Q, THIS%NEQ, 'Q', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%Z, THIS%NEQ, 'Z', TRIM(THIS%ORIGIN))
!-------ALLOCATE MEMORY FOR BCGS WORKING ARRAYS
      THIS%NIABCGS = 1
      IF (THIS%ILINMETH ==  2) THEN
        THIS%NIABCGS = THIS%NEQ
      END IF
      CALL mem_allocate(THIS%T, THIS%NIABCGS, 'T', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%V, THIS%NIABCGS, 'V', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%DHAT, THIS%NIABCGS, 'DHAT', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%PHAT, THIS%NIABCGS, 'PHAT', TRIM(THIS%ORIGIN))
      CALL mem_allocate(THIS%QHAT, THIS%NIABCGS, 'QHAT', TRIM(THIS%ORIGIN))
!-------INITIALIZE IMSLINEAR VECTORS
      DO n = 1, iscllen
        THIS%DSCALE(n)  = DONE
        THIS%DSCALE2(n) = DONE
      END DO
      DO n = 1, THIS%NJAPC
        THIS%APC(n)  = DZERO
      END DO
!-------WORKING VECTORS
      DO n = 1, THIS%NEQ
        THIS%ID(n)   = IZERO
        THIS%D(n)    = DZERO
        THIS%P(n)    = DZERO
        THIS%Q(n)    = DZERO
        THIS%Z(n)    = DZERO
      END DO
      DO n = 1, THIS%NIAPC
        THIS%IW(n)   = IZERO
        THIS%W(n)    = DZERO
      END DO
!-------BCGS WORKING VECTORS
      DO n = 1, THIS%NIABCGS
        THIS%T(n)    = DZERO
        THIS%V(n)    = DZERO
        THIS%DHAT(n) = DZERO
        THIS%PHAT(n) = DZERO
        THIS%QHAT(n) = DZERO
      END DO
!-------ILUT AND MILUT WORKING VECTORS      
      DO n = 1, ijlu
        THIS%JLU(n)   = DZERO
      END DO
      DO n = 1, ijw
        THIS%JW(n)   = DZERO
      END DO
      DO n = 1, iwlu
        THIS%WLU(n)  = DZERO
      END DO
!-------REORDERING VECTORS
      DO n = 1, i0 + 1
        THIS%IARO(n) = IZERO
      END DO
      DO n = 1, iolen
        THIS%JARO(n) = IZERO
        THIS%ARO(n)  = DZERO
      END DO
!
!-------REVERSE CUTHILL MCKEE AND MINIMUM DEGREE ORDERING
      IF (THIS%IORD.NE.0) THEN
        CALL IMSLINEARSUB_CALC_ORDER(IOUT, THIS%IPRIMS, THIS%IORD,THIS%NEQ,     &
                                     THIS%NJA,THIS%IA,THIS%JA,                  &
                                     THIS%LORDER,THIS%IORDER)
      END IF
!      
!-------ALLOCATE MEMORY FOR STORING ITERATION CONVERGENCE DATA
      
!
!-------RETURN
      RETURN
    END SUBROUTINE IMSLINEAR_AR

    subroutine imslinear_summary(this, mxiter)
      class(IMSLINEAR_DATA), intent(inout) :: this
      integer(I4B), intent(in) :: mxiter
!     + + + LOCAL VARIABLES + + +
      CHARACTER (LEN= 10) :: clin(0:2)
      CHARACTER (LEN= 31) :: clintit(0:2)
      CHARACTER (LEN= 20) :: cipc(0:4)
      CHARACTER (LEN= 20) :: cscale(0:2)
      CHARACTER (LEN= 25) :: corder(0:2)
      CHARACTER (LEN= 16), DIMENSION(0:4) :: ccnvgopt
      CHARACTER (LEN= 15) :: clevel
      CHARACTER (LEN= 15) :: cdroptol 
      integer(I4B) :: i, j, n
!     + + + PARAMETERS + + +
!       DATA
      DATA clin  /'UNKNOWN   ', &
                  'CG        ', &
     &            'BCGS      '/
      DATA clintit  /'             UNKNOWN           ', &
                     '       CONJUGATE-GRADIENT      ', &
     &               'BICONJUGATE-GRADIENT STABILIZED'/
      DATA cipc  /'UNKNOWN             ', &
     &            'INCOMPLETE LU       ', &
     &            'MOD. INCOMPLETE LU  ', &
     &            'INCOMPLETE LUT      ', &
     &            'MOD. INCOMPLETE LUT '/
      DATA cscale/'NO SCALING          ', &
     &            'SYMMETRIC SCALING   ', &
     &            'L2 NORM SCALING     '/
      DATA corder/'ORIGINAL ORDERING        ', &
     &            'RCM ORDERING             ', &
     &            'MINIMUM DEGREE ORDERING  '/
      DATA ccnvgopt  /'INFINITY NORM   ', &
     &                'INFINITY NORM S ', &
     &                'L2 NORM         ', &
     &                'RELATIVE L2NORM ', &
                      'L2 NORM W. REL. '/
!       OUTPUT FORMATS
02010 FORMAT (1X,/,7X,'SOLUTION BY THE',1X,A31,1X,'METHOD', &
     &        /,1X,66('-'),/, &
     &        ' MAXIMUM OF ',I6,' CALLS OF SOLUTION ROUTINE',/, &
     &        ' MAXIMUM OF ',I6, &
     &        ' INTERNAL ITERATIONS PER CALL TO SOLUTION ROUTINE',/, &
     &        ' LINEAR ACCELERATION METHOD            =',1X,A,/, &
     &        ' MATRIX PRECONDITIONING TYPE           =',1X,A,/, &
     &        ' MATRIX SCALING APPROACH               =',1X,A,/, &
     &        ' MATRIX REORDERING APPROACH            =',1X,A,/, &
     &        ' NUMBER OF ORTHOGONALIZATIONS          =',I9,/, &
     &        ' HEAD CHANGE CRITERION FOR CLOSURE     =',E15.5,/, &
     &        ' RESIDUAL CHANGE CRITERION FOR CLOSURE =',E15.5,/, &
     &        ' RESIDUAL CONVERGENCE OPTION           =',I9,/, &
     &        ' RESIDUAL CONVERGENCE NORM             =',1X,A,/, &
     &        ' RELAXATION FACTOR                     =',E15.5)
02015 FORMAT (' NUMBER OF LEVELS                      =',A15,/, &
     &        ' DROP TOLERANCE                        =',A15,//)
2030  FORMAT(1X,A20,1X,6(I6,1X))
2040  FORMAT(1X,20('-'),1X,6(6('-'),1X))
2050  FORMAT(1X,62('-'),/)      !
!------------------------------------------------------------------
      !
      ! -- initialize clevel and cdroptol
      clevel = ''
      cdroptol = ''
      !
      ! -- PRINT MXITER,ITER1,IPC,ISCL,IORD,HCLOSE,RCLOSE
      write (this%iout,2010)                                        &
                        clintit(THIS%ILINMETH), MXITER, THIS%ITER1, &
                        clin(THIS%ILINMETH), cipc(THIS%IPC),        &
                        cscale(THIS%ISCL), corder(THIS%IORD),       &
                        THIS%NORTH, THIS%HCLOSE, THIS%RCLOSE,       &
                        THIS%ICNVGOPT, ccnvgopt(THIS%ICNVGOPT),     &
                        THIS%RELAX
      if (this%level > 0) then
        write (clevel, '(i15)') this%level
      end if
      if (this%droptol > DZERO) then
        write (cdroptol, '(e15.5)') this%droptol
      end if
      IF (this%level > 0 .or. this%droptol > DZERO) THEN
        write (this%iout,2015) trim(adjustl(clevel)),               &
                               trim(adjustl(cdroptol))
      ELSE
         WRITE (this%iout,'(//)')
      END IF
      
      if (this%iord /= 0) then
        !                                                                       
        ! -- WRITE SUMMARY OF REORDERING INFORMATION TO LIST FILE                                                  
        if (this%iprims ==  2) then 
          DO i = 1, this%neq, 6 
            WRITE (this%iout,2030) 'ORIGINAL NODE      :',                      &
                              (j,j=i,MIN(i+5,this%neq))                      
            WRITE (this%iout,2040) 
            WRITE (this%iout,2030) 'REORDERED INDEX    :',                      &
                              (this%lorder(j),j=i,MIN(i+5,this%neq))              
            WRITE (this%iout,2030) 'REORDERED NODE     :',                      &
                              (this%iorder(j),j=i,MIN(i+5,this%neq))              
            WRITE (this%iout,2050) 
          END DO 
        END IF 
      end if
      !
      ! -- return
    return
    end subroutine imslinear_summary 
    
    subroutine allocate_scalars(this)
      use MemoryManagerModule, only: mem_allocate
      class(IMSLINEAR_DATA), intent(inout) :: this
      !
      ! -- scalars
      call mem_allocate(this%iout, 'IOUT', this%origin)
      call mem_allocate(this%ilinmeth, 'ILINMETH', this%origin)
      call mem_allocate(this%iter1, 'ITER1', this%origin)
      call mem_allocate(this%ipc, 'IPC', this%origin)
      call mem_allocate(this%iscl, 'ISCL', this%origin)
      call mem_allocate(this%iord, 'IORD', this%origin)
      call mem_allocate(this%north, 'NORTH', this%origin)
      call mem_allocate(this%icnvgopt, 'ICNVGOPT', this%origin)
      call mem_allocate(this%iacpc, 'IACPC', this%origin)
      call mem_allocate(this%niterc, 'NITERC', this%origin)
      call mem_allocate(this%niabcgs, 'NIABCGS', this%origin)
      call mem_allocate(this%niapc, 'NIAPC', this%origin)
      call mem_allocate(this%njapc, 'NJAPC', this%origin)
      call mem_allocate(this%hclose, 'HCLOSE', this%origin)
      call mem_allocate(this%rclose, 'RCLOSE', this%origin)
      call mem_allocate(this%relax, 'RELAX', this%origin)
      call mem_allocate(this%epfact, 'EPFACT', this%origin)
      call mem_allocate(this%l2norm0, 'L2NORM0', this%origin)
      call mem_allocate(this%droptol, 'DROPTOL', this%origin)
      call mem_allocate(this%level, 'LEVEL', this%origin)
      call mem_allocate(this%njlu, 'NJLU', this%origin)
      call mem_allocate(this%njw, 'NJW', this%origin)
      call mem_allocate(this%nwlu, 'NWLU', this%origin)
      !
      ! -- initialize
      this%iout = 0
      this%ilinmeth = 0
      this%iter1 = 0
      this%ipc = 0
      this%iscl = 0
      this%iord = 0
      this%north = 0
      this%icnvgopt = 0
      this%iacpc = 0
      this%niterc = 0
      this%niabcgs = 0
      this%niapc = 0
      this%njapc = 0
      this%hclose = DZERO
      this%rclose = DZERO
      this%relax = DZERO
      this%epfact = DZERO
      this%l2norm0 = 0
      this%droptol = DZERO
      this%level = 0
      this%njlu = 0
      this%njw = 0
      this%nwlu = 0
      !
      ! --Return
      return
    end subroutine allocate_scalars
    
    subroutine IMSLINEAR_DA(this)
      use MemoryManagerModule, only: mem_deallocate
      class(IMSLINEAR_DATA), intent(inout) :: this
      !
      ! -- arrays
      call mem_deallocate(this%dscale)
      call mem_deallocate(this%dscale2)
      call mem_deallocate(this%iapc)
      call mem_deallocate(this%japc)
      call mem_deallocate(this%apc)
      call mem_deallocate(this%iw)
      call mem_deallocate(this%w)
      call mem_deallocate(this%jlu)
      call mem_deallocate(this%jw)
      call mem_deallocate(this%wlu)
      call mem_deallocate(this%lorder)
      call mem_deallocate(this%iorder)
      call mem_deallocate(this%iaro)
      call mem_deallocate(this%jaro)
      call mem_deallocate(this%aro)
      call mem_deallocate(this%id)
      call mem_deallocate(this%d)
      call mem_deallocate(this%p)
      call mem_deallocate(this%q)
      call mem_deallocate(this%z)
      call mem_deallocate(this%t)
      call mem_deallocate(this%v)
      call mem_deallocate(this%dhat)
      call mem_deallocate(this%phat)
      call mem_deallocate(this%qhat)
      !
      ! -- scalars
      call mem_deallocate(this%iout)
      call mem_deallocate(this%ilinmeth)
      call mem_deallocate(this%iter1)
      call mem_deallocate(this%ipc)
      call mem_deallocate(this%iscl)
      call mem_deallocate(this%iord)
      call mem_deallocate(this%north)
      call mem_deallocate(this%icnvgopt)
      call mem_deallocate(this%iacpc)
      call mem_deallocate(this%niterc)
      call mem_deallocate(this%niabcgs)
      call mem_deallocate(this%niapc)
      call mem_deallocate(this%njapc)
      call mem_deallocate(this%hclose)
      call mem_deallocate(this%rclose)
      call mem_deallocate(this%relax)
      call mem_deallocate(this%epfact)
      call mem_deallocate(this%l2norm0)
      call mem_deallocate(this%droptol)
      call mem_deallocate(this%level)
      call mem_deallocate(this%njlu)
      call mem_deallocate(this%njw)
      call mem_deallocate(this%nwlu)
      !
      ! -- nullify pointers
      nullify(this%iprims)
      nullify(this%neq)
      nullify(this%nja)
      nullify(this%ia)
      nullify(this%ja)
      nullify(this%amat)
      nullify(this%rhs)
      nullify(this%x)
      !
      ! --Return
      return
    end subroutine IMSLINEAR_DA
    
    SUBROUTINE SET_IMSLINEAR_INPUT(THIS, IFDPARAM)
      IMPLICIT NONE
!     + + + DUMMY ARGUMENTS + + +
      CLASS(IMSLINEAR_DATA), INTENT(INOUT) :: THIS
      integer(I4B), INTENT(IN) :: IFDPARAM
!     + + + LOCAL DEFINITIONS + + +
!     + + + PARAMETERS + + +
!     + + + FUNCTIONS + + +
!
!     + + + CODE + + +
      SELECT CASE ( IFDPARAM )
        ! Simple option
        CASE(1)
          THIS%ITER1 = 50
          THIS%ILINMETH=1
          THIS%IPC = 1
          THIS%ISCL = 0
          THIS%IORD = 0
          THIS%HCLOSE = DEM3
          THIS%RCLOSE = DEM1
          THIS%RELAX = DZERO
          THIS%LEVEL = 0
          THIS%DROPTOL = DZERO
          THIS%NORTH = 0
        ! Moderate
        CASE(2)
          THIS%ITER1 = 100
          THIS%ILINMETH=2
          THIS%IPC = 2
          THIS%ISCL = 0
          THIS%IORD = 0
          THIS%HCLOSE = DEM2
          THIS%RCLOSE = DEM1
          THIS%RELAX = 0.97D0
          THIS%LEVEL = 0
          THIS%DROPTOL = DZERO
          THIS%NORTH = 0
        ! Complex
        CASE(3)
          THIS%ITER1 = 500
          THIS%ILINMETH=2
          THIS%IPC = 3
          THIS%ISCL = 0
          THIS%IORD = 0
          THIS%HCLOSE = DEM1
          THIS%RCLOSE = DEM1
          THIS%RELAX = DZERO
          THIS%LEVEL = 5
          THIS%DROPTOL = DEM4
          THIS%NORTH = 2
      END SELECT
      RETURN
    END SUBROUTINE SET_IMSLINEAR_INPUT

      SUBROUTINE IMSLINEAR_AP(THIS,ICNVG,KSTP,KITER,IN_ITER,                  &
                              NCONV, CONVNMOD, CONVMODSTART, LOCDV, LOCDR,    &
                              CACCEL, ITINNER, CONVLOCDV, CONVLOCDR,          &
                              DVMAX, DRMAX, CONVDVMAX, CONVDRMAX)
!
!     ******************************************************************
!     SOLUTION BY THE CONJUGATE GRADIENT METHOD -
!                                          UP TO ITER1 ITERATIONS
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE SimModule
      IMPLICIT NONE
!     + + + DUMMY ARGUMENTS + + +
      CLASS(IMSLINEAR_DATA), INTENT(INOUT) :: THIS
      integer(I4B), INTENT(INOUT)                          :: ICNVG
      integer(I4B), INTENT(IN)                             :: KSTP
      integer(I4B), INTENT(IN)                             :: KITER
      integer(I4B), INTENT(INOUT)                          :: IN_ITER
      ! CONVERGENCE INFORMATION
      integer(I4B), INTENT(IN) :: NCONV
      integer(I4B), INTENT(IN) :: CONVNMOD
      integer(I4B), DIMENSION(CONVNMOD+1), INTENT(INOUT) ::CONVMODSTART
      integer(I4B), DIMENSION(CONVNMOD), INTENT(INOUT) :: LOCDV
      integer(I4B), DIMENSION(CONVNMOD), INTENT(INOUT) :: LOCDR
      character(len=31), DIMENSION(NCONV), INTENT(INOUT) :: CACCEL
      integer(I4B), DIMENSION(NCONV), INTENT(INOUT) :: ITINNER
      integer(I4B), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVLOCDV
      integer(I4B), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVLOCDR
      real(DP), DIMENSION(CONVNMOD), INTENT(INOUT) :: DVMAX
      real(DP), DIMENSION(CONVNMOD), INTENT(INOUT) :: DRMAX
      real(DP), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVDVMAX
      real(DP), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVDRMAX
      
!     + + + LOCAL DEFINITIONS + + +
      integer(I4B) :: n
      integer(I4B) :: innerit
      integer(I4B) :: irc
      integer(I4B) :: itmax
      real(DP) :: tv
      real(DP) :: rmax
!     + + + PARAMETERS + + +
!     + + + FUNCTIONS + + +
!
!     + + + CODE + + +
!
!-------SET EPFACT BASED ON MFUSG TIMESTEP
      IF (THIS%ICNVGOPT ==  2) THEN
        IF (KSTP ==  1) THEN
          THIS%EPFACT = 0.01
        ELSE
          THIS%EPFACT = 0.10
        END IF
      ELSE IF (THIS%ICNVGOPT ==  4) THEN
        THIS%EPFACT = DEM4
      ELSE
        THIS%EPFACT = DONE
      END IF

!-------SCALE PROBLEM
      IF (THIS%ISCL.NE.0) THEN
        CALL IMSLINEARSUB_SCALE(0,THIS%ISCL,                                    &
                         THIS%NEQ,THIS%NJA,THIS%IA,THIS%JA,                     &
                         THIS%AMAT,THIS%X,THIS%RHS,                             &
                         THIS%DSCALE,THIS%DSCALE2)
      END IF
!
!-------PERMUTE ROWS, COLUMNS, AND RHS
      IF (THIS%IORD.NE.0) THEN
        CALL ims_dperm(THIS%NEQ, THIS%NJA, THIS%AMAT,THIS%JA,THIS%IA, &
     &                 THIS%ARO,THIS%JARO,THIS%IARO,THIS%LORDER,THIS%ID,1)
        CALL ims_vperm(THIS%NEQ, THIS%X, THIS%LORDER)
        CALL ims_vperm(THIS%NEQ, THIS%RHS, THIS%LORDER)
        THIS%IA0 => THIS%IARO
        THIS%JA0 => THIS%JARO
        THIS%A0  => THIS%ARO
      ELSE
        THIS%IA0 => THIS%IA
        THIS%JA0 => THIS%JA
        THIS%A0  => THIS%AMAT
      END IF
!
!-------UPDATE PRECONDITIONER
      CALL IMSLINEARSUB_PCU(this%iout,THIS%NJA,THIS%NEQ,THIS%NIAPC,THIS%NJAPC,  &
                            THIS%IPC, THIS%RELAX, THIS%A0, THIS%IA0, THIS%JA0,  &
                            THIS%APC,THIS%IAPC,THIS%JAPC,THIS%IW,THIS%W,        &
                            THIS%LEVEL, THIS%DROPTOL, THIS%NJLU, THIS%NJW,      &
                            THIS%NWLU, THIS%JLU, THIS%JW, THIS%WLU)
!-------INITIALIZE SOLUTION VARIABLE AND ARRAYS
      IF (KITER ==  1 ) THIS%NITERC = 0
      irc    = 1
      ICNVG  = 0
      DO n = 1, THIS%NEQ
        THIS%D(n) = DZERO
        THIS%P(n) = DZERO
        THIS%Q(n) = DZERO
        THIS%Z(n) = DZERO
      END DO
!-------CALCULATE INITIAL RESIDUAL
      CALL IMSLINEARSUB_MV(THIS%NJA,THIS%NEQ,THIS%A0,THIS%X,THIS%D,             &
                           THIS%IA0,THIS%JA0)
      rmax = DZERO
      THIS%L2NORM0 = DZERO
      DO n = 1, THIS%NEQ
        tv   = THIS%D(n)
        THIS%D(n) = THIS%RHS(n) - tv
        IF (ABS( THIS%D(n) ) > rmax ) rmax = ABS( THIS%D(n) )
        THIS%L2NORM0 = THIS%L2NORM0 + THIS%D(n) * THIS%D(n)
      END DO
      THIS%L2NORM0 = SQRT(THIS%L2NORM0)
!-------CHECK FOR EXACT SOLUTION
      itmax = THIS%ITER1
      IF (rmax ==  DZERO) THEN
        itmax = 0
        ICNVG = 1
      END IF
!-------SOLUTION BY THE CONJUGATE GRADIENT METHOD
      IF (THIS%ILINMETH ==  1) THEN
        CALL IMSLINEARSUB_CG(ICNVG, itmax, innerit,                             &
                             THIS%NEQ, THIS%NJA, THIS%NIAPC, THIS%NJAPC,        &
                             THIS%IPC, THIS%NITERC, THIS%ICNVGOPT, THIS%NORTH,  &
                             THIS%HCLOSE, THIS%RCLOSE, THIS%L2NORM0,            &
                             THIS%EPFACT, THIS%IA0, THIS%JA0, THIS%A0,          &
                             THIS%IAPC, THIS%JAPC, THIS%APC,                    &
                             THIS%X, THIS%RHS, THIS%D, THIS%P, THIS%Q, THIS%Z,  &
                             THIS%NJLU, THIS%IW, THIS%JLU,                      &
                             NCONV, CONVNMOD, CONVMODSTART, LOCDV, LOCDR,       &
                             CACCEL, ITINNER, CONVLOCDV, CONVLOCDR,             &
                             DVMAX, DRMAX, CONVDVMAX, CONVDRMAX)
!-------SOLUTION BY THE BICONJUGATE GRADIENT STABILIZED METHOD
      ELSE IF (THIS%ILINMETH ==  2) THEN
        CALL IMSLINEARSUB_BCGS(ICNVG, itmax, innerit,                           &
                               THIS%NEQ, THIS%NJA, THIS%NIAPC, THIS%NJAPC,      &
                               THIS%IPC, THIS%NITERC, THIS%ICNVGOPT, THIS%NORTH,&
                               THIS%ISCL, THIS%DSCALE,                          &
                               THIS%HCLOSE, THIS%RCLOSE, THIS%L2NORM0,          &
                               THIS%EPFACT,  THIS%IA0, THIS%JA0, THIS%A0,       &
                               THIS%IAPC, THIS%JAPC, THIS%APC,                  &
                               THIS%X, THIS%RHS, THIS%D, THIS%P, THIS%Q,        &
                               THIS%T, THIS%V, THIS%DHAT, THIS%PHAT, THIS%QHAT, &
                               THIS%NJLU, THIS%IW, THIS%JLU,                    &
                               NCONV, CONVNMOD, CONVMODSTART, LOCDV, LOCDR,     &
                               CACCEL, ITINNER, CONVLOCDV, CONVLOCDR,           &
                               DVMAX, DRMAX, CONVDVMAX, CONVDRMAX)
      END IF
!
!-------BACK PERMUTE AMAT, SOLUTION, AND RHS
      IF (THIS%IORD.NE.0) THEN
        CALL ims_dperm(THIS%NEQ, THIS%NJA, THIS%A0, THIS%JA0, THIS%IA0,         &
     &                 THIS%AMAT, THIS%JA,THIS%IA,THIS%IORDER,THIS%ID,1)
        CALL ims_vperm(THIS%NEQ, THIS%X, THIS%IORDER)
        CALL ims_vperm(THIS%NEQ, THIS%RHS, THIS%IORDER)
      END IF
!
!-------UNSCALE PROBLEM
      IF (THIS%ISCL.NE.0) THEN
        CALL IMSLINEARSUB_SCALE(1, THIS%ISCL,                                   &
                                THIS%NEQ, THIS%NJA, THIS%IA, THIS%JA,           &
                                THIS%AMAT, THIS%X, THIS%RHS,                    &
                                THIS%DSCALE, THIS%DSCALE2)
      END IF
!
!-------SET IMS INNER ITERATION NUMBER (IN_ITER) TO NUMBER OF
!       IMSLINEAR INNER ITERATIONS (innerit)
      IN_ITER = innerit
!
!-------RETURN
      RETURN
!
      END SUBROUTINE IMSLINEAR_AP

    
! -- IMSLinearModule subroutines that do not depend on data stored in the IMSLinearModule class
!    all data is passed through subroutine calls
!                                                                       
!-------ROUTINE TO CALCULATE LORDER AND IORDER FOR REORDERING           
      SUBROUTINE IMSLINEARSUB_CALC_ORDER(IOUT, IPRIMS, IORD, NEQ, NJA, IA, JA,  &
     &                                   LORDER, IORDER)                     
      use SimModule, only: ustop, store_error, count_errors
      IMPLICIT NONE 
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(IN) :: IOUT 
        integer(I4B), INTENT(IN) :: IPRIMS 
        integer(I4B), INTENT(IN) :: IORD 
        integer(I4B), INTENT(IN) :: NEQ 
        integer(I4B), INTENT(IN) :: NJA 
        integer(I4B), DIMENSION(NEQ+1), INTENT(IN)  :: IA 
        integer(I4B), DIMENSION(NJA),   INTENT(IN)  :: JA 
        integer(I4B), DIMENSION(NEQ), INTENT(INOUT) :: LORDER 
        integer(I4B), DIMENSION(NEQ), INTENT(INOUT) :: IORDER 
!       + + + LOCAL DEFINITIONS + + +                                     
        character (len=LINELENGTH) :: errmsg 
        integer(I4B) :: n 
        integer(I4B) :: nsp 
        integer(I4B), DIMENSION(:), ALLOCATABLE :: iwork0, iwork1 
        integer(I4B) :: iflag 
        integer(I4B) :: i,j 
!       + + + PARAMETERS + + +                                            
!       + + + FUNCTIONS + + +                                             
!       + + + FORMATS + + +                                               
!       + + + CODE + + +                                                  
        DO n = 1, NEQ 
          LORDER(n) = IZERO 
          IORDER(n) = IZERO 
        END DO 
        ALLOCATE ( iwork0(NEQ)  ) 
        SELECT CASE ( IORD ) 
          CASE ( 1 ) 
            ALLOCATE ( iwork1(NEQ) ) 
            CALL ims_genrcm(NEQ, NJA, IA, JA,                           &
     &                      LORDER, iwork0, iwork1 )                        
          CASE ( 2 ) 
            nsp = 3 * NEQ + 4 * NJA 
            ALLOCATE ( iwork1(nsp)  ) 
            CALL ims_odrv(NEQ, NJA, nsp, IA, JA, LORDER, iwork0,        &
                          iwork1, iflag)                           
            IF (iflag.NE.0) THEN 
              write (errmsg,'(A)') 'ERROR CREATING MINIMUM DEGREE '//   &
     &                   'ORDER PERMUTATION '                           
              call store_error(errmsg) 
              !call ustop()                                             
            END IF 
        END SELECT 
!                                                                       
!         GENERATE INVERSE OF LORDER                                    
        DO n = 1, NEQ 
          IORDER( LORDER(n) ) = n 
        END DO 
!
!         DEALLOCATE TEMPORARY STORAGE                                  
        DEALLOCATE ( iwork0, iwork1 ) 
!
        if (count_errors() > 0) then
          call parser%StoreErrorUnit()
          call ustop()
        endif
!
!---------RETURN                                                        
        RETURN 
      END SUBROUTINE IMSLINEARSUB_CALC_ORDER 
!                                                                       
!-------ROUTINE TO SCALE THE COEFFICIENT MATRIX (AMAT),                 
!       THE RHS (B), AND THE ESTIMATE OF X (X)                          
      SUBROUTINE IMSLINEARSUB_SCALE(IOPT, ISCL, NEQ, NJA, IA, JA, AMAT, X, B,   &
     &                              DSCALE, DSCALE2)                            
        IMPLICIT NONE 
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(IN) :: IOPT 
        integer(I4B), INTENT(IN) :: ISCL 
        integer(I4B), INTENT(IN) :: NEQ 
        integer(I4B), INTENT(IN) :: NJA 
        integer(I4B), DIMENSION(NEQ+1), INTENT(IN) :: IA 
        integer(I4B), DIMENSION(NJA),   INTENT(IN) :: JA 
        real(DP), DIMENSION(NJA),  INTENT(INOUT) :: AMAT 
        real(DP), DIMENSION(NEQ),  INTENT(INOUT) :: X 
        real(DP), DIMENSION(NEQ),  INTENT(INOUT) :: B 
        real(DP), DIMENSION(NEQ),  INTENT(INOUT) :: DSCALE 
        real(DP), DIMENSION(NEQ), INTENT(INOUT)  :: DSCALE2 
!       + + + LOCAL DEFINITIONS + + +                                     
        integer(I4B) :: i, n 
        integer(I4B) :: id, jc 
        integer(I4B) :: i0, i1 
        real(DP) :: v, c1, c2 
!       + + + FUNCTIONS + + +                                             
!       + + + CODE + + +                                                  
!                                                                       
!---------SCALE SCALE AMAT, X, AND B                                    
        IF (IOPT ==  0) THEN 
!-----------SYMMETRIC SCALING                                           
          SELECT CASE ( ISCL ) 
            CASE ( 1 ) 
              DO n = 1, NEQ 
                id   = IA(n) 
                v    = AMAT(id) 
                c1   = DONE / SQRT( ABS( v ) ) 
                DSCALE(n)  = c1 
                DSCALE2(n) = c1 
              END DO 
!               SCALE AMAT -- AMAT = DSCALE(row) * AMAT(i) * DSCALE2(col)
              DO n = 1, NEQ 
                c1 = DSCALE(n) 
                i0 = IA(n) 
                i1 = IA(n+1) - 1 
                DO i = i0, i1 
                  jc = JA(i) 
                  c2 = DSCALE2(jc) 
                  AMAT(i) = c1 * AMAT(i) * c2 
                END DO 
              END DO 
!-----------L-2 NORM SCALING                                            
            CASE ( 2 ) 
!               SCALE EACH ROW SO THAT THE L-2 NORM IS 1                
              DO n = 1, NEQ 
                c1 = DZERO 
                i0 = IA(n) 
                i1 = IA(n+1) - 1 
                DO i = i0, i1 
                  c1 = c1 + AMAT(i) * AMAT(i) 
                END DO 
                c1 = SQRT( c1 ) 
                IF (c1 ==  DZERO) THEN 
                  c1 = DONE 
                ELSE 
                  c1 = DONE / c1 
                END IF 
                DSCALE(n) = c1 
!                 INITIAL SCALING OF AMAT -- AMAT = DSCALE(row) * AMAT(i)
                DO i = i0, i1 
                  AMAT(i) = c1 * AMAT(i) 
                END DO 
              END DO 
!               SCALE EACH COLUMN SO THAT THE L-2 NORM IS 1             
              DO n = 1, NEQ 
                DSCALE2(n) = DZERO 
              END DO 
              c2 = DZERO 
              DO n = 1, NEQ 
                i0 = IA(n) 
                i1 = IA(n+1) - 1 
                DO i = i0, i1 
                  jc = JA(i) 
                  c2 = AMAT(i) 
                  DSCALE2(jc) = DSCALE2(jc) + c2 * c2 
                END DO 
              END DO 
              DO n = 1, NEQ 
                c2 = DSCALE2(n) 
                IF (c2 ==  DZERO) THEN 
                  c2 = DONE 
                ELSE 
                  c2 = DONE / SQRT( c2 ) 
                END IF 
                DSCALE2(n) = c2 
              END DO 
!               FINAL SCALING OF AMAT -- AMAT = DSCALE2(col) * AMAT(i)  
              DO n = 1, NEQ 
                i0 = IA(n) 
                i1 = IA(n+1) - 1 
                DO i = i0, i1 
                  jc = JA(i) 
                  c2 = DSCALE2(jc) 
                  AMAT(i) = c2 * AMAT(i) 
                END DO 
              END DO 
          END SELECT 
!-----------SCALE X AND B                                               
          DO n = 1, NEQ 
            c1    = DSCALE(n) 
            c2    = DSCALE2(n) 
            X(n)  = X(n) / c2 
            B(n)  = B(n) * c1 
          END DO 
!---------UNSCALE SCALE AMAT, X, AND B                                  
        ELSE 
          DO n = 1, NEQ 
            c1 = DSCALE(n) 
            i0 = IA(n) 
            i1 = IA(n+1) - 1 
!             UNSCALE AMAT                                              
            DO i = i0, i1 
              jc = JA(i) 
              c2 = DSCALE2(jc) 
              AMAT(i) = ( DONE / c1 ) * AMAT(i) * ( DONE / c2 ) 
            END DO 
!             UNSCALE X AND B                                           
            c2   = DSCALE2(n) 
            X(n) = X(n) * c2 
            B(n) = B(n) / c1 
          END DO 
        END IF 
!---------RETURN                                                        
        RETURN 
      END SUBROUTINE IMSLINEARSUB_SCALE 
!                                                                       
!-------ROUTINE TO UPDATE THE PRECONDITIONER                            
      SUBROUTINE IMSLINEARSUB_PCU(IOUT, NJA, NEQ, NIAPC, NJAPC, IPC, RELAX,     &
                                  AMAT, IA, JA, APC, IAPC, JAPC, IW, W,         &
                                  LEVEL, DROPTOL, NJLU, NJW, NWLU, JLU, JW, WLU)               
      use SimModule, only: ustop, store_error, count_errors
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(IN) :: IOUT 
        integer(I4B), INTENT(IN) :: NJA 
        integer(I4B), INTENT(IN) :: NEQ 
        integer(I4B), INTENT(IN) :: NIAPC 
        integer(I4B), INTENT(IN) :: NJAPC 
        integer(I4B), INTENT(IN) :: IPC 
        real(DP), INTENT(IN) :: RELAX 
        real(DP), DIMENSION(NJA),  INTENT(IN)     :: AMAT 
        integer(I4B), DIMENSION(NEQ+1), INTENT(IN)    :: IA 
        integer(I4B), DIMENSION(NJA), INTENT(IN)      :: JA 
        real(DP), DIMENSION(NJAPC), INTENT(INOUT) :: APC 
        integer(I4B), DIMENSION(NIAPC+1), INTENT(INOUT) :: IAPC 
        integer(I4B), DIMENSION(NJAPC), INTENT(INOUT)   :: JAPC 
        integer(I4B), DIMENSION(NIAPC), INTENT(INOUT)   :: IW 
        real(DP), DIMENSION(NIAPC), INTENT(INOUT) :: W 
        ! ILUT
        integer(I4B), INTENT(IN) :: LEVEL
        real(DP), INTENT(IN) :: DROPTOL
        integer(I4B), INTENT(IN) :: NJLU
        integer(I4B), INTENT(IN) :: NJW
        integer(I4B), INTENT(IN) :: NWLU
        integer(I4B), DIMENSION(NJLU), INTENT(INOUT) :: JLU
        integer(I4B), DIMENSION(NJW),  INTENT(INOUT) :: JW
        real(DP), DIMENSION(NWLU),  INTENT(INOUT) :: WLU
!       + + + LOCAL DEFINITIONS + + +                                     
        character(len=LINELENGTH) :: errmsg
        character(len=80), dimension(3) :: cerr
        integer(I4B) :: izero 
        integer(I4B) :: ierr
        real(DP) :: delta 
!       + + + FUNCTIONS + + +  
!       + + + DATA + + +
        DATA cerr  /'INCOMPREHENSIBLE ERROR - MATRIX MUST BE WRONG.              ', &
                    'INSUFFICIENT STORAGE IN ARRAYS ALU, JLU TO STORE FACTORS.   ', &
                    'ZERO ROW ENCOUNTERED.                                       '/
        
!       + + + FORMATS + + +                                               
 2000   FORMAT (/,' MATRIX IS SEVERELY NON-DIAGONALLY DOMINANT.',               &
     &          /,' ADDING SMALL VALUE TO PIVOT (IMSLINEARSUB_PCU)')           
!       + + + CODE + + +                                                  
        izero = 0 
        delta = DZERO 
        PCSCALE: DO
          SELECT CASE(IPC) 
!             ILU0 AND MILU0                                              
            CASE (1,2) 
              CALL IMSLINEARSUB_PCILU0(NJA, NEQ, AMAT, IA, JA,                   &
                                       APC, IAPC, JAPC, IW, W,                   &
                                       RELAX, izero, delta)                    
!             ILUT AND MILUT                                             
            CASE (3,4) 
              ierr = 0
              CALL IMSLINEARSUB_PCMILUT(NEQ, AMAT, JA, IA,                      &
                                        LEVEL, DROPTOL, RELAX,                  &
                                        APC, JLU, IW, NJAPC, WLU, JW, ierr,     &
                                        izero, delta)
              IF (ierr.NE.0) THEN
                write(errmsg,'(4x,a,1x,a)') &
                    '****ERROR. ILUT ERROR: ', cerr(-ierr)
                call store_error(errmsg)
                call parser%StoreErrorUnit()
                call ustop()
              END IF
!           ADDITIONAL PRECONDITIONERS                     
            CASE DEFAULT
              izero = 0
          END SELECT
          IF (izero < 1) THEN 
            EXIT PCSCALE 
          END IF 
          delta = 1.5D0 * delta + 0.001 
          izero = 0 
          IF (delta > DHALF) THEN 
            WRITE(IOUT,2000) 
            delta = DHALF 
            izero = 2 
          END IF
        END DO PCSCALE
!---------RETURN                                                        
        RETURN 
      END SUBROUTINE IMSLINEARSUB_PCU 
!                                                                       
!-------JACOBI PRECONDITIONER - INVERSE OF DIAGONAL                     
      SUBROUTINE IMSLINEARSUB_PCJ(NJA, NEQ, AMAT, APC, IA, JA) 
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(IN) :: NJA 
        integer(I4B), INTENT(IN) :: NEQ 
        real(DP), DIMENSION(NJA),  INTENT(IN)      :: AMAT 
        real(DP), DIMENSION(NEQ),  INTENT(INOUT)   :: APC 
        integer(I4B), DIMENSION(NEQ+1), INTENT(IN) :: IA 
        integer(I4B), DIMENSION(NJA),   INTENT(IN) :: JA 
!       + + + LOCAL DEFINITIONS + + +                                     
        integer(I4B) :: i, n 
        integer(I4B) :: ic0, ic1 
        integer(I4B) :: id 
        real(DP) :: tv 
!       + + + PARAMETERS + + +                                            
!       + + + FUNCTIONS + + +                                             
!       + + + CODE + + +                                                  
        DO n = 1, NEQ 
            ic0 = IA(n) 
            ic1 = IA(n+1) - 1 
            id = IA(n) 
            DO i = ic0, ic1 
              IF (JA(i) ==  n) THEN 
                id = i 
                EXIT 
              END IF 
            END DO 
            tv  = AMAT(id) 
            IF (ABS( tv ) > DZERO ) tv = DONE / tv 
            APC(n) = tv 
        END DO 
!---------RETURN                                                        
        RETURN 
      END SUBROUTINE IMSLINEARSUB_PCJ 
                                                                        
      SUBROUTINE IMSLINEARSUB_JACA(NEQ, A, D1, D2) 
        IMPLICIT NONE 
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(IN) :: NEQ 
        real(DP), DIMENSION(NEQ),  INTENT(IN)    :: A 
        real(DP), DIMENSION(NEQ),  INTENT(IN)    :: D1 
        real(DP), DIMENSION(NEQ),  INTENT(INOUT) :: D2 
!       + + + LOCAL DEFINITIONS + + +                                     
        integer(I4B) :: n 
        real(DP) :: tv 
!       + + + PARAMETERS + + +                                            
!       + + + FUNCTIONS + + +                                             
!       + + + CODE + + +                                                  
        DO n = 1, NEQ 
          tv     = A(n) * D1(n) 
          D2(n) = tv 
        END DO 
!---------RETURN                                                        
        RETURN 
      END SUBROUTINE IMSLINEARSUB_JACA 
                                                                        
      SUBROUTINE IMSLINEARSUB_PCILU0(NJA, NEQ, AMAT, IA, JA,                    &
                                     APC, IAPC, JAPC, IW, W,                    &
                                     RELAX, IZERO, DELTA)                        
        IMPLICIT NONE 
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(IN) :: NJA 
        integer(I4B), INTENT(IN) :: NEQ 
        real(DP), DIMENSION(NJA),  INTENT(IN)     :: AMAT 
        integer(I4B), DIMENSION(NEQ+1), INTENT(IN)    :: IA 
        integer(I4B), DIMENSION(NJA), INTENT(IN)      :: JA 
        real(DP), DIMENSION(NJA), INTENT(INOUT)   :: APC 
        integer(I4B), DIMENSION(NEQ+1), INTENT(INOUT) :: IAPC 
        integer(I4B), DIMENSION(NJA), INTENT(INOUT)   :: JAPC 
        integer(I4B), DIMENSION(NEQ), INTENT(INOUT)   :: IW 
        real(DP), DIMENSION(NEQ), INTENT(INOUT)   :: W 
        real(DP), INTENT(IN) :: RELAX 
        integer(I4B), INTENT(INOUT) :: IZERO 
        real(DP), INTENT(IN) :: DELTA 
!       + + + LOCAL DEFINITIONS + + +                                     
        integer(I4B) :: ic0, ic1 
        integer(I4B) :: iic0, iic1 
        integer(I4B) :: iu, iiu 
        integer(I4B) :: j, n 
        integer(I4B) :: jj 
        integer(I4B) :: jcol, jw 
        integer(I4B) :: jjcol 
        real(DP) :: drelax 
        real(DP) :: sd1 
        real(DP) :: tl 
        real(DP) :: rs 
        real(DP) :: d 
!       + + + PARAMETERS + + +                                            
!       + + + FUNCTIONS + + +                                             
!       + + + CODE + + +                                                  
        drelax = RELAX 
        DO n = 1, NEQ 
          IW(n)  = 0 
          W(n)   = DZERO 
        END DO 
        MAIN: DO n = 1, NEQ 
          ic0 = IA(n) 
          ic1 = IA(n+1) - 1 
          DO j = ic0, ic1 
            jcol      = JA(j) 
            IW(jcol) = 1 
            W(jcol) = W(jcol) + AMAT(j) 
          END DO 
          ic0 = IAPC(n) 
          ic1 = IAPC(n+1) - 1 
          iu  = JAPC(n) 
          rs   = DZERO 
          LOWER: DO j = ic0, iu-1 
            jcol     = JAPC(j) 
            iic0     = IAPC(jcol) 
            iic1     = IAPC(jcol+1) - 1 
            iiu      = JAPC(jcol) 
            tl       = W(jcol) * APC(jcol) 
            W(jcol) = tl 
            DO jj = iiu, iic1 
              jjcol = JAPC(jj) 
              jw    = IW(jjcol) 
              IF (jw.NE.0) THEN 
                W(jjcol) = W(jjcol) - tl * APC(jj) 
              ELSE 
                rs = rs + tl * APC(jj) 
              END IF 
            END DO 
          END DO LOWER 
!           DIAGONAL - CALCULATE INVERSE OF DIAGONAL FOR SOLUTION       
          d   = W(n) 
          tl  = ( DONE + DELTA ) * d - ( drelax * rs ) 
!-----------ENSURE THAT THE SIGN OF THE DIAGONAL HAS NOT CHANGED AND IS 
          sd1 = SIGN(d,tl) 
          IF (sd1.NE.d) THEN 
!             USE SMALL VALUE IF DIAGONAL SCALING IS NOT EFFECTIVE FOR
!             PIVOTS THAT CHANGE THE SIGN OF THE DIAGONAL               
            IF (IZERO > 1) THEN 
              tl = SIGN(DEM6,d) 
!             DIAGONAL SCALING CONTINUES TO BE EFFECTIVE                
            ELSE 
              IZERO = 1 
              EXIT MAIN 
            END IF 
          END IF 
          IF (ABS(tl) ==  DZERO) THEN 
!             USE SMALL VALUE IF DIAGONAL SCALING IS NOT EFFECTIVE FOR
!             ZERO PIVOTS                                               
            IF (IZERO > 1) THEN 
              tl = SIGN(DEM6,d) 
!             DIAGONAL SCALING CONTINUES TO BE EFFECTIVE FOR ELIMINATING 
            ELSE 
              IZERO = 1 
              EXIT MAIN 
            END IF 
          END IF 
          APC(n) = DONE / tl 
!           RESET POINTER FOR IW TO ZERO                                
          IW(n) = 0 
          W(n)  = DZERO 
          DO j = ic0, ic1 
            jcol = JAPC(j) 
            APC(j) = W(jcol) 
            IW(jcol) = 0 
            W(jcol) = DZERO 
          END DO 
      END DO MAIN 
!                                                                       
!---------RESET IZERO IF SUCCESSFUL COMPLETION OF MAIN                  
        IZERO = 0 
!                                                                       
!---------RETURN                                                        
        RETURN 
      END SUBROUTINE IMSLINEARSUB_PCILU0 
                                                                        
      SUBROUTINE IMSLINEARSUB_ILU0A(NJA, NEQ, APC, IAPC, JAPC, R, D) 
        IMPLICIT NONE 
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(IN) :: NJA 
        integer(I4B), INTENT(IN) :: NEQ 
        real(DP), DIMENSION(NJA),  INTENT(IN)  :: APC 
        integer(I4B), DIMENSION(NEQ+1), INTENT(IN) :: IAPC 
        integer(I4B), DIMENSION(NJA), INTENT(IN)   :: JAPC 
        real(DP), DIMENSION(NEQ),  INTENT(IN)     :: R 
        real(DP), DIMENSION(NEQ),  INTENT(INOUT)  :: D 
!       + + + LOCAL DEFINITIONS + + +                                     
        integer(I4B) :: ic0, ic1 
        integer(I4B) :: iu 
        integer(I4B) :: jcol 
        integer(I4B) :: j, n 
        real(DP) :: tv 
!       + + + FUNCTIONS + + +                                             
!       + + + CODE + + +                                                  
!         FORWARD SOLVE - APC * D = R                                   
        FORWARD: DO n = 1, NEQ 
          tv   = R(n) 
          ic0 = IAPC(n) 
          ic1 = IAPC(n+1) - 1 
          iu  = JAPC(n) - 1 
          LOWER: DO j = ic0, iu 
            jcol = JAPC(j) 
            tv    = tv - APC(j) * D(jcol) 
          END DO LOWER 
          D(n) = tv 
        END DO FORWARD 
!         BACKWARD SOLVE - D = D / U                                    
        BACKWARD: DO n = NEQ, 1, -1 
          ic0 = IAPC(n) 
          ic1 = IAPC(n+1) - 1 
          iu  = JAPC(n) 
          tv   = D(n) 
          UPPER: DO j = iu, ic1 
            jcol = JAPC(j) 
            tv    = tv - APC(j) * D(jcol) 
          END DO UPPER 
!           COMPUTE D FOR DIAGONAL - D = D / U                          
          D(n) =  tv * APC(n) 
        END DO BACKWARD 
!---------RETURN                                                        
        RETURN 
      END SUBROUTINE IMSLINEARSUB_ILU0A 
                                                                        
      SUBROUTINE IMSLINEARSUB_CG(ICNVG, ITMAX, INNERIT,                         &
                                 NEQ, NJA, NIAPC, NJAPC,                        &
                                 IPC, NITERC, ICNVGOPT, NORTH,                  &
                                 HCLOSE, RCLOSE, L2NORM0, EPFACT,               &
                                 IA0, JA0, A0, IAPC, JAPC, APC,                 &
                                 X, B, D, P, Q, Z,                              &
                                 NJLU, IW, JLU,                                 &
                                 NCONV, CONVNMOD, CONVMODSTART, LOCDV, LOCDR,   &
                                 CACCEL, ITINNER, CONVLOCDV, CONVLOCDR,         &
                                 DVMAX, DRMAX, CONVDVMAX, CONVDRMAX)                                        
        IMPLICIT NONE 
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(INOUT) :: ICNVG 
        integer(I4B), INTENT(IN)    :: ITMAX 
        integer(I4B), INTENT(INOUT) :: INNERIT 
        integer(I4B), INTENT(IN)    :: NEQ 
        integer(I4B), INTENT(IN)    :: NJA 
        integer(I4B), INTENT(IN)    :: NIAPC 
        integer(I4B), INTENT(IN)    :: NJAPC 
        integer(I4B), INTENT(IN)    :: IPC 
        integer(I4B), INTENT(INOUT) :: NITERC 
        integer(I4B), INTENT(IN)    :: ICNVGOPT 
        integer(I4B), INTENT(IN)    :: NORTH 
        real(DP), INTENT(IN) :: HCLOSE 
        real(DP), INTENT(IN) :: RCLOSE 
        real(DP), INTENT(IN) :: L2NORM0 
        real(DP), INTENT(IN) :: EPFACT 
        integer(I4B), DIMENSION(NEQ+1), INTENT(IN) :: IA0 
        integer(I4B), DIMENSION(NJA), INTENT(IN) :: JA0 
        real(DP), DIMENSION(NJA), INTENT(IN) :: A0 
        integer(I4B), DIMENSION(NIAPC+1), INTENT(IN) :: IAPC 
        integer(I4B), DIMENSION(NJAPC), INTENT(IN) :: JAPC 
        real(DP), DIMENSION(NJAPC), INTENT(IN) :: APC 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: X 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: B 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: D 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: P 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: Q 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: Z 
        ! ILUT
        integer(I4B), INTENT(IN) :: NJLU
        integer(I4B), DIMENSION(NIAPC), INTENT(IN) :: IW
        integer(I4B), DIMENSION(NJLU), INTENT(IN) :: JLU
        ! CONVERGENCE INFORMATION
        integer(I4B), INTENT(IN) :: NCONV
        integer(I4B), INTENT(IN) :: CONVNMOD
        integer(I4B), DIMENSION(CONVNMOD+1), INTENT(INOUT) ::CONVMODSTART
        integer(I4B), DIMENSION(CONVNMOD), INTENT(INOUT) :: LOCDV
        integer(I4B), DIMENSION(CONVNMOD), INTENT(INOUT) :: LOCDR
        character(len=31), DIMENSION(NCONV), INTENT(INOUT) :: CACCEL
        integer(I4B), DIMENSION(NCONV), INTENT(INOUT) :: ITINNER
        integer(I4B), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVLOCDV
        integer(I4B), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVLOCDR
        real(DP), DIMENSION(CONVNMOD), INTENT(INOUT) :: DVMAX
        real(DP), DIMENSION(CONVNMOD), INTENT(INOUT) :: DRMAX
        real(DP), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVDVMAX
        real(DP), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVDRMAX
!       + + + LOCAL DEFINITIONS + + + 
        LOGICAL :: LORTH
        logical :: lsame 
        character(len=31) :: cval
        integer(I4B) :: n 
        integer(I4B) :: iiter 
        integer(I4B) :: xloc, rloc
        integer(I4B) :: im, im0, im1
        real(DP) :: tv 
        real(DP) :: deltax 
        real(DP) :: rmax 
        real(DP) :: l2norm 
        real(DP) :: rcnvg 
        real(DP) :: denom
        real(DP) :: alpha, beta 
        real(DP) :: rho, rho0 
!       + + + PARAMETERS + + +                                            
!       + + + FUNCTIONS + + +                                         
!                                                                       
!         + + + CODE + + +                                              
        rho0 = DZERO 
        rho = DZERO 
        INNERIT  = 0 
!                                                                       
!-------INNER ITERATION                                                 
        INNER: DO iiter = 1, itmax 
           INNERIT = INNERIT + 1 
           NITERC  = NITERC  + 1 
!----------APPLY PRECONDITIONER                                         
          SELECT CASE (IPC) 
!             ILU0 AND MILU0              
            CASE (1,2) 
              CALL IMSLINEARSUB_ILU0A(NJA, NEQ, APC, IAPC, JAPC, D, Z) 
!             ILUT AND MILUT
            CASE (3,4)
              CALL IMSLINEARSUB_PCMILUT_LUSOL(NEQ, D, Z, APC, JLU, IW) 
          END SELECT 
          rho = IMSLINEARSUB_DP(NEQ, D, Z) 
!-----------COMPUTE DIRECTIONAL VECTORS                                 
          IF (IITER ==  1) THEN 
            DO n = 1, NEQ 
              P(n) = Z(n) 
            END DO 
          ELSE
            !denom = rho0 + SIGN(DPREC,rho0)
            !beta = rho / denom
            beta = rho / rho0 
            DO n = 1, NEQ 
              P(n) = Z(n) + beta * P(n) 
            END DO 
          END IF 
!-----------COMPUTE ITERATES                                            
!           UPDATE Q                                                   
          CALL IMSLINEARSUB_MV(NJA, NEQ, A0, P, Q, IA0, JA0) 
          denom =  IMSLINEARSUB_DP(NEQ, P, Q)
          denom = denom + SIGN(DPREC, denom) 
          alpha = rho / denom
!-----------UPDATE X AND RESIDUAL                                       
          deltax = DZERO 
          rmax   = DZERO 
          l2norm = DZERO 
          DO im = 1, CONVNMOD
            DVMAX(im) = DZERO
            DRMAX(im) = DZERO
          END DO
          im  = 1
          im0 = CONVMODSTART(1)
          im1 = CONVMODSTART(2)
          DO n = 1, NEQ
            ! -- determine current model index
            if (n == im1) then
              im = im + 1
              im0 = CONVMODSTART(im)
              im1 = CONVMODSTART(im+1)
            end if
            ! -- identify deltax and rmax
            tv      = alpha * P(n) 
            X(n)  = X(n) + tv 
            IF (ABS(tv) > ABS(deltax)) THEN
              deltax = tv
              xloc = n
            END IF
            IF (ABS(tv) > ABS(DVMAX(im))) THEN
              DVMAX(im) = tv
              LOCDV(im) = n
            END IF
            tv      = D(n) 
            tv      = tv - alpha * Q(n) 
            D(n)  = tv 
            IF (ABS(tv) > ABS(rmax)) THEN
              rmax = tv
              rloc = n
            END IF
            IF (ABS(tv) > ABS(DRMAX(im))) THEN
              DRMAX(im) = tv
              LOCDR(im) = n
            END IF
            l2norm = l2norm + tv * tv 
          END DO 
          l2norm = SQRT(l2norm) 
!-----------SAVE SOLVER CONVERGENCE INFORMATION
          IF (NCONV > 1) THEN
            n = NITERC
            WRITE(cval, '(g15.7)') alpha
            CACCEL(n) = cval
            ITINNER(n)    = iiter
            DO im = 1, CONVNMOD
              CONVLOCDV(im, n) = LOCDV(im)
              CONVLOCDR(im, n) = LOCDR(im)
              CONVDVMAX(im, n) = DVMAX(im)
              CONVDRMAX(im, n) = DRMAX(im)
            END DO
          END IF
!-----------TEST FOR SOLVER CONVERGENCE                                 
          IF (ICNVGOPT ==  2 .OR. ICNVGOPT ==  3 .OR. ICNVGOPT ==  4) THEN 
            rcnvg = l2norm 
          ELSE 
            rcnvg = rmax 
          END IF 
          CALL IMSLINEARSUB_TESTCNVG(ICNVGOPT, ICNVG, INNERIT,                  &
                                     deltax, rcnvg,                             &
                                     L2NORM0, EPFACT, HCLOSE, RCLOSE)         
!
!           CHECK FOR EXACT SOLUTION                                    
          IF (rcnvg ==  DZERO) ICNVG = 1 
          IF (ICNVG.NE.0) EXIT INNER 
!-----------CHECK THAT CURRENT AND PREVIOUS rho ARE DIFFERENT           
          lsame = IS_SAME(rho, rho0) 
          IF (lsame) THEN 
            EXIT INNER 
          END IF 
!-----------RECALCULATE THE RESIDUAL
          IF (NORTH > 0) THEN
            LORTH = mod(iiter+1,NORTH) == 0
            IF (LORTH) THEN
              CALL IMSLINEARSUB_MV(NJA, NEQ, A0, X, D, IA0, JA0)
              CALL IMSLINEARSUB_AXPY(NEQ, B, -DONE, D, D)
            END IF
          END IF
!-----------SAVE CURRENT INNER ITERATES                                 
          rho0 = rho 
        END DO INNER 
!---------RESET ICNVG        
        IF (ICNVG < 0) ICNVG = 0
!---------RETURN                                                        
        RETURN 
      END SUBROUTINE IMSLINEARSUB_CG 
                                                                        
      SUBROUTINE IMSLINEARSUB_BCGS(ICNVG, ITMAX, INNERIT,                       &
                                   NEQ, NJA, NIAPC, NJAPC,                      &
                                   IPC, NITERC, ICNVGOPT, NORTH, ISCL, DSCALE,  &
                                   HCLOSE, RCLOSE, L2NORM0, EPFACT,             &
                                   IA0, JA0, A0, IAPC, JAPC, APC,               &
                                   X, B, D, P, Q,                               &
                                   T, V, DHAT, PHAT, QHAT,                      &
                                   NJLU, IW, JLU,                               &
                                   NCONV, CONVNMOD, CONVMODSTART, LOCDV, LOCDR, &
                                   CACCEL, ITINNER, CONVLOCDV, CONVLOCDR,       &
                                   DVMAX, DRMAX, CONVDVMAX, CONVDRMAX)                                
        IMPLICIT NONE 
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(INOUT) :: ICNVG 
        integer(I4B), INTENT(IN)    :: ITMAX 
        integer(I4B), INTENT(INOUT) :: INNERIT 
        integer(I4B), INTENT(IN)    :: NEQ 
        integer(I4B), INTENT(IN)    :: NJA 
        integer(I4B), INTENT(IN)    :: NIAPC 
        integer(I4B), INTENT(IN)    :: NJAPC 
        integer(I4B), INTENT(IN)    :: IPC 
        integer(I4B), INTENT(INOUT) :: NITERC 
        integer(I4B), INTENT(IN)    :: ICNVGOPT 
        integer(I4B), INTENT(IN)    :: NORTH
        integer(I4B), INTENT(IN)    :: ISCL 
        real(DP), DIMENSION(NEQ), INTENT(IN) :: DSCALE 
        real(DP), INTENT(IN) :: HCLOSE 
        real(DP), INTENT(IN) :: RCLOSE 
        real(DP), INTENT(IN) :: L2NORM0 
        real(DP), INTENT(IN) :: EPFACT 
        integer(I4B), DIMENSION(NEQ+1), INTENT(IN) :: IA0 
        integer(I4B), DIMENSION(NJA), INTENT(IN) :: JA0 
        real(DP), DIMENSION(NJA), INTENT(IN) :: A0 
        integer(I4B), DIMENSION(NIAPC+1), INTENT(IN) :: IAPC 
        integer(I4B), DIMENSION(NJAPC), INTENT(IN) :: JAPC 
        real(DP), DIMENSION(NJAPC), INTENT(IN) :: APC 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: X 
        real(DP), DIMENSION(NEQ), INTENT(IN) :: B 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: D 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: P 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: Q 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: T 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: V 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: DHAT 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: PHAT 
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: QHAT
        ! ILUT
        integer(I4B), INTENT(IN) :: NJLU
        integer(I4B), DIMENSION(NIAPC), INTENT(IN) :: IW
        integer(I4B), DIMENSION(NJLU), INTENT(IN) :: JLU
        ! CONVERGENCE INFORMATION
        integer(I4B), INTENT(IN) :: NCONV
        integer(I4B), INTENT(IN) :: CONVNMOD
        integer(I4B), DIMENSION(CONVNMOD+1), INTENT(INOUT) ::CONVMODSTART
        integer(I4B), DIMENSION(CONVNMOD), INTENT(INOUT) :: LOCDV
        integer(I4B), DIMENSION(CONVNMOD), INTENT(INOUT) :: LOCDR
        character(len=31), DIMENSION(NCONV), INTENT(INOUT) :: CACCEL
        integer(I4B), DIMENSION(NCONV), INTENT(INOUT) :: ITINNER
        integer(I4B), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVLOCDV
        integer(I4B), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVLOCDR
        real(DP), DIMENSION(CONVNMOD), INTENT(INOUT) :: DVMAX
        real(DP), DIMENSION(CONVNMOD), INTENT(INOUT) :: DRMAX
        real(DP), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVDVMAX
        real(DP), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVDRMAX
!       + + + LOCAL DEFINITIONS + + +  
        LOGICAL :: LORTH
        logical :: lsame 
        character(len=15) :: cval1, cval2
        integer(I4B) :: n 
        integer(I4B) :: iiter 
        integer(I4B) :: xloc, rloc
        integer(I4B) :: im, im0, im1
        real(DP) :: tv 
        real(DP) :: deltax 
        real(DP) :: rmax
        real(DP) :: l2norm 
        real(DP) :: rcnvg 
        real(DP) :: alpha, alpha0 
        real(DP) :: beta 
        real(DP) :: rho, rho0 
        real(DP) :: omega, omega0 
        real(DP) :: numer, denom
!       + + + PARAMETERS + + +                                            
!       + + + FUNCTIONS + + +                                         
!                                                                       
!         + + + CODE + + +                                              
        INNERIT  = 0 
                                                                        
        alpha  = DZERO
        alpha0 = DZERO
        beta   = DZERO 
        rho    = DZERO 
        rho0   = DZERO
        omega  = DZERO
        omega0 = DZERO
!                                                                       
!-------SAVE INITIAL RESIDUAL                                           
        DO n = 1, NEQ 
          DHAT(n) = D(n)
        END DO
!                                                                       
!-------INNER ITERATION                                                 
        INNER: DO iiter = 1, itmax 
           INNERIT = INNERIT + 1 
           NITERC = NITERC + 1 
!----------CALCULATE rho                                                
          rho = IMSLINEARSUB_DP(NEQ, DHAT, D) 
!-----------COMPUTE DIRECTIONAL VECTORS                                 
          IF (IITER ==  1) THEN 
            DO n = 1, NEQ 
              P(n) = D(n) 
            END DO 
          ELSE 
            beta = ( rho / rho0 ) * ( alpha0 / omega0 ) 
            DO n = 1, NEQ 
              P(n) = D(n) + beta * ( P(n) - omega0 * V(n) ) 
            END DO 
          END IF 
!----------APPLY PRECONDITIONER TO UPDATE PHAT                          
          SELECT CASE (IPC) 
!             ILU0 AND MILU0
            CASE (1,2) 
              CALL IMSLINEARSUB_ILU0A(NJA, NEQ, APC, IAPC, JAPC, P, PHAT) 
!             ILUT AND MILUT
            CASE (3,4)
              CALL IMSLINEARSUB_PCMILUT_LUSOL(NEQ, P, PHAT, APC, JLU, IW) 
          END SELECT 
!-----------COMPUTE ITERATES                                            
!           UPDATE V WITH A AND PHAT                                    
          CALL IMSLINEARSUB_MV(NJA, NEQ, A0, PHAT, V, IA0, JA0) 
!           UPDATE alpha WITH DHAT AND V                                
          denom = IMSLINEARSUB_DP(NEQ, DHAT, V) 
          denom = denom + SIGN(DPREC, denom) 
          alpha = rho / denom 
!-----------UPDATE Q                                                    
          DO n = 1, NEQ 
            Q(n) = D(n) - alpha * V(n)  
          END DO 
!!-----------CALCULATE INFINITY NORM OF Q - TEST FOR TERMINATION         
!!           TERMINATE IF rmax IS LESS THAN MACHINE PRECISION (DPREC) 
!          rmax = DZERO 
!          DO n = 1, NEQ 
!              tv = Q(n) 
!              IF (ISCL.NE.0 ) tv = tv / DSCALE(n) 
!              IF (ABS(tv) > ABS(rmax) ) rmax = tv 
!          END DO 
!          IF (ABS(rmax).LE.DPREC) THEN 
!            deltax = DZERO 
!            DO n = 1, NEQ 
!              tv      = alpha * PHAT(n) 
!              IF (ISCL.NE.0) THEN 
!                tv = tv * DSCALE(n) 
!              END IF 
!              X(n)  = X(n) + tv 
!              IF (ABS(tv) > ABS(deltax) ) deltax = tv 
!            END DO 
!            CALL IMSLINEARSUB_TESTCNVG(ICNVGOPT, ICNVG, INNERIT,                &
!                                       deltax, rmax,                            &
!                                       rmax, EPFACT, HCLOSE, RCLOSE )          
!            IF (ICNVG.NE.0 ) EXIT INNER 
!          END IF 
!-----------APPLY PRECONDITIONER TO UPDATE QHAT                         
          SELECT CASE (IPC) 
!            ILU0 AND MILU0            
            CASE (1,2) 
              CALL IMSLINEARSUB_ILU0A(NJA, NEQ, APC, IAPC, JAPC, Q, QHAT) 
!             ILUT AND MILUT
            CASE (3,4)
              CALL IMSLINEARSUB_PCMILUT_LUSOL(NEQ, Q, QHAT, APC, JLU, IW)
          END SELECT
!           UPDATE T WITH A AND QHAT                                    
          CALL IMSLINEARSUB_MV(NJA, NEQ, A0, QHAT, T, IA0, JA0) 
!-----------UPDATE omega                                                
          numer = IMSLINEARSUB_DP(NEQ, T, Q) 
          denom = IMSLINEARSUB_DP(NEQ, T, T)
          denom = denom + SIGN(DPREC,denom) 
          omega = numer / denom 
!-----------UPDATE X AND RESIDUAL                                       
          deltax = DZERO 
          rmax   = DZERO 
          l2norm = DZERO 
          DO im = 1, CONVNMOD
            DVMAX(im) = DZERO
            DRMAX(im) = DZERO
          END DO
          im  = 1
          im0 = CONVMODSTART(1)
          im1 = CONVMODSTART(2)
          DO n = 1, NEQ 
            ! -- determine current model index
            if (n == im1) then
              im = im + 1
              im0 = CONVMODSTART(im)
              im1 = CONVMODSTART(im+1)
            end if
!-------------X AND DX                                                  
            tv = alpha * PHAT(n) + omega * QHAT(n) 
            X(n)  = X(n) + tv 
            IF (ISCL.NE.0) THEN 
              tv = tv * DSCALE(n) 
            END IF 
            IF (ABS(tv) > ABS(deltax)) THEN
              deltax = tv
              xloc = n
            END IF
            IF (ABS(tv) > ABS(DRMAX(im))) THEN
              DVMAX(im) = tv
              LOCDV(im) = n
            END IF
!-------------RESIDUAL                                                  
            tv      = Q(n) - omega * T(n) 
            D(n)  = tv 
            IF (ISCL.NE.0) THEN 
              tv = tv / DSCALE(n) 
            END IF 
            IF (ABS(tv) > ABS(rmax)) THEN
              rmax = tv
              rloc = n
            END IF
            IF (ABS(tv) > ABS(DRMAX(im))) THEN
              DRMAX(im) = tv
              LOCDR(im) = n
            END IF
            l2norm = l2norm + tv * tv 
          END DO
          l2norm = sqrt(l2norm)
!-----------SAVE SOLVER CONVERGENCE INFORMATION
          IF (NCONV > 1) THEN
            n = NITERC
            WRITE(cval1,'(g15.7)') alpha
            WRITE(cval2,'(g15.7)') omega
            CACCEL(n) = trim(adjustl(cval1)) // ',' // trim(adjustl(cval2))
            ITINNER(n)    = iiter
            DO im = 1, CONVNMOD
              CONVLOCDV(im, n) = LOCDV(im)
              CONVLOCDR(im, n) = LOCDR(im)
              CONVDVMAX(im, n) = DVMAX(im)
              CONVDRMAX(im, n) = DRMAX(im)
            END DO
          END IF
!-----------TEST FOR SOLVER CONVERGENCE                                 
          IF (ICNVGOPT ==  2 .OR. ICNVGOPT ==  3 .OR. ICNVGOPT ==  4) THEN 
            rcnvg = l2norm 
          ELSE 
            rcnvg = rmax 
          END IF 
          CALL IMSLINEARSUB_TESTCNVG(ICNVGOPT, ICNVG, INNERIT,                  &
                                     deltax, rcnvg,                             &
                                     L2NORM0, EPFACT, HCLOSE, RCLOSE)         
!           CHECK FOR EXACT SOLUTION                                    
          IF (rcnvg ==  DZERO) ICNVG = 1 
          IF (ICNVG.NE.0) EXIT INNER
!-----------CHECK THAT CURRENT AND PREVIOUS rho, alpha, AND omega ARE 
!           DIFFERENT
          lsame = IS_SAME(rho, rho0) 
          IF (lsame) THEN 
            EXIT INNER 
          END IF 
          lsame = IS_SAME(alpha, alpha0) 
          IF (lsame) THEN 
            EXIT INNER 
          END IF 
          lsame = IS_SAME(omega, omega0) 
          IF (lsame) THEN 
            EXIT INNER 
          END IF 
!-----------RECALCULATE THE RESIDUAL
          IF (NORTH > 0) THEN
            LORTH = mod(iiter+1,NORTH) == 0
            IF (LORTH) THEN
              CALL IMSLINEARSUB_MV(NJA, NEQ, A0,X , D, IA0, JA0)
              CALL IMSLINEARSUB_AXPY(NEQ, B, -DONE, D, D)
              !DO n = 1, NEQ
              !  tv   = D(n)
              !  D(n) = B(n) - tv
              !END DO
            END IF
          END IF
!-----------SAVE CURRENT INNER ITERATES                                 
          rho0   = rho
          alpha0 = alpha
          omega0 = omega
        END DO INNER
!---------RESET ICNVG        
        IF (ICNVG < 0) ICNVG = 0
!---------RETURN                                                        
        RETURN 
      END SUBROUTINE IMSLINEARSUB_BCGS 
!                                                                       
!---------TEST FOR SOLVER CONVERGENCE                                   
        SUBROUTINE IMSLINEARSUB_TESTCNVG(Icnvgopt, Icnvg, Iiter,                &
                                         Hmax, Rmax,                            &
                                         Rmax0, Epfact, Hclose, Rclose )         
        IMPLICIT NONE 
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(IN)         :: Icnvgopt 
        integer(I4B), INTENT(INOUT)      :: Icnvg 
        integer(I4B), INTENT(IN)         :: Iiter
        real(DP), INTENT(IN) :: Hmax 
        real(DP), INTENT(IN) :: Rmax 
        real(DP), INTENT(IN) :: Rmax0 
        real(DP), INTENT(IN) :: Epfact 
        real(DP), INTENT(IN) :: Hclose 
        real(DP), INTENT(IN) :: Rclose 
!       + + + LOCAL DEFINITIONS + + +                                     
!       + + + FUNCTIONS + + +                                             
!       + + + CODE + + +                                                  
        IF (Icnvgopt ==  0) THEN 
          IF (ABS(Hmax) <=  Hclose .AND. ABS(Rmax) <=  Rclose) THEN 
            Icnvg = 1 
          END IF 
        ELSE IF (Icnvgopt ==  1) THEN 
          IF (ABS(Hmax) <=  Hclose .AND. ABS(Rmax) <=  Rclose .AND.               &
              iiter ==  1) THEN 
            Icnvg = 1 
          END IF 
        ELSE IF (Icnvgopt ==  2) THEN 
          IF (ABS(Hmax) <=  Hclose .OR. Rmax <=  Rclose) THEN 
            Icnvg = 1 
          ELSE IF (Rmax <=  Rmax0*Epfact) THEN 
            Icnvg = -1 
          END IF
        ELSE IF (Icnvgopt ==  3) THEN 
          IF (ABS(Hmax) <=  Hclose) THEN
            Icnvg = 1 
          ELSE IF (Rmax <=  Rmax0*Rclose) THEN  
            Icnvg = -1 
          END IF
        ELSE IF (Icnvgopt ==  4) THEN 
          IF (ABS(Hmax) <=  Hclose .AND. Rmax <=  Rclose) THEN
            Icnvg = 1 
          ELSE IF (Rmax <=  Rmax0*Epfact) THEN  
            Icnvg = -1 
          END IF
       END IF 
!---------RETURN                                                        
        RETURN 
      END SUBROUTINE IMSLINEARSUB_TESTCNVG 
!                                                                       
!---------GENERATE IAPC AND JAPC FROM IA AND JA                         
!         JAPC(1:NEQ) HAS THE POSITION OF THE UPPER ENTRY FOR A ROW     
!         JAPC(NEQ+1:NJA) IS THE COLUMN POSITION FOR ENTRY              
!         APC(1:NEQ) PRECONDITIONED INVERSE OF THE DIAGONAL             
!         APC(NEQ+1:NJA) PRECONDITIONED ENTRIES FOR OFF DIAGONALS       
        SUBROUTINE IMSLINEARSUB_PCCRS(NEQ, NJA, IA, JA,                         &
                                      IAPC,JAPC)                               
        IMPLICIT NONE 
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(IN)         :: NEQ 
        integer(I4B), INTENT(IN)         :: NJA 
        integer(I4B), DIMENSION(NEQ+1), INTENT(IN)    :: IA 
        integer(I4B), DIMENSION(NJA), INTENT(IN)      :: JA 
        integer(I4B), DIMENSION(NEQ+1), INTENT(INOUT) :: IAPC 
        integer(I4B), DIMENSION(NJA), INTENT(INOUT)   :: JAPC 
!       + + + LOCAL DEFINITIONS + + +                                     
        integer(I4B) :: n, j 
        integer(I4B) :: i0, i1 
        integer(I4B) :: nlen 
        integer(I4B) :: ic,ip 
        integer(I4B) :: jcol 
        integer(I4B), DIMENSION(:), ALLOCATABLE :: iarr 
!       + + + FUNCTIONS + + +                                             
!       + + + CODE + + +                                                  
        ip = NEQ + 1 
        DO n = 1, NEQ 
          i0 = IA(n) 
          i1 = IA(n+1) - 1 
          nlen = i1 - i0 
          ALLOCATE( iarr(nlen) ) 
          ic = 0 
          DO j = i0, i1 
            jcol = JA(j) 
            IF (jcol ==  n) CYCLE 
            ic = ic + 1 
            iarr(ic) = jcol 
          END DO 
          CALL IMSLINEARSUB_ISORT(nlen,iarr) 
          IAPC(n) = ip 
          DO j = 1, nlen 
            jcol = iarr(j) 
            JAPC(ip) = jcol 
            ip = ip + 1 
          END DO 
          DEALLOCATE(iarr) 
        END DO 
        IAPC(NEQ+1) = NJA + 1 
!---------POSITION OF THE FIRST UPPER ENTRY FOR ROW                     
        DO n = 1, NEQ 
          i0 = IAPC(n) 
          i1 = IAPC(n+1) - 1 
          JAPC(n) = IAPC(n+1) 
          DO j = i0, i1 
            jcol = JAPC(j) 
            IF (jcol > n) THEN 
              JAPC(n) = j 
              EXIT 
            END IF 
          END DO 
        END DO 
!---------RETURN                                                        
        RETURN 
      END SUBROUTINE IMSLINEARSUB_PCCRS 
!                                                                       
!-------SIMPLE IN-PLACE SORTING ROUTINE FOR AN INTEGER ARRAY             
      SUBROUTINE IMSLINEARSUB_ISORT(NVAL, IARRAY) 
        IMPLICIT NONE 
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B),INTENT(IN) :: NVAL 
        integer(I4B),DIMENSION(NVAL),INTENT(INOUT) :: IARRAY 
!       + + + LOCAL DEFINITIONS + + +                                     
        integer(I4B) :: i, j, itemp 
!       + + + FUNCTIONS + + +                                             
!       + + + CODE + + +                                                  
        DO i = 1, NVAL-1 
            DO j = i+1, NVAL 
                if(IARRAY(i) > IARRAY(j)) then 
                    itemp = IARRAY(j) 
                    IARRAY(j) = IARRAY(i) 
                    IARRAY(i) = itemp 
                END IF 
            END DO                                                      
        END DO                                                          
!---------RETURN                                                        
        RETURN                                                          
      END SUBROUTINE IMSLINEARSUB_ISORT
!      
!-------INITIALIZE REAL VECTOR      
      SUBROUTINE IMSLINEARSUB_SETX(NR, D1, C)
        IMPLICIT NONE
!     + + + DUMMY ARGUMENTS + + +
        integer(I4B), INTENT(IN) :: NR
        real(DP), DIMENSION(NR),  INTENT(INOUT) :: D1
        real(DP),  INTENT(IN)                   :: C
!     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
!     + + + FUNCTIONS + + +
!     + + + CODE + + +
!
        DO n = 1, NR
          D1(n) = C
        END DO
!---------RETURN
        RETURN
      END SUBROUTINE IMSLINEARSUB_SETX
                                                                        
!       COPY ONE real(DP) VECTOR TO ANOTHER                      
      SUBROUTINE IMSLINEARSUB_DCOPY(NR, V, R)                                 
        IMPLICIT NONE                                                   
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(IN) :: NR                                       
        real(DP), DIMENSION(NR), INTENT(IN)    :: V              
        real(DP), DIMENSION(NR), INTENT(INOUT) :: R              
!       + + + LOCAL DEFINITIONS + + +                                     
        integer(I4B) :: n                                                    
!       + + + FUNCTIONS + + +                                             
!       + + + CODE + + +                                                  
        DO n = 1, NR                                                    
          R(n) = V(n)                                                   
        END DO                                                          
!---------RETURN                                                        
        RETURN                                                          
      END SUBROUTINE IMSLINEARSUB_DCOPY                                       
                                                                        
!       COPY ONE INTEGER VECTOR TO ANOTHER                              
      SUBROUTINE IMSLINEARSUB_ICOPY(NR, V, R)                                 
        IMPLICIT NONE                                                   
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(IN) :: NR                                       
        integer(I4B), DIMENSION(NR), INTENT(IN)    :: V                      
        integer(I4B), DIMENSION(NR), INTENT(INOUT) :: R                      
!       + + + LOCAL DEFINITIONS + + +                                     
        integer(I4B) :: n                                                    
!       + + + FUNCTIONS + + +                                             
!       + + + CODE + + +                                                  
        DO n = 1, NR                                                    
          R(n) = V(n)                                                   
        END DO                                                          
!---------RETURN                                                        
        RETURN                                                          
      END SUBROUTINE IMSLINEARSUB_ICOPY                                       
!      
!-------SCALE A REAL VECTOR WITH A CONSTANT      
      SUBROUTINE IMSLINEARSUB_RSCAL(NR, C, D1)
        IMPLICIT NONE
!     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NR
        real(DP), INTENT(IN) :: C
        real(DP), DIMENSION(NR),  INTENT(INOUT) :: D1
!     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
!     + + + FUNCTIONS + + +
!     + + + CODE + + +
        DO n = 1, NR
          D1(n) = C * D1(n)
        END DO
!---------RETURN
        RETURN
      END SUBROUTINE IMSLINEARSUB_RSCAL

      
      SUBROUTINE IMSLINEARSUB_MV(NJA, NEQ, A, D1, D2, IA, JA)                        
        IMPLICIT NONE                                                   
!       + + + DUMMY ARGUMENTS + + +                                       
        integer(I4B), INTENT(IN) :: NJA                                      
        integer(I4B), INTENT(IN) :: NEQ                                      
        real(DP), DIMENSION(NJA),  INTENT(IN)    :: A            
        real(DP), DIMENSION(NEQ),  INTENT(IN)    :: D1           
        real(DP), DIMENSION(NEQ),  INTENT(INOUT) :: D2           
        integer(I4B), DIMENSION(NEQ+1), INTENT(IN) :: IA                     
        integer(I4B), DIMENSION(NJA), INTENT(IN)   :: JA                     
!       + + + LOCAL DEFINITIONS + + +                                     
        integer(I4B) :: ic0, ic1                                             
        integer(I4B) :: icol                                                 
        integer(I4B) :: m, n                                                 
        real(DP) :: tv                                           
!       + + + PARAMETERS + + +                                            
!       + + + FUNCTIONS + + +                                             
!       + + + CODE + + +                                                  
        DO n = 1, NEQ                                                   
!           ADD DIAGONAL AND OFF-DIAGONAL TERMS                         
          tv     = DZERO                                                
          ic0   = IA(n)                                                 
          ic1   = IA(n+1)-1                                             
          DO m = ic0, ic1                                               
            icol = JA(m)                                                
            tv  = tv + A(m) * D1(icol)                                  
          END DO                                                        
          D2(n) = tv                                                    
        END DO                                                          
!---------RETURN                                                        
        RETURN                                                          
      END SUBROUTINE IMSLINEARSUB_MV  
      
      SUBROUTINE IMSLINEARSUB_AXPY(NEQ, D1, DC, D2, DR)
        IMPLICIT NONE
!     + + + DUMMY ARGUMENTS + + +
        integer(I4B), INTENT(IN) :: NEQ
        real(DP), DIMENSION(NEQ), INTENT(IN)    :: D1
        real(DP), INTENT(IN) :: DC
        real(DP), DIMENSION(NEQ), INTENT(IN)    :: D2
        real(DP), DIMENSION(NEQ), INTENT(INOUT) :: DR
!     + + + LOCAL DEFINITIONS + + +
        integer(I4B) :: n
!     + + + FUNCTIONS + + +
!     + + + CODE + + +
         DO n = 1, NEQ
          DR(n) = D1(n) + DC * D2(n)
         END DO
!---------RETURN
        RETURN
      END SUBROUTINE IMSLINEARSUB_AXPY

      
    FUNCTION IMSLINEARSUB_DP(neq, a, b) RESULT(c)
      ! -- return variable
      real(DP) :: c
!     + + + dummy arguments + + +
      integer(I4B), intent(in) :: neq
      real(DP), dimension(neq),  intent(in) :: a
      real(DP), dimension(neq),  intent(in) :: b
!     + + + local definitions + + +
      integer(I4B) :: n
!     + + + parameters + + +
!     + + + functions + + +
!     + + + code + + +
      c = DZERO
      do n = 1, neq
        c = c + a(n) * b(n)
      end do
      !---------return
      return
    END FUNCTION IMSLINEARSUB_DP

      
    FUNCTION IMSLINEARSUB_RNRM2(neq, a) RESULT(c)
      ! -- return variable
      real(DP) :: c
!     + + + dummy arguments + + +
      integer(I4B), intent(in) :: neq
      real(DP), dimension(neq),  intent(in) :: a
!     + + + local definitions + + +
      integer(I4B) :: n
      real(DP) :: ssq
      real(DP) :: scale
      real(DP) :: norm
      real(DP) :: absan
!     + + + parameters + + +
!     + + + functions + + +
!     + + + code + + +
      if (neq < 1) then
        norm = DZERO
      else if (neq == 1) then
        norm = ABS(a(1))
      else
        scale = DZERO
        ssq = DONE
        do n = 1, neq
          if (a(n) /= DZERO) then
            absan = abs(a(n))
            if (scale < absan) then
              ssq = DONE + ssq * (scale/absan)**2
              scale = absan
            else
              ssq = ssq + (absan/scale)**2
            end if
          end if
        end do
        norm = scale * sqrt(ssq)
      END IF
      c = norm
      !---------return
      return
    END FUNCTION IMSLINEARSUB_RNRM2
!
!    
!-------BEGINNING OF SUBROUTINES FROM OTHER LIBRARIES                   
                                                                        
      !       SUBSET OF SPARSKIT VERSION 2 SOURCE CODE
      !
      !  SPARSKIT VERSION 2 SUBROUTINES INCLUDED INCLUDE:
      !
      !    1 - IMSLINEARSUB_PCMILUT
      !    2 - IMSLINEARSUB_PCMILUT_LUSOL
      !    3 - IMSLINEARSUB_PCMILUT_QSPLIT
      !
      !-----------------------------------------------------------------------
      !                   S P A R S K I T   V E R S I O N  2.
      !-----------------------------------------------------------------------
      !
      !Latest update : Tue Mar  8 11:01:12 CST 2005
      !
      !-----------------------------------------------------------------------
      !
      !Welcome  to SPARSKIT  VERSION  2.  SPARSKIT is  a  package of  FORTRAN
      !subroutines  for working  with  sparse matrices.  It includes  general
      !sparse  matrix  manipulation  routines  as  well as  a  few  iterative
      !solvers, see detailed description of contents below.
      !
      ! Copyright (C) 2005, the Regents of the University of Minnesota
      !
      !SPARSKIT is  free software; you  can redistribute it and/or  modify it
      !under the terms of the  GNU Lesser General Public License as published
      !by the  Free Software Foundation [version  2.1 of the  License, or any
      !later version.]
      !
      !A copy of  the licencing agreement is attached in  the file LGPL.  For
      !additional information  contact the Free Software  Foundation Inc., 59
      !Temple Place - Suite 330, Boston, MA 02111, USA or visit the web-site
      !
      ! http://www.gnu.org/copyleft/lesser.html
      !
      !
      !DISCLAIMER
      !----------
      !
      !SPARSKIT  is distributed  in  the hope  that  it will  be useful,  but
      !WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
      !MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
      !Lesser General Public License for more details.
      !
      !For more information contact saad@cs.umn.edu
      !
      !

      SUBROUTINE IMSLINEARSUB_PCMILUT(n, a, ja, ia, lfil, droptol, relax,       &
                                      alu, jlu, ju, iwk, w, jw, ierr,           &
                                      izero, delta)
      !-----------------------------------------------------------------------
        integer(I4B) :: n
        real(DP) :: a(*),alu(*),w(n+1),droptol,relax
        integer(I4B) :: ja(*),ia(n+1),jlu(*),ju(n),jw(2*n),lfil,iwk,ierr
        integer(I4B) :: izero
        real(DP) :: delta
        !----------------------------------------------------------------------*
        !                      *** ILUT preconditioner ***                     *
        !      incomplete LU factorization with dual truncation mechanism      *
        !----------------------------------------------------------------------*
        !     Author: Yousef Saad *May, 5, 1990, Latest revision, August 1996  *
        !----------------------------------------------------------------------*
        ! PARAMETERS
        !-----------
        !
        ! on entry:
        !==========
        ! n       = integer. The row dimension of the matrix A. The matrix
        !
        ! a,ja,ia = matrix stored in Compressed Sparse Row format.
        !
        ! lfil    = integer. The fill-in parameter. Each row of L and each row
        !           of U will have a maximum of lfil elements (excluding the
        !           diagonal element). lfil must be .ge. 0.
        !           ** WARNING: THE MEANING OF LFIL HAS CHANGED WITH RESPECT TO
        !           EARLIER VERSIONS.
        !
        ! droptol = real. Sets the threshold for dropping small terms 
        !           in the factorization. See below for details on dropping 
        !           strategy.
        !
        !
        ! iwk     = integer. The lengths of arrays alu and jlu. If the arrays
        !           are not big enough to store the ILU factorizations, ilut
        !           will stop with an error message.
        !
        ! On return:
        !===========
        !
        ! alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
        !           the L and U factors together. The diagonal (stored in
        !           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
        !           contains the i-th row of L (excluding the diagonal entry=1)
        !           followed by the i-th row of U.
        !
        ! ju      = integer array of length n containing the pointers to
        !           the beginning of each row of U in the matrix alu,jlu.
        !
        ! ierr    = integer. Error message with the following meaning.
        !           ierr  = 0    --> successful return.
        !           ierr .gt. 0  --> zero pivot encountered at step number ierr.
        !           ierr  = -1   --> Error. input matrix may be wrong.
        !                            (The elimination process has generated a
        !                            row in L or U whose length is .gt.  n.)
        !           ierr  = -2   --> The matrix L overflows the array al.
        !           ierr  = -3   --> The matrix U overflows the array alu.
        !           ierr  = -4   --> Illegal value for lfil.
        !           ierr  = -5   --> zero row encountered.
        !
        ! work arrays:
        !=============
        ! jw      = integer work array of length 2*n.
        ! w       = real work array of length n+1.
        !
        !----------------------------------------------------------------------
        ! w, ju (1:n) store the working array [1:ii-1 = L-part, ii:n = u]
        ! jw(n+1:2n)  stores nonzero indicators
        !
        ! Notes:
        ! ------
        ! The diagonal elements of the input matrix must be  nonzero (at least
        ! 'structurally').
        !
        !----------------------------------------------------------------------*
        !---- Dual drop strategy works as follows.                             *
        !                                                                      *
        !     1) Thresholding in L and U as set by droptol. Any element whose  *
        !        magnitude is less than some tolerance (relative to the abs    *
        !        value of diagonal element in u) is dropped.                   *
        !                                                                      *
        !     2) Keeping only the largest lfil elements in the i-th row of L   *
        !        and the largest lfil elements in the i-th row of U (excluding *
        !        diagonal elements).                                           *
        !                                                                      *
        ! Flexibility: one  can use  droptol=0  to get  a strategy  based on   *
        ! keeping  the largest  elements in  each row  of L  and U.   Taking   *
        ! droptol .ne.  0 but lfil=n will give  the usual threshold strategy   *
        ! (however, fill-in is then unpredictable).                            *
        !----------------------------------------------------------------------*
        !     locals
        integer(I4B) :: ju0,k,j1,j2,j,ii,i,lenl,lenu,jj,jrow,jpos,ilen
        real(DP) :: tnorm, t, abs, s, fact
        real(DP) :: rs, d, sd1, tl
        if (lfil .lt. 0) goto 998
        !-----------------------------------------------------------------------
        !     initialize ju0 (points to next element to be added to alu,jlu)
        !     and pointer array.
        !-----------------------------------------------------------------------
        ju0 = n+2
        jlu(1) = ju0
        !
        !     initialize nonzero indicator array.
        !
        do j = 1, n
          jw(n+j)  = 0
        end do
        !-----------------------------------------------------------------------
        !     beginning of main loop.
        !-----------------------------------------------------------------------
        main: do ii = 1, n
          j1 = ia(ii)
          j2 = ia(ii+1) - 1
          rs = DZERO
          tnorm = DZERO
          do k = j1, j2
            tnorm = tnorm+abs(a(k))
          end do
          if (tnorm .eq. DZERO) goto 999
          tnorm = tnorm/real(j2-j1+1)
          !
          !     unpack L-part and U-part of row of A in arrays w
          !
          lenu = 1
          lenl = 0
          jw(ii) = ii
          w(ii) = DZERO
          jw(n+ii) = ii
          !
          do j = j1, j2
            k = ja(j)
            t = a(j)
            if (k .lt. ii) then
              lenl = lenl+1
              jw(lenl) = k
              w(lenl) = t
              jw(n+k) = lenl
            else if (k .eq. ii) then
              w(ii) = t
            else
              lenu = lenu+1
              jpos = ii+lenu-1
              jw(jpos) = k
              w(jpos) = t
              jw(n+k) = jpos
            end if
          end do
          jj = 0
          ilen = 0
          !
          !     eliminate previous rows
          !
150       jj = jj+1
          if (jj .gt. lenl) goto 160
          !-----------------------------------------------------------------------
          !     in order to do the elimination in the correct order we must select
          !     the smallest column index among jw(k), k=jj+1, ..., lenl.
          !-----------------------------------------------------------------------
          jrow = jw(jj)
          k = jj
          !
          !     determine smallest column index
          !
          do j = jj+1, lenl
            if (jw(j) .lt. jrow) then
              jrow = jw(j)
              k = j
            end if
          end do
          !
          if (k .ne. jj) then
            !     exchange in jw
            j = jw(jj)
            jw(jj) = jw(k)
            jw(k) = j
            !     exchange in jr
            jw(n+jrow) = jj
            jw(n+j) = k
            !     exchange in w
            s = w(jj)
            w(jj) = w(k)
            w(k) = s
          end if
          !
          !     zero out element in row by setting jw(n+jrow) to zero.
          !
          jw(n+jrow) = 0
          !
          !     get the multiplier for row to be eliminated (jrow).
          !
          fact = w(jj)*alu(jrow)
          if (abs(fact) .le. droptol) then
            rs = rs + w(jj)
            goto 150
          end if
          !
          !     combine current row and row jrow
          !
          do k = ju(jrow), jlu(jrow+1)-1
            s = fact*alu(k)
            j = jlu(k)
            jpos = jw(n+j)
            if (j .ge. ii) then
              !
              !     dealing with upper part.
              !
              if (jpos .eq. 0) then
                !
                !     this is a fill-in element
                !
                lenu = lenu+1
                if (lenu .gt. n) goto 995
                i = ii+lenu-1
                jw(i) = j
                jw(n+j) = i
                w(i) = - s
              else
                !
                !     this is not a fill-in element
                !
                w(jpos) = w(jpos) - s

              end if
            else
              !
              !     dealing  with lower part.
              !
              if (jpos .eq. 0) then
                !
                !     this is a fill-in element
                !
                lenl = lenl+1
                if (lenl .gt. n) goto 995
                jw(lenl) = j
                jw(n+j) = lenl
                w(lenl) = - s
              else
                !
                !     this is not a fill-in element
                !
                w(jpos) = w(jpos) - s
              end if
            end if
          end do
          !
          !     store this pivot element -- (from left to right -- no danger of
          !     overlap with the working elements in L (pivots).
          !
          ilen = ilen+1
          w(ilen) = fact
          jw(ilen) = jrow
          goto 150
160       continue
          !
          !     reset double-pointer to zero (U-part)
          !
          do k = 1, lenu
            jw(n+jw(ii+k-1)) = 0
          end do
          !
          !     update L-matrix
          !
          lenl = ilen
          ilen = min0(lenl,lfil)
          !
          !     sort by quick-split
          !
          call IMSLINEARSUB_PCMILUT_QSPLIT(lenl, w, jw, ilen)
          !
          !     store L-part
          !
          do k = 1, ilen
            !            if (ju0 .gt. iwk) goto 996
            if (ju0 .gt. iwk) then
              write (*,'(//1x,2i10)') ju0, iwk
              goto 996
            end if
            alu(ju0) =  w(k)
            jlu(ju0) =  jw(k)
            ju0 = ju0+1
          end do
          !
          !     save pointer to beginning of row ii of U
          !
          ju(ii) = ju0
          !
          !     update U-matrix -- first apply dropping strategy
          !
          ilen = 0
          do k = 1, lenu-1
            if (abs(w(ii+k)) .gt. droptol*tnorm) then
              ilen = ilen+1
              w(ii+ilen) = w(ii+k)
              jw(ii+ilen) = jw(ii+k)
            else
              rs = rs + w(ii+k)
            end if
          end do
          lenu = ilen+1
          ilen = min0(lenu,lfil)
          !
          call IMSLINEARSUB_PCMILUT_QSPLIT(lenu-1, w(ii+1), jw(ii+1), ilen)
          !
          !     copy
          !
          t = abs(w(ii))
          !         if (ilen + ju0 .gt. iwk) goto 997
          if (ilen + ju0 .gt. iwk) then
            write (*,'(//1x,2i10)') (ilen + ju0), iwk
            goto 997
          end if
          do k = ii+1, ii+ilen-1
            jlu(ju0) = jw(k)
            alu(ju0) = w(k)
            t = t + abs(w(k) )
            ju0 = ju0+1
          end do
          !!
          !!     add dropped terms to diagonal element
          !!
          !IF (relax > DZERO) THEN
          !  w(ii) = w(ii) + relax * rs
          !END IF
          !!
          !!     store inverse of diagonal element of u
          !!
          !if (w(ii) == DZERO) w(ii) = (DEM4 + droptol)*tnorm
          !!
          !alu(ii) = DONE / w(ii)

          !    diagonal - calculate inverse of diagonal for solution       
          d   = w(ii) 
          tl  = ( DONE + delta ) * d + ( relax * rs ) 

          !    ensure that the sign of the diagonal has not changed
          sd1 = SIGN(d,tl) 
          IF (sd1.NE.d) THEN 
            !  use small value if diagonal scaling is not effective for 
            !    pivots that change the sign of the diagonal               
            IF (izero > 1) THEN 
              tl = SIGN(DONE,d) * (DEM4 + droptol) * tnorm 
            !  diagonal scaling continues to be effective                
            ELSE 
              izero = 1 
              exit main 
            END IF 
          END IF
          !    ensure that the diagonal is not zero
          IF (ABS(tl) == DZERO) THEN 
            !  use small value if diagonal scaling is not effective
            !    zero pivots                                               
            IF (izero > 1) THEN 
              tl = SIGN(DONE,d) * (DEM4 + droptol) * tnorm 
            !  diagonal scaling continues to be effective 
            ELSE 
              izero = 1 
              exit main 
            END IF 
          END IF
          w(ii) = tl
          alu(ii) = DONE / w(ii)           
          !
          !     update pointer to beginning of next row of U.
          !
          jlu(ii+1) = ju0
          !-----------------------------------------------------------------------
          !     end main loop
          !-----------------------------------------------------------------------
        end do main
        ierr = 0
        return
        !
        !     incomprehensible error. Matrix must be wrong.
        !
995     ierr = -1
        return
        !
        !     insufficient storage in L.
        !
996     ierr = -2
        return
        !
        !     insufficient storage in U.
        !
997     ierr = -3
        return
        !
        !     illegal lfil entered.
        !
998     ierr = -4
        return
        !
        !     zero row encountered
        !
999     ierr = -5
        return
      !----------------end-of-ilut--------------------------------------------
      !-----------------------------------------------------------------------
      END SUBROUTINE IMSLINEARSUB_PCMILUT

      !-----------------------------------------------------------------------
      SUBROUTINE IMSLINEARSUB_PCMILUT_LUSOL(n, y, x, alu, jlu, ju)
        integer(I4B) :: n
        real(DP) :: x(n), y(n), alu(*)
        integer(I4B) :: jlu(*), ju(*)
        !-----------------------------------------------------------------------
        !
        ! This routine solves the system (LU) x = y,
        ! given an LU decomposition of a matrix stored in (alu, jlu, ju)
        ! modified sparse row format
        !
        !-----------------------------------------------------------------------
        ! on entry:
        ! n   = dimension of system
        ! y   = the right-hand-side vector
        ! alu, jlu, ju
        !     = the LU matrix as provided from the ILU routines.
        !
        ! on return
        ! x   = solution of LU x = y.
        !-----------------------------------------------------------------------
        !
        ! Note: routine is in place: call IMSLINEARSUB_PCMILUT_LUSOL (n, x, x, alu, jlu, ju)
        !       will solve the system with rhs x and overwrite the result on x .
        !
        !-----------------------------------------------------------------------
        ! -- local
        !
        integer(I4B) :: i, k
        !
        ! forward solve
        !
        do i = 1, n
          x(i) = y(i)
          do k = jlu(i), ju(i)-1
            x(i) = x(i) - alu(k)* x(jlu(k))
          end do
        end do
        !
        !     backward solve.
        !
        do i = n, 1, -1
          do k = ju(i), jlu(i+1)-1
            x(i) = x(i) - alu(k)*x(jlu(k))
          end do
          x(i) = alu(i)*x(i)
        end do
        !
        return
      !----------------end of IMSLINEARSUB_PCMILUT_LUSOL ------------------------------------------
      !-----------------------------------------------------------------------
      END SUBROUTINE IMSLINEARSUB_PCMILUT_LUSOL
                                           
      !-----------------------------------------------------------------------
      SUBROUTINE IMSLINEARSUB_PCMILUT_QSPLIT(n, a, ind, ncut)
        integer(I4B) :: n
        real(DP) :: a(n)
        integer(I4B) :: ind(n), ncut
        !-----------------------------------------------------------------------
        !     does a quick-sort split of a real array.
        !     on input a(1:n). is a real array
        !     on output a(1:n) is permuted such that its elements satisfy:
        !
        !     abs(a(i)) .ge. abs(a(ncut)) for i .lt. ncut and
        !     abs(a(i)) .le. abs(a(ncut)) for i .gt. ncut
        !
        !     ind(1:n) is an integer array which permuted in the same way as a(*
        !-----------------------------------------------------------------------
        real(DP) :: tmp, abskey
        integer(I4B) :: itmp, first, last
        integer(I4B) :: mid
        integer(I4B) :: j
        !-----
        first = 1
        last = n
        if (ncut .lt. first .or. ncut .gt. last) return
        !
        !     outer loop -- while mid .ne. ncut do
        !
00001   mid = first
        abskey = abs(a(mid))
        do j = first+1, last
          if (abs(a(j)) .gt. abskey) then
            mid = mid+1
            !     interchange
            tmp = a(mid)
            itmp = ind(mid)
            a(mid) = a(j)
            ind(mid) = ind(j)
            a(j)  = tmp
            ind(j) = itmp
          end if
        end do
        !
        !     interchange
        !
        tmp = a(mid)
        a(mid) = a(first)
        a(first)  = tmp
        !
        itmp = ind(mid)
        ind(mid) = ind(first)
        ind(first) = itmp
        !
        !     test for while loop
        !
        if (mid .eq. ncut) return
        if (mid .gt. ncut) then
          last = mid-1
        else
          first = mid+1
        end if
        goto 1
        !----------------end-of-IMSLINEARSUB_PCMILUT_QSPLIT------------------------------------------
        !-----------------------------------------------------------------------
      END SUBROUTINE IMSLINEARSUB_PCMILUT_QSPLIT

END MODULE IMSLinearModule

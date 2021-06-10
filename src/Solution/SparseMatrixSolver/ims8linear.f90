MODULE IMSLinearModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, LENSOLUTIONNAME, LENMEMPATH,          &
                             IZERO, DZERO, DPREC, DSAME,                       &
                             DEM8, DEM6, DEM5, DEM4, DEM3, DEM2, DEM1,         &
                             DHALF, DONE, DTWO,                                &
                             VDEBUG
  use GenericUtilitiesModule, only: sim_message 
  use IMSLinearBaseModule, only: imsbase_cg, ims_base_bcgs,           &
                                 ims_base_pccrs, ims_base_calc_order,  &
                                 ims_base_scale, ims_base_pcu,         &
                                 ims_base_mv 
  use IMSReorderingModule, only: ims_dperm, ims_vperm
  use BlockParserModule, only: BlockParserType

  IMPLICIT NONE
  private
  
  TYPE, PUBLIC :: ImsLinearDataType
    character(len=LENMEMPATH) :: memoryPath                              !< the path for storing variables in the memory manager
    integer(I4B), POINTER :: iout => NULL()                              !< simulation listing file unit
    integer(I4B), POINTER :: IPRIMS => NULL()                            !< print flag
    integer(I4B), POINTER :: ILINMETH => NULL()                          !< linear accelerator (1) cg, (2) bicgstab
    integer(I4B), POINTER :: ITER1 => NULL()                             !< maximum inner iterations
    integer(I4B), POINTER :: IPC => NULL()                               !< preconditioner flag
    integer(I4B), POINTER :: ISCL => NULL()                              !< scaling flag
    integer(I4B), POINTER :: IORD => NULL()                              !< reordering flag
    integer(I4B), POINTER :: NORTH => NULL()                             !< orthogonalization interval
    integer(I4B), POINTER :: ICNVGOPT => NULL()                          !< rclose convergence option flag
    integer(I4B), POINTER :: IACPC => NULL()                             !< ia for the preconditioner
    integer(I4B), POINTER :: NITERC => NULL()                            !< 
    integer(I4B), POINTER :: NIABCGS => NULL()                           !<
    integer(I4B), POINTER :: NIAPC => NULL()                             !< number of rows
    integer(I4B), POINTER :: NJAPC => NULL()                             !< number of non-zero entries
    real(DP), POINTER :: DVCLOSE => NULL()                               !< dependent variable convergence criteria
    real(DP), POINTER :: RCLOSE => NULL()                                !< flow convergence criteria
    real(DP), POINTER :: RELAX => NULL()                                 !< preconditioner relaxation factor
    real(DP), POINTER :: EPFACT => NULL()                                !<
    real(DP), POINTER :: L2NORM0 => NULL()                               !< initial L2 norm
    ! ILUT VARIABLES
    integer(I4B), POINTER :: LEVEL => NULL()                             !< number of levels
    real(DP), POINTER :: DROPTOL => NULL()                               !< drop tolerance
    integer(I4B), POINTER :: NJLU => NULL()                              !< 
    integer(I4B), POINTER :: NJW => NULL()                               !<
    integer(I4B), POINTER :: NWLU => NULL()                              !<
    ! POINTERS TO SOLUTION VARIABLES
    integer(I4B), POINTER :: NEQ => NULL()                               !< number of equations (rows in matrix)
    integer(I4B), POINTER :: NJA => NULL()                               !< number of non-zero values in amat
    integer(I4B), dimension(:), pointer, contiguous :: IA => NULL()      !< position of start of each row
    integer(I4B), dimension(:), pointer, contiguous :: JA => NULL()      !< column pointer
    real(DP), dimension(:), pointer, contiguous :: AMAT => NULL()        !< coefficient matrix
    real(DP), dimension(:), pointer, contiguous :: RHS => NULL()         !< right-hand side of equation
    real(DP), dimension(:), pointer, contiguous :: X => NULL()           !< dependent variable
    ! VECTORS
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: DSCALE => NULL()      !< scaling factor
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: DSCALE2 => NULL()     !< unscaling factor
    integer(I4B), POINTER,DIMENSION(:),CONTIGUOUS :: IAPC => NULL()      !< position of start of each row in preconditioner matrix
    integer(I4B), POINTER,DIMENSION(:),CONTIGUOUS :: JAPC => NULL()      !< preconditioner matrix column pointer
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: APC => NULL()         !< preconditioner coefficient matrix
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: LORDER => NULL()  !< reordering mapping
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: IORDER => NULL()  !< mapping to restore reordered matrix
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: IARO => NULL()    !< position of start of each row in reordered matrix
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: JARO => NULL()    !< reordered matrix column pointer
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: ARO => NULL()         !< reordered coefficient matrix
    ! WORKING ARRAYS
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: IW => NULL()      !< integer working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: W => NULL()           !< real working array
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: ID => NULL()      !< integer working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: D => NULL()           !< real working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: P => NULL()           !< real working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: Q => NULL()           !< real working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: Z => NULL()           !< real working array
    ! BICGSTAB WORKING ARRAYS
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: T => NULL()           !< real working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: V => NULL()           !< real working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: DHAT => NULL()        !< real working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: PHAT => NULL()        !< real working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: QHAT => NULL()        !< real working array
    ! POINTERS FOR USE WITH BOTH ORIGINAL AND RCM ORDERINGS
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: IA0 => NULL()     !< pointer to current ia array
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: JA0 => NULL()     !< pointer to current ja array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: A0 => NULL()          !< pointer to current coefficient matrix
    ! ILUT WORKING ARRAYS
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: JLU => NULL()     !< ilut integer working array 
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: JW => NULL()      !< ilut integer working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: WLU => NULL()         !< ilut real working array
    
    ! PROCEDURES (METHODS)
    CONTAINS
      PROCEDURE :: IMSLINEAR_ALLOCATE => imslinear_ar
      procedure :: imslinear_summary
      PROCEDURE :: IMSLINEAR_APPLY => imslinear_ap
      procedure :: IMSLINEAR_DA => imslinear_da
      procedure, private :: allocate_scalars
      ! -- PRIVATE PROCEDURES
      PROCEDURE, PRIVATE :: SET_IMSLINEAR_INPUT => imslinear_set_input
  END TYPE ImsLinearDataType
  
  type(BlockParserType), private :: parser

  
  CONTAINS
  
    !> @ brief Allocate storage and read data
    !!
    !!  Allocate storage for linear accelerators and read data
    !!
    !<
    SUBROUTINE imslinear_ar(THIS, NAME, INIU, IOUT, IPRIMS, MXITER, IFDPARAM, &
                            IMSLINEARM, NEQ, NJA, IA, JA, AMAT, RHS, X,       &
                            NINNER, LFINDBLOCK)
      ! -- modules
      use MemoryManagerModule, only: mem_allocate
      use MemoryHelperModule,  only: create_mem_path
      use SimModule, only: ustop, store_error, count_errors,            &
                           deprecation_warning
      ! -- dummy variables
      CLASS(ImsLinearDataType), INTENT(INOUT) :: THIS
      CHARACTER (LEN=LENSOLUTIONNAME), INTENT(IN) :: NAME        !< solution name
      integer(I4B), INTENT(IN) :: INIU                           !< IMS input file unit
      integer(I4B), INTENT(IN) :: IOUT                           !< simulation listing file unit
      integer(I4B), TARGET, INTENT(IN) :: IPRIMS                 !< print option
      integer(I4B), INTENT(IN) :: MXITER                         !< maximum outer iterations
      integer(I4B), INTENT(IN) :: IFDPARAM                       !< 
      integer(I4B), INTENT(INOUT) :: IMSLINEARM                  !<
      integer(I4B), TARGET, INTENT(IN) :: NEQ                    !< number of equations
      integer(I4B), TARGET, INTENT(IN) :: NJA                    !< number of non-zero entries in the coefficient matrix
      integer(I4B), DIMENSION(NEQ+1), TARGET, INTENT(IN) :: IA   !< pointer to the start of a row in the coefficient matrix
      integer(I4B), DIMENSION(NJA), TARGET, INTENT(IN) :: JA     !< column pointer
      real(DP), DIMENSION(NJA), TARGET, INTENT(IN) :: AMAT       !< coefficient matrix
      real(DP), DIMENSION(NEQ), TARGET, INTENT(INOUT) :: RHS     !< right-hand side
      real(DP), DIMENSION(NEQ), TARGET, INTENT(INOUT) :: X       !< dependent variables
      integer(I4B), TARGET, INTENT(INOUT) :: NINNER              !< maximum number of inner iterations
      integer(I4B), INTENT(IN), OPTIONAL :: LFINDBLOCK           !<
      ! -- local variables
      LOGICAL :: lreaddata
      character(len=LINELENGTH) :: errmsg
      character(len=LINELENGTH) :: warnmsg
      character(len=LINELENGTH) :: keyword
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
      !
      ! -- SET LREADDATA
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
      ! -- DEFINE NAME      
      this%memoryPath = create_mem_path(name, 'IMSLinear')
      !
      ! -- SET POINTERS TO SOLUTION STORAGE
      THIS%IPRIMS => IPRIMS
      THIS%NEQ => NEQ
      THIS%NJA => NJA
      THIS%IA => IA
      THIS%JA => JA
      THIS%AMAT => AMAT
      THIS%RHS => RHS
      THIS%X => X
      !
      ! -- ALLOCATE SCALAR VARIABLES
      call this%allocate_scalars()
      !
      ! -- initialize iout
      this%iout = iout
      !
      ! -- DEFAULT VALUES
      THIS%IORD = 0
      THIS%ISCL = 0
      THIS%IPC = 0
      THIS%LEVEL = 0
      !
      ! -- TRANSFER COMMON VARIABLES FROM IMS TO IMSLINEAR
      THIS%ILINMETH = 0
      
      THIS%IACPC = 0
      THIS%RELAX = DZERO !0.97
      
      THIS%DROPTOL = DZERO
      
      THIS%NORTH = 0

      THIS%ICNVGOPT = 0
      !
      ! -- PRINT A MESSAGE IDENTIFYING IMSLINEAR SOLVER PACKAGE
      write(iout,2000)
02000 FORMAT (1X,/1X,'IMSLINEAR -- UNSTRUCTURED LINEAR SOLUTION',               &
     &        ' PACKAGE, VERSION 8, 04/28/2017')
      !
      ! -- SET DEFAULT IMSLINEAR PARAMETERS
      CALL THIS%SET_IMSLINEAR_INPUT(IFDPARAM)
      NINNER = this%iter1
      !
      ! -- Initialize block parser
      call parser%Initialize(iniu, iout)
      !
      ! -- get IMSLINEAR block
      if (lreaddata) then
        call parser%GetBlock('LINEAR', isfound, ierr, &
          supportOpenClose=.true., blockRequired=.FALSE.)
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
            case ('INNER_DVCLOSE')
              this%DVCLOSE = parser%GetDouble()
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
                write(errmsg,'(3a)')                                             &
                  'UNKNOWN IMSLINEAR LINEAR_ACCELERATION METHOD (',              &
                  trim(keyword), ').'
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
                write(errmsg,'(3a)')                                             &
                  'UNKNOWN IMSLINEAR SCALING_METHOD (', trim(keyword), ').'
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
                write(errmsg,'(3a)')                                             &
                  'UNKNOWN IMSLINEAR REORDERING_METHOD (', trim(keyword), ').'
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
                write(errmsg,'(a,1x,a)')                                         &
                  'IMSLINEAR PRECONDITIONER_LEVELS MUST BE GREATER THAN',        &
                  'OR EQUAL TO ZERO'
                call store_error(errmsg)
              end if
            case ('PRECONDITIONER_DROP_TOLERANCE')
              r = parser%GetDouble()
              THIS%DROPTOL = r
              if (r < DZERO) then
                write(errmsg,'(a,1x,a)')                                         &
                  'IMSLINEAR PRECONDITIONER_DROP_TOLERANCE',                     &
                  'MUST BE GREATER THAN OR EQUAL TO ZERO'
                call store_error(errmsg)
              end if
            !
            ! -- deprecated variables
            case ('INNER_HCLOSE')
              this%DVCLOSE = parser%GetDouble()
              !
              ! -- create warning message
              write(warnmsg,'(a)')                                               &
                'SETTING INNER_DVCLOSE TO INNER_HCLOSE VALUE'
              !
              ! -- create deprecation warning
              call deprecation_warning('LINEAR', 'INNER_HCLOSE', '6.1.1',        &
                                       warnmsg, parser%GetUnit())
            !
            ! -- default
            case default
                write(errmsg,'(3a)')                                             &
                  'UNKNOWN IMSLINEAR KEYWORD (', trim(keyword), ').'
              call store_error(errmsg)
          end select
        end do
        write(iout,'(1x,a)') 'END OF LINEAR DATA'
      else
        if (IFDPARAM ==  0) THEN
          write(errmsg,'(a)') 'NO LINEAR BLOCK DETECTED.'
          call store_error(errmsg)
        end if
      end if
      
      IMSLINEARM = THIS%ILINMETH
      !
      ! -- DETERMINE PRECONDITIONER
      IF (THIS%LEVEL > 0 .OR. THIS%DROPTOL > DZERO) THEN
        THIS%IPC = 3
      ELSE
        THIS%IPC = 1
      END IF
      IF (THIS%RELAX > DZERO) THEN
        THIS%IPC = THIS%IPC + 1
      END IF
      !
      ! -- ERROR CHECKING FOR OPTIONS
      IF (THIS%ISCL < 0 ) THIS%ISCL = 0
      IF (THIS%ISCL > 2 ) THEN
        WRITE(errmsg,'(A)') 'IMSLINEAR7AR ISCL MUST BE <= 2'
        call store_error(errmsg)
      END IF
      IF (THIS%IORD < 0 ) THIS%IORD = 0
      IF (THIS%IORD > 2) THEN
        WRITE(errmsg,'(A)') 'IMSLINEAR7AR IORD MUST BE <= 2'
        call store_error(errmsg)
      END IF
      IF (THIS%NORTH < 0) THEN
        WRITE(errmsg,'(A)') 'IMSLINEAR7AR NORTH MUST >= 0'
        call store_error(errmsg)
      END IF
      IF (THIS%RCLOSE == DZERO) THEN
        IF (THIS%ICNVGOPT /= 3) THEN
          WRITE(errmsg,'(A)') 'IMSLINEAR7AR RCLOSE MUST > 0.0'
          call store_error(errmsg)
        END IF
      END IF
      IF (THIS%RELAX < DZERO) THEN
        WRITE(errmsg,'(A)') 'IMSLINEAR7AR RELAX MUST BE >= 0.0'
        call store_error(errmsg)
      END IF
      IF (THIS%RELAX > DONE) THEN
        WRITE(errmsg,'(A)') 'IMSLINEAR7AR RELAX MUST BE <= 1.0'
        call store_error(errmsg)
      END IF
      !
      ! -- CHECK FOR ERRORS IN IMSLINEAR      
      if (count_errors() > 0) then
        call parser%StoreErrorUnit()
        call ustop()
      endif
      !
      ! -- INITIALIZE IMSLINEAR VARIABLES
      THIS%NITERC = 0
      !
      ! -- ALLOCATE AND INITIALIZE MEMORY FOR IMSLINEAR
      iscllen  = 1
      IF (THIS%ISCL.NE.0 ) iscllen  = NEQ
      CALL mem_allocate(THIS%DSCALE, iscllen, 'DSCALE', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%DSCALE2, iscllen, 'DSCALE2', TRIM(THIS%memoryPath))
      !      
      ! -- ALLOCATE MEMORY FOR PRECONDITIONING MATRIX
      ijlu      = 1
      ijw       = 1
      iwlu      = 1
      !
      ! -- ILU0 AND MILU0
      THIS%NIAPC = THIS%NEQ
      THIS%NJAPC = THIS%NJA
      !
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
      !
      ! -- ALLOCATE BASE PRECONDITIONER VECTORS
      CALL mem_allocate(THIS%IAPC, THIS%NIAPC+1, 'IAPC', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%JAPC, THIS%NJAPC, 'JAPC', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%APC, THIS%NJAPC, 'APC', TRIM(THIS%memoryPath))
      !
      ! -- ALLOCATE MEMORY FOR ILU0 AND MILU0 NON-ZERO ROW ENTRY VECTOR
      CALL mem_allocate(THIS%IW, THIS%NIAPC, 'IW', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%W, THIS%NIAPC, 'W', TRIM(THIS%memoryPath))
      !
      ! -- ALLOCATE MEMORY FOR ILUT VECTORS
      CALL mem_allocate(THIS%JLU, ijlu, 'JLU', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%JW, ijw, 'JW', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%WLU, iwlu, 'WLU', TRIM(THIS%memoryPath))
      !
      ! -- GENERATE IAPC AND JAPC FOR ILU0 AND MILU0
      IF (THIS%IPC ==  1 .OR. THIS%IPC ==  2) THEN
        CALL ims_base_pccrs(THIS%NEQ,THIS%NJA,THIS%IA,THIS%JA,              &
                                THIS%IAPC,THIS%JAPC)
      END IF
      !
      ! -- ALLOCATE SPACE FOR PERMUTATION VECTOR
      i0     = 1
      iolen  = 1
      IF (THIS%IORD.NE.0) THEN
        i0     = THIS%NEQ
        iolen  = THIS%NJA
      END IF
      CALL mem_allocate(THIS%LORDER, i0, 'LORDER', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%IORDER, i0, 'IORDER', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%IARO, i0+1, 'IARO', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%JARO, iolen, 'JARO', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%ARO, iolen, 'ARO', TRIM(THIS%memoryPath))
      !
      ! -- ALLOCATE WORKING VECTORS FOR IMSLINEAR SOLVER
      CALL mem_allocate(THIS%ID, THIS%NEQ, 'ID', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%D, THIS%NEQ, 'D', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%P, THIS%NEQ, 'P', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%Q, THIS%NEQ, 'Q', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%Z, THIS%NEQ, 'Z', TRIM(THIS%memoryPath))
      !
      ! -- ALLOCATE MEMORY FOR BCGS WORKING ARRAYS
      THIS%NIABCGS = 1
      IF (THIS%ILINMETH ==  2) THEN
        THIS%NIABCGS = THIS%NEQ
      END IF
      CALL mem_allocate(THIS%T, THIS%NIABCGS, 'T', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%V, THIS%NIABCGS, 'V', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%DHAT, THIS%NIABCGS, 'DHAT', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%PHAT, THIS%NIABCGS, 'PHAT', TRIM(THIS%memoryPath))
      CALL mem_allocate(THIS%QHAT, THIS%NIABCGS, 'QHAT', TRIM(THIS%memoryPath))
      !
      ! -- INITIALIZE IMSLINEAR VECTORS
      DO n = 1, iscllen
        THIS%DSCALE(n)  = DONE
        THIS%DSCALE2(n) = DONE
      END DO
      DO n = 1, THIS%NJAPC
        THIS%APC(n)  = DZERO
      END DO
      !
      ! -- WORKING VECTORS
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
      !
      ! -- BCGS WORKING VECTORS
      DO n = 1, THIS%NIABCGS
        THIS%T(n)    = DZERO
        THIS%V(n)    = DZERO
        THIS%DHAT(n) = DZERO
        THIS%PHAT(n) = DZERO
        THIS%QHAT(n) = DZERO
      END DO
      !
      ! -- ILUT AND MILUT WORKING VECTORS      
      DO n = 1, ijlu
        THIS%JLU(n)   = DZERO
      END DO
      DO n = 1, ijw
        THIS%JW(n)   = DZERO
      END DO
      DO n = 1, iwlu
        THIS%WLU(n)  = DZERO
      END DO
      !
      ! -- REORDERING VECTORS
      DO n = 1, i0 + 1
        THIS%IARO(n) = IZERO
      END DO
      DO n = 1, iolen
        THIS%JARO(n) = IZERO
        THIS%ARO(n)  = DZERO
      END DO
      !
      ! -- REVERSE CUTHILL MCKEE AND MINIMUM DEGREE ORDERING
      IF (THIS%IORD.NE.0) THEN
        CALL ims_base_calc_order(IOUT, THIS%IPRIMS, THIS%IORD,THIS%NEQ,     &
                                     THIS%NJA,THIS%IA,THIS%JA,                  &
                                     THIS%LORDER,THIS%IORDER)
      END IF
      !      
      ! -- ALLOCATE MEMORY FOR STORING ITERATION CONVERGENCE DATA
      !
      ! -- RETURN
      RETURN
    END SUBROUTINE imslinear_ar

    !> @ brief Write summary of settings
    !!
    !!  Write summary of linear accelerator settings.
    !!
    !<
    subroutine imslinear_summary(this, mxiter)
      ! -- dummy variables
      class(ImsLinearDataType), intent(inout) :: this
      integer(I4B), intent(in) :: mxiter               !< maximum number of outer iterations
      ! -- local variables
      CHARACTER (LEN= 10) :: clin(0:2)
      CHARACTER (LEN= 31) :: clintit(0:2)
      CHARACTER (LEN= 20) :: cipc(0:4)
      CHARACTER (LEN= 20) :: cscale(0:2)
      CHARACTER (LEN= 25) :: corder(0:2)
      CHARACTER (LEN= 16), DIMENSION(0:4) :: ccnvgopt
      CHARACTER (LEN= 15) :: clevel
      CHARACTER (LEN= 15) :: cdroptol 
      integer(I4B) :: i
      integer(I4B) :: j
      ! -- data
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
      ! -- formats
02010 FORMAT (1X,/,7X,'SOLUTION BY THE',1X,A31,1X,'METHOD', &
     &        /,1X,66('-'),/, &
     &        ' MAXIMUM OF ',I0,' CALLS OF SOLUTION ROUTINE',/, &
     &        ' MAXIMUM OF ',I0, &
     &        ' INTERNAL ITERATIONS PER CALL TO SOLUTION ROUTINE',/, &
     &        ' LINEAR ACCELERATION METHOD            =',1X,A,/, &
     &        ' MATRIX PRECONDITIONING TYPE           =',1X,A,/, &
     &        ' MATRIX SCALING APPROACH               =',1X,A,/, &
     &        ' MATRIX REORDERING APPROACH            =',1X,A,/, &
     &        ' NUMBER OF ORTHOGONALIZATIONS          =',1X,I0,/, &
     &        ' HEAD CHANGE CRITERION FOR CLOSURE     =',E15.5,/, &
     &        ' RESIDUAL CHANGE CRITERION FOR CLOSURE =',E15.5,/, &
     &        ' RESIDUAL CONVERGENCE OPTION           =',1X,I0,/, &
     &        ' RESIDUAL CONVERGENCE NORM             =',1X,A,/, &
     &        ' RELAXATION FACTOR                     =',E15.5)
02015 FORMAT (' NUMBER OF LEVELS                      =',A15,/, &
     &        ' DROP TOLERANCE                        =',A15,//)
2030  FORMAT(1X,A20,1X,6(I6,1X))
2040  FORMAT(1X,20('-'),1X,6(6('-'),1X))
2050  FORMAT(1X,62('-'),/)      !
! -- -----------------------------------------------------------
      !
      ! -- initialize clevel and cdroptol
      clevel = ''
      cdroptol = ''
      !
      ! -- PRINT MXITER,ITER1,IPC,ISCL,IORD,DVCLOSE,RCLOSE
      write(this%iout,2010)                                         &
                        clintit(THIS%ILINMETH), MXITER, THIS%ITER1, &
                        clin(THIS%ILINMETH), cipc(THIS%IPC),        &
                        cscale(THIS%ISCL), corder(THIS%IORD),       &
                        THIS%NORTH, THIS%DVCLOSE, THIS%RCLOSE,      &
                        THIS%ICNVGOPT, ccnvgopt(THIS%ICNVGOPT),     &
                        THIS%RELAX
      if (this%level > 0) then
        write(clevel, '(i15)') this%level
      end if
      if (this%droptol > DZERO) then
        write(cdroptol, '(e15.5)') this%droptol
      end if
      IF (this%level > 0 .or. this%droptol > DZERO) THEN
        write(this%iout,2015) trim(adjustl(clevel)),               &
                               trim(adjustl(cdroptol))
      ELSE
         write(this%iout,'(//)')
      END IF
      
      if (this%iord /= 0) then
        !                                                                       
        ! -- WRITE SUMMARY OF REORDERING INFORMATION TO LIST FILE                                                  
        if (this%iprims ==  2) then 
          DO i = 1, this%neq, 6 
            write(this%iout,2030) 'ORIGINAL NODE      :',                      &
                              (j,j=i,MIN(i+5,this%neq))                      
            write(this%iout,2040) 
            write(this%iout,2030) 'REORDERED INDEX    :',                      &
                              (this%lorder(j),j=i,MIN(i+5,this%neq))              
            write(this%iout,2030) 'REORDERED NODE     :',                      &
                              (this%iorder(j),j=i,MIN(i+5,this%neq))              
            write(this%iout,2050) 
          END DO 
        END IF 
      end if
      !
      ! -- return
      return
    end subroutine imslinear_summary 
    
    !> @ brief Allocate and initialize scalars
    !!
    !!  Allocate and inititialize linear accelerator scalars
    !!
    !<
    subroutine allocate_scalars(this)
      ! -- modules
      use MemoryManagerModule, only: mem_allocate
      ! -- dummy variables
      class(ImsLinearDataType), intent(inout) :: this
      !
      ! -- allocate scalars
      call mem_allocate(this%iout, 'IOUT', this%memoryPath)
      call mem_allocate(this%ilinmeth, 'ILINMETH', this%memoryPath)
      call mem_allocate(this%iter1, 'ITER1', this%memoryPath)
      call mem_allocate(this%ipc, 'IPC', this%memoryPath)
      call mem_allocate(this%iscl, 'ISCL', this%memoryPath)
      call mem_allocate(this%iord, 'IORD', this%memoryPath)
      call mem_allocate(this%north, 'NORTH', this%memoryPath)
      call mem_allocate(this%icnvgopt, 'ICNVGOPT', this%memoryPath)
      call mem_allocate(this%iacpc, 'IACPC', this%memoryPath)
      call mem_allocate(this%niterc, 'NITERC', this%memoryPath)
      call mem_allocate(this%niabcgs, 'NIABCGS', this%memoryPath)
      call mem_allocate(this%niapc, 'NIAPC', this%memoryPath)
      call mem_allocate(this%njapc, 'NJAPC', this%memoryPath)
      call mem_allocate(this%dvclose, 'DVCLOSE', this%memoryPath)
      call mem_allocate(this%rclose, 'RCLOSE', this%memoryPath)
      call mem_allocate(this%relax, 'RELAX', this%memoryPath)
      call mem_allocate(this%epfact, 'EPFACT', this%memoryPath)
      call mem_allocate(this%l2norm0, 'L2NORM0', this%memoryPath)
      call mem_allocate(this%droptol, 'DROPTOL', this%memoryPath)
      call mem_allocate(this%level, 'LEVEL', this%memoryPath)
      call mem_allocate(this%njlu, 'NJLU', this%memoryPath)
      call mem_allocate(this%njw, 'NJW', this%memoryPath)
      call mem_allocate(this%nwlu, 'NWLU', this%memoryPath)
      !
      ! -- initialize scalars
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
      this%dvclose = DZERO
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
      ! -- return
      return
    end subroutine allocate_scalars
    
    !> @ brief Deallocate memory
    !!
    !!  Deallocate linear accelerator memory.
    !!
    !<
    subroutine imslinear_da(this)
      ! -- modules
      use MemoryManagerModule, only: mem_deallocate
      ! -- dummy variables
      class(ImsLinearDataType), intent(inout) :: this
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
      call mem_deallocate(this%dvclose)
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
      ! -- return
      return
    end subroutine imslinear_da
    
    !> @ brief Set default settings
    !!
    !!  Set default linear accelerator settings.
    !!
    !<
    SUBROUTINE imslinear_set_input(THIS, IFDPARAM)
      ! -- dummy variables
      CLASS(ImsLinearDataType), INTENT(INOUT) :: THIS
      integer(I4B), INTENT(IN) :: IFDPARAM  !< complexity option
      ! -- code
      SELECT CASE ( IFDPARAM )
        !
        ! -- Simple option
        CASE(1)
          THIS%ITER1 = 50
          THIS%ILINMETH=1
          THIS%IPC = 1
          THIS%ISCL = 0
          THIS%IORD = 0
          THIS%DVCLOSE = DEM3
          THIS%RCLOSE = DEM1
          THIS%RELAX = DZERO
          THIS%LEVEL = 0
          THIS%DROPTOL = DZERO
          THIS%NORTH = 0
        !
        ! -- Moderate
        CASE(2)
          THIS%ITER1 = 100
          THIS%ILINMETH=2
          THIS%IPC = 2
          THIS%ISCL = 0
          THIS%IORD = 0
          THIS%DVCLOSE = DEM2
          THIS%RCLOSE = DEM1
          THIS%RELAX = 0.97D0
          THIS%LEVEL = 0
          THIS%DROPTOL = DZERO
          THIS%NORTH = 0
        !
        ! -- Complex
        CASE(3)
          THIS%ITER1 = 500
          THIS%ILINMETH=2
          THIS%IPC = 3
          THIS%ISCL = 0
          THIS%IORD = 0
          THIS%DVCLOSE = DEM1
          THIS%RCLOSE = DEM1
          THIS%RELAX = DZERO
          THIS%LEVEL = 5
          THIS%DROPTOL = DEM4
          THIS%NORTH = 2
      END SELECT
      !
      ! -- return
      RETURN
    END SUBROUTINE imslinear_set_input

    !> @ brief Base linear accelerator subroutine
    !!
    !!  Base linear accelerator subroutine that scales and reorders
    !!  the system of equations, if necessary, updates the preconditioner,
    !!  and calls the appropriate linear accelerator.
    !!
    !< 
    SUBROUTINE imslinear_ap(THIS,ICNVG,KSTP,KITER,IN_ITER,                  &
                            NCONV, CONVNMOD, CONVMODSTART, LOCDV, LOCDR,    &
                            CACCEL, ITINNER, CONVLOCDV, CONVLOCDR,          &
                            DVMAX, DRMAX, CONVDVMAX, CONVDRMAX)
      ! -- modules
      USE SimModule
      ! -- dummy variables
      CLASS(ImsLinearDataType), INTENT(INOUT) :: THIS
      integer(I4B), INTENT(INOUT)                          :: ICNVG           !<
      integer(I4B), INTENT(IN)                             :: KSTP            !<
      integer(I4B), INTENT(IN)                             :: KITER           !<
      integer(I4B), INTENT(INOUT)                          :: IN_ITER         !<
      ! CONVERGENCE INFORMATION
      integer(I4B), INTENT(IN) :: NCONV                                       !<
      integer(I4B), INTENT(IN) :: CONVNMOD                                    !<
      integer(I4B), DIMENSION(CONVNMOD+1), INTENT(INOUT) ::CONVMODSTART       !<
      integer(I4B), DIMENSION(CONVNMOD), INTENT(INOUT) :: LOCDV               !<
      integer(I4B), DIMENSION(CONVNMOD), INTENT(INOUT) :: LOCDR               !<
      character(len=31), DIMENSION(NCONV), INTENT(INOUT) :: CACCEL            !<
      integer(I4B), DIMENSION(NCONV), INTENT(INOUT) :: ITINNER                !<
      integer(I4B), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVLOCDV    !<
      integer(I4B), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVLOCDR    !<
      real(DP), DIMENSION(CONVNMOD), INTENT(INOUT) :: DVMAX                   !<
      real(DP), DIMENSION(CONVNMOD), INTENT(INOUT) :: DRMAX                   !<
      real(DP), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVDVMAX        !<
      real(DP), DIMENSION(CONVNMOD, NCONV), INTENT(INOUT) :: CONVDRMAX        !<
      ! -- local variables
      integer(I4B) :: n
      integer(I4B) :: innerit
      integer(I4B) :: irc
      integer(I4B) :: itmax
      real(DP) :: tv
      real(DP) :: rmax
      !
      ! -- set epfact based on timestep
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
      !
      ! -- SCALE PROBLEM
      IF (THIS%ISCL.NE.0) THEN
        CALL ims_base_scale(0,THIS%ISCL,                                    &
                         THIS%NEQ,THIS%NJA,THIS%IA,THIS%JA,                     &
                         THIS%AMAT,THIS%X,THIS%RHS,                             &
                         THIS%DSCALE,THIS%DSCALE2)
      END IF
      !
      ! -- PERMUTE ROWS, COLUMNS, AND RHS
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
      ! -- UPDATE PRECONDITIONER
      CALL ims_base_pcu(this%iout,THIS%NJA,THIS%NEQ,THIS%NIAPC,THIS%NJAPC,  &
                            THIS%IPC, THIS%RELAX, THIS%A0, THIS%IA0, THIS%JA0,  &
                            THIS%APC,THIS%IAPC,THIS%JAPC,THIS%IW,THIS%W,        &
                            THIS%LEVEL, THIS%DROPTOL, THIS%NJLU, THIS%NJW,      &
                            THIS%NWLU, THIS%JLU, THIS%JW, THIS%WLU)
      !
      ! -- INITIALIZE SOLUTION VARIABLE AND ARRAYS
      IF (KITER ==  1 ) THIS%NITERC = 0
      irc    = 1
      ICNVG  = 0
      DO n = 1, THIS%NEQ
        THIS%D(n) = DZERO
        THIS%P(n) = DZERO
        THIS%Q(n) = DZERO
        THIS%Z(n) = DZERO
      END DO
      !
      ! -- CALCULATE INITIAL RESIDUAL
      CALL ims_base_mv(THIS%NJA,THIS%NEQ,THIS%A0,THIS%X,THIS%D,             &
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
      !
      ! -- CHECK FOR EXACT SOLUTION
      itmax = THIS%ITER1
      IF (rmax ==  DZERO) THEN
        itmax = 0
        ICNVG = 1
      END IF
      !
      ! -- SOLUTION BY THE CONJUGATE GRADIENT METHOD
      IF (THIS%ILINMETH ==  1) THEN
        CALL imsbase_cg(ICNVG, itmax, innerit,                             &
                             THIS%NEQ, THIS%NJA, THIS%NIAPC, THIS%NJAPC,        &
                             THIS%IPC, THIS%NITERC, THIS%ICNVGOPT, THIS%NORTH,  &
                             THIS%DVCLOSE, THIS%RCLOSE, THIS%L2NORM0,           &
                             THIS%EPFACT, THIS%IA0, THIS%JA0, THIS%A0,          &
                             THIS%IAPC, THIS%JAPC, THIS%APC,                    &
                             THIS%X, THIS%RHS, THIS%D, THIS%P, THIS%Q, THIS%Z,  &
                             THIS%NJLU, THIS%IW, THIS%JLU,                      &
                             NCONV, CONVNMOD, CONVMODSTART, LOCDV, LOCDR,       &
                             CACCEL, ITINNER, CONVLOCDV, CONVLOCDR,             &
                             DVMAX, DRMAX, CONVDVMAX, CONVDRMAX)
      !
      ! -- SOLUTION BY THE BICONJUGATE GRADIENT STABILIZED METHOD
      ELSE IF (THIS%ILINMETH ==  2) THEN
        CALL ims_base_bcgs(ICNVG, itmax, innerit,                           &
                               THIS%NEQ, THIS%NJA, THIS%NIAPC, THIS%NJAPC,      &
                               THIS%IPC, THIS%NITERC, THIS%ICNVGOPT, THIS%NORTH,&
                               THIS%ISCL, THIS%DSCALE,                          &
                               THIS%DVCLOSE, THIS%RCLOSE, THIS%L2NORM0,         &
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
      ! -- BACK PERMUTE AMAT, SOLUTION, AND RHS
      IF (THIS%IORD.NE.0) THEN
        CALL ims_dperm(THIS%NEQ, THIS%NJA, THIS%A0, THIS%JA0, THIS%IA0,         &
     &                 THIS%AMAT, THIS%JA,THIS%IA,THIS%IORDER,THIS%ID,1)
        CALL ims_vperm(THIS%NEQ, THIS%X, THIS%IORDER)
        CALL ims_vperm(THIS%NEQ, THIS%RHS, THIS%IORDER)
      END IF
      !
      ! -- UNSCALE PROBLEM
      IF (THIS%ISCL.NE.0) THEN
        CALL ims_base_scale(1, THIS%ISCL,                                   &
                                THIS%NEQ, THIS%NJA, THIS%IA, THIS%JA,           &
                                THIS%AMAT, THIS%X, THIS%RHS,                    &
                                THIS%DSCALE, THIS%DSCALE2)
      END IF
      !
      ! -- SET IMS INNER ITERATION NUMBER (IN_ITER) TO NUMBER OF
      !       IMSLINEAR INNER ITERATIONS (innerit)
      IN_ITER = innerit
      !
      ! -- RETURN
      RETURN
    END SUBROUTINE imslinear_ap

END MODULE IMSLinearModule

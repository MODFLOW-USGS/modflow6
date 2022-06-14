MODULE IMSLinearModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, LENSOLUTIONNAME, LENMEMPATH,          &
                             IZERO, DZERO, DPREC, DSAME,                       &
                             DEM8, DEM6, DEM5, DEM4, DEM3, DEM2, DEM1,         &
                             DHALF, DONE, DTWO,                                &
                             VDEBUG
  use GenericUtilitiesModule, only: sim_message 
  use IMSLinearBaseModule, only: ims_base_cg, ims_base_bcgs, &
                                 ims_base_pccrs, ims_base_calc_order, &
                                 ims_base_scale, ims_base_pcu, &
                                 ims_base_residual
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
    integer(I4B), POINTER :: IACPC => NULL()                             !< preconditioner CRS row pointers
    integer(I4B), POINTER :: NITERC => NULL()                            !< 
    integer(I4B), POINTER :: NIABCGS => NULL()                           !< size of working vectors for BCGS linear accelerator
    integer(I4B), POINTER :: NIAPC => NULL()                             !< preconditioner number of rows
    integer(I4B), POINTER :: NJAPC => NULL()                             !< preconditioner number of non-zero entries
    real(DP), POINTER :: DVCLOSE => NULL()                               !< dependent variable convergence criteria
    real(DP), POINTER :: RCLOSE => NULL()                                !< flow convergence criteria
    real(DP), POINTER :: RELAX => NULL()                                 !< preconditioner MILU0/MILUT relaxation factor
    real(DP), POINTER :: EPFACT => NULL()                                !< factor for decreasing convergence criteria in seubsequent Picard iterations
    real(DP), POINTER :: L2NORM0 => NULL()                               !< initial L2 norm
    ! -- ilut variables
    integer(I4B), POINTER :: LEVEL => NULL()                             !< preconditioner number of levels
    real(DP), POINTER :: DROPTOL => NULL()                               !< preconditioner drop tolerance
    integer(I4B), POINTER :: NJLU => NULL()                              !< length of jlu work vector
    integer(I4B), POINTER :: NJW => NULL()                               !< length of jw work vector
    integer(I4B), POINTER :: NWLU => NULL()                              !< length of wlu work vector
    ! -- pointers to solution variables
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
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: T => NULL()           !< BICGSTAB real working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: V => NULL()           !< BICGSTAB real working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: DHAT => NULL()        !< BICGSTAB real working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: PHAT => NULL()        !< BICGSTAB real working array
    real(DP), POINTER, DIMENSION(:), CONTIGUOUS :: QHAT => NULL()        !< rBICGSTAB eal working array
    ! POINTERS FOR USE WITH BOTH ORIGINAL AND RCM ORDERINGS
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: IA0 => NULL()     !< pointer to current CRS row pointers
    integer(I4B), POINTER, DIMENSION(:), CONTIGUOUS :: JA0 => NULL()     !< pointer to current CRS column pointers
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
  
  
  CONTAINS
  
    !> @ brief Allocate storage and read data
    !!
    !!  Allocate storage for linear accelerators and read data
    !!
    !<
    SUBROUTINE imslinear_ar(this, NAME, parser, IOUT, IPRIMS, MXITER, IFDPARAM, &
                            IMSLINEARM, NEQ, NJA, IA, JA, AMAT, RHS, X,         &
                            NINNER, LFINDBLOCK)
      ! -- modules
      use MemoryManagerModule, only: mem_allocate
      use MemoryHelperModule,  only: create_mem_path
      use SimModule, only: store_error, count_errors,            &
                           deprecation_warning
      ! -- dummy variables
      CLASS(ImsLinearDataType), INTENT(INOUT) :: this            !< ImsLinearDataType instance
      CHARACTER (LEN=LENSOLUTIONNAME), INTENT(IN) :: NAME        !< solution name
      type(BlockParserType) :: parser                            !< block parser
      integer(I4B), INTENT(IN) :: IOUT                           !< simulation listing file unit
      integer(I4B), TARGET, INTENT(IN) :: IPRIMS                 !< print option
      integer(I4B), INTENT(IN) :: MXITER                         !< maximum outer iterations
      integer(I4B), INTENT(IN) :: IFDPARAM                       !< complexity option
      integer(I4B), INTENT(INOUT) :: IMSLINEARM                  !< linear method option (1) CG (2) BICGSTAB 
      integer(I4B), TARGET, INTENT(IN) :: NEQ                    !< number of equations
      integer(I4B), TARGET, INTENT(IN) :: NJA                    !< number of non-zero entries in the coefficient matrix
      integer(I4B), DIMENSION(NEQ+1), TARGET, INTENT(IN) :: IA   !< pointer to the start of a row in the coefficient matrix
      integer(I4B), DIMENSION(NJA), TARGET, INTENT(IN) :: JA     !< column pointer
      real(DP), DIMENSION(NJA), TARGET, INTENT(IN) :: AMAT       !< coefficient matrix
      real(DP), DIMENSION(NEQ), TARGET, INTENT(INOUT) :: RHS     !< right-hand side
      real(DP), DIMENSION(NEQ), TARGET, INTENT(INOUT) :: X       !< dependent variables
      integer(I4B), TARGET, INTENT(INOUT) :: NINNER              !< maximum number of inner iterations
      integer(I4B), INTENT(IN), OPTIONAL :: LFINDBLOCK           !< flag indicating if the linear block is present (1) or missing (0)

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
      this%memoryPath = create_mem_path(name, 'IMSLINEAR')
      !
      ! -- SET POINTERS TO SOLUTION STORAGE
      this%IPRIMS => IPRIMS
      this%NEQ => NEQ
      this%NJA => NJA
      this%IA => IA
      this%JA => JA
      this%AMAT => AMAT
      this%RHS => RHS
      this%X => X
      !
      ! -- ALLOCATE SCALAR VARIABLES
      call this%allocate_scalars()
      !
      ! -- initialize iout
      this%iout = iout
      !
      ! -- DEFAULT VALUES
      this%IORD = 0
      this%ISCL = 0
      this%IPC = 0
      this%LEVEL = 0
      !
      ! -- TRANSFER COMMON VARIABLES FROM IMS TO IMSLINEAR
      this%ILINMETH = 0
      
      this%IACPC = 0
      this%RELAX = DZERO !0.97
      
      this%DROPTOL = DZERO
      
      this%NORTH = 0

      this%ICNVGOPT = 0
      !
      ! -- PRINT A MESSAGE IDENTIFYING IMSLINEAR SOLVER PACKAGE
      write(iout,2000)
02000 FORMAT (1X,/1X,'IMSLINEAR -- UNSTRUCTURED LINEAR SOLUTION',               &
     &        ' PACKAGE, VERSION 8, 04/28/2017')
      !
      ! -- SET DEFAULT IMSLINEAR PARAMETERS
      CALL this%SET_IMSLINEAR_INPUT(IFDPARAM)
      NINNER = this%iter1
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
                this%ICNVGOPT = 1
              else if (keyword == 'L2NORM_RCLOSE') then
                this%ICNVGOPT = 2
              else if (keyword == 'RELATIVE_RCLOSE') then
                this%ICNVGOPT = 3
              else if (keyword == 'L2NORM_RELATIVE_RCLOSE') then
                this%ICNVGOPT = 4
              end if
            case ('INNER_MAXIMUM')
              i = parser%GetInteger()
              this%iter1 = i
              NINNER = i
            case ('LINEAR_ACCELERATION')
              call parser%GetStringCaps(keyword)
              if (keyword.eq.'CG') then
                this%ILINMETH = 1
              else if (keyword.eq.'BICGSTAB') then
                this%ILINMETH = 2
              else
                this%ILINMETH = 0
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
              this%ISCL = i
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
              this%IORD = i
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
              this%DROPTOL = r
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
      
      IMSLINEARM = this%ILINMETH
      !
      ! -- DETERMINE PRECONDITIONER
      IF (this%LEVEL > 0 .OR. this%DROPTOL > DZERO) THEN
        this%IPC = 3
      ELSE
        this%IPC = 1
      END IF
      IF (this%RELAX > DZERO) THEN
        this%IPC = this%IPC + 1
      END IF
      !
      ! -- ERROR CHECKING FOR OPTIONS
      IF (this%ISCL < 0 ) this%ISCL = 0
      IF (this%ISCL > 2 ) THEN
        WRITE(errmsg,'(A)') 'IMSLINEAR7AR ISCL MUST BE <= 2'
        call store_error(errmsg)
      END IF
      IF (this%IORD < 0 ) this%IORD = 0
      IF (this%IORD > 2) THEN
        WRITE(errmsg,'(A)') 'IMSLINEAR7AR IORD MUST BE <= 2'
        call store_error(errmsg)
      END IF
      IF (this%NORTH < 0) THEN
        WRITE(errmsg,'(A)') 'IMSLINEAR7AR NORTH MUST >= 0'
        call store_error(errmsg)
      END IF
      IF (this%RCLOSE == DZERO) THEN
        IF (this%ICNVGOPT /= 3) THEN
          WRITE(errmsg,'(A)') 'IMSLINEAR7AR RCLOSE MUST > 0.0'
          call store_error(errmsg)
        END IF
      END IF
      IF (this%RELAX < DZERO) THEN
        WRITE(errmsg,'(A)') 'IMSLINEAR7AR RELAX MUST BE >= 0.0'
        call store_error(errmsg)
      END IF
      IF (this%RELAX > DONE) THEN
        WRITE(errmsg,'(A)') 'IMSLINEAR7AR RELAX MUST BE <= 1.0'
        call store_error(errmsg)
      END IF
      !
      ! -- CHECK FOR ERRORS IN IMSLINEAR      
      if (count_errors() > 0) then
        call parser%StoreErrorUnit()
      endif
      !
      ! -- INITIALIZE IMSLINEAR VARIABLES
      this%NITERC = 0
      !
      ! -- ALLOCATE AND INITIALIZE MEMORY FOR IMSLINEAR
      iscllen  = 1
      IF (this%ISCL.NE.0 ) iscllen  = NEQ
      CALL mem_allocate(this%DSCALE, iscllen, 'DSCALE', TRIM(this%memoryPath))
      CALL mem_allocate(this%DSCALE2, iscllen, 'DSCALE2', TRIM(this%memoryPath))
      !      
      ! -- ALLOCATE MEMORY FOR PRECONDITIONING MATRIX
      ijlu      = 1
      ijw       = 1
      iwlu      = 1
      !
      ! -- ILU0 AND MILU0
      this%NIAPC = this%NEQ
      this%NJAPC = this%NJA
      !
      ! -- ILUT AND MILUT
      IF (this%IPC ==  3 .OR. this%IPC ==  4) THEN
        this%NIAPC = this%NEQ
        IF (this%LEVEL > 0) THEN
          iwk = this%NEQ * (this%LEVEL * 2 + 1)
        ELSE
          iwk = 0
          DO n = 1, NEQ
            i = IA(n+1) - IA(n)
            IF (i > iwk) THEN
              iwk = i
            END IF
          END DO
          iwk = this%NEQ * iwk
        END IF
        this%NJAPC = iwk
        ijlu = iwk
        ijw = 2 * this%NEQ
        iwlu = this%NEQ + 1
      END IF
      this%NJLU = ijlu
      this%NJW = ijw
      this%NWLU = iwlu
      !
      ! -- ALLOCATE BASE PRECONDITIONER VECTORS
      CALL mem_allocate(this%IAPC, this%NIAPC+1, 'IAPC', TRIM(this%memoryPath))
      CALL mem_allocate(this%JAPC, this%NJAPC, 'JAPC', TRIM(this%memoryPath))
      CALL mem_allocate(this%APC, this%NJAPC, 'APC', TRIM(this%memoryPath))
      !
      ! -- ALLOCATE MEMORY FOR ILU0 AND MILU0 NON-ZERO ROW ENTRY VECTOR
      CALL mem_allocate(this%IW, this%NIAPC, 'IW', TRIM(this%memoryPath))
      CALL mem_allocate(this%W, this%NIAPC, 'W', TRIM(this%memoryPath))
      !
      ! -- ALLOCATE MEMORY FOR ILUT VECTORS
      CALL mem_allocate(this%JLU, ijlu, 'JLU', TRIM(this%memoryPath))
      CALL mem_allocate(this%JW, ijw, 'JW', TRIM(this%memoryPath))
      CALL mem_allocate(this%WLU, iwlu, 'WLU', TRIM(this%memoryPath))
      !
      ! -- GENERATE IAPC AND JAPC FOR ILU0 AND MILU0
      IF (this%IPC ==  1 .OR. this%IPC ==  2) THEN
        CALL ims_base_pccrs(this%NEQ,this%NJA,this%IA,this%JA,              &
                                this%IAPC,this%JAPC)
      END IF
      !
      ! -- ALLOCATE SPACE FOR PERMUTATION VECTOR
      i0     = 1
      iolen  = 1
      IF (this%IORD.NE.0) THEN
        i0     = this%NEQ
        iolen  = this%NJA
      END IF
      CALL mem_allocate(this%LORDER, i0, 'LORDER', TRIM(this%memoryPath))
      CALL mem_allocate(this%IORDER, i0, 'IORDER', TRIM(this%memoryPath))
      CALL mem_allocate(this%IARO, i0+1, 'IARO', TRIM(this%memoryPath))
      CALL mem_allocate(this%JARO, iolen, 'JARO', TRIM(this%memoryPath))
      CALL mem_allocate(this%ARO, iolen, 'ARO', TRIM(this%memoryPath))
      !
      ! -- ALLOCATE WORKING VECTORS FOR IMSLINEAR SOLVER
      CALL mem_allocate(this%ID, this%NEQ, 'ID', TRIM(this%memoryPath))
      CALL mem_allocate(this%D, this%NEQ, 'D', TRIM(this%memoryPath))
      CALL mem_allocate(this%P, this%NEQ, 'P', TRIM(this%memoryPath))
      CALL mem_allocate(this%Q, this%NEQ, 'Q', TRIM(this%memoryPath))
      CALL mem_allocate(this%Z, this%NEQ, 'Z', TRIM(this%memoryPath))
      !
      ! -- ALLOCATE MEMORY FOR BCGS WORKING ARRAYS
      this%NIABCGS = 1
      IF (this%ILINMETH ==  2) THEN
        this%NIABCGS = this%NEQ
      END IF
      CALL mem_allocate(this%T, this%NIABCGS, 'T', TRIM(this%memoryPath))
      CALL mem_allocate(this%V, this%NIABCGS, 'V', TRIM(this%memoryPath))
      CALL mem_allocate(this%DHAT, this%NIABCGS, 'DHAT', TRIM(this%memoryPath))
      CALL mem_allocate(this%PHAT, this%NIABCGS, 'PHAT', TRIM(this%memoryPath))
      CALL mem_allocate(this%QHAT, this%NIABCGS, 'QHAT', TRIM(this%memoryPath))
      !
      ! -- INITIALIZE IMSLINEAR VECTORS
      DO n = 1, iscllen
        this%DSCALE(n)  = DONE
        this%DSCALE2(n) = DONE
      END DO
      DO n = 1, this%NJAPC
        this%APC(n)  = DZERO
      END DO
      !
      ! -- WORKING VECTORS
      DO n = 1, this%NEQ
        this%ID(n)   = IZERO
        this%D(n)    = DZERO
        this%P(n)    = DZERO
        this%Q(n)    = DZERO
        this%Z(n)    = DZERO
      END DO
      DO n = 1, this%NIAPC
        this%IW(n)   = IZERO
        this%W(n)    = DZERO
      END DO
      !
      ! -- BCGS WORKING VECTORS
      DO n = 1, this%NIABCGS
        this%T(n)    = DZERO
        this%V(n)    = DZERO
        this%DHAT(n) = DZERO
        this%PHAT(n) = DZERO
        this%QHAT(n) = DZERO
      END DO
      !
      ! -- ILUT AND MILUT WORKING VECTORS      
      DO n = 1, ijlu
        this%JLU(n)   = DZERO
      END DO
      DO n = 1, ijw
        this%JW(n)   = DZERO
      END DO
      DO n = 1, iwlu
        this%WLU(n)  = DZERO
      END DO
      !
      ! -- REORDERING VECTORS
      DO n = 1, i0 + 1
        this%IARO(n) = IZERO
      END DO
      DO n = 1, iolen
        this%JARO(n) = IZERO
        this%ARO(n)  = DZERO
      END DO
      !
      ! -- REVERSE CUTHILL MCKEE AND MINIMUM DEGREE ORDERING
      IF (this%IORD.NE.0) THEN
        CALL ims_base_calc_order(this%IORD,this%NEQ, this%NJA,this%IA,this%JA,   &
                                 this%LORDER,this%IORDER)
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
      class(ImsLinearDataType), intent(inout) :: this  !< ImsLinearDataType instance
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
      ! -- write common variables to all linear accelerators
      write(this%iout,2010)                                         &
                        clintit(this%ILINMETH), MXITER, this%ITER1, &
                        clin(this%ILINMETH), cipc(this%IPC),        &
                        cscale(this%ISCL), corder(this%IORD),       &
                        this%NORTH, this%DVCLOSE, this%RCLOSE,      &
                        this%ICNVGOPT, ccnvgopt(this%ICNVGOPT),     &
                        this%RELAX
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
      class(ImsLinearDataType), intent(inout) :: this  !< ImsLinearDataType instance
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
      class(ImsLinearDataType), intent(inout) :: this !< linear datatype instance
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
    SUBROUTINE imslinear_set_input(this, IFDPARAM)
      ! -- dummy variables
      CLASS(ImsLinearDataType), INTENT(INOUT) :: this !< ImsLinearDataType instance
      integer(I4B), INTENT(IN) :: IFDPARAM            !< complexity option
      ! -- code
      SELECT CASE ( IFDPARAM )
        !
        ! -- Simple option
        CASE(1)
          this%ITER1 = 50
          this%ILINMETH=1
          this%IPC = 1
          this%ISCL = 0
          this%IORD = 0
          this%DVCLOSE = DEM3
          this%RCLOSE = DEM1
          this%RELAX = DZERO
          this%LEVEL = 0
          this%DROPTOL = DZERO
          this%NORTH = 0
        !
        ! -- Moderate
        CASE(2)
          this%ITER1 = 100
          this%ILINMETH=2
          this%IPC = 2
          this%ISCL = 0
          this%IORD = 0
          this%DVCLOSE = DEM2
          this%RCLOSE = DEM1
          this%RELAX = 0.97D0
          this%LEVEL = 0
          this%DROPTOL = DZERO
          this%NORTH = 0
        !
        ! -- Complex
        CASE(3)
          this%ITER1 = 500
          this%ILINMETH=2
          this%IPC = 3
          this%ISCL = 0
          this%IORD = 0
          this%DVCLOSE = DEM1
          this%RCLOSE = DEM1
          this%RELAX = DZERO
          this%LEVEL = 5
          this%DROPTOL = DEM4
          this%NORTH = 2
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
    SUBROUTINE imslinear_ap(this,ICNVG,KSTP,KITER,IN_ITER,                  &
                            NCONV, CONVNMOD, CONVMODSTART, LOCDV, LOCDR,    &
                            CACCEL, ITINNER, CONVLOCDV, CONVLOCDR,          &
                            DVMAX, DRMAX, CONVDVMAX, CONVDRMAX)
      ! -- modules
      USE SimModule
      ! -- dummy variables
      CLASS(ImsLinearDataType), INTENT(INOUT) :: this                         !< ImsLinearDataType instance
      integer(I4B), INTENT(INOUT)                          :: ICNVG           !< convergence flag (1) non-convergence (0)
      integer(I4B), INTENT(IN)                             :: KSTP            !< time step number
      integer(I4B), INTENT(IN)                             :: KITER           !< outer iteration number
      integer(I4B), INTENT(INOUT)                          :: IN_ITER         !< inner iteration number
      ! -- convergence information dummy variables
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
      real(DP) :: dnrm2
      !
      ! -- set epfact based on timestep
      IF (this%ICNVGOPT ==  2) THEN
        IF (KSTP ==  1) THEN
          this%EPFACT = 0.01
        ELSE
          this%EPFACT = 0.10
        END IF
      ELSE IF (this%ICNVGOPT ==  4) THEN
        this%EPFACT = DEM4
      ELSE
        this%EPFACT = DONE
      END IF
      !
      ! -- SCALE PROBLEM
      IF (this%ISCL.NE.0) THEN
        CALL ims_base_scale(0,this%ISCL,                                    &
                         this%NEQ,this%NJA,this%IA,this%JA,                     &
                         this%AMAT,this%X,this%RHS,                             &
                         this%DSCALE,this%DSCALE2)
      END IF
      !
      ! -- PERMUTE ROWS, COLUMNS, AND RHS
      IF (this%IORD /= 0) THEN
        CALL dperm(this%NEQ, this%AMAT, this%JA, this%IA, &
                   this%ARO, this%JARO, this%IARO, &
                   this%LORDER, this%ID, 1)
        CALL dvperm(this%NEQ, this%X, this%LORDER)
        CALL dvperm(this%NEQ, this%RHS, this%LORDER)
        this%IA0 => this%IARO
        this%JA0 => this%JARO
        this%A0  => this%ARO
      ELSE
        this%IA0 => this%IA
        this%JA0 => this%JA
        this%A0  => this%AMAT
      END IF
      !
      ! -- UPDATE PRECONDITIONER
      CALL ims_base_pcu(this%iout,this%NJA,this%NEQ,this%NIAPC,this%NJAPC,  &
                            this%IPC, this%RELAX, this%A0, this%IA0, this%JA0,  &
                            this%APC,this%IAPC,this%JAPC,this%IW,this%W,        &
                            this%LEVEL, this%DROPTOL, this%NJLU, this%NJW,      &
                            this%NWLU, this%JLU, this%JW, this%WLU)
      !
      ! -- INITIALIZE SOLUTION VARIABLE AND ARRAYS
      IF (KITER ==  1 ) this%NITERC = 0
      irc    = 1
      ICNVG  = 0
      DO n = 1, this%NEQ
        this%D(n) = DZERO
        this%P(n) = DZERO
        this%Q(n) = DZERO
        this%Z(n) = DZERO
      END DO
      !
      ! -- CALCULATE INITIAL RESIDUAL
      call ims_base_residual(this%NEQ, this%NJA, this%X, this%RHS, this%D, &
                             this%A0, this%IA0, this%JA0)
      this%L2NORM0 = dnrm2(this%NEQ, this%D, 1)
      !
      ! -- CHECK FOR EXACT SOLUTION
      itmax = this%ITER1
      IF (this%L2NORM0 ==  DZERO) THEN
        itmax = 0
        ICNVG = 1
      END IF
      !
      ! -- SOLUTION BY THE CONJUGATE GRADIENT METHOD
      IF (this%ILINMETH ==  1) THEN
        CALL ims_base_cg(ICNVG, itmax, innerit,                             &
                             this%NEQ, this%NJA, this%NIAPC, this%NJAPC,        &
                             this%IPC, this%NITERC, this%ICNVGOPT, this%NORTH,  &
                             this%DVCLOSE, this%RCLOSE, this%L2NORM0,           &
                             this%EPFACT, this%IA0, this%JA0, this%A0,          &
                             this%IAPC, this%JAPC, this%APC,                    &
                             this%X, this%RHS, this%D, this%P, this%Q, this%Z,  &
                             this%NJLU, this%IW, this%JLU,                      &
                             NCONV, CONVNMOD, CONVMODSTART, LOCDV, LOCDR,       &
                             CACCEL, ITINNER, CONVLOCDV, CONVLOCDR,             &
                             DVMAX, DRMAX, CONVDVMAX, CONVDRMAX)
      !
      ! -- SOLUTION BY THE BICONJUGATE GRADIENT STABILIZED METHOD
      ELSE IF (this%ILINMETH ==  2) THEN
        CALL ims_base_bcgs(ICNVG, itmax, innerit,                           &
                               this%NEQ, this%NJA, this%NIAPC, this%NJAPC,      &
                               this%IPC, this%NITERC, this%ICNVGOPT, this%NORTH,&
                               this%ISCL, this%DSCALE,                          &
                               this%DVCLOSE, this%RCLOSE, this%L2NORM0,         &
                               this%EPFACT,  this%IA0, this%JA0, this%A0,       &
                               this%IAPC, this%JAPC, this%APC,                  &
                               this%X, this%RHS, this%D, this%P, this%Q,        &
                               this%T, this%V, this%DHAT, this%PHAT, this%QHAT, &
                               this%NJLU, this%IW, this%JLU,                    &
                               NCONV, CONVNMOD, CONVMODSTART, LOCDV, LOCDR,     &
                               CACCEL, ITINNER, CONVLOCDV, CONVLOCDR,           &
                               DVMAX, DRMAX, CONVDVMAX, CONVDRMAX)
      END IF
      !
      ! -- BACK PERMUTE AMAT, SOLUTION, AND RHS
      IF (this%IORD /= 0) THEN
        CALL dperm(this%NEQ, this%A0, this%JA0, this%IA0, &
                   this%AMAT, this%JA, this%IA, &
                   this%IORDER, this%ID, 1)
        CALL dvperm(this%NEQ, this%X, this%IORDER)
        CALL dvperm(this%NEQ, this%RHS, this%IORDER)
      END IF
      !
      ! -- UNSCALE PROBLEM
      IF (this%ISCL.NE.0) THEN
        CALL ims_base_scale(1, this%ISCL,                                   &
                                this%NEQ, this%NJA, this%IA, this%JA,           &
                                this%AMAT, this%X, this%RHS,                    &
                                this%DSCALE, this%DSCALE2)
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

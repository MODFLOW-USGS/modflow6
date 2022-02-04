MODULE PetscSolverModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, LENSOLUTIONNAME, LENMEMPATH,          &
                             IZERO, DZERO, DPREC, DSAME,                       &
                             DEM8, DEM6, DEM5, DEM4, DEM3, DEM2, DEM1,         &
                             DHALF, DONE, DTWO,                                &
                             VDEBUG
  use GenericUtilitiesModule, only: sim_message
  use BlockParserModule, only: BlockParserType

  IMPLICIT NONE
  private
  
  TYPE, PUBLIC :: PetscSolverDataType
    character(len=LENMEMPATH) :: memoryPath                              !< the path for storing variables in the memory manager
    integer(I4B), POINTER :: iout => NULL()                              !< simulation listing file unit
    integer(I4B), POINTER :: iprims => NULL()                            !< print flag
    ! -- pointers to solution variables
    integer(I4B), POINTER :: neq => NULL()                               !< number of equations (rows in matrix)
    integer(I4B), POINTER :: nja => NULL()                               !< number of non-zero values in amat
    integer(I4B), dimension(:), pointer, contiguous :: ia => NULL()      !< position of start of each row
    integer(I4B), dimension(:), pointer, contiguous :: ja => NULL()      !< column pointer
    real(DP), dimension(:), pointer, contiguous :: amat => NULL()        !< coefficient matrix
    real(DP), dimension(:), pointer, contiguous :: rhs => NULL()         !< right-hand side of equation
    real(DP), dimension(:), pointer, contiguous :: x => NULL()           !< dependent variable
    
    ! PROCEDURES (METHODS)
    CONTAINS
      PROCEDURE :: petcs_solver_allocate_read
      PROCEDURE :: petcs_solver_execute
      procedure :: petcs_solver_deallocate
  END TYPE PetscSolverDataType
  
  
  CONTAINS
  
    !> @brief Allocate storage and read data
    !!
    !<
    SUBROUTINE petcs_solver_allocate_read(this, name, parser, IOUT, IPRIMS,     &
                            NEQ, NJA, IA, JA, AMAT, RHS, X)
      ! -- modules
      use MemoryManagerModule, only: mem_allocate
      use MemoryHelperModule,  only: create_mem_path
      use SimModule, only: store_error, count_errors,            &
                           deprecation_warning
      ! -- dummy variables
      CLASS(PetscSolverDataType), INTENT(INOUT) :: this          !< PetscSolverDataType instance
      CHARACTER (LEN=lensolutionname), INTENT(IN) :: name        !< solution name
      type(BlockParserType) :: parser                            !< block parser
      integer(I4B), INTENT(IN) :: iout                           !< simulation listing file unit
      integer(I4B), TARGET, INTENT(IN) :: iprims                 !< print option
      integer(I4B), TARGET, INTENT(IN) :: NEQ                    !< number of equations
      integer(I4B), TARGET, INTENT(IN) :: NJA                    !< number of non-zero entries in the coefficient matrix
      integer(I4B), DIMENSION(NEQ+1), TARGET, INTENT(IN) :: IA   !< pointer to the start of a row in the coefficient matrix
      integer(I4B), DIMENSION(NJA), TARGET, INTENT(IN) :: JA     !< column pointer
      real(DP), DIMENSION(NJA), TARGET, INTENT(IN) :: AMAT       !< coefficient matrix
      real(DP), DIMENSION(NEQ), TARGET, INTENT(INOUT) :: RHS     !< right-hand side
      real(DP), DIMENSION(NEQ), TARGET, INTENT(INOUT) :: X       !< dependent variables

      ! -- local variables
      LOGICAL :: lreaddata
      character(len=LINELENGTH) :: errmsg
      character(len=LINELENGTH) :: keyword
      integer(I4B) :: ierr
      logical :: isfound, endOfBlock

      !
      ! -- DEFINE NAME      
      this%memoryPath = create_mem_path(name, 'PetscSolver')
      !
      ! -- SET POINTERS
      this%IPRIMS => IPRIMS
      this%NEQ => NEQ
      this%NJA => NJA
      this%IA => IA
      this%JA => JA
      this%AMAT => AMAT
      this%RHS => RHS
      this%X => X
      !
      ! -- initialize iout
      this%iout = iout
      !
      ! -- SET DEFAULT PARAMETERS (TODO)
      
      !
      ! -- get PETSC block
      if (lreaddata) then
        call parser%GetBlock('PETSC', isfound, ierr, &
          supportOpenClose=.true., blockRequired=.FALSE.)
      else
        isfound = .FALSE.
      end if
      !
      ! -- parse PETSC block if detected
      if (isfound) then
        write(iout,'(/1x,a)')'PROCESSING PETSC DATA'
        do
          call parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
          call parser%GetStringCaps(keyword)
          ! -- parse keyword
          select case (keyword)
            ! -- default
            case default
                write(errmsg,'(3a)')                                             &
                  'UNKNOWN PETSC KEYWORD (', trim(keyword), ').'
              call store_error(errmsg)
          end select
        end do
        write(iout,'(1x,a)') 'END OF PETSC DATA'
      end if
      
      ! -- RETURN
      RETURN
    END SUBROUTINE petcs_solver_allocate_read

    !> @brief Deallocate memory
    !!
    !!  Deallocate linear accelerator memory.
    !!
    !<
    subroutine petcs_solver_deallocate(this)
      ! -- modules
      use MemoryManagerModule, only: mem_deallocate
      ! -- dummy variables
      class(PetscSolverDataType), intent(inout) :: this !< linear datatype instance
      
      ! -- scalars
      call mem_deallocate(this%iout)
      
      ! -- nullify pointers
      nullify(this%iprims)
      nullify(this%neq)
      nullify(this%nja)
      nullify(this%ia)
      nullify(this%ja)
      nullify(this%amat)
      nullify(this%rhs)
      nullify(this%x)
      
      ! -- return
      return
    end subroutine petcs_solver_deallocate
    

    !> @brief Solve linear equation
    !!
    !< 
    SUBROUTINE petcs_solver_execute(this,ICNVG,KSTP,KITER,IN_ITER,                  &
                            NCONV, CONVNMOD, CONVMODSTART, LOCDV, LOCDR,    &
                            CACCEL, ITINNER, CONVLOCDV, CONVLOCDR,          &
                            DVMAX, DRMAX, CONVDVMAX, CONVDRMAX)
      ! -- modules
      USE SimModule
      ! -- dummy variables
      CLASS(PetscSolverDataType), INTENT(INOUT) :: this                         !< PetscSolverDataType instance
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
    END SUBROUTINE petcs_solver_execute

END MODULE PetscSolverModule

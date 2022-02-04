module PetscSolverModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, LENSOLUTIONNAME, LENMEMPATH,          &
                             IZERO, DZERO, DPREC, DSAME,                       &
                             DEM8, DEM6, DEM5, DEM4, DEM3, DEM2, DEM1,         &
                             DHALF, DONE, DTWO,                                &
                             VDEBUG
  use GenericUtilitiesModule, only: sim_message
  use BlockParserModule, only: BlockParserType

  implicit none
  private
  
  type, public :: PetscSolverDataType
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
    
    ! procedures (methods)
    contains
      procedure :: petcs_solver_allocate_read
      procedure :: petcs_solver_execute
      procedure :: petcs_solver_deallocate
  end type PetscSolverDataType
  
  
  contains
  
    !> @brief Allocate storage and read data
    !!
    !<
    subroutine petcs_solver_allocate_read(this, name, parser, IOUT, IPRIMS,     &
                            NEQ, NJA, IA, JA, AMAT, RHS, X)
      ! -- modules
      use MemoryManagerModule, only: mem_allocate
      use MemoryHelperModule,  only: create_mem_path
      use SimModule, only: store_error, count_errors,            &
                           deprecation_warning
      ! -- dummy variables
      class(PetscSolverDataType), intent(INOUT) :: this          !< PetscSolverDataType instance
      character (LEN=lensolutionname), intent(IN) :: name        !< solution name
      type(BlockParserType) :: parser                            !< block parser
      integer(I4B), intent(IN) :: iout                           !< simulation listing file unit
      integer(I4B), TARGET, intent(IN) :: iprims                 !< print option
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
    SUBROUTINE petcs_solver_execute(this)
#include <petsc/finclude/petscksp.h>
      use petscksp
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                   Variable declarations
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!  Variables:
!     ksp     - linear solver context
!     ksp      - Krylov subspace method context
!     pc       - preconditioner context
!     x, b, u  - approx solution, right-hand-side, exact solution vectors
!     A        - matrix that defines linear system
!     its      - iterations for convergence
!     norm     - norm of error in solution
!     rctx     - random number generator context
!
!  Note that vectors are declared as PETSc "Vec" objects.  These vectors
!  are mathematical objects that contain more than just an array of
!  double precision numbers. I.e., vectors in PETSc are not just
!        double precision x(*).
!  However, local vector data can be easily accessed via VecGetArray().
!  See the Fortran section of the PETSc users manual for details.
!
      
      ! -- dummy variables
      CLASS(PetscSolverDataType), INTENT(INOUT) :: this                       !< PetscSolverDataType instance

      ! -- local variables
      PetscReal  norm
      PetscInt  i,j,II,JJ,m,n,its
      PetscInt  Istart,Iend,ione
      PetscErrorCode ierr
      PetscMPIInt     rank,size
      PetscBool   flg
      PetscScalar v,one,neg_one
      Vec         x,b,u
      Mat         A
      KSP         ksp
      PetscRandom rctx

!  These variables are not currently used.
!      PC          pc
!      PCType      ptype
!      PetscReal tol


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                 Beginning of program
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
      if (ierr .ne. 0) then
        print*,'Unable to initialize PETSc'
        stop
      endif
      m = 3
      n = 3
      one  = 1.0
      neg_one = -1.0
      ione    = 1
      call PetscOptionsGetInt(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,'-m',m,flg,ierr)
      call PetscOptionsGetInt(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,'-n',n,flg,ierr)
      call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)
      call MPI_Comm_size(PETSC_COMM_WORLD,size,ierr)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!      Compute the matrix and right-hand-side vector that define
!      the linear system, Ax = b.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!  Create parallel matrix, specifying only its global dimensions.
!  When using MatCreate(), the matrix format can be specified at
!  runtime. Also, the parallel partitioning of the matrix is
!  determined by PETSc at runtime.

      call MatCreate(PETSC_COMM_WORLD,A,ierr)
      call MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,m*n,m*n,ierr)
      call MatSetFromOptions(A,ierr)
      call MatSetUp(A,ierr)

!  Currently, all PETSc parallel matrix formats are partitioned by
!  contiguous chunks of rows across the processors.  Determine which
!  rows of the matrix are locally owned.

      call MatGetOwnershipRange(A,Istart,Iend,ierr)

!  Set matrix elements for the 2-D, five-point stencil in parallel.
!   - Each processor needs to insert only elements that it owns
!     locally (but any non-local elements will be sent to the
!     appropriate processor during matrix assembly).
!   - Always specify global row and columns of matrix entries.
!   - Note that MatSetValues() uses 0-based row and column numbers
!     in Fortran as well as in C.

!     Note: this uses the less common natural ordering that orders first
!     all the unknowns for x = h then for x = 2h etc; Hence you see JH = II +- n
!     instead of JJ = II +- m as you might expect. The more standard ordering
!     would first do all variables for y = h, then y = 2h etc.

      do 10, II=Istart,Iend-1
        v = -1.0
        i = II/n
        j = II - i*n
        if (i.gt.0) then
          JJ = II - n
          call MatSetValues(A,ione,II,ione,JJ,v,INSERT_VALUES,ierr)
        endif
        if (i.lt.m-1) then
          JJ = II + n
          call MatSetValues(A,ione,II,ione,JJ,v,INSERT_VALUES,ierr)
        endif
        if (j.gt.0) then
          JJ = II - 1
          call MatSetValues(A,ione,II,ione,JJ,v,INSERT_VALUES,ierr)
        endif
        if (j.lt.n-1) then
          JJ = II + 1
          call MatSetValues(A,ione,II,ione,JJ,v,INSERT_VALUES,ierr)
        endif
        v = 4.0
        call  MatSetValues(A,ione,II,ione,II,v,INSERT_VALUES,ierr)
 10   continue

!  Assemble matrix, using the 2-step process:
!       MatAssemblyBegin(), MatAssemblyEnd()
!  Computations can be done while messages are in transition,
!  by placing code between these two statements.

      call MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
      call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)

!  Create parallel vectors.
!   - Here, the parallel partitioning of the vector is determined by
!     PETSc at runtime.  We could also specify the local dimensions
!     if desired -- or use the more general routine VecCreate().
!   - When solving a linear system, the vectors and matrices MUST
!     be partitioned accordingly.  PETSc automatically generates
!     appropriately partitioned matrices and vectors when MatCreate()
!     and VecCreate() are used with the same communicator.
!   - Note: We form 1 vector from scratch and then duplicate as needed.

      call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,m*n,u,ierr)
      call VecSetFromOptions(u,ierr)
      call VecDuplicate(u,b,ierr)
      call VecDuplicate(b,x,ierr)

!  Set exact solution; then compute right-hand-side vector.
!  By default we use an exact solution of a vector with all
!  elements of 1.0;  Alternatively, using the runtime option
!  -random_sol forms a solution vector with random components.

      call PetscOptionsHasName(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,'-random_exact_sol',flg,ierr)
      if (flg) then
         call PetscRandomCreate(PETSC_COMM_WORLD,rctx,ierr)
         call PetscRandomSetFromOptions(rctx,ierr)
         call VecSetRandom(u,rctx,ierr)
         call PetscRandomDestroy(rctx,ierr)
      else
         call VecSet(u,one,ierr)
      endif
      call MatMult(A,u,b,ierr)

!  View the exact solution vector if desired

      call PetscOptionsHasName(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,'-view_exact_sol',flg,ierr)
      if (flg) then
         call VecView(u,PETSC_VIEWER_STDOUT_WORLD,ierr)
      endif

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         Create the linear solver and set various options
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!  Create linear solver context

      call KSPCreate(PETSC_COMM_WORLD,ksp,ierr)

!  Set operators. Here the matrix that defines the linear system
!  also serves as the preconditioning matrix.

      call KSPSetOperators(ksp,A,A,ierr)


!  Set runtime options, e.g.,
!      -ksp_type <type> -pc_type <type> -ksp_monitor -ksp_rtol <rtol>
!  These options will override those specified above as long as
!  KSPSetFromOptions() is called _after_ any other customization
!  routines.

      call KSPSetFromOptions(ksp,ierr)


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                      Solve the linear system
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      call KSPSolve(ksp,b,x,ierr)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                     Check solution and clean up
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!  Check the error
      call VecAXPY(x,neg_one,u,ierr)
      call VecNorm(x,NORM_2,norm,ierr)
      call KSPGetIterationNumber(ksp,its,ierr)
      if (rank .eq. 0) then
        if (norm .gt. 1.e-12) then
           write(6,100) norm,its
        else
           write(6,110) its
        endif
      endif
  100 format('Norm of error ',e11.4,' iterations ',i5)
  110 format('Norm of error < 1.e-12 iterations ',i5)

!  Free work space.  All PETSc objects should be destroyed when they
!  are no longer needed.

      call KSPDestroy(ksp,ierr)
      call VecDestroy(u,ierr)
      call VecDestroy(x,ierr)
      call VecDestroy(b,ierr)
      call MatDestroy(A,ierr)
    END SUBROUTINE petcs_solver_execute

END MODULE PetscSolverModule

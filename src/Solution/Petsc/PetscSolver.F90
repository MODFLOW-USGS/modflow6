module PetscSolverModule
#include <petsc/finclude/petscksp.h>
  use petscksp
  
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
    Mat         Amat_petsc
    Vec         x_petsc
    Vec         rhs_petsc

    ! procedures (methods)
    contains
      procedure :: petsc_solver_allocate_read
      procedure :: petsc_solver_execute
      procedure :: petsc_solver_deallocate
      ! -- PRIVATE PROCEDURES
      procedure, private :: allocate_scalars
  end type PetscSolverDataType
  
  
  contains
  
    !> @brief Allocate storage and read data
    !!
    !<
    subroutine petsc_solver_allocate_read(this, name, parser, IOUT, IPRIMS,     &
                            NEQ, NJA, IA, JA, AMAT, RHS, X)
      ! -- modules
#include <petsc/finclude/petscksp.h>
      use petscksp
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
      integer(I4B) :: err
      logical :: isfound, endOfBlock
      PetscErrorCode ierr


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

      ! -- ALLOCATE SCALAR VARIABLES
      call this%allocate_scalars()
      !
      ! -- initialize iout
      this%iout = iout
      
      !  Create parallel matrix, specifying only its global dimensions.
      !  When using MatCreate(), the matrix format can be specified at
      !  runtime. Also, the parallel partitioning of the matrix is
      !  determined by PETSc at runtime.

      call MatCreate(PETSC_COMM_WORLD,this%Amat_petsc,ierr)
      CHKERRQ(ierr)
      call MatSetSizes(this%Amat_petsc,PETSC_DECIDE,PETSC_DECIDE,this%neq,size(this%x),ierr)
      CHKERRQ(ierr)
      call MatSetFromOptions(this%Amat_petsc,ierr)
      CHKERRQ(ierr)
      call MatSetUp(this%Amat_petsc,ierr)
      CHKERRQ(ierr)

      !  Create parallel vectors.
      !   - Here, the parallel partitioning of the vector is determined by
      !     PETSc at runtime.  We could also specify the local dimensions
      !     if desired -- or use the more general routine VecCreate().
      !   - When solving a linear system, the vectors and matrices MUST
      !     be partitioned accordingly.  PETSc automatically generates
      !     appropriately partitioned matrices and vectors when MatCreate()
      !     and VecCreate() are used with the same communicator.
      !   - Note: We form 1 vector from scratch and then duplicate as needed
      call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,size(this%x),this%x_petsc,ierr)
      CHKERRQ(ierr)
      call VecSetFromOptions(this%x_petsc,ierr)
      CHKERRQ(ierr)
      call VecDuplicate(this%x_petsc,this%rhs_petsc,ierr)
      CHKERRQ(ierr)

      !
      ! -- SET DEFAULT PARAMETERS (TODO)
      
      !
      ! -- get PETSC block
      if (lreaddata) then
        call parser%GetBlock('PETSC', isfound, err, &
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
    END SUBROUTINE petsc_solver_allocate_read

    !> @brief Deallocate memory
    !!
    !!  Deallocate linear accelerator memory.
    !!
    !<
    subroutine petsc_solver_deallocate(this)
      ! -- modules
#include <petsc/finclude/petscksp.h>
      use petscksp
      use MemoryManagerModule, only: mem_deallocate
      ! -- dummy variables
      class(PetscSolverDataType), intent(inout) :: this !< linear datatype instance
      ! -- local variables
      PetscErrorCode ierr

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

      call MatDestroy(this%Amat_petsc,ierr)
      CHKERRQ(ierr)
      call VecDestroy(this%x_petsc,ierr)
      CHKERRQ(ierr)
      call VecDestroy(this%rhs_petsc,ierr)
      CHKERRQ(ierr)
      
      ! -- return
      return
    end subroutine petsc_solver_deallocate
    

    !> @brief Solve linear equation
    !!
    !< 
    SUBROUTINE petsc_solver_execute(this)
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
!     x, rhs  - approx solution, right-hand-side, exact solution vectors
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
      PetscInt  m,n
      PetscInt  ione
      PetscErrorCode ierr
      PetscMPIInt rank, size_
      PetscBool   flg
      PetscScalar one,neg_one

      KSP         ksp
      PetscScalar,pointer :: x_pointer(:)
      integer(I4B) :: row, ipos

!  These variables are not currently used.
!      PC          pc
!      PCType      ptype
!      PetscReal tol


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                 Beginning of program
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      m = 3
      n = 3
      one  = 1.0
      neg_one = -1.0
      ione    = 1
      call PetscOptionsGetInt(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,'-m',m,flg,ierr)
      CHKERRQ(ierr)
      call PetscOptionsGetInt(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,'-n',n,flg,ierr)
      CHKERRQ(ierr)
      call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)
      CHKERRQ(ierr)
      call MPI_Comm_size(PETSC_COMM_WORLD,size_,ierr)
      CHKERRQ(ierr)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!      Compute the matrix and right-hand-side vector that define
!      the linear system, Ax = rhs.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



!  Fill matrix
      do row = 1, this%neq
        do ipos = this%ia(row), this%ia(row+1) - 1
          call MatSetValues(this%Amat_petsc, ione, row-1, ione, this%ja(ipos)-1, this%amat(ipos), INSERT_VALUES, ierr)
          CHKERRQ(ierr)
        end do
      end do

!  Assemble matrix, using the 2-step process:
!       MatAssemblyBegin(), MatAssemblyEnd()
!  Computations can be done while messages are in transition,
!  by placing code between these two statements.

      call MatAssemblyBegin(this%Amat_petsc,MAT_FINAL_ASSEMBLY,ierr)
      CHKERRQ(ierr)
      call MatAssemblyEnd(this%Amat_petsc,MAT_FINAL_ASSEMBLY,ierr)
      CHKERRQ(ierr)


      do ipos = 1, size(this%x)
        call VecSetValues(this%x_petsc, ione, ipos-1, this%x(ipos),INSERT_VALUES, ierr)
        CHKERRQ(ierr)
      end do
      call VecAssemblyBegin(this%x_petsc,ierr)
      CHKERRQ(ierr)
      call VecAssemblyEnd(this%x_petsc,ierr)
      CHKERRQ(ierr)

      ! create rhs

      do ipos = 1, size(this%rhs)
        call VecSetValues(this%rhs_petsc, ione, ipos-1, this%rhs(ipos),INSERT_VALUES, ierr)
        CHKERRQ(ierr)
      end do
      call VecAssemblyBegin(this%rhs_petsc,ierr)
      CHKERRQ(ierr)
      call VecAssemblyEnd(this%rhs_petsc,ierr)
      CHKERRQ(ierr)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         Create the linear solver and set various options
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!  Create linear solver context

      call KSPCreate(PETSC_COMM_WORLD,ksp,ierr)
      CHKERRQ(ierr)

!  Set operators. Here the matrix that defines the linear system
!  also serves as the preconditioning matrix.

      call KSPSetOperators(ksp,this%Amat_petsc,this%Amat_petsc,ierr)
      CHKERRQ(ierr)


!  Set runtime options, e.g.,
!      -ksp_type <type> -pc_type <type> -ksp_monitor -ksp_rtol <rtol>
!  These options will override those specified above as long as
!  KSPSetFromOptions() is called _after_ any other customization
!  routines.

      call KSPSetFromOptions(ksp,ierr)
      CHKERRQ(ierr)


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                      Solve the linear system
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      call KSPSolve(ksp,this%rhs_petsc,this%x_petsc,ierr)
      CHKERRQ(ierr)

      ! copy `x` to `this%x`
      call VecGetArrayReadF90(this%x_petsc, x_pointer, ierr)
      CHKERRQ(ierr)

      do ipos = 1, size(this%x)
        this%x(ipos) = x_pointer(ipos)
      end do

      call VecRestoreArrayReadF90(this%x_petsc, x_pointer, ierr)
      CHKERRQ(ierr)
      


!  Free work space.  All PETSc objects should be destroyed when they
!  are no longer needed.

      call KSPDestroy(ksp,ierr)
      CHKERRQ(ierr)


    END SUBROUTINE petsc_solver_execute

    !> @ brief Allocate and initialize scalars
    !!
    !!  Allocate and inititialize linear accelerator scalars
    !!
    !<
    subroutine allocate_scalars(this)
      ! -- modules
      use MemoryManagerModule, only: mem_allocate
      ! -- dummy variables
      class(PetscSolverDataType), intent(inout) :: this  !< PetscSolverDataType instance
      !
      ! -- allocate scalars
      call mem_allocate(this%iout, 'IOUT', this%memoryPath)
      !
      ! -- initialize scalars
      this%iout = 0
      !
      ! -- return
      return
    end subroutine allocate_scalars

END MODULE PetscSolverModule

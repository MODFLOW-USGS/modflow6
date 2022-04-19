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
  use IMSLinearModule, only: ImsLinearDataType

  implicit none
  private

   ! -- enumerator that defines the linear acceleration method
  enum, bind(C)
    enumerator :: LIN_ACCEL_INVALID=0
    enumerator :: LIN_ACCEL_CG=1
    enumerator :: LIN_ACCEL_BCGS=2
  end enum

  type, private :: PetscContext
    Vec :: x_old
    Vec :: delta_x
    real(DP) :: dvclose
  end type
  
  type, public :: PetscSolverDataType
    character(len=LENMEMPATH) :: memoryPath                              !< the path for storing variables in the memory manager
    integer(I4B), pointer :: iout => NULL()                              !< simulation listing file unit
    integer(I4B), pointer :: iprims => NULL()                            !< print flag
    integer(I4B), POINTER :: lin_accel => NULL()                         !< linear accelerator: LIN_ACCEL_CG, LIN_ACCEL_BCGS
    ! -- pointers to solution variables
    integer(I4B), pointer :: neq => NULL()                               !< number of equations (rows in matrix)
    integer(I4B), pointer :: nja => NULL()                               !< number of non-zero values in amat
    integer(I4B), dimension(:), pointer, contiguous :: ia => NULL()      !< position of start of each row
    integer(I4B), dimension(:), pointer, contiguous :: ja => NULL()      !< column pointer
    real(DP), dimension(:), pointer, contiguous :: amat => NULL()        !< coefficient matrix
    real(DP), dimension(:), pointer, contiguous :: rhs => NULL()         !< right-hand side of equation
    real(DP), dimension(:), pointer, contiguous :: x => NULL()           !< dependent variable
    Mat :: Amat_petsc
    Vec :: x_petsc
    Vec :: rhs_petsc
    KSP :: ksp
    
    ! procedures (methods)
    contains
      procedure :: allocate_read
      procedure :: execute
      procedure :: deallocate
      ! -- PRIVATE PROCEDURES
      procedure, private :: allocate_scalars
  end type PetscSolverDataType
  
  
  contains
  
    !> @brief Allocate storage and read data
    !!
    !<
    subroutine allocate_read(this, name, parser, iout, iprims, imslinear, &
                            neq, nja, ia, ja, amat, rhs, x)
      ! -- modules
      use MemoryManagerModule, only: mem_allocate
      use MemoryHelperModule,  only: create_mem_path
      use SimModule, only: store_error, count_errors,            &
                           deprecation_warning
      ! -- dummy variables
      class(PetscSolverDataType), intent(inout) :: this          !< PetscSolverDataType instance
      character (LEN=lensolutionname), intent(in) :: name        !< solution name
      type(BlockParserType) :: parser                            !< block parser
      integer(I4B), intent(in) :: iout                           !< simulation listing file unit
      integer(I4B), target, intent(in) :: iprims                 !< print option
      type(ImsLinearDataType), pointer, intent(in) :: imslinear  !< linear accelerator (1) CG (2) BICGSTAB 
      integer(I4B), target, intent(in) :: neq                    !< number of equations
      integer(I4B), target, intent(in) :: nja                    !< number of non-zero entries in the coefficient matrix
      integer(I4B), dimension(neq+1), target, intent(in) :: ia   !< pointer to the start of a row in the coefficient matrix
      integer(I4B), dimension(nja), target, intent(in) :: ja     !< column pointer
      real(DP), dimension(nja), target, intent(in) :: amat       !< coefficient matrix
      real(DP), dimension(neq), target, intent(inout) :: rhs     !< right-hand side
      real(DP), dimension(neq), target, intent(inout) :: x       !< dependent variables

      ! -- local variables
      character(len=LINELENGTH) :: errmsg
      PetscErrorCode ierr
      class(PetscContext), pointer :: petsc_context => null()
      PC pc

      this%memoryPath = create_mem_path(name, 'PetscSolver')

      this%iprims => iprims
      this%neq => neq
      this%nja => nja
      this%ia => ia
      this%ja => ja
      this%amat => amat
      this%rhs => rhs
      this%x => x

      call this%allocate_scalars()
      this%iout = iout
      this%lin_accel = imslinear%ILINMETH
      
      !  Create matrix
      ! TODO: Calculate number of nonzeros per row properly instead of hardcoding to 13
      call MatCreateSeqAIJ(PETSC_COMM_WORLD, this%neq, size(this%x), 13,         &
                           PETSC_NULL_INTEGER, this%Amat_petsc,ierr)
      CHKERRQ(ierr)
      call MatSetFromOptions(this%Amat_petsc, ierr)
      CHKERRQ(ierr)
      call MatSetUp(this%Amat_petsc, ierr)
      CHKERRQ(ierr)

      !  Create petsc vectors.
      call VecCreateSeq(PETSC_COMM_WORLD, size(this%x), this%x_petsc, ierr)
      CHKERRQ(ierr)
      call VecSetFromOptions(this%x_petsc, ierr)
      CHKERRQ(ierr)
      call VecDuplicate(this%x_petsc, this%rhs_petsc, ierr)
      CHKERRQ(ierr)

      !  Create linear solver context
      call KSPCreate(PETSC_COMM_WORLD, this%ksp,ierr)
      CHKERRQ(ierr)
      if (this%lin_accel == LIN_ACCEL_CG) then
        call KSPSetType(this%ksp, KSPCG, ierr)
        CHKERRQ(ierr)
      else if (this%lin_accel == LIN_ACCEL_BCGS) then
        call KSPSetType(this%ksp, KSPBCGS, ierr)
        CHKERRQ(ierr)
      else
        write(errmsg,'(3a)')                                             &
          'UNKNOWN IMSLINEAR LINEAR_ACCELERATION METHOD (',              &
          this%lin_accel, ').'
        call store_error(errmsg)
      end if

      ! Set preconditioner
      call KSPGetPC(this%ksp, pc, ierr)
      CHKERRQ(ierr)
      call PCSetType(pc, PCILU, ierr)
      CHKERRQ(ierr)

      call KSPSetOperators(this%ksp, this%Amat_petsc, this%Amat_petsc, ierr)
      CHKERRQ(ierr)
      call KSPSetFromOptions(this%ksp, ierr)
      CHKERRQ(ierr)

      ! Init context
      allocate(petsc_context)
      call VecDuplicate(this%x_petsc, petsc_context%x_old, ierr)
      CHKERRQ(ierr)
      call VecDuplicate(this%x_petsc, petsc_context%delta_x, ierr)
      CHKERRQ(ierr)
      petsc_context%dvclose = imslinear%dvclose


      ! Set convergence test and pass context
      call KSPSetConvergenceTest(this%ksp, check_convergence, petsc_context, destroy_context, ierr)
      CHKERRQ(ierr)


    end subroutine allocate_read

    subroutine check_convergence(ksp, n, rnorm, flag, petsc_context, ierr)
      KSP :: ksp                                      !< Iterative context
      PetscInt :: n                                   !< Iteration number
      PetscReal :: rnorm                              !< 2-norm (preconditioned) residual value
      KSPConvergedReason :: flag                      !< Converged reason
      class(PetscContext), pointer :: petsc_context   !< optional user-defined monitor context
      PetscErrorCode :: ierr                          !< error
      ! local
      PetscScalar :: alpha = -1.0
      PetscReal :: norm
      Vec :: x

      call KSPGetSolution(ksp,x,ierr)
      CHKERRQ(ierr)

      if (n == 0) then
        ! skip first iteration
        call VecCopy(x, petsc_context%x_old, ierr)
        CHKERRQ(ierr)
        flag = KSP_CONVERGED_ITERATING
        return
      end if

      call VecWAXPY(petsc_context%delta_x, alpha, x, petsc_context%x_old, ierr)
      CHKERRQ(ierr)

      call VecNorm(petsc_context%delta_x, NORM_INFINITY, norm, ierr)
      CHKERRQ(ierr)

      call VecCopy(x, petsc_context%x_old, ierr)
      CHKERRQ(ierr)
      
      if (norm < petsc_context%dvclose) then
        flag = KSP_CONVERGED_RTOL ! Converged
      else
        flag = KSP_CONVERGED_ITERATING ! Not yet converged
      end if
    end subroutine check_convergence

    subroutine destroy_context(context, ierr)
      class(PetscContext), pointer :: context   !< optional user-defined monitor context
      PetscErrorCode :: ierr                    !< error

      call VecDestroy(context%x_old, ierr)
      CHKERRQ(ierr)
      call VecDestroy(context%delta_x, ierr)
      CHKERRQ(ierr)
      deallocate(context)
    end subroutine destroy_context



    !> @brief Deallocate memory
    !!
    !!  Deallocate linear accelerator memory.
    !!
    !<
    subroutine deallocate(this)
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
      call mem_deallocate(this%lin_accel)
      
      ! -- nullify pointers
      nullify(this%iprims)
      nullify(this%neq)
      nullify(this%nja)
      nullify(this%ia)
      nullify(this%ja)
      nullify(this%amat)
      nullify(this%rhs)
      nullify(this%x)

      call MatDestroy(this%Amat_petsc, ierr)
      CHKERRQ(ierr)
      call VecDestroy(this%x_petsc, ierr)
      CHKERRQ(ierr)
      call VecDestroy(this%rhs_petsc, ierr)
      CHKERRQ(ierr)
      call KSPDestroy(this%ksp, ierr)
      CHKERRQ(ierr)
      ! -- return
      return
    end subroutine deallocate
    

    !> @brief Solve linear equation
    !< 
    subroutine execute(this, kiter)
      class(PetscSolverDataType), intent(inout) :: this !< PetscSolverDataType instance
      integer(I4B) :: kiter
      ! local
      PetscErrorCode :: ierr
      PetscInt :: ione = 1
      PetscScalar, pointer :: x_pointer(:)
      integer(I4B) :: row, ipos, n

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
      call MatAssemblyBegin(this%Amat_petsc, MAT_FINAL_ASSEMBLY, ierr)
      CHKERRQ(ierr)
      call MatAssemblyEnd(this%Amat_petsc, MAT_FINAL_ASSEMBLY, ierr)
      CHKERRQ(ierr)

      do n = 1, size(this%x)
        call VecSetValues(this%x_petsc, ione, n-1, this%x(n), INSERT_VALUES, ierr)
        CHKERRQ(ierr)
      end do
      call VecAssemblyBegin(this%x_petsc, ierr)
      CHKERRQ(ierr)
      call VecAssemblyEnd(this%x_petsc, ierr)
      CHKERRQ(ierr)

      ! create RHS
      do n = 1, size(this%rhs)
        call VecSetValues(this%rhs_petsc, ione, n-1, this%rhs(n), INSERT_VALUES, ierr)
        CHKERRQ(ierr)
      end do
      call VecAssemblyBegin(this%rhs_petsc, ierr)
      CHKERRQ(ierr)
      call VecAssemblyEnd(this%rhs_petsc, ierr)
      CHKERRQ(ierr)
      
      ! ! print system
      ! if (kiter == 1) then
      !   call PetscViewerASCIIOpen(PETSC_COMM_WORLD, 'amat.txt', viewer, ierr)
      !   CHKERRQ(ierr)
      !   call MatView(this%Amat_petsc, viewer, ierr)        
      !   CHKERRQ(ierr)
      !   call PetscViewerDestroy(viewer,ierr)
      !   CHKERRQ(ierr)
      !   call PetscViewerASCIIOpen(PETSC_COMM_WORLD, 'rhs.txt', viewer, ierr)
      !   CHKERRQ(ierr)
      !   call VecView(this%rhs_petsc, viewer, ierr)        
      !   CHKERRQ(ierr)
      !   call PetscViewerDestroy(viewer,ierr)
      !   CHKERRQ(ierr)
      ! end if
      
      ! Solve the linear system
      call KSPSolve(this%ksp, this%rhs_petsc, this%x_petsc, ierr)
      CHKERRQ(ierr)

      ! copy solution
      call VecGetArrayReadF90(this%x_petsc, x_pointer, ierr)
      CHKERRQ(ierr)
      do n = 1, size(this%x)
        this%x(n) = x_pointer(n)
      end do
      call VecRestoreArrayReadF90(this%x_petsc, x_pointer, ierr)
      CHKERRQ(ierr)

    end subroutine execute

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
      call mem_allocate(this%lin_accel, 'LIN_ACCEL', this%memoryPath)
      !
      ! -- initialize scalars
      this%iout = 0
      this%lin_accel = 0
      !
      ! -- return
      return
    end subroutine allocate_scalars

END MODULE PetscSolverModule

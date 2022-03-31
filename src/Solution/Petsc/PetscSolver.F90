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

   ! -- enumerator that defines the linear acceleration method
  enum, bind(C)
    enumerator :: LIN_ACCEL_INVALID=0
    enumerator :: LIN_ACCEL_CG=1
    enumerator :: LIN_ACCEL_BCGSL=2
  end enum
  
  type, public :: PetscSolverDataType
    character(len=LENMEMPATH) :: memoryPath                              !< the path for storing variables in the memory manager
    integer(I4B), pointer :: iout => NULL()                              !< simulation listing file unit
    integer(I4B), pointer :: iprims => NULL()                            !< print flag
    integer(I4B), POINTER :: lin_accel => NULL()                         !< linear accelerator: LIN_ACCEL_CG, LIN_ACCEL_BCGSL
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
    subroutine allocate_read(this, name, parser, iout, iprims,     &
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
      integer(I4B), target, intent(in) :: neq                    !< number of equations
      integer(I4B), target, intent(in) :: nja                    !< number of non-zero entries in the coefficient matrix
      integer(I4B), dimension(neq+1), target, intent(in) :: ia   !< pointer to the start of a row in the coefficient matrix
      integer(I4B), dimension(nja), target, intent(in) :: ja     !< column pointer
      real(DP), dimension(nja), target, intent(in) :: amat       !< coefficient matrix
      real(DP), dimension(neq), target, intent(inout) :: rhs     !< right-hand side
      real(DP), dimension(neq), target, intent(inout) :: x       !< dependent variables

      ! -- local variables
      logical :: lreaddata
      character(len=LINELENGTH) :: errmsg
      character(len=LINELENGTH) :: keyword
      integer(I4B) :: err
      logical :: isfound, endOfBlock
      PetscErrorCode ierr

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
      
      !  Create matrix 
      call MatCreateSeqAIJ(PETSC_COMM_WORLD, this%neq, size(this%x), 7,         &
                           PETSC_NULL_INTEGER, this%Amat_petsc,ierr)
      CHKERRQ(ierr)
      call MatSetFromOptions(this%Amat_petsc, ierr)
      CHKERRQ(ierr)
      call MatSetUp(this%Amat_petsc, ierr)
      CHKERRQ(ierr)

      !  Create petsc vectors.
      call VecCreateSeq(PETSC_COMM_WORLD, size(this%x), this%x_petsc,ierr)
      CHKERRQ(ierr)
      call VecSetFromOptions(this%x_petsc, ierr)
      CHKERRQ(ierr)
      call VecDuplicate(this%x_petsc, this%rhs_petsc, ierr)
      CHKERRQ(ierr)

      ! get IMSLINEAR options block
      ! We parse the linear block, but do we really want to share this setting?
      ! TODO: Design something which lets PETSc as well as imslinear expand in the future
      if (lreaddata) then
        call parser%GetBlock('LINEAR', isfound, err, &
          supportOpenClose=.true., blockRequired=.FALSE.)
      else
        isfound = .FALSE.
      end if
      !
      ! -- parse LINEAR block if detected
      if (isfound) then
        write(iout,'(/1x,a)')'PROCESSING LINEAR DATA'
        do
          call parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
          call parser%GetStringCaps(keyword)
          ! -- parse keyword
          select case (keyword)
            case ('LINEAR_ACCELERATION')
              call parser%GetStringCaps(keyword)
              if (keyword.eq.'CG') then
                this%lin_accel = LIN_ACCEL_CG
              else if (keyword.eq.'BICGSTAB') then
                this%lin_accel = LIN_ACCEL_BCGSL
              else
                this%lin_accel = LIN_ACCEL_INVALID
                write(errmsg,'(3a)')                                             &
                  'UNKNOWN IMSLINEAR LINEAR_ACCELERATION METHOD (',              &
                  trim(keyword), ').'
                call store_error(errmsg)
              end if
            ! -- default
            case default
                write(errmsg,'(3a)')                                             &
                  'UNKNOWN LINEAR KEYWORD (', trim(keyword), ').'
              call store_error(errmsg)
          end select
        end do
        write(iout,'(1x,a)') 'END OF LINEAR DATA'
      end if

      !  Create linear solver context
      call KSPCreate(PETSC_COMM_WORLD,this%ksp,ierr)
      CHKERRQ(ierr)
      call KSPSetOperators(this%ksp,this%Amat_petsc,this%Amat_petsc,ierr)
      CHKERRQ(ierr)
      call KSPSetFromOptions(this%ksp,ierr)
      CHKERRQ(ierr)

      return
    end subroutine allocate_read

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

      call KSPDestroy(this%ksp,ierr)
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
      !PetscViewer :: viewer

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
      call VecAssemblyBegin(this%x_petsc,ierr)
      CHKERRQ(ierr)
      call VecAssemblyEnd(this%x_petsc,ierr)
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
      !
      ! -- initialize scalars
      this%iout = 0
      !
      ! -- return
      return
    end subroutine allocate_scalars

END MODULE PetscSolverModule

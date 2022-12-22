module MpiRunControlModule
#if defined(__WITH_PETSC__)
#include <petsc/finclude/petscksp.h>
  use petscksp
#endif
  use mpi
  use MpiWorldModule
  use SimVariablesModule, only: proc_id, nr_procs
  use KindModule, only: I4B, LGP
  use RunControlModule, only: RunControlType
  implicit none
  private

  public :: create_mpi_run_control

  type, public, extends(RunControlType) :: MpiRunControlType
  contains
    ! override
    procedure :: start => mpi_ctrl_start
    procedure :: finish => mpi_ctrl_finish
    ! private
    procedure, private :: wait_for_debugger
  end type MpiRunControlType

contains

  function create_mpi_run_control() result(controller)
    class(RunControlType), pointer :: controller
    ! local
    class(MpiRunControlType), pointer :: mpi_controller

    allocate(mpi_controller)
    controller => mpi_controller

  end function create_mpi_run_control

  subroutine mpi_ctrl_start(this)
    use SimModule, only: ustop
    class(MpiRunControlType) :: this
    ! local
    integer :: ierr
    character(len=*), parameter :: petsc_db_file = '/home/russcher/.petscrc'
    logical(LGP) :: petsc_db_exists, wait_dbg
    class(MpiWorldType), pointer :: mpi_world
    ! if PETSc we need their initialize
    wait_dbg = .false.
#if defined(__WITH_PETSC__)
    inquire(file=petsc_db_file, exist=petsc_db_exists)
    if (.not. petsc_db_exists) then
      write(*,*) 'No petsc database file found: ', petsc_db_file
      call ustop()
    end if
    call PetscInitialize(petsc_db_file, ierr)
    MF6_COMM_WORLD = PETSC_COMM_WORLD
    CHKERRQ(ierr)
    call PetscOptionsHasName(PETSC_NULL_OPTIONS, PETSC_NULL_CHARACTER, &
                             '-wait_dbg', wait_dbg, ierr)
    CHKERRQ(ierr)
#else
    call MPI_Init(ierr)
    MF6_COMM_WORLD = MPI_COMM_WORLD
#endif

    mpi_world => get_mpi_world()
    call mpi_world%init()

    call MPI_Comm_size(MF6_COMM_WORLD, nr_procs, ierr)
    call MPI_Comm_rank(MF6_COMM_WORLD, proc_id, ierr)

    ! possibly wait to attach debugger here
    if (wait_dbg) call this%wait_for_debugger()

    ! start everything else by calling parent
    call this%RunControlType%start()

  end subroutine mpi_ctrl_start

  subroutine wait_for_debugger(this)
    class(MpiRunControlType) :: this
    ! local
    integer :: ierr
    integer(I4B) :: icnt

    if (proc_id == 0) then
      icnt = 0
      write(*,*) 'Hit enter to continue...'
      read(*,*)
    end if
    call MPI_Barrier(MF6_COMM_WORLD, ierr)

  end subroutine wait_for_debugger

  subroutine mpi_ctrl_finish(this)
    class(MpiRunControlType) :: this
    ! local
    integer :: ierr

    ! finish mpi    
#if defined(__WITH_PETSC__)
    call PetscFinalize(ierr)
    CHKERRQ(ierr)
#else
    call MPI_Finalize(ierr)
#endif

    ! finish everything else by calling parent
    call this%RunControlType%finish()

  end subroutine mpi_ctrl_finish

end module MpiRunControlModule
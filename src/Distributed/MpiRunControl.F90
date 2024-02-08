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

    allocate (mpi_controller)
    controller => mpi_controller

  end function create_mpi_run_control

  subroutine mpi_ctrl_start(this)
    use SimModule, only: ustop, store_error

    class(MpiRunControlType) :: this
    ! local
    integer :: ierr
    character(len=*), parameter :: petsc_db_file = '.petscrc'
    logical(LGP) :: petsc_db_exists, wait_dbg, is_parallel_mode
    type(MpiWorldType), pointer :: mpi_world

    wait_dbg = .false.
    mpi_world => get_mpi_world()

    ! if PETSc we need their initialize
#if defined(__WITH_PETSC__)
    ! PetscInitialize calls MPI_Init only when it is not called yet,
    ! which could be through the API. If it is already called, we
    ! should assign the MPI communicator to PETSC_COMM_WORLD first
    ! (PETSc manual)
    if (mpi_world%has_comm()) then
      PETSC_COMM_WORLD = mpi_world%comm
    end if

    inquire (file=petsc_db_file, exist=petsc_db_exists)
    if (.not. petsc_db_exists) then
      call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
      CHKERRQ(ierr)
    else
      call PetscInitialize(petsc_db_file, ierr)
      CHKERRQ(ierr)
    end if

    if (.not. mpi_world%has_comm()) then
      call mpi_world%set_comm(PETSC_COMM_WORLD)
    end if

    call PetscOptionsHasName(PETSC_NULL_OPTIONS, PETSC_NULL_CHARACTER, &
                             '-wait_dbg', wait_dbg, ierr)
    CHKERRQ(ierr)
    call PetscOptionsHasName(PETSC_NULL_OPTIONS, PETSC_NULL_CHARACTER, &
                             '-p', is_parallel_mode, ierr)
    CHKERRQ(ierr)
#else
    if (.not. mpi_world%has_comm()) then
      call MPI_Init(ierr)
      call CHECK_MPI(ierr)
      call mpi_world%set_comm(MPI_COMM_WORLD)
    end if
#endif

    call mpi_world%init()

    call MPI_Comm_size(mpi_world%comm, nr_procs, ierr)
    call MPI_Comm_rank(mpi_world%comm, proc_id, ierr)

    ! possibly wait to attach debugger here
    if (wait_dbg) call this%wait_for_debugger()

    if (is_parallel_mode .and. nr_procs == 1) then
      write (*, '(a,/)') '(WARNING. Running parallel mode on only 1 process)'
    end if

    ! start everything else by calling parent
    call this%RunControlType%start()

  end subroutine mpi_ctrl_start

  subroutine wait_for_debugger(this)
    class(MpiRunControlType) :: this
    ! local
    integer :: ierr
    integer(I4B) :: icnt
    type(MpiWorldType), pointer :: mpi_world

    mpi_world => get_mpi_world()
    if (proc_id == 0) then
      icnt = 0
      write (*, *) 'Hit enter to continue...'
      read (*, *)
    end if
    call MPI_Barrier(mpi_world%comm, ierr)

  end subroutine wait_for_debugger

  subroutine mpi_ctrl_finish(this)
    class(MpiRunControlType) :: this
    ! local
    integer :: ierr

    ! finish mpi
#if defined(__WITH_PETSC__)
    ! NB: PetscFinalize calls MPI_Finalize only when MPI_Init
    ! was called before PetscInitialize
    call PetscFinalize(ierr)
    CHKERRQ(ierr)
#else
    call MPI_Finalize(ierr)
    call CHECK_MPI(ierr)
#endif

    ! finish everything else by calling parent
    call this%RunControlType%finish()

  end subroutine mpi_ctrl_finish

end module MpiRunControlModule

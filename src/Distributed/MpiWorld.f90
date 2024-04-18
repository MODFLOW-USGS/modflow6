module MpiWorldModule
  use KindModule, only: I4B, LGP
  use SimVariablesModule, only: nr_procs, proc_id
  use mpi
  implicit none
  private

  public :: get_mpi_world
  public :: mpi_stop
  public :: CHECK_MPI

  type, public :: MpiWorldType
    integer(I4B) :: mpi_rank !< the id for this process
    integer(I4B) :: world_size !< the total nr. of processes in the MPI job
    integer(I4B), pointer :: comm => null() !< the MF6 communicator, either was it passed to
                                            !! use through the API, or we created MPI_COMM_WORLD
  contains
    procedure :: has_comm => mpiw_has_comm
    procedure :: set_comm => mpiw_set_comm
    procedure :: init => mpiw_init
    procedure :: begin_order => mpiw_begin_order
    procedure :: end_order => mpiw_end_order
    procedure :: destroy => mpiw_destroy
  end type MpiWorldType

  ! singleton pattern
  type(MpiWorldType), pointer :: global_mpi_world

contains

  function get_mpi_world() result(world)
    type(MpiWorldType), pointer :: world

    if (.not. associated(global_mpi_world)) then
      allocate (global_mpi_world)
    end if
    world => global_mpi_world

  end function get_mpi_world

  !> @brief Returns true when a communicator has been set.
  !<
  function mpiw_has_comm(this) result(has_comm)
    class(MpiWorldType) :: this
    logical(LGP) :: has_comm

    has_comm = associated(this%comm)

  end function mpiw_has_comm

  !> @brief Sets a communicator on this world, can
  !< be done only once.
  subroutine mpiw_set_comm(this, comm)
    class(MpiWorldType) :: this
    integer(I4B) :: comm

    allocate (this%comm)
    this%comm = comm

  end subroutine mpiw_set_comm

  subroutine mpiw_init(this)
    class(MpiWorldType) :: this
    ! local
    integer :: ierr

    call MPI_Comm_size(this%comm, this%world_size, ierr)
    call MPI_Comm_rank(this%comm, this%mpi_rank, ierr)
    nr_procs = this%world_size
    proc_id = this%mpi_rank

  end subroutine mpiw_init

  subroutine mpiw_begin_order(this)
    class(MpiWorldType) :: this
    ! local
    integer :: buffer
    integer :: status(MPI_STATUS_SIZE)
    integer :: ierr

    if (this%mpi_rank > 0) then
      call mpi_recv(buffer, 1, MPI_INTEGER, this%mpi_rank - 1, this%mpi_rank, &
                    this%comm, status, ierr)
    end if

  end subroutine mpiw_begin_order

  subroutine mpiw_end_order(this)
    class(MpiWorldType) :: this
    ! local
    integer :: ierr

    if (this%mpi_rank < this%world_size - 1) then
      call mpi_send(this%mpi_rank, 1, MPI_INTEGER, this%mpi_rank + 1, &
                    this%mpi_rank + 1, this%comm, ierr)
    end if

  end subroutine mpiw_end_order

  subroutine mpiw_destroy(this)
    class(MpiWorldType) :: this

    if (associated(this%comm)) then
      deallocate (this%comm)
    end if

    if (associated(global_mpi_world)) then
      deallocate (global_mpi_world)
    end if

  end subroutine mpiw_destroy

  !> @brief Check the MPI error code, report, and
  !< terminate when not MPI_SUCCESS
  subroutine CHECK_MPI(mpi_error_code)
    use SimModule, only: store_error
    integer :: mpi_error_code
    ! local
    character(len=1024) :: mpi_err_msg
    integer :: err_len
    integer :: ierr

    if (mpi_error_code /= MPI_SUCCESS) then
      call MPI_Error_string(mpi_error_code, mpi_err_msg, err_len, ierr)
      call store_error("Internal error: "//trim(mpi_err_msg), terminate=.true.)
    end if

  end subroutine CHECK_MPI

  subroutine mpi_stop(status)
    integer(I4B) :: status
    ! local
    type(MpiWorldType), pointer :: mpi_world
    integer :: ierr

    mpi_world => get_mpi_world()
    write (*, *) "" ! empty line
    call MPI_Abort(mpi_world%comm, status, ierr)

  end subroutine mpi_stop

end module MpiWorldModule

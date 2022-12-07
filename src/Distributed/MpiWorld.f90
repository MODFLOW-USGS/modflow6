module MpiWorldModule
  use KindModule, only: I4B
  use SimVariablesModule, only: nr_procs, proc_id
  use mpi  
  implicit none
  private

  public :: get_mpi_world  
  integer(I4B), public :: MF6_COMM_WORLD = -1

  type, public :: MpiWorldType
    integer(I4B) :: mpi_rank
    integer(I4B) :: world_size
  contains
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

  subroutine mpiw_init(this)
    class(MpiWorldType) :: this
    ! local
    integer :: ierr
    
    call MPI_Comm_size(MF6_COMM_WORLD, this%world_size, ierr)
    call MPI_Comm_rank(MF6_COMM_WORLD, this%mpi_rank, ierr)    
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
                    MF6_COMM_WORLD, status, ierr)
    end if

  end subroutine mpiw_begin_order

  subroutine mpiw_end_order(this)
    class(MpiWorldType) :: this
    ! local
    integer :: ierr

    if (this%mpi_rank < this%world_size-1) then
      call mpi_send(this%mpi_rank, 1, MPI_INTEGER, this%mpi_rank + 1, this%mpi_rank + 1, &
                    MF6_COMM_WORLD, ierr)
    end if

  end subroutine mpiw_end_order

  subroutine mpiw_destroy(this)
    class(MpiWorldType) :: this

    if (associated(global_mpi_world)) then
      deallocate (global_mpi_world)
    end if

  end subroutine mpiw_destroy

end module MpiWorldModule
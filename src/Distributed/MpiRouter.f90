module MpiRouterModule
  use RouterBaseModule
  use KindModule, only: I4B
  use VirtualSolutionModule
  implicit none
  private

  public :: create_mpi_router

  type, public, extends(RouterBaseType) :: MpiRouterType
  contains
    procedure :: route => mpi_route
  end type MpiRouterType

contains

  !> Factory method to create MPI router
  !<
  function create_mpi_router() result(router)
    class(RouterBaseType), pointer :: router
    ! local
    class(MpiRouterType), pointer :: mpi_router

    allocate(mpi_router)
    router => mpi_router

  end function create_mpi_router

  subroutine mpi_route(this, virtual_sol, stage)
    class(MpiRouterType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage

    ! snd+rcv header

    ! snd+rcv maps

    ! snd+rcv data

    ! async. wait

  end subroutine mpi_route

end module MpiRouterModule
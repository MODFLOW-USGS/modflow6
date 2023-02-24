module RouterFactoryModule
  use RouterBaseModule
  use SerialRouterModule, only: create_serial_router
#if defined(__WITH_MPI__)
  use MpiRouterModule, only: create_mpi_router
#endif
  implicit none
  private

  public :: create_router

contains

  !> @ Brief Create the proper router, depends on
  !! simulation mode (parallel or sequential) and type
  !! of build (with or without mpi)
  !<
  function create_router(sim_mode) result(router)
    character(len=*) :: sim_mode !< simulation mode: SEQUENTIAL or PARALLEL
    class(RouterBaseType), pointer :: router !< the router object

    if (sim_mode == 'SEQUENTIAL') then
      router => create_serial_router()
#if defined(__WITH_MPI__)
    else if (sim_mode == 'PARALLEL') then
      router => create_mpi_router()
#endif
    else
      router => null()
    end if

  end function create_router

end module RouterFactoryModule

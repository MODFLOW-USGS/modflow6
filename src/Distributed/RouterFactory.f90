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

  function create_router(router_mode) result(router)
    class(RouterBaseType), pointer :: router
    character(len=*) :: router_mode

    if (router_mode == 'SEQ') then
      router => create_serial_router()
#if defined(__WITH_MPI__)
    else if (router_mode == 'PAR') then
      router => create_mpi_router()
#endif
    else
      router => null()
    end if

  end function create_router

end module RouterFactoryModule
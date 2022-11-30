module RunControlFactoryModule
  use RunControlModule
#if defined(__WITH_MPI__)
  use MpiRunControlModule
#endif
  implicit none
  private

  public :: create_run_control

contains

  function create_run_control() result(controller)
    class(RunControlType), pointer :: controller

#if defined(__WITH_MPI__)
    controller => create_mpi_run_control()
#else
    controller => create_seq_run_control()
#endif

  end function create_run_control

end module RunControlFactoryModule
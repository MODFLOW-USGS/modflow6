module RunControlFactoryModule
  use RunControlModule
#if defined(__WITH_MPI__)
  use MpiRunControlModule
#endif
  use ConstantsModule, only: LINELENGTH
  implicit none
  private

  public :: create_run_control

contains

  function create_run_control() result(controller)
    use SimModule, only: store_error
    use SimVariablesModule, only: simulation_mode
    class(RunControlType), pointer :: controller
    ! local
    character(len=LINELENGTH) :: errmsg

    errmsg = ''
#if defined(__WITH_MPI__)
    if (simulation_mode == 'PARALLEL') then
      controller => create_mpi_run_control()
    else
      controller => create_seq_run_control()
    end if
#else
    if (simulation_mode == 'PARALLEL') then
      write (errmsg, '(a)') &
        'Can not run parallel mode with this executable: no MPI'
      call store_error(errmsg, terminate=.true.)
    end if
    controller => create_seq_run_control()
#endif

  end function create_run_control

end module RunControlFactoryModule

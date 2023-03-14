module RunControlModule
  use KindModule, only: I4B
  use SimStagesModule
  use VirtualDataManagerModule
  use MapperModule
  use ListsModule, only: baseconnectionlist, basesolutionlist
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                          get_smc_from_list
  use NumericalSolutionModule, only: NumericalSolutionType
  implicit none
  private

  public :: create_seq_run_control

  type, public :: RunControlType
    class(VirtualDataManagerType), pointer :: virtual_data_store !< contains globally accessible data, timely synchronized
                                                               !! by direct linking (local) or message passing (remote)
    type(MapperType) :: mapper !< a 'mapper' for filling the interface models: this needs a better name/place
  contains
    procedure :: start => ctrl_start
    procedure :: at_stage => ctrl_at_stage
    procedure :: finish => ctrl_finish
    ! private
    procedure, private :: init_handler
    procedure, private :: before_df_handler
    procedure, private :: after_df_handler
    procedure, private :: destroy
  end type RunControlType

contains

  function create_seq_run_control() result(run_controller)
    class(RunControlType), pointer :: run_controller

    allocate (run_controller)

  end function create_seq_run_control

  subroutine ctrl_start(this)
    class(RunControlType) :: this

    allocate (this%virtual_data_store)

  end subroutine ctrl_start

  subroutine ctrl_finish(this)
    use SimVariablesModule, only: iout
    use MemoryManagerModule, only: mem_write_usage, mem_da
    use TimerModule, only: elapsed_time
    use SimModule, only: final_message
    class(RunControlType) :: this

    ! clean up
    call this%destroy()

    ! -- Write memory usage, elapsed time and terminate
    call mem_write_usage(iout)
    call mem_da()
    call elapsed_time(iout, 1)
    call final_message()

  end subroutine ctrl_finish

  !> @brief This will call the handler for a particular stage
  !< in the simulation run
  subroutine ctrl_at_stage(this, stage)
    class(RunControlType) :: this
    integer(I4B) :: stage

    if (stage == STG_INIT) then
      call this%init_handler()
    else if (stage == STG_BEFORE_CON_DF) then
      call this%before_df_handler()
    else if (stage == STG_AFTER_CON_DF) then
      call this%after_df_handler()
    end if

    call this%virtual_data_store%synchronize(stage)
    call this%mapper%scatter(0, stage)

  end subroutine ctrl_at_stage

  subroutine init_handler(this)
    use SimVariablesModule, only: simulation_mode
    class(RunControlType), target :: this

    call this%virtual_data_store%create(simulation_mode)
    call this%virtual_data_store%init()
    call this%mapper%init()

  end subroutine init_handler

  subroutine before_df_handler(this)
    class(RunControlType), target :: this
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj_ptr
    class(NumericalSolutionType), pointer :: sol

    ! Interface models are created now and we know which
    ! remote models and exchanges are required in the
    ! virtual solution. Also set the synchronization handler
    ! to the numerical solutions.
    do i = 1, basesolutionlist%Count()
      obj_ptr => basesolutionlist%GetItem(i)
      select type (obj_ptr)
      class is (NumericalSolutionType)
        sol => obj_ptr
        call this%virtual_data_store%add_solution(sol)
        sol%synchronize => rc_solution_sync
        sol%synchronize_ctx => this
      end select
    end do

    call this%mapper%add_exchange_vars()

  end subroutine before_df_handler

  subroutine after_df_handler(this)
    class(RunControlType) :: this

    call this%mapper%add_interface_vars()

  end subroutine after_df_handler

  !> @brief Synchronizes from within numerical solution (delegate)
  !<
  subroutine rc_solution_sync(num_sol, stage, ctx)
    use NumericalSolutionModule, only: NumericalSolutionType
    class(NumericalSolutionType) :: num_sol
    integer(I4B) :: stage
    class(*), pointer :: ctx

    select type (ctx)
    class is (RunControlType)
      call ctx%virtual_data_store%synchronize_sln(num_sol%id, stage)
      call ctx%mapper%scatter(num_sol%id, stage)
    end select

  end subroutine rc_solution_sync

  subroutine destroy(this)
    class(RunControlType) :: this

    call this%virtual_data_store%destroy()
    deallocate (this%virtual_data_store)

  end subroutine destroy

end module RunControlModule

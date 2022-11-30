module RunControlModule
  use KindModule, only: I4B
  use SimStagesModule
  use VirtualDataStoreModule
  use MapperModule  
  use ListsModule, only: baseconnectionlist, basesolutionlist
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                          GetSpatialModelConnectionFromList
  use NumericalSolutionModule, only: NumericalSolutionType
  implicit none
  private

  public :: create_seq_run_control

  type, public :: RunControlType
    class(VirtualDataStoreType), pointer :: virtual_data_store !< contains globally accessible data, timely synchronized 
                                                               !! by direct linking (local) or message passing (remote)
    type(MapperType) :: mapper !< a 'mapper' for filling the interface models: this needs a better name/place
  contains
    procedure :: start => ctrl_start
    procedure :: at_stage => ctrl_at_stage
    procedure :: finish => ctrl_finish    
    ! private
    procedure, private :: before_df_handler
    procedure, private :: after_df_handler
    procedure, private :: before_ar_handler
    procedure, private :: after_ar_handler
    procedure, private :: init_mapper
    procedure, private :: destroy
  end type RunControlType

contains

  function create_seq_run_control() result(run_controller)
    class(RunControlType), pointer :: run_controller

    allocate(run_controller)

  end function create_seq_run_control

  subroutine ctrl_start(this)
    use SimModule, only: initial_message
    use TimerModule, only: start_time
    class(RunControlType) :: this
    
    ! print initial message
    call initial_message()
    
    ! get start time
    call start_time()

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

    select case (stage)
      case (STG_BEFORE_DF)
        call this%before_df_handler()
      case (STG_AFTER_DF)
        call this%after_df_handler()
      case (STG_BEFORE_AR)
        call this%before_ar_handler()
      case (STG_AFTER_AR)
        call this%after_ar_handler()
      case default
        ! nothing be done
        return
    end select

  end subroutine ctrl_at_stage

  subroutine before_df_handler(this)
    use SimVariablesModule, only: simulation_mode
    class(RunControlType), target :: this
    ! local
    integer(I4B) :: i
    class(*), pointer :: sol
    
    call this%virtual_data_store%create(simulation_mode)
    call this%mapper%init()

    do i = 1, basesolutionlist%Count()
      sol => basesolutionlist%GetItem(i)
      select type (sol)
      class is (NumericalSolutionType)
        call this%virtual_data_store%add_solution(sol)
        sol%synchronize => rc_solution_sync
        sol%synchronize_ctx => this
      end select
    end do

    call this%virtual_data_store%synchronize(STG_BEFORE_INIT)
    call this%virtual_data_store%synchronize(STG_BEFORE_DF)

  end subroutine before_df_handler

  subroutine after_df_handler(this)
    class(RunControlType) :: this
    
    ! after define: we can initialize the mapper:
    call this%init_mapper()    

    call this%virtual_data_store%synchronize(STG_AFTER_DF)
    call this%mapper%scatter(0, STG_AFTER_DF)

  end subroutine after_df_handler

  subroutine before_ar_handler(this)
    class(RunControlType) :: this

    call this%virtual_data_store%synchronize(STG_BEFORE_AR)
    call this%mapper%scatter(0, STG_BEFORE_AR)

  end subroutine before_ar_handler

  subroutine after_ar_handler(this)
    class(RunControlType) :: this

    call this%virtual_data_store%synchronize(STG_AFTER_AR)
    call this%mapper%scatter(0, STG_AFTER_AR)

  end subroutine after_ar_handler

  subroutine init_mapper(this)
    class(RunControlType) :: this
    ! local
    integer(I4B) :: iconn
    class(SpatialModelConnectionType), pointer :: conn

    do iconn = 1, baseconnectionlist%Count()
      conn => GetSpatialModelConnectionFromList(baseconnectionlist, iconn)
      ! add the variables for this interface model to our mapper
      call this%mapper%add_dist_vars(conn%owner%idsoln, &
                                     conn%ifaceDistVars, &
                                     conn%interfaceMap)
    end do

  end subroutine init_mapper

  !> @brief Synchronizes from within numerical solution (delegate)
  !<
  subroutine rc_solution_sync(num_sol, stage, ctx)
    use NumericalSolutionModule, only: NumericalSolutionType
    class(NumericalSolutionType) :: num_sol
    integer(I4B) :: stage
    class(*), pointer :: ctx

    select type (ctx)
    class is (RunControlType)
      call ctx%virtual_data_store%synchronize(num_sol%id, stage)
      call ctx%mapper%scatter(num_sol%id, stage)
    end select

  end subroutine rc_solution_sync

  subroutine destroy(this)
    class(RunControlType) :: this

    call this%virtual_data_store%destroy()
    deallocate (this%virtual_data_store)

  end subroutine destroy

end module RunControlModule
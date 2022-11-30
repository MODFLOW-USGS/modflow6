module RunControlModule
  use KindModule, only: I4B
  use SimStagesModule
  implicit none
  private

  public :: create_seq_run_control

  type, public :: RunControlType
  contains
    procedure :: start => ctrl_start
    procedure :: at_stage => ctrl_at_stage
    procedure :: finish => ctrl_finish
    ! private
    procedure, private :: before_df_handler
    procedure, private :: after_df_handler
    procedure, private :: before_ar_handler
    procedure, private :: after_ar_handler
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

  end subroutine ctrl_start

  subroutine ctrl_finish(this)    
    use SimVariablesModule, only: iout
    use MemoryManagerModule, only: mem_write_usage, mem_da
    use TimerModule, only: elapsed_time
    use SimModule, only: final_message
    class(RunControlType) :: this

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
    class(RunControlType) :: this
  end subroutine before_df_handler

  subroutine after_df_handler(this)
    class(RunControlType) :: this
  end subroutine after_df_handler

  subroutine before_ar_handler(this)
    class(RunControlType) :: this
  end subroutine before_ar_handler

  subroutine after_ar_handler(this)
    class(RunControlType) :: this
  end subroutine after_ar_handler

end module RunControlModule
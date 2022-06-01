!> @brief Core MODFLOW 6 module
!!
!! This module contains the core components for MODFLOW 6. This module
!! is used by the stand-alone executable and the share object versions
!! of MODFLOW 6.
!!
!<
module Mf6CoreModule 
  use KindModule,             only: I4B, LGP
  use ListsModule,            only: basesolutionlist, solutiongrouplist,         &
                                    basemodellist, baseexchangelist,             &
                                    baseconnectionlist
  use BaseModelModule,        only: BaseModelType, GetBaseModelFromList
  use BaseExchangeModule,     only: BaseExchangeType, GetBaseExchangeFromList
  use SpatialModelConnectionModule, only: SpatialModelConnectionType,            &
                                          GetSpatialModelConnectionFromList
  use BaseSolutionModule,     only: BaseSolutionType, GetBaseSolutionFromList
  use SolutionGroupModule,    only: SolutionGroupType, GetSolutionGroupFromList
  implicit none  

  contains
  
    !> @brief Main controller
    !!
    !! This subroutine is the main controller for MODFLOW 6.
    !!
    !<
    subroutine Mf6Run
        ! -- modules 
        use CommandArguments, only: GetCommandLineArguments
        use TdisModule, only: totim, totalsimtime  
        use KindModule, only: DP
        ! -- local
        logical(LGP) :: hasConverged
        !
        ! -- parse any command line arguments
        call GetCommandLineArguments()
        !
        ! initialize simulation
        call Mf6Initialize()
        !
        ! -- time loop
        do while (totim < totalsimtime)
          
          ! perform a time step
          hasConverged = Mf6Update()
          
          ! if not converged, break
          if (.not. hasConverged) exit 
          
        end do
        !
        ! -- finalize simulation
        call Mf6Finalize()
        
    end subroutine Mf6Run
      
    !> @brief Initialize a simulation
    !!
    !! This subroutine initializes a MODFLOW 6 simulation. The subroutine:
    !!   - creates the simulation
    !!   - defines
    !!   - allocates and reads static data
    !!
    !<
    subroutine Mf6Initialize()
      ! -- modules
      use SimulationCreateModule, only: simulation_cr
      !
      ! -- print banner and info to screen
      call printInfo()
      
      ! -- create
      call simulation_cr()
      
      ! -- define
      call simulation_df()
        
      ! -- allocate and read
      call simulation_ar()
      
    end subroutine Mf6Initialize
    
    !> @brief Run a time step
    !!
    !! This function runs a single time step to completion.
    !!
    !! @return  hasConverged   boolean indicating if convergence was achieved for the time step
    !!
    !<
    function Mf6Update() result(hasConverged)
      ! -- return variable
      logical(LGP) :: hasConverged
      !
      ! -- prepare timestep
      call Mf6PrepareTimestep()
      !
      ! -- do timestep
      call Mf6DoTimestep()      
      !
      ! -- after timestep
      hasConverged = Mf6FinalizeTimestep()
      !
    end function Mf6Update
    
    !> @brief Finalize the simulation
    !!
    !! This subroutine finalizes a simulation. Steps include:
    !!   - final processing
    !!   - deallocate memory
    !!
    !<
    subroutine Mf6Finalize()
      ! -- modules
      use, intrinsic :: iso_fortran_env, only: output_unit
      use ListsModule,            only: lists_da
      use MemoryManagerModule,    only: mem_write_usage, mem_da
      use TimerModule,            only: elapsed_time   
      use SimVariablesModule,     only: iout
      use SimulationCreateModule, only: simulation_da  
      use TdisModule,             only: tdis_da
      use SimModule,              only: final_message
      ! -- local variables
      integer(I4B) :: im
      integer(I4B) :: ic
      integer(I4B) :: is
      integer(I4B) :: isg
      class(SolutionGroupType), pointer :: sgp => null()
      class(BaseSolutionType), pointer :: sp => null()
      class(BaseModelType), pointer :: mp => null()
      class(BaseExchangeType), pointer :: ep => null()
      class(SpatialModelConnectionType), pointer :: mc => null()
      !
      ! -- FINAL PROCESSING (FP)
      ! -- Final processing for each model
      do im = 1, basemodellist%Count()
        mp => GetBaseModelFromList(basemodellist, im)
        call mp%model_fp()
      enddo
      !
      ! -- Final processing for each exchange
      do ic = 1, baseexchangelist%Count()
        ep => GetBaseExchangeFromList(baseexchangelist, ic)
        call ep%exg_fp()
      enddo
      !
      ! -- Final processing for each solution
      do is=1,basesolutionlist%Count()
        sp => GetBaseSolutionFromList(basesolutionlist, is)
        call sp%sln_fp()
      enddo
      !
      ! -- DEALLOCATE (DA)
      ! -- Deallocate tdis
      call tdis_da()
      !
      ! -- Deallocate for each model
      do im = 1, basemodellist%Count()
        mp => GetBaseModelFromList(basemodellist, im)
        call mp%model_da()
        deallocate(mp)
      enddo
      !
      ! -- Deallocate for each exchange
      do ic = 1, baseexchangelist%Count()
        ep => GetBaseExchangeFromList(baseexchangelist, ic)
        call ep%exg_da()
        deallocate(ep)
      enddo
      !
      ! -- Deallocate for each connection
      do ic = 1, baseconnectionlist%Count()
        mc => GetSpatialModelConnectionFromList(baseconnectionlist, ic)
        call mc%exg_da()
        deallocate(mc)
      enddo
      !
      ! -- Deallocate for each solution
      do is=1,basesolutionlist%Count()
        sp => GetBaseSolutionFromList(basesolutionlist, is)
        call sp%sln_da()
        deallocate(sp)
      enddo
      !
      ! -- Deallocate solution group and simulation variables
      do isg = 1, solutiongrouplist%Count()
        sgp => GetSolutionGroupFromList(solutiongrouplist, isg)
        call sgp%sgp_da()
        deallocate(sgp)
      enddo
      call simulation_da()
      call lists_da()
      !
      ! -- Write memory usage, elapsed time and terminate
      call mem_write_usage(iout)
      call mem_da()
      call elapsed_time(iout, 1)
      call final_message()
      !        
    end subroutine Mf6Finalize
    
    !> @brief Print info to screen
    !!
    !! This subroutine prints the banner to the screen.
    !!
    !<
    subroutine printInfo()
      use SimModule, only: initial_message
      use TimerModule, only: start_time
      !
      ! -- print initial message
      call initial_message()
      !
      ! -- get start time
      call start_time()    
      return
    end subroutine printInfo
    
    !> @brief Define the simulation
    !!
    !! This subroutine defined the simulation. Steps include:
    !!   - define each model
    !!   - define each solution
    !!
    !<
    subroutine simulation_df()
      ! -- local variables
      integer(I4B) :: im
      integer(I4B) :: ic
      integer(I4B) :: is
      class(BaseSolutionType), pointer :: sp => null()
      class(BaseModelType), pointer :: mp => null()
      class(BaseExchangeType), pointer :: ep => null()
      class(SpatialModelConnectionType), pointer :: mc => null()
      
      ! -- Define each model
      do im = 1, basemodellist%Count()
        mp => GetBaseModelFromList(basemodellist, im)
        call mp%model_df()
      enddo
      !
      ! -- Define each exchange
      do ic = 1, baseexchangelist%Count()
        ep => GetBaseExchangeFromList(baseexchangelist, ic)
        call ep%exg_df()
      enddo
      !
      ! -- when needed, this is were the interface models are
      ! created and added to the numerical solutions
      call connections_cr()
      !
      ! -- Define each connection
      do ic = 1, baseconnectionlist%Count()
        mc => GetSpatialModelConnectionFromList(baseconnectionlist, ic)
        call mc%exg_df()
      enddo
      !
      ! -- Define each solution
      do is = 1, basesolutionlist%Count()
        sp => GetBaseSolutionFromList(basesolutionlist, is)
        call sp%sln_df()
      enddo
    
    end subroutine simulation_df
    
    !> @brief Simulation allocate and read
    !!
    !! This subroutine allocates and read static data for the simulation. 
    !! Steps include:
    !!   - allocate and read for each model
    !!   - allocate and read for each exchange
    !!   - allocate and read for each solution
    !!
    !<
    subroutine simulation_ar()  
      ! -- local variables
      integer(I4B) :: im
      integer(I4B) :: ic
      integer(I4B) :: is
      class(BaseSolutionType), pointer :: sp => null()
      class(BaseModelType), pointer :: mp => null()
      class(BaseExchangeType), pointer :: ep => null()
      class(SpatialModelConnectionType), pointer :: mc => null()
      
      ! -- Allocate and read each model
      do im = 1, basemodellist%Count()
        mp => GetBaseModelFromList(basemodellist, im)
        call mp%model_ar()
      enddo
      !
      ! -- Allocate and read each exchange
      do ic = 1, baseexchangelist%Count()
        ep => GetBaseExchangeFromList(baseexchangelist, ic)
        call ep%exg_ar()
      enddo
      !
      ! -- Allocate and read all model connections
      do ic = 1, baseconnectionlist%Count()
        mc => GetSpatialModelConnectionFromList(baseconnectionlist, ic)
        call mc%exg_ar()
      enddo
      !
      ! -- Allocate and read each solution
      do is=1,basesolutionlist%Count()
        sp => GetBaseSolutionFromList(basesolutionlist, is)
        call sp%sln_ar()
      enddo
      !
    end subroutine simulation_ar

    !> @brief Create the model connections from the exchanges
    !!
    !! This will upgrade the numerical exchanges in the solution,
    !! whenever the configuration requires this, to Connection 
    !! objects. Currently we anticipate:
    !!
    !!   GWF-GWF => GwfGwfConnection
    !!   GWT-GWT => GwtGwtConecction
    !<
    subroutine connections_cr()
      use ConnectionBuilderModule
      use SimVariablesModule, only: iout
      integer(I4B) :: isol
      type(ConnectionBuilderType) :: connectionBuilder
      class(BaseSolutionType), pointer :: sol => null()

      write(iout,'(/a)') 'PROCESSING MODEL CONNECTIONS'

      if (baseexchangelist%Count() == 0) then
        ! if this is not a coupled simulation in any way,
        ! then we will not need model connections
        return
      end if

      do isol = 1, basesolutionlist%Count()
        sol => GetBaseSolutionFromList(basesolutionlist, isol)
        call connectionBuilder%processSolution(sol)
      end do

      write(iout,'(a)') 'END OF MODEL CONNECTIONS'
    end subroutine connections_cr
    
    !> @brief Read and prepare time step
    !!
    !! This subroutine reads and prepares period data for the simulation. 
    !! Steps include:
    !!   - read and prepare for each model
    !!   - read and prepare for each exchange
    !!   - reset convergence flag
    !!   - calculate maximum time step for each model
    !!   - calculate maximum time step for each exchange
    !!   - calculate maximum time step for each solution
    !!   - set time discretization timestep using smallest maximum timestep
    !!
    !<
    subroutine Mf6PrepareTimestep()
      ! -- modules
      use KindModule,             only: I4B
      use ConstantsModule,        only: LINELENGTH, MNORMAL, MVALIDATE
      use TdisModule,             only: tdis_set_counters, tdis_set_timestep, &
                                        kstp, kper
      use ListsModule,            only: basemodellist, baseexchangelist
      use BaseModelModule,        only: BaseModelType, GetBaseModelFromList
      use BaseExchangeModule,     only: BaseExchangeType, GetBaseExchangeFromList
      use BaseSolutionModule,     only: BaseSolutionType, GetBaseSolutionFromList
      use SimModule,              only: converge_reset
      use SimVariablesModule,     only: isim_mode
      ! -- local variables
      class(BaseModelType), pointer :: mp => null()
      class(BaseExchangeType), pointer :: ep => null()
      class(SpatialModelConnectionType), pointer :: mc => null()
      class(BaseSolutionType), pointer :: sp => null()
      character(len=LINELENGTH) :: line
      character(len=LINELENGTH) :: fmt
      integer(I4B) :: im
      integer(I4B) :: ie
      integer(I4B) :: ic
      integer(I4B) :: is
      !
      ! -- initialize fmt
      fmt = "(/,a,/)"
      !
      ! -- period update
      call tdis_set_counters()
      !
      ! -- set base line
      write(line, '(a,i0,a,i0,a)')                                                 &
        'start timestep kper="', kper, '" kstp="', kstp, '" mode="'
      !
      ! -- evaluate simulation mode
      select case (isim_mode)
        case (MVALIDATE)
          line = trim(line) // 'validate"'
        case(MNORMAL)
          line = trim(line) // 'normal"'
      end select
      
      ! -- Read and prepare each model
      do im = 1, basemodellist%Count()
        mp => GetBaseModelFromList(basemodellist, im)
        call mp%model_message(line, fmt=fmt)
        call mp%model_rp()
      enddo
      !
      ! -- Read and prepare each exchange
      do ie = 1, baseexchangelist%Count()
        ep => GetBaseExchangeFromList(baseexchangelist, ie)
        call ep%exg_rp()
      enddo
      !
      ! -- Read and prepare each connection
      do ic = 1, baseconnectionlist%Count()
        mc => GetSpatialModelConnectionFromList(baseconnectionlist, ic)
        call mc%exg_rp()
      enddo
      !
      ! -- reset simulation convergence flag
      call converge_reset()
      !
      ! -- time update for each model
      do im = 1, basemodellist%Count()
        mp => GetBaseModelFromList(basemodellist, im)
        call mp%model_calculate_delt()
      enddo
      !
      ! -- time update for each exchange
      do ie = 1, baseexchangelist%Count()
        ep => GetBaseExchangeFromList(baseexchangelist, ie)
        call ep%exg_calculate_delt()
      enddo
      !
      ! -- time update for each connection
      do ic = 1, baseconnectionlist%Count()
        mc => GetSpatialModelConnectionFromList(baseconnectionlist, ic)
        call mc%exg_calculate_delt()
      enddo
      !
      ! -- time update for each solution
      do is=1,basesolutionlist%Count()
        sp => GetBaseSolutionFromList(basesolutionlist, is)
        call sp%sln_calculate_delt()
      enddo
      !
      ! -- set time step
      call tdis_set_timestep()
      
    end subroutine Mf6PrepareTimestep
    
    !> @brief Run time step
    !!
    !! This subroutine runs a single time step for the simulation. 
    !! Steps include:
    !!   - formulate the system of equations for each model and exchange
    !!   - solve each solution
    !!
    !<
    subroutine Mf6DoTimestep()
      ! --  modules
      use KindModule,           only: I4B
      use ListsModule,          only: solutiongrouplist
      use SimVariablesModule,   only: iFailedStepRetry
      use SolutionGroupModule,  only: SolutionGroupType, GetSolutionGroupFromList
      ! -- local variables
      class(SolutionGroupType), pointer :: sgp => null()
      integer(I4B) :: isg
      logical :: finishedTrying
      
      ! -- By default, the solution groups will be solved once, and
      !    may fail.  But if adaptive stepping is active, then
      !    the solution groups may be solved over and over with 
      !    progressively smaller time steps to see if convergence
      !    can be obtained.
      iFailedStepRetry = 0
      retryloop: do
        
        do isg = 1, solutiongrouplist%Count()
          sgp => GetSolutionGroupFromList(solutiongrouplist, isg)
          call sgp%sgp_ca()
        enddo
        
        call sim_step_retry(finishedTrying)
        if (finishedTrying) exit retryloop
        iFailedStepRetry = iFailedStepRetry + 1
        
      end do retryloop
        
    end subroutine Mf6DoTimestep
    
    !> @brief Rerun time step
    !!
    !! This subroutine reruns a single time step for the simulation when
    !! the adaptive time step option is used. 
    !!
    !<
    subroutine sim_step_retry(finishedTrying)
      ! -- modules
      use KindModule, only: DP
      use SimVariablesModule, only: lastStepFailed
      use SimModule, only: converge_reset
      use TdisModule, only: kstp, kper, delt, tdis_delt_reset
      use AdaptiveTimeStepModule, only: ats_reset_delt
      ! -- dummy variables
      logical, intent(out) :: finishedTrying  !< boolean that indicates if no 
                                              ! additional reruns of the time step are required
      !
      ! -- Check with ats to reset delt and keep trying
      finishedTrying = .true.
      call ats_reset_delt(kstp, kper, lastStepFailed, delt, finishedTrying)
      !
      if (.not. finishedTrying) then
        !
        ! -- Reset delt, which requires updating pertim, totim
        !    and end of period and simulation indicators        
        call tdis_delt_reset(delt)
        !
        ! -- Reset state of the simulation convergence flag
        call converge_reset()
        
      end if
      !
      ! -- return
      return
    end subroutine sim_step_retry
    
    !> @brief Finalize time step
    !!
    !! This function finalizes a single time step for the simulation   
    !! and writes output for the time step. Steps include:
    !!   - write output for each model
    !!   - write output for each exchange
    !!   - write output for each solutions
    !!   - perform a final convergence check and whether the simulation
    !!     can continue if convergence was not achieved
    !!
    !! @return  hasConverged   boolean indicating if convergence was achieved for the time step
    !!
    !<
    function Mf6FinalizeTimestep() result(hasConverged)
      ! -- modules
      use KindModule,             only: I4B
      use ConstantsModule,        only: LINELENGTH, MNORMAL, MVALIDATE
      use ListsModule,            only: basesolutionlist, basemodellist, baseexchangelist    
      use BaseModelModule,        only: BaseModelType, GetBaseModelFromList
      use BaseExchangeModule,     only: BaseExchangeType, GetBaseExchangeFromList
      use BaseSolutionModule,     only: BaseSolutionType, GetBaseSolutionFromList
      use SimModule,              only: converge_check
      use SimVariablesModule,     only: isim_mode
      ! -- return variable
      logical(LGP) :: hasConverged  
      ! -- local variables
      class(BaseSolutionType), pointer :: sp => null()
      class(BaseModelType), pointer :: mp => null()
      class(BaseExchangeType), pointer :: ep => null()
      class(SpatialModelConnectionType), pointer :: mc => null()
      character(len=LINELENGTH) :: line
      character(len=LINELENGTH) :: fmt
      integer(I4B) :: im
      integer(I4B) :: ix
      integer(I4B) :: ic
      integer(I4B) :: is
      !
      ! -- initialize format and line
      fmt = "(/,a,/)"
      line = 'end timestep'
      !
      ! -- evaluate simulation mode
      select case (isim_mode)
        case(MVALIDATE)
          !
          ! -- Write final message for timestep for each model 
          do im = 1, basemodellist%Count()
            mp => GetBaseModelFromList(basemodellist, im)
            call mp%model_message(line, fmt=fmt)
          end do
        case(MNORMAL)
          !
          ! -- Write output and final message for timestep for each model 
          do im = 1, basemodellist%Count()
            mp => GetBaseModelFromList(basemodellist, im)
            call mp%model_ot()
            call mp%model_message(line, fmt=fmt)
          enddo
          !
          ! -- Write output for each exchange
          do ix = 1, baseexchangelist%Count()
            ep => GetBaseExchangeFromList(baseexchangelist, ix)
            call ep%exg_ot()
          enddo
          !
          ! -- Write output for each connection
          do ic = 1, baseconnectionlist%Count()
            mc => GetSpatialModelConnectionFromList(baseconnectionlist, ic)
            call mc%exg_ot()
          end do
          !
          ! -- Write output for each solution
          do is=1,basesolutionlist%Count()
            sp => GetBaseSolutionFromList(basesolutionlist, is)
            call sp%sln_ot()
          enddo
      end select
      !
      ! -- Check if we're done
      call converge_check(hasConverged)
      !
      ! -- return
      return    
      
    end function Mf6FinalizeTimestep
  
end module Mf6CoreModule

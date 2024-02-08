module ConnectionFactoryModule
  use KindModule, only: I4B, LGP
  use SimModule, only: store_error, count_errors, ustop
  use SimVariablesModule, only: iout
  use ListModule, only: ListType, isEqualIface, ListNodeType
  use BaseSolutionModule, only: BaseSolutionType
  use NumericalSolutionModule, only: NumericalSolutionType
  use BaseExchangeModule, only: BaseExchangeType, GetBaseExchangeFromList
  use DisConnExchangeModule, only: DisConnExchangeType, &
                                   GetDisConnExchangeFromList
  use NumericalModelModule, only: NumericalModelType
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                          cast_as_smc, &
                                          get_smc_from_list, &
                                          add_smc_to_list

  implicit none
  private

  type, public :: ConnectionFactoryType
    logical(LGP) :: dev_always_ifmod = .false. !< development option: force interface model on all exchanges
  contains
    procedure, pass(this) :: create_connections
    procedure, private, pass(this) :: process_exchanges
    procedure, private, pass(this) :: setup_solution_connections
    procedure, private, pass(this) :: setup_model_connections
  end type ConnectionFactoryType

contains

  !> @brief Create model connections from the solution's exchanges.
  !!
  !! This routine processes all exchanges in a solution and,
  !! when required, creates model connections of the proper
  !! type for a subset, removes the subset from the solution,
  !! replaces it with the newly created connections.
  !<
  subroutine create_connections(this, solution)
    ! -- dummy
    class(ConnectionFactoryType) :: this !< the connection builder object
    class(BaseSolutionType), pointer :: solution !< the solution for which the exchanges are processed
    ! -- local
    class(NumericalSolutionType), pointer :: num_soln
    type(ListType) :: new_cnxns

    ! we only deal with Num. Sol. here
    select type (solution)
    class is (NumericalSolutionType)
      num_soln => solution
    class default
      return
    end select

    ! create the connections and add local exchanges
    call this%process_exchanges(num_soln%exchangelist, new_cnxns)
    if (new_cnxns%Count() == 0) then
      return
    end if

    write (iout, '(1x,a,i0,a,a)') 'Created ', new_cnxns%Count(), &
      ' model connections for solution ', trim(solution%name)

    ! craete the topology of models participating in the interfaces
    call this%setup_model_connections(new_cnxns)

    ! replace numerical exchanges in solution with connections
    call this%setup_solution_connections(new_cnxns, num_soln)

    ! clean up local resources
    call new_cnxns%Clear(destroy=.false.)

  end subroutine create_connections

  !> @brief Create connections from exchanges
  !!
  !! If the configuration demands it, this will create connections
  !! for the exchanges (one connection per exchange), add them to
  !! the global list, and return them.
  !<
  subroutine process_exchanges(this, exchanges, new_cnxns)
    ! -- modules
    use ListsModule, only: baseconnectionlist, baseexchangelist
    use VersionModule, only: IDEVELOPMODE
    ! -- dummy
    class(ConnectionFactoryType) :: this !< the connection factory
    type(ListType), pointer, intent(in) :: exchanges !< the exchanges
    type(ListType), intent(inout) :: new_cnxns !< the created connections
    ! -- local
    class(DisConnExchangeType), pointer :: cnxn_exg
    class(BaseExchangeType), pointer :: base_exg
    class(SpatialModelConnectionType), pointer :: mdl_cnxn
    logical(LGP) :: periodic
    integer(I4B) :: iex, ibasex

    do iex = 1, exchanges%Count()
      cnxn_exg => GetDisConnExchangeFromList(exchanges, iex)
      if (.not. associated(cnxn_exg)) then
        ! if it is not DisConnExchangeType, we can skip it
        cycle
      end if

      ! for now, if we have XT3D on the interface, we use a connection,
      ! (this will be more generic in the future)
      if (cnxn_exg%use_interface_model() .or. cnxn_exg%dev_ifmod_on &
          .or. this%dev_always_ifmod) then

        ! we should not get period connections here
        periodic = (cnxn_exg%v_model1 == cnxn_exg%v_model2)
        if (periodic) then
          write (*, *) 'Error (which should never happen): interface model '// &
            'does not support periodic boundary condition'
          call ustop()
        end if

        if (cnxn_exg%v_model1%is_local) then
          ! create model connection for model 1
          mdl_cnxn => create_model_connection(cnxn_exg%model1, cnxn_exg)
          call add_smc_to_list(baseconnectionlist, mdl_cnxn)
          call add_smc_to_list(new_cnxns, mdl_cnxn)
        end if

        ! and for model 2
        if (cnxn_exg%v_model2%is_local) then
          mdl_cnxn => create_model_connection(cnxn_exg%model2, cnxn_exg)
          call add_smc_to_list(baseconnectionlist, mdl_cnxn)
          call add_smc_to_list(new_cnxns, mdl_cnxn)
        end if

        ! remove this exchange from the base list, ownership
        ! now lies with the connection
        do ibasex = 1, baseexchangelist%Count()
          base_exg => GetBaseExchangeFromList(baseexchangelist, ibasex)
          if (cnxn_exg%id == base_exg%id) then
            call baseexchangelist%RemoveNode(ibasex, .false.)
            exit
          end if
        end do
      end if
    end do

  end subroutine process_exchanges

  !> @brief Create a model connection of a given type
  !<
  function create_model_connection(model, exchange) result(connection)
    use SimModule, only: ustop
    use gwfModule, only: gwfModelType
    use gwtModule, only: gwtModelType
    use gwfgwfConnectionModule, only: gwfgwfConnectionType
    use gwtgwtConnectionModule, only: gwtgwtConnectionType

    class(NumericalModelType), pointer, intent(in) :: model !< the model for which the connection will be created
    class(DisConnExchangeType), pointer, intent(in) :: exchange !< the type of connection
    class(SpatialModelConnectionType), pointer :: connection !< the created connection

    ! concrete connection types
    class(gwfgwfConnectionType), pointer :: gwfgwfConnection => null()
    class(gwtgwtConnectionType), pointer :: gwtgwtConnection => null()

    connection => null()

    ! create the requested connections
    select case (exchange%typename)
    case ('GWF-GWF')
      allocate (gwfgwfConnectionType :: gwfgwfConnection)
      call gwfgwfConnection%construct(model, exchange)
      connection => gwfgwfConnection
      gwfgwfConnection => null()
    case ('GWT-GWT')
      allocate (gwtgwtConnectionType :: gwtgwtConnection)
      call gwtgwtConnection%construct(model, exchange)
      connection => gwtgwtConnection
      gwtgwtConnection => null()
    case default
      write (*, *) 'Error: unsupported exchange type: '//exchange%typename
      call ustop()
    end select

  end function create_model_connection

  !> @brief Add connections to solution, removing exchanges replaced by a connection.
  !<
  subroutine setup_solution_connections(this, connections, solution)
    class(ConnectionFactoryType) :: this !< the connection builder object
    type(ListType), intent(inout) :: connections !< the connections created for the solution
    class(NumericalSolutionType), pointer, intent(in) :: solution !< the solution to which the connections are set
    ! local
    class(*), pointer :: p_exg1, p_exg2, p_cnxn
    class(SpatialModelConnectionType), pointer :: connection
    integer(I4B) :: iex, iconn
    logical(LGP) :: keep_exg
    type(ListType) :: kept

    ! first add all exchanges not replaced by the connections to a list
    do iex = 1, solution%exchangelist%Count()
      p_exg1 => solution%exchangelist%GetItem(iex)
      ! will this exchange be replaced by a connection?
      keep_exg = .true.
      do iconn = 1, connections%Count()
        connection => get_smc_from_list(connections, iconn)
        p_exg2 => connection%prim_exchange
        if (associated(p_exg2, p_exg1)) then
          ! if so, don't add it to the list
          keep_exg = .false.
          exit
        end if
      end do

      if (keep_exg) then
        call kept%Add(p_exg1)
      end if
    end do

    ! first add persisting exchanges
    call solution%exchangelist%Clear(destroy=.false.)
    do iex = 1, kept%Count()
      p_exg1 => kept%GetItem(iex)
      call solution%exchangelist%Add(p_exg1)
    end do

    ! now add connections
    do iconn = 1, connections%Count()
      p_cnxn => connections%GetItem(iconn)
      call solution%exchangelist%Add(p_cnxn)
    end do

    ! clean up
    call kept%Clear(destroy=.false.)

  end subroutine setup_solution_connections

  !> @brief Create connectivity of models which contribute to the interface
  !!
  !! This loops over all connections and creates a halo with all
  !! models from the numerical solution. The model halo will be used to
  !! extend the interface grid to include cells from models which are
  !< indirectly connected, through yet another exchange object.
  subroutine setup_model_connections(this, connections)
    class(ConnectionFactoryType) :: this !< the connection builder object
    type(ListType), intent(inout) :: connections !< all connections that are created for this solution
    ! local
    integer(I4B) :: iconn
    class(SpatialModelConnectionType), pointer :: connection

    ! create halo for the model connections
    do iconn = 1, connections%Count()
      connection => get_smc_from_list(connections, iconn)
      call connection%createModelHalo()
    end do

  end subroutine setup_model_connections

end module ConnectionFactoryModule

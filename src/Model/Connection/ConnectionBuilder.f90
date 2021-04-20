module ConnectionBuilderModule
  use KindModule, only: I4B, LGP
  use ListModule, only: ListType, arePointersEqual, isEqualIface, ListNodeType
  use BaseSolutionModule, only: BaseSolutionType
  use NumericalSolutionModule, only: NumericalSolutionType
  use DisConnExchangeModule, only: DisConnExchangeType,               &
                                GetDisConnExchangeFromList
  use NumericalModelModule, only: NumericalModelType
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                CastAsSpatialModelConnectionClass,    &
                                GetSpatialModelConnectionFromList,    &
                                AddSpatialModelConnectionToList
  
  implicit none  
  private
  
  type, public :: ConnectionBuilderType
  contains
    procedure, pass(this) :: processSolution
    procedure, private, pass(this) :: processExchanges
    procedure, private, pass(this) :: setConnectionsToSolution
    procedure, private, pass(this) :: assignExchangesToConnections
  end type ConnectionBuilderType  
  
  contains

  subroutine processSolution(this, solution)
    class(ConnectionBuilderType) :: this
    class(BaseSolutionType), pointer :: solution
    class(NumericalSolutionType), pointer :: numSol
    ! local
    type(ListType) :: newConnections

    ! we only deal with Num. Sol. here
    select type (solution)
    class is (NumericalSolutionType)
      numSol => solution
    class default
      return
    end select

    ! 1. create the connections and add local exchanges
    call this%processExchanges(numSol%exchangelist, newConnections)   

    ! 2. replace numerical exchanges in solution with connections
    call this%setConnectionsToSolution(newConnections, numSol)

    ! 3. set the global exchanges from this solution to
    ! the model connections
    call this%assignExchangesToConnections(numSol%exchangelist, newConnections)

  end subroutine processSolution

  subroutine processExchanges(this, exchanges, newConnections)    
    use ListsModule, only: baseconnectionlist
    class(ConnectionBuilderType) :: this
    type(ListType), pointer, intent(in) :: exchanges
    type(ListType), intent(inout) :: newConnections
    ! local
    class(DisConnExchangeType), pointer :: conEx
    integer(I4B) :: iex
    class(SpatialModelConnectionType), pointer :: modelConnection

    do iex = 1, exchanges%Count()
      conEx => GetDisConnExchangeFromList(exchanges, iex)
      if (.not. associated(conEx)) then
        ! if it is not DisConnExchangeType, we can skip it
        continue
      end if
    
      ! if we have XT3D on the interface, we use a connection:
      if (conEx%ixt3d > 0) then

        ! fetch connection for model 1:
        modelConnection => lookupConnection(conEx%model1, conEx%typename)
        if (.not. associated(modelConnection)) then
          ! create new model connection
          modelConnection => createModelConnection(conEx%model1, conEx%typename)
          call AddSpatialModelConnectionToList(baseconnectionlist, modelConnection)
          call AddSpatialModelConnectionToList(newConnections, modelConnection)
        end if
          
        ! add exchange to connection
        call modelConnection%addExchange(conEx)
        
        ! and fetch for model 2
        modelConnection => lookupConnection(conEx%model2, conEx%typename)
        if (.not. associated(modelConnection)) then
          ! create new model connection
          modelConnection => createModelConnection(conEx%model2, conEx%typename)
          call AddSpatialModelConnectionToList(baseconnectionlist, modelConnection)
          call AddSpatialModelConnectionToList(newConnections, modelConnection)
        end if

        ! add exchange to connection
        call modelConnection%addExchange(conEx)

      end if
    end do

  end subroutine processExchanges
  
  function createModelConnection(model, connectionType) result(connection)
    use SimModule, only: ustop
    use GwfGwfConnectionModule, only: GwfGwfConnectionType
    use GwfModule, only: GwfModelType
    
    class(NumericalModelType), pointer , intent(in) :: model
    character(len=*), intent(in)                    :: connectionType
    class(SpatialModelConnectionType), pointer :: connection
    
    ! different concrete connection types:
    class(GwfGwfConnectionType), pointer :: gwfConnection => null()
    
    connection => null()
    
    ! select on type of connection to create
    select case(connectionType)       
      case('GWF-GWF')      
        allocate(GwfGwfConnectionType :: gwfConnection)
        call gwfConnection%construct(model)
        connection => gwfConnection        
        gwfConnection => null()
        
      !case('GWT-GWT')
      !case('GWF-GWT')      
      case default
        write(*,*) 'Error (which should never happen): undefined exchangetype found'
        call ustop()
    end select   
    
  end function createModelConnection
  
  
  ! function gets an existing connection for the model, based on the
  ! type of exchange. Return null() when not found
  function lookupConnection(model, exchangeType) result(connection)
    use ListsModule, only: baseconnectionlist
    
    class(NumericalModelType), pointer  :: model
    character(len=*)                    :: exchangeType
    class(SpatialModelConnectionType), pointer :: connection    
    
    ! locals
    integer(I4B) :: i
    class(SpatialModelConnectionType), pointer :: candidate
    
    connection => null()
    
    call baseconnectionlist%Reset()
    do i = 1, baseconnectionlist%Count()      
      candidate => GetSpatialModelConnectionFromList(baseconnectionlist,i)      
      if (candidate%typename == exchangeType) then
        if (associated(candidate%owner, model)) then
          connection => candidate
          return
        end if
      end if      
    end do
    
  end function lookupConnection

  !> @brief Set connections to the solution
  !!
  !! Replaces the exchanges in the solution with the
  !! corresponding connections.
  !<
  subroutine setConnectionsToSolution(this, connections, solution)
    class(ConnectionBuilderType) :: this                          !< the connection builder object
    type(ListType), intent(inout) :: connections                  !< the connections created for the solution
    class(NumericalSolutionType), pointer, intent(in) :: solution !< the solution to which the connections are set
    ! local
    type(ListType) :: keepList
    class(*), pointer :: exPtr, connPtr
    procedure(isEqualIface), pointer :: equalFct
    class(SpatialModelConnectionType), pointer :: conn
    integer(I4B) :: iex, iconn
    logical(LGP) :: keepExchange

    equalFct => arePointersEqual
    ! first add all exchanges not replaced by the connections to a list
    do iex = 1, solution%exchangelist%Count()
      exPtr => solution%exchangelist%GetItem(iex)
      ! will this exchange be replaced by a connection?
      keepExchange = .true.
      do iconn = 1, connections%Count()
        conn => GetSpatialModelConnectionFromList(connections,iconn)
        if (conn%localExchanges%Contains(exPtr, equalFct)) then
          ! if so, don't add it to the list
          keepExchange = .false.
          exit
        end if
      end do

      if (keepExchange) then
        call keepList%Add(exPtr)
      end if
    end do

    ! first add persisting exchanges
    call solution%exchangelist%Clear(destroy=.false.)
    do iex = 1, keepList%Count()
      exPtr => keepList%GetItem(iex)
      call solution%exchangelist%Add(exPtr)
    end do

    ! now add connections
    do iconn = 1, connections%Count()
      connPtr => connections%GetItem(iconn)
      call solution%exchangelist%Add(connPtr)
    end do

    ! clean up
    call keepList%Clear(destroy=.false.)

  end subroutine setConnectionsToSolution

  !> @brief Add global exchanges from a certain numerical solution
  !! to the connections.
  !!
  !! This concerns all exchanges of the proper type. Inside the
  !! connection it will be used to extend the interface grid with
  !! the possibility to include cells from models which are indirectly
  !! connected, through yet another exchange object.
  !<
  subroutine assignExchangesToConnections(this, exchanges, connections)
    class(ConnectionBuilderType) :: this              !< the connection builder object
    type(ListType), pointer, intent(in) :: exchanges  !< all exchanges in a solution
    type(ListType), intent(inout) :: connections         !< all connections that are created for this solution
    ! local
    integer(I4B) :: iex, iconn
    class(DisConnExchangeType), pointer :: conEx
    class(SpatialModelConnectionType), pointer :: modelConn
    class(*), pointer :: exPtr
    type(ListType) :: keepList

    ! first filter on exchanges of proper type
    do iex = 1, exchanges%Count()
      conEx => GetDisConnExchangeFromList(exchanges, iex)
      if (.not. associated(conEx)) then
        ! if it is not DisConnExchangeType, we should skip it
        continue
      end if
      exPtr => conEx
      call keepList%Add(exPtr)
    end do

    ! now add them to the model connections
    do iconn = 1, connections%Count()
      modelConn => GetSpatialModelConnectionFromList(connections, iconn)
      do iex = 1, keepList%Count()
        exPtr => keepList%GetItem(iex)
        call modelConn%globalExchanges%Add(exPtr)
      end do
    end do

    ! clean
    call keepList%Clear(destroy=.false.)
   
  end subroutine assignExchangesToConnections
  
  
end module ConnectionBuilderModule

module ConnectionBuilderModule
  use KindModule, only: I4B
  use NumericalExchangeModule,  only: NumericalExchangeType
  use NumericalModelModule,     only: NumericalModelType
  use ModelConnectionModule
  
  implicit none  
  private
  
  type, public :: ConnectionBuilderType
  contains
    ! TODO: better name? 
    ! This procedure either creates a connection, or extends it for the given exchange.
    procedure, pass(this) :: processExchangeImpl
	  generic, public :: processExchange => processExchangeImpl
  end type ConnectionBuilderType  
  
  contains

  subroutine processExchangeImpl(this, exchange)
    use ListsModule, only: connectionlist
  
    class(ConnectionBuilderType) :: this
    class(NumericalExchangeType), pointer :: exchange
    
    ! local variables
    class(ModelConnectionType), pointer :: modelConnection
    class(*), pointer                   :: newConnection 
    
    ! fetch connection for model 1:
    modelConnection => lookupConnection(exchange%m1, exchange%typename)
    if (.not. associated(modelConnection)) then
      ! create new model connection
      newConnection => createModelConnection(exchange%m1, exchange%typename)      
      ! add to global list
      call connectionList%Add(newConnection)
      modelConnection => CastAsModelConnectionClass(newConnection)
    end if
      
    ! add exchange to connection
    call modelConnection%addExchange(exchange)
    
    ! and fetch for model 2
    modelConnection => lookupConnection(exchange%m2, exchange%typename)
    if (.not. associated(modelConnection)) then
      ! create new model connection
      newConnection => createModelConnection(exchange%m2, exchange%typename)
      call connectionList%Add(newConnection)
      modelConnection => CastAsModelConnectionClass(newConnection)
    end if
      
    ! add exchange to connection
    call modelConnection%addExchange(exchange)
          
  end subroutine processExchangeImpl
  
  function createModelConnection(model, connectionType) result(connection)
    use SimModule, only: ustop
    use GwfGwfConnectionModule, only: GwfGwfConnectionType
    use GwfModule, only: GwfModelType
    
    class(NumericalModelType), pointer , intent(in) :: model
    character(len=*), intent(in)                    :: connectionType
    class(ModelConnectionType), pointer :: connection
    
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
    use ListsModule, only: connectionlist
    
    class(NumericalModelType), pointer  :: model
    character(len=*)                    :: exchangeType
    class(ModelConnectionType), pointer :: connection    
    
    ! locals
    integer(I4B) :: i
    class(ModelConnectionType), pointer :: candidate
    
    connection => null()
    
    call connectionlist%Reset()
    do i = 1, connectionlist%Count()
      candidate => GetConnectionFromList(connectionlist,i)      
      if (candidate%connectionType == exchangeType) then
        if (associated(candidate%owner, model)) then
          connection => candidate
          return
        end if
      end if
      
    end do
    
  end function lookupConnection
  
  
end module ConnectionBuilderModule
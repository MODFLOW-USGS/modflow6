module ConnectionBuilderModule
  use KindModule, only: I4B
  use BaseExchangeModule, only: BaseExchangeType
  use DisConnExchangeModule, only: DisConnExchangeType
  use NumericalModelModule, only: NumericalModelType
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                CastAsSpatialModelConnectionClass,    &
                                GetSpatialModelConnectionFromList
  
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
    use ListsModule, only: baseconnectionlist  
    class(ConnectionBuilderType) :: this
    class(BaseExchangeType), pointer :: exchange
    ! local
    class(DisConnExchangeType), pointer :: conEx
    class(SpatialModelConnectionType), pointer :: modelConnection
    class(*), pointer                   :: newConnection     

    select type(exchange)
      class is(DisConnExchangeType)
        conEx => exchange
      class default
      ! don't do anything here for now...
      return
    end select
    
    ! fetch connection for model 1:
    modelConnection => lookupConnection(conEx%model1, conEx%typename)
    if (.not. associated(modelConnection)) then
      ! create new model connection
      newConnection => createModelConnection(conEx%model1, conEx%typename)      
      ! add to global list
      call baseconnectionlist%Add(newConnection)
      modelConnection => CastAsSpatialModelConnectionClass(newConnection)
    end if
      
    ! add exchange to connection
    call modelConnection%addExchange(conEx)
    
    ! and fetch for model 2
    modelConnection => lookupConnection(conEx%model2, conEx%typename)
    if (.not. associated(modelConnection)) then
      ! create new model connection
      newConnection => createModelConnection(conEx%model2, conEx%typename)
      call baseconnectionlist%Add(newConnection)
      modelConnection => CastAsSpatialModelConnectionClass(newConnection)
    end if
      
    ! add exchange to connection
    call modelConnection%addExchange(conEx)
          
  end subroutine processExchangeImpl
  
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
  
  
end module ConnectionBuilderModule

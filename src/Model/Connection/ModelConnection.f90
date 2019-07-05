! TODO: module description
module ModelConnectionModule
  use KindModule, only: I4B
  use ConstantsModule, only: LENPACKAGENAME
  use ListModule
  use NumericalModelModule, only: NumericalModelType
  use NumericalExchangeModule, only: NumericalExchangeType 
  
  implicit none
	private
  
  public :: GetConnectionFromList
  public :: CastAsModelConnectionClass
  
	! Abstract base class for a model's connection with other models. It will enable e.g.
	! the interfacing between a GWFModel and its GWF-neigbours, similarly for a
	! GWTModel, and also the (heterogeneous) interfacing between models of type
	! GWT and GWF.
  type, abstract, public :: ModelConnectionType		
    class(NumericalModelType), pointer  :: owner => null() ! the model whose connection this is  
    character(len=7)                    :: connectionType
    character(len=LENPACKAGENAME)       :: name
  
  contains
    
    procedure (defineConnectionIFace), deferred, pass(this) :: mc_df
        
    ! derived types should decide for themselves how the overall connection is 
    ! altered when another model is connected
    procedure (addExchangeIFace), deferred, pass(this) :: addExchange
       
  end type ModelConnectionType
    
  abstract interface
  
    subroutine defineConnectionIFace(this)
      import :: ModelConnectionType
      class(ModelConnectionType), intent(inout) :: this
    end subroutine defineConnectionIFace
  
    subroutine addExchangeIFace(this, exchange)
      import :: ModelConnectionType, NumericalExchangeType
      class(ModelConnectionType), intent(inout) :: this
      class(NumericalExchangeType), pointer, intent(in) :: exchange      
    end subroutine addExchangeIFace   
    
  end interface

  ! derived classes should now how they change when a model is connected
 
  
contains ! module procedures
  
  ! list routines
  function CastAsModelConnectionClass(obj) result (res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(ModelConnectionType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (ModelConnectionType)
      res => obj
    end select
    return
  end function CastAsModelConnectionClass
  
  function GetConnectionFromList(list, idx) result(res)
    type(ListType),       intent(inout) :: list
    integer(I4B),              intent(in)    :: idx
    class(ModelConnectionType), pointer    :: res
    
    ! local
    class(*), pointer :: obj
    obj => list%GetItem(idx)
    res => CastAsModelConnectionClass(obj)
    !
    return
  end function GetConnectionFromList
  
  
  
end module ModelConnectionModule
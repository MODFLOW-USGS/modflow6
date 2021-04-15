! TODO: module description
module ModelConnectionModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENPACKAGENAME, LENMEMPATH
  use SparseModule, only:sparsematrix
  use ListModule
  use NumericalModelModule, only: NumericalModelType
  use DisConnExchangeModule, only: DisConnExchangeType 
  
  implicit none
  private
  
  public :: GetConnectionFromList
  public :: CastAsModelConnectionClass
  
	! Abstract base class for a model's connection with other models. It will enable e.g.
	! the interfacing between a GWFModel and its GWF-neigbours, similarly for a
	! GWTModel, and also the (heterogeneous) interfacing between models of type
	! GWT and GWF. It is built from DisConnExchangeType objects.
  type, abstract, public :: ModelConnectionType
    class(NumericalModelType), pointer  :: owner => null()  ! the model whose connection this is  
    character(len=7)                    :: connectionType
    character(len=LENPACKAGENAME)       :: name
    character(len=LENMEMPATH)            :: memoryPath
    
    integer(I4B)                        :: iNewton          ! == 1 for newton-raphson, 0 otherwise
  
    ! all numerical exchanges in the same solution
    type(ListType)                     :: globalExchanges
       
  contains
    
    procedure (defineConnectionIFace), deferred, pass(this)       :: mc_df
    procedure (allocateReadIFace), deferred, pass(this)           :: mc_ar
    procedure (addConnectionsToMatrixIFace), deferred, pass(this) :: mc_ac  
    procedure (calculateCoefficientsIFace), deferred, pass(this)  :: mc_cf
    procedure (mapCoefficientsIFace), deferred, pass(this)        :: mc_mc
    procedure (fillCoefficientsIFace), deferred, pass(this)       :: mc_fc
    procedure (deallocateIFace), deferred, pass(this)             :: mc_da
    
    ! derived types should decide for themselves how the overall connection is 
    ! altered when another model is connected
    procedure (addExchangeIFace), deferred, pass(this) :: addExchange
    
  end type ModelConnectionType
    
  abstract interface
  
    subroutine defineConnectionIFace(this)
      import :: ModelConnectionType
      class(ModelConnectionType), intent(inout) :: this
    end subroutine defineConnectionIFace
  
    subroutine allocateReadIFace(this)
      import :: ModelConnectionType
      class(ModelConnectionType), intent(inout) :: this
    end subroutine allocateReadIFace
    
    subroutine addConnectionsToMatrixIFace(this, sparse)
      import :: ModelConnectionType, sparsematrix
      class(ModelConnectionType), intent(inout) :: this
      type(sparsematrix), intent(inout) :: sparse
    end subroutine
    
    subroutine calculateCoefficientsIFace(this, kiter)
      import :: ModelConnectionType, I4B
      class(ModelConnectionType), intent(inout) :: this
      integer(I4B), intent(in) :: kiter
    end subroutine calculateCoefficientsIFace
    
    subroutine mapCoefficientsIFace(this, iasln, jasln)
      import :: ModelConnectionType, I4B
      class(ModelConnectionType), intent(inout) :: this
      integer(I4B), dimension(:), intent(in) :: iasln
      integer(I4B), dimension(:), intent(in) :: jasln
    end subroutine mapCoefficientsIFace
    
    subroutine fillCoefficientsIFace(this, kiter, amatsln, njasln, rhssln, inwtflag)
      import :: ModelConnectionType, I4B, DP
      class(ModelConnectionType), intent(inout) :: this
      integer(I4B), intent(in) :: kiter
      real(DP), dimension(:), intent(inout) :: amatsln
      integer(I4B),intent(in) :: njasln
      real(DP), dimension(:), intent(inout) :: rhssln
      integer(I4B), intent(in) :: inwtflag
    end subroutine fillCoefficientsIFace

    subroutine deallocateIFace(this)
      import :: ModelConnectionType, I4B, DP
      class(ModelConnectionType), intent(inout) :: this
    end subroutine deallocateIFace
     
    subroutine addExchangeIFace(this, exchange)
      import :: ModelConnectionType, DisConnExchangeType
      class(ModelConnectionType), intent(inout) :: this
      class(DisConnExchangeType), pointer, intent(in) :: exchange
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

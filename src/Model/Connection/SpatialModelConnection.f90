! TODO: module description
module SpatialModelConnectionModule
  use KindModule, only: I4B
	use ModelConnectionModule
	use NumericalModelModule, only: NumericalModelType
  use NumericalExchangeModule, only: NumericalExchangeType, GetNumericalExchangeFromList
  use GridConnectionModule, only: GridConnectionType
  use ListModule, only: ListType
  
	implicit none
	private

	! Class to manage spatial connection of a model to one or more models of the same type.
	! Spatial connection here means that the model domains (spatial discretization) are adjacent
	! and connected.
	type, public, abstract, extends(ModelConnectionType) :: SpatialModelConnectionType
        
    ! aggregation:
    type(ListType), pointer :: exchangeList => null()
    
    ! TODO_MJR: mem mgt of these guys:
    integer(I4B) :: nrOfConnections ! TODO_MJR: do we need this one?
    type(GridConnectionType), pointer :: gridConnection => null()
    
    integer(I4B), dimension(:), pointer, contiguous :: globalOffdiagIdx => null()
    
  contains
    procedure, pass(this) :: spatialConnection_ctor
    generic, public :: construct => spatialConnection_ctor
    procedure, pass(this) :: addExchange => addExchangeToSpatialConnection
    procedure, pass(this) :: mc_df => defineSpatialConnection 
    procedure, pass(this) :: mc_mc => mapCoefficients
    procedure, pass(this) :: mc_ac => addConnectionsToMatrix
    procedure, private, pass(this) :: getNrOfConnections
  end type SpatialModelConnectionType

contains ! module procedures
  
  subroutine spatialConnection_ctor(this, model, name)
    class(SpatialModelConnectionType), intent(inout) :: this
    class(NumericalModelType), intent(in), pointer :: model
    character(len=*), intent(in) :: name
    
    ! base props:
    this%name = name
    this%memoryOrigin = trim(this%name)
    this%owner => model
    
    this%nrOfConnections = 0
    
    allocate(this%exchangeList)
    allocate(this%gridConnection)    
    
  end subroutine spatialConnection_ctor
  
  subroutine addExchangeToSpatialConnection(this, exchange)
    class(SpatialModelConnectionType), intent(inout) :: this
	  class(NumericalExchangeType), pointer, intent(in) :: exchange
    ! local
    class(*), pointer :: exg
    
    ! assign exchange to unlim. polymorphic pointer 
    ! to able to add it to the list
    exg => exchange
    call this%exchangeList%Add(exg)
    
  end subroutine addExchangeToSpatialConnection
  
  subroutine defineSpatialConnection(this)
    class(SpatialModelConnectionType), intent(inout) :: this    
    
    ! create the grid connection data structure
    this%nrOfConnections = this%getNrOfConnections()
    call this%gridConnection%construct(this%nrOfConnections, this%name)
     
    ! TODO_MJR: move this?
    allocate(this%globalOffdiagIdx(this%nrOfConnections))
    
  end subroutine defineSpatialConnection
  
  subroutine mapCoefficients(this, iasln, jasln)
    use SimModule, only: ustop
    use GridConnectionModule
    class(SpatialModelConnectionType), intent(inout) :: this
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! local
    integer(I4B) :: i, nLinks
    integer(I4B) :: iglo, jglo ! global (solution matrix) indices
    type(LinkedNodeType), dimension(:), pointer :: links => null()
    integer(I4B) :: csrIndex
    
    links => this%gridConnection%linkedNodes    
    nLinks = this%gridConnection%nrOfLinks
       
    do i=1, nLinks
      iglo = links(i)%ownIndex + this%owner%moffset
      jglo = links(i)%linkedIndex + links(i)%connectedModel%moffset
      csrIndex = getCSRIndex(iglo, jglo, iasln, jasln)
      if (csrIndex == -1) then
        ! this should not be possible
        write(*,*) 'Error: cannot find cell connection in global system'
        call ustop()
      end if
      this%globalOffdiagIdx(i) = csrIndex
    end do
    
  end subroutine mapCoefficients
  
  ! add connections to global matrix, does not fill in symmetric elements: i.e.,
  ! it needs to be called twice for two connected cells (elements m-n and n-m)
  subroutine addConnectionsToMatrix(this, sparse)
    use SparseModule, only:sparsematrix
    class(SpatialModelConnectionType), intent(inout) :: this
    type(sparsematrix), intent(inout) :: sparse 
    ! local
    integer(I4B) :: ic
    integer(I4B) :: iglo, jglo        ! global row (i) and column (j) numbers
    integer(I4B) :: iex, iconn
    type(NumericalExchangeType), pointer :: numEx
    
    numEx => null()
    
    ! fill primary links, with local numbering: n => m or m <= n, 
    ! and calculate global numbers for sparse
    do iex=1, this%exchangeList%Count()
      numEx => GetNumericalExchangeFromList(this%exchangeList, iex)
      do iconn=1, numEx%nexg
        if (associated(numEx%m1, this%owner)) then          
          call this%gridConnection%addLink( numEx%nodem1(iconn),  &
                                            numEx%nodem2(iconn),  &
                                            numEx%cl1(iconn),     &
                                            numEx%cl2(iconn),     &
                                            numEx%hwva(iconn),    &
                                            numEx%ihc(iconn),     &
                                            numEx%m2)          
          iglo = numEx%nodem1(iconn) + this%owner%moffset
          jglo = numEx%nodem2(iconn) + numEx%m2%moffset          
        else  
          ! then with nodes, lenghts, models reversed:
          call this%gridConnection%addLink( numEx%nodem2(iconn),  &
                                            numEx%nodem1(iconn),  &
                                            numEx%cl2(iconn),     &
                                            numEx%cl1(iconn),     &
                                            numEx%hwva(iconn),    &
                                            numEx%ihc(iconn),     &
                                            numEx%m1)
          iglo = numEx%nodem2(iconn) + this%owner%moffset
          jglo = numEx%nodem1(iconn) + numEx%m1%moffset          
        end if      
                
        ! add global numbers to sparse TODO_MJR: enable this
        ! call sparse%addconnection(iglo, jglo, 1)        
      end do
    end do
    
    
  end subroutine
  
  ! count total nr. of connection between cells, from the exchanges
  function getNrOfConnections(this) result(nrConns)
    class(SpatialModelConnectionType), intent(inout) :: this
    integer(I4B) :: nrConns
    
    !local
    integer(I4B) :: iex
    type(NumericalExchangeType), pointer :: numEx
    
    nrConns = 0
    do iex = 1, this%exchangeList%Count()
      numEx => GetNumericalExchangeFromList(this%exchangeList, iex)
      nrConns = nrConns + numEx%nexg
    end do
    
  end function getNrOfConnections
  
  ! TODO_MJR: move this to generic place?
  ! return index for element i,j in CSR storage, and -1 when not there
  function getCSRIndex(i, j, ia, ja) result(csrIndex)
    integer(I4B) :: i, j                          ! the element to get the index for
    integer(I4B), dimension(:), intent(in) :: ia  ! csr ia
    integer(I4B), dimension(:), intent(in) :: ja  ! csr ja
    integer(I4B) :: csrIndex                 ! the resulting index
    ! local
    integer(I4B) :: idx
    
    csrIndex = -1
    do idx = ia(i), ia(i+1)-1
      if (ja(idx) == j) then
        csrIndex = idx
        return
      end if
    end do
    
  end function
  
end module SpatialModelConnectionModule

	
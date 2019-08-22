module GwfInterfaceModelModule
  use KindModule, only: I4B, DP
  use GwfModule, only: GwfModelType
  use GridConnectionModule
  use GwfDisuModule
  
  implicit none
  private
  
  type, public, extends(GwfModelType) :: GwfInterfaceModelType
    
  contains    
    procedure, pass(this) :: construct
    procedure, pass(this) :: buildDiscretization
  end type
 
contains

  ! minimal construction
  subroutine construct(this, name)
    class(GwfInterfaceModelType), intent(inout) :: this
    character(len=*), intent(in)  :: name
    
    call this%allocate_scalars(name)
    
    ! set default model options
    this%inewton = 0
    
  end subroutine
   
  subroutine buildDiscretization(this, gridConnection)    
    use SparseModule, only: sparsematrix
    class(GwfInterfaceModelType), intent(inout) :: this
    class(GridConnectionType), intent(in) :: gridConnection
    ! local
    integer(I4B) :: icell, nrOfCells
    
    ! create disu
    call disu_cr(this%dis, this%name, -1, -1)
    
    ! nodes, nja, nvert
    nrOfCells = gridConnection%nrOfCells    
    this%dis%nodes = nrOfCells
    this%dis%nodesuser = nrOfCells
    
    call this%dis%allocate_arrays()
    
    ! fill data
    do icell = 1, nrOfCells
      this%dis%top(icell) = 0.0
    end do
    
  end subroutine
  
end module GwfInterfaceModelModule
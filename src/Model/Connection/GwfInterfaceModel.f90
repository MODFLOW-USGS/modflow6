module GwfInterfaceModelModule
  use KindModule, only: I4B, DP
  use NumericalModelModule, only: NumericalModelType
  use GwfModule, only: GwfModelType
  use GwfNpfModule ,only: npf_cr
  use GridConnectionModule
  use GwfDisuModule
  use GwfNpfModule
  use GwfOcModule
  implicit none
  private
  
  ! Interface model, to help determining conductivity coefficients in the interface
  ! region between a GwfModel and its neighbors. Note: this model itself will not be
  ! part of the solution matrix. The DISU discretization is a composition of parts of 
  ! multiple grids, possibly of different type.
  type, public, extends(GwfModelType) :: GwfInterfaceModelType
    
  contains    
    procedure, pass(this) :: construct
    procedure, pass(this) :: createModel
    ! private stuff
    procedure, private, pass(this) :: buildDiscretization    
  end type
 
contains

  ! minimal construction
  subroutine construct(this, name)
    class(GwfInterfaceModelType), intent(inout) :: this
    character(len=*), intent(in)  :: name
        
    call this%allocate_scalars(name)
    this%name = name
    
    ! set default model options
    this%inewton = 0
    
    ! we need this dummy value
    this%innpf = 999
    
  end subroutine
   
  ! set up the interface model, analogously to what happens in gwf_cr
  subroutine createModel(this, gridConnection)
    class(GwfInterfaceModelType), intent(inout) :: this
    class(GridConnectionType), intent(in) :: gridConnection
    
    ! create discretization
    call this%buildDiscretization(gridConnection)
    
    ! create packages
    ! TODO_MJR: this really depends on how we want to merge
    ! a heteregeneous composition of connected models...
    call npf_cr(this%npf, this%name, this%innpf, this%iout)    
    ! call xt3d_cr..., etc.
    call oc_cr(this%oc, this%name, this%inoc, this%iout)
    
  end subroutine
  
  ! note: this should become private
  subroutine buildDiscretization(this, gridConnection)  
    use ConnectionsModule 
    use SparseModule, only: sparsematrix
    class(GwfInterfaceModelType), intent(inout) :: this
    class(GridConnectionType), intent(in) :: gridConnection
    ! local
    integer(I4B) :: icell, nrOfCells, idx
    integer(I4B) :: ierror
    type(NumericalModelType), pointer :: model
    type(ConnectionsType), pointer :: connections
    
    ! create disu
    call disu_cr(this%dis, this%name, -1, -1)
    
    ! the following is similar to dis_df, we should refactor this
    ! set nodes, nvertices skipped for as long as possible
    nrOfCells = gridConnection%nrOfCells    
    this%dis%nodes = nrOfCells
    this%dis%nodesuser = nrOfCells
    call this%dis%allocate_arrays()
    
    ! fill data
    do icell = 1, nrOfCells
      
      idx = gridConnection%idxToGlobal(icell)%index
      model => gridConnection%idxToGlobal(icell)%model
      
      this%dis%top(icell) = model%dis%top(idx)
      this%dis%bot(icell) = model%dis%bot(idx)
      this%dis%area(icell) = model%dis%area(idx)
      
    end do
     
    ! grid connections follow from GridConnection:
    this%dis%con => gridConnection%connections
    this%dis%njas =  this%dis%con%njas
  
    ! TODO_MJR: add vertices, cell2d here?
    
  end subroutine
  
  subroutine setNpfData(npf, this)
    class(GwfInterfaceModelType), intent(inout) :: this
    class(GwfNpftype) :: npf
    
        
  end subroutine
  
end module GwfInterfaceModelModule
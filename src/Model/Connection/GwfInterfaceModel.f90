module GwfInterfaceModelModule
  use KindModule, only: I4B, DP  
  use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList
  use GwfModule, only: GwfModelType
  use GridConnectionModule
  use BaseDisModule
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
    class(GridConnectionType), pointer    :: gridConnection => null()
    class(GwfModelType), private, pointer :: gwfModel => null() ! conveniently points to the owning model in gridconnection
  contains
    procedure, pass(this) :: construct
    procedure, pass(this) :: createModel
    procedure, pass(this) :: defineModel
    procedure, pass(this) :: allocateAndReadModel    
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
    
  end subroutine construct
   
  ! set up the interface model, analogously to what happens in gwf_cr
  subroutine createModel(this, gridConn)
    use MemoryManagerModule, only: mem_allocate
    use Xt3dModule, only: xt3d_cr
    class(GwfInterfaceModelType), intent(inout) :: this
    class(GridConnectionType), pointer, intent(in) :: gridConn
    ! local
    integer(I4B) :: im
    class(NumericalModelType), pointer :: numMod
    
    this%gridConnection => gridConn
    numMod => this%gridConnection%model
    select type (numMod)
      class is (GwfModelType)
        this%gwfModel => numMod
      end select
    
    this%inewton = this%gwfModel%inewton
      
    ! create discretization and packages
    call this%buildDiscretization()
    call npf_cr(this%npf, this%name, this%innpf, this%iout) 
    call xt3d_cr(this%xt3d, this%name, this%innpf, this%iout)
    ! continue as in gwf_cr...
    
    ! set XT3D when the owning model has XT3D enabled
    this%npf%ixt3d = this%gwfModel%npf%ixt3d
    
  end subroutine createModel
  
  subroutine defineModel(this, satomega)
    class(GwfInterfaceModelType), intent(inout) :: this
    real(DP) :: satomega
    
    this%moffset = 0
    this%npf%satomega = satomega
    
    ! dis%dis_df, npf%npf_d ... we cannot call these yet, 
    ! the following comes from npf_df:
    this%npf%dis => this%dis
    this%npf%xt3d => this%xt3d    
    this%npf%xt3d%ixt3d = this%npf%ixt3d
    call this%npf%xt3d%xt3d_df(this%dis)
     
    this%neq = this%dis%nodes
    this%nja = this%dis%nja
    this%ia  => this%dis%con%ia
    this%ja  => this%dis%con%ja
    
    call this%allocate_arrays()
    
  end subroutine defineModel
  
  subroutine allocateAndReadModel(this)
    class(GwfInterfaceModelType), target, intent(inout) :: this
    
    this%npf%set_data_func => setNpfData  
    this%npf%func_caller => this
    call this%npf%npf_ar(this%ic, this%ibound, this%x)
    
  end subroutine allocateAndReadModel
  
  subroutine buildDiscretization(this)  
    use ConnectionsModule 
    use SparseModule, only: sparsematrix
    class(GwfInterfaceModelType), intent(inout) :: this
    ! local
    integer(I4B) :: icell, nrOfCells, idx
    integer(I4B) :: ierror
    type(NumericalModelType), pointer :: model
    type(ConnectionsType), pointer :: connections
    class(DisBaseType), pointer :: disbase
    class(GwfDisuType), pointer :: disu
    real(DP) :: x,y
    
    ! create disu
    call disu_cr(this%dis, this%name, -1, -1)
    
    ! the following is similar to dis_df, we should refactor this
    ! set nodes, nvertices skipped for as long as possible
    nrOfCells = this%gridConnection%nrOfCells    
    this%dis%nodes = nrOfCells
    this%dis%nodesuser = nrOfCells
    this%dis%nja = this%gridConnection%connections%nja
    call this%dis%allocate_arrays()
    
    ! fill data
    do icell = 1, nrOfCells
      
      idx = this%gridConnection%idxToGlobal(icell)%index
      model => this%gridConnection%idxToGlobal(icell)%model
      
      this%dis%top(icell) = model%dis%top(idx)
      this%dis%bot(icell) = model%dis%bot(idx)
      this%dis%area(icell) = model%dis%area(idx)
    end do
     
    ! grid connections follow from GridConnection:
    this%dis%con => this%gridConnection%connections
    this%dis%njas =  this%dis%con%njas
        
    disbase => this%dis
    select type(disbase)
    type is(GwfDisuType)
      disu => disbase
      
      ! copy cell x,y
      do icell = 1, nrOfCells
        idx = this%gridConnection%idxToGlobal(icell)%index
        model => this%gridConnection%idxToGlobal(icell)%model
        call model%dis%get_cellxy(idx, x, y)
        ! we need to have the origins in here explicitly since
        ! we are merging grids with possibly different origins
        ! TODO_MJR: how 'bout rotation?
        disu%cellxy(1,icell) = x + model%dis%xorigin
        disu%cellxy(2,icell) = y + model%dis%yorigin
      end do
    end select
    
  end subroutine buildDiscretization
  
  subroutine setNpfData(ifaceModelObj, npf)
    use MemoryManagerModule, only: mem_allocate
    class(GwfNpftype) :: npf
    class(*), pointer :: ifaceModelObj
    ! local
    type(GwfInterfaceModelType), pointer :: ifModel
    integer :: icell, idx, nrOfCells
    class(NumericalModelType), pointer :: numModel
    class(GwfModelType), pointer :: gwfModel
    
    ! cast to interface model here, should never fail
    ifModel => null()
    if (associated(ifaceModelObj)) then
      select type (ifaceModelObj)
      class is (GwfInterfaceModelType)
        ifModel => ifaceModelObj        
      end select
    endif      
    if (.not. associated(ifModel)) then
      return
    endif
    
    ! set the data in the npf package
    nrOfCells = ifModel%gridConnection%nrOfCells
    do icell=1, nrOfCells
      idx = ifModel%gridConnection%idxToGlobal(icell)%index
      numModel => ifModel%gridConnection%idxToGlobal(icell)%model
      ! cast for now...
      select type (numModel)
      class is (GwfModelType)
        gwfModel => numModel        
      end select
      
      ! copy celltype, K11
      npf%icelltype(icell) = gwfModel%npf%icelltype(idx) 
      npf%k11(icell) = gwfModel%npf%k11(idx) 
      
      ! do 'OR' on the K properties
      ! TODO_MJR: get this out of the loop!
      if (gwfModel%npf%ik22 > 0) npf%ik22 = 1
      if (gwfModel%npf%ik33 > 0) npf%ik33 = 1
      if (gwfModel%npf%iangle1 > 0) npf%iangle1 = 1
      if (gwfModel%npf%iangle2 > 0) npf%iangle2 = 1
      if (gwfModel%npf%iangle3 > 0) npf%iangle3 = 1
      if (gwfModel%npf%icalcspdis > 0) npf%icalcspdis = 1
    end do
            
    ! allocate when active
    if (npf%ik22 > 0) then
      call mem_allocate(npf%k22, nrOfCells, 'K22', trim(npf%origin))  
    end if
    if (npf%ik33 > 0) then
      call mem_allocate(npf%k33, nrOfCells, 'K33', trim(npf%origin))  
    end if
    if (npf%iangle1 > 0) then
      call mem_allocate(npf%angle1, nrOfCells, 'ANGLE1', trim(npf%origin))  
    end if
    if (npf%iangle2 > 0) then
      call mem_allocate(npf%angle2, nrOfCells, 'ANGLE2', trim(npf%origin))  
    end if
    if (npf%iangle3 > 0) then
      call mem_allocate(npf%angle3, nrOfCells, 'ANGLE3', trim(npf%origin))  
    end if
    
    ! and copy the remaining arrays
    do icell=1, nrOfCells
      idx = ifModel%gridConnection%idxToGlobal(icell)%index
      numModel => ifModel%gridConnection%idxToGlobal(icell)%model
      ! cast for now...
      select type (numModel)
      class is (GwfModelType)
        gwfModel => numModel        
      end select
      
      if (npf%ik22 > 0) npf%k22(icell) = gwfModel%npf%k22(idx)
      if (npf%ik33 > 0) npf%k33(icell) = gwfModel%npf%k33(idx)
      if (npf%iangle1 > 0) npf%angle1(icell) = gwfModel%npf%angle1(idx)
      if (npf%iangle2 > 0) npf%angle2(icell) = gwfModel%npf%angle2(idx)
      if (npf%iangle3 > 0) npf%angle3(icell) = gwfModel%npf%angle3(idx)
      
    end do
    
  end subroutine setNpfData
  
end module GwfInterfaceModelModule
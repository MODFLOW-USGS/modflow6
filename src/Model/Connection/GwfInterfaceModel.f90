module GwfInterfaceModelModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO
  use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList
  use GwfModule, only: GwfModelType
  use GridConnectionModule
  use BaseDisModule
  use GwfDisuModule
  use GwfNpfModule
  use GwfNpfOptionsModule
  use GwfNpfGridDataModule
  use GwfOcModule
  implicit none
  private
  
  !> The GWF Interface Model is a utility to calculate the solution's
  !! exchange coefficients from the interface between a GWF model and 
  !! its GWF neighbors. The interface model itself will not be part 
  !! of the solution, it is not being solved. 
  !! Patching (a part of the) discretizations of two GWF models in a
  !! general way, e.g. DIS+DIS with refinement, requires the resulting 
  !< discretization to be of type DISU.  
  type, public, extends(GwfModelType) :: GwfInterfaceModelType    
    class(GridConnectionType), pointer    :: gridConnection => null() !< The grid connection class will provide the connections
                                                                      !! object for the interface grid
    class(GwfModelType), private, pointer :: owner => null()          !< the real GWF model for which the exchange coefficients
                                                                      !! are calculated with this interface model
  contains
    procedure, pass(this) :: construct
    procedure, pass(this) :: createModel
    procedure, pass(this) :: defineModel
    procedure, pass(this) :: allocateAndReadModel

    ! override
    procedure :: model_da => deallocateModel

    ! private
    procedure, private, pass(this) :: buildDiscretization
    procedure, private, pass(this) :: setNpfOptions
    procedure, private, pass(this) :: setNpfGridData
  end type
 
contains
 
  !> Construction and minimal initialization
  !<
  subroutine construct(this, name, iout)
    use MemoryHelperModule, only: create_mem_path
    class(GwfInterfaceModelType), intent(inout) :: this !< the interface model
    character(len=*), intent(in)  :: name               !< the interface model's name
    integer(I4B), intent(in) :: iout                    !< the output unit, to be passed 
                                                        !! to the packages as well
    
    this%memoryPath = create_mem_path(name)
    call this%allocate_scalars(name)
    
    ! set default model options
    this%inewton = 0
    
    this%iout = iout    
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
    class(NumericalModelType), pointer :: numMod
    
    this%gridConnection => gridConn
    numMod => this%gridConnection%model
    select type (numMod)
      class is (GwfModelType)
        this%owner => numMod
      end select
    
    this%inewton = this%owner%inewton
    this%inewtonur = this%owner%inewtonur
      
    ! create discretization and packages
    call this%buildDiscretization()
    call npf_cr(this%npf, this%name, this%innpf, this%iout)
    call xt3d_cr(this%xt3d, this%name, this%innpf, this%iout)
    ! continue as in gwf_cr...
    
  end subroutine createModel
  
  subroutine defineModel(this, satomega)
    class(GwfInterfaceModelType), intent(inout) :: this
    real(DP) :: satomega
    ! local
    type(GwfNpfOptionsType) :: npfOptions

    this%moffset = 0
    this%npf%satomega = satomega

    ! define NPF package
    call npfOptions%construct()
    call this%setNpfOptions(npfOptions)
    call this%npf%npf_df(this%dis, this%xt3d, 0, npfOptions)
    call npfOptions%destroy()
    
    this%neq = this%dis%nodes
    this%nja = this%dis%nja
    this%ia  => this%dis%con%ia
    this%ja  => this%dis%con%ja
    
    call this%allocate_arrays()
    
  end subroutine defineModel
  
  subroutine allocateAndReadModel(this)
    class(GwfInterfaceModelType), target, intent(inout) :: this
    ! local
    type(GwfNpfGridDataType) :: npfGridData
    
    call npfGridData%construct(this%dis%nodes)
    call this%setNpfGridData(npfGridData)
    call this%npf%npf_ar(this%ic, this%ibound, this%x, npfGridData)
    call npfGridData%destroy()
    
  end subroutine allocateAndReadModel
  
  subroutine buildDiscretization(this)  
    use ConnectionsModule 
    use SparseModule, only: sparsematrix
    class(GwfInterfaceModelType), intent(inout) :: this
    ! local
    integer(I4B) :: icell, nrOfCells, idx
    type(NumericalModelType), pointer :: model
    class(DisBaseType), pointer :: disbase
    class(GwfDisuType), pointer :: disu
    real(DP) :: x,y
    
    ! create disu
    call disu_cr(this%dis, this%name, -1, this%iout)
    disbase => this%dis
    select type(disbase)
    type is(GwfDisuType)
      disu => disbase
    end select
          
    ! the following is similar to dis_df, we should refactor this
    ! set nodes, nvertices skipped for as long as possible
    nrOfCells = this%gridConnection%nrOfCells    
    this%dis%nodes = nrOfCells
    this%dis%nodesuser = nrOfCells
    this%dis%nja = this%gridConnection%connections%nja

    call this%dis%allocate_arrays()
    ! these are otherwise allocated in dis%read_dimensions    
    call disu%allocate_arrays_mem()
    
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

    ! TODO_MJR:
    ! create or copy vertices, can we avoid it somehow???
    ! if not, it will look like this:
    !
    ! 1. determine total nr. of verts
    ! 2. allocate vertices list
    ! 3. create sparse
    ! 4. get vertex data per cell, add functions to base
    ! 5. add vertex (x,y) to list and connectivity to sparse
    ! 6. generate ia/ja from sparse
    
  end subroutine buildDiscretization
  
  subroutine deallocateModel(this)
  use MemoryManagerModule, only: mem_deallocate
    class(GwfInterfaceModelType) :: this
    
    ! -- Internal flow packages deallocate
    call this%dis%dis_da()
    call this%npf%npf_da()
    call this%xt3d%xt3d_da()
    !
    ! -- Internal package objects
    deallocate(this%dis)
    deallocate(this%npf)
    deallocate(this%xt3d)
    !
    ! -- Scalars
    call mem_deallocate(this%inic)
    call mem_deallocate(this%inoc)
    call mem_deallocate(this%inobs)
    call mem_deallocate(this%innpf)
    call mem_deallocate(this%inbuy)
    call mem_deallocate(this%insto)
    call mem_deallocate(this%incsub)
    call mem_deallocate(this%inmvr)
    call mem_deallocate(this%inhfb)
    call mem_deallocate(this%ingnc)
    call mem_deallocate(this%iss)
    call mem_deallocate(this%inewtonur)
    !
    ! -- NumericalModelType
    call this%NumericalModelType%model_da()
    
  end subroutine
  
  subroutine setNpfOptions(this, npfOptions)
    class(GwfInterfaceModelType) :: this
    type(GwfNpfOptionsType) :: npfOptions

    ! for now, assuming full homogeneity, so just take
    ! the options from the owning model's npf package
    npfOptions%icellavg = this%owner%npf%icellavg
    npfOptions%ithickstrt = this%owner%npf%ithickstrt
    npfOptions%iperched = this%owner%npf%iperched
    npfOptions%ivarcv = this%owner%npf%ivarcv
    npfOptions%idewatcv = this%owner%npf%idewatcv
    npfOptions%irewet = this%owner%npf%irewet
    npfOptions%wetfct = this%owner%npf%wetfct
    npfOptions%iwetit = this%owner%npf%iwetit
    npfOptions%ihdwet = this%owner%npf%ihdwet
    npfOptions%ixt3d = this%owner%npf%ixt3d

  end subroutine setNpfOptions

  !> @brief Loop over the interface grid and fill the structure 
  !! with NPF grid data, copied from the models that participate 
  !! in this interface
  !<
  subroutine setNpfGridData(this, npfGridData)
    class(GwfInterfaceModelType) :: this    !< the interface model
    type(GwfNpfGridDataType) :: npfGridData !< grid data to be set
    ! local
    integer(I4B) :: icell, idx
    class(*), pointer :: objPtr
    class(GwfModelType), pointer :: gwfModel

    ! TODO_MJR: deal with inhomogeneity, for now, we assume
    ! that we can just take the owning model's settings...
    npfGridData%ik22 = this%owner%npf%ik22
    npfGridData%ik33 =  this%owner%npf%ik33
    npfGridData%iwetdry = this%owner%npf%iwetdry
    npfGridData%iangle1 = this%owner%npf%iangle1
    npfGridData%iangle2 = this%owner%npf%iangle2
    npfGridData%iangle3 = this%owner%npf%iangle3
    
    do icell = 1, this%gridConnection%nrOfCells
      idx = this%gridConnection%idxToGlobal(icell)%index
      objPtr => this%gridConnection%idxToGlobal(icell)%model
      gwfModel => CastAsGwfModelClass(objPtr)

      npfGridData%icelltype(icell) = gwfModel%npf%icelltype(idx)
      npfGridData%k11(icell) = gwfModel%npf%k11(idx)
      if (npfGridData%ik22 == 1) npfGridData%k22(icell) = gwfModel%npf%k22(idx)
      if (npfGridData%ik33 == 1) npfGridData%k33(icell) = gwfModel%npf%k33(idx)
      if (npfGridData%iwetdry == 1) npfGridData%wetdry(icell) = gwfModel%npf%wetdry(idx)
      if (npfGridData%iangle1 == 1) npfGridData%angle1(icell) = gwfModel%npf%angle1(idx)
      if (npfGridData%iangle2 == 1) npfGridData%angle2(icell) = gwfModel%npf%angle2(idx)
      if (npfGridData%iangle3 == 1) npfGridData%angle3(icell) = gwfModel%npf%angle3(idx)

    end do

  end subroutine setNpfGridData
  
  function CastAsGwfModelClass(obj) result (res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(GwfModelType), pointer :: res

    res => null()
    if (.not. associated(obj)) return

    select type (obj)
    class is (GwfModelType)
      res => obj
    end select
    return
  end function CastAsGwfModelClass
  
end module GwfInterfaceModelModule

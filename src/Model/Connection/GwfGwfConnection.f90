! TODO: module description
module GwfGwfConnectionModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO, DONE, DEM6, LENCOMPONENTNAME
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, mem_checkin  
  use SparseModule, only:sparsematrix
  use SpatialModelConnectionModule  
  use GwfInterfaceModelModule
  use NumericalModelModule
  use GwfModule, only: GwfModelType
  use GwfNpfModule, only: hcond, vcond
  use ConnectionsModule, only: ConnectionsType
  
  implicit none
  private

  ! Connecting two groundwaterflow models in space
  type, public, extends(SpatialModelConnectionType) :: GwfGwfConnectionType

    ! aggregation, the model with the connection:
    type(GwfModelType), pointer :: gwfModel => null()
    
    ! composition, the interface model
    type(GwfInterfaceModelType), pointer  :: interfaceModel => null()   
        
    ! memory managed data:
    integer(I4B), pointer :: iVarCV => null()   ! == 1: vertical conductance varies with water table
    integer(I4B), pointer :: iDewatCV => null() ! == 1: vertical conductance accounts for dewatered portion of underlying cell
    real(DP), pointer     :: satOmega => null() !
    integer(I4B), pointer :: iCellAvg => null() ! TODO_MJR: discuss this, iCellAvg same value per connection, user can now specify per exchange?
    
  contains 
    procedure, pass(this) :: gwfGwfConnection_ctor
    generic, public :: construct => gwfGwfConnection_ctor
    
    ! implement (abstract) virtual
    procedure, pass(this) :: mc_ar => gwfgwfcon_ar
    procedure, pass(this) :: mc_df => gwfgwfcon_df 
    procedure, pass(this) :: mc_ac => gwfgwfcon_ac
    procedure, pass(this) :: mc_cf => gwfgwfcon_cf
    procedure, pass(this) :: mc_fc => gwfgwfcon_fc
    procedure, pass(this) :: mc_da => gwfgwfcon_da
    
    ! local stuff
    procedure, pass(this), private :: allocateScalars
    procedure, pass(this), private :: createCoefficientMatrix
    procedure, pass(this), private :: maskConnections
    procedure, pass(this), private :: syncInterfaceModel
    
  end type GwfGwfConnectionType

contains
  
   subroutine gwfGwfConnection_ctor(this, model)
    use NumericalModelModule, only: NumericalModelType
    class(GwfGwfConnectionType), intent(inout)  :: this
    class(NumericalModelType), pointer          :: model ! note: this must be a GwfModelType
    ! local
    character(len=LENCOMPONENTNAME) :: name
    integer(I4B), save :: iconn = 1 ! static counter to ensure unique name

    this%gwfModel => CastToGwfModel(model)
        
    if(len(trim(model%name)) + 4 > LENCOMPONENTNAME) then
      ! this will give problems storing in memory manager, fix here
      write(name,'(a,i4.4)') 'G2C', iconn
      iconn = iconn + 1
    else
      name = trim(model%name)//'_G2C'
    end if
    
    ! first call base constructor
    call this%SpatialModelConnectionType%spatialConnection_ctor(model, name)
    
    call this%allocateScalars()
    
    this%connectionType = 'GWF-GWF'
    this%iVarCV = 0
    this%iDewatCV = 0
    this%satOmega = DZERO  
    this%iCellAvg = 0
    this%iNewton = this%gwfModel%inewton
    
    allocate(this%interfaceModel)
  
  end subroutine gwfGwfConnection_ctor
      
  subroutine gwfgwfcon_df(this)
    class(GwfGwfConnectionType), intent(inout) :: this    
    ! local
    type(sparsematrix) :: sparse
    
    if (this%gwfModel%npf%ixt3d > 0) then
      this%stencilDepth = 2
    end if    
    
    this%satOmega = this%gwfModel%npf%satomega
    
    
    ! now call base class, this sets up the GridConnection
    call this%spatialcon_df()    
    
    ! grid conn is defined, so we first create the interface model
    ! here, and the remainder of this routine is define.
    ! we basically follow the logic that is present in sln_df()
    call this%interfaceModel%construct(this%name)
    call this%interfaceModel%createModel(this%gridConnection)
    
    ! define, from here
    call this%interfaceModel%defineModel(this%satOmega)
     
    ! point x, ibound, and rhs to connection
    this%interfaceModel%x => this%x
    call mem_checkin(this%interfaceModel%x, 'X', this%interfaceModel%memoryPath, 'X', this%memoryPath)
    this%interfaceModel%rhs => this%rhs
    call mem_checkin(this%interfaceModel%rhs, 'RHS', this%interfaceModel%memoryPath, 'RHS', this%memoryPath)
    this%interfaceModel%ibound => this%iactive
    call mem_checkin(this%interfaceModel%ibound, 'IBOUND', this%interfaceModel%memoryPath, 'IBOUND', this%memoryPath)
    
    ! assign connections, fill ia/ja, map connections (following sln_connect) and mask
    call sparse%init(this%neq, this%neq, 7)
    call this%interfaceModel%model_ac(sparse)
    
    ! create amat from sparse
    call this%createCoefficientMatrix(sparse)
    call sparse%destroy()
    
    ! map connections
    call this%interfaceModel%model_mc(this%ia, this%ja)  
      
    ! mask
    call this%maskConnections()
    
  end subroutine gwfgwfcon_df
  
  subroutine createCoefficientMatrix(this, sparse)
    use SimModule, only: ustop
    class(GwfGwfConnectionType), intent(inout) :: this
    type(sparsematrix) :: sparse
    ! local
    integer(I4B) :: ierror
        
    this%nja = sparse%nnz
    call mem_allocate(this%ia, this%neq + 1, 'IA', this%memoryPath)
    call mem_allocate(this%ja, this%nja, 'JA', this%memoryPath)
    call mem_allocate(this%amat, this%nja, 'AMAT', this%memoryPath)    
    call sparse%sort()
    call sparse%filliaja(this%ia, this%ja, ierror)
    if (ierror /= 0) then
      write(*,*) 'Error: cannot fill ia/ja for model connection'
      call ustop()
    end if
    
  end subroutine createCoefficientMatrix
  
  subroutine maskConnections(this)
    use SimModule, only: ustop
    use CsrUtilsModule, only: getCSRIndex
    class(GwfGwfConnectionType), intent(inout) :: this
    ! local
    integer(I4B) :: ipos, n, m, nloc, mloc, csrIdx
    type(ConnectionsType), pointer :: conn
    
    ! set the mask on connections that are calculated by the interface model
    conn => this%interfaceModel%dis%con
    do n = 1, conn%nodes
      ! only for connections internal to the owning model
      if (.not. associated(this%gridConnection%idxToGlobal(n)%model, this%owner)) then
        cycle
      end if      
      nloc = this%gridConnection%idxToGlobal(n)%index
      
      do ipos = conn%ia(n) + 1, conn%ia(n + 1) - 1
        m = conn%ja(ipos)
        if (.not. associated(this%gridConnection%idxToGlobal(m)%model, this%owner)) then
            cycle
        end if
        mloc = this%gridConnection%idxToGlobal(m)%index
        
        if (conn%mask(ipos) > 0) then
          ! calculated by interface model, set local model's mask to zero
          csrIdx = getCSRIndex(nloc, mloc, this%owner%ia, this%owner%ja)          
          if (csrIdx == -1) then
            ! this can only happen with periodic boundary conditions, 
            ! then there is no need to set the mask
            if (this%gridConnection%isPeriodic(nloc, mloc)) cycle
            
            write(*,*) 'Error: cannot find cell connection in global system'
            call ustop()
          end if
          
          call this%owner%dis%con%set_mask(csrIdx, 0)          
        end if
      end do
    end do
    
  end subroutine maskConnections
  
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(GwfGwfConnectionType), intent(inout)  :: this
    ! local

    call mem_allocate(this%iVarCV, 'IVARCV', this%memoryPath)
    call mem_allocate(this%iDewatCV, 'IDEWATCV', this%memoryPath)
    call mem_allocate(this%satOmega, 'SATOMEGA', this%memoryPath)
    call mem_allocate(this%iCellAvg, 'ICELLAVG', this%memoryPath)
    
  end subroutine allocateScalars
  
  ! We can only call this after the *_df is finished, e.g. it is not until
  ! GwfGwfExchange%gwf_gwf_df(...) that the exchange data is being read from file
  ! So, in this routine, we have to do create, define, and allocate&read 
  subroutine gwfgwfcon_ar(this)
  use GridConnectionModule, only: GridConnectionType
    class(GwfGwfConnectionType), intent(inout)  :: this
    ! local    
    integer(I4B) :: icell, idx, localIdx
    class(NumericalModelType), pointer :: model
    type(GridConnectionType), pointer :: gc
    
    gc => this%gridConnection
    ! init x and ibound with model data
    do icell = 1, gc%nrOfCells     
      idx = gc%idxToGlobal(icell)%index
      model => gc%idxToGlobal(icell)%model
      
      this%interfaceModel%x(icell) = model%x(idx)
      this%interfaceModel%ibound(icell) = model%ibound(idx)      
    end do
    
    ! *_ar
    call this%interfaceModel%allocateAndReadModel()  

    ! fill mapping to global coordinate index, which can be
    ! done now because moffset is set in sln_df
    do localIdx = 1, gc%nrOfCells
      gc%idxToGlobalIdx(localIdx) = gc%idxToGlobal(localIdx)%index +        &
                                    gc%idxToGlobal(localIdx)%model%moffset
    end do

    ! TODO_MJR: ar mover
    ! TODO_MJR: angledx checks    
    ! TODO_MJR: ar observation
    
  end subroutine gwfgwfcon_ar
     
  subroutine gwfgwfcon_ac(this, sparse)        
    class(GwfGwfConnectionType), intent(inout) :: this
    type(sparsematrix), intent(inout) :: sparse 
    ! local
    integer(I4B) :: n, m, ipos
    integer(I4B) :: nglo, mglo
    
    do n = 1, this%neq
      if (.not. associated(this%gridConnection%idxToGlobal(n)%model, this%owner)) then
        ! only add connections for own model to global matrix
        cycle
      end if
      nglo = this%gridConnection%idxToGlobal(n)%index + this%gridConnection%idxToGlobal(n)%model%moffset
      do ipos = this%ia(n) + 1, this%ia(n+1) - 1
        m = this%ja(ipos)
        mglo = this%gridConnection%idxToGlobal(m)%index + this%gridConnection%idxToGlobal(m)%model%moffset
        
        call sparse%addconnection(nglo, mglo, 1)
      end do
    end do
    
  end subroutine gwfgwfcon_ac
    
  ! calculate or adjust matrix coefficients which are affected
  ! by the connection of GWF models
  subroutine gwfgwfcon_cf(this, kiter)
    class(GwfGwfConnectionType), intent(inout)  :: this
    integer(I4B), intent(in) :: kiter
    ! local
    integer(I4B) :: i
    
    ! reset interface system
    do i = 1, this%nja
      this%amat(i) = 0.0_DP
    end do
    do i = 1, this%neq
      this%rhs(i) = 0.0_DP
    end do
    
    ! copy model data into interface model
    call this%syncInterfaceModel()
    
    ! calculate (wetting/drying, saturation)
    call this%interfaceModel%model_cf(kiter)
    
  end subroutine gwfgwfcon_cf
  
  subroutine syncInterfaceModel(this)
    class(GwfGwfConnectionType), intent(inout) :: this
    integer(I4B) :: icell, idx
    class(NumericalModelType), pointer :: model
    
    ! copy head values
    do icell = 1, this%gridConnection%nrOfCells      
      idx = this%gridConnection%idxToGlobal(icell)%index
      model => this%gridConnection%idxToGlobal(icell)%model
      
      this%x(icell) = model%x(idx)
    end do
  
  end subroutine syncInterfaceModel
  
  ! write the calculated conductances into the global system matrix
  subroutine gwfgwfcon_fc(this, kiter, amatsln, njasln, rhssln, inwtflag)    
    class(GwfGwfConnectionType), intent(inout) :: this
    integer(I4B), intent(in) :: kiter
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B),intent(in) :: njasln
    real(DP), dimension(:), intent(inout) ::rhssln
    integer(I4B), intent(in) :: inwtflag
    ! local
    integer(I4B) :: n, ipos, nglo
    
    ! fill (and add to...) coefficients for interface
    call this%interfaceModel%model_fc(kiter, this%amat, this%nja, inwtflag)
    
    ! map back to solution matrix
    do n = 1, this%neq
      ! we cannot check with the mask here, because cross-terms are not
      ! necessarily from primary connections. But, we only need the coefficients
      ! for our own model (i.e. fluxes into cells belonging to this%owner):
      if (.not. associated(this%gridConnection%idxToGlobal(n)%model, this%owner)) then
        ! only add connections for own model to global matrix
        cycle
      end if
      
      nglo = this%gridConnection%idxToGlobal(n)%index + this%gridConnection%idxToGlobal(n)%model%moffset
      rhssln(nglo) = rhssln(nglo) + this%rhs(n)
      
      do ipos = this%ia(n), this%ia(n+1) - 1
        amatsln(this%mapIdxToSln(ipos)) = amatsln(this%mapIdxToSln(ipos)) + this%amat(ipos)
      end do
    end do    
  end subroutine gwfgwfcon_fc

  ! deallocate resources
  subroutine gwfgwfcon_da(this)    
    class(GwfGwfConnectionType), intent(inout) :: this

    call mem_deallocate(this%iVarCV)
    call mem_deallocate(this%iDewatCV)
    call mem_deallocate(this%satOmega)
    call mem_deallocate(this%iCellAvg)

    call mem_deallocate(this%ia)
    call mem_deallocate(this%ja)
    call mem_deallocate(this%amat)
    
    call this%interfaceModel%model_da()
    deallocate(this%interfaceModel)
    
    call this%spatialcon_da()
    
  end subroutine gwfgwfcon_da
  
  ! unsafe routine, you have to know what you're doing with this
  function CastToGwfModel(obj) result(gwfmodel)
    use NumericalModelModule, only: NumericalModelType
    class(NumericalModelType), pointer :: obj
    class(GwfModelType), pointer :: gwfmodel
    
    gwfmodel => null()
    select type(obj)
      type is (GwfModelType)
        gwfmodel => obj
      end select
      
  end function CastToGwfModel
  
end module GwfGwfConnectionModule

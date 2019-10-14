! TODO: module description
module GwfGwfConnectionModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO, DONE, DEM6
  use MemoryManagerModule, only: mem_allocate
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
    integer(I4B), pointer             :: iVarCV => null()   ! == 1: vertical conductance varies with water table
    integer(I4B), pointer             :: iDewatCV => null() ! == 1: vertical conductance accounts for dewatered portion of underlying cell
    real(DP), pointer                 :: satOmega => null() !
    integer(I4B), pointer             :: iCellAvg => null() ! TODO_MJR: discuss this, iCellAvg same value per connection, user can now specify per exchange?
    
  contains 
    procedure, pass(this) :: gwfGwfConnection_ctor
	  generic, public :: construct => gwfGwfConnection_ctor
    
    ! implement (abstract) virtual
    procedure, pass(this) :: mc_ar => gwfgwfcon_ar
    procedure, pass(this) :: mc_df => gwfgwfcon_df 
    procedure, pass(this) :: mc_ac => gwfgwfcon_ac
    procedure, pass(this) :: mc_cf => gwfgwfcon_cf
    procedure, pass(this) :: mc_fc => gwfgwfcon_fc
    
    ! local stuff
    procedure, pass(this), private :: allocateScalars
  end type GwfGwfConnectionType

contains
  
   subroutine gwfGwfConnection_ctor(this, model)
    use NumericalModelModule, only: NumericalModelType
    class(GwfGwfConnectionType), intent(inout)  :: this
    class(NumericalModelType), pointer          :: model ! note: this must be a GwfModelType
    ! local
    
    this%gwfModel => CastToGwfModel(model)
    
    ! first call base constructor
    call this%construct(model, trim(model%name)//'_GWF2CONN')
    
    call this%allocateScalars()
    
    this%connectionType = 'GWF-GWF'
    this%iVarCV = 0
    this%iDewatCV = 0
    this%satOmega = DEM6  
    this%iCellAvg = 0
    
    allocate(this%interfaceModel)
  
  end subroutine gwfGwfConnection_ctor
   
   
  subroutine gwfgwfcon_df(this)
    use SpatialModelConnectionModule, only: getCSRIndex ! TODO_MJR: move that function
    use SimModule, only: ustop
    class(GwfGwfConnectionType), intent(inout) :: this    
    ! local
    integer(I4B) :: ierror, ipos, n, m, nloc, mloc, csrIdx
    type(sparsematrix) :: sparse
    type(ConnectionsType), pointer :: conn
    
    if (this%gwfModel%npf%ixt3d > 0) then
      this%stencilDepth = 2
    end if
    
    ! now call base class, this sets up the GridConnection
    call this%spatialcon_df()    
    
    ! grid conn is defined, so we can now create the interface model
    ! and do the define part here, c.f. what happens in sln_df()
    
    ! == create ==
    call this%interfaceModel%construct(this%name)
    call this%interfaceModel%createModel(this%gridConnection)
    
    
    ! == define ==
    call this%interfaceModel%defineModel()
    
    ! -- calculate and set offsets 
    this%interfaceModel%moffset = 0
    
    ! -- Allocate and initialize solution arrays in parent class    
    ! -- Go through each model and point x, ibound, and rhs to solution
    this%interfaceModel%x => this%x
    this%interfaceModel%rhs => this%rhs
    this%interfaceModel%ibound => this%iactive
    
    ! -- Create the sparsematrix instance
    call sparse%init(this%neq, this%neq, 7)
    
    ! -- Assign connections, fill ia/ja, map connections (following sln_connect) and mask
    call this%interfaceModel%model_ac(sparse)
    
    ! create amat from sparse (and keep sparse for adding connections to global system)
    this%nja = sparse%nnz    
    call mem_allocate(this%ia, this%neq + 1, 'IA', this%memoryOrigin)
    call mem_allocate(this%ja, this%nja, 'JA', this%memoryOrigin)
    call mem_allocate(this%amat, this%nja, 'AMAT', this%memoryOrigin)    
    call sparse%sort()
    call sparse%filliaja(this%ia, this%ja, ierror)
    call sparse%destroy()
    
    ! map connections
    call this%interfaceModel%model_mc(this%ia, this%ja)    
    
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
            ! this should not be possible
            write(*,*) 'Error: cannot find cell connection in global system'
            call ustop()
          end if
          
          call this%owner%dis%con%set_mask(csrIdx, 0)          
        end if
      end do
    end do
      
  end subroutine gwfgwfcon_df

  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(GwfGwfConnectionType), intent(inout)  :: this
    ! local

    call mem_allocate(this%iVarCV, 'IVARCV', this%memoryOrigin)
    call mem_allocate(this%iDewatCV, 'IDEWATCV', this%memoryOrigin)
    call mem_allocate(this%satOmega, 'SATOMEGA', this%memoryOrigin)
    call mem_allocate(this%iCellAvg, 'ICELLAVG', this%memoryOrigin)    
    
  end subroutine allocateScalars
  
  ! We can only call this after the *_df is finished, e.g. it is not until
  ! GwfGwfExchange%gwf_gwf_df(...) that the exchange data is being read from file
  ! So, in this routine, we have to do create, define, and allocate&read 
  subroutine gwfgwfcon_ar(this)
    class(GwfGwfConnectionType), intent(inout)  :: this
    ! local    
   
    call this%interfaceModel%allocateAndReadModel()    
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
     
    ! copy model data into interface model
    call this%interfaceModel%syncModelData()
    
    ! calculate (wetting/drying, saturation)
    call this%interfaceModel%model_cf(kiter)
    
  end subroutine gwfgwfcon_cf
    
  ! write the calculated conductances into the global system matrix
  subroutine gwfgwfcon_fc(this, kiter, amatsln, njasln, inwtflag)    
    class(GwfGwfConnectionType), intent(inout) :: this
    integer(I4B), intent(in) :: kiter
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B),intent(in) :: njasln
    integer(I4B), intent(in) :: inwtflag
    ! local
    integer(I4B) :: i, n, m, ipos
    
    ! we iterate, so this should be reset (c.f. sln_reset())
    do i = 1, this%nja
      this%amat(i) = 0.0_DP
    end do
    do i = 1, this%neq
      this%rhs(i) = 0.0_DP
    end do
    
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
      do ipos = this%ia(n), this%ia(n+1) - 1
        m = this%ja(ipos)
        write(*,*) n, ' - ', m, ': ', this%amat(ipos)
        amatsln(this%mapIdxToSln(ipos)) = amatsln(this%mapIdxToSln(ipos)) + this%amat(ipos)
      end do
    end do
    write(*,*)    
  end subroutine gwfgwfcon_fc
  
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

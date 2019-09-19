! TODO: module description
module GwfGwfConnectionModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO, DONE, DEM6
  use MemoryManagerModule, only: mem_allocate
  use SpatialModelConnectionModule  
  use GwfInterfaceModelModule
  use NumericalModelModule
  use GwfModule, only: GwfModelType
  use GwfNpfModule, only: hcond, vcond
  
  implicit none
  private

  ! Connecting two groundwaterflow models in space
  type, public, extends(SpatialModelConnectionType) :: GwfGwfConnectionType
    
    ! aggregation, the model with the connection:
    type(GwfModelType), pointer :: gwfModel => null()
    
    ! composition, the interface model
    type(GwfInterfaceModelType), pointer  :: interfaceModel => null()   
    
    ! the interface model doesn't live in a solution, so we need these
    ! TODO_MJR: probably move to parent class
    real(DP), dimension(:), pointer       :: amat
    integer(I4B)                          :: nja
    
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
    procedure, pass(this) :: mc_cf => gwfgwfcon_cf
    procedure, pass(this) :: mc_fc => gwfgwfcon_fc
    
    ! local stuff
    procedure, pass(this), private :: allocateScalars
    procedure, pass(this), private :: allocateArrays
  end type GwfGwfConnectionType

contains

  subroutine gwfGwfConnection_ctor(this, model)
    use NumericalModelModule, only: NumericalModelType
    class(GwfGwfConnectionType), intent(inout)  :: this
    class(NumericalModelType), pointer          :: model ! note: this must be a GwfModelType
    
    ! first call base constructor
    call this%construct(model, trim(model%name)//'_GWF2CONN') ! 
    
    call this%allocateScalars()
    call this%allocateArrays()
    
    this%connectionType = 'GWF-GWF'
    this%gwfModel => CastToGwfModel(model)
    
    this%iVarCV = 0
    this%iDewatCV = 0
    this%satOmega = DEM6  
    this%iCellAvg = 0
    
    allocate(this%interfaceModel)
    
  end subroutine gwfGwfConnection_ctor
  
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: LENORIGIN
    class(GwfGwfConnectionType), intent(inout)  :: this
    ! local

    call mem_allocate(this%iVarCV, 'IVARCV', this%memoryOrigin)
    call mem_allocate(this%iDewatCV, 'IDEWATCV', this%memoryOrigin)
    call mem_allocate(this%satOmega, 'SATOMEGA', this%memoryOrigin)
    call mem_allocate(this%iCellAvg, 'ICELLAVG', this%memoryOrigin)
    
  end subroutine allocateScalars
  
  subroutine allocateArrays(this)
    class(GwfGwfConnectionType), intent(inout)  :: this
    
  end subroutine allocateArrays
  
  subroutine gwfgwfcon_ar(this)
    class(GwfGwfConnectionType), intent(inout)  :: this
    
    ! construct the interface model here after *_df has been finished
    call this%interfaceModel%construct(this%name)    
    ! following is create, define, and allocate/read
    call this%interfaceModel%createModel(this%gridConnection)
    
    ! TODO_MJR: ar mover
    ! TODO_MJR: angledx checks    
    ! TODO_MJR: ar observation    
    
    ! create amat for interface
    this%nja = this%interfaceModel%dis%con%nja
    allocate(this%amat(this%nja))    
    
  end subroutine gwfgwfcon_ar
  
  ! calculate or adjust matrix coefficients which are affected
  ! by the connection of GWF models
  subroutine gwfgwfcon_cf(this, kiter)
    class(GwfGwfConnectionType), intent(inout)  :: this
    integer(I4B), intent(in) :: kiter
    ! local
     
    ! copy model data into interface model
    call this%interfaceModel%syncModelData()
    
    ! calculate
    call this%interfaceModel%model_cf(kiter)
    
  end subroutine gwfgwfcon_cf
    
  ! write the calculated conductances into the global system matrix
  subroutine gwfgwfcon_fc(this, kiter, amatsln, njasln, inwtflag)
    use ConnectionsModule, only: ConnectionsType
    class(GwfGwfConnectionType), intent(inout) :: this
    integer(I4B), intent(in) :: kiter
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B),intent(in) :: njasln
    integer(I4B), intent(in) :: inwtflag
    ! local
    type(ConnectionsType), pointer :: conn => null()
    integer(I4B) :: mloc, nloc, j
    integer(I4B) :: mglob, nglob
    real(DP) :: conductance
    
    ! we iterate, so this should be reset (c.f. sln_reset())
    this%amat = 0
    
    ! fill (and add to...) coefficients for interface
    call this%interfaceModel%model_fc(kiter, this%amat, this%nja, inwtflag)
    
    ! map back to solution matrix
    conn => this%interfaceModel%dis%con
    do mloc=1, conn%nodes
      if (.not. associated(this%gridConnection%idxToGlobal(mloc)%model, this%gwfModel)) then
        ! only write coefficients for own model to global matrix, cycle otherwise
        cycle
      end if
      ! copy into global matrix
      do j=conn%ia(mloc), conn%ia(mloc+1)-1
        amatsln(this%mapIdxToSln(j)) = amatsln(this%mapIdxToSln(j)) + this%amat(j)
      end do
    end do      
        
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

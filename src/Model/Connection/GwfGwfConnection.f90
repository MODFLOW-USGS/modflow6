! TODO: module description
module GwfGwfConnectionModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO, DONE, DEM6
  use MemoryManagerModule, only: mem_allocate
  use SpatialModelConnectionModule
  use GwfModule, only: GwfModelType
  
  implicit none
  private

  ! Connecting two groundwaterflow models in space
  type, public, extends(SpatialModelConnectionType) :: GwfGwfConnectionType
    
    ! aggregation:
    type(GwfModelType), pointer :: gwfModel => null()
    
    ! memory managed data:
    real(DP), pointer                 :: satOmega => null() !
    integer(I4B), pointer             :: iCellAvg => null() ! TODO_MJR: discuss this, iCellAvg same value per connection, user could specify per exchange...
    real(DP), dimension(:), pointer   :: satConductance => null()
    
  contains 
    procedure, pass(this) :: gwfGwfConnection_ctor
	  generic, public :: construct => gwfGwfConnection_ctor
    
    ! implement (abstract) virtual
    procedure, pass(this) :: mc_ar => allocateRead
    procedure, pass(this) :: mc_cf => calculateCoefficients
    
    ! local stuff
    procedure, pass(this), private :: allocateScalars
    procedure, pass(this), private :: allocateArrays
    procedure, pass(this), private :: calculateSaturatedConductance
  end type GwfGwfConnectionType

contains

  subroutine gwfGwfConnection_ctor(this, model)
    use NumericalModelModule, only: NumericalModelType
    class(GwfGwfConnectionType), intent(inout)  :: this
    class(NumericalModelType), pointer          :: model ! note: this must be a GwfModelType
    
    ! first call base constructor
    call this%construct(model, model%name//'_GWFGWF_CONN') ! 
    
    call this%allocateScalars()
    call this%allocateArrays()
    
    ! construct GWF part here
    select type(model)
      type is(GwfModelType)
        this%gwfModel => model
      end select
    
    
      
    this%satOmega = DEM6  
    this%iCellAvg = 0
    
  end subroutine gwfGwfConnection_ctor
  
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: LENORIGIN
    class(GwfGwfConnectionType), intent(inout)  :: this
    ! local

    call mem_allocate(this%satOmega, 'SATOMEGA', this%memoryOrigin)
    call mem_allocate(this%iCellAvg, 'ICELLAVG', this%memoryOrigin)
    
  end subroutine allocateScalars
  
  subroutine allocateArrays(this)
    class(GwfGwfConnectionType), intent(inout)  :: this
    
  end subroutine allocateArrays
  
  subroutine allocateRead(this)
    class(GwfGwfConnectionType), intent(inout)  :: this
    
    ! TODO_MJR: ar mover
    ! TODO_MJR: angledx checks
    
    call this%calculateSaturatedConductance()
    
    ! TODO_MJR: ar observation    
    
  end subroutine allocateRead
  
  
  subroutine calculateSaturatedConductance(this)
    use GwfNpfModule, only: hcond, vcond
  
    class(GwfGwfConnectionType), intent(inout)  :: this
    ! local
    type(GwfModelType), pointer :: model2
    integer(I4B) :: i, nLinks
    
    integer(I4B) :: n, m
    real(DP) :: topn, topm, botn, botm, satn, satm
    integer(I4B) :: ihc                             ! == 1 for horizontally connected, == 0 for vertical
    real(DP) :: hWidth, vArea
    real(DP) :: cl1, cl2 ! connection lengths 
    
    real(DP) :: hyn, hym
    real(DP) :: k11n, k11m
    real(DP), dimension(3) :: vg
    
    ! loop over connected nodes
    ! TODO_MJR: consider performance
    nLinks = this%gridConnection%nrOfLinks
    
    ! we didn't know size by construction yet, so we allocate here. TODO_MJR: ok?
    call mem_allocate(this%satConductance, nLinks, 'CONDSAT', this%memoryOrigin) 
    
    do i=1, nLinks
      n = this%gridConnection%linkedNodes(i)%ownIndex
      m = this%gridConnection%linkedNodes(i)%linkedIndex
      ihc = this%gridConnection%linkedNodes(i)%connectionType
      
      ! We know this is a gwf model:
      ! (TODO_MJR: maybe replace model pointer by index in model list?)
      model2 => CastToGwfModel(this%gridConnection%linkedNodes(i)%connectedModel)      
      
      ! properties owner, through gwfModel pointer
      topn = this%gwfModel%dis%top(n)
      botn = this%gwfModel%dis%bot(n)
      satn = this%gwfModel%npf%sat(n)
      
      ! properties connected model    
      topm = model2%dis%top(m)      
      botm = model2%dis%bot(m)      
      satm = model2%npf%sat(m)
      
      if (ihc == 0) then ! vertical connection
        ! calculate effective hyd. cond.
        vg(1) = DZERO
        vg(2) = DZERO
        vg(3) = DONE
        hyn = this%gwfModel%npf%hy_eff(n, 0, ihc, vg=vg)
        hym = model2%npf%hy_eff(m, 0, ihc, vg=vg)        
        vArea = this%gridConnection%linkedNodes(i)%hwva
        
        ! vertical conductance calculation       
        this%satConductance(i) = vcond(1, 1, 1, 1, 0, 1, 1, DONE,        &
                        botn, botm,                                      &
                        hyn, hym,                                        &
                        satn, satm,                                      &
                        topn, topm,                                      &
                        botn, botm,                                      &
                        vArea)
      
      else ! horizontal connection TODO_MJR: how about ihc == 2??    
        
        k11n = this%gwfModel%npf%k11(n)
        k11m = model2%npf%k11(m)
        hWidth = this%gridConnection%linkedNodes(i)%hwva
        cl1 = this%gridConnection%linkedNodes(i)%length1
        cl2 = this%gridConnection%linkedNodes(i)%length2
        
        ! TODO_MJR: check here for anisotropy and if, recalculate k11n,k11m
        
        ! horizontal conductance calculation
        this%satConductance(i) = hcond(1, 1, 1, 1, this%inewton, 0, ihc,        &
                        this%icellavg, 0, 0, DONE,                              &
                        topn, topm, satn, satm, k11n, k11m,                     &
                        topn, topm,                                             &
                        botn, botm,                                             &
                        cl1, cl2,                                               &
                        hWidth, this%satOmega)
        
      end if
      
    end do
    
    
  end subroutine calculateSaturatedConductance
  
  ! calculate or adjust matrix coefficients which are affected
  ! by the connection of GWF models
  subroutine calculateCoefficients(this, kiter)
    class(GwfGwfConnectionType), intent(inout)  :: this
    integer(I4B), intent(in) :: kiter
    
    
    
  end subroutine calculateCoefficients
  
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

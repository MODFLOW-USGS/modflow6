module GwtInterfaceModelModule
  use KindModule, only: I4B
  use NumericalModelModule, only: NumericalModelType
  use GwtModule, only: GwtModelType
  use GwtAdvModule, only: adv_cr, GwtAdvType
  use GwtAdvOptionsModule, only: GwtAdvOptionsType
  use GridConnectionModule

  implicit none
  private

  !> The GWT Interface Model is a utility to calculate the solution's
  !! exchange coefficients from the interface between a GWT model and 
  !! its GWT neighbors. The interface model itself will not be part 
  !! of the solution, it is not being solved. 
  type, public, extends(GwtModelType) :: GwtInterfaceModelType
  
    class(GwtModelType), private, pointer :: owner => null() !< the real GWT model for which the exchange coefficients
                                                             !! are calculated with this interface model
  contains
    procedure, pass(this) :: construct
    procedure, pass(this) :: createModel
    ! override
    procedure :: model_da => deallocateModel
  end type GwtInterfaceModelType

contains

!> @brief Construction and minimal initialization
!<
subroutine construct(this, name, iout)
  use MemoryHelperModule, only: create_mem_path
  class(GwtInterfaceModelType), intent(inout) :: this !< the GWT interface model
  character(len=*), intent(in)  :: name               !< the interface model's name
  integer(I4B), intent(in) :: iout                    !< the output unit, to be passed 
                                                      !! to the packages as well
  
  this%memoryPath = create_mem_path(name)
  call this%allocate_scalars(name)

  this%iout = iout
  
end subroutine construct
 
!> @brief Set up the interface model, analogously to what 
!< happens in gwt_cr
subroutine createModel(this, gridConn)
  use MemoryManagerModule, only: mem_allocate
  use Xt3dModule, only: xt3d_cr
  class(GwtInterfaceModelType), intent(inout) :: this        !< the GWT interface model
  class(GridConnectionType), pointer, intent(in) :: gridConn !< the grid connection data for creating a DISU
  ! local
  class(NumericalModelType), pointer :: numMod

  numMod => gridConn%model
  select type (numMod)
  class is (GwtModelType)
    this%owner => numMod
  end select
   
  ! create disu for interface
  
  ! create packages
  ! TODO_MJR: fmi
  call adv_cr(this%adv, this%name, -1, this%iout, this%fmi)
    
end subroutine createModel

subroutine deallocateModel(this)
  class(GwtInterfaceModelType) :: this !< the GWT interface model

  ! dealloc base
  call this%model_da()

end subroutine deallocateModel


end module GwtInterfaceModelModule
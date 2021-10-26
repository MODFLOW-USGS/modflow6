module GwtInterfaceModelModule
  use KindModule, only: I4B  
  use MemoryManagerModule, only: mem_allocate
  use MemoryHelperModule, only: create_mem_path
  use NumericalModelModule, only: NumericalModelType
  use GwtModule, only: GwtModelType, CastAsGwtModel
  use GwfDisuModule, only: disu_cr, CastAsDisuType
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
    class(GridConnectionType), pointer    :: gridConnection => null() !< The grid connection class will provide the interface grid
    class(GwtModelType), private, pointer :: owner => null()          !< the real GWT model for which the exchange coefficients
                                                                      !! are calculated with this interface model
  contains
    procedure, pass(this) :: gwtifmod_cr
    procedure, pass(this) :: model_df => gwtifmod_df
    procedure, pass(this) :: model_ar => gwtifmod_ar
    procedure, pass(this) :: model_da => gwtifmod_da
  end type GwtInterfaceModelType

contains

!> @brief Create the interface model, analogously to what 
!< happens in gwt_cr
subroutine gwtifmod_cr(this, name, iout, gridConn)
  class(GwtInterfaceModelType), intent(inout) :: this        !< the GWT interface model
  character(len=*), intent(in)  :: name                      !< the interface model's name
  integer(I4B), intent(in) :: iout                           !< the output unit
  class(GridConnectionType), pointer, intent(in) :: gridConn !< the grid connection data for creating a DISU
  ! local
  class(NumericalModelType), pointer :: numMod
  class(*), pointer :: modelPtr

  this%memoryPath = create_mem_path(name)
  call this%allocate_scalars(name)

  this%iout = iout
  this%gridConnection => gridConn
  modelPtr => gridConn%model
  this%owner => CastAsGwtModel(modelPtr)

  ! create dis and packages
  ! TODO_MJR: fmi
  call disu_cr(this%dis, this%name, -1, this%iout)
  call adv_cr(this%adv, this%name, -1, this%iout, this%fmi)
    
end subroutine gwtifmod_cr

!> @brief Define the GWT interface model
!<
subroutine gwtifmod_df(this)
  class(GwtInterfaceModelType) :: this !< the GWT interface model
  ! local
  class(*), pointer :: disPtr

  this%moffset = 0

  ! define DISU
  disPtr => this%dis
  call this%gridConnection%getDiscretization(CastAsDisuType(disPtr))

end subroutine gwtifmod_df


!> @brief Allocate and read the GWT interface model and its packages
!<
subroutine gwtifmod_ar(this)
  class(GwtInterfaceModelType) :: this !< the GWT interface model
  ! local
  class(*), pointer :: disPtr
  type(GwtAdvOptionsType) :: advecOpt

  advecOpt%iAdvScheme = 2

  ! define DISU    
  disPtr => this%dis
  call this%gridConnection%getDiscretization(CastAsDisuType(disPtr))
  call this%adv%adv_ar(this%dis, this%ibound, advecOpt)

end subroutine gwtifmod_ar

!> @brief Clean up resources
!<
subroutine gwtifmod_da(this)
  class(GwtInterfaceModelType) :: this !< the GWT interface model

  ! dealloc base
  !call this%model_da()

end subroutine gwtifmod_da


end module GwtInterfaceModelModule
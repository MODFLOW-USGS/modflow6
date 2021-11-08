module GwtInterfaceModelModule
  use KindModule, only: I4B, DP  
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use MemoryHelperModule, only: create_mem_path
  use NumericalModelModule, only: NumericalModelType
  use GwtModule, only: GwtModelType, CastAsGwtModel
  use GwfDisuModule, only: disu_cr, CastAsDisuType
  use GwtFmiModule, only: fmi_cr, GwtFmiType
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
  call disu_cr(this%dis, this%name, -1, this%iout)
  call fmi_cr(this%fmi, this%name, 0, this%iout)
  call adv_cr(this%adv, this%name, 0, this%iout, this%fmi)
    
end subroutine gwtifmod_cr

!> @brief Define the GWT interface model
!<
subroutine gwtifmod_df(this)
  class(GwtInterfaceModelType) :: this !< the GWT interface model
  ! local
  class(*), pointer :: disPtr
  integer(I4B) :: i

  this%moffset = 0

  ! define DISU
  disPtr => this%dis
  call this%gridConnection%getDiscretization(CastAsDisuType(disPtr))
  call this%fmi%fmi_df(this%dis, 0)

   ! assign or point model members to dis members
  this%neq = this%dis%nodes
  this%nja = this%dis%nja
  this%ia  => this%dis%con%ia
  this%ja  => this%dis%con%ja
  !
  ! allocate model arrays, now that neq and nja are assigned
  call this%allocate_arrays()

  do i = 1, size(this%flowja)
    this%flowja = 0.0_DP
  end do

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
  call this%fmi%fmi_ar(this%ibound)
  call this%adv%adv_ar(this%dis, this%ibound, advecOpt)
  this%inadv = 999
  
end subroutine gwtifmod_ar

!> @brief Clean up resources
!<
subroutine gwtifmod_da(this)
  class(GwtInterfaceModelType) :: this !< the GWT interface model

  call this%dis%dis_da()
  call this%fmi%fmi_da()
  call this%adv%adv_da()
  
  deallocate(this%dis)
  deallocate(this%fmi)
  deallocate(this%adv)

  ! scalars
  call mem_deallocate(this%inic)
  call mem_deallocate(this%infmi)
  call mem_deallocate(this%inadv)
  call mem_deallocate(this%indsp)
  call mem_deallocate(this%inssm)
  call mem_deallocate(this%inmst)
  call mem_deallocate(this%inmvt)
  call mem_deallocate(this%inoc)
  call mem_deallocate(this%inobs)

  ! base
  call this%NumericalModelType%model_da()

end subroutine gwtifmod_da


end module GwtInterfaceModelModule
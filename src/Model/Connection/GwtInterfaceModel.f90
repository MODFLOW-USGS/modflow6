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
  use GwtDspModule, only: dsp_cr, GwtDspType
  use GwtDspOptionsModule, only: GwtDspOptionsType
  use GwtDspGridDataModule, only: GwtDspGridDataType
  use GwtObsModule, only: gwt_obs_cr
  use GridConnectionModule

  implicit none
  private

  !> The GWT Interface Model is a utility to calculate the solution's
  !! exchange coefficients from the interface between a GWT model and
  !! its GWT neighbors. The interface model itself will not be part
  !! of the solution, it is not being solved.
  type, public, extends(GwtModelType) :: GwtInterfaceModelType

    integer(i4B), pointer :: iAdvScheme => null() !< the advection scheme: 0 = up, 1 = central, 2 = tvd
    integer(i4B), pointer :: ixt3d => null() !< xt3d setting: 0 = off, 1 = lhs, 2 = rhs

    class(GridConnectionType), pointer :: gridConnection => null() !< The grid connection class will provide the interface grid
    class(GwtModelType), private, pointer :: owner => null() !< the real GWT model for which the exchange coefficients
                                                             !! are calculated with this interface model

    real(DP), dimension(:), pointer, contiguous :: porosity => null() !< to be filled with MST porosity

  contains
    procedure, pass(this) :: gwtifmod_cr
    procedure :: model_df => gwtifmod_df
    procedure :: model_ar => gwtifmod_ar
    procedure :: model_da => gwtifmod_da
    procedure :: allocate_scalars
    procedure :: setDspGridData
  end type GwtInterfaceModelType

contains

!> @brief Create the interface model, analogously to what
!< happens in gwt_cr
  subroutine gwtifmod_cr(this, name, iout, gridConn)
    class(GwtInterfaceModelType) :: this !< the GWT interface model
    character(len=*), intent(in) :: name !< the interface model's name
    integer(I4B), intent(in) :: iout !< the output unit
    class(GridConnectionType), pointer, intent(in) :: gridConn !< the grid connection data for creating a DISU
    ! local
    class(*), pointer :: modelPtr
    integer(I4B), target :: inobs
    integer(I4B) :: adv_unit, dsp_unit

    this%memoryPath = create_mem_path(name)
    call this%allocate_scalars(name)

    ! defaults
    this%iAdvScheme = 0
    this%ixt3d = 0

    this%iout = iout
    this%gridConnection => gridConn
    modelPtr => gridConn%model
    this%owner => CastAsGwtModel(modelPtr)

    inobs = 0
    adv_unit = 0
    dsp_unit = 0
    if (this%owner%inadv > 0) then
      this%inadv = huge(1_I4B)
      adv_unit = huge(1_I4B)
    end if
    if (this%owner%indsp > 0) then
      this%indsp = huge(1_I4B)
      dsp_unit = huge(1_I4B)
    end if

    ! create dis and packages
    call disu_cr(this%dis, this%name, -1, this%iout)
    call fmi_cr(this%fmi, this%name, 0, this%iout)
    call adv_cr(this%adv, this%name, adv_unit, this%iout, this%fmi)
    call dsp_cr(this%dsp, this%name, dsp_unit, this%iout, this%fmi)
    call gwt_obs_cr(this%obs, inobs)

  end subroutine gwtifmod_cr

  subroutine allocate_scalars(this, modelname)
    class(GwtInterfaceModelType) :: this !< the GWT interface model
    character(len=*), intent(in) :: modelname !< the model name

    call this%GwtModelType%allocate_scalars(modelname)

    call mem_allocate(this%iAdvScheme, 'ADVSCHEME', this%memoryPath)
    call mem_allocate(this%ixt3d, 'IXT3D', this%memoryPath)

  end subroutine allocate_scalars

!> @brief Define the GWT interface model
!<
  subroutine gwtifmod_df(this)
    class(GwtInterfaceModelType) :: this !< the GWT interface model
    ! local
    class(*), pointer :: disPtr
    type(GwtAdvOptionsType) :: adv_options
    type(GwtDspOptionsType) :: dsp_options
    integer(I4B) :: i

    this%moffset = 0
    adv_options%iAdvScheme = this%iAdvScheme
    dsp_options%ixt3d = this%ixt3d

    ! define DISU
    disPtr => this%dis
    call this%gridConnection%getDiscretization(CastAsDisuType(disPtr))
    call this%fmi%fmi_df(this%dis, 0)

    if (this%inadv > 0) then
      call this%adv%adv_df(adv_options)
    end if
    if (this%indsp > 0) then
      call this%dsp%dsp_df(this%dis, dsp_options)
    end if

    ! assign or point model members to dis members
    this%neq = this%dis%nodes
    this%nja = this%dis%nja
    this%ia => this%dis%con%ia
    this%ja => this%dis%con%ja
    !
    ! allocate model arrays, now that neq and nja are assigned
    call this%allocate_arrays()
    call mem_allocate(this%porosity, this%neq, 'POROSITY', this%memoryPath)

    do i = 1, size(this%flowja)
      this%flowja = 0.0_DP
    end do
    do i = 1, this%neq
      this%porosity = 0.0_DP
    end do

  end subroutine gwtifmod_df

!> @brief Override allocate and read the GWT interface model and its
!! packages so that we can create stuff from memory instead of input
!< files
  subroutine gwtifmod_ar(this)
    class(GwtInterfaceModelType) :: this !< the GWT interface model
    ! local
    type(GwtDspGridDataType) :: dspGridData

    call this%fmi%fmi_ar(this%ibound)
    if (this%inadv > 0) then
      call this%adv%adv_ar(this%dis, this%ibound)
    end if
    if (this%indsp > 0) then
      this%dsp%idiffc = this%owner%dsp%idiffc
      this%dsp%idisp = this%owner%dsp%idisp
      call dspGridData%construct(this%neq)
      call this%setDspGridData(dspGridData)
      call this%dsp%dsp_ar(this%ibound, this%porosity, dspGridData)
    end if

  end subroutine gwtifmod_ar

!> @brief set dsp grid data from models
!<
  subroutine setDspGridData(this, gridData)
    class(GwtInterfaceModelType) :: this !< the GWT interface model
    type(GwtDspGridDataType) :: gridData !< the dsp grid data to be set
    ! local
    integer(I4B) :: i, idx
    class(GwtModelType), pointer :: gwtModel
    class(*), pointer :: modelPtr

    do i = 1, this%neq
      modelPtr => this%gridConnection%idxToGlobal(i)%model
      gwtModel => CastAsGwtModel(modelPtr)
      idx = this%gridConnection%idxToGlobal(i)%index

      if (this%dsp%idiffc > 0) then
        gridData%diffc(i) = gwtModel%dsp%diffc(idx)
      end if
      if (this%dsp%idisp > 0) then
        gridData%alh(i) = gwtModel%dsp%alh(idx)
        gridData%alv(i) = gwtModel%dsp%alv(idx)
        gridData%ath1(i) = gwtModel%dsp%ath1(idx)
        gridData%ath2(i) = gwtModel%dsp%ath2(idx)
        gridData%atv(i) = gwtModel%dsp%atv(idx)
      end if

    end do

  end subroutine setDspGridData

!> @brief Clean up resources
!<
  subroutine gwtifmod_da(this)
    class(GwtInterfaceModelType) :: this !< the GWT interface model

    ! this
    call mem_deallocate(this%iAdvScheme)
    call mem_deallocate(this%ixt3d)
    call mem_deallocate(this%porosity)

    ! gwt packages
    call this%dis%dis_da()
    call this%fmi%fmi_da()
    call this%adv%adv_da()
    call this%dsp%dsp_da()

    deallocate (this%dis)
    deallocate (this%fmi)
    deallocate (this%adv)
    deallocate (this%dsp)

    ! gwt scalars
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

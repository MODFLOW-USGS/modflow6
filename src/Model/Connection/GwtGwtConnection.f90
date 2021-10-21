module GwtGwtConnectionModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LINELENGTH, LENCOMPONENTNAME
  use SimModule, only: ustop
  use MemoryManagerModule, only: mem_allocate
  use SpatialModelConnectionModule
  use NumericalModelModule
  use GwtModule
  use GwtGwtExchangeModule
  use GwtInterfaceModelModule
  use SparseModule, only: sparsematrix

  implicit none
  private

  !> Connects a GWT model to other GWT models in space. Derives
  !! from NumericalExchangeType so the solution can use it to
  !! fetch the coefficients for this connection.
  !<
  type, public, extends(SpatialModelConnectionType) :: GwtGwtConnectionType

    type(GwtModelType), pointer :: gwtModel => null()                 !< the model for which this connection exists
    type(GwtInterfaceModelType), pointer :: interfaceModel => null()  !< the interface model
    integer(I4B), pointer :: iAdvScheme => null()                     !< the advection scheme at the interface: 
                                                                      !! 0 = upstream, 1 = central, 2 = TVD

    integer(I4B) :: iout                                              !< the list file for the interface model

  contains

    procedure, pass(this) :: gwtGwtConnection_ctor
    generic, public :: construct => gwtGwtConnection_ctor

    procedure, pass(this) :: exg_ar => gwtgwtcon_ar
    procedure, pass(this) :: exg_df => gwtgwtcon_df 
    procedure, pass(this) :: exg_ac => gwtgwtcon_ac
    procedure, pass(this) :: exg_rp => gwtgwtcon_rp
    procedure, pass(this) :: exg_cf => gwtgwtcon_cf
    procedure, pass(this) :: exg_fc => gwtgwtcon_fc
    procedure, pass(this) :: exg_da => gwtgwtcon_da
    procedure, pass(this) :: exg_cq => gwtgwtcon_cq
    procedure, pass(this) :: exg_bd => gwtgwtcon_bd
    procedure, pass(this) :: exg_ot => gwtgwtcon_ot

    ! local stuff
    procedure, pass(this), private :: allocateScalars

  end type GwtGwtConnectionType

contains

  !> @brief Basic construction of the connection
  !<
subroutine gwtGwtConnection_ctor(this, model)  
  use InputOutputModule, only: openfile
  class(GwtGwtConnectionType) :: this         !< the connection
  class(NumericalModelType), pointer :: model !< the model owning this connection, 
                                              !! this must be a GwtModelType
  ! local
  character(len=LINELENGTH) :: fname
  character(len=LENCOMPONENTNAME) :: name

  this%gwtModel => CastToGwtModel(model)
  
  if (model%id > 99999) then
    write(*,*) 'Error: running 100000 submodels or more is not yet supported'
    call ustop()
  end if
  write(name,'(a,i5.5)') 'GTC_', model%id

  ! .lst file for interface model
  fname = trim(model%name)//'.im.lst'
  call openfile(this%iout, 0, fname, 'LIST', filstat_opt='REPLACE')
  write(this%iout, '(a,a)') 'Creating GWT-GWT connection for model ',           &
                            trim(this%gwtModel%name)

! first call base constructor
call this%SpatialModelConnectionType%spatialConnection_ctor(model, name)

call this%allocateScalars()
this%typename = 'GWT-GWT'
this%iAdvScheme = 0

allocate(this%interfaceModel)

end subroutine gwtGwtConnection_ctor

subroutine allocateScalars(this)
  class(GwtGwtConnectionType) :: this !< the connection

  call mem_allocate(this%iAdvScheme, 'IADVSCHEME', this%memoryPath)
  
end subroutine allocateScalars

!> @brief define the GWT-GWT connection
!<
subroutine gwtgwtcon_df(this)
  class(GwtGwtConnectionType) :: this !< the connection
  ! local
  integer(I4B) :: iex
  class(GwtExchangeType), pointer :: gwtEx
  character(len=LENCOMPONENTNAME) :: imName

  ! determine advection scheme (the GWT-GWT exchanges
  ! have been read at this point)
  do iex = 1, this%localExchanges%Count()
    gwtEx => GetGwfExchangeFromList(this%localExchanges, iex)
    if (gwtEx%iAdvScheme > this%iAdvScheme) then
      this%iAdvScheme = gwtEx%iAdvScheme
    end if
  end do

  ! prepare to extend when TVD:
  if (this%gwtModel%adv%iadvwt == 2) then
    this%internalStencilDepth = 2
  end if
  if (this%iAdvScheme == 2) then
    this%exchangeStencilDepth = 2
  end if

  ! now set up the GridConnection
  call this%spatialcon_df()

  ! we have to 'catch up' and create the interface model
  ! here, then the remainder of this routine will be define
  write(imName,'(a,i5.5)') 'GWTIM_', this%gwtModel%id
  call this%interfaceModel%construct(imName, this%iout)
  call this%interfaceModel%createModel(this%gridConnection)

end subroutine gwtgwtcon_df

!> @brief allocate and read/set the connection's data structures
!<
subroutine gwtgwtcon_ar(this)
  class(GwtGwtConnectionType) :: this !< the connection

end subroutine gwtgwtcon_ar

!> @brief add connections for the global system for
!< this connection
subroutine gwtgwtcon_ac(this, sparse)
  class(GwtGwtConnectionType) :: this         !< this connection
  type(sparsematrix), intent(inout) :: sparse !< sparse matrix to store the connections

end subroutine gwtgwtcon_ac

subroutine gwtgwtcon_rp(this)
  class(GwtGwtConnectionType) :: this !< the connection

end subroutine gwtgwtcon_rp

subroutine gwtgwtcon_cf(this, kiter)
  class(GwtGwtConnectionType) :: this !< the connection
  integer(I4B), intent(in) :: kiter   !< the iteration counter

end subroutine gwtgwtcon_cf

subroutine gwtgwtcon_fc(this, kiter, iasln, amatsln, rhssln, inwtflag)
  class(GwtGwtConnectionType) :: this               !< the connection
  integer(I4B), intent(in) :: kiter                 !< the iteration counter
  integer(I4B), dimension(:), intent(in) :: iasln   !< global system's IA array
  real(DP), dimension(:), intent(inout) :: amatsln  !< global system matrix coefficients
  real(DP), dimension(:), intent(inout) ::rhssln    !< global right-hand-side
  integer(I4B), optional, intent(in) :: inwtflag    !< newton-raphson flag

end subroutine gwtgwtcon_fc

subroutine gwtgwtcon_cq(this, icnvg, isuppress_output, isolnid)
  class(GwtGwtConnectionType) :: this          !< the connection
  integer(I4B), intent(inout) :: icnvg         !< convergence flag
  integer(I4B), intent(in) :: isuppress_output !< suppress output when =1
  integer(I4B), intent(in) :: isolnid          !< solution id

end subroutine gwtgwtcon_cq

subroutine gwtgwtcon_bd(this, icnvg, isuppress_output, isolnid)
  class(GwtGwtConnectionType) :: this           !< the connection
  integer(I4B), intent(inout) :: icnvg          !< convergence flag
  integer(I4B), intent(in) :: isuppress_output  !< suppress output when =1
  integer(I4B), intent(in) :: isolnid           !< solution id

end subroutine gwtgwtcon_bd

subroutine gwtgwtcon_ot(this)
  class(GwtGwtConnectionType) :: this !< the connection

end subroutine gwtgwtcon_ot

subroutine gwtgwtcon_da(this)
  class(GwtGwtConnectionType) :: this !< the connection

end subroutine gwtgwtcon_da

!> @brief Cast NumericalModelType to GwtModelType
!< TODO_MJR: move this to GWT
function CastToGwtModel(obj) result(gwtmodel)
  use NumericalModelModule, only: NumericalModelType
  class(NumericalModelType), pointer :: obj !< The numerical model to be cast
  class(GwtModelType), pointer :: gwtmodel  !< The GWT model
  
  gwtmodel => null()
  select type(obj)
    type is (GwtModelType)
      gwtmodel => obj
    end select
    
end function CastToGwtModel

end module
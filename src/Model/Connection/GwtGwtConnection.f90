module GwtGwtConnectionModule
  use KindModule, only: I4B, DP, lGP
  use ConstantsModule, only: LINELENGTH, LENCOMPONENTNAME, DZERO   
  use CsrUtilsModule, only: getCSRIndex
  use SimModule, only: ustop
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use SpatialModelConnectionModule
  use NumericalModelModule
  use GwtModule
  use GwtGwtExchangeModule
  use GwtInterfaceModelModule
  use SparseModule, only: sparsematrix
  use ConnectionsModule, only: ConnectionsType
  use TopologyModule, only: GlobalCellType

  implicit none
  private

  !> Connects a GWT model to other GWT models in space. Derives
  !! from NumericalExchangeType so the solution can use it to
  !! fetch the coefficients for this connection.
  !<
  type, public, extends(SpatialModelConnectionType) :: GwtGwtConnectionType

    type(GwtModelType), pointer :: gwtModel => null()                   !< the model for which this connection exists
    type(GwtInterfaceModelType), pointer :: gwtInterfaceModel => null() !< the interface model
    integer(I4B), pointer :: iIfaceAdvScheme => null()                  !< the advection scheme at the interface:
                                                                        !! 0 = upstream, 1 = central, 2 = TVD
    integer(I4B), pointer :: iIfaceXt3d => null()                       !< XT3D in the interface DSP package: 0 = no, 1 = lhs, 2 = rhs
    real(DP), dimension(:), pointer, contiguous :: exgflowja => null()  !< intercell flows at the interface, coming from
                                                                        !! multiple GWF models
    
    real(DP), dimension(:), pointer, contiguous :: gwfflowja => null()     !< flowja for the interface model
    real(DP), dimension(:), pointer, contiguous :: gwfsat => null()     !< gwfsat for the interface model
    real(DP), dimension(:), pointer, contiguous :: gwfhead => null()    !< gwfhead for the interface model
    real(DP), dimension(:,:), pointer, contiguous :: gwfspdis => null() !< gwfspdis for the interface model

    integer(I4B) :: iout                                                !< the list file for the interface model

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

    ! overriding 'protected'
    procedure, pass(this) :: validateConnection

    ! local stuff
    procedure, pass(this), private :: allocate_scalars
    procedure, pass(this), private :: allocate_arrays
    procedure, pass(this), private :: syncInterfaceModel
    procedure, pass(this), private :: setGridExtent

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
  class(*), pointer :: modelPtr

  modelPtr => model
  this%gwtModel => CastAsGwtModel(modelPtr)

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

  call this%allocate_scalars()
  this%typename = 'GWT-GWT'
  this%iIfaceAdvScheme = 0
  this%iIfaceXt3d = 1

  allocate(this%gwtInterfaceModel)
  this%interfaceModel => this%gwtInterfaceModel

end subroutine gwtGwtConnection_ctor

!> @brief Allocate scalar variables for this connection
!<
subroutine allocate_scalars(this)
  class(GwtGwtConnectionType) :: this !< the connection

  call mem_allocate(this%iIfaceAdvScheme, 'IADVSCHEME', this%memoryPath)
  call mem_allocate(this%iIfaceXt3d, 'IXT3D', this%memoryPath)

end subroutine allocate_scalars

!> @brief Allocate array variables for this connection
!<
subroutine allocate_arrays(this)
  class(GwtGwtConnectionType) :: this !< the connection
  ! local
  integer(I4B) :: i

  call mem_allocate(this%gwfflowja, this%interfaceModel%nja, 'FLOWJA',         &
                    this%memoryPath)
  call mem_allocate(this%gwfsat, this%neq, 'GWFSAT', this%memoryPath)
  call mem_allocate(this%gwfhead, this%neq, 'GWFHEAD', this%memoryPath)
  call mem_allocate(this%gwfspdis, 3, this%neq, 'GWFSPDIS', this%memoryPath)

  do i = 1, size(this%gwfflowja)
    this%gwfflowja = 0.0_DP
  end do

  do i = 1, this%neq
    this%gwfsat = 0.0_DP
  end do

end subroutine allocate_arrays

!> @brief define the GWT-GWT connection
!<
subroutine gwtgwtcon_df(this)
  class(GwtGwtConnectionType) :: this !< the connection
  ! local
  character(len=LENCOMPONENTNAME) :: imName  
  integer(I4B) :: iex
  class(GwtExchangeType), pointer :: gwtEx  

  ! determine advection scheme (the GWT-GWT exchanges
  ! have been read at this point)
  do iex = 1, this%localExchanges%Count()
    gwtEx => GetGwtExchangeFromList(this%localExchanges, iex)
    if (gwtEx%iAdvScheme > this%iIfaceAdvScheme) then
      this%iIfaceAdvScheme = gwtEx%iAdvScheme
    end if
  end do

  ! determine xt3d setting on interface
  do iex = 1, this%localExchanges%Count()
    gwtEx => GetGwtExchangeFromList(this%localExchanges, iex)
    if (gwtEx%ixt3d == 0) then
      this%iIfaceXt3d = 0
      exit
    end if
    if (gwtEx%ixt3d == 2) then
      this%iIfaceXt3d = 2 ! no exit, other exchange might have =0 which overrules this
    end if
  end do

  ! determine the required size of the interface model grid
  call this%setGridExtent()

  ! now set up the GridConnection
  call this%spatialcon_df()

  ! we have to 'catch up' and create the interface model
  ! here, then the remainder of this routine will be define
  write(imName,'(a,i5.5)') 'GWTIM_', this%gwtModel%id
  call this%gwtInterfaceModel%gwtifmod_cr(imName, this%iout, this%gridConnection)
  this%gwtInterfaceModel%iAdvScheme = this%iIfaceAdvScheme

  call this%gwtInterfaceModel%model_df()

  call this%allocate_arrays()

  ! connect X, RHS, IBOUND, and flowja
  call this%spatialcon_setmodelptrs()

  this%gwtInterfaceModel%fmi%gwfflowja => this%gwfflowja
  this%gwtInterfaceModel%fmi%gwfsat => this%gwfsat
  this%gwtInterfaceModel%fmi%gwfhead => this%gwfhead
  this%gwtInterfaceModel%fmi%gwfspdis => this%gwfspdis
  
  ! add connections from the interface model to solution matrix
  call this%spatialcon_connect()

end subroutine gwtgwtcon_df

!> @brief Set required extent of the interface grid from
!< the configuration
subroutine setGridExtent(this)
  class(GwtGwtConnectionType) :: this !< the connection
  ! local  
  logical(LGP) :: hasAdv, hasDsp

  hasAdv = this%gwtModel%inadv > 0
  hasDsp = this%gwtModel%indsp > 0

  if (hasAdv) then    
    if (this%iIfaceAdvScheme == 2) then
      this%exchangeStencilDepth = 2
      if (this%gwtModel%adv%iadvwt == 2) then
        this%internalStencilDepth = 2
      end if
    end if
  end if

  if (hasDsp) then    
    if (this%iIfaceXt3d > 0) then
      this%exchangeStencilDepth = 2
      if (this%gwtModel%dsp%ixt3d > 0) then
        this%internalStencilDepth = 2
      end if
    end if
  end if

end subroutine setGridExtent

!> @brief allocate and read/set the connection's data structures
!<
subroutine gwtgwtcon_ar(this)
  class(GwtGwtConnectionType) :: this !< the connection
  ! local
  integer(I4B) :: i, idx
  class(GwtModelType), pointer :: gwtModel
  class(*), pointer :: modelPtr
  
  ! check if we can construct an interface model
  ! NB: only makes sense after the models' allocate&read have been
  ! called, which is why we do it here
  call this%validateConnection()

  ! fill porosity from mst packages, needed for dsp
  if (this%gwtModel%inmst > 0) then
    do i = 1, this%neq
      modelPtr => this%gridConnection%idxToGlobal(i)%model
      gwtModel => CastAsGwtModel(modelPtr)
      idx = this%gridConnection%idxToGlobal(i)%index
      this%gwtInterfaceModel%porosity(i) = gwtModel%mst%porosity(idx)
    end do
  end if

  ! allocate and read base
  call this%spatialcon_ar()

  ! ... and now the interface model
  call this%gwtInterfaceModel%model_ar()

end subroutine gwtgwtcon_ar

!> @brief validate this connection prior to constructing 
!< the interface model
subroutine validateConnection(this)
  use SimVariablesModule, only: errmsg
  use SimModule, only: count_errors
  class(GwtGwtConnectionType) :: this !< this connection

  ! base validation, the spatial/geometry part
  call this%SpatialModelConnectionType%validateConnection()

  ! GWT related matters
  ! ...

  ! abort on errors
  if(count_errors() > 0) then
    write(errmsg, '(1x,a)') 'Errors occurred while processing exchange(s)'
    call ustop()
  end if
  
end subroutine validateConnection


!> @brief add connections to the global system for
!< this connection
subroutine gwtgwtcon_ac(this, sparse)
  class(GwtGwtConnectionType) :: this         !< this connection
  type(sparsematrix), intent(inout) :: sparse !< sparse matrix to store the connections
  ! local
  integer(I4B) :: ic, iglo, jglo
  type(GlobalCellType) :: boundaryCell, connectedCell

  ! connections to other models
  do ic = 1, this%gridConnection%nrOfBoundaryCells
    boundaryCell = this%gridConnection%boundaryCells(ic)%cell
    connectedCell = this%gridConnection%connectedCells(ic)%cell
    iglo = boundaryCell%index + boundaryCell%model%moffset
    jglo = connectedCell%index + connectedCell%model%moffset
    call sparse%addconnection(iglo, jglo, 1)
    call sparse%addconnection(jglo, iglo, 1)
  end do

  ! and internal connections
  call this%spatialcon_ac(sparse)

end subroutine gwtgwtcon_ac

subroutine gwtgwtcon_rp(this)
  class(GwtGwtConnectionType) :: this !< the connection

end subroutine gwtgwtcon_rp

subroutine gwtgwtcon_cf(this, kiter)
  class(GwtGwtConnectionType) :: this !< the connection
  integer(I4B), intent(in) :: kiter   !< the iteration counter
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

  call this%gwtInterfaceModel%model_cf(kiter)
  
end subroutine gwtgwtcon_cf


!> @brief called during advance (*_ad), to copy the data
!! from the models into the connection's placeholder arrays
!<
subroutine syncInterfaceModel(this)
  class(GwtGwtConnectionType) :: this !< the connection
  ! local
  integer(I4B) :: i, n, m, ipos, iposLoc, idx
  type(ConnectionsType), pointer :: imCon                 !< interface model connections
  type(GlobalCellType), dimension(:), pointer :: toGlobal !< map interface index to global cell
  type(GlobalCellType), pointer :: boundaryCell, connectedCell
  class(GwtModelType), pointer :: gwtModel
  class(*), pointer :: modelPtr

  ! for readability
  imCon => this%gwtInterfaceModel%dis%con
  toGlobal => this%gridConnection%idxToGlobal
  
  ! loop over connections in interface
  do n = 1, this%neq
    do ipos = imCon%ia(n) + 1, imCon%ia(n+1) - 1
      m = imCon%ja(ipos)
      if (associated(toGlobal(n)%model, toGlobal(m)%model)) then
        ! internal connection for a model, copy from its flowja
        iposLoc = getCSRIndex(toGlobal(n)%index, toGlobal(m)%index,             &
                              toGlobal(n)%model%ia, toGlobal(n)%model%ja)
        modelPtr => toGlobal(n)%model
        gwtModel => CastAsGwtModel(modelPtr)                      
        this%gwfflowja(ipos) = gwtModel%fmi%gwfflowja(iposLoc)
      end if      
    end do
  end do

  ! the flowja for exchange cells
  do i = 1, this%gridConnection%nrOfBoundaryCells
    boundaryCell => this%gridConnection%boundaryCells(i)%cell
    connectedCell => this%gridConnection%connectedCells(i)%cell
    n = this%gridConnection%getInterfaceIndex(boundaryCell%index,             &
                                              boundaryCell%model)
    m = this%gridConnection%getInterfaceIndex(connectedCell%index,            &
                                              connectedCell%model)
    ipos = getCSRIndex(n, m, imCon%ia, imCon%ja)
    this%gwfflowja(ipos) = this%exgflowja(i)
    ipos = getCSRIndex(m, n, imCon%ia, imCon%ja)
    this%gwfflowja(ipos) = -this%exgflowja(i)
  end do

  ! copy concentrations
  do i = 1, this%gridConnection%nrOfCells      
    idx = this%gridConnection%idxToGlobal(i)%index
    this%x(i) = this%gridConnection%idxToGlobal(i)%model%x(idx)
    this%gwtInterfaceModel%xold(i) = this%gridConnection%idxToGlobal(i)%model%xold(idx)
  end do

  ! copy fmi
  do i = 1, this%gridConnection%nrOfCells      
    idx = this%gridConnection%idxToGlobal(i)%index
    modelPtr => this%gridConnection%idxToGlobal(i)%model
    gwtModel => CastAsGwtModel(modelPtr)

    this%gwfsat(i) = gwtModel%fmi%gwfsat(idx)
    this%gwfhead(i) = gwtModel%fmi%gwfhead(idx)
    this%gwfspdis(1, i) = gwtModel%fmi%gwfspdis(1, idx)
    this%gwfspdis(2, i) = gwtModel%fmi%gwfspdis(2, idx)
    this%gwfspdis(3, i) = gwtModel%fmi%gwfspdis(3, idx)
  end do

end subroutine syncInterfaceModel


subroutine gwtgwtcon_fc(this, kiter, iasln, amatsln, rhssln, inwtflag)
  class(GwtGwtConnectionType) :: this               !< the connection
  integer(I4B), intent(in) :: kiter                 !< the iteration counter
  integer(I4B), dimension(:), intent(in) :: iasln   !< global system's IA array
  real(DP), dimension(:), intent(inout) :: amatsln  !< global system matrix coefficients
  real(DP), dimension(:), intent(inout) ::rhssln    !< global right-hand-side
  integer(I4B), optional, intent(in) :: inwtflag    !< newton-raphson flag
  ! local
  integer(I4B) :: n, nglo, ipos

  call this%gwtInterfaceModel%model_fc(kiter, this%amat, this%nja, inwtflag)
  
  ! map back to solution matrix
  do n = 1, this%neq
    ! We only need the coefficients for our own model 
    ! (i.e. rows in the matrix that belong to this%owner):
    if (.not. associated(this%gridConnection%idxToGlobal(n)%model, this%owner)) then
      cycle
    end if
    
    nglo = this%gridConnection%idxToGlobal(n)%index + this%gridConnection%idxToGlobal(n)%model%moffset
    rhssln(nglo) = rhssln(nglo) + this%rhs(n)
    
    do ipos = this%ia(n), this%ia(n+1) - 1
      amatsln(this%mapIdxToSln(ipos)) = amatsln(this%mapIdxToSln(ipos)) + this%amat(ipos)
    end do
  end do

end subroutine gwtgwtcon_fc

subroutine gwtgwtcon_cq(this, icnvg, isuppress_output, isolnid)
  class(GwtGwtConnectionType) :: this          !< the connection
  integer(I4B), intent(inout) :: icnvg         !< convergence flag
  integer(I4B), intent(in) :: isuppress_output !< suppress output when =1
  integer(I4B), intent(in) :: isolnid          !< solution id

  call this%gwtInterfaceModel%model_cq(icnvg, isuppress_output)

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
  ! local
  class(GwtExchangeType), pointer :: gwtEx
  integer(I4B) :: iex
  logical(LGP) :: isOpen

  ! scalars
  call mem_deallocate(this%iIfaceAdvScheme)
  call mem_deallocate(this%iIfaceXt3d)

  ! arrays
  call mem_deallocate(this%gwfflowja)
  call mem_deallocate(this%gwfsat)
  call mem_deallocate(this%gwfhead)
  call mem_deallocate(this%gwfspdis)

  ! interface model
  call this%gwtInterfaceModel%model_da()
  deallocate(this%gwtInterfaceModel)

  ! dealloc base
  call this%spatialcon_da()

  inquire(this%iout, opened=isOpen)
    if (isOpen) then
      close(this%iout)
    end if

  ! we need to deallocate the baseexchanges we own:
  do iex=1, this%localExchanges%Count()
    gwtEx => GetGwtExchangeFromList(this%localExchanges, iex)
    if (associated(gwtEx%model1, this%gwtModel)) then
      call gwtEx%exg_da()
    end if
  end do

end subroutine gwtgwtcon_da

end module
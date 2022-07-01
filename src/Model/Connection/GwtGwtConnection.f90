module GwtGwtConnectionModule
  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: LINELENGTH, LENCOMPONENTNAME, DZERO, LENBUDTXT
  use CsrUtilsModule, only: getCSRIndex
  use SimModule, only: ustop
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use SpatialModelConnectionModule
  use NumericalModelModule
  use GwtModule
  use DisConnExchangeModule
  use GwtGwtExchangeModule
  use GwtInterfaceModelModule
  use SparseModule, only: sparsematrix
  use ConnectionsModule, only: ConnectionsType
  use CellWithNbrsModule, only: GlobalCellType

  implicit none
  private

  public :: CastAsGwtGwtConnection

  !> Connects a GWT model to other GWT models in space. Derives
  !! from NumericalExchangeType so the solution can use it to
  !! fetch the coefficients for this connection.
  !<
  type, public, extends(SpatialModelConnectionType) :: GwtGwtConnectionType

    type(GwtModelType), pointer :: gwtModel => null() !< the model for which this connection exists
    type(GwtExchangeType), pointer :: gwtExchange => null() !< the primary exchange, cast to GWT-GWT
    logical(LGP) :: exchangeIsOwned !< there are two connections (in serial) for an exchange,
                                    !! one of them needs to manage/own the exchange (e.g. clean up)
    type(GwtInterfaceModelType), pointer :: gwtInterfaceModel => null() !< the interface model
    integer(I4B), pointer :: iIfaceAdvScheme => null() !< the advection scheme at the interface:
                                                       !! 0 = upstream, 1 = central, 2 = TVD
    integer(I4B), pointer :: iIfaceXt3d => null() !< XT3D in the interface DSP package: 0 = no, 1 = lhs, 2 = rhs
    real(DP), dimension(:), pointer, contiguous :: exgflowja => null() !< intercell flows at the interface, coming from GWF interface model
    integer(I4B), pointer :: exgflowSign => null() !< indicates the flow direction of exgflowja
    real(DP), dimension(:), pointer, contiguous :: exgflowjaGwt => null() !< gwt-flowja at the interface (this is a subset of the GWT
                                                                          !! interface model flowja's)

    real(DP), dimension(:), pointer, contiguous :: gwfflowja => null() !< gwfflowja for the interface model
    real(DP), dimension(:), pointer, contiguous :: gwfsat => null() !< gwfsat for the interface model
    real(DP), dimension(:), pointer, contiguous :: gwfhead => null() !< gwfhead for the interface model
    real(DP), dimension(:, :), pointer, contiguous :: gwfspdis => null() !< gwfspdis for the interface model

    real(DP), dimension(:), pointer, contiguous :: conc => null() !< pointer to concentration array
    integer(I4B), dimension(:), pointer, contiguous :: icbound => null() !< store pointer to gwt ibound array

    integer(I4B) :: iout = 0 !< the list file for the interface model

  contains

    procedure, pass(this) :: gwtGwtConnection_ctor
    generic, public :: construct => gwtGwtConnection_ctor

    procedure :: exg_ar => gwtgwtcon_ar
    procedure :: exg_df => gwtgwtcon_df
    procedure :: exg_ac => gwtgwtcon_ac
    procedure :: exg_rp => gwtgwtcon_rp
    procedure :: exg_ad => gwtgwtcon_ad
    procedure :: exg_cf => gwtgwtcon_cf
    procedure :: exg_fc => gwtgwtcon_fc
    procedure :: exg_da => gwtgwtcon_da
    procedure :: exg_cq => gwtgwtcon_cq
    procedure :: exg_bd => gwtgwtcon_bd
    procedure :: exg_ot => gwtgwtcon_ot

    ! overriding 'protected'
    procedure, pass(this) :: validateConnection

    ! local stuff
    procedure, pass(this), private :: allocate_scalars
    procedure, pass(this), private :: allocate_arrays
    procedure, pass(this), private :: syncInterfaceModel
    procedure, pass(this), private :: setGridExtent
    procedure, pass(this), private :: setFlowToExchange

  end type GwtGwtConnectionType

contains

!> @brief Basic construction of the connection
!<
  subroutine gwtGwtConnection_ctor(this, model, gwtEx)
    use InputOutputModule, only: openfile
    class(GwtGwtConnectionType) :: this !< the connection
    class(NumericalModelType), pointer :: model !< the model owning this connection,
                                                !! this must be a GwtModelType
    class(DisConnExchangeType), pointer :: gwtEx !< the GWT-GWT exchange the interface model is created for
    ! local
    character(len=LINELENGTH) :: fname
    character(len=LENCOMPONENTNAME) :: name
    class(*), pointer :: objPtr
    logical(LGP) :: write_ifmodel_listfile = .false.

    objPtr => model
    this%gwtModel => CastAsGwtModel(objPtr)
    objPtr => gwtEx
    this%gwtExchange => CastAsGwtExchange(objPtr)

    this%exchangeIsOwned = associated(model, gwtEx%model1)

    if (this%exchangeIsOwned) then
      write (name, '(a,i0)') 'GWTCON1_', gwtEx%id
    else
      write (name, '(a,i0)') 'GWTCON2_', gwtEx%id
    end if

    ! .lst file for interface model
    if (write_ifmodel_listfile) then
      fname = trim(name)//'.im.lst'
      call openfile(this%iout, 0, fname, 'LIST', filstat_opt='REPLACE')
      write (this%iout, '(4a)') 'Creating GWT-GWT connection for model ', &
        trim(this%gwtModel%name), 'from exchange ', &
        trim(gwtEx%name)
    end if

    ! first call base constructor
    call this%SpatialModelConnectionType%spatialConnection_ctor(model, &
                                                                gwtEx, &
                                                                name)

    call this%allocate_scalars()
    this%typename = 'GWT-GWT'
    this%iIfaceAdvScheme = 0
    this%iIfaceXt3d = 1
    this%exgflowSign = 1

    allocate (this%gwtInterfaceModel)
    this%interfaceModel => this%gwtInterfaceModel

  end subroutine gwtGwtConnection_ctor

!> @brief Allocate scalar variables for this connection
!<
  subroutine allocate_scalars(this)
    class(GwtGwtConnectionType) :: this !< the connection

    call mem_allocate(this%iIfaceAdvScheme, 'IADVSCHEME', this%memoryPath)
    call mem_allocate(this%iIfaceXt3d, 'IXT3D', this%memoryPath)
    call mem_allocate(this%exgflowSign, 'EXGFLOWSIGN', this%memoryPath)

  end subroutine allocate_scalars

!> @brief Allocate array variables for this connection
!<
  subroutine allocate_arrays(this)
    class(GwtGwtConnectionType) :: this !< the connection
    ! local
    integer(I4B) :: i

    call mem_allocate(this%gwfflowja, this%interfaceModel%nja, 'GWFFLOWJA', &
                      this%memoryPath)
    call mem_allocate(this%gwfsat, this%neq, 'GWFSAT', this%memoryPath)
    call mem_allocate(this%gwfhead, this%neq, 'GWFHEAD', this%memoryPath)
    call mem_allocate(this%gwfspdis, 3, this%neq, 'GWFSPDIS', this%memoryPath)

    call mem_allocate(this%exgflowjaGwt, this%gridConnection%nrOfBoundaryCells, &
                      'EXGFLOWJAGWT', this%memoryPath)

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

    ! determine advection scheme (the GWT-GWT exchange
    ! has been read at this point)
    this%iIfaceAdvScheme = this%gwtExchange%iAdvScheme

    ! determine xt3d setting on interface
    this%iIfaceXt3d = this%gwtExchange%ixt3d

    ! determine the required size of the interface model grid
    call this%setGridExtent()

    ! now set up the GridConnection
    call this%spatialcon_df()

    ! we have to 'catch up' and create the interface model
    ! here, then the remainder of this routine will be define
    if (this%exchangeIsOwned) then
      write (imName, '(a,i0)') 'GWTIM1_', this%gwtExchange%id
    else
      write (imName, '(a,i0)') 'GWTIM2_', this%gwtExchange%id
    end if
    call this%gwtInterfaceModel%gwtifmod_cr(imName, &
                                            this%iout, &
                                            this%gridConnection)
    this%gwtInterfaceModel%iAdvScheme = this%iIfaceAdvScheme
    this%gwtInterfaceModel%ixt3d = this%iIfaceXt3d
    call this%gwtInterfaceModel%model_df()

    call this%allocate_arrays()

    ! connect X, RHS, IBOUND, and flowja
    call this%spatialcon_setmodelptrs()

    this%gwtInterfaceModel%fmi%gwfflowja => this%gwfflowja
    this%gwtInterfaceModel%fmi%gwfsat => this%gwfsat
    this%gwtInterfaceModel%fmi%gwfhead => this%gwfhead
    this%gwtInterfaceModel%fmi%gwfspdis => this%gwfspdis

    ! connect pointers (used by BUY)
    this%conc => this%gwtInterfaceModel%x
    this%icbound => this%gwtInterfaceModel%ibound

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

    ! AR the movers and obs through the exchange
    if (this%exchangeIsOwned) then
      !cdl implement this when MVT is ready
      !cdl if (this%gwtExchange%inmvt > 0) then
      !cdl   call this%gwtExchange%mvt%mvt_ar()
      !cdl end if
      if (this%gwtExchange%inobs > 0) then
        call this%gwtExchange%obs%obs_ar()
      end if
    end if

  end subroutine gwtgwtcon_ar

!> @brief validate this connection prior to constructing
!< the interface model
  subroutine validateConnection(this)
    use SimVariablesModule, only: errmsg
    use SimModule, only: count_errors, store_error
    class(GwtGwtConnectionType) :: this !< this connection

    ! base validation, the spatial/geometry part
    call this%SpatialModelConnectionType%validateConnection()

    ! GWT related matters
    if ((this%gwtExchange%gwtmodel1%inadv > 0 .and. &
         this%gwtExchange%gwtmodel2%inadv == 0) .or. &
        (this%gwtExchange%gwtmodel2%inadv > 0 .and. &
         this%gwtExchange%gwtmodel1%inadv == 0)) then
      write (errmsg, '(1x,a,a,a)') 'Cannot connect GWT models in exchange ', &
        trim(this%gwtExchange%name), ' because one model is configured with ADV &
        &and the other one is not'
      call store_error(errmsg)
    end if

    if ((this%gwtExchange%gwtmodel1%indsp > 0 .and. &
         this%gwtExchange%gwtmodel2%indsp == 0) .or. &
        (this%gwtExchange%gwtmodel2%indsp > 0 .and. &
         this%gwtExchange%gwtmodel1%indsp == 0)) then
      write (errmsg, '(1x,a,a,a)') 'Cannot connect GWT models in exchange ', &
        trim(this%gwtExchange%name), ' because one model is configured with DSP &
        &and the other one is not'
      call store_error(errmsg)
    end if

    ! abort on errors
    if (count_errors() > 0) then
      write (errmsg, '(1x,a)') 'Errors occurred while processing exchange(s)'
      call ustop()
    end if

  end subroutine validateConnection

!> @brief add connections to the global system for
!< this connection
  subroutine gwtgwtcon_ac(this, sparse)
    class(GwtGwtConnectionType) :: this !< this connection
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

    ! Call exchange rp routines
    if (this%exchangeIsOwned) then
      call this%gwtExchange%exg_rp()
    end if

  end subroutine gwtgwtcon_rp

!> @brief Advance this connection
  !<
  subroutine gwtgwtcon_ad(this)
    class(GwtGwtConnectionType) :: this !< this connection

    ! copy model data into interface model
    call this%syncInterfaceModel()

    ! recalculate dispersion ellipse
    if (this%gwtInterfaceModel%indsp > 0) call this%gwtInterfaceModel%dsp%dsp_ad()

    if (this%exchangeIsOwned) then
      call this%gwtExchange%exg_ad()
    end if

  end subroutine gwtgwtcon_ad

  subroutine gwtgwtcon_cf(this, kiter)
    class(GwtGwtConnectionType) :: this !< the connection
    integer(I4B), intent(in) :: kiter !< the iteration counter
    ! local
    integer(I4B) :: i

    ! copy model data into interface model
    ! (when kiter == 1, this is already done in _ad)
    if (kiter > 1) call this%syncInterfaceModel()

    ! reset interface system
    do i = 1, this%nja
      this%amat(i) = 0.0_DP
    end do
    do i = 1, this%neq
      this%rhs(i) = 0.0_DP
    end do

    call this%gwtInterfaceModel%model_cf(kiter)

  end subroutine gwtgwtcon_cf

!> @brief called during advance (*_ad), to copy the data
!! from the models into the connection's placeholder arrays
!<
  subroutine syncInterfaceModel(this)
    class(GwtGwtConnectionType) :: this !< the connection
    ! local
    integer(I4B) :: i, n, m, ipos, iposLoc, idx
    type(ConnectionsType), pointer :: imCon !< interface model connections
    type(GlobalCellType), dimension(:), pointer :: toGlobal !< map interface index to global cell
    type(GlobalCellType), pointer :: boundaryCell, connectedCell
    class(GwtModelType), pointer :: gwtModel
    class(*), pointer :: modelPtr

    ! for readability
    imCon => this%gwtInterfaceModel%dis%con
    toGlobal => this%gridConnection%idxToGlobal

    ! loop over connections in interface
    do n = 1, this%neq
      do ipos = imCon%ia(n) + 1, imCon%ia(n + 1) - 1
        m = imCon%ja(ipos)
        if (associated(toGlobal(n)%model, toGlobal(m)%model)) then
          ! internal connection for a model, copy from its flowja
          iposLoc = getCSRIndex(toGlobal(n)%index, toGlobal(m)%index, &
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
      n = this%gridConnection%getInterfaceIndex(boundaryCell%index, &
                                                boundaryCell%model)
      m = this%gridConnection%getInterfaceIndex(connectedCell%index, &
                                                connectedCell%model)
      ipos = getCSRIndex(n, m, imCon%ia, imCon%ja)
      this%gwfflowja(ipos) = this%exgflowja(i) * this%exgflowSign
      ipos = getCSRIndex(m, n, imCon%ia, imCon%ja)
      this%gwfflowja(ipos) = -this%exgflowja(i) * this%exgflowSign
    end do

    ! copy concentrations
    do i = 1, this%gridConnection%nrOfCells
      idx = this%gridConnection%idxToGlobal(i)%index
      this%x(i) = this%gridConnection%idxToGlobal(i)%model%x(idx)
      this%gwtInterfaceModel%xold(i) = &
        this%gridConnection%idxToGlobal(i)%model%xold(idx)
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
    class(GwtGwtConnectionType) :: this !< the connection
    integer(I4B), intent(in) :: kiter !< the iteration counter
    integer(I4B), dimension(:), intent(in) :: iasln !< global system's IA array
    real(DP), dimension(:), intent(inout) :: amatsln !< global system matrix coefficients
    real(DP), dimension(:), intent(inout) :: rhssln !< global right-hand-side
    integer(I4B), optional, intent(in) :: inwtflag !< newton-raphson flag
    ! local
    integer(I4B) :: n, nglo, ipos

    call this%gwtInterfaceModel%model_fc(kiter, this%amat, this%nja, inwtflag)

    ! map back to solution matrix
    do n = 1, this%neq
      ! We only need the coefficients for our own model
      ! (i.e. rows in the matrix that belong to this%owner):
      if (.not. associated(this%gridConnection%idxToGlobal(n)%model, &
                           this%owner)) then
        cycle
      end if

      nglo = this%gridConnection%idxToGlobal(n)%index + &
             this%gridConnection%idxToGlobal(n)%model%moffset
      rhssln(nglo) = rhssln(nglo) + this%rhs(n)

      do ipos = this%ia(n), this%ia(n + 1) - 1
        amatsln(this%mapIdxToSln(ipos)) = amatsln(this%mapIdxToSln(ipos)) + &
                                          this%amat(ipos)
      end do
    end do

    ! FC the movers through the exchange; we can call
    ! exg_fc() directly because it only handles mover terms (unlike in GwfExchange%exg_fc)
    if (this%exchangeIsOwned) then
      call this%gwtExchange%exg_fc(kiter, iasln, amatsln, rhssln, inwtflag)
    end if

  end subroutine gwtgwtcon_fc

  subroutine gwtgwtcon_cq(this, icnvg, isuppress_output, isolnid)
    class(GwtGwtConnectionType) :: this !< the connection
    integer(I4B), intent(inout) :: icnvg !< convergence flag
    integer(I4B), intent(in) :: isuppress_output !< suppress output when =1
    integer(I4B), intent(in) :: isolnid !< solution id

    call this%gwtInterfaceModel%model_cq(icnvg, isuppress_output)
    call this%setFlowToExchange()

  end subroutine gwtgwtcon_cq

  !> @brief Set the flows (flowja from interface model) to the
  !< simvals in the exchange, leaving the budget calcution in there
  subroutine setFlowToExchange(this)
    class(GwtGwtConnectionType) :: this !< this connection
    ! local
    integer(I4B) :: i
    integer(I4B) :: nIface, mIface, ipos
    class(GwtExchangeType), pointer :: gwtEx

    gwtEx => this%gwtExchange
    if (this%exchangeIsOwned) then
      do i = 1, gwtEx%nexg
        gwtEx%simvals(i) = DZERO

        if (gwtEx%gwtmodel1%ibound(gwtEx%nodem1(i)) /= 0 .and. &
            gwtEx%gwtmodel2%ibound(gwtEx%nodem2(i)) /= 0) then

          nIface = this%gridConnection%getInterfaceIndex(gwtEx%nodem1(i), &
                                                         gwtEx%model1)
          mIface = this%gridConnection%getInterfaceIndex(gwtEx%nodem2(i), &
                                                         gwtEx%model2)
          ipos = getCSRIndex(nIface, mIface, this%gwtInterfaceModel%ia, &
                             this%gwtInterfaceModel%ja)
          gwtEx%simvals(i) = this%gwtInterfaceModel%flowja(ipos)

        end if
      end do
    end if

  end subroutine setFlowToExchange

  subroutine gwtgwtcon_bd(this, icnvg, isuppress_output, isolnid)
    use BudgetModule, only: rate_accumulator
    class(GwtGwtConnectionType) :: this !< the connection
    integer(I4B), intent(inout) :: icnvg !< convergence flag
    integer(I4B), intent(in) :: isuppress_output !< suppress output when =1
    integer(I4B), intent(in) :: isolnid !< solution id

    ! call exchange budget routine, also calls bd
    ! for movers.
    if (this%exchangeIsOwned) then
      call this%gwtExchange%exg_bd(icnvg, isuppress_output, isolnid)
    end if

  end subroutine gwtgwtcon_bd

  subroutine gwtgwtcon_ot(this)
    class(GwtGwtConnectionType) :: this !< the connection

    ! Call exg_ot() here as it handles all output processing
    ! based on gwtExchange%simvals(:), which was correctly
    ! filled from gwtgwtcon
    if (this%exchangeIsOwned) then
      call this%gwtExchange%exg_ot()
    end if

  end subroutine gwtgwtcon_ot

  subroutine gwtgwtcon_da(this)
    class(GwtGwtConnectionType) :: this !< the connection
    ! local
    logical(LGP) :: isOpen

    ! scalars
    call mem_deallocate(this%iIfaceAdvScheme)
    call mem_deallocate(this%iIfaceXt3d)
    call mem_deallocate(this%exgflowSign)

    ! arrays
    call mem_deallocate(this%gwfflowja)
    call mem_deallocate(this%gwfsat)
    call mem_deallocate(this%gwfhead)
    call mem_deallocate(this%gwfspdis)
    call mem_deallocate(this%exgflowjaGwt)

    ! interface model
    call this%gwtInterfaceModel%model_da()
    deallocate (this%gwtInterfaceModel)

    ! dealloc base
    call this%spatialcon_da()

    inquire (this%iout, opened=isOpen)
    if (isOpen) then
      close (this%iout)
    end if

    ! we need to deallocate the exchange we own:
    if (this%exchangeIsOwned) then
      call this%gwtExchange%exg_da()
    end if

  end subroutine gwtgwtcon_da

!> @brief Cast to GwtGwtConnectionType
!<
  function CastAsGwtGwtConnection(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj !< object to be cast
    class(GwtGwtConnectionType), pointer :: res !< the GwtGwtConnection

    res => null()
    if (.not. associated(obj)) return

    select type (obj)
    class is (GwtGwtConnectionType)
      res => obj
    end select
    return
  end function CastAsGwtGwtConnection

end module

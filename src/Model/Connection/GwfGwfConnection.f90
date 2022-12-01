module GwfGwfConnectionModule
  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: DZERO, DONE, DEM6, LENVARNAME, &
                             LENCOMPONENTNAME, LENMEMPATH, LINELENGTH
  use CsrUtilsModule, only: getCSRIndex
  use SparseModule, only: sparsematrix
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use SimModule, only: ustop
  use SpatialModelConnectionModule
  use GwfInterfaceModelModule
  use NumericalModelModule
  use GwfModule, only: GwfModelType, CastAsGwfModel
  use DisConnExchangeModule
  use GwfGwfExchangeModule, only: GwfExchangeType, GetGwfExchangeFromList, &
                                  CastAsGwfExchange
  use GwfNpfModule, only: GwfNpfType, hcond, vcond
  use GwfBuyModule, only: GwfBuyType
  use BaseDisModule, only: DisBaseType
  use ConnectionsModule, only: ConnectionsType
  use CellWithNbrsModule, only: GlobalCellType
  use DistributedDataModule

  implicit none
  private

  public :: CastAsGwfGwfConnection

  !> Connecting a GWF model to other models in space, implements
  !! NumericalExchangeType so the solution can used this object to determine
  !! the coefficients for the coupling between two adjacent models.
  !<
  type, public, extends(SpatialModelConnectionType) :: GwfGwfConnectionType

    type(GwfModelType), pointer :: gwfModel => null() !< the model for which this connection exists
    type(GwfExchangeType), pointer :: gwfExchange => null() !< the primary exchange, cast to its concrete type
    logical(LGP) :: exchangeIsOwned !< there are two connections (in serial) for an exchange,
                                    !! one of them needs to manage/own the exchange (e.g. clean up)
    type(GwfInterfaceModelType), pointer :: gwfInterfaceModel => null() !< the interface model
    integer(I4B), pointer :: iXt3dOnExchange => null() !< run XT3D on the interface,
                                                       !! 0 = don't, 1 = matrix, 2 = rhs
    integer(I4B) :: iout = 0 !< the list file for the interface model

  contains
    procedure, pass(this) :: gwfGwfConnection_ctor
    generic, public :: construct => gwfGwfConnection_ctor

    ! overriding NumericalExchangeType
    procedure :: exg_df => gwfgwfcon_df
    procedure :: exg_ar => gwfgwfcon_ar
    procedure :: exg_rp => gwfgwfcon_rp
    procedure :: exg_ad => gwfgwfcon_ad
    procedure :: exg_cf => gwfgwfcon_cf
    procedure :: exg_fc => gwfgwfcon_fc
    procedure :: exg_da => gwfgwfcon_da
    procedure :: exg_cq => gwfgwfcon_cq
    procedure :: exg_bd => gwfgwfcon_bd
    procedure :: exg_ot => gwfgwfcon_ot

    ! overriding 'protected'
    procedure, pass(this) :: validateConnection

    ! local stuff
    procedure, pass(this), private :: allocateScalars
    procedure, pass(this), private :: setGridExtent
    procedure, pass(this), private :: validateGwfExchange
    procedure, pass(this), private :: setFlowToExchange
    procedure, pass(this), private :: setNpfEdgeProps

  end type GwfGwfConnectionType

contains

  !> @brief Basic construction of the connection
  !<
  subroutine gwfGwfConnection_ctor(this, model, gwfEx)
    use NumericalModelModule, only: NumericalModelType
    use InputOutputModule, only: openfile
    class(GwfGwfConnectionType) :: this !< the connection
    class(NumericalModelType), pointer :: model !< the model owning this connection,
                                                !! this must of course be a GwfModelType
    class(DisConnExchangeType), pointer :: gwfEx !< the exchange the interface model is created for
    ! local
    character(len=LINELENGTH) :: fname
    character(len=LENCOMPONENTNAME) :: name
    class(*), pointer :: objPtr
    logical(LGP) :: write_ifmodel_listfile = .false.

    objPtr => model
    this%gwfModel => CastAsGwfModel(objPtr)
    objPtr => gwfEx
    this%gwfExchange => CastAsGwfExchange(objPtr)

    this%exchangeIsOwned = associated(gwfEx%model1, model)

    if (this%exchangeIsOwned) then
      write (name, '(a,i0)') 'GWFCON1_', gwfEx%id
    else
      write (name, '(a,i0)') 'GWFCON2_', gwfEx%id
    end if

    ! .lst file for interface model
    if (write_ifmodel_listfile) then
      fname = trim(name)//'.im.lst'
      call openfile(this%iout, 0, fname, 'LIST', filstat_opt='REPLACE')
      write (this%iout, '(4a)') 'Creating GWF-GWF connection for model ', &
        trim(this%gwfModel%name), ' from exchange ', &
        trim(gwfEx%name)
    end if

    ! first call base constructor
    call this%SpatialModelConnectionType%spatialConnection_ctor(model, &
                                                                gwfEx, &
                                                                name)

    call this%allocateScalars()

    this%typename = 'GWF-GWF'
    this%iXt3dOnExchange = 0

    allocate (this%gwfInterfaceModel)
    this%interfaceModel => this%gwfInterfaceModel

  end subroutine gwfGwfConnection_ctor

  !> @brief Define the connection
  !!
  !! This sets up the GridConnection (for creating the
  !! interface grid), creates and defines the interface
  !< model
  subroutine gwfgwfcon_df(this)
    class(GwfGwfConnectionType) :: this !< this connection
    ! local
    character(len=LENCOMPONENTNAME) :: imName !< the interface model's name

    ! determine the required size of the interface grid
    call this%setGridExtent()

    ! this sets up the GridConnection
    call this%spatialcon_df()

    ! Now grid conn is defined, we create the interface model
    ! here, and the remainder of this routine is define.
    ! we basically follow the logic that is present in sln_df()
    if (this%exchangeIsOwned) then
      write (imName, '(a,i0)') 'GWFIM1_', this%gwfExchange%id
    else
      write (imName, '(a,i0)') 'GWFIM2_', this%gwfExchange%id
    end if
    call this%gwfInterfaceModel%gwfifm_cr(imName, this%iout, this%gridConnection)
    call this%gwfInterfaceModel%set_idsoln(this%gwfModel%idsoln)
    this%gwfInterfaceModel%npf%satomega = this%gwfModel%npf%satomega
    this%gwfInterfaceModel%npf%ixt3d = this%iXt3dOnExchange
    call this%gwfInterfaceModel%model_df()

    ! Take these settings from the owning model, TODO_MJR:
    ! what if the owner iangle1 == 0 but the neighbor doesn't?
    this%gwfInterfaceModel%npf%ik22 = this%gwfModel%npf%ik22
    this%gwfInterfaceModel%npf%ik33 = this%gwfModel%npf%ik33
    this%gwfInterfaceModel%npf%iwetdry = this%gwfModel%npf%iwetdry
    this%gwfInterfaceModel%npf%iangle1 = this%gwfModel%npf%iangle1
    this%gwfInterfaceModel%npf%iangle2 = this%gwfModel%npf%iangle2
    this%gwfInterfaceModel%npf%iangle3 = this%gwfModel%npf%iangle3

    call this%addDistVar('X', '', this%gwfInterfaceModel%name, &
                         SYNC_NODES, '', (/BEFORE_AR, BEFORE_AD, BEFORE_CF/))
    call this%addDistVar('IBOUND', '', this%gwfInterfaceModel%name, &
                         SYNC_NODES, '', (/BEFORE_AR, BEFORE_AD, BEFORE_CF/))
    call this%addDistVar('XOLD', '', this%gwfInterfaceModel%name, &
                         SYNC_NODES, '', (/BEFORE_AD, BEFORE_CF/))
    call this%addDistVar('ICELLTYPE', 'NPF', this%gwfInterfaceModel%name, &
                         SYNC_NODES, '', (/BEFORE_AR/))
    call this%addDistVar('K11', 'NPF', this%gwfInterfaceModel%name, &
                         SYNC_NODES, '', (/BEFORE_AR/))
    call this%addDistVar('K22', 'NPF', this%gwfInterfaceModel%name, &
                         SYNC_NODES, '', (/BEFORE_AR/))
    call this%addDistVar('K33', 'NPF', this%gwfInterfaceModel%name, &
                         SYNC_NODES, '', (/BEFORE_AR/))
    if (this%gwfInterfaceModel%npf%iangle1 == 1) then
      call this%addDistVar('ANGLE1', 'NPF', this%gwfInterfaceModel%name, &
                           SYNC_NODES, '', (/BEFORE_AR/))
    end if
    if (this%gwfInterfaceModel%npf%iangle2 == 1) then
      call this%addDistVar('ANGLE2', 'NPF', this%gwfInterfaceModel%name, &
                           SYNC_NODES, '', (/BEFORE_AR/))
    end if
    if (this%gwfInterfaceModel%npf%iangle3 == 1) then
      call this%addDistVar('ANGLE3', 'NPF', this%gwfInterfaceModel%name, &
                           SYNC_NODES, '', (/BEFORE_AR/))
    end if
    if (this%gwfInterfaceModel%npf%iwetdry == 1) then
      call this%addDistVar('WETDRY', 'NPF', this%gwfInterfaceModel%name, &
                           SYNC_NODES, '', (/BEFORE_AR/))
    end if
    call this%addDistVar('TOP', 'DIS', this%gwfInterfaceModel%name, &
                         SYNC_NODES, '', (/BEFORE_AR/))
    call this%addDistVar('BOT', 'DIS', this%gwfInterfaceModel%name, &
                         SYNC_NODES, '', (/BEFORE_AR/))
    call this%addDistVar('AREA', 'DIS', this%gwfInterfaceModel%name, &
                         SYNC_NODES, '', (/BEFORE_AR/))
    call this%mapVariables()

    if (this%gwfInterfaceModel%npf%ixt3d > 0) then
      this%gwfInterfaceModel%npf%iangle1 = 1
      this%gwfInterfaceModel%npf%iangle2 = 1
      this%gwfInterfaceModel%npf%iangle3 = 1
    end if

    ! set defaults
    ! TODO_MJR: loop this
    this%gwfInterfaceModel%npf%angle1 = 0.0_DP
    this%gwfInterfaceModel%npf%angle2 = 0.0_DP
    this%gwfInterfaceModel%npf%angle3 = 0.0_DP

    ! point X, RHS, IBOUND to connection
    call this%spatialcon_setmodelptrs()

    ! connect interface model to spatial connection
    call this%spatialcon_connect()

  end subroutine gwfgwfcon_df

  !> @brief Set the required size of the interface grid from
  !< the configuration
  subroutine setGridExtent(this)
    class(GwfGwfConnectionType) :: this !< the connection
    ! local

    this%iXt3dOnExchange = this%gwfExchange%ixt3d
    if (this%iXt3dOnExchange > 0) then
      this%exchangeStencilDepth = 2
      if (this%gwfModel%npf%ixt3d > 0) then
        this%internalStencilDepth = 2
      end if
    end if

  end subroutine setGridExtent

  !> @brief allocation of scalars in the connection
  !<
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(GwfGwfConnectionType) :: this !< the connection
    ! local

    call mem_allocate(this%iXt3dOnExchange, 'IXT3DEXG', this%memoryPath)

  end subroutine allocateScalars

  !> @brief Allocate and read the connection
  !<
  subroutine gwfgwfcon_ar(this)
    use GridConnectionModule, only: GridConnectionType
    class(GwfGwfConnectionType) :: this !< this connection
    ! local

    ! check if we can construct an interface model
    ! NB: only makes sense after the models' allocate&read have been
    ! called, which is why we do it here
    call this%validateConnection()

    ! allocate and read base
    call this%spatialcon_ar()

    ! ... and now the interface model
    call this%gwfInterfaceModel%model_ar()

    ! AR the movers and obs through the exchange
    if (this%exchangeIsOwned) then
      if (this%gwfExchange%inmvr > 0) then
        call this%gwfExchange%mvr%mvr_ar()
      end if
      if (this%gwfExchange%inobs > 0) then
        call this%gwfExchange%obs%obs_ar()
      end if
    end if

  end subroutine gwfgwfcon_ar

  !> @brief Read time varying data when required
  !<
  subroutine gwfgwfcon_rp(this)
    class(GwfGwfConnectionType) :: this !< this connection

    ! Call exchange rp routines
    if (this%exchangeIsOwned) then
      call this%gwfExchange%exg_rp()
    end if

    return
  end subroutine gwfgwfcon_rp

  !> @brief Advance this connection
  !<
  subroutine gwfgwfcon_ad(this)
    class(GwfGwfConnectionType) :: this !< this connection

    ! this triggers the BUY density calculation
    if (this%gwfInterfaceModel%inbuy > 0) call this%gwfInterfaceModel%buy%buy_ad()

    if (this%exchangeIsOwned) then
      call this%gwfExchange%exg_ad()
    end if

  end subroutine gwfgwfcon_ad

  !> @brief Calculate (or adjust) matrix coefficients,
  !! in this case those which are determined or affected
  !< by the connection of a GWF model with its neigbors
  subroutine gwfgwfcon_cf(this, kiter)
    class(GwfGwfConnectionType) :: this !< this connection
    integer(I4B), intent(in) :: kiter !< the iteration counter
    ! local
    integer(I4B) :: i

    ! reset interface system
    do i = 1, this%nja
      this%amat(i) = 0.0_DP
    end do
    do i = 1, this%neq
      this%rhs(i) = 0.0_DP
    end do

    ! calculate (wetting/drying, saturation)
    call this%gwfInterfaceModel%model_cf(kiter)

  end subroutine gwfgwfcon_cf

  !> @brief Write the calculated coefficients into the global
  !< system matrix and the rhs
  subroutine gwfgwfcon_fc(this, kiter, iasln, amatsln, rhssln, inwtflag)
    class(GwfGwfConnectionType) :: this !< this connection
    integer(I4B), intent(in) :: kiter !< the iteration counter
    integer(I4B), dimension(:), intent(in) :: iasln !< global system's IA array
    real(DP), dimension(:), intent(inout) :: amatsln !< global system matrix coefficients
    real(DP), dimension(:), intent(inout) :: rhssln !< global right-hand-side
    integer(I4B), optional, intent(in) :: inwtflag !< newton-raphson flag
    ! local
    integer(I4B) :: n, ipos, nglo

    ! fill (and add to...) coefficients for interface
    call this%gwfInterfaceModel%model_fc(kiter, this%amat, this%nja, inwtflag)

    ! map back to solution matrix
    do n = 1, this%neq
      ! we cannot check with the mask here, because cross-terms are not
      ! necessarily from primary connections. But, we only need the coefficients
      ! for our own model (i.e. fluxes into cells belonging to this%owner):
      if (.not. this%gridConnection%idxToGlobal(n)%dmodel == this%owner) then
        ! only add connections for own model to global matrix
        cycle
      end if

      nglo = this%gridConnection%idxToGlobal(n)%index + &
             this%gridConnection%idxToGlobal(n)%dmodel%moffset
      rhssln(nglo) = rhssln(nglo) + this%rhs(n)

      do ipos = this%ia(n), this%ia(n + 1) - 1
        amatsln(this%mapIdxToSln(ipos)) = amatsln(this%mapIdxToSln(ipos)) + &
                                          this%amat(ipos)
      end do
    end do

    ! FC the movers through the exchange; we cannot call
    ! exg_fc() directly because it calculates matrix terms
    if (this%exchangeIsOwned) then
      if (this%gwfExchange%inmvr > 0) then
        call this%gwfExchange%mvr%mvr_fc()
      end if
    end if

  end subroutine gwfgwfcon_fc

  !> @brief Validate this connection
  !! This is called before proceeding to construct
  !! the interface model
  !<
  subroutine validateConnection(this)
    use SimVariablesModule, only: errmsg
    use SimModule, only: count_errors
    class(GwfGwfConnectionType) :: this !< this connection
    ! local

    ! base validation (geometry/spatial)
    call this%SpatialModelConnectionType%validateConnection()
    call this%validateGwfExchange()

    ! abort on errors
    if (count_errors() > 0) then
      write (errmsg, '(1x,a)') 'Errors occurred while processing exchange(s)'
      call ustop()
    end if

  end subroutine validateConnection

  !> @brief Validate the exchange, intercepting those
  !! cases where two models have to be connected with an interface
  !! model, where the individual configurations don't allow this
  !!
  !! Stops with error message on config mismatch
  !<
  subroutine validateGwfExchange(this)
    use SimVariablesModule, only: errmsg
    use SimModule, only: store_error
    use GwfNpfModule, only: GwfNpfType
    class(GwfGwfConnectionType) :: this !< this connection
    ! local
    class(GwfExchangeType), pointer :: gwfEx
    class(*), pointer :: modelPtr
    class(GwfModelType), pointer :: gwfModel1
    class(GwfModelType), pointer :: gwfModel2
    type(GwfBuyType), pointer :: buy1, buy2
    logical(LGP) :: compatible

    gwfEx => this%gwfExchange
    modelPtr => this%gwfExchange%model1
    gwfModel1 => CastAsGwfModel(modelPtr)
    modelPtr => this%gwfExchange%model2
    gwfModel2 => CastAsGwfModel(modelPtr)

    ! GNC not allowed
    if (gwfEx%ingnc /= 0) then
      write (errmsg, '(1x,2a)') 'Ghost node correction not supported '// &
        'with interface model for exchange', &
        trim(gwfEx%name)
      call store_error(errmsg)
    end if

    if ((gwfModel1%inbuy > 0 .and. gwfModel2%inbuy == 0) .or. &
        (gwfModel1%inbuy == 0 .and. gwfModel2%inbuy > 0)) then
      write (errmsg, '(1x,2a)') 'Buoyancy package should be enabled/disabled '// &
        'simultaneously in models connected with the '// &
        'interface model for exchange ', &
        trim(gwfEx%name)
      call store_error(errmsg)

    end if

    if (gwfModel1%inbuy > 0 .and. gwfModel2%inbuy > 0) then
      ! does not work with XT3D
      if (this%iXt3dOnExchange > 0) then
        write (errmsg, '(1x,2a)') 'Connecting models with BUY package not '// &
          'allowed with XT3D enabled on exchange ', &
          trim(gwfEx%name)
        call store_error(errmsg)
      end if

      ! check compatibility of buoyancy
      compatible = .true.
      buy1 => gwfModel1%buy
      buy2 => gwfModel2%buy
      if (buy1%iform /= buy2%iform) compatible = .false.
      if (buy1%denseref /= buy2%denseref) compatible = .false.
      if (buy1%nrhospecies /= buy2%nrhospecies) compatible = .false.
      if (.not. all(buy1%drhodc == buy2%drhodc)) compatible = .false.
      if (.not. all(buy1%crhoref == buy2%crhoref)) compatible = .false.
      if (.not. all(buy1%cauxspeciesname == buy2%cauxspeciesname)) then
        compatible = .false.
      end if

      if (.not. compatible) then
        write (errmsg, '(1x,6a)') 'Buoyancy packages in model ', &
          trim(gwfEx%model1%name), ' and ', &
          trim(gwfEx%model2%name), &
          ' should be equivalent to construct an '// &
          ' interface model for exchange ', &
          trim(gwfEx%name)
        call store_error(errmsg)
      end if

    end if

  end subroutine validateGwfExchange

  !> @brief Deallocate all resources
  !<
  subroutine gwfgwfcon_da(this)
    use KindModule, only: LGP
    class(GwfGwfConnectionType) :: this !< this connection
    ! local
    logical(LGP) :: isOpen

    ! scalars
    call mem_deallocate(this%iXt3dOnExchange)

    call this%gwfInterfaceModel%model_da()
    deallocate (this%gwfInterfaceModel)

    call this%spatialcon_da()

    inquire (this%iout, opened=isOpen)
    if (isOpen) then
      close (this%iout)
    end if

    ! we need to deallocate the baseexchange we own:
    if (this%exchangeIsOwned) then
      call this%gwfExchange%exg_da()
    end if

  end subroutine gwfgwfcon_da

  !> @brief Calculate intra-cell flows
  !! The calculation will be dispatched to the interface
  !! model, and then mapped back to real-world cell ids.
  !<
  subroutine gwfgwfcon_cq(this, icnvg, isuppress_output, isolnid)
    class(GwfGwfConnectionType) :: this !< this connection
    integer(I4B), intent(inout) :: icnvg !< convergence flag
    integer(I4B), intent(in) :: isuppress_output !< suppress output when =1
    integer(I4B), intent(in) :: isolnid !< solution id

    call this%gwfInterfaceModel%model_cq(icnvg, isuppress_output)

    call this%setFlowToExchange()

    !cdl Could we allow GwfExchange to do this instead, using
    !    simvals?
    ! if needed, we add the edge properties to the model's NPF
    ! package for its spdis calculation:
    if (this%gwfModel%npf%icalcspdis == 1) then
      call this%setNpfEdgeProps()
    end if

    ! Add exchange flows to each model flowja diagonal.  This used
    ! to be done in setNpfEdgeProps, but there was a sign issue
    ! and flowja was only updated if icalcspdis was 1 (it should
    ! always be updated.
    if (this%exchangeIsOwned) then
      call this%gwfExchange%gwf_gwf_add_to_flowja()
    end if

  end subroutine gwfgwfcon_cq

  !> @brief Set the flows (flowja from interface model) to the
  !< simvals in the exchange, leaving the budget calcution in there
  subroutine setFlowToExchange(this)
    use InterfaceMapModule
    class(GwfGwfConnectionType) :: this !< this connection
    ! local
    integer(I4B) :: i
    class(GwfExchangeType), pointer :: gwfEx
    type(IndexMapSgnType), pointer :: map

    if (this%exchangeIsOwned) then
      gwfEx => this%gwfExchange
      map => this%interfaceMap%exchange_map(this%interfaceMap%prim_exg_idx)

      ! use (half of) the exchange map in reverse:
      do i = 1, size(map%src_idx)
        if (map%sign(i) < 0) cycle ! simvals is defined from exg%m1 => exg%m2
        gwfEx%simvals(map%src_idx(i)) = &
          this%gwfInterfaceModel%flowja(map%tgt_idx(i))
      end do
    end if

  end subroutine setFlowToExchange

  !> @brief Set flowja as edge properties in the model,
  !< so it can be used for e.g. specific discharge calculation
  subroutine setNpfEdgeProps(this)
    class(GwfGwfConnectionType) :: this !< this connection
    ! local
    integer(I4B) :: n, m, ipos, isym
    integer(I4B) :: nLoc, mLoc, iposLoc
    integer(I4B) :: ihc
    real(DP) :: rrate
    real(DP) :: area
    real(DP) :: satThick
    real(DP) :: nx, ny, nz
    real(DP) :: cx, cy, cz
    real(DP) :: conLen
    real(DP) :: dist
    real(DP) :: cl
    logical :: nozee
    type(ConnectionsType), pointer :: imCon !< interface model connections
    class(GwfNpfType), pointer :: imNpf !< interface model npf package
    class(DisBaseType), pointer :: imDis !< interface model discretization
    type(GlobalCellType), dimension(:), pointer :: toGlobal !< map interface index to global cell

    ! for readability
    imDis => this%gwfInterfaceModel%dis
    imCon => this%gwfInterfaceModel%dis%con
    imNpf => this%gwfInterfaceModel%npf
    toGlobal => this%gridConnection%idxToGlobal

    nozee = .false.
    if (imNpf%ixt3d > 0) then
      nozee = imNpf%xt3d%nozee
    end if

    ! loop over flowja in the interface model and set edge properties
    ! for flows crossing the boundary, and set flowja for internal
    ! flows affected by the connection.
    do n = 1, this%neq
      if (.not. toGlobal(n)%dmodel == this%owner) then
        ! only add flows to own model
        cycle
      end if

      nLoc = toGlobal(n)%index

      do ipos = imCon%ia(n) + 1, imCon%ia(n + 1) - 1
        if (imCon%mask(ipos) < 1) then
          ! skip this connection, it's masked so not determined by us
          cycle
        end if

        m = imCon%ja(ipos)
        mLoc = toGlobal(m)%index

        if (.not. toGlobal(m)%dmodel == this%owner) then
          ! boundary connection, set edge properties
          isym = imCon%jas(ipos)
          ihc = imCon%ihc(isym)
          area = imCon%hwva(isym)
          satThick = imNpf%calcSatThickness(n, m, ihc)
          rrate = this%gwfInterfaceModel%flowja(ipos)

          call imDis%connection_normal(n, m, ihc, nx, ny, nz, ipos)
          call imDis%connection_vector(n, m, nozee, imNpf%sat(n), imNpf%sat(m), &
                                       ihc, cx, cy, cz, conLen)

          if (ihc == 0) then
            ! check if n is below m
            if (nz > 0) rrate = -rrate
          else
            area = area * satThick
          end if

          cl = imCon%cl1(isym)
          if (m < n) then
            cl = imCon%cl2(isym)
          end if
          dist = conLen * cl / (imCon%cl1(isym) + imCon%cl2(isym))
          call this%gwfModel%npf%set_edge_properties(nLoc, ihc, rrate, area, &
                                                     nx, ny, dist)
        else
          ! internal, need to set flowja for n-m
          ! TODO_MJR: should we mask the flowja calculation in the model?
          iposLoc = getCSRIndex(nLoc, mLoc, this%gwfModel%ia, this%gwfModel%ja)

          ! update flowja with correct value
          this%gwfModel%flowja(iposLoc) = this%gwfInterfaceModel%flowja(ipos)
        end if
      end do
    end do

  end subroutine setNpfEdgeProps

  !> @brief Calculate the budget terms for this connection, this is
  !! dispatched to the GWF-GWF exchange
  subroutine gwfgwfcon_bd(this, icnvg, isuppress_output, isolnid)
    class(GwfGwfConnectionType) :: this !< this connection
    integer(I4B), intent(inout) :: icnvg !< convergence flag
    integer(I4B), intent(in) :: isuppress_output !< suppress output when =1
    integer(I4B), intent(in) :: isolnid !< solution id
    ! local

    ! call exchange budget routine, also calls bd
    ! for movers.
    if (this%exchangeIsOwned) then
      call this%gwfExchange%exg_bd(icnvg, isuppress_output, isolnid)
    end if

  end subroutine gwfgwfcon_bd

  !> @brief Write output for exchange (and calls
  !< save on the budget)
  subroutine gwfgwfcon_ot(this)
    class(GwfGwfConnectionType) :: this !< this connection
    ! local

    ! Call exg_ot() here as it handles all output processing
    ! based on gwfExchange%simvals(:), which was correctly
    ! filled from gwfgwfcon
    if (this%exchangeIsOwned) then
      call this%gwfExchange%exg_ot()
    end if

  end subroutine gwfgwfcon_ot

  !> @brief Cast to GwfGwfConnectionType
  !<
  function CastAsGwfGwfConnection(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj !< object to be cast
    class(GwfGwfConnectionType), pointer :: res !< the GwfGwfConnection

    res => null()
    if (.not. associated(obj)) return

    select type (obj)
    class is (GwfGwfConnectionType)
      res => obj
    end select
    return
  end function CastAsGwfGwfConnection

end module GwfGwfConnectionModule

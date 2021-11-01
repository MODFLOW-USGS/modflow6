module GwfGwfConnectionModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO, DONE, DEM6, LENCOMPONENTNAME, LINELENGTH  
  use CsrUtilsModule, only: getCSRIndex
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, mem_checkin  
  use SimModule, only: ustop
  use SparseModule, only:sparsematrix
  use SpatialModelConnectionModule  
  use GwfInterfaceModelModule
  use NumericalModelModule
  use GwfModule, only: GwfModelType
  use GwfGwfExchangeModule, only: GwfExchangeType, GetGwfExchangeFromList
  use GwfNpfModule, only: GwfNpfType, hcond, vcond
  use BaseDisModule, only: DisBaseType
  use ConnectionsModule, only: ConnectionsType
  use TopologyModule, only: GlobalCellType
  
  implicit none
  private

  !> Connecting two GWF models in space, implements NumericalExchangeType
  !! so the solution can used this object to determine the coefficients
  !! for the coupling between two adjacent models.
  !<
  type, public, extends(SpatialModelConnectionType) :: GwfGwfConnectionType

    type(GwfModelType), pointer :: gwfModel => null()                 !< the model for which this connection exists
    type(GwfInterfaceModelType), pointer  :: interfaceModel => null() !< the interface model
    integer(I4B), pointer :: iXt3dOnExchange => null()                !< run XT3D on the interface,
                                                                      !! 0 = don't, 1 = matrix, 2 = rhs

    integer(I4B) :: iout                                              !< the list file for the interface model
    
  contains 
    procedure, pass(this) :: gwfGwfConnection_ctor
    generic, public :: construct => gwfGwfConnection_ctor
    
    ! overriding NumericalExchangeType
    procedure, pass(this) :: exg_ar => gwfgwfcon_ar
    procedure, pass(this) :: exg_df => gwfgwfcon_df 
    procedure, pass(this) :: exg_ac => gwfgwfcon_ac
    procedure, pass(this) :: exg_rp => gwfgwfcon_rp
    procedure, pass(this) :: exg_cf => gwfgwfcon_cf
    procedure, pass(this) :: exg_fc => gwfgwfcon_fc
    procedure, pass(this) :: exg_da => gwfgwfcon_da
    procedure, pass(this) :: exg_cq => gwfgwfcon_cq
    procedure, pass(this) :: exg_bd => gwfgwfcon_bd
    procedure, pass(this) :: exg_ot => gwfgwfcon_ot

    ! overriding 'protected'
    procedure, pass(this) :: validateConnection
    
    ! local stuff
    procedure, pass(this), private :: allocateScalars
    procedure, pass(this), private :: maskOwnerConnections
    procedure, pass(this), private :: syncInterfaceModel
    procedure, pass(this), private :: validateGwfExchange
    procedure, pass(this), private :: setFlowToExchanges
    procedure, pass(this), private :: printExchangeFlow
    
  end type GwfGwfConnectionType

contains
  
  !> @brief Basic construction of the connection
  !<
  subroutine gwfGwfConnection_ctor(this, model)
    use NumericalModelModule, only: NumericalModelType
    use InputOutputModule, only: openfile
    class(GwfGwfConnectionType) :: this         !< the connection
    class(NumericalModelType), pointer :: model !< the model owning this connection, 
                                                !! this must of course be a GwfModelType
    ! local
    character(len=LINELENGTH) :: fname
    character(len=LENCOMPONENTNAME) :: name

    this%gwfModel => CastToGwfModel(model)
    
    if (model%id > 99999) then
      write(*,*) 'Error: running 100000 submodels or more is not yet supported'
      call ustop()
    end if
    write(name,'(a,i5.5)') 'GFC_', model%id

    ! .lst file for interface model
    fname = trim(model%name)//'.im.lst'
    call openfile(this%iout, 0, fname, 'LIST', filstat_opt='REPLACE')
    write(this%iout, '(a,a)') 'Creating GWF-GWF connection for model ',          &
                              trim(this%gwfModel%name)
    
    ! first call base constructor
    call this%SpatialModelConnectionType%spatialConnection_ctor(model, name)
    
    call this%allocateScalars()
    
    this%typename = 'GWF-GWF'
    this%iXt3dOnExchange = 1
    
    allocate(this%interfaceModel)
  
  end subroutine gwfGwfConnection_ctor
      
  !> @brief Define the connection
  !! 
  !! This sets up the GridConnection (for creating the 
  !! interface grid), creates and defines the interface 
  !< model
  subroutine gwfgwfcon_df(this)
    class(GwfGwfConnectionType) :: this !< this connection    
    ! local
    type(sparsematrix) :: sparse
    real(DP) :: satOmega
    character(len=LENCOMPONENTNAME) :: imName !< the interface model's name
        
    satOmega = this%gwfModel%npf%satomega

    if (this%gwfModel%npf%ixt3d > 0) then
      this%internalStencilDepth = 2
    end if
    if (this%iXt3dOnExchange > 0) then
      this%exchangeStencilDepth = 2
    end if

    ! this sets up the GridConnection
    call this%spatialcon_df()
    
    ! Now grid conn is defined, we create the interface model
    ! here, and the remainder of this routine is define.
    ! we basically follow the logic that is present in sln_df()
    write(imName,'(a,i5.5)') 'IFM_', this%gwfModel%id
    call this%interfaceModel%construct(imName, this%iout)
    call this%interfaceModel%createModel(this%gridConnection)
    this%interfaceModel%npf%ixt3d = this%iXt3dOnExchange

    ! define, from here
    call this%interfaceModel%defineModel(satOmega)
     
    ! point x, ibound, and rhs to connection
    this%interfaceModel%x => this%x
    call mem_checkin(this%interfaceModel%x, 'X', this%interfaceModel%memoryPath, 'X', this%memoryPath)
    this%interfaceModel%rhs => this%rhs
    call mem_checkin(this%interfaceModel%rhs, 'RHS', this%interfaceModel%memoryPath, 'RHS', this%memoryPath)
    this%interfaceModel%ibound => this%active
    call mem_checkin(this%interfaceModel%ibound, 'IBOUND', this%interfaceModel%memoryPath, 'IBOUND', this%memoryPath)
    
    ! assign connections, fill ia/ja, map connections (following sln_connect) and mask
    call sparse%init(this%neq, this%neq, 7)
    call this%interfaceModel%model_ac(sparse)
    
    ! create amat from sparse
    call this%createCoefficientMatrix(sparse)
    call sparse%destroy()
    
    ! map connections
    call this%interfaceModel%model_mc(this%ia, this%ja)  
      
    ! mask
    call this%maskOwnerConnections()
    
  end subroutine gwfgwfcon_df
    
  !> @brief Mask the owner's connections
  !!
  !! Determine which connections are handled by the interface model 
  !! (using the connections object in its discretization) and
  !< set their mask to zero for the owning model.
  subroutine maskOwnerConnections(this)
    use CsrUtilsModule, only: getCSRIndex
    class(GwfGwfConnectionType) :: this !< the connection
    ! local
    integer(I4B) :: ipos, n, m, nloc, mloc, csrIdx
    type(ConnectionsType), pointer :: conn
    
    ! set the mask on connections that are calculated by the interface model
    conn => this%interfaceModel%dis%con
    do n = 1, conn%nodes
      ! only for connections internal to the owning model
      if (.not. associated(this%gridConnection%idxToGlobal(n)%model, this%owner)) then
        cycle
      end if      
      nloc = this%gridConnection%idxToGlobal(n)%index
      
      do ipos = conn%ia(n) + 1, conn%ia(n + 1) - 1
        m = conn%ja(ipos)
        if (.not. associated(this%gridConnection%idxToGlobal(m)%model, this%owner)) then
            cycle
        end if
        mloc = this%gridConnection%idxToGlobal(m)%index
        
        if (conn%mask(ipos) > 0) then
          ! calculated by interface model, set local model's mask to zero
          csrIdx = getCSRIndex(nloc, mloc, this%owner%ia, this%owner%ja)          
          if (csrIdx == -1) then
            ! this can only happen with periodic boundary conditions, 
            ! then there is no need to set the mask
            if (this%gridConnection%isPeriodic(nloc, mloc)) cycle
            
            write(*,*) 'Error: cannot find cell connection in global system'
            call ustop()
          end if
          
          call this%owner%dis%con%set_mask(csrIdx, 0)          
        end if
      end do
    end do
    
  end subroutine maskOwnerConnections
  
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
    integer(I4B) :: icell, idx, localIdx
    class(NumericalModelType), pointer :: model
    type(GridConnectionType), pointer :: gc !< pointer to the grid connection    
    integer(I4B) :: iex
    class(GwfExchangeType), pointer :: gwfEx

    ! check if we can construct an interface model
    ! NB: only makes sense after the models' allocate&read have been
    ! called, which is why we do it here
    call this%validateConnection()

    gc => this%gridConnection

    ! init x and ibound with model data
    do icell = 1, gc%nrOfCells     
      idx = gc%idxToGlobal(icell)%index
      model => gc%idxToGlobal(icell)%model
      
      this%interfaceModel%x(icell) = model%x(idx)
      this%interfaceModel%ibound(icell) = model%ibound(idx)      
    end do
    
    ! *_ar
    call this%interfaceModel%allocateAndReadModel()  

    ! fill mapping to global index (which can be
    ! done now because moffset is set in sln_df)
    do localIdx = 1, gc%nrOfCells
      gc%idxToGlobalIdx(localIdx) = gc%idxToGlobal(localIdx)%index +        &
                                    gc%idxToGlobal(localIdx)%model%moffset
    end do

    ! loop over exchanges and AR the movers and obs
    do iex=1, this%localExchanges%Count()
      gwfEx => GetGwfExchangeFromList(this%localExchanges, iex)
      if (associated(gwfEx%gwfmodel1, this%gwfModel)) then
        if (gwfEx%inmvr > 0) then
          call gwfEx%mvr%mvr_ar()
        end if
        if (gwfEx%inobs > 0) then
          call gwfEx%obs%obs_ar()
        end if
      end if
    end do
  end subroutine gwfgwfcon_ar

  !> @brief Read time varying data when required
  !<
  subroutine gwfgwfcon_rp(this)
    use TdisModule, only: readnewdata
    class(GwfGwfConnectionType) :: this !< this connection
    ! local
    integer(I4B) :: iex
    class(GwfExchangeType), pointer :: gwfEx
    
    if (.not. readnewdata) return

    ! loop over exchanges and RP the movers
    do iex=1, this%localExchanges%Count()
      gwfEx => GetGwfExchangeFromList(this%localExchanges, iex)
      if (associated(gwfEx%gwfmodel1, this%gwfModel)) then
        if (gwfEx%inmvr > 0) then
          call gwfEx%mvr%mvr_rp()
        end if
      end if
    end do

    return
  end subroutine gwfgwfcon_rp

  !> @brief Add connections, handled by the interface model,
  !< to the global system's sparse
  subroutine gwfgwfcon_ac(this, sparse)        
    class(GwfGwfConnectionType) :: this         !< this connection
    type(sparsematrix), intent(inout) :: sparse !< sparse matrix to store the connections
    ! local
    integer(I4B) :: n, m, ipos
    integer(I4B) :: nglo, mglo
    
    do n = 1, this%neq
      if (.not. associated(this%gridConnection%idxToGlobal(n)%model, this%owner)) then
        ! only add connections for own model to global matrix
        cycle
      end if
      nglo = this%gridConnection%idxToGlobal(n)%index + this%gridConnection%idxToGlobal(n)%model%moffset
      do ipos = this%ia(n) + 1, this%ia(n+1) - 1
        m = this%ja(ipos)
        mglo = this%gridConnection%idxToGlobal(m)%index + this%gridConnection%idxToGlobal(m)%model%moffset
        
        call sparse%addconnection(nglo, mglo, 1)
      end do
    end do
    
  end subroutine gwfgwfcon_ac
    
  !> @brief Calculate (or adjust) matrix coefficients,
  !! in this case those which are determined or affected
  !< by the connection of a GWF model with its neigbors
  subroutine gwfgwfcon_cf(this, kiter)
    class(GwfGwfConnectionType) :: this !< this connection
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
    
    ! calculate (wetting/drying, saturation)
    call this%interfaceModel%model_cf(kiter)
    
  end subroutine gwfgwfcon_cf
  
  !> @brief Synchronize the interface model
  !! Fills interface model data from the
  !! contributing GWF models, at the iteration
  !< level
  subroutine syncInterfaceModel(this)
    class(GwfGwfConnectionType) :: this !< this connection
    ! local
    integer(I4B) :: icell, idx
    class(NumericalModelType), pointer :: model
    
    ! copy head values
    do icell = 1, this%gridConnection%nrOfCells      
      idx = this%gridConnection%idxToGlobal(icell)%index
      model => this%gridConnection%idxToGlobal(icell)%model
      
      this%x(icell) = model%x(idx)
    end do
  
  end subroutine syncInterfaceModel
  
  !> @brief Write the calculated coefficients into the global 
  !< system matrix and the rhs
  subroutine gwfgwfcon_fc(this, kiter, iasln, amatsln, rhssln, inwtflag)
    class(GwfGwfConnectionType) :: this               !< this connection
    integer(I4B), intent(in) :: kiter                 !< the iteration counter
    integer(I4B), dimension(:), intent(in) :: iasln   !< global system's IA array
    real(DP), dimension(:), intent(inout) :: amatsln  !< global system matrix coefficients
    real(DP), dimension(:), intent(inout) ::rhssln    !< global right-hand-side
    integer(I4B), optional, intent(in) :: inwtflag    !< newton-raphson flag
    ! local
    integer(I4B) :: n, ipos, nglo, iex
    class(GwfExchangeType), pointer :: gwfEx
    
    ! fill (and add to...) coefficients for interface
    call this%interfaceModel%model_fc(kiter, this%amat, this%nja, inwtflag)
    
    ! map back to solution matrix
    do n = 1, this%neq
      ! we cannot check with the mask here, because cross-terms are not
      ! necessarily from primary connections. But, we only need the coefficients
      ! for our own model (i.e. fluxes into cells belonging to this%owner):
      if (.not. associated(this%gridConnection%idxToGlobal(n)%model, this%owner)) then
        ! only add connections for own model to global matrix
        cycle
      end if
      
      nglo = this%gridConnection%idxToGlobal(n)%index + this%gridConnection%idxToGlobal(n)%model%moffset
      rhssln(nglo) = rhssln(nglo) + this%rhs(n)
      
      do ipos = this%ia(n), this%ia(n+1) - 1
        amatsln(this%mapIdxToSln(ipos)) = amatsln(this%mapIdxToSln(ipos)) + this%amat(ipos)
      end do
    end do

    ! loop over exchanges and FC the movers
    do iex=1, this%localExchanges%Count()
      gwfEx => GetGwfExchangeFromList(this%localExchanges, iex)
      if (associated(gwfEx%gwfmodel1, this%gwfModel)) then
        if (gwfEx%inmvr > 0) then
          call gwfEx%mvr%mvr_fc()
        end if
      end if
    end do

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
    integer(I4B) :: iex
    class(GwfExchangeType), pointer :: gwfEx
    
    ! base validation (geometry/spatial)
    call this%SpatialModelConnectionType%validateConnection()

    ! loop over exchanges
    do iex=1, this%localExchanges%Count()
      gwfEx => GetGwfExchangeFromList(this%localExchanges, iex)
      call this%validateGwfExchange(gwfEx)
    end do

    ! abort on errors
    if(count_errors() > 0) then
      write(errmsg, '(1x,a)') 'Errors occurred while processing exchange'
      call ustop()
    end if

  end subroutine validateConnection

  !> @brief Validate the exchange, intercepting those
  !! cases where two models have to be connected with an interface
  !! model, where the individual configurations don't allow this
  !!
  !! Stops with error message on config mismatch
  !<
  subroutine validateGwfExchange(this, exchange)
    use SimVariablesModule, only: errmsg
    use SimModule, only: store_error
    use GwfNpfModule, only: GwfNpfType
    class(GwfGwfConnectionType) :: this !< this connection
    class(GwfExchangeType) :: exchange  !< the GWF-GWF exchange to validate
    ! local
    class(GwfNpfType), pointer :: npf1, npf2

    ! NPF
    npf1 => exchange%gwfmodel1%npf
    npf2 => exchange%gwfmodel2%npf
    if (npf1%iangle1 /= npf2%iangle1 .or. &
        npf1%iangle2 /= npf2%iangle2 .or. &
        npf1%iangle3 /= npf2%iangle3) then
      write(errmsg, '(1x,a,a,a,a,a)') 'Cannot create interface model between ',  &
                                      trim(exchange%gwfmodel1%name), ' and ',    &
                                      trim(exchange%gwfmodel2%name),             &
                                      ', incompatible NPF config (angle)'
      call store_error(errmsg)
    end if
    if (npf1%ik22 /= npf2%ik22 .or. &
        npf1%ik33 /= npf2%ik33) then
      write(errmsg, '(1x,a,a,a,a,a)') 'Cannot create interface model between ',  &
                                      trim(exchange%gwfmodel1%name), ' and ',    &
                                      trim(exchange%gwfmodel2%name),             &
                                      ', incompatible NPF config (k22/k33)'
      call store_error(errmsg)
    end if
    if (npf1%iwetdry /= npf2%iwetdry) then
      write(errmsg, '(1x,a,a,a,a,a)') 'Cannot create interface model between ',  &
                                      trim(exchange%gwfmodel1%name), ' and ',    &
                                      trim(exchange%gwfmodel2%name),             &
                                      ', incompatible NPF config (wetdry)'
      call store_error(errmsg)
    end if

    ! GNC not allowed
    if (exchange%ingnc /= 0) then
      write(errmsg, '(1x,a)') 'Ghost node correction not supported '//           &
                              'for interface model'
      call store_error(errmsg)
    end if

  end subroutine validateGwfExchange

  !> @brief Deallocate all resources
  !<
  subroutine gwfgwfcon_da(this)    
    use KindModule, only: LGP
    class(GwfGwfConnectionType) :: this !< this connection
    ! local
    logical(LGP) :: isOpen    
    integer(I4B) :: iex
    class(GwfExchangeType), pointer :: gwfEx

    call mem_deallocate(this%iXt3dOnExchange)
    
    call this%interfaceModel%model_da()
    deallocate(this%interfaceModel)
    
    call this%spatialcon_da()

    inquire(this%iout, opened=isOpen)
    if (isOpen) then
      close(this%iout)
    end if

    ! we need to deallocate the baseexchange we own:
    do iex=1, this%localExchanges%Count()
      gwfEx => GetGwfExchangeFromList(this%localExchanges, iex)
      if (associated(gwfEx%gwfmodel1, this%gwfModel)) then
        call gwfEx%exg_da()
      end if
    end do
    
  end subroutine gwfgwfcon_da

  !> @brief Calculate intra-cell flows
  !! The calculation will be dispatched to the interface
  !! model, and then mapped back to real-world cell ids.
  !<
  subroutine gwfgwfcon_cq(this, icnvg, isuppress_output, isolnid)
    class(GwfGwfConnectionType) :: this          !< this connection
    integer(I4B), intent(inout) :: icnvg         !< convergence flag
    integer(I4B), intent(in) :: isuppress_output !< suppress output when =1
    integer(I4B), intent(in) :: isolnid          !< solution id
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
    logical :: nozee
    type(ConnectionsType), pointer :: imCon                 !< interface model connections
    class(GwfNpfType), pointer :: imNpf                     !< interface model npf package
    class(DisBaseType), pointer :: imDis                    !< interface model discretization
    type(GlobalCellType), dimension(:), pointer :: toGlobal !< map interface index to global cell

    imDis => this%interfaceModel%dis
    imCon => this%interfaceModel%dis%con
    imNpf => this%interfaceModel%npf
    toGlobal => this%gridConnection%idxToGlobal

    call this%interfaceModel%model_cq(icnvg, isuppress_output)

    if (this%gwfModel%npf%icalcspdis /= 1) return

    nozee = .false.
    if (imNpf%ixt3d > 0) then
      nozee = imNpf%xt3d%nozee
    end if

    ! loop over flowja in the interface model and set edge properties
    ! for flows crossing the boundary, and set flowja for internal
    ! flows affected by the connection.
    do n = 1, this%neq
      if (.not. associated(toGlobal(n)%model, this%owner)) then
        ! only add flows to own model
        cycle
      end if

      nLoc = toGlobal(n)%index

      do ipos = imCon%ia(n)+1, imCon%ia(n+1) - 1
        if (imCon%mask(ipos) < 1) then
          ! skip this connection, it's masked so not determined by us
          cycle
        end if

        m = imCon%ja(ipos)
        mLoc =  toGlobal(m)%index

        if (.not. associated(toGlobal(m)%model, this%owner)) then
          ! boundary connection, set edge properties
          isym = imCon%jas(ipos)
          ihc = imCon%ihc(isym)
          area = imCon%hwva(isym)          
          satThick = imNpf%calcSatThickness(n, m, ihc)
          rrate = this%interfaceModel%flowja(ipos)

          call imDis%connection_normal(n, m, ihc, nx, ny, nz, ipos)   
          call imDis%connection_vector(n, m, nozee, imNpf%sat(n), imNpf%sat(m), &
                                       ihc, cx, cy, cz, conLen)

          if (ihc == 0) then
            ! check if n is below m
            if (nz > 0) rrate = -rrate
          else
            area = area * satThick
          end if

          dist = conLen * imCon%cl1(isym) / (imCon%cl1(isym) + imCon%cl2(isym))
          call this%gwfModel%npf%set_edge_properties(nLoc, ihc, rrate, area,    &
                                                     nx, ny, dist)
          this%gwfModel%flowja(this%gwfModel%ia(nLoc)) =                        &
            this%gwfModel%flowja(this%gwfModel%ia(nLoc)) + rrate
        else
          ! internal, need to set flowja for n-m
          ! TODO_MJR: should we mask the flowja calculation in the model?
          iposLoc = getCSRIndex(nLoc, mLoc, this%gwfModel%ia, this%gwfModel%ja)

          ! update flowja with correct value
          this%gwfModel%flowja(iposLoc) = this%interfaceModel%flowja(ipos)
        end if
      end do
    end do

    call this%setFlowToExchanges()    

  end subroutine gwfgwfcon_cq

  !> @brief Set the flows (flowja from interface model) to the 
  !< simvals in the exchanges, leaving the budget calcution in there
  subroutine setFlowToExchanges(this)
    class(GwfGwfConnectionType) :: this !< this connection
    ! local
    integer(I4B) :: iex, i
    integer(I4B) :: nIface, mIface, ipos
    class(GwfExchangeType), pointer :: gwfEx

    do iex=1, this%localExchanges%Count()
      gwfEx => GetGwfExchangeFromList(this%localExchanges, iex)
      do i = 1, gwfEx%nexg

        gwfEx%simvals(i) = DZERO

        if (gwfEx%gwfmodel1%ibound(gwfEx%nodem1(i)) /= 0 .and.                  &
            gwfEx%gwfmodel2%ibound(gwfEx%nodem2(i)) /= 0) then

          nIface = this%gridConnection%getInterfaceIndex(gwfEx%nodem1(i), gwfEx%model1)
          mIface = this%gridConnection%getInterfaceIndex(gwfEx%nodem2(i), gwfEx%model2)
          ipos = getCSRIndex(nIface, mIface, this%interfaceModel%ia, this%interfaceModel%ja)
          gwfEx%simvals(i) = this%interfaceModel%flowja(ipos)

        end if
      end do
    end do

  end subroutine setFlowToExchanges

  !> @brief Calculate the budget terms for this connection, this is
  !! dispatched to the GWF-GWF exchanges.
  subroutine gwfgwfcon_bd(this, icnvg, isuppress_output, isolnid)
    class(GwfGwfConnectionType) :: this           !< this connection
    integer(I4B), intent(inout) :: icnvg          !< convergence flag
    integer(I4B), intent(in) :: isuppress_output  !< suppress output when =1
    integer(I4B), intent(in) :: isolnid           !< solution id
    ! local
    integer(I4B) :: iex
    class(GwfExchangeType), pointer :: gwfEx

    ! call exchange budget routine, and only call
    ! it once, remember we have 2 interface models
    ! per 1 GWF-GWF exchange. This also calls bd
    ! for movers.
    do iex=1, this%localExchanges%Count()
      gwfEx => GetGwfExchangeFromList(this%localExchanges, iex)
      if (associated(gwfEx%gwfmodel1, this%gwfModel)) then
        call gwfEx%exg_bd(icnvg, isuppress_output, isolnid)
      end if
    end do
    
  end subroutine gwfgwfcon_bd

  !> @brief Write output for exchanges (and calls
  !< save on the budget)
  subroutine gwfgwfcon_ot(this)
    class(GwfGwfConnectionType) :: this           !< this connection
    ! local
    integer(I4B) :: iex
    integer(I4B) :: ibudfl
    class(GwfExchangeType), pointer :: gwfEx
    
    ! we don't call gwf_gwf_ot here, but
    ! we do want to save the budget
    do iex=1, this%localExchanges%Count()
      gwfEx => GetGwfExchangeFromList(this%localExchanges, iex)
      if (associated(gwfEx%gwfmodel1, this%gwfModel)) then
        
        call gwfEx%gwf_gwf_bdsav()
        
        if (gwfEx%iprflow /= 0) then
          call this%printExchangeFlow(gwfEx)
        end if

        if(gwfEx%inmvr > 0) then
          ibudfl = 1
          call gwfEx%mvr%mvr_ot_bdsummary(ibudfl)
        end if
      end if
    end do

  end subroutine gwfgwfcon_ot

  !> @brief Print realized exchanged flow for this GWF-GWF
  !< Exchange to screen
  subroutine printExchangeFlow(this, gwfEx)
    use SimVariablesModule, only: iout
    class(GwfGwfConnectionType) :: this      !< this connection
    class(GwfExchangeType), pointer :: gwfEx !< the exchange for printing
    ! local
    integer(I4B) :: i
    character(len=*), parameter :: fmtheader =                                   &
      "(/1x, 'Exchange rates for connection between models ', a, ' and ', a)"
    character(len=*), parameter :: fmtbody = "(/1x, 2a16, f16.5)"
    character(len=LINELENGTH) :: node1str, node2str

    write(iout, fmtheader) trim(gwfEx%model1%name), trim(gwfEx%model2%name)
    do i = 1, gwfEx%nexg
      call gwfEx%model1%dis%noder_to_string(gwfEx%nodem1(i), node1str)
      call gwfEx%model2%dis%noder_to_string(gwfEx%nodem2(i), node2str)
      write(iout, fmtbody) trim(node1str), trim(node2str), gwfEx%simvals(i)
    end do
    
  end subroutine printExchangeFlow

  
  !> @brief Cast NumericalModelType to GwfModelType
  !<
  function CastToGwfModel(obj) result(gwfmodel)
    use NumericalModelModule, only: NumericalModelType
    class(NumericalModelType), pointer :: obj !< The numerical model to be cast
    class(GwfModelType), pointer :: gwfmodel  !< The GWF model
    
    gwfmodel => null()
    select type(obj)
      type is (GwfModelType)
        gwfmodel => obj
      end select
      
  end function CastToGwfModel
  
end module GwfGwfConnectionModule

module SpatialModelConnectionModule
  use KindModule, only: I4B, DP, LGP
  use SparseModule, only: sparsematrix
  use ConnectionsModule, only: ConnectionsType
  use CsrUtilsModule, only: getCSRIndex
  use SimModule, only: ustop
  use NumericalModelModule, only: NumericalModelType
  use NumericalExchangeModule, only: NumericalExchangeType
  use DisConnExchangeModule, only: DisConnExchangeType, GetDisConnExchangeFromList
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, mem_checkin
  use MemoryHelperModule, only: create_mem_path
  use GridConnectionModule, only: GridConnectionType
  use InterfaceMapModule
  use DistVariableModule
  use DistributedModelModule, only: DistributedModelType, get_dist_model
  use ListModule, only: ListType
  use VectorIntModule, only: VectorInt

  implicit none
  private
  public :: CastAsSpatialModelConnectionClass
  public :: AddSpatialModelConnectionToList
  public :: GetSpatialModelConnectionFromList

  !> Class to manage spatial connection of a model to one
  !! or more models of the same type. Spatial connection here
  !! means that the model domains (spatial discretization) are
  !! adjacent and connected via DisConnExchangeType object(s).
  !! The connection itself is a Numerical Exchange as well,
  !! and part of a Numerical Solution providing the amat and rhs
  !< values for the exchange.
  type, public, extends(NumericalExchangeType) :: SpatialModelConnectionType

    class(NumericalModelType), pointer :: owner => null() !< the model whose connection this is
    class(NumericalModelType), pointer :: interfaceModel => null() !< the interface model
    integer(I4B), pointer :: nrOfConnections => null() !< total nr. of connected cells (primary)

    class(DisConnExchangeType), pointer :: primaryExchange => null() !< the exchange for which the interface model is created
    type(VectorInt), pointer :: haloModels !< models that are potentially in the halo of this interface
    type(VectorInt), pointer :: haloExchanges !< exchanges that are potentially part of the halo of this interface (includes primary)
    integer(I4B), pointer :: internalStencilDepth => null() !< size of the computational stencil for the interior
                                                            !! default = 1, xt3d = 2, ...
    integer(I4B), pointer :: exchangeStencilDepth => null() !< size of the computational stencil at the interface
                                                            !! default = 1, xt3d = 2, ...

    ! The following variables are equivalent to those in Numerical Solution:
    integer(I4B), pointer :: neq => null() !< nr. of equations in matrix system
    integer(I4B), pointer :: nja => null() !< nr. of nonzero matrix elements
    integer(I4B), dimension(:), pointer, contiguous :: ia => null() !< sparse indexing IA
    integer(I4B), dimension(:), pointer, contiguous :: ja => null() !< sparse indexing JA
    real(DP), dimension(:), pointer, contiguous :: amat => null() !< matrix coefficients
    real(DP), dimension(:), pointer, contiguous :: rhs => null() !< rhs of interface system
    real(DP), dimension(:), pointer, contiguous :: x => null() !< dependent variable of interface system
    integer(I4B), dimension(:), pointer, contiguous :: active => null() !< cell status (c.f. ibound) of interface system

    ! these are not in the memory manager
    class(GridConnectionType), pointer :: gridConnection => null() !< facility to build the interface grid connection structure
    integer(I4B), dimension(:), pointer :: mapIdxToSln => null() !< mapping between interface matrix and the solution matrix
    type(ListType) :: ifaceDistVars !< list with distributed variables for this interface
    type(InterfaceMapType), pointer :: interfaceMap => null() !< a map of the interface into models and exchanges

  contains

    ! public
    procedure, pass(this) :: spatialConnection_ctor
    generic :: construct => spatialConnection_ctor

    ! partly overriding NumericalExchangeType:
    procedure :: exg_df => spatialcon_df
    procedure :: exg_ar => spatialcon_ar
    procedure :: exg_ac => spatialcon_ac
    procedure :: exg_mc => spatialcon_mc
    procedure :: exg_da => spatialcon_da

    ! protected
    procedure, pass(this) :: spatialcon_df
    procedure, pass(this) :: spatialcon_ar
    procedure, pass(this) :: spatialcon_ac
    procedure, pass(this) :: spatialcon_da
    procedure, pass(this) :: spatialcon_setmodelptrs
    procedure, pass(this) :: spatialcon_connect
    procedure, pass(this) :: validateConnection
    procedure, pass(this) :: addDistVar
    procedure, pass(this) :: createModelHalo

    ! private
    procedure, private, pass(this) :: setupGridConnection
    procedure, private, pass(this) :: getNrOfConnections
    procedure, private, pass(this) :: allocateScalars
    procedure, private, pass(this) :: allocateArrays
    procedure, private, pass(this) :: createCoefficientMatrix
    procedure, private, pass(this) :: maskOwnerConnections
    procedure, private, pass(this) :: addModelNeighbors

  end type SpatialModelConnectionType

contains ! module procedures

  !> @brief Construct the spatial connection base
  !!
  !! This constructor is typically called from a derived class.
  !<
  subroutine spatialConnection_ctor(this, model, exchange, name)
    class(SpatialModelConnectionType) :: this !< the connection
    class(NumericalModelType), intent(in), pointer :: model !< the model that owns the connection
    class(DisConnExchangeType), intent(in), pointer :: exchange !< the primary exchange from which
                                                                !! the connection is created
    character(len=*), intent(in) :: name !< the connection name (for memory management mostly)

    this%name = name
    this%memoryPath = create_mem_path(this%name)

    this%owner => model
    this%primaryExchange => exchange

    allocate (this%gridConnection)
    allocate (this%haloModels)
    allocate (this%haloExchanges)
    call this%allocateScalars()

    this%internalStencilDepth = 1
    this%exchangeStencilDepth = 1
    this%nrOfConnections = 0

    ! this should be set in derived ctor
    this%interfaceModel => null()

  end subroutine spatialConnection_ctor

  !> @brief Find all models that might participate in this interface
  !<
  subroutine createModelHalo(this, exchanges)
    class(SpatialModelConnectionType) :: this !< this connection
    type(ListType) :: exchanges !< all exchanges in the numerical solution
    ! local
    integer(I4B) :: i
    class(DisConnExchangeType), pointer :: dcx

    ! sanity:
    do i = 1, exchanges%Count()
      dcx => GetDisConnExchangeFromList(exchanges, i)
      if (.not. associated(dcx)) then
        write (*, *) 'Error: global exchange list for connection ', &
                     trim(this%name), ' is corrupt'
        call ustop()
      end if
      if (dcx%typename /= this%typename) then
        write (*, *) 'Error: global exchange list for connection ', &
                     trim(this%name), ' contains wrong type'
        call ustop()
      end if
    end do

    call this%haloModels%init()
    call this%haloExchanges%init()

    call this%addModelNeighbors(this%owner%id, exchanges, &
                                this%exchangeStencilDepth)

  end subroutine createModelHalo

  !> @brief Add neighbors and nbrs-of-nbrs to the model tree
  !<
  recursive subroutine addModelNeighbors(this, dist_model_id, &
                                         global_exchanges, &
                                         depth, mask)
    class(SpatialModelConnectionType) :: this !< this connection
    integer(I4B) :: dist_model_id !< the model (id) to add neighbors for
    type(ListType) :: global_exchanges !< list with all exchanges
    integer(I4B), value :: depth !< the maximal number of exchanges between
    integer(I4B), optional :: mask !< don't add this one as a neighbor
    ! local
    integer(I4B) :: i, n
    class(DisConnExchangeType), pointer :: conn_ex
    integer(I4B) :: neighbor_id
    integer(I4B) :: model_mask
    type(VectorInt) :: nbr_models

    if (.not. present(mask)) then
      model_mask = 0
    else
      model_mask = mask
    end if

    call nbr_models%init()

    ! first find all direct neighbors of the model and add them,
    ! avoiding duplicates
    do i = 1, global_exchanges%Count()
      neighbor_id = -1
      conn_ex => GetDisConnExchangeFromList(global_exchanges, i)
      if (conn_ex%dmodel1%id == dist_model_id) then
        neighbor_id = conn_ex%dmodel2%id
      else if (conn_ex%dmodel2%id == dist_model_id) then
        neighbor_id = conn_ex%dmodel1%id
      end if

      ! check if there is a neighbor, and it is not masked
      ! (to prevent back-and-forth connections)
      if (neighbor_id > 0) then

        ! check if masked
        if (neighbor_id == model_mask) cycle

        if (.not. nbr_models%contains(neighbor_id)) then
          call nbr_models%push_back(neighbor_id)
        end if

        if (.not. this%haloModels%contains(neighbor_id)) then
          call this%haloModels%push_back(neighbor_id)
        end if

        if (.not. this%haloExchanges%contains(conn_ex%id)) then
          call this%haloExchanges%push_back(conn_ex%id)
        end if

      end if
    end do

    depth = depth - 1
    if (depth == 0) then
      call nbr_models%destroy()
      return
    end if
    
    ! now recurse on the neighbors up to the specified depth
    do n = 1, nbr_models%size
      call this%addModelNeighbors(nbr_models%at(n), global_exchanges, &
                                  depth, dist_model_id)
    end do

    call nbr_models%destroy()

  end subroutine addModelNeighbors

  !> @brief Define this connection, mostly sets up the grid
  !< connection, allocates arrays, and links x,rhs, and ibound
  subroutine spatialcon_df(this)
    class(SpatialModelConnectionType) :: this !< this connection
    ! local
    integer(I4B) :: i
    class(DistributedModelType), pointer :: dist_model

    ! create the grid connection data structure
    this%nrOfConnections = this%getNrOfConnections()
    call this%gridConnection%construct(this%owner, &
                                       this%nrOfConnections, &
                                       this%name)
    this%gridConnection%internalStencilDepth = this%internalStencilDepth
    this%gridConnection%exchangeStencilDepth = this%exchangeStencilDepth
    this%gridConnection%haloExchanges => this%haloExchanges
    do i = 1, this%haloModels%size
      dist_model => get_dist_model(this%haloModels%at(i))
      call this%gridConnection%addToRegionalModels(dist_model)
    end do
    call this%setupGridConnection()

    this%neq = this%gridConnection%nrOfCells
    call this%allocateArrays()

  end subroutine spatialcon_df

  !> @brief Allocate the connection,
  !<
  subroutine spatialcon_ar(this)
    class(SpatialModelConnectionType) :: this !< this connection
    ! local
    integer(I4B) :: iface_idx
    class(GridConnectionType), pointer :: gc

    ! fill mapping to global index (which can be
    ! done now because moffset is set in sln_df)
    gc => this%gridConnection
    do iface_idx = 1, gc%nrOfCells
      gc%idxToGlobalIdx(iface_idx) = gc%idxToGlobal(iface_idx)%index + &
                                     gc%idxToGlobal(iface_idx)%dmodel%moffset
    end do

  end subroutine spatialcon_ar

  !> @brief set model pointers to connection
  !<
  subroutine spatialcon_setmodelptrs(this)
    class(SpatialModelConnectionType) :: this !< this connection

    ! point x, ibound, and rhs to connection
    this%interfaceModel%x => this%x
    call mem_checkin(this%interfaceModel%x, 'X', &
                     this%interfaceModel%memoryPath, 'X', &
                     this%memoryPath)
    this%interfaceModel%rhs => this%rhs
    call mem_checkin(this%interfaceModel%rhs, 'RHS', &
                     this%interfaceModel%memoryPath, 'RHS', &
                     this%memoryPath)
    this%interfaceModel%ibound => this%active
    call mem_checkin(this%interfaceModel%ibound, 'IBOUND', &
                     this%interfaceModel%memoryPath, 'IBOUND', &
                     this%memoryPath)

  end subroutine spatialcon_setmodelptrs

  !> @brief map interface model connections to our sparse matrix,
  !< analogously to what happens in sln_connect.
  subroutine spatialcon_connect(this)
    class(SpatialModelConnectionType) :: this !< this connection
    ! local
    type(sparsematrix) :: sparse

    call sparse%init(this%neq, this%neq, 7)
    call this%interfaceModel%model_ac(sparse)

    ! create amat from sparse
    call this%createCoefficientMatrix(sparse)
    call sparse%destroy()

    ! map connections
    call this%interfaceModel%model_mc(this%ia, this%ja)
    call this%maskOwnerConnections()

  end subroutine spatialcon_connect

  !> @brief Mask the owner's connections
  !!
  !! Determine which connections are handled by the interface model
  !! (using the connections object in its discretization) and
  !< set their mask to zero for the owning model.
  subroutine maskOwnerConnections(this)
    use CsrUtilsModule, only: getCSRIndex
    class(SpatialModelConnectionType) :: this !< the connection
    ! local
    integer(I4B) :: ipos, n, m, nloc, mloc, csrIdx
    type(ConnectionsType), pointer :: conn

    ! set the mask on connections that are calculated by the interface model
    conn => this%interfaceModel%dis%con
    do n = 1, conn%nodes
      ! only for connections internal to the owning model
      if (.not. this%gridConnection%idxToGlobal(n)%dmodel == this%owner) then
        cycle
      end if
      nloc = this%gridConnection%idxToGlobal(n)%index

      do ipos = conn%ia(n) + 1, conn%ia(n + 1) - 1
        m = conn%ja(ipos)
        if (.not. this%gridConnection%idxToGlobal(m)%dmodel == this%owner) then
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

            write (*, *) 'Error: cannot find cell connection in global system'
            call ustop()
          end if

          if (this%owner%dis%con%mask(csrIdx) > 0) then
            call this%owner%dis%con%set_mask(csrIdx, 0)
          else
            ! edge case, someone will be calculating this connection
            ! so we ignore it here (TODO_MJR: add name)
            write (*, *) 'Debug: overlap detected, ignoring connection ', &
              nloc, ':', mloc, ' for model ', trim(this%owner%name), &
              ' in Exchange ???'
            call conn%set_mask(ipos, 0)
          end if
        end if
      end do
    end do

  end subroutine maskOwnerConnections

  !> @brief Add connections, handled by the interface model,
  !< to the global system's sparse
  subroutine spatialcon_ac(this, sparse)
    class(SpatialModelConnectionType) :: this !< this connection
    type(sparsematrix), intent(inout) :: sparse !< sparse matrix to store the connections
    ! local
    integer(I4B) :: n, m, ipos
    integer(I4B) :: nglo, mglo

    do n = 1, this%neq
      if (.not. this%gridConnection%idxToGlobal(n)%dmodel == this%owner) then
        ! only add connections for own model to global matrix
        cycle
      end if
      nglo = this%gridConnection%idxToGlobal(n)%index + &
             this%gridConnection%idxToGlobal(n)%dmodel%moffset
      do ipos = this%ia(n) + 1, this%ia(n + 1) - 1
        m = this%ja(ipos)
        mglo = this%gridConnection%idxToGlobal(m)%index + &
               this%gridConnection%idxToGlobal(m)%dmodel%moffset

        call sparse%addconnection(nglo, mglo, 1)
      end do
    end do

  end subroutine spatialcon_ac

  !> @brief Creates the mapping from the local system
  !< matrix to the global one
  subroutine spatialcon_mc(this, iasln, jasln)
    use SimModule, only: ustop
    class(SpatialModelConnectionType) :: this !< this connection
    integer(I4B), dimension(:), intent(in) :: iasln !< global IA array
    integer(I4B), dimension(:), intent(in) :: jasln !< global JA array
    ! local
    integer(I4B) :: m, n, mglo, nglo, ipos, csrIdx
    logical(LGP) :: isOwned

    allocate (this%mapIdxToSln(this%nja))

    do n = 1, this%neq
      isOwned = (this%gridConnection%idxToGlobal(n)%dmodel == this%owner)
      do ipos = this%ia(n), this%ia(n + 1) - 1
        m = this%ja(ipos)
        nglo = this%gridConnection%idxToGlobal(n)%index + &
               this%gridConnection%idxToGlobal(n)%dmodel%moffset
        mglo = this%gridConnection%idxToGlobal(m)%index + &
               this%gridConnection%idxToGlobal(m)%dmodel%moffset
        csrIdx = getCSRIndex(nglo, mglo, iasln, jasln)
        if (csrIdx == -1 .and. isOwned) then
          ! this should not be possible
          write (*, *) 'Error: cannot find cell connection in global system'
          call ustop()
        end if

        this%mapIdxToSln(ipos) = csrIdx
      end do
    end do

  end subroutine spatialcon_mc

  !> @brief Deallocation
  !<
  subroutine spatialcon_da(this)
    class(SpatialModelConnectionType) :: this !< this connection

    call mem_deallocate(this%neq)
    call mem_deallocate(this%nja)
    call mem_deallocate(this%internalStencilDepth)
    call mem_deallocate(this%exchangeStencilDepth)
    call mem_deallocate(this%nrOfConnections)

    call mem_deallocate(this%ia)
    call mem_deallocate(this%ja)
    call mem_deallocate(this%amat)

    call mem_deallocate(this%x)
    call mem_deallocate(this%rhs)
    call mem_deallocate(this%active)

    call this%haloModels%destroy()
    call this%haloExchanges%destroy()
    deallocate (this%haloModels)
    deallocate (this%haloExchanges)

    call this%gridConnection%destroy()
    call this%ifaceDistVars%Clear(destroy=.true.)
    deallocate (this%gridConnection)
    deallocate (this%interfaceMap)
    deallocate (this%mapIdxToSln)

  end subroutine spatialcon_da

  !> @brief Set up the grid connection
  !!
  !! This works in three steps:
  !! 1. set the primary connections
  !! 2. create the topology of connected models, finding
  !!    neighbors of neighboring models when required
  !! 3. extend the interface grid, using that information
  !<
  subroutine setupGridConnection(this)
    class(SpatialModelConnectionType) :: this !< this connection
    ! local

    ! connect cells from primary exchange
    call this%gridConnection%connectPrimaryExchange(this%primaryExchange)
    
    ! now scan for nbr-of-nbrs and create final data structures
    call this%gridConnection%extendConnection()

    ! construct the interface map
    call this%gridConnection%buildInterfaceMap()
    this%interfaceMap => this%gridConnection%interfaceMap

  end subroutine setupGridConnection

  !> @brief Allocation of scalars
  !<
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(SpatialModelConnectionType) :: this !< this connection

    call mem_allocate(this%neq, 'NEQ', this%memoryPath)
    call mem_allocate(this%nja, 'NJA', this%memoryPath)
    call mem_allocate(this%internalStencilDepth, 'INTSTDEPTH', this%memoryPath)
    call mem_allocate(this%exchangeStencilDepth, 'EXGSTDEPTH', this%memoryPath)
    call mem_allocate(this%nrOfConnections, 'NROFCONNS', this%memoryPath)

  end subroutine allocateScalars

  !> @brief Allocation of arrays
  !<
  subroutine allocateArrays(this)
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
    class(SpatialModelConnectionType) :: this !< this connection
    ! local
    integer(I4B) :: i

    call mem_allocate(this%x, this%neq, 'X', this%memoryPath)
    call mem_allocate(this%rhs, this%neq, 'RHS', this%memoryPath)
    call mem_allocate(this%active, this%neq, 'IACTIVE', this%memoryPath)

    ! c.f. NumericalSolution
    do i = 1, this%neq
      this%x(i) = DZERO
      this%active(i) = 1 ! default is active
      this%rhs(i) = DZERO
    end do

  end subroutine allocateArrays

  !> @brief Returns total nr. of primary connections
  !<
  function getNrOfConnections(this) result(nrConns)
    class(SpatialModelConnectionType) :: this !< this connection
    integer(I4B) :: nrConns
    !local

    nrConns = this%primaryExchange%nexg

  end function getNrOfConnections

  !> @brief Create connection's matrix (ia,ja,amat) from sparse
  !<
  subroutine createCoefficientMatrix(this, sparse)
    use SimModule, only: ustop
    class(SpatialModelConnectionType) :: this !< this connection
    type(sparsematrix), intent(inout) :: sparse !< the sparse matrix with the cell connections
    ! local
    integer(I4B) :: ierror

    this%nja = sparse%nnz
    call mem_allocate(this%ia, this%neq + 1, 'IA', this%memoryPath)
    call mem_allocate(this%ja, this%nja, 'JA', this%memoryPath)
    call mem_allocate(this%amat, this%nja, 'AMAT', this%memoryPath)

    call sparse%sort()
    call sparse%filliaja(this%ia, this%ja, ierror)

    if (ierror /= 0) then
      write (*, *) 'Error: cannot fill ia/ja for model connection'
      call ustop()
    end if

  end subroutine createCoefficientMatrix

  !> @brief Validate this connection
  !<
  subroutine validateConnection(this)
    use SimVariablesModule, only: errmsg
    use SimModule, only: store_error
    class(SpatialModelConnectionType) :: this !< this connection
    ! local
    class(DisConnExchangeType), pointer :: conEx => null()

    conEx => this%primaryExchange
    if (conEx%ixt3d > 0) then
      ! if XT3D, we need these angles:
      if (conEx%model1%dis%con%ianglex == 0) then
        write (errmsg, '(1x,a,a,a,a,a)') 'XT3D configured on the exchange ', &
          trim(conEx%name), ' but the discretization in model ', &
          trim(conEx%model1%name), ' has no ANGLDEGX specified'
        call store_error(errmsg)
      end if
      if (conEx%model2%dis%con%ianglex == 0) then
        write (errmsg, '(1x,a,a,a,a,a)') 'XT3D configured on the exchange ', &
          trim(conEx%name), ' but the discretization in model ', &
          trim(conEx%model2%name), ' has no ANGLDEGX specified'
        call store_error(errmsg)
      end if
    end if

  end subroutine validateConnection

  subroutine addDistVar(this, var_name, subcomp_name, map_type, &
                        sync_stages, exg_var_name)
    class(SpatialModelConnectionType) :: this !< this connection
    character(len=*) :: var_name !< name of variable, e.g. "K11"
    character(len=*) :: subcomp_name !< subcomponent, e.g. "NPF"
    integer(I4B) :: map_type !< can be 0 = scalar, 1 = node based, 2 = connection based,
                             !! 3 = exchange based (connections crossing model boundaries)
    integer(I4B), dimension(:) :: sync_stages !< when to sync, e.g. (/ STAGE_AD, STAGE_CF /)
                                              !! which is before AD and CF    
    character(len=*), optional :: exg_var_name !< needed for exchange variables, e.g. SIMVALS
    ! local
    type(DistVarType), pointer :: distVar => null()
    class(*), pointer :: obj

    if (.not. present(exg_var_name)) exg_var_name = ''

    allocate (distVar)
    distVar%var_name = var_name
    distVar%subcomp_name = subcomp_name
    distVar%comp_name = this%interfaceModel%name
    distVar%map_type = map_type
    distVar%sync_stages = sync_stages
    distVar%exg_var_name = exg_var_name

    obj => distVar
    call this%ifaceDistVars%Add(obj)

  end subroutine addDistVar

  !> @brief Cast to SpatialModelConnectionType
  !<
  function CastAsSpatialModelConnectionClass(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj !< object to be cast
    class(SpatialModelConnectionType), pointer :: res !< the instance of SpatialModelConnectionType
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (SpatialModelConnectionType)
      res => obj
    end select
    return
  end function CastAsSpatialModelConnectionClass

  !> @brief Add connection to a list
  !<
  subroutine AddSpatialModelConnectionToList(list, conn)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list !< the list
    class(SpatialModelConnectionType), pointer, intent(in) :: conn !< the connection
    ! -- local
    class(*), pointer :: obj
    !
    obj => conn
    call list%Add(obj)
    !
    return
  end subroutine AddSpatialModelConnectionToList

  !> @brief Get the connection from a list
  !<
  function GetSpatialModelConnectionFromList(list, idx) result(res)
    type(ListType), intent(inout) :: list !< the list
    integer(I4B), intent(in) :: idx !< the index of the connection
    class(SpatialModelConnectionType), pointer :: res !< the returned connection

    ! local
    class(*), pointer :: obj
    obj => list%GetItem(idx)
    res => CastAsSpatialModelConnectionClass(obj)
    !
    return
  end function GetSpatialModelConnectionFromList

end module SpatialModelConnectionModule

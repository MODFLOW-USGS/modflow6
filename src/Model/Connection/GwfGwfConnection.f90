module GwfGwfConnectionModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO, DONE, DEM6, LENCOMPONENTNAME, LINELENGTH
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, mem_checkin  
  use SimModule, only: ustop
  use SparseModule, only:sparsematrix
  use SpatialModelConnectionModule  
  use GwfInterfaceModelModule
  use NumericalModelModule
  use GwfModule, only: GwfModelType
  use GwfNpfModule, only: hcond, vcond
  use ConnectionsModule, only: ConnectionsType
  
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
    procedure, pass(this) :: exg_cf => gwfgwfcon_cf
    procedure, pass(this) :: exg_fc => gwfgwfcon_fc
    procedure, pass(this) :: exg_da => gwfgwfcon_da
    
    ! local stuff
    procedure, pass(this), private :: allocateScalars
    procedure, pass(this), private :: maskConnections
    procedure, pass(this), private :: syncInterfaceModel
    
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
      this%intStencilDepth = 2
    end if
    if (this%iXt3dOnExchange > 0) then
      this%extStencilDepth = 2
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
    call this%maskConnections()
    
  end subroutine gwfgwfcon_df
    
  !> @brief Mask the owner's connections
  !!
  !! Determine which connections are handled by the interface model 
  !! (using the connections object in its discretization) and
  !< set their mask to zero for the owning model.
  subroutine maskConnections(this)
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
    
  end subroutine maskConnections
  
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

    ! TODO_MJR: movers and observations
    
  end subroutine gwfgwfcon_ar
     
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
    integer(I4B) :: n, ipos, nglo
    
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
  end subroutine gwfgwfcon_fc

  !> @brief Deallocate all resources
  !<
  subroutine gwfgwfcon_da(this)    
    class(GwfGwfConnectionType) :: this !< this connection

    call mem_deallocate(this%iXt3dOnExchange)
    
    call this%interfaceModel%model_da()
    deallocate(this%interfaceModel)
    
    call this%spatialcon_da()
    
  end subroutine gwfgwfcon_da
  
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

module GwfGweExchangeModule
  use KindModule,                   only: DP, I4B, LGP
  use ConstantsModule,              only: LENPACKAGENAME
  use ListsModule,                  only: basemodellist, baseexchangelist,      &
                                          baseconnectionlist
  use SimModule,                    only: store_error
  use SimVariablesModule,           only: errmsg
  use BaseExchangeModule,           only: BaseExchangeType, AddBaseExchangeToList
  use SpatialModelConnectionModule, only: SpatialModelConnectionType,           &
                                          GetSpatialModelConnectionFromList
  use GweGweConnectionModule,       only: GweGweConnectionType, CastAsGweGweConnection
  use GwfGwfConnectionModule,       only: GwfGwfConnectionType, CastAsGwfGwfConnection
  use GwfGwfExchangeModule,         only: GwfExchangeType,                      &
                                          GetGwfExchangeFromList
  use BaseModelModule,              only: BaseModelType, GetBaseModelFromList
  use GwfModule,                    only: GwfModelType
  use GweModule,                    only: GweModelType
  use BndModule,                    only: BndType, GetBndFromList

  
  implicit none
  public :: GwfGweExchangeType
  public :: gwfgwe_cr
  
  type, extends(BaseExchangeType) :: GwfGweExchangeType

    integer(I4B), pointer :: m1id => null()
    integer(I4B), pointer :: m2id => null()

  contains
    
    procedure :: exg_df
    procedure :: exg_ar
    procedure :: exg_da
    procedure, private :: set_model_pointers
    procedure, private :: allocate_scalars
    procedure, private :: gwfbnd2gwefmi
    procedure, private :: gwfconn2gweconn
    procedure, private :: link_connections
    
  end type GwfGweExchangeType
  
  contains
  
  subroutine gwfgwe_cr(filename, id, m1id, m2id)
! ******************************************************************************
! gwfgwe_cr -- Create a new GWF to GWE exchange object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    character(len=*), intent(in) :: filename
    integer(I4B), intent(in) :: id
    integer(I4B), intent(in) :: m1id
    integer(I4B), intent(in) :: m2id
    ! -- local
    class(BaseExchangeType), pointer :: baseexchange => null()
    type(GwfGweExchangeType), pointer :: exchange => null()
    character(len=20) :: cint
! ------------------------------------------------------------------------------
    !
    ! -- Create a new exchange and add it to the baseexchangelist container
    allocate(exchange)
    baseexchange => exchange
    call AddBaseExchangeToList(baseexchangelist, baseexchange)
    !
    ! -- Assign id and name
    exchange%id = id
    write(cint, '(i0)') id
    exchange%name = 'GWF-GWE_' // trim(adjustl(cint))
    exchange%memoryPath = exchange%name
    !
    ! -- allocate scalars
    call exchange%allocate_scalars()
    exchange%m1id = m1id
    exchange%m2id = m2id
    !
    ! -- set model pointers
    call exchange%set_model_pointers()
    !
    ! -- return
    return
  end subroutine gwfgwe_cr
  
  subroutine set_model_pointers(this)
! ******************************************************************************
! set_model_pointers -- allocate and read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfGweExchangeType) :: this
    ! -- local
    class(BaseModelType), pointer :: mb => null()
    type(GwfModelType), pointer :: gwfmodel => null()
    type(GweModelType), pointer :: gwemodel => null()
! ------------------------------------------------------------------------------
    !
    ! -- set gwfmodel
    gwfmodel => null()
    mb => GetBaseModelFromList(basemodellist, this%m1id)
    select type (mb)
    type is (GwfModelType)
      gwfmodel => mb
    end select
    !
    ! -- set gwemodel
    gwemodel => null()
    mb => GetBaseModelFromList(basemodellist, this%m2id)
    select type (mb)
    type is (GweModelType)
      gwemodel => mb
    end select
    !
    ! -- Verify that gwf model is of the correct type
    if (.not. associated(gwfmodel)) then
      write(errmsg, '(3a)') 'Problem with GWF-GWE exchange ', trim(this%name), &
        '.  Specified GWF Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Verify that gwe model is of the correct type
    if (.not. associated(gwemodel)) then
      write(errmsg, '(3a)') 'Problem with GWF-GWE exchange ', trim(this%name), &
        '.  Specified GWF Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Tell transport model fmi flows are not read from file
    gwemodel%fmi%flows_from_file = .false.
    !
    ! -- Set a pointer to the GWF bndlist.  This will allow the transport model
    !    to look through the flow packages and establish a link to GWF flows
    gwemodel%fmi%gwfbndlist => gwfmodel%bndlist
    !
    ! -- return
    return
  end subroutine set_model_pointers
  
  subroutine exg_df(this)
! ******************************************************************************
! exg_df -- define
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfGweExchangeType) :: this
    ! -- local
    class(BaseModelType), pointer :: mb => null()
    type(GwfModelType), pointer :: gwfmodel => null()
    type(GweModelType), pointer :: gwemodel => null()
! ------------------------------------------------------------------------------
    !
    !
    ! -- set gwfmodel
    mb => GetBaseModelFromList(basemodellist, this%m1id)
    select type (mb)
    type is (GwfModelType)
      gwfmodel => mb
    end select
    !
    ! -- set gwemodel
    mb => GetBaseModelFromList(basemodellist, this%m2id)
    select type (mb)
    type is (GweModelType)
      gwemodel => mb
    end select
    !
    ! -- Set pointer to flowja
    gwemodel%fmi%gwfflowja => gwfmodel%flowja
    !
    ! -- Set the npf flag so that specific discharge is available for 
    !    transport calculations if dispersion is active
    if (gwemodel%indsp > 0) then
      gwfmodel%npf%icalcspdis = 1
    end if
    !
    ! -- return
    return
  end subroutine exg_df
  
  subroutine exg_ar(this)
! ******************************************************************************
! exg_ar -- 
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfGweExchangeType) :: this
    ! -- local
    class(BaseModelType), pointer :: mb => null()
    type(GwfModelType), pointer :: gwfmodel => null()
    type(GweModelType), pointer :: gwemodel => null()
    ! -- formats
    character(len=*),parameter :: fmtdiserr = &
      "('GWF and GWE Models do not have the same discretization for exchange&
      & ',a,'.&
      &  GWF Model has ', i0, ' user nodes and ', i0, ' reduced nodes.&
      &  GWE Model has ', i0, ' user nodes and ', i0, ' reduced nodes.&
      &  Ensure discretization packages, including IDOMAIN, are identical.')"
! ------------------------------------------------------------------------------
    !
    ! -- set gwfmodel
    mb => GetBaseModelFromList(basemodellist, this%m1id)
    select type (mb)
    type is (GwfModelType)
      gwfmodel => mb
    end select
    !
    ! -- set gwemodel
    mb => GetBaseModelFromList(basemodellist, this%m2id)
    select type (mb)
    type is (GweModelType)
      gwemodel => mb
    end select
    !
    ! -- Check to make sure sizes are identical
    if (gwemodel%dis%nodes /= gwfmodel%dis%nodes .or.&
        gwemodel%dis%nodesuser /= gwfmodel%dis%nodesuser) then
      write(errmsg, fmtdiserr) trim(this%name), &
                               gwfmodel%dis%nodesuser, &
                               gwfmodel%dis%nodes, &
                               gwemodel%dis%nodesuser, &
                               gwemodel%dis%nodes
      call store_error(errmsg, terminate=.TRUE.)
    end if
    !
    ! -- setup pointers to gwf variables allocated in gwf_ar
    gwemodel%fmi%gwfhead   => gwfmodel%x
    gwemodel%fmi%gwfsat    => gwfmodel%npf%sat
    gwemodel%fmi%gwfspdis  => gwfmodel%npf%spdis
    !
    ! -- setup pointers to the flow storage rates. GWF strg arrays are
    !    available after the gwf_ar routine is called.
    if(gwemodel%inmst > 0) then
      if (gwfmodel%insto > 0) then
        gwemodel%fmi%gwfstrgss => gwfmodel%sto%strgss
        gwemodel%fmi%igwfstrgss = 1
        if (gwfmodel%sto%iusesy == 1) then
          gwemodel%fmi%gwfstrgsy => gwfmodel%sto%strgsy
          gwemodel%fmi%igwfstrgsy = 1
        endif
      endif
    endif
    !
    ! -- Set a pointer to conc
    if (gwfmodel%inbuy > 0) then
      call gwfmodel%buy%set_concentration_pointer(gwemodel%name, gwemodel%x, &
                                                  gwemodel%ibound)
    endif
    !
    ! -- transfer the boundary package information from gwf to gwe
    call this%gwfbnd2gwefmi()
    !
    ! -- if mover package is active, then set a pointer to it's budget object
    if (gwfmodel%inmvr /= 0) then
      gwemodel%fmi%mvrbudobj => gwfmodel%mvr%budobj
    end if
    !
    ! -- connect Connections
    call this%gwfconn2gweconn(gwfmodel, gwemodel)
    !
    ! -- return
    return
  end subroutine exg_ar
  
  !> @brief Link GWE connections to GWF connections or exchanges
  !<
  subroutine gwfconn2gweconn(this, gwfModel, gweModel)
    use SimModule, only: store_error
    use SimVariablesModule, only: iout
    class(GwfGweExchangeType) :: this       !< this exchange
    type(GwfModelType), pointer :: gwfModel !< the flow model
    type(GweModelType), pointer :: gweModel !< the transport model
    ! local  
    class(SpatialModelConnectionType), pointer :: conn => null()  
    class(*), pointer :: objPtr => null()
    class(GweGweConnectionType), pointer :: gweConn => null()
    class(GwfGwfConnectionType), pointer :: gwfConn => null()
    class(GwfExchangeType), pointer :: gwfEx => null()
    integer(I4B) :: ic1, ic2, iex
    integer(I4B) :: gwfConnIdx, gwfExIdx
    logical(LGP) :: areEqual

    ! loop over all connections
    gweloop: do ic1 = 1, baseconnectionlist%Count()

      conn => GetSpatialModelConnectionFromList(baseconnectionlist,ic1)
      if (.not. associated(conn%owner, gweModel)) cycle gweloop

      ! start with a GWE conn.
      objPtr => conn
      gweConn => CastAsGweGweConnection(objPtr)
      gwfConnIdx = -1
      gwfExIdx = -1

      ! find matching GWF conn. in same list
      gwfloop: do ic2 = 1, baseconnectionlist%Count()
        conn => GetSpatialModelConnectionFromList(baseconnectionlist,ic2)
        
        if (associated(conn%owner, gwfModel)) then
          objPtr => conn
          gwfConn => CastAsGwfGwfConnection(objPtr)          

          ! for now, connecting the same nodes nrs will be 
          ! sufficient evidence of equality
          areEqual = all(gwfConn%primaryExchange%nodem1 ==                      &
                            gweConn%primaryExchange%nodem1)
          areEqual = areEqual .and. all(gwfConn%primaryExchange%nodem2 ==       &
                            gweConn%primaryExchange%nodem2)
          if (areEqual) then
            ! same DIS, same exchange: link and go to next GWE conn.
            write(iout,'(/6a)') 'Linking exchange ',                           &
                                trim(gweConn%primaryExchange%name),             &
                                ' to ', trim(gwfConn%primaryExchange%name),     &
                                ' (using interface model) for GWE model ',      &
                                trim(gweModel%name)
            gwfConnIdx = ic2
            call this%link_connections(gweConn, gwfConn)
            exit gwfloop
          end if
        end if
      end do gwfloop

      ! fallback option: coupling to old gwfgwf exchange,
      ! (this will go obsolete at some point)
      if (gwfConnIdx == -1) then
        gwfloopexg: do iex = 1, baseexchangelist%Count()
          gwfEx => GetGwfExchangeFromList(baseexchangelist, iex)
          
          ! -- There is no guarantee that iex is a gwfExg, in which case
          !    it will return as null.  cycle if so.
          if (.not. associated(gwfEx)) cycle gwfloopexg

          if (associated(gwfEx%model1, gwfModel) .or.                           &
              associated(gwfEx%model2, gwfModel)) then
            ! again, connecting the same nodes nrs will be 
            ! sufficient evidence of equality
            areEqual = all(gwfEx%nodem1 == gweConn%primaryExchange%nodem1)
            areEqual = areEqual .and.                                           &
                        all(gwfEx%nodem2 == gweConn%primaryExchange%nodem2)
            if (areEqual) then 
              ! link exchange to connection
              write(iout,'(/6a)') 'Linking exchange ',                          &
                                trim(gweConn%primaryExchange%name),             &
                                ' to ', trim(gwfEx%name), ' for GWE model ',    &
                                trim(gweModel%name)
              gwfExIdx = iex
              gweConn%exgflowja => gwfEx%simvals
              
              !cdl link up mvt to mvr
              if (gwfEx%inmvr > 0) then
                if (gweConn%exchangeIsOwned) then
                  !cdl todo: check and make sure gweEx has mvt active
                  call gweConn%gweExchange%mvt%set_pointer_mvrbudobj(gwfEx%mvr%budobj)
                end if
              end if
              
              if (associated(gwfEx%model2, gwfModel)) gweConn%exgflowSign = -1
              gweConn%gweInterfaceModel%fmi%flows_from_file = .false.

              exit gwfloopexg
            end if
          end if


        end do gwfloopexg
      end if

      if (gwfConnIdx == -1 .and. gwfExIdx == -1) then
        ! none found, report
        write(errmsg, '(/6a)') 'Missing GWF-GWF exchange when connecting GWE'// &
            ' model ', trim(gweModel%name), ' with exchange ',                  &
            trim(gweConn%primaryExchange%name), ' to GWF model ',               &
            trim(gwfModel%name)
        call store_error(errmsg, terminate=.true.)
      end if

    end do gweloop

  end subroutine gwfconn2gweconn  


  !> @brief Links a GWE connection to its GWF counterpart
  !<
  subroutine link_connections(this, gweConn, gwfConn)
    class(GwfGweExchangeType) :: this      !< this exchange
    class(GweGweConnectionType), pointer :: gweConn !< GWE connection
    class(GwfGwfConnectionType), pointer :: gwfConn !< GWF connection

    !gweConn%exgflowja => gwfConn%exgflowja
    gweConn%exgflowja => gwfConn%gwfExchange%simvals
    
    !cdl link up mvt to mvr
    if (gwfConn%gwfExchange%inmvr > 0) then
      if (gweConn%exchangeIsOwned) then
        !cdl todo: check and make sure gweEx has mvt active
        call gweConn%gweExchange%mvt%set_pointer_mvrbudobj(gwfConn%gwfExchange%mvr%budobj)
      end if
    end if
    
    if (associated(gwfConn%gwfExchange%model2, gwfConn%owner)) gweConn%exgflowSign = -1

    ! fmi flows are not read from file
    gweConn%gweInterfaceModel%fmi%flows_from_file = .false.

    ! set concentration pointer for buoyancy
    call gwfConn%gwfInterfaceModel%buy%set_concentration_pointer(               &
                        gweConn%gweModel%name,                                  &
                        gweConn%conc,                                           &
                        gweConn%icbound)

  end subroutine link_connections
  
  subroutine exg_da(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwfGweExchangeType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    call mem_deallocate(this%m1id)
    call mem_deallocate(this%m2id)
    !
    ! -- return
    return
  end subroutine exg_da

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfGweExchangeType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    call mem_allocate(this%m1id, 'M1ID', this%memoryPath)
    call mem_allocate(this%m2id, 'M2ID', this%memoryPath)
    this%m1id = 0
    this%m2id = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars

  subroutine gwfbnd2gwefmi(this)
! ******************************************************************************
! gwfbnd2gwefmi
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfGweExchangeType) :: this
    ! -- local
    integer(I4B) :: ngwfpack, ip, iterm, imover
    class(BaseModelType), pointer :: mb => null()
    type(GwfModelType), pointer :: gwfmodel => null()
    type(GweModelType), pointer :: gwemodel => null()
    class(BndType), pointer :: packobj => null()
! ------------------------------------------------------------------------------
    !
    ! -- set gwfmodel
    mb => GetBaseModelFromList(basemodellist, this%m1id)
    select type (mb)
    type is (GwfModelType)
      gwfmodel => mb
    end select
    !
    ! -- set gwemodel
    mb => GetBaseModelFromList(basemodellist, this%m2id)
    select type (mb)
    type is (GweModelType)
      gwemodel => mb
    end select
    !
    ! -- Call routines in FMI that will set pointers to the necessary flow
    !    data (SIMVALS and SIMTOMVR) stored within each GWF flow package
    ngwfpack = gwfmodel%bndlist%Count()
    iterm = 1
    do ip = 1, ngwfpack
      packobj => GetBndFromList(gwfmodel%bndlist, ip)
      call gwemodel%fmi%gwfpackages(iterm)%set_pointers(                       &
                                                        'SIMVALS',             &
                                                         packobj%memoryPath)
      iterm = iterm + 1
      !
      ! -- If a mover is active for this package, then establish a separate
      !    pointer link for the mover flows stored in SIMTOMVR
      imover = packobj%imover
      if (packobj%isadvpak /= 0) imover = 0
      if (imover /= 0) then
        call gwemodel%fmi%gwfpackages(iterm)%set_pointers(                     &
                                                          'SIMTOMVR',          &
                                                          packobj%memoryPath)
        iterm = iterm + 1
      end if
    end do
    !
    ! -- return
    return
  end subroutine gwfbnd2gwefmi

end module GwfGweExchangeModule
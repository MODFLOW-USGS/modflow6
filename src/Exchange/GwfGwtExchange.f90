module GwfGwtExchangeModule
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LENPACKAGENAME
  use ListsModule, only: basemodellist, baseexchangelist, &
                         baseconnectionlist
  use SimModule, only: store_error
  use SimVariablesModule, only: errmsg
  use BaseExchangeModule, only: BaseExchangeType, AddBaseExchangeToList
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                          GetSpatialModelConnectionFromList
  use GwtGwtConnectionModule, only: GwtGwtConnectionType, CastAsGwtGwtConnection
  use GwfGwfConnectionModule, only: GwfGwfConnectionType, CastAsGwfGwfConnection
  use GwfGwfExchangeModule, only: GwfExchangeType, &
                                  GetGwfExchangeFromList
  use BaseModelModule, only: BaseModelType, GetBaseModelFromList
  use GwfModule, only: GwfModelType
  use GwtModule, only: GwtModelType
  use BndModule, only: BndType, GetBndFromList

  implicit none
  public :: GwfGwtExchangeType
  public :: gwfgwt_cr

  type, extends(BaseExchangeType) :: GwfGwtExchangeType

    integer(I4B), pointer :: m1id => null()
    integer(I4B), pointer :: m2id => null()

  contains

    procedure :: exg_df
    procedure :: exg_ar
    procedure :: exg_da
    procedure, private :: set_model_pointers
    procedure, private :: allocate_scalars
    procedure, private :: gwfbnd2gwtfmi
    procedure, private :: gwfconn2gwtconn
    procedure, private :: link_connections

  end type GwfGwtExchangeType

contains

  subroutine gwfgwt_cr(filename, id, m1id, m2id)
! ******************************************************************************
! gwfgwt_cr -- Create a new GWF to GWT exchange object
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
    type(GwfGwtExchangeType), pointer :: exchange => null()
    character(len=20) :: cint
! ------------------------------------------------------------------------------
    !
    ! -- Create a new exchange and add it to the baseexchangelist container
    allocate (exchange)
    baseexchange => exchange
    call AddBaseExchangeToList(baseexchangelist, baseexchange)
    !
    ! -- Assign id and name
    exchange%id = id
    write (cint, '(i0)') id
    exchange%name = 'GWF-GWT_'//trim(adjustl(cint))
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
  end subroutine gwfgwt_cr

  subroutine set_model_pointers(this)
! ******************************************************************************
! set_model_pointers -- allocate and read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfGwtExchangeType) :: this
    ! -- local
    class(BaseModelType), pointer :: mb => null()
    type(GwfModelType), pointer :: gwfmodel => null()
    type(GwtModelType), pointer :: gwtmodel => null()
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
    ! -- set gwtmodel
    gwtmodel => null()
    mb => GetBaseModelFromList(basemodellist, this%m2id)
    select type (mb)
    type is (GwtModelType)
      gwtmodel => mb
    end select
    !
    ! -- Verify that gwf model is of the correct type
    if (.not. associated(gwfmodel)) then
      write (errmsg, '(3a)') 'Problem with GWF-GWT exchange ', trim(this%name), &
        '.  Specified GWF Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Verify that gwt model is of the correct type
    if (.not. associated(gwtmodel)) then
      write (errmsg, '(3a)') 'Problem with GWF-GWT exchange ', trim(this%name), &
        '.  Specified GWF Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Tell transport model fmi flows are not read from file
    gwtmodel%fmi%flows_from_file = .false.
    !
    ! -- Set a pointer to the GWF bndlist.  This will allow the transport model
    !    to look through the flow packages and establish a link to GWF flows
    gwtmodel%fmi%gwfbndlist => gwfmodel%bndlist
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
    use MemoryManagerModule, only: mem_checkin
    ! -- dummy
    class(GwfGwtExchangeType) :: this
    ! -- local
    class(BaseModelType), pointer :: mb => null()
    type(GwfModelType), pointer :: gwfmodel => null()
    type(GwtModelType), pointer :: gwtmodel => null()
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
    ! -- set gwtmodel
    mb => GetBaseModelFromList(basemodellist, this%m2id)
    select type (mb)
    type is (GwtModelType)
      gwtmodel => mb
    end select
    !
    ! -- Check to make sure that flow is solved before transport and in a
    !    different IMS solution
    if (gwfmodel%idsoln >= gwtmodel%idsoln) then
      write (errmsg, '(3a)') 'Problem with GWF-GWT exchange ', trim(this%name), &
        '.  The GWF model must be solved by a different IMS than the GWT model. &
        &Furthermore, the IMS specified for GWF must be listed in mfsim.nam &
        &before the IMS for GWT.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Set pointer to flowja
    gwtmodel%fmi%gwfflowja => gwfmodel%flowja
    call mem_checkin(gwtmodel%fmi%gwfflowja, &
                     'GWFFLOWJA', gwtmodel%fmi%memoryPath, &
                     'FLOWJA', gwfmodel%memoryPath)

    !
    ! -- Set the npf flag so that specific discharge is available for
    !    transport calculations if dispersion is active
    if (gwtmodel%indsp > 0) then
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
    use MemoryManagerModule, only: mem_checkin
    ! -- dummy
    class(GwfGwtExchangeType) :: this
    ! -- local
    class(BaseModelType), pointer :: mb => null()
    type(GwfModelType), pointer :: gwfmodel => null()
    type(GwtModelType), pointer :: gwtmodel => null()
    ! -- formats
    character(len=*), parameter :: fmtdiserr = &
      "('GWF and GWT Models do not have the same discretization for exchange&
      & ',a,'.&
      &  GWF Model has ', i0, ' user nodes and ', i0, ' reduced nodes.&
      &  GWT Model has ', i0, ' user nodes and ', i0, ' reduced nodes.&
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
    ! -- set gwtmodel
    mb => GetBaseModelFromList(basemodellist, this%m2id)
    select type (mb)
    type is (GwtModelType)
      gwtmodel => mb
    end select
    !
    ! -- Check to make sure sizes are identical
    if (gwtmodel%dis%nodes /= gwfmodel%dis%nodes .or. &
        gwtmodel%dis%nodesuser /= gwfmodel%dis%nodesuser) then
      write (errmsg, fmtdiserr) trim(this%name), &
        gwfmodel%dis%nodesuser, &
        gwfmodel%dis%nodes, &
        gwtmodel%dis%nodesuser, &
        gwtmodel%dis%nodes
      call store_error(errmsg, terminate=.TRUE.)
    end if
    !
    ! -- setup pointers to gwf variables allocated in gwf_ar
    gwtmodel%fmi%gwfhead => gwfmodel%x
    call mem_checkin(gwtmodel%fmi%gwfhead, &
                     'GWFHEAD', gwtmodel%fmi%memoryPath, &
                     'X', gwfmodel%memoryPath)
    gwtmodel%fmi%gwfsat => gwfmodel%npf%sat
    call mem_checkin(gwtmodel%fmi%gwfsat, &
                     'GWFSAT', gwtmodel%fmi%memoryPath, &
                     'SAT', gwfmodel%npf%memoryPath)
    gwtmodel%fmi%gwfspdis => gwfmodel%npf%spdis
    call mem_checkin(gwtmodel%fmi%gwfspdis, &
                     'GWFSPDIS', gwtmodel%fmi%memoryPath, &
                     'SPDIS', gwfmodel%npf%memoryPath)
    !
    ! -- setup pointers to the flow storage rates. GWF strg arrays are
    !    available after the gwf_ar routine is called.
    if (gwtmodel%inmst > 0) then
      if (gwfmodel%insto > 0) then
        gwtmodel%fmi%gwfstrgss => gwfmodel%sto%strgss
        gwtmodel%fmi%igwfstrgss = 1
        if (gwfmodel%sto%iusesy == 1) then
          gwtmodel%fmi%gwfstrgsy => gwfmodel%sto%strgsy
          gwtmodel%fmi%igwfstrgsy = 1
        end if
      end if
    end if
    !
    ! -- Set a pointer to conc in buy
    if (gwfmodel%inbuy > 0) then
      call gwfmodel%buy%set_concentration_pointer(gwtmodel%name, gwtmodel%x, &
                                                  gwtmodel%ibound)
    end if
    !
    ! -- Set a pointer to conc (which could be a temperature) in vsc
    if (gwfmodel%invsc > 0) then
      call gwfmodel%vsc%set_concentration_pointer(gwtmodel%name, gwtmodel%x, &
                                                  gwtmodel%ibound)
    end if
    !
    ! -- transfer the boundary package information from gwf to gwt
    call this%gwfbnd2gwtfmi()
    !
    ! -- if mover package is active, then set a pointer to it's budget object
    if (gwfmodel%inmvr /= 0) then
      gwtmodel%fmi%mvrbudobj => gwfmodel%mvr%budobj
    end if
    !
    ! -- connect Connections
    call this%gwfconn2gwtconn(gwfmodel, gwtmodel)
    !
    ! -- return
    return
  end subroutine exg_ar

  !> @brief Link GWT connections to GWF connections or exchanges
  !<
  subroutine gwfconn2gwtconn(this, gwfModel, gwtModel)
    use SimModule, only: store_error
    use SimVariablesModule, only: iout
    use MemoryManagerModule, only: mem_checkin
    class(GwfGwtExchangeType) :: this !< this exchange
    type(GwfModelType), pointer :: gwfModel !< the flow model
    type(GwtModelType), pointer :: gwtModel !< the transport model
    ! local
    class(SpatialModelConnectionType), pointer :: conn => null()
    class(*), pointer :: objPtr => null()
    class(GwtGwtConnectionType), pointer :: gwtConn => null()
    class(GwfGwfConnectionType), pointer :: gwfConn => null()
    class(GwfExchangeType), pointer :: gwfEx => null()
    integer(I4B) :: ic1, ic2, iex
    integer(I4B) :: gwfConnIdx, gwfExIdx
    logical(LGP) :: areEqual

    ! loop over all connections
    gwtloop: do ic1 = 1, baseconnectionlist%Count()

      conn => GetSpatialModelConnectionFromList(baseconnectionlist, ic1)
      if (.not. associated(conn%owner, gwtModel)) cycle gwtloop

      ! start with a GWT conn.
      objPtr => conn
      gwtConn => CastAsGwtGwtConnection(objPtr)
      gwfConnIdx = -1
      gwfExIdx = -1

      ! find matching GWF conn. in same list
      gwfloop: do ic2 = 1, baseconnectionlist%Count()
        conn => GetSpatialModelConnectionFromList(baseconnectionlist, ic2)

        if (associated(conn%owner, gwfModel)) then
          objPtr => conn
          gwfConn => CastAsGwfGwfConnection(objPtr)

          ! for now, connecting the same nodes nrs will be
          ! sufficient evidence of equality
          areEqual = all(gwfConn%primaryExchange%nodem1 == &
                         gwtConn%primaryExchange%nodem1)
          areEqual = areEqual .and. all(gwfConn%primaryExchange%nodem2 == &
                                        gwtConn%primaryExchange%nodem2)
          if (areEqual) then
            ! same DIS, same exchange: link and go to next GWT conn.
            write (iout, '(/6a)') 'Linking exchange ', &
              trim(gwtConn%primaryExchange%name), &
              ' to ', trim(gwfConn%primaryExchange%name), &
              ' (using interface model) for GWT model ', &
              trim(gwtModel%name)
            gwfConnIdx = ic2
            call this%link_connections(gwtConn, gwfConn)
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

          if (associated(gwfEx%model1, gwfModel) .or. &
              associated(gwfEx%model2, gwfModel)) then
            ! again, connecting the same nodes nrs will be
            ! sufficient evidence of equality
            areEqual = all(gwfEx%nodem1 == gwtConn%primaryExchange%nodem1)
            areEqual = areEqual .and. &
                       all(gwfEx%nodem2 == gwtConn%primaryExchange%nodem2)
            if (areEqual) then
              ! link exchange to connection
              write (iout, '(/6a)') 'Linking exchange ', &
                trim(gwtConn%primaryExchange%name), &
                ' to ', trim(gwfEx%name), ' for GWT model ', &
                trim(gwtModel%name)
              gwfExIdx = iex
              if (gwtConn%exchangeIsOwned) then
                gwtConn%gwtExchange%gwfsimvals => gwfEx%simvals
                call mem_checkin(gwtConn%gwtExchange%gwfsimvals, &
                                 'GWFSIMVALS', gwtConn%gwtExchange%memoryPath, &
                                 'SIMVALS', gwfEx%memoryPath)
              end if

              !cdl link up mvt to mvr
              if (gwfEx%inmvr > 0) then
                if (gwtConn%exchangeIsOwned) then
                  !cdl todo: check and make sure gwtEx has mvt active
                  call gwtConn%gwtExchange%mvt%set_pointer_mvrbudobj( &
                    gwfEx%mvr%budobj)
                end if
              end if

              if (associated(gwfEx%model2, gwfModel)) gwtConn%exgflowSign = -1
              gwtConn%gwtInterfaceModel%fmi%flows_from_file = .false.

              exit gwfloopexg
            end if
          end if

        end do gwfloopexg
      end if

      if (gwfConnIdx == -1 .and. gwfExIdx == -1) then
        ! none found, report
        write (errmsg, '(/6a)') 'Missing GWF-GWF exchange when connecting GWT'// &
          ' model ', trim(gwtModel%name), ' with exchange ', &
          trim(gwtConn%primaryExchange%name), ' to GWF model ', &
          trim(gwfModel%name)
        call store_error(errmsg, terminate=.true.)
      end if

    end do gwtloop

  end subroutine gwfconn2gwtconn

  !> @brief Links a GWT connection to its GWF counterpart
  !<
  subroutine link_connections(this, gwtConn, gwfConn)
    use MemoryManagerModule, only: mem_checkin
    class(GwfGwtExchangeType) :: this !< this exchange
    class(GwtGwtConnectionType), pointer :: gwtConn !< GWT connection
    class(GwfGwfConnectionType), pointer :: gwfConn !< GWF connection

    !gwtConn%exgflowja => gwfConn%exgflowja
    if (gwtConn%exchangeIsOwned) then
      gwtConn%gwtExchange%gwfsimvals => gwfConn%gwfExchange%simvals
      call mem_checkin(gwtConn%gwtExchange%gwfsimvals, &
                       'GWFSIMVALS', gwtConn%gwtExchange%memoryPath, &
                       'SIMVALS', gwfConn%gwfExchange%memoryPath)
    end if

    !cdl link up mvt to mvr
    if (gwfConn%gwfExchange%inmvr > 0) then
      if (gwtConn%exchangeIsOwned) then
        !cdl todo: check and make sure gwtEx has mvt active
        call gwtConn%gwtExchange%mvt%set_pointer_mvrbudobj( &
          gwfConn%gwfExchange%mvr%budobj)
      end if
    end if

    if (associated(gwfConn%gwfExchange%model2, gwfConn%owner)) then
      gwtConn%exgflowSign = -1
    end if

    ! fmi flows are not read from file
    gwtConn%gwtInterfaceModel%fmi%flows_from_file = .false.

    ! set concentration pointer for buoyancy
    call gwfConn%gwfInterfaceModel%buy%set_concentration_pointer( &
      gwtConn%gwtModel%name, &
      gwtConn%conc, &
      gwtConn%icbound)

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
    class(GwfGwtExchangeType) :: this
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
    class(GwfGwtExchangeType) :: this
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

  subroutine gwfbnd2gwtfmi(this)
! ******************************************************************************
! gwfbnd2gwtfmi
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfGwtExchangeType) :: this
    ! -- local
    integer(I4B) :: ngwfpack, ip, iterm, imover
    class(BaseModelType), pointer :: mb => null()
    type(GwfModelType), pointer :: gwfmodel => null()
    type(GwtModelType), pointer :: gwtmodel => null()
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
    ! -- set gwtmodel
    mb => GetBaseModelFromList(basemodellist, this%m2id)
    select type (mb)
    type is (GwtModelType)
      gwtmodel => mb
    end select
    !
    ! -- Call routines in FMI that will set pointers to the necessary flow
    !    data (SIMVALS and SIMTOMVR) stored within each GWF flow package
    ngwfpack = gwfmodel%bndlist%Count()
    iterm = 1
    do ip = 1, ngwfpack
      packobj => GetBndFromList(gwfmodel%bndlist, ip)
      call gwtmodel%fmi%gwfpackages(iterm)%set_pointers( &
        'SIMVALS', &
        packobj%memoryPath)
      iterm = iterm + 1
      !
      ! -- If a mover is active for this package, then establish a separate
      !    pointer link for the mover flows stored in SIMTOMVR
      imover = packobj%imover
      if (packobj%isadvpak /= 0) imover = 0
      if (imover /= 0) then
        call gwtmodel%fmi%gwfpackages(iterm)%set_pointers( &
          'SIMTOMVR', &
          packobj%memoryPath)
        iterm = iterm + 1
      end if
    end do
    !
    ! -- return
    return
  end subroutine gwfbnd2gwtfmi

end module GwfGwtExchangeModule

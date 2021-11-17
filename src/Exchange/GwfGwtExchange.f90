module GwfGwtExchangeModule
  use KindModule,              only: DP, I4B
  use ConstantsModule,         only: LENPACKAGENAME
  use ListsModule,                  only: basemodellist, baseexchangelist, baseconnectionlist
  use SimVariablesModule,      only: errmsg
  use BaseExchangeModule,      only: BaseExchangeType, AddBaseExchangeToList
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, GetSpatialModelConnectionFromList
  use GwtGwtConnectionModule,       only: GwtGwtConnectionType
  use GwfGwfConnectionModule,       only: GwfGwfConnectionType
  use BaseModelModule,         only: BaseModelType, GetBaseModelFromList
  use GwfModule,               only: GwfModelType
  use GwtModule,               only: GwtModelType
  use BndModule,               only: BndType, GetBndFromList

  
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
    allocate(exchange)
    baseexchange => exchange
    call AddBaseExchangeToList(baseexchangelist, baseexchange)
    !
    ! -- Assign id and name
    exchange%id = id
    write(cint, '(i0)') id
    exchange%name = 'GWF-GWT_' // trim(adjustl(cint))
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
    ! -- Tell transport model fmi flows are not read from file
    gwtmodel%fmi%flows_from_file = .false.
    !
    ! -- setup pointer to gwf variables that were allocated in gwf_df
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
    ! -- dummy
    class(GwfGwtExchangeType) :: this
    ! -- local
    class(BaseModelType), pointer :: mb => null()
    type(GwfModelType), pointer :: gwfmodel => null()
    type(GwtModelType), pointer :: gwtmodel => null()
    integer(I4B) :: ngwfpack, ip
    class(BndType), pointer :: packobj => null()
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
    ! -- Set pointer to flowja
    gwtmodel%fmi%gwfflowja => gwfmodel%flowja
    !
    ! -- Set the npf flag so that specific discharge is available for 
    !    transport calculations if dispersion is active
    if (gwtmodel%indsp > 0) then
      gwfmodel%npf%icalcspdis = 1
    end if
    !
    ! -- Set the auxiliary names for gwf flow packages in gwt%fmi
    ngwfpack = gwfmodel%bndlist%Count()
    do ip = 1, ngwfpack
      packobj => GetBndFromList(gwfmodel%bndlist, ip)
      call gwtmodel%fmi%gwfpackages(ip)%set_auxname(packobj%naux,              &
                                                    packobj%auxname)
    end do
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
    use SimModule, only: store_error
    ! -- dummy
    class(GwfGwtExchangeType) :: this
    ! -- local
    class(BaseModelType), pointer :: mb => null()
    type(GwfModelType), pointer :: gwfmodel => null()
    type(GwtModelType), pointer :: gwtmodel => null()
    ! -- formats
    character(len=*),parameter :: fmtdiserr = &
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
    if (gwtmodel%dis%nodes /= gwfmodel%dis%nodes .or.&
        gwtmodel%dis%nodesuser /= gwfmodel%dis%nodesuser) then
      write(errmsg, fmtdiserr) trim(this%name), &
                               gwfmodel%dis%nodesuser, &
                               gwfmodel%dis%nodes, &
                               gwtmodel%dis%nodesuser, &
                               gwtmodel%dis%nodes
      call store_error(errmsg, terminate=.TRUE.)
    end if
    !
    ! -- setup pointers to gwf variables allocated in gwf_ar
    gwtmodel%fmi%gwfhead   => gwfmodel%x
    gwtmodel%fmi%gwfsat    => gwfmodel%npf%sat
    gwtmodel%fmi%gwfspdis  => gwfmodel%npf%spdis
    !
    ! -- setup pointers to the flow storage rates. GWF strg arrays are
    !    available after the gwf_ar routine is called.
    if(gwtmodel%inmst > 0) then
      if (gwfmodel%insto > 0) then
        gwtmodel%fmi%gwfstrgss => gwfmodel%sto%strgss
        gwtmodel%fmi%igwfstrgss = 1
        if (gwfmodel%sto%iusesy == 1) then
          gwtmodel%fmi%gwfstrgsy => gwfmodel%sto%strgsy
          gwtmodel%fmi%igwfstrgsy = 1
        endif
      endif
    endif
    !
    ! -- Set a pointer to conc
    if (gwfmodel%inbuy > 0) then
      call gwfmodel%buy%set_concentration_pointer(gwtmodel%name, gwtmodel%x, &
                                                  gwtmodel%ibound)
    endif
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
  
  !> @brief Link GWT connection to GWF connection
  !<
  subroutine gwfconn2gwtconn(this, gwfModel, gwtModel)
    use SimModule, only: store_error
    class(GwfGwtExchangeType) :: this        !< this exchange
    type(GwfModelType), pointer :: gwfModel !< the flow model
    type(GwtModelType), pointer :: gwtModel !< the transport model
    ! local
    class(SpatialModelConnectionType), pointer :: conn => null()    
    class(GwtGwtConnectionType), pointer :: gwtConn => null()
    class(GwfGwfConnectionType), pointer :: gwfConn => null()    
    integer(I4B) :: ic, gwfIdx, gwtIdx

    gwtIdx = -1
    gwfIdx = -1
    do ic = 1, baseconnectionlist%Count()
      conn => GetSpatialModelConnectionFromList(baseconnectionlist,ic)
      if (associated(conn%owner, gwtModel)) then
        gwtIdx = ic
      end if
      if (associated(conn%owner, gwfModel)) then
        gwfIdx = ic
      end if
      if (gwfIdx > 0 .and. gwtIdx > 0) then
        exit
      end if
    end do
    
    if (gwtIdx == -1) then
      ! apparently there is no gwt-gwt exchange, so
      ! no need to link interface to flow counterpart
      return
    end if
    
    if (gwtIdx > 0 .and. gwfIdx == -1) then
      write(errmsg, *) 'Connecting GWT model ', trim(gwtModel%name),           &
                       ' (which has a GWT-GWT exchange) to GWF requires the GWF&
                       & interface model to be active. (Activate by setting the&
                       & environmental variable DEV_ALWAYS_USE_IFMOD=1)'
      call store_error(errmsg, terminate=.true.)
    end if
    
    conn => GetSpatialModelConnectionFromList(baseconnectionlist,gwtIdx)
    select type(conn)
    type is (GwtGwtConnectionType)
      gwtConn => conn
    end select
    conn => GetSpatialModelConnectionFromList(baseconnectionlist,gwfIdx)
    select type(conn)
    type is (GwfGwfConnectionType)
      gwfConn => conn
    end select
    gwtConn%exgflowja => gwfConn%exgflowja

    ! fmi flows are not read from file
    gwtConn%gwtInterfaceModel%fmi%flows_from_file = .false.

  end subroutine gwfconn2gwtconn
  
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
    character (len=LENPACKAGENAME) :: text
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
    ! -- Allocate the gwfpackages in fmi and transfer information
    ngwfpack = gwfmodel%bndlist%Count()
    iterm = 1
    do ip = 1, ngwfpack
      packobj => GetBndFromList(gwfmodel%bndlist, ip)
      call gwtmodel%fmi%gwfpackages(iterm)%set_pointers( &
                           packobj%packName, &
                           packobj%text, &
                           packobj%auxname, &
                           packobj%nbound, &
                           packobj%naux, &
                           packobj%nodelist, &
                           packobj%simvals, &
                           packobj%auxvar)
      iterm = iterm + 1
      !
      ! -- Check in mover if it is active and not an advanced stress
      !    package
      imover = packobj%imover
      if (packobj%isadvpak /= 0) imover = 0
      if (imover /= 0) then
        text = trim(adjustl(packobj%text)) // '-TO-MVR'
        call gwtmodel%fmi%gwfpackages(iterm)%set_pointers( &
                             packobj%packName, &
                             text, &
                             packobj%auxname, &
                             packobj%nbound, &
                             packobj%naux, &
                             packobj%nodelist, &
                             packobj%simtomvr, &
                             packobj%auxvar)
        iterm = iterm + 1
      end if
    end do
    !
    ! -- return
    return
  end subroutine gwfbnd2gwtfmi

end module GwfGwtExchangeModule
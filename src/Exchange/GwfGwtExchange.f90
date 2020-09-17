module GwfGwtExchangeModule
  
  use KindModule,              only: DP, I4B
  use ListsModule,             only: basemodellist, baseexchangelist
  use BaseExchangeModule,      only: BaseExchangeType, AddBaseExchangeToList
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
    integer(I4B) :: ngwfpack, ip
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
    ! -- Tell transport model fmi flows are not read from file
    gwtmodel%fmi%flows_from_file = .false.
    !
    ! -- setup pointer to gwf variables that were allocated in gwf_df
    gwtmodel%fmi%gwfbndlist => gwfmodel%bndlist
    ngwfpack = gwfmodel%bndlist%Count()
    !
    ! -- allocate arrays in fmi of size ngwfpack
    call gwtmodel%fmi%allocate_gwfpackages(ngwfpack)
    !
    ! -- Assign values in the fmi package
    do ip = 1, ngwfpack
      packobj => GetBndFromList(gwfmodel%bndlist, ip)
      call gwtmodel%fmi%gwfpackages(ip)%set_name(packobj%packName)
      gwtmodel%fmi%flowpacknamearray(ip) = packobj%packName
    end do
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
    ! -- return
    return
  end subroutine exg_ar
  
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
    integer(I4B) :: ngwfpack, ip
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
    ! -- Allocate the gwfpackages in fmi and transfer information
    ngwfpack = gwfmodel%bndlist%Count()
    do ip = 1, ngwfpack
      packobj => GetBndFromList(gwfmodel%bndlist, ip)
      call gwtmodel%fmi%gwfpackages(ip)%set_pointers( &
                           packobj%packName, &
                           packobj%auxname, &
                           packobj%nbound, &
                           packobj%naux, &
                           packobj%nodelist, &
                           packobj%hcof, &
                           packobj%rhs, &
                           packobj%auxvar, &
                           packobj%xnew)
    end do
    !
    ! -- return
    return
  end subroutine gwfbnd2gwtfmi

end module GwfGwtExchangeModule
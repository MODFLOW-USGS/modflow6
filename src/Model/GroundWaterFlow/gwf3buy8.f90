module GwfBuyModule
  
  use KindModule,                 only: DP, I4B
  use ConstantsModule,            only: DHALF, DZERO, DONE
  use NumericalPackageModule,     only: NumericalPackageType
  use BaseDisModule,              only: DisBaseType
  use GwfNpfModule,               only: GwfNpfType
  
  implicit none

  private
  public :: GwfBuyType
  public :: buy_cr

  type, extends(NumericalPackageType) :: GwfBuyType
    type(GwfNpfType), pointer                   :: npf        => null()         ! npf object
    integer(I4B), pointer                       :: iform      => null()         ! formulation: 0 freshwater head, 1 hydraulic head, 2 hh rhs
    integer(I4B), pointer                       :: ireadelev  => null()         ! if 1 then elev has been allocated and filled
    integer(I4B), pointer                       :: ireaddense => null()         ! if 1 then dense has been read from input file
    integer(I4B), pointer                       :: iconcset   => null()         ! if 1 then conc is pointed to a gwt model%x
    real(DP), pointer                           :: denseref   => null()         ! reference fluid density
    real(DP), pointer                           :: drhodc     => null()         ! change in density with change in concentration
    real(DP), dimension(:), pointer, contiguous :: dense      => null()         ! density
    real(DP), dimension(:), pointer, contiguous :: elev       => null()         ! cell center elevation (optional; if not specified, hten use (top+bot)/2)
    integer(I4B), dimension(:), pointer         :: ibound     => null()         ! store pointer to ibound
    real(DP), dimension(:), pointer             :: conc       => null()         ! pointer to concentration array
    integer(I4B), dimension(:), pointer         :: icbund     => null()         ! store pointer to gwt ibound array
    integer(I4B), dimension(:), pointer, contiguous :: iauxpak => null() ! aux col for component concentration
  contains    
    procedure :: buy_ar
    procedure :: buy_rp
    procedure :: buy_cf
    procedure :: buy_cf_bnd
    procedure :: buy_fc
    procedure :: buy_flowja
    procedure :: buy_da
    procedure, private :: calcbuy
    procedure, private :: calchhterms
    procedure, private :: buy_calcdens
    procedure, private :: buy_calcelev
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data
    procedure :: set_concentration_pointer
    procedure :: set_iauxpak_pointer
  end type GwfBuyType
  
  contains
  
  function calcdens(denseref, drhodc, conc) result(dense)
! ******************************************************************************
! calcdens -- generic function to calculate fluid density from concentration
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), intent(in) :: denseref
    real(DP), intent(in) :: drhodc
    real(DP), intent(in) :: conc
    ! -- return
    real(DP) :: dense
    ! -- local
! ------------------------------------------------------------------------------
    !
    dense = denseref + drhodc * conc
    !
    ! -- return
    return
  end function calcdens

  subroutine buy_cr(buyobj, name_model, inunit, iout)
! ******************************************************************************
! buy_cr -- Create a new BUY object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwfBuyType), pointer :: buyobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(buyobj)
    !
    ! -- create name and origin
    call buyobj%set_names(1, name_model, 'BUY', 'BUY')
    !
    ! -- Allocate scalars
    call buyobj%allocate_scalars()
    !
    ! -- Set variables
    buyobj%inunit = inunit
    buyobj%iout = iout
    !
    ! -- Initialize block parser
    call buyobj%parser%Initialize(buyobj%inunit, buyobj%iout)
    !
    ! -- Return
    return
  end subroutine buy_cr

  subroutine buy_ar(this, dis, npf, ibound)
! ******************************************************************************
! buy_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(GwfBuyType)                       :: this
    class(DisBaseType), pointer, intent(in) :: dis
    type(GwfNpfType), pointer, intent(in) :: npf
    integer(I4B), dimension(:), pointer     :: ibound
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtbuy =                                    &
      "(1x,/1x,'BUY -- BUOYANCY PACKAGE, VERSION 1, 5/16/2018',                &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the buoyancy package.
    write(this%iout, fmtbuy) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis     => dis
    this%npf     => npf
    this%ibound  => ibound
    !
    ! -- Allocate arrays
    call this%allocate_arrays(dis%nodes)
    !
    ! -- Read storage options
    call this%read_options()
    !
    ! -- Calculate cell elevations
    call this%buy_calcelev()
    !
    ! -- Return
    return
  end subroutine buy_ar

  subroutine buy_rp(this)
! ******************************************************************************
! buy_rp -- Check for new buy period data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    use TdisModule, only: kper, nper
    ! -- dummy
    class(GwfBuyType) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg
    integer(I4B) :: ierr
    logical :: isfound
    ! -- formats
    character(len=*),parameter :: fmtblkerr = &
      "('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*),parameter :: fmtlsp = &
      "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
! ------------------------------------------------------------------------------
    !
    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
    if (this%ionper < kper) then
      !
      ! -- get period block
      call this%parser%GetBlock('PERIOD', isfound, ierr, &
                                supportOpenClose=.true.)
      if(isfound) then
        !
        ! -- read ionper and check for increasing period numbers
        call this%read_check_ionper()
      else
        !
        ! -- PERIOD block not found
        if (ierr < 0) then
          ! -- End of file found; data applies for remainder of simulation.
          this%ionper = nper + 1
        else
          ! -- Found invalid block
          write(errmsg, fmtblkerr) adjustl(trim(line))
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
      endif
    end if
    !
    if(this%ionper == kper) then
      call this%read_data()
    else
      write(this%iout,fmtlsp) 'BUY'
    endif
    !
    ! -- Check to make sure that dense is available from being read or 
    !    from being set as a pointer
    if (this%ireaddense == 0 .and. this%iconcset == 0) then
      write(errmsg, '(a)') '****ERROR. DENSITY NOT SET FOR BUY &
                            &PACKAGE.  A GWF-GWT EXCHANGE MUST BE &
                            &SPECIFIED OR DENSITY MUST BE SPECIFIED &
                            &IN THE BUY PERIOD BLOCK.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine buy_rp

  subroutine buy_cf(this, kiter)
! ******************************************************************************
! buy_cf -- Fill coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfBuyType) :: this
    integer(I4B) :: kiter
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- update density using the last concentration
    call this%buy_calcdens()
    !
    ! -- Calculate the elev array
    if (this%ireadelev == 0) then
      !todo: now calling once at beginning call this%buy_calcelev()
    end if
    !
    ! -- Return
    return
  end subroutine buy_cf
  
  subroutine buy_cf_bnd(this, packobj, hnew) !, hcof, rhs, auxnam, auxvar)
! ******************************************************************************
! buy_cf_bnd -- Fill coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use BndModule, only: BndType
    ! -- dummy
    class(GwfBuyType) :: this
    class(BndType), pointer :: packobj
    real(DP), intent(in), dimension(:) :: hnew
    ! -- local
    integer(I4B) :: n, locdense, locelev
! ------------------------------------------------------------------------------
    !
    ! -- Return if freshwater head formulation
    if (this%iform == 0) return
    !
    ! -- Add buoyancy terms for head-dependent boundaries
    locdense = 0
    locelev = 0
    do n = 1, packobj%naux
      if (packobj%auxname(n) == 'DENSITY') then
        locdense = n
      else if (packobj%auxname(n) == 'ELEVATION') then
        locelev = n
      end if
    end do
    !
    ! -- Add density terms based on boundary package type
    select case (packobj%filtyp)
      case('GHB', 'LAK')
        !
        ! -- general head boundary
        call buy_cf_ghb(packobj, hnew, this%dense, this%elev, this%denseref, &
                        locdense, locelev)
      case('RIV')
        !
        ! -- river
        call buy_cf_riv(packobj, hnew, this%dense, this%elev, this%denseref, &
                        locdense, locelev)
      case('DRN')
        !
        ! drain
        call buy_cf_drn(packobj, hnew, this%dense, this%denseref)
      case default
        !
        ! -- nothing
    end select
    !
    ! -- Return
    return
  end subroutine buy_cf_bnd
  
  subroutine buy_cf_ghb(packobj, hnew, dense, elev, denseref, locdense, locelev)
    ! -- modules
    use BndModule, only: BndType
    class(BndType), pointer :: packobj
    ! -- dummy
    real(DP), intent(in), dimension(:) :: hnew
    real(DP), intent(in), dimension(:) :: dense
    real(DP), intent(in), dimension(:) :: elev
    real(DP), intent(in) :: denseref
    integer(I4B), intent(in) :: locdense
    integer(I4B), intent(in) :: locelev
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: node
    real(DP) :: denseghb
    real(DP) :: elevghb
    real(DP) :: avgdense
    real(DP) :: avgelev
    real(DP) :: hghb
    real(DP) :: cond
    real(DP) :: t1, t2, term
    do n = 1, packobj%nbound
      node = packobj%nodelist(n)
      if (packobj%ibound(node) <= 0) cycle
      !
      ! -- density
      denseghb = denseref
      if (locdense > 0) denseghb = packobj%auxvar(locdense, n)
      avgdense = DHALF * denseghb + DHALF * dense(node)
      !
      ! -- elevation
      elevghb = elev(node)
      if (locelev > 0) elevghb = packobj%auxvar(locelev, n)
      avgelev = DHALF * elevghb + DHALF * elev(node)
      !
      ! -- boundary head and conductance
      hghb = packobj%bound(1, n)
      cond = packobj%bound(2, n)
      !
      ! -- calculate and add terms
      t1 = avgdense / denseref - DONE
      t2 = (denseghb - dense(node)) / denseref
      term = cond * (-t1 + DHALF * t2)
      packobj%hcof(n) = packobj%hcof(n) + term
      term = t1 * hghb + DHALF * t2 * hghb - t2 * avgelev
      term = term * cond
      packobj%rhs(n) = packobj%rhs(n) - term
    end do
  end subroutine buy_cf_ghb
  
  subroutine buy_cf_riv(packobj, hnew, dense, elev, denseref, locdense, locelev)
    ! -- modules
    use BndModule, only: BndType
    class(BndType), pointer :: packobj
    ! -- dummy
    real(DP), intent(in), dimension(:) :: hnew
    real(DP), intent(in), dimension(:) :: dense
    real(DP), intent(in), dimension(:) :: elev
    real(DP), intent(in) :: denseref
    integer(I4B), intent(in) :: locdense
    integer(I4B), intent(in) :: locelev
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: node
    real(DP) :: denseriv
    real(DP) :: elevriv
    real(DP) :: avgdense
    real(DP) :: avgelev
    real(DP) :: hriv
    real(DP) :: rbot
    real(DP) :: cond
    real(DP) :: t1, t2, term
    do n = 1, packobj%nbound
      node = packobj%nodelist(n)
      if (packobj%ibound(node) <= 0) cycle
      !
      ! -- density
      denseriv = denseref
      if (locdense > 0) denseriv = packobj%auxvar(locdense, n)
      avgdense = DHALF * denseriv + DHALF * dense(node)
      !
      ! -- elevation
      elevriv = elev(node)
      if (locelev > 0) elevriv = packobj%auxvar(locelev, n)
      avgelev = DHALF * elevriv + DHALF * elev(node)
      !
      ! -- boundary head and conductance
      hriv = packobj%bound(1, n)
      cond = packobj%bound(2, n)
      rbot = packobj%bound(3, n)
      !
      ! -- calculate and add terms depending on whether head is above rbot
      if (hnew(node) > rbot) then
        t1 = avgdense / denseref - DONE
        t2 = (denseriv - dense(node)) / denseref
        term = cond * (-t1 + DHALF * t2)
        packobj%hcof(n) = packobj%hcof(n) + term
        term = t1 * hriv + DHALF * t2 * hriv - t2 * avgelev
        term = term * cond
        packobj%rhs(n) = packobj%rhs(n) - term
      else
        term = cond * (denseriv / denseref - DONE) * (hriv - rbot)
        packobj%rhs(n) = packobj%rhs(n) - term
      end if
    end do
  end subroutine buy_cf_riv
  
  subroutine buy_cf_drn(packobj, hnew, dense, denseref)
    ! -- modules
    use BndModule, only: BndType
    class(BndType), pointer :: packobj
    ! -- dummy
    real(DP), intent(in), dimension(:) :: hnew
    real(DP), intent(in), dimension(:) :: dense
    real(DP), intent(in) :: denseref
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: node
    real(DP) :: rho
    real(DP) :: hbnd
    real(DP) :: cond
    real(DP) :: hcofterm
    real(DP) :: rhsterm
    do n = 1, packobj%nbound
      node = packobj%nodelist(n)
      if (packobj%ibound(node) <= 0) cycle
      rho = dense(node)
      hbnd = packobj%bound(1, n)
      cond = packobj%bound(2, n)
      if (hnew(node) > hbnd) then
        hcofterm = - cond * (rho  / denseref - DONE)
        rhsterm = hcofterm * hbnd
        packobj%hcof(n) = packobj%hcof(n) + hcofterm
        packobj%rhs(n) = packobj%rhs(n) + rhsterm
      end if
    end do
  end subroutine buy_cf_drn
  
  subroutine buy_fc(this, kiter, njasln, amat, idxglo, rhs, hnew)
! ******************************************************************************
! buy_fc -- Fill coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfBuyType) :: this
    integer(I4B) :: kiter
    integer,intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amat
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), dimension(:), intent(inout) :: rhs
    real(DP), intent(inout), dimension(:) :: hnew
    ! -- local
    integer(I4B) :: n, m, ipos, idiag
    real(DP) :: rhsterm, amatnn, amatnm
! ------------------------------------------------------------------------------
    ! -- initialize
    amatnn = DZERO
    amatnm = DZERO
    !
    ! -- fill buoyancy flow term
    do n = 1, this%dis%nodes
      if(this%ibound(n) == 0) cycle
      idiag = this%dis%con%ia(n)
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        m = this%dis%con%ja(ipos)
        if(this%ibound(m) == 0) cycle
        if(this%iform == 0) then
          call this%calcbuy(n, m, ipos, hnew(n), hnew(m), rhsterm)
        else
          call this%calchhterms(n, m, ipos, hnew(n), hnew(m), rhsterm,         &
                                amatnn, amatnm)
        endif 
        !
        ! -- Add terms to rhs, diagonal, and off diagonal
        rhs(n) = rhs(n) - rhsterm
        amat(idxglo(idiag)) = amat(idxglo(idiag)) - amatnn
        amat(idxglo(ipos)) = amat(idxglo(ipos)) + amatnm
      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine buy_fc

  subroutine buy_flowja(this, hnew, flowja)
! ******************************************************************************
! buy_flowja -- Add buy term to flowja
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(GwfBuyType) :: this
    real(DP),intent(in),dimension(:) :: hnew
    real(DP),intent(inout),dimension(:) :: flowja
    integer(I4B) :: n, m, ipos
    real(DP) :: deltaQ
    real(DP) :: rhsterm, amatnn, amatnm
! ------------------------------------------------------------------------------
    !
    ! -- Calculate the flow across each cell face and store in flowja
    do n=1,this%dis%nodes
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        m = this%dis%con%ja(ipos)
        if(m < n) cycle
        if(this%iform == 0) then
          ! -- equivalent freshwater head formulation
          call this%calcbuy(n, m, ipos, hnew(n), hnew(m), deltaQ)
        else
          ! -- hydraulic head formulation
          call this%calchhterms(n, m, ipos, hnew(n), hnew(m), rhsterm,         &
                                amatnn, amatnm)
          deltaQ = amatnm * hnew(m) - amatnn * hnew(n) + rhsterm
        endif
        flowja(ipos) = flowja(ipos) + deltaQ
        flowja(this%dis%con%isym(ipos)) = flowja(this%dis%con%isym(ipos)) -    &
          deltaQ
      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine buy_flowja
 
  subroutine buy_da(this)
! ******************************************************************************
! buy_da -- Deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwfBuyType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if(this%inunit > 0) then
      call mem_deallocate(this%elev)
      call mem_deallocate(this%dense)
      this%conc => null()
      this%icbund => null()
    endif
    !
    ! -- Scalars
    call mem_deallocate(this%iform)
    call mem_deallocate(this%ireadelev)
    call mem_deallocate(this%ireaddense)
    call mem_deallocate(this%iconcset)
    call mem_deallocate(this%denseref)
    call mem_deallocate(this%drhodc)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine buy_da

  subroutine calcbuy(this, n, m, icon, hn, hm, buy)
! ******************************************************************************
! calcbuy -- Calculate buyancy term for this connection
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwfNpfModule, only: hcond, vcond
    ! -- dummy
    class(GwfBuyType) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: m
    integer(I4B), intent(in) :: icon
    real(DP), intent(in) :: hn
    real(DP), intent(in) :: hm
    real(DP), intent(inout) :: buy
    ! -- local
    integer(I4B) :: ihc
    real(DP) :: densen, densem, cl1, cl2, avgdense, wt, elevn, elevm, &
                     cond, tp, bt
    real(DP) :: hyn
    real(DP) :: hym
! ------------------------------------------------------------------------------
    !
    ! -- Average density
    densen = this%dense(n)
    densem = this%dense(m)
    if (m > n) then
      cl1 = this%dis%con%cl1(this%dis%con%jas(icon))
      cl2 = this%dis%con%cl2(this%dis%con%jas(icon))
    else
      cl1 = this%dis%con%cl2(this%dis%con%jas(icon))
      cl2 = this%dis%con%cl1(this%dis%con%jas(icon))
    end if
    wt =  cl1 / (cl1 + cl2)
    avgdense = wt * densen + (DONE - wt) * densem
    !
    ! -- Elevations
    if(this%ireadelev == 0) then
      tp = this%dis%top(n)
      bt = this%dis%bot(n)
      elevn = bt + DHALF * this%npf%sat(n) * (tp - bt)
      tp = this%dis%top(m)
      bt = this%dis%bot(m)
      elevm = bt + DHALF * this%npf%sat(m) * (tp - bt)
    else
      elevn = this%elev(n)
      elevm = this%elev(m)
    endif
    !
    ihc = this%dis%con%ihc(this%dis%con%jas(icon))
    hyn = this%npf%hy_eff(n, m, ihc, ipos=icon)
    hym = this%npf%hy_eff(m, n, ihc, ipos=icon)
    !
    ! -- Conductance
    if(this%dis%con%ihc(this%dis%con%jas(icon)) == 0) then
      cond = vcond(this%ibound(n), this%ibound(m),                             &
                      this%npf%icelltype(n), this%npf%icelltype(m),            &
                      this%npf%inewton,                                        &
                      this%npf%ivarcv, this%npf%idewatcv,                      &
                      this%npf%condsat(this%dis%con%jas(icon)), hn, hm,        &
                      hyn, hym,                                                &
                      this%npf%sat(n), this%npf%sat(m),                        &
                      this%dis%top(n), this%dis%top(m),                        &
                      this%dis%bot(n), this%dis%bot(m),                        &
                      this%dis%con%hwva(this%dis%con%jas(icon)))
    else
      cond = hcond(this%ibound(n), this%ibound(m),                             &
                     this%npf%icelltype(n), this%npf%icelltype(m),             &
                     this%npf%inewton, this%npf%inewton,                       &
                     this%dis%con%ihc(this%dis%con%jas(icon)),                 &
                     this%npf%icellavg, this%npf%iusgnrhc, this%npf%inwtupw,   &
                     this%npf%condsat(this%dis%con%jas(icon)),                 &
                     hn, hm, this%npf%sat(n), this%npf%sat(m),                 &
                     hyn, hym,                                                 &
                     this%dis%top(n), this%dis%top(m),                         &
                     this%dis%bot(n), this%dis%bot(m),                         &
                     this%dis%con%cl1(this%dis%con%jas(icon)),                 &
                     this%dis%con%cl2(this%dis%con%jas(icon)),                 &
                     this%dis%con%hwva(this%dis%con%jas(icon)),                &
                     this%npf%satomega, this%npf%satmin)
    endif
    !
    ! -- Calculate buoyancy term
    buy = cond * (avgdense - this%denseref) / this%denseref * (elevm - elevn)
    !
    ! -- Return
    return
  end subroutine calcbuy


  subroutine calchhterms(this, n, m, icon, hn, hm, rhsterm, amatnn, amatnm)
! ******************************************************************************
! calchhterms -- Calculate hydraulic head term for this connection
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwfNpfModule, only: hcond, vcond
    ! -- dummy
    class(GwfBuyType) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: m
    integer(I4B), intent(in) :: icon
    real(DP), intent(in) :: hn
    real(DP), intent(in) :: hm
    real(DP), intent(inout) :: rhsterm
    real(DP), intent(inout) :: amatnn
    real(DP), intent(inout) :: amatnm
    ! -- local
    integer(I4B) :: ihc
    real(DP) :: densen, densem, cl1, cl2, avgdense, wt, elevn, elevm, cond
    real(DP) :: rhonormn, rhonormm
    real(DP) :: rhoterm
    real(DP) :: elevnm
    real(DP) :: hphi
    real(DP) :: hyn
    real(DP) :: hym
! ------------------------------------------------------------------------------
    !
    ! -- Average density
    densen = this%dense(n)
    densem = this%dense(m)
    if (m > n) then
      cl1 = this%dis%con%cl1(this%dis%con%jas(icon))
      cl2 = this%dis%con%cl2(this%dis%con%jas(icon))
    else
      cl1 = this%dis%con%cl2(this%dis%con%jas(icon))
      cl2 = this%dis%con%cl1(this%dis%con%jas(icon))
    end if
    wt =  cl1 / (cl1 + cl2)
    avgdense = wt * densen + (1.0 - wt) * densem
    !
    ! -- Elevations
    elevn = this%elev(n)
    elevm = this%elev(m)
    elevnm = (DONE - wt) * elevn + wt * elevm
    !
    ihc = this%dis%con%ihc(this%dis%con%jas(icon))
    hyn = this%npf%hy_eff(n, m, ihc, ipos=icon)
    hym = this%npf%hy_eff(m, n, ihc, ipos=icon)
    !
    ! -- Conductance
    if(ihc == 0) then
      cond = vcond(this%ibound(n), this%ibound(m),                             &
                      this%npf%icelltype(n), this%npf%icelltype(m),            &
                      this%npf%inewton,                                        &
                      this%npf%ivarcv, this%npf%idewatcv,                      &
                      this%npf%condsat(this%dis%con%jas(icon)), hn, hm,        &
                      hyn, hym,                                                &
                      this%npf%sat(n), this%npf%sat(m),                        &
                      this%dis%top(n), this%dis%top(m),                        &
                      this%dis%bot(n), this%dis%bot(m),                        &
                      this%dis%con%hwva(this%dis%con%jas(icon)))
    else
      cond = hcond(this%ibound(n), this%ibound(m),                             &
                     this%npf%icelltype(n), this%npf%icelltype(m),             &
                     this%npf%inewton, this%npf%inewton,                       &
                     this%dis%con%ihc(this%dis%con%jas(icon)),                 &
                     this%npf%icellavg, this%npf%iusgnrhc, this%npf%inwtupw,   &
                     this%npf%condsat(this%dis%con%jas(icon)),                 &
                     hn, hm, this%npf%sat(n), this%npf%sat(m),                 &
                     hyn, hym,                                                 &
                     this%dis%top(n), this%dis%top(m),                         &
                     this%dis%bot(n), this%dis%bot(m),                         &
                     this%dis%con%cl1(this%dis%con%jas(icon)),                 &
                     this%dis%con%cl2(this%dis%con%jas(icon)),                 &
                     this%dis%con%hwva(this%dis%con%jas(icon)),                &
                     this%npf%satomega, this%npf%satmin)
    endif
    !
    ! -- Calculate terms
    rhonormn = densen / this%denseref
    rhonormm = densem / this%denseref
    rhoterm = wt * rhonormn + (DONE - wt) * rhonormm
    amatnn = cond * (rhoterm - DONE)
    amatnm = amatnn
    rhsterm = -cond * (rhonormm - rhonormn) * elevnm
    if (this%iform == 1) then
      ! -- rhs (lag the h terms and keep matrix symmetric)
      hphi = (DONE - wt) * hn + wt * hm
      rhsterm = rhsterm + cond * hphi * (rhonormm - rhonormn)
    else
      ! -- lhs, results in asymmetric matrix due to weight term
      amatnn = amatnn - cond * (DONE - wt) * (rhonormm - rhonormn)
      amatnm = amatnm + cond * wt * (rhonormm - rhonormn)
    endif
    !
    ! -- Return
    return
  end subroutine calchhterms

  subroutine buy_calcdens(this)
! ******************************************************************************
! buy_calcdens -- calculate fluid density from concentration
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfBuyType) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Calculate the density using the specified concentration array
    if (this%ireaddense == 0) then
      do n = 1, this%dis%nodes
        if(this%icbund(n) == 0) then
          this%dense(n) = this%conc(n)  !set to cinact
        else
          this%dense(n) = calcdens(this%denseref, this%drhodc, this%conc(n))
        endif
      enddo
    endif
    !
    ! -- Return
    return
  end subroutine buy_calcdens
  
  subroutine buy_calcelev(this)
! ******************************************************************************
! buy_calcelev -- Calculate cell elevations to use in density flow equations
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfBuyType) :: this
    ! -- local
    integer(I4B) :: n
    real(DP) :: tp, bt, frac
! ------------------------------------------------------------------------------
    !
    ! -- Calculate the elev array
    do n = 1, this%dis%nodes
      tp = this%dis%top(n)
      bt = this%dis%bot(n)
      frac = this%npf%sat(n)
      this%elev(n) = bt + DHALF * frac * (tp - bt)
    end do
    !
    ! -- Return
    return
  end subroutine buy_calcelev
  
  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwfBuyType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%iform, 'IFORM', this%origin)
    call mem_allocate(this%ireadelev, 'IREADELEV', this%origin)
    call mem_allocate(this%ireaddense, 'IREADDENSE', this%origin)
    call mem_allocate(this%iconcset, 'ICONCSET', this%origin)
    call mem_allocate(this%denseref, 'DENSEREF', this%origin)
    call mem_allocate(this%drhodc, 'DRHODC', this%origin)
    !
    ! -- Initialize
    this%ireadelev = 0
    this%iconcset = 0
    this%ireaddense = 0
    this%denseref = 1000.d0
    this%drhodc = 0.7d0
    !
    ! -- Initialize default to LHS implementation of hydraulic head formulation
    this%iform = 2
    this%iasym = 1
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this, nodes)
! ******************************************************************************
! allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfBuyType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- Allocate
    call mem_allocate(this%dense, nodes, 'DENSE', this%origin)
    call mem_allocate(this%elev, nodes, 'ELEV', this%origin)
    !
    ! -- Initialize
    do i = 1, nodes
      this%dense(i) = this%denseref
      this%elev(i) = DZERO
    enddo
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine read_options(this)
! ******************************************************************************
! read_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error
    ! -- dummy
    class(GwfBuyType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING BUY OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('HHFORMULATION_RHS')
            this%iform = 1
            this%iasym = 0
            write(this%iout,'(4x,a)') &
              'HYDDRAULIC HEAD FORMULATION SET TO RIGHT-HAND SIDE'
          case ('DENSEREF')
            this%denseref = this%parser%GetDouble()
            write(this%iout, '(4x,a,1pg15.6)')                                 &
                             'REFERENCE DENSITY HAS BEEN SET TO: ',  &
                             this%denseref
          case ('DRHODC')
            this%drhodc = this%parser%GetDouble()
            write(this%iout, '(4x,a,1pg15.6)')                                 &
                             'DRHODC HAS BEEN SET TO: ',  &
                             this%drhodc
          case ('DEV_EFH_FORMULATION')
            call this%parser%DevOpt()
            this%iform = 0
            this%iasym = 0
            write(this%iout,'(4x,a)') &
              'FORMULATION SET TO EQUIVALENT FRESHWATER HEAD'
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN BUY OPTION: ',         &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF BUY OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_data(this)
! ******************************************************************************
! read_data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error
    ! -- dummy
    class(GwfBuyType) :: this
    ! -- local
    character(len=LINELENGTH) :: line, keyword
    integer(I4B) :: istart, istop, lloc
    logical :: endOfBlock
    character(len=24) :: aname(2)
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Setup the label
    aname(1) = '       ELEVATION'
    aname(2) = '           DENSE'
    !
    do
      call this%parser%GetNextLine(endOfBlock)
      if (endOfBlock) exit
      call this%parser%GetStringCaps(keyword)
      call this%parser%GetRemainingLine(line)
      !
      ! -- Parse the keywords
      lloc = 1
      select case (keyword)
      case ('ELEVATION')
        this%ireadelev = 1
        call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,    &
                                      this%parser%iuactive, this%elev,         &
                                      aname(1))
      case ('DENSE')
        this%ireaddense = 1
        call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,    &
                                      this%parser%iuactive, this%dense,        &
                                      aname(2))
      end select
    end do
    !
    ! -- Return
    return
  end subroutine read_data

  subroutine set_concentration_pointer(this, conc, icbund)
! ******************************************************************************
! set_concentration_pointer
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfBuyType) :: this
    real(DP), dimension(:), pointer :: conc
    integer(I4B), dimension(:), pointer :: icbund
    ! -- local
! ------------------------------------------------------------------------------
    !
    this%iconcset = 1
    this%conc => conc
    this%icbund => icbund
    !
    ! -- Return
    return
  end subroutine set_concentration_pointer
  
  subroutine set_iauxpak_pointer(this, iauxpak)
! ******************************************************************************
! set_concentration_pointer
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfBuyType) :: this
    integer(I4B), dimension(:), pointer, contiguous :: iauxpak
    ! -- local
! ------------------------------------------------------------------------------
    !
    this%iauxpak => iauxpak
    !
    ! -- Return
    return
  end subroutine set_iauxpak_pointer
  
end module GwfBuyModule
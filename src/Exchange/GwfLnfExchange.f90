module GwfLnfExchangeModule

  use KindModule, only: DP, I4B
  use ArrayHandlersModule,     only: ExpandArray
  use BaseModelModule,         only: GetBaseModelFromList
  use BaseExchangeModule,      only: BaseExchangeType, AddBaseExchangeToList
  use ConstantsModule,         only: LENBOUNDNAME, NAMEDBOUNDFLAG, LINELENGTH, &
                                     TABCENTER, TABLEFT
  use ListsModule,             only: basemodellist
  use NumericalExchangeModule, only: NumericalExchangeType
  use NumericalModelModule,    only: NumericalModelType
  use GwfModule,               only: GwfModelType
  use LnfModule,               only: LnfModelType
  use GhostNodeModule,         only: GhostNodeType
  use GwfMvrModule,            only: GwfMvrType
  use ObserveModule,           only: ObserveType
  use ObsModule,               only: ObsType
  use SimModule,               only: count_errors, store_error,                &
                                     store_error_unit, ustop
  use BlockParserModule,       only: BlockParserType
  use TableModule,             only: TableType, table_cr

  implicit none

  private
  public :: gwflnfexchange_create

  type, extends(NumericalExchangeType) :: GwfLnfExchangeType
    type(GwfModelType), pointer                      :: gwfmodel   => null()     ! pointer to GWF Model
    type(LnfModelType), pointer                      :: Lnfmodel   => null()     ! pointer to Lnf Model
    integer(I4B), pointer                            :: inewton     => null()    ! newton flag (1 newton is on)
    integer(I4B), pointer                            :: iflowdrycell => null()   ! global index for flow to dry cell correction (0: no correction; 1: on)
    integer(I4B), pointer                            :: icellavg    => null()    ! cell averaging
    integer(I4B), pointer                            :: ivarcv      => null()    ! variable cv
    integer(I4B), pointer                            :: idewatcv    => null()    ! dewatered cv
    integer(I4B), pointer                            :: ianglex     => null()    ! flag indicating anglex was read, if read, ianglex is index in auxvar
    integer(I4B), pointer                            :: icdist      => null()    ! flag indicating cdist was read, if read, icdist is index in auxvar
    integer(I4B), pointer                            :: inamedbound => null()    ! flag to read boundnames
    real(DP), pointer                                :: satomega    => null()    ! saturation smoothing
    integer(I4B), dimension(:), pointer, contiguous  :: ifdc        => null()    ! 
    integer(I4B), dimension(:), pointer, contiguous  :: icelltype   => null()    ! 
    integer(I4B), dimension(:), pointer, contiguous  :: iflowtype   => null()    ! 
    integer(I4B), dimension(:), pointer, contiguous  :: ihc         => null()    ! horizontal connection indicator array
    real(DP), dimension(:), pointer, contiguous      :: condsat     => null()    ! saturated conductance
    real(DP), dimension(:), pointer, contiguous      :: cl1         => null()    ! connection length 1
    real(DP), dimension(:), pointer, contiguous      :: cl2         => null()    ! connection length 2
    real(DP), dimension(:), pointer, contiguous      :: fahl         => null()   ! connection area
    real(DP), dimension(:), pointer, contiguous      :: akrc        => null()    ! stores upstream relative K of connection
    real(DP), dimension(:), pointer, contiguous      :: flengw      => null()    ! length of lnf cell that is connected to gwf cell
    real(DP), dimension(:), pointer, contiguous      :: fskin1      => null()    ! skin factor 1
    real(DP), dimension(:), pointer, contiguous      :: fskin2      => null()    ! skin factor 2
    real(DP), dimension(:), pointer, contiguous      :: hwadi       => null()    ! stores downstream flow to dry cell effective head
    real(DP), dimension(:), pointer, contiguous      :: dwadi       => null()    ! stores derivative of downstream flow to dry cell effective head
    real(DP), dimension(:), pointer, contiguous      :: dkdhc       => null()    ! stores derivative of upstream relative K of connection
    integer(I4B), pointer                            :: ingnc       => null()    ! unit number for gnc (0 if off)
    type(GhostNodeType), pointer                     :: gnc         => null()    ! gnc object
    integer(I4B), pointer                            :: inmvr       => null()    ! unit number for mover (0 if off)
    type(GwfMvrType), pointer                        :: mvr         => null()    ! water mover object
    integer(I4B), pointer                            :: inobs       => null()    ! unit number for GWF-LNF observations
    type(ObsType), pointer                           :: obs         => null()    ! observation object
    character(len=LENBOUNDNAME), dimension(:),                                  &
                                 pointer, contiguous :: boundname   => null()    ! boundnames
    !
    ! -- table objects
    type(TableType), pointer :: outputtab1 => null()
    type(TableType), pointer :: outputtab2 => null()

  contains

    procedure          :: exg_df      => gwf_lnf_df
    procedure          :: exg_ac      => gwf_lnf_ac
    procedure          :: exg_mc      => gwf_lnf_mc
    procedure          :: exg_ar      => gwf_lnf_ar
    procedure          :: exg_rp      => gwf_lnf_rp
    procedure          :: exg_ad      => gwf_lnf_ad
    procedure          :: exg_cf      => gwf_lnf_cf
    procedure          :: exg_fc      => gwf_lnf_fc
    procedure          :: exg_fn      => gwf_lnf_fn
    procedure          :: exg_cq      => gwf_lnf_cq
    procedure          :: exg_bd      => gwf_lnf_bd
    procedure          :: exg_ot      => gwf_lnf_ot
    procedure          :: exg_da      => gwf_lnf_da
    procedure          :: exg_fp      => gwf_lnf_fp
    procedure          :: get_iasym   => gwf_lnf_get_iasym
    procedure          :: allocate_scalars
    procedure          :: allocate_arrays
    procedure          :: read_options
    procedure          :: read_data
    procedure          :: read_gnc
    procedure          :: read_mvr
    procedure, private :: gethwadi
    procedure, private :: getdwadi  
    procedure, private :: condcalc
    procedure, private :: rewet
    procedure, private :: qcalc
    procedure, private :: gwf_lnf_df_obs
    procedure, private :: gwf_lnf_rp_obs
    procedure, public  :: gwf_lnf_save_simvals
  end type GwfLnfExchangeType

contains

  subroutine gwflnfexchange_create(filename, id, m1id, m2id)
! ******************************************************************************
! Create a new GWF to lnf exchange object.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use BaseModelModule, only: BaseModelType
    use ListsModule, only: baseexchangelist
    use ObsModule, only: obs_cr
    ! -- dummy
    character(len=*),intent(in) :: filename
    integer(I4B), intent(in) :: id, m1id, m2id
    ! -- local
    type(GwfLnfExchangeType), pointer :: exchange
    class(BaseModelType), pointer :: mb
    class(BaseExchangeType), pointer :: baseexchange
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
    exchange%name = 'GWF-LNF_' // trim(adjustl(cint))
    !
    ! -- allocate scalars and set defaults
    call exchange%allocate_scalars()
    exchange%filename = filename
    exchange%typename = 'GWF-LNF'
    exchange%implicit = .true.
    !
    ! -- set exchange%m1
    mb => GetBaseModelFromList(basemodellist, m1id)
    select type (mb)
    class is (NumericalModelType)
      exchange%m1=>mb
    end select
    !
    ! -- set exchange%m2
    mb => GetBaseModelFromList(basemodellist, m2id)
    select type (mb)
    class is (NumericalModelType)
      exchange%m2=>mb
    end select
    !
    ! -- set gwfmodel1
    mb => GetBaseModelFromList(basemodellist, m1id)
    select type (mb)
    type is (GwfModelType)
      exchange%gwfmodel => mb
    end select
    !
    ! -- set gwfmodel2
    mb => GetBaseModelFromList(basemodellist, m2id)
    select type (mb)
    type is (LnfModelType)
      exchange%lnfmodel => mb
    end select
    !
    ! -- Create the obs package
    call obs_cr(exchange%obs, exchange%inobs)
    !
    ! -- return
    return
  end subroutine gwflnfexchange_create

  subroutine gwf_lnf_df(this)
! ******************************************************************************
! gwf_lnf_df -- Define GWF to lnf exchange object.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: iout
    use InputOutputModule, only: getunit, openfile
    use GhostNodeModule, only: gnc_cr
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    ! -- local
    integer(I4B) :: inunit
! ------------------------------------------------------------------------------
    !
    ! -- open the file
    inunit = getunit()
    write(iout,'(/a,a)') ' Creating exchange: ', this%name
    call openfile(inunit, iout, this%filename, 'GWF-LNF')
    !
    call this%parser%Initialize(inunit, iout)
    !
    ! -- Ensure models are in same solution
    if(this%gwfmodel%idsoln /= this%lnfmodel%idsoln) then
      call store_error('ERROR.  TWO MODELS ARE CONNECTED ' //                  &
        'IN A GWF EXCHANGE BUT THEY ARE IN DIFFERENT SOLUTIONS. ' //           &
        'GWF AND LNF MODELS MUST BE IN SAME SOLUTION: ' //                      &
        trim(this%gwfmodel%name) // ' ' // trim(this%lnfmodel%name) )
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- read options
    call this%read_options(iout)
    !
    ! -- read dimensions
    call this%read_dimensions(iout)
    !
    ! -- allocate arrays
    call this%allocate_arrays()
    !
    ! -- read exchange data
    call this%read_data(iout)
    !
    ! -- call each model and increase the edge count
    call this%gwfmodel%npf%increase_edge_count(this%nexg)
    call this%lnfmodel%npf%increase_edge_count(this%nexg)
    !
    ! -- Create and read ghost node information
    if(this%ingnc > 0) then
      call gnc_cr(this%gnc, this%name, this%ingnc, iout)
      call this%read_gnc(iout)
    endif
    !
    ! -- Read mover information
    if(this%inmvr > 0) then
      call this%read_mvr(iout)
    endif
    !
    ! -- close the file
    close(inunit)
    !
    ! -- Store obs
    call this%gwf_lnf_df_obs()
    call this%obs%obs_df(iout, this%name, 'GWF-LNF', this%gwfmodel%dis)
    !
    ! -- return
    return
  end subroutine gwf_lnf_df

  subroutine gwf_lnf_ac(this, sparse)
! ******************************************************************************
! gwf_lnf_ac -- override parent exg_ac so that gnc can add
!   connections here.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only:sparsematrix
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- call parent model to add exchange connections
    call this%NumericalExchangeType%exg_ac(sparse)
    !
    ! -- add gnc connections
    if(this%ingnc > 0) then
      call this%gnc%gnc_ac(sparse)
    endif
    !
    ! -- Return
    return
  end subroutine gwf_lnf_ac

  subroutine gwf_lnf_mc(this, iasln, jasln)
! ******************************************************************************
! gwf_lnf_mc -- Map the connections in the global matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only:sparsematrix
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- call parent model to map exchange connections
    call this%NumericalExchangeType%exg_mc(iasln, jasln)
    !
    ! -- map gnc connections
    if(this%ingnc > 0) then
      call this%gnc%gnc_mc(iasln, jasln)
    endif
    !
    ! -- Return
    return
  end subroutine gwf_lnf_mc

  subroutine gwf_lnf_ar(this)
! ******************************************************************************
! gwf_lnf_ar -- Calculate the saturated conductance.  Must be called after
!               npf_ar for both models.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH, DZERO, DHALF, DONE, DSMALLANG, DPI
    use SimModule, only: store_error, ustop
    use GwfNpfModule, only: condmean, vcond, hcond
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    ! -- local
    integer(I4B) :: ifcon, igeon, cellnum
    integer(I4B) :: n, ng, nl, ij, jj, ijs
    real(DP) :: sarea, hrad, dex, dexn, dez
    real(DP) :: faniso, ro, rod
    real(DP) :: csat, skinf
 
    real(DP) :: botn, botm
    real(DP) :: area, perim
    real(DP) :: thickn, thickm
    real(DP) :: angle, hyn, hym
    real(DP) :: fawidth
    real(DP), dimension(3) :: vg
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- If mover is active, then call ar routine
    if(this%inmvr > 0) call this%mvr%mvr_ar()
    !
    ! -- Check to see if horizontal anisotropy in either gwfmodel.
    !    If so, then ANGLDEGX must be provided as an auxiliary variable for this
    !    GWF-LNF exchange (this%ianglex > 0).
    if(this%gwfmodel%npf%ik22 /= 0) then
      if(this%ianglex == 0) then
        write(errmsg, '(a)') 'Error.  GWF-LNF requires that ANGLDEGX be ' //   &
                             'specified as an auxiliary variable because ' //  &
                             'K22 was specified in one or both ' // &
                             'groundwater models.'
        call store_error(errmsg)
        call ustop()
      endif
    endif
    !
    ! -- Check to see if specific discharge is needed for model1 or model2.
    !    If so, then ANGLDEGX must be provided as an auxiliary variable for this
    !    GWF-LNF exchange (this%ianglex > 0).
    if(this%gwfmodel%npf%icalcspdis /= 0 .or. &
       this%lnfmodel%npf%icalcspdis /= 0) then
      if(this%ianglex == 0) then
        write(errmsg, '(a)') 'Error.  GWF-LNF requires that ANGLDEGX be ' //   &
                             'specified as an auxiliary variable because ' //  &
                             'specific discharge is being calculated in' // &
                             ' one or both groundwater models.'
        call store_error(errmsg)
        call ustop()
      endif
      if(this%icdist == 0) then
        write(errmsg, '(a)') 'Error.  GWF-LNF requires that CDIST be ' //   &
                             'specified as an auxiliary variable because ' //  &
                             'specific discharge is being calculated in' // &
                             ' one or both groundwater models.'
        call store_error(errmsg)
        call ustop()
      endif
    endif
    !
    ! -- Go through each connection and calculate the saturated conductance
    do n = 1, this%nexg
      !
      ng = this%nodem1(n)
      nl = this%nodem2(n)        
      ifcon = this%iflowtype(n)

      cellnum = this%lnfmodel%disl%iageocellnum(nl)
      area = this%lnfmodel%disl%jametries(this%lnfmodel%disl%iageom(nl))%obj%area_sat(cellnum)
      perim = this%lnfmodel%disl%jametries(this%lnfmodel%disl%iageom(nl))%obj%perimeter_sat(cellnum)

      sarea = perim * this%flengw(n)
      ! -- get hydraulic radius as twice area over perimeter
      hrad = 2.0 * area / perim
      ! -----------------------------------------------------------------------
      ! -- compute effective cell radius of LNF cell
      if(this%ihc(n) == 1) then
        ! -- compute effective GWF cell radius for horizontal LNF cell
        dex = 0.0
        dexn = 0.0
        do ij = this%gwfmodel%dis%con%ia(ng)+1,this%gwfmodel%dis%con%ia(ng+1)-1
          jj = this%gwfmodel%dis%con%ja(ij)
          ijs = this%gwfmodel%dis%con%jas(ij)
          if(this%gwfmodel%dis%con%ihc(ijs) == 0) cycle
          dex = dex + this%gwfmodel%dis%con%cl1(ijs)
          dexn = dexn + 1.0
        enddo
        dex = 2.0 * dex / dexn
        dez = this%gwfmodel%dis%top(ng) - this%gwfmodel%dis%bot(ng)
        faniso = this%fskin2(n)
        ro = dex**2 * sqrt(1.0/faniso) + dez**2 * sqrt(faniso)
        rod = (1.0/faniso)**0.25 + faniso**0.25
        ro = 0.28 * sqrt(ro) / rod
      else
        ! -- compute GWF cell effective radius for vertical LNF cell
        ro = 0.0
        dexn = 0.0
        do ij = this%gwfmodel%dis%con%ia(ng)+1,this%gwfmodel%dis%con%ia(ng+1)-1
           jj = this%gwfmodel%dis%con%ja(ij)
           ijs = this%gwfmodel%dis%con%jas(ij)
           if(this%gwfmodel%dis%con%ihc(ijs) == 0) cycle
           ro = ro + this%gwfmodel%dis%con%cl1(ijs)**2
           dexn = dexn + 1.0
        enddo
        ro = 0.28 * sqrt(2.0*ro/dexn)
      endif
      this%cl1(n) = hrad
      this%cl2(n) = ro
      this%fahl(n) = sarea
      ! -----------------------------------------------------------------------
      if(ifcon == 2) then
        !
        ! -- conductance is input in fskin1
        csat = this%fskin1(n)
      elseif(ifcon == 3) then
        !
        ! -- conductance is computed for skin
        csat = this%fskin1(n)/this%fskin2(n) * sarea
      else
        !
        ! -- modified Thiem solution variant is used to compute conductance
        skinf = this%fskin1(n)
        if(ifcon == 4) then
          ! -- compute skin factor from efficiency
          skinf = log(ro / hrad) * (1.0 - skinf) / skinf
        endif
        csat = log(ro / hrad) + skinf
        ! SRP Fix: k11 and k22 should both be used
        ! SRP Fix: Handle vertical connection case
        csat = 2.0 * DPI * this%gwfmodel%npf%k11(ng) * sqrt(1.0/this%fskin2(n)) * this%flengw(n) / csat
      endif
      this%condsat(n) = csat
    enddo
    !
    ! ------------------------------------------------------------------------------
    ! --Set global flag for flow to dry cell condition if any cell uses it
    this%iflowdrycell = 0
    do n = 1, this%nexg
      if(this%ifdc(n) /= 0) then
        this%iflowdrycell = 1
        cycle
      endif
    enddo
    !
    ! -- Observation AR
    call this%obs%obs_ar()
    !
    ! -- Return
    return
  end subroutine gwf_lnf_ar

  subroutine gwf_lnf_rp(this)
! ******************************************************************************
! gwf_lnf_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: readnewdata
    ! -- dummy
    class(GwfLnfExchangeType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Check with TDIS on whether or not it is time to RP
    if (.not. readnewdata) return
    !
    ! -- Read and prepare for mover
    if(this%inmvr > 0) call this%mvr%mvr_rp()
    !
    ! -- Read and prepare for observations
    call this%gwf_lnf_rp_obs()
    !
    ! -- Return
    return
  end subroutine gwf_lnf_rp

  subroutine gwf_lnf_ad(this, isolnid, kpicard, isubtime)
! ******************************************************************************
! gwf_lnf_ad -- Initialize package x values to zero for explicit exchanges
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer(I4B), intent(in) :: isolnid
    integer(I4B), intent(in) :: kpicard
    integer(I4B), intent(in) :: isubtime
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Advance mover
    if(this%inmvr > 0) call this%mvr%mvr_ad()
    !
    ! -- Push simulated values to preceding time/subtime step
    call this%obs%obs_ad()
    !
    ! -- Return
    return
  end subroutine gwf_lnf_ad

  subroutine gwf_lnf_cf(this, kiter)
! ******************************************************************************
! gwf_lnf_cf -- Calculate the conductance term.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DSMALLANG, DEM5, DPI
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer(I4B), intent(in) :: kiter
    integer(I4B) :: n, ng, nl, iups, ihbot, igeon, cellnum
    real(DP) :: hup, botg, botl, both, alnfdir
    real(DP) :: depth, pw, psat, eff_leng, thksat
    ! -- local
! ------------------------------------------------------------------------------
    ! -- fill akrc with upstream kr of connection
    do n = 1, this%nexg
      if(this%icelltype(n) == 0)then
        this%akrc(n) = 1.0
      else  
        ng = this%nodem1(n)
        nl = this%nodem2(n)
        ! -- find upstream and higher bottom of the two cells
        if(this%lnfmodel%npf%hnew(nl) > this%gwfmodel%npf%hnew(ng))then
          iups = nl
          hup = this%lnfmodel%npf%hnew(nl)
        else
          iups = ng
          hup = this%gwfmodel%npf%hnew(ng)
        endif
        ihbot = ng
        botg = this%gwfmodel%dis%bot(ng)
        botl = this%lnfmodel%disl%bot(nl)
        if( botl > botg) then
          ihbot = nl
          both = botl
        else
          ihbot = ng
          both = botg
        endif
        ! -----------------------------------------
        !alnfdir = this%lnfmodel%dis%fangle(nl)
        depth = hup - both
        if(this%ihc(n) == 1) then
          ! -- fill akrc for horizontal LNF cell using upstream wetted perimeter
          if(hup < (botl-DEM5)) cycle ! akrc is zero if head is below bottom
          cellnum = this%lnfmodel%disl%iageocellnum(nl)
          pw = this%lnfmodel%disl%jametries(this%lnfmodel%disl%iageom(nl))%obj%perimeter_wet(cellnum, depth)
          psat = this%lnfmodel%disl%jametries(this%lnfmodel%disl%iageom(nl))%obj%perimeter_sat(cellnum)
          this%akrc(n) = pw / psat
        else
          ! -- fill akrc for vertical or angled LNF cell using upstream wetted perimeter
          ! SRP FIX: Support vertical connections - alnfdir not defined
          if(abs(alnfdir - DPI/2.) > DSMALLANG) then
            ! -- adjust effective length for angled segment
            eff_leng = this%flengw(n) * sin(alnfdir)
          endif
          call sat_thik(depth, eff_leng, thksat)
          this%akrc(n) = thksat
        endif
      endif
    enddo
    ! ------------------------------------------------------------------------------
    ! -- Fill hwadi array for each node with flow-correction head value
    if(this%iflowdrycell /= 0)then
      allocate(this%hwadi(this%nexg))
      call this%gethwadi()
    endif
    !
    ! -- Rewet cells across models using the wetdry parameters in each model's
    !    npf package, and the head in the connected model.
    call this%rewet(kiter)
    !
    ! -- Return
    return
  end subroutine gwf_lnf_cf
  
  subroutine sat_thik(depth, effective_length, thksat)
! ******************************************************************************
! sat_thik -- computes saturated thickness of vertical LNF cell using straight
!             line with parabolic smoothing as in NWT
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    double precision, intent(in) :: depth
    double precision, intent(in) :: effective_length
    double precision,intent(inout) :: thksat
    double precision :: eps = 1.0e-6
    double precision :: acof,x,y
!--------------------------------------------------------------------------------------
    acof = 1.0 / (1.0 - eps)
    x = depth / effective_length
    if(x < 0) then
      y = 0.0
    elseif(x.LT.eps)then
      y = acof * 0.5 / eps * x**2
    elseif(x.LT.1.0-eps)then
      y = acof * x + (1.0-acof)*0.5
    elseif(x.LT.1.0)then
      x = 1.0 - x
      y = acof *0.5/eps * x**2
      y = 1.0-y
    else
      y = 1.0
    endif
    thksat = y
  !
  ! -- Return
    return
  end subroutine sat_thik

  subroutine gethwadi(this)
! ******************************************************************************
! gethwadi -- Fill the hwadi array with flow to dry cell head (smoothed)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer :: n, ng, nl, iups, ihbot
    double precision :: hup, hdn, botg, botl, both, x, y
! ------------------------------------------------------------------------------
    do n =1,this%nexg
      this%hwadi(n) = 0.0
      if(this%ifdc(n) /= 0)then
        ng = this%nodem1(n)
        nl = this%nodem2(n)
        !if(this%lnfmodel%disl%idomain(nl) == 0 .or. this%gwfmodel%dis%idomain(ng) == 0) cycle
        ! -- find upstream and higher bottom of the two cells
        if(this%lnfmodel%npf%hnew(nl) > this%gwfmodel%npf%hnew(ng))then
          iups = nl
          hup = this%lnfmodel%npf%hnew(nl)
          hdn = this%gwfmodel%npf%hnew(ng)
        else
          iups = ng
          hup = this%gwfmodel%npf%hnew(ng)
          hdn = this%lnfmodel%npf%hnew(nl)
        endif
        ihbot = ng
        botg = this%gwfmodel%dis%bot(ng)
        botl = this%lnfmodel%disl%bot(nl)
        if( botl > botg) then
          ihbot = nl
          both = botl
        else
          ihbot = ng
          both = botg
        endif
        ! -----------------------------------------
        x = hdn - ihbot
        call wadifn(x,y)
        this%hwadi(n) = y + ihbot
      endif
    enddo
    !
    ! -- Return
    return
  end subroutine gethwadi

  subroutine wadifn(x,y)
! ******************************************************************************
! wadifn -- smoothened flow to dry cell function
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DEM4
    double precision, intent(in) :: x
    double precision, intent(out) :: y
  ! -- local
! ------------------------------------------------------------------------------
    !
    if(x > DEM4) then
      y = x
    elseif(x > -DEM4)then
      y = 0.25 * x**2 / DEM4 + 0.5 * x + 0.25 * DEM4
    else
      y = 0.0
    endif
    !
    ! -- Return
    return
  end subroutine wadifn
  
  subroutine getdwadi (this)
! ******************************************************************************
! getdwadi -- Fill the dwadi array with derivative of flow to dry cell head (smoothed)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer :: n, ng, nl, iups, ihbot
    double precision :: hup, hdn, botg, botl, both, x, y
! ------------------------------------------------------------------------------
    do n =1,this%nexg
      this%dwadi(n) = 1.0
      if(this%ifdc(n) /= 0)then
        ng = this%nodem1(n)
        nl = this%nodem2(n)
        !if(this%lnfmodel%dis%idomain(nl) == 0 .or. this%gwfmodel%dis%idomain(ng) == 0) cycle
        ! -- find upstream and higher bottom of the two cells
        if(this%lnfmodel%npf%hnew(nl) > this%gwfmodel%npf%hnew(ng))then
          iups = nl
          hup = this%lnfmodel%npf%hnew(nl)
          hdn = this%gwfmodel%npf%hnew(ng)
        else
          iups = ng
          hup = this%gwfmodel%npf%hnew(ng)
          hdn = this%lnfmodel%npf%hnew(nl)
        endif
        ihbot = ng
        botg = this%gwfmodel%dis%bot(ng)
        botl = this%lnfmodel%dis%bot(nl)
        if( botl > botg) then
          ihbot = nl
          both = botl
        else
          ihbot = ng
          both = botg
        endif
        ! -----------------------------------------
        x = hdn - ihbot
        call dwadifn(x,y)
        this%dwadi(n) = y
      endif
    enddo
    !
    ! -- Return
    return
  end subroutine getdwadi  
   
  subroutine dwadifn(x,y)
! ******************************************************************************
! dwadifn -- derivative of flow to dry cell function
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DEM4
    double precision, intent(in) :: x
    double precision, intent(out) :: y
  ! -- local
 ! ------------------------------------------------------------------------------
    !
    if(x > DEM4) then
      y = 1.0
    elseif(x > -DEM4)then
      y = 0.5 * x / DEM4 + 0.5
    else
      y = 0.0
    endif
    !
    ! -- Return
    return
  end subroutine dwadifn
  
  subroutine gwf_lnf_fc(this, kiter, iasln, amatsln, inwtflag)
! ******************************************************************************
! gwf_lnf_fc -- Fill the matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DHALF
    use GwfNpfModule, only: hcond, vcond
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer(I4B), intent(in) :: kiter
    integer(I4B), dimension(:), intent(in) :: iasln
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B), optional, intent(in) :: inwtflag
    ! -- local
    integer(I4B) :: inwt, iexg
    integer(I4B) :: njasln
! ------------------------------------------------------------------------------
    !
    ! -- calculate the conductance for each exchange connection
    call this%condcalc()
    !
    ! -- if gnc is active, then copy cond into gnc cond (might consider a
    !    pointer here in the future)
    if(this%ingnc > 0) then
      do iexg = 1, this%nexg
        this%gnc%cond(iexg) = this%cond(iexg)
      enddo
    endif
    !
    ! -- Call fill method of parent to put this%cond into amatsln
    call this%NumericalExchangeType%exg_fc(kiter, iasln, amatsln)
    !
    ! -- Fill the gnc terms in the solution matrix
    if(this%ingnc > 0) then
      call this%gnc%gnc_fc(kiter, amatsln)
    endif
    !
    ! -- Call mvr fc routine
    if(this%inmvr > 0) call this%mvr%mvr_fc()
    !
    ! -- Set inwt to exchange newton, but shut off if requested by caller
    inwt = this%inewton
    if(present(inwtflag)) then
      if (inwtflag == 0) inwt = 0
    endif
    if (inwt /= 0) then
      call this%exg_fn(kiter, iasln, amatsln, this%gwfmodel%rhs, this%lnfmodel%rhs)
    endif
    !
    ! -- Ghost node Newton-Raphson
    if (this%ingnc > 0) then
      if (inwt /= 0) then
        njasln = size(amatsln)
        call this%gnc%gnc_fn(kiter, njasln, amatsln, this%condsat,             &
          ihc_opt=this%ihc, ivarcv_opt=this%ivarcv,                            &
          ictm1_opt=this%gwfmodel%npf%icelltype,                              &
          ictm2_opt=this%lnfmodel%npf%icelltype)
      endif
    endif
    !
    ! -- Return
    return
  end subroutine gwf_lnf_fc

 subroutine gwf_lnf_fn(this, kiter, iasln, amatsln, rhsg, rhsl)
! ******************************************************************************
! lnf_npf_fn -- Formulate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    !dummy
    !  class(NumericalExchangeType) :: this  * need this as well to access the matrix
    class(GwfLnfExchangeType) :: this
    integer,intent(in) :: kiter
    integer, dimension(:), intent(in) :: iasln
    double precision, dimension(:), intent(inout) :: amatsln
    double precision, dimension(:), intent(inout) :: rhsg, rhsl
    !local
    integer :: n, cellnum
    integer :: iups, ihbot, igeon, ng, nl, ngsln, nlsln
    integer :: iup, idn, ipivup, ipivdn, ioffup, ioffdn, ibup, ibdn
    double precision :: satcond, hdn, hdif
    double precision :: consterm, rterm, dwaditerm, botg, botl, both
    double precision :: filledterm, term 
    double precision :: alnfdir, hup, depth, pw, psat, eff_leng, thksat
    double precision :: epsilon = 1.0e-5
    double precision :: smallangle = 1.0d-5 
    double precision :: pi = 3.14159265
! ------------------------------------------------------------------------------
    !
    ! allocate and initialize derivative terms
    allocate(this%dkdhc(this%nexg))
    do n=1, this%nexg
      this%dkdhc(n) = 0.0
    enddo
    !
    if(this%iflowdrycell /= 0)then
      allocate(this%dwadi(this%nexg))
      !  do n=1, this%nexg
      !    this%dwadi(n) = 1.0
      !  enddo
    endif
    !
! ------------------------------------------------------------------------------
    ! -- Fill dkdhc with upstream dkdh of connection
    do n=1, this%nexg
      ng = this%nodem1(n)
      nl = this%nodem2(n)
      ! -- find upstream and higher bottom of the two cells
      if(this%lnfmodel%npf%hnew(nl) > this%gwfmodel%npf%hnew(ng))then
        iups = nl
        hup = this%lnfmodel%npf%hnew(nl)
      else
        iups = ng
        hup = this%gwfmodel%npf%hnew(ng)
      endif
      ihbot = ng
      botg = this%gwfmodel%dis%bot(ng)
      botl = this%lnfmodel%disl%bot(nl)
      if( botl > botg) then
        ihbot = nl
        both = botl
      else
        ihbot = ng
        both = botg
      endif
      ! -----------------------------------------
      !alnfdir = this%lnfmodel%dis%fangle(nl)
      hup = hup + epsilon
      depth = hup - both
      if(this%ihc(n)) then
        ! -- fill dkdhc for horizontal LNF cell using upstream wetted perimeter
        if(hup < (botl-epsilon)) cycle ! akrc is zero if head is below bottom
        cellnum = this%lnfmodel%disl%iageocellnum(nl)
        pw =  this%lnfmodel%disl%jametries(this%lnfmodel%disl%iageom(nl))%obj%perimeter_wet(cellnum, depth)
        psat =  this%lnfmodel%disl%jametries(this%lnfmodel%disl%iageom(nl))%obj%perimeter_sat(cellnum)
        this%dkdhc(n) = pw / psat
      else
        ! SRP FIX: handle vertical connections with angle alnfdir - need to calculate
        ! -- fill dkdhc for vertical or angled LNF cell using upstream wetted perimeter
        if(abs(alnfdir - pi/2.) > 1.0e-6) then
          ! -- adjust effective length for angled segment
          eff_leng = this%lnfmodel%disl%celllen(n) * sin(alnfdir)
        endif
      
        call sat_thik(depth, eff_leng, thksat)
        this%dkdhc(n) = thksat
      endif
      this%dkdhc(n) = (this%dkdhc(n) - this%akrc(n)) / epsilon
    enddo
    !
! ------------------------------------------------------------------------------
    ! -- Fill dwadi of downstream node with derivative of flow correction term
    if(this%iflowdrycell /= 0)then
      call this%getdwadi()
    endif
    !
! ------------------------------------------------------------------------------
    ! add newton terms to solution matrix
    do n=1, this%nexg
      ng = this%nodem1(n)
      nl = this%nodem2(n)
      ngsln = ng + this%m1%moffset
      nlsln = nl + this%m2%moffset
      !if(this%gwfmodel%dis%idomain(ng) == 0 .or. this%lnfmodel%disl%idomain(nl) == 0) cycle
      ! --
      ! -- find upstream and downstream cells
      if(this%lnfmodel%npf%hnew(nl) > this%gwfmodel%npf%hnew(ng))then
        iup = nl
        idn = ng
        ipivup = iasln(nlsln)              ! diagonal term of row nl
        ioffup = this%idxsymglo(n)         ! offdiagonal term of row nl which is model2
        ipivdn = iasln(ngsln)              ! diagonal term of row ng
        ioffdn = this%idxglo(n)            ! offdiagonal term of row ng which is model1
        !ibup = this%lnfmodel%disl%idomain(nl)
        !ibdn = this%gwfmodel%dis%idomain(ng)
        hup = this%lnfmodel%npf%hnew(nl)
        hdn = this%gwfmodel%npf%hnew(ng)
      else
        iup = ng
        idn = nl
        ipivup = iasln(ngsln)              ! diagonal term of row ng
        ioffup = this%idxglo(n)            ! offdiagonal term of row nl which is model1
        ipivdn = iasln(nlsln)              ! diagonal term of row nl
        ioffdn = this%idxsymglo(n)         ! offdiagonal term of row ng which is model2
        !ibup = this%gwfmodel%dis%idomain(ng)
        !ibdn = this%lnfmodel%disl%idomain(nl)
        hup = this%gwfmodel%npf%hnew(ng)
        hdn = this%lnfmodel%npf%hnew(nl)
      endif
      ! --
      ! -- Compute additional term to Jacobian
      satcond = this%condsat(n)
      hdif = hdn
      if(this%iflowdrycell /= 0)then
        hdif = this%hwadi(n)
      endif
      hdif = hup - hdif
      ! compute additional term
      consterm = satcond * hdif
      !filledterm is found in array cond
      filledterm = this%cond(n)
      term = consterm * this%dkdhc(n)
      rterm = term * hup
      ! --
      dwaditerm = 1.0
      if(this%ifdc(n) /= 0) then
        ! -- Add flow to dry cell term
        dwaditerm = this%dwadi(n)
        rterm = rterm - filledterm * (dwaditerm * hdn - hdn)
      endif
      ! ----fill terms for downstream row of matrix
      amatsln(ipivdn) =  amatsln(ipivdn) - (dwaditerm - 1.0) * filledterm
      !if(ibdn > 0) then
      amatsln(ioffdn) = amatsln(ioffdn) + term
      !endif
      ! ----fill terms for upstream row of matrix
      amatsln(ipivup) = amatsln(ipivup) - term
      !if(ibup > 0) then
      amatsln(ioffup) = amatsln(ioffup) * dwaditerm
      !endif
      ! ----fill right hand side terms
      if(this%lnfmodel%npf%hnew(nl) > this%gwfmodel%npf%hnew(ng))then !nl is upstream and ng is downstream
        rhsl(nl) = rhsl(nl) - rterm
        rhsg(ng) = rhsg(ng) + rterm
      else 
        rhsg(ng) = rhsg(ng) - rterm
        rhsl(nl) = rhsl(nl) + rterm
      endif  
      ! rhssln(iup) = rhssln(iup) - rterm   
      ! rhssln(idn) = rhssln(idn) + rterm
        !
    end do
    !
! ------------------------------------------------------------------------------
    ! deallocate newton matrices
    deallocate(this%dkdhc)
    if(this%iflowdrycell /= 0) then
      deallocate(this%dwadi)
    endif
    !
    ! -- Return
    return
  end subroutine gwf_lnf_fn
  
  subroutine gwf_lnf_cq(this, icnvg, isuppress_output, isolnid)
! ******************************************************************************
! gwf_lnf_cq -- Calculate flow between two cells
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO, DPIO180
    use GwfNpfModule, only: thksatnm
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    character(len=LINELENGTH) :: node1str, node2str
    character(len=*), parameter :: fmtdata =                                   &
     "(2a16, 4(1pg16.6))"
    integer(I4B), intent(inout) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B), intent(in) :: isolnid
    ! -- local
    integer(I4B) :: iexg, cellnum
    integer(I4B) :: n1, n2
    real(DP) :: nx, ny
    integer(I4B) :: ihc
    real(DP) :: rrate
    real(DP) :: area
    real(DP) :: dltot
    real(DP) :: distance

! ------------------------------------------------------------------------------
    !
    ! -- Return if there neither model needs to calculate specific discharge
    if (this%gwfmodel%npf%icalcspdis == 0 .and. &
        this%lnfmodel%npf%icalcspdis == 0) return
    !
    ! -- initialize
    if(this%iprflow /= 0) then
      !write(iout, fmtheader) trim(adjustl(this%name)), this%id, 'NODEM1',      &
      !                       'NODEM2', 'COND', 'X_M1', 'X_M2', 'FLOW'
      do iexg = 1, this%nexg
        n1 = this%nodem1(iexg)
        n2 = this%nodem2(iexg)
        ! SRP Fix - Does zero angle make sense for connection?
        nx = DZERO
        ny = DZERO
        ihc = this%ihc(iexg)
        rrate = this%qcalc(iexg, n1, n2)
        cellnum = this%lnfmodel%disl%iageocellnum(n2)
        area = this%lnfmodel%disl%jametries(this%lnfmodel%disl%iageom(n2))%obj%perimeter_sat(cellnum) * this%flengw(n2)
        !call this%m1%dis%noder_to_string(n1, node1str)
        !call this%m2%dis%noder_to_string(n2, node2str)
        !write(iout, fmtdata) trim(adjustl(node1str)), trim(adjustl(node2str)), &
        !                     this%cond(iexg), this%m1%x(n1), this%m2%x(n2),    &
        !                     flow
             !
        ! -- Submit this connection and flow information to the npf
        !    package of gwfmodel1
        if(this%icdist > 0) then
          dltot = this%auxvar(this%icdist, iexg)
        else
          call ustop('error in gwf_gwf_cq')
        endif
        distance = dltot * this%cl1(iexg) / (this%cl1(iexg) + this%cl2(iexg))

        ! -- Submit this connection and flow information to the npf
        !    package of gwfmodel
        if (this%gwfmodel%npf%icalcspdis == 1) then
          call this%gwfmodel%npf%set_edge_properties(n1, ihc, rrate, area,      &
                                                     nx, ny, distance)
        endif
        !
        ! -- Submit this connection and flow information to the npf
        !    package of lnfmodel
        if(this%icdist > 0) then
          dltot = this%auxvar(this%icdist, iexg)
        else
          call ustop('error in gwf_lnf_cq')
        endif
        if (this%lnfmodel%npf%icalcspdis == 1) then
          distance = dltot * this%cl2(iexg) / (this%cl1(iexg) + this%cl2(iexg))
          if (ihc /= 0) rrate = -rrate
          call this%lnfmodel%npf%set_edge_properties(n2, ihc, rrate, area,     &
                                                     -nx, -ny, distance)
        endif
      enddo
    endif
    !
    ! -- return
    return
  end subroutine gwf_lnf_cq
  
  subroutine gwf_lnf_bd(this, icnvg, isuppress_output, isolnid)
! ******************************************************************************
! gwf_lnf_bd -- Budget for implicit gwf to gwf exchange; the budget for the
!               explicit exchange connections is handled for each model by
!               the exchange boundary package.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO, LENBUDTXT, LENPACKAGENAME
    !use TdisModule, only: kstp, kper
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer(I4B), intent(inout) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B), intent(in) :: isolnid
    ! -- local
    character(len=LENBOUNDNAME) :: bname
    character(len=LENPACKAGENAME+4) :: packname1
    character(len=LENPACKAGENAME+4) :: packname2
    character(len=LENBUDTXT), dimension(1) :: budtxt
    character(len=20) :: nodestr
    integer(I4B) :: ntabrows
    integer(I4B) :: nodeu
    real(DP), dimension(2, 1) :: budterm
    integer(I4B) :: i, n1, n2, n1u, n2u
    integer(I4B) :: ibinun1, ibinun2
    integer(I4B) :: icbcfl, ibudfl
    real(DP) :: ratin, ratout, rrate, deltaqgnc
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- initialize local variables
    budtxt(1) = '    FLOW-JA-FACE'
    packname1 = 'EXG '//this%name
    packname1 = adjustr(packname1)
    packname2 = 'EXG '//this%name
    packname2 = adjustr(packname2)
    !
    ! -- update output tables
    if (this%iprflow /= 0) then
      !
      ! -- update titles
      if (this%gwfmodel%oc%oc_save('BUDGET')) then
        call this%outputtab1%set_title(packname1)
      end if
      if (this%lnfmodel%oc%oc_save('BUDGET')) then 
        call this%outputtab2%set_title(packname2)
      end if
      !
      ! -- update maxbound of tables
      ntabrows = 0
      do i = 1, this%nexg
        n1 = this%nodem1(i)
        n2 = this%nodem2(i)
        !
        ! -- If both cells are active then calculate flow rate
        if (this%gwfmodel%ibound(n1) /= 0 .and.                                  &
            this%lnfmodel%ibound(n2) /= 0) then
          ntabrows = ntabrows + 1
        end if
      end do
      if (ntabrows > 0) then
        call this%outputtab1%set_maxbound(ntabrows)
        call this%outputtab2%set_maxbound(ntabrows)
      end if
    end if
    !
    ! -- Print and write budget terms for model 1
    !
    ! -- Set binary unit numbers for saving flows
    if(this%ipakcb /= 0) then
      ibinun1 = this%gwfmodel%oc%oc_save_unit('BUDGET')
      ibinun2 = this%lnfmodel%oc%oc_save_unit('BUDGET')
    else
      ibinun1 = 0
      ibinun2 = 0
    endif
    !
    ! -- If save budget flag is zero for this stress period, then
    !    shut off saving
    if(.not. this%gwfmodel%oc%oc_save('BUDGET')) ibinun1 = 0
    if(.not. this%lnfmodel%oc%oc_save('BUDGET')) ibinun2 = 0
    if(isuppress_output /= 0) then
      ibinun1 = 0
      ibinun2 = 0
    endif
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if(ibinun1 /= 0) then
      call this%gwfmodel%dis%record_srcdst_list_header(budtxt(1),             &
                                       this%m1%name, this%name,                &
                                       this%m2%name, this%name,                &
                                       this%naux, this%auxname,                &
                                       ibinun1, this%nexg, this%gwfmodel%iout)
    endif
    if(ibinun2 /= 0) then
      call this%lnfmodel%dis%record_srcdst_list_header(budtxt(1),             &
                                       this%m2%name, this%name,                &
                                       this%m1%name, this%name,                &
                                       this%naux, this%auxname,                &
                                       ibinun2, this%nexg, this%lnfmodel%iout)
    endif
    !
    ! Initialize accumulators
    ratin = DZERO
    ratout = DZERO
    !
    ! -- Loop through all exchanges
    do i = 1, this%nexg
      !
      ! -- Assign boundary name
      if (this%inamedbound>0) then
        bname = this%boundname(i)
      else
        bname = ''
      endif
      !
      ! -- Calculate the flow rate between n1 and n2
      rrate = DZERO
      n1 = this%nodem1(i)
      n2 = this%nodem2(i)
      !
      ! -- If both cells are active then calculate flow rate
      if(this%gwfmodel%ibound(n1) /= 0 .and. &
          this%lnfmodel%ibound(n2) /= 0) then
        rrate = this%qcalc(i, n1, n2)
        !
        ! -- add ghost node contribution
        if(this%ingnc > 0) then
          deltaqgnc = this%gnc%deltaqgnc(i)
          rrate = rrate + deltaqgnc
        endif
        !
        ! -- Print the individual rates to model list files if requested
        if(this%iprflow /= 0) then
          if(this%gwfmodel%oc%oc_save('BUDGET')) then
            !
            ! -- set nodestr and write outputtab table
            nodeu = this%gwfmodel%dis%get_nodeuser(n1)
            call this%gwfmodel%dis%nodeu_to_string(nodeu, nodestr)
            call this%outputtab1%print_list_entry(i, trim(adjustl(nodestr)),     &
                                                  rrate, bname)
          end if
          if(this%lnfmodel%oc%oc_save('BUDGET')) then
            !
            ! -- set nodestr and write outputtab table
            nodeu = this%lnfmodel%dis%get_nodeuser(n1)
            call this%lnfmodel%dis%nodeu_to_string(nodeu, nodestr)
            call this%outputtab1%print_list_entry(i, trim(adjustl(nodestr)),     &
                                                  -rrate, bname)
          end if
        endif
        if(rrate < DZERO) then
          ratout = ratout - rrate
        else
          ratin = ratin + rrate
        endif
      endif
      !
      ! -- If saving cell-by-cell flows in list, write flow
      n1u = this%gwfmodel%dis%get_nodeuser(n1)
      n2u = this%lnfmodel%dis%get_nodeuser(n2)
      if(ibinun1 /= 0)                                                         &
        call this%gwfmodel%dis%record_mf6_list_entry(                         &
          ibinun1, n1u, n2u, rrate, this%naux, this%auxvar(:, i),              &
          .false., .false.)
      if(ibinun2 /= 0)                                                         &
        call this%lnfmodel%dis%record_mf6_list_entry(                         &
          ibinun2, n2u, n1u, -rrate, this%naux, this%auxvar(:, i),              &
          .false., .false.)
      !
    enddo
    !
    ! -- Add the budget terms to model 1
    budterm(1, 1) = ratin
    budterm(2, 1) = ratout
    call this%m1%model_bdentry(budterm, budtxt, this%name)
    !
    ! -- Print and write budget terms for model 2
    !
    ! -- Set binary unit numbers for saving flows
    if(this%ipakcb /= 0) then
      ibinun2 = this%lnfmodel%oc%oc_save_unit('BUDGET')
    else
      ibinun2 = 0
    endif
    !
    ! -- If save budget flag is zero for this stress period, then
    !    shut off saving
    if(.not. this%lnfmodel%oc%oc_save('BUDGET')) ibinun2 = 0
    if(isuppress_output /= 0) then
      ibinun2 = 0
    endif
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if(ibinun2 /= 0) then
      call this%lnfmodel%dis%record_srcdst_list_header(budtxt(1),             &
                                       this%m2%name, this%name,                &
                                       this%m1%name, this%name,                &
                                       this%naux, this%auxname,                &
                                       ibinun2, this%nexg, this%lnfmodel%iout)
    endif
    !
    ! Initialize accumulators
    ratin = DZERO
    ratout = DZERO
    !
    ! -- Loop through all exchanges
    do i = 1, this%nexg
      !
      ! -- Assign boundary name
      if (this%inamedbound>0) then
        bname = this%boundname(i)
      else
        bname = ''
      endif
      !
      ! -- Calculate the flow rate between n1 and n2
      rrate = DZERO
      n1 = this%nodem1(i)
      n2 = this%nodem2(i)
      !
      ! -- If both cells are active then calculate flow rate
      if(this%gwfmodel%ibound(n1) /= 0 .and. &
          this%lnfmodel%ibound(n2) /= 0) then
        rrate = this%qcalc(i, n1, n2)
        !
        ! -- add ghost node contribution
        if(this%ingnc > 0) then
          deltaqgnc = this%gnc%deltaqgnc(i)
          rrate = rrate + deltaqgnc
        endif
        !
        ! -- Print the individual rates to model list files if requested
        if(this%iprflow /= 0) then
          if(this%lnfmodel%oc%oc_save('BUDGET')) then
            !
            ! -- set nodestr and write outputtab table
            nodeu = this%lnfmodel%dis%get_nodeuser(n2)
            call this%lnfmodel%dis%nodeu_to_string(nodeu, nodestr)
            call this%outputtab2%print_list_entry(i, trim(adjustl(nodestr)),     &
                                                  -rrate, bname)
          end if
        endif
        if(rrate < DZERO) then
          ratout = ratout - rrate
        else
          ratin = ratin + rrate
        endif
      endif
      !
      ! -- If saving cell-by-cell flows in list, write flow
      n1u = this%gwfmodel%dis%get_nodeuser(n1)
      n2u = this%lnfmodel%dis%get_nodeuser(n2)
      if(ibinun2 /= 0)                                                         &
        call this%lnfmodel%dis%record_mf6_list_entry(                         &
          ibinun2, n2u, n1u, -rrate, this%naux, this%auxvar(:, i),             &
          .false., .false.)
      !
    enddo
    !
    ! -- Add the budget terms to model 2
    budterm(1, 1) = ratout
    budterm(2, 1) = ratin
    call this%m2%model_bdentry(budterm, budtxt, this%name)
    !
    ! -- Set icbcfl, ibudfl to zero so that flows will be printed and
    !    saved, if the options were set in the MVR package
    icbcfl = 1
    ibudfl = 1
    !
    ! -- Call mvr bd routine
    if(this%inmvr > 0) call this%mvr%mvr_bd(icbcfl, ibudfl, isuppress_output)
    !
    ! -- Calculate and write simulated values for observations
    if(this%inobs /= 0) then
      call this%gwf_lnf_save_simvals()
    endif
    !
    ! -- return
    return
  end subroutine gwf_lnf_bd

  subroutine gwf_lnf_ot(this)
! ******************************************************************************
! gwf_gwf_ot
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: iout
    use ConstantsModule, only: DZERO, LINELENGTH
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    ! -- local
    integer(I4B) :: iexg, n1, n2
    real(DP) :: flow, deltaqgnc
    character(len=LINELENGTH) :: node1str, node2str
    ! -- format
    character(len=*), parameter :: fmtheader =                                 &
     "(/1x, 'SUMMARY OF EXCHANGE RATES FOR EXCHANGE ', a, ' WITH ID ', i0, /,  &
       &2a16, 5a16, /, 112('-'))"
    character(len=*), parameter :: fmtheader2 =                                &
     "(/1x, 'SUMMARY OF EXCHANGE RATES FOR EXCHANGE ', a, ' WITH ID ', i0, /,  &
       &2a16, 4a16, /, 96('-'))"
    character(len=*), parameter :: fmtdata =                                   &
     "(2a16, 5(1pg16.6))"
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    deltaqgnc = DZERO
    !
    ! -- Write a table of exchanges
    if(this%iprflow /= 0) then
      if(this%ingnc > 0) then
        write(iout, fmtheader) trim(adjustl(this%name)), this%id, 'NODEM1',    &
                             'NODEM2', 'COND', 'X_M1', 'X_M2', 'DELTAQGNC',    &
                             'FLOW'
      else
        write(iout, fmtheader2) trim(adjustl(this%name)), this%id, 'NODEM1',   &
                             'NODEM2', 'COND', 'X_M1', 'X_M2', 'FLOW'
      endif
      do iexg = 1, this%nexg
        n1 = this%nodem1(iexg)
        n2 = this%nodem2(iexg)
        flow = this%qcalc(iexg, n1, n2)
        call this%m1%dis%noder_to_string(n1, node1str)
        call this%m2%dis%noder_to_string(n2, node2str)
        if(this%ingnc > 0) then
          deltaqgnc = this%gnc%deltaqgnc(iexg)
          write(iout, fmtdata) trim(adjustl(node1str)),                        &
                               trim(adjustl(node2str)),                        &
                               this%cond(iexg), this%m1%x(n1), this%m2%x(n2),  &
                               deltaqgnc, flow + deltaqgnc
        else
          write(iout, fmtdata) trim(adjustl(node1str)),                        &
                               trim(adjustl(node2str)),                        &
                               this%cond(iexg), this%m1%x(n1), this%m2%x(n2),  &
                               flow
        endif
      enddo
    endif
    !
    ! -- Mover budget output
    if(this%inmvr > 0) call this%mvr%mvr_ot()
    !
    ! -- OBS output
    call this%obs%obs_ot()
    !
    ! -- return
    return
  end subroutine gwf_lnf_ot

  subroutine read_options(this, iout)
! ******************************************************************************
! read_options -- Read Options
! Subroutine: (1) read options from input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ArrayHandlersModule, only: ifind
    use ConstantsModule, only: LINELENGTH, DEM6
    use InputOutputModule, only: getunit, openfile, urdaux
    use SimModule, only: store_error, store_error_unit, ustop
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, keyword, fname
    integer(I4B) :: istart,istop,lloc,ierr,ival
    integer(I4B) :: inobs
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr,                        &
      supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(iout,'(1x,a)')'PROCESSING GWF GWT EXCHANGE OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case('AUXILIARY')
            call this%parser%GetRemainingLine(line)
            lloc = 1
            call urdaux(this%naux, this%parser%iuactive, iout, lloc, istart,   &
                        istop, this%auxname, line, 'GWF_GWT_Exchange')
            !
            ! -- If ANGLDEGX is an auxiliary variable, then anisotropy can be
            !    used in either model.  Store ANGLDEGX position in this%ianglex
            ival = ifind(this%auxname, 'ANGLDEGX')
            if(ival > 0) this%ianglex = ival
            ival = ifind(this%auxname, 'CDIST')
            if(ival > 0) this%icdist = ival
          case ('PRINT_INPUT')
            this%iprpak = 1
            write(iout,'(4x,a)') &
              'THE LIST OF EXCHANGES WILL BE PRINTED.'
          case ('PRINT_FLOWS')
            this%iprflow = 1
            write(iout,'(4x,a)') &
              'EXCHANGE FLOWS WILL BE PRINTED TO LIST FILES.'
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(iout,'(4x,a)') &
              'EXCHANGE FLOWS WILL BE SAVED TO BINARY BUDGET FILES.'
          case ('ALTERNATIVE_CELL_AVERAGING')
            call this%parser%GetStringCaps(keyword)
            select case(keyword)
            case('LOGARITHMIC')
              this%icellavg = 1
            case('AMT-LMK')
              this%icellavg = 2
            case default
              write(errmsg,'(4x,a,a)')'UNKNOWN CELL AVERAGING METHOD: ',       &
                                       trim(keyword)
              call store_error(errmsg)
              call this%parser%StoreErrorUnit()
              call ustop()
            end select
            write(iout,'(4x,a,a)')                                             &
              'CELL AVERAGING METHOD HAS BEEN SET TO: ', trim(keyword)
          case ('VARIABLECV')
            this%ivarcv = 1
            write(iout,'(4x,a)')                                               &
              'VERTICAL CONDUCTANCE VARIES WITH WATER TABLE.'
            call this%parser%GetStringCaps(keyword)
            if(keyword == 'DEWATERED') then
              this%idewatcv = 1
              write(iout,'(4x,a)')                                             &
                'VERTICAL CONDUCTANCE ACCOUNTS FOR DEWATERED PORTION OF   ' // &
                'AN UNDERLYING CELL.'
            endif
          case ('NEWTON')
            this%inewton = 1
            write(iout, '(4x,a)')                                              &
                             'NEWTON-RAPHSON method used for unconfined cells'
          case ('GNC6')
            call this%parser%GetStringCaps(keyword)
            if(keyword /= 'FILEIN') then
              call store_error('GNC6 KEYWORD MUST BE FOLLOWED BY ' //          &
                '"FILEIN" then by filename.')
              call this%parser%StoreErrorUnit()
              call ustop()
            endif
            call this%parser%GetString(fname)
            if(fname == '') then
              call store_error('NO GNC6 FILE SPECIFIED.')
              call this%parser%StoreErrorUnit()
              call ustop()
            endif
            this%ingnc = getunit()
            call openfile(this%ingnc, iout, fname, 'GNC')
            write(iout,'(4x,a)')                                               &
              'GHOST NODES WILL BE READ FROM ', trim(fname)
          case ('MVR6')
            call this%parser%GetStringCaps(keyword)
            if(keyword /= 'FILEIN') then
              call store_error('MVR6 KEYWORD MUST BE FOLLOWED BY ' //          &
                '"FILEIN" then by filename.')
              call this%parser%StoreErrorUnit()
              call ustop()
            endif
            call this%parser%GetString(fname)
            if(fname == '') then
              call store_error('NO MVR6 FILE SPECIFIED.')
              call this%parser%StoreErrorUnit()
              call ustop()
            endif
            this%inmvr = getunit()
            call openfile(this%inmvr, iout, fname, 'MVR')
            write(iout,'(4x,a)')                                               &
              'WATER MOVER INFORMATION WILL BE READ FROM ', trim(fname)
          case ('BOUNDNAMES')
            this%inamedbound = 1
            write(iout,'(4x,a)') 'EXCHANGE BOUNDARIES HAVE NAMES' // &
                                      ' IN LAST COLUMN.'
          case ('OBS6')
            call this%parser%GetStringCaps(keyword)
            if(keyword /= 'FILEIN') then
              call store_error('OBS8 KEYWORD MUST BE FOLLOWED BY ' //         &
                '"FILEIN" then by filename.')
              call this%parser%StoreErrorUnit()
              call ustop()
            endif
            this%obs%active = .true.
            call this%parser%GetString(this%obs%inputFilename)
            inobs = GetUnit()
            call openfile(inobs, iout, this%obs%inputFilename, 'OBS')
            this%obs%inUnitObs = inobs
          case default
            write(errmsg,'(4x,a,a)')'***ERROR. UNKNOWN GWF EXCHANGE OPTION: ', &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(iout,'(1x,a)')'END OF GWF EXCHANGE OPTIONS'
    end if
    !
    ! -- set omega value used for saturation calculations
    if (this%inewton > 0) then
      this%satomega = DEM6
    end if
    !
    ! -- return
    return
  end subroutine read_options

  subroutine read_data(this, iout)
! ******************************************************************************
! read_data -- Read EXGDATA block
! Subroutine: (1) read list of EXGs from input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, store_error_unit, count_errors
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LINELENGTH) :: errmsg, nodestr, node1str, node2str, cellid
    character(len=2) :: cnfloat
    integer(I4B) :: lloc, ierr, nerr, iaux
    integer(I4B) :: iexg, nodem1, nodem2, nodeum1, nodeum2
    logical :: isfound, endOfBlock
    ! -- format
    character(len=*), parameter :: fmtexglabel = "(5x, 8a10, 50(a16))"
    character(len=*), parameter :: fmtexgdata  =                               &
      "(5x, a, 1x, a, i10, i10, i10, i10, i10, f10.3, f10.3, 50(1pg16.6))"
    character(len=40) :: fmtexgdata2
! ------------------------------------------------------------------------------
    !
    ! -- get ExchangeData block
    call this%parser%GetBlock('EXCHANGEDATA', isfound, ierr,                   &
                              supportOpenClose=.true.)
    !
    ! -- parse ExchangeData block if detected
    if (isfound) then
      write(iout,'(1x,a)')'PROCESSING EXCHANGEDATA'
      if(this%iprpak /= 0) then
        if (this%inamedbound==0) then
          write(iout, fmtexglabel) 'NODEM1', 'NODEM2', 'IFDC', 'ICELLTYPE',    &
              'IFLOWTYPE', 'FLENGW', 'FSKIN1', 'FSKIN2',                       &
              (adjustr(this%auxname(iaux)), iaux = 1, this%naux)
        else
          write(iout, fmtexglabel) 'NODEM1', 'NODEM2', 'IFDC', 'ICELLTYPE',    &
              'IFLOWTYPE', 'FLENGW', 'FSKIN1', 'FSKIN2',                       &
              (adjustr(this%auxname(iaux)),iaux=1,this%naux),                  &
              ' BOUNDNAME      '
          ! Define format suitable for writing input data,
          ! any auxiliary variables, and boundname.
          write(cnfloat,'(i0)') 3+this%naux
          fmtexgdata2 = '(5x, a, 1x, a, i10, i10, i10, i10, i10, f10.3, f10.3, '        &
             // trim(cnfloat) // '(1pg16.6), 1x, a)'  
            
        endif
      endif
      do iexg = 1, this%nexg
        call this%parser%GetNextLine(endOfBlock)
        lloc = 1
        !
        ! -- Read and check node 1
        call this%parser%GetCellid(this%m1%dis%ndim, cellid, flag_string=.true.)
        nodem1 = this%m1%dis%noder_from_cellid(cellid, this%parser%iuactive,   &
                                               iout, flag_string=.true.)
        this%nodem1(iexg) = nodem1
        !
        ! -- Read and check node 2
        call this%parser%GetCellid(this%m2%dis%ndim, cellid, flag_string=.true.)
        nodem2 = this%m2%dis%noder_from_cellid(cellid, this%parser%iuactive,   &
                                               iout, flag_string=.true.)
        this%nodem2(iexg) = nodem2
        !
        ! -- Read rest of input line
        this%ihc(iexg) = this%parser%GetInteger()
        this%ifdc(iexg) = this%parser%GetInteger()
        this%icelltype(iexg) = this%parser%GetInteger()
        this%iflowtype(iexg) = this%parser%GetInteger()
        this%flengw(iexg) = this%parser%GetDouble()
        this%fskin1(iexg) = this%parser%GetDouble()
        this%fskin2(iexg) = this%parser%GetDouble()
        do iaux = 1, this%naux
          this%auxvar(iaux, iexg) = this%parser%GetDouble()
        enddo
        if (this%inamedbound==1) then
          call this%parser%GetStringCaps(this%boundname(iexg))
        endif
        !
        ! -- Write the data to listing file if requested
        if(this%iprpak /= 0) then
          nodeum1 = this%m1%dis%get_nodeuser(nodem1)
          call this%m1%dis%nodeu_to_string(nodeum1, node1str)
          nodeum2 = this%m2%dis%get_nodeuser(nodem2)
          call this%m2%dis%nodeu_to_string(nodeum2, node2str)
          if (this%inamedbound == 0) then
            write(iout, fmtexgdata) trim(node1str), trim(node2str),            &
                        this%ifdc(iexg), this%icelltype(iexg),                 & 
                        this%iflowtype(iexg), this%fskin1(iexg),               &
                        this%fskin2(iexg),                                     &
                        (this%auxvar(iaux, iexg), iaux=1,this%naux)
          else
            write(iout, fmtexgdata2) trim(node1str), trim(node2str),           &
                        this%ifdc(iexg), this%icelltype(iexg),                 & 
                        this%iflowtype(iexg), this%fskin1(iexg),               &
                        this%fskin2(iexg),                                     &
                        (this%auxvar(iaux, iexg), iaux=1,this%naux),           &
                        trim(this%boundname(iexg))
          endif
        endif
        !
        ! -- Check to see if nodem1 is outside of active domain
        if(nodem1 <= 0) then
          call this%gwfmodel%dis%nodeu_to_string(nodeum1, nodestr)
          write(errmsg, *)                                                     &
                  trim(adjustl(this%gwfmodel%name)) //                        &
                  ' Cell is outside active grid domain: ' //                   &
                  trim(adjustl(nodestr))
          call store_error(errmsg)
        endif
        !
        ! -- Check to see if nodem2 is outside of active domain
        if(nodem2 <= 0) then
          call this%lnfmodel%dis%nodeu_to_string(nodeum2, nodestr)
          write(errmsg, *)                                                     &
                  trim(adjustl(this%lnfmodel%name)) //                        &
                  ' Cell is outside active grid domain: ' //                   &
                  trim(adjustl(nodestr))
          call store_error(errmsg)
        endif
      enddo
      !
      ! -- Stop if errors
      nerr = count_errors()
      if(nerr > 0) then
        call store_error('Errors encountered in exchange input file.')
        call this%parser%StoreErrorUnit()
        call ustop()
      endif
      !
      write(iout,'(1x,a)')'END OF EXCHANGEDATA'
    else
      write(errmsg, '(1x,a)')'ERROR.  REQUIRED EXCHANGEDATA BLOCK NOT FOUND.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine read_data

  subroutine read_gnc(this, iout)
! ******************************************************************************
! read_gnc -- Read ghost node information.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error, store_error_unit, count_errors, ustop
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: i, nm1, nm2, nmgnc1, nmgnc2
    character(len=LINELENGTH) :: errmsg
    character(len=*), parameter :: fmterr = &
      "('EXCHANGE NODES ', i0, ' AND ', i0,"  // &
      "' NOT CONSISTENT WITH GNC NODES ', i0, ' AND ', i0)"
! ------------------------------------------------------------------------------
    !
    ! -- If exchange has ghost nodes, then initialize ghost node object
    !    This will read the ghost node blocks from the gnc input file.
    call this%gnc%gnc_df(this%m1, m2=this%m2)
    !
    ! -- Verify gnc is implicit if exchange has Newton Terms
    if(.not. this%gnc%implicit .and. this%inewton /= 0) then
      call store_error('GNC IS EXPLICIT, BUT GWF EXCHANGE HAS ACTIVE NEWTON.')
      call store_error('ADD IMPLICIT OPTION TO GNC OR REMOVE NEWTON FROM ' // &
        'GWF EXCHANGE.')
      call store_error_unit(this%ingnc)
      call ustop()
    endif
    !
    ! -- Perform checks to ensure GNCs match with GWF-GWF nodes
    if(this%nexg /= this%gnc%nexg) then
      call store_error('NUMBER OF EXCHANGES DOES NOT MATCH NUMBER OF GNCs')
      call store_error_unit(this%ingnc)
      call ustop()
    endif
    !
    ! -- Go through each entry and confirm
    do i = 1, this%nexg
      if(this%nodem1(i) /= this%gnc%nodem1(i) .or.                             &
          this%nodem2(i) /= this%gnc%nodem2(i) ) then
        nm1 = this%gwfmodel%dis%get_nodeuser(this%nodem1(i))
        nm2 = this%lnfmodel%dis%get_nodeuser(this%nodem2(i))
        nmgnc1 = this%gwfmodel%dis%get_nodeuser(this%gnc%nodem1(i))
        nmgnc2 = this%lnfmodel%dis%get_nodeuser(this%gnc%nodem2(i))
        write(errmsg, fmterr) nm1, nm2, nmgnc1, nmgnc2
        call store_error(errmsg)
      endif
    enddo
    if(count_errors() > 0) then
      call store_error_unit(this%ingnc)
      call ustop()
    endif
    !
    ! -- close the file
    close(this%ingnc)
    !
    ! -- return
    return
  end subroutine read_gnc

  subroutine read_mvr(this, iout)
! ******************************************************************************
! read_mvr -- Read water mover information.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwfMvrModule, only: mvr_cr
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer(I4B), intent(in) :: iout
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Create and initialize the mover object
    call mvr_cr(this%mvr, this%name, this%inmvr, iout, iexgmvr=1)
    !
    ! -- Return
    return
  end subroutine read_mvr

  subroutine rewet(this, kiter)
! ******************************************************************************
! rewet -- Check for rewetting across models
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kper, kstp
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer(I4B), intent(in) :: kiter
    ! -- local
    integer(I4B) :: iexg
    integer(I4B) :: n, m
    integer(I4B) :: ibdn, ibdm
    integer(I4B) :: ihc
    real(DP) :: hn, hm
    integer(I4B) :: irewet
    character(len=30) :: nodestrn, nodestrm
    character(len=*),parameter :: fmtrwt =                                     &
      "(1x, 'CELL ',A,' REWET FROM GWF MODEL ',A,' CELL ',A,                   &
       &' FOR ITER. ',I0, ' STEP ',I0, ' PERIOD ', I0)"
! ------------------------------------------------------------------------------
    !
    ! -- Use model 1 to rewet model 2 and vice versa
    do iexg = 1, this%nexg
      n = this%nodem1(iexg)
      m = this%nodem2(iexg)
      hn = this%gwfmodel%x(n)
      hm = this%lnfmodel%x(m)
      ibdn = this%gwfmodel%ibound(n)
      ibdm = this%lnfmodel%ibound(m)
      ihc = this%ihc(iexg)
      call this%gwfmodel%npf%rewet_check(kiter, n, hm, ibdm, ihc,             &
        this%gwfmodel%x, irewet)
      if(irewet == 1) then
        call this%gwfmodel%dis%noder_to_string(n, nodestrn)
        call this%lnfmodel%dis%noder_to_string(m, nodestrm)
        write(this%gwfmodel%iout, fmtrwt) trim(nodestrn),                     &
          trim(this%lnfmodel%name), trim(nodestrm), kiter, kstp, kper
      endif
      call this%lnfmodel%npf%rewet_check(kiter, m, hn, ibdn, ihc,             &
        this%lnfmodel%x, irewet)
      if(irewet == 1) then
        call this%gwfmodel%dis%noder_to_string(n, nodestrm)
        call this%lnfmodel%dis%noder_to_string(m, nodestrn)
        write(this%lnfmodel%iout, fmtrwt) trim(nodestrn),                     &
          trim(this%gwfmodel%name), trim(nodestrm), kiter, kstp, kper
      endif
      !
    enddo
    !
    ! -- Return
    return
  end subroutine rewet

  subroutine condcalc(this)
! ******************************************************************************
! condcalc -- Calculate the conductance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DHALF, DZERO, DONE
    use GwfNpfModule, only: hcond, vcond
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    ! -- local
    integer(I4B) :: iexg
    integer(I4B) :: n, m, ihc
    integer(I4B) :: ibdn, ibdm
    integer(I4B) :: ictn, ictm
    real(DP) :: topn, topm
    real(DP) :: botn, botm
    real(DP) :: satn, satm
    real(DP) :: hyn, hym
    real(DP) :: angle
    real(DP) :: hn, hm
    real(DP) :: cond
    real(DP) :: fawidth
    real(DP), dimension(3) :: vg
! ------------------------------------------------------------------------------
    !
    ! -- calculate the conductance for each exchange connection
    do iexg = 1, this%nexg
      this%cond(iexg) = this%condsat(iexg) * this%akrc(iexg)
    enddo  
    !
    ! -- Return
    return
  end subroutine condcalc

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: LENORIGIN, DZERO
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    ! -- local
    character(len=LENORIGIN) :: origin
! ------------------------------------------------------------------------------
    !
    ! -- create the origin name
    origin = trim(this%name)
    !
    ! -- Call parent type allocate_scalars
    call this%NumericalExchangeType%allocate_scalars()
    !
    call mem_allocate(this%icellavg, 'ICELLAVG', origin)
    call mem_allocate(this%ivarcv, 'IVARCV', origin)
    call mem_allocate(this%idewatcv, 'IDEWATCV', origin)
    call mem_allocate(this%inewton, 'INEWTON', origin)
    call mem_allocate(this%iflowdrycell, 'IFLOWDRYCELL', origin)    
    call mem_allocate(this%ianglex, 'IANGLEX', origin)
    call mem_allocate(this%icdist, 'ICDIST', origin)
    call mem_allocate(this%ingnc, 'INGNC', origin)
    call mem_allocate(this%inmvr, 'INMVR', origin)
    call mem_allocate(this%inobs, 'INOBS', origin)
    call mem_allocate(this%inamedbound, 'INAMEDBOUND', origin)
    call mem_allocate(this%satomega, 'SATOMEGA', origin)
    this%icellavg = 0
    this%ivarcv = 0
    this%idewatcv = 0
    this%inewton = 0
    this%iflowdrycell = 0
    this%ianglex = 0
    this%icdist = 0
    this%ingnc = 0
    this%inmvr = 0
    this%inobs = 0
    this%inamedbound = 0
    this%satomega = DZERO
    !
    ! -- return
    return
  end subroutine allocate_scalars

  subroutine gwf_lnf_da(this)
! ******************************************************************************
! gwf_gwf_da
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Call parent type allocate_scalars
    call this%NumericalExchangeType%exg_da()
    !
    ! -- objects
    if(this%ingnc > 0) then
      call this%gnc%gnc_da()
      deallocate(this%gnc)
    endif
    if (this%inmvr > 0) then
      call this%mvr%mvr_da()
      deallocate(this%mvr)
    endif
    call this%obs%obs_da()
    deallocate(this%obs)
    !
    ! -- arrays
    call mem_deallocate(this%ihc)
    call mem_deallocate(this%ifdc)
    call mem_deallocate(this%icelltype)
    call mem_deallocate(this%iflowtype)
    call mem_deallocate(this%flengw)
    call mem_deallocate(this%fskin1)
    call mem_deallocate(this%fskin2)
    call mem_deallocate(this%condsat)
    call mem_deallocate(this%cl1)
    call mem_deallocate(this%cl2)    
    call mem_deallocate(this%fahl)    
    call mem_deallocate(this%akrc)
    deallocate(this%boundname)
    !
    ! -- output table objects
    if (associated(this%outputtab1)) then
      call this%outputtab1%table_da()
      deallocate(this%outputtab1)
      nullify(this%outputtab1)
    end if
    if (associated(this%outputtab2)) then
      call this%outputtab2%table_da()
      deallocate(this%outputtab2)
      nullify(this%outputtab2)
    end if
    !
    ! -- scalars
    call mem_deallocate(this%icellavg)
    call mem_deallocate(this%ivarcv)
    call mem_deallocate(this%idewatcv)
    call mem_deallocate(this%inewton)
    call mem_deallocate(this%iflowdrycell)
    call mem_deallocate(this%ianglex)
    call mem_deallocate(this%icdist)
    call mem_deallocate(this%ingnc)
    call mem_deallocate(this%inmvr)
    call mem_deallocate(this%inobs)
    call mem_deallocate(this%inamedbound)
    call mem_deallocate(this%satomega)
    !
    ! -- return
    return
  end subroutine gwf_lnf_da

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: LENORIGIN
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    ! -- local
    character(len=LINELENGTH) :: text
    character(len=LENORIGIN) :: origin
    integer(I4B) :: ntabcol
! ------------------------------------------------------------------------------
    !
    ! -- create the origin name
    origin = trim(this%name)
    !
    ! -- Call parent type allocate_scalars
    call this%NumericalExchangeType%allocate_arrays()
    !
    call mem_allocate(this%ihc, this%nexg, 'IHC', origin)
    call mem_allocate(this%ifdc, this%nexg, '', origin)
    call mem_allocate(this%icelltype, this%nexg, '', origin)
    call mem_allocate(this%iflowtype, this%nexg, '', origin)
    call mem_allocate(this%flengw, this%nexg, '', origin)
    call mem_allocate(this%fskin1, this%nexg, '', origin)
    call mem_allocate(this%fskin2, this%nexg, '', origin)
    call mem_allocate(this%condsat, this%nexg, 'CONDSAT', origin)
    call mem_allocate(this%cl1, this%nexg, 'CL1', origin)
    call mem_allocate(this%cl2, this%nexg, 'CL2', origin)
    call mem_allocate(this%fahl, this%nexg, 'FAHL', origin)
    call mem_allocate(this%akrc, this%nexg, 'AKRC', origin)   
    !
    ! -- Allocate boundname
    if(this%inamedbound==1) then
      allocate(this%boundname(this%nexg))
    else
      allocate(this%boundname(1))
    endif
    this%boundname(:) = ''
    !
    ! -- allocate and initialize the output table
    if (this%iprflow /= 0) then
      !
      ! -- dimension table
      ntabcol = 3
      if (this%inamedbound > 0) then
        ntabcol = ntabcol + 1
      end if
      !
      ! -- initialize the output table objects
      !    outouttab1
      call table_cr(this%outputtab1, this%name, '    ')
      call this%outputtab1%table_df(this%nexg, ntabcol, this%gwfmodel%iout,     &
                                    transient=.TRUE.)
      text = 'NUMBER'
      call this%outputtab1%initialize_column(text, 10, alignment=TABCENTER)
      text = 'CELLID'
      call this%outputtab1%initialize_column(text, 20, alignment=TABLEFT)
      text = 'RATE'
      call this%outputtab1%initialize_column(text, 15, alignment=TABCENTER)
      if (this%inamedbound > 0) then
        text = 'NAME'
        call this%outputtab1%initialize_column(text, 20, alignment=TABLEFT)
      end if
      !    outouttab2
      call table_cr(this%outputtab2, this%name, '    ')
      call this%outputtab2%table_df(this%nexg, ntabcol, this%lnfmodel%iout,     &
                                    transient=.TRUE.)
      text = 'NUMBER'
      call this%outputtab2%initialize_column(text, 10, alignment=TABCENTER)
      text = 'CELLID'
      call this%outputtab2%initialize_column(text, 20, alignment=TABLEFT)
      text = 'RATE'
      call this%outputtab2%initialize_column(text, 15, alignment=TABCENTER)
      if (this%inamedbound > 0) then
        text = 'NAME'
        call this%outputtab2%initialize_column(text, 20, alignment=TABLEFT)
      end if
    end if
    !
    ! -- return
    return
  end subroutine allocate_arrays

  subroutine gwf_lnf_df_obs(this)
! ******************************************************************************
! gwf_lnf_df_obs
!   -- Store observation type supported by GWF-GWF exchange.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    ! -- local
    integer(I4B) :: indx
! ------------------------------------------------------------------------------
    !
    ! -- Store obs type and assign procedure pointer
    !    for gwf-gwf observation type.
    call this%obs%StoreObsType('flow-ja-face', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => gwf_lnf_process_obsID
    !
    ! -- return
    return
  end subroutine gwf_lnf_df_obs

  subroutine gwf_lnf_rp_obs(this)
! ******************************************************************************
! gwf_lnf_rp_obs
!   -- Handle observation IDs that are exchange-boundary names.
!      Store exchange numbers included in observation.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    ! -- local
    integer(I4B) :: i, j, n
    class(ObserveType), pointer :: obsrv => null()
    character(len=LENBOUNDNAME) :: bname
    character(len=1000) :: ermsg
    logical :: jfound
    ! -- formats
10  format('Error: Boundary "',a,'" for observation "',a,               &
           '" is invalid in package "',a,'"')
! ------------------------------------------------------------------------------
    !
    do i=1,this%obs%npakobs
      obsrv => this%obs%pakobs(i)%obsrv
      !
      ! -- indxbnds needs to be deallocated and reallocated (using
      !    ExpandArray) each stress period because list of boundaries
      !    can change each stress period.
      ! -- Not true for exchanges, but leave this in for now anyway.
      if (allocated(obsrv%indxbnds)) then
        deallocate(obsrv%indxbnds)
      endif
      obsrv%BndFound = .false.
      !
      bname = obsrv%FeatureName
      if (bname /= '') then
        ! -- Observation location(s) is(are) based on a boundary name.
        !    Iterate through all boundaries to identify and store
        !    corresponding index(indices) in bound array.
        jfound = .false.
        do j=1,this%nexg
          if (this%boundname(j) == bname) then
            jfound = .true.
            obsrv%BndFound = .true.
            obsrv%CurrentTimeStepEndValue = DZERO
            call ExpandArray(obsrv%indxbnds)
            n = size(obsrv%indxbnds)
            obsrv%indxbnds(n) = j
          endif
        enddo
        if (.not. jfound) then
          write(ermsg,10)trim(bname)
          call store_error(ermsg)
        endif
      else
        ! -- Observation location is a single exchange number
        if (obsrv%intPak1 <= this%nexg) then
          jfound = .true.
          obsrv%BndFound = .true.
          obsrv%CurrentTimeStepEndValue = DZERO
          call ExpandArray(obsrv%indxbnds)
          n = size(obsrv%indxbnds)
          obsrv%indxbnds(n) = obsrv%intPak1
        else
          jfound = .false.
        endif
      endif
    enddo
    !
    if (count_errors() > 0) then
      call store_error_unit(this%inobs)
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine gwf_lnf_rp_obs

  subroutine gwf_lnf_fp(this)
! ******************************************************************************
! gwf_lnf_fp
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfLnfExchangeType) :: this
! ------------------------------------------------------------------------------
    !
    return
  end subroutine gwf_lnf_fp
  
  function qcalc(this, iexg, n1, n2)
! ******************************************************************************
! qcalc -- calculate flow between two cells, positive into n1
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: qcalc
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    integer(I4B), intent(in) :: iexg
    integer(I4B), intent(in) :: n1
    integer(I4B), intent(in) :: n2
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Calculate flow between nodes in the two models
    qcalc = this%cond(iexg) * this%m2%x(n2) - this%cond(iexg) * this%m1%x(n1)
    !
    ! -- return
    return
  end function qcalc

  function gwf_lnf_get_iasym(this) result (iasym)
! ******************************************************************************
! gwf_lnf_get_iasym -- return 1 if any option causes the matrix to be asymmetric.
!   Otherwise return 0.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfLnfExchangeType) :: this
    ! -- local
    integer(I4B) :: iasym
! ------------------------------------------------------------------------------
    !
    ! -- Start by setting iasym to zero
    iasym = 0
    !
    ! -- Groundwater flow
    if (this%inewton /= 0) iasym = 1
    !
    ! -- GNC
    if (this%ingnc > 0) then
      if (this%gnc%iasym /= 0) iasym = 1
    endif
    !
    ! -- return
    return
  end function gwf_lnf_get_iasym

  subroutine gwf_lnf_save_simvals(this)
! ******************************************************************************
! gwf_lnf_save_simvals
!   -- Calculate observations this time step and call
!      ObsType%SaveOneSimval for each GWF-LNF Type observation.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    use SimModule, only: store_error, store_error_unit, ustop
    use ConstantsModule, only: DZERO
    use ObserveModule, only: ObserveType
    class(GwfLnfExchangeType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, j, n1, n2, nbndobs
    integer(I4B) :: iexg
    real(DP) :: v
    character(len=100) :: msg
    type(ObserveType), pointer :: obsrv => null()
! ------------------------------------------------------------------------------
    !
    ! -- Write simulated values for all gwf-lnf observations
    if (this%obs%npakobs > 0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        nbndobs = size(obsrv%indxbnds)
        do j = 1,  nbndobs
          iexg = obsrv%indxbnds(j)
          v = DZERO
          select case (obsrv%ObsTypeId)
          case ('FLOW-JA-FACE')
            n1 = this%nodem1(iexg)
            n2 = this%nodem2(iexg)
            v = this%qcalc(iexg, n1, n2)
            if(this%ingnc > 0) then
              v = v + this%gnc%deltaqgnc(iexg)
            endif
          case default
            msg = 'Error: Unrecognized observation type: ' //                  &
                  trim(obsrv%ObsTypeId)
            call store_error(msg)
            call store_error_unit(this%inobs)
            call ustop()
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        enddo
      enddo
    endif
    !
    return
  end subroutine gwf_lnf_save_simvals

  subroutine gwf_lnf_process_obsID(obsrv, dis, inunitobs, iout)
! ******************************************************************************
! -- This procedure is pointed to by ObsDataType%ProcesssIdPtr. It processes
!    the ID string of an observation definition for GWF-LNF-package observations
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use InputOutputModule, only: urword
    use ObserveModule, only: ObserveType
    use BaseDisModule, only: DisBaseType
    ! -- dummy
    type(ObserveType),      intent(inout) :: obsrv
    class(DisBaseType), intent(in)    :: dis
    integer(I4B),            intent(in)    :: inunitobs
    integer(I4B),            intent(in)    :: iout
    ! -- local
    integer(I4B) :: n, iexg, istat
    integer(I4B) :: icol, istart, istop
    real(DP) :: r
    character(len=LINELENGTH) :: strng
! ------------------------------------------------------------------------------
    !
    strng = obsrv%IDstring
    icol = 1
    ! -- get exchange index
    call urword(strng, icol, istart, istop, 0, n, r, iout, inunitobs)
    read (strng(istart:istop), '(i10)', iostat=istat) iexg
    if (istat == 0) then
      obsrv%intPak1 = iexg
    else
      ! Integer can't be read from strng; it's presumed to be an exchange
      ! boundary name (already converted to uppercase)
      obsrv%FeatureName = strng(istart:istop)
      ! -- Observation may require summing rates from multiple exchange
      !    boundaries, so assign intPak1 as a value that indicates observation
      !    is for a named exchange boundary or group of exchange boundaries.
      obsrv%intPak1 = NAMEDBOUNDFLAG
    endif
    !
    return
  end subroutine gwf_lnf_process_obsID

end module GwfLnfExchangeModule


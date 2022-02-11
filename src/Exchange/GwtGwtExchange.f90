!> @brief This module contains the GwtGwtExchangeModule Module
!!
!! This module contains the code for connecting two GWT Models.
!! The methods are based on the simple two point flux approximation
!! with the option to use ghost nodes to improve accuracy.  This
!! exchange is used by GwtGwtConnection with the more sophisticated
!! interface model coupling approach when XT3D is needed.
!!
!<
module GwtGwtExchangeModule

  use KindModule,              only: DP, I4B, LGP
  use SimVariablesModule,      only: errmsg  
  use SimModule,               only: store_error
  use BaseModelModule,         only: BaseModelType, GetBaseModelFromList
  use BaseExchangeModule,      only: BaseExchangeType, AddBaseExchangeToList
  use ConstantsModule,         only: LENBOUNDNAME, NAMEDBOUNDFLAG, LINELENGTH, &
                                     TABCENTER, TABLEFT, LENAUXNAME, DNODATA
  use ListModule,              only: ListType
  use ListsModule,             only: basemodellist
  use DisConnExchangeModule,   only: DisConnExchangeType
  use GwtModule,               only: GwtModelType
  !cdl use GhostNodeModule,         only: GhostNodeType
  !cdl use GwtMvrModule,            only: GwtMvrType
  use ObserveModule,           only: ObserveType
  use ObsModule,               only: ObsType
  use SimModule,               only: count_errors, store_error, store_error_unit
  use SimVariablesModule,      only: errmsg
  use BlockParserModule,       only: BlockParserType
  use TableModule,             only: TableType, table_cr

  implicit none

  private
  public :: GwtExchangeType
  public :: gwtexchange_create
  public :: GetGwtExchangeFromList
  public :: CastAsGwtExchange

  !> @brief Derived type for GwtExchangeType 
  !!
  !! This derived type contains information and methods for
  !! connecting two GWT models.
  !!
  !<
  type, extends(DisConnExchangeType) :: GwtExchangeType
    type(GwtModelType), pointer                      :: gwtmodel1   => null()    !< pointer to GWT Model 1
    type(GwtModelType), pointer                      :: gwtmodel2   => null()    !< pointer to GWT Model 2
    ! 
    ! -- GWT specific option block:    
    integer(I4B), pointer                            :: iprflow     => null()    !< print flag for cell by cell flows
    integer(I4B), pointer                            :: ipakcb      => null()    !< save flag for cell by cell flows
    integer(I4B), pointer                            :: iAdvScheme               !< the advection scheme at the interface:
                                                                                 !! 0 = upstream, 1 = central, 2 = TVD

    !cdl integer(I4B), pointer                            :: inewton     => null()    !< newton flag (1 newton is on)
    !cdl integer(I4B), pointer                            :: icellavg    => null()    !< cell averaging
    !cdl integer(I4B), pointer                            :: ivarcv      => null()    !< variable cv
    !cdl integer(I4B), pointer                            :: idewatcv    => null()    !< dewatered cv
    !cdl integer(I4B), pointer                            :: ingnc       => null()    !< unit number for gnc (0 if off)
    !cdl type(GhostNodeType), pointer                     :: gnc         => null()    !< gnc object
    !cdl integer(I4B), pointer                            :: inmvr       => null()    !< unit number for mover (0 if off)
    !cdl type(GwtMvrType), pointer                        :: mvr         => null()    !< water mover object
    integer(I4B), pointer                            :: inobs       => null()    !< unit number for GWT-GWT observations
    type(ObsType), pointer                           :: obs         => null()    !< observation object
    !
    ! -- internal data
    real(DP), dimension(:), pointer, contiguous      :: cond        => null()    !< conductance
    !cdl real(DP), dimension(:), pointer, contiguous      :: condsat     => null()    !< saturated conductance
    !cdl integer(I4B), dimension(:), pointer, contiguous  :: idxglo      => null()    !< mapping to global (solution) amat
    !cdl integer(I4B), dimension(:), pointer, contiguous  :: idxsymglo   => null()    !< mapping to global (solution) symmetric amat
    !cdl real(DP), pointer                                :: satomega    => null()    !< saturation smoothing
    real(DP), dimension(:), pointer, contiguous      :: simvals     => null()    !< simulated flow rate for each exchange
    !
    ! -- table objects
    type(TableType), pointer :: outputtab1 => null()
    type(TableType), pointer :: outputtab2 => null()    

  contains

    procedure          :: exg_df      => gwt_gwt_df
    !cdl procedure          :: exg_ac      => gwt_gwt_ac
    !cdl procedure          :: exg_mc      => gwt_gwt_mc
    procedure          :: exg_ar      => gwt_gwt_ar
    procedure          :: exg_rp      => gwt_gwt_rp
    procedure          :: exg_ad      => gwt_gwt_ad
    !cdl procedure          :: exg_cf      => gwt_gwt_cf
    !cdl procedure          :: exg_fc      => gwt_gwt_fc
    !cdl procedure          :: exg_fn      => gwt_gwt_fn
    !cdl procedure          :: exg_cq      => gwt_gwt_cq
    procedure          :: exg_bd      => gwt_gwt_bd
    procedure          :: exg_ot      => gwt_gwt_ot
    procedure          :: exg_da      => gwt_gwt_da
    procedure          :: exg_fp      => gwt_gwt_fp
    !cdl procedure          :: get_iasym   => gwt_gwt_get_iasym
    procedure          :: connects_model => gwt_gwt_connects_model
    procedure          :: use_interface_model
    procedure          :: allocate_scalars
    procedure          :: allocate_arrays
    procedure          :: read_options
    procedure          :: parse_option
    !cdl procedure          :: read_gnc
    !cdl procedure          :: read_mvr
    !cdl procedure, private :: condcalc
    !cdl procedure, private :: rewet
    !cdl procedure, private :: qcalc
    procedure          :: gwt_gwt_bdsav
    procedure, private :: gwt_gwt_df_obs
    procedure, private :: gwt_gwt_rp_obs
    procedure, public  :: gwt_gwt_save_simvals
    !cdl procedure, private :: gwt_gwt_calc_simvals
    !cdl procedure, public  :: gwt_gwt_set_spdis
    procedure, private :: validate_exchange
  end type GwtExchangeType

contains

  !> @ brief Create GWT GWT exchange
  !!
  !! Create a new GWT to GWT exchange object.
  !!
  !<
  subroutine gwtexchange_create(filename, id, m1id, m2id)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use BaseModelModule, only: BaseModelType
    use ListsModule, only: baseexchangelist
    use ObsModule, only: obs_cr
    use MemoryHelperModule, only: create_mem_path
    ! -- dummy
    character(len=*),intent(in) :: filename   !< filename for reading
    integer(I4B), intent(in) :: id            !< id for the exchange
    integer(I4B), intent(in) :: m1id          !< id for model 1
    integer(I4B), intent(in) :: m2id          !< id for model 2
    ! -- local
    type(GwtExchangeType), pointer :: exchange
    class(BaseModelType), pointer :: mb
    class(BaseExchangeType), pointer :: baseexchange
    character(len=20) :: cint
    !
    ! -- Create a new exchange and add it to the baseexchangelist container
    allocate(exchange)
    baseexchange => exchange
    call AddBaseExchangeToList(baseexchangelist, baseexchange)
    !
    ! -- Assign id and name
    exchange%id = id
    write(cint, '(i0)') id
    exchange%name = 'GWT-GWT_' // trim(adjustl(cint))
    exchange%memoryPath = create_mem_path(exchange%name)
    !
    ! -- allocate scalars and set defaults
    call exchange%allocate_scalars()
    exchange%filename = filename
    exchange%typename = 'GWT-GWT'
    exchange%iAdvScheme = 0
    exchange%ixt3d = 1
    !
    ! -- set gwtmodel1
    mb => GetBaseModelFromList(basemodellist, m1id)    
    select type (mb)
    type is (GwtModelType)
      exchange%model1 => mb
      exchange%gwtmodel1 => mb
    end select
    !
    ! -- set gwtmodel2
    mb => GetBaseModelFromList(basemodellist, m2id)
    select type (mb)
    type is (GwtModelType)
      exchange%model2 => mb
      exchange%gwtmodel2 => mb
    end select
    !
    ! -- Create the obs package
    call obs_cr(exchange%obs, exchange%inobs)
    !
    ! -- return
    return
  end subroutine gwtexchange_create

  !> @ brief Define GWT GWT exchange
  !!
  !! Define GWT to GWT exchange object.
  !!
  !<
  subroutine gwt_gwt_df(this)
    ! -- modules
    use SimVariablesModule, only: iout
    use InputOutputModule, only: getunit, openfile
    use GhostNodeModule, only: gnc_cr
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    ! -- local
    integer(I4B) :: inunit
    !
    ! -- open the file
    inunit = getunit()
    write(iout,'(/a,a)') ' Creating exchange: ', this%name
    call openfile(inunit, iout, this%filename, 'GWT-GWT')
    !
    call this%parser%Initialize(inunit, iout)
    !
    ! -- Ensure models are in same solution
    if(this%gwtmodel1%idsoln /= this%gwtmodel2%idsoln) then
      call store_error('ERROR.  TWO MODELS ARE CONNECTED ' //                  &
        'IN A GWT EXCHANGE BUT THEY ARE IN DIFFERENT SOLUTIONS. ' //           &
        'GWT MODELS MUST BE IN SAME SOLUTION: ' //                             &
        trim(this%gwtmodel1%name) // ' ' // trim(this%gwtmodel2%name) )
      call this%parser%StoreErrorUnit()
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
    !cdl call this%gwtmodel1%npf%increase_edge_count(this%nexg)
    !cdl call this%gwtmodel2%npf%increase_edge_count(this%nexg)
    !
    ! -- Create and read ghost node information
    !cdl if(this%ingnc > 0) then
    !cdl   call gnc_cr(this%gnc, this%name, this%ingnc, iout)
    !cdl   call this%read_gnc()
    !cdl endif
    !cdl !
    !cdl ! -- Read mover information
    !cdl if(this%inmvr > 0) then
    !cdl   call this%read_mvr(iout)
    !cdl endif
    !
    ! -- close the file
    close(inunit)
    !
    ! -- Store obs
    call this%gwt_gwt_df_obs()
    call this%obs%obs_df(iout, this%name, 'GWT-GWT', this%gwtmodel1%dis)    
    !
    ! -- validate
    call this%validate_exchange()
    !
    ! -- return
    return
  end subroutine gwt_gwt_df

  !> @brief validate exchange data after reading
  !<
  subroutine validate_exchange(this)
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    ! local
    
    ! Periodic boundary condition in exchange don't allow XT3D (=interface model)
    if (associated(this%model1, this%model2)) then
      if (this%ixt3d > 0) then
        write(errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name),             &
                             ' is a periodic boundary condition which cannot'// &
                             ' be configured with XT3D'
        call store_error(errmsg, terminate=.TRUE.)
      end if
    end if

    !cdl ! Check to see if horizontal anisotropy is in either model1 or model2.
    !cdl ! If so, then ANGLDEGX must be provided as an auxiliary variable for this
    !cdl ! GWT-GWT exchange (this%ianglex > 0).
    !cdl if(this%gwtmodel1%npf%ik22 /= 0 .or. this%gwtmodel2%npf%ik22 /= 0) then
    !cdl   if(this%ianglex == 0) then
    !cdl     write(errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name),             &
    !cdl                          ' requires that ANGLDEGX be specified as an'//     &
    !cdl                          ' auxiliary variable because K22 was specified'//  &
    !cdl                          ' in one or both groundwater models.'
    !cdl     call store_error(errmsg, terminate=.TRUE.)
    !cdl   endif
    !cdl endif
    !cdl 
    !cdl ! Check to see if specific discharge is needed for model1 or model2.
    !cdl ! If so, then ANGLDEGX must be provided as an auxiliary variable for this
    !cdl ! GWT-GWT exchange (this%ianglex > 0).
    !cdl if(this%gwtmodel1%npf%icalcspdis /= 0 .or. &
    !cdl    this%gwtmodel2%npf%icalcspdis /= 0) then
    !cdl   if(this%ianglex == 0) then
    !cdl     write(errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name),             &
    !cdl                          ' requires that ANGLDEGX be specified as an'//     &
    !cdl                          ' auxiliary variable because specific discharge'// &
    !cdl                          ' is being calculated in one or both'//            &
    !cdl                          ' groundwater models.'
    !cdl     call store_error(errmsg, terminate=.TRUE.)
    !cdl   endif
    !cdl   if(this%icdist == 0) then
    !cdl     write(errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name),             &
    !cdl                          ' requires that CDIST be specified as an'//        &
    !cdl                          ' auxiliary variable because specific discharge'// &
    !cdl                          ' is being calculated in one or both'//            &
    !cdl                          ' groundwater models.'
    !cdl     call store_error(errmsg, terminate=.TRUE.)
    !cdl   endif
    !cdl endif

    if (this%ixt3d > 0 .and. this%ianglex == 0) then
      write(errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name),               &
                           ' requires that ANGLDEGX be specified as an'//       &
                           ' auxiliary variable because XT3D is enabled'
      call store_error(errmsg, terminate=.TRUE.)
    end if

  end subroutine validate_exchange

  !> @ brief Add connections
  !!
  !! override parent exg_ac so that gnc can add connections here.
  !!
  !<
  !cdl subroutine gwt_gwt_ac(this, sparse)
  !cdl   ! -- modules
  !cdl   use SparseModule, only:sparsematrix
  !cdl   ! -- dummy
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   type(sparsematrix), intent(inout) :: sparse
  !cdl   ! -- local
  !cdl   integer(I4B) :: n, iglo, jglo
  !cdl   !
  !cdl   ! -- add exchange connections
  !cdl   do n = 1, this%nexg
  !cdl     iglo = this%nodem1(n) + this%gwtmodel1%moffset
  !cdl     jglo = this%nodem2(n) + this%gwtmodel2%moffset
  !cdl     call sparse%addconnection(iglo, jglo, 1)
  !cdl     call sparse%addconnection(jglo, iglo, 1)
  !cdl   enddo
  !cdl   !
  !cdl   ! -- add gnc connections
  !cdl   if(this%ingnc > 0) then
  !cdl     call this%gnc%gnc_ac(sparse)
  !cdl   endif
  !cdl   !
  !cdl   ! -- Return
  !cdl   return
  !cdl end subroutine gwt_gwt_ac
  !cdl 
  !cdl !> @ brief Map connections
  !cdl !!
  !cdl !! Map the connections in the global matrix
  !cdl !!
  !cdl !<
  !cdl subroutine gwt_gwt_mc(this, iasln, jasln)
  !cdl   ! -- modules
  !cdl   use SparseModule, only:sparsematrix
  !cdl   ! -- dummy
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   integer(I4B), dimension(:), intent(in) :: iasln
  !cdl   integer(I4B), dimension(:), intent(in) :: jasln
  !cdl   ! -- local
  !cdl   integer(I4B) :: n, iglo, jglo, ipos
  !cdl   !
  !cdl   ! -- map exchange connections
  !cdl   do n = 1, this%nexg
  !cdl     iglo = this%nodem1(n)+this%gwtmodel1%moffset
  !cdl     jglo = this%nodem2(n)+this%gwtmodel2%moffset
  !cdl     ! -- find jglobal value in row iglo and store in idxglo
  !cdl     do ipos = iasln(iglo), iasln(iglo + 1) - 1
  !cdl       if(jglo == jasln(ipos)) then
  !cdl         this%idxglo(n) = ipos
  !cdl         exit
  !cdl       endif
  !cdl     enddo
  !cdl     ! -- find and store symmetric location
  !cdl     do ipos = iasln(jglo), iasln(jglo + 1) - 1
  !cdl       if(iglo == jasln(ipos)) then
  !cdl         this%idxsymglo(n) = ipos
  !cdl         exit
  !cdl       endif
  !cdl     enddo
  !cdl   enddo
  !cdl   !
  !cdl   ! -- map gnc connections
  !cdl   if(this%ingnc > 0) then
  !cdl     call this%gnc%gnc_mc(iasln, jasln)
  !cdl   endif
  !cdl   !
  !cdl   ! -- Return
  !cdl   return
  !cdl end subroutine gwt_gwt_mc

  !> @ brief Allocate and read
  !!
  !! Allocated and read and calculate saturated conductance
  !!
  !<
  subroutine gwt_gwt_ar(this)
    ! -- modules
    !cdl use ConstantsModule, only: LINELENGTH, DZERO, DHALF, DONE, DPIO180
    !cdl use GwtNpfModule, only: condmean, vcond, hcond
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    !cdl ! -- local
    !cdl integer(I4B) :: iexg
    !cdl integer(I4B) :: n, m, ihc
    !cdl real(DP) :: topn, topm
    !cdl real(DP) :: botn, botm
    !cdl real(DP) :: satn, satm
    !cdl real(DP) :: thickn, thickm
    !cdl real(DP) :: angle, hyn, hym
    !cdl real(DP) :: csat
    !cdl real(DP) :: fawidth
    !cdl real(DP), dimension(3) :: vg
    !
    ! -- If mover is active, then call ar routine
    !cdl if(this%inmvr > 0) call this%mvr%mvr_ar()
    !
    ! -- Go through each connection and calculate the saturated conductance
    !cdl do iexg = 1, this%nexg
    !cdl   !
    !cdl   ihc = this%ihc(iexg)
    !cdl   n = this%nodem1(iexg)
    !cdl   m = this%nodem2(iexg)
    !cdl   topn = this%gwtmodel1%dis%top(n)
    !cdl   topm = this%gwtmodel2%dis%top(m)
    !cdl   botn = this%gwtmodel1%dis%bot(n)
    !cdl   botm = this%gwtmodel2%dis%bot(m)
    !cdl   satn = this%gwtmodel1%npf%sat(n)
    !cdl   satm = this%gwtmodel2%npf%sat(m)
    !cdl   thickn = (topn - botn) * satn
    !cdl   thickm = (topm - botm) * satm
    !cdl   !
    !cdl   ! -- Calculate conductance depending on connection orientation
    !cdl   if(ihc == 0) then
    !cdl     !
    !cdl     ! -- Vertical conductance for fully saturated conditions
    !cdl     vg(1) = DZERO
    !cdl     vg(2) = DZERO
    !cdl     vg(3) = DONE
    !cdl     hyn = this%gwtmodel1%npf%hy_eff(n, 0, ihc, vg=vg)
    !cdl     hym = this%gwtmodel2%npf%hy_eff(m, 0, ihc, vg=vg)
    !cdl     csat = vcond(1, 1, 1, 1, 0, 1, 1, DONE,                                &
    !cdl                   botn, botm,                                              &
    !cdl                   hyn, hym,                                                &
    !cdl                   satn, satm,                                              &
    !cdl                   topn, topm,                                              &
    !cdl                   botn, botm,                                              &
    !cdl                   this%hwva(iexg))
    !cdl   else
    !cdl     !
    !cdl     ! -- Calculate horizontal conductance
    !cdl     hyn = this%gwtmodel1%npf%k11(n)
    !cdl     hym = this%gwtmodel2%npf%k11(m)
    !cdl     !
    !cdl     ! -- Check for anisotropy in models, and recalculate hyn and hym
    !cdl     if(this%ianglex > 0) then
    !cdl       angle = this%auxvar(this%ianglex, iexg) * DPIO180
    !cdl       vg(1) = abs(cos(angle))
    !cdl       vg(2) = abs(sin(angle))
    !cdl       vg(3) = DZERO
    !cdl       !
    !cdl       ! -- anisotropy in model 1
    !cdl       if(this%gwtmodel1%npf%ik22 /= 0) then
    !cdl         hyn = this%gwtmodel1%npf%hy_eff(n, 0, ihc, vg=vg)
    !cdl       endif
    !cdl       !
    !cdl       ! -- anisotropy in model 2
    !cdl       if(this%gwtmodel2%npf%ik22 /= 0) then
    !cdl         hym = this%gwtmodel2%npf%hy_eff(m, 0, ihc, vg=vg)
    !cdl       endif
    !cdl     endif
    !cdl     !
    !cdl     fawidth = this%hwva(iexg)
    !cdl     csat = hcond(1, 1, 1, 1, this%inewton, 0, ihc,                        &
    !cdl                   this%icellavg, 0, 0, DONE,                              &
    !cdl                   topn, topm, satn, satm, hyn, hym,                       &
    !cdl                   topn, topm,                                             &
    !cdl                   botn, botm,                                             &
    !cdl                   this%cl1(iexg), this%cl2(iexg),                         &
    !cdl                   fawidth, this%satomega)
    !cdl   endif
    !cdl   !
    !cdl   ! -- store csat in condsat
    !cdl   this%condsat(iexg) = csat
    !cdl enddo
    !
    ! -- Observation AR
    call this%obs%obs_ar()
    !
    ! -- Return
    return
  end subroutine gwt_gwt_ar  


  !> @ brief Read and prepare
  !!
  !! Read new data for mover and obs
  !!
  !<
  subroutine gwt_gwt_rp(this)
    ! -- modules
    use TdisModule, only: readnewdata
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    !
    ! -- Check with TDIS on whether or not it is time to RP
    if (.not. readnewdata) return
    !
    ! -- Read and prepare for mover
    !cdl if(this%inmvr > 0) call this%mvr%mvr_rp()
    !
    ! -- Read and prepare for observations
    call this%gwt_gwt_rp_obs()
    !
    ! -- Return
    return
  end subroutine gwt_gwt_rp

  !> @ brief Advance
  !!
  !! Advance mover and obs
  !!
  !<
  subroutine gwt_gwt_ad(this)
    ! -- modules
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    ! -- local
    !
    ! -- Advance mover
    !cdl if(this%inmvr > 0) call this%mvr%mvr_ad()
    !
    ! -- Push simulated values to preceding time step
    call this%obs%obs_ad()
    !
    ! -- Return
    return
  end subroutine gwt_gwt_ad

  !> @ brief Calculate coefficients
  !!
  !! Rewet as necessary
  !!
  !<
  !cdl subroutine gwt_gwt_cf(this, kiter)
  !cdl   ! -- dummy
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   integer(I4B), intent(in) :: kiter
  !cdl   ! -- local
  !cdl   !
  !cdl   ! -- Rewet cells across models using the wetdry parameters in each model's
  !cdl   !    npf package, and the head in the connected model.
  !cdl   call this%rewet(kiter)
  !cdl   !
  !cdl   ! -- Return
  !cdl   return
  !cdl end subroutine gwt_gwt_cf
  
  !> @ brief Fill coefficients
  !!
  !! Calculate conductance and fill coefficient matrix
  !!
  !<
  !cdl subroutine gwt_gwt_fc(this, kiter, iasln, amatsln, rhssln, inwtflag)
  !cdl   ! -- modules
  !cdl   use ConstantsModule, only: DHALF
  !cdl   use GwtNpfModule, only: hcond, vcond
  !cdl   ! -- dummy
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   integer(I4B), intent(in) :: kiter
  !cdl   integer(I4B), dimension(:), intent(in) :: iasln
  !cdl   real(DP), dimension(:), intent(inout) :: amatsln
  !cdl   real(DP), dimension(:), intent(inout) ::rhssln
  !cdl   integer(I4B), optional, intent(in) :: inwtflag
  !cdl   ! -- local
  !cdl   integer(I4B) :: inwt, iexg
  !cdl   integer(I4B) :: i, nodem1sln, nodem2sln, idiagsln
  !cdl   integer(I4B) :: njasln
  !cdl   !
  !cdl   ! -- calculate the conductance for each exchange connection
  !cdl   call this%condcalc()
  !cdl   !
  !cdl   ! -- if gnc is active, then copy cond into gnc cond (might consider a
  !cdl   !    pointer here in the future)
  !cdl   if(this%ingnc > 0) then
  !cdl     do iexg = 1, this%nexg
  !cdl       this%gnc%cond(iexg) = this%cond(iexg)
  !cdl     enddo
  !cdl   endif
  !cdl   !
  !cdl   ! -- Put this%cond into amatsln
  !cdl   do i = 1, this%nexg
  !cdl     amatsln(this%idxglo(i)) = this%cond(i)
  !cdl     amatsln(this%idxsymglo(i)) = this%cond(i)
  !cdl     nodem1sln = this%nodem1(i) + this%gwtmodel1%moffset
  !cdl     nodem2sln = this%nodem2(i) + this%gwtmodel2%moffset
  !cdl     idiagsln = iasln(nodem1sln)
  !cdl     amatsln(idiagsln) = amatsln(idiagsln) - this%cond(i)
  !cdl     idiagsln = iasln(nodem2sln)
  !cdl     amatsln(idiagsln) = amatsln(idiagsln) - this%cond(i)
  !cdl   enddo
  !cdl   !
  !cdl   ! -- Fill the gnc terms in the solution matrix
  !cdl   if(this%ingnc > 0) then
  !cdl     call this%gnc%gnc_fc(kiter, amatsln)
  !cdl   endif
  !cdl   !
  !cdl   ! -- Call mvr fc routine
  !cdl   if(this%inmvr > 0) call this%mvr%mvr_fc()
  !cdl   !
  !cdl   ! -- Set inwt to exchange newton, but shut off if requested by caller
  !cdl   inwt = this%inewton
  !cdl   if(present(inwtflag)) then
  !cdl     if (inwtflag == 0) inwt = 0
  !cdl   endif
  !cdl   if (inwt /= 0) then
  !cdl     call this%exg_fn(kiter, iasln, amatsln)
  !cdl   endif
  !cdl   !
  !cdl   ! -- Ghost node Newton-Raphson
  !cdl   if (this%ingnc > 0) then
  !cdl     if (inwt /= 0) then
  !cdl       njasln = size(amatsln)
  !cdl       call this%gnc%gnc_fn(kiter, njasln, amatsln, this%condsat,             &
  !cdl         ihc_opt=this%ihc, ivarcv_opt=this%ivarcv,                            &
  !cdl         ictm1_opt=this%gwtmodel1%npf%icelltype,                              &
  !cdl         ictm2_opt=this%gwtmodel2%npf%icelltype)
  !cdl     endif
  !cdl   endif
  !cdl   !
  !cdl   ! -- Return
  !cdl   return
  !cdl end subroutine gwt_gwt_fc

  !> @ brief Fill Newton
  !!
  !! Fill amatsln with Newton terms
  !!
  !<
  !cdl subroutine gwt_gwt_fn(this, kiter, iasln, amatsln)
  !cdl   ! -- modules
  !cdl   use SmoothingModule, only: sQuadraticSaturationDerivative
  !cdl   ! -- dummy
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   integer(I4B), intent(in) :: kiter
  !cdl   integer(I4B), dimension(:), intent(in) :: iasln
  !cdl   real(DP), dimension(:), intent(inout) :: amatsln
  !cdl   ! -- local
  !cdl   logical :: nisup
  !cdl   integer(I4B) :: iexg
  !cdl   integer(I4B) :: n, m
  !cdl   integer(I4B) :: nodensln, nodemsln
  !cdl   integer(I4B) :: ibdn, ibdm
  !cdl   integer(I4B) :: idiagnsln, idiagmsln
  !cdl   real(DP) :: topn, topm
  !cdl   real(DP) :: botn, botm
  !cdl   real(DP) :: topup, botup
  !cdl   real(DP) :: hn, hm
  !cdl   real(DP) :: hup, hdn
  !cdl   real(DP) :: cond
  !cdl   real(DP) :: term
  !cdl   real(DP) :: consterm
  !cdl   real(DP) :: derv
  !cdl   !
  !cdl   do iexg = 1, this%nexg
  !cdl     n = this%nodem1(iexg)
  !cdl     m = this%nodem2(iexg)
  !cdl     nodensln = this%nodem1(iexg) + this%gwtmodel1%moffset
  !cdl     nodemsln = this%nodem2(iexg) + this%gwtmodel2%moffset
  !cdl     ibdn = this%gwtmodel1%ibound(n)
  !cdl     ibdm = this%gwtmodel2%ibound(m)
  !cdl     topn = this%gwtmodel1%dis%top(n)
  !cdl     topm = this%gwtmodel2%dis%top(m)
  !cdl     botn = this%gwtmodel1%dis%bot(n)
  !cdl     botm = this%gwtmodel2%dis%bot(m)
  !cdl     hn = this%gwtmodel1%x(n)
  !cdl     hm = this%gwtmodel2%x(m)
  !cdl     if(this%ihc(iexg) == 0) then
  !cdl       ! -- vertical connection, newton not supported
  !cdl     else
  !cdl       ! -- determine upstream node
  !cdl       nisup = .false.
  !cdl       if(hm < hn) nisup = .true.
  !cdl       !
  !cdl       ! -- set upstream top and bot
  !cdl       if(nisup) then
  !cdl         topup = topn
  !cdl         botup = botn
  !cdl         hup = hn
  !cdl         hdn = hm
  !cdl       else
  !cdl         topup = topm
  !cdl         botup = botm
  !cdl         hup = hm
  !cdl         hdn = hn
  !cdl       endif
  !cdl       !
  !cdl       ! -- no newton terms if upstream cell is confined
  !cdl       if (nisup) then
  !cdl         if (this%gwtmodel1%npf%icelltype(n) == 0) cycle
  !cdl       else
  !cdl         if (this%gwtmodel2%npf%icelltype(m) == 0) cycle
  !cdl       end if
  !cdl       !
  !cdl       ! -- set topup and botup
  !cdl       if(this%ihc(iexg) == 2) then
  !cdl         topup = min(topn, topm)
  !cdl         botup = max(botn, botm)
  !cdl       endif
  !cdl       !
  !cdl       ! get saturated conductivity for derivative
  !cdl       cond = this%condsat(iexg)
  !cdl       !
  !cdl       ! -- TO DO deal with MODFLOW-NWT upstream weighting option
  !cdl       !
  !cdl       ! -- compute terms
  !cdl       consterm = -cond * (hup - hdn)
  !cdl       derv = sQuadraticSaturationDerivative(topup, botup, hup)
  !cdl       idiagnsln = iasln(nodensln)
  !cdl       idiagmsln = iasln(nodemsln)
  !cdl       if(nisup) then
  !cdl         !
  !cdl         ! -- fill jacobian with n being upstream
  !cdl         term = consterm * derv
  !cdl         this%gwtmodel1%rhs(n) = this%gwtmodel1%rhs(n) + term * hn
  !cdl         this%gwtmodel2%rhs(m) = this%gwtmodel2%rhs(m) - term * hn
  !cdl         amatsln(idiagnsln) = amatsln(idiagnsln) + term
  !cdl         if(ibdm > 0) then
  !cdl           amatsln(this%idxsymglo(iexg)) = amatsln(this%idxsymglo(iexg)) - term
  !cdl         endif
  !cdl       else
  !cdl         !
  !cdl         ! -- fill jacobian with m being upstream
  !cdl         term = -consterm * derv
  !cdl         this%gwtmodel1%rhs(n) = this%gwtmodel1%rhs(n) + term * hm
  !cdl         this%gwtmodel2%rhs(m) = this%gwtmodel2%rhs(m) - term * hm
  !cdl         amatsln(idiagmsln) = amatsln(idiagmsln) - term
  !cdl         if(ibdn > 0) then
  !cdl           amatsln(this%idxglo(iexg)) = amatsln(this%idxglo(iexg)) + term
  !cdl         endif
  !cdl       endif
  !cdl     endif
  !cdl   enddo
  !cdl   !
  !cdl   ! -- Return
  !cdl   return
  !cdl end subroutine gwt_gwt_fn

  !> @ brief Calculate flow
  !!
  !! Calculate flow between two cells and store in simvals, also set
  !! information needed for specific discharge calculation
  !!
  !<
  !cdl subroutine gwt_gwt_cq(this, icnvg, isuppress_output, isolnid)
  !cdl   ! -- modules
  !cdl   ! -- dummy
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   integer(I4B), intent(inout) :: icnvg
  !cdl   integer(I4B), intent(in) :: isuppress_output
  !cdl   integer(I4B), intent(in) :: isolnid
  !cdl   ! -- local
  !cdl   !
  !cdl   ! -- calculate flow and store in simvals
  !cdl   call this%gwt_gwt_calc_simvals()    
  !cdl   !
  !cdl   ! -- calculate specific discharge and set to model
  !cdl   call this%gwt_gwt_set_spdis()
  !cdl   !
  !cdl   ! -- return
  !cdl   return
  !cdl end subroutine gwt_gwt_cq


  !> @brief Calculate flow rates for the exchanges and
  !< store them in a member array
  !cdl subroutine gwt_gwt_calc_simvals(this)
  !cdl   use ConstantsModule, only: DZERO
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   ! local
  !cdl   integer(I4B) :: i
  !cdl   integer(I4B) :: n1, n2
  !cdl   integer(I4B) :: ibdn1, ibdn2
  !cdl   real(DP) :: rrate
  !cdl 
  !cdl   do i = 1, this%nexg
  !cdl     rrate = DZERO
  !cdl     n1 = this%nodem1(i)
  !cdl     n2 = this%nodem2(i)
  !cdl     ibdn1 = this%gwtmodel1%ibound(n1)
  !cdl     ibdn2 = this%gwtmodel2%ibound(n2)
  !cdl     if(ibdn1 /= 0 .and. ibdn2 /= 0) then
  !cdl       rrate = this%qcalc(i, n1, n2)
  !cdl       if(this%ingnc > 0) then
  !cdl         rrate = rrate + this%gnc%deltaqgnc(i)
  !cdl       endif
  !cdl     endif
  !cdl     this%simvals(i) = rrate
  !cdl   end do
  !cdl   
  !cdl   return
  !cdl end subroutine gwt_gwt_calc_simvals

  !> @brief Calculate specific discharge from flow rates
  !< and set them to the models
  !cdl subroutine gwt_gwt_set_spdis(this)
  !cdl   use ConstantsModule, only: DZERO, DPIO180
  !cdl   use GwtNpfModule, only: thksatnm
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   ! local
  !cdl   integer(I4B) :: iusg
  !cdl   integer(I4B) :: i
  !cdl   integer(I4B) :: n1, n2
  !cdl   integer(I4B) :: ibdn1, ibdn2
  !cdl   integer(I4B) :: ictn1, ictn2
  !cdl   integer(I4B) :: ihc
  !cdl   real(DP) :: rrate
  !cdl   real(DP) :: topn1, topn2
  !cdl   real(DP) :: botn1, botn2
  !cdl   real(DP) :: satn1, satn2
  !cdl   real(DP) :: hn1, hn2
  !cdl   real(DP) :: nx, ny
  !cdl   real(DP) :: distance
  !cdl   real(DP) :: dltot
  !cdl   real(DP) :: hwva
  !cdl   real(DP) :: area
  !cdl   real(DP) :: thksat
  !cdl   real(DP) :: angle
  !cdl 
  !cdl   ! -- Return if there neither model needs to calculate specific discharge
  !cdl   if (this%gwtmodel1%npf%icalcspdis == 0 .and. &
  !cdl       this%gwtmodel2%npf%icalcspdis == 0) return
  !cdl   !
  !cdl   ! -- initialize
  !cdl   iusg = 0
  !cdl   !
  !cdl   ! -- Loop through all exchanges using the flow rate 
  !cdl   !    stored in simvals 
  !cdl   do i = 1, this%nexg
  !cdl     rrate = this%simvals(i)
  !cdl     n1 = this%nodem1(i)
  !cdl     n2 = this%nodem2(i)
  !cdl     ihc = this%ihc(i)
  !cdl     hwva = this%hwva(i)
  !cdl     ibdn1 = this%gwtmodel1%ibound(n1)
  !cdl     ibdn2 = this%gwtmodel2%ibound(n2)
  !cdl     ictn1 = this%gwtmodel1%npf%icelltype(n1)
  !cdl     ictn2 = this%gwtmodel2%npf%icelltype(n2)
  !cdl     topn1 = this%gwtmodel1%dis%top(n1)
  !cdl     topn2 = this%gwtmodel2%dis%top(n2)
  !cdl     botn1 = this%gwtmodel1%dis%bot(n1)
  !cdl     botn2 = this%gwtmodel2%dis%bot(n2)
  !cdl     satn1 = this%gwtmodel1%npf%sat(n1)
  !cdl     satn2 = this%gwtmodel2%npf%sat(n2)
  !cdl     hn1 = this%gwtmodel1%x(n1)
  !cdl     hn2 = this%gwtmodel2%x(n2)
  !cdl     !
  !cdl     ! -- Calculate face normal components
  !cdl     if(ihc == 0) then
  !cdl       nx = DZERO
  !cdl       ny = DZERO
  !cdl       area = hwva
  !cdl       if (botn1 < botn2) then
  !cdl         ! -- n1 is beneath n2, so rate is positive downward.  Flip rate
  !cdl         !    upward so that points in positive z direction
  !cdl         rrate = - rrate
  !cdl       endif
  !cdl     else
  !cdl       if(this%ianglex > 0) then
  !cdl         angle = this%auxvar(this%ianglex, i) * DPIO180
  !cdl         nx = cos(angle)
  !cdl         ny = sin(angle)
  !cdl       else
  !cdl         ! error?
  !cdl         call store_error('error in gwt_gwt_cq', terminate=.TRUE.)
  !cdl       endif
  !cdl       !
  !cdl       ! -- Calculate the saturated thickness at interface between n1 and n2
  !cdl       thksat = thksatnm(ibdn1, ibdn2, ictn1, ictn2, this%inewton, ihc,       & 
  !cdl                         iusg, hn1, hn2, satn1, satn2,                        &
  !cdl                         topn1, topn2, botn1, botn2, this%satomega)
  !cdl       area = hwva * thksat
  !cdl     endif
  !cdl     !
  !cdl     ! -- Submit this connection and flow information to the npf
  !cdl     !    package of gwtmodel1
  !cdl     if(this%icdist > 0) then
  !cdl       dltot = this%auxvar(this%icdist, i)
  !cdl     else
  !cdl       call store_error('error in gwt_gwt_cq', terminate=.TRUE.)
  !cdl     endif
  !cdl     distance = dltot * this%cl1(i) / (this%cl1(i) + this%cl2(i))
  !cdl     if (this%gwtmodel1%npf%icalcspdis == 1) then
  !cdl       call this%gwtmodel1%npf%set_edge_properties(n1, ihc, rrate, area,      &
  !cdl                                                   nx, ny, distance)
  !cdl       this%gwtmodel1%flowja(this%gwtmodel1%ia(n1)) =                         &
  !cdl         this%gwtmodel1%flowja(this%gwtmodel1%ia(n1)) + rrate
  !cdl     endif
  !cdl     !
  !cdl     ! -- Submit this connection and flow information to the npf
  !cdl     !    package of gwtmodel2
  !cdl     if(this%icdist > 0) then
  !cdl       dltot = this%auxvar(this%icdist, i)
  !cdl     else
  !cdl       call store_error('error in gwt_gwt_cq', terminate=.TRUE.)
  !cdl     endif
  !cdl     if (this%gwtmodel2%npf%icalcspdis == 1) then
  !cdl       distance = dltot * this%cl2(i) / (this%cl1(i) + this%cl2(i))
  !cdl       if (ihc /= 0) rrate = -rrate
  !cdl       call this%gwtmodel2%npf%set_edge_properties(n2, ihc, rrate, area,      &
  !cdl                                                   -nx, -ny, distance)
  !cdl       this%gwtmodel2%flowja(this%gwtmodel2%ia(n2)) =                         &
  !cdl         this%gwtmodel2%flowja(this%gwtmodel2%ia(n2)) + rrate
  !cdl     endif
  !cdl     !
  !cdl   enddo
  !cdl   !
  !cdl   return
  !cdl end subroutine gwt_gwt_set_spdis
  
  
  !> @ brief Budget
  !!
  !! Accumulate budget terms
  !!
  !<
  subroutine gwt_gwt_bd(this, icnvg, isuppress_output, isolnid)
    ! -- modules
    use ConstantsModule, only: DZERO, LENBUDTXT, LENPACKAGENAME
    use BudgetModule, only: rate_accumulator
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    integer(I4B), intent(inout) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B), intent(in) :: isolnid
    ! -- local
    character(len=LENBUDTXT), dimension(1) :: budtxt
    real(DP), dimension(2, 1) :: budterm
    real(DP) :: ratin, ratout
    ! -- formats
    !
    ! -- initialize
    budtxt(1) = '    FLOW-JA-FACE'
    !
    ! -- Calculate ratin/ratout and pass to model budgets
    call rate_accumulator(this%simvals, ratin, ratout)
    !
    ! -- Add the budget terms to model 1
    budterm(1, 1) = ratin
    budterm(2, 1) = ratout
    call this%gwtmodel1%model_bdentry(budterm, budtxt, this%name)
    !
    ! -- Add the budget terms to model 2
    budterm(1, 1) = ratout
    budterm(2, 1) = ratin
    call this%gwtmodel2%model_bdentry(budterm, budtxt, this%name)
    !
    ! -- Call mvr bd routine
    !cdl if(this%inmvr > 0) call this%mvr%mvr_bd()
    !
    ! -- return
    return
  end subroutine gwt_gwt_bd
  
  !> @ brief Budget save
  !!
  !! Output individual flows to listing file and binary budget files
  !!
  !<
  subroutine gwt_gwt_bdsav(this)
    ! -- modules
    use ConstantsModule, only: DZERO, LENBUDTXT, LENPACKAGENAME
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    ! -- local
    character(len=LENBOUNDNAME) :: bname
    character(len=LENPACKAGENAME+4) :: packname1
    character(len=LENPACKAGENAME+4) :: packname2
    character(len=LENBUDTXT), dimension(1) :: budtxt
    character(len=20) :: nodestr
    integer(I4B) :: ntabrows
    integer(I4B) :: nodeu
    integer(I4B) :: i, n1, n2, n1u, n2u
    integer(I4B) :: ibinun1, ibinun2
    integer(I4B) :: icbcfl, ibudfl
    real(DP) :: ratin, ratout, rrate
    integer(I4B) :: isuppress_output
    ! -- formats
    !
    ! -- initialize local variables
    isuppress_output = 0
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
      if (this%gwtmodel1%oc%oc_save('BUDGET')) then
        call this%outputtab1%set_title(packname1)
      end if
      if (this%gwtmodel2%oc%oc_save('BUDGET')) then 
        call this%outputtab2%set_title(packname2)
      end if
      !
      ! -- set table kstp and kper
      call this%outputtab1%set_kstpkper(kstp, kper)
      call this%outputtab2%set_kstpkper(kstp, kper)
      !
      ! -- update maxbound of tables
      ntabrows = 0
      do i = 1, this%nexg
        n1 = this%nodem1(i)
        n2 = this%nodem2(i)
        !
        ! -- If both cells are active then calculate flow rate
        if (this%gwtmodel1%ibound(n1) /= 0 .and.                                  &
            this%gwtmodel2%ibound(n2) /= 0) then
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
      ibinun1 = this%gwtmodel1%oc%oc_save_unit('BUDGET')
    else
      ibinun1 = 0
    endif
    !
    ! -- If save budget flag is zero for this stress period, then
    !    shut off saving
    if(.not. this%gwtmodel1%oc%oc_save('BUDGET')) ibinun1 = 0
    if(isuppress_output /= 0) then
      ibinun1 = 0
    endif
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if(ibinun1 /= 0) then
      call this%gwtmodel1%dis%record_srcdst_list_header(budtxt(1),             &
                                       this%gwtmodel1%name, this%name,                &
                                       this%gwtmodel2%name, this%name,                &
                                       this%naux, this%auxname,                &
                                       ibinun1, this%nexg, this%gwtmodel1%iout)
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
      if(this%gwtmodel1%ibound(n1) /= 0 .and. &
          this%gwtmodel2%ibound(n2) /= 0) then
        rrate = this%simvals(i)
        !
        ! -- Print the individual rates to model list files if requested
        if(this%iprflow /= 0) then
          if(this%gwtmodel1%oc%oc_save('BUDGET')) then
            !
            ! -- set nodestr and write outputtab table
            nodeu = this%gwtmodel1%dis%get_nodeuser(n1)
            call this%gwtmodel1%dis%nodeu_to_string(nodeu, nodestr)
            call this%outputtab1%print_list_entry(i, trim(adjustl(nodestr)),     &
                                                  rrate, bname)
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
      n1u = this%gwtmodel1%dis%get_nodeuser(n1)
      n2u = this%gwtmodel2%dis%get_nodeuser(n2)
      if(ibinun1 /= 0)                                                         &
        call this%gwtmodel1%dis%record_mf6_list_entry(                         &
          ibinun1, n1u, n2u, rrate, this%naux, this%auxvar(:, i),              &
          .false., .false.)
      !
    enddo
    !
    ! -- Print and write budget terms for model 2
    !
    ! -- Set binary unit numbers for saving flows
    if(this%ipakcb /= 0) then
      ibinun2 = this%gwtmodel2%oc%oc_save_unit('BUDGET')
    else
      ibinun2 = 0
    endif
    !
    ! -- If save budget flag is zero for this stress period, then
    !    shut off saving
    if(.not. this%gwtmodel2%oc%oc_save('BUDGET')) ibinun2 = 0
    if(isuppress_output /= 0) then
      ibinun2 = 0
    endif
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if(ibinun2 /= 0) then
      call this%gwtmodel2%dis%record_srcdst_list_header(budtxt(1),             &
                                       this%gwtmodel2%name, this%name,                &
                                       this%gwtmodel1%name, this%name,                &
                                       this%naux, this%auxname,                &
                                       ibinun2, this%nexg, this%gwtmodel2%iout)
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
      if(this%gwtmodel1%ibound(n1) /= 0 .and. &
          this%gwtmodel2%ibound(n2) /= 0) then
        rrate = this%simvals(i)
        !
        ! -- Print the individual rates to model list files if requested
        if(this%iprflow /= 0) then
          if(this%gwtmodel2%oc%oc_save('BUDGET')) then
            !
            ! -- set nodestr and write outputtab table
            nodeu = this%gwtmodel2%dis%get_nodeuser(n2)
            call this%gwtmodel2%dis%nodeu_to_string(nodeu, nodestr)
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
      n1u = this%gwtmodel1%dis%get_nodeuser(n1)
      n2u = this%gwtmodel2%dis%get_nodeuser(n2)
      if(ibinun2 /= 0)                                                         &
        call this%gwtmodel2%dis%record_mf6_list_entry(                         &
          ibinun2, n2u, n1u, -rrate, this%naux, this%auxvar(:, i),             &
          .false., .false.)
      !
    enddo
    !
    ! -- Set icbcfl, ibudfl to zero so that flows will be printed and
    !    saved, if the options were set in the MVR package
    icbcfl = 1
    ibudfl = 1
    !
    ! -- Call mvr bd routine
    !cdl if(this%inmvr > 0) call this%mvr%mvr_bdsav(icbcfl, ibudfl, isuppress_output)
    !
    ! -- Calculate and write simulated values for observations
    if(this%inobs /= 0) then
      call this%gwt_gwt_save_simvals()
    endif
    !
    ! -- return
    return
  end subroutine gwt_gwt_bdsav
  
  !> @ brief Output
  !!
  !! Write output
  !!
  !<
  subroutine gwt_gwt_ot(this)
    ! -- modules
    use SimVariablesModule, only: iout
    use ConstantsModule, only: DZERO, LINELENGTH
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    ! -- local
    integer(I4B) :: iexg, n1, n2
    !cdl integer(I4B) :: ibudfl
    real(DP) :: flow  !cdl , deltaqgnc
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
    !
    ! -- Call bdsave
    call this%gwt_gwt_bdsav()
    !
    ! -- Initialize
    !cdl deltaqgnc = DZERO
    !
    ! -- Write a table of exchanges
    if(this%iprflow /= 0) then
      !cdl if(this%ingnc > 0) then
      !cdl   write(iout, fmtheader) trim(adjustl(this%name)), this%id, 'NODEM1',    &
      !cdl                        'NODEM2', 'COND', 'X_M1', 'X_M2', 'DELTAQGNC',    &
      !cdl                        'FLOW'
      !cdl else
        write(iout, fmtheader2) trim(adjustl(this%name)), this%id, 'NODEM1',   &
                             'NODEM2', 'COND', 'X_M1', 'X_M2', 'FLOW'
      !cdl endif
      do iexg = 1, this%nexg
        n1 = this%nodem1(iexg)
        n2 = this%nodem2(iexg)
        flow = this%simvals(iexg)
        call this%gwtmodel1%dis%noder_to_string(n1, node1str)
        call this%gwtmodel2%dis%noder_to_string(n2, node2str)
        !cdl if(this%ingnc > 0) then
        !cdl   deltaqgnc = this%gnc%deltaqgnc(iexg)
        !cdl   write(iout, fmtdata) trim(adjustl(node1str)),                        &
        !cdl                        trim(adjustl(node2str)),                        &
        !cdl                        this%cond(iexg), this%gwtmodel1%x(n1),          &
        !cdl                        this%gwtmodel2%x(n2), deltaqgnc, flow
        !cdl else
          write(iout, fmtdata) trim(adjustl(node1str)),                        &
                               trim(adjustl(node2str)),                        &
                               this%cond(iexg), this%gwtmodel1%x(n1),          &
                               this%gwtmodel2%x(n2), flow
        !cdl endif
      enddo
    endif
    !
    ! -- Mover budget output
    !cdl ibudfl = 1
    !cdl if(this%inmvr > 0) call this%mvr%mvr_ot_bdsummary(ibudfl)
    !
    ! -- OBS output
    call this%obs%obs_ot()
    !
    ! -- return
    return
  end subroutine gwt_gwt_ot

  !> @ brief Read options
  !!
  !! Read the options block
  !!
  !<
  subroutine read_options(this, iout)
    ! -- modules
    use ConstantsModule, only: LINELENGTH, LENAUXNAME, DEM6
    use MemoryManagerModule, only: mem_allocate    
    use SimModule, only: store_error, store_error_unit
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LINELENGTH) :: keyword
    logical :: isfound
    logical :: endOfBlock    
    integer(I4B) :: ierr
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr,                        &
      supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(iout,'(1x,a)')'PROCESSING GWT-GWT EXCHANGE OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) then
          exit
        end if
        call this%parser%GetStringCaps(keyword)

        ! first parse option in base
        if (this%DisConnExchangeType%parse_option(keyword, iout)) then
          cycle
        end if

        ! it's probably ours
        if (this%parse_option(keyword, iout)) then
          cycle
        end if

        ! unknown option
        errmsg = "Unknown GWT-GWT exchange option '" // trim(keyword) // "'."
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end do

      write(iout,'(1x,a)') 'END OF GWT-GWT EXCHANGE OPTIONS'
    end if
    !
    ! -- set omega value used for saturation calculations
    !cdl if (this%inewton > 0) then
    !cdl   this%satomega = DEM6
    !cdl end if
    !
    ! -- return
    return
  end subroutine read_options

  !> @brief parse option from exchange file
  !<
  function parse_option(this, keyword, iout) result(parsed)
    use InputOutputModule, only: getunit, openfile
    class(GwtExchangeType) :: this                   !<  GwtExchangeType
    character(len=LINELENGTH), intent(in) :: keyword !< the option name
    integer(I4B), intent(in) :: iout                 !< for logging    
    logical(LGP) :: parsed                           !< true when parsed
    ! local    
    !cdl character(len=LINELENGTH) :: fname
    integer(I4B) :: inobs
    character(len=LINELENGTH) :: subkey

    parsed = .true.

    select case (keyword)
    case ('PRINT_FLOWS')
      this%iprflow = 1
      write(iout,'(4x,a)') &
        'EXCHANGE FLOWS WILL BE PRINTED TO LIST FILES.'
    case ('SAVE_FLOWS')
      this%ipakcb = -1
      write(iout,'(4x,a)') &
        'EXCHANGE FLOWS WILL BE SAVED TO BINARY BUDGET FILES.'
    !cdl case ('ALTERNATIVE_CELL_AVERAGING')
    !cdl   call this%parser%GetStringCaps(subkey)
    !cdl   select case(subkey)
    !cdl   case('LOGARITHMIC')
    !cdl     this%icellavg = 1
    !cdl   case('AMT-LMK')
    !cdl     this%icellavg = 2
    !cdl   case default
    !cdl     errmsg = "Unknown cell averaging method '" // trim(subkey) // "'."
    !cdl     call store_error(errmsg)
    !cdl     call this%parser%StoreErrorUnit()
    !cdl   end select
    !cdl   write(iout,'(4x,a,a)')                                             &
    !cdl     'CELL AVERAGING METHOD HAS BEEN SET TO: ', trim(subkey)
    !cdl case ('VARIABLECV')
    !cdl   this%ivarcv = 1
    !cdl   write(iout,'(4x,a)')                                               &
    !cdl     'VERTICAL CONDUCTANCE VARIES WITH WATER TABLE.'
    !cdl   call this%parser%GetStringCaps(subkey)
    !cdl   if(subkey == 'DEWATERED') then
    !cdl     this%idewatcv = 1
    !cdl     write(iout,'(4x,a)')                                             &
    !cdl       'VERTICAL CONDUCTANCE ACCOUNTS FOR DEWATERED PORTION OF   ' // &
    !cdl       'AN UNDERLYING CELL.'
    !cdl   endif
    !cdl case ('NEWTON')
    !cdl   this%inewton = 1
    !cdl   write(iout, '(4x,a)')                                              &
    !cdl                    'NEWTON-RAPHSON method used for unconfined cells'          
    !cdl case ('GNC6')
    !cdl   call this%parser%GetStringCaps(subkey)
    !cdl   if(subkey /= 'FILEIN') then
    !cdl     call store_error('GNC6 KEYWORD MUST BE FOLLOWED BY ' //          &
    !cdl       '"FILEIN" then by filename.')
    !cdl     call this%parser%StoreErrorUnit()
    !cdl   endif
    !cdl   call this%parser%GetString(fname)
    !cdl   if(fname == '') then
    !cdl     call store_error('NO GNC6 FILE SPECIFIED.')
    !cdl     call this%parser%StoreErrorUnit()
    !cdl   endif
    !cdl   this%ingnc = getunit()
    !cdl   call openfile(this%ingnc, iout, fname, 'GNC')
    !cdl   write(iout,'(4x,a)')                                               &
    !cdl     'GHOST NODES WILL BE READ FROM ', trim(fname)
    !cdl case ('MVR6')
    !cdl   call this%parser%GetStringCaps(subkey)
    !cdl   if(subkey /= 'FILEIN') then
    !cdl     call store_error('MVR6 KEYWORD MUST BE FOLLOWED BY ' //          &
    !cdl       '"FILEIN" then by filename.')
    !cdl     call this%parser%StoreErrorUnit()
    !cdl   endif
    !cdl   call this%parser%GetString(fname)
    !cdl   if(fname == '') then
    !cdl     call store_error('NO MVR6 FILE SPECIFIED.')
    !cdl     call this%parser%StoreErrorUnit()
    !cdl   endif
    !cdl   this%inmvr = getunit()
    !cdl   call openfile(this%inmvr, iout, fname, 'MVR')
    !cdl   write(iout,'(4x,a)')                                               &
    !cdl     'WATER MOVER INFORMATION WILL BE READ FROM ', trim(fname)
    case ('OBS6')
      call this%parser%GetStringCaps(subkey)
      if(subkey /= 'FILEIN') then
        call store_error('OBS8 KEYWORD MUST BE FOLLOWED BY ' //         &
          '"FILEIN" then by filename.')
        call this%parser%StoreErrorUnit()
      endif
      this%obs%active = .true.
      call this%parser%GetString(this%obs%inputFilename)
      inobs = GetUnit()
      call openfile(inobs, iout, this%obs%inputFilename, 'OBS')
      this%obs%inUnitObs = inobs
    case ('ADVSCHEME')
      call this%parser%GetStringCaps(subkey)
      select case(subkey)
      case('UPSTREAM')
        this%iAdvScheme = 0
      case('CENTRAL')
        this%iAdvScheme = 1
      case('TVD')
        this%iAdvScheme = 2
      case default
        errmsg = "Unknown weighting method for advection: '" // trim(subkey) // "'."
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end select

      write(iout,'(4x,a,a)')                                                      &
        'CELL AVERAGING METHOD HAS BEEN SET TO: ', trim(subkey)

    case ('XT3D_OFF')
      this%ixt3d = 0
      write(iout, '(4x,a)') 'XT3D FORMULATION HAS BEEN SHUT OFF.'
    case ('XT3D_RHS')
      this%ixt3d = 2
      write(iout, '(4x,a)') 'XT3D RIGHT-HAND SIDE FORMULATION IS SELECTED.'
    case default
      parsed = .false.
    end select

  end function parse_option
  
  !> @ brief Read ghost nodes
  !!
  !! Read and process ghost nodes 
  !!
  !<
  !cdl subroutine read_gnc(this)
  !cdl   ! -- modules
  !cdl   use SimModule, only: store_error, store_error_unit, count_errors
  !cdl   use ConstantsModule, only: LINELENGTH
  !cdl   ! -- dummy
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   ! -- local
  !cdl   integer(I4B) :: i, nm1, nm2, nmgnc1, nmgnc2
  !cdl   character(len=*), parameter :: fmterr = &
  !cdl     "('EXCHANGE NODES ', i0, ' AND ', i0,"  // &
  !cdl     "' NOT CONSISTENT WITH GNC NODES ', i0, ' AND ', i0)"
  !cdl   !
  !cdl   ! -- If exchange has ghost nodes, then initialize ghost node object
  !cdl   !    This will read the ghost node blocks from the gnc input file.
  !cdl   call this%gnc%gnc_df(this%gwtmodel1, m2=this%gwtmodel2)
  !cdl   !
  !cdl   ! -- Verify gnc is implicit if exchange has Newton Terms
  !cdl   if(.not. this%gnc%implicit .and. this%inewton /= 0) then
  !cdl     call store_error('GNC IS EXPLICIT, BUT GWT EXCHANGE HAS ACTIVE NEWTON.')
  !cdl     call store_error('ADD IMPLICIT OPTION TO GNC OR REMOVE NEWTON FROM ' // &
  !cdl       'GWT EXCHANGE.')
  !cdl     call store_error_unit(this%ingnc)
  !cdl   endif
  !cdl   !
  !cdl   ! -- Perform checks to ensure GNCs match with GWT-GWT nodes
  !cdl   if(this%nexg /= this%gnc%nexg) then
  !cdl     call store_error('NUMBER OF EXCHANGES DOES NOT MATCH NUMBER OF GNCs')
  !cdl     call store_error_unit(this%ingnc)
  !cdl   endif
  !cdl   !
  !cdl   ! -- Go through each entry and confirm
  !cdl   do i = 1, this%nexg
  !cdl     if(this%nodem1(i) /= this%gnc%nodem1(i) .or.                             &
  !cdl         this%nodem2(i) /= this%gnc%nodem2(i) ) then
  !cdl       nm1 = this%gwtmodel1%dis%get_nodeuser(this%nodem1(i))
  !cdl       nm2 = this%gwtmodel2%dis%get_nodeuser(this%nodem2(i))
  !cdl       nmgnc1 = this%gwtmodel1%dis%get_nodeuser(this%gnc%nodem1(i))
  !cdl       nmgnc2 = this%gwtmodel2%dis%get_nodeuser(this%gnc%nodem2(i))
  !cdl       write(errmsg, fmterr) nm1, nm2, nmgnc1, nmgnc2
  !cdl       call store_error(errmsg)
  !cdl     endif
  !cdl   enddo
  !cdl   if(count_errors() > 0) then
  !cdl     call store_error_unit(this%ingnc)
  !cdl   endif
  !cdl   !
  !cdl   ! -- close the file
  !cdl   close(this%ingnc)
  !cdl   !
  !cdl   ! -- return
  !cdl   return
  !cdl end subroutine read_gnc
  
  !> @ brief Read mover
  !!
  !! Read and process movers
  !!
  !<
  !cdl subroutine read_mvr(this, iout)
  !cdl   ! -- modules
  !cdl   use GwtMvrModule, only: mvr_cr
  !cdl   ! -- dummy
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   integer(I4B), intent(in) :: iout
  !cdl   ! -- local
  !cdl   !
  !cdl   ! -- Create and initialize the mover object  Here, dis is set to the one
  !cdl   !    for gwtmodel1 so that a call to save flows has an associated dis
  !cdl   !    object.  Because the conversion flags for the mover are both false,
  !cdl   !    the dis object does not convert from reduced to user node numbers. 
  !cdl   !    So in this case, the dis object is just writing unconverted package
  !cdl   !    numbers to the binary budget file.
  !cdl   call mvr_cr(this%mvr, this%name, this%inmvr, iout, this%gwtmodel1%dis,     &
  !cdl               iexgmvr=1)
  !cdl   !
  !cdl   ! -- Return
  !cdl   return
  !cdl end subroutine read_mvr
  
  !> @ brief Rewet
  !!
  !! Check if rewetting should propagate from one model to another
  !!
  !<
  !cdl subroutine rewet(this, kiter)
  !cdl   ! -- modules
  !cdl   use TdisModule, only: kper, kstp
  !cdl   ! -- dummy
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   integer(I4B), intent(in) :: kiter
  !cdl   ! -- local
  !cdl   integer(I4B) :: iexg
  !cdl   integer(I4B) :: n, m
  !cdl   integer(I4B) :: ibdn, ibdm
  !cdl   integer(I4B) :: ihc
  !cdl   real(DP) :: hn, hm
  !cdl   integer(I4B) :: irewet
  !cdl   character(len=30) :: nodestrn, nodestrm
  !cdl   character(len=*),parameter :: fmtrwt =                                     &
  !cdl     "(1x, 'CELL ',A,' REWET FROM GWT MODEL ',A,' CELL ',A,                   &
  !cdl      &' FOR ITER. ',I0, ' STEP ',I0, ' PERIOD ', I0)"
  !cdl   !
  !cdl   ! -- Use model 1 to rewet model 2 and vice versa
  !cdl   do iexg = 1, this%nexg
  !cdl     n = this%nodem1(iexg)
  !cdl     m = this%nodem2(iexg)
  !cdl     hn = this%gwtmodel1%x(n)
  !cdl     hm = this%gwtmodel2%x(m)
  !cdl     ibdn = this%gwtmodel1%ibound(n)
  !cdl     ibdm = this%gwtmodel2%ibound(m)
  !cdl     ihc = this%ihc(iexg)
  !cdl     call this%gwtmodel1%npf%rewet_check(kiter, n, hm, ibdm, ihc,             &
  !cdl       this%gwtmodel1%x, irewet)
  !cdl     if(irewet == 1) then
  !cdl       call this%gwtmodel1%dis%noder_to_string(n, nodestrn)
  !cdl       call this%gwtmodel2%dis%noder_to_string(m, nodestrm)
  !cdl       write(this%gwtmodel1%iout, fmtrwt) trim(nodestrn),                     &
  !cdl         trim(this%gwtmodel2%name), trim(nodestrm), kiter, kstp, kper
  !cdl     endif
  !cdl     call this%gwtmodel2%npf%rewet_check(kiter, m, hn, ibdn, ihc,             &
  !cdl       this%gwtmodel2%x, irewet)
  !cdl     if(irewet == 1) then
  !cdl       call this%gwtmodel1%dis%noder_to_string(n, nodestrm)
  !cdl       call this%gwtmodel2%dis%noder_to_string(m, nodestrn)
  !cdl       write(this%gwtmodel2%iout, fmtrwt) trim(nodestrn),                     &
  !cdl         trim(this%gwtmodel1%name), trim(nodestrm), kiter, kstp, kper
  !cdl     endif
  !cdl     !
  !cdl   enddo
  !cdl   !
  !cdl   ! -- Return
  !cdl   return
  !cdl end subroutine rewet
  
  !> @ brief Calculate the conductance
  !!
  !! Calculate the conductance based on state
  !!
  !<
  !cdl subroutine condcalc(this)
  !cdl   ! -- modules
  !cdl   use ConstantsModule, only: DHALF, DZERO, DONE
  !cdl   use GwtNpfModule, only: hcond, vcond
  !cdl   ! -- dummy
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   ! -- local
  !cdl   integer(I4B) :: iexg
  !cdl   integer(I4B) :: n, m, ihc
  !cdl   integer(I4B) :: ibdn, ibdm
  !cdl   integer(I4B) :: ictn, ictm
  !cdl   real(DP) :: topn, topm
  !cdl   real(DP) :: botn, botm
  !cdl   real(DP) :: satn, satm
  !cdl   real(DP) :: hyn, hym
  !cdl   real(DP) :: angle
  !cdl   real(DP) :: hn, hm
  !cdl   real(DP) :: cond
  !cdl   real(DP) :: fawidth
  !cdl   real(DP), dimension(3) :: vg
  !cdl   !
  !cdl   ! -- Calculate conductance and put into amat
  !cdl   do iexg = 1, this%nexg
  !cdl     ihc = this%ihc(iexg)
  !cdl     n = this%nodem1(iexg)
  !cdl     m = this%nodem2(iexg)
  !cdl     ibdn = this%gwtmodel1%ibound(n)
  !cdl     ibdm = this%gwtmodel2%ibound(m)
  !cdl     ictn = this%gwtmodel1%npf%icelltype(n)
  !cdl     ictm = this%gwtmodel2%npf%icelltype(m)
  !cdl     topn = this%gwtmodel1%dis%top(n)
  !cdl     topm = this%gwtmodel2%dis%top(m)
  !cdl     botn = this%gwtmodel1%dis%bot(n)
  !cdl     botm = this%gwtmodel2%dis%bot(m)
  !cdl     satn = this%gwtmodel1%npf%sat(n)
  !cdl     satm = this%gwtmodel2%npf%sat(m)
  !cdl     hn = this%gwtmodel1%x(n)
  !cdl     hm = this%gwtmodel2%x(m)
  !cdl     !
  !cdl     ! -- Calculate conductance depending on connection orientation
  !cdl     if(ihc == 0) then
  !cdl       !
  !cdl       ! -- Vertical connection
  !cdl       vg(1) = DZERO
  !cdl       vg(2) = DZERO
  !cdl       vg(3) = DONE
  !cdl       hyn = this%gwtmodel1%npf%hy_eff(n, 0, ihc, vg=vg)
  !cdl       hym = this%gwtmodel2%npf%hy_eff(m, 0, ihc, vg=vg)
  !cdl       cond = vcond(ibdn, ibdm, ictn, ictm, this%inewton, this%ivarcv,        &
  !cdl                    this%idewatcv, this%condsat(iexg), hn, hm, hyn, hym,      &
  !cdl                    satn, satm, topn, topm, botn, botm, this%hwva(iexg))
  !cdl     else
  !cdl       !
  !cdl       ! -- Horizontal Connection
  !cdl       hyn = this%gwtmodel1%npf%k11(n)
  !cdl       hym = this%gwtmodel2%npf%k11(m)
  !cdl       !
  !cdl       ! -- Check for anisotropy in models, and recalculate hyn and hym
  !cdl       if(this%ianglex > 0) then
  !cdl         angle = this%auxvar(this%ianglex, iexg)
  !cdl         vg(1) = abs(cos(angle))
  !cdl         vg(2) = abs(sin(angle))
  !cdl         vg(3) = DZERO
  !cdl         !
  !cdl         ! -- anisotropy in model 1
  !cdl         if(this%gwtmodel1%npf%ik22 /= 0) then
  !cdl           hyn = this%gwtmodel1%npf%hy_eff(n, 0, ihc, vg=vg)
  !cdl         endif
  !cdl         !
  !cdl         ! -- anisotropy in model 2
  !cdl         if(this%gwtmodel2%npf%ik22 /= 0) then
  !cdl           hym = this%gwtmodel2%npf%hy_eff(m, 0, ihc, vg=vg)
  !cdl         endif
  !cdl       endif
  !cdl       !
  !cdl       fawidth = this%hwva(iexg)
  !cdl       cond = hcond(ibdn, ibdm, ictn, ictm, this%inewton, this%inewton,       &
  !cdl                    this%ihc(iexg), this%icellavg, 0, 0, this%condsat(iexg),  &
  !cdl                    hn, hm, satn, satm, hyn, hym, topn, topm, botn, botm,     &
  !cdl                    this%cl1(iexg), this%cl2(iexg), fawidth, this%satomega)
  !cdl     endif
  !cdl     !
  !cdl     this%cond(iexg) = cond
  !cdl     !
  !cdl   enddo
  !cdl   !
  !cdl   ! -- Return
  !cdl   return
  !cdl end subroutine condcalc

  !> @ brief Allocate scalars
  !!
  !! Allocate scalar variables
  !!
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    ! -- local
    !
    call this%DisConnExchangeType%allocate_scalars()
    !
    call mem_allocate(this%iprflow, 'IPRFLOW', this%memoryPath)
    call mem_allocate(this%ipakcb, 'IPAKCB', this%memoryPath)
    call mem_allocate(this%inobs, 'INOBS', this%memoryPath)
    call mem_allocate(this%iAdvScheme, 'IADVSCHEME', this%memoryPath)
    this%iprpak = 0
    this%iprflow = 0
    this%ipakcb = 0
    this%inobs = 0
    this%iAdvScheme = 0
    !
    !cdl call mem_allocate(this%inmvr, 'INMVR', this%memoryPath)
    !cdl call mem_allocate(this%icellavg, 'ICELLAVG', this%memoryPath)
    !cdl call mem_allocate(this%ivarcv, 'IVARCV', this%memoryPath)
    !cdl call mem_allocate(this%idewatcv, 'IDEWATCV', this%memoryPath)
    !cdl call mem_allocate(this%inewton, 'INEWTON', this%memoryPath)    
    !cdl call mem_allocate(this%ingnc, 'INGNC', this%memoryPath)
    !cdl call mem_allocate(this%satomega, 'SATOMEGA', this%memoryPath)
    !cdl this%icellavg = 0
    !cdl this%ivarcv = 0
    !cdl this%idewatcv = 0
    !cdl this%inewton = 0    
    !cdl this%ingnc = 0
    !cdl this%inmvr = 0
    !cdl this%satomega = DZERO
    !
    ! -- return
    return
  end subroutine allocate_scalars

  !> @ brief Deallocate
  !!
  !! Deallocate memory associated with this object
  !!
  !<
  subroutine gwt_gwt_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    ! -- local
    !
    ! -- objects
    !cdl if(this%ingnc > 0) then
    !cdl   call this%gnc%gnc_da()
    !cdl   deallocate(this%gnc)
    !cdl endif
    !cdl if (this%inmvr > 0) then
    !cdl   call this%mvr%mvr_da()
    !cdl   deallocate(this%mvr)
    !cdl endif
    call this%obs%obs_da()
    deallocate(this%obs)
    !
    ! -- arrays
    call mem_deallocate(this%cond)    
    !cdl call mem_deallocate(this%condsat)
    !cdl call mem_deallocate(this%idxglo)
    !cdl call mem_deallocate(this%idxsymglo)
    call mem_deallocate(this%simvals)
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
    deallocate(this%filename)
    call mem_deallocate(this%iprflow)
    call mem_deallocate(this%ipakcb)
    call mem_deallocate(this%inobs)
    call mem_deallocate(this%iAdvScheme)
    !
    !cdl call mem_deallocate(this%icellavg)
    !cdl call mem_deallocate(this%ivarcv)
    !cdl call mem_deallocate(this%idewatcv)
    !cdl call mem_deallocate(this%inewton)
    !cdl call mem_deallocate(this%ingnc)
    !cdl call mem_deallocate(this%inmvr)
    !cdl call mem_deallocate(this%satomega)
    !
    ! -- deallocate base
    call this%DisConnExchangeType%disconnex_da()
    !
    ! -- return
    return
  end subroutine gwt_gwt_da
  
  !> @ brief Allocate arrays
  !!
  !! Allocate arrays
  !!
  !<
  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    ! -- local
    character(len=LINELENGTH) :: text
    integer(I4B) :: ntabcol, i
    !
    call this%DisConnExchangeType%allocate_arrays()
    !   
    call mem_allocate(this%cond, this%nexg, 'COND', this%memoryPath)
    !cdl call mem_allocate(this%idxglo, this%nexg, 'IDXGLO', this%memoryPath)
    !cdl call mem_allocate(this%idxsymglo, this%nexg, 'IDXSYMGLO', this%memoryPath)    !
    !cdl call mem_allocate(this%condsat, this%nexg, 'CONDSAT', this%memoryPath)
    call mem_allocate(this%simvals, this%nexg, 'SIMVALS', this%memoryPath)
    !
    ! -- Initialize
    do i = 1, this%nexg
      this%cond(i) = DNODATA
    end do
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
      call this%outputtab1%table_df(this%nexg, ntabcol, this%gwtmodel1%iout,     &
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
      call this%outputtab2%table_df(this%nexg, ntabcol, this%gwtmodel2%iout,     &
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

  !> @ brief Define observations
  !!
  !! Define the observations associated with this object
  !!
  !<
  subroutine gwt_gwt_df_obs(this)
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    ! -- local
    integer(I4B) :: indx
    !
    ! -- Store obs type and assign procedure pointer
    !    for gwt-gwt observation type.
    call this%obs%StoreObsType('flow-ja-face', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => gwt_gwt_process_obsID
    !
    ! -- return
    return
  end subroutine gwt_gwt_df_obs
  
  !> @ brief Read and prepare observations
  !!
  !! Handle observation exchanges exchange-boundary names.
  !!
  !<
  subroutine gwt_gwt_rp_obs(this)
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: j
    class(ObserveType), pointer :: obsrv => null()
    character(len=LENBOUNDNAME) :: bname
    logical :: jfound
    ! -- formats
10  format('Exchange "',a,'" for observation "',a,               &
           '" is invalid in package "',a,'"')
20  format('Exchange id "',i0,'" for observation "',a,               &
           '" is invalid in package "',a,'"')
    !
    do i = 1, this%obs%npakobs
      obsrv => this%obs%pakobs(i)%obsrv
      !
      ! -- indxbnds needs to be reset each stress period because 
      !    list of boundaries can change each stress period.
      ! -- Not true for exchanges, but leave this in for now anyway.
      call obsrv%ResetObsIndex()
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
            call obsrv%AddObsIndex(j)
          endif
        enddo
        if (.not. jfound) then
          write(errmsg, 10) trim(bname), trim(obsrv%ObsTypeId) , trim(this%name)
          call store_error(errmsg)
        endif
      else
        ! -- Observation location is a single exchange number
        if (obsrv%intPak1 <= this%nexg .and. obsrv%intPak1 > 0) then
          jfound = .true.
          obsrv%BndFound = .true.
          obsrv%CurrentTimeStepEndValue = DZERO
          call obsrv%AddObsIndex(obsrv%intPak1)
        else
          jfound = .false.
        endif
        if (.not. jfound) then
          write(errmsg, 20) obsrv%intPak1, trim(obsrv%ObsTypeId) , trim(this%name)
          call store_error(errmsg)
        endif
      endif
    enddo
    !
    ! -- write summary of error messages
    if (count_errors() > 0) then
      call store_error_unit(this%inobs)
    endif
    !
    ! -- Return
    return
  end subroutine gwt_gwt_rp_obs
  
  !> @ brief Final processing
  !!
  !! Conduct any final processing
  !!
  !<
  subroutine gwt_gwt_fp(this)
    ! -- dummy
    class(GwtExchangeType) :: this  !<  GwtExchangeType
    !
    return
  end subroutine gwt_gwt_fp
  
  !> @ brief Calculate flow
  !!
  !! Calculate the flow for the specified exchange and node numbers
  !!
  !<
  !cdl function qcalc(this, iexg, n1, n2)
  !cdl   ! -- return
  !cdl   real(DP) :: qcalc
  !cdl   ! -- dummy
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   integer(I4B), intent(in) :: iexg
  !cdl   integer(I4B), intent(in) :: n1
  !cdl   integer(I4B), intent(in) :: n2
  !cdl   ! -- local
  !cdl   !
  !cdl   ! -- Calculate flow between nodes in the two models
  !cdl   qcalc = this%cond(iexg) * (this%gwtmodel2%x(n2) - this%gwtmodel1%x(n1))
  !cdl   !
  !cdl   ! -- return
  !cdl   return
  !cdl end function qcalc

  !> @ brief Set symmetric flag
  !!
  !! Return flag indicating whether or not this exchange will cause the
  !! coefficient matrix to be asymmetric.
  !!
  !<
  !cdl function gwt_gwt_get_iasym(this) result (iasym)
  !cdl   ! -- dummy
  !cdl   class(GwtExchangeType) :: this  !<  GwtExchangeType
  !cdl   ! -- local
  !cdl   integer(I4B) :: iasym
  !cdl   !
  !cdl   ! -- Start by setting iasym to zero
  !cdl   iasym = 0
  !cdl   !
  !cdl   ! -- Groundwater flow
  !cdl   if (this%inewton /= 0) iasym = 1
  !cdl   !
  !cdl   ! -- GNC
  !cdl   if (this%ingnc > 0) then
  !cdl     if (this%gnc%iasym /= 0) iasym = 1
  !cdl   endif
  !cdl   !
  !cdl   ! -- return
  !cdl   return
  !cdl end function gwt_gwt_get_iasym

  !> @brief Return true when this exchange provides matrix 
  !! coefficients for solving @param model
  !<
  function gwt_gwt_connects_model(this, model) result(is_connected)
    class(GwtExchangeType) :: this                      !<  GwtExchangeType
    class(BaseModelType), pointer, intent(in) :: model  !< the model to which the exchange might hold a connection
    logical(LGP) :: is_connected                        !< true, when connected

    is_connected = .false.
    ! only connected when model is GwtModelType of course
    select type(model)
    class is (GwtModelType)    
    if (associated(this%gwtmodel1, model)) then
      is_connected = .true.
    else if (associated(this%gwtmodel2, model)) then
      is_connected = .true.
    end if    
    end select

  end function gwt_gwt_connects_model

  !> @brief Should interface model be used for this exchange
  !<
  function use_interface_model(this) result(useIM)
    class(GwtExchangeType) :: this !<  GwtExchangeType
    logical(LGP) :: useIM          !< true when interface model should be used
  
    useIM = (this%ixt3d > 0)
  
  end function

  !> @ brief Save simulated flow observations
  !!
  !! Save the simulated flows for each exchange
  !!
  !<
  subroutine gwt_gwt_save_simvals(this)
    ! -- dummy
    use SimModule, only: store_error, store_error_unit
    use ConstantsModule, only: DZERO
    use ObserveModule, only: ObserveType
    class(GwtExchangeType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: n1
    integer(I4B) :: n2
    integer(I4B) :: iexg
    real(DP) :: v
    character(len=100) :: msg
    type(ObserveType), pointer :: obsrv => null()
    !
    ! -- Write simulated values for all gwt-gwt observations
    if (this%obs%npakobs > 0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        do j = 1,  obsrv%indxbnds_count
          iexg = obsrv%indxbnds(j)
          v = DZERO
          select case (obsrv%ObsTypeId)
          case ('FLOW-JA-FACE')
            n1 = this%nodem1(iexg)
            n2 = this%nodem2(iexg)
            v = this%simvals(iexg)
          case default
            msg = 'Error: Unrecognized observation type: ' //                  &
                  trim(obsrv%ObsTypeId)
            call store_error(msg)
            call store_error_unit(this%inobs)
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        enddo
      enddo
    endif
    !
    return
  end subroutine gwt_gwt_save_simvals

  !> @ brief Obs ID processer
  !!
  !! Process observations for this exchange
  !!
  !<
  subroutine gwt_gwt_process_obsID(obsrv, dis, inunitobs, iout)
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
      obsrv%FeatureName = trim(adjustl(strng))
      ! -- Observation may require summing rates from multiple exchange
      !    boundaries, so assign intPak1 as a value that indicates observation
      !    is for a named exchange boundary or group of exchange boundaries.
      obsrv%intPak1 = NAMEDBOUNDFLAG
    endif
    !
    return
  end subroutine gwt_gwt_process_obsID

  !> @ brief Cast polymorphic object as exchange
  !!
  !! Cast polymorphic object as exchange
  !!
  !<
  function CastAsGwtExchange(obj) result (res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(GwtExchangeType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (GwtExchangeType)
      res => obj
    end select
    return
  end function CastAsGwtExchange

  !> @ brief Get exchange from list
  !!
  !! Return an exchange from the list for specified index
  !!
  !<
  function GetGwtExchangeFromList(list, idx) result (res)
    implicit none
    ! -- dummy
    type(ListType),            intent(inout) :: list
    integer(I4B),                   intent(in)    :: idx
    class(GwtExchangeType), pointer    :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsGwtExchange(obj)
    !
    return
  end function GetGwtExchangeFromList



end module GwtGwtExchangeModule


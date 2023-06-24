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

  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: errmsg, model_loc_idx
  use SimModule, only: store_error
  use BaseModelModule, only: BaseModelType, GetBaseModelFromList
  use BaseExchangeModule, only: BaseExchangeType, AddBaseExchangeToList
  use ConstantsModule, only: LENBOUNDNAME, NAMEDBOUNDFLAG, LINELENGTH, &
                             TABCENTER, TABLEFT, LENAUXNAME, DNODATA, &
                             LENMODELNAME
  use ListModule, only: ListType
  use ListsModule, only: basemodellist
  use VirtualModelModule, only: get_virtual_model
  use DisConnExchangeModule, only: DisConnExchangeType
  use GwtModule, only: GwtModelType
  use TspMvtModule, only: TspMvtType
  use ObserveModule, only: ObserveType
  use ObsModule, only: ObsType
  use SimModule, only: count_errors, store_error, &
                       store_error_unit, ustop
  use SimVariablesModule, only: errmsg
  use BlockParserModule, only: BlockParserType
  use TableModule, only: TableType, table_cr
  use MatrixBaseModule

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
    !
    ! -- names of the GWF models that are connected by this exchange
    character(len=LENMODELNAME) :: gwfmodelname1 = '' !< name of gwfmodel that corresponds to gwtmodel1
    character(len=LENMODELNAME) :: gwfmodelname2 = '' !< name of gwfmodel that corresponds to gwtmodel2
    real(DP), dimension(:), pointer, contiguous :: gwfsimvals => null() !< simulated gwf flow rate for each exchange
    !
    ! -- pointers to gwt models
    type(GwtModelType), pointer :: gwtmodel1 => null() !< pointer to GWT Model 1
    type(GwtModelType), pointer :: gwtmodel2 => null() !< pointer to GWT Model 2
    !
    ! -- GWT specific option block:
    integer(I4B), pointer :: inewton => null() !< unneeded newton flag allows for mvt to be used here
    integer(I4B), pointer :: iprflow => null() !< print flag for cell by cell flows
    integer(I4B), pointer :: ipakcb => null() !< save flag for cell by cell flows
    integer(I4B), pointer :: iAdvScheme !< the advection scheme at the interface:
                                                                                 !! 0 = upstream, 1 = central, 2 = TVD
    !
    ! -- Mover transport package
    integer(I4B), pointer :: inmvt => null() !< unit number for mover transport (0 if off)
    type(TspMvtType), pointer :: mvt => null() !< water mover object
    !
    ! -- Observation package
    integer(I4B), pointer :: inobs => null() !< unit number for GWT-GWT observations
    type(ObsType), pointer :: obs => null() !< observation object
    !
    ! -- internal data
    real(DP), dimension(:), pointer, contiguous :: cond => null() !< conductance
    real(DP), dimension(:), pointer, contiguous :: simvals => null() !< simulated flow rate for each exchange
    !
    ! -- table objects
    type(TableType), pointer :: outputtab1 => null()
    type(TableType), pointer :: outputtab2 => null()

  contains

    procedure :: exg_df => gwt_gwt_df
    procedure :: exg_ar => gwt_gwt_ar
    procedure :: exg_rp => gwt_gwt_rp
    procedure :: exg_ad => gwt_gwt_ad
    procedure :: exg_fc => gwt_gwt_fc
    procedure :: exg_bd => gwt_gwt_bd
    procedure :: exg_ot => gwt_gwt_ot
    procedure :: exg_da => gwt_gwt_da
    procedure :: exg_fp => gwt_gwt_fp
    procedure :: connects_model => gwt_gwt_connects_model
    procedure :: use_interface_model
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: read_options
    procedure :: parse_option
    procedure :: read_mvt
    procedure :: gwt_gwt_bdsav
    procedure, private :: gwt_gwt_df_obs
    procedure, private :: gwt_gwt_rp_obs
    procedure, public :: gwt_gwt_save_simvals
    procedure, private :: validate_exchange
  end type GwtExchangeType

contains

  !> @ brief Create GWT GWT exchange
  !!
  !! Create a new GWT to GWT exchange object.
  !!
  !<
  subroutine gwtexchange_create(filename, name, id, m1_id, m2_id)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use BaseModelModule, only: BaseModelType
    use ListsModule, only: baseexchangelist
    use ObsModule, only: obs_cr
    use MemoryHelperModule, only: create_mem_path
    ! -- dummy
    character(len=*), intent(in) :: filename !< filename for reading
    integer(I4B), intent(in) :: id !< id for the exchange
    character(len=*) :: name !< the exchange name
    integer(I4B), intent(in) :: m1_id !< id for model 1
    integer(I4B), intent(in) :: m2_id !< id for model 2
    ! -- local
    type(GwtExchangeType), pointer :: exchange
    class(BaseModelType), pointer :: mb
    class(BaseExchangeType), pointer :: baseexchange
    integer(I4B) :: m1_index, m2_index
    !
    ! -- Create a new exchange and add it to the baseexchangelist container
    allocate (exchange)
    baseexchange => exchange
    call AddBaseExchangeToList(baseexchangelist, baseexchange)
    !
    ! -- Assign id and name
    exchange%id = id
    exchange%name = name
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
    m1_index = model_loc_idx(m1_id)
    mb => GetBaseModelFromList(basemodellist, m1_index)
    select type (mb)
    type is (GwtModelType)
      exchange%model1 => mb
      exchange%gwtmodel1 => mb
    end select
    exchange%v_model1 => get_virtual_model(m1_id)
    !
    ! -- set gwtmodel2
    m2_index = model_loc_idx(m2_id)
    mb => GetBaseModelFromList(basemodellist, m2_index)
    select type (mb)
    type is (GwtModelType)
      exchange%model2 => mb
      exchange%gwtmodel2 => mb
    end select
    exchange%v_model2 => get_virtual_model(m2_id)
    !
    ! -- Verify that gwt model1 is of the correct type
    if (.not. associated(exchange%gwtmodel1) .and. m1_index > 0) then
      write (errmsg, '(3a)') 'Problem with GWT-GWT exchange ', &
        trim(exchange%name), &
        '.  First specified GWT Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Verify that gwf model2 is of the correct type
    if (.not. associated(exchange%gwtmodel2) .and. m2_index > 0) then
      write (errmsg, '(3a)') 'Problem with GWT-GWT exchange ', &
        trim(exchange%name), &
        '.  Second specified GWT Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
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
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    integer(I4B) :: inunit
    !
    ! -- open the file
    inunit = getunit()
    write (iout, '(/a,a)') ' Creating exchange: ', this%name
    call openfile(inunit, iout, this%filename, 'GWT-GWT')
    !
    call this%parser%Initialize(inunit, iout)
    !
    ! -- Ensure models are in same solution
    if (this%gwtmodel1%idsoln /= this%gwtmodel2%idsoln) then
      call store_error('Two models are connected in a GWT '// &
                       'exchange but they are in different solutions. '// &
                       'GWT models must be in same solution: '// &
                       trim(this%gwtmodel1%name)//' '//trim(this%gwtmodel2%name))
      call this%parser%StoreErrorUnit()
    end if
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
    ! -- Read mover information
    if (this%inmvt > 0) then
      call this%read_mvt(iout)
      call this%mvt%mvt_df(this%gwtmodel1%dis)
    end if
    !
    ! -- close the file
    close (inunit)
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
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! local

    ! Ensure gwfmodel names were entered
    if (this%gwfmodelname1 == '') then
      write (errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name), &
                            ' requires that GWFMODELNAME1 be entered in the &
                            &OPTIONS block.'
      call store_error(errmsg)
    end if
    if (this%gwfmodelname2 == '') then
      write (errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name), &
                            ' requires that GWFMODELNAME2 be entered in the &
                            &OPTIONS block.'
      call store_error(errmsg)
    end if

    ! Periodic boundary condition in exchange don't allow XT3D (=interface model)
    if (associated(this%model1, this%model2)) then
      if (this%ixt3d > 0) then
        write (errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name), &
          ' is a periodic boundary condition which cannot'// &
          ' be configured with XT3D'
        call store_error(errmsg)
      end if
    end if

    ! Check to see if dispersion is on in either model1 or model2.
    ! If so, then ANGLDEGX must be provided as an auxiliary variable for this
    ! GWT-GWT exchange (this%ianglex > 0).
    if (this%gwtmodel1%indsp /= 0 .or. this%gwtmodel2%indsp /= 0) then
      if (this%ianglex == 0) then
        write (errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name), &
          ' requires that ANGLDEGX be specified as an'// &
          ' auxiliary variable because dispersion was '// &
          'specified in one or both transport models.'
        call store_error(errmsg)
      end if
    end if

    if (this%ixt3d > 0 .and. this%ianglex == 0) then
      write (errmsg, '(3a)') 'GWT-GWT exchange ', trim(this%name), &
        ' requires that ANGLDEGX be specified as an'// &
        ' auxiliary variable because XT3D is enabled'
      call store_error(errmsg)
    end if

    if (count_errors() > 0) then
      call ustop()
    end if

  end subroutine validate_exchange

  !> @ brief Allocate and read
  !!
  !! Allocated and read and calculate saturated conductance
  !!
  !<
  subroutine gwt_gwt_ar(this)
    ! -- modules
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    !
    ! -- If mover is active, then call ar routine
    if (this%inmvt > 0) call this%mvt%mvt_ar()
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
    class(GwtExchangeType) :: this !<  GwtExchangeType
    !
    ! -- Check with TDIS on whether or not it is time to RP
    if (.not. readnewdata) return
    !
    ! -- Read and prepare for mover
    if (this%inmvt > 0) call this%mvt%mvt_rp()
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
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    !
    ! -- Advance mover
    !if(this%inmvt > 0) call this%mvt%mvt_ad()
    !
    ! -- Push simulated values to preceding time step
    call this%obs%obs_ad()
    !
    ! -- Return
    return
  end subroutine gwt_gwt_ad

  !> @ brief Fill coefficients
  !!
  !! Calculate conductance and fill coefficient matrix
  !!
  !<
  subroutine gwt_gwt_fc(this, kiter, matrix_sln, rhs_sln, inwtflag)
    ! -- modules
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    integer(I4B), intent(in) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    real(DP), dimension(:), intent(inout) :: rhs_sln
    integer(I4B), optional, intent(in) :: inwtflag
    ! -- local
    !
    ! -- Call mvt fc routine
    if (this%inmvt > 0) call this%mvt%mvt_fc(this%gwtmodel1%x, this%gwtmodel2%x)
    !
    ! -- Return
    return
  end subroutine gwt_gwt_fc

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
    class(GwtExchangeType) :: this !<  GwtExchangeType
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
    ! -- Call mvt bd routine
    if (this%inmvt > 0) call this%mvt%mvt_bd(this%gwtmodel1%x, this%gwtmodel2%x)
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
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    character(len=LENBOUNDNAME) :: bname
    character(len=LENPACKAGENAME + 4) :: packname1
    character(len=LENPACKAGENAME + 4) :: packname2
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
        if (this%gwtmodel1%ibound(n1) /= 0 .and. &
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
    if (this%ipakcb /= 0) then
      ibinun1 = this%gwtmodel1%oc%oc_save_unit('BUDGET')
    else
      ibinun1 = 0
    end if
    !
    ! -- If save budget flag is zero for this stress period, then
    !    shut off saving
    if (.not. this%gwtmodel1%oc%oc_save('BUDGET')) ibinun1 = 0
    if (isuppress_output /= 0) then
      ibinun1 = 0
    end if
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if (ibinun1 /= 0) then
      call this%gwtmodel1%dis%record_srcdst_list_header(budtxt(1), &
                                                        this%gwtmodel1%name, &
                                                        this%name, &
                                                        this%gwtmodel2%name, &
                                                        this%name, &
                                                        this%naux, this%auxname, &
                                                        ibinun1, this%nexg, &
                                                        this%gwtmodel1%iout)
    end if
    !
    ! Initialize accumulators
    ratin = DZERO
    ratout = DZERO
    !
    ! -- Loop through all exchanges
    do i = 1, this%nexg
      !
      ! -- Assign boundary name
      if (this%inamedbound > 0) then
        bname = this%boundname(i)
      else
        bname = ''
      end if
      !
      ! -- Calculate the flow rate between n1 and n2
      rrate = DZERO
      n1 = this%nodem1(i)
      n2 = this%nodem2(i)
      !
      ! -- If both cells are active then calculate flow rate
      if (this%gwtmodel1%ibound(n1) /= 0 .and. &
          this%gwtmodel2%ibound(n2) /= 0) then
        rrate = this%simvals(i)
        !
        ! -- Print the individual rates to model list files if requested
        if (this%iprflow /= 0) then
          if (this%gwtmodel1%oc%oc_save('BUDGET')) then
            !
            ! -- set nodestr and write outputtab table
            nodeu = this%gwtmodel1%dis%get_nodeuser(n1)
            call this%gwtmodel1%dis%nodeu_to_string(nodeu, nodestr)
            call this%outputtab1%print_list_entry(i, trim(adjustl(nodestr)), &
                                                  rrate, bname)
          end if
        end if
        if (rrate < DZERO) then
          ratout = ratout - rrate
        else
          ratin = ratin + rrate
        end if
      end if
      !
      ! -- If saving cell-by-cell flows in list, write flow
      n1u = this%gwtmodel1%dis%get_nodeuser(n1)
      n2u = this%gwtmodel2%dis%get_nodeuser(n2)
      if (ibinun1 /= 0) &
        call this%gwtmodel1%dis%record_mf6_list_entry( &
        ibinun1, n1u, n2u, rrate, this%naux, this%auxvar(:, i), &
        .false., .false.)
      !
    end do
    !
    ! -- Print and write budget terms for model 2
    !
    ! -- Set binary unit numbers for saving flows
    if (this%ipakcb /= 0) then
      ibinun2 = this%gwtmodel2%oc%oc_save_unit('BUDGET')
    else
      ibinun2 = 0
    end if
    !
    ! -- If save budget flag is zero for this stress period, then
    !    shut off saving
    if (.not. this%gwtmodel2%oc%oc_save('BUDGET')) ibinun2 = 0
    if (isuppress_output /= 0) then
      ibinun2 = 0
    end if
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if (ibinun2 /= 0) then
      call this%gwtmodel2%dis%record_srcdst_list_header(budtxt(1), &
                                                        this%gwtmodel2%name, &
                                                        this%name, &
                                                        this%gwtmodel1%name, &
                                                        this%name, &
                                                        this%naux, this%auxname, &
                                                        ibinun2, this%nexg, &
                                                        this%gwtmodel2%iout)
    end if
    !
    ! Initialize accumulators
    ratin = DZERO
    ratout = DZERO
    !
    ! -- Loop through all exchanges
    do i = 1, this%nexg
      !
      ! -- Assign boundary name
      if (this%inamedbound > 0) then
        bname = this%boundname(i)
      else
        bname = ''
      end if
      !
      ! -- Calculate the flow rate between n1 and n2
      rrate = DZERO
      n1 = this%nodem1(i)
      n2 = this%nodem2(i)
      !
      ! -- If both cells are active then calculate flow rate
      if (this%gwtmodel1%ibound(n1) /= 0 .and. &
          this%gwtmodel2%ibound(n2) /= 0) then
        rrate = this%simvals(i)
        !
        ! -- Print the individual rates to model list files if requested
        if (this%iprflow /= 0) then
          if (this%gwtmodel2%oc%oc_save('BUDGET')) then
            !
            ! -- set nodestr and write outputtab table
            nodeu = this%gwtmodel2%dis%get_nodeuser(n2)
            call this%gwtmodel2%dis%nodeu_to_string(nodeu, nodestr)
            call this%outputtab2%print_list_entry(i, trim(adjustl(nodestr)), &
                                                  -rrate, bname)
          end if
        end if
        if (rrate < DZERO) then
          ratout = ratout - rrate
        else
          ratin = ratin + rrate
        end if
      end if
      !
      ! -- If saving cell-by-cell flows in list, write flow
      n1u = this%gwtmodel1%dis%get_nodeuser(n1)
      n2u = this%gwtmodel2%dis%get_nodeuser(n2)
      if (ibinun2 /= 0) &
        call this%gwtmodel2%dis%record_mf6_list_entry( &
        ibinun2, n2u, n1u, -rrate, this%naux, this%auxvar(:, i), &
        .false., .false.)
      !
    end do
    !
    ! -- Set icbcfl, ibudfl to zero so that flows will be printed and
    !    saved, if the options were set in the MVT package
    icbcfl = 1
    ibudfl = 1
    !
    ! -- Call mvt bd routine
    !cdl todo: if(this%inmvt > 0) call this%mvt%mvt_bdsav(icbcfl, ibudfl, isuppress_output)
    !
    ! -- Calculate and write simulated values for observations
    if (this%inobs /= 0) then
      call this%gwt_gwt_save_simvals()
    end if
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
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    integer(I4B) :: iexg, n1, n2
    integer(I4B) :: ibudfl
    real(DP) :: flow
    character(len=LINELENGTH) :: node1str, node2str
    ! -- format
    character(len=*), parameter :: fmtheader = &
     "(/1x, 'SUMMARY OF EXCHANGE RATES FOR EXCHANGE ', a, ' WITH ID ', i0, /, &
       &2a16, 5a16, /, 112('-'))"
    character(len=*), parameter :: fmtheader2 = &
     "(/1x, 'SUMMARY OF EXCHANGE RATES FOR EXCHANGE ', a, ' WITH ID ', i0, /, &
       &2a16, 4a16, /, 96('-'))"
    character(len=*), parameter :: fmtdata = &
                                   "(2a16, 5(1pg16.6))"
    !
    ! -- Call bdsave
    call this%gwt_gwt_bdsav()
    !
    ! -- Write a table of exchanges
    if (this%iprflow /= 0) then
      write (iout, fmtheader2) trim(adjustl(this%name)), this%id, 'NODEM1', &
        'NODEM2', 'COND', 'X_M1', 'X_M2', 'FLOW'
      do iexg = 1, this%nexg
        n1 = this%nodem1(iexg)
        n2 = this%nodem2(iexg)
        flow = this%simvals(iexg)
        call this%gwtmodel1%dis%noder_to_string(n1, node1str)
        call this%gwtmodel2%dis%noder_to_string(n2, node2str)
        write (iout, fmtdata) trim(adjustl(node1str)), &
          trim(adjustl(node2str)), &
          this%cond(iexg), this%gwtmodel1%x(n1), &
          this%gwtmodel2%x(n2), flow
      end do
    end if
    !
    !cdl Implement when MVT is ready
    ! -- Mover budget output
    ibudfl = 1
    if (this%inmvt > 0) call this%mvt%mvt_ot_bdsummary(ibudfl)
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
    class(GwtExchangeType) :: this !<  GwtExchangeType
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LINELENGTH) :: keyword
    logical :: isfound
    logical :: endOfBlock
    integer(I4B) :: ierr
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (iout, '(1x,a)') 'PROCESSING GWT-GWT EXCHANGE OPTIONS'
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
        errmsg = "Unknown GWT-GWT exchange option '"//trim(keyword)//"'."
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end do

      write (iout, '(1x,a)') 'END OF GWT-GWT EXCHANGE OPTIONS'
    end if
    !
    ! -- return
    return
  end subroutine read_options

  !> @brief parse option from exchange file
  !<
  function parse_option(this, keyword, iout) result(parsed)
    use InputOutputModule, only: getunit, openfile
    class(GwtExchangeType) :: this !<  GwtExchangeType
    character(len=LINELENGTH), intent(in) :: keyword !< the option name
    integer(I4B), intent(in) :: iout !< for logging
    logical(LGP) :: parsed !< true when parsed
    ! local
    character(len=LINELENGTH) :: fname
    integer(I4B) :: inobs, ilen
    character(len=LINELENGTH) :: subkey

    parsed = .true.

    select case (keyword)
    case ('GWFMODELNAME1')
      call this%parser%GetStringCaps(subkey)
      ilen = len_trim(subkey)
      if (ilen > LENMODELNAME) then
        write (errmsg, '(a,a)') &
          'Invalid model name: ', trim(subkey)
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end if
      if (this%gwfmodelname1 /= '') then
        call store_error('GWFMODELNAME1 has already been set to ' &
                         //trim(this%gwfmodelname1)// &
                         '. Cannot set more than once.')
        call this%parser%StoreErrorUnit()
      end if
      this%gwfmodelname1 = subkey(1:LENMODELNAME)
      write (iout, '(4x,a,a)') &
        'GWFMODELNAME1 IS SET TO: ', trim(this%gwfmodelname1)
    case ('GWFMODELNAME2')
      call this%parser%GetStringCaps(subkey)
      ilen = len_trim(subkey)
      if (ilen > LENMODELNAME) then
        write (errmsg, '(a,a)') &
          'Invalid model name: ', trim(subkey)
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end if
      if (this%gwfmodelname2 /= '') then
        call store_error('GWFMODELNAME2 has already been set to ' &
                         //trim(this%gwfmodelname2)// &
                         '. Cannot set more than once.')
        call this%parser%StoreErrorUnit()
      end if
      this%gwfmodelname2 = subkey(1:LENMODELNAME)
      write (iout, '(4x,a,a)') &
        'GWFMODELNAME2 IS SET TO: ', trim(this%gwfmodelname2)
    case ('PRINT_FLOWS')
      this%iprflow = 1
      write (iout, '(4x,a)') &
        'EXCHANGE FLOWS WILL BE PRINTED TO LIST FILES.'
    case ('SAVE_FLOWS')
      this%ipakcb = -1
      write (iout, '(4x,a)') &
        'EXCHANGE FLOWS WILL BE SAVED TO BINARY BUDGET FILES.'
    case ('MVT6')
      call this%parser%GetStringCaps(subkey)
      if (subkey /= 'FILEIN') then
        call store_error('MVT6 keyword must be followed by '// &
                         '"FILEIN" then by filename.')
        call this%parser%StoreErrorUnit()
      end if
      call this%parser%GetString(fname)
      if (fname == '') then
        call store_error('No MVT6 file specified.')
        call this%parser%StoreErrorUnit()
      end if
      this%inmvt = getunit()
      call openfile(this%inmvt, iout, fname, 'MVT')
      write (iout, '(4x,a)') &
        'WATER MOVER TRANSPORT INFORMATION WILL BE READ FROM ', trim(fname)
    case ('OBS6')
      call this%parser%GetStringCaps(subkey)
      if (subkey /= 'FILEIN') then
        call store_error('OBS8 keyword must be followed by '// &
                         '"FILEIN" then by filename.')
        call this%parser%StoreErrorUnit()
      end if
      this%obs%active = .true.
      call this%parser%GetString(this%obs%inputFilename)
      inobs = GetUnit()
      call openfile(inobs, iout, this%obs%inputFilename, 'OBS')
      this%obs%inUnitObs = inobs
    case ('ADV_SCHEME')
      call this%parser%GetStringCaps(subkey)
      select case (subkey)
      case ('UPSTREAM')
        this%iAdvScheme = 0
      case ('CENTRAL')
        this%iAdvScheme = 1
      case ('TVD')
        this%iAdvScheme = 2
      case default
        errmsg = "Unknown weighting method for advection: '"//trim(subkey)//"'."
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end select
      write (iout, '(4x,a,a)') &
        'CELL AVERAGING METHOD HAS BEEN SET TO: ', trim(subkey)
    case ('DSP_XT3D_OFF')
      this%ixt3d = 0
      write (iout, '(4x,a)') 'XT3D FORMULATION HAS BEEN SHUT OFF.'
    case ('DSP_XT3D_RHS')
      this%ixt3d = 2
      write (iout, '(4x,a)') 'XT3D RIGHT-HAND SIDE FORMULATION IS SELECTED.'
    case ('ADVSCHEME')
      errmsg = 'ADVSCHEME is no longer a valid keyword.  Use ADV_SCHEME &
        &instead.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    case ('XT3D_OFF')
      errmsg = 'XT3D_OFF is no longer a valid keyword.  Use DSP_XT3D_OFF &
        &instead.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    case ('XT3D_RHS')
      errmsg = 'XT3D_RHS is no longer a valid keyword.  Use DSP_XT3D_RHS &
        &instead.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    case default
      parsed = .false.
    end select

  end function parse_option

  !> @ brief Read mover
  !!
  !! Read and process movers
  !!
  !<
  subroutine read_mvt(this, iout)
    ! -- modules
    use TspMvtModule, only: mvt_cr
    ! -- dummy
    class(GwtExchangeType) :: this !<  GwtExchangeType
    integer(I4B), intent(in) :: iout
    ! -- local
    !
    ! -- Create and initialize the mover object  Here, fmi is set to the one
    !    for gwtmodel1 so that a call to save flows has an associated dis
    !    object.
    call mvt_cr(this%mvt, this%name, this%inmvt, iout, this%gwtmodel1%fmi, &
                this%gwtmodel1%eqnsclfac, &
                gwfmodelname1=this%gwfmodelname1, &
                gwfmodelname2=this%gwfmodelname2, &
                fmi2=this%gwtmodel2%fmi)
    !
    ! -- Return
    return
  end subroutine read_mvt

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
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    !
    call this%DisConnExchangeType%allocate_scalars()
    !
    call mem_allocate(this%inewton, 'INEWTON', this%memoryPath)
    call mem_allocate(this%iprflow, 'IPRFLOW', this%memoryPath)
    call mem_allocate(this%ipakcb, 'IPAKCB', this%memoryPath)
    call mem_allocate(this%inobs, 'INOBS', this%memoryPath)
    call mem_allocate(this%iAdvScheme, 'IADVSCHEME', this%memoryPath)
    this%inewton = 0
    this%iprpak = 0
    this%iprflow = 0
    this%ipakcb = 0
    this%inobs = 0
    this%iAdvScheme = 0
    !
    call mem_allocate(this%inmvt, 'INMVT', this%memoryPath)
    this%inmvt = 0
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
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    !
    ! -- objects
    if (this%inmvt > 0) then
      call this%mvt%mvt_da()
      deallocate (this%mvt)
    end if
    call this%obs%obs_da()
    deallocate (this%obs)
    !
    ! -- arrays
    call mem_deallocate(this%cond)
    call mem_deallocate(this%simvals)
    call mem_deallocate(this%gwfsimvals, 'GWFSIMVALS', this%memoryPath) ! linked memory
    !
    ! -- output table objects
    if (associated(this%outputtab1)) then
      call this%outputtab1%table_da()
      deallocate (this%outputtab1)
      nullify (this%outputtab1)
    end if
    if (associated(this%outputtab2)) then
      call this%outputtab2%table_da()
      deallocate (this%outputtab2)
      nullify (this%outputtab2)
    end if
    !
    ! -- scalars
    deallocate (this%filename)
    call mem_deallocate(this%inewton)
    call mem_deallocate(this%iprflow)
    call mem_deallocate(this%ipakcb)
    call mem_deallocate(this%inobs)
    call mem_deallocate(this%iAdvScheme)
    call mem_deallocate(this%inmvt)
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
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    character(len=LINELENGTH) :: text
    integer(I4B) :: ntabcol, i
    !
    call this%DisConnExchangeType%allocate_arrays()
    !
    call mem_allocate(this%cond, this%nexg, 'COND', this%memoryPath)
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
      call this%outputtab1%table_df(this%nexg, ntabcol, this%gwtmodel1%iout, &
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
      call this%outputtab2%table_df(this%nexg, ntabcol, this%gwtmodel2%iout, &
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
    class(GwtExchangeType) :: this !<  GwtExchangeType
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
    class(GwtExchangeType) :: this !<  GwtExchangeType
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: j
    class(ObserveType), pointer :: obsrv => null()
    character(len=LENBOUNDNAME) :: bname
    logical :: jfound
    ! -- formats
10  format('Exchange "', a, '" for observation "', a, &
           '" is invalid in package "', a, '"')
20  format('Exchange id "', i0, '" for observation "', a, &
           '" is invalid in package "', a, '"')
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
        do j = 1, this%nexg
          if (this%boundname(j) == bname) then
            jfound = .true.
            obsrv%BndFound = .true.
            obsrv%CurrentTimeStepEndValue = DZERO
            call obsrv%AddObsIndex(j)
          end if
        end do
        if (.not. jfound) then
          write (errmsg, 10) trim(bname), trim(obsrv%ObsTypeId), trim(this%name)
          call store_error(errmsg)
        end if
      else
        ! -- Observation location is a single exchange number
        if (obsrv%intPak1 <= this%nexg .and. obsrv%intPak1 > 0) then
          jfound = .true.
          obsrv%BndFound = .true.
          obsrv%CurrentTimeStepEndValue = DZERO
          call obsrv%AddObsIndex(obsrv%intPak1)
        else
          jfound = .false.
        end if
        if (.not. jfound) then
          write (errmsg, 20) obsrv%intPak1, trim(obsrv%ObsTypeId), trim(this%name)
          call store_error(errmsg)
        end if
      end if
    end do
    !
    ! -- write summary of error messages
    if (count_errors() > 0) then
      call store_error_unit(this%inobs)
    end if
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
    class(GwtExchangeType) :: this !<  GwtExchangeType
    !
    return
  end subroutine gwt_gwt_fp

  !> @brief Return true when this exchange provides matrix
  !! coefficients for solving @param model
  !<
  function gwt_gwt_connects_model(this, model) result(is_connected)
    class(GwtExchangeType) :: this !<  GwtExchangeType
    class(BaseModelType), pointer, intent(in) :: model !< the model to which the exchange might hold a connection
    logical(LGP) :: is_connected !< true, when connected

    is_connected = .false.
    ! only connected when model is GwtModelType of course
    select type (model)
    class is (GwtModelType)
      if (associated(this%gwtmodel1, model)) then
        is_connected = .true.
      else if (associated(this%gwtmodel2, model)) then
        is_connected = .true.
      end if
    end select

  end function gwt_gwt_connects_model

  !> @brief Should interface model be used for this exchange
  !!
  !! For now this always returns true, since we do not support
  !! a classic-style two-point flux approximation for GWT-GWT.
  !! If we ever add logic to support a simpler non-interface
  !! model flux calculation, then logic should be added here to
  !! set the return accordingly.
  !<
  function use_interface_model(this) result(use_im)
    class(GwtExchangeType) :: this !<  GwtExchangeType
    logical(LGP) :: use_im !< true when interface model should be used

    ! For now set use_im to .true. since the interface model approach
    ! must currently be used for any GWT-GWT exchange.
    use_im = .true.

  end function

  !> @ brief Save simulated flow observations
  !!
  !! Save the simulated flows for each exchange
  !!
  !<
  subroutine gwt_gwt_save_simvals(this)
    ! -- dummy
    use SimModule, only: store_error, store_error_unit
    use SimVariablesModule, only: errmsg
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
    type(ObserveType), pointer :: obsrv => null()
    !
    ! -- Write simulated values for all gwt-gwt observations
    if (this%obs%npakobs > 0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        do j = 1, obsrv%indxbnds_count
          iexg = obsrv%indxbnds(j)
          v = DZERO
          select case (obsrv%ObsTypeId)
          case ('FLOW-JA-FACE')
            n1 = this%nodem1(iexg)
            n2 = this%nodem2(iexg)
            v = this%simvals(iexg)
          case default
            errmsg = 'Unrecognized observation type: '// &
                     trim(obsrv%ObsTypeId)
            call store_error(errmsg)
            call store_error_unit(this%inobs)
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      end do
    end if
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
    type(ObserveType), intent(inout) :: obsrv
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: inunitobs
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: n, iexg, istat
    integer(I4B) :: icol, istart, istop
    real(DP) :: r
    character(len=LINELENGTH) :: strng
    !
    strng = obsrv%IDstring
    icol = 1
    ! -- get exchange index
    call urword(strng, icol, istart, istop, 1, n, r, iout, inunitobs)
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
    end if
    !
    return
  end subroutine gwt_gwt_process_obsID

  !> @ brief Cast polymorphic object as exchange
  !!
  !! Cast polymorphic object as exchange
  !!
  !<
  function CastAsGwtExchange(obj) result(res)
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
  function GetGwtExchangeFromList(list, idx) result(res)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: idx
    class(GwtExchangeType), pointer :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsGwtExchange(obj)
    !
    return
  end function GetGwtExchangeFromList

end module GwtGwtExchangeModule


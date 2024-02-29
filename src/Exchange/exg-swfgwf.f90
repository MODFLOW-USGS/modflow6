!> @brief This module contains the SwfGwfExchangeModule Module
!!
!! This module contains the code for connecting a SWF model with
!! a GWF model.
!!
!<
module SwfGwfExchangeModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, DZERO
  use SimVariablesModule, only: errmsg, iout, model_loc_idx
  use SimModule, only: count_errors, store_error, store_error_unit, &
                       store_error_filename
  use InputOutputModule, only: getunit, openfile
  use MemoryManagerModule, only: mem_allocate
  use BaseModelModule, only: BaseModelType, GetBaseModelFromList
  use NumericalModelModule, only: NumericalModelType
  use BaseExchangeModule, only: BaseExchangeType, AddBaseExchangeToList
  use ListsModule, only: basemodellist, baseexchangelist
  use BlockParserModule, only: BlockParserType
  use ObsModule, only: obs_cr, ObsType
  use MemoryHelperModule, only: create_mem_path
  use NumericalExchangeModule, only: NumericalExchangeType
  use GwfModule, only: GwfModelType
  use SwfModule, only: SwfModelType
  use MatrixBaseModule
  use TableModule, only: TableType, table_cr
  use MemoryManagerModule, only: mem_allocate

  private
  public :: swfgwf_cr

  type, extends(NumericalExchangeType) :: SwfGwfExchangeType

    class(NumericalModelType), pointer :: model1 => null() !< model 1
    class(NumericalModelType), pointer :: model2 => null() !< model 2
    class(SwfModelType), pointer :: swfmodel1 => null() !< pointer to SWF Model 1
    class(GwfModelType), pointer :: gwfmodel2 => null() !< pointer to GWF Model 2

    integer(I4B), pointer :: ipr_input => null() !< flag to print input
    integer(I4B), pointer :: ipr_flow => null() !< print flag for cell by cell flows

    integer(I4B), pointer :: nexg => null() !< number of exchanges
    integer(I4B), dimension(:), pointer, contiguous :: nodem1 => null() !< node numbers in model 1
    integer(I4B), dimension(:), pointer, contiguous :: nodem2 => null() !< node numbers in model 2
    real(DP), dimension(:), pointer, contiguous :: cond => null() !< conductance, size: nexg
    integer(I4B), dimension(:), pointer, contiguous :: idxglo => null() !< mapping to global (solution) amat
    integer(I4B), dimension(:), pointer, contiguous :: idxsymglo => null() !< mapping to global (solution) symmetric amat
    real(DP), dimension(:), pointer, contiguous :: simvals => null() !< simulated flow rate for each exchange

    type(BlockParserType) :: parser !< block parser for input file (controlled from derived type)

    integer(I4B), pointer :: inobs => null() !< unit number for GWF-GWF observations
    type(ObsType), pointer :: obs => null() !< observation object

    ! -- table objects
    type(TableType), pointer :: outputtab1 => null()
    type(TableType), pointer :: outputtab2 => null()

  contains

    procedure :: exg_df => swf_gwf_df
    procedure :: exg_ac => swf_gwf_ac
    procedure :: exg_mc => swf_gwf_mc
    procedure :: exg_fc => swf_gwf_fc
    procedure :: exg_cq => swf_gwf_cq
    procedure :: exg_bd => swf_gwf_bd
    procedure :: exg_ot => swf_gwf_ot
    procedure :: exg_da => swf_gwf_da
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: source_options
    !procedure :: parse_option
    procedure :: source_dimensions
    procedure :: source_data
    procedure :: swf_gwf_calc_simvals
    procedure :: swf_gwf_save_simvals
    procedure :: qcalc
    procedure :: swf_gwf_add_to_flowja
    procedure, private :: swf_gwf_chd_bd
    !todo: procedure :: swf_gwf_bdsav_model
    procedure :: swf_gwf_bdsav
    procedure :: connects_model => swf_gwf_connects_model

    procedure, pass(this) :: noder
    procedure, pass(this) :: cellstr

  end type SwfGwfExchangeType

contains

  !> @ brief Create SWF GWF exchange
  !!
  !! Create a new SWF to GWF exchange object.
  !<
  subroutine swfgwf_cr(filename, name, id, m1_id, m2_id, input_mempath)
    ! -- dummy
    character(len=*), intent(in) :: filename !< filename for reading
    character(len=*) :: name !< exchange name
    integer(I4B), intent(in) :: id !< id for the exchange
    integer(I4B), intent(in) :: m1_id !< id for model 1
    integer(I4B), intent(in) :: m2_id !< id for model 2
    character(len=*), intent(in) :: input_mempath
    ! -- local
    type(SwfGwfExchangeType), pointer :: exchange
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
    exchange%input_mempath = input_mempath
    !
    ! -- allocate scalars and set defaults
    call exchange%allocate_scalars()
    exchange%filename = filename
    exchange%typename = 'SWF-GWF'
    !
    ! -- set swfmodel1
    m1_index = model_loc_idx(m1_id)
    if (m1_index > 0) then
      mb => GetBaseModelFromList(basemodellist, m1_index)
      select type (mb)
      type is (SwfModelType)
        exchange%model1 => mb
        exchange%swfmodel1 => mb
      end select
    end if
    ! exchange%v_model1 => get_virtual_model(m1_id)
    ! exchange%is_datacopy = .not. exchange%v_model1%is_local
    !
    ! -- set gwfmodel2
    m2_index = model_loc_idx(m2_id)
    if (m2_index > 0) then
      mb => GetBaseModelFromList(basemodellist, m2_index)
      select type (mb)
      type is (GwfModelType)
        exchange%model2 => mb
        exchange%gwfmodel2 => mb
      end select
    end if
    ! exchange%v_model2 => get_virtual_model(m2_id)
    !
    ! -- Verify that gwf model1 is of the correct type
    if (.not. associated(exchange%swfmodel1) .and. m1_index > 0) then
      write (errmsg, '(3a)') 'Problem with SWF-GWF exchange ', &
        trim(exchange%name), &
        '.  Specified SWF Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Verify that gwf model2 is of the correct type
    if (.not. associated(exchange%gwfmodel2) .and. m2_index > 0) then
      write (errmsg, '(3a)') 'Problem with SWF-GWF exchange ', &
        trim(exchange%name), &
        '.  Specified GWF Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Create the obs package
    call obs_cr(exchange%obs, exchange%inobs)
    !
    ! -- Return
    return
  end subroutine swfgwf_cr

  !> @ brief Define SWF GWF exchange
  !!
  !! Define SWF to GWF exchange object.
  !<
  subroutine swf_gwf_df(this)
    ! -- modules
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    ! -- local
    !
    ! -- log the exchange
    write (iout, '(/a,a)') ' Creating exchange: ', this%name
    !
    ! -- Ensure models are in same solution
    if (associated(this%swfmodel1) .and. associated(this%gwfmodel2)) then
      if (this%swfmodel1%idsoln /= this%gwfmodel2%idsoln) then
        call store_error('Two models are connected in a SWF-GWF '// &
                         'exchange but they are in different solutions. '// &
                         'Models must be in same solution: '// &
                         trim(this%swfmodel1%name)//' '// &
                         trim(this%gwfmodel2%name))
        call this%parser%StoreErrorUnit()
      end if
    end if
    !
    ! -- source options
    call this%source_options(iout)
    !
    ! -- source dimensions
    call this%source_dimensions(iout)
    !
    ! -- allocate arrays
    call this%allocate_arrays()
    !
    ! -- source exchange data
    call this%source_data(iout)
    !
    ! -- Store obs
    ! call this%swf_gwf_df_obs()
    ! if (associated(this%swfmodel1)) then
    !   call this%obs%obs_df(iout, this%name, 'SWF-GWF', this%swfmodel1%dis)
    ! end if
    ! !
    ! ! -- validate
    ! call this%validate_exchange()
    !
    ! -- Return
    return
  end subroutine swf_gwf_df

  !> @ brief Add connections
  !!
  !! Override parent exg_ac so that gnc can add connections here.
  !<
  subroutine swf_gwf_ac(this, sparse)
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    integer(I4B) :: n, iglo, jglo
    !
    ! -- add exchange connections
    do n = 1, this%nexg
      iglo = this%nodem1(n) + this%swfmodel1%moffset
      jglo = this%nodem2(n) + this%gwfmodel2%moffset
      call sparse%addconnection(iglo, jglo, 1)
      call sparse%addconnection(jglo, iglo, 1)
    end do
    !
    ! -- Return
    return
  end subroutine swf_gwf_ac

  !> @ brief Map connections
  !!
  !! Map the connections in the global matrix
  !<
  subroutine swf_gwf_mc(this, matrix_sln)
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    class(MatrixBaseType), pointer :: matrix_sln !< the system matrix
    ! -- local
    integer(I4B) :: n, iglo, jglo
    !
    ! -- map exchange connections
    do n = 1, this%nexg
      iglo = this%nodem1(n) + this%swfmodel1%moffset
      jglo = this%nodem2(n) + this%gwfmodel2%moffset
      this%idxglo(n) = matrix_sln%get_position(iglo, jglo)
      this%idxsymglo(n) = matrix_sln%get_position(jglo, iglo)
    end do
    !
    ! -- Return
    return
  end subroutine swf_gwf_mc

  !> @ brief Fill coefficients
  !!
  !! Fill conductance into coefficient matrix.  For now assume
  !! all connections are vertical and no newton correction is
  !! needed.
  !<
  subroutine swf_gwf_fc(this, kiter, matrix_sln, rhs_sln, inwtflag)
    ! -- modules
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    integer(I4B), intent(in) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    real(DP), dimension(:), intent(inout) :: rhs_sln
    integer(I4B), optional, intent(in) :: inwtflag
    ! -- local
    integer(I4B) :: i, nodem1sln, nodem2sln
    !
    ! -- Put this%cond into amatsln
    do i = 1, this%nexg
      call matrix_sln%set_value_pos(this%idxglo(i), this%cond(i))
      call matrix_sln%set_value_pos(this%idxsymglo(i), this%cond(i))

      nodem1sln = this%nodem1(i) + this%swfmodel1%moffset
      nodem2sln = this%nodem2(i) + this%gwfmodel2%moffset
      call matrix_sln%add_diag_value(nodem1sln, -this%cond(i))
      call matrix_sln%add_diag_value(nodem2sln, -this%cond(i))
    end do
    !
    ! -- Return
    return
  end subroutine swf_gwf_fc

  !> @ brief Calculate flow
  !!
  !! Calculate flow between two cells and store in simvals, also set
  !! information needed for specific discharge calculation
  !<
  subroutine swf_gwf_cq(this, icnvg, isuppress_output, isolnid)
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    integer(I4B), intent(inout) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B), intent(in) :: isolnid
    !
    ! -- calculate flow and store in simvals
    call this%swf_gwf_calc_simvals()
    !
    ! -- set flows to model edges in NPF
    ! todo: do we add these flows for specific discharge calculation?
    !call this%swf_gwf_set_flow_to_npf()
    !
    ! -- add exchange flows to model's flowja diagonal
    call this%swf_gwf_add_to_flowja()
    !
    ! -- Return
    return
  end subroutine swf_gwf_cq

  !> @ brief Deallocate
  !!
  !! Deallocate memory associated with this object
  !<
  subroutine swf_gwf_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    !
    ! -- objects
    call this%obs%obs_da()
    deallocate (this%obs)
    !
    ! -- arrays
    call mem_deallocate(this%nodem1)
    call mem_deallocate(this%nodem2)
    call mem_deallocate(this%cond)
    call mem_deallocate(this%idxglo)
    call mem_deallocate(this%idxsymglo)
    call mem_deallocate(this%simvals)
    !
    ! -- scalars
    deallocate (this%filename)
    call mem_deallocate(this%ipr_input)
    call mem_deallocate(this%ipr_flow)
    call mem_deallocate(this%nexg)
    call mem_deallocate(this%inobs)
    !
    ! -- Return
    return
  end subroutine swf_gwf_da

  !> @ brief Allocate scalars
  !!
  !! Allocate scalar variables
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    !
    allocate (this%filename)
    call mem_allocate(this%ipr_input, 'IPR_INPUT', this%memoryPath)
    call mem_allocate(this%ipr_flow, 'IPR_FLOW', this%memoryPath)
    call mem_allocate(this%nexg, 'NEXG', this%memoryPath)
    call mem_allocate(this%inobs, 'INOBS', this%memoryPath)
    !
    this%ipr_input = 0
    this%ipr_flow = 0
    this%nexg = 0
    this%inobs = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  !> @brief Allocate array data, using the number of
  !! connected nodes @param nexg
  !<
  subroutine allocate_arrays(this)
    ! -- dummy
    class(SwfGwfExchangeType) :: this !< instance of exchange object
    !
    allocate (this%filename)
    this%filename = ''
    call mem_allocate(this%nodem1, this%nexg, 'NODEM1', this%memoryPath)
    call mem_allocate(this%nodem2, this%nexg, 'NODEM2', this%memoryPath)
    call mem_allocate(this%cond, this%nexg, 'COND', this%memoryPath)
    call mem_allocate(this%idxglo, this%nexg, 'IDXGLO', this%memoryPath)
    call mem_allocate(this%idxsymglo, this%nexg, 'IDXSYMGLO', this%memoryPath)
    call mem_allocate(this%simvals, this%nexg, 'SIMVALS', this%memoryPath)
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  !> @ brief Source options
  !!
  !! Source the options block
  !<
  subroutine source_options(this, iout)
    ! -- modules
    use ConstantsModule, only: LENVARNAME, DEM6
    use InputOutputModule, only: getunit, openfile
    use MemoryManagerExtModule, only: mem_set_value
    use CharacterStringModule, only: CharacterStringType
    use ExgSwfgwfInputModule, only: ExgSwfgwfParamFoundType
    use SourceCommonModule, only: filein_fname
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  GwfExchangeType
    integer(I4B), intent(in) :: iout
    ! -- local
    type(ExgSwfgwfParamFoundType) :: found
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%ipr_input, 'IPR_INPUT', &
                       this%input_mempath, found%ipr_input)
    call mem_set_value(this%ipr_flow, 'IPR_FLOW', &
                       this%input_mempath, found%ipr_flow)
    !
    write (iout, '(1x,a)') 'PROCESSING SWF-GWF EXCHANGE OPTIONS'
    !
    if (found%ipr_input) then
      write (iout, '(4x,a)') &
        'THE LIST OF EXCHANGES WILL BE PRINTED.'
    end if
    !
    if (found%ipr_flow) then
      write (iout, '(4x,a)') &
        'EXCHANGE FLOWS WILL BE PRINTED TO LIST FILES.'
    end if
    !
    ! -- enforce 0 or 1 OBS6_FILENAME entries in option block
    ! if (.not. this%is_datacopy) then
    !   if (filein_fname(this%obs%inputFilename, 'OBS6_FILENAME', &
    !                    this%input_mempath, this%filename)) then
    !     this%obs%active = .true.
    !     this%obs%inUnitObs = GetUnit()
    !     call openfile(this%obs%inUnitObs, iout, this%obs%inputFilename, 'OBS')
    !   end if
    ! end if
    !
    write (iout, '(1x,a)') 'END OF SWF-GWF EXCHANGE OPTIONS'
    !
    ! -- Return
    return
  end subroutine source_options

  !> @brief Source dimension from input context
  !<
  subroutine source_dimensions(this, iout)
    ! -- modules
    use MemoryManagerExtModule, only: mem_set_value
    use ExgSwfgwfInputModule, only: ExgSwfgwfParamFoundType
    ! -- dummy
    class(SwfGwfExchangeType) :: this !< instance of exchange object
    integer(I4B), intent(in) :: iout !< for logging
    ! -- local
    type(ExgSwfgwfParamFoundType) :: found
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%nexg, 'NEXG', this%input_mempath, found%nexg)
    !
    write (iout, '(1x,a)') 'PROCESSING EXCHANGE DIMENSIONS'
    !
    if (found%nexg) then
      write (iout, '(4x,a,i0)') 'NEXG = ', this%nexg
    end if
    !
    write (iout, '(1x,a)') 'END OF EXCHANGE DIMENSIONS'
    !
    ! -- return
    return
  end subroutine source_dimensions

  !> @brief
  !<
  function noder(this, model, cellid, iout)
    ! -- modules
    use GeomUtilModule, only: get_node
    ! -- dummy
    class(SwfGwfExchangeType) :: this !< instance of exchange object
    class(NumericalModelType), pointer, intent(in) :: model
    integer(I4B), dimension(:), pointer, intent(in) :: cellid
    integer(I4B), intent(in) :: iout !< the output file unit
    integer(I4B) :: noder, node
    !
    if (model%dis%ndim == 1) then
      node = cellid(1)
    elseif (model%dis%ndim == 2) then
      node = get_node(cellid(1), 1, cellid(2), &
                      model%dis%mshape(1), 1, &
                      model%dis%mshape(2))
    else
      node = get_node(cellid(1), cellid(2), cellid(3), &
                      model%dis%mshape(1), &
                      model%dis%mshape(2), &
                      model%dis%mshape(3))
    end if
    noder = model%dis%get_nodenumber(node, 0)
    !
    ! -- return
    return
  end function noder

  !> @brief
  !<
  function cellstr(this, model, cellid, iout)
    ! -- modules
    ! -- dummy
    class(SwfGwfExchangeType) :: this !< instance of exchange object
    class(NumericalModelType), pointer, intent(in) :: model
    integer(I4B), dimension(:), pointer, intent(in) :: cellid
    integer(I4B), intent(in) :: iout !< the output file unit
    character(len=20) :: cellstr
    character(len=*), parameter :: fmtndim1 = &
                                   "('(',i0,')')"
    character(len=*), parameter :: fmtndim2 = &
                                   "('(',i0,',',i0,')')"
    character(len=*), parameter :: fmtndim3 = &
                                   "('(',i0,',',i0,',',i0,')')"
    !
    cellstr = ''
    !
    select case (model%dis%ndim)
    case (1)
      write (cellstr, fmtndim1) cellid(1)
    case (2)
      write (cellstr, fmtndim2) cellid(1), cellid(2)
    case (3)
      write (cellstr, fmtndim3) cellid(1), cellid(2), cellid(3)
    case default
    end select
    !
    ! -- return
    return
  end function cellstr

  !> @brief Source exchange data from input context
  !<
  subroutine source_data(this, iout)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(SwfGwfExchangeType) :: this !< instance of exchange object
    integer(I4B), intent(in) :: iout !< the output file unit
    ! -- local
    integer(I4B), dimension(:, :), contiguous, pointer :: cellidm1
    integer(I4B), dimension(:, :), contiguous, pointer :: cellidm2
    real(DP), dimension(:), contiguous, pointer :: cond
    character(len=20) :: cellstr1, cellstr2
    integer(I4B) :: nerr
    integer(I4B) :: iexg, nodem1, nodem2
    ! -- format
    character(len=*), parameter :: fmtexglabel = "(1x, 3a10, 50(a16))"
    character(len=*), parameter :: fmtexgdata = &
                                   "(5x, a, 1x, a ,50(1pg16.6))"
    !
    call mem_setptr(cellidm1, 'CELLIDM1', this%input_mempath)
    call mem_setptr(cellidm2, 'CELLIDM2', this%input_mempath)
    call mem_setptr(cond, 'COND', this%input_mempath)
    !
    write (iout, '(1x,a)') 'PROCESSING EXCHANGEDATA'
    !
    if (this%ipr_input /= 0) then
      write (iout, fmtexglabel) 'NODEM1', 'NODEM2', 'COND'
    end if
    !
    do iexg = 1, this%nexg
      !
      if (associated(this%model1)) then
        !
        ! -- Determine user node number
        nodem1 = this%noder(this%model1, cellidm1(:, iexg), iout)
        this%nodem1(iexg) = nodem1
        !
      else
        this%nodem1(iexg) = -1
      end if
      !
      if (associated(this%model2)) then
        !
        ! -- Determine user node number
        nodem2 = this%noder(this%model2, cellidm2(:, iexg), iout)
        this%nodem2(iexg) = nodem2
        !
      else
        this%nodem2(iexg) = -1
      end if
      !
      ! -- Read rest of input line
      this%cond(iexg) = cond(iexg)
      !
      ! -- Write the data to listing file if requested
      if (this%ipr_input /= 0) then
        cellstr1 = this%cellstr(this%model1, cellidm1(:, iexg), iout)
        cellstr2 = this%cellstr(this%model2, cellidm2(:, iexg), iout)
        write (iout, fmtexgdata) trim(cellstr1), trim(cellstr2), &
          this%cond(iexg)
      end if
      !
      ! -- Check to see if nodem1 is outside of active domain
      if (associated(this%model1)) then
        if (nodem1 <= 0) then
          cellstr1 = this%cellstr(this%model1, cellidm1(:, iexg), iout)
          write (errmsg, *) &
            trim(adjustl(this%model1%name))// &
            ' Cell is outside active grid domain ('// &
            trim(adjustl(cellstr1))//').'
          call store_error(errmsg)
        end if
      end if
      !
      ! -- Check to see if nodem2 is outside of active domain
      if (associated(this%model2)) then
        if (nodem2 <= 0) then
          cellstr2 = this%cellstr(this%model2, cellidm2(:, iexg), iout)
          write (errmsg, *) &
            trim(adjustl(this%model2%name))// &
            ' Cell is outside active grid domain ('// &
            trim(adjustl(cellstr2))//').'
          call store_error(errmsg)
        end if
      end if
    end do
    !
    write (iout, '(1x,a)') 'END OF EXCHANGEDATA'
    !
    ! -- Stop if errors
    nerr = count_errors()
    if (nerr > 0) then
      call store_error('Errors encountered in exchange input file.')
      call store_error_filename(this%filename)
    end if
    !
    ! -- Return
    return
  end subroutine source_data

  !> @brief Calculate flow rates for the exchanges and store them in a member
  !! array
  !<
  subroutine swf_gwf_calc_simvals(this)
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: n1, n2
    integer(I4B) :: ibdn1, ibdn2
    real(DP) :: rrate
    !
    do i = 1, this%nexg
      rrate = DZERO
      n1 = this%nodem1(i)
      n2 = this%nodem2(i)
      ibdn1 = this%swfmodel1%ibound(n1)
      ibdn2 = this%gwfmodel2%ibound(n2)
      if (ibdn1 /= 0 .and. ibdn2 /= 0) then
        rrate = this%qcalc(i, n1, n2)
      end if
      this%simvals(i) = rrate
    end do
    !
    ! -- Return
    return
  end subroutine swf_gwf_calc_simvals

  !> @ brief Calculate flow
  !!
  !! Calculate the flow for the specified exchange and node numbers
  !<
  function qcalc(this, iexg, n1, n2)
    ! -- return
    real(DP) :: qcalc
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    integer(I4B), intent(in) :: iexg
    integer(I4B), intent(in) :: n1
    integer(I4B), intent(in) :: n2
    ! -- local
    !
    ! -- Calculate flow between nodes in the two models
    qcalc = this%cond(iexg) * (this%gwfmodel2%x(n2) - this%swfmodel1%x(n1))
    !
    ! -- Return
    return
  end function qcalc

  !> @brief Add exchange flow to each model flowja diagonal position so that
  !! residual is calculated correctly.
  !<
  subroutine swf_gwf_add_to_flowja(this)
    ! -- modules
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: flow
    !
    do i = 1, this%nexg
      !
      if (associated(this%swfmodel1)) then
        n = this%nodem1(i)
        if (this%swfmodel1%ibound(n) > 0) then
          flow = this%simvals(i)
          idiag = this%swfmodel1%ia(n)
          this%swfmodel1%flowja(idiag) = this%swfmodel1%flowja(idiag) + flow
        end if
      end if
      !
      if (associated(this%gwfmodel2)) then
        n = this%nodem2(i)
        if (this%gwfmodel2%ibound(n) > 0) then
          flow = -this%simvals(i)
          idiag = this%gwfmodel2%ia(n)
          this%gwfmodel2%flowja(idiag) = this%gwfmodel2%flowja(idiag) + flow
        end if
      end if
      !
    end do
    !
    ! -- Return
    return
  end subroutine swf_gwf_add_to_flowja

  !> @ brief Budget
  !!
  !! Accumulate budget terms
  !<
  subroutine swf_gwf_bd(this, icnvg, isuppress_output, isolnid)
    ! -- modules
    use ConstantsModule, only: DZERO, LENBUDTXT, LENPACKAGENAME
    use BudgetModule, only: rate_accumulator
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    integer(I4B), intent(inout) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B), intent(in) :: isolnid
    ! -- local
    character(len=LENBUDTXT), dimension(1) :: budtxt
    real(DP), dimension(2, 1) :: budterm
    real(DP) :: ratin, ratout
    !
    ! -- initialize
    budtxt(1) = '    FLOW-JA-FACE'
    !
    ! -- Calculate ratin/ratout and pass to model budgets
    call rate_accumulator(this%simvals, ratin, ratout)
    !
    ! -- Add the budget terms to model 1
    if (associated(this%swfmodel1)) then
      budterm(1, 1) = ratin
      budterm(2, 1) = ratout
      call this%swfmodel1%model_bdentry(budterm, budtxt, this%name)
    end if
    !
    ! -- Add the budget terms to model 2
    if (associated(this%gwfmodel2)) then
      budterm(1, 1) = ratout
      budterm(2, 1) = ratin
      call this%gwfmodel2%model_bdentry(budterm, budtxt, this%name)
    end if
    !
    ! -- Add any flows from one model into a constant head in another model
    !    as a separate budget term called FLOW-JA-FACE-CHD
    call this%swf_gwf_chd_bd()
    !
    ! -- Return
    return
  end subroutine swf_gwf_bd

  !> @ brief swf-gwf-chd-bd
  !!
  !! Account for flow from an external model into a chd cell
  !<
  subroutine swf_gwf_chd_bd(this)
    ! -- modules
    use ConstantsModule, only: DZERO, LENBUDTXT, LENPACKAGENAME
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  GwfExchangeType
    ! -- local
    character(len=LENBUDTXT), dimension(1) :: budtxt
    integer(I4B) :: n
    integer(I4B) :: i
    real(DP), dimension(2, 1) :: budterm
    real(DP) :: ratin, ratout
    real(DP) :: q
    !
    ! -- initialize
    budtxt(1) = 'FLOW-JA-FACE-CHD'
    !
    ! -- Add the constant-head budget terms for flow from model 2 into model 1
    if (associated(this%swfmodel1)) then
      ratin = DZERO
      ratout = DZERO
      do i = 1, this%nexg
        n = this%nodem1(i)
        if (this%swfmodel1%ibound(n) < 0) then
          q = this%simvals(i)
          if (q > DZERO) then
            ratout = ratout + q
          else
            ratin = ratin - q
          end if
        end if
      end do
      budterm(1, 1) = ratin
      budterm(2, 1) = ratout
      call this%swfmodel1%model_bdentry(budterm, budtxt, this%name)
    end if
    !
    ! -- Add the constant-head budget terms for flow from model 1 into model 2
    if (associated(this%gwfmodel2)) then
      ratin = DZERO
      ratout = DZERO
      do i = 1, this%nexg
        n = this%nodem2(i)
        if (this%gwfmodel2%ibound(n) < 0) then
          ! -- flip flow sign as flow is relative to model 1
          q = -this%simvals(i)
          if (q > DZERO) then
            ratout = ratout + q
          else
            ratin = ratin - q
          end if
        end if
      end do
      budterm(1, 1) = ratin
      budterm(2, 1) = ratout
      call this%gwfmodel2%model_bdentry(budterm, budtxt, this%name)
    end if
    !
    ! -- Return
    return
  end subroutine swf_gwf_chd_bd

  !> @ brief Budget save
  !!
  !! Output individual flows to listing file and binary budget files
  !<
  subroutine swf_gwf_bdsav(this)
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    ! -- local
    integer(I4B) :: icbcfl, ibudfl
    ! !
    ! ! -- budget for model1
    ! if (associated(this%swfmodel1)) then
    !   call this%swf_gwf_bdsav_model(this%swfmodel1, this%gwfmodel2%name)
    ! end if
    ! !
    ! ! -- budget for model2
    ! if (associated(this%gwfmodel2)) then
    !   call this%swf_gwf_bdsav_model(this%gwfmodel2, this%swfmodel1%name)
    ! end if
    !
    ! -- Set icbcfl, ibudfl to zero so that flows will be printed and
    !    saved, if the options were set in the MVR package
    icbcfl = 1
    ibudfl = 1
    !
    ! -- Calculate and write simulated values for observations
    if (this%inobs /= 0) then
      call this%swf_gwf_save_simvals()
    end if
    !
    ! -- Return
    return
  end subroutine swf_gwf_bdsav

  ! subroutine swf_gwf_bdsav_model(this, model, neighbor_name)
  !   ! -- modules
  !   use ConstantsModule, only: DZERO, LENBUDTXT, LENPACKAGENAME
  !   use TdisModule, only: kstp, kper
  !   ! -- dummy
  !   class(SwfGwfExchangeType) :: this !< this exchange
  !   class(NumericalModelType), pointer :: model !< the model to save budget for
  !   character(len=*), intent(in) :: neighbor_name !< name of the connected neighbor model
  !   ! -- local
  !   character(len=LENPACKAGENAME + 4) :: packname
  !   character(len=LENBUDTXT), dimension(1) :: budtxt
  !   type(TableType), pointer :: output_tab
  !   character(len=20) :: nodestr
  !   character(len=LENBOUNDNAME) :: bname
  !   integer(I4B) :: ntabrows
  !   integer(I4B) :: nodeu
  !   integer(I4B) :: i, n1, n2, n1u, n2u
  !   integer(I4B) :: ibinun
  !   real(DP) :: ratin, ratout, rrate
  !   logical(LGP) :: is_for_model1
  !   !
  !   budtxt(1) = '    FLOW-JA-FACE'
  !   packname = 'EXG '//this%name
  !   packname = adjustr(packname)
  !   if (associated(model, this%swfmodel1)) then
  !     output_tab => this%outputtab1
  !     is_for_model1 = .true.
  !   else
  !     output_tab => this%outputtab2
  !     is_for_model1 = .false.
  !   end if
  !   !
  !   ! -- update output tables
  !   if (this%ipr_flow /= 0) then
  !     !
  !     ! -- update titles
  !     if (model%oc%oc_save('BUDGET')) then
  !       call output_tab%set_title(packname)
  !     end if
  !     !
  !     ! -- set table kstp and kper
  !     call output_tab%set_kstpkper(kstp, kper)
  !     !
  !     ! -- update maxbound of tables
  !     ntabrows = 0
  !     do i = 1, this%nexg
  !       n1 = this%nodem1(i)
  !       n2 = this%nodem2(i)
  !       !
  !       ! -- If both cells are active then calculate flow rate
  !       if (this%swfmodel1%ibound(n1) /= 0 .and. &
  !           this%gwfmodel2%ibound(n2) /= 0) then
  !         ntabrows = ntabrows + 1
  !       end if
  !     end do
  !     if (ntabrows > 0) then
  !       call output_tab%set_maxbound(ntabrows)
  !     end if
  !   end if
  !   !
  !   ! -- Print and write budget terms
  !   !
  !   ! -- Set binary unit numbers for saving flows
  !   if (this%ipakcb /= 0) then
  !     ibinun = model%oc%oc_save_unit('BUDGET')
  !   else
  !     ibinun = 0
  !   end if
  !   !
  !   ! -- If save budget flag is zero for this stress period, then
  !   !    shut off saving
  !   if (.not. model%oc%oc_save('BUDGET')) ibinun = 0
  !   !
  !   ! -- If cell-by-cell flows will be saved as a list, write header.
  !   if (ibinun /= 0) then
  !     call model%dis%record_srcdst_list_header(budtxt(1), &
  !                                              model%name, &
  !                                              this%name, &
  !                                              neighbor_name, &
  !                                              this%name, &
  !                                              this%naux, this%auxname, &
  !                                              ibinun, this%nexg, &
  !                                              model%iout)
  !   end if
  !   !
  !   ! Initialize accumulators
  !   ratin = DZERO
  !   ratout = DZERO
  !   !
  !   ! -- Loop through all exchanges
  !   do i = 1, this%nexg
  !     !
  !     ! -- Assign boundary name
  !     if (this%inamedbound > 0) then
  !       bname = this%boundname(i)
  !     else
  !       bname = ''
  !     end if
  !     !
  !     ! -- Calculate the flow rate between n1 and n2
  !     rrate = DZERO
  !     n1 = this%nodem1(i)
  !     n2 = this%nodem2(i)
  !     !
  !     ! -- If both cells are active then calculate flow rate
  !     if (this%v_model1%ibound%get(n1) /= 0 .and. &
  !         this%v_model2%ibound%get(n2) /= 0) then
  !       rrate = this%simvals(i)
  !       !
  !       ! -- Print the individual rates to model list files if requested
  !       if (this%ipr_flow /= 0) then
  !         if (model%oc%oc_save('BUDGET')) then
  !           !
  !           ! -- set nodestr and write outputtab table
  !           if (is_for_model1) then
  !             nodeu = model%dis%get_nodeuser(n1)
  !             call model%dis%nodeu_to_string(nodeu, nodestr)
  !             call output_tab%print_list_entry(i, trim(adjustl(nodestr)), &
  !                                              rrate, bname)
  !           else
  !             nodeu = model%dis%get_nodeuser(n2)
  !             call model%dis%nodeu_to_string(nodeu, nodestr)
  !             call output_tab%print_list_entry(i, trim(adjustl(nodestr)), &
  !                                              -rrate, bname)
  !           end if
  !         end if
  !       end if
  !       if (rrate < DZERO) then
  !         ratout = ratout - rrate
  !       else
  !         ratin = ratin + rrate
  !       end if
  !     end if
  !     !
  !     ! -- If saving cell-by-cell flows in list, write flow
  !     n1u = this%v_model1%dis_get_nodeuser(n1)
  !     n2u = this%v_model2%dis_get_nodeuser(n2)
  !     if (ibinun /= 0) then
  !       if (is_for_model1) then
  !         call model%dis%record_mf6_list_entry(ibinun, n1u, n2u, rrate, &
  !                                              this%naux, this%auxvar(:, i), &
  !                                              .false., .false.)
  !       else
  !         call model%dis%record_mf6_list_entry(ibinun, n2u, n1u, -rrate, &
  !                                              this%naux, this%auxvar(:, i), &
  !                                              .false., .false.)
  !       end if
  !     end if
  !     !
  !   end do
  !   !
  !   ! -- Return
  !   return
  ! end subroutine swf_gwf_bdsav_model

  !> @ brief Output
  !!
  !! Write output
  !<
  subroutine swf_gwf_ot(this)
    ! -- modules
    use SimVariablesModule, only: iout
    use ConstantsModule, only: DZERO, LINELENGTH
    ! -- dummy
    class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
    ! -- local
    integer(I4B) :: iexg, n1, n2
    real(DP) :: flow
    character(len=LINELENGTH) :: node1str, node2str
    ! -- format
    character(len=*), parameter :: fmtheader2 = &
     "(/1x, 'SUMMARY OF EXCHANGE RATES FOR EXCHANGE ', a, ' WITH ID ', i0, /, &
       &2a16, 4a16, /, 96('-'))"
    character(len=*), parameter :: fmtdata = &
                                   "(2a16, 5(1pg16.6))"
    !
    ! -- Call bdsave
    call this%swf_gwf_bdsav()
    !
    ! -- Write a table of exchanges
    if (this%ipr_flow /= 0) then
      write (iout, fmtheader2) trim(adjustl(this%name)), this%id, 'NODEM1', &
        'NODEM2', 'COND', 'X_M1', 'X_M2', 'FLOW'
      do iexg = 1, this%nexg
        n1 = this%nodem1(iexg)
        n2 = this%nodem2(iexg)
        flow = this%simvals(iexg)
        call this%swfmodel1%dis%noder_to_string(n1, node1str)
        call this%gwfmodel2%dis%noder_to_string(n2, node2str)
        write (iout, fmtdata) trim(adjustl(node1str)), &
          trim(adjustl(node2str)), &
          this%cond(iexg), this%swfmodel1%x(n1), &
          this%gwfmodel2%x(n2), flow
      end do
    end if
    !
    ! -- OBS output
    call this%obs%obs_ot()
    !
    ! -- Return
    return
  end subroutine swf_gwf_ot

  !> @ brief Save simulated flow observations
  !!
  !! Save the simulated flows for each exchange
  !<
  subroutine swf_gwf_save_simvals(this)
    ! -- modules
    use SimModule, only: store_error, store_error_unit
    use SimVariablesModule, only: errmsg
    use ConstantsModule, only: DZERO
    use ObserveModule, only: ObserveType
    ! -- dummy
    class(SwfGwfExchangeType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: n1
    integer(I4B) :: n2
    integer(I4B) :: iexg
    real(DP) :: v
    type(ObserveType), pointer :: obsrv => null()
    !
    ! -- Write simulated values for all gwf-gwf observations
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
    ! -- Return
    return
  end subroutine swf_gwf_save_simvals

  !> @brief Should return true when the exchange should be added to the
  !! solution where the model resides
  !<
  function swf_gwf_connects_model(this, model) result(is_connected)
    ! -- dummy
    class(SwfGwfExchangeType) :: this !< the instance of the exchange
    class(BaseModelType), pointer, intent(in) :: model !< the model to which the exchange might hold a connection
    ! -- return
    logical(LGP) :: is_connected !< true, when connected
    !
    is_connected = .false.
    select type (model)
    class is (GwfModelType)
      if (associated(this%gwfmodel2, model)) then
        is_connected = .true.
      end if
    class is (SwfModelType)
      if (associated(this%swfmodel1, model)) then
        is_connected = .true.
      end if
    end select
    !
    ! -- Return
    return
  end function

end module SwfGwfExchangeModule

! module SwfGwfExchangeModule

!   use KindModule, only: DP, I4B, LGP
!   use SimVariablesModule, only: errmsg
!   use SimModule, only: store_error
!   use BaseModelModule, only: BaseModelType, GetBaseModelFromList
!   use BaseExchangeModule, only: BaseExchangeType, AddBaseExchangeToList
!   use ConstantsModule, only: LENBOUNDNAME, NAMEDBOUNDFLAG, LINELENGTH, &
!                              TABCENTER, TABLEFT, LENAUXNAME, DNODATA
!   use ListModule, only: ListType
!   use ListsModule, only: basemodellist
!   use DisConnExchangeModule, only: DisConnExchangeType
!   use GwfModule, only: GwfModelType
!   use SwfModule, only: SwfModelType
!   use VirtualModelModule, only: VirtualModelType
!   use GhostNodeModule, only: GhostNodeType
!   use GwfMvrModule, only: GwfMvrType
!   use ObserveModule, only: ObserveType
!   use ObsModule, only: ObsType
!   use SimModule, only: count_errors, store_error, store_error_unit
!   use SimVariablesModule, only: errmsg, model_loc_idx
!   use BlockParserModule, only: BlockParserType
!   use TableModule, only: TableType, table_cr
!   use MatrixBaseModule

!   implicit none

!   private
!   public :: SwfGwfExchangeType
!   public :: swfgwf_cr
!   public :: GetSwfGwfExchangeFromList
!   public :: CastAsSwfGwfExchange

!   !> @brief Derived type for SwfGwfExchangeType
!   !!
!   !! This derived type contains information and methods for
!   !! connecting a SWF model with a GWF model.
!   !<
!   type, extends(DisConnExchangeType) :: SwfGwfExchangeType
!     class(SwfModelType), pointer :: swfmodel1 => null() !< pointer to SWF Model 1
!     class(GwfModelType), pointer :: gwfmodel2 => null() !< pointer to GWF Model 2
!     !
!     ! -- GWF specific option block:
!     integer(I4B), pointer :: iprflow => null() !< print flag for cell by cell flows
!     integer(I4B), pointer :: ipakcb => null() !< save flag for cell by cell flows
!     integer(I4B), pointer :: inewton => null() !< newton flag (1 newton is on)
!     integer(I4B), pointer :: icellavg => null() !< cell averaging
!     integer(I4B), pointer :: ivarcv => null() !< variable cv
!     integer(I4B), pointer :: idewatcv => null() !< dewatered cv
!     integer(I4B), pointer :: ingnc => null() !< unit number for gnc (0 if off)
!     type(GhostNodeType), pointer :: gnc => null() !< gnc object
!     integer(I4B), pointer :: inmvr => null() !< unit number for mover (0 if off)
!     type(GwfMvrType), pointer :: mvr => null() !< water mover object
!     integer(I4B), pointer :: inobs => null() !< unit number for GWF-GWF observations
!     type(ObsType), pointer :: obs => null() !< observation object
!     !
!     ! -- internal data
!     real(DP), dimension(:), pointer, contiguous :: cond => null() !< conductance
!     real(DP), dimension(:), pointer, contiguous :: condsat => null() !< saturated conductance
!     integer(I4B), dimension(:), pointer, contiguous :: idxglo => null() !< mapping to global (solution) amat
!     integer(I4B), dimension(:), pointer, contiguous :: idxsymglo => null() !< mapping to global (solution) symmetric amat
!     real(DP), pointer :: satomega => null() !< saturation smoothing
!     real(DP), dimension(:), pointer, contiguous :: simvals => null() !< simulated flow rate for each exchange
!     !
!     ! -- table objects
!     type(TableType), pointer :: outputtab1 => null()
!     type(TableType), pointer :: outputtab2 => null()

!   contains

!     procedure :: exg_df => swf_gwf_df
!     procedure :: exg_ac => swf_gwf_ac
!     procedure :: exg_mc => swf_gwf_mc
!     procedure :: exg_ar => swf_gwf_ar
!     procedure :: exg_rp => swf_gwf_rp
!     procedure :: exg_ad => swf_gwf_ad
!     procedure :: exg_cf => swf_gwf_cf
!     procedure :: exg_fc => swf_gwf_fc
!     procedure :: exg_fn => swf_gwf_fn
!     procedure :: exg_cq => swf_gwf_cq
!     procedure :: exg_bd => swf_gwf_bd
!     procedure :: exg_ot => swf_gwf_ot
!     procedure :: exg_da => swf_gwf_da
!     procedure :: exg_fp => swf_gwf_fp
!     procedure :: get_iasym => swf_gwf_get_iasym
!     procedure :: connects_model => swf_gwf_connects_model
!     procedure :: use_interface_model
!     procedure :: allocate_scalars
!     procedure :: allocate_arrays
!     procedure :: read_options
!     procedure :: parse_option
!     procedure :: read_gnc
!     procedure :: read_mvr
!     procedure, private :: calc_cond_sat
!     procedure, private :: condcalc
!     procedure, private :: rewet
!     procedure, private :: qcalc
!     procedure :: swf_gwf_bdsav
!     procedure, private :: swf_gwf_bdsav_model
!     procedure, private :: swf_gwf_df_obs
!     procedure, private :: swf_gwf_rp_obs
!     procedure, public :: swf_gwf_save_simvals
!     procedure, private :: swf_gwf_calc_simvals
!     procedure, public :: swf_gwf_set_flow_to_npf
!     procedure, private :: validate_exchange
!     procedure :: swf_gwf_add_to_flowja
!   end type SwfGwfExchangeType

! contains

!   !> @ brief Create SWF GWF exchange
!   !!
!   !! Create a new SWF to GWF exchange object.
!   !<
!   subroutine swfgwf_cr(filename, name, id, m1_id, m2_id)
!     ! -- modules
!     use ConstantsModule, only: LINELENGTH
!     use BaseModelModule, only: BaseModelType
!     use VirtualModelModule, only: get_virtual_model
!     use ListsModule, only: baseexchangelist
!     use ObsModule, only: obs_cr
!     use MemoryHelperModule, only: create_mem_path
!     ! -- dummy
!     character(len=*), intent(in) :: filename !< filename for reading
!     character(len=*) :: name !< exchange name
!     integer(I4B), intent(in) :: id !< id for the exchange
!     integer(I4B), intent(in) :: m1_id !< id for model 1
!     integer(I4B), intent(in) :: m2_id !< id for model 2
!     ! -- local
!     type(SwfGwfExchangeType), pointer :: exchange
!     class(BaseModelType), pointer :: mb
!     class(BaseExchangeType), pointer :: baseexchange
!     integer(I4B) :: m1_index, m2_index
!     !
!     ! -- Create a new exchange and add it to the baseexchangelist container
!     allocate (exchange)
!     baseexchange => exchange
!     call AddBaseExchangeToList(baseexchangelist, baseexchange)
!     !
!     ! -- Assign id and name
!     exchange%id = id
!     exchange%name = name
!     exchange%memoryPath = create_mem_path(exchange%name)
!     !
!     ! -- allocate scalars and set defaults
!     call exchange%allocate_scalars()
!     exchange%filename = filename
!     exchange%typename = 'GWF-GWF'
!     !
!     ! -- set swfmodel1
!     m1_index = model_loc_idx(m1_id)
!     if (m1_index > 0) then
!       mb => GetBaseModelFromList(basemodellist, m1_index)
!       select type (mb)
!       type is (SwfModelType)
!         exchange%model1 => mb
!         exchange%swfmodel1 => mb
!       end select
!     end if
!     exchange%v_model1 => get_virtual_model(m1_id)
!     exchange%is_datacopy = .not. exchange%v_model1%is_local
!     !
!     ! -- set gwfmodel2
!     m2_index = model_loc_idx(m2_id)
!     if (m2_index > 0) then
!       mb => GetBaseModelFromList(basemodellist, m2_index)
!       select type (mb)
!       type is (GwfModelType)
!         exchange%model2 => mb
!         exchange%gwfmodel2 => mb
!       end select
!     end if
!     exchange%v_model2 => get_virtual_model(m2_id)
!     !
!     ! -- Verify that gwf model1 is of the correct type
!     if (.not. associated(exchange%swfmodel1) .and. m1_index > 0) then
!       write (errmsg, '(3a)') 'Problem with GWF-GWF exchange ', &
!         trim(exchange%name), &
!         '.  First specified GWF Model does not appear to be of the correct type.'
!       call store_error(errmsg, terminate=.true.)
!     end if
!     !
!     ! -- Verify that gwf model2 is of the correct type
!     if (.not. associated(exchange%gwfmodel2) .and. m2_index > 0) then
!       write (errmsg, '(3a)') 'Problem with GWF-GWF exchange ', &
!         trim(exchange%name), &
!         '.  Second specified GWF Model does not appear to be of the correct type.'
!       call store_error(errmsg, terminate=.true.)
!     end if
!     !
!     ! -- Create the obs package
!     call obs_cr(exchange%obs, exchange%inobs)
!     !
!     ! -- Return
!     return
!   end subroutine swfgwf_cr

!   !> @ brief Define GWF GWF exchange
!   !!
!   !! Define GWF to GWF exchange object.
!   !<
!   subroutine swf_gwf_df(this)
!     ! -- modules
!     use SimVariablesModule, only: iout
!     use InputOutputModule, only: getunit, openfile
!     use GhostNodeModule, only: gnc_cr
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     integer(I4B) :: inunit
!     !
!     ! -- open the file
!     inunit = getunit()
!     write (iout, '(/a,a)') ' Creating exchange: ', this%name
!     call openfile(inunit, iout, this%filename, 'GWF-GWF')
!     !
!     call this%parser%Initialize(inunit, iout)
!     !
!     ! -- Ensure models are in same solution
!     if (associated(this%swfmodel1) .and. associated(this%gwfmodel2)) then
!       if (this%swfmodel1%idsoln /= this%gwfmodel2%idsoln) then
!         call store_error('Two models are connected in a GWF '// &
!                          'exchange but they are in different solutions. '// &
!                          'GWF models must be in same solution: '// &
!                          trim(this%swfmodel1%name)//' '// &
!                          trim(this%gwfmodel2%name))
!         call this%parser%StoreErrorUnit()
!       end if
!     end if
!     !
!     ! -- read options
!     call this%read_options(iout)
!     !
!     ! -- read dimensions
!     call this%read_dimensions(iout)
!     !
!     ! -- allocate arrays
!     call this%allocate_arrays()
!     !
!     ! -- read exchange data
!     call this%read_data(iout)
!     !
!     ! -- call each model and increase the edge count
!     ! TODO: increase edge for gwf so velocity includes vertical leakage component?
!     ! if (associated(this%swfmodel1)) then
!     !   call this%swfmodel1%npf%increase_edge_count(this%nexg)
!     ! end if
!     ! if (associated(this%gwfmodel2)) then
!     !   call this%gwfmodel2%npf%increase_edge_count(this%nexg)
!     ! end if
!     !
!     ! -- Create and read ghost node information
!     if (this%ingnc > 0) then
!       call gnc_cr(this%gnc, this%name, this%ingnc, iout)
!       call this%read_gnc()
!     end if
!     !
!     ! -- Read mover information
!     if (this%inmvr > 0) then
!       call this%read_mvr(iout)
!     end if
!     !
!     ! -- close the file
!     close (inunit)
!     !
!     ! -- Store obs
!     call this%swf_gwf_df_obs()
!     if (associated(this%swfmodel1)) then
!       call this%obs%obs_df(iout, this%name, 'GWF-GWF', this%swfmodel1%dis)
!     end if
!     !
!     ! -- validate
!     call this%validate_exchange()
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_df

!   !> @brief validate exchange data after reading
!   !<
!   subroutine validate_exchange(this)
!     ! -- modules
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     logical(LGP) :: has_k22, has_spdis, has_vsc
!     !
!     ! Periodic boundary condition in exchange don't allow XT3D (=interface model)
!     if (associated(this%model1, this%model2)) then
!       if (this%ixt3d > 0) then
!         write (errmsg, '(3a)') 'GWF-GWF exchange ', trim(this%name), &
!           ' is a periodic boundary condition which cannot'// &
!           ' be configured with XT3D'
!         call store_error(errmsg, terminate=.TRUE.)
!       end if
!     end if
!     !
!     ! XT3D needs angle information
!     if (this%ixt3d > 0 .and. this%ianglex == 0) then
!       write (errmsg, '(3a)') 'GWF-GWF exchange ', trim(this%name), &
!         ' requires that ANGLDEGX be specified as an'// &
!         ' auxiliary variable because XT3D is enabled'
!       call store_error(errmsg, terminate=.TRUE.)
!     end if
!     !
!     ! determine if specific functionality is demanded,
!     ! in model 1 or model 2 (in parallel, only one of
!     ! the models is checked, but the exchange is duplicated)
!     has_k22 = .false.
!     has_spdis = .false.
!     has_vsc = .false.
!     ! if (associated(this%swfmodel1)) then
!     !   has_k22 = (this%swfmodel1%npf%ik22 /= 0)
!     !   has_spdis = (this%swfmodel1%npf%icalcspdis /= 0)
!     !   has_vsc = (this%swfmodel1%npf%invsc /= 0)
!     ! end if
!     if (associated(this%gwfmodel2)) then
!       has_k22 = has_k22 .or. (this%gwfmodel2%npf%ik22 /= 0)
!       has_spdis = has_spdis .or. (this%gwfmodel2%npf%icalcspdis /= 0)
!       has_vsc = has_vsc .or. (this%gwfmodel2%npf%invsc /= 0)
!     end if
!     !
!     ! If horizontal anisotropy is in either model1 or model2,
!     ! ANGLDEGX must be provided as an auxiliary variable for this
!     ! GWF-GWF exchange (this%ianglex > 0).
!     if (has_k22) then
!       if (this%ianglex == 0) then
!         write (errmsg, '(3a)') 'GWF-GWF exchange ', trim(this%name), &
!           ' requires that ANGLDEGX be specified as an'// &
!           ' auxiliary variable because K22 was specified'// &
!           ' in one or both groundwater models.'
!         call store_error(errmsg, terminate=.TRUE.)
!       end if
!     end if
!     !
!     ! If specific discharge is needed for model1 or model2,
!     ! ANGLDEGX must be provided as an auxiliary variable for this
!     ! GWF-GWF exchange (this%ianglex > 0).
!     if (has_spdis) then
!       if (this%ianglex == 0) then
!         write (errmsg, '(3a)') 'GWF-GWF exchange ', trim(this%name), &
!           ' requires that ANGLDEGX be specified as an'// &
!           ' auxiliary variable because specific discharge'// &
!           ' is being calculated in one or both'// &
!           ' groundwater models.'
!         call store_error(errmsg, terminate=.TRUE.)
!       end if
!       if (this%icdist == 0) then
!         write (errmsg, '(3a)') 'GWF-GWF exchange ', trim(this%name), &
!           ' requires that CDIST be specified as an'// &
!           ' auxiliary variable because specific discharge'// &
!           ' is being calculated in one or both'// &
!           ' groundwater models.'
!         call store_error(errmsg, terminate=.TRUE.)
!       end if
!     end if
!     !
!     ! If viscosity is on in either model, then terminate with an
!     ! error as viscosity package doesn't work yet with exchanges.
!     if (has_vsc) then
!       write (errmsg, '(3a)') 'GWF-GWF exchange ', trim(this%name), &
!         ' requires that the Viscosity Package is inactive'// &
!         ' in both of the connected models.'
!       call store_error(errmsg, terminate=.TRUE.)
!     end if
!     !
!     ! -- Return
!     return
!   end subroutine validate_exchange

!   !> @ brief Add connections
!   !!
!   !! Override parent exg_ac so that gnc can add connections here.
!   !<
!   subroutine swf_gwf_ac(this, sparse)
!     ! -- modules
!     use SparseModule, only: sparsematrix
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     type(sparsematrix), intent(inout) :: sparse
!     ! -- local
!     integer(I4B) :: n, iglo, jglo
!     !
!     ! -- add exchange connections
!     do n = 1, this%nexg
!       iglo = this%nodem1(n) + this%swfmodel1%moffset
!       jglo = this%nodem2(n) + this%gwfmodel2%moffset
!       call sparse%addconnection(iglo, jglo, 1)
!       call sparse%addconnection(jglo, iglo, 1)
!     end do
!     !
!     ! -- add gnc connections
!     if (this%ingnc > 0) then
!       call this%gnc%gnc_ac(sparse)
!     end if
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_ac

!   !> @ brief Map connections
!   !!
!   !! Map the connections in the global matrix
!   !<
!   subroutine swf_gwf_mc(this, matrix_sln)
!     ! -- modules
!     use SparseModule, only: sparsematrix
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     class(MatrixBaseType), pointer :: matrix_sln !< the system matrix
!     ! -- local
!     integer(I4B) :: n, iglo, jglo
!     !
!     ! -- map exchange connections
!     do n = 1, this%nexg
!       iglo = this%nodem1(n) + this%swfmodel1%moffset
!       jglo = this%nodem2(n) + this%gwfmodel2%moffset
!       this%idxglo(n) = matrix_sln%get_position(iglo, jglo)
!       this%idxsymglo(n) = matrix_sln%get_position(jglo, iglo)
!     end do
!     !
!     ! -- map gnc connections
!     if (this%ingnc > 0) then
!       call this%gnc%gnc_mc(matrix_sln)
!     end if
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_mc

!   !> @ brief Allocate and read
!   !!
!   !! Allocated and read and calculate saturated conductance
!   !<
!   subroutine swf_gwf_ar(this)
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     !
!     ! -- If mover is active, then call ar routine
!     if (this%inmvr > 0) call this%mvr%mvr_ar()
!     !
!     ! -- Calculate the saturated conductance for all connections
!     if (.not. this%use_interface_model()) call this%calc_cond_sat()
!     !
!     ! -- Observation AR
!     call this%obs%obs_ar()
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_ar

!   !> @ brief Read and prepare
!   !!
!   !! Read new data for mover and obs
!   !<
!   subroutine swf_gwf_rp(this)
!     ! -- modules
!     use TdisModule, only: readnewdata
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     !
!     ! -- Check with TDIS on whether or not it is time to RP
!     if (.not. readnewdata) return
!     !
!     ! -- Read and prepare for mover
!     if (this%inmvr > 0) call this%mvr%mvr_rp()
!     !
!     ! -- Read and prepare for observations
!     call this%swf_gwf_rp_obs()
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_rp

!   !> @ brief Advance
!   !!
!   !! Advance mover and obs
!   !<
!   subroutine swf_gwf_ad(this)
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     !
!     ! -- Advance mover
!     if (this%inmvr > 0) call this%mvr%mvr_ad()
!     !
!     ! -- Push simulated values to preceding time step
!     call this%obs%obs_ad()
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_ad

!   !> @ brief Calculate coefficients
!   !!
!   !! Rewet as necessary
!   !<
!   subroutine swf_gwf_cf(this, kiter)
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     integer(I4B), intent(in) :: kiter
!     !
!     ! -- Rewet cells across models using the wetdry parameters in each model's
!     !    npf package, and the head in the connected model.
!     call this%rewet(kiter)
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_cf

!   !> @ brief Fill coefficients
!   !!
!   !! Calculate conductance and fill coefficient matrix
!   !<
!   subroutine swf_gwf_fc(this, kiter, matrix_sln, rhs_sln, inwtflag)
!     ! -- modules
!     use ConstantsModule, only: DHALF
!     use GwfNpfModule, only: hcond, vcond
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     integer(I4B), intent(in) :: kiter
!     class(MatrixBaseType), pointer :: matrix_sln
!     real(DP), dimension(:), intent(inout) :: rhs_sln
!     integer(I4B), optional, intent(in) :: inwtflag
!     ! -- local
!     integer(I4B) :: inwt, iexg
!     integer(I4B) :: i, nodem1sln, nodem2sln
!     !
!     ! -- calculate the conductance for each exchange connection
!     call this%condcalc()
!     !
!     ! -- if gnc is active, then copy cond into gnc cond (might consider a
!     !    pointer here in the future)
!     if (this%ingnc > 0) then
!       do iexg = 1, this%nexg
!         this%gnc%cond(iexg) = this%cond(iexg)
!       end do
!     end if
!     !
!     ! -- Put this%cond into amatsln
!     do i = 1, this%nexg
!       call matrix_sln%set_value_pos(this%idxglo(i), this%cond(i))
!       call matrix_sln%set_value_pos(this%idxsymglo(i), this%cond(i))

!       nodem1sln = this%nodem1(i) + this%swfmodel1%moffset
!       nodem2sln = this%nodem2(i) + this%gwfmodel2%moffset
!       call matrix_sln%add_diag_value(nodem1sln, -this%cond(i))
!       call matrix_sln%add_diag_value(nodem2sln, -this%cond(i))
!     end do
!     !
!     ! -- Fill the gnc terms in the solution matrix
!     if (this%ingnc > 0) then
!       call this%gnc%gnc_fc(kiter, matrix_sln)
!     end if
!     !
!     ! -- Call mvr fc routine
!     if (this%inmvr > 0) call this%mvr%mvr_fc()
!     !
!     ! -- Set inwt to exchange newton, but shut off if requested by caller
!     inwt = this%inewton
!     if (present(inwtflag)) then
!       if (inwtflag == 0) inwt = 0
!     end if
!     if (inwt /= 0) then
!       call this%exg_fn(kiter, matrix_sln)
!     end if
!     !
!     ! -- Ghost node Newton-Raphson
!     ! if (this%ingnc > 0) then
!     !   if (inwt /= 0) then
!     !     call this%gnc%gnc_fn(kiter, matrix_sln, this%condsat, &
!     !                          ihc_opt=this%ihc, ivarcv_opt=this%ivarcv, &
!     !                          ictm1_opt=this%swfmodel1%npf%icelltype, &
!     !                          ictm2_opt=this%gwfmodel2%npf%icelltype)
!     !   end if
!     ! end if
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_fc

!   !> @ brief Fill Newton
!   !!
!   !! Fill amatsln with Newton terms
!   !<
!   subroutine swf_gwf_fn(this, kiter, matrix_sln)
!     ! -- modules
!     use SmoothingModule, only: sQuadraticSaturationDerivative
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     integer(I4B), intent(in) :: kiter
!     class(MatrixBaseType), pointer :: matrix_sln
!     ! -- local
!     logical :: nisup
!     integer(I4B) :: iexg
!     integer(I4B) :: n, m
!     integer(I4B) :: nodensln, nodemsln
!     integer(I4B) :: ibdn, ibdm
!     real(DP) :: topn, topm
!     real(DP) :: botn, botm
!     real(DP) :: topup, botup
!     real(DP) :: hn, hm
!     real(DP) :: hup, hdn
!     real(DP) :: cond
!     real(DP) :: term
!     real(DP) :: consterm
!     real(DP) :: derv
!     !
!     do iexg = 1, this%nexg
!       n = this%nodem1(iexg)
!       m = this%nodem2(iexg)
!       nodensln = this%nodem1(iexg) + this%swfmodel1%moffset
!       nodemsln = this%nodem2(iexg) + this%gwfmodel2%moffset
!       ibdn = this%swfmodel1%ibound(n)
!       ibdm = this%gwfmodel2%ibound(m)
!       topn = this%swfmodel1%dis%top(n)
!       topm = this%gwfmodel2%dis%top(m)
!       botn = this%swfmodel1%dis%bot(n)
!       botm = this%gwfmodel2%dis%bot(m)
!       hn = this%swfmodel1%x(n)
!       hm = this%gwfmodel2%x(m)
!       if (this%ihc(iexg) == 0) then
!         ! -- vertical connection, newton not supported
!       else
!         ! ! -- determine upstream node
!         ! nisup = .false.
!         ! if (hm < hn) nisup = .true.
!         ! !
!         ! ! -- set upstream top and bot
!         ! if (nisup) then
!         !   topup = topn
!         !   botup = botn
!         !   hup = hn
!         !   hdn = hm
!         ! else
!         !   topup = topm
!         !   botup = botm
!         !   hup = hm
!         !   hdn = hn
!         ! end if
!         ! !
!         ! ! -- no newton terms if upstream cell is confined
!         ! if (nisup) then
!         !   if (this%swfmodel1%npf%icelltype(n) == 0) cycle
!         ! else
!         !   if (this%gwfmodel2%npf%icelltype(m) == 0) cycle
!         ! end if
!         ! !
!         ! ! -- set topup and botup
!         ! if (this%ihc(iexg) == 2) then
!         !   topup = min(topn, topm)
!         !   botup = max(botn, botm)
!         ! end if
!         ! !
!         ! ! get saturated conductivity for derivative
!         ! cond = this%condsat(iexg)
!         ! !
!         ! ! -- TO DO deal with MODFLOW-NWT upstream weighting option
!         ! !
!         ! ! -- compute terms
!         ! consterm = -cond * (hup - hdn)
!         ! derv = sQuadraticSaturationDerivative(topup, botup, hup)
!         ! if (nisup) then
!         !   !
!         !   ! -- fill jacobian with n being upstream
!         !   term = consterm * derv
!         !   this%swfmodel1%rhs(n) = this%swfmodel1%rhs(n) + term * hn
!         !   this%gwfmodel2%rhs(m) = this%gwfmodel2%rhs(m) - term * hn
!         !   call matrix_sln%add_diag_value(nodensln, term)
!         !   if (ibdm > 0) then
!         !     call matrix_sln%add_value_pos(this%idxsymglo(iexg), -term)
!         !   end if
!         ! else
!         !   !
!         !   ! -- fill jacobian with m being upstream
!         !   term = -consterm * derv
!         !   this%swfmodel1%rhs(n) = this%swfmodel1%rhs(n) + term * hm
!         !   this%gwfmodel2%rhs(m) = this%gwfmodel2%rhs(m) - term * hm
!         !   call matrix_sln%add_diag_value(nodemsln, -term)
!         !   if (ibdn > 0) then
!         !     call matrix_sln%add_value_pos(this%idxglo(iexg), term)
!         !   end if
!         ! end if
!       end if
!     end do
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_fn

!   !> @ brief Calculate flow
!   !!
!   !! Calculate flow between two cells and store in simvals, also set
!   !! information needed for specific discharge calculation
!   !<
!   subroutine swf_gwf_cq(this, icnvg, isuppress_output, isolnid)
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     integer(I4B), intent(inout) :: icnvg
!     integer(I4B), intent(in) :: isuppress_output
!     integer(I4B), intent(in) :: isolnid
!     !
!     ! -- calculate flow and store in simvals
!     call this%swf_gwf_calc_simvals()
!     !
!     ! -- set flows to model edges in NPF
!     call this%swf_gwf_set_flow_to_npf()
!     !
!     ! -- add exchange flows to model's flowja diagonal
!     call this%swf_gwf_add_to_flowja()
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_cq

!   !> @brief Calculate flow rates for the exchanges and store them in a member
!   !! array
!   !<
!   subroutine swf_gwf_calc_simvals(this)
!     ! -- modules
!     use ConstantsModule, only: DZERO
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     integer(I4B) :: i
!     integer(I4B) :: n1, n2
!     integer(I4B) :: ibdn1, ibdn2
!     real(DP) :: rrate
!     !
!     do i = 1, this%nexg
!       rrate = DZERO
!       n1 = this%nodem1(i)
!       n2 = this%nodem2(i)
!       ibdn1 = this%swfmodel1%ibound(n1)
!       ibdn2 = this%gwfmodel2%ibound(n2)
!       if (ibdn1 /= 0 .and. ibdn2 /= 0) then
!         rrate = this%qcalc(i, n1, n2)
!         if (this%ingnc > 0) then
!           rrate = rrate + this%gnc%deltaqgnc(i)
!         end if
!       end if
!       this%simvals(i) = rrate
!     end do
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_calc_simvals

!   !> @brief Add exchange flow to each model flowja diagonal position so that
!   !! residual is calculated correctly.
!   !<
!   subroutine swf_gwf_add_to_flowja(this)
!     ! -- modules
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     integer(I4B) :: i
!     integer(I4B) :: n
!     integer(I4B) :: idiag
!     real(DP) :: flow
!     !
!     do i = 1, this%nexg
!       !
!       if (associated(this%swfmodel1)) then
!         flow = this%simvals(i)
!         n = this%nodem1(i)
!         idiag = this%swfmodel1%ia(n)
!         this%swfmodel1%flowja(idiag) = this%swfmodel1%flowja(idiag) + flow
!       end if
!       !
!       if (associated(this%gwfmodel2)) then
!         flow = -this%simvals(i)
!         n = this%nodem2(i)
!         idiag = this%gwfmodel2%ia(n)
!         this%gwfmodel2%flowja(idiag) = this%gwfmodel2%flowja(idiag) + flow
!       end if
!       !
!     end do
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_add_to_flowja

!   !> @brief Set flow rates to the edges in the models
!   !<
!   subroutine swf_gwf_set_flow_to_npf(this)
!     ! -- modules
!     use ConstantsModule, only: DZERO, DPIO180
!     use GwfNpfModule, only: thksatnm
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     integer(I4B) :: iusg
!     integer(I4B) :: i
!     integer(I4B) :: n1, n2
!     integer(I4B) :: ibdn1, ibdn2
!     integer(I4B) :: ictn1, ictn2
!     integer(I4B) :: ihc
!     real(DP) :: rrate
!     real(DP) :: topn1, topn2
!     real(DP) :: botn1, botn2
!     real(DP) :: satn1, satn2
!     real(DP) :: hn1, hn2
!     real(DP) :: nx, ny
!     real(DP) :: distance
!     real(DP) :: dltot
!     real(DP) :: hwva
!     real(DP) :: area
!     real(DP) :: thksat
!     real(DP) :: angle
!     !
!     ! -- Return if neither model needs to calculate specific discharge
!     ! if (this%swfmodel1%npf%icalcspdis == 0 .and. &
!     !     this%gwfmodel2%npf%icalcspdis == 0) return
!     ! !
!     ! ! -- initialize
!     ! iusg = 0
!     ! !
!     ! ! -- Loop through all exchanges using the flow rate
!     ! !    stored in simvals
!     ! do i = 1, this%nexg
!     !   rrate = this%simvals(i)
!     !   n1 = this%nodem1(i)
!     !   n2 = this%nodem2(i)
!     !   ihc = this%ihc(i)
!     !   hwva = this%hwva(i)
!     !   ibdn1 = this%swfmodel1%ibound(n1)
!     !   ibdn2 = this%gwfmodel2%ibound(n2)
!     !   ictn1 = this%swfmodel1%npf%icelltype(n1)
!     !   ictn2 = this%gwfmodel2%npf%icelltype(n2)
!     !   topn1 = this%swfmodel1%dis%top(n1)
!     !   topn2 = this%gwfmodel2%dis%top(n2)
!     !   botn1 = this%swfmodel1%dis%bot(n1)
!     !   botn2 = this%gwfmodel2%dis%bot(n2)
!     !   satn1 = this%swfmodel1%npf%sat(n1)
!     !   satn2 = this%gwfmodel2%npf%sat(n2)
!     !   hn1 = this%swfmodel1%x(n1)
!     !   hn2 = this%gwfmodel2%x(n2)
!     !   !
!     !   ! -- Calculate face normal components
!     !   if (ihc == 0) then
!     !     nx = DZERO
!     !     ny = DZERO
!     !     area = hwva
!     !     if (botn1 < botn2) then
!     !       ! -- n1 is beneath n2, so rate is positive downward.  Flip rate
!     !       !    upward so that points in positive z direction
!     !       rrate = -rrate
!     !     end if
!     !   else
!     !     if (this%ianglex > 0) then
!     !       angle = this%auxvar(this%ianglex, i) * DPIO180
!     !       nx = cos(angle)
!     !       ny = sin(angle)
!     !     else
!     !       ! error?
!     !       call store_error('error in swf_gwf_cq', terminate=.TRUE.)
!     !     end if
!     !     !
!     !     ! -- Calculate the saturated thickness at interface between n1 and n2
!     !     thksat = thksatnm(ibdn1, ibdn2, ictn1, ictn2, this%inewton, ihc, &
!     !                       iusg, hn1, hn2, satn1, satn2, &
!     !                       topn1, topn2, botn1, botn2, this%satomega)
!     !     area = hwva * thksat
!     !   end if
!     !   !
!     !   ! -- Submit this connection and flow information to the npf
!     !   !    package of swfmodel1
!     !   if (this%icdist > 0) then
!     !     dltot = this%auxvar(this%icdist, i)
!     !   else
!     !     call store_error('error in swf_gwf_cq', terminate=.TRUE.)
!     !   end if
!     !   distance = dltot * this%cl1(i) / (this%cl1(i) + this%cl2(i))
!     !   if (this%swfmodel1%npf%icalcspdis == 1) then
!     !     call this%swfmodel1%npf%set_edge_properties(n1, ihc, rrate, area, &
!     !                                                 nx, ny, distance)
!     !   end if
!     !   !
!     !   ! -- Submit this connection and flow information to the npf
!     !   !    package of gwfmodel2
!     !   if (this%icdist > 0) then
!     !     dltot = this%auxvar(this%icdist, i)
!     !   else
!     !     call store_error('error in swf_gwf_cq', terminate=.TRUE.)
!     !   end if
!     !   if (this%gwfmodel2%npf%icalcspdis == 1) then
!     !     distance = dltot * this%cl2(i) / (this%cl1(i) + this%cl2(i))
!     !     if (ihc /= 0) rrate = -rrate
!     !     call this%gwfmodel2%npf%set_edge_properties(n2, ihc, rrate, area, &
!     !                                                 -nx, -ny, distance)
!     !   end if
!     !   !
!     ! end do
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_set_flow_to_npf

!   !> @ brief Budget
!   !!
!   !! Accumulate budget terms
!   !<
!   subroutine swf_gwf_bd(this, icnvg, isuppress_output, isolnid)
!     ! -- modules
!     use ConstantsModule, only: DZERO, LENBUDTXT, LENPACKAGENAME
!     use BudgetModule, only: rate_accumulator
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     integer(I4B), intent(inout) :: icnvg
!     integer(I4B), intent(in) :: isuppress_output
!     integer(I4B), intent(in) :: isolnid
!     ! -- local
!     character(len=LENBUDTXT), dimension(1) :: budtxt
!     real(DP), dimension(2, 1) :: budterm
!     real(DP) :: ratin, ratout
!     !
!     ! -- initialize
!     budtxt(1) = '    FLOW-JA-FACE'
!     !
!     ! -- Calculate ratin/ratout and pass to model budgets
!     call rate_accumulator(this%simvals, ratin, ratout)
!     !
!     ! -- Add the budget terms to model 1
!     if (associated(this%swfmodel1)) then
!       budterm(1, 1) = ratin
!       budterm(2, 1) = ratout
!       call this%swfmodel1%model_bdentry(budterm, budtxt, this%name)
!     end if
!     !
!     ! -- Add the budget terms to model 2
!     if (associated(this%gwfmodel2)) then
!       budterm(1, 1) = ratout
!       budterm(2, 1) = ratin
!       call this%gwfmodel2%model_bdentry(budterm, budtxt, this%name)
!     end if
!     !
!     ! -- Call mvr bd routine
!     if (this%inmvr > 0) call this%mvr%mvr_bd()
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_bd

!   !> @ brief Budget save
!   !!
!   !! Output individual flows to listing file and binary budget files
!   !<
!   subroutine swf_gwf_bdsav(this)
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     integer(I4B) :: icbcfl, ibudfl
!     !
!     ! -- budget for model1
!     if (associated(this%swfmodel1)) then
!       ! TODO: NEED WAY TO BDSAV FOR SWF MODEL call this%swf_gwf_bdsav_model(this%swfmodel1)
!     end if
!     !
!     ! -- budget for model2
!     if (associated(this%gwfmodel2)) then
!       call this%swf_gwf_bdsav_model(this%gwfmodel2)
!     end if
!     !
!     ! -- Set icbcfl, ibudfl to zero so that flows will be printed and
!     !    saved, if the options were set in the MVR package
!     icbcfl = 1
!     ibudfl = 1
!     !
!     ! -- Call mvr bd routine
!     if (this%inmvr > 0) call this%mvr%mvr_bdsav(icbcfl, ibudfl, 0)
!     !
!     ! -- Calculate and write simulated values for observations
!     if (this%inobs /= 0) then
!       call this%swf_gwf_save_simvals()
!     end if
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_bdsav

!   subroutine swf_gwf_bdsav_model(this, model)
!     ! -- modules
!     use ConstantsModule, only: DZERO, LENBUDTXT, LENPACKAGENAME
!     use TdisModule, only: kstp, kper
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !< this exchange
!     class(GwfModelType), pointer :: model !< the model to save budget for
!     ! -- local
!     character(len=LENPACKAGENAME + 4) :: packname
!     character(len=LENBUDTXT), dimension(1) :: budtxt
!     type(TableType), pointer :: output_tab
!     class(VirtualModelType), pointer :: nbr_model
!     character(len=20) :: nodestr
!     character(len=LENBOUNDNAME) :: bname
!     integer(I4B) :: ntabrows
!     integer(I4B) :: nodeu
!     integer(I4B) :: i, n1, n2, n1u, n2u
!     integer(I4B) :: ibinun
!     real(DP) :: ratin, ratout, rrate
!     logical(LGP) :: is_for_model1
!     !
!     budtxt(1) = '    FLOW-JA-FACE'
!     packname = 'EXG '//this%name
!     packname = adjustr(packname)
!     ! if (associated(model, this%swfmodel1)) then
!     !   output_tab => this%outputtab1
!     !   nbr_model => this%v_model2
!     !   is_for_model1 = .true.
!     ! else
!       output_tab => this%outputtab2
!       nbr_model => this%v_model1
!       is_for_model1 = .false.
!     ! end if
!     !
!     ! -- update output tables
!     if (this%iprflow /= 0) then
!       !
!       ! -- update titles
!       if (model%oc%oc_save('BUDGET')) then
!         call output_tab%set_title(packname)
!       end if
!       !
!       ! -- set table kstp and kper
!       call output_tab%set_kstpkper(kstp, kper)
!       !
!       ! -- update maxbound of tables
!       ntabrows = 0
!       do i = 1, this%nexg
!         n1 = this%nodem1(i)
!         n2 = this%nodem2(i)
!         !
!         ! -- If both cells are active then calculate flow rate
!         if (this%v_model1%ibound%get(n1) /= 0 .and. &
!             this%v_model2%ibound%get(n2) /= 0) then
!           ntabrows = ntabrows + 1
!         end if
!       end do
!       if (ntabrows > 0) then
!         call output_tab%set_maxbound(ntabrows)
!       end if
!     end if
!     !
!     ! -- Print and write budget terms
!     !
!     ! -- Set binary unit numbers for saving flows
!     if (this%ipakcb /= 0) then
!       ibinun = model%oc%oc_save_unit('BUDGET')
!     else
!       ibinun = 0
!     end if
!     !
!     ! -- If save budget flag is zero for this stress period, then
!     !    shut off saving
!     if (.not. model%oc%oc_save('BUDGET')) ibinun = 0
!     !
!     ! -- If cell-by-cell flows will be saved as a list, write header.
!     if (ibinun /= 0) then
!       call model%dis%record_srcdst_list_header(budtxt(1), &
!                                                model%name, &
!                                                this%name, &
!                                                nbr_model%name, &
!                                                this%name, &
!                                                this%naux, this%auxname, &
!                                                ibinun, this%nexg, &
!                                                model%iout)
!     end if
!     !
!     ! Initialize accumulators
!     ratin = DZERO
!     ratout = DZERO
!     !
!     ! -- Loop through all exchanges
!     do i = 1, this%nexg
!       !
!       ! -- Assign boundary name
!       if (this%inamedbound > 0) then
!         bname = this%boundname(i)
!       else
!         bname = ''
!       end if
!       !
!       ! -- Calculate the flow rate between n1 and n2
!       rrate = DZERO
!       n1 = this%nodem1(i)
!       n2 = this%nodem2(i)
!       !
!       ! -- If both cells are active then calculate flow rate
!       if (this%v_model1%ibound%get(n1) /= 0 .and. &
!           this%v_model2%ibound%get(n2) /= 0) then
!         rrate = this%simvals(i)
!         !
!         ! -- Print the individual rates to model list files if requested
!         if (this%iprflow /= 0) then
!           if (model%oc%oc_save('BUDGET')) then
!             !
!             ! -- set nodestr and write outputtab table
!             if (is_for_model1) then
!               nodeu = model%dis%get_nodeuser(n1)
!               call model%dis%nodeu_to_string(nodeu, nodestr)
!               call output_tab%print_list_entry(i, trim(adjustl(nodestr)), &
!                                                rrate, bname)
!             else
!               nodeu = model%dis%get_nodeuser(n2)
!               call model%dis%nodeu_to_string(nodeu, nodestr)
!               call output_tab%print_list_entry(i, trim(adjustl(nodestr)), &
!                                                -rrate, bname)
!             end if
!           end if
!         end if
!         if (rrate < DZERO) then
!           ratout = ratout - rrate
!         else
!           ratin = ratin + rrate
!         end if
!       end if
!       !
!       ! -- If saving cell-by-cell flows in list, write flow
!       n1u = this%v_model1%dis_get_nodeuser(n1)
!       n2u = this%v_model2%dis_get_nodeuser(n2)
!       if (ibinun /= 0) then
!         if (is_for_model1) then
!           call model%dis%record_mf6_list_entry(ibinun, n1u, n2u, rrate, &
!                                                this%naux, this%auxvar(:, i), &
!                                                .false., .false.)
!         else
!           call model%dis%record_mf6_list_entry(ibinun, n2u, n1u, -rrate, &
!                                                this%naux, this%auxvar(:, i), &
!                                                .false., .false.)
!         end if
!       end if
!       !
!     end do
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_bdsav_model

!   !> @ brief Output
!   !!
!   !! Write output
!   !<
!   subroutine swf_gwf_ot(this)
!     ! -- modules
!     use SimVariablesModule, only: iout
!     use ConstantsModule, only: DZERO, LINELENGTH
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     integer(I4B) :: iexg, n1, n2
!     integer(I4B) :: ibudfl
!     real(DP) :: flow, deltaqgnc
!     character(len=LINELENGTH) :: node1str, node2str
!     ! -- format
!     character(len=*), parameter :: fmtheader = &
!      "(/1x, 'SUMMARY OF EXCHANGE RATES FOR EXCHANGE ', a, ' WITH ID ', i0, /, &
!        &2a16, 5a16, /, 112('-'))"
!     character(len=*), parameter :: fmtheader2 = &
!      "(/1x, 'SUMMARY OF EXCHANGE RATES FOR EXCHANGE ', a, ' WITH ID ', i0, /, &
!        &2a16, 4a16, /, 96('-'))"
!     character(len=*), parameter :: fmtdata = &
!                                    "(2a16, 5(1pg16.6))"
!     !
!     ! -- Call bdsave
!     call this%swf_gwf_bdsav()
!     !
!     ! -- Initialize
!     deltaqgnc = DZERO
!     !
!     ! -- Write a table of exchanges
!     if (this%iprflow /= 0) then
!       if (this%ingnc > 0) then
!         write (iout, fmtheader) trim(adjustl(this%name)), this%id, 'NODEM1', &
!           'NODEM2', 'COND', 'X_M1', 'X_M2', 'DELTAQGNC', &
!           'FLOW'
!       else
!         write (iout, fmtheader2) trim(adjustl(this%name)), this%id, 'NODEM1', &
!           'NODEM2', 'COND', 'X_M1', 'X_M2', 'FLOW'
!       end if
!       do iexg = 1, this%nexg
!         n1 = this%nodem1(iexg)
!         n2 = this%nodem2(iexg)
!         flow = this%simvals(iexg)
!         call this%v_model1%dis_noder_to_string(n1, node1str)
!         call this%v_model2%dis_noder_to_string(n2, node2str)

!         if (this%ingnc > 0) then
!           deltaqgnc = this%gnc%deltaqgnc(iexg)
!           write (iout, fmtdata) trim(adjustl(node1str)), &
!             trim(adjustl(node2str)), &
!             this%cond(iexg), this%v_model1%x%get(n1), &
!             this%v_model2%x%get(n2), deltaqgnc, flow
!         else
!           write (iout, fmtdata) trim(adjustl(node1str)), &
!             trim(adjustl(node2str)), &
!             this%cond(iexg), this%v_model1%x%get(n1), &
!             this%v_model2%x%get(n2), flow
!         end if
!       end do
!     end if
!     !
!     ! -- Mover budget output
!     ibudfl = 1
!     if (this%inmvr > 0) call this%mvr%mvr_ot_bdsummary(ibudfl)
!     !
!     ! -- OBS output
!     call this%obs%obs_ot()
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_ot

!   !> @ brief Read options
!   !!
!   !! Read the options block
!   !<
!   subroutine read_options(this, iout)
!     ! -- modules
!     use ConstantsModule, only: LINELENGTH, LENAUXNAME, DEM6
!     use MemoryManagerModule, only: mem_allocate
!     use SimModule, only: store_error, store_error_unit
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     integer(I4B), intent(in) :: iout
!     ! -- local
!     character(len=LINELENGTH) :: keyword
!     logical :: isfound
!     logical :: endOfBlock
!     integer(I4B) :: ierr
!     !
!     ! -- get options block
!     call this%parser%GetBlock('OPTIONS', isfound, ierr, &
!                               supportOpenClose=.true., blockRequired=.false.)
!     !
!     ! -- parse options block if detected
!     if (isfound) then
!       write (iout, '(1x,a)') 'PROCESSING GWF-GWF EXCHANGE OPTIONS'
!       do
!         call this%parser%GetNextLine(endOfBlock)
!         if (endOfBlock) then
!           exit
!         end if
!         call this%parser%GetStringCaps(keyword)
!         !
!         ! first parse option in base
!         if (this%DisConnExchangeType%parse_option(keyword, iout)) then
!           cycle
!         end if
!         !
!         ! it's probably ours
!         if (this%parse_option(keyword, iout)) then
!           cycle
!         end if
!         !
!         ! unknown option
!         errmsg = "Unknown GWF-GWF exchange option '"//trim(keyword)//"'."
!         call store_error(errmsg)
!         call this%parser%StoreErrorUnit()
!       end do
!       !
!       write (iout, '(1x,a)') 'END OF GWF-GWF EXCHANGE OPTIONS'
!     end if
!     !
!     ! -- set omega value used for saturation calculations
!     if (this%inewton > 0) then
!       this%satomega = DEM6
!     end if
!     !
!     ! -- Return
!     return
!   end subroutine read_options

!   !> @brief parse option from exchange file
!   !<
!   function parse_option(this, keyword, iout) result(parsed)
!     ! -- modules
!     use InputOutputModule, only: getunit, openfile
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     character(len=LINELENGTH), intent(in) :: keyword !< the option name
!     integer(I4B), intent(in) :: iout !< for logging
!     logical(LGP) :: parsed !< true when parsed
!     ! -- local
!     character(len=LINELENGTH) :: fname
!     integer(I4B) :: inobs
!     character(len=LINELENGTH) :: subkey
!     character(len=:), allocatable :: line
!     !
!     parsed = .true.
!     !
!     sel_opt:select case(keyword)
!     case ('PRINT_FLOWS')
!     this%iprflow = 1
!     write (iout, '(4x,a)') &
!       'EXCHANGE FLOWS WILL BE PRINTED TO LIST FILES.'
!     case ('SAVE_FLOWS')
!     this%ipakcb = -1
!     write (iout, '(4x,a)') &
!       'EXCHANGE FLOWS WILL BE SAVED TO BINARY BUDGET FILES.'
!     case ('ALTERNATIVE_CELL_AVERAGING')
!     call this%parser%GetStringCaps(subkey)
!     select case (subkey)
!     case ('LOGARITHMIC')
!       this%icellavg = 1
!     case ('AMT-LMK')
!       this%icellavg = 2
!     case default
!       errmsg = "Unknown cell averaging method '"//trim(subkey)//"'."
!       call store_error(errmsg)
!       call this%parser%StoreErrorUnit()
!     end select
!     write (iout, '(4x,a,a)') &
!       'CELL AVERAGING METHOD HAS BEEN SET TO: ', trim(subkey)
!     case ('VARIABLECV')
!     this%ivarcv = 1
!     write (iout, '(4x,a)') &
!       'VERTICAL CONDUCTANCE VARIES WITH WATER TABLE.'
!     call this%parser%GetStringCaps(subkey)
!     if (subkey == 'DEWATERED') then
!       this%idewatcv = 1
!       write (iout, '(4x,a)') &
!         'VERTICAL CONDUCTANCE ACCOUNTS FOR DEWATERED PORTION OF   '// &
!         'AN UNDERLYING CELL.'
!     end if
!     case ('NEWTON')
!     this%inewton = 1
!     write (iout, '(4x,a)') &
!       'NEWTON-RAPHSON method used for unconfined cells'
!     case ('GNC6')
!     call this%parser%GetStringCaps(subkey)
!     if (subkey /= 'FILEIN') then
!       call store_error('GNC6 keyword must be followed by '// &
!                        '"FILEIN" then by filename.')
!       call this%parser%StoreErrorUnit()
!     end if
!     call this%parser%GetString(fname)
!     if (fname == '') then
!       call store_error('No GNC6 file specified.')
!       call this%parser%StoreErrorUnit()
!     end if
!     this%ingnc = getunit()
!     call openfile(this%ingnc, iout, fname, 'GNC')
!     write (iout, '(4x,a)') &
!       'GHOST NODES WILL BE READ FROM ', trim(fname)
!     case ('MVR6')
!     if (this%is_datacopy) then
!       call this%parser%GetRemainingLine(line)
!       exit sel_opt
!     end if
!     call this%parser%GetStringCaps(subkey)
!     if (subkey /= 'FILEIN') then
!       call store_error('MVR6 keyword must be followed by '// &
!                        '"FILEIN" then by filename.')
!       call this%parser%StoreErrorUnit()
!     end if
!     call this%parser%GetString(fname)
!     if (fname == '') then
!       call store_error('No MVR6 file specified.')
!       call this%parser%StoreErrorUnit()
!     end if
!     this%inmvr = getunit()
!     call openfile(this%inmvr, iout, fname, 'MVR')
!     write (iout, '(4x,a)') &
!       'WATER MOVER INFORMATION WILL BE READ FROM ', trim(fname)
!     case ('OBS6')
!     if (this%is_datacopy) then
!       call this%parser%GetRemainingLine(line)
!       exit sel_opt
!     end if
!     call this%parser%GetStringCaps(subkey)
!     if (subkey /= 'FILEIN') then
!       call store_error('OBS8 keyword must be followed by '// &
!                        '"FILEIN" then by filename.')
!       call this%parser%StoreErrorUnit()
!     end if
!     this%obs%active = .true.
!     call this%parser%GetString(this%obs%inputFilename)
!     inobs = GetUnit()
!     call openfile(inobs, iout, this%obs%inputFilename, 'OBS')
!     this%obs%inUnitObs = inobs
!     case default
!     parsed = .false.
!     end select sel_opt
!     !
!     ! -- Return
!     return
!   end function parse_option

!   !> @ brief Read ghost nodes
!   !!
!   !! Read and process ghost nodes
!   !<
!   subroutine read_gnc(this)
!     ! -- modules
!     use SimModule, only: store_error, store_error_unit, count_errors
!     use ConstantsModule, only: LINELENGTH
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     integer(I4B) :: i, nm1, nm2, nmgnc1, nmgnc2
!     character(len=*), parameter :: fmterr = &
!                                    "('EXCHANGE NODES ', i0, ' AND ', i0,"// &
!                                    "' NOT CONSISTENT WITH GNC NODES ',  "// &
!                                    "i0, ' AND ', i0)"
!     !
!     ! -- If exchange has ghost nodes, then initialize ghost node object
!     !    This will read the ghost node blocks from the gnc input file.
!     call this%gnc%gnc_df(this%swfmodel1, m2=this%gwfmodel2)
!     !
!     ! -- Verify gnc is implicit if exchange has Newton Terms
!     if (.not. this%gnc%implicit .and. this%inewton /= 0) then
!       call store_error('GNC is explicit, but GWF exchange has active newton.')
!       call store_error('Add implicit option to GNC or remove NEWTON from '// &
!                        'GWF exchange.')
!       call store_error_unit(this%ingnc)
!     end if
!     !
!     ! -- Perform checks to ensure GNCs match with GWF-GWF nodes
!     if (this%nexg /= this%gnc%nexg) then
!       call store_error('Number of exchanges does not match number of GNCs')
!       call store_error_unit(this%ingnc)
!     end if
!     !
!     ! -- Go through each entry and confirm
!     do i = 1, this%nexg
!       if (this%nodem1(i) /= this%gnc%nodem1(i) .or. &
!           this%nodem2(i) /= this%gnc%nodem2(i)) then
!         nm1 = this%swfmodel1%dis%get_nodeuser(this%nodem1(i))
!         nm2 = this%gwfmodel2%dis%get_nodeuser(this%nodem2(i))
!         nmgnc1 = this%swfmodel1%dis%get_nodeuser(this%gnc%nodem1(i))
!         nmgnc2 = this%gwfmodel2%dis%get_nodeuser(this%gnc%nodem2(i))
!         write (errmsg, fmterr) nm1, nm2, nmgnc1, nmgnc2
!         call store_error(errmsg)
!       end if
!     end do
!     if (count_errors() > 0) then
!       call store_error_unit(this%ingnc)
!     end if
!     !
!     ! -- close the file
!     close (this%ingnc)
!     !
!     ! -- Return
!     return
!   end subroutine read_gnc

!   !> @ brief Read mover
!   !!
!   !! Read and process movers
!   !<
!   subroutine read_mvr(this, iout)
!     ! -- modules
!     use GwfMvrModule, only: mvr_cr
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     integer(I4B), intent(in) :: iout
!     !
!     ! -- Create and initialize the mover object  Here, dis is set to the one
!     !    for swfmodel1 so that a call to save flows has an associated dis
!     !    object.  Because the conversion flags for the mover are both false,
!     !    the dis object does not convert from reduced to user node numbers.
!     !    So in this case, the dis object is just writing unconverted package
!     !    numbers to the binary budget file.
!     call mvr_cr(this%mvr, this%name, this%inmvr, iout, this%swfmodel1%dis, &
!                 iexgmvr=1)
!     !
!     ! -- Return
!     return
!   end subroutine read_mvr

!   !> @ brief Rewet
!   !!
!   !! Check if rewetting should propagate from one model to another
!   !<
!   subroutine rewet(this, kiter)
!     ! -- modules
!     use TdisModule, only: kper, kstp
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     integer(I4B), intent(in) :: kiter
!     ! -- local
!     integer(I4B) :: iexg
!     integer(I4B) :: n, m
!     integer(I4B) :: ibdn, ibdm
!     integer(I4B) :: ihc
!     real(DP) :: hn, hm
!     integer(I4B) :: irewet
!     character(len=30) :: nodestrn, nodestrm
!     character(len=*), parameter :: fmtrwt = &
!       "(1x, 'CELL ',A,' REWET FROM GWF MODEL ',A,' CELL ',A, &
!        &' FOR ITER. ',I0, ' STEP ',I0, ' PERIOD ', I0)"
!     !
!     ! -- Use model 1 to rewet model 2 and vice versa
!     ! do iexg = 1, this%nexg
!     !   n = this%nodem1(iexg)
!     !   m = this%nodem2(iexg)
!     !   hn = this%swfmodel1%x(n)
!     !   hm = this%gwfmodel2%x(m)
!     !   ibdn = this%swfmodel1%ibound(n)
!     !   ibdm = this%gwfmodel2%ibound(m)
!     !   ihc = this%ihc(iexg)
!     !   call this%swfmodel1%npf%rewet_check(kiter, n, hm, ibdm, ihc, &
!     !                                       this%swfmodel1%x, irewet)
!     !   if (irewet == 1) then
!     !     call this%swfmodel1%dis%noder_to_string(n, nodestrn)
!     !     call this%gwfmodel2%dis%noder_to_string(m, nodestrm)
!     !     write (this%swfmodel1%iout, fmtrwt) trim(nodestrn), &
!     !       trim(this%gwfmodel2%name), trim(nodestrm), kiter, kstp, kper
!     !   end if
!     !   call this%gwfmodel2%npf%rewet_check(kiter, m, hn, ibdn, ihc, &
!     !                                       this%gwfmodel2%x, irewet)
!     !   if (irewet == 1) then
!     !     call this%swfmodel1%dis%noder_to_string(n, nodestrm)
!     !     call this%gwfmodel2%dis%noder_to_string(m, nodestrn)
!     !     write (this%gwfmodel2%iout, fmtrwt) trim(nodestrn), &
!     !       trim(this%swfmodel1%name), trim(nodestrm), kiter, kstp, kper
!     !   end if
!     !   !
!     ! end do
!     !
!     ! -- Return
!     return
!   end subroutine rewet

!   subroutine calc_cond_sat(this)
!     ! -- modules
!     use ConstantsModule, only: LINELENGTH, DZERO, DHALF, DONE, DPIO180
!     use GwfNpfModule, only: condmean, vcond, hcond
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     integer(I4B) :: iexg
!     integer(I4B) :: n, m, ihc
!     real(DP) :: topn, topm
!     real(DP) :: botn, botm
!     real(DP) :: satn, satm
!     real(DP) :: thickn, thickm
!     real(DP) :: angle, hyn, hym
!     real(DP) :: csat
!     real(DP) :: fawidth
!     real(DP), dimension(3) :: vg
!     !
!     do iexg = 1, this%nexg
!       !
!       ihc = this%ihc(iexg)
!       n = this%nodem1(iexg)
!       m = this%nodem2(iexg)
!       topn = this%swfmodel1%dis%top(n)
!       topm = this%gwfmodel2%dis%top(m)
!       botn = this%swfmodel1%dis%bot(n)
!       botm = this%gwfmodel2%dis%bot(m)
!       satn = DONE  ! cdl this%swfmodel1%npf%sat(n)
!       satm = this%gwfmodel2%npf%sat(m)
!       thickn = (topn - botn) * satn
!       thickm = (topm - botm) * satm
!       !
!       ! -- Calculate conductance depending on connection orientation
!       if (ihc == 0) then
!         !
!         ! -- Vertical conductance for fully saturated conditions
!         vg(1) = DZERO
!         vg(2) = DZERO
!         vg(3) = DONE
!         hyn = DONE ! cdl this%swfmodel1%npf%hy_eff(n, 0, ihc, vg=vg)
!         hym = this%gwfmodel2%npf%hy_eff(m, 0, ihc, vg=vg)
!         csat = vcond(1, 1, 1, 1, 0, 1, 1, DONE, &
!                      botn, botm, &
!                      hyn, hym, &
!                      satn, satm, &
!                      topn, topm, &
!                      botn, botm, &
!                      this%hwva(iexg))
!       else
!         !
!         ! -- Calculate horizontal conductance
!         hyn = DONE ! cdl this%swfmodel1%npf%k11(n)
!         hym = this%gwfmodel2%npf%k11(m)
!         !
!         ! -- Check for anisotropy in models, and recalculate hyn and hym
!         if (this%ianglex > 0) then
!           angle = this%auxvar(this%ianglex, iexg) * DPIO180
!           vg(1) = abs(cos(angle))
!           vg(2) = abs(sin(angle))
!           vg(3) = DZERO
!           !
!           ! -- anisotropy in model 1
!           ! if (this%swfmodel1%npf%ik22 /= 0) then
!           !   hyn = this%swfmodel1%npf%hy_eff(n, 0, ihc, vg=vg)
!           ! end if
!           !
!           ! -- anisotropy in model 2
!           if (this%gwfmodel2%npf%ik22 /= 0) then
!             hym = this%gwfmodel2%npf%hy_eff(m, 0, ihc, vg=vg)
!           end if
!         end if
!         !
!         fawidth = this%hwva(iexg)
!         csat = hcond(1, 1, 1, 1, this%inewton, 0, ihc, &
!                      this%icellavg, 0, 0, DONE, &
!                      topn, topm, satn, satm, hyn, hym, &
!                      topn, topm, &
!                      botn, botm, &
!                      this%cl1(iexg), this%cl2(iexg), &
!                      fawidth, this%satomega)
!       end if
!       !
!       ! -- store csat in condsat
!       this%condsat(iexg) = csat
!     end do
!     !
!     ! -- Return
!     return
!   end subroutine calc_cond_sat

!   !> @ brief Calculate the conductance
!   !!
!   !! Calculate the conductance based on state
!   !<
!   subroutine condcalc(this)
!     ! -- modules
!     use ConstantsModule, only: DHALF, DZERO, DONE
!     use GwfNpfModule, only: hcond, vcond
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     integer(I4B) :: iexg
!     integer(I4B) :: n, m, ihc
!     integer(I4B) :: ibdn, ibdm
!     integer(I4B) :: ictn, ictm
!     real(DP) :: topn, topm
!     real(DP) :: botn, botm
!     real(DP) :: satn, satm
!     real(DP) :: hyn, hym
!     real(DP) :: angle
!     real(DP) :: hn, hm
!     real(DP) :: cond
!     real(DP) :: fawidth
!     real(DP), dimension(3) :: vg
!     !
!     ! -- Calculate conductance and put into amat
!     do iexg = 1, this%nexg
!       ihc = this%ihc(iexg)
!       n = this%nodem1(iexg)
!       m = this%nodem2(iexg)
!       ibdn = this%swfmodel1%ibound(n)
!       ibdm = this%gwfmodel2%ibound(m)
!       ictn = 1 ! cdl this%swfmodel1%npf%icelltype(n)
!       ictm = this%gwfmodel2%npf%icelltype(m)
!       topn = this%swfmodel1%dis%top(n)
!       topm = this%gwfmodel2%dis%top(m)
!       botn = this%swfmodel1%dis%bot(n)
!       botm = this%gwfmodel2%dis%bot(m)
!       satn = DONE ! cdl this%swfmodel1%npf%sat(n)
!       satm = this%gwfmodel2%npf%sat(m)
!       hn = this%swfmodel1%x(n)
!       hm = this%gwfmodel2%x(m)
!       !
!       ! -- Calculate conductance depending on connection orientation
!       if (ihc == 0) then
!         !
!         ! -- Vertical connection
!         vg(1) = DZERO
!         vg(2) = DZERO
!         vg(3) = DONE
!         hyn = DONE ! cdl this%swfmodel1%npf%hy_eff(n, 0, ihc, vg=vg)
!         hym = this%gwfmodel2%npf%hy_eff(m, 0, ihc, vg=vg)
!         cond = vcond(ibdn, ibdm, ictn, ictm, this%inewton, this%ivarcv, &
!                      this%idewatcv, this%condsat(iexg), hn, hm, hyn, hym, &
!                      satn, satm, topn, topm, botn, botm, this%hwva(iexg))
!       else
!         ! !
!         ! ! -- Horizontal Connection
!         ! hyn = this%swfmodel1%npf%k11(n)
!         ! hym = this%gwfmodel2%npf%k11(m)
!         ! !
!         ! ! -- Check for anisotropy in models, and recalculate hyn and hym
!         ! if (this%ianglex > 0) then
!         !   angle = this%auxvar(this%ianglex, iexg)
!         !   vg(1) = abs(cos(angle))
!         !   vg(2) = abs(sin(angle))
!         !   vg(3) = DZERO
!         !   !
!         !   ! -- anisotropy in model 1
!         !   if (this%swfmodel1%npf%ik22 /= 0) then
!         !     hyn = this%swfmodel1%npf%hy_eff(n, 0, ihc, vg=vg)
!         !   end if
!         !   !
!         !   ! -- anisotropy in model 2
!         !   if (this%gwfmodel2%npf%ik22 /= 0) then
!         !     hym = this%gwfmodel2%npf%hy_eff(m, 0, ihc, vg=vg)
!         !   end if
!         ! end if
!         ! !
!         ! fawidth = this%hwva(iexg)
!         ! cond = hcond(ibdn, ibdm, ictn, ictm, this%inewton, this%inewton, &
!         !              this%ihc(iexg), this%icellavg, 0, 0, this%condsat(iexg), &
!         !              hn, hm, satn, satm, hyn, hym, topn, topm, botn, botm, &
!         !              this%cl1(iexg), this%cl2(iexg), fawidth, this%satomega)
!       end if
!       !
!       this%cond(iexg) = cond
!       !
!     end do
!     !
!     ! -- Return
!     return
!   end subroutine condcalc

!   !> @ brief Allocate scalars
!   !!
!   !! Allocate scalar variables
!   !<
!   subroutine allocate_scalars(this)
!     ! -- modules
!     use MemoryManagerModule, only: mem_allocate
!     use ConstantsModule, only: DZERO
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     !
!     call this%DisConnExchangeType%allocate_scalars()
!     !
!     call mem_allocate(this%iprflow, 'IPRFLOW', this%memoryPath)
!     call mem_allocate(this%ipakcb, 'IPAKCB', this%memoryPath)
!     this%iprpak = 0
!     this%iprflow = 0
!     this%ipakcb = 0
!     !
!     call mem_allocate(this%icellavg, 'ICELLAVG', this%memoryPath)
!     call mem_allocate(this%ivarcv, 'IVARCV', this%memoryPath)
!     call mem_allocate(this%idewatcv, 'IDEWATCV', this%memoryPath)
!     call mem_allocate(this%inewton, 'INEWTON', this%memoryPath)
!     call mem_allocate(this%ingnc, 'INGNC', this%memoryPath)
!     call mem_allocate(this%inmvr, 'INMVR', this%memoryPath)
!     call mem_allocate(this%inobs, 'INOBS', this%memoryPath)
!     call mem_allocate(this%satomega, 'SATOMEGA', this%memoryPath)
!     this%icellavg = 0
!     this%ivarcv = 0
!     this%idewatcv = 0
!     this%inewton = 0
!     this%ingnc = 0
!     this%inmvr = 0
!     this%inobs = 0
!     this%satomega = DZERO
!     !
!     ! -- Return
!     return
!   end subroutine allocate_scalars

!   !> @ brief Deallocate
!   !!
!   !! Deallocate memory associated with this object
!   !<
!   subroutine swf_gwf_da(this)
!     ! -- modules
!     use MemoryManagerModule, only: mem_deallocate
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     !
!     ! -- objects
!     if (this%ingnc > 0) then
!       call this%gnc%gnc_da()
!       deallocate (this%gnc)
!     end if
!     if (this%inmvr > 0) then
!       call this%mvr%mvr_da()
!       deallocate (this%mvr)
!     end if
!     call this%obs%obs_da()
!     deallocate (this%obs)
!     !
!     ! -- arrays
!     call mem_deallocate(this%cond)
!     call mem_deallocate(this%condsat)
!     call mem_deallocate(this%idxglo)
!     call mem_deallocate(this%idxsymglo)
!     call mem_deallocate(this%simvals)
!     !
!     ! -- output table objects
!     if (associated(this%outputtab1)) then
!       call this%outputtab1%table_da()
!       deallocate (this%outputtab1)
!       nullify (this%outputtab1)
!     end if
!     if (associated(this%outputtab2)) then
!       call this%outputtab2%table_da()
!       deallocate (this%outputtab2)
!       nullify (this%outputtab2)
!     end if
!     !
!     ! -- scalars
!     deallocate (this%filename)
!     call mem_deallocate(this%iprflow)
!     call mem_deallocate(this%ipakcb)
!     !
!     call mem_deallocate(this%icellavg)
!     call mem_deallocate(this%ivarcv)
!     call mem_deallocate(this%idewatcv)
!     call mem_deallocate(this%inewton)
!     call mem_deallocate(this%ingnc)
!     call mem_deallocate(this%inmvr)
!     call mem_deallocate(this%inobs)
!     call mem_deallocate(this%satomega)
!     !
!     ! -- deallocate base
!     call this%DisConnExchangeType%disconnex_da()
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_da

!   !> @ brief Allocate arrays
!   !!
!   !! Allocate arrays
!   !<
!   subroutine allocate_arrays(this)
!     ! -- modules
!     use MemoryManagerModule, only: mem_allocate
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     character(len=LINELENGTH) :: text
!     integer(I4B) :: ntabcol, i
!     !
!     call this%DisConnExchangeType%allocate_arrays()
!     !
!     call mem_allocate(this%cond, this%nexg, 'COND', this%memoryPath)
!     call mem_allocate(this%idxglo, this%nexg, 'IDXGLO', this%memoryPath)
!     call mem_allocate(this%idxsymglo, this%nexg, 'IDXSYMGLO', this%memoryPath) !
!     call mem_allocate(this%condsat, this%nexg, 'CONDSAT', this%memoryPath)
!     call mem_allocate(this%simvals, this%nexg, 'SIMVALS', this%memoryPath)
!     !
!     ! -- Initialize
!     do i = 1, this%nexg
!       this%cond(i) = DNODATA
!     end do
!     !
!     ! -- allocate and initialize the output table
!     if (this%iprflow /= 0) then
!       !
!       ! -- dimension table
!       ntabcol = 3
!       if (this%inamedbound > 0) then
!         ntabcol = ntabcol + 1
!       end if
!       !
!       ! -- initialize the output table objects
!       !    outouttab1
!       if (this%v_model1%is_local) then
!         call table_cr(this%outputtab1, this%name, '    ')
!         call this%outputtab1%table_df(this%nexg, ntabcol, this%swfmodel1%iout, &
!                                       transient=.TRUE.)
!         text = 'NUMBER'
!         call this%outputtab1%initialize_column(text, 10, alignment=TABCENTER)
!         text = 'CELLID'
!         call this%outputtab1%initialize_column(text, 20, alignment=TABLEFT)
!         text = 'RATE'
!         call this%outputtab1%initialize_column(text, 15, alignment=TABCENTER)
!         if (this%inamedbound > 0) then
!           text = 'NAME'
!           call this%outputtab1%initialize_column(text, 20, alignment=TABLEFT)
!         end if
!       end if
!       !    outouttab2
!       if (this%v_model2%is_local) then
!         call table_cr(this%outputtab2, this%name, '    ')
!         call this%outputtab2%table_df(this%nexg, ntabcol, this%gwfmodel2%iout, &
!                                       transient=.TRUE.)
!         text = 'NUMBER'
!         call this%outputtab2%initialize_column(text, 10, alignment=TABCENTER)
!         text = 'CELLID'
!         call this%outputtab2%initialize_column(text, 20, alignment=TABLEFT)
!         text = 'RATE'
!         call this%outputtab2%initialize_column(text, 15, alignment=TABCENTER)
!         if (this%inamedbound > 0) then
!           text = 'NAME'
!           call this%outputtab2%initialize_column(text, 20, alignment=TABLEFT)
!         end if
!       end if
!     end if
!     !
!     ! -- Return
!     return
!   end subroutine allocate_arrays

!   !> @ brief Define observations
!   !!
!   !! Define the observations associated with this object
!   !<
!   subroutine swf_gwf_df_obs(this)
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     integer(I4B) :: indx
!     !
!     ! -- Store obs type and assign procedure pointer
!     !    for gwf-gwf observation type.
!     call this%obs%StoreObsType('flow-ja-face', .true., indx)
!     this%obs%obsData(indx)%ProcessIdPtr => swf_gwf_process_obsID
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_df_obs

!   !> @ brief Read and prepare observations
!   !!
!   !! Handle observation exchanges exchange-boundary names.
!   !<
!   subroutine swf_gwf_rp_obs(this)
!     ! -- modules
!     use ConstantsModule, only: DZERO
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     integer(I4B) :: i
!     integer(I4B) :: j
!     class(ObserveType), pointer :: obsrv => null()
!     character(len=LENBOUNDNAME) :: bname
!     logical :: jfound
!     ! -- formats
! 10  format('Exchange "', a, '" for observation "', a, &
!            '" is invalid in package "', a, '"')
! 20  format('Exchange id "', i0, '" for observation "', a, &
!            '" is invalid in package "', a, '"')
!     !
!     do i = 1, this%obs%npakobs
!       obsrv => this%obs%pakobs(i)%obsrv
!       !
!       ! -- indxbnds needs to be reset each stress period because
!       !    list of boundaries can change each stress period.
!       ! -- Not true for exchanges, but leave this in for now anyway.
!       call obsrv%ResetObsIndex()
!       obsrv%BndFound = .false.
!       !
!       bname = obsrv%FeatureName
!       if (bname /= '') then
!         ! -- Observation location(s) is(are) based on a boundary name.
!         !    Iterate through all boundaries to identify and store
!         !    corresponding index(indices) in bound array.
!         jfound = .false.
!         do j = 1, this%nexg
!           if (this%boundname(j) == bname) then
!             jfound = .true.
!             obsrv%BndFound = .true.
!             obsrv%CurrentTimeStepEndValue = DZERO
!             call obsrv%AddObsIndex(j)
!           end if
!         end do
!         if (.not. jfound) then
!           write (errmsg, 10) trim(bname), trim(obsrv%ObsTypeId), trim(this%name)
!           call store_error(errmsg)
!         end if
!       else
!         ! -- Observation location is a single exchange number
!         if (obsrv%intPak1 <= this%nexg .and. obsrv%intPak1 > 0) then
!           jfound = .true.
!           obsrv%BndFound = .true.
!           obsrv%CurrentTimeStepEndValue = DZERO
!           call obsrv%AddObsIndex(obsrv%intPak1)
!         else
!           jfound = .false.
!         end if
!         if (.not. jfound) then
!           write (errmsg, 20) obsrv%intPak1, trim(obsrv%ObsTypeId), trim(this%name)
!           call store_error(errmsg)
!         end if
!       end if
!     end do
!     !
!     ! -- write summary of error messages
!     if (count_errors() > 0) then
!       call store_error_unit(this%inobs)
!     end if
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_rp_obs

!   !> @ brief Final processing
!   !!
!   !! Conduct any final processing
!   !<
!   subroutine swf_gwf_fp(this)
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_fp

!   !> @ brief Calculate flow
!   !!
!   !! Calculate the flow for the specified exchange and node numbers
!   !<
!   function qcalc(this, iexg, n1, n2)
!     ! -- return
!     real(DP) :: qcalc
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     integer(I4B), intent(in) :: iexg
!     integer(I4B), intent(in) :: n1
!     integer(I4B), intent(in) :: n2
!     ! -- local
!     !
!     ! -- Calculate flow between nodes in the two models
!     qcalc = this%cond(iexg) * (this%gwfmodel2%x(n2) - this%swfmodel1%x(n1))
!     !
!     ! -- Return
!     return
!   end function qcalc

!   !> @ brief Set symmetric flag
!   !!
!   !! Return flag indicating whether or not this exchange will cause the
!   !! coefficient matrix to be asymmetric.
!   !<
!   function swf_gwf_get_iasym(this) result(iasym)
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- local
!     integer(I4B) :: iasym
!     !
!     ! -- Start by setting iasym to zero
!     iasym = 0
!     !
!     ! -- Groundwater flow
!     if (this%inewton /= 0) iasym = 1
!     !
!     ! -- GNC
!     if (this%ingnc > 0) then
!       if (this%gnc%iasym /= 0) iasym = 1
!     end if
!     !
!     ! -- Return
!     return
!   end function swf_gwf_get_iasym

!   !> @brief Return true when this exchange provides matrix
!   !! coefficients for solving @param model
!   !<
!   function swf_gwf_connects_model(this, model) result(is_connected)
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     class(BaseModelType), pointer, intent(in) :: model !< the model to which the exchange might hold a connection
!     ! -- return
!     logical(LGP) :: is_connected !< true, when connected
!     !
!     is_connected = .false.
!     !
!     ! only connected when model is GwfModelType of course
!     select type (model)
!     class is (GwfModelType)
!       ! if (associated(this%swfmodel1, model)) then
!       !   is_connected = .true.
!       if (associated(this%gwfmodel2, model)) then
!         is_connected = .true.
!       end if
!     class is (SwfModelType)
!       if (associated(this%swfmodel1, model)) then
!         is_connected = .true.
!       end if
!     end select
!     !
!     ! -- Return
!     return
!   end function swf_gwf_connects_model

!   !> @brief Should interface model be used for this exchange
!   !<
!   function use_interface_model(this) result(use_im)
!     ! -- dummy
!     class(SwfGwfExchangeType) :: this !<  SwfGwfExchangeType
!     ! -- return
!     logical(LGP) :: use_im !< true when interface model should be used
!     !
!     use_im = this%DisConnExchangeType%use_interface_model()
!     use_im = use_im .or. (this%ixt3d > 0)
!     !
!     ! -- Return
!     return
!   end function

!   !> @ brief Save simulated flow observations
!   !!
!   !! Save the simulated flows for each exchange
!   !<
!   subroutine swf_gwf_save_simvals(this)
!     ! -- modules
!     use SimModule, only: store_error, store_error_unit
!     use SimVariablesModule, only: errmsg
!     use ConstantsModule, only: DZERO
!     use ObserveModule, only: ObserveType
!     ! -- dummy
!     class(SwfGwfExchangeType), intent(inout) :: this
!     ! -- local
!     integer(I4B) :: i
!     integer(I4B) :: j
!     integer(I4B) :: n1
!     integer(I4B) :: n2
!     integer(I4B) :: iexg
!     real(DP) :: v
!     type(ObserveType), pointer :: obsrv => null()
!     !
!     ! -- Write simulated values for all gwf-gwf observations
!     if (this%obs%npakobs > 0) then
!       call this%obs%obs_bd_clear()
!       do i = 1, this%obs%npakobs
!         obsrv => this%obs%pakobs(i)%obsrv
!         do j = 1, obsrv%indxbnds_count
!           iexg = obsrv%indxbnds(j)
!           v = DZERO
!           select case (obsrv%ObsTypeId)
!           case ('FLOW-JA-FACE')
!             n1 = this%nodem1(iexg)
!             n2 = this%nodem2(iexg)
!             v = this%simvals(iexg)
!           case default
!             errmsg = 'Unrecognized observation type: '// &
!                      trim(obsrv%ObsTypeId)
!             call store_error(errmsg)
!             call store_error_unit(this%inobs)
!           end select
!           call this%obs%SaveOneSimval(obsrv, v)
!         end do
!       end do
!     end if
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_save_simvals

!   !> @ brief Obs ID processer
!   !!
!   !! Process observations for this exchange
!   !<
!   subroutine swf_gwf_process_obsID(obsrv, dis, inunitobs, iout)
!     ! -- modules
!     use ConstantsModule, only: LINELENGTH
!     use InputOutputModule, only: urword
!     use ObserveModule, only: ObserveType
!     use BaseDisModule, only: DisBaseType
!     ! -- dummy
!     type(ObserveType), intent(inout) :: obsrv
!     class(DisBaseType), intent(in) :: dis
!     integer(I4B), intent(in) :: inunitobs
!     integer(I4B), intent(in) :: iout
!     ! -- local
!     integer(I4B) :: n, iexg, istat
!     integer(I4B) :: icol, istart, istop
!     real(DP) :: r
!     character(len=LINELENGTH) :: strng
!     !
!     strng = obsrv%IDstring
!     icol = 1
!     ! -- get exchange index
!     call urword(strng, icol, istart, istop, 1, n, r, iout, inunitobs)
!     read (strng(istart:istop), '(i10)', iostat=istat) iexg
!     if (istat == 0) then
!       obsrv%intPak1 = iexg
!     else
!       ! Integer can't be read from strng; it's presumed to be an exchange
!       ! boundary name (already converted to uppercase)
!       obsrv%FeatureName = trim(adjustl(strng))
!       ! -- Observation may require summing rates from multiple exchange
!       !    boundaries, so assign intPak1 as a value that indicates observation
!       !    is for a named exchange boundary or group of exchange boundaries.
!       obsrv%intPak1 = NAMEDBOUNDFLAG
!     end if
!     !
!     ! -- Return
!     return
!   end subroutine swf_gwf_process_obsID

!   !> @ brief Cast polymorphic object as exchange
!   !!
!   !! Cast polymorphic object as exchange
!   !<
!   function CastAsSwfGwfExchange(obj) result(res)
!     implicit none
!     ! -- dummy
!     class(*), pointer, intent(inout) :: obj
!     ! -- return
!     class(SwfGwfExchangeType), pointer :: res
!     !
!     res => null()
!     if (.not. associated(obj)) return
!     !
!     select type (obj)
!     class is (SwfGwfExchangeType)
!       res => obj
!     end select
!     !
!     ! -- Return
!     return
!   end function CastAsSwfGwfExchange

!   !> @ brief Get exchange from list
!   !!
!   !! Return an exchange from the list for specified index
!   !<
!   function GetSwfGwfExchangeFromList(list, idx) result(res)
!     implicit none
!     ! -- dummy
!     type(ListType), intent(inout) :: list
!     integer(I4B), intent(in) :: idx
!     ! -- return
!     class(SwfGwfExchangeType), pointer :: res
!     ! -- local
!     class(*), pointer :: obj
!     !
!     obj => list%GetItem(idx)
!     res => CastAsSwfGwfExchange(obj)
!     !
!     ! -- Return
!     return
!   end function GetSwfGwfExchangeFromList

! end module SwfGwfExchangeModule


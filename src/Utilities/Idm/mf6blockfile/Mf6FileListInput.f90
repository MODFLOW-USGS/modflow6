!> @brief This module contains the Mf6FileListInputModule
!!
!! This module contains the routines for reading period block
!! list based input.
!!
!<
module Mf6FileListInputModule

  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: DZERO, IZERO, LINELENGTH, LENMEMPATH, LENVARNAME, &
                             LENTIMESERIESNAME, LENAUXNAME, LENBOUNDNAME, &
                             LENCOMPONENTNAME
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, count_errors, store_error_unit
  use InputOutputModule, only: openfile, getunit
  use InputDefinitionModule, only: InputParamDefinitionType
  use MemoryManagerModule, only: mem_setptr
  use CharacterStringModule, only: CharacterStringType
  use ModflowInputModule, only: ModflowInputType, getModflowInput
  use TimeSeriesManagerModule, only: TimeSeriesManagerType, tsmanager_cr
  use StructArrayModule, only: StructArrayType, constructStructArray, &
                               destructStructArray
  use AsciiInputLoadTypeModule, only: AsciiDynamicPkgLoadBaseType
  use BoundInputContextModule, only: BoundInputContextType
  use StructVectorModule, only: StructVectorType, TSStringLocType
  use DynamicParamFilterModule, only: DynamicParamFilterType

  implicit none
  private
  public :: BoundListInputType

  !> @brief Abstract base class for ascii list loaders
  !!
  !! Abstract class with types and routines common to Ascii list
  !! based loaders.
  !!
  !<
  type, abstract, extends(AsciiDynamicPkgLoadBaseType) :: ListInputBaseType
    integer(I4B) :: ts_active
    integer(I4B) :: ibinary
    integer(I4B) :: oc_inunit
    type(TimeSeriesManagerType), pointer :: tsmanager => null()
    type(StructArrayType), pointer :: structarray => null()
    type(DynamicParamFilterType) :: filter
  contains
    procedure :: base_init
    procedure :: base_destroy
    procedure :: df
    procedure :: ad
    procedure :: reset
    procedure :: read_control_record
  end type ListInputBaseType

  !> @brief Boundary package list loader.
  !!
  !! Creates boundary input context for a package,
  !! (e.g. CHD or MAW) and updates that context in
  !! read and prepare (RP) routines.
  !!
  !<
  type, extends(ListInputBaseType) :: BoundListInputType
    integer(I4B) :: iboundname
    type(BoundInputContextType) :: bound_context
  contains
    procedure :: ainit => bndlist_init
    procedure :: rp => bndlist_rp
    procedure :: destroy => bndlist_destroy
    procedure :: ts_link_bnd => bndlist_ts_link_bnd
    procedure :: ts_link_aux => bndlist_ts_link_aux
    procedure :: ts_link => bndlist_ts_link
    procedure :: ts_update => bndlist_ts_update
    procedure :: create_structarray => bndlist_create_structarray
  end type BoundListInputType

contains

  subroutine bndlist_init(this, mf6_input, component_name, component_input_name, &
                          input_name, iperblock, parser, iout)
    use BlockParserModule, only: BlockParserType
    use LoadMf6FileModule, only: LoadMf6FileType
    class(BoundListInputType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: component_input_name
    character(len=*), intent(in) :: input_name
    integer(I4B), intent(in) :: iperblock
    type(BlockParserType), pointer, intent(inout) :: parser
    integer(I4B), intent(in) :: iout
    type(LoadMf6FileType) :: loader
    character(len=LINELENGTH) :: blockname
    integer(I4B) :: iblk
    !
    ! -- initialize scalars
    this%iboundname = 0
    !
    ! -- initialize base class
    call this%base_init(mf6_input, component_name, component_input_name, &
                        input_name, iperblock, parser, loader, iout)
    !
    ! -- initialize package input context
    call this%bound_context%init(mf6_input, this%readasarrays)
    !
    ! -- load blocks after OPTIONS and DIMENSIONS
    do iblk = 1, size(this%mf6_input%block_dfns)
      !
      ! -- log block header via loader or directly here?
      !
      ! -- set blockname
      blockname = this%mf6_input%block_dfns(iblk)%blockname
      !
      ! -- base_init loads OPTIONS and DIMENSIONS blocks if defined
      if (blockname == 'OPTIONS' .or. blockname == 'DIMENSIONS') cycle
      if (blockname == 'PERIOD') exit
      !
      ! -- load block
      call loader%load_block(iblk)
      !
      if (this%mf6_input%block_dfns(iblk)%aggregate) then
        if (this%mf6_input%block_dfns(iblk)%timeseries) then
          if (this%ts_active > 0) then
            call this%ts_update(loader%structarray)
          end if
        end if
      end if
      !
    end do
    !
    call loader%finalize()
    !
    ! -- initialize input param filter
    call this%filter%init(this%mf6_input, this%readasarrays, &
                          this%bound_context%naux, &
                          this%bound_context%inamedbound, &
                          this%iout)
    !
    ! -- store in scope SA cols for list input
    call this%filter%get_flt_params(this%param_names, this%nparam)
    !
    ! -- construct and set up the struct array object
    call this%create_structarray()
    !
    ! -- finalize input context setup
    call this%bound_context%enable()
    !
    ! -- return
    return
  end subroutine bndlist_init

  subroutine bndlist_rp(this, parser)
    ! -- modules
    use BlockParserModule, only: BlockParserType
    use StructVectorModule, only: StructVectorType
    use IdmLoggerModule, only: idm_log_header, idm_log_close
    ! -- dummy
    class(BoundListInputType), intent(inout) :: this
    type(BlockParserType), pointer, intent(inout) :: parser
    ! -- local
    logical(LGP) :: ts_active
    !
    call this%reset()
    !
    call this%read_control_record(parser)
    !
    ! -- log lst file header
    call idm_log_header(this%mf6_input%component_name, &
                        this%mf6_input%subcomponent_name, this%iout)
    !
    if (this%ibinary == 1) then
      !
      this%bound_context%nbound = &
        this%structarray%read_from_binary(this%oc_inunit, this%iout)
      !
      call parser%terminateblock()
      !
      close (this%oc_inunit)
      this%ibinary = 0
      this%oc_inunit = 0
      !
    else
      !
      ts_active = (this%ts_active /= 0)
      !
      this%bound_context%nbound = &
        this%structarray%read_from_parser(parser, ts_active, this%iout)
    end if
    !
    ! update ts links
    if (this%ts_active /= 0) then
      call this%ts_update(this%structarray)
    end if
    !
    ! -- close logging statement
    call idm_log_close(this%mf6_input%component_name, &
                       this%mf6_input%subcomponent_name, this%iout)
    !
    ! -- return
    return
  end subroutine bndlist_rp

  subroutine bndlist_destroy(this)
    ! -- modules
    class(BoundListInputType), intent(inout) :: this !< BoundListInputType
    !
    call this%base_destroy()
    call this%bound_context%destroy()
    !
    ! -- return
    return
  end subroutine bndlist_destroy

  subroutine bndlist_ts_link_bnd(this, structvector, ts_strloc)
    ! -- modules
    use TimeSeriesLinkModule, only: TimeSeriesLinkType
    use TimeSeriesManagerModule, only: read_value_or_time_series
    use StructVectorModule, only: StructVectorType, TSStringLocType
    ! -- dummy
    class(BoundListInputType), intent(inout) :: this
    type(StructVectorType), pointer, intent(in) :: structvector
    type(TSStringLocType), pointer, intent(in) :: ts_strloc
    ! -- local
    real(DP), pointer :: bndElem
    type(TimeSeriesLinkType), pointer :: tsLinkBnd
    type(StructVectorType), pointer :: sv_bound
    character(len=LENBOUNDNAME) :: boundname
    !
    nullify (tsLinkBnd)
    !
    ! -- set bound element
    bndElem => structvector%dbl1d(ts_strloc%row)
    !
    ! -- set link
    call read_value_or_time_series(ts_strloc%token, ts_strloc%row, &
                                   ts_strloc%structarray_col, bndElem, &
                                   this%mf6_input%subcomponent_name, &
                                   'BND', this%tsmanager, &
                                   this%bound_context%iprpak, tsLinkBnd)
    !
    if (associated(tsLinkBnd)) then
      !
      ! -- set variable name
      tsLinkBnd%Text = structvector%idt%mf6varname
      !
      ! -- set boundname if provided
      if (this%bound_context%inamedbound > 0) then
        sv_bound => this%structarray%get(this%iboundname)
        boundname = sv_bound%charstr1d(ts_strloc%row)
        tsLinkBnd%BndName = boundname
      end if
    end if
    !
    ! -- return
    return
  end subroutine bndlist_ts_link_bnd

  subroutine bndlist_ts_link_aux(this, structvector, ts_strloc)
    ! -- modules
    use TimeSeriesLinkModule, only: TimeSeriesLinkType
    use TimeSeriesManagerModule, only: read_value_or_time_series
    use StructVectorModule, only: StructVectorType, TSStringLocType
    ! -- dummy
    class(BoundListInputType), intent(inout) :: this
    type(StructVectorType), pointer, intent(in) :: structvector
    type(TSStringLocType), pointer, intent(in) :: ts_strloc
    ! -- local
    real(DP), pointer :: bndElem
    type(TimeSeriesLinkType), pointer :: tsLinkAux
    type(StructVectorType), pointer :: sv_bound
    character(len=LENBOUNDNAME) :: boundname
    !
    nullify (tsLinkAux)
    !
    ! -- set bound element
    bndElem => structvector%dbl2d(ts_strloc%col, ts_strloc%row)
    !
    ! -- set link
    call read_value_or_time_series(ts_strloc%token, ts_strloc%row, &
                                   ts_strloc%structarray_col, bndElem, &
                                   this%mf6_input%subcomponent_name, &
                                   'AUX', this%tsmanager, &
                                   this%bound_context%iprpak, tsLinkAux)

    if (associated(tsLinkAux)) then
      !
      ! -- set variable name
      tsLinkAux%Text = this%bound_context%auxname_cst(ts_strloc%col)
      !
      ! -- set boundname if provided
      if (this%bound_context%inamedbound > 0) then
        sv_bound => this%structarray%get(this%iboundname)
        boundname = sv_bound%charstr1d(ts_strloc%row)
        tsLinkAux%BndName = boundname
      end if
      !
    end if
    !
    ! -- return
    return
  end subroutine bndlist_ts_link_aux

  subroutine bndlist_ts_update(this, structarray)
    ! -- modules
    use StructVectorModule, only: TSStringLocType
    use StructVectorModule, only: StructVectorType
    ! -- dummy
    class(BoundListInputType), intent(inout) :: this
    type(StructArrayType), pointer, intent(inout) :: structarray
    ! -- local
    integer(I4B) :: n, m
    type(TSStringLocType), pointer :: ts_strloc
    type(StructVectorType), pointer :: sv
    !
    do m = 1, structarray%count()

      sv => structarray%get(m)

      if (sv%idt%timeseries) then
        !
        do n = 1, sv%ts_strlocs%count()
          ts_strloc => sv%get_ts_strloc(n)
          call this%ts_link(sv, ts_strloc)
        end do
        !
        call sv%clear()
      end if
    end do
    !
    ! -- return
    return
  end subroutine bndlist_ts_update

  subroutine bndlist_ts_link(this, structvector, ts_strloc)
    ! -- modules
    use StructVectorModule, only: StructVectorType, TSStringLocType
    ! -- dummy
    class(BoundListInputType), intent(inout) :: this
    type(StructVectorType), pointer, intent(in) :: structvector
    type(TSStringLocType), pointer, intent(in) :: ts_strloc
    ! -- local
    !
    select case (structvector%memtype)
    case (2) ! -- dbl1d
      !
      call this%ts_link_bnd(structvector, ts_strloc)
      !
    case (6) ! -- dbl2d
      !
      call this%ts_link_aux(structvector, ts_strloc)
      !
    case default
    end select
    !
    ! -- return
    return
  end subroutine bndlist_ts_link

  subroutine bndlist_create_structarray(this)
    ! -- modules
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    ! -- dummy
    class(BoundListInputType), intent(inout) :: this
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: icol
    !
    ! -- construct and set up the struct array object
    this%structarray => constructStructArray(this%mf6_input, this%nparam, &
                                             this%bound_context%maxbound, 0, &
                                             this%mf6_input%mempath, &
                                             this%mf6_input%component_mempath)
    !
    ! -- set up struct array
    do icol = 1, this%nparam
      !
      idt => get_param_definition_type(this%mf6_input%param_dfns, &
                                       this%mf6_input%component_type, &
                                       this%mf6_input%subcomponent_type, &
                                       'PERIOD', &
                                       this%param_names(icol), this%input_name)
      !
      ! -- allocate variable in memory manager
      call this%structarray%mem_create_vector(icol, idt)
      !
      ! -- store boundname index when found
      if (idt%mf6varname == 'BOUNDNAME') this%iboundname = icol
      !
    end do
    !
    ! -- return
    return
  end subroutine bndlist_create_structarray

  subroutine base_init(this, mf6_input, component_name, component_input_name, &
                       input_name, iperblock, parser, loader, iout)
    use ConstantsModule, only: LENCOMPONENTNAME
    use BlockParserModule, only: BlockParserType
    use LoadMf6FileModule, only: LoadMf6FileType
    use MemoryManagerModule, only: get_isize
    use IdmLoggerModule, only: idm_log_header
    class(ListInputBaseType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: component_input_name
    character(len=*), intent(in) :: input_name
    integer(I4B), intent(in) :: iperblock
    type(BlockParserType), intent(inout) :: parser
    type(LoadMf6FileType), intent(inout) :: loader
    integer(I4B), intent(in) :: iout
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: ts_fnames
    character(len=LINELENGTH) :: fname
    integer(I4B) :: ts6_size, n
    character(len=LINELENGTH) :: blockname
    integer(I4B) :: iblk
    !
    ! -- init loader
    call this%DynamicPkgLoadType%init(mf6_input, component_name, &
                                      component_input_name, input_name, &
                                      iperblock, iout)
    !
    ! -- initialize
    this%ts_active = 0
    this%ibinary = 0
    this%oc_inunit = 0
    !
    ! -- initialize static loader
    call loader%init(parser, mf6_input, this%input_name, iout)
    !
    ! -- load OPTIONS and DIMENSIONS blocks
    do iblk = 1, size(this%mf6_input%block_dfns)
      !
      ! -- set blockname
      blockname = this%mf6_input%block_dfns(iblk)%blockname
      !
      ! -- step 1 loads OPTIONS and DIMENSIONS blocks if defined
      if (blockname /= 'OPTIONS' .and. blockname /= 'DIMENSIONS') exit
      !
      ! -- load block
      call loader%load_block(iblk)
      !
    end do
    !
    ! -- create tsmanager
    allocate (this%tsmanager)
    call tsmanager_cr(this%tsmanager, iout)
    !
    ! -- determine if TS6 files were provided in OPTIONS block
    call get_isize('TS6_FILENAME', this%mf6_input%mempath, ts6_size)
    !
    if (ts6_size > 0) then
      !
      this%ts_active = 1
      call mem_setptr(ts_fnames, 'TS6_FILENAME', this%mf6_input%mempath)
      !
      do n = 1, size(ts_fnames)
        fname = ts_fnames(n)
        call this%tsmanager%add_tsfile(fname, GetUnit())
      end do
      !
    end if
    !
    ! -- define TS manager
    call this%tsmanager%tsmanager_df()
    !
    ! -- return
    return
  end subroutine base_init

  subroutine base_destroy(this)
    ! -- modules
    class(ListInputBaseType), intent(inout) :: this !< ListInputType
    !
    deallocate (this%tsmanager)
    !
    ! -- deallocate StructArray
    call destructStructArray(this%structarray)
    !
    ! -- return
    return
  end subroutine base_destroy

  subroutine df(this)
    ! -- modules
    ! -- dummy
    class(ListInputBaseType), intent(inout) :: this !< ListInputType
    !
    ! -- define tsmanager
    !call this%tsmanager%tsmanager_df()
    !
    ! -- return
    return
  end subroutine df

  subroutine ad(this)
    ! -- modules
    class(ListInputBaseType), intent(inout) :: this !< ListInputType
    !
    ! -- advance timeseries
    call this%tsmanager%ad()
    !
    ! -- return
    return
  end subroutine ad

  subroutine reset(this)
    ! -- modules
    class(ListInputBaseType), intent(inout) :: this !< ListInputType
    !
    ! -- reset tsmanager
    call this%tsmanager%reset(this%mf6_input%subcomponent_name)
    !
    ! -- return
    return
  end subroutine reset

  subroutine read_control_record(this, parser)
    ! -- modules
    use InputOutputModule, only: urword
    use OpenSpecModule, only: form, access
    use ConstantsModule, only: LINELENGTH
    use BlockParserModule, only: BlockParserType
    ! -- dummy
    class(ListInputBaseType), intent(inout) :: this
    type(BlockParserType), intent(inout) :: parser
    ! -- local
    integer(I4B) :: lloc, istart, istop, idum, inunit, itmp, ierr
    integer(I4B) :: nunopn = 99
    character(len=:), allocatable :: line
    character(len=LINELENGTH) :: fname
    logical :: exists
    real(DP) :: r
    ! -- formats
    character(len=*), parameter :: fmtocne = &
      &"('Specified OPEN/CLOSE file ',(A),' does not exist')"
    character(len=*), parameter :: fmtobf = &
      &"(1X,/1X,'OPENING BINARY FILE ON UNIT ',I0,':',/1X,A)"
    !
    inunit = parser%getunit()
    !
    ! -- Read to the first non-commented line
    lloc = 1
    call parser%line_reader%rdcom(inunit, this%iout, line, ierr)
    call urword(line, lloc, istart, istop, 1, idum, r, this%iout, inunit)
    !
    if (line(istart:istop) == 'OPEN/CLOSE') then
      !
      ! -- get filename
      call urword(line, lloc, istart, istop, 0, idum, r, &
                  this%iout, inunit)
      !
      fname = line(istart:istop)
      !
      ! -- check to see if file OPEN/CLOSE file exists
      inquire (file=fname, exist=exists)
      !
      if (.not. exists) then
        write (errmsg, fmtocne) line(istart:istop)
        call store_error(errmsg)
        call store_error('Specified OPEN/CLOSE file does not exist')
        call store_error_unit(inunit)
      end if
      !
      ! -- Check for (BINARY) keyword
      call urword(line, lloc, istart, istop, 1, idum, r, &
                  this%iout, inunit)
      !
      if (line(istart:istop) == '(BINARY)') this%ibinary = 1
      !
      ! -- Open the file depending on ibinary flag
      if (this%ibinary == 1) then
        this%oc_inunit = nunopn
        itmp = this%iout
        !
        if (this%iout > 0) then
          itmp = 0
          write (this%iout, fmtobf) this%oc_inunit, trim(adjustl(fname))
        end if
        !
        call openfile(this%oc_inunit, itmp, fname, 'OPEN/CLOSE', &
                      fmtarg_opt=form, accarg_opt=access)
      end if
    end if
    !
    if (this%ibinary == 0) then
      call parser%line_reader%bkspc(parser%getunit())
    end if
    !
    ! -- return
    return
  end subroutine read_control_record

end module Mf6FileListInputModule

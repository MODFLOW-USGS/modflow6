!> @brief This module contains the StressListInputModule
!!
!! This module contains the routines for reading period block
!! list based input.
!!
!<
module StressListInputModule

  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: DZERO, IZERO, LINELENGTH, LENMEMPATH, LENVARNAME, &
                             LENTIMESERIESNAME, LENAUXNAME, LENBOUNDNAME
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, count_errors, store_error_unit
  use InputOutputModule, only: openfile, getunit
  use InputDefinitionModule, only: InputParamDefinitionType
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_setptr
  use CharacterStringModule, only: CharacterStringType
  use ModflowInputModule, only: ModflowInputType, getModflowInput
  use TimeSeriesManagerModule, only: TimeSeriesManagerType, tsmanager_cr
  use BoundInputContextModule, only: BoundInputContextType
  use StructArrayModule, only: StructArrayType, constructStructArray, &
                               destructStructArray
  use AsciiInputLoadTypeModule, only: AsciiDynamicPkgLoadBaseType

  implicit none
  private
  public :: StressListInputType

  !> @brief Ascii list based dynamic loader type
  !<
  type, extends(AsciiDynamicPkgLoadBaseType) :: StressListInputType
    integer(I4B) :: ts_active
    integer(I4B) :: ibinary
    integer(I4B) :: oc_inunit
    integer(I4B) :: ncol
    integer(I4B) :: iboundname
    character(len=LENVARNAME), dimension(:), allocatable :: cols
    type(TimeSeriesManagerType), pointer :: tsmanager => null()
    type(StructArrayType), pointer :: structarray
    type(BoundInputContextType) :: bndctx
  contains
    procedure :: init => inlist_init
    procedure :: df => inlist_df
    procedure :: ad => inlist_ad
    procedure :: rp => inlist_rp
    procedure :: destroy => inlist_destroy
    procedure :: reset => inlist_reset
    procedure :: ts_link => inlist_ts_link
    procedure :: ts_update => inlist_ts_update
    procedure :: create_structarray
    procedure :: read_control_record
  end type StressListInputType

contains

  subroutine inlist_init(this, mf6_input, modelname, modelfname, &
                         source, iperblock, iout)
    use MemoryManagerModule, only: get_isize
    class(StressListInputType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: source
    integer(I4B), intent(in) :: iperblock
    integer(I4B), intent(in) :: iout
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: ts_fnames
    character(len=LINELENGTH) :: fname
    integer(I4B) :: ts6_size, n
    !
    call this%DynamicPkgLoadType%init(mf6_input, modelname, modelfname, &
                                      source, iperblock, iout)
    !
    ! -- initialize
    this%ts_active = 0
    this%ibinary = 0
    this%oc_inunit = 0
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
    ! -- initialize package input context
    call this%bndctx%init(mf6_input, .false.)
    !
    ! -- set SA cols in scope for list input
    call this%bndctx%filtered_cols(this%cols, this%ncol)
    !
    ! -- construct and set up the struct array object
    call this%create_structarray()
    !
    ! -- finalize input context setup
    call this%bndctx%enable()
    !
    ! -- return
    return
  end subroutine inlist_init

  subroutine inlist_df(this)
    ! -- modules
    class(StressListInputType), intent(inout) :: this !< StressListInputType
    !
    ! -- define tsmanager
    call this%tsmanager%tsmanager_df()
    !
    ! -- return
    return
  end subroutine inlist_df

  subroutine inlist_ad(this)
    ! -- modules
    class(StressListInputType), intent(inout) :: this !< StressListInputType
    !
    ! -- advance tsmanager
    call this%tsmanager%ad()
    !
    ! -- return
    return
  end subroutine inlist_ad

  subroutine inlist_rp(this, parser)
    ! -- modules
    use BlockParserModule, only: BlockParserType
    use StructVectorModule, only: StructVectorType
    use IdmLoggerModule, only: idm_log_header, idm_log_close
    ! -- dummy
    class(StressListInputType), intent(inout) :: this
    type(BlockParserType), pointer, intent(inout) :: parser
    ! -- locals
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
      this%bndctx%nbound = &
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
      this%bndctx%nbound = &
        this%structarray%read_from_parser(parser, &
                                          ts_active, this%iout)
    end if
    !
    ! update ts links
    if (this%ts_active /= 0) then
      call this%ts_update()
    end if
    !
    ! -- close logging statement
    call idm_log_close(this%mf6_input%component_name, &
                       this%mf6_input%subcomponent_name, this%iout)
    !
    ! -- return
    return
  end subroutine inlist_rp

  subroutine inlist_destroy(this)
    ! -- modules
    class(StressListInputType), intent(inout) :: this !< StressListInputType
    !
    deallocate (this%cols)
    deallocate (this%tsmanager)
    call destructStructArray(this%structarray)
    call this%bndctx%destroy()
    !
    ! -- return
    return
  end subroutine inlist_destroy

  subroutine inlist_reset(this)
    ! -- modules
    class(StressListInputType), intent(inout) :: this !< StressListInputType
    !
    ! -- reset tsmanager
    call this%tsmanager%reset(this%mf6_input%subcomponent_name)
    !
    ! -- return
    return
  end subroutine inlist_reset

  subroutine inlist_ts_link(this, structvector, ts_strloc)
    ! -- modules
    use TimeSeriesLinkModule, only: TimeSeriesLinkType
    use TimeSeriesManagerModule, only: read_value_or_time_series
    use StructVectorModule, only: StructVectorType, TSStringLocType
    !use ArrayHandlersModule, only: ifind
    ! -- dummy
    class(StressListInputType), intent(inout) :: this
    type(StructVectorType), pointer, intent(in) :: structvector
    type(TSStringLocType), pointer, intent(in) :: ts_strloc
    ! -- locals
    real(DP), pointer :: bndElem => null()
    type(TimeSeriesLinkType), pointer :: tsLinkBnd => null()
    type(TimeSeriesLinkType), pointer :: tsLinkAux => null()
    type(StructVectorType), pointer :: sv_bound
    character(len=LENBOUNDNAME) :: boundname
    !
    select case (structvector%memtype)
    case (2)
      !
      tsLinkBnd => NULL()
      !
      ! -- set bound element
      bndElem => structvector%dbl1d(ts_strloc%row)
      !
      ! -- set link
      call read_value_or_time_series(ts_strloc%token, ts_strloc%row, &
                                     ts_strloc%structarray_col, bndElem, &
                                     this%mf6_input%subcomponent_name, &
                                     'BND', this%tsmanager, &
                                     this%bndctx%iprpak, tsLinkBnd)
      !
      if (associated(tsLinkBnd)) then
        !
        ! -- set variable name
        tsLinkBnd%Text = structvector%idt%mf6varname
        !
        ! -- set boundname if provided
        if (this%bndctx%inamedbound > 0) then
          sv_bound => this%structarray%get(this%iboundname)
          boundname = sv_bound%charstr1d(ts_strloc%row)
          tsLinkBnd%BndName = boundname
        end if

        ! Flux is handled from model context

      end if
      !
    case (6)
      !
      tsLinkAux => NULL()
      !
      ! -- set bound element
      bndElem => structvector%dbl2d(ts_strloc%col, ts_strloc%row)
      !
      ! -- set link
      call read_value_or_time_series(ts_strloc%token, ts_strloc%row, &
                                     ts_strloc%structarray_col, bndElem, &
                                     this%mf6_input%subcomponent_name, &
                                     'AUX', this%tsmanager, &
                                     this%bndctx%iprpak, tsLinkAux)

      if (associated(tsLinkAux)) then
        !
        ! -- set variable name
        tsLinkAux%Text = this%bndctx%auxname_cst(ts_strloc%col)
        !
        ! -- set boundname if provided
        if (this%bndctx%inamedbound > 0) then
          sv_bound => this%structarray%get(this%iboundname)
          boundname = sv_bound%charstr1d(ts_strloc%row)
          tsLinkAux%BndName = boundname
        end if
        !
      end if
      !
    case default
    end select
    !
    ! -- return
    return
  end subroutine inlist_ts_link

  subroutine inlist_ts_update(this)
    ! -- modules
    use StructVectorModule, only: TSStringLocType
    use StructVectorModule, only: StructVectorType
    ! -- dummy
    class(StressListInputType), intent(inout) :: this
    ! -- locals
    integer(I4B) :: n, m
    type(TSStringLocType), pointer :: ts_strloc
    type(StructVectorType), pointer :: sv
    !
    !
    do m = 1, this%structarray%count()

      sv => this%structarray%get(m)

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
  end subroutine inlist_ts_update

  subroutine create_structarray(this)
    ! -- modules
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    ! -- dummy
    class(StressListInputType), intent(inout) :: this
    ! -- locals
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: icol
    !
    ! -- construct and set up the struct array object
    this%structarray => constructStructArray(this%mf6_input, this%ncol, &
                                             this%bndctx%maxbound, 0, &
                                             this%mf6_input%mempath, &
                                             this%mf6_input%component_mempath)
    !
    ! -- set up struct array
    do icol = 1, this%ncol
      !
      idt => get_param_definition_type(this%mf6_input%param_dfns, &
                                       this%mf6_input%component_type, &
                                       this%mf6_input%subcomponent_type, &
                                       'PERIOD', &
                                       this%cols(icol), this%sourcename)
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
  end subroutine create_structarray

  subroutine read_control_record(this, parser)
    ! -- modules
    use InputOutputModule, only: urword
    use OpenSpecModule, only: form, access
    use ConstantsModule, only: LINELENGTH
    use BlockParserModule, only: BlockParserType
    ! -- dummy
    class(StressListInputType), intent(inout) :: this
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

end module StressListInputModule

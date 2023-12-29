!> @brief This module contains the StressGridInputModule
!!
!! This module contains the routines for reading period block
!! array based input.
!!
!<
module StressGridInputModule

  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: DZERO, IZERO, LINELENGTH, LENMEMPATH, LENVARNAME, &
                             LENTIMESERIESNAME, LENAUXNAME
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename
  use InputDefinitionModule, only: InputParamDefinitionType
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_setptr
  use CharacterStringModule, only: CharacterStringType
  use BlockParserModule, only: BlockParserType
  use ModflowInputModule, only: ModflowInputType, getModflowInput
  use BoundInputContextModule, only: BoundInputContextType
  use TimeArraySeriesManagerModule, only: TimeArraySeriesManagerType, &
                                          tasmanager_cr
  use AsciiInputLoadTypeModule, only: AsciiDynamicPkgLoadBaseType

  implicit none
  private
  public :: StressGridInputType

  !> @brief Pointer type for read state variable
  !<
  type ReadStateVar
    integer, pointer :: invar
  end type ReadStateVar

  !> @brief Ascii grid based dynamic loader type
  !<
  type, extends(AsciiDynamicPkgLoadBaseType) :: StressGridInputType
    integer(I4B) :: tas_active !< Are TAS6 inputs defined
    integer(I4B) :: nparam !< number of dynamic parameters other than AUX
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: aux_tasnames => null() !< array of AUXVAR TAS names
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: param_tasnames => null() !< array of dynamic param TAS names
    character(len=LENVARNAME), dimension(:), &
      allocatable :: param_names !< dynamic param names
    type(ReadStateVar), dimension(:), allocatable :: param_reads !< read states for current load
    integer(I4B), dimension(:), allocatable :: idt_idxs !< idt indexes corresponding to dfn param list
    type(TimeArraySeriesManagerType), pointer :: tasmanager => null() !< TAS manager object
    type(BoundInputContextType) :: bndctx !< boundary package input context
  contains
    procedure :: init => ingrid_init
    procedure :: df => ingrid_df
    procedure :: ad => ingrid_ad
    procedure :: rp => ingrid_rp
    procedure :: destroy => ingrid_destroy
    procedure :: reset => ingrid_reset
    procedure :: params_alloc => ingrid_params_alloc
    procedure :: param_load => ingrid_param_load
    procedure :: tas_arrays_alloc => ingrid_tas_arrays_alloc
    procedure :: tas_links_create => ingrid_tas_links_create
  end type StressGridInputType

contains

  subroutine ingrid_init(this, mf6_input, modelname, modelfname, &
                         source, iperblock, iout)
    use MemoryManagerModule, only: get_isize
    class(StressGridInputType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: source
    integer(I4B), intent(in) :: iperblock
    integer(I4B), intent(in) :: iout
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: tas_fnames
    character(len=LINELENGTH) :: fname
    integer(I4B) :: tas6_size, n
    !
    call this%DynamicPkgLoadType%init(mf6_input, modelname, modelfname, &
                                      source, iperblock, iout)
    ! -- initialize
    this%tas_active = 0
    this%nparam = 0
    this%iout = iout
    !
    ! -- create tasmanager
    allocate (this%tasmanager)
    call tasmanager_cr(this%tasmanager, modelname=this%mf6_input%component_name, &
                       iout=this%iout)
    !
    ! -- determine if TAS6 files were provided in OPTIONS block
    call get_isize('TAS6_FILENAME', this%mf6_input%mempath, tas6_size)
    !
    if (tas6_size > 0) then
      !
      this%tas_active = 1
      !
      call mem_setptr(tas_fnames, 'TAS6_FILENAME', this%mf6_input%mempath)
      !
      ! -- add files to tasmanager
      do n = 1, size(tas_fnames)
        fname = tas_fnames(n)
        call this%tasmanager%add_tasfile(fname)
      end do
      !
    end if
    !
    ! -- initialize input context memory
    call this%bndctx%init(mf6_input, .true.)
    !
    ! -- allocate dfn params
    call this%params_alloc()
    !
    ! -- allocate memory for storing TAS strings
    call this%tas_arrays_alloc()
    !
    ! -- return
    return
  end subroutine ingrid_init

  subroutine ingrid_df(this)
    ! -- modules
    class(StressGridInputType), intent(inout) :: this !< Mf6FileGridInputType
    !
    call this%tasmanager%tasmanager_df()
    !
    ! -- return
    return
  end subroutine ingrid_df

  subroutine ingrid_ad(this)
    ! -- modules
    class(StressGridInputType), intent(inout) :: this !< Mf6FileGridInputType
    !
    call this%tasmanager%ad()
    !
    ! -- return
    return
  end subroutine ingrid_ad

  subroutine ingrid_rp(this, parser)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    use BlockParserModule, only: BlockParserType
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    use ArrayHandlersModule, only: ifind
    use SourceCommonModule, only: ifind_charstr
    use IdmLoggerModule, only: idm_log_header, idm_log_close, idm_log_var
    class(StressGridInputType), intent(inout) :: this !< Mf6FileGridInputType
    type(BlockParserType), pointer, intent(inout) :: parser
    ! -- locals
    logical(LGP) :: endOfBlock
    character(len=LINELENGTH) :: keyword, param_tag
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iaux, iparam
    character(len=LENTIMESERIESNAME) :: tas_name
    !
    ! -- reset for this period
    call this%reset()
    !
    ! -- log lst file header
    call idm_log_header(this%mf6_input%component_name, &
                        this%mf6_input%subcomponent_name, this%iout)
    !
    ! -- read array block
    do
      ! -- initialize
      iaux = 0
      !
      ! -- read next line
      call parser%GetNextLine(endOfBlock)
      if (endOfBlock) exit
      !
      ! -- read param_tag
      call parser%GetStringCaps(param_tag)
      !
      ! -- is param tag an auxvar?
      iaux = ifind_charstr(this%bndctx%auxname_cst, param_tag)
      !
      ! -- any auvxar corresponds to the definition tag 'AUX'
      if (iaux > 0) param_tag = 'AUX'
      !
      ! -- set input definition
      idt => get_param_definition_type(this%mf6_input%param_dfns, &
                                       this%mf6_input%component_type, &
                                       this%mf6_input%subcomponent_type, &
                                       'PERIOD', param_tag, this%sourcename)
      !
      ! -- look for TAS keyword if tas is active
      if (this%tas_active /= 0) then
        call parser%GetStringCaps(keyword)
        !
        if (keyword == 'TIMEARRAYSERIES') then
          call parser%GetStringCaps(tas_name)
          !
          if (param_tag == 'AUX') then
            this%aux_tasnames(iaux) = tas_name
          else
            iparam = ifind(this%param_names, param_tag)
            this%param_tasnames(iparam) = tas_name
            this%param_reads(iparam)%invar = 2
          end if
          !
          ! -- log variable
          call idm_log_var(param_tag, this%mf6_input%mempath, this%iout, .true.)
          !
          ! -- cycle to next input param
          cycle
        end if
        !
      end if
      !
      ! -- read and load the parameter
      call this%param_load(parser, idt%datatype, idt%mf6varname, idt%tagname, &
                           this%mf6_input%mempath, iaux)
      !
    end do
    !
    !
    if (this%tas_active /= 0) then
      call this%tas_links_create(parser%iuactive)
    end if
    !
    ! -- log lst file header
    call idm_log_close(this%mf6_input%component_name, &
                       this%mf6_input%subcomponent_name, this%iout)
    !
    ! -- return
    return
  end subroutine ingrid_rp

  subroutine ingrid_destroy(this)
    ! -- modules
    class(StressGridInputType), intent(inout) :: this !< Mf6FileGridInputType
    !
    deallocate (this%tasmanager)
    !
    ! -- return
    return
  end subroutine ingrid_destroy

  subroutine ingrid_reset(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate, mem_setptr, get_isize
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    class(StressGridInputType), intent(inout) :: this !< StressGridInputType
    integer(I4B) :: n, m
    !
    if (this%tas_active /= 0) then
      !
      ! -- reset tasmanager
      call this%tasmanager%reset(this%mf6_input%subcomponent_name)
      !
      ! -- reinitialize tas name arrays
      call this%bndctx%param_init('CHARSTR1D', 'AUXTASNAME', &
                                  this%mf6_input%mempath, this%sourcename)
      call this%bndctx%param_init('CHARSTR1D', 'PARAMTASNAME', &
                                  this%mf6_input%mempath, this%sourcename)
    end if
    !
    do n = 1, this%nparam
      if (this%param_reads(n)%invar /= 0) then
        !
        ! -- reset read state
        this%param_reads(n)%invar = 0
        !
      end if
    end do
    !
    ! -- explicitly reset auxvar array each period
    do m = 1, this%bndctx%ncpl
      do n = 1, this%bndctx%naux
        this%bndctx%auxvar(n, m) = DZERO
      end do
    end do
    !
    ! -- return
    return
  end subroutine ingrid_reset

  subroutine ingrid_params_alloc(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    use ArrayHandlersModule, only: expandarray
    ! -- dummy
    class(StressGridInputType), intent(inout) :: this !< StressGridInputType
    type(InputParamDefinitionType), pointer :: idt
    character(len=LENVARNAME), dimension(:), allocatable :: read_state_varnames
    integer(I4B), pointer :: intvar
    integer(I4B) :: iparam
    !
    ! -- allocate period dfn params
    call this%bndctx%bound_params_allocate(this%sourcename)
    !
    ! -- allocate dfn input params
    do iparam = 1, size(this%mf6_input%param_dfns)
      !
      ! -- assign param definition pointer
      idt => this%mf6_input%param_dfns(iparam)
      !
      if (idt%blockname == 'PERIOD') then
        !
        ! -- store parameter info
        if (idt%tagname /= 'AUX') then
          this%nparam = this%nparam + 1
          !
          ! -- reallocate param info arrays
          call expandarray(this%param_names)
          call expandarray(this%idt_idxs)
          call expandarray(read_state_varnames)
          !
          ! -- internal mf6 param name
          this%param_names(this%nparam) = idt%mf6varname
          ! -- idt list index of param
          this%idt_idxs(this%nparam) = iparam
          ! -- allocate and store name of read state variable
          read_state_varnames(this%nparam) = &
            this%bndctx%allocate_read_state_var(idt%mf6varname)
          !
        end if
        !
      end if
    end do
    !
    ! -- allocate and set param_reads pointer array
    allocate (this%param_reads(this%nparam))
    !
    ! store read state variable pointers
    do iparam = 1, this%nparam
      call mem_setptr(intvar, read_state_varnames(iparam), this%mf6_input%mempath)
      this%param_reads(iparam)%invar => intvar
    end do
    !
    ! -- cleanup
    deallocate (read_state_varnames)
    !
    ! -- return
    return
  end subroutine ingrid_params_alloc

  subroutine ingrid_param_load(this, parser, datatype, varname, &
                               tagname, mempath, iaux)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    use ArrayHandlersModule, only: ifind
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    use Double1dReaderModule, only: read_dbl1d
    use Double2dReaderModule, only: read_dbl2d
    use Integer1dReaderModule, only: read_int1d
    use IdmLoggerModule, only: idm_log_var
    ! -- dummy
    class(StressGridInputType), intent(inout) :: this !< StressGridInputType
    type(BlockParserType), intent(in) :: parser
    character(len=*), intent(in) :: datatype
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: mempath
    integer(I4B), intent(in) :: iaux
    ! -- locals
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    integer(I4B) :: iparam
    !
    select case (datatype)
    case ('INTEGER1D')
      !
      call mem_setptr(int1d, varname, mempath)
      call read_int1d(parser, int1d, varname)
      call idm_log_var(int1d, tagname, mempath, this%iout)
      !
    case ('DOUBLE1D')
      !
      call mem_setptr(dbl1d, varname, mempath)
      call read_dbl1d(parser, dbl1d, varname)
      call idm_log_var(dbl1d, tagname, mempath, this%iout)
      !
    case ('DOUBLE2D')
      !
      call mem_setptr(dbl2d, varname, mempath)
      call read_dbl1d(parser, dbl2d(iaux, :), varname)
      call idm_log_var(dbl2d, tagname, mempath, this%iout)
      !
    case default
      !
      call store_error('Programming error. (IDM) unsupported memload &
                       &data type for param='//trim(tagname))
      call store_error_filename(this%sourcename)
      !
    end select
    !
    iparam = ifind(this%param_names, varname)
    !
    ! -- if param is tracked set read state
    if (iparam > 0) then
      this%param_reads(iparam)%invar = 1
    end if
    !
    ! -- return
    return
  end subroutine ingrid_param_load

  subroutine ingrid_tas_arrays_alloc(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    class(StressGridInputType), intent(inout) :: this !< StressGridInputType
    !
    ! -- count params other than AUX
    if (this%tas_active /= 0) then
      !
      call mem_allocate(this%aux_tasnames, LENTIMESERIESNAME, this%bndctx%naux, &
                        'AUXTASNAME', this%mf6_input%mempath)
      call mem_allocate(this%param_tasnames, LENTIMESERIESNAME, this%nparam, &
                        'PARAMTASNAME', this%mf6_input%mempath)
      !
      call this%bndctx%param_init('CHARSTR1D', 'AUXTASNAME', &
                                  this%mf6_input%mempath, &
                                  this%sourcename)
      call this%bndctx%param_init('CHARSTR1D', 'PARAMTASNAME', &
                                  this%mf6_input%mempath, &
                                  this%sourcename)
      !
    else
      !
      call mem_allocate(this%aux_tasnames, LENTIMESERIESNAME, 0, &
                        'AUXTASNAME', this%mf6_input%mempath)
      call mem_allocate(this%param_tasnames, LENTIMESERIESNAME, 0, &
                        'PARAMTASNAME', this%mf6_input%mempath)
      !
    end if
    !
    ! -- return
    return
  end subroutine ingrid_tas_arrays_alloc

  ! FLUX and SFAC are handled in model context
  subroutine ingrid_tas_links_create(this, inunit)
    ! -- modules
    use InputDefinitionModule, only: InputParamDefinitionType
    ! -- dummy
    class(StressGridInputType), intent(inout) :: this !< StressGridInputType
    integer(I4B), intent(in) :: inunit
    ! -- locals
    type(InputParamDefinitionType), pointer :: idt
    ! -- non-contiguous beacuse a slice of bound is passed
    real(DP), dimension(:), pointer :: auxArrayPtr, bndArrayPtr
    real(DP), dimension(:), pointer, contiguous :: bound
    integer(I4B), dimension(:), pointer, contiguous :: nodelist
    character(len=LENTIMESERIESNAME) :: tas_name
    character(len=LENAUXNAME) :: aux_name
    logical :: convertFlux
    integer(I4B) :: n
    !
    ! -- initialize
    nullify (auxArrayPtr)
    nullify (bndArrayPtr)
    nullify (nodelist)
    convertflux = .false.
    !
    ! Create AUX Time Array Series links
    do n = 1, this%bndctx%naux
      tas_name = this%aux_tasnames(n)
      !
      if (tas_name /= '') then
        !
        ! -- set auxvar pointer
        auxArrayPtr => this%bndctx%auxvar(n, :)
        !
        aux_name = this%bndctx%auxname_cst(n)
        !
        call this%tasmanager%MakeTasLink(this%mf6_input%subcomponent_name, &
                                         auxArrayPtr, this%bndctx%iprpak, &
                                         tas_name, aux_name, convertFlux, &
                                         nodelist, inunit)
      end if
      !
    end do
    !
    ! Create BND Time Array Series links
    do n = 1, this%nparam
      !
      ! -- assign param definition pointer
      idt => this%mf6_input%param_dfns(this%idt_idxs(n))
      !
      if (idt%timeseries) then
        !
        if (this%param_reads(n)%invar == 2) then
          tas_name = this%param_tasnames(n)
          !
          call mem_setptr(bound, idt%mf6varname, this%mf6_input%mempath)
          !
          ! -- set bound pointer
          bndArrayPtr => bound(:)
          !
          call this%tasmanager%MakeTasLink(this%mf6_input%subcomponent_name, &
                                           bndArrayPtr, this%bndctx%iprpak, &
                                           tas_name, idt%mf6varname, &
                                           convertFlux, nodelist, inunit)
        end if
      end if
    end do

    !
    ! -- return
    return
  end subroutine ingrid_tas_links_create

end module StressGridInputModule

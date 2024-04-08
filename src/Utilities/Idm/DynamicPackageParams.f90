!> @brief This module contains the DynamicPackageParamsModule
!!
!!
!<
module DynamicPackageParamsModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, DZERO, IZERO
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename
  use MemoryManagerModule, only: mem_allocate
  use ModflowInputModule, only: ModflowInputType
  use InputDefinitionModule, only: InputParamDefinitionType
  use DefinitionSelectModule, only: get_param_definition_type, &
                                    get_aggregate_definition_type, &
                                    idt_parse_rectype
  use ArrayHandlersModule, only: expandarray
  use CharacterStringModule, only: CharacterStringType

  implicit none
  private
  public :: DynamicPackageParamsType
  public :: allocate_param_charstr
  public :: allocate_param_int1d, allocate_param_int2d
  public :: allocate_param_dbl1d, allocate_param_dbl2d

  !> @brief dynamic parameter filter type
  !!
  !!
  !<
  type :: DynamicPackageParamsType
    character(len=LINELENGTH), dimension(:), allocatable :: params !< in scope param tags
    type(InputParamDefinitionType), pointer :: setting_idt => null()
    type(InputParamDefinitionType), pointer :: setval_idt => null()
    integer(I4B) :: naux !< number of aux variables in package
    integer(I4B) :: inamedbound !< package inamedbound setting
    integer(I4B) :: nparam !< number of in scope params
    type(ModflowInputType) :: mf6_input !< description of input
  contains
    procedure :: init
    procedure :: destroy
    procedure :: set_filtered_list
    procedure :: set_filtered_grid
    procedure :: set_filtered_setting
    procedure :: package_params
    procedure :: filter_settings_type
    procedure :: filter_setting
  end type DynamicPackageParamsType

contains

  !> @brief initialize dynamic param filter
  !!
  !<
  subroutine init(this, mf6_input, readasarrays, aggregate, naux, inamedbound)
    ! -- modules
    ! -- dummy
    class(DynamicPackageParamsType) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    logical(LGP), intent(in) :: readasarrays
    logical(LGP), intent(in) :: aggregate
    integer(I4B), intent(in) :: naux
    integer(I4B), intent(in) :: inamedbound
    ! -- local
    !
    this%mf6_input = mf6_input
    this%nparam = 0
    this%naux = naux
    this%inamedbound = inamedbound
    !
    ! -- determine in scope input params
    if (readasarrays) then
      call this%set_filtered_grid()
    else if (aggregate) then
      call this%set_filtered_list()
    else
      call this%set_filtered_setting()
    end if
    !
    ! --return
    return
  end subroutine init

  !> @brief destroy
  !!
  !<
  subroutine destroy(this)
    ! -- modules
    ! -- dummy
    class(DynamicPackageParamsType) :: this
    !
    ! -- deallocate
    if (allocated(this%params)) deallocate (this%params)
    if (associated(this%setting_idt)) deallocate (this%setting_idt)
    if (associated(this%setval_idt)) deallocate (this%setval_idt)
    !
    ! --return
    return
  end subroutine destroy

  !> @brief array based input dynamic param filter
  !!
  !<
  subroutine set_filtered_grid(this)
    ! -- modules
    ! -- dummy
    class(DynamicPackageParamsType) :: this
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B), dimension(:), allocatable :: idt_idxs
    integer(I4B) :: keepcnt, iparam
    logical(LGP) :: keep
    !
    ! -- initialize
    keepcnt = 0
    !
    ! -- allocate dfn input params
    do iparam = 1, size(this%mf6_input%param_dfns)
      !
      keep = .true.
      !
      ! -- assign param definition pointer
      idt => this%mf6_input%param_dfns(iparam)
      !
      if (idt%blockname /= 'PERIOD') then
        keep = .false.
      end if
      !
      if (idt%tagname == 'AUX') then
        if (this%naux == 0) then
          keep = .false.
        end if
      end if
      !
      if (keep) then
        keepcnt = keepcnt + 1
        call expandarray(idt_idxs)
        idt_idxs(keepcnt) = iparam
      end if
    end do
    !
    ! -- update nparam
    this%nparam = keepcnt
    !
    ! -- allocate filtcols
    allocate (this%params(this%nparam))
    !
    ! -- set filtcols
    do iparam = 1, this%nparam
      idt => this%mf6_input%param_dfns(idt_idxs(iparam))
      this%params(iparam) = trim(idt%tagname)
    end do
    !
    ! -- cleanup
    deallocate (idt_idxs)
    !
    ! -- return
    return
  end subroutine set_filtered_grid

  !> @brief create array of in scope list input columns
  !!
  !! Filter the recarray description of list input parameters
  !! to determine which columns are to be read in this run.
  !<
  subroutine set_filtered_list(this)
    ! -- modules
    use DefinitionSelectModule, only: idt_datatype
    ! -- dummy
    class(DynamicPackageParamsType) :: this
    ! -- local
    type(InputParamDefinitionType), pointer :: ra_idt, idt
    character(len=LINELENGTH), dimension(:), allocatable :: ra_cols
    integer(I4B) :: ra_ncol, icol, keepcnt
    logical(LGP) :: keep
    !
    ! -- initialize
    keepcnt = 0
    !
    ! -- get aggregate param definition for period block
    ra_idt => &
      get_aggregate_definition_type(this%mf6_input%aggregate_dfns, &
                                    this%mf6_input%component_type, &
                                    this%mf6_input%subcomponent_type, &
                                    'PERIOD')
    !
    ! -- split recarray definition
    call idt_parse_rectype(ra_idt, ra_cols, ra_ncol)
    !
    ! -- determine which columns are in scope
    do icol = 1, ra_ncol
      !
      keep = .false.
      !
      ! -- set dfn pointer to recarray parameter
      idt => get_param_definition_type(this%mf6_input%param_dfns, &
                                       this%mf6_input%component_type, &
                                       this%mf6_input%subcomponent_type, &
                                       'PERIOD', ra_cols(icol), '')
      !
      if (ra_cols(icol) == 'RECARRAY') then
        ! no-op
      else if (ra_cols(icol) == 'AUX') then
        if (this%naux > 0) then
          keep = .true.
        end if
      else if (ra_cols(icol) == 'BOUNDNAME') then
        if (this%inamedbound /= 0) then
          keep = .true.
        end if
      else
        if (idt_datatype(idt) == 'KEYSTRING') then
          ! -- determine in scope params of settings type
          call this%filter_settings_type(idt, keepcnt)
        else
          ! -- determine if the param is scope
          keep = pkg_param_in_scope(this%mf6_input, ra_cols(icol))
        end if
      end if
      !
      if (keep) then
        keepcnt = keepcnt + 1
        call expandarray(this%params)
        this%params(keepcnt) = trim(ra_cols(icol))
      end if
    end do
    !
    ! -- update nparam
    this%nparam = keepcnt
    !
    ! -- cleanup
    deallocate (ra_cols)
    !
    ! -- return
    return
  end subroutine set_filtered_list

  !> @brief
  !!
  !<
  subroutine set_filtered_setting(this)
    ! -- modules
    ! -- dummy
    class(DynamicPackageParamsType) :: this
    ! -- local
    character(len=LINELENGTH), dimension(:), allocatable :: ks_params
    character(len=LINELENGTH) :: setting_tag
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B), dimension(:), allocatable :: idt_idxs
    integer(I4B) :: iparam, ks_nparam, keepcnt
    !
    if (this%mf6_input%subcomponent_type /= 'STO') then
      errmsg = 'IDM unsupported setting type load for pkg='// &
               trim(this%mf6_input%subcomponent_type)//'.'
      call store_error(errmsg, .true.)
    end if
    !
    ! -- initialize
    this%nparam = 0
    ks_nparam = 0
    keepcnt = 0
    setting_tag = trim(this%mf6_input%subcomponent_type)//'SETTING'
    !
    ! -- allocate dfn input params
    do iparam = 1, size(this%mf6_input%param_dfns)
      !
      ! -- assign param definition pointer
      idt => this%mf6_input%param_dfns(iparam)
      !
      if (idt%blockname /= 'PERIOD') then
        cycle
      else
        ks_nparam = ks_nparam + 1
        call expandarray(idt_idxs)
        idt_idxs(ks_nparam) = iparam
      end if
    end do
    !
    ! -- allocate filtcols
    allocate (ks_params(ks_nparam))
    !
    ! -- set filtcols
    do iparam = 1, ks_nparam
      idt => this%mf6_input%param_dfns(idt_idxs(iparam))
      ks_params(iparam) = trim(idt%tagname)
    end do
    !
    ! -- create the setting idt
    call this%filter_setting(setting_tag, keepcnt, ks_params, ks_nparam)
    !
    ! -- cleanup
    deallocate (idt_idxs)
    deallocate (ks_params)
    !
    ! -- return
    return
  end subroutine set_filtered_setting

  !> @brief allocate and set input array to filtered param set
  !!
  !<
  subroutine package_params(this, params, nparam)
    ! -- modules
    ! -- dummy
    class(DynamicPackageParamsType) :: this
    character(len=LINELENGTH), dimension(:), allocatable, &
      intent(inout) :: params
    integer(I4B), intent(inout) :: nparam
    integer(I4B) :: n
    !
    if (allocated(params)) deallocate (params)
    !
    nparam = this%nparam
    !
    allocate (params(nparam))
    !
    do n = 1, nparam
      params(n) = this%params(n)
    end do
    !
    ! -- return
    return
  end subroutine package_params

  !> @brief filter a non-advanced package (e.g. TVK) setting type
  !!
  !<
  subroutine filter_setting(this, setting_tag, keepcnt, ks_cols, ks_ncol)
    ! -- modules
    use MemoryManagerModule, only: get_isize, mem_setptr
    use DefinitionSelectModule, only: idt_copy
    ! -- dummy
    class(DynamicPackageParamsType) :: this
    character(len=*), intent(in) :: setting_tag
    integer(I4B), intent(inout) :: keepcnt
    character(len=LINELENGTH), dimension(:), allocatable, intent(in) :: ks_cols
    integer(I4B), intent(in) :: ks_ncol
    ! -- return
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    character(len=LINELENGTH) :: datatype
    integer(I4B) :: icol
    !
    datatype = ''
    !
    ! -- verify datatypes of associated parameters are consistent
    do icol = 1, ks_ncol
      !
      idt => get_param_definition_type(this%mf6_input%param_dfns, &
                                       this%mf6_input%component_type, &
                                       this%mf6_input%subcomponent_type, &
                                       'PERIOD', ks_cols(icol), '')
      !
      if (datatype /= '' .and. datatype /= idt%datatype) then
        errmsg = 'IDM unimplemented. DynamicParamFilterType::filter_setting &
                 &extended use case needed for setting params with non-&
                 &identical datatypes.'
        call store_error(errmsg, .true.)
      else
        datatype = idt%datatype
      end if
    end do
    !
    ! -- use last idt as datatype template for setting idt
    if (ks_ncol > 0) then
      this%setting_idt => idt_copy(idt)
      this%setting_idt%mf6varname = setting_tag
    end if
    !
    ! -- return
    return
  end subroutine filter_setting

  !> @brief filter a package SETTING type
  !!
  !<
  subroutine filter_settings_type(this, setting_idt, keepcnt)
    ! -- modules
    ! -- dummy
    class(DynamicPackageParamsType) :: this
    type(InputParamDefinitionType), pointer, intent(in) :: setting_idt
    integer(I4B), intent(inout) :: keepcnt
    ! -- local
    character(len=LINELENGTH), dimension(:), allocatable :: ks_cols
    integer(I4B) :: ks_ncol
    !
    ! -- split recarray definition
    call idt_parse_rectype(setting_idt, ks_cols, ks_ncol)
    !
    ! -- filter keystring type (e.g. TVK)
    call this%filter_setting(setting_idt%tagname, keepcnt, ks_cols, ks_ncol)
    !
    ! -- cleanup
    deallocate (ks_cols)
    !
    ! -- return
    return
  end subroutine filter_settings_type

  !> @brief allocate character string type array
  !<
  subroutine allocate_param_charstr(strlen, nrow, varname, mempath)
    integer(I4B), intent(in) :: strlen !< string number of characters
    integer(I4B), intent(in) :: nrow !< integer array number of rows
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable mempath
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: charstr1d
    integer(I4B) :: n
    !
    call mem_allocate(charstr1d, strlen, nrow, varname, mempath)
    do n = 1, nrow
      charstr1d(n) = ''
    end do
  end subroutine allocate_param_charstr

  !> @brief allocate int1d
  !<
  subroutine allocate_param_int1d(nrow, varname, mempath)
    integer(I4B), intent(in) :: nrow !< integer array number of rows
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable mempath
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B) :: n
    !
    call mem_allocate(int1d, nrow, varname, mempath)
    do n = 1, nrow
      int1d(n) = IZERO
    end do
  end subroutine allocate_param_int1d

  !> @brief allocate int2d
  !<
  subroutine allocate_param_int2d(ncol, nrow, varname, mempath)
    integer(I4B), intent(in) :: ncol !< integer array number of cols
    integer(I4B), intent(in) :: nrow !< integer array number of rows
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable mempath
    integer(I4B), dimension(:, :), pointer, contiguous :: int2d
    integer(I4B) :: n, m
    !
    call mem_allocate(int2d, ncol, nrow, varname, mempath)
    do m = 1, nrow
      do n = 1, ncol
        int2d(n, m) = IZERO
      end do
    end do
  end subroutine allocate_param_int2d

  !> @brief allocate dbl1d
  !<
  subroutine allocate_param_dbl1d(nrow, varname, mempath)
    integer(I4B), intent(in) :: nrow !< integer array number of rows
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable mempath
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B) :: n
    !
    call mem_allocate(dbl1d, nrow, varname, mempath)
    do n = 1, nrow
      dbl1d(n) = DZERO
    end do
  end subroutine allocate_param_dbl1d

  !> @brief allocate dbl2d
  !<
  subroutine allocate_param_dbl2d(ncol, nrow, varname, mempath)
    integer(I4B), intent(in) :: ncol !< integer array number of cols
    integer(I4B), intent(in) :: nrow !< integer array number of rows
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable mempath
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    integer(I4B) :: n, m
    !
    call mem_allocate(dbl2d, ncol, nrow, varname, mempath)
    do m = 1, nrow
      do n = 1, ncol
        dbl2d(n, m) = DZERO
      end do
    end do
  end subroutine allocate_param_dbl2d

  !> @brief determine if input param is in scope for a package
  !!
  !<
  function pkg_param_in_scope(mf6_input, tagname) result(in_scope)
    ! -- modules
    use MemoryManagerModule, only: get_isize, mem_setptr
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: tagname
    ! -- return
    logical(LGP) :: in_scope
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: pdim_isize, popt_isize
    integer(I4B), pointer :: pdim
    !
    ! -- initialize
    in_scope = .false.
    !
    idt => get_param_definition_type(mf6_input%param_dfns, &
                                     mf6_input%component_type, &
                                     mf6_input%subcomponent_type, &
                                     'PERIOD', tagname, '')
    !
    if (idt%required) then
      ! -- required params always included
      in_scope = .true.
    else
      !
      ! -- package specific logic to determine if input params to be read
      select case (mf6_input%subcomponent_type)
      case ('EVT')
        !
        if (tagname == 'PXDP' .or. tagname == 'PETM') then
          call get_isize('NSEG', mf6_input%mempath, pdim_isize)
          if (pdim_isize > 0) then
            call mem_setptr(pdim, 'NSEG', mf6_input%mempath)
            if (pdim > 1) then
              in_scope = .true.
            end if
          end if
        else if (tagname == 'PETM0') then
          call get_isize('SURFRATESPEC', mf6_input%mempath, popt_isize)
          if (popt_isize > 0) then
            in_scope = .true.
          end if
        end if
        !
      case default
        errmsg = 'IDM unimplemented. DynamicPackageParamsType::pkg_param_in_scope &
                 &add case tagname='//trim(idt%tagname)
        call store_error(errmsg, .true.)
        !call store_error_filename(sourcename)
      end select
    end if
    !
    ! -- return
    return
  end function pkg_param_in_scope

end module DynamicPackageParamsModule

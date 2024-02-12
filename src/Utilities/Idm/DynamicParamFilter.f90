!> @brief This module contains the DynamicParamFilterModule
!!
!! This module contains a type definition for filtering
!! out dynamic parameters that are not in scope for the run
!!
!<
module DynamicParamFilterModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename
  use ModflowInputModule, only: ModflowInputType
  use InputDefinitionModule, only: InputParamDefinitionType
  use DefinitionSelectModule, only: get_param_definition_type, &
                                    get_aggregate_definition_type, &
                                    idt_parse_rectype
  use ArrayHandlersModule, only: expandarray

  implicit none
  private
  public :: DynamicParamFilterType

  !> @brief dynamic parameter filter type
  !!
  !! This type is used to filter out unneeded input parameters
  !! for list and array based dynamic input. It also unpacks
  !! composite dfn parameter types (RECORD, RECARRAY, and
  !! KEYSTRING) and defines helper arrays for the processing
  !! of input SETTINGS types.
  !!
  !<
  type :: DynamicParamFilterType
    type(ModflowInputType) :: mf6_input !< description of input
    character(len=LINELENGTH), dimension(:), allocatable :: flt_params !< in scope param tags
    integer(I4B) :: nfltparam !< number of in scope params
    integer(I4B) :: naux
    integer(I4B) :: inamedbound
    integer(I4B) :: iout
  contains
    procedure :: init
    procedure :: destroy
    procedure :: set_filtered_list
    procedure :: set_filtered_grid
    procedure :: get_flt_params
  end type DynamicParamFilterType

contains

  !> @brief initialize dynamic param filter
  !!
  !<
  subroutine init(this, mf6_input, readasarrays, naux, inamedbound, iout)
    ! -- modules
    ! -- dummy
    class(DynamicParamFilterType) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    logical(LGP), intent(in) :: readasarrays
    integer(I4B), intent(in) :: naux
    integer(I4B), intent(in) :: inamedbound
    integer(I4B), intent(in) :: iout
    ! -- local
    !
    this%mf6_input = mf6_input
    this%nfltparam = 0
    this%naux = naux
    this%inamedbound = inamedbound
    this%iout = iout
    !
    ! -- determine in scope input params
    if (readasarrays) then
      call this%set_filtered_grid()
    else
      call this%set_filtered_list()
    end if
    !
    ! --return
    return
  end subroutine init

  !> @brief destroy dynamic param filter
  !!
  !<
  subroutine destroy(this)
    ! -- modules
    ! -- dummy
    class(DynamicParamFilterType) :: this
    !
    ! -- deallocate
    if (allocated(this%flt_params)) deallocate (this%flt_params)
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
    class(DynamicParamFilterType) :: this
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
    ! -- update nfltparam
    this%nfltparam = keepcnt
    !
    ! -- allocate filtcols
    allocate (this%flt_params(this%nfltparam))
    !
    ! -- set filtcols
    do iparam = 1, this%nfltparam
      idt => this%mf6_input%param_dfns(idt_idxs(iparam))
      this%flt_params(iparam) = trim(idt%tagname)
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
    ! -- dummy
    class(DynamicParamFilterType) :: this
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
        ! -- determine if the param is scope
        keep = pkg_param_in_scope(this%mf6_input, ra_cols(icol))
      end if
      !
      if (keep) then
        keepcnt = keepcnt + 1
        call expandarray(this%flt_params)
        this%flt_params(keepcnt) = trim(ra_cols(icol))
      end if
    end do
    !
    ! -- update nfltparam
    this%nfltparam = keepcnt
    !
    ! -- cleanup
    deallocate (ra_cols)
    !
    ! -- return
    return
  end subroutine set_filtered_list

  !> @brief allocate and set input array to filtered param set
  !!
  !<
  subroutine get_flt_params(this, cols, ncol)
    ! -- modules
    ! -- dummy
    class(DynamicParamFilterType) :: this
    character(len=LINELENGTH), dimension(:), allocatable, &
      intent(inout) :: cols
    integer(I4B), intent(inout) :: ncol
    integer(I4B) :: n
    !
    if (allocated(cols)) deallocate (cols)
    !
    ncol = this%nfltparam
    !
    allocate (cols(ncol))
    !
    do n = 1, ncol
      cols(n) = this%flt_params(n)
    end do
    !
    ! -- return
    return
  end subroutine get_flt_params

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
        errmsg = 'IDM unimplemented. DynamicParamFilterType::pkg_param_in_scope &
                 &add case tagname='//trim(idt%tagname)
        call store_error(errmsg, .true.)
        !call store_error_filename(sourcename)
      end select
    end if
    !
    ! -- return
    return
  end function pkg_param_in_scope

end module DynamicParamFilterModule

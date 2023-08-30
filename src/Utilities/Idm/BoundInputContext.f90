! -- Generic List Reader Module
module BoundInputContextModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LENAUXNAME, LENVARNAME, LENBOUNDNAME
  use ModflowInputModule, only: ModflowInputType
  use CharacterStringModule, only: CharacterStringType

  implicit none
  private
  public :: BoundInputContextType

  !> @brief derived type for boundary package input context
  !!
  !! This derived type is input context used by dynamic package loaders.
  !! Some variables (e.g. iprpak) in the type may have already been created
  !! by a static loader whereas others (e.g. nboound) are created by this
  !! type, updated by to dynamic loader, and accessed from the model package.
  !!
  !<
  type :: BoundInputContextType
    integer(I4B), pointer :: naux => null() !< number of auxiliary variables
    integer(I4B), pointer :: maxbound => null() !< max list input records per period
    integer(I4B), pointer :: inamedbound => null() !< are bound names optioned
    integer(I4B), pointer :: iprpak => null() ! print input option
    integer(I4B), pointer :: nbound => null() !< number of bounds in period
    integer(I4B), pointer :: ncpl => null() !< number of cells per layer
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: auxname_cst => null() !< array of auxiliary names
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: boundname_cst => null() !< array of bound names
    real(DP), dimension(:, :), pointer, &
      contiguous :: auxvar => null() !< auxiliary variable array
    integer(I4B), dimension(:), pointer, contiguous :: mshape => null() !< model shape
    character(len=LENVARNAME), dimension(:), allocatable :: filtcols !< list input in scope columns
    integer(I4B) :: nfiltcol !< list input number of in scope columns
    logical(LGP) :: readasarrays !< grid or list based input
    type(ModflowInputType) :: mf6_input !< description of input
  contains
    procedure :: init => bndctx_init
    procedure :: create_context
    procedure :: enable
    procedure :: allocate_read_state_var
    procedure :: destroy => bndctx_destroy
    procedure :: set_filtered_cols
    procedure :: filtered_cols
  end type BoundInputContextType

contains

  !> @brief initialize boundary input context
  !!
  !<
  subroutine bndctx_init(this, mf6_input, readasarrays)
    ! -- modules
    ! -- dummy
    class(BoundInputContextType) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    logical(LGP), intent(in) :: readasarrays
    !
    this%mf6_input = mf6_input
    this%readasarrays = readasarrays
    !
    ! -- create the dynamic package input context
    call this%create_context()
    !
    ! -- determine in scope list input columns
    if (.not. readasarrays) then
      call this%set_filtered_cols()
    end if
    !
    ! --return
    return
  end subroutine bndctx_init

  !> @brief create boundary input context
  !!
  !<
  subroutine create_context(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr, get_isize
    use MemoryManagerExtModule, only: mem_set_value
    ! -- dummy
    class(BoundInputContextType) :: this
    integer(I4B), dimension(:, :), pointer, contiguous :: cellid => null()
    logical(LGP) :: found
    !
    ! -- set pointers to defined scalars
    call mem_setptr(this%naux, 'NAUX', this%mf6_input%mempath)
    !
    ! -- allocate memory managed scalars
    call mem_allocate(this%nbound, 'NBOUND', this%mf6_input%mempath)
    call mem_allocate(this%ncpl, 'NCPL', this%mf6_input%mempath)
    !
    ! -- internally allocate package optional scalars
    allocate (this%maxbound)
    allocate (this%inamedbound)
    allocate (this%iprpak)
    !
    ! -- initialize allocated and internal scalars
    this%nbound = 0
    this%ncpl = 0
    this%maxbound = 0
    this%inamedbound = 0
    this%iprpak = 0
    this%nfiltcol = 0
    !
    ! -- update optional scalars
    call mem_set_value(this%inamedbound, 'BOUNDNAMES', this%mf6_input%mempath, &
                       found)
    call mem_set_value(this%maxbound, 'MAXBOUND', this%mf6_input%mempath, found)
    call mem_set_value(this%iprpak, 'IPRPAK', this%mf6_input%mempath, found)
    !
    ! -- set pointers to defined arrays
    call mem_setptr(this%mshape, 'MODEL_SHAPE', &
                    this%mf6_input%component_mempath)
    !
    ! -- update ncpl as shape is known
    if (size(this%mshape) == 2) then
      this%ncpl = this%mshape(2)
    else if (size(this%mshape) == 3) then
      this%ncpl = this%mshape(2) * this%mshape(3)
    end if
    !
    ! -- set auxname_cst and iauxmultcol
    if (this%naux > 0) then
      call mem_setptr(this%auxname_cst, 'AUXILIARY', this%mf6_input%mempath)
    else
      call mem_allocate(this%auxname_cst, LENAUXNAME, 0, &
                        'AUXILIARY', this%mf6_input%mempath)
    end if
    !
    ! -- allocate cellid if this is not list input
    if (this%readasarrays) then
      call mem_allocate(cellid, 0, 0, 'CELLID', this%mf6_input%mempath)
    end if
    !
    ! -- return
    return
  end subroutine create_context

  !> @brief enable bound input context
  !!
  !! This routine should be invoked after the loader allocates dynamic
  !! input params. This routine will assign pointers to arrays if they
  !! have been allocoated or allocate the arrays if they have not been.
  !!
  !<
  subroutine enable(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr, get_isize
    use MemoryManagerExtModule, only: mem_set_value
    ! -- dummy
    class(BoundInputContextType) :: this
    ! -- local
    !
    ! -- allocate or set pointer to BOUNDNAME
    if (this%inamedbound == 0) then
      call mem_allocate(this%boundname_cst, LENBOUNDNAME, 0, &
                        'BOUNDNAME', this%mf6_input%mempath)
      !
    else
      call mem_setptr(this%boundname_cst, 'BOUNDNAME', this%mf6_input%mempath)
    end if
    !
    ! -- allocate or set pointer to AUXVAR
    if (this%naux == 0) then
      call mem_allocate(this%auxvar, 0, 0, 'AUXVAR', this%mf6_input%mempath)
      !
    else
      call mem_setptr(this%auxvar, 'AUXVAR', this%mf6_input%mempath)
    end if
    !
    ! -- return
    return
  end subroutine enable

  !> @brief allocate a read state variable
  !!
  !! Create and set a read state variable, e.g. 'INRECHARGE',
  !! which are updated per iper load as follows:
  !! -1: unset, not in use
  !!  0: not read in most recent period block
  !!  1: numeric input read in most recent period block
  !!  2: time series input read in most recent period block
  !!
  !<
  function allocate_read_state_var(this, mf6varname) result(varname)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr, mem_allocate
    ! -- dummy
    class(BoundInputContextType) :: this
    character(len=*), intent(in) :: mf6varname
    ! -- locals
    character(len=LENVARNAME) :: varname
    integer(I4B) :: ilen
    integer(I4B), pointer :: intvar
    character(len=2) :: prefix = 'IN'
    !
    ! -- assign first column as the block number
    ilen = len_trim(mf6varname)
    !
    if (ilen > (LENVARNAME - len(prefix))) then
      varname = prefix//mf6varname(1:(LENVARNAME - len(prefix)))
    else
      varname = prefix//trim(mf6varname)
    end if
    !
    call mem_allocate(intvar, varname, this%mf6_input%mempath)
    intvar = -1
    !
    ! -- return
    return
  end function allocate_read_state_var

  !> @brief destroy boundary input context
  !!
  !<
  subroutine bndctx_destroy(this)
    ! -- modules
    ! -- dummy
    class(BoundInputContextType) :: this
    !
    ! -- deallocate
    deallocate (this%maxbound)
    deallocate (this%inamedbound)
    deallocate (this%iprpak)
    !
    ! -- nullify
    nullify (this%naux)
    nullify (this%nbound)
    nullify (this%ncpl)
    nullify (this%maxbound)
    nullify (this%inamedbound)
    nullify (this%iprpak)
    nullify (this%auxname_cst)
    nullify (this%boundname_cst)
    nullify (this%auxvar)
    nullify (this%mshape)
    !
    deallocate (this%filtcols)
    !
    ! --return
    return
  end subroutine bndctx_destroy

  !> @brief create array of in scope list input columns
  !!
  !! Filter the recarray description of list input parameters
  !! to determine which columns are to be read in this run.
  !<
  subroutine set_filtered_cols(this)
    ! -- modules
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_aggregate_definition_type
    use ArrayHandlersModule, only: expandarray
    use InputOutputModule, only: parseline
    ! -- dummy
    class(BoundInputContextType) :: this
    ! -- local
    type(InputParamDefinitionType), pointer :: ra_idt
    character(len=:), allocatable :: parse_str
    character(len=LENVARNAME), dimension(:), allocatable :: dfncols
    integer(I4B), dimension(:), allocatable :: idxs
    integer(I4B) :: dfnncol, icol, keepcnt
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
    parse_str = trim(ra_idt%datatype)//' '
    call parseline(parse_str, dfnncol, dfncols)
    !
    ! -- determine which columns are in scope
    do icol = 1, dfnncol
      !
      keep = .false.
      !
      if (dfncols(icol) == 'RECARRAY') then
        ! no-op
      else if (dfncols(icol) == 'AUX') then
        if (this%naux > 0) then
          keep = .true.
        end if
      else if (dfncols(icol) == 'BOUNDNAME') then
        if (this%inamedbound /= 0) then
          keep = .true.
        end if
      else
        keep = pkg_param_in_scope(this%mf6_input, dfncols(icol))
      end if
      !
      if (keep) then
        keepcnt = keepcnt + 1
        call expandarray(idxs)
        idxs(keepcnt) = icol
      end if
    end do
    !
    ! -- update nfiltcol
    this%nfiltcol = keepcnt
    !
    ! -- allocate filtcols
    allocate (this%filtcols(this%nfiltcol))
    !
    ! -- set filtcols
    do icol = 1, this%nfiltcol
      this%filtcols(icol) = dfncols(idxs(icol))
    end do
    !
    ! -- cleanup
    deallocate (dfncols)
    deallocate (idxs)
    deallocate (parse_str)
    !
    ! -- return
    return
  end subroutine set_filtered_cols

  !> @brief allocate and set input array to filtered param set
  !!
  !<
  subroutine filtered_cols(this, cols, ncol)
    ! -- modules
    ! -- dummy
    class(BoundInputContextType) :: this
    character(len=LENVARNAME), dimension(:), allocatable, &
      intent(inout) :: cols
    integer(I4B), intent(inout) :: ncol
    integer(I4B) :: n
    !
    if (allocated(cols)) deallocate (cols)
    !
    ncol = this%nfiltcol
    !
    allocate (cols(ncol))
    !
    do n = 1, ncol
      cols(n) = this%filtcols(n)
    end do
    !
    ! -- return
    return
  end subroutine filtered_cols

  !> @brief determine if input param is in scope for a package
  !!
  !<
  function pkg_param_in_scope(mf6_input, tagname) result(in_scope)
    ! -- modules
    use MemoryManagerModule, only: get_isize, mem_setptr
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: tagname
    ! -- return
    logical(LGP) :: in_scope
    ! -- locals
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
      end select
    end if
    !
    ! -- return
    return
  end function pkg_param_in_scope

end module BoundInputContextModule

!> @brief This module contains the BoundInputContextModule
!!
!! This module contains a type that stores and creates context
!! relevant to stress package inputs.
!!
!<
module BoundInputContextModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, IZERO, LINELENGTH, LENAUXNAME, &
                             LENVARNAME, LENBOUNDNAME
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename
  use ModflowInputModule, only: ModflowInputType
  use InputDefinitionModule, only: InputParamDefinitionType
  use CharacterStringModule, only: CharacterStringType

  implicit none
  private
  public :: BoundInputContextType
  public :: ReadStateVarType

  !> @brief Pointer type for read state variable
  !<
  type ReadStateVarType
    integer, pointer :: invar
  end type ReadStateVarType

  !> @brief derived type for boundary package input context
  !!
  !! This derived type defines input context used by dynamic package loaders.
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
    logical(LGP) :: readasarrays !< grid or list based input
    type(ModflowInputType) :: mf6_input !< description of input
  contains
    procedure :: init => bndctx_init
    procedure :: create_context
    procedure :: enable
    procedure :: list_params_create
    procedure :: array_params_create
    procedure :: param_init
    procedure :: destroy => bndctx_destroy
    procedure :: rsv_alloc
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
    integer(I4B), dimension(:, :), pointer, contiguous :: cellid
    logical(LGP) :: found
    !
    ! -- initialize
    nullify (cellid)
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
  !! have been allocated and allocate the arrays if not.
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

  subroutine list_params_create(this, params, nparam, input_name)
    ! -- modules
    use ConstantsModule, only: DZERO, IZERO
    use MemoryManagerModule, only: mem_allocate
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    ! -- dummy
    class(BoundInputContextType) :: this
    character(len=*), dimension(:), allocatable, intent(in) :: params
    integer(I4B), intent(in) :: nparam
    character(len=*), intent(in) :: input_name
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B), dimension(:, :), pointer, contiguous :: int2d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: charstr1d
    integer(I4B) :: iparam, m, n
    !
    ! --
    do iparam = 1, nparam
      idt => get_param_definition_type(this%mf6_input%param_dfns, &
                                       this%mf6_input%component_type, &
                                       this%mf6_input%subcomponent_type, &
                                       'PERIOD', params(iparam), '')
      !
      ! allocate based on dfn datatype
      select case (idt%datatype)
      case ('INTEGER')
        call mem_allocate(int1d, this%maxbound, idt%mf6varname, &
                          this%mf6_input%mempath)
        do n = 1, this%maxbound
          int1d(n) = IZERO
        end do
        !
      case ('DOUBLE')
        call mem_allocate(dbl1d, this%maxbound, idt%mf6varname, &
                          this%mf6_input%mempath)
        do n = 1, this%maxbound
          dbl1d(n) = DZERO
        end do
        !
      case ('STRING')
        call mem_allocate(charstr1d, LENBOUNDNAME, this%maxbound, &
                          idt%mf6varname, this%mf6_input%mempath)
        do n = 1, this%maxbound
          charstr1d(n) = ''
        end do
        !
      case ('INTEGER1D')
        if (idt%shape == 'NCELLDIM') then
          call mem_allocate(int2d, size(this%mshape), this%maxbound, &
                            idt%mf6varname, this%mf6_input%mempath)
          do m = 1, this%maxbound
            do n = 1, size(this%mshape)
              int2d(n, m) = IZERO
            end do
          end do
        else
          errmsg = 'IDM unimplemented. BoundInputContext::list_params_create &
                   &shape='//trim(idt%shape)
          call store_error(errmsg)
          call store_error_filename(input_name)
        end if
        !
      case ('DOUBLE1D')
        if (idt%shape == 'NAUX') then
          call mem_allocate(dbl2d, this%naux, this%maxbound, &
                            idt%mf6varname, this%mf6_input%mempath)
          do m = 1, this%maxbound
            do n = 1, this%naux
              dbl2d(n, m) = DZERO
            end do
          end do
        else
          errmsg = 'IDM unimplemented. BoundInputContext::list_params_create &
                   &tagname='//trim(idt%tagname)
          call store_error(errmsg)
          call store_error_filename(input_name)
        end if
        !
      case default
        errmsg = 'IDM unimplemented. BoundInputContext::list_params_create &
                 &datatype='//trim(idt%datatype)
        call store_error(errmsg)
        call store_error_filename(input_name)
      end select
    end do
    !
    ! -- return
    return
  end subroutine list_params_create

  !> @brief allocate dfn array input period block parameters
  !!
  !! Currently supports numeric (i.e. array based) params
  !!
  !<
  subroutine array_params_create(this, params, nparam, input_name)
    ! -- modules
    use ConstantsModule, only: DZERO, IZERO
    use MemoryManagerModule, only: mem_allocate
    use DefinitionSelectModule, only: get_param_definition_type
    ! -- dummy
    class(BoundInputContextType) :: this
    character(len=*), dimension(:), allocatable, intent(in) :: params
    integer(I4B), intent(in) :: nparam
    character(len=*), intent(in) :: input_name
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    integer(I4B) :: iparam, m, n
    !
    ! -- allocate dfn input params
    do iparam = 1, nparam
      !
      ! -- assign param definition pointer
      idt => get_param_definition_type(this%mf6_input%param_dfns, &
                                       this%mf6_input%component_type, &
                                       this%mf6_input%subcomponent_type, &
                                       'PERIOD', params(iparam), '')
      !
      if (idt%blockname == 'PERIOD') then
        select case (idt%datatype)
        case ('INTEGER1D')
          call mem_allocate(int1d, this%ncpl, idt%mf6varname, &
                            this%mf6_input%mempath)
          do n = 1, this%ncpl
            int1d(n) = IZERO
          end do
          !
        case ('DOUBLE1D')
          call mem_allocate(dbl1d, this%ncpl, idt%mf6varname, &
                            this%mf6_input%mempath)
          do n = 1, this%ncpl
            dbl1d(n) = DZERO
          end do
          !
        case ('DOUBLE2D')
          call mem_allocate(dbl2d, this%naux, this%ncpl, idt%mf6varname, &
                            this%mf6_input%mempath)
          do m = 1, this%ncpl
            do n = 1, this%naux
              dbl2d(n, m) = DZERO
            end do
          end do
          !
        case default
          errmsg = 'IDM unimplemented. BoundInputContext::array_params_create &
                   &datatype='//trim(idt%datatype)
          call store_error(errmsg)
          call store_error_filename(input_name)
        end select
      end if
    end do
    !
    ! -- return
    return
  end subroutine array_params_create

  subroutine param_init(this, datatype, varname, input_name)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(BoundInputContextType) :: this
    character(len=*), intent(in) :: datatype
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: input_name
    ! -- local
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: charstr1d
    integer(I4B) :: n, m
    !
    select case (datatype)
    case ('INTEGER1D')
      call mem_setptr(int1d, varname, this%mf6_input%mempath)
      do n = 1, this%ncpl
        int1d(n) = IZERO
      end do
      !
    case ('DOUBLE1D')
      call mem_setptr(dbl1d, varname, this%mf6_input%mempath)
      do n = 1, this%ncpl
        dbl1d(n) = DZERO
      end do
      !
    case ('DOUBLE2D')
      call mem_setptr(dbl2d, varname, this%mf6_input%mempath)
      do m = 1, this%ncpl
        do n = 1, this%naux
          dbl2d(n, m) = DZERO
        end do
      end do
      !
    case ('CHARSTR1D')
      call mem_setptr(charstr1d, varname, this%mf6_input%mempath)
      do n = 1, size(charstr1d)
        charstr1d(n) = ''
      end do
      !
    case default
      errmsg = 'IDM unimplemented. BoundInputContext::param_init &
               &datatype='//trim(datatype)
      call store_error(errmsg)
      call store_error_filename(input_name)
    end select
    !
    ! -- return
    return
  end subroutine param_init

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
    ! --return
    return
  end subroutine bndctx_destroy

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
  function rsv_alloc(this, mf6varname) result(varname)
    ! -- modules
    use ConstantsModule, only: LENVARNAME
    use MemoryManagerModule, only: mem_setptr, mem_allocate
    ! -- dummy
    class(BoundInputContextType) :: this
    character(len=*), intent(in) :: mf6varname
    ! -- local
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
  end function rsv_alloc

end module BoundInputContextModule

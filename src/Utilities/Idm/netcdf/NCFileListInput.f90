!> @brief This module contains the NCFileListInputModule
!!
!! This module contains the routines for loading MODFLOW 6
!! netcdf dynamic list input to the input context.
!!
!<
module NCFileListInputModule

  use netcdf
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENBOUNDNAME, DNODATA, INODATA
  use SimModule, only: store_error, store_error_filename
  use SimVariablesModule, only: errmsg
  use MemoryManagerModule, only: mem_allocate, mem_setptr
  use CharacterStringModule, only: CharacterStringType
  use NCModelInputsModule, only: NCModelPackageInputType
  use DynamicParamFilterModule, only: DynamicParamFilterType
  use BoundInputContextModule, only: BoundInputContextType
  use NCInputLoadTypeModule, only: NCDynamicPkgLoadBaseType
  use InputDefinitionModule, only: InputParamDefinitionType
  use DefinitionSelectModule, only: get_param_definition_type
  use LoadNCFileModule, only: IDM_NETCDF4_MAX_DIM, nf_verify
  use ModflowInputModule, only: ModflowInputType

  implicit none
  private
  public :: NCBoundListInputType

  !> @brief NetCDF list based dynamic loader type
  !<
  type, extends(NCDynamicPkgLoadBaseType) :: NCBoundListInputType
    !type(TimeSeriesManagerType), pointer :: tsmanager => null()
    integer(I4B) :: ncid
    integer(I4B) :: iiper
    type(DynamicParamFilterType) :: filter
    type(BoundInputContextType) :: bound_context
  contains
    procedure :: init => bndlist_init
    procedure :: df => bndlist_df
    procedure :: ad => bndlist_ad
    procedure :: rp => bndlist_rp
    procedure :: validate => bndlist_validate
    procedure :: destroy => bndlist_destroy
  end type NCBoundListInputType

contains

  !> @brief dynamic list loader init
  !<
  subroutine bndlist_init(this, mf6_input, component_name, component_input_name, &
                          input_name, iperblock, iout)
    ! -- modules
    use netcdf
    use MemoryManagerModule, only: get_isize
    use InputOutputModule, only: getunit
    ! -- dummy
    class(NCBoundListInputType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: component_input_name
    character(len=*), intent(in) :: input_name
    integer(I4B), intent(in) :: iperblock
    integer(I4B), intent(in) :: iout
    ! -- local
    !
    ! --initialize
    call this%DynamicPkgLoadType%init(mf6_input, component_name, &
                                      component_input_name, &
                                      input_name, iperblock, iout)
    this%iiper = 0
    !
    ! -- initialize bound context
    call this%bound_context%init(this%mf6_input, this%readasarrays)
    !
    ! -- allocate bound params
    call this%filter%init(this%mf6_input, this%readasarrays, &
                          this%bound_context%naux, &
                          this%bound_context%inamedbound, &
                          this%iout)
    !
    call this%filter%get_flt_params(this%param_names, this%nparam)
    !
    call this%bound_context%list_params_create(this%param_names, this%nparam, &
                                               this%input_name)
    !
    call this%bound_context%enable()
    !
    ! -- return
    return
  end subroutine bndlist_init

  !> @brief dynamic list loader define
  !<
  subroutine bndlist_df(this)
    ! -- modules
    ! -- dummy
    class(NCBoundListInputType), intent(inout) :: this
    !
    ! -- return
    return
  end subroutine bndlist_df

  !> @brief dynamic list loader advance
  !<
  subroutine bndlist_ad(this)
    ! -- modules
    ! -- dummy
    class(NCBoundListInputType), intent(inout) :: this
    !
    ! -- return
    return
  end subroutine bndlist_ad

  !> @brief dynamic list loader read and prepare
  !<
  subroutine bndlist_rp(this, ncid, ncpkg)
    ! -- modules
    ! -- dummy
    class(NCBoundListInputType), intent(inout) :: this
    integer(I4B), intent(in) :: ncid
    type(NCModelPackageInputType), pointer, intent(inout) :: ncpkg
    !
    ! -- increment iiper
    this%iiper = this%iiper + 1
    !
    ! -- dynamic load
    call rp_list(this%mf6_input, ncpkg, this%bound_context, ncid, &
                 this%iiper, this%input_name, this%iout)
    !
    ! -- return
    return
  end subroutine bndlist_rp

  !> @brief dynamic list loader validate
  !!
  !! Verify expected input package parameters are in input file
  !!
  !<
  subroutine bndlist_validate(this, ncpkg)
    ! -- modules
    ! -- dummy
    class(NCBoundListInputType), intent(inout) :: this
    type(NCModelPackageInputType), pointer, intent(inout) :: ncpkg
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam, ivar
    character(len=LINELENGTH) :: tagname, varname
    logical(LGP) :: found
    !
    ! --
    if (this%iperblock /= ncpkg%iperblock) then
      errmsg = 'IPERBLOCK ERROR'
      call store_error(errmsg)
      call store_error_filename(this%input_name)
    end if
    !
    ! -- verify required package variables are defined
    do iparam = 1, this%nparam
      tagname = this%param_names(iparam)
      !
      idt => &
        get_param_definition_type(this%mf6_input%param_dfns, &
                                  this%mf6_input%component_type, &
                                  this%mf6_input%subcomponent_type, &
                                  'PERIOD', tagname, '')
      !
      if (idt%required) then
        found = .false.
        do ivar = 1, ncpkg%blocklist(this%iperblock)%varnum
          varname = ncpkg%blocklist(this%iperblock)%tagnames(ivar)
          if (varname == tagname) then
            found = .true.
            exit
          end if
        end do
        !
        if (.not. found) then
          errmsg = 'Required PERIOD package variable not found => '//trim(tagname)
          call store_error(errmsg)
          call store_error_filename(this%input_name)
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine bndlist_validate

  !> @brief dynamic list loader destroy
  !<
  subroutine bndlist_destroy(this)
    ! -- modules
    ! -- dummy
    class(NCBoundListInputType), intent(inout) :: this
    if (allocated(this%param_names)) deallocate (this%param_names)
    call this%bound_context%destroy()
  end subroutine bndlist_destroy

  subroutine rp_list(mf6_input, ncpkg, bndctx, ncid, iiper, nc_fname, iout)
    use netcdf
    use TdisModule, only: kper
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(NCModelPackageInputType), intent(in) :: ncpkg
    type(BoundInputContextType), intent(in) :: bndctx
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: iiper
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B), dimension(:), contiguous, pointer :: mshape
    integer(I4B) :: iparam, iblock, nbound, prev_nbound, varid
    character(len=LINELENGTH) :: tagname
    !
    ! -- initialize
    nbound = 0
    prev_nbound = 0
    !
    ! -- set mshape input context pointer
    call mem_setptr(mshape, 'MODEL_SHAPE', mf6_input%component_mempath)
    !
    do iblock = 1, size(ncpkg%blocklist)
      if (ncpkg%blocklist(iblock)%blockname == 'PERIOD') then
        do iparam = 1, ncpkg%blocklist(iblock)%varnum
          !
          tagname = ncpkg%blocklist(iblock)%tagnames(iparam)
          !
          idt => &
            get_param_definition_type(mf6_input%param_dfns, &
                                      mf6_input%component_type, &
                                      mf6_input%subcomponent_type, &
                                      'PERIOD', tagname, nc_fname)
          !
          varid = ncpkg%blocklist(iblock)%varids(iparam)
          nbound = load_var(mf6_input, ncid, idt, mshape, &
                            varid, iiper, nc_fname, iout)
          !
          if (prev_nbound /= 0 .and. nbound /= prev_nbound) then
            write (errmsg, '(a,i0,a)') &
              'Package input nbound inconsistency in period ', kper, &
              ' for data in package "'//trim(ncpkg%subcomponent_name)//'". '
            call store_error(errmsg)
            call store_error_filename(nc_fname)
          else
            prev_nbound = nbound
          end if
        end do
      end if
    end do
    !
    ! -- set nbound
    bndctx%nbound = nbound
    !
    ! -- return
    return
  end subroutine rp_list

  function load_var(mf6_input, ncid, idt, mshape, &
                    varid, iiper, nc_fname, iout) result(nbound)
    use netcdf
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), intent(in) :: ncid
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iiper
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- result
    integer(I4B) :: nbound
    ! -- local
    integer(I4B) :: n
    character(len=LINELENGTH) :: varname, dimname
    integer(I4B) :: vartype, ndims, nattr
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM) :: dimids, dimlens
    !
    ! -- initialize
    nbound = 0
    !
    ! -- inquire for variable info
    call nf_verify(nf90_inquire_variable(ncid, varid, varname, vartype, ndims, &
                                         dimids, nattr), ncid, iout)
    !
    ! -- set variable dimensions
    do n = 1, ndims
      !
      call nf_verify(nf90_inquire_dimension(ncid, dimids(n), dimname, &
                                            dimlens(n)), ncid, iout)
    end do
    !
    ! -- read and load variable data
    select case (idt%datatype)
    case ('STRING')
      nbound = load_charstr1d(mf6_input, ncid, idt, mshape, &
                              dimlens, varid, iiper, iout)
      !
    case ('INTEGER')
      nbound = load_int1d(mf6_input, ncid, idt, mshape, &
                          dimlens, varid, iiper, iout)
      !
    case ('INTEGER1D')
      nbound = load_int2d(mf6_input, ncid, idt, mshape, &
                          dimlens, varid, iiper, iout)
      !
    case ('DOUBLE')
      nbound = load_dbl1d(mf6_input, ncid, idt, mshape, &
                          dimlens, varid, iiper, iout)
      !
    case ('DOUBLE1D')
      nbound = load_dbl2d(mf6_input, ncid, idt, mshape, &
                          dimlens, varid, iiper, iout)
      !
    case default
      errmsg = 'IDM unimplemented. NCFileListInput::load_var &
               &datatype='//trim(idt%datatype)
      call store_error(errmsg)
      call store_error_filename(nc_fname)
    end select
    !
    ! -- return
    return
  end function load_var

  function load_charstr1d(mf6_input, ncid, idt, mshape, dimlens, &
                          varid, iiper, iout) result(nbound)
    use InputOutputModule, only: lowcase, upcase
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), intent(in) :: ncid
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM), intent(in) :: dimlens
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iiper
    integer(I4B), intent(in) :: iout
    ! -- result
    integer(I4B) :: nbound
    ! -- local
    type(CharacterStringType), dimension(:), &
      contiguous, pointer :: charstr1d
    character(len=LENBOUNDNAME), dimension(:), &
      contiguous, pointer :: cstr1d
    integer(I4B) :: n
    !
    ! -- initialize
    nbound = 0
    !
    ! -- allocate local character string array
    allocate (cstr1d(dimlens(2)))
    !
    ! -- read string array
    call nf_verify(nf90_get_var(ncid, varid, cstr1d, &
                                start=(/1, iiper/), &
                                count=(/dimlens(1), dimlens(2), 1/)), ncid, iout)
    !
    ! -- copy local data to managed memory
    call mem_setptr(charstr1d, idt%mf6varname, mf6_input%mempath)
    !
    do n = 1, size(cstr1d)
      charstr1d(n) = format_input_str(cstr1d(n), LENBOUNDNAME)
      if (charstr1d(n) == '') then
        nbound = n - 1
        exit
      end if
    end do
    !
    if (nbound == 0) then
      nbound = size(cstr1d)
    end if
    !
    ! -- deallocate local array
    deallocate (cstr1d)
    !
    ! -- return
    return
  end function load_charstr1d

  function load_int1d(mf6_input, ncid, idt, mshape, dimlens, &
                      varid, iiper, iout) result(nbound)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), intent(in) :: ncid
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM), intent(in) :: dimlens
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iiper
    integer(I4B), intent(in) :: iout
    ! -- result
    integer(I4B) :: nbound
    ! -- local
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B) :: n
    !
    nbound = 0
    !
    ! -- read and load data to managed memory
    call mem_setptr(int1d, idt%mf6varname, mf6_input%mempath)
    !
    call nf_verify(nf90_get_var(ncid, varid, int1d, &
                                start=(/1, iiper/), &
                                count=(/dimlens(1), 1/)), ncid, iout)
    !
    ! -- set nbound
    do n = 1, size(int1d)
      if (int1d(n) == INODATA) then
        nbound = n - 1
        exit
      end if
    end do
    !
    if (nbound == 0) then
      nbound = size(int1d)
    end if
    !
    ! -- return
    return
  end function load_int1d

  function load_int2d(mf6_input, ncid, idt, mshape, dimlens, &
                      varid, iiper, iout) result(nbound)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), intent(in) :: ncid
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM), intent(in) :: dimlens
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iiper
    integer(I4B), intent(in) :: iout
    ! -- result
    integer(I4B) :: nbound
    ! -- local
    integer(I4B), dimension(:, :), pointer, contiguous :: int2d
    integer(I4B) :: n, m
    !
    nbound = 0
    !
    ! -- read and load data to managed memory
    call mem_setptr(int2d, idt%mf6varname, mf6_input%mempath)
    call nf_verify(nf90_get_var(ncid, varid, int2d, &
                                start=(/1, 1, iiper/), &
                                count=(/dimlens(1), dimlens(2), 1/)), ncid, iout)
    !
    ! -- set nbound
    outer: do n = 1, size(int2d, 2)
      do m = 1, size(int2d, 1)
        if (int2d(m, n) == INODATA) then
          nbound = n - 1
          exit outer
        end if
      end do
    end do outer
    !
    if (nbound == 0) then
      nbound = size(int2d, 2)
    end if
    !
    ! -- return
    return
  end function load_int2d

  function load_dbl1d(mf6_input, ncid, idt, mshape, dimlens, &
                      varid, iiper, iout) result(nbound)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), intent(in) :: ncid
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM), intent(in) :: dimlens
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iiper
    integer(I4B), intent(in) :: iout
    ! -- result
    integer(I4B) :: nbound
    ! -- local
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B) :: n
    !
    nbound = 0
    !
    ! -- read and load data to managed memory
    call mem_setptr(dbl1d, idt%mf6varname, mf6_input%mempath)
    call nf_verify(nf90_get_var(ncid, varid, dbl1d, &
                                start=(/1, iiper/), &
                                count=(/dimlens(1), 1/)), ncid, iout)
    !
    ! -- set nbound
    do n = 1, size(dbl1d)
      if (dbl1d(n) == DNODATA) then
        nbound = n - 1
        exit
      end if
    end do
    !
    if (nbound == 0) then
      nbound = size(dbl1d)
    end if
    !
    ! -- return
    return
  end function load_dbl1d

  function load_dbl2d(mf6_input, ncid, idt, mshape, dimlens, &
                      varid, iiper, iout) result(nbound)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), intent(in) :: ncid
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM), intent(in) :: dimlens
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iiper
    integer(I4B), intent(in) :: iout
    ! -- result
    integer(I4B) :: nbound
    ! -- local
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    integer(I4B) :: n, m
    !
    nbound = 0
    !
    ! -- read and load data to managed memory
    call mem_setptr(dbl2d, idt%mf6varname, mf6_input%mempath)
    call nf_verify(nf90_get_var(ncid, varid, dbl2d, &
                                start=(/1, 1, iiper/), &
                                count=(/dimlens(1), dimlens(2), 1/)), ncid, iout)
    !
    ! -- set nbound
    outer: do n = 1, size(dbl2d, 2)
      do m = 1, size(dbl2d, 1)
        if (dbl2d(m, n) == DNODATA) then
          nbound = n - 1
          exit outer
        end if
      end do
    end do outer
    !
    if (nbound == 0) then
      nbound = size(dbl2d, 2)
    end if
    !
    ! -- return
    return
  end function load_dbl2d

  function format_input_str(input_str, len) result(str)
    use InputOutputModule, only: lowcase, upcase
    character(len=*), intent(in) :: input_str
    integer(I4B), intent(in) :: len
    character(len=len) :: str
    integer(I4B) :: idx
    !
    ! -- initialize
    idx = 0
    str = trim(input_str)
    !
    ! -- identify '"' character if present
    idx = index(str, '"')
    !
    if (idx > 0) then
      str = str(1:idx - 1)
    end if
    !
    call upcase(str)
    !
    ! -- return
    return
  end function format_input_str

end module NCFileListInputModule

!> @brief This module contains the NCContextBuildModule
!!
!<
module NCContextBuildModule

  use netcdf
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENCOMPONENTNAME, LENPACKAGETYPE
  use SimModule, only: store_error, store_error_filename
  use SimVariablesModule, only: errmsg
  use InputDefinitionModule, only: InputParamDefinitionType
  use NCModelInputsModule, only: NCModelInputsType, &
                                 NCModelPackageInputType
  use InputDefinitionModule, only: InputParamDefinitionType
  use DefinitionSelectModule, only: package_scoped_param_dfn
  use LoadNCFileModule, only: IDM_NETCDF4_MAX_DIM, nf_verify

  implicit none
  private
  public :: create_ncpkg_context

contains

  !> @brief get package list
  !<
  function get_nc_package(nc_context, pkgtype, comp_type, subcomp_type, &
                          comp_name, subcomp_name, iout) result(ncpkg)
    use SourceCommonModule, only: idm_subcomponent_name
    ! -- dummy variables
    type(NCModelInputsType), intent(in) :: nc_context
    character(len=*), intent(in) :: pkgtype
    character(len=*), intent(in) :: comp_type
    character(len=*), intent(in) :: subcomp_type
    character(len=*), intent(in) :: comp_name
    character(len=*), intent(in) :: subcomp_name
    integer(I4B), intent(in) :: iout
    ! -- return
    class(NCModelPackageInputType), pointer :: ncpkg
    ! -- local variables
    character(len=LENCOMPONENTNAME) :: sc_name
    !
    ! -- initialize
    nullify (ncpkg)
    !
    if (nc_context%has_package(comp_name, subcomp_name)) then
      ncpkg => nc_context%get_package(comp_name, subcomp_name)
      if (ncpkg%subcomponent_type /= subcomp_type) then
        ! -- pkgtype (EVT6 or RCH6) used in namefile packages block-
        !    dfns were initialized to list but need to be array.
        call ncpkg%reset(pkgtype, subcomp_type)
      end if
    else
      errmsg = 'NC package context not found:'// &
               trim(comp_name)//'/'//trim(sc_name)
      call store_error(errmsg)
      call store_error_filename(nc_context%modelfname)
    end if
    !
    ! -- return
    return
  end function get_nc_package

  !> @brief get package list
  !<
  function read_as_arrays(param_dfns)
    ! -- dummy variables
    type(InputParamDefinitionType), dimension(:), &
      pointer, intent(in) :: param_dfns
    ! -- return
    logical(LGP) :: read_as_arrays
    ! -- local variables
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: i
    !
    ! -- initialize
    read_as_arrays = .false.
    !
    do i = 1, size(param_dfns)
      idt => param_dfns(i)
      if (param_dfns(i)%tagname == 'READASARRAYS') then
        read_as_arrays = .true.
        exit
      end if
    end do
    !
    ! -- return
    return
  end function read_as_arrays

  subroutine add_block_var(ncpkg, ncid, varid, varname, &
                           modelfname, iout)
    ! -- modules
    use IdmDfnSelectorModule, only: param_definitions
    ! -- dummy
    type(NCModelPackageInputType), pointer, intent(in) :: ncpkg
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    character(len=*), intent(inout) :: varname
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: iout
    ! -- local
    type(InputParamDefinitionType), dimension(:), &
      pointer :: param_dfns
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: griddata
    logical(LGP) :: array_input
    !
    ! -- set dfns and input type
    param_dfns => param_definitions(ncpkg%component_type, &
                                    ncpkg%subcomponent_type)
    array_input = read_as_arrays(param_dfns)
    !
    ! -- assume tagname is unique to input dfn set
    idt => package_scoped_param_dfn(param_dfns, ncpkg%component_type, &
                                    ncpkg%subcomponent_type, varname, '')
    !
    if (array_input) then
      ! -- verify variable attribute mf6_iper is defined for dynamic array inputs
      if (idt%blockname == 'PERIOD') then
        if (nf90_inquire_attribute(ncid, varid, "mf6_iper", &
                                   len=griddata) /= NF90_NOERR) then
          errmsg = 'Required attribute mf6_iper not found for array &
                   &input param "'//trim(varname)//'".'
          call store_error(errmsg)
          call store_error_filename(modelfname)
        end if
      end if
    end if
    !
    ! -- add package variable
    call ncpkg%add(idt%blockname, varname, varid)
    !
    ! -- return
    return
  end subroutine add_block_var

  subroutine create_pkg_ipers(ncpkg, ncid, varid, varname, iout)
    ! -- modules
    ! -- dummy
    type(NCModelPackageInputType), pointer, intent(in) :: ncpkg
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    character(len=*), intent(inout) :: varname
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: vartype, ndims, nattr
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM) :: dimids, dimlens
    character(len=LINELENGTH) :: dimname
    !
    ! -- inquire for IPER variable info
    call nf_verify(nf90_inquire_variable(ncid, varid, varname, vartype, ndims, &
                                         dimids, nattr), ncid, iout)
    !
    ! -- allocate ipers array
    if (ndims == 1) then
      call nf_verify(nf90_inquire_dimension(ncid, dimids(1), dimname, &
                                            dimlens(1)), ncid, iout)
      ! -- allocate and set ipers array
      allocate (ncpkg%ipers(dimlens(1)))
      call nf_verify(nf90_get_var(ncid, varid, ncpkg%ipers), ncid, iout)
    end if
    !
    ! -- return
    return
  end subroutine create_pkg_ipers

  subroutine split_mf6_input(mf6_input, tokens, input_name, iout)
    use ArrayHandlersModule, only: expandarray
    character(len=*), intent(in) :: mf6_input
    character(len=*), dimension(:), allocatable, intent(inout) :: tokens
    character(len=*), intent(in) :: input_name
    integer(I4B), intent(in) :: iout
    integer(I4B) :: idx, ntoken
    character(len=LINELENGTH) :: token, str
    !
    ! -- initialize
    ntoken = 0
    token = ''
    !
    ! -- set index of colon character
    idx = index(mf6_input, ':')
    !
    if (idx > 0) then
      ! -- split input string into package type and path tokens
      token = mf6_input(1:idx - 1)
      str = mf6_input(idx + 1:len_trim(mf6_input))
      ntoken = ntoken + 1
      call expandarray(tokens)
      tokens(ntoken) = trim(token)
      !
      ! -- split path string on forward slash delimiter
      do
        idx = index(str, '/')
        if (idx <= 0) exit
        token = str(1:idx - 1)
        str = str(idx + 1:len_trim(str))
        ntoken = ntoken + 1
        call expandarray(tokens)
        tokens(ntoken) = trim(token)
      end do
      !
      ! -- add last token
      ntoken = ntoken + 1
      call expandarray(tokens)
      tokens(ntoken) = trim(str)
    else
      errmsg = 'Invalid variable mf6_input attribute: '//trim(mf6_input)
      call store_error(errmsg)
      call store_error_filename(input_name)
    end if
    !
    ! -- return
    return
  end subroutine split_mf6_input

  subroutine add_package_var(nc_context, modelname, varid, iout)
    ! -- modules
    use MemoryHelperModule, only: split_mem_path
    use SourceCommonModule, only: idm_subcomponent_type
    use SourceCommonModule, only: idm_subcomponent_name
    ! -- dummy
    type(NCModelInputsType), intent(inout) :: nc_context
    character(len=*), intent(in) :: modelname
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iout
    ! -- local
    type(NCModelPackageInputType), pointer :: ncpkg
    character(len=LINELENGTH) :: input_str
    character(len=LENCOMPONENTNAME) :: c_name, sc_name, sc_type
    character(len=LENPACKAGETYPE) :: pkgtype
    character(len=LINELENGTH) :: varname
    character(len=LINELENGTH), dimension(:), allocatable :: tokens
    integer(I4B) :: ncid
    !
    ! -- initialize varname
    varname = ''
    !
    ! -- set ncid
    ncid = nc_context%ncid
    !
    ! -- process mf6_input attribute
    if (nf90_get_att(ncid, varid, "mf6_input", &
                     input_str) == NF90_NOERR) then
      !
      call split_mf6_input(input_str, tokens, nc_context%modelfname, iout)
      !
      if (size(tokens) == 4) then
        pkgtype = trim(tokens(1))
        sc_type = idm_subcomponent_type(nc_context%modeltype, pkgtype)
        c_name = trim(tokens(2))
        sc_name = trim(tokens(3))
        varname = trim(tokens(4))
      else
        ! error
      end if
      !
      ncpkg => get_nc_package(nc_context, pkgtype, nc_context%component_type, &
                              sc_type, c_name, sc_name, iout)

      if (varname == 'IPER') then
        ! -- allocate and set package ipers
        call create_pkg_ipers(ncpkg, ncid, varid, varname, iout)
        !
      else
        ! -- track package parameter
        call add_block_var(ncpkg, ncid, varid, varname, &
                           nc_context%modelfname, iout)
      end if
      !
      deallocate (tokens)
    else
      ! no-op, variable is not intended as modflow input. set warning?
    end if
    !
    ! -- return
    return
  end subroutine add_package_var

  function verify_global_attr(nc_context) result(modelname)
    use InputOutputModule, only: lowcase, upcase
    type(NCModelInputsType), intent(inout) :: nc_context
    ! -- return
    character(len=LENCOMPONENTNAME) :: modelname
    ! -- local
    character(len=LENPACKAGETYPE) :: modeltype
    !
    ! -- initialize modelname
    modelname = ''
    !
    ! -- verify expected mf6_modeltype file attribute
    if (nf90_get_att(nc_context%ncid, NF90_GLOBAL, "mf6_modeltype", &
                     modeltype) == NF90_NOERR) then
      !
      call upcase(modeltype)
      ! -- verify modeltype matches this model
      if (nc_context%modeltype /= modeltype) then
        errmsg = 'NC input file model type does not match namefile model type'
        call store_error(errmsg)
        call store_error_filename(nc_context%modelfname)
      end if
      !
    else
      errmsg = 'NC input file global attribute "mf6_modeltype" not found.'
      call store_error(errmsg)
      call store_error_filename(nc_context%modelfname)
      !
    end if
    !
    ! -- verify expected mf6_modelname file attribute
    if (nf90_get_att(nc_context%ncid, NF90_GLOBAL, "mf6_modelname", &
                     modelname) == NF90_NOERR) then
      !
      call upcase(modelname)
      ! -- verify modelname matches this model
      if (nc_context%modelname /= modelname) then
        errmsg = 'NC input file model name does not match namefile model name'
        call store_error(errmsg)
        call store_error_filename(nc_context%modelfname)
      end if
      !
    else
      errmsg = 'NC input file global attribute "mf6_modelname" not found.'
      call store_error(errmsg)
      call store_error_filename(nc_context%modelfname)
      !
    end if
    !
    ! -- return
    return
  end function verify_global_attr

  subroutine create_ncpkg_context(nc_context, iout)
    use InputOutputModule, only: lowcase, upcase
    type(NCModelInputsType), intent(inout) :: nc_context
    integer(I4B), intent(in) :: iout
    integer(I4B) :: ndim, nvar, nattr, unlimDimID, format
    integer(I4B), dimension(:), allocatable :: varids
    ! -- local
    character(len=LENCOMPONENTNAME) :: modelname
    integer(I4B) :: iparam, ncid
    !
    ncid = nc_context%ncid
    !
    ! -- verify expected global attributes
    modelname = verify_global_attr(nc_context)
    !
    ! -- inquire for root dataset info
    call nf_verify(nf90_inquire(ncid, ndim, nvar, nattr, unlimdimid, format), &
                   ncid, iout)
    !
    ! -- allocate and set varids
    allocate (varids(nvar))
    call nf_verify(nf90_inq_varids(ncid, nvar, varids), ncid, iout)
    !
    ! -- identify package variables; build block variable lists
    do iparam = 1, nvar
      !
      call add_package_var(nc_context, modelname, varids(iparam), iout)
      !
    end do
    !
    ! -- cleanup
    deallocate (varids)
    !
    ! -- return
    return
  end subroutine create_ncpkg_context

end module NCContextBuildModule

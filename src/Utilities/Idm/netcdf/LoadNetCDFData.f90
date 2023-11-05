module LoadNetCDFDataModule

  use netcdf
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMEMPATH, LENVARNAME, &
                             LENAUXNAME, LENBOUNDNAME, LENMODELNAME, &
                             LENPACKAGENAME, LENCOMPONENTNAME, DNODATA, &
                             INODATA, LENPACKAGETYPE, LENMEMADDRESS
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename
  use ArrayHandlersModule, only: expandarray
  use CharacterStringModule, only: CharacterStringType
  use InputDefinitionModule, only: InputParamDefinitionType
  use DefinitionSelectModule, only: get_param_definition_type, &
                                    package_scoped_param_dfn, &
                                    get_aggregate_definition_type
  use ModflowInputModule, only: ModflowInputType
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_setptr
  use MemoryHelperModule, only: create_mem_path
  use SimVariablesModule, only: idm_context
  use IdmLoggerModule, only: idm_log_var, idm_log_header, idm_log_close
  use NC4ModelInputsModule, only: NC4BlockVariablesType, &
                                  NC4ModelInputsType, &
                                  NC4ModelPackageInputType

  implicit none
  private
  public nc_fopen, nc_fclose
  public input_load, nc4_rp_list, nc4_rp_array
  public nc4_pkg_context

  integer(I4B), parameter :: IDM_NETCDF4_MAX_DIM = 6

contains

  function nc_fopen(nc_fname, iout) result(ncid)
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- return
    integer(I4B) :: ncid
    ! -- local
    !
    ! -- initialize
    ncid = -1
    !
    ! -- open netcdf file
    call nf_verify(nf90_open(nc_fname, NF90_NOWRITE, ncid), ncid, iout)
    write (iout, '(a,i0)') 'Model NetCDF4 Input "'//trim(nc_fname)// &
      '" opened NF90_NOWRITE, ncid=', ncid
    !
    ! -- return
    return
  end function nc_fopen

  subroutine nc_fclose(ncid, iout)
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: iout
    ! -- local
    !
    ! -- close netcdf file
    call nf_verify(nf90_close(ncid), ncid, iout)
    !
    ! -- return
    return
  end subroutine nc_fclose

  subroutine input_load(mf6_input, nc4pkg, ncid, nc_fname, iout)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(NC4ModelPackageInputType), pointer, intent(in) :: nc4pkg
    integer(I4B), intent(in) :: ncid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(:), contiguous, pointer :: mshape
    integer(I4B) :: iblock
    !
    ! -- set pointer to model shape
    if (mf6_input%pkgtype(1:3) /= 'DIS') then
      call mem_setptr(mshape, 'MODEL_SHAPE', mf6_input%component_mempath)
    else
      mshape => null()
    end if
    !
    ! -- log package load header
    call idm_log_header(mf6_input%component_name, &
                        mf6_input%subcomponent_name, iout)
    !
    ! -- load blocks until period
    do iblock = 1, size(nc4pkg%blocklist)
      if (nc4pkg%blocklist(iblock)%blockname == 'PERIOD') then
        !
        exit
        !
      else
        !
        if (nc4pkg%blocklist(iblock)%aggregate) then
          ! -- load list stype input
          call load_aggregate_block(mf6_input, nc4pkg, mshape, &
                                    iblock, ncid, nc_fname, iout)
        else
          ! -- load block variables
          call load_block_var(mf6_input, nc4pkg, mshape, &
                              iblock, ncid, nc_fname, iout)
        end if
        !
      end if
    end do
    !
    ! -- close logging
    call idm_log_close(mf6_input%component_name, &
                       mf6_input%subcomponent_name, iout)
    !
    ! -- return
    return
  end subroutine input_load

  subroutine load_aggregate_block(mf6_input, nc4pkg, mshape, &
                                  iblock, ncid, nc_fname, iout)
    use InputOutputModule, only: parseline
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(NC4ModelPackageInputType), pointer, intent(in) :: nc4pkg
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    integer(I4B), intent(in) :: iblock
    integer(I4B), intent(in) :: ncid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    type(InputParamDefinitionType), pointer :: idt, idta
    character(len=16), dimension(:), allocatable :: words
    character(len=:), allocatable :: parse_str
    integer(I4B) :: icol, nwords
    logical(LGP) :: found
    !
    ! -- set input definition for this block
    idta => get_aggregate_definition_type(mf6_input%aggregate_dfns, &
                                          mf6_input%component_type, &
                                          mf6_input%subcomponent_type, &
                                          mf6_input%block_dfns(iblock)%blockname)
    !
    ! -- identify variable names
    parse_str = trim(idta%datatype)//' '
    call parseline(parse_str, nwords, words)
    !
    !
    ! -- Skip first col which is RECARRAY
    do icol = 2, nwords
      !
      found = load_aggregate_var(mf6_input, nc4pkg, mshape, iblock, idta, &
                                 words(icol), ncid, nc_fname, iout)
      !
      ! -- ensure required params found
      if (.not. found) then
        if (idt%required) then
          errmsg = 'Required input parameter "'//trim(idt%tagname)// &
                   '" not found for package "'// &
                   trim(nc4pkg%subcomponent_name)//'".'
          call store_error(errmsg)
          call store_error_filename(nc_fname)
        end if
      end if
      !
    end do
    !
    ! -- cleanup
    if (allocated(parse_str)) deallocate (parse_str)
    if (allocated(words)) deallocate (words)
    !
    ! -- return
    return
  end subroutine load_aggregate_block

  function load_aggregate_var(mf6_input, nc4pkg, mshape, iblock, idta, &
                              tagname, ncid, nc_fname, iout) result(found)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(NC4ModelPackageInputType), pointer, intent(in) :: nc4pkg
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    integer(I4B), intent(in) :: iblock
    type(InputParamDefinitionType), pointer, intent(in) :: idta
    character(len=*), intent(in) :: tagname
    integer(I4B), intent(in) :: ncid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- return
    logical(LGP) :: found
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    type(InputParamDefinitionType), target :: idtp
    integer(I4B) :: iparam
    !
    ! -- initialize
    found = .false.
    !
    do iparam = 1, nc4pkg%blocklist(iblock)%varnum
      !
      ! -- set param definition
      idt => get_param_definition_type(mf6_input%param_dfns, &
                                       mf6_input%component_type, &
                                       mf6_input%subcomponent_type, &
                                       nc4pkg%blocklist(iblock)%blockname, &
                                       tagname, nc_fname)

      if (nc4pkg%blocklist(iblock)%varnames(iparam) == trim(tagname)) then
        !
        ! -- set found
        found = .true.
        !
        ! -- adjust definition to reflect list style input
        idtp = aggregate_param_dfn(idta, idt, nc_fname)
        idt => idtp
        !
        ! -- load
        if (idt%datatype == 'IRREGINT1D') then
          call load_varint1d_type(mf6_input, mshape, idt, ncid, &
                                  nc4pkg%blocklist(iblock)%varids(iparam), iout)
        else
          call load_static_var(mf6_input, idt, mshape, ncid, &
                               nc4pkg%blocklist(iblock)%varids(iparam), &
                               nc_fname, iout)
        end if
        !
        exit
      end if
    end do
    !
    ! -- return
    return
  end function load_aggregate_var

  subroutine load_block_var(mf6_input, nc4pkg, mshape, &
                            iblock, ncid, nc_fname, iout)
    use SourceCommonModule, only: set_model_shape, mem_allocate_naux
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(NC4ModelPackageInputType), pointer, intent(in) :: nc4pkg
    integer(I4B), dimension(:), contiguous, pointer, intent(inout) :: mshape
    integer(I4B), intent(in) :: iblock
    integer(I4B), intent(in) :: ncid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam
    !
    ! -- load variables in block order
    do iparam = 1, nc4pkg%blocklist(iblock)%varnum
      !
      ! -- TODO: ensure nc file mf6_param is always using tagname
      idt => &
        get_param_definition_type(mf6_input%param_dfns, &
                                  mf6_input%component_type, &
                                  mf6_input%subcomponent_type, &
                                  nc4pkg%blocklist(iblock)%blockname, &
                                  nc4pkg%blocklist(iblock)%varnames(iparam), &
                                  nc_fname)
      ! -- load the variable data
      call load_static_var(mf6_input, idt, mshape, ncid, &
                           nc4pkg%blocklist(iblock)%varids(iparam), &
                           nc_fname, iout)
      !
    end do
    !
    ! -- block post processing
    select case (nc4pkg%blocklist(iblock)%blockname)
    case ('OPTIONS')
      ! -- allocate naux and set to 0 if not allocated
      do iparam = 1, size(mf6_input%param_dfns)
        idt => mf6_input%param_dfns(iparam)
        !
        if (idt%blockname == 'OPTIONS' .and. &
            idt%tagname == 'AUXILIARY') then
          call mem_allocate_naux(mf6_input%mempath)
          exit
        end if
      end do
    case ('DIMENSIONS')
      ! -- set model shape if discretization dimensions have been read
      if (mf6_input%pkgtype(1:3) == 'DIS') then
        call set_model_shape(mf6_input%pkgtype, nc_fname, &
                             mf6_input%component_mempath, &
                             mf6_input%mempath, mshape)
      end if
    case default
    end select
    !
    ! -- return
    return
  end subroutine load_block_var

  function aggregate_param_dfn(idta, idtp, nc_fname) result(idt)
    type(InputParamDefinitionType), pointer, intent(in) :: idta
    type(InputParamDefinitionType), pointer, intent(in) :: idtp
    character(len=*), intent(in) :: nc_fname
    type(InputParamDefinitionType) :: idt !< input data type object describing this record
    !
    !
    ! -- copy from input dfn
    idt%component_type = trim(idtp%component_type)
    idt%subcomponent_type = trim(idtp%subcomponent_type)
    idt%blockname = trim(idtp%blockname)
    idt%tagname = trim(idtp%tagname)
    idt%mf6varname = trim(idtp%mf6varname)
    idt%shape = idta%shape
    idt%required = idtp%required
    idt%in_record = idtp%in_record
    idt%preserve_case = idtp%preserve_case
    idt%layered = idtp%layered
    idt%timeseries = idtp%timeseries
    !
    ! -- set datatype
    select case (idtp%datatype)
    case ('INTEGER')
      idt%datatype = 'INTEGER1D'
    case ('DOUBLE')
      idt%datatype = 'DOUBLE1D'
    case ('INTEGER1D')
      if (idt%mf6varname == 'ICVERT') then
        ! -- jagged array dependent on NCVERT 1d shape array
        idt%datatype = 'IRREGINT1D'
        idt%shape = idtp%shape
      else
        errmsg = 'IDM UNIMPLEMENTED LoadNetCDFData::aggregate_param_dfn &
                 &tagname='//trim(idt%tagname)
        call store_error(errmsg)
        call store_error_filename(nc_fname)
      end if
    case default
      errmsg = 'IDM UNIMPLEMENTED LoadNetCDFData::aggregate_param_dfn &
               &datatype='//trim(idtp%datatype)
      call store_error(errmsg)
      call store_error_filename(nc_fname)
    end select
    !
    ! -- return
    return
  end function aggregate_param_dfn

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

  subroutine load_static_var(mf6_input, idt, mshape, ncid, varid, nc_fname, iout)
    type(ModflowInputType), intent(in) :: mf6_input
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    !
    ! -- allocate and load data type
    select case (idt%datatype)
    case ('KEYWORD')
      call load_keyword_type(mf6_input, idt, ncid, varid, iout)
    case ('STRING')
      if (idt%shape == 'NAUX') then
        call load_auxvar_names(mf6_input, idt, ncid, varid, iout)
      else
        call load_string_type(mf6_input, idt, ncid, varid, iout)
      end if
    case ('INTEGER')
      call load_integer_type(mf6_input, idt, ncid, varid, iout)
    case ('INTEGER1D')
      call load_integer1d_type(mf6_input, mshape, idt, ncid, &
                               varid, nc_fname, iout)
    case ('INTEGER2D')
      call load_integer2d_type(mf6_input, mshape, idt, ncid, varid, iout)
    case ('INTEGER3D')
      call load_integer3d_type(mf6_input, mshape, idt, ncid, varid, iout)
    case ('DOUBLE')
      call load_double_type(mf6_input, idt, ncid, varid, iout)
    case ('DOUBLE1D')
      call load_double1d_type(mf6_input, mshape, idt, ncid, varid, nc_fname, iout)
    case ('DOUBLE2D')
      call load_double2d_type(mf6_input, mshape, idt, ncid, varid, iout)
    case ('DOUBLE3D')
      call load_double3d_type(mf6_input, mshape, idt, ncid, varid, iout)
    case default
      if (idt%datatype(1:6) == 'RECORD') then
        call load_record_type(mf6_input, idt, ncid, varid, nc_fname, iout)
      else
        errmsg = 'IDM UNIMPLEMENTED LoadNetCDFData::load_static_var &
                 &datatype='//trim(idt%datatype)
        call store_error(errmsg)
        call store_error_filename(nc_fname)
      end if
    end select
    !
    ! -- return
    return
  end subroutine load_static_var

  !> @brief load type keyword
  !<
  subroutine load_keyword_type(mf6_input, idt, ncid, varid, iout)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), pointer :: intvar
    !
    call mem_allocate(intvar, idt%mf6varname, mf6_input%mempath)
    intvar = 1
    call idm_log_var(intvar, idt%mf6varname, mf6_input%mempath, &
                     idt%datatype, iout)
    !
    return
  end subroutine load_keyword_type

  !> @brief load type record
  !!
  !! Currently only supports file record types
  !<
  subroutine load_record_type(mf6_input, idt, ncid, varid, nc_fname, iout)
    use InputOutputModule, only: parseline
    use MemoryManagerModule, only: get_isize
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    type(InputParamDefinitionType), pointer :: io_idt
    character(len=40), dimension(:), allocatable :: words
    character(len=:), allocatable :: parse_str
    character(len=LINELENGTH) :: cstr
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: charstr1d
    integer(I4B) :: nwords, isize, idx
    logical(LGP) :: found
    !
    ! -- initialization
    found = .false.
    !
    ! -- set split string
    parse_str = trim(idt%datatype)//' '
    !
    ! -- split
    call parseline(parse_str, nwords, words)
    !
    ! -- a filein/fileout record tag definition has 4 tokens
    if (nwords == 4) then
      !
      ! -- verify third definition token is FILEIN/FILEOUT
      if (words(3) == 'FILEIN' .or. words(3) == 'FILEOUT') then
        !
        ! -- matches, read and load file name
        io_idt => &
          get_param_definition_type(mf6_input%param_dfns, &
                                    mf6_input%component_type, &
                                    mf6_input%subcomponent_type, &
                                    'OPTIONS', words(4), nc_fname)
        !
        ! -- io tag loaded
        found = .true.
      end if
    end if
    !
    ! -- load the data if found
    if (found) then
      !
      if (words(3) == 'FILEIN') then
        !
        ! -- FILEIN record types support 1+ file specifications
        call get_isize(io_idt%mf6varname, mf6_input%mempath, isize)
        !
        ! -- allocate / reallocate
        if (isize < 0) then
          ! -- does not exist, allocate
          call mem_allocate(charstr1d, LINELENGTH, 1, io_idt%mf6varname, &
                            mf6_input%mempath)
          idx = 1
        else
          ! -- exists, reallocate
          call mem_setptr(charstr1d, io_idt%mf6varname, mf6_input%mempath)
          call mem_reallocate(charstr1d, LINELENGTH, isize + 1, &
                              io_idt%mf6varname, mf6_input%mempath)
          idx = isize + 1
        end if
        !
        ! -- read and set the data
        call nf_verify(nf90_get_var(ncid, varid, cstr), ncid, iout)
        charstr1d(idx) = trim(cstr)
        !
      else if (words(3) == 'FILEOUT') then
        ! -- FILEOUT record types support a single file specification
        call load_string_type(mf6_input, io_idt, ncid, varid, iout)
      end if
      !
    else
      errmsg = 'IDM UNIMPLEMENTED LoadNetCDFData::load_record_type &
               &tagname='//trim(idt%tagname)
      call store_error(errmsg)
      call store_error_filename(nc_fname)
    end if
    !
    ! -- cleanup
    if (allocated(parse_str)) deallocate (parse_str)
    if (allocated(words)) deallocate (words)
    !
    return
  end subroutine load_record_type

  !> @brief load type string
  !<
  subroutine load_string_type(mf6_input, idt, ncid, varid, iout)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LINELENGTH), pointer :: cstr
    !
    call mem_allocate(cstr, LINELENGTH, idt%mf6varname, mf6_input%mempath)
    call nf_verify(nf90_get_var(ncid, varid, cstr), ncid, iout)
    call idm_log_var(cstr, idt%mf6varname, mf6_input%mempath, iout)
    !
    return
  end subroutine load_string_type

  subroutine load_auxvar_names(mf6_input, idt, ncid, varid, iout)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LENAUXNAME), dimension(:), pointer :: cstr1d
    type(CharacterStringType), dimension(:), &
      pointer, contiguous :: charstr1d !< variable for allocation
    integer(I4B), pointer :: naux
    character(len=LINELENGTH) :: varname, dimname
    integer(I4B) :: vartype, ndims, nattr, n
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM) :: dimids, dimlens
    !
    ! -- inquire for variable info
    call nf_verify(nf90_inquire_variable(ncid, varid, varname, vartype, ndims, &
                                         dimids, nattr), ncid, iout)
    !
    ! -- set variable dimensions
    do n = 1, ndims
      ! -- dim 1 is strlen, dim 2 is naux
      call nf_verify(nf90_inquire_dimension(ncid, dimids(n), dimname, &
                                            dimlens(n)), ncid, iout)
    end do
    !
    ! -- allocate and set naux
    call mem_allocate(naux, idt%shape, mf6_input%mempath)
    naux = dimlens(2)
    !
    ! -- allocate local string array
    allocate (cstr1d(naux))
    !
    ! -- load data
    call nf_verify(nf90_get_var(ncid, varid, cstr1d), ncid, iout)
    !
    ! -- allocate charstring array
    call mem_allocate(charstr1d, LENAUXNAME, naux, &
                      idt%mf6varname, mf6_input%mempath)
    !
    ! --copy local string input to charstr array
    do n = 1, naux
      charstr1d(n) = trim(cstr1d(n))
    end do
    !
    ! -- cleanup
    deallocate (cstr1d)
    !
    return
  end subroutine load_auxvar_names

  !> @brief load type integer
  !<
  subroutine load_integer_type(mf6_input, idt, ncid, varid, iout)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), pointer :: intvar
    !
    call mem_allocate(intvar, idt%mf6varname, mf6_input%mempath)
    call nf_verify(nf90_get_var(ncid, varid, intvar), ncid, iout)
    call idm_log_var(intvar, idt%mf6varname, mf6_input%mempath, &
                     idt%datatype, iout)
    !
    return
  end subroutine load_integer_type

  !> @brief load type 1d integer
  !<
  subroutine load_integer1d_type(mf6_input, mshape, idt, ncid, &
                                 varid, nc_fname, iout)
    use SourceCommonModule, only: get_shape_from_string
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(:), contiguous, pointer :: int1d
    integer(I4B), dimension(:, :), contiguous, pointer :: int2d
    integer(I4B), dimension(:, :, :), contiguous, pointer :: int3d
    integer(I4B), dimension(:), allocatable :: array_shape
    integer(I4B) :: nvals, i, j, k, n
    !
    ! -- initialize
    n = 0
    nvals = 0
    !
    if (idt%shape == 'NODES') then
      ! -- set number of values
      nvals = product(mshape)
      !
      ! -- allocate managed memory
      call mem_allocate(int1d, nvals, idt%mf6varname, mf6_input%mempath)
      !
      if (size(mshape) == 3) then
        ! -- allocate local array
        allocate (int3d(mshape(3), mshape(2), mshape(1)))
        ! -- read the data into local array
        call nf_verify(nf90_get_var(ncid, varid, int3d), ncid, iout)
        !
        ! -- copy to flat array
        do k = 1, size(int3d, dim=3)
          do j = 1, size(int3d, dim=2)
            do i = 1, size(int3d, dim=1)
              n = n + 1
              int1d(n) = int3d(i, j, k)
            end do
          end do
        end do
        !
        ! -- deallocate local array
        deallocate (int3d)
        !
      else if (size(mshape) == 2) then
        ! -- allocate local array
        allocate (int2d(mshape(2), mshape(1)))
        ! -- read the data into local array
        call nf_verify(nf90_get_var(ncid, varid, int2d), ncid, iout)
        !
        ! -- copy to flat array
        do j = 1, size(int2d, dim=2)
          do i = 1, size(int2d, dim=1)
            n = n + 1
            int1d(n) = int2d(i, j)
          end do
        end do
        !
        ! -- deallocate local array
        deallocate (int2d)
        !
      else
        write (errmsg, '(a,i0)') &
          'IDM UNIMPLEMENTED LoadNetCDFData::load_integer1d_type &
          &size mshape=', size(mshape)
        call store_error(errmsg)
        call store_error_filename(nc_fname)
      end if
    else
      ! -- interpret shape
      call get_shape_from_string(idt%shape, array_shape, mf6_input%mempath)
      !
      ! -- set nvals
      nvals = array_shape(1)
      !
      ! -- allocate managed memory
      call mem_allocate(int1d, nvals, idt%mf6varname, mf6_input%mempath)
      !
      ! -- read and set data
      call nf_verify(nf90_get_var(ncid, varid, int1d), ncid, iout)
      !
    end if
    !
    ! -- log variable
    call idm_log_var(int1d, idt%mf6varname, mf6_input%mempath, iout)
    !
    return
  end subroutine load_integer1d_type

  !> @brief load intvector type
  !<
  subroutine load_varint1d_type(mf6_input, mshape, idt, ncid, varid, iout)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(:), pointer, contiguous :: int1d, intvector_shape
    character(len=LINELENGTH) :: varname, dimname
    integer(I4B) :: vartype, ndims, nattr, n, numvals
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM) :: dimids, dimlens
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
    ! -- set pointer to shape array
    call mem_setptr(intvector_shape, idt%shape, mf6_input%mempath)
    !
    ! -- set total number of values
    numvals = sum(intvector_shape)
    !
    ! -- TODO check numvals against dimlens(1)?
    !
    ! -- allocate managed memory
    call mem_allocate(int1d, dimlens(1), idt%mf6varname, mf6_input%mempath)
    !
    ! -- read data into managed memory
    call nf_verify(nf90_get_var(ncid, varid, int1d), ncid, iout)
    !
    ! -- log variable
    call idm_log_var(int1d, idt%mf6varname, mf6_input%mempath, iout)
    !
    ! -- return
    return
  end subroutine load_varint1d_type

  !> @brief load type 2d integer
  !<
  subroutine load_integer2d_type(mf6_input, mshape, idt, ncid, varid, iout)
    use SourceCommonModule, only: get_shape_from_string
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(:, :), pointer, contiguous :: int2d
    integer(I4B), dimension(:), allocatable :: array_shape
    integer(I4B) :: nsize1, nsize2
    !
    ! -- determine the array shape from the input data defintion
    !    idt%shape, which looks like "NCOL, NROW, NLAY"
    call get_shape_from_string(idt%shape, array_shape, mf6_input%mempath)
    nsize1 = array_shape(1)
    nsize2 = array_shape(2)
    !
    ! -- allocate, read and log
    call mem_allocate(int2d, nsize1, nsize2, idt%mf6varname, mf6_input%mempath)
    call nf_verify(nf90_get_var(ncid, varid, int2d), ncid, iout)
    call idm_log_var(int2d, idt%mf6varname, mf6_input%mempath, iout)
    !
    ! -- return
    return
  end subroutine load_integer2d_type

  !> @brief load type 3d integer
  !<
  subroutine load_integer3d_type(mf6_input, mshape, idt, ncid, varid, iout)
    use SourceCommonModule, only: get_shape_from_string
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(:, :, :), pointer, contiguous :: int3d
    integer(I4B), dimension(:), allocatable :: array_shape
    integer(I4B) :: nsize1, nsize2, nsize3
    !
    ! -- determine the array shape from the input data defintion
    !    idt%shape, which looks like "NCOL, NROW, NLAY"
    call get_shape_from_string(idt%shape, array_shape, mf6_input%mempath)
    nsize1 = array_shape(1)
    nsize2 = array_shape(2)
    nsize3 = array_shape(3)
    !
    ! -- allocate, read and log
    call mem_allocate(int3d, nsize1, nsize2, nsize3, idt%mf6varname, &
                      mf6_input%mempath)
    call nf_verify(nf90_get_var(ncid, varid, int3d), ncid, iout)
    call idm_log_var(int3d, idt%mf6varname, mf6_input%mempath, iout)
    !
    ! -- return
    return
  end subroutine load_integer3d_type

  !> @brief load type double
  !<
  subroutine load_double_type(mf6_input, idt, ncid, varid, iout)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iout
    ! -- local
    real(DP), pointer :: dblvar
    !
    call mem_allocate(dblvar, idt%mf6varname, mf6_input%mempath)
    call nf_verify(nf90_get_var(ncid, varid, dblvar), ncid, iout)
    call idm_log_var(dblvar, idt%mf6varname, mf6_input%mempath, iout)
    !
    ! -- return
    return
  end subroutine load_double_type

  !> @brief load type 1d double
  !<
  subroutine load_double1d_type(mf6_input, mshape, idt, ncid, &
                                varid, nc_fname, iout)
    use SourceCommonModule, only: get_shape_from_string
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    real(DP), dimension(:), contiguous, pointer :: dbl1d
    real(DP), dimension(:, :), contiguous, pointer :: dbl2d
    real(DP), dimension(:, :, :), contiguous, pointer :: dbl3d
    integer(I4B), dimension(:), allocatable :: array_shape
    integer(I4B) :: nvals, i, j, k, n
    !
    ! -- initialize
    n = 0
    nvals = 0
    !
    if (idt%shape == 'NODES') then
      ! -- set number of values
      nvals = product(mshape)
      !
      ! -- allocate managed memory
      call mem_allocate(dbl1d, nvals, idt%mf6varname, mf6_input%mempath)
      !
      if (size(mshape) == 3) then
        ! -- allocate local array
        allocate (dbl3d(mshape(3), mshape(2), mshape(1)))
        ! -- read the data into local array
        call nf_verify(nf90_get_var(ncid, varid, dbl3d), ncid, iout)
        !
        ! -- copy to flat array
        do k = 1, size(dbl3d, dim=3)
          do j = 1, size(dbl3d, dim=2)
            do i = 1, size(dbl3d, dim=1)
              n = n + 1
              dbl1d(n) = dbl3d(i, j, k)
            end do
          end do
        end do
        !
        ! -- deallocate local array
        deallocate (dbl3d)
        !
      else if (size(mshape) == 2) then
        ! -- allocate local array
        allocate (dbl2d(mshape(2), mshape(1)))
        ! -- read the data into local array
        call nf_verify(nf90_get_var(ncid, varid, dbl2d), ncid, iout)
        !
        ! -- copy to flat array
        do j = 1, size(dbl2d, dim=2)
          do i = 1, size(dbl2d, dim=1)
            n = n + 1
            dbl1d(n) = dbl2d(i, j)
          end do
        end do
        !
        ! -- deallocate local array
        deallocate (dbl2d)
        !
      else
        write (errmsg, '(a,i0)') &
          'IDM UNIMPLEMENTED LoadNetCDFData::load_double1d_type &
          &size mshape=', size(mshape)
        call store_error(errmsg)
        call store_error_filename(nc_fname)
      end if
    else
      ! -- interpret shape
      call get_shape_from_string(idt%shape, array_shape, mf6_input%mempath)
      !
      ! -- set nvals
      nvals = array_shape(1)
      !
      ! -- allocate managed memory
      call mem_allocate(dbl1d, nvals, idt%mf6varname, mf6_input%mempath)
      !
      ! -- read and set data
      call nf_verify(nf90_get_var(ncid, varid, dbl1d), ncid, iout)
    end if
    !
    ! -- log variable
    call idm_log_var(dbl1d, idt%mf6varname, mf6_input%mempath, iout)
    !
    ! -- return
    return
  end subroutine load_double1d_type

  !> @brief load type 2d double
  !<
  subroutine load_double2d_type(mf6_input, mshape, idt, ncid, varid, iout)
    use SourceCommonModule, only: get_shape_from_string
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iout
    ! -- local
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    integer(I4B), dimension(:), allocatable :: array_shape
    integer(I4B) :: nsize1, nsize2
    !
    ! -- determine the array shape from the input dfn shape
    call get_shape_from_string(idt%shape, array_shape, mf6_input%mempath)
    nsize1 = array_shape(1)
    nsize2 = array_shape(2)
    !
    call mem_allocate(dbl2d, nsize1, nsize2, idt%mf6varname, mf6_input%mempath)
    call nf_verify(nf90_get_var(ncid, varid, dbl2d), ncid, iout)
    call idm_log_var(dbl2d, idt%mf6varname, mf6_input%mempath, iout)
    !
    ! -- return
    return
  end subroutine load_double2d_type

  !> @brief load type 3d double
  !<
  subroutine load_double3d_type(mf6_input, mshape, idt, ncid, varid, iout)
    use SourceCommonModule, only: get_shape_from_string
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    type(InputParamDefinitionType), intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iout
    ! -- local
    real(DP), dimension(:, :, :), pointer, contiguous :: dbl3d
    integer(I4B), dimension(:), allocatable :: array_shape
    integer(I4B) :: nsize1, nsize2, nsize3
    !
    ! -- determine the array shape from the input dfn shape
    call get_shape_from_string(idt%shape, array_shape, mf6_input%mempath)
    nsize1 = array_shape(1)
    nsize2 = array_shape(2)
    nsize3 = array_shape(3)
    !
    call mem_allocate(dbl3d, nsize1, nsize2, nsize3, idt%mf6varname, &
                      mf6_input%mempath)
    call nf_verify(nf90_get_var(ncid, varid, dbl3d), ncid, iout)
    call idm_log_var(dbl3d, idt%mf6varname, mf6_input%mempath, iout)
    !
    return
  end subroutine load_double3d_type

  subroutine nc4_rp_list(mf6_input, ncpkg, bndctx, ncid, iiper, nc_fname, iout)
    use TdisModule, only: kper
    use BoundInputContextModule, only: BoundInputContextType
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(NC4ModelPackageInputType), intent(in) :: ncpkg
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
          tagname = ncpkg%blocklist(iblock)%varnames(iparam)
          !
          idt => &
            get_param_definition_type(mf6_input%param_dfns, &
                                      mf6_input%component_type, &
                                      mf6_input%subcomponent_type, &
                                      'PERIOD', tagname, nc_fname)
          !
          varid = ncpkg%blocklist(iblock)%varids(iparam)
          nbound = load_var_rp_list(mf6_input, ncid, idt, mshape, &
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
          !
        end do
      end if
    end do
    !
    ! -- set nbound
    bndctx%nbound = nbound
    !
    ! -- return
    return
  end subroutine nc4_rp_list

  function load_var_rp_list(mf6_input, ncid, idt, mshape, &
                            varid, iiper, nc_fname, iout) result(nbound)
    use BoundInputContextModule, only: BoundInputContextType
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
      nbound = load_charstr1d_rp_list(mf6_input, ncid, idt, mshape, &
                                      dimlens, varid, iiper, iout)
      !
    case ('INTEGER')
      nbound = load_int1d_rp_list(mf6_input, ncid, idt, mshape, &
                                  dimlens, varid, iiper, iout)
      !
    case ('INTEGER1D')
      nbound = load_int2d_rp_list(mf6_input, ncid, idt, mshape, &
                                  dimlens, varid, iiper, iout)
      !
    case ('DOUBLE')
      nbound = load_dbl1d_rp_list(mf6_input, ncid, idt, mshape, &
                                  dimlens, varid, iiper, iout)
      !
    case ('DOUBLE1D')
      nbound = load_dbl2d_rp_list(mf6_input, ncid, idt, mshape, &
                                  dimlens, varid, iiper, iout)
      !
    case default
      errmsg = 'IDM UNIMPLEMENTED LoadNetCDFData::load_var_rp_list &
               &datatype='//trim(idt%datatype)
      call store_error(errmsg)
      call store_error_filename(nc_fname)
    end select
    !
    ! -- return
    return
  end function load_var_rp_list

  function load_charstr1d_rp_list(mf6_input, ncid, idt, mshape, dimlens, &
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
  end function load_charstr1d_rp_list

  function load_int1d_rp_list(mf6_input, ncid, idt, mshape, dimlens, &
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
  end function load_int1d_rp_list

  function load_int2d_rp_list(mf6_input, ncid, idt, mshape, dimlens, &
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
  end function load_int2d_rp_list

  function load_dbl1d_rp_list(mf6_input, ncid, idt, mshape, dimlens, &
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
  end function load_dbl1d_rp_list

  function load_dbl2d_rp_list(mf6_input, ncid, idt, mshape, dimlens, &
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
  end function load_dbl2d_rp_list

  subroutine nc4_rp_array(mf6_input, ncpkg, param_names, param_reads, &
                          bndctx, ncid, nc_fname, iout)
    use BoundInputContextModule, only: BoundInputContextType
    use SourceCommonModule, only: ReadStateVarType
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(NC4ModelPackageInputType), intent(in) :: ncpkg
    character(len=LENVARNAME), dimension(:), &
      allocatable :: param_names !< dynamic param names
    type(ReadStateVarType), dimension(:), allocatable :: param_reads
    type(BoundInputContextType), intent(in) :: bndctx
    integer(I4B), intent(in) :: ncid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam, iblock, varid
    character(len=LINELENGTH) :: tagname
    !
    do iblock = 1, size(ncpkg%blocklist)
      if (ncpkg%blocklist(iblock)%blockname == 'PERIOD') then
        do iparam = 1, ncpkg%blocklist(iblock)%varnum
          !
          ! -- set varid
          varid = ncpkg%blocklist(iblock)%varids(iparam)
          !
          ! -- set tagname
          tagname = ncpkg%blocklist(iblock)%varnames(iparam)
          !
          idt => &
            get_param_definition_type(mf6_input%param_dfns, &
                                      mf6_input%component_type, &
                                      mf6_input%subcomponent_type, &
                                      'PERIOD', tagname, nc_fname)
          !
          call load_var_rp_array(mf6_input, param_names, &
                                 param_reads, ncid, idt, varid, &
                                 nc_fname, iout)
        end do
      end if
    end do
    !
    ! -- return
    return
  end subroutine nc4_rp_array

  subroutine load_var_rp_array(mf6_input, param_names, param_reads, &
                               ncid, idt, varid, nc_fname, iout)
    use TdisModule, only: kper
    use BoundInputContextModule, only: BoundInputContextType
    use SourceCommonModule, only: ReadStateVarType
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=LENVARNAME), dimension(:), &
      allocatable :: param_names !< dynamic param names
    type(ReadStateVarType), dimension(:), allocatable :: param_reads
    integer(I4B), intent(in) :: ncid
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), intent(in) :: varid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: n, load_idx, len
    character(len=LINELENGTH) :: varname, dimname
    integer(I4B) :: vartype, ndims, nattr
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM) :: dimids, dimlens
    integer(I4B), dimension(:), allocatable :: ipers
    !
    ! -- index for loading kper data
    load_idx = 0
    !
    ! -- inquire for variable info
    call nf_verify(nf90_inquire_variable(ncid, varid, varname, vartype, ndims, &
                                         dimids, nattr), ncid, iout)
    !
    ! -- set variable dimensions
    do n = 1, ndims
      !
      ! -- (1+: data, last-1: maxbound, last:ipers)
      call nf_verify(nf90_inquire_dimension(ncid, dimids(n), dimname, &
                                            dimlens(n)), ncid, iout)
    end do
    !
    ! -- set current load index
    if (nf90_inquire_attribute(ncid, varid, "mf6_iper", &
                               len=len) == NF90_NOERR) then
      ! -- allocate local array
      allocate (ipers(len))
      ! -- read variable ipers array
      if (nf90_get_att(ncid, varid, "mf6_iper", &
                       ipers) == NF90_NOERR) then
        ! -- check load periods
        do n = 1, size(ipers)
          !
          if (ipers(n) == kper) then
            ! -- load, save index
            load_idx = n
            exit
          end if
        end do
      end if
    end if
    !
    ! -- read and load variable data if load_idx defined
    if (load_idx > 0) then
      select case (idt%datatype)
        !case ('INTEGER1D')
        !  call load_int3d_rp_array(mf6_input, ncid, idt, &
        !                           dimlens, varid, load_idx, iout)
        !
      case ('DOUBLE1D')
        call load_dbl3d_rp_array(mf6_input, ncid, param_names, param_reads, &
                                 idt, dimlens, varid, load_idx, iout)
        !
      case default
        errmsg = 'IDM UNIMPLEMENTED LoadNetCDFData::load_var_rp_array &
                 &datatype='//trim(idt%datatype)
        call store_error(errmsg)
        call store_error_filename(nc_fname)
      end select
    end if
    !
    ! -- return
    return
  end subroutine load_var_rp_array

!  subroutine load_int3d_rp_array(mf6_input, ncid, idt, &
!                                 dimlens, varid, load_idx, iout)
!    ! -- dummy
!    type(ModflowInputType), intent(in) :: mf6_input
!    integer(I4B), intent(in) :: ncid
!    type(InputParamDefinitionType), pointer, intent(in) :: idt
!    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM), intent(in)  :: dimlens
!    integer(I4B), intent(in) :: varid
!    integer(I4B), intent(in) :: load_idx
!    integer(I4B), intent(in) :: iout
!    ! -- local
!    integer(I4B), dimension(:), pointer, contiguous :: int1d
!    !
!    ! -- return
!    return
!  end subroutine load_int3d_rp_array

  subroutine load_dbl3d_rp_array(mf6_input, ncid, param_names, param_reads, &
                                 idt, dimlens, varid, load_idx, iout)
    ! -- modules
    use SourceCommonModule, only: ReadStateVarType
    use ArrayHandlersModule, only: ifind
    use SourceCommonModule, only: ifind_charstr
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), intent(in) :: ncid
    character(len=LENVARNAME), dimension(:), &
      allocatable :: param_names !< dynamic param names
    type(ReadStateVarType), dimension(:), allocatable :: param_reads
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM), intent(in) :: dimlens
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: load_idx
    integer(I4B), intent(in) :: iout
    ! -- local
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    real(DP), dimension(:, :, :), pointer, contiguous :: dbl3d
    integer(I4B), dimension(:), pointer, contiguous :: ialayer
    integer(I4B) :: n, i, j, k, layer, iparam, ilayer
    !
    ! -- initialize
    n = 0
    layer = 1
    ilayer = 0
    nullify (ialayer)
    !
    ! -- TEMP set layer index array
    ilayer = ifind(param_names, 'IRCH')
    if (ilayer <= 0) iparam = ifind(param_names, 'IEVT')
    if (ilayer > 0) then
      call mem_setptr(ialayer, param_names(ilayer), mf6_input%mempath)
    end if
    !
    ! -- set iparam to name index
    iparam = ifind(param_names, idt%tagname)
    !
    ! -- set pointer to managed memory variable
    call mem_setptr(dbl1d, idt%mf6varname, mf6_input%mempath)
    !
    ! -- allocate local array
    allocate (dbl3d(dimlens(3), dimlens(2), dimlens(1)))
    !
    ! -- read and load data to local array
    ! -- dims: (1: ncol, 2: nrow, 3: nlay, 4: iper)
    call nf_verify(nf90_get_var(ncid, varid, dbl3d, &
                                start=(/1, 1, 1, load_idx/), &
                                count=(/dimlens(1), dimlens(2), &
                                        dimlens(3), 1/)), ncid, iout)
    !
    ! -- copy data from local array to managed memory
    do k = 1, size(dbl3d, dim=3)
      do j = 1, size(dbl3d, dim=2)
        do i = 1, size(dbl3d, dim=1)
          if (n < size(dbl1d)) then
            n = n + 1
          else
            n = 1
            layer = layer + 1
          end if
          if (dbl3d(i, j, k) /= DNODATA) then
            dbl1d(n) = dbl3d(i, j, k)
            if (ilayer > 0) ialayer(n) = layer
          end if
        end do
      end do
    end do
    !
    ! -- set read state vars
    if (iparam > 0) then
      param_reads(iparam)%invar = 1
    end if
    if (ilayer > 0) then
      param_reads(ilayer)%invar = 1
    end if
    !
    ! -- deallocate local array
    deallocate (dbl3d)
    !
    ! -- return
    return
  end subroutine load_dbl3d_rp_array

  subroutine nf_verify(res, ncid, iout)
    integer(I4B), intent(in) :: res
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: iout
    ! -- local variables
    character(len=LINELENGTH) :: errstr
    !
    ! -- strings are set for a subset of errors
    !    but the exit status will always be reported
    if (res /= NF90_NOERR) then
      !
      select case (res)
      case (NF90_EINVAL) ! (-36)
        errstr = 'Invalid Argument'
      case (NF90_EPERM) ! (-37)
        errstr = 'Write to read only'
      case (NF90_EINVALCOORDS) ! (-40)
        errstr = 'Index exceeds dimension bound'
      case (NF90_ENAMEINUSE) ! (-42)
        errstr = 'String match to name in use'
      case (NF90_ENOTATT) ! (-43)
        errstr = 'Attribute not found'
      case (NF90_EBADDIM) ! (-46)
        errstr = 'Invalid dimension id or name'
      case (NF90_ENOTVAR) ! (-49)
        errstr = 'Variable not found'
      case (NF90_ENOTNC) ! (-51)
        errstr = 'Not a netcdf file'
      case (NF90_ECHAR) ! (-56)
        errstr = 'Attempt to convert between text & numbers'
      case (NF90_EEDGE) ! (-57)
        errstr = 'Edge+start exceeds dimension bound'
      case (NF90_ESTRIDE) ! (-58)
        errstr = 'Illegal stride'
      case (NF90_EBADNAME) ! (-59)
        errstr = 'Attribute or variable name contains illegal characters'
      case default
        errstr = ''
      end select
      !
      if (errstr /= '') then
        write (errmsg, '(a,a,a,i0,a,i0,a)') 'NETCDF4 library error [error="', &
          trim(errstr), '", exit code=', res, ', ncid=', ncid, '].'
      else
        write (errmsg, '(a,i0,a,i0,a)') 'NETCDF4 library error [exit code=', &
          res, ', ncid=', ncid, '].'
      end if
      !
      call store_error(errmsg, .true.)
    end if
    !
    ! -- return
    return
  end subroutine nf_verify

  !> @brief get package list
  !<
  function get_nc4_package(nc4_context, modelname, scname, &
                           ptype, sctype, iout) result(ncpkg)
    ! -- dummy variables
    type(NC4ModelInputsType), intent(in) :: nc4_context
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: scname
    character(len=*), intent(in) :: ptype
    character(len=*), intent(in) :: sctype
    integer(I4B), intent(in) :: iout
    ! -- return
    class(NC4ModelPackageInputType), pointer :: ncpkg
    ! -- local variables
    class(NC4ModelPackageInputType), pointer :: pkg
    integer(I4B) :: n
    !
    ! -- initialize
    ncpkg => null()
    !
    do n = 1, nc4_context%pkglist%count()
      pkg => nc4_context%get(n)
      if (pkg%subcomponent_name == scname .and. &
          pkg%component_name == modelname) then
        ncpkg => pkg
        exit
      end if
    end do
    !
    if (.not. associated(ncpkg)) then
      errmsg = 'NC package context not found:'//trim(modelname)//'/'//trim(scname)
      call store_error(errmsg)
      call store_error_filename(nc4_context%modelfname)
    else
      if (ncpkg%subcomponent_type /= sctype) then
        ! -- pkgtype (EVT6 or RCH6) used in namefile packages block-
        !    dfns were initialized to list but need to be array.
        call ncpkg%reset(ptype, sctype)
      end if
    end if
    !
    ! -- return
    return
  end function get_nc4_package

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

  subroutine add_block_var(ncpkg, ncid, varid, varname, ctype, sctype, &
                           mempath, modelfname, iout)
    ! -- modules
    use IdmDfnSelectorModule, only: param_definitions
    ! -- dummy
    type(NC4ModelPackageInputType), pointer, intent(in) :: ncpkg
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    character(len=*), intent(inout) :: varname
    character(len=*), intent(in) :: ctype
    character(len=*), intent(in) :: sctype
    character(len=*), intent(in) :: mempath
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
    param_dfns => param_definitions(ctype, sctype)
    array_input = read_as_arrays(param_dfns)
    !
    ! -- assume tagname is unique to input dfn set
    idt => package_scoped_param_dfn(param_dfns, ncpkg%component_type, &
                                    sctype, varname, '')
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
    write (iout, '(a)') 'NC4 input parameter "'//trim(varname)// &
      '" discovered for package "'//trim(mempath)//'".'
    !
    ! -- return
    return
  end subroutine add_block_var

  subroutine create_pkg_ipers(ncpkg, ncid, varid, varname, iout)
    ! -- modules
    ! -- dummy
    type(NC4ModelPackageInputType), pointer, intent(in) :: ncpkg
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

  subroutine add_package_var(nc4_context, modelname, varid, iout)
    ! -- modules
    use MemoryHelperModule, only: split_mem_path
    use SourceCommonModule, only: idm_subcomponent_type
    ! -- dummy
    type(NC4ModelInputsType), intent(inout) :: nc4_context
    character(len=*), intent(in) :: modelname
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iout
    ! -- local
    type(NC4ModelPackageInputType), pointer :: ncpkg
    character(len=LENCOMPONENTNAME) :: component, scname
    character(len=LINELENGTH) :: input_str
    character(len=LENPACKAGETYPE) :: sctype, pkgtype
    character(len=LINELENGTH) :: mem_address
    character(len=LENMEMPATH) :: mempath
    character(len=LINELENGTH) :: varname
    integer(I4B) :: ncid, idx
    !
    ! -- initialize
    ncid = nc4_context%ncid
    varname = ''
    !
    if (nf90_get_att(ncid, varid, "mf6_input", &
                     input_str) == NF90_NOERR) then
      !
      idx = index(input_str, ':')
      !
      if (idx > 0) then
        ! -- split input string into package type and path tokens
        pkgtype = input_str(1:idx - 1)
        ! -- subcomponent type
        sctype = idm_subcomponent_type(nc4_context%modeltype, pkgtype)
        ! -- not a true address- tagname instead of mf6varname
        mem_address = input_str(idx + 1:len_trim(input_str))
      else
        ! TODO: invalid mf6_input string error
      end if
      !
      idx = index(mem_address, '/', back=.true.)
      !
      if (idx > 0) then
        varname = mem_address(idx + 1:len_trim(mem_address))
        ! -- split address into mempath and input tag
        mempath = mem_address(1:idx - 1)
        call split_mem_path(mempath, component, scname)
        !
        ncpkg => get_nc4_package(nc4_context, component, scname, &
                                 pkgtype, sctype, iout)
        !
        if (varname == 'IPER') then
          ! -- allocate and set package ipers
          call create_pkg_ipers(ncpkg, ncid, varid, varname, iout)
          !
        else
          ! -- track package parameter
          call add_block_var(ncpkg, ncid, varid, varname, &
                             nc4_context%component_type, sctype, &
                             mempath, nc4_context%modelfname, iout)
        end if
      else
        ! TODO: invalid mf6_input string
      end if
    end if
    !
    ! -- return
    return
  end subroutine add_package_var

  subroutine nc4_pkg_context(nc4_context, iout)
    use InputOutputModule, only: lowcase, upcase
    type(NC4ModelInputsType), intent(inout) :: nc4_context
    integer(I4B), intent(in) :: iout
    integer(I4B) :: ndim, nvar, nattr, unlimDimID, format
    integer(I4B), dimension(:), allocatable :: varids
    ! -- local
    character(len=LENPACKAGETYPE) :: modeltype
    character(len=LENCOMPONENTNAME) :: modelname
    integer(I4B) :: iparam, ncid
    !
    ncid = nc4_context%ncid
    !
    ! -- verify expected mf6_modeltype file attribute
    if (nf90_get_att(ncid, NF90_GLOBAL, "mf6_modeltype", &
                     modeltype) == NF90_NOERR) then
      !
      call upcase(modeltype)
      ! -- verify modeltype matches this model
      if (nc4_context%modeltype /= modeltype) then
        errmsg = 'NC4 input file model type does not match namefile model type'
        call store_error(errmsg)
        call store_error_filename(nc4_context%modelfname)
      end if
      !
    else
      errmsg = 'NC4 input file global attribute "mf6_modeltype" not found.'
      call store_error(errmsg)
      call store_error_filename(nc4_context%modelfname)
      !
    end if
    !
    ! -- verify expected mf6_modelname file attribute
    if (nf90_get_att(ncid, NF90_GLOBAL, "mf6_modelname", &
                     modelname) == NF90_NOERR) then
      !
      call upcase(modelname)
      ! -- verify modelname matches this model
      if (nc4_context%modelname /= modelname) then
        errmsg = 'NC4 input file model name does not match namefile model name'
        call store_error(errmsg)
        call store_error_filename(nc4_context%modelfname)
      end if
      !
    else
      errmsg = 'NC4 input file global attribute "mf6_modelname" not found.'
      call store_error(errmsg)
      call store_error_filename(nc4_context%modelfname)
      !
    end if
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
      call add_package_var(nc4_context, modelname, varids(iparam), iout)
      !
    end do
    !
    ! -- cleanup
    deallocate (varids)
    !
    ! -- return
    return
  end subroutine nc4_pkg_context

end module LoadNetCDFDataModule

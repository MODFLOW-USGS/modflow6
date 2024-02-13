module LoadNCFileModule

  use netcdf
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENAUXNAME
  use SimVariablesModule, only: errmsg, idm_context
  use SimModule, only: store_error, store_error_filename
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_setptr
  use CharacterStringModule, only: CharacterStringType
  use InputDefinitionModule, only: InputParamDefinitionType
  use DefinitionSelectModule, only: get_param_definition_type, &
                                    get_aggregate_definition_type
  use ModflowInputModule, only: ModflowInputType
  use IdmLoggerModule, only: idm_log_var, idm_log_header, idm_log_close
  use NCModelInputsModule, only: NCModelPackageInputType

  implicit none
  private
  public :: nc_fopen, nc_fclose
  public :: input_load
  public :: IDM_NETCDF4_MAX_DIM
  public :: nf_verify

  integer(I4B), parameter :: IDM_NETCDF4_MAX_DIM = 6

contains

  !> @brief Open netcdf file
  !<
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
    !
    ! -- return
    return
  end function nc_fopen

  !> @brief Close netcdf file
  !<
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

  !> @brief Static input load
  !<
  subroutine input_load(mf6_input, ncpkg, ncid, nc_fname, iout)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(NCModelPackageInputType), pointer, intent(in) :: ncpkg
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
    do iblock = 1, size(ncpkg%blocklist)
      if (ncpkg%blocklist(iblock)%blockname == 'PERIOD') then
        !
        exit
        !
      else
        !
        if (ncpkg%blocklist(iblock)%aggregate) then
          ! -- load list stype input
          call load_aggregate_block(mf6_input, ncpkg, mshape, &
                                    iblock, ncid, nc_fname, iout)
        else
          ! -- load block variables
          call load_block_var(mf6_input, ncpkg, mshape, &
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

  !> @brief Static load of aggregate block
  !<
  subroutine load_aggregate_block(mf6_input, ncpkg, mshape, &
                                  iblock, ncid, nc_fname, iout)
    use InputOutputModule, only: parseline
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(NCModelPackageInputType), pointer, intent(in) :: ncpkg
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape
    integer(I4B), intent(in) :: iblock
    integer(I4B), intent(in) :: ncid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    type(InputParamDefinitionType), pointer :: idta
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
      found = load_aggregate_var(mf6_input, ncpkg, mshape, iblock, idta, &
                                 words(icol), ncid, nc_fname, iout)
      !
      ! -- ensure required params found
      if (.not. found) then
        if (idta%required) then
          errmsg = 'Required input parameter "'//trim(words(icol))// &
                   '" not found for package "'// &
                   trim(ncpkg%subcomponent_name)//'".'
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

  function load_aggregate_var(mf6_input, ncpkg, mshape, iblock, idta, &
                              tagname, ncid, nc_fname, iout) result(found)
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(NCModelPackageInputType), pointer, intent(in) :: ncpkg
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
    integer(I4B) :: iparam
    !
    ! -- initialize
    found = .false.
    !
    do iparam = 1, ncpkg%blocklist(iblock)%varnum
      !
      ! -- set param definition
      idt => get_param_definition_type(mf6_input%param_dfns, &
                                       mf6_input%component_type, &
                                       mf6_input%subcomponent_type, &
                                       ncpkg%blocklist(iblock)%blockname, &
                                       tagname, nc_fname)

      if (ncpkg%blocklist(iblock)%tagnames(iparam) == trim(tagname)) then
        !
        ! -- set found
        found = .true.
        !
        ! -- adjust definition to reflect list style input
        idt => aggregate_param_dfn(idta, idt, nc_fname)
        !
        ! -- load
        if (idt%datatype == 'JAGGEDINT1D') then
          call load_varint1d_type(mf6_input, mshape, idt, ncid, &
                                  ncpkg%blocklist(iblock)%varids(iparam), iout)
        else
          call load_static_var(mf6_input, idt, mshape, ncid, &
                               ncpkg%blocklist(iblock)%varids(iparam), &
                               nc_fname, iout)
        end if
        !
        ! -- cleanup
        deallocate (idt)
        !
        exit
      end if
    end do
    !
    ! -- return
    return
  end function load_aggregate_var

  subroutine load_block_var(mf6_input, ncpkg, mshape, &
                            iblock, ncid, nc_fname, iout)
    use MemoryManagerModule, only: get_isize
    use SourceCommonModule, only: set_model_shape, mem_allocate_naux
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(NCModelPackageInputType), pointer, intent(in) :: ncpkg
    integer(I4B), dimension(:), contiguous, pointer, intent(inout) :: mshape
    integer(I4B), intent(in) :: iblock
    integer(I4B), intent(in) :: ncid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam, ts6_size
    !
    ! -- load variables in block order
    do iparam = 1, ncpkg%blocklist(iblock)%varnum
      !
      ! -- set input definition
      idt => &
        get_param_definition_type(mf6_input%param_dfns, &
                                  mf6_input%component_type, &
                                  mf6_input%subcomponent_type, &
                                  ncpkg%blocklist(iblock)%blockname, &
                                  ncpkg%blocklist(iblock)%tagnames(iparam), &
                                  nc_fname)
      ! -- load the variable data
      call load_static_var(mf6_input, idt, mshape, ncid, &
                           ncpkg%blocklist(iblock)%varids(iparam), &
                           nc_fname, iout)
      !
    end do
    !
    ! -- block post processing
    select case (ncpkg%blocklist(iblock)%blockname)
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
      !
      ! -- determine if TS6 files were provided in OPTIONS block
      call get_isize('TS6_FILENAME', mf6_input%mempath, ts6_size)
      !
      if (ts6_size > 0) then
        ncpkg%ts_active = .true.
        !
        ! -- timeseries is not yet supported
        errmsg = 'IDM unimplemented. TimeSeries not yet supported &
               &for NetCDF inputs.'
        call store_error(errmsg)
        call store_error_filename(nc_fname)
        !
      end if
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
    use DefinitionSelectModule, only: idt_copy
    type(InputParamDefinitionType), pointer, intent(in) :: idta
    type(InputParamDefinitionType), pointer, intent(in) :: idtp
    character(len=*), intent(in) :: nc_fname
    type(InputParamDefinitionType), pointer :: idt
    !
    ! -- copy from input dfn
    idt => idt_copy(idtp)
    idt%shape = idta%shape
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
        idt%datatype = 'JAGGEDINT1D'
        idt%shape = idtp%shape
      else
        errmsg = 'IDM unimplemented. LoadNCFile::aggregate_param_dfn &
                 &tagname='//trim(idt%tagname)
        call store_error(errmsg)
        call store_error_filename(nc_fname)
      end if
    case default
      errmsg = 'IDM unimplemented. LoadNCFile::aggregate_param_dfn &
               &datatype='//trim(idtp%datatype)
      call store_error(errmsg)
      call store_error_filename(nc_fname)
    end select
    !
    ! -- return
    return
  end function aggregate_param_dfn

  subroutine load_static_var(mf6_input, idt, mshape, ncid, varid, nc_fname, iout)
    use DefinitionSelectModule, only: idt_datatype
    type(ModflowInputType), intent(in) :: mf6_input
    type(InputParamDefinitionType), pointer, intent(in) :: idt
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
      !if (idt%datatype(1:6) == 'RECORD') then
      if (idt_datatype(idt) == 'RECORD') then
        call load_record_type(mf6_input, idt, ncid, varid, nc_fname, iout)
      else
        errmsg = 'IDM unimplemented. LoadNCFile::load_static_var &
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
    type(InputParamDefinitionType), pointer, intent(in) :: idt
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
      errmsg = 'IDM unimplemented. LoadNCFile::load_record_type &
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
      else if (size(mshape) == 1) then
        ! -- read the data into memory managed array
        call nf_verify(nf90_get_var(ncid, varid, int1d), ncid, iout)
        !
      else
        write (errmsg, '(a,i0)') &
          'IDM unimplemented. LoadNCFile::load_integer1d_type &
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
      else if (size(mshape) == 1) then
        ! -- read the data into memory managed array
        call nf_verify(nf90_get_var(ncid, varid, dbl1d), ncid, iout)
        !
      else
        write (errmsg, '(a,i0)') &
          'IDM unimplemented. LoadNCFile::load_double1d_type &
          &size tag='//trim(idt%tagname)//', mshape=', size(mshape)
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

end module LoadNCFileModule

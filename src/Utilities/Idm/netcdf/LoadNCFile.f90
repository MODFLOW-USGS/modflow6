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
  public :: LoadNCFileType
  public :: NETCDF_MAX_DIM
  public :: NETCDF_ATTR_STRLEN
  public :: nf_verify

  integer(I4B), parameter :: NETCDF_MAX_DIM = 6
  integer(I4B), parameter :: NETCDF_ATTR_STRLEN = 80

  !> @brief Static parser based input loader
  !!
  !! This type defines a static input context loader
  !! for traditional mf6 ascii input files.
  !!
  !<
  type :: LoadNCFileType
    type(NCModelPackageInputType), pointer :: ncpkg => null()
    integer(I4B) :: ncid !< netcdf integer file handle
    character(len=LINELENGTH) :: nc_fname !< name of netcdf input file
    integer(I4B), dimension(:), pointer, contiguous :: mshape => null() !< model shape
    type(ModflowInputType) :: mf6_input !< description of input
    character(len=LINELENGTH) :: filename !< name of ascii input file
    logical(LGP) :: ts_active !< is timeseries active
    integer(I4B) :: iout !< inunit for list log
  contains
    procedure :: load
    procedure :: init
    procedure :: load_block
    procedure :: finalize
    procedure :: block_post_process
    procedure :: load_aggregate_block
    procedure :: load_aggregate_var
    procedure :: load_block_var
    procedure :: aggregate_param_dfn
    procedure :: load_static_var
  end type LoadNCFileType

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

  !> @brief load all static input blocks
  !!
  !! Invoke this routine to load all static input blocks
  !! in single call.
  !!
  !<
  subroutine load(this, ncpkg, ncid, mf6_input, nc_fname, iout)
    ! -- modules
    use MemoryManagerModule, only: get_isize
    ! -- dummy
    class(LoadNCFileType) :: this
    type(NCModelPackageInputType), pointer, intent(in) :: ncpkg
    integer(I4B), intent(in) :: ncid
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: iblk
    !
    ! -- initialize static load
    call this%init(ncpkg, ncid, mf6_input, nc_fname, iout)
    !
    ! -- process blocks
    do iblk = 1, size(this%mf6_input%block_dfns)
      !
      ! -- don't load dynamic input data
      if (this%mf6_input%block_dfns(iblk)%blockname == 'PERIOD') exit
      !
      ! -- load the block
      call this%load_block(iblk)
      !
    end do
    !
    ! -- finalize static load
    call this%finalize()
    !
    ! --return
    return
  end subroutine load

  !> @brief init
  !!
  !! init / finalize are only used when load_block() will be called
  !!
  !<
  subroutine init(this, ncpkg, ncid, mf6_input, nc_fname, iout)
    ! -- modules
    use MemoryManagerModule, only: get_isize
    ! -- dummy
    class(LoadNCFileType) :: this
    type(NCModelPackageInputType), pointer, intent(in) :: ncpkg
    integer(I4B), intent(in) :: ncid
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: isize
    !
    this%ncpkg => ncpkg
    this%ncid = ncid
    this%mf6_input = mf6_input
    this%nc_fname = nc_fname
    this%ts_active = .false.
    this%iout = iout
    !
    call get_isize('MODEL_SHAPE', mf6_input%component_mempath, isize)
    !
    if (isize > 0) then
      call mem_setptr(this%mshape, 'MODEL_SHAPE', mf6_input%component_mempath)
    end if
    !
    ! -- log lst file header
    call idm_log_header(this%mf6_input%component_name, &
                        this%mf6_input%subcomponent_name, this%iout)
    !
    ! -- return
    return
  end subroutine init

  !> @brief load a single block
  !!
  !! Assumed in order load of single (next) block.
  !!
  !<
  subroutine load_block(this, iblk)
    ! -- modules
    class(LoadNCFileType) :: this
    integer(I4B), intent(in) :: iblk
    ! -- local
    !
    ! --
    !
    if (this%ncpkg%blocklist(iblk)%aggregate) then
      ! -- load list stype input
      call this%load_aggregate_block(iblk)
    else
      ! -- load block variables
      call this%load_block_var(iblk)
    end if
    !
    ! --return
    return
  end subroutine load_block

  !> @brief finalize
  !!
  !! init / finalize are only used when load_block() will be called
  !!
  !<
  subroutine finalize(this)
    ! -- modules
    ! -- dummy
    class(LoadNCFileType) :: this
    ! -- local
    !
    ! -- close logging block
    call idm_log_close(this%mf6_input%component_name, &
                       this%mf6_input%subcomponent_name, this%iout)
    !
    ! -- return
    return
  end subroutine finalize

  !> @brief Post parse block handling
  !!
  !<
  subroutine block_post_process(this, iblk)
    ! -- modules
    use MemoryManagerModule, only: get_isize
    use SourceCommonModule, only: set_model_shape, mem_allocate_naux
    ! -- dummy
    class(LoadNCFileType) :: this
    integer(I4B), intent(in) :: iblk
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam, ts6_size
    !
    ! -- block post processing
    select case (this%ncpkg%blocklist(iblk)%blockname)
    case ('OPTIONS')
      ! -- allocate naux and set to 0 if not allocated
      do iparam = 1, size(this%mf6_input%param_dfns)
        idt => this%mf6_input%param_dfns(iparam)
        !
        if (idt%blockname == 'OPTIONS' .and. &
            idt%tagname == 'AUXILIARY') then
          call mem_allocate_naux(this%mf6_input%mempath)
          exit
        end if
      end do
      !
      ! -- determine if TS6 files were provided in OPTIONS block
      call get_isize('TS6_FILENAME', this%mf6_input%mempath, ts6_size)
      !
      if (ts6_size > 0) then
        !this%ncpkg%ts_active = .true.
        !
        ! -- timeseries is not yet supported
        errmsg = 'IDM unimplemented. TS6/TAS6 not yet supported &
               &for NetCDF inputs.'
        call store_error(errmsg)
        call store_error_filename(this%nc_fname)
        !
      end if
    case ('DIMENSIONS')
      ! -- set model shape if discretization dimensions have been read
      if (this%mf6_input%pkgtype(1:3) == 'DIS') then
        call set_model_shape(this%mf6_input%pkgtype, this%nc_fname, &
                             this%mf6_input%component_mempath, &
                             this%mf6_input%mempath, this%mshape)
      end if
    case default
    end select
    !
    ! -- return
    return
  end subroutine block_post_process

  !> @brief Static load of aggregate block
  !<
  subroutine load_aggregate_block(this, iblk)
    use InputOutputModule, only: parseline
    ! -- dummy
    class(LoadNCFileType) :: this
    integer(I4B), intent(in) :: iblk
    ! -- local
    type(InputParamDefinitionType), pointer :: idta
    character(len=16), dimension(:), allocatable :: words
    character(len=:), allocatable :: parse_str
    integer(I4B) :: icol, nwords
    logical(LGP) :: found
    !
    ! -- set input definition for this block
    idta => &
      get_aggregate_definition_type(this%mf6_input%aggregate_dfns, &
                                    this%mf6_input%component_type, &
                                    this%mf6_input%subcomponent_type, &
                                    this%mf6_input%block_dfns(iblk)%blockname)
    !
    ! -- identify variable names
    parse_str = trim(idta%datatype)//' '
    call parseline(parse_str, nwords, words)
    !
    !
    ! -- Skip first col which is RECARRAY
    do icol = 2, nwords
      !
      found = this%load_aggregate_var(iblk, idta, words(icol))
      !
      ! -- ensure required params found
      if (.not. found) then
        if (idta%required) then
          errmsg = 'Required input parameter "'//trim(words(icol))// &
                   '" not found for package "'// &
                   trim(this%ncpkg%subcomponent_name)//'".'
          call store_error(errmsg)
          call store_error_filename(this%nc_fname)
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

  function load_aggregate_var(this, iblk, idta, tagname) result(found)
    ! -- dummy
    class(LoadNCFileType) :: this
    integer(I4B), intent(in) :: iblk
    type(InputParamDefinitionType), pointer, intent(in) :: idta
    character(len=*), intent(in) :: tagname
    ! -- return
    logical(LGP) :: found
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam
    !
    ! -- initialize
    found = .false.
    !
    do iparam = 1, this%ncpkg%blocklist(iblk)%varnum
      !
      ! -- set param definition
      idt => get_param_definition_type(this%mf6_input%param_dfns, &
                                       this%mf6_input%component_type, &
                                       this%mf6_input%subcomponent_type, &
                                       this%ncpkg%blocklist(iblk)%blockname, &
                                       tagname, this%nc_fname)

      if (this%ncpkg%blocklist(iblk)%tagnames(iparam) == trim(tagname)) then
        !
        ! -- set found
        found = .true.
        !
        ! -- adjust definition to reflect list style input
        idt => this%aggregate_param_dfn(idta, idt, this%nc_fname)
        !
        ! -- load
        if (idt%datatype == 'JAGGEDINT2D') then
          call load_varint1d_type(this%mf6_input, this%mshape, idt, this%ncid, &
                                  this%ncpkg%blocklist(iblk)%varids(iparam), &
                                  this%iout)
        else
          call this%load_static_var(idt, &
                                    this%ncpkg%blocklist(iblk)%varids(iparam))
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

  subroutine load_block_var(this, iblk)
    use MemoryManagerModule, only: get_isize
    use SourceCommonModule, only: set_model_shape, mem_allocate_naux
    use MemoryTypeModule, only: MemoryType
    use MemoryManagerModule, only: get_from_memorylist
    ! -- dummy
    class(LoadNCFileType) :: this
    integer(I4B), intent(in) :: iblk
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam
    logical(LGP) :: found
    type(MemoryType), pointer :: mt
    !
    ! -- disu vertices/cell2d blocks are contingent on NVERT dimension
    if (this%mf6_input%pkgtype == 'DISU6' .or. &
        this%mf6_input%pkgtype == 'DISL6') then
      if (this%mf6_input%block_dfns(iblk)%blockname == 'VERTICES' .or. &
          this%mf6_input%block_dfns(iblk)%blockname == 'CELL2D') then
        call get_from_memorylist('NVERT', this%mf6_input%mempath, mt, found, &
                                 .false.)
        if (.not. found) return
        if (mt%intsclr == 0) return
      end if
    end if
    !
    ! -- load variables in block order
    do iparam = 1, this%ncpkg%blocklist(iblk)%varnum
      !
      ! -- set input definition
      idt => &
        get_param_definition_type(this%mf6_input%param_dfns, &
                                  this%mf6_input%component_type, &
                                  this%mf6_input%subcomponent_type, &
                                  this%ncpkg%blocklist(iblk)%blockname, &
                                  this%ncpkg%blocklist(iblk)%tagnames(iparam), &
                                  this%nc_fname)
      ! -- load the variable data
      call this%load_static_var(idt, this%ncpkg%blocklist(iblk)%varids(iparam))
      !
    end do
    !
    ! -- block post processing
    call this%block_post_process(iblk)
    !
    ! -- return
    return
  end subroutine load_block_var

  function aggregate_param_dfn(this, idta, idtp, nc_fname) result(idt)
    use DefinitionSelectModule, only: idt_copy
    class(LoadNCFileType) :: this
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
        idt%datatype = 'JAGGEDINT2D'
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
      call store_error_filename(this%nc_fname)
    end select
    !
    ! -- return
    return
  end function aggregate_param_dfn

  subroutine load_static_var(this, idt, varid)
    use DefinitionSelectModule, only: idt_datatype
    class(LoadNCFileType) :: this
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), intent(in) :: varid
    ! -- local
    !
    ! -- allocate and load data type
    select case (idt%datatype)
    case ('KEYWORD')
      call load_keyword_type(this%mf6_input, idt, this%ncid, varid, this%iout)
    case ('STRING')
      if (idt%shape == 'NAUX') then
        call load_auxvar_names(this%mf6_input, idt, this%ncid, varid, this%iout)
      else
        call load_string_type(this%mf6_input, idt, this%ncid, varid, this%iout)
      end if
    case ('INTEGER')
      call load_integer_type(this%mf6_input, idt, this%ncid, varid, this%iout)
    case ('INTEGER1D')
      call load_integer1d_type(this%mf6_input, this%mshape, idt, this%ncid, &
                               varid, this%nc_fname, this%iout)
    case ('INTEGER2D')
      call load_integer2d_type(this%mf6_input, this%mshape, idt, this%ncid, &
                               varid, this%iout)
    case ('INTEGER3D')
      call load_integer3d_type(this%mf6_input, this%mshape, idt, this%ncid, &
                               varid, this%iout)
    case ('DOUBLE')
      call load_double_type(this%mf6_input, idt, this%ncid, varid, this%iout)
    case ('DOUBLE1D')
      call load_double1d_type(this%mf6_input, this%mshape, idt, this%ncid, &
                              varid, this%nc_fname, this%iout)
    case ('DOUBLE2D')
      call load_double2d_type(this%mf6_input, this%mshape, idt, this%ncid, &
                              varid, this%iout)
    case ('DOUBLE3D')
      call load_double3d_type(this%mf6_input, this%mshape, idt, this%ncid, &
                              varid, this%iout)
    case default
      if (idt_datatype(idt) == 'RECORD') then
        call load_record_type(this%mf6_input, idt, this%ncid, varid, &
                              this%nc_fname, this%iout)
      else
        errmsg = 'IDM unimplemented. LoadNCFile::load_static_var &
                 &datatype='//trim(idt%datatype)
        call store_error(errmsg)
        call store_error_filename(this%nc_fname)
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
  subroutine load_io_tag(mf6_input, idt, which, ncid, varid, nc_fname, iout)
    use InputOutputModule, only: parseline
    use MemoryManagerModule, only: get_isize
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    character(len=*), intent(in) :: which
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LINELENGTH) :: cstr
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: charstr1d
    integer(I4B) :: isize, idx
    !
    if (which == 'FILEIN') then
      !
      ! -- FILEIN record types support 1+ file specifications
      call get_isize(idt%mf6varname, mf6_input%mempath, isize)
      !
      ! -- allocate / reallocate
      if (isize < 0) then
        ! -- does not exist, allocate
        call mem_allocate(charstr1d, LINELENGTH, 1, idt%mf6varname, &
                          mf6_input%mempath)
        idx = 1
      else
        ! -- exists, reallocate
        call mem_setptr(charstr1d, idt%mf6varname, mf6_input%mempath)
        call mem_reallocate(charstr1d, LINELENGTH, isize + 1, &
                            idt%mf6varname, mf6_input%mempath)
        idx = isize + 1
      end if
      !
      ! -- read and set the data
      call nf_verify(nf90_get_var(ncid, varid, cstr), ncid, iout)
      charstr1d(idx) = trim(cstr)
      !
    else if (which == 'FILEOUT') then
      ! -- FILEOUT record types support a single file specification
      call load_string_type(mf6_input, idt, ncid, varid, iout)
    end if
    !
    ! -- return
    return
  end subroutine load_io_tag

  !> @brief load type record
  !!
  !! Currently only supports file record types
  !<
  subroutine load_record_type(mf6_input, idt, ncid, varid, nc_fname, iout)
    use InputOutputModule, only: parseline
    use DefinitionSelectModule, only: idt_parse_rectype
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: varid
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    type(InputParamDefinitionType), pointer :: io_idt
    character(len=LINELENGTH), dimension(:), allocatable :: words
    integer(I4B) :: nwords
    logical(LGP) :: found
    !
    ! -- initialization
    found = .false.
    !
    ! -- split definition
    call idt_parse_rectype(idt, words, nwords)
    !
    select case (nwords)
    case (3)
      !
      ! -- verify third definition token is FILEIN/FILEOUT
      if (words(2) == 'FILEIN' .or. words(2) == 'FILEOUT') then
        !
        ! -- matches, read and load file name
        io_idt => &
          get_param_definition_type(mf6_input%param_dfns, &
                                    mf6_input%component_type, &
                                    mf6_input%subcomponent_type, &
                                    'OPTIONS', words(3), nc_fname)
        call load_io_tag(mf6_input, io_idt, words(2), ncid, varid, nc_fname, iout)
        !
        ! -- io tag loaded
        found = .true.
      end if
    case default
    end select
    !
    if (found) then
    else
      errmsg = 'IDM unimplemented. LoadNCFile::load_record_type &
               &tagname='//trim(idt%tagname)
      call store_error(errmsg)
      call store_error_filename(nc_fname)

    end if
    !
    ! -- cleanup
    if (allocated(words)) deallocate (words)
    !
    return
  end subroutine load_record_type

  !> @brief load type string
  !<
  subroutine load_string_type(mf6_input, idt, ncid, varid, iout)
    use InputOutputModule, only: upcase
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
    if (idt%preserve_case) then
      ! -- no-op
    else
      call upcase(cstr)
    end if
    call idm_log_var(cstr, idt%mf6varname, mf6_input%mempath, iout)
    !
    return
  end subroutine load_string_type

  subroutine load_auxvar_names(mf6_input, idt, ncid, varid, iout)
    use InputOutputModule, only: upcase
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
    character(len=LENAUXNAME) :: auxvar
    integer(I4B) :: vartype, ndims, nattr, n
    integer(I4B), dimension(NETCDF_MAX_DIM) :: dimids, dimlens
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
      auxvar = cstr1d(n)
      call upcase(auxvar)
      charstr1d(n) = trim(auxvar)
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
    integer(I4B), dimension(NETCDF_MAX_DIM) :: dimids, dimlens
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
        write (errmsg, '(a,a,a,i0,a,i0,a)') 'NetCDF library error [error="', &
          trim(errstr), '", exit code=', res, ', ncid=', ncid, '].'
      else
        write (errmsg, '(a,i0,a,i0,a)') 'NetCDF library error [exit code=', &
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

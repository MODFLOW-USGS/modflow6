!> @brief This module contains the LoadMf6FileTypeModule
!!
!! This module contains the input data model routines for
!! loading the data from a MODFLOW 6 input file using the
!! block parser.
!!
!<
module LoadMf6FileTypeModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMEMPATH
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use BlockParserModule, only: BlockParserType
  use LayeredArrayReaderModule, only: read_dbl1d_layered, &
                                      read_dbl2d_layered, &
                                      read_dbl3d_layered, &
                                      read_int1d_layered, &
                                      read_int2d_layered, &
                                      read_int3d_layered
  use Double1dReaderModule, only: read_dbl1d
  use Double2dReaderModule, only: read_dbl2d
  use Integer1dReaderModule, only: read_int1d
  use Integer2dReaderModule, only: read_int2d
  use InputOutputModule, only: parseline
  use InputDefinitionModule, only: InputParamDefinitionType
  use InputDefinitionSelectorModule, only: get_param_definition_type, &
                                           get_aggregate_definition_type
  use ModflowInputModule, only: ModflowInputType, getModflowInput
  use MemoryManagerModule, only: mem_allocate, mem_setptr
  use MemoryHelperModule, only: create_mem_path
  use IdmLoggerModule, only: idm_log_var, idm_log_header, idm_log_close

  implicit none
  private
  public :: idm_load

  interface idm_load
    module procedure idm_load_from_blockparser
  end interface idm_load

contains

  !> @brief procedure to load a file
  !!
  !! Use parser to load information from an input file into the __INPUT__
  !! memory context location of the memory manager.
  !!
  !<
  subroutine idm_load_from_blockparser(parser, filetype, &
                                       component_type, subcomponent_type, &
                                       component_name, subcomponent_name, &
                                       subpackages, iout)
    use SimVariablesModule, only: idm_context
    type(BlockParserType), intent(inout) :: parser !< block parser
    character(len=*), intent(in) :: filetype !< file type to load, such as DIS6, DISV6, NPF6
    character(len=*), intent(in) :: component_type !< component type, such as GWF or GWT
    character(len=*), intent(in) :: subcomponent_type !< subcomponent type, such as DIS or NPF
    character(len=*), intent(in) :: component_name !< component name, such as MYGWFMODEL
    character(len=*), intent(in) :: subcomponent_name !< subcomponent name, such as MYWELLPACKAGE
    character(len=*), dimension(:), intent(in) :: subpackages !< array of subpackage types, such as ["TVK6", "OBS6"]
    integer(I4B), intent(in) :: iout !< unit number for output
    integer(I4B) :: iblock !< consecutive block number as defined in definition file
    type(ModflowInputType) :: mf6_input !< ModflowInputType
    character(len=LENMEMPATH) :: componentMemPath
    integer(I4B), dimension(:), contiguous, pointer :: mshape => null()
    !
    ! -- construct input object
    mf6_input = getModflowInput(filetype, component_type, &
                                subcomponent_type, component_name, &
                                subcomponent_name, subpackages)
    !
    ! -- model shape memory path
    componentMemPath = create_mem_path(component=mf6_input%component_name, &
                                       context=idm_context)
    !
    ! -- log lst file header
    call idm_log_header(mf6_input%component_name, &
                        mf6_input%subcomponent_name, iout)
    !
    ! -- process blocks
    do iblock = 1, size(mf6_input%p_block_dfns)
      call parse_block(parser, mf6_input, iblock, mshape, iout)
      !
      ! -- set model shape if discretization dimensions have been read
      if (mf6_input%p_block_dfns(iblock)%blockname == 'DIMENSIONS' .and. &
          filetype(1:3) == 'DIS') then
        call set_model_shape(mf6_input%file_type, componentMemPath, &
                             mf6_input%memoryPath, mshape)
      end if
    end do
    !
    ! -- close logging statement
    call idm_log_close(mf6_input%component_name, &
                       mf6_input%subcomponent_name, iout)
    !
    ! -- release allocated memory
    call mf6_input%destroy()
  end subroutine idm_load_from_blockparser

  !> @brief procedure to load a block
  !!
  !! Use parser to load information from a block into the __INPUT__
  !! memory context location of the memory manager.
  !!
  !<
  subroutine parse_block(parser, mf6_input, iblock, mshape, iout)
    use MemoryTypeModule, only: MemoryType
    use MemoryManagerModule, only: get_from_memorylist
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(ModflowInputType), intent(in) :: mf6_input !< ModflowInputType
    integer(I4B), intent(in) :: iblock !< consecutive block number as defined in definition file
    integer(I4B), dimension(:), contiguous, pointer, intent(inout) :: mshape !< model shape
    integer(I4B), intent(in) :: iout !< unit number for output
    logical(LGP) :: isblockfound
    logical(LGP) :: endOfBlock
    logical(LGP) :: supportOpenClose
    integer(I4B) :: ierr
    logical(LGP) :: found
    type(MemoryType), pointer :: mt
    !
    ! -- disu vertices/cell2d blocks are contingent on NVERT dimension
    if (mf6_input%file_type == 'DISU6' .and. &
        (mf6_input%p_block_dfns(iblock)%blockname == 'VERTICES' .or. &
         mf6_input%p_block_dfns(iblock)%blockname == 'CELL2D')) then
      call get_from_memorylist('NVERT', mf6_input%memoryPath, mt, found, .false.)
      if (.not. found .or. mt%intsclr == 0) return
    end if
    !
    ! -- block open/close support
    supportOpenClose = (mf6_input%p_block_dfns(iblock)%blockname /= 'GRIDDATA')
    !
    ! -- parser search for block
    call parser%GetBlock(mf6_input%p_block_dfns(iblock)%blockname, isblockfound, &
                         ierr, supportOpenClose=supportOpenClose, &
                         blockRequired=mf6_input%p_block_dfns(iblock)%required)
    !
    ! -- process block
    if (isblockfound) then
      if (mf6_input%p_block_dfns(iblock)%aggregate) then
        !
        ! -- process block recarray type, set of variable 1d/2d types
        call parse_structarray_block(parser, mf6_input, iblock, mshape, iout)
      else
        do
          ! process each line in block
          call parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
          !
          ! -- process line as tag(s)
          call parse_tag(parser, mf6_input, iblock, mshape, iout, .false.)
        end do
      end if
    end if

    return
  end subroutine parse_block

  !> @brief check subpackage
  !!
  !! Check and make sure that the subpackage is valid for
  !! this input file and load the filename of the subpackage
  !! into the memory manager.
  !!
  !<
  subroutine subpackage_check(parser, mf6_input, checktag, iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(ModflowInputType), intent(in) :: mf6_input !< ModflowInputType
    character(len=LINELENGTH), intent(in) :: checktag !< subpackage string, such as TVK6
    integer(I4B), intent(in) :: iout !< unit number for output
    character(len=LINELENGTH) :: tag, fname_tag
    type(InputParamDefinitionType), pointer :: idt !< input data type object describing this record
    integer(I4B) :: isubpkg

    do isubpkg = 1, size(mf6_input%subpackages)
      if (checktag == mf6_input%subpackages(isubpkg)) then
        fname_tag = trim(checktag)//'_FILENAME'
        call parser%GetStringCaps(tag)
        if (tag == 'FILEIN') then
          idt => get_param_definition_type(mf6_input%p_param_dfns, &
                                           mf6_input%component_type, &
                                           mf6_input%subcomponent_type, &
                                           fname_tag)
          call load_string_type(parser, idt, mf6_input%memoryPath, iout)
        else
          errmsg = 'Subpackage keyword must be followed by "FILEIN" '// &
                   'then by filename.'
          call store_error(errmsg)
        end if
      end if
    end do
  end subroutine subpackage_check

  !> @brief load an individual input record into memory
  !!
  !! Load an individual input record into the memory
  !! manager.  Allow for recursive calls in the case that multiple
  !! tags are on a single line.
  !!
  !<
  recursive subroutine parse_tag(parser, mf6_input, iblock, mshape, iout, &
                                 recursive_call)
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(ModflowInputType), intent(in) :: mf6_input !< ModflowInputType
    integer(I4B), intent(in) :: iblock !< consecutive block number as defined in definition file
    integer(I4B), dimension(:), contiguous, pointer, intent(inout) :: mshape !< model shape
    integer(I4B), intent(in) :: iout !< unit number for output
    logical(LGP), intent(in) :: recursive_call !< true if recursive call
    character(len=LINELENGTH) :: tag
    type(InputParamDefinitionType), pointer :: idt !< input data type object describing this record
    !
    ! -- read tag name
    call parser%GetStringCaps(tag)
    if (recursive_call) then
      if (tag == '') then
        ! no data on line so return
        return
      end if
    end if
    !
    ! -- find keyword in input definition
    idt => get_param_definition_type(mf6_input%p_param_dfns, &
                                     mf6_input%component_type, &
                                     mf6_input%subcomponent_type, &
                                     tag)
    !
    ! -- allocate and load data type
    select case (idt%datatype)
    case ('KEYWORD')
      call load_keyword_type(parser, idt, mf6_input%memoryPath, iout)
      !
      ! -- load filename if subpackage tag
      call subpackage_check(parser, mf6_input, tag, iout)
      !
      ! -- set as dev option
      if (mf6_input%p_block_dfns(iblock)%blockname == 'OPTIONS' .and. &
          idt%tagname(1:4) == 'DEV_') then
        call parser%DevOpt()
      end if
    case ('STRING')
      call load_string_type(parser, idt, mf6_input%memoryPath, iout)
    case ('INTEGER')
      call load_integer_type(parser, idt, mf6_input%memoryPath, iout)
    case ('INTEGER1D')
      call load_integer1d_type(parser, idt, mf6_input%memoryPath, mshape, iout)
    case ('INTEGER2D')
      call load_integer2d_type(parser, idt, mf6_input%memoryPath, mshape, iout)
    case ('INTEGER3D')
      call load_integer3d_type(parser, idt, mf6_input%memoryPath, mshape, iout)
    case ('DOUBLE')
      call load_double_type(parser, idt, mf6_input%memoryPath, iout)
    case ('DOUBLE1D')
      call load_double1d_type(parser, idt, mf6_input%memoryPath, mshape, iout)
    case ('DOUBLE2D')
      call load_double2d_type(parser, idt, mf6_input%memoryPath, mshape, iout)
    case ('DOUBLE3D')
      call load_double3d_type(parser, idt, mf6_input%memoryPath, mshape, iout)
    case default
      write (errmsg, '(4x,a,a)') 'Failure reading data for tag: ', trim(tag)
      call store_error(errmsg)
      call parser%StoreErrorUnit()
    end select
    !
    ! -- continue line if in same record
    if (idt%in_record) then
      ! recursively call parse tag again to read rest of line
      call parse_tag(parser, mf6_input, iblock, mshape, iout, .true.)
    end if
    !
    ! --
    return
  end subroutine parse_tag

  !> @brief parse a structured array record into memory manager
  !!
  !! A structarray is similar to a numpy recarray.  It it used to
  !! load a list of data in which each column in the list may be a
  !! different type.  Each column in the list is stored as a 1d
  !! vector.
  !!
  !<
  subroutine parse_structarray_block(parser, mf6_input, iblock, mshape, iout)
    use StructArrayModule, only: StructArrayType, constructStructArray, &
                                 destructStructArray
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(ModflowInputType), intent(in) :: mf6_input !< ModflowInputType
    integer(I4B), intent(in) :: iblock !< consecutive block number as defined in definition file
    integer(I4B), dimension(:), contiguous, pointer, intent(inout) :: mshape !< model shape
    integer(I4B), intent(in) :: iout !< unit number for output
    type(InputParamDefinitionType), pointer :: idt !< input data type object describing this record
    integer(I4B), pointer :: nrow
    integer(I4B) :: icol
    integer(I4B) :: ncol
    integer(I4B) :: nwords
    character(len=16), dimension(:), allocatable :: words
    type(StructArrayType), pointer :: struct_array
    character(len=:), allocatable :: parse_str
    !
    ! -- set input definition for this block
    idt => get_aggregate_definition_type(mf6_input%p_aggregate_dfns, &
                                         mf6_input%component_type, &
                                         mf6_input%subcomponent_type, &
                                         mf6_input%p_block_dfns(iblock)%blockname)
    !
    ! -- identify variable names, ignore first RECARRAY column
    parse_str = trim(idt%datatype)//' '
    call parseline(parse_str, nwords, words)
    ncol = nwords - 1
    !
    ! -- use shape to set the max num of rows
    call mem_setptr(nrow, idt%shape, mf6_input%memoryPath)
    !
    ! -- create a structured array
    struct_array => constructStructArray(ncol, nrow)
    do icol = 1, ncol
      !
      ! -- set pointer to input definition for this 1d vector
      idt => get_param_definition_type(mf6_input%p_param_dfns, &
                                       mf6_input%component_type, &
                                       mf6_input%subcomponent_type, &
                                       words(icol + 1))
      !
      ! -- allocate variable in memory manager
      call struct_array%mem_create_vector(icol, idt%datatype, idt%mf6varname, &
                                          mf6_input%memoryPath, idt%shape, &
                                          idt%preserve_case)
    end do
    !
    ! -- read the structured array
    call struct_array%read_from_parser(parser, iout)
    call parser%terminateblock()
    !
    ! -- destroy the structured array reader
    call destructStructArray(struct_array)
    !
    ! --
    return
  end subroutine parse_structarray_block

  !> @brief load type keyword
  !<
  subroutine load_keyword_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(InputParamDefinitionType), intent(in) :: idt !< input data type object describing this record
    character(len=*), intent(in) :: memoryPath !< memorypath to put loaded information
    integer(I4B), intent(in) :: iout !< unit number for output
    integer(I4B), pointer :: intvar
    call mem_allocate(intvar, idt%mf6varname, memoryPath)
    intvar = 1
    call idm_log_var(intvar, idt%mf6varname, memoryPath, iout)
    return
  end subroutine load_keyword_type

  !> @brief load type string
  !<
  subroutine load_string_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(InputParamDefinitionType), intent(in) :: idt !< input data type object describing this record
    character(len=*), intent(in) :: memoryPath !< memorypath to put loaded information
    integer(I4B), intent(in) :: iout !< unit number for output
    character(len=LINELENGTH), pointer :: cstr
    integer(I4B) :: ilen
    ilen = LINELENGTH
    call mem_allocate(cstr, ilen, idt%mf6varname, memoryPath)
    call parser%GetString(cstr, (.not. idt%preserve_case))
    return
  end subroutine load_string_type

  !> @brief load type integer
  !<
  subroutine load_integer_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(InputParamDefinitionType), intent(in) :: idt !< input data type object describing this record
    character(len=*), intent(in) :: memoryPath !< memorypath to put loaded information
    integer(I4B), intent(in) :: iout !< unit number for output
    integer(I4B), pointer :: intvar
    call mem_allocate(intvar, idt%mf6varname, memoryPath)
    intvar = parser%GetInteger()
    call idm_log_var(intvar, idt%mf6varname, memoryPath, iout)
    return
  end subroutine load_integer_type

  !> @brief load type 1d integer
  !<
  subroutine load_integer1d_type(parser, idt, memoryPath, mshape, iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(InputParamDefinitionType), intent(in) :: idt !< input data type object describing this record
    character(len=*), intent(in) :: memoryPath !< memorypath to put loaded information
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape !< model shape
    integer(I4B), intent(in) :: iout !< unit number for output
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    !integer(I4B), pointer :: nsize1
    integer(I4B) :: nlay
    integer(I4B) :: nvals
    integer(I4B), dimension(:), allocatable :: array_shape
    integer(I4B), dimension(:), allocatable :: layer_shape
    character(len=LINELENGTH) :: keyword

    ! Check if it is a full grid sized array (NODES), otherwise use
    ! idt%shape to construct shape from variables in memoryPath
    if (idt%shape == 'NODES') then
      nvals = product(mshape)
    else
      call get_shape_from_string(idt%shape, array_shape, memoryPath)
      nvals = array_shape(1)
    end if

    ! allocate memory for the array
    call mem_allocate(int1d, nvals, idt%mf6varname, memoryPath)

    ! check to see if the user specified "LAYERED" input
    keyword = ''
    if (idt%layered) then
      call parser%GetStringCaps(keyword)
    end if

    ! read the array from the input file
    if (keyword == 'LAYERED' .and. idt%layered) then
      call get_layered_shape(mshape, nlay, layer_shape)
      call read_int1d_layered(parser, int1d, idt%mf6varname, nlay, layer_shape)
    else
      call read_int1d(parser, int1d, idt%mf6varname)
    end if

    ! log information on the loaded array to the list file
    call idm_log_var(int1d, idt%mf6varname, memoryPath, iout)
    return
  end subroutine load_integer1d_type

  !> @brief load type 2d integer
  !<
  subroutine load_integer2d_type(parser, idt, memoryPath, mshape, iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(InputParamDefinitionType), intent(in) :: idt !< input data type object describing this record
    character(len=*), intent(in) :: memoryPath !< memorypath to put loaded information
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape !< model shape
    integer(I4B), intent(in) :: iout !< unit number for output
    integer(I4B), dimension(:, :), pointer, contiguous :: int2d
    integer(I4B) :: nlay
    integer(I4B) :: nsize1, nsize2
    integer(I4B), dimension(:), allocatable :: array_shape
    integer(I4B), dimension(:), allocatable :: layer_shape
    character(len=LINELENGTH) :: keyword

    ! determine the array shape from the input data defintion (idt%shape),
    ! which looks like "NCOL, NROW, NLAY"
    call get_shape_from_string(idt%shape, array_shape, memoryPath)
    nsize1 = array_shape(1)
    nsize2 = array_shape(2)

    ! create a new 3d memory managed variable
    call mem_allocate(int2d, nsize1, nsize2, idt%mf6varname, memoryPath)

    ! check to see if the user specified "LAYERED" input
    keyword = ''
    if (idt%layered) then
      call parser%GetStringCaps(keyword)
    end if

    ! read the array from the input file
    if (keyword == 'LAYERED' .and. idt%layered) then
      call get_layered_shape(mshape, nlay, layer_shape)
      call read_int2d_layered(parser, int2d, idt%mf6varname, nlay, layer_shape)
    else
      call read_int2d(parser, int2d, idt%mf6varname)
    end if

    ! log information on the loaded array to the list file
    call idm_log_var(int2d, idt%mf6varname, memoryPath, iout)
    return
  end subroutine load_integer2d_type

  !> @brief load type 3d integer
  !<
  subroutine load_integer3d_type(parser, idt, memoryPath, mshape, iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(InputParamDefinitionType), intent(in) :: idt !< input data type object describing this record
    character(len=*), intent(in) :: memoryPath !< memorypath to put loaded information
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape !< model shape
    integer(I4B), intent(in) :: iout !< unit number for output
    integer(I4B), dimension(:, :, :), pointer, contiguous :: int3d
    integer(I4B) :: nlay
    integer(I4B) :: nsize1, nsize2, nsize3
    integer(I4B), dimension(:), allocatable :: array_shape
    integer(I4B), dimension(:), allocatable :: layer_shape
    character(len=LINELENGTH) :: keyword
    integer(I4B), dimension(:), pointer, contiguous :: int1d_ptr

    ! determine the array shape from the input data defintion (idt%shape),
    ! which looks like "NCOL, NROW, NLAY"
    call get_shape_from_string(idt%shape, array_shape, memoryPath)
    nsize1 = array_shape(1)
    nsize2 = array_shape(2)
    nsize3 = array_shape(3)

    ! create a new 3d memory managed variable
    call mem_allocate(int3d, nsize1, nsize2, nsize3, idt%mf6varname, &
                      memoryPath)

    ! check to see if the user specified "LAYERED" input
    keyword = ''
    if (idt%layered) then
      call parser%GetStringCaps(keyword)
    end if

    ! read the array from the input file
    if (keyword == 'LAYERED' .and. idt%layered) then
      call get_layered_shape(mshape, nlay, layer_shape)
      call read_int3d_layered(parser, int3d, idt%mf6varname, nlay, &
                              layer_shape)
    else
      int1d_ptr(1:nsize1 * nsize2 * nsize3) => int3d(:, :, :)
      call read_int1d(parser, int1d_ptr, idt%mf6varname)
    end if

    ! log information on the loaded array to the list file
    call idm_log_var(int3d, idt%mf6varname, memoryPath, iout)

    return
  end subroutine load_integer3d_type

  !> @brief load type double
  !<
  subroutine load_double_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(InputParamDefinitionType), intent(in) :: idt !< input data type object describing this record
    character(len=*), intent(in) :: memoryPath !< memorypath to put loaded information
    integer(I4B), intent(in) :: iout !< unit number for output
    real(DP), pointer :: dblvar
    call mem_allocate(dblvar, idt%mf6varname, memoryPath)
    dblvar = parser%GetDouble()
    call idm_log_var(dblvar, idt%mf6varname, memoryPath, iout)
    return
  end subroutine load_double_type

  !> @brief load type 1d double
  !<
  subroutine load_double1d_type(parser, idt, memoryPath, mshape, iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(InputParamDefinitionType), intent(in) :: idt !< input data type object describing this record
    character(len=*), intent(in) :: memoryPath !< memorypath to put loaded information
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape !< model shape
    integer(I4B), intent(in) :: iout !< unit number for output
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    !integer(I4B), pointer :: nsize1
    integer(I4B) :: nlay
    integer(I4B) :: nvals
    integer(I4B), dimension(:), allocatable :: array_shape
    integer(I4B), dimension(:), allocatable :: layer_shape
    character(len=LINELENGTH) :: keyword

    ! Check if it is a full grid sized array (NODES)
    if (idt%shape == 'NODES') then
      nvals = product(mshape)
    else
      call get_shape_from_string(idt%shape, array_shape, memoryPath)
      nvals = array_shape(1)
    end if

    ! allocate memory for the array
    call mem_allocate(dbl1d, nvals, idt%mf6varname, memoryPath)

    ! check to see if the user specified "LAYERED" input
    keyword = ''
    if (idt%layered) then
      call parser%GetStringCaps(keyword)
    end if

    ! read the array from the input file
    if (keyword == 'LAYERED' .and. idt%layered) then
      call get_layered_shape(mshape, nlay, layer_shape)
      call read_dbl1d_layered(parser, dbl1d, idt%mf6varname, nlay, layer_shape)
    else
      call read_dbl1d(parser, dbl1d, idt%mf6varname)
    end if

    ! log information on the loaded array to the list file
    call idm_log_var(dbl1d, idt%mf6varname, memoryPath, iout)
    return
  end subroutine load_double1d_type

  !> @brief load type 2d double
  !<
  subroutine load_double2d_type(parser, idt, memoryPath, mshape, iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(InputParamDefinitionType), intent(in) :: idt !< input data type object describing this record
    character(len=*), intent(in) :: memoryPath !< memorypath to put loaded information
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape !< model shape
    integer(I4B), intent(in) :: iout !< unit number for output
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    integer(I4B) :: nlay
    integer(I4B) :: nsize1, nsize2
    integer(I4B), dimension(:), allocatable :: array_shape
    integer(I4B), dimension(:), allocatable :: layer_shape
    character(len=LINELENGTH) :: keyword

    ! determine the array shape from the input data defintion (idt%shape),
    ! which looks like "NCOL, NROW, NLAY"
    call get_shape_from_string(idt%shape, array_shape, memoryPath)
    nsize1 = array_shape(1)
    nsize2 = array_shape(2)

    ! create a new 3d memory managed variable
    call mem_allocate(dbl2d, nsize1, nsize2, idt%mf6varname, memoryPath)

    ! check to see if the user specified "LAYERED" input
    keyword = ''
    if (idt%layered) then
      call parser%GetStringCaps(keyword)
    end if

    ! read the array from the input file
    if (keyword == 'LAYERED' .and. idt%layered) then
      call get_layered_shape(mshape, nlay, layer_shape)
      call read_dbl2d_layered(parser, dbl2d, idt%mf6varname, nlay, layer_shape)
    else
      call read_dbl2d(parser, dbl2d, idt%mf6varname)
    end if

    ! log information on the loaded array to the list file
    call idm_log_var(dbl2d, idt%mf6varname, memoryPath, iout)
    return
  end subroutine load_double2d_type

  !> @brief load type 3d double
  !<
  subroutine load_double3d_type(parser, idt, memoryPath, mshape, iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(InputParamDefinitionType), intent(in) :: idt !< input data type object describing this record
    character(len=*), intent(in) :: memoryPath !< memorypath to put loaded information
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: mshape !< model shape
    integer(I4B), intent(in) :: iout !< unit number for output
    real(DP), dimension(:, :, :), pointer, contiguous :: dbl3d
    integer(I4B) :: nlay
    integer(I4B) :: nsize1, nsize2, nsize3
    integer(I4B), dimension(:), allocatable :: array_shape
    integer(I4B), dimension(:), allocatable :: layer_shape
    character(len=LINELENGTH) :: keyword
    real(DP), dimension(:), pointer, contiguous :: dbl1d_ptr

    ! determine the array shape from the input data defintion (idt%shape),
    ! which looks like "NCOL, NROW, NLAY"
    call get_shape_from_string(idt%shape, array_shape, memoryPath)
    nsize1 = array_shape(1)
    nsize2 = array_shape(2)
    nsize3 = array_shape(3)

    ! create a new 3d memory managed variable
    call mem_allocate(dbl3d, nsize1, nsize2, nsize3, idt%mf6varname, &
                      memoryPath)

    ! check to see if the user specified "LAYERED" input
    keyword = ''
    if (idt%layered) then
      call parser%GetStringCaps(keyword)
    end if

    ! read the array from the input file
    if (keyword == 'LAYERED' .and. idt%layered) then
      call get_layered_shape(mshape, nlay, layer_shape)
      call read_dbl3d_layered(parser, dbl3d, idt%mf6varname, nlay, &
                              layer_shape)
    else
      dbl1d_ptr(1:nsize1 * nsize2 * nsize3) => dbl3d(:, :, :)
      call read_dbl1d(parser, dbl1d_ptr, idt%mf6varname)
    end if

    ! log information on the loaded array to the list file
    call idm_log_var(dbl3d, idt%mf6varname, memoryPath, iout)

    return
  end subroutine load_double3d_type

  !> @brief routine for setting the model shape
  !!
  !! The model shape must be set in the memory manager because
  !! individual packages need to know the shape of the arrays
  !! to read.
  !!
  !<
  subroutine set_model_shape(ftype, model_mempath, dis_mempath, model_shape)
    use MemoryTypeModule, only: MemoryType
    use MemoryManagerModule, only: get_from_memorylist
    character(len=*), intent(in) :: ftype
    character(len=*), intent(in) :: model_mempath
    character(len=*), intent(in) :: dis_mempath
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: model_shape
    integer(I4B), pointer :: ndim1
    integer(I4B), pointer :: ndim2
    integer(I4B), pointer :: ndim3

    select case (ftype)
    case ('DIS6')
      call mem_allocate(model_shape, 3, 'MODEL_SHAPE', model_mempath)
      call mem_setptr(ndim1, 'NLAY', dis_mempath)
      call mem_setptr(ndim2, 'NROW', dis_mempath)
      call mem_setptr(ndim3, 'NCOL', dis_mempath)
      model_shape = [ndim1, ndim2, ndim3]
    case ('DISV6')
      call mem_allocate(model_shape, 2, 'MODEL_SHAPE', model_mempath)
      call mem_setptr(ndim1, 'NLAY', dis_mempath)
      call mem_setptr(ndim2, 'NCPL', dis_mempath)
      model_shape = [ndim1, ndim2]
    case ('DISU6')
      call mem_allocate(model_shape, 1, 'MODEL_SHAPE', model_mempath)
      call mem_setptr(ndim1, 'NODES', dis_mempath)
      model_shape = [ndim1]
    end select

    return
  end subroutine set_model_shape

  subroutine get_layered_shape(mshape, nlay, layer_shape)
    integer(I4B), dimension(:), intent(in) :: mshape
    integer(I4B), intent(out) :: nlay
    integer(I4B), dimension(:), allocatable, intent(out) :: layer_shape
    integer(I4B) :: ndim

    ndim = size(mshape)
    nlay = 0

    if (ndim == 1) then ! disu
      nlay = 1
      allocate (layer_shape(1))
      layer_shape(1) = mshape(1)
    else if (ndim == 2) then ! disv
      nlay = mshape(1)
      allocate (layer_shape(1))
      layer_shape(1) = mshape(2)
    else if (ndim == 3) then ! disu
      nlay = mshape(1)
      allocate (layer_shape(2))
      layer_shape(1) = mshape(3) ! ncol
      layer_shape(2) = mshape(2) ! nrow
    end if

  end subroutine get_layered_shape

  subroutine get_shape_from_string(shape_string, array_shape, memoryPath)
    character(len=*), intent(in) :: shape_string
    integer(I4B), dimension(:), allocatable, intent(inout) :: array_shape
    character(len=*), intent(in) :: memoryPath !< memorypath to put loaded information
    integer(I4B) :: ndim
    integer(I4B) :: i
    integer(I4B), pointer :: int_ptr
    character(len=16), dimension(:), allocatable :: array_shape_string
    character(len=:), allocatable :: shape_string_copy

    ! parse the string into multiple words
    shape_string_copy = trim(shape_string)//' '
    call ParseLine(shape_string_copy, ndim, array_shape_string)
    allocate (array_shape(ndim))

    ! find shape in memory manager and put into array_shape
    do i = 1, ndim
      call mem_setptr(int_ptr, array_shape_string(i), memoryPath)
      array_shape(i) = int_ptr
    end do

  end subroutine get_shape_from_string

end module LoadMf6FileTypeModule

module LoadMfInputModule
  
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMEMPATH, LENVARNAME, MAXCHARLEN
  use SimVariablesModule, only: iout, errmsg
  use SimModule, only: store_error
  use BlockParserModule, only: BlockParserType
  use ArrayReadersModule, only: ReadArray
  use InputOutputModule,  only: openfile, parseline, getunit
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use InputDefinitionSelectorModule, only: param_definitions, &
                                           aggregate_definitions, &
                                           block_definitions, &
                                           get_param_definition_type, &
                                           get_aggregate_definition_type
  use IdmTypesModule, only: ModflowInputType, ModflowInput
  use MemoryManagerModule, only: mem_allocate, mem_setptr
  use MemoryHelperModule, only: create_mem_path
  use IdmLoggerModule, only: idm_log_type, idm_log_header, idm_log_close
  
  implicit none
  private
  public :: idm_load
  public :: set_model_shape

  interface idm_load
    module procedure idm_load_mf6file, idm_load_from_blockparser
  end interface idm_load
  
  contains

  subroutine idm_load_mf6file(mf6_input, ndim, iout)
    type(ModflowInputType), pointer, intent(in) :: mf6_input
    integer(I4B), pointer, intent(in) :: ndim
    integer(I4B) :: iout
    type(BlockParserType) :: parser
    integer(I4B) :: inunit
    integer(I4B) :: iblock

    ! ensure aggregate definitions have dimensions
    call update_aggregate_shapes(mf6_input, iout)

    ! open the file
    inunit =  getunit()
    call openfile(inunit, 0, mf6_input%file_spec, mf6_input%file_type)

    ! initialize parser
    call parser%initialize(inunit, iout)

    ! process blocks
    do iblock = 1, size(mf6_input%p_block_dfns)
      call parse_block(parser, mf6_input, iblock, ndim, iout)
    end do

    ! close
    close(inunit)
  end subroutine idm_load_mf6file

  subroutine idm_load_from_blockparser(parser, filetype, filename, &
                                       component_type, subcomponent_type, &
                                       component_name, subcomponent_name, &
                                       ndim, iout)
    type(BlockParserType), intent(inout) :: parser
    character(len=*), intent(in) :: filetype
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    integer(I4B), pointer, intent(in) :: ndim
    integer(I4B) :: iout
    integer(I4B) :: iblock
    type(ModflowInputType), pointer :: mf6_input

    mf6_input => ModflowInput(filetype, filename, component_type, &
                              subcomponent_type, component_name, &
                              subcomponent_name)
    write(iout, '(4x,a)') 'idm_load mempath='//trim(mf6_input%memoryPath)

    call idm_log_header(mf6_input%component_name, iout)

    ! ensure aggregate definitions have dimensions
    call update_aggregate_shapes(mf6_input, iout)

    ! process blocks
    do iblock = 1, size(mf6_input%p_block_dfns)
      call parse_block(parser, mf6_input, iblock, ndim, iout)
    end do

    call idm_log_close(mf6_input%component_name, iout)
  end subroutine idm_load_from_blockparser

  subroutine parse_block(parser, mf6_input, iblock, ndim, iout)
    type(BlockParserType), intent(inout) :: parser
    type(ModflowInputType), pointer, intent(in) :: mf6_input
    integer(I4B), intent(in) :: iblock
    integer(I4B), pointer, intent(in) :: ndim
    integer(I4B), intent(in) :: iout
    logical(LGP) :: isblockfound
    logical(LGP) :: endOfBlock
    logical(LGP) :: supportOpenClose
    integer(I4B) :: ierr

    supportOpenClose = (mf6_input%p_block_dfns(iblock)%blockname == 'GRIDDATA')

    call parser%GetBlock(mf6_input%p_block_dfns(iblock)%blockname, isblockfound, ierr, &
                         supportOpenClose=supportOpenClose, &
                         blockRequired=mf6_input%p_block_dfns(iblock)%required)

    if (isblockfound) then
      !write(iout, '(4x,a,a)') 'block: ', trim(mf6_input%p_block_dfns(iblock)%blockname)
      if (mf6_input%p_block_dfns(iblock)%aggregate) then
        call parse_structarray_block(parser, mf6_input, iblock, ndim, iout)

      else
        do
          ! process each line in block
          call parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit

          call parse_tag(parser, mf6_input, iblock, ndim, iout, .false.)
        end do
      end if
    end if

    return
  end subroutine parse_block

  recursive subroutine parse_tag(parser, mf6_input, iblock, ndim, iout, recursive_call)
    type(BlockParserType), intent(inout) :: parser
    type(ModflowInputType), pointer, intent(in) :: mf6_input
    integer(I4B), intent(in) :: iblock
    integer(I4B), pointer, intent(in) :: ndim
    integer(I4B), intent(in) :: iout
    logical(LGP), intent(in) :: recursive_call
    character(len=LINELENGTH) :: tag
    type(InputParamDefinitionType), pointer :: idt

    ! read tag name
    call parser%GetStringCaps(tag)
    !write(iout, '(4x,a,a)') 'tag: ', trim(tag)
    if (recursive_call) then
      if (tag == '') then
        ! no data on line so return
        return
      end if
    end if

    ! find keyword in input definition
    idt => get_param_definition_type(mf6_input%p_param_dfns, &
                                     mf6_input%component_type, &
                                     mf6_input%subcomponent_type, &
                                     tag)

    select case (idt%datatype)
      case('KEYWORD')
        call load_keyword_type(parser, idt, mf6_input%memoryPath, iout)
      case('STRING')
        call load_string_type(parser, idt, mf6_input%memoryPath, iout)
      case('INTEGER')
        call load_integer_type(parser, idt, mf6_input%memoryPath, iout)
      case('INTEGER1D')
        call load_integer1d_type(parser, idt, mf6_input, iout)
      case('INTEGER3D')
        call load_integer3d_type(parser, idt, mf6_input%memoryPath, ndim, iout)
      case('DOUBLE')
        call load_double_type(parser, idt, mf6_input%memoryPath, iout)
      case('DOUBLE1D')
        call load_double1d_type(parser, idt, mf6_input, iout)
      case('DOUBLE2D')
        call load_double2d_type(parser, idt, mf6_input%memoryPath, ndim, iout)
      case('DOUBLE3D')
        call load_double3d_type(parser, idt, mf6_input%memoryPath, ndim, iout)
      case default
        write(errmsg, '(4x,a,a)') 'Failure reading data for tag: ', trim(tag)
        call store_error(errmsg)
        call parser%StoreErrorUnit()
    end select

    if (idt%in_record) then
      ! recursively call parse tag again to read rest of line
      !call parse_tag(parser, component_type, subcomponent_type, &
      !  blockname, iout, memoryPath, .true., input_definition_types)
      call parse_tag(parser, mf6_input, iblock, ndim, iout, .true.)
    end if

    return
  end subroutine parse_tag

  subroutine parse_structarray_block(parser, mf6_input, iblock, ndim, iout)
    use StructArrayModule, only: StructArrayType, constructStructArray
    type(BlockParserType), intent(inout) :: parser
    type(ModflowInputType), pointer, intent(in) :: mf6_input
    integer(I4B), intent(in) :: iblock
    integer(I4B), pointer, intent(in) :: ndim
    integer(I4B), intent(in) :: iout
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B), pointer :: nrow
    integer(I4B) :: icol
    integer(I4B) :: ncol
    integer(I4B) :: nwords
    character(len=16), dimension(:), allocatable :: words
    type(StructArrayType), pointer :: struct_array

    ! find the data type for this block
    idt => get_aggregate_definition_type(mf6_input%p_aggregate_dfns, mf6_input%component_type, &
      mf6_input%subcomponent_type, mf6_input%p_block_dfns(iblock)%blockname)

    ! parse to find the names of the columns in this struct array
    ! ncol is one less than nwords because RECARRAY is first item
    call parseline(idt%datatype, nwords, words)
    ncol = nwords - 1

    ! use shape to set the max num of rows for this struct array
    call mem_setptr(nrow, idt%shape, mf6_input%memoryPath)

    ! create a structured array
    struct_array => constructStructArray(ncol, nrow)
    do icol = 1, ncol

      ! set pointer to input definition for this 1d vector
      idt => get_param_definition_type(mf6_input%p_param_dfns, mf6_input%component_type, &
        mf6_input%subcomponent_type, words(icol + 1))

      ! allocate a new vector in the memory manager using idt information
      ! and add it to the struct_array in position icol
      call struct_array%mem_create_vector(icol, idt%datatype, nrow, &
                                          idt%mf6varname, mf6_input%memoryPath, &
                                          idt%preserve_case)
    end do

    ! read the structured array
    call struct_array%read_from_parser(parser, iout)
    call parser%terminateblock()

    ! destroy the structured array reader

    return
  end subroutine parse_structarray_block
  
  subroutine load_keyword_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    integer(I4B), pointer :: intvar
    call mem_allocate(intvar, idt%mf6varname, memoryPath)
    intvar = 1
    call idm_log_type(intvar, idt%mf6varname, memoryPath, 'TEST', iout)
    return
  end subroutine load_keyword_type
  
  subroutine load_string_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    character(len=LINELENGTH), pointer :: cstr
    integer(I4B) :: ilen
    ilen = LINELENGTH
    call mem_allocate(cstr, ilen, idt%mf6varname, memoryPath)
    call parser%GetString(cstr, (.not. idt%preserve_case))
    return
  end subroutine load_string_type
  
  subroutine load_integer_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    integer(I4B), pointer :: intvar
    call mem_allocate(intvar, idt%mf6varname, memoryPath)
    intvar = parser%GetInteger()
    call idm_log_type(intvar, idt%mf6varname, memoryPath, 'TEST', iout)
    return
  end subroutine load_integer_type
  
  subroutine load_integer1d_type(parser, idt, mf6_input, iout)
    use SimVariablesModule, only: idm_mempath_prefix
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    type(ModflowInputType), pointer, intent(in) :: mf6_input
    integer(I4B), intent(in) :: iout
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B), pointer :: nsize1
    integer(I4B) :: nvals
    integer(I4B), dimension(:), contiguous, pointer :: mshape
    character(len=LENMEMPATH) :: idmMemoryPath

    idmMemoryPath = create_mem_path(component=mf6_input%component_name, context=idm_mempath_prefix)
    
    if (idt%shape == 'NODES') then
      call mem_setptr(mshape, 'MODEL_SHAPE', idmMemoryPath)
      nvals = product(mshape)
      call mem_allocate(int1d, nvals, idt%mf6varname, mf6_input%memoryPath)
    else
      call mem_setptr(nsize1, idt%shape, mf6_input%memoryPath)
      call mem_allocate(int1d, nsize1, idt%mf6varname, mf6_input%memoryPath)
    end if
    
    call read_grid_array(parser, mshape, idt%tagname, intarray=int1d)

    call idm_log_type(int1d, idt%mf6varname, mf6_input%memoryPath, 'TEST', iout)
    return
  end subroutine load_integer1d_type

  subroutine load_integer3d_type(parser, idt, memoryPath, ndim, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), pointer, intent(in) :: ndim
    integer(I4B), intent(in) :: iout
    integer(I4B), dimension(:, :, :), pointer, contiguous :: int3d
    integer(I4B), pointer :: nsize1
    integer(I4B), pointer :: nsize2
    integer(I4B), pointer :: nsize3
    character(len=LINELENGTH) :: keyword

    ! find sizes in memory manager
    call mem_setptr(nsize1, 'NCOL', memoryPath)
    call mem_setptr(nsize2, 'NROW', memoryPath)
    call mem_setptr(nsize3, 'NLAY', memoryPath)
    
    ! allocate the array using the memory manager
    call mem_allocate(int3d, nsize1, nsize2, nsize3, idt%mf6varname, memoryPath)
    
    ! fill the array from the file
    if (idt%blockname == 'GRIDDATA') then
      call parser%GetStringCaps(keyword)
      if (keyword == 'LAYERED') then
        ! read by layer
        call ReadArray(parser%iuactive, int3d(:,:,:), &
                       idt%mf6varname, ndim, nsize1, nsize2, &
                       nsize3, iout, 1, nsize3)
      else
        ! read full 3d array
        call ReadArray(parser%iuactive, int3d(:,:,:), idt%mf6varname, &
                       ndim, nsize1 * nsize2 * nsize3, iout)
      end if
    else
      ! read full 3d array
      call ReadArray(parser%iuactive, int3d(:,:,:), idt%mf6varname, &
                     ndim, nsize1 * nsize2 * nsize3, iout)
    end if
    
    call idm_log_type(int3d, idt%mf6varname, memoryPath, 'TEST', iout)
    return
  end subroutine load_integer3d_type
  
  subroutine load_double_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    real(DP), pointer :: dblvar
    call mem_allocate(dblvar, idt%mf6varname, memoryPath)
    dblvar = parser%GetDouble()
    call idm_log_type(dblvar, idt%mf6varname, memoryPath, 'TEST', iout)
    return
  end subroutine load_double_type
  
  subroutine load_double1d_type(parser, idt, mf6_input, iout)
    use SimVariablesModule, only: idm_mempath_prefix
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    type(ModflowInputType), pointer, intent(in) :: mf6_input
    integer(I4B), intent(in) :: iout
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B), pointer :: nsize1
    integer(I4B) :: nvals
    integer(I4B), dimension(:), contiguous, pointer :: mshape
    character(len=LENMEMPATH) :: idmMemoryPath

    idmMemoryPath = create_mem_path(component=mf6_input%component_name, context=idm_mempath_prefix)
    
    if (idt%shape == 'NODES') then
      call mem_setptr(mshape, 'MODEL_SHAPE', idmMemoryPath)
      nvals = product(mshape)
      call mem_allocate(dbl1d, nvals, idt%mf6varname, mf6_input%memoryPath)
    else
      call mem_setptr(nsize1, idt%shape, mf6_input%memoryPath)
      call mem_allocate(dbl1d, nsize1, idt%mf6varname, mf6_input%memoryPath)
      allocate(mshape(1))
      mshape(1) = nsize1
    end if
    
    call read_grid_array(parser, mshape, idt%tagname, dbl1d)
    call idm_log_type(dbl1d, idt%mf6varname, mf6_input%memoryPath, 'TEST', iout)
    return
  end subroutine load_double1d_type
  
  subroutine load_double2d_type(parser, idt, memoryPath, ndim, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), pointer, intent(in) :: ndim
    integer(I4B), intent(in) :: iout
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    integer(I4B), pointer :: nsize1
    integer(I4B), pointer :: nsize2

    
    ! find sizes in memory manager
    call mem_setptr(nsize1, 'NCOL', memoryPath)
    call mem_setptr(nsize2, 'NROW', memoryPath)
    
    ! allocate the array using the memory manager
    call mem_allocate(dbl2d, nsize1, nsize2, idt%mf6varname, memoryPath)
    
    ! fill the array from the file
    call ReadArray(parser%iuactive, dbl2d, idt%mf6varname, &
                   ndim, nsize1, nsize2, iout, 0)
    
    call idm_log_type(dbl2d, idt%mf6varname, memoryPath, 'TEST', iout)
    return
  end subroutine load_double2d_type
  
  subroutine load_double3d_type(parser, idt, memoryPath, ndim, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), pointer, intent(in) :: ndim
    integer(I4B), intent(in) :: iout
    real(DP), dimension(:, :, :), pointer, contiguous :: dbl3d
    integer(I4B), pointer :: nsize1
    integer(I4B), pointer :: nsize2
    integer(I4B), pointer :: nsize3
    character(len=LINELENGTH) :: keyword

    ! find sizes in memory manager
    call mem_setptr(nsize1, 'NCOL', memoryPath)
    call mem_setptr(nsize2, 'NROW', memoryPath)
    call mem_setptr(nsize3, 'NLAY', memoryPath)
    
    ! allocate the array using the memory manager
    call mem_allocate(dbl3d, nsize1, nsize2, nsize3, idt%mf6varname, memoryPath)
    
    ! fill the array from the file
    if (idt%blockname == 'GRIDDATA') then
      call parser%GetStringCaps(keyword)
      if (keyword == 'LAYERED') then
        ! read by layer
        call ReadArray(parser%iuactive, dbl3d(:,:,:), &
                        idt%mf6varname, ndim, nsize1, nsize2, &
                        nsize3, iout, 1, nsize3)
      else
        ! read full 3d array
        call ReadArray(parser%iuactive, dbl3d(:,:,:), idt%mf6varname, &
                       ndim, nsize1 * nsize2 * nsize3, iout)
      end if
    else
      ! read full 3d array
      call ReadArray(parser%iuactive, dbl3d(:,:,:), idt%mf6varname, &
                     ndim, nsize1 * nsize2 * nsize3, iout)
    end if
    
    call idm_log_type(dbl3d, idt%mf6varname, memoryPath, 'TEST', iout)
    return
  end subroutine load_double3d_type
  
  subroutine set_model_shape(ftype, model_mempath, dis_mempath)
    character(len=*) :: ftype
    character(len=*), intent(in) :: model_mempath
    character(len=*), intent(in) :: dis_mempath
    integer(I4B), dimension(:), pointer, contiguous :: model_shape
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

  subroutine update_aggregate_shapes(mf6_input, iout)
    type(ModflowInputType), pointer, intent(in) :: mf6_input
    integer(I4B), intent(in) :: iout
    type(BlockParserType) :: parser
    integer(I4B) :: inunit
    logical(LGP) :: update_shapes
    integer(I4B) :: iaggregate, iblock
    logical(LGP) :: isblockfound, endOfBlock
    integer(I4B) :: ierr
    integer(I4B) :: nlines
    integer(I4B), pointer :: blockshape

    ! check if file needs to be opened
    update_shapes = .false.
    do iaggregate = 1, size(mf6_input%p_aggregate_dfns)
      if (mf6_input%p_aggregate_dfns(iaggregate)%shape == '') then
        update_shapes = .true.
        exit
      end if
    end do

    ! file has to be read so check all blocks
    if (update_shapes) then
      ! set up file parsing
      inunit = getunit()
      call openfile(inunit, 0, mf6_input%file_spec, mf6_input%file_type)
      call parser%initialize(inunit, iout)

      ! check and update shape for each block against aggregate dfn
      do iblock = 1, size(mf6_input%p_block_dfns)
        call parser%GetBlock(mf6_input%p_block_dfns(iblock)%blockname, isblockfound, ierr)

        if (isblockfound) then
          nlines = 0
          do
            call parser%GetNextLine(endOfBlock)
            if (endOfBlock) then
              exit
            else
              nlines = nlines + 1
            end if
          end do

          do iaggregate = 1, size(mf6_input%p_aggregate_dfns)
            if (mf6_input%p_aggregate_dfns(iaggregate)%blockname == &
                mf6_input%p_block_dfns(iblock)%blockname .and. &
                mf6_input%p_aggregate_dfns(iaggregate)%shape == '') then
              ! TODO: handle exceeding LENVARNAME
              mf6_input%p_aggregate_dfns(iaggregate)%shape = 'n'//trim(mf6_input%p_aggregate_dfns(iaggregate)%mf6varname)
              call mem_allocate(blockshape, mf6_input%p_aggregate_dfns(iaggregate)%shape, mf6_input%memoryPath)
              blockshape = nlines
            end if
          end do

        end if
      end do

      ! close file and release resourses
      call parser%clear()
    end if
  end subroutine update_aggregate_shapes
    
  subroutine read_grid_array(parser, mshape, array_name, dblarray, intarray)
    type(BlockParserType), intent(inout) :: parser
    integer(I4B), dimension(:), intent(in) :: mshape
    character(len=*), intent(in) :: array_name
    real(DP), dimension(:), optional, intent(inout) :: dblarray
    integer(I4B), dimension(:), optional, intent(inout) :: intarray
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: nvals
    integer(I4B) :: ndim
    integer(I4B) :: ndim1
    integer(I4B) :: ndim2
    integer(I4B) :: ndim3
    integer(I4B) :: k1
    integer(I4B) :: k2
    integer(I4B) :: iout
    
    ndim = size(mshape)
    if (present(dblarray)) then
      nvals = size(dblarray)
    end if
    if (present(intarray)) then
      nvals = size(intarray)
    end if
    iout = 0
    
    ! disu
    if (ndim == 1) then
      ndim1 = mshape(1)  ! nodesuser
      ndim2 = 1          ! none
      ndim3 = 1          ! none
      k1 = 0
      k2 = 0
      
    ! disv
    else if (ndim == 2) then
      ndim1 = mshape(1)  ! nlay
      ndim2 = 1          ! none
      ndim3 = mshape(2)  ! ncpl
      k1 = 1
      k2 = ndim1
      
    ! dis
    else if (ndim == 3) then
      ndim1 = mshape(1)  ! nlay
      ndim2 = mshape(2)  ! nrow
      ndim3 = mshape(3)  ! ncol
      k1 = 1
      k2 = ndim1
    end if
    
    call parser%GetStringCaps(keyword)
    if (keyword == 'LAYERED') then

      ! float array
      if (present(dblarray)) then
        call ReadArray(parser%iuactive, dblarray, &
                       array_name, ndim, ndim3, ndim2, &
                       ndim1, nvals, iout, k1, k2)
      end if

      ! integer array
      if (present(intarray)) then
        call ReadArray(parser%iuactive, intarray, &
                       array_name, ndim, ndim3, ndim2, &
                       ndim1, nvals, iout, k1, k2)
      end if
      
      
    else
      
      ! float array
      if (present(dblarray)) then
        call ReadArray(parser%iuactive, dblarray, array_name, &
                       ndim, nvals, iout, 0)
      end if

      ! integer array
      if (present(intarray)) then
        call ReadArray(parser%iuactive, intarray, array_name, &
                       ndim, nvals, iout, 0)
      end if
      
      
    end if

    return
  end subroutine read_grid_array
  
      
end module LoadMfInputModule

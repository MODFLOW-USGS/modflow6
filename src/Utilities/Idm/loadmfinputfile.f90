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
  public :: idm_load, idm_deallocate
  public :: set_model_shape

  interface idm_load
    module procedure idm_load_modflowfile, idm_load_modflowfile_from_parser
  end interface idm_load
  
  contains

  subroutine idm_load_modflowfile(pModflowInput, iout)
    type(ModflowInputType), pointer, intent(in) :: pModflowInput
    integer(I4B) :: iout
    type(BlockParserType) :: parser
    integer(I4B) :: inunit
    integer(I4B) :: iblock

    ! ensure aggregate definitions have dimensions
    call update_aggregate_shapes(pModflowInput, iout)

    ! open the file
    inunit =  getunit()
    call openfile(inunit, 0, pModflowInput%file_spec, pModflowInput%file_type)

    ! initialize parser
    call parser%initialize(inunit, iout)

    ! process blocks
    do iblock = 1, size(pModflowInput%p_block_dfns)
      call parse_block(parser, pModflowInput, iblock, iout)
    end do

    ! close
    close(inunit)
  end subroutine idm_load_modflowfile

  subroutine idm_load_modflowfile_from_parser(parser, filetype, filename, &
                                              component_type, subcomponent_type, &
                                              component_name, subcomponent_name, &
                                              iout)
    type(BlockParserType), intent(inout) :: parser
    character(len=*), intent(in) :: filetype
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    integer(I4B) :: iout
    integer(I4B) :: iblock
    type(ModflowInputType), target :: pModflowInput

    pModflowInput = ModflowInput(filetype, filename, component_type, &
                                 subcomponent_type, component_name, &
                                 subcomponent_name)

    call idm_log_header(pModflowInput%component_name, iout)

    ! ensure aggregate definitions have dimensions
    call update_aggregate_shapes(pModflowInput, iout)

    ! process blocks
    do iblock = 1, size(pModflowInput%p_block_dfns)
      call parse_block(parser, pModflowInput, iblock, iout)
    end do

    call idm_log_close(pModflowInput%component_name, iout)
  end subroutine idm_load_modflowfile_from_parser

  !subroutine idm_deallocate(pModflowInput, iout)
  subroutine idm_deallocate(filetype, filename, component_type, &
                            subcomponent_type, component_name, &
                            subcomponent_name, iout)
    use MemoryTypeModule, only: MemoryType
    use MemoryManagerModule, only: get_from_memorylist, mem_setptr, mem_deallocate, get_mem_rank
    use CharacterStringModule, only: CharacterStringType
    use SimVariablesModule, only: idm_mempath_prefix
    character(len=*), intent(in) :: filetype
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    integer(I4B), intent(in) :: iout
    type(ModflowInputType), target :: pModflowInput
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    logical(LGP) :: checkfail = .false.
    type(InputParamDefinitionType), pointer :: tmp_ptr
    integer(I4B) :: i
    character(len=MAXCHARLEN), pointer :: strsclr => null() !< pointer to the character string
    logical(LGP), pointer :: logicalsclr => null() !< pointer to the logical
    integer(I4B), pointer :: intsclr => null() !< pointer to the integer
    real(DP), pointer :: dblsclr => null() !< pointer to the double
    character(len=:), dimension(:), pointer, contiguous :: astr1d => null() !< pointer to the 1d character string array
    integer(I4B), dimension(:), pointer, contiguous :: aint1d => null() !< pointer to 1d integer array
    integer(I4B), dimension(:, :), pointer, contiguous :: aint2d => null() !< pointer to 2d integer array
    integer(I4B), dimension(:, :, :), pointer, contiguous :: aint3d => null() !< pointer to 3d integer array
    real(DP), dimension(:), pointer, contiguous :: adbl1d => null() !< pointer to 1d double array
    real(DP), dimension(:, :), pointer, contiguous :: adbl2d => null() !< pointer to 2d double array
    real(DP), dimension(:, :, :), pointer, contiguous :: adbl3d => null() !< pointer to 3d double array
    type(CharacterStringType), dimension(:), pointer, contiguous :: &
      acharstr1d => null() !< pointer to the 1d character string array
    integer(I4B) :: rank

    pModflowInput = ModflowInput(filetype, filename, component_type, &
                                 subcomponent_type, component_name, &
                                 subcomponent_name)

    found = .false.
    call get_from_memorylist('MODEL_SHAPE', trim(idm_mempath_prefix)//trim(pModflowInput%component_name), mt, found, checkfail)
    if (found) then
      call mem_setptr(aint1d, 'MODEL_SHAPE', trim(idm_mempath_prefix)//trim(pModflowInput%component_name))
      call mem_deallocate(aint1d)
    end if

    do i = 1, size(pModflowInput%p_param_dfns)
      found = .false.
      tmp_ptr => pModflowInput%p_param_dfns(i)
      call get_from_memorylist(tmp_ptr%mf6varname, pModflowInput%memoryPath, mt, found, checkfail)
      if (found) then
        rank = -1
        call get_mem_rank(tmp_ptr%mf6varname, pModflowInput%memoryPath, rank)
        select case (mt%memtype(1:index(mt%memtype, ' ')))
        case ('LOGICAL')
          call mem_setptr(logicalsclr, tmp_ptr%mf6varname, pModflowInput%memoryPath)
          call mem_deallocate(logicalsclr)
        case ('INTEGER')
          if (rank == 0) then
            call mem_setptr(intsclr, tmp_ptr%mf6varname, pModflowInput%memoryPath)
            call mem_deallocate(intsclr)
          elseif (rank == 1) then
            call mem_setptr(aint1d, tmp_ptr%mf6varname, pModflowInput%memoryPath)
            call mem_deallocate(aint1d)
          elseif (rank == 2) then
            call mem_setptr(aint2d, tmp_ptr%mf6varname, pModflowInput%memoryPath)
            call mem_deallocate(aint2d)
          elseif (rank == 3) then
            call mem_setptr(aint3d, tmp_ptr%mf6varname, pModflowInput%memoryPath)
            call mem_deallocate(aint3d)
          end if
        case ('DOUBLE')
          if (rank == 0) then
            call mem_setptr(dblsclr, tmp_ptr%mf6varname, pModflowInput%memoryPath)
            call mem_deallocate(dblsclr)
          elseif (rank == 1) then
            call mem_setptr(adbl1d, tmp_ptr%mf6varname, pModflowInput%memoryPath)
            call mem_deallocate(adbl1d)
          elseif (rank == 2) then
            call mem_setptr(adbl2d, tmp_ptr%mf6varname, pModflowInput%memoryPath)
            call mem_deallocate(adbl2d)
          elseif (rank == 3) then
            call mem_setptr(adbl3d, tmp_ptr%mf6varname, pModflowInput%memoryPath)
            call mem_deallocate(adbl3d)
          end if
        case ('STRING')
          if (rank == 0) then
            call mem_setptr(strsclr, tmp_ptr%mf6varname, pModflowInput%memoryPath)
            call mem_deallocate(strsclr)
          !elseif (rank == 1) then
          !  call setptr_charstr1d(acharstr1d, tmp_ptr%mf6varname, pModflowInput%memoryPath)
          !  call mem_deallocate(acharstr1d)
          end if
        end select
      end if
    end do
  end subroutine idm_deallocate

  subroutine parse_block(parser, pModflowInput, iblock, iout)
    type(BlockParserType), intent(inout) :: parser
    type(ModflowInputType), pointer, intent(in) :: pModflowInput
    integer(I4B), intent(in) :: iblock
    integer(I4B), intent(in) :: iout
    logical(LGP) :: isblockfound
    logical(LGP) :: istagfound
    logical(LGP) :: endOfBlock
    logical(LGP) :: supportOpenClose
    logical(LGP) :: blockRequired
    logical(LGP) :: isStructArrayBlock
    integer(I4B) :: ierr

    supportOpenClose = (pModflowInput%p_block_dfns(iblock)%blockname == 'GRIDDATA')

    call parser%GetBlock(pModflowInput%p_block_dfns(iblock)%blockname, isblockfound, ierr, &
                         supportOpenClose=supportOpenClose, &
                         blockRequired=pModflowInput%p_block_dfns(iblock)%required)

    if (isblockfound) then
      !write(iout, '(4x,a,a)') 'block: ', trim(pModflowInput%p_block_dfns(iblock)%blockname)
      if (pModflowInput%p_block_dfns(iblock)%aggregate) then
        call parse_structarray_block(parser, pModflowInput, iblock, iout)

      else
        do
          ! process each line in block
          call parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit

          call parse_tag(parser, pModflowInput, iblock, iout, .false.)
        end do
      end if
    end if

    return
  end subroutine parse_block

  recursive subroutine parse_tag(parser, pModflowInput, iblock, iout, recursive_call)
    type(BlockParserType), intent(inout) :: parser
    type(ModflowInputType), pointer, intent(in) :: pModflowInput
    integer(I4B), intent(in) :: iblock
    integer(I4B), intent(in) :: iout
    logical(LGP), intent(in) :: recursive_call
    character(len=LINELENGTH) :: tag
    type(InputParamDefinitionType), pointer :: idt
    character(len=16), dimension(:), allocatable :: words

    ! read tag name
    call parser%GetStringCaps(tag)
    !write(iout, '(4x,a,a)') 'tag: ', trim(tag)
    if (recursive_call) then
      if (tag == '') then
        ! no data on line so return
        return
      end if
    end if
    print *, 'processing input tag ' // trim(tag)

    ! find keyword in input definition
    idt => get_param_definition_type(pModflowInput%p_param_dfns, &
                                     pModflowInput%component_type, &
                                     pModflowInput%subcomponent_type, &
                                     tag)
    print *, 'processing input tag type ' // trim(idt%datatype)

    select case (idt%datatype)
      case('KEYWORD')
        call load_keyword_type(parser, idt, pModflowInput%memoryPath, iout)
      case('STRING')
        call load_string_type(parser, idt, pModflowInput%memoryPath, iout)
      case('INTEGER')
        call load_integer_type(parser, idt, pModflowInput%memoryPath, iout)
      case('INTEGER1D')
        call load_integer1d_type(parser, idt, pModflowInput, iout)
      case('INTEGER3D')
        call load_integer3d_type(parser, idt, pModflowInput%memoryPath, iout)
      case('DOUBLE')
        call load_double_type(parser, idt, pModflowInput%memoryPath, iout)
      case('DOUBLE1D')
        call load_double1d_type(parser, idt, pModflowInput, iout)
      case('DOUBLE2D')
        call load_double2d_type(parser, idt, pModflowInput%memoryPath, iout)
      case('DOUBLE3D')
        call load_double3d_type(parser, idt, pModflowInput%memoryPath, iout)
      case default
        write(errmsg, '(4x,a,a)') 'Failure reading data for tag: ', trim(tag)
        call store_error(errmsg)
        call parser%StoreErrorUnit()
    end select

    if (idt%in_record) then
      ! recursively call parse tag again to read rest of line
      !call parse_tag(parser, component_type, subcomponent_type, &
      !  blockname, iout, memoryPath, .true., input_definition_types)
      call parse_tag(parser, pModflowInput, iblock, iout, .true.)
    end if

    return
  end subroutine parse_tag

  subroutine parse_structarray_block(parser, pModflowInput, iblock, iout)
    use StructArrayModule, only: StructArrayType, constructStructArray
    type(BlockParserType), intent(inout) :: parser
    type(ModflowInputType), pointer, intent(in) :: pModflowInput
    integer(I4B), intent(in) :: iblock
    integer(I4B), intent(in) :: iout
    type(InputParamDefinitionType), pointer :: idt
    character(len=LINELENGTH) :: tag
    integer(I4B), pointer :: nrow
    integer(I4B) :: icol
    integer(I4B) :: ncol
    integer(I4B) :: nwords
    character(len=16), dimension(:), allocatable :: words
    type(StructArrayType) :: struct_array

    ! find the data type for this block
    idt => get_aggregate_definition_type(pModflowInput%p_aggregate_dfns, pModflowInput%component_type, &
      pModflowInput%subcomponent_type, pModflowInput%p_block_dfns(iblock)%blockname)

    ! parse to find the names of the columns in this struct array
    ! ncol is one less than nwords because STRUCTARRAY is first item
    !call parseline(idt%mf6varname, nwords, words)
    call parseline(idt%datatype, nwords, words)
    ncol = nwords - 1

    ! use shape to set the max num of rows for this struct array
    call mem_setptr(nrow, idt%shape, pModflowInput%memoryPath)
    !write(iout, '(4x,a,i3)') 'SA nrow: ', nrow

    ! create a structured array
    struct_array = constructStructArray(ncol, nrow)
    do icol = 1, ncol

      ! set pointer to input definition for this 1d vector
      idt => get_param_definition_type(pModflowInput%p_param_dfns, pModflowInput%component_type, &
        pModflowInput%subcomponent_type, words(icol + 1))

      !write(iout, '(4x,a)') 'creating : '//trim(idt%mf6varname)

      ! allocate a new vector in the memory manager using idt information
      ! and add it to the struct_array in position icol
      call struct_array%mem_create_vector(icol, idt%datatype, nrow, &
                                          idt%mf6varname, pModflowInput%memoryPath, &
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
  
  subroutine load_integer1d_type(parser, idt, pModflowInput, iout)
    use SimVariablesModule, only: idm_mempath_prefix
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    type(ModflowInputType), pointer, intent(in) :: pModflowInput
    integer(I4B), intent(in) :: iout
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B), pointer :: nsize1
    integer(I4B) :: nvals
    integer(I4B), dimension(:), contiguous, pointer :: mshape
    
    if (idt%shape == 'NODES') then
      call mem_setptr(mshape, 'MODEL_SHAPE', trim(idm_mempath_prefix)//trim(pModflowInput%component_name))
      nvals = product(mshape)
      call mem_allocate(int1d, nvals, idt%mf6varname, pModflowInput%memoryPath)
    else
      call mem_setptr(nsize1, idt%shape, pModflowInput%memoryPath)
      call mem_allocate(int1d, nsize1, idt%mf6varname, pModflowInput%memoryPath)
    end if
    
    call read_grid_array(parser, mshape, idt%tagname, intarray=int1d)

    call idm_log_type(int1d, idt%mf6varname, pModflowInput%memoryPath, 'TEST', iout)
    return
  end subroutine load_integer1d_type

  subroutine load_integer3d_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    integer(I4B), dimension(:, :, :), pointer, contiguous :: int3d
    integer(I4B), pointer :: nsize1
    integer(I4B), pointer :: nsize2
    integer(I4B), pointer :: nsize3
    integer(I4B) :: ndim = 3
    integer(I4B) :: nwords
    character(len=16), dimension(:), allocatable :: words
    character(len=LINELENGTH) :: keyword

    ! TODO: fix
    ! split shape into three sizes
    !call parseline(idt%shape, nwords, words)
    
    ! find sizes in memory manager
    !call mem_setptr(nsize1, words(1), memoryPath)
    !call mem_setptr(nsize2, words(2), memoryPath)
    !call mem_setptr(nsize3, words(3), memoryPath)
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
  
  subroutine load_double1d_type(parser, idt, pModflowInput, iout)
    use SimVariablesModule, only: idm_mempath_prefix
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    type(ModflowInputType), pointer, intent(in) :: pModflowInput
    integer(I4B), intent(in) :: iout
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B), pointer :: nsize1
    integer(I4B) :: ndim = 1
    integer(I4B) :: nvals
    integer(I4B), dimension(:), contiguous, pointer :: mshape
    
    if (idt%shape == 'NODES') then
      call mem_setptr(mshape, 'MODEL_SHAPE', trim(idm_mempath_prefix)//trim(pModflowInput%component_name))
      nvals = product(mshape)
      call mem_allocate(dbl1d, nvals, idt%mf6varname, pModflowInput%memoryPath)
    else
      call mem_setptr(nsize1, idt%shape, pModflowInput%memoryPath)
      call mem_allocate(dbl1d, nsize1, idt%mf6varname, pModflowInput%memoryPath)
      allocate(mshape(1))
      mshape(1) = nsize1
    end if
    
    call read_grid_array(parser, mshape, idt%tagname, dbl1d)
    
    !call ReadArray(parser%iuactive, dbl1d, idt%mf6varname, &
    !               ndim, nsize1, iout, 0)
    
    
    call idm_log_type(dbl1d, idt%mf6varname, pModflowInput%memoryPath, 'TEST', iout)
    return
  end subroutine load_double1d_type
  
  subroutine load_double2d_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    integer(I4B), pointer :: nsize1
    integer(I4B), pointer :: nsize2
    integer(I4B) :: ndim = 2
    integer(I4B) :: nwords
    character(len=16), dimension(:), allocatable :: words

    ! TODO: fix
    ! split shape into two sizes
    !call parseline(idt%shape, nwords, words)
    
    ! find sizes in memory manager
    !call mem_setptr(nsize1, words(1), memoryPath)
    !call mem_setptr(nsize2, words(2), memoryPath)
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
  
  subroutine load_double3d_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputParamDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    real(DP), dimension(:, :, :), pointer, contiguous :: dbl3d
    integer(I4B), pointer :: nsize1
    integer(I4B), pointer :: nsize2
    integer(I4B), pointer :: nsize3
    integer(I4B) :: ndim = 3
    integer(I4B) :: nwords
    character(len=16), dimension(:), allocatable :: words
    character(len=LINELENGTH) :: keyword

    ! TODO: fix
    ! split shape into three sizes
    !call parseline(idt%shape, nwords, words)
    
    ! find sizes in memory manager
    !call mem_setptr(nsize1, words(1), memoryPath)
    !call mem_setptr(nsize2, words(2), memoryPath)
    !call mem_setptr(nsize3, words(3), memoryPath)
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
  
  subroutine set_model_shape(ftype, component_name, memoryPath)
    character(len=*) :: ftype
    character(len=*), intent(in) :: component_name
    !character(len=LENMEMPATH), intent(in) :: memoryPath
    character(len=*), intent(in) :: memoryPath
    integer(I4B), dimension(:), pointer, contiguous :: model_shape
    integer(I4B), pointer :: ndim1
    integer(I4B), pointer :: ndim2
    integer(I4B), pointer :: ndim3
    
    select case (ftype)
    case ('DIS6')
      call mem_allocate(model_shape, 3, 'MODEL_SHAPE', component_name)
      call mem_setptr(ndim1, 'NLAY', memoryPath)
      call mem_setptr(ndim2, 'NROW', memoryPath)
      call mem_setptr(ndim3, 'NCOL', memoryPath)
      model_shape = [ndim1, ndim2, ndim3]
    case ('DISV6')
      call mem_allocate(model_shape, 2, 'MODEL_SHAPE', component_name)
      call mem_setptr(ndim1, 'NLAY', memoryPath)
      call mem_setptr(ndim2, 'NCPL', memoryPath)
      model_shape = [ndim1, ndim2]
    case ('DISU6')
      call mem_allocate(model_shape, 1, 'MODEL_SHAPE', component_name)
      call mem_setptr(ndim1, 'NODES', memoryPath)
      model_shape = [ndim1]
    end select
    
    return
  end subroutine set_model_shape

  subroutine update_aggregate_shapes(pModflowInput, iout)
    type(ModflowInputType), pointer, intent(in) :: pModflowInput
    integer(I4B), intent(in) :: iout
    type(BlockParserType) :: parser
    integer(I4B) :: inunit
    type(InputParamDefinitionType), pointer :: idt
    logical(LGP) :: update_shapes
    integer(I4B) :: iaggregate, iblock
    logical(LGP) :: isblockfound, endOfBlock
    integer(I4B) :: ierr
    integer(I4B) :: nlines
    integer(I4B), pointer :: blockshape

    ! check if file needs to be opened
    update_shapes = .false.
    do iaggregate = 1, size(pModflowInput%p_aggregate_dfns)
      if (pModflowInput%p_aggregate_dfns(iaggregate)%shape == '') then
        update_shapes = .true.
        exit
      end if
    end do

    ! file has to be read so check all blocks
    if (update_shapes) then
      ! set up file parsing
      inunit = getunit()
      call openfile(inunit, 0, pModflowInput%file_spec, pModflowInput%file_type)
      call parser%initialize(inunit, iout)

      ! check and update shape for each block against aggregate dfn
      do iblock = 1, size(pModflowInput%p_block_dfns)
        call parser%GetBlock(pModflowInput%p_block_dfns(iblock)%blockname, isblockfound, ierr)

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

          do iaggregate = 1, size(pModflowInput%p_aggregate_dfns)
            if (pModflowInput%p_aggregate_dfns(iaggregate)%blockname == &
                pModflowInput%p_block_dfns(iblock)%blockname .and. &
                pModflowInput%p_aggregate_dfns(iaggregate)%shape == '') then
              ! TODO: handle exceeding LENVARNAME
              pModflowInput%p_aggregate_dfns(iaggregate)%shape = 'n'//pModflowInput%p_aggregate_dfns(iaggregate)%mf6varname
              call mem_allocate(blockshape, pModflowInput%p_aggregate_dfns(iaggregate)%shape, pModflowInput%memoryPath)
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

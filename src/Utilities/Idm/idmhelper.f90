module IdmHelperModule
  
  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: iout, errmsg
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use IdmTypesModule, only: ModflowInputType, ModflowInput
  use MemoryManagerModule, only: get_from_memorylist, mem_setptr, &
                                 mem_deallocate, get_mem_rank
  
  implicit none
  private
  public :: idm_deallocate

  contains

  subroutine idm_deallocate(filetype, filename, component_type, &
                            subcomponent_type, component_name, &
                            subcomponent_name, iout)
    use MemoryTypeModule, only: MemoryType
    use SimVariablesModule, only: idm_mempath_prefix
    use MemoryHelperModule, only: create_mem_path
    use ConstantsModule, only: LENMEMPATH
    character(len=*), intent(in) :: filetype
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    integer(I4B), intent(in) :: iout
    type(ModflowInputType), pointer :: mf6_input
    type(InputParamDefinitionType), pointer :: param_dfn
    type(MemoryType), pointer :: mt
    logical(LGP) :: found = .false.
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: i, rank
    integer(I4B), dimension(:), pointer, contiguous :: aint1d => null() !< pointer to 1d integer array
    character(len=LENMEMPATH) :: idmMemoryPath

    mf6_input => ModflowInput(filetype, filename, component_type, &
                              subcomponent_type, component_name, &
                              subcomponent_name)

    idmMemoryPath = create_mem_path(component=mf6_input%component_name, context=idm_mempath_prefix)
    call get_from_memorylist('MODEL_SHAPE', idmMemoryPath, mt, found, checkfail)
    if (found) then
      call mem_setptr(aint1d, 'MODEL_SHAPE', idmMemoryPath)
      call mem_deallocate(aint1d)
    end if

    do i = 1, size(mf6_input%p_param_dfns)
      found = .false.
      param_dfn => mf6_input%p_param_dfns(i)
      call get_from_memorylist(param_dfn%mf6varname, mf6_input%memoryPath, mt, found, checkfail)
      if (found) then
        rank = -1
        call get_mem_rank(param_dfn%mf6varname, mf6_input%memoryPath, rank)
        select case (mt%memtype(1:index(mt%memtype, ' ')))
        case ('LOGICAL')
          call idm_deallocate_logical(param_dfn, mf6_input)
        case ('INTEGER')
          call idm_deallocate_int(param_dfn, mf6_input, rank)
        case ('DOUBLE')
          call idm_deallocate_dbl(param_dfn, mf6_input, rank)
        case ('STRING')
          call idm_deallocate_str(param_dfn, mf6_input, rank)
        end select
      end if
    end do
  end subroutine idm_deallocate

  subroutine idm_deallocate_logical(param_dfn, mf6_input)
    type(InputParamDefinitionType), pointer, intent(in) :: param_dfn
    type(ModflowInputType), pointer, intent(in) :: mf6_input
    logical(LGP), pointer :: logicalsclr => null() !< pointer to the logical

    call mem_setptr(logicalsclr, param_dfn%mf6varname, mf6_input%memoryPath)
    call mem_deallocate(logicalsclr)
  end subroutine idm_deallocate_logical

  subroutine idm_deallocate_int(param_dfn, mf6_input, rank)
    type(InputParamDefinitionType), pointer, intent(in) :: param_dfn
    type(ModflowInputType), pointer, intent(in) :: mf6_input
    integer(I4B), intent(in) :: rank
    integer(I4B), pointer :: intsclr => null() !< pointer to the integer
    integer(I4B), dimension(:), pointer, contiguous :: aint1d => null() !< pointer to 1d integer array
    integer(I4B), dimension(:, :), pointer, contiguous :: aint2d => null() !< pointer to 2d integer array
    integer(I4B), dimension(:, :, :), pointer, contiguous :: aint3d => null() !< pointer to 3d integer array

    if (rank == 0) then
      call mem_setptr(intsclr, param_dfn%mf6varname, mf6_input%memoryPath)
      call mem_deallocate(intsclr)
    elseif (rank == 1) then
      call mem_setptr(aint1d, param_dfn%mf6varname, mf6_input%memoryPath)
      call mem_deallocate(aint1d)
    elseif (rank == 2) then
      call mem_setptr(aint2d, param_dfn%mf6varname, mf6_input%memoryPath)
      call mem_deallocate(aint2d)
    elseif (rank == 3) then
      call mem_setptr(aint3d, param_dfn%mf6varname, mf6_input%memoryPath)
      call mem_deallocate(aint3d)
    end if
  end subroutine idm_deallocate_int

  subroutine idm_deallocate_dbl(param_dfn, mf6_input, rank)
    type(InputParamDefinitionType), pointer, intent(in) :: param_dfn
    type(ModflowInputType), pointer, intent(in) :: mf6_input
    integer(I4B), intent(in) :: rank
    real(DP), pointer :: dblsclr => null() !< pointer to the double
    real(DP), dimension(:), pointer, contiguous :: adbl1d => null() !< pointer to 1d double array
    real(DP), dimension(:, :), pointer, contiguous :: adbl2d => null() !< pointer to 2d double array
    real(DP), dimension(:, :, :), pointer, contiguous :: adbl3d => null() !< pointer to 3d double array

    if (rank == 0) then
      call mem_setptr(dblsclr, param_dfn%mf6varname, mf6_input%memoryPath)
      call mem_deallocate(dblsclr)
    elseif (rank == 1) then
      call mem_setptr(adbl1d, param_dfn%mf6varname, mf6_input%memoryPath)
      call mem_deallocate(adbl1d)
    elseif (rank == 2) then
      call mem_setptr(adbl2d, param_dfn%mf6varname, mf6_input%memoryPath)
      call mem_deallocate(adbl2d)
    elseif (rank == 3) then
      call mem_setptr(adbl3d, param_dfn%mf6varname, mf6_input%memoryPath)
      call mem_deallocate(adbl3d)
    end if
  end subroutine idm_deallocate_dbl

  subroutine idm_deallocate_str(param_dfn, mf6_input, rank)
    use ConstantsModule, only: MAXCHARLEN
    use CharacterStringModule, only: CharacterStringType
    type(InputParamDefinitionType), pointer, intent(in) :: param_dfn
    type(ModflowInputType), pointer, intent(in) :: mf6_input
    integer(I4B), intent(in) :: rank
    character(len=:), pointer :: strsclr => null() !< pointer to the character string
    character(len=MAXCHARLEN), pointer :: idm_str => null()
    !type(CharacterStringType), dimension(:), pointer, contiguous :: &
    !  acharstr1d => null() !< pointer to the 1d character string array

    if (rank == 0) then
      call mem_setptr(strsclr, param_dfn%mf6varname, mf6_input%memoryPath)
      idm_str => strsclr
      call mem_deallocate(idm_str)
    !elseif (rank == 1) then
    !  call setptr_charstr1d(acharstr1d, param_dfn%mf6varname, mf6_input%memoryPath)
    !  call mem_deallocate(acharstr1d)
    end if
  end subroutine idm_deallocate_str

end module IdmHelperModule

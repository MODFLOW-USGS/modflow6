module IdmFileModflowModule

  use KindModule, only: I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMEMPATH, LENVARNAME, &
                             LENCOMPONENTNAME, LENFTYPE, MAXCHARLEN
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use MemoryManagerModule,  only: mem_setptr, get_from_memorylist
  use MemoryTypeModule, only: MemoryType
  use IdmTypesModule, only: ModflowInputType, &
                            ModflowInput
  use LoadMfInputModule, only: idm_load, set_model_shape

  implicit none
  private
  public :: load_from_modflow_inputs

  type MTPointerType
    type(MemoryType), pointer :: pMT
  end type MTPointerType

  contains

  subroutine load_from_modflow_inputs(iunit_lst, namfile)
    integer(I4B), intent(in) :: iunit_lst
    character(len=*), intent(in) :: namfile
    type(ModflowInputType), target :: pModflowInput
    character(len=LENFTYPE) :: file_type = 'MF6'
    !character(len=MAXCHARLEN) :: file_spec = 'mfsim.nam'
    character(len=MAXCHARLEN) :: file_spec
    character(len=LENCOMPONENTNAME) :: component_type = 'SIM'
    character(len=LENCOMPONENTNAME) :: subcomponent_type = 'NAM'
    character(len=LENCOMPONENTNAME) :: component_name = 'SIM'
    character(len=LENCOMPONENTNAME) :: subcomponent_name = 'NAM'

    file_spec = trim(namfile)
    pModflowInput = ModflowInput(file_type, file_spec, component_type, &
                                 subcomponent_type, component_name, &
                                 subcomponent_name)

    call idm_load(pModflowInput, iunit_lst)
    call process_mfsim_nam(iunit_lst)
  end subroutine load_from_modflow_inputs

  subroutine process_mfsim_nam(iunit_lst)
    integer(I4B), intent(in) :: iunit_lst

    ! tdis
    call process_mfsim_nam_tdis(iunit_lst)

    ! models
    call process_mfsim_nam_models(iunit_lst)

    ! xchgs  Not yet supported
    call process_mfsim_nam_xchgs(iunit_lst)

    ! ims
    call process_mfsim_nam_ims(iunit_lst)
  end subroutine process_mfsim_nam

  subroutine process_mfsim_nam_tdis(iunit_lst)
    integer(I4B), intent(in) :: iunit_lst
    logical(LGP) :: found
    type(ModflowInputType), target :: pModflowInput
    type(MTPointerType) :: mt
    character(len=LENFTYPE) :: file_type
    character(len=MAXCHARLEN) :: file_spec
    character(len=LENCOMPONENTNAME) :: component_type = 'SIM'
    character(len=LENCOMPONENTNAME) :: subcomponent_type = 'TDIS'
    !character(len=LENMEMPATH) :: mem_path = 'SIM/NAM'
    character(len=LENMEMPATH) :: mem_path = '__INPUT__/SIM/NAM'
    character(len=LENVARNAME) :: var_name = 'TDIS6'

    found = .false.
    mt%pMT => null()

    call get_from_memorylist(var_name, mem_path, mt%pMT, found, check=.false.)

    if (found) then
      file_type = trim(var_name)
      file_spec = trim(mt%pMT%strsclr)

      pModflowInput = ModflowInput(file_type, file_spec, component_type, &
                                   subcomponent_type, component_type, &
                                   subcomponent_type)

      call idm_load(pModflowInput, iunit_lst)

    else
      write(errmsg, '(a)') 'IDM expected memory path not found; var='//trim(var_name)//', path='//trim(mem_path)
      call store_error(errmsg, terminate=.false.)
    end if
  end subroutine process_mfsim_nam_tdis

  subroutine process_mfsim_nam_models(iunit_lst)
    integer(I4B), intent(in) :: iunit_lst
    type(ModflowInputType), target :: pModflowInput
    type(MTPointerType), pointer, dimension(:) :: mt
    integer(I4B) :: imodel
    character(len=LENFTYPE) :: file_type
    character(len=MAXCHARLEN) :: file_spec
    character(len=LENCOMPONENTNAME) :: component_type
    character(len=LENCOMPONENTNAME) :: subcomponent_type = 'NAM'
    character(len=LENCOMPONENTNAME) :: component_name
    character(len=LENCOMPONENTNAME) :: subcomponent_name = 'NAM'
    !character(len=LENMEMPATH) :: mem_path = 'SIM/NAM'
    character(len=LENMEMPATH) :: mem_path = '__INPUT__/SIM/NAM'
    character(len=LENMEMPATH) :: model_mem_path
    character(len=LENVARNAME), dimension(3) :: var_names = &
    [ character(len=LENVARNAME) :: 'MTYPE', 'MFNAME', 'MNAME' ]

    allocate(mt(size(var_names)))
    call set_mempath_ptrs(mem_path, var_names, mt)

    !do imodel = 1, mt(1)%pMT%isize / LINELENGTH
    do imodel = 1, mt(1)%pMT%isize
      file_type = mt(1)%pMT%acharstr1d(imodel)
      file_spec = mt(2)%pMT%acharstr1d(imodel)
      component_name = mt(3)%pMT%acharstr1d(imodel)
      model_mem_path = '__INPUT__/'//trim(component_name)//'/'//trim(subcomponent_type)
      write (iunit_lst, '(1x,a)') 'IDM file_type => '//file_type
      write (iunit_lst, '(1x,a)') 'IDM file_spec => '//file_spec
      write (iunit_lst, '(1x,a)') 'IDM component_name => '//component_name

      select case (file_type)
      case('GWF6')
        component_type = 'GWF'
      case('GWT6')
        component_type = 'GWT'
      case default
        component_type = ''
      end select

      pModflowInput = ModflowInput(file_type, file_spec, component_type, &
                                   subcomponent_type, component_name, &
                                   subcomponent_name)

      call idm_load(pModflowInput, iunit_lst)
      call process_model_nam_pkgs(model_mem_path, component_type, &
                                  component_name, iunit_lst)
    end do

    deallocate(mt)
  end subroutine process_mfsim_nam_models

  subroutine process_model_nam_pkgs(mem_path, component_type, component_name, iunit_lst)
    character(len=*), intent(in) :: mem_path
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: component_name
    integer(I4B), intent(in) :: iunit_lst
    type(ModflowInputType), target :: pModflowInput
    type(MTPointerType), pointer, dimension(:) :: mt
    integer(I4B) :: ipkg
    logical(LGP) :: set_shape = .false.
    character(len=LENFTYPE) :: file_type
    character(len=MAXCHARLEN) :: file_spec
    character(len=LENCOMPONENTNAME) :: subcomponent_type
    character(len=LENCOMPONENTNAME) :: subcomponent_name
    character(len=LENVARNAME), dimension(3) :: var_names = &
    [ character(len=LENVARNAME) :: 'FTYPE', 'FNAME', 'PNAME' ]

    allocate(mt(size(var_names)))
    call set_mempath_ptrs(mem_path, var_names, mt)

    !do ipkg = 1, mt(1)%pMT%isize / LINELENGTH
    do ipkg = 1, mt(1)%pMT%isize
      file_type = mt(1)%pMT%acharstr1d(ipkg)
      file_spec = mt(2)%pMT%acharstr1d(ipkg)
      subcomponent_name = mt(3)%pMT%acharstr1d(ipkg)

      select case (file_type)
      case('DIS6')
        subcomponent_type = 'DIS'
        set_shape = .true.
      case('IC6')
        subcomponent_type = 'IC'
      case('NPF6')
        subcomponent_type = 'NPF'
      case('CHD6')
        subcomponent_type = 'CHD'
      case('OC6')
        subcomponent_type = 'OC'
      case default
        subcomponent_type = ''
      end select

      pModflowInput = ModflowInput(file_type, file_spec, component_type, &
                                   subcomponent_type, component_name, &
                                   subcomponent_name)

      call idm_load(pModflowInput, iunit_lst)

      if (set_shape) then
        call set_model_shape(pModflowInput%file_type, '__INPUT__/'//pModflowInput%component_name, pModflowInput%memoryPath)
        set_shape = .false.
      end if

    end do

    deallocate(mt)
  end subroutine process_model_nam_pkgs

  subroutine process_mfsim_nam_xchgs(iunit_lst)
    integer(I4B), intent(in) :: iunit_lst
    type(ModflowInputType), target :: pModflowInput
    type(MTPointerType), pointer, dimension(:) :: mt
    integer(I4B) :: ixchg
    character(len=LENFTYPE) :: file_type
    character(len=MAXCHARLEN) :: file_spec
    character(len=LENCOMPONENTNAME) :: component_type
    character(len=LENCOMPONENTNAME) :: subcomponent_type
    character(len=LENCOMPONENTNAME) :: component_name
    character(len=LENCOMPONENTNAME) :: subcomponent_name
    !character(len=LENMEMPATH) :: mem_path = 'SIM/NAM'
    character(len=LENMEMPATH) :: mem_path = '__INPUT__/SIM/NAM'
    character(len=LENVARNAME), dimension(4) :: var_names = &
    [ character(len=LENVARNAME) :: 'EXGTYPE', 'EXGFILE', 'EXGMNAMEA', 'EXGMNAMEB' ]

    allocate(mt(size(var_names)))
    call set_mempath_ptrs(mem_path, var_names, mt)

    ! Not yet implemented
    !do ixchg = 1, mt(1)%pMT%isize / LINELENGTH
    do ixchg = 1, mt(1)%pMT%isize
      file_type = mt(1)%pMT%acharstr1d(ixchg)
      file_spec = mt(2)%pMT%acharstr1d(ixchg)

      write(errmsg, '(a)') 'IDM xchgs found but functionality not yet implemented'
      call store_error(errmsg, terminate=.false.)

!      pModflowInput = ModflowInput(file_type, file_spec, component_type, &
!                                   subcomponent_type, component_name, &
!                                   subcomponent_name)

!      call idm_load(pModflowInput, iunit_lst)
    end do

    deallocate(mt)
  end subroutine process_mfsim_nam_xchgs

  subroutine process_mfsim_nam_ims(iunit_lst)
    integer(I4B), intent(in) :: iunit_lst
    type(ModflowInputType), target :: pModflowInput
    type(MTPointerType), pointer, dimension(:) :: mt
    integer(I4B) :: isln
    character(len=LENFTYPE) :: file_type
    character(len=MAXCHARLEN) :: file_spec
    character(len=LENCOMPONENTNAME) :: component_type = 'SLN'
    character(len=LENCOMPONENTNAME) :: subcomponent_type = 'IMS'
    character(len=LENCOMPONENTNAME) :: component_name = 'SLN'
    character(len=LENCOMPONENTNAME) :: subcomponent_name
    !character(len=LENMEMPATH) :: mem_path = 'SIM/NAM'
    character(len=LENMEMPATH) :: mem_path = '__INPUT__/SIM/NAM'
    character(len=LENVARNAME), dimension(3) :: var_names = &
    [ character(len=LENVARNAME) :: 'SLNTYPE', 'SLNFNAME', 'SLNMNAMES' ]

    allocate(mt(size(var_names)))
    call set_mempath_ptrs(mem_path, var_names, mt)

    !do isln = 1, mt(1)%pMT%isize / LINELENGTH
    do isln = 1, mt(1)%pMT%isize
      file_type = mt(1)%pMT%acharstr1d(isln)
      file_spec = mt(2)%pMT%acharstr1d(isln)
      subcomponent_name = mt(3)%pMT%acharstr1d(isln)

      pModflowInput = ModflowInput(file_type, file_spec, component_type, &
                                   subcomponent_type, component_name, &
                                   subcomponent_name)

      call idm_load(pModflowInput, iunit_lst)
    end do

    deallocate(mt)
  end subroutine process_mfsim_nam_ims

  subroutine set_mempath_ptrs(mem_path, var_names, mt)
    character(len=LENMEMPATH), intent(in) :: mem_path
    character(len=LENVARNAME), dimension(:), intent(in) :: var_names
    type(MTPointerType), pointer, dimension(:), intent(inout) :: mt
    integer(I4B) :: ivar
    logical(LGP) :: found

    do ivar = 1, size(var_names)
      found = .false.
      mt(ivar)%pMT => null()
      call get_from_memorylist(var_names(ivar), mem_path, mt(ivar)%pMT, found, check=.false.)
      if (.not. found) then
        write(errmsg, '(a)') 'IDM expected memory path not found; var='//trim(var_names(ivar))//', path='//trim(mem_path)
        call store_error(errmsg, terminate=.false.)
      end if
    end do
  end subroutine set_mempath_ptrs

end module IdmFileModflowModule

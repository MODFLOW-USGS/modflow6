!> @brief This module contains the IdmSimulationModule
!!
!! This module contains the high-level routines for loading
!! sim namefile parameters into the input context
!!
!<
module IdmSimulationModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMEMPATH
  use SimModule, only: store_error
  use SimVariablesModule, only: iout
  use InputOutputModule, only: openfile, getunit
  use InputDefinitionModule, only: InputParamDefinitionType
  use ModflowInputModule, only: ModflowInputType, getModflowInput
  use IdmMf6FileModule, only: input_load

  implicit none
  private
  public :: simnam_load
  public :: load_models

contains

  !> @brief load simulation summary info to input context
  !<
  subroutine simnam_load_dim()
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    use SimVariablesModule, only: idm_context
    use CharacterStringModule, only: CharacterStringType
    character(len=LENMEMPATH) :: sim_mempath, simnam_mempath
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: mtypes !< model types
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: etypes !< model types
    integer(I4B), pointer :: nummodels => null()
    integer(I4B), pointer :: numexchanges => null()
    !
    ! -- set memory paths
    sim_mempath = create_mem_path(component='SIM', context=idm_context)
    simnam_mempath = create_mem_path('SIM', 'NAM', idm_context)
    !
    ! -- set pointers to loaded simnam arrays
    call mem_setptr(mtypes, 'MTYPE', simnam_mempath)
    call mem_setptr(etypes, 'EXGTYPE', simnam_mempath)
    !
    ! -- allocate variables
    call mem_allocate(nummodels, 'NUMMODELS', sim_mempath)
    call mem_allocate(numexchanges, 'NUMEXCHANGES', sim_mempath)
    !
    ! -- set values
    nummodels = size(mtypes)
    numexchanges = size(etypes)
    !
    ! -- return
    return
  end subroutine simnam_load_dim

  !> @brief MODFLOW 6 mfsim.nam parameter set default value
  !<
  subroutine set_default_value(intvar, mf6varname)
    use SimVariablesModule, only: isimcontinue, isimcheck
    integer(I4B), pointer, intent(in) :: intvar
    character(len=*), intent(in) :: mf6varname
    character(len=LINELENGTH) :: errmsg
    logical(LGP) :: terminate = .true.
    !
    ! -- load defaults for keyword/integer types
    select case (mf6varname)
      !
    case ('CONTINUE')
      intvar = isimcontinue
      !
    case ('NOCHECK')
      intvar = isimcheck
      !
    case ('MAXERRORS')
      intvar = 1000 !< MessageType max_message
      !
    case ('MXITER')
      intvar = 1
      !
    case ('PRINT_INPUT')
      intvar = 0
      !
    case default
      write (errmsg, '(a,a)') &
        'IdmSimulation set_default_value unhandled variable: ', &
        trim(mf6varname)
      call store_error(errmsg, terminate)
    end select
    !
    ! -- return
    return
  end subroutine set_default_value

  !> @brief MODFLOW 6 mfsim.nam input context parameter allocation
  !<
  subroutine simnam_allocate()
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: get_isize, mem_allocate
    use SimVariablesModule, only: idm_context
    use CharacterStringModule, only: CharacterStringType
    character(len=LENMEMPATH) :: input_mempath
    type(ModflowInputType) :: mf6_input
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam, isize
    logical(LGP) :: terminate = .true.
    integer(I4B), pointer :: intvar
    character(len=LINELENGTH), pointer :: cstr
    type(CharacterStringType), dimension(:), &
      pointer, contiguous :: acharstr1d
    character(len=LINELENGTH) :: errmsg
    !
    ! -- set memory path
    input_mempath = create_mem_path('SIM', 'NAM', idm_context)
    !
    ! -- create description of input
    mf6_input = getModflowInput('NAM6', 'SIM', 'NAM', 'SIM', 'NAM')
    !
    ! -- allocate sim namfile parameters if not in input context
    do iparam = 1, size(mf6_input%param_dfns)
      !
      ! -- assign param definition pointer
      idt => mf6_input%param_dfns(iparam)
      !
      ! -- check if variable is already allocated
      call get_isize(idt%mf6varname, input_mempath, isize)
      !
      if (isize < 0) then
        !
        ! -- reset pointers
        nullify (intvar)
        nullify (acharstr1d)
        nullify (cstr)
        !
        select case (idt%datatype)
        case ('KEYWORD', 'INTEGER')
          !
          ! -- allocate and set default
          call mem_allocate(intvar, idt%mf6varname, input_mempath)
          call set_default_value(intvar, idt%mf6varname)
        case ('STRING')
          !
          ! -- did this param originate from sim namfile RECARRAY type
          if (idt%in_record) then
            !
            ! -- allocate 0 size CharacterStringType array
            call mem_allocate(acharstr1d, LINELENGTH, 0, idt%mf6varname, &
                              input_mempath)
          else
            !
            ! -- allocate empty string
            call mem_allocate(cstr, LINELENGTH, idt%mf6varname, input_mempath)
            cstr = ''
          end if
        case default
          write (errmsg, '(a,a)') &
            'IdmSimulation unhandled datatype: ', &
            trim(idt%datatype)
          call store_error(errmsg, terminate)
        end select
      end if
    end do
    !
    ! -- return
    return
  end subroutine simnam_allocate

  !> @brief source indenpendent model load entry point
  !<
  subroutine load_models(model_loadmask, iout)
    ! -- modules
    use IdmMf6FileModule, only: load_models_mf6
    ! -- dummy
    integer(I4B), dimension(:), intent(in) :: model_loadmask
    integer(I4B), intent(in) :: iout
    ! -- locals
    !
    ! -- mf6 blockfile model load
    call load_models_mf6(model_loadmask, iout)
    !
    ! -- return
    return
  end subroutine load_models

  function input_param_log() result(paramlog)
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use SimVariablesModule, only: idm_context
    character(len=LENMEMPATH) :: simnam_mempath
    integer(I4B) :: paramlog
    integer(I4B), pointer :: p
    !
    ! -- read and set input value of PRINT_INPUT
    simnam_mempath = create_mem_path('SIM', 'NAM', idm_context)
    call mem_setptr(p, 'PRINT_INPUT', simnam_mempath)
    !
    paramlog = p
    !
    ! -- return
    return
  end function input_param_log

  !> @brief MODFLOW 6 mfsim.nam input load routine
  !<
  subroutine simnam_load(paramlog)
    use SimVariablesModule, only: simfile
    use GenericUtilitiesModule, only: sim_message
    integer(I4B), intent(inout) :: paramlog
    integer(I4B) :: inunit
    logical :: lexist
    character(len=LINELENGTH) :: line
    !
    ! -- load mfsim.nam if it exists
    inquire (file=trim(adjustl(simfile)), exist=lexist)
    !
    if (lexist) then
      !
      ! -- write name of namfile to stdout
      write (line, '(2(1x,a))') 'Using Simulation name file:', &
        trim(adjustl(simfile))
      call sim_message(line, skipafter=1)
      !
      ! -- open namfile and load to input context
      inunit = getunit()
      call openfile(inunit, iout, trim(adjustl(simfile)), 'NAM')
      call input_load('NAM6', 'SIM', 'NAM', 'SIM', 'NAM', inunit, iout)
      close (inunit)
    end if
    !
    ! -- allocate any unallocated simnam params
    call simnam_allocate()
    !
    ! -- read and set input parameter logging keyword
    paramlog = input_param_log()
    !
    ! -- memload summary info
    call simnam_load_dim()
    !
    ! --return
    return
  end subroutine simnam_load

end module IdmSimulationModule

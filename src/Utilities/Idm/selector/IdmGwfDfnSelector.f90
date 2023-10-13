! ** Do Not Modify! MODFLOW 6 system generated file. **
module IdmGwfDfnSelectorModule

  use ConstantsModule, only: LENVARNAME
  use SimModule, only: store_error
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use GwfChdInputModule
  use GwfDisInputModule
  use GwfDisuInputModule
  use GwfDisvInputModule
  use GwfDrnInputModule
  use GwfEvtInputModule
  use GwfEvtaInputModule
  use GwfGhbInputModule
  use GwfNpfInputModule
  use GwfRchInputModule
  use GwfRchaInputModule
  use GwfRivInputModule
  use GwfWelInputModule
  use GwfNamInputModule

  implicit none
  private
  public :: GwfParamFoundType
  public :: gwf_param_definitions
  public :: gwf_aggregate_definitions
  public :: gwf_block_definitions
  public :: gwf_idm_multi_package
  public :: gwf_idm_sfac_param
  public :: gwf_idm_integrated

  type GwfParamFoundType
    logical :: auxiliary = .false.
    logical :: auxmultname = .false.
    logical :: boundnames = .false.
    logical :: iprpak = .false.
    logical :: iprflow = .false.
    logical :: ipakcb = .false.
    logical :: ts_filerecord = .false.
    logical :: ts6 = .false.
    logical :: filein = .false.
    logical :: ts6_filename = .false.
    logical :: obs_filerecord = .false.
    logical :: obs6 = .false.
    logical :: obs6_filename = .false.
    logical :: inewton = .false.
    logical :: maxbound = .false.
    logical :: cellid = .false.
    logical :: head = .false.
    logical :: auxvar = .false.
    logical :: boundname = .false.
    logical :: length_units = .false.
    logical :: nogrb = .false.
    logical :: xorigin = .false.
    logical :: yorigin = .false.
    logical :: angrot = .false.
    logical :: nlay = .false.
    logical :: nrow = .false.
    logical :: ncol = .false.
    logical :: delr = .false.
    logical :: delc = .false.
    logical :: top = .false.
    logical :: botm = .false.
    logical :: idomain = .false.
    logical :: voffsettol = .false.
    logical :: nodes = .false.
    logical :: nja = .false.
    logical :: nvert = .false.
    logical :: bot = .false.
    logical :: area = .false.
    logical :: iac = .false.
    logical :: ja = .false.
    logical :: ihc = .false.
    logical :: cl12 = .false.
    logical :: hwva = .false.
    logical :: angldegx = .false.
    logical :: iv = .false.
    logical :: xv = .false.
    logical :: yv = .false.
    logical :: icell2d = .false.
    logical :: xc = .false.
    logical :: yc = .false.
    logical :: ncvert = .false.
    logical :: icvert = .false.
    logical :: ncpl = .false.
    logical :: auxdepthname = .false.
    logical :: mover = .false.
    logical :: icubicsfac = .false.
    logical :: elev = .false.
    logical :: cond = .false.
    logical :: fixed_cell = .false.
    logical :: surfratespec = .false.
    logical :: nseg = .false.
    logical :: surface = .false.
    logical :: rate = .false.
    logical :: depth = .false.
    logical :: pxdp = .false.
    logical :: petm = .false.
    logical :: petm0 = .false.
    logical :: readasarrays = .false.
    logical :: tas_filerecord = .false.
    logical :: tas6 = .false.
    logical :: tas6_filename = .false.
    logical :: ievt = .false.
    logical :: bhead = .false.
    logical :: cellavg = .false.
    logical :: ithickstrt = .false.
    logical :: cvoptions = .false.
    logical :: ivarcv = .false.
    logical :: idewatcv = .false.
    logical :: iperched = .false.
    logical :: rewet_record = .false.
    logical :: irewet = .false.
    logical :: wetfct = .false.
    logical :: iwetit = .false.
    logical :: ihdwet = .false.
    logical :: xt3doptions = .false.
    logical :: ixt3d = .false.
    logical :: ixt3drhs = .false.
    logical :: isavspdis = .false.
    logical :: isavsat = .false.
    logical :: ik22overk = .false.
    logical :: ik33overk = .false.
    logical :: tvk_filerecord = .false.
    logical :: tvk6 = .false.
    logical :: tvk6_filename = .false.
    logical :: iusgnrhc = .false.
    logical :: inwtupw = .false.
    logical :: satmin = .false.
    logical :: satomega = .false.
    logical :: icelltype = .false.
    logical :: k = .false.
    logical :: k22 = .false.
    logical :: k33 = .false.
    logical :: angle1 = .false.
    logical :: angle2 = .false.
    logical :: angle3 = .false.
    logical :: wetdry = .false.
    logical :: recharge = .false.
    logical :: irch = .false.
    logical :: stage = .false.
    logical :: rbot = .false.
    logical :: flowred = .false.
    logical :: afrcsv_rec = .false.
    logical :: afrcsv = .false.
    logical :: fileout = .false.
    logical :: afrcsvfile = .false.
    logical :: q = .false.
    logical :: list = .false.
    logical :: print_input = .false.
    logical :: print_flows = .false.
    logical :: save_flows = .false.
    logical :: newtonoptions = .false.
    logical :: newton = .false.
    logical :: under_relaxation = .false.
    logical :: ftype = .false.
    logical :: fname = .false.
    logical :: pname = .false.
  end type GwfParamFoundType

contains

  subroutine set_param_pointer(input_dfn, input_dfn_target)
    type(InputParamDefinitionType), dimension(:), pointer :: input_dfn
    type(InputParamDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine set_param_pointer

  subroutine set_block_pointer(input_dfn, input_dfn_target)
    type(InputBlockDefinitionType), dimension(:), pointer :: input_dfn
    type(InputBlockDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine set_block_pointer

  function gwf_param_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('CHD')
      call set_param_pointer(input_definition, gwf_chd_param_definitions)
    case ('DIS')
      call set_param_pointer(input_definition, gwf_dis_param_definitions)
    case ('DISU')
      call set_param_pointer(input_definition, gwf_disu_param_definitions)
    case ('DISV')
      call set_param_pointer(input_definition, gwf_disv_param_definitions)
    case ('DRN')
      call set_param_pointer(input_definition, gwf_drn_param_definitions)
    case ('EVT')
      call set_param_pointer(input_definition, gwf_evt_param_definitions)
    case ('EVTA')
      call set_param_pointer(input_definition, gwf_evta_param_definitions)
    case ('GHB')
      call set_param_pointer(input_definition, gwf_ghb_param_definitions)
    case ('NPF')
      call set_param_pointer(input_definition, gwf_npf_param_definitions)
    case ('RCH')
      call set_param_pointer(input_definition, gwf_rch_param_definitions)
    case ('RCHA')
      call set_param_pointer(input_definition, gwf_rcha_param_definitions)
    case ('RIV')
      call set_param_pointer(input_definition, gwf_riv_param_definitions)
    case ('WEL')
      call set_param_pointer(input_definition, gwf_wel_param_definitions)
    case ('NAM')
      call set_param_pointer(input_definition, gwf_nam_param_definitions)
    case default
    end select
    return
  end function gwf_param_definitions

  function gwf_aggregate_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('CHD')
      call set_param_pointer(input_definition, gwf_chd_aggregate_definitions)
    case ('DIS')
      call set_param_pointer(input_definition, gwf_dis_aggregate_definitions)
    case ('DISU')
      call set_param_pointer(input_definition, gwf_disu_aggregate_definitions)
    case ('DISV')
      call set_param_pointer(input_definition, gwf_disv_aggregate_definitions)
    case ('DRN')
      call set_param_pointer(input_definition, gwf_drn_aggregate_definitions)
    case ('EVT')
      call set_param_pointer(input_definition, gwf_evt_aggregate_definitions)
    case ('EVTA')
      call set_param_pointer(input_definition, gwf_evta_aggregate_definitions)
    case ('GHB')
      call set_param_pointer(input_definition, gwf_ghb_aggregate_definitions)
    case ('NPF')
      call set_param_pointer(input_definition, gwf_npf_aggregate_definitions)
    case ('RCH')
      call set_param_pointer(input_definition, gwf_rch_aggregate_definitions)
    case ('RCHA')
      call set_param_pointer(input_definition, gwf_rcha_aggregate_definitions)
    case ('RIV')
      call set_param_pointer(input_definition, gwf_riv_aggregate_definitions)
    case ('WEL')
      call set_param_pointer(input_definition, gwf_wel_aggregate_definitions)
    case ('NAM')
      call set_param_pointer(input_definition, gwf_nam_aggregate_definitions)
    case default
    end select
    return
  end function gwf_aggregate_definitions

  function gwf_block_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('CHD')
      call set_block_pointer(input_definition, gwf_chd_block_definitions)
    case ('DIS')
      call set_block_pointer(input_definition, gwf_dis_block_definitions)
    case ('DISU')
      call set_block_pointer(input_definition, gwf_disu_block_definitions)
    case ('DISV')
      call set_block_pointer(input_definition, gwf_disv_block_definitions)
    case ('DRN')
      call set_block_pointer(input_definition, gwf_drn_block_definitions)
    case ('EVT')
      call set_block_pointer(input_definition, gwf_evt_block_definitions)
    case ('EVTA')
      call set_block_pointer(input_definition, gwf_evta_block_definitions)
    case ('GHB')
      call set_block_pointer(input_definition, gwf_ghb_block_definitions)
    case ('NPF')
      call set_block_pointer(input_definition, gwf_npf_block_definitions)
    case ('RCH')
      call set_block_pointer(input_definition, gwf_rch_block_definitions)
    case ('RCHA')
      call set_block_pointer(input_definition, gwf_rcha_block_definitions)
    case ('RIV')
      call set_block_pointer(input_definition, gwf_riv_block_definitions)
    case ('WEL')
      call set_block_pointer(input_definition, gwf_wel_block_definitions)
    case ('NAM')
      call set_block_pointer(input_definition, gwf_nam_block_definitions)
    case default
    end select
    return
  end function gwf_block_definitions

  function gwf_idm_multi_package(subcomponent) result(multi_package)
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (subcomponent)
    case ('CHD')
      multi_package = gwf_chd_multi_package
    case ('DIS')
      multi_package = gwf_dis_multi_package
    case ('DISU')
      multi_package = gwf_disu_multi_package
    case ('DISV')
      multi_package = gwf_disv_multi_package
    case ('DRN')
      multi_package = gwf_drn_multi_package
    case ('EVT')
      multi_package = gwf_evt_multi_package
    case ('EVTA')
      multi_package = gwf_evta_multi_package
    case ('GHB')
      multi_package = gwf_ghb_multi_package
    case ('NPF')
      multi_package = gwf_npf_multi_package
    case ('RCH')
      multi_package = gwf_rch_multi_package
    case ('RCHA')
      multi_package = gwf_rcha_multi_package
    case ('RIV')
      multi_package = gwf_riv_multi_package
    case ('WEL')
      multi_package = gwf_wel_multi_package
    case ('NAM')
      multi_package = gwf_nam_multi_package
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="GWF"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function gwf_idm_multi_package

  function gwf_idm_sfac_param(subcomponent) result(sfac_param)
    character(len=*), intent(in) :: subcomponent
    character(len=LENVARNAME) :: sfac_param
    select case (subcomponent)
    case ('CHD')
      sfac_param = gwf_chd_aux_sfac_param
    case ('DIS')
      sfac_param = gwf_dis_aux_sfac_param
    case ('DISU')
      sfac_param = gwf_disu_aux_sfac_param
    case ('DISV')
      sfac_param = gwf_disv_aux_sfac_param
    case ('DRN')
      sfac_param = gwf_drn_aux_sfac_param
    case ('EVT')
      sfac_param = gwf_evt_aux_sfac_param
    case ('EVTA')
      sfac_param = gwf_evta_aux_sfac_param
    case ('GHB')
      sfac_param = gwf_ghb_aux_sfac_param
    case ('NPF')
      sfac_param = gwf_npf_aux_sfac_param
    case ('RCH')
      sfac_param = gwf_rch_aux_sfac_param
    case ('RCHA')
      sfac_param = gwf_rcha_aux_sfac_param
    case ('RIV')
      sfac_param = gwf_riv_aux_sfac_param
    case ('WEL')
      sfac_param = gwf_wel_aux_sfac_param
    case ('NAM')
      sfac_param = gwf_nam_aux_sfac_param
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="GWF"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function gwf_idm_sfac_param

  function gwf_idm_integrated(subcomponent) result(integrated)
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (subcomponent)
    case ('CHD')
      integrated = .true.
    case ('DIS')
      integrated = .true.
    case ('DISU')
      integrated = .true.
    case ('DISV')
      integrated = .true.
    case ('DRN')
      integrated = .true.
    case ('EVT')
      integrated = .true.
    case ('EVTA')
      integrated = .true.
    case ('GHB')
      integrated = .true.
    case ('NPF')
      integrated = .true.
    case ('RCH')
      integrated = .true.
    case ('RCHA')
      integrated = .true.
    case ('RIV')
      integrated = .true.
    case ('WEL')
      integrated = .true.
    case ('NAM')
      integrated = .true.
    case default
    end select
    return
  end function gwf_idm_integrated

end module IdmGwfDfnSelectorModule

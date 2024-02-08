module IdmDfnSelectorModule

  use ConstantsModule, only: LENVARNAME
  use SimModule, only: store_error
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType

  use SimNamInputModule

  use ExggwfgwfInputModule
  use ExggwfgwtInputModule
  use ExggwtgwtInputModule

  use gwfnamInputModule
  use gwfchdInputModule
  use gwfdisInputModule
  use gwfdisuInputModule
  use gwfdisvInputModule
  use gwfdrnInputModule
  use gwfevtInputModule
  use gwfevtaInputModule
  use gwfghbInputModule
  use gwficInputModule
  use gwfnpfInputModule
  use gwfrchInputModule
  use gwfrchaInputModule
  use gwfrivInputModule
  use gwfwelInputModule
  use gwtnamInputModule
  use gwtdisInputModule
  use gwtdisuInputModule
  use gwtdisvInputModule
  use gwtdspInputModule
  use gwtcncInputModule
  use gwticInputModule

  implicit none
  private

  public :: idm_param_definitions
  public :: idm_aggregate_definitions
  public :: idm_block_definitions
  public :: idm_multi_package
  public :: idm_integrated
  public :: idm_component

  public :: exg_param_definitions
  public :: exg_aggregate_definitions
  public :: exg_block_definitions
  public :: exg_idm_multi_package
  public :: exg_idm_integrated

  public :: sim_param_definitions
  public :: sim_aggregate_definitions
  public :: sim_block_definitions
  public :: sim_idm_multi_package
  public :: sim_idm_integrated

  public :: gwf_param_definitions
  public :: gwf_aggregate_definitions
  public :: gwf_block_definitions
  public :: gwf_idm_multi_package
  public :: gwf_idm_integrated
  public :: gwt_param_definitions
  public :: gwt_aggregate_definitions
  public :: gwt_block_definitions
  public :: gwt_idm_multi_package
  public :: gwt_idm_integrated

contains

  function idm_param_definitions(component, subcomponent) result(def)
    character(len=*), intent(in) :: component
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (component)
    case ('SIM')
      def => sim_param_definitions(subcomponent)
    case ('GWF')
      def => gwf_param_definitions(subcomponent)
    case ('GWT')
      def => gwt_param_definitions(subcomponent)
    case ('EXG')
      def => exg_param_definitions(subcomponent)
    case default
    end select
    return
  end function idm_param_definitions

  function idm_aggregate_definitions(component, subcomponent) result(def)
    character(len=*), intent(in) :: component
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (component)
    case ('SIM')
      def => sim_aggregate_definitions(subcomponent)
    case ('GWF')
      def => gwf_aggregate_definitions(subcomponent)
    case ('GWT')
      def => gwt_aggregate_definitions(subcomponent)
    case ('EXG')
      def => exg_aggregate_definitions(subcomponent)
    case default
    end select
    return
  end function idm_aggregate_definitions

  function idm_block_definitions(component, subcomponent) result(def)
    character(len=*), intent(in) :: component
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (component)
    case ('SIM')
      def => sim_block_definitions(subcomponent)
    case ('GWF')
      def => gwf_block_definitions(subcomponent)
    case ('GWT')
      def => gwt_block_definitions(subcomponent)
    case ('EXG')
      def => exg_block_definitions(subcomponent)
    case default
    end select
    return
  end function idm_block_definitions

  function idm_multi_package(component, subcomponent) result(multi_package)
    character(len=*), intent(in) :: component
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (component)
    case ('SIM')
      multi_package = sim_idm_multi_package(subcomponent)
    case ('GWF')
      multi_package = gwf_idm_multi_package(subcomponent)
    case ('GWT')
      multi_package = gwt_idm_multi_package(subcomponent)
    case ('EXG')
      multi_package = exg_idm_multi_package(subcomponent)
    case default
      call store_error('Idm selector component not found; '//&
                       &'component="'//trim(component)//&
                       &'", subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function idm_multi_package

  function idm_integrated(component, subcomponent) result(integrated)
    character(len=*), intent(in) :: component
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (component)
    case ('SIM')
      integrated = sim_idm_integrated(subcomponent)
    case ('GWF')
      integrated = gwf_idm_integrated(subcomponent)
    case ('GWT')
      integrated = gwt_idm_integrated(subcomponent)
    case ('EXG')
      integrated = exg_idm_integrated(subcomponent)
    case default
    end select
    return
  end function idm_integrated

  function idm_component(component) result(integrated)
    character(len=*), intent(in) :: component
    logical :: integrated
    integrated = .false.
    select case (component)
    case ('SIM')
      integrated = .true.
    case ('GWF')
      integrated = .true.
    case ('GWT')
      integrated = .true.
    case ('EXG')
      integrated = .true.
    case default
    end select
    return
  end function idm_component

  subroutine exg_set_param_pointer(input_dfn, input_dfn_target)
    type(InputParamDefinitionType), dimension(:), pointer :: input_dfn
    type(InputParamDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine exg_set_param_pointer

  subroutine exg_set_block_pointer(input_dfn, input_dfn_target)
    type(InputBlockDefinitionType), dimension(:), pointer :: input_dfn
    type(InputBlockDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine exg_set_block_pointer

  function exg_param_definitions(subcomponent) result(def)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (subcomponent)
    case ('GWFGWF')
      call exg_set_param_pointer(def, exg_gwfgwf_param_definitions)
    case ('GWFGWT')
      call exg_set_param_pointer(def, exg_gwfgwt_param_definitions)
    case ('GWTGWT')
      call exg_set_param_pointer(def, exg_gwtgwt_param_definitions)
    case default
    end select
    return
  end function exg_param_definitions

  function exg_aggregate_definitions(subcomponent) result(def)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (subcomponent)
    case ('GWFGWF')
      call exg_set_param_pointer(def, exg_gwfgwf_aggregate_definitions)
    case ('GWFGWT')
      call exg_set_param_pointer(def, exg_gwfgwt_aggregate_definitions)
    case ('GWTGWT')
      call exg_set_param_pointer(def, exg_gwtgwt_aggregate_definitions)
    case default
    end select
    return
  end function exg_aggregate_definitions

  function exg_block_definitions(subcomponent) result(def)
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (subcomponent)
    case ('GWFGWF')
      call exg_set_block_pointer(def, exg_gwfgwf_block_definitions)
    case ('GWFGWT')
      call exg_set_block_pointer(def, exg_gwfgwt_block_definitions)
    case ('GWTGWT')
      call exg_set_block_pointer(def, exg_gwtgwt_block_definitions)
    case default
    end select
    return
  end function exg_block_definitions

  function exg_idm_multi_package(subcomponent) result(multi_package)
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (subcomponent)
    case ('GWFGWF')
      multi_package = exg_gwfgwf_multi_package
    case ('GWFGWT')
      multi_package = exg_gwfgwt_multi_package
    case ('GWTGWT')
      multi_package = exg_gwtgwt_multi_package
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="EXG"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function exg_idm_multi_package

  function exg_idm_integrated(subcomponent) result(integrated)
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (subcomponent)
    case ('GWFGWF')
      integrated = .true.
    case ('GWFGWT')
      integrated = .true.
    case ('GWTGWT')
      integrated = .true.
    case default
    end select
    return
  end function exg_idm_integrated

  subroutine sim_set_param_pointer(input_dfn, input_dfn_target)
    type(InputParamDefinitionType), dimension(:), pointer :: input_dfn
    type(InputParamDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine sim_set_param_pointer

  subroutine sim_set_block_pointer(input_dfn, input_dfn_target)
    type(InputBlockDefinitionType), dimension(:), pointer :: input_dfn
    type(InputBlockDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine sim_set_block_pointer

  function sim_param_definitions(subcomponent) result(def)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (subcomponent)
    case ('NAM')
      call sim_set_param_pointer(def, sim_nam_param_definitions)
    case default
    end select
    return
  end function sim_param_definitions

  function sim_aggregate_definitions(subcomponent) result(def)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (subcomponent)
    case ('NAM')
      call sim_set_param_pointer(def, sim_nam_aggregate_definitions)
    case default
    end select
    return
  end function sim_aggregate_definitions

  function sim_block_definitions(subcomponent) result(def)
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (subcomponent)
    case ('NAM')
      call sim_set_block_pointer(def, sim_nam_block_definitions)
    case default
    end select
    return
  end function sim_block_definitions

  function sim_idm_multi_package(subcomponent) result(multi_package)
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (subcomponent)
    case ('NAM')
      multi_package = sim_nam_multi_package
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="SIM"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function sim_idm_multi_package

  function sim_idm_integrated(subcomponent) result(integrated)
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (subcomponent)
    case ('NAM')
      integrated = .true.
    case default
    end select
    return
  end function sim_idm_integrated

  subroutine gwf_set_param_pointer(input_dfn, input_dfn_target)
    type(InputParamDefinitionType), dimension(:), pointer :: input_dfn
    type(InputParamDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine gwf_set_param_pointer
  subroutine gwt_set_param_pointer(input_dfn, input_dfn_target)
    type(InputParamDefinitionType), dimension(:), pointer :: input_dfn
    type(InputParamDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine gwt_set_param_pointer

  subroutine gwf_set_block_pointer(input_dfn, input_dfn_target)
    type(InputBlockDefinitionType), dimension(:), pointer :: input_dfn
    type(InputBlockDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine gwf_set_block_pointer
  subroutine gwt_set_block_pointer(input_dfn, input_dfn_target)
    type(InputBlockDefinitionType), dimension(:), pointer :: input_dfn
    type(InputBlockDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine gwt_set_block_pointer

  function gwf_param_definitions(subcomponent) result(def)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (subcomponent)
    case ('NAM')
      call gwf_set_param_pointer(def, gwf_nam_param_definitions)
    case ('CHD')
      call gwf_set_param_pointer(def, gwf_chd_param_definitions)
    case ('DIS')
      call gwf_set_param_pointer(def, gwf_dis_param_definitions)
    case ('DISU')
      call gwf_set_param_pointer(def, gwf_disu_param_definitions)
    case ('DISV')
      call gwf_set_param_pointer(def, gwf_disv_param_definitions)
    case ('DRN')
      call gwf_set_param_pointer(def, gwf_drn_param_definitions)
    case ('EVT')
      call gwf_set_param_pointer(def, gwf_evt_param_definitions)
    case ('EVTA')
      call gwf_set_param_pointer(def, gwf_evta_param_definitions)
    case ('GHB')
      call gwf_set_param_pointer(def, gwf_ghb_param_definitions)
    case ('IC')
      call gwf_set_param_pointer(def, gwf_ic_param_definitions)
    case ('NPF')
      call gwf_set_param_pointer(def, gwf_npf_param_definitions)
    case ('RCH')
      call gwf_set_param_pointer(def, gwf_rch_param_definitions)
    case ('RCHA')
      call gwf_set_param_pointer(def, gwf_rcha_param_definitions)
    case ('RIV')
      call gwf_set_param_pointer(def, gwf_riv_param_definitions)
    case ('WEL')
      call gwf_set_param_pointer(def, gwf_wel_param_definitions)
    case default
    end select
    return
  end function gwf_param_definitions
  function gwt_param_definitions(subcomponent) result(def)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (subcomponent)
    case ('NAM')
      call gwt_set_param_pointer(def, gwt_nam_param_definitions)
    case ('DIS')
      call gwt_set_param_pointer(def, gwt_dis_param_definitions)
    case ('DISU')
      call gwt_set_param_pointer(def, gwt_disu_param_definitions)
    case ('DISV')
      call gwt_set_param_pointer(def, gwt_disv_param_definitions)
    case ('DSP')
      call gwt_set_param_pointer(def, gwt_dsp_param_definitions)
    case ('CNC')
      call gwt_set_param_pointer(def, gwt_cnc_param_definitions)
    case ('IC')
      call gwt_set_param_pointer(def, gwt_ic_param_definitions)
    case default
    end select
    return
  end function gwt_param_definitions

  function gwf_aggregate_definitions(subcomponent) result(def)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (subcomponent)
    case ('NAM')
      call gwf_set_param_pointer(def, gwf_nam_aggregate_definitions)
    case ('CHD')
      call gwf_set_param_pointer(def, gwf_chd_aggregate_definitions)
    case ('DIS')
      call gwf_set_param_pointer(def, gwf_dis_aggregate_definitions)
    case ('DISU')
      call gwf_set_param_pointer(def, gwf_disu_aggregate_definitions)
    case ('DISV')
      call gwf_set_param_pointer(def, gwf_disv_aggregate_definitions)
    case ('DRN')
      call gwf_set_param_pointer(def, gwf_drn_aggregate_definitions)
    case ('EVT')
      call gwf_set_param_pointer(def, gwf_evt_aggregate_definitions)
    case ('EVTA')
      call gwf_set_param_pointer(def, gwf_evta_aggregate_definitions)
    case ('GHB')
      call gwf_set_param_pointer(def, gwf_ghb_aggregate_definitions)
    case ('IC')
      call gwf_set_param_pointer(def, gwf_ic_aggregate_definitions)
    case ('NPF')
      call gwf_set_param_pointer(def, gwf_npf_aggregate_definitions)
    case ('RCH')
      call gwf_set_param_pointer(def, gwf_rch_aggregate_definitions)
    case ('RCHA')
      call gwf_set_param_pointer(def, gwf_rcha_aggregate_definitions)
    case ('RIV')
      call gwf_set_param_pointer(def, gwf_riv_aggregate_definitions)
    case ('WEL')
      call gwf_set_param_pointer(def, gwf_wel_aggregate_definitions)
    case default
    end select
    return
  end function gwf_aggregate_definitions
  function gwt_aggregate_definitions(subcomponent) result(def)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (subcomponent)
    case ('NAM')
      call gwt_set_param_pointer(def, gwt_nam_aggregate_definitions)
    case ('DIS')
      call gwt_set_param_pointer(def, gwt_dis_aggregate_definitions)
    case ('DISU')
      call gwt_set_param_pointer(def, gwt_disu_aggregate_definitions)
    case ('DISV')
      call gwt_set_param_pointer(def, gwt_disv_aggregate_definitions)
    case ('DSP')
      call gwt_set_param_pointer(def, gwt_dsp_aggregate_definitions)
    case ('CNC')
      call gwt_set_param_pointer(def, gwt_cnc_aggregate_definitions)
    case ('IC')
      call gwt_set_param_pointer(def, gwt_ic_aggregate_definitions)
    case default
    end select
    return
  end function gwt_aggregate_definitions

  function gwf_block_definitions(subcomponent) result(def)
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (subcomponent)
    case ('NAM')
      call gwf_set_block_pointer(def, gwf_nam_block_definitions)
    case ('CHD')
      call gwf_set_block_pointer(def, gwf_chd_block_definitions)
    case ('DIS')
      call gwf_set_block_pointer(def, gwf_dis_block_definitions)
    case ('DISU')
      call gwf_set_block_pointer(def, gwf_disu_block_definitions)
    case ('DISV')
      call gwf_set_block_pointer(def, gwf_disv_block_definitions)
    case ('DRN')
      call gwf_set_block_pointer(def, gwf_drn_block_definitions)
    case ('EVT')
      call gwf_set_block_pointer(def, gwf_evt_block_definitions)
    case ('EVTA')
      call gwf_set_block_pointer(def, gwf_evta_block_definitions)
    case ('GHB')
      call gwf_set_block_pointer(def, gwf_ghb_block_definitions)
    case ('IC')
      call gwf_set_block_pointer(def, gwf_ic_block_definitions)
    case ('NPF')
      call gwf_set_block_pointer(def, gwf_npf_block_definitions)
    case ('RCH')
      call gwf_set_block_pointer(def, gwf_rch_block_definitions)
    case ('RCHA')
      call gwf_set_block_pointer(def, gwf_rcha_block_definitions)
    case ('RIV')
      call gwf_set_block_pointer(def, gwf_riv_block_definitions)
    case ('WEL')
      call gwf_set_block_pointer(def, gwf_wel_block_definitions)
    case default
    end select
    return
  end function gwf_block_definitions
  function gwt_block_definitions(subcomponent) result(def)
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: def
    nullify (def)
    select case (subcomponent)
    case ('NAM')
      call gwt_set_block_pointer(def, gwt_nam_block_definitions)
    case ('DIS')
      call gwt_set_block_pointer(def, gwt_dis_block_definitions)
    case ('DISU')
      call gwt_set_block_pointer(def, gwt_disu_block_definitions)
    case ('DISV')
      call gwt_set_block_pointer(def, gwt_disv_block_definitions)
    case ('DSP')
      call gwt_set_block_pointer(def, gwt_dsp_block_definitions)
    case ('CNC')
      call gwt_set_block_pointer(def, gwt_cnc_block_definitions)
    case ('IC')
      call gwt_set_block_pointer(def, gwt_ic_block_definitions)
    case default
    end select
    return
  end function gwt_block_definitions

  function gwf_idm_multi_package(subcomponent) result(multi_package)
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (subcomponent)
    case ('NAM')
      multi_package = gwf_nam_multi_package
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
    case ('IC')
      multi_package = gwf_ic_multi_package
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
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="GWF"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function gwf_idm_multi_package
  function gwt_idm_multi_package(subcomponent) result(multi_package)
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (subcomponent)
    case ('NAM')
      multi_package = gwt_nam_multi_package
    case ('DIS')
      multi_package = gwt_dis_multi_package
    case ('DISU')
      multi_package = gwt_disu_multi_package
    case ('DISV')
      multi_package = gwt_disv_multi_package
    case ('DSP')
      multi_package = gwt_dsp_multi_package
    case ('CNC')
      multi_package = gwt_cnc_multi_package
    case ('IC')
      multi_package = gwt_ic_multi_package
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="GWF"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function gwt_idm_multi_package

  function gwf_idm_integrated(subcomponent) result(integrated)
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (subcomponent)
    case ('NAM')
      integrated = .true.
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
    case ('IC')
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
    case default
    end select
    return
  end function gwf_idm_integrated
  function gwt_idm_integrated(subcomponent) result(integrated)
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (subcomponent)
    case ('NAM')
      integrated = .true.
    case ('DIS')
      integrated = .true.
    case ('DISU')
      integrated = .true.
    case ('DISV')
      integrated = .true.
    case ('DSP')
      integrated = .true.
    case ('CNC')
      integrated = .true.
    case ('IC')
      integrated = .true.
    case default
    end select
    return
  end function gwt_idm_integrated

end module IdmDfnSelectorModule

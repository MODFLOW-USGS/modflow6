module GwtDspInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwt_dsp_param_definitions
  public gwt_dsp_aggregate_definitions
  public gwt_dsp_block_definitions
  type(InputParamDefinitionType), parameter :: &
    gwt_dsp_param_definitions(*) = &
    [ &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWT', 'DSP', 'OPTIONS', 'XT3D_OFF', 'XT3D_OFF', 'KEYWORD', '', .false., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWT', 'DSP', 'OPTIONS', 'XT3D_RHS', 'XT3D_RHS', 'KEYWORD', '', .false., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWT', 'DSP', 'GRIDDATA', 'DIFFC', 'DIFFC', 'DOUBLE1D', 'NODES', .false., &
    .false., .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWT', 'DSP', 'GRIDDATA', 'ALH', 'ALH', 'DOUBLE1D', 'NODES', .false., &
    .false., .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWT', 'DSP', 'GRIDDATA', 'ALV', 'ALV', 'DOUBLE1D', 'NODES', .false., &
    .false., .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWT', 'DSP', 'GRIDDATA', 'ATH1', 'ATH1', 'DOUBLE1D', 'NODES', .false., &
    .false., .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWT', 'DSP', 'GRIDDATA', 'ATH2', 'ATH2', 'DOUBLE1D', 'NODES', .false., &
    .false., .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWT', 'DSP', 'GRIDDATA', 'ATV', 'ATV', 'DOUBLE1D', 'NODES', .false., &
    .false., .false., .true. &
    ) &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwt_dsp_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType :: &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwt_dsp_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .false., & ! required
    .false. & ! aggregate
    ) &
    ]

end module GwtDspInputModule

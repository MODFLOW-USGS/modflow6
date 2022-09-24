module GwtDspInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwt_dsp_param_definitions
  public gwt_dsp_aggregate_definitions
  public gwt_dsp_block_definitions

  type(InputParamDefinitionType), parameter :: &
    mf6var_xt3d_off = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'DSP', & ! subcomponent
    'OPTIONS', & ! block
    'XT3D_OFF', & ! tag name
    'XT3D_OFF', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_xt3d_rhs = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'DSP', & ! subcomponent
    'OPTIONS', & ! block
    'XT3D_RHS', & ! tag name
    'XT3D_RHS', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_diffc = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'DSP', & ! subcomponent
    'GRIDDATA', & ! block
    'DIFFC', & ! tag name
    'DIFFC', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_alh = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'DSP', & ! subcomponent
    'GRIDDATA', & ! block
    'ALH', & ! tag name
    'ALH', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_alv = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'DSP', & ! subcomponent
    'GRIDDATA', & ! block
    'ALV', & ! tag name
    'ALV', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_ath1 = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'DSP', & ! subcomponent
    'GRIDDATA', & ! block
    'ATH1', & ! tag name
    'ATH1', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_ath2 = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'DSP', & ! subcomponent
    'GRIDDATA', & ! block
    'ATH2', & ! tag name
    'ATH2', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_atv = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'DSP', & ! subcomponent
    'GRIDDATA', & ! block
    'ATV', & ! tag name
    'ATV', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwt_dsp_param_definitions(*) = &
    [ &
    mf6var_xt3d_off, &
    mf6var_xt3d_rhs, &
    mf6var_diffc, &
    mf6var_alh, &
    mf6var_alv, &
    mf6var_ath1, &
    mf6var_ath2, &
    mf6var_atv &
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

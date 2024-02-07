! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwtDspInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwt_dsp_param_definitions
  public gwt_dsp_aggregate_definitions
  public gwt_dsp_block_definitions
  public GwtDspParamFoundType
  public gwt_dsp_multi_package

  type GwtDspParamFoundType
    logical :: xt3d_off = .false.
    logical :: xt3d_rhs = .false.
    logical :: diffc = .false.
    logical :: alh = .false.
    logical :: alv = .false.
    logical :: ath1 = .false.
    logical :: ath2 = .false.
    logical :: atv = .false.
  end type GwtDspParamFoundType

  logical :: gwt_dsp_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    gwtdsp_xt3d_off = InputParamDefinitionType &
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
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwtdsp_xt3d_rhs = InputParamDefinitionType &
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
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwtdsp_diffc = InputParamDefinitionType &
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
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwtdsp_alh = InputParamDefinitionType &
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
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwtdsp_alv = InputParamDefinitionType &
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
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwtdsp_ath1 = InputParamDefinitionType &
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
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwtdsp_ath2 = InputParamDefinitionType &
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
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwtdsp_atv = InputParamDefinitionType &
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
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwt_dsp_param_definitions(*) = &
    [ &
    gwtdsp_xt3d_off, &
    gwtdsp_xt3d_rhs, &
    gwtdsp_diffc, &
    gwtdsp_alh, &
    gwtdsp_alv, &
    gwtdsp_ath1, &
    gwtdsp_ath2, &
    gwtdsp_atv &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwt_dsp_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType &
    ( &
    '', & ! component
    '', & ! subcomponent
    '', & ! block
    '', & ! tag name
    '', & ! fortran variable
    '', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    ) &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwt_dsp_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module GwtDspInputModule

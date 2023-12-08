! ** Do Not Modify! MODFLOW 6 system generated file. **
module GweDspInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwe_dsp_param_definitions
  public gwe_dsp_aggregate_definitions
  public gwe_dsp_block_definitions
  public GweDspParamFoundType
  public gwe_dsp_multi_package

  type GweDspParamFoundType
    logical :: xt3d_off = .false.
    logical :: xt3d_rhs = .false.
    logical :: alh = .false.
    logical :: alv = .false.
    logical :: ath1 = .false.
    logical :: ath2 = .false.
    logical :: atv = .false.
    logical :: ktw = .false.
    logical :: kts = .false.
  end type GweDspParamFoundType

  logical :: gwe_dsp_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    gwedsp_xt3d_off = InputParamDefinitionType &
    ( &
    'GWE', & ! component
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
    gwedsp_xt3d_rhs = InputParamDefinitionType &
    ( &
    'GWE', & ! component
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
    gwedsp_alh = InputParamDefinitionType &
    ( &
    'GWE', & ! component
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
    gwedsp_alv = InputParamDefinitionType &
    ( &
    'GWE', & ! component
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
    gwedsp_ath1 = InputParamDefinitionType &
    ( &
    'GWE', & ! component
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
    gwedsp_ath2 = InputParamDefinitionType &
    ( &
    'GWE', & ! component
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
    gwedsp_atv = InputParamDefinitionType &
    ( &
    'GWE', & ! component
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
    gwedsp_ktw = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DSP', & ! subcomponent
    'GRIDDATA', & ! block
    'KTW', & ! tag name
    'KTW', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedsp_kts = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DSP', & ! subcomponent
    'GRIDDATA', & ! block
    'KTS', & ! tag name
    'KTS', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwe_dsp_param_definitions(*) = &
    [ &
    gwedsp_xt3d_off, &
    gwedsp_xt3d_rhs, &
    gwedsp_alh, &
    gwedsp_alv, &
    gwedsp_ath1, &
    gwedsp_ath2, &
    gwedsp_atv, &
    gwedsp_ktw, &
    gwedsp_kts &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwe_dsp_aggregate_definitions(*) = &
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
    gwe_dsp_block_definitions(*) = &
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

end module GweDspInputModule

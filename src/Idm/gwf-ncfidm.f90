! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwfNcfInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_ncf_param_definitions
  public gwf_ncf_aggregate_definitions
  public gwf_ncf_block_definitions
  public GwfNcfParamFoundType
  public gwf_ncf_multi_package
  public gwf_ncf_subpackages

  type GwfNcfParamFoundType
    logical :: ogc_wkt = .false.
  end type GwfNcfParamFoundType

  logical :: gwf_ncf_multi_package = .false.

  character(len=16), parameter :: &
    gwf_ncf_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwfncf_ogc_wkt = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'OGC_WKT', & ! tag name
    'OGC_WKT', & ! fortran variable
    'STRING', & ! type
    'LENBIGLINE', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_ncf_param_definitions(*) = &
    [ &
    gwfncf_ogc_wkt &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwf_ncf_aggregate_definitions(*) = &
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
    gwf_ncf_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module GwfNcfInputModule

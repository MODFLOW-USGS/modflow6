! ** Do Not Modify! MODFLOW 6 system generated file. **
module UtlNcfInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public utl_ncf_param_definitions
  public utl_ncf_aggregate_definitions
  public utl_ncf_block_definitions
  public UtlNcfParamFoundType
  public utl_ncf_multi_package
  public utl_ncf_subpackages

  type UtlNcfParamFoundType
    logical :: ogc_wkt = .false.
  end type UtlNcfParamFoundType

  logical :: utl_ncf_multi_package = .false.

  character(len=16), parameter :: &
    utl_ncf_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    utlncf_ogc_wkt = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'OGC_WKT', & ! tag name
    'OGC_WKT', & ! fortran variable
    'STRING', & ! type
    'LENBIGLINE', & ! shape
    'CRS well-known text (WKT) string', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utl_ncf_param_definitions(*) = &
    [ &
    utlncf_ogc_wkt &
    ]

  type(InputParamDefinitionType), parameter :: &
    utl_ncf_aggregate_definitions(*) = &
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
    '', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    ) &
    ]

  type(InputBlockDefinitionType), parameter :: &
    utl_ncf_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module UtlNcfInputModule

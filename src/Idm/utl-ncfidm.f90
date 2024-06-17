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
    logical :: deflate = .false.
    logical :: shuffle = .false.
    logical :: ugc_record = .false.
    logical :: chunking_ugrid = .false.
    logical :: ugc_time = .false.
    logical :: ugc_face = .false.
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
    utlncf_deflate = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'DEFLATE', & ! tag name
    'DEFLATE', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'variable compression deflate level', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utlncf_shuffle = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'SHUFFLE', & ! tag name
    'SHUFFLE', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    '', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utlncf_ugc_record = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'UGC_RECORD', & ! tag name
    'UGC_RECORD', & ! fortran variable
    'RECORD CHUNKING_UGRID UGC_TIME UGC_FACE', & ! type
    '', & ! shape
    'ugrid time and face dimension chunking parameters', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utlncf_chunking_ugrid = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'CHUNKING_UGRID', & ! tag name
    'CHUNKING_UGRID', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'keyword when defining ugrid chunking parameters', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utlncf_ugc_time = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'UGC_TIME', & ! tag name
    'UGC_TIME', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'chunking parameter for the time dimension', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utlncf_ugc_face = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'UGC_FACE', & ! tag name
    'UGC_FACE', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'chunking parameter for the face dimension', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utl_ncf_param_definitions(*) = &
    [ &
    utlncf_ogc_wkt, &
    utlncf_deflate, &
    utlncf_shuffle, &
    utlncf_ugc_record, &
    utlncf_chunking_ugrid, &
    utlncf_ugc_time, &
    utlncf_ugc_face &
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

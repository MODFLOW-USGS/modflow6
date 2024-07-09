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
    logical :: chunk_record = .false.
    logical :: chunking = .false.
    logical :: chunk_time = .false.
    logical :: chunk_face = .false.
    logical :: chunk_z = .false.
    logical :: chunk_y = .false.
    logical :: chunk_x = .false.
    logical :: attr_off = .false.
    logical :: ncpl = .false.
    logical :: lat = .false.
    logical :: lon = .false.
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
    utlncf_chunk_record = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'CHUNK_RECORD', & ! tag name
    'CHUNK_RECORD', & ! fortran variable
    'RECORD CHUNKING CHUNK_TIME CHUNK_FACE CHUNK_Z CHUNK_Y CHUNK_X', & ! type
    '', & ! shape
    'netcdf export chunking record', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utlncf_chunking = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'CHUNKING', & ! tag name
    'CHUNKING', & ! fortran variable
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
    utlncf_chunk_time = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'CHUNK_TIME', & ! tag name
    'CHUNK_TIME', & ! fortran variable
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
    utlncf_chunk_face = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'CHUNK_FACE', & ! tag name
    'CHUNK_FACE', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'chunking parameter for the ugrid face dimension', & ! longname
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utlncf_chunk_z = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'CHUNK_Z', & ! tag name
    'CHUNK_Z', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'chunking parameter for structured z', & ! longname
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utlncf_chunk_y = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'CHUNK_Y', & ! tag name
    'CHUNK_Y', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'chunking parameter for structured y', & ! longname
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utlncf_chunk_x = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'CHUNK_X', & ! tag name
    'CHUNK_X', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'chunking parameter for structured x', & ! longname
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utlncf_attr_off = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'OPTIONS', & ! block
    'MODFLOW6_ATTR_OFF', & ! tag name
    'ATTR_OFF', & ! fortran variable
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
    utlncf_ncpl = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'DIMENSIONS', & ! block
    'NCPL', & ! tag name
    'NCPL', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'number of cells in layer', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utlncf_lat = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'GRIDDATA', & ! block
    'LAT', & ! tag name
    'LAT', & ! fortran variable
    'DOUBLE1D', & ! type
    'NCPL', & ! shape
    'cell center latitude', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utlncf_lon = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'NCF', & ! subcomponent
    'GRIDDATA', & ! block
    'LON', & ! tag name
    'LON', & ! fortran variable
    'DOUBLE1D', & ! type
    'NCPL', & ! shape
    'cell center longitude', & ! longname
    .false., & ! required
    .false., & ! multi-record
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
    utlncf_chunk_record, &
    utlncf_chunking, &
    utlncf_chunk_time, &
    utlncf_chunk_face, &
    utlncf_chunk_z, &
    utlncf_chunk_y, &
    utlncf_chunk_x, &
    utlncf_attr_off, &
    utlncf_ncpl, &
    utlncf_lat, &
    utlncf_lon &
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
    ), &
    InputBlockDefinitionType( &
    'DIMENSIONS', & ! blockname
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

end module UtlNcfInputModule

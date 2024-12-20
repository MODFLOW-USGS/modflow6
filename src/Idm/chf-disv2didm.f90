! ** Do Not Modify! MODFLOW 6 system generated file. **
module ChfDisv2DInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public chf_disv2d_param_definitions
  public chf_disv2d_aggregate_definitions
  public chf_disv2d_block_definitions
  public ChfDisv2dParamFoundType
  public chf_disv2d_multi_package
  public chf_disv2d_subpackages

  type ChfDisv2dParamFoundType
    logical :: length_units = .false.
    logical :: nogrb = .false.
    logical :: xorigin = .false.
    logical :: yorigin = .false.
    logical :: angrot = .false.
    logical :: export_ascii = .false.
    logical :: nodes = .false.
    logical :: nvert = .false.
    logical :: bottom = .false.
    logical :: idomain = .false.
    logical :: iv = .false.
    logical :: xv = .false.
    logical :: yv = .false.
    logical :: icell2d = .false.
    logical :: xc = .false.
    logical :: yc = .false.
    logical :: ncvert = .false.
    logical :: icvert = .false.
  end type ChfDisv2dParamFoundType

  logical :: chf_disv2d_multi_package = .false.

  character(len=16), parameter :: &
    chf_disv2d_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_length_units = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'OPTIONS', & ! block
    'LENGTH_UNITS', & ! tag name
    'LENGTH_UNITS', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'model length units', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_nogrb = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'OPTIONS', & ! block
    'NOGRB', & ! tag name
    'NOGRB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'do not write binary grid file', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_xorigin = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'OPTIONS', & ! block
    'XORIGIN', & ! tag name
    'XORIGIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'x-position of the model grid origin', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_yorigin = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'OPTIONS', & ! block
    'YORIGIN', & ! tag name
    'YORIGIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'y-position of the model grid origin', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_angrot = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'OPTIONS', & ! block
    'ANGROT', & ! tag name
    'ANGROT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'rotation angle', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_export_ascii = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'OPTIONS', & ! block
    'EXPORT_ARRAY_ASCII', & ! tag name
    'EXPORT_ASCII', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'export array variables to layered ascii files.', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_nodes = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'DIMENSIONS', & ! block
    'NODES', & ! tag name
    'NODES', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'number of cells per layer', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_nvert = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'DIMENSIONS', & ! block
    'NVERT', & ! tag name
    'NVERT', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'number of columns', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_bottom = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'GRIDDATA', & ! block
    'BOTTOM', & ! tag name
    'BOTTOM', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    'model bottom elevation', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_idomain = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'GRIDDATA', & ! block
    'IDOMAIN', & ! tag name
    'IDOMAIN', & ! fortran variable
    'INTEGER1D', & ! type
    'NODES', & ! shape
    'idomain existence array', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_iv = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'VERTICES', & ! block
    'IV', & ! tag name
    'IV', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'vertex number', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_xv = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'VERTICES', & ! block
    'XV', & ! tag name
    'XV', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'x-coordinate for vertex', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_yv = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'VERTICES', & ! block
    'YV', & ! tag name
    'YV', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'y-coordinate for vertex', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_icell2d = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'CELL2D', & ! block
    'ICELL2D', & ! tag name
    'ICELL2D', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'cell2d number', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_xc = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'CELL2D', & ! block
    'XC', & ! tag name
    'XC', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'x-coordinate for cell center', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_yc = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'CELL2D', & ! block
    'YC', & ! tag name
    'YC', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'y-coordinate for cell center', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_ncvert = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'CELL2D', & ! block
    'NCVERT', & ! tag name
    'NCVERT', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'number of cell vertices', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_icvert = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'CELL2D', & ! block
    'ICVERT', & ! tag name
    'ICVERT', & ! fortran variable
    'INTEGER1D', & ! type
    'NCVERT', & ! shape
    'array of vertex numbers', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chf_disv2d_param_definitions(*) = &
    [ &
    chfdisv2d_length_units, &
    chfdisv2d_nogrb, &
    chfdisv2d_xorigin, &
    chfdisv2d_yorigin, &
    chfdisv2d_angrot, &
    chfdisv2d_export_ascii, &
    chfdisv2d_nodes, &
    chfdisv2d_nvert, &
    chfdisv2d_bottom, &
    chfdisv2d_idomain, &
    chfdisv2d_iv, &
    chfdisv2d_xv, &
    chfdisv2d_yv, &
    chfdisv2d_icell2d, &
    chfdisv2d_xc, &
    chfdisv2d_yc, &
    chfdisv2d_ncvert, &
    chfdisv2d_icvert &
    ]

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_vertices = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'VERTICES', & ! block
    'VERTICES', & ! tag name
    'VERTICES', & ! fortran variable
    'RECARRAY IV XV YV', & ! type
    'NVERT', & ! shape
    'vertices data', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdisv2d_cell2d = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DISV2D', & ! subcomponent
    'CELL2D', & ! block
    'CELL2D', & ! tag name
    'CELL2D', & ! fortran variable
    'RECARRAY ICELL2D XC YC NCVERT ICVERT', & ! type
    'NODES', & ! shape
    'cell2d data', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chf_disv2d_aggregate_definitions(*) = &
    [ &
    chfdisv2d_vertices, &
    chfdisv2d_cell2d &
    ]

  type(InputBlockDefinitionType), parameter :: &
    chf_disv2d_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'DIMENSIONS', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'VERTICES', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'CELL2D', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module ChfDisv2DInputModule

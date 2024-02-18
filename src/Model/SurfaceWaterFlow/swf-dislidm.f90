! ** Do Not Modify! MODFLOW 6 system generated file. **
module SwfDislInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public swf_disl_param_definitions
  public swf_disl_aggregate_definitions
  public swf_disl_block_definitions
  public SwfDislParamFoundType
  public swf_disl_multi_package

  type SwfDislParamFoundType
    logical :: length_units = .false.
    logical :: length_convert = .false.
    logical :: time_convert = .false.
    logical :: nogrb = .false.
    logical :: xorigin = .false.
    logical :: yorigin = .false.
    logical :: angrot = .false.
    logical :: nodes = .false.
    logical :: nvert = .false.
    logical :: reach_length = .false.
    logical :: reach_bottom = .false.
    logical :: toreach = .false.
    logical :: idomain = .false.
    logical :: iv = .false.
    logical :: xv = .false.
    logical :: yv = .false.
    logical :: zv = .false.
    logical :: icell2d = .false.
    logical :: fdc = .false.
    logical :: ncvert = .false.
    logical :: icvert = .false.
  end type SwfDislParamFoundType

  logical :: swf_disl_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    swfdisl_length_units = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'LENGTH_UNITS', & ! tag name
    'LENGTH_UNITS', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_length_convert = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'LENGTH_CONVERT', & ! tag name
    'LENGTH_CONVERT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_time_convert = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'TIME_CONVERT', & ! tag name
    'TIME_CONVERT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_nogrb = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'NOGRB', & ! tag name
    'NOGRB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_xorigin = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'XORIGIN', & ! tag name
    'XORIGIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_yorigin = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'YORIGIN', & ! tag name
    'YORIGIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_angrot = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'ANGROT', & ! tag name
    'ANGROT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_nodes = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'DIMENSIONS', & ! block
    'NODES', & ! tag name
    'NODES', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_nvert = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'DIMENSIONS', & ! block
    'NVERT', & ! tag name
    'NVERT', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_reach_length = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'GRIDDATA', & ! block
    'REACH_LENGTH', & ! tag name
    'REACH_LENGTH', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_reach_bottom = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'GRIDDATA', & ! block
    'REACH_BOTTOM', & ! tag name
    'REACH_BOTTOM', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_toreach = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'GRIDDATA', & ! block
    'TOREACH', & ! tag name
    'TOREACH', & ! fortran variable
    'INTEGER1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_idomain = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'GRIDDATA', & ! block
    'IDOMAIN', & ! tag name
    'IDOMAIN', & ! fortran variable
    'INTEGER1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_iv = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'VERTICES', & ! block
    'IV', & ! tag name
    'IV', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_xv = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'VERTICES', & ! block
    'XV', & ! tag name
    'XV', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_yv = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'VERTICES', & ! block
    'YV', & ! tag name
    'YV', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_zv = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'VERTICES', & ! block
    'ZV', & ! tag name
    'ZV', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_icell2d = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'CELL2D', & ! block
    'ICELL2D', & ! tag name
    'ICELL2D', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_fdc = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'CELL2D', & ! block
    'FDC', & ! tag name
    'FDC', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_ncvert = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'CELL2D', & ! block
    'NCVERT', & ! tag name
    'NCVERT', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_icvert = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'CELL2D', & ! block
    'ICVERT', & ! tag name
    'ICVERT', & ! fortran variable
    'INTEGER1D', & ! type
    'NCVERT', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swf_disl_param_definitions(*) = &
    [ &
    swfdisl_length_units, &
    swfdisl_length_convert, &
    swfdisl_time_convert, &
    swfdisl_nogrb, &
    swfdisl_xorigin, &
    swfdisl_yorigin, &
    swfdisl_angrot, &
    swfdisl_nodes, &
    swfdisl_nvert, &
    swfdisl_reach_length, &
    swfdisl_reach_bottom, &
    swfdisl_toreach, &
    swfdisl_idomain, &
    swfdisl_iv, &
    swfdisl_xv, &
    swfdisl_yv, &
    swfdisl_zv, &
    swfdisl_icell2d, &
    swfdisl_fdc, &
    swfdisl_ncvert, &
    swfdisl_icvert &
    ]

  type(InputParamDefinitionType), parameter :: &
    swfdisl_vertices = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'VERTICES', & ! block
    'VERTICES', & ! tag name
    'VERTICES', & ! fortran variable
    'RECARRAY IV XV YV ZV', & ! type
    'NVERT', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfdisl_cell2d = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'DISL', & ! subcomponent
    'CELL2D', & ! block
    'CELL2D', & ! tag name
    'CELL2D', & ! fortran variable
    'RECARRAY ICELL2D FDC NCVERT ICVERT', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swf_disl_aggregate_definitions(*) = &
    [ &
    swfdisl_vertices, &
    swfdisl_cell2d &
    ]

  type(InputBlockDefinitionType), parameter :: &
    swf_disl_block_definitions(*) = &
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

end module SwfDislInputModule

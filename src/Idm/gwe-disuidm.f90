! ** Do Not Modify! MODFLOW 6 system generated file. **
module GweDisuInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwe_disu_param_definitions
  public gwe_disu_aggregate_definitions
  public gwe_disu_block_definitions
  public GweDisuParamFoundType
  public gwe_disu_multi_package
  public gwe_disu_subpackages

  type GweDisuParamFoundType
    logical :: length_units = .false.
    logical :: nogrb = .false.
    logical :: xorigin = .false.
    logical :: yorigin = .false.
    logical :: angrot = .false.
    logical :: voffsettol = .false.
    logical :: export_ascii = .false.
    logical :: nodes = .false.
    logical :: nja = .false.
    logical :: nvert = .false.
    logical :: top = .false.
    logical :: bot = .false.
    logical :: area = .false.
    logical :: idomain = .false.
    logical :: iac = .false.
    logical :: ja = .false.
    logical :: ihc = .false.
    logical :: cl12 = .false.
    logical :: hwva = .false.
    logical :: angldegx = .false.
    logical :: iv = .false.
    logical :: xv = .false.
    logical :: yv = .false.
    logical :: icell2d = .false.
    logical :: xc = .false.
    logical :: yc = .false.
    logical :: ncvert = .false.
    logical :: icvert = .false.
  end type GweDisuParamFoundType

  logical :: gwe_disu_multi_package = .false.

  character(len=16), parameter :: &
    gwe_disu_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwedisu_length_units = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_nogrb = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_xorigin = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_yorigin = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_angrot = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_voffsettol = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'OPTIONS', & ! block
    'VERTICAL_OFFSET_TOLERANCE', & ! tag name
    'VOFFSETTOL', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_export_ascii = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'OPTIONS', & ! block
    'EXPORT_ARRAY_ASCII', & ! tag name
    'EXPORT_ASCII', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_nodes = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_nja = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'DIMENSIONS', & ! block
    'NJA', & ! tag name
    'NJA', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_nvert = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_top = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'GRIDDATA', & ! block
    'TOP', & ! tag name
    'TOP', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_bot = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'GRIDDATA', & ! block
    'BOT', & ! tag name
    'BOT', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_area = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'GRIDDATA', & ! block
    'AREA', & ! tag name
    'AREA', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_idomain = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_iac = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'CONNECTIONDATA', & ! block
    'IAC', & ! tag name
    'IAC', & ! fortran variable
    'INTEGER1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_ja = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'CONNECTIONDATA', & ! block
    'JA', & ! tag name
    'JA', & ! fortran variable
    'INTEGER1D', & ! type
    'NJA', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_ihc = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'CONNECTIONDATA', & ! block
    'IHC', & ! tag name
    'IHC', & ! fortran variable
    'INTEGER1D', & ! type
    'NJA', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_cl12 = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'CONNECTIONDATA', & ! block
    'CL12', & ! tag name
    'CL12', & ! fortran variable
    'DOUBLE1D', & ! type
    'NJA', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_hwva = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'CONNECTIONDATA', & ! block
    'HWVA', & ! tag name
    'HWVA', & ! fortran variable
    'DOUBLE1D', & ! type
    'NJA', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_angldegx = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'CONNECTIONDATA', & ! block
    'ANGLDEGX', & ! tag name
    'ANGLDEGX', & ! fortran variable
    'DOUBLE1D', & ! type
    'NJA', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_iv = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_xv = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_yv = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_icell2d = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_xc = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'CELL2D', & ! block
    'XC', & ! tag name
    'XC', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_yc = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'CELL2D', & ! block
    'YC', & ! tag name
    'YC', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_ncvert = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwedisu_icvert = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
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
    gwe_disu_param_definitions(*) = &
    [ &
    gwedisu_length_units, &
    gwedisu_nogrb, &
    gwedisu_xorigin, &
    gwedisu_yorigin, &
    gwedisu_angrot, &
    gwedisu_voffsettol, &
    gwedisu_export_ascii, &
    gwedisu_nodes, &
    gwedisu_nja, &
    gwedisu_nvert, &
    gwedisu_top, &
    gwedisu_bot, &
    gwedisu_area, &
    gwedisu_idomain, &
    gwedisu_iac, &
    gwedisu_ja, &
    gwedisu_ihc, &
    gwedisu_cl12, &
    gwedisu_hwva, &
    gwedisu_angldegx, &
    gwedisu_iv, &
    gwedisu_xv, &
    gwedisu_yv, &
    gwedisu_icell2d, &
    gwedisu_xc, &
    gwedisu_yc, &
    gwedisu_ncvert, &
    gwedisu_icvert &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwedisu_vertices = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'VERTICES', & ! block
    'VERTICES', & ! tag name
    'VERTICES', & ! fortran variable
    'RECARRAY IV XV YV', & ! type
    'NVERT', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwedisu_cell2d = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'DISU', & ! subcomponent
    'CELL2D', & ! block
    'CELL2D', & ! tag name
    'CELL2D', & ! fortran variable
    'RECARRAY ICELL2D XC YC NCVERT ICVERT', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwe_disu_aggregate_definitions(*) = &
    [ &
    gwedisu_vertices, &
    gwedisu_cell2d &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwe_disu_block_definitions(*) = &
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
    'CONNECTIONDATA', & ! blockname
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

end module GweDisuInputModule

! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwfDisuInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_disu_param_definitions
  public gwf_disu_aggregate_definitions
  public gwf_disu_block_definitions
  public GwfDisuParamFoundType
  public gwf_disu_multi_package

  type GwfDisuParamFoundType
    logical :: length_units = .false.
    logical :: nogrb = .false.
    logical :: xorigin = .false.
    logical :: yorigin = .false.
    logical :: angrot = .false.
    logical :: voffsettol = .false.
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
  end type GwfDisuParamFoundType

  logical :: gwf_disu_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    gwfdisu_length_units = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_nogrb = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_xorigin = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_yorigin = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_angrot = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_voffsettol = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_nodes = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_nja = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_nvert = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_top = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_bot = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_area = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_idomain = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_iac = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_ja = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_ihc = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_cl12 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_hwva = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_angldegx = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_iv = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_xv = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_yv = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_icell2d = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_xc = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_yc = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_ncvert = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfdisu_icvert = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwf_disu_param_definitions(*) = &
    [ &
    gwfdisu_length_units, &
    gwfdisu_nogrb, &
    gwfdisu_xorigin, &
    gwfdisu_yorigin, &
    gwfdisu_angrot, &
    gwfdisu_voffsettol, &
    gwfdisu_nodes, &
    gwfdisu_nja, &
    gwfdisu_nvert, &
    gwfdisu_top, &
    gwfdisu_bot, &
    gwfdisu_area, &
    gwfdisu_idomain, &
    gwfdisu_iac, &
    gwfdisu_ja, &
    gwfdisu_ihc, &
    gwfdisu_cl12, &
    gwfdisu_hwva, &
    gwfdisu_angldegx, &
    gwfdisu_iv, &
    gwfdisu_xv, &
    gwfdisu_yv, &
    gwfdisu_icell2d, &
    gwfdisu_xc, &
    gwfdisu_yc, &
    gwfdisu_ncvert, &
    gwfdisu_icvert &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwfdisu_vertices = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISU', & ! subcomponent
    'VERTICES', & ! block
    'VERTICES', & ! tag name
    'VERTICES', & ! fortran variable
    'RECARRAY IV XV YV', & ! type
    'NVERT', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisu_cell2d = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISU', & ! subcomponent
    'CELL2D', & ! block
    'CELL2D', & ! tag name
    'CELL2D', & ! fortran variable
    'RECARRAY ICELL2D XC YC NCVERT ICVERT', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_disu_aggregate_definitions(*) = &
    [ &
    gwfdisu_vertices, &
    gwfdisu_cell2d &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_disu_block_definitions(*) = &
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
    .false., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'CELL2D', & ! blockname
    .false., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module GwfDisuInputModule

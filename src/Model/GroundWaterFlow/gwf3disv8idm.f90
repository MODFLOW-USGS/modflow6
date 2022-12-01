module GwfDisvInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_disv_param_definitions
  public gwf_disv_aggregate_definitions
  public gwf_disv_block_definitions
  public GwfDisvParamFoundType

  type GwfDisvParamFoundType
    logical :: length_units = .false.
    logical :: nogrb = .false.
    logical :: xorigin = .false.
    logical :: yorigin = .false.
    logical :: angrot = .false.
    logical :: nlay = .false.
    logical :: ncpl = .false.
    logical :: nvert = .false.
    logical :: top = .false.
    logical :: botm = .false.
    logical :: idomain = .false.
    logical :: iv = .false.
    logical :: xv = .false.
    logical :: yv = .false.
    logical :: icell2d = .false.
    logical :: xc = .false.
    logical :: yc = .false.
    logical :: ncvert = .false.
    logical :: icvert = .false.
  end type GwfDisvParamFoundType

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_length_units = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'OPTIONS', & ! block
    'LENGTH_UNITS', & ! tag name
    'LENGTH_UNITS', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_nogrb = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'OPTIONS', & ! block
    'NOGRB', & ! tag name
    'NOGRB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_xorigin = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'OPTIONS', & ! block
    'XORIGIN', & ! tag name
    'XORIGIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_yorigin = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'OPTIONS', & ! block
    'YORIGIN', & ! tag name
    'YORIGIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_angrot = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'OPTIONS', & ! block
    'ANGROT', & ! tag name
    'ANGROT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_nlay = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'DIMENSIONS', & ! block
    'NLAY', & ! tag name
    'NLAY', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_ncpl = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'DIMENSIONS', & ! block
    'NCPL', & ! tag name
    'NCPL', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_nvert = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'DIMENSIONS', & ! block
    'NVERT', & ! tag name
    'NVERT', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_top = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'GRIDDATA', & ! block
    'TOP', & ! tag name
    'TOP', & ! fortran variable
    'DOUBLE1D', & ! type
    'NCPL', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_botm = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'GRIDDATA', & ! block
    'BOTM', & ! tag name
    'BOTM', & ! fortran variable
    'DOUBLE2D', & ! type
    'NCPL NLAY', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_idomain = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'GRIDDATA', & ! block
    'IDOMAIN', & ! tag name
    'IDOMAIN', & ! fortran variable
    'INTEGER2D', & ! type
    'NCPL NLAY', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_iv = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'VERTICES', & ! block
    'IV', & ! tag name
    'IV', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_xv = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'VERTICES', & ! block
    'XV', & ! tag name
    'XV', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_yv = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'VERTICES', & ! block
    'YV', & ! tag name
    'YV', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_icell2d = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'CELL2D', & ! block
    'ICELL2D', & ! tag name
    'ICELL2D', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_xc = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'CELL2D', & ! block
    'XC', & ! tag name
    'XC', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_yc = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'CELL2D', & ! block
    'YC', & ! tag name
    'YC', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_ncvert = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'CELL2D', & ! block
    'NCVERT', & ! tag name
    'NCVERT', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_icvert = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'CELL2D', & ! block
    'ICVERT', & ! tag name
    'ICVERT', & ! fortran variable
    'INTEGER1D', & ! type
    'NCVERT', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_disv_param_definitions(*) = &
    [ &
    gwfdisv_length_units, &
    gwfdisv_nogrb, &
    gwfdisv_xorigin, &
    gwfdisv_yorigin, &
    gwfdisv_angrot, &
    gwfdisv_nlay, &
    gwfdisv_ncpl, &
    gwfdisv_nvert, &
    gwfdisv_top, &
    gwfdisv_botm, &
    gwfdisv_idomain, &
    gwfdisv_iv, &
    gwfdisv_xv, &
    gwfdisv_yv, &
    gwfdisv_icell2d, &
    gwfdisv_xc, &
    gwfdisv_yc, &
    gwfdisv_ncvert, &
    gwfdisv_icvert &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_vertices = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'VERTICES', & ! block
    'VERTICES', & ! tag name
    'VERTICES', & ! fortran variable
    'RECARRAY IV XV YV', & ! type
    'NVERT', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfdisv_cell2d = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DISV', & ! subcomponent
    'CELL2D', & ! block
    'CELL2D', & ! tag name
    'CELL2D', & ! fortran variable
    'RECARRAY ICELL2D XC YC NCVERT ICVERT', & ! type
    'NCPL', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_disv_aggregate_definitions(*) = &
    [ &
    gwfdisv_vertices, &
    gwfdisv_cell2d &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_disv_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'DIMENSIONS', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'VERTICES', & ! blockname
    .true., & ! required
    .true. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'CELL2D', & ! blockname
    .true., & ! required
    .true. & ! aggregate
    ) &
    ]

end module GwfDisvInputModule

! ** Do Not Modify! MODFLOW 6 system generated file. **
module PrtDisvInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public prt_disv_param_definitions
  public prt_disv_aggregate_definitions
  public prt_disv_block_definitions
  public PrtDisvParamFoundType
  public prt_disv_multi_package

  type PrtDisvParamFoundType
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
  end type PrtDisvParamFoundType

  logical :: prt_disv_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    prtdisv_length_units = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_nogrb = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_xorigin = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_yorigin = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_angrot = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_nlay = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
    'DIMENSIONS', & ! block
    'NLAY', & ! tag name
    'NLAY', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdisv_ncpl = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
    'DIMENSIONS', & ! block
    'NCPL', & ! tag name
    'NCPL', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdisv_nvert = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
    'DIMENSIONS', & ! block
    'NVERT', & ! tag name
    'NVERT', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdisv_top = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
    'GRIDDATA', & ! block
    'TOP', & ! tag name
    'TOP', & ! fortran variable
    'DOUBLE1D', & ! type
    'NCPL', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdisv_botm = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
    'GRIDDATA', & ! block
    'BOTM', & ! tag name
    'BOTM', & ! fortran variable
    'DOUBLE2D', & ! type
    'NCPL NLAY', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdisv_idomain = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
    'GRIDDATA', & ! block
    'IDOMAIN', & ! tag name
    'IDOMAIN', & ! fortran variable
    'INTEGER2D', & ! type
    'NCPL NLAY', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdisv_iv = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_xv = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_yv = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_icell2d = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_xc = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_yc = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_ncvert = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_icvert = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prt_disv_param_definitions(*) = &
    [ &
    prtdisv_length_units, &
    prtdisv_nogrb, &
    prtdisv_xorigin, &
    prtdisv_yorigin, &
    prtdisv_angrot, &
    prtdisv_nlay, &
    prtdisv_ncpl, &
    prtdisv_nvert, &
    prtdisv_top, &
    prtdisv_botm, &
    prtdisv_idomain, &
    prtdisv_iv, &
    prtdisv_xv, &
    prtdisv_yv, &
    prtdisv_icell2d, &
    prtdisv_xc, &
    prtdisv_yc, &
    prtdisv_ncvert, &
    prtdisv_icvert &
    ]

  type(InputParamDefinitionType), parameter :: &
    prtdisv_vertices = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
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
    prtdisv_cell2d = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DISV', & ! subcomponent
    'CELL2D', & ! block
    'CELL2D', & ! tag name
    'CELL2D', & ! fortran variable
    'RECARRAY ICELL2D XC YC NCVERT ICVERT', & ! type
    'NCPL', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prt_disv_aggregate_definitions(*) = &
    [ &
    prtdisv_vertices, &
    prtdisv_cell2d &
    ]

  type(InputBlockDefinitionType), parameter :: &
    prt_disv_block_definitions(*) = &
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

end module PrtDisvInputModule

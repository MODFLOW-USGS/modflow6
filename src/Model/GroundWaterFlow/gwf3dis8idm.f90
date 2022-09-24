module GwfDisInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_dis_param_definitions
  public gwf_dis_aggregate_definitions
  public gwf_dis_block_definitions

  type(InputParamDefinitionType), parameter :: &
    mf6var_length_units = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
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
    mf6var_nogrb = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
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
    mf6var_xorigin = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
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
    mf6var_yorigin = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
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
    mf6var_angrot = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
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
    mf6var_nlay = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
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
    mf6var_nrow = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
    'DIMENSIONS', & ! block
    'NROW', & ! tag name
    'NROW', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_ncol = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
    'DIMENSIONS', & ! block
    'NCOL', & ! tag name
    'NCOL', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_delr = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
    'GRIDDATA', & ! block
    'DELR', & ! tag name
    'DELR', & ! fortran variable
    'DOUBLE1D', & ! type
    'NCOL', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_delc = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
    'GRIDDATA', & ! block
    'DELC', & ! tag name
    'DELC', & ! fortran variable
    'DOUBLE1D', & ! type
    'NROW', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_top = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
    'GRIDDATA', & ! block
    'TOP', & ! tag name
    'TOP', & ! fortran variable
    'DOUBLE2D', & ! type
    'NCOL, NROW', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_botm = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
    'GRIDDATA', & ! block
    'BOTM', & ! tag name
    'BOTM', & ! fortran variable
    'DOUBLE3D', & ! type
    'NCOL, NROW, NLAY', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_idomain = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'DIS', & ! subcomponent
    'GRIDDATA', & ! block
    'IDOMAIN', & ! tag name
    'IDOMAIN', & ! fortran variable
    'INTEGER3D', & ! type
    'NCOL, NROW, NLAY', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_dis_param_definitions(*) = &
    [ &
    mf6var_length_units, &
    mf6var_nogrb, &
    mf6var_xorigin, &
    mf6var_yorigin, &
    mf6var_angrot, &
    mf6var_nlay, &
    mf6var_nrow, &
    mf6var_ncol, &
    mf6var_delr, &
    mf6var_delc, &
    mf6var_top, &
    mf6var_botm, &
    mf6var_idomain &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwf_dis_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType :: &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_dis_block_definitions(*) = &
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
    ) &
    ]

end module GwfDisInputModule

module GwtIcInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwt_ic_param_definitions
  public gwt_ic_aggregate_definitions
  public gwt_ic_block_definitions
  public GwtIcParamFoundType

  type GwtIcParamFoundType
    logical :: print_input = .false.
    logical :: strt = .false.
  end type GwtIcParamFoundType

  type(InputParamDefinitionType), parameter :: &
    gwtic_print_input = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'IC', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_INPUT', & ! tag name
    'PRINT_INPUT', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtic_strt = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'IC', & ! subcomponent
    'GRIDDATA', & ! block
    'STRT', & ! tag name
    'STRT', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwt_ic_param_definitions(*) = &
    [ &
    gwtic_print_input, &
    gwtic_strt &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwt_ic_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType :: &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwt_ic_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ) &
    ]

end module GwtIcInputModule

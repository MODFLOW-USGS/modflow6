module GwfIcInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_ic_param_definitions
  public gwf_ic_aggregate_definitions
  public gwf_ic_block_definitions
  public GwfIcParamFoundType

  type GwfIcParamFoundType
    logical :: print_input = .false.
    logical :: strt = .false.
  end type GwfIcParamFoundType

  type(InputParamDefinitionType), parameter :: &
    gwfic_print_input = InputParamDefinitionType &
    ( &
    'GWF', & ! component
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
    gwfic_strt = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'IC', & ! subcomponent
    'GRIDDATA', & ! block
    'STRT', & ! tag name
    'STRT', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_ic_param_definitions(*) = &
    [ &
    gwfic_print_input, &
    gwfic_strt &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwf_ic_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType :: &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_ic_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .false., & ! required
    .false. & ! aggregate
    ) &
    ]

end module GwfIcInputModule

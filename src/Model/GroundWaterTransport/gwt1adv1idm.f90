module GwtAdvInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwt_adv_param_definitions
  public gwt_adv_aggregate_definitions
  public gwt_adv_block_definitions
  public GwtAdvParamFoundType

  type GwtAdvParamFoundType
    logical :: scheme = .false.
  end type GwtAdvParamFoundType

  type(InputParamDefinitionType), parameter :: &
    gwtadv_scheme = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'ADV', & ! subcomponent
    'OPTIONS', & ! block
    'SCHEME', & ! tag name
    'SCHEME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwt_adv_param_definitions(*) = &
    [ &
    gwtadv_scheme &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwt_adv_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType :: &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwt_adv_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false. & ! aggregate
    ) &
    ]

end module GwtAdvInputModule

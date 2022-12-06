module GwtMstInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwt_mst_param_definitions
  public gwt_mst_aggregate_definitions
  public gwt_mst_block_definitions
  public GwtMstParamFoundType

  type GwtMstParamFoundType
    logical :: ipakcb = .false.
    logical :: decay_order_1 = .false.
    logical :: decay_order_0 = .false.
    logical :: sorption = .false.
    logical :: porosity = .false.
    logical :: decay = .false.
    logical :: decay_sorbed = .false.
    logical :: bulk_density = .false.
    logical :: distcoef = .false.
    logical :: sp2 = .false.
  end type GwtMstParamFoundType

  type(InputParamDefinitionType), parameter :: &
    gwtmst_ipakcb = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MST', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_FLOWS', & ! tag name
    'IPAKCB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmst_decay_order_1 = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MST', & ! subcomponent
    'OPTIONS', & ! block
    'FIRST_ORDER_DECAY', & ! tag name
    'DECAY_ORDER_1', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmst_decay_order_0 = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MST', & ! subcomponent
    'OPTIONS', & ! block
    'ZERO_ORDER_DECAY', & ! tag name
    'DECAY_ORDER_0', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmst_sorption = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MST', & ! subcomponent
    'OPTIONS', & ! block
    'SORPTION', & ! tag name
    'SORPTION', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmst_porosity = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MST', & ! subcomponent
    'GRIDDATA', & ! block
    'POROSITY', & ! tag name
    'POROSITY', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmst_decay = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MST', & ! subcomponent
    'GRIDDATA', & ! block
    'DECAY', & ! tag name
    'DECAY', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmst_decay_sorbed = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MST', & ! subcomponent
    'GRIDDATA', & ! block
    'DECAY_SORBED', & ! tag name
    'DECAY_SORBED', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmst_bulk_density = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MST', & ! subcomponent
    'GRIDDATA', & ! block
    'BULK_DENSITY', & ! tag name
    'BULK_DENSITY', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmst_distcoef = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MST', & ! subcomponent
    'GRIDDATA', & ! block
    'DISTCOEF', & ! tag name
    'DISTCOEF', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmst_sp2 = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MST', & ! subcomponent
    'GRIDDATA', & ! block
    'SP2', & ! tag name
    'SP2', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwt_mst_param_definitions(*) = &
    [ &
    gwtmst_ipakcb, &
    gwtmst_decay_order_1, &
    gwtmst_decay_order_0, &
    gwtmst_sorption, &
    gwtmst_porosity, &
    gwtmst_decay, &
    gwtmst_decay_sorbed, &
    gwtmst_bulk_density, &
    gwtmst_distcoef, &
    gwtmst_sp2 &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwt_mst_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType :: &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwt_mst_block_definitions(*) = &
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

end module GwtMstInputModule

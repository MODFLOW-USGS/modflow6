module GwtFmiInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwt_fmi_param_definitions
  public gwt_fmi_aggregate_definitions
  public gwt_fmi_block_definitions
  public GwtFmiParamFoundType

  type GwtFmiParamFoundType
    logical :: ipakcb = .false.
    logical :: iflowerr = .false.
    logical :: flowtype = .false.
    logical :: filein = .false.
    logical :: fname = .false.
  end type GwtFmiParamFoundType

  type(InputParamDefinitionType), parameter :: &
    gwtfmi_ipakcb = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'FMI', & ! subcomponent
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
    gwtfmi_iflowerr = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'FMI', & ! subcomponent
    'OPTIONS', & ! block
    'FLOW_IMBALANCE_CORRECTION', & ! tag name
    'IFLOWERR', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtfmi_flowtype = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'FMI', & ! subcomponent
    'PACKAGEDATA', & ! block
    'FLOWTYPE', & ! tag name
    'FLOWTYPE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtfmi_filein = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'FMI', & ! subcomponent
    'PACKAGEDATA', & ! block
    'FILEIN', & ! tag name
    'FILEIN', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtfmi_fname = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'FMI', & ! subcomponent
    'PACKAGEDATA', & ! block
    'FNAME', & ! tag name
    'FNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwt_fmi_param_definitions(*) = &
    [ &
    gwtfmi_ipakcb, &
    gwtfmi_iflowerr, &
    gwtfmi_flowtype, &
    gwtfmi_filein, &
    gwtfmi_fname &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwtfmi_packagedata = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'FMI', & ! subcomponent
    'PACKAGEDATA', & ! block
    'PACKAGEDATA', & ! tag name
    'PACKAGEDATA', & ! fortran variable
    'RECARRAY FLOWTYPE FILEIN FNAME', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwt_fmi_aggregate_definitions(*) = &
    [ &
    gwtfmi_packagedata &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwt_fmi_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'PACKAGEDATA', & ! blockname
    .false., & ! required
    .true. & ! aggregate
    ) &
    ]

end module GwtFmiInputModule

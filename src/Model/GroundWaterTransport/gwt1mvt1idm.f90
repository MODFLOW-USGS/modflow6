module GwtMvtInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwt_mvt_param_definitions
  public gwt_mvt_aggregate_definitions
  public gwt_mvt_block_definitions
  public GwtMvtParamFoundType

  type GwtMvtParamFoundType
    logical :: print_input = .false.
    logical :: print_flows = .false.
    logical :: ipakcb = .false.
    logical :: budget_filerecord = .false.
    logical :: budget = .false.
    logical :: fileout = .false.
    logical :: budgetfile = .false.
    logical :: budgetcsv_filerecord = .false.
    logical :: budgetcsv = .false.
    logical :: budgetcsvfile = .false.
  end type GwtMvtParamFoundType

  type(InputParamDefinitionType), parameter :: &
    gwtmvt_print_input = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MVT', & ! subcomponent
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
    gwtmvt_print_flows = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MVT', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_FLOWS', & ! tag name
    'PRINT_FLOWS', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmvt_ipakcb = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MVT', & ! subcomponent
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
    gwtmvt_budget_filerecord = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MVT', & ! subcomponent
    'OPTIONS', & ! block
    'BUDGET_FILERECORD', & ! tag name
    'BUDGET_FILERECORD', & ! fortran variable
    'RECORD BUDGET FILEOUT BUDGETFILE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmvt_budget = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MVT', & ! subcomponent
    'OPTIONS', & ! block
    'BUDGET', & ! tag name
    'BUDGET', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmvt_fileout = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MVT', & ! subcomponent
    'OPTIONS', & ! block
    'FILEOUT', & ! tag name
    'FILEOUT', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmvt_budgetfile = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MVT', & ! subcomponent
    'OPTIONS', & ! block
    'BUDGETFILE', & ! tag name
    'BUDGETFILE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmvt_budgetcsv_filerecord = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MVT', & ! subcomponent
    'OPTIONS', & ! block
    'BUDGETCSV_FILERECORD', & ! tag name
    'BUDGETCSV_FILERECORD', & ! fortran variable
    'RECORD BUDGETCSV FILEOUT BUDGETCSVFILE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmvt_budgetcsv = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MVT', & ! subcomponent
    'OPTIONS', & ! block
    'BUDGETCSV', & ! tag name
    'BUDGETCSV', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwtmvt_budgetcsvfile = InputParamDefinitionType &
    ( &
    'GWT', & ! component
    'MVT', & ! subcomponent
    'OPTIONS', & ! block
    'BUDGETCSVFILE', & ! tag name
    'BUDGETCSVFILE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwt_mvt_param_definitions(*) = &
    [ &
    gwtmvt_print_input, &
    gwtmvt_print_flows, &
    gwtmvt_ipakcb, &
    gwtmvt_budget_filerecord, &
    gwtmvt_budget, &
    gwtmvt_fileout, &
    gwtmvt_budgetfile, &
    gwtmvt_budgetcsv_filerecord, &
    gwtmvt_budgetcsv, &
    gwtmvt_budgetcsvfile &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwt_mvt_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType :: &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwt_mvt_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ) &
    ]

end module GwtMvtInputModule

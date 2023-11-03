! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwtIcInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwt_ic_param_definitions
  public gwt_ic_aggregate_definitions
  public gwt_ic_block_definitions
  public GwtIcParamFoundType
  public gwt_ic_multi_package

  type GwtIcParamFoundType
    logical :: strt = .false.
  end type GwtIcParamFoundType

  logical :: gwt_ic_multi_package = .false.

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
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwt_ic_param_definitions(*) = &
    [ &
    gwtic_strt &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwt_ic_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType &
    ( &
    '', & ! component
    '', & ! subcomponent
    '', & ! block
    '', & ! tag name
    '', & ! fortran variable
    '', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    ) &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwt_ic_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module GwtIcInputModule

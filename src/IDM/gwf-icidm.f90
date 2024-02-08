! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwfIcInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_ic_param_definitions
  public gwf_ic_aggregate_definitions
  public gwf_ic_block_definitions
  public GwfIcParamFoundType
  public gwf_ic_multi_package

  type GwfIcParamFoundType
    logical :: strt = .false.
  end type GwfIcParamFoundType

  logical :: gwf_ic_multi_package = .false.

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
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_ic_param_definitions(*) = &
    [ &
    gwfic_strt &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwf_ic_aggregate_definitions(*) = &
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
    gwf_ic_block_definitions(*) = &
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

end module GwfIcInputModule

! ** Do Not Modify! MODFLOW 6 system generated file. **
module ExgGwfswfInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public exg_gwfswf_param_definitions
  public exg_gwfswf_aggregate_definitions
  public exg_gwfswf_block_definitions
  public ExgGwfswfParamFoundType
  public exg_gwfswf_multi_package

  type ExgGwfswfParamFoundType
    logical :: ipr_input = .false.
    logical :: ipr_flow = .false.
    logical :: obs_filerecord = .false.
    logical :: obs6 = .false.
    logical :: filein = .false.
    logical :: obs6_filename = .false.
    logical :: nexg = .false.
    logical :: cellidm1 = .false.
    logical :: cellidm2 = .false.
    logical :: cond = .false.
  end type ExgGwfswfParamFoundType

  logical :: exg_gwfswf_multi_package = .true.

  type(InputParamDefinitionType), parameter :: &
    exggwfswf_ipr_input = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFSWF', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_INPUT', & ! tag name
    'IPR_INPUT', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfswf_ipr_flow = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFSWF', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_FLOWS', & ! tag name
    'IPR_FLOW', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfswf_obs_filerecord = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFSWF', & ! subcomponent
    'OPTIONS', & ! block
    'OBS_FILERECORD', & ! tag name
    'OBS_FILERECORD', & ! fortran variable
    'RECORD OBS6 FILEIN OBS6_FILENAME', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfswf_obs6 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFSWF', & ! subcomponent
    'OPTIONS', & ! block
    'OBS6', & ! tag name
    'OBS6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfswf_filein = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFSWF', & ! subcomponent
    'OPTIONS', & ! block
    'FILEIN', & ! tag name
    'FILEIN', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfswf_obs6_filename = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFSWF', & ! subcomponent
    'OPTIONS', & ! block
    'OBS6_FILENAME', & ! tag name
    'OBS6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfswf_nexg = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFSWF', & ! subcomponent
    'DIMENSIONS', & ! block
    'NEXG', & ! tag name
    'NEXG', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfswf_cellidm1 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFSWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'CELLIDM1', & ! tag name
    'CELLIDM1', & ! fortran variable
    'INTEGER1D', & ! type
    'NCELLDIM', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfswf_cellidm2 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFSWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'CELLIDM2', & ! tag name
    'CELLIDM2', & ! fortran variable
    'INTEGER1D', & ! type
    'NCELLDIM', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exggwfswf_cond = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFSWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'COND', & ! tag name
    'COND', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exg_gwfswf_param_definitions(*) = &
    [ &
    exggwfswf_ipr_input, &
    exggwfswf_ipr_flow, &
    exggwfswf_obs_filerecord, &
    exggwfswf_obs6, &
    exggwfswf_filein, &
    exggwfswf_obs6_filename, &
    exggwfswf_nexg, &
    exggwfswf_cellidm1, &
    exggwfswf_cellidm2, &
    exggwfswf_cond &
    ]

  type(InputParamDefinitionType), parameter :: &
    exggwfswf_exchangedata = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'GWFSWF', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'EXCHANGEDATA', & ! tag name
    'EXCHANGEDATA', & ! fortran variable
    'RECARRAY CELLIDM1 CELLIDM2 COND', & ! type
    'NEXG', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exg_gwfswf_aggregate_definitions(*) = &
    [ &
    exggwfswf_exchangedata &
    ]

  type(InputBlockDefinitionType), parameter :: &
    exg_gwfswf_block_definitions(*) = &
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
    'EXCHANGEDATA', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module ExgGwfswfInputModule

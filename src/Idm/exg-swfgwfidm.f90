! ** Do Not Modify! MODFLOW 6 system generated file. **
module ExgSwfgwfInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public exg_swfgwf_param_definitions
  public exg_swfgwf_aggregate_definitions
  public exg_swfgwf_block_definitions
  public ExgSwfgwfParamFoundType
  public exg_swfgwf_multi_package
  public exg_swfgwf_subpackages

  type ExgSwfgwfParamFoundType
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
  end type ExgSwfgwfParamFoundType

  logical :: exg_swfgwf_multi_package = .true.

  character(len=16), parameter :: &
    exg_swfgwf_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    exgswfgwf_ipr_input = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWFGWF', & ! subcomponent
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
    exgswfgwf_ipr_flow = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWFGWF', & ! subcomponent
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
    exgswfgwf_obs_filerecord = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWFGWF', & ! subcomponent
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
    exgswfgwf_obs6 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWFGWF', & ! subcomponent
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
    exgswfgwf_filein = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWFGWF', & ! subcomponent
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
    exgswfgwf_obs6_filename = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWFGWF', & ! subcomponent
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
    exgswfgwf_nexg = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWFGWF', & ! subcomponent
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
    exgswfgwf_cellidm1 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWFGWF', & ! subcomponent
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
    exgswfgwf_cellidm2 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWFGWF', & ! subcomponent
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
    exgswfgwf_cond = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWFGWF', & ! subcomponent
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
    exg_swfgwf_param_definitions(*) = &
    [ &
    exgswfgwf_ipr_input, &
    exgswfgwf_ipr_flow, &
    exgswfgwf_obs_filerecord, &
    exgswfgwf_obs6, &
    exgswfgwf_filein, &
    exgswfgwf_obs6_filename, &
    exgswfgwf_nexg, &
    exgswfgwf_cellidm1, &
    exgswfgwf_cellidm2, &
    exgswfgwf_cond &
    ]

  type(InputParamDefinitionType), parameter :: &
    exgswfgwf_exchangedata = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWFGWF', & ! subcomponent
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
    exg_swfgwf_aggregate_definitions(*) = &
    [ &
    exgswfgwf_exchangedata &
    ]

  type(InputBlockDefinitionType), parameter :: &
    exg_swfgwf_block_definitions(*) = &
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

end module ExgSwfgwfInputModule

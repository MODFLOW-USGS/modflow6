! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwfRchaInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_rcha_param_definitions
  public gwf_rcha_aggregate_definitions
  public gwf_rcha_block_definitions
  public GwfRchaParamFoundType
  public gwf_rcha_multi_package

  type GwfRchaParamFoundType
    logical :: readasarrays = .false.
    logical :: fixed_cell = .false.
    logical :: auxiliary = .false.
    logical :: auxmultname = .false.
    logical :: iprpak = .false.
    logical :: iprflow = .false.
    logical :: ipakcb = .false.
    logical :: tas_filerecord = .false.
    logical :: tas6 = .false.
    logical :: filein = .false.
    logical :: tas6_filename = .false.
    logical :: obs_filerecord = .false.
    logical :: obs6 = .false.
    logical :: obs6_filename = .false.
    logical :: irch = .false.
    logical :: recharge = .false.
    logical :: auxvar = .false.
  end type GwfRchaParamFoundType

  logical :: gwf_rcha_multi_package = .true.

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_readasarrays = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'OPTIONS', & ! block
    'READASARRAYS', & ! tag name
    'READASARRAYS', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_fixed_cell = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'OPTIONS', & ! block
    'FIXED_CELL', & ! tag name
    'FIXED_CELL', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_auxiliary = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'OPTIONS', & ! block
    'AUXILIARY', & ! tag name
    'AUXILIARY', & ! fortran variable
    'STRING', & ! type
    'NAUX', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_auxmultname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'OPTIONS', & ! block
    'AUXMULTNAME', & ! tag name
    'AUXMULTNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_iprpak = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_INPUT', & ! tag name
    'IPRPAK', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_iprflow = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_FLOWS', & ! tag name
    'IPRFLOW', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_ipakcb = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_FLOWS', & ! tag name
    'IPAKCB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_tas_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'OPTIONS', & ! block
    'TAS_FILERECORD', & ! tag name
    'TAS_FILERECORD', & ! fortran variable
    'RECORD TAS6 FILEIN TAS6_FILENAME', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_tas6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'OPTIONS', & ! block
    'TAS6', & ! tag name
    'TAS6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_filein = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
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
    gwfrcha_tas6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'OPTIONS', & ! block
    'TAS6_FILENAME', & ! tag name
    'TAS6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_obs_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
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
    gwfrcha_obs6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
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
    gwfrcha_obs6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
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
    gwfrcha_irch = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'PERIOD', & ! block
    'IRCH', & ! tag name
    'IRCH', & ! fortran variable
    'INTEGER1D', & ! type
    'NCPL', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_recharge = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'PERIOD', & ! block
    'RECHARGE', & ! tag name
    'RECHARGE', & ! fortran variable
    'DOUBLE1D', & ! type
    'NCPL', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfrcha_auxvar = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'RCHA', & ! subcomponent
    'PERIOD', & ! block
    'AUX', & ! tag name
    'AUXVAR', & ! fortran variable
    'DOUBLE2D', & ! type
    'NAUX NCPL', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_rcha_param_definitions(*) = &
    [ &
    gwfrcha_readasarrays, &
    gwfrcha_fixed_cell, &
    gwfrcha_auxiliary, &
    gwfrcha_auxmultname, &
    gwfrcha_iprpak, &
    gwfrcha_iprflow, &
    gwfrcha_ipakcb, &
    gwfrcha_tas_filerecord, &
    gwfrcha_tas6, &
    gwfrcha_filein, &
    gwfrcha_tas6_filename, &
    gwfrcha_obs_filerecord, &
    gwfrcha_obs6, &
    gwfrcha_obs6_filename, &
    gwfrcha_irch, &
    gwfrcha_recharge, &
    gwfrcha_auxvar &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwf_rcha_aggregate_definitions(*) = &
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
    gwf_rcha_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'PERIOD', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .true. & ! block_variable
    ) &
    ]

end module GwfRchaInputModule

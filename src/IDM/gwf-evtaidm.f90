! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwfEvtaInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_evta_param_definitions
  public gwf_evta_aggregate_definitions
  public gwf_evta_block_definitions
  public GwfEvtaParamFoundType
  public gwf_evta_multi_package

  type GwfEvtaParamFoundType
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
    logical :: ievt = .false.
    logical :: surface = .false.
    logical :: rate = .false.
    logical :: depth = .false.
    logical :: auxvar = .false.
  end type GwfEvtaParamFoundType

  logical :: gwf_evta_multi_package = .true.

  type(InputParamDefinitionType), parameter :: &
    gwfevta_readasarrays = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_fixed_cell = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_auxiliary = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_auxmultname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_iprpak = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_iprflow = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_ipakcb = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_tas_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_tas6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_filein = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_tas6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_obs_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_obs6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_obs6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
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
    gwfevta_ievt = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
    'PERIOD', & ! block
    'IEVT', & ! tag name
    'IEVT', & ! fortran variable
    'INTEGER1D', & ! type
    'NCPL', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfevta_surface = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
    'PERIOD', & ! block
    'SURFACE', & ! tag name
    'SURFACE', & ! fortran variable
    'DOUBLE1D', & ! type
    'NCPL', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfevta_rate = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
    'PERIOD', & ! block
    'RATE', & ! tag name
    'RATE', & ! fortran variable
    'DOUBLE1D', & ! type
    'NCPL', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfevta_depth = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
    'PERIOD', & ! block
    'DEPTH', & ! tag name
    'DEPTH', & ! fortran variable
    'DOUBLE1D', & ! type
    'NCPL', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfevta_auxvar = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'EVTA', & ! subcomponent
    'PERIOD', & ! block
    'AUX', & ! tag name
    'AUXVAR', & ! fortran variable
    'DOUBLE2D', & ! type
    'NAUX NCPL', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_evta_param_definitions(*) = &
    [ &
    gwfevta_readasarrays, &
    gwfevta_fixed_cell, &
    gwfevta_auxiliary, &
    gwfevta_auxmultname, &
    gwfevta_iprpak, &
    gwfevta_iprflow, &
    gwfevta_ipakcb, &
    gwfevta_tas_filerecord, &
    gwfevta_tas6, &
    gwfevta_filein, &
    gwfevta_tas6_filename, &
    gwfevta_obs_filerecord, &
    gwfevta_obs6, &
    gwfevta_obs6_filename, &
    gwfevta_ievt, &
    gwfevta_surface, &
    gwfevta_rate, &
    gwfevta_depth, &
    gwfevta_auxvar &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwf_evta_aggregate_definitions(*) = &
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
    gwf_evta_block_definitions(*) = &
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

end module GwfEvtaInputModule

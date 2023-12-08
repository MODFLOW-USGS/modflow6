! ** Do Not Modify! MODFLOW 6 system generated file. **
module GweCntInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwe_cnt_param_definitions
  public gwe_cnt_aggregate_definitions
  public gwe_cnt_block_definitions
  public GweCntParamFoundType
  public gwe_cnt_multi_package

  type GweCntParamFoundType
    logical :: auxiliary = .false.
    logical :: auxmultname = .false.
    logical :: boundnames = .false.
    logical :: iprflow = .false.
    logical :: ipakcb = .false.
    logical :: iprpak = .false.
    logical :: ts_filerecord = .false.
    logical :: ts6 = .false.
    logical :: filein = .false.
    logical :: ts6_filename = .false.
    logical :: obs_filerecord = .false.
    logical :: obs6 = .false.
    logical :: obs6_filename = .false.
    logical :: maxbound = .false.
    logical :: cellid = .false.
    logical :: tspvar = .false.
    logical :: auxvar = .false.
    logical :: boundname = .false.
  end type GweCntParamFoundType

  logical :: gwe_cnt_multi_package = .true.

  type(InputParamDefinitionType), parameter :: &
    gwecnt_auxiliary = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
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
    gwecnt_auxmultname = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
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
    gwecnt_boundnames = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'OPTIONS', & ! block
    'BOUNDNAMES', & ! tag name
    'BOUNDNAMES', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwecnt_iprflow = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_INPUT', & ! tag name
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
    gwecnt_ipakcb = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_FLOWS', & ! tag name
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
    gwecnt_iprpak = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_FLOWS', & ! tag name
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
    gwecnt_ts_filerecord = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'OPTIONS', & ! block
    'TS_FILERECORD', & ! tag name
    'TS_FILERECORD', & ! fortran variable
    'RECORD TS6 FILEIN TS6_FILENAME', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwecnt_ts6 = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'OPTIONS', & ! block
    'TS6', & ! tag name
    'TS6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwecnt_filein = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
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
    gwecnt_ts6_filename = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'OPTIONS', & ! block
    'TS6_FILENAME', & ! tag name
    'TS6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwecnt_obs_filerecord = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
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
    gwecnt_obs6 = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
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
    gwecnt_obs6_filename = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
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
    gwecnt_maxbound = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'DIMENSIONS', & ! block
    'MAXBOUND', & ! tag name
    'MAXBOUND', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwecnt_cellid = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'PERIOD', & ! block
    'CELLID', & ! tag name
    'CELLID', & ! fortran variable
    'INTEGER1D', & ! type
    'NCELLDIM', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwecnt_tspvar = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'PERIOD', & ! block
    'TEMP', & ! tag name
    'TSPVAR', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwecnt_auxvar = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'PERIOD', & ! block
    'AUX', & ! tag name
    'AUXVAR', & ! fortran variable
    'DOUBLE1D', & ! type
    'NAUX', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwecnt_boundname = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'PERIOD', & ! block
    'BOUNDNAME', & ! tag name
    'BOUNDNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwe_cnt_param_definitions(*) = &
    [ &
    gwecnt_auxiliary, &
    gwecnt_auxmultname, &
    gwecnt_boundnames, &
    gwecnt_iprflow, &
    gwecnt_ipakcb, &
    gwecnt_iprpak, &
    gwecnt_ts_filerecord, &
    gwecnt_ts6, &
    gwecnt_filein, &
    gwecnt_ts6_filename, &
    gwecnt_obs_filerecord, &
    gwecnt_obs6, &
    gwecnt_obs6_filename, &
    gwecnt_maxbound, &
    gwecnt_cellid, &
    gwecnt_tspvar, &
    gwecnt_auxvar, &
    gwecnt_boundname &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwecnt_spd = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'CNT', & ! subcomponent
    'PERIOD', & ! block
    'STRESS_PERIOD_DATA', & ! tag name
    'SPD', & ! fortran variable
    'RECARRAY CELLID TEMP AUX BOUNDNAME', & ! type
    'MAXBOUND', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwe_cnt_aggregate_definitions(*) = &
    [ &
    gwecnt_spd &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwe_cnt_block_definitions(*) = &
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
    'PERIOD', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .true. & ! block_variable
    ) &
    ]

end module GweCntInputModule

! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwfWelInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_wel_param_definitions
  public gwf_wel_aggregate_definitions
  public gwf_wel_block_definitions
  public GwfWelParamFoundType
  public gwf_wel_multi_package

  type GwfWelParamFoundType
    logical :: auxiliary = .false.
    logical :: auxmultname = .false.
    logical :: boundnames = .false.
    logical :: iprpak = .false.
    logical :: iprflow = .false.
    logical :: ipakcb = .false.
    logical :: flowred = .false.
    logical :: afrcsv_rec = .false.
    logical :: afrcsv = .false.
    logical :: fileout = .false.
    logical :: afrcsvfile = .false.
    logical :: ts_filerecord = .false.
    logical :: ts6 = .false.
    logical :: filein = .false.
    logical :: ts6_filename = .false.
    logical :: obs_filerecord = .false.
    logical :: obs6 = .false.
    logical :: obs6_filename = .false.
    logical :: mover = .false.
    logical :: maxbound = .false.
    logical :: cellid = .false.
    logical :: q = .false.
    logical :: auxvar = .false.
    logical :: boundname = .false.
  end type GwfWelParamFoundType

  logical :: gwf_wel_multi_package = .true.

  type(InputParamDefinitionType), parameter :: &
    gwfwel_auxiliary = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_auxmultname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_boundnames = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_iprpak = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_iprflow = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_ipakcb = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_flowred = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
    'OPTIONS', & ! block
    'AUTO_FLOW_REDUCE', & ! tag name
    'FLOWRED', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfwel_afrcsv_rec = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
    'OPTIONS', & ! block
    'AFRCSV_FILERECORD', & ! tag name
    'AFRCSV_REC', & ! fortran variable
    'RECORD AUTO_FLOW_REDUCE_CSV FILEOUT AFRCSVFILE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfwel_afrcsv = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
    'OPTIONS', & ! block
    'AUTO_FLOW_REDUCE_CSV', & ! tag name
    'AFRCSV', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfwel_fileout = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
    'OPTIONS', & ! block
    'FILEOUT', & ! tag name
    'FILEOUT', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfwel_afrcsvfile = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
    'OPTIONS', & ! block
    'AFRCSVFILE', & ! tag name
    'AFRCSVFILE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfwel_ts_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_ts6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_filein = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_ts6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_obs_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_obs6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_obs6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_mover = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
    'OPTIONS', & ! block
    'MOVER', & ! tag name
    'MOVER', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfwel_maxbound = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_cellid = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_q = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
    'PERIOD', & ! block
    'Q', & ! tag name
    'Q', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfwel_auxvar = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwfwel_boundname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
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
    gwf_wel_param_definitions(*) = &
    [ &
    gwfwel_auxiliary, &
    gwfwel_auxmultname, &
    gwfwel_boundnames, &
    gwfwel_iprpak, &
    gwfwel_iprflow, &
    gwfwel_ipakcb, &
    gwfwel_flowred, &
    gwfwel_afrcsv_rec, &
    gwfwel_afrcsv, &
    gwfwel_fileout, &
    gwfwel_afrcsvfile, &
    gwfwel_ts_filerecord, &
    gwfwel_ts6, &
    gwfwel_filein, &
    gwfwel_ts6_filename, &
    gwfwel_obs_filerecord, &
    gwfwel_obs6, &
    gwfwel_obs6_filename, &
    gwfwel_mover, &
    gwfwel_maxbound, &
    gwfwel_cellid, &
    gwfwel_q, &
    gwfwel_auxvar, &
    gwfwel_boundname &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwfwel_spd = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'WEL', & ! subcomponent
    'PERIOD', & ! block
    'STRESS_PERIOD_DATA', & ! tag name
    'SPD', & ! fortran variable
    'RECARRAY CELLID Q AUX BOUNDNAME', & ! type
    'MAXBOUND', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_wel_aggregate_definitions(*) = &
    [ &
    gwfwel_spd &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_wel_block_definitions(*) = &
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

end module GwfWelInputModule

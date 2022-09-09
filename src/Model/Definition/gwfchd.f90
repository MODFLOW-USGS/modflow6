module GwfChdInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, InputBlockDefinitionType
  private
  public gwf_chd_param_definitions
  public gwf_chd_aggregate_definitions
  public gwf_chd_block_definitions
  type(InputParamDefinitionType), parameter :: gwf_chd_param_definitions(*) = &
  [ &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'AUXILIARY', &   ! tag name
      'AUXILIARY', &   ! fortran variable
      'STRING', &   ! type
      'NAUX', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'AUXMULTNAME', &   ! tag name
      'AUXMULTNAME', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'BOUNDNAMES', &   ! tag name
      'BOUNDNAMES', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'PRINT_INPUT', &   ! tag name
      'PRINT_INPUT', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'PRINT_FLOWS', &   ! tag name
      'PRINT_FLOWS', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'SAVE_FLOWS', &   ! tag name
      'SAVE_FLOWS', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'TS_FILERECORD', &   ! tag name
      'TS_FILERECORD', &   ! fortran variable
      'RECORD TS6 FILEIN TS6_FILENAME', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'TS6', &   ! tag name
      'TS6', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'FILEIN', &   ! tag name
      'FILEIN', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'TS6_FILENAME', &   ! tag name
      'TS6_FILENAME', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .true. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'OBS_FILERECORD', &   ! tag name
      'OBS_FILERECORD', &   ! fortran variable
      'RECORD OBS6 FILEIN OBS6_FILENAME', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'OBS6', &   ! tag name
      'OBS6', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'OPTIONS', &   ! block
      'OBS6_FILENAME', &   ! tag name
      'OBS6_FILENAME', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .true. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'DIMENSIONS', &   ! block
      'MAXBOUND', &   ! tag name
      'MAXBOUND', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'PERIOD', &   ! block
      'CELLID', &   ! tag name
      'CELLID', &   ! fortran variable
      'INTEGER1D', &   ! type
      'NCELLDIM', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'PERIOD', &   ! block
      'HEAD', &   ! tag name
      'HEAD', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'PERIOD', &   ! block
      'AUX', &   ! tag name
      'AUX', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NAUX', &   ! shape
      .false., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'PERIOD', &   ! block
      'BOUNDNAME', &   ! tag name
      'BOUNDNAME', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ) &
  ]

  type(InputParamDefinitionType), parameter :: gwf_chd_aggregate_definitions(*) = &
  [ &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'CHD', &   ! subcomponent
      'PERIOD', &   ! block
      'STRESS_PERIOD_DATA', &   ! tag name
      'IPER', &   ! fortran variable
      'RECARRAY CELLID HEAD AUX BOUNDNAME', &   ! type
      'MAXBOUND', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ) &
  ]

  type(InputBlockDefinitionType), parameter :: gwf_chd_block_definitions(*) = &
  [ &
    InputBlockDefinitionType( &
      'OPTIONS', & ! blockname
      .true., & ! required
      .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
      'DIMENSIONS', & ! blockname
      .true., & ! required
      .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
      'PERIOD', & ! blockname
      .true., & ! required
      .true. & ! aggregate
    ) &
  ]

end module GwfChdInputModule

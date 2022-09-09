module SimNamInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, InputBlockDefinitionType
  private
  public sim_nam_param_definitions
  public sim_nam_aggregate_definitions
  public sim_nam_block_definitions
  type(InputParamDefinitionType), parameter :: sim_nam_param_definitions(*) = &
  [ &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'OPTIONS', &   ! block
      'CONTINUE', &   ! tag name
      'CONTINUE', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'OPTIONS', &   ! block
      'NOCHECK', &   ! tag name
      'NOCHECK', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'OPTIONS', &   ! block
      'MEMORY_PRINT_OPTION', &   ! tag name
      'IPRMEM', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'OPTIONS', &   ! block
      'MAXERRORS', &   ! tag name
      'MAXERRORS', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'TIMING', &   ! block
      'TDIS6', &   ! tag name
      'TDIS6', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .true. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'MODELS', &   ! block
      'MTYPE', &   ! tag name
      'MTYPE', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'MODELS', &   ! block
      'MFNAME', &   ! tag name
      'MFNAME', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .true. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'MODELS', &   ! block
      'MNAME', &   ! tag name
      'MNAME', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'EXCHANGES', &   ! block
      'EXGTYPE', &   ! tag name
      'EXGTYPE', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'EXCHANGES', &   ! block
      'EXGFILE', &   ! tag name
      'EXGFILE', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .true. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'EXCHANGES', &   ! block
      'EXGMNAMEA', &   ! tag name
      'EXGMNAMEA', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'EXCHANGES', &   ! block
      'EXGMNAMEB', &   ! tag name
      'EXGMNAMEB', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'SOLUTIONGROUP', &   ! block
      'MXITER', &   ! tag name
      'MXITER', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'SOLUTIONGROUP', &   ! block
      'SLNTYPE', &   ! tag name
      'SLNTYPE', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'SOLUTIONGROUP', &   ! block
      'SLNFNAME', &   ! tag name
      'SLNFNAME', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .true. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'SOLUTIONGROUP', &   ! block
      'SLNMNAMES', &   ! tag name
      'SLNMNAMES', &   ! fortran variable
      'STRING', &   ! type
      ':', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ) &
  ]

  type(InputParamDefinitionType), parameter :: sim_nam_aggregate_definitions(*) = &
  [ &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'MODELS', &   ! block
      'MODELS', &   ! tag name
      'MODELS', &   ! fortran variable
      'RECARRAY MTYPE MFNAME MNAME', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'EXCHANGES', &   ! block
      'EXCHANGES', &   ! tag name
      'EXCHANGES', &   ! fortran variable
      'RECARRAY EXGTYPE EXGFILE EXGMNAMEA EXGMNAMEB', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SIM', &   ! component
      'NAM', &   ! subcomponent
      'SOLUTIONGROUP', &   ! block
      'SOLUTIONGROUP', &   ! tag name
      'SOLUTIONGROUP', &   ! fortran variable
      'RECARRAY SLNTYPE SLNFNAME SLNMNAMES', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ) &
  ]

  type(InputBlockDefinitionType), parameter :: sim_nam_block_definitions(*) = &
  [ &
    InputBlockDefinitionType( &
      'OPTIONS', & ! blockname
      .false., & ! required
      .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
      'TIMING', & ! blockname
      .true., & ! required
      .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
      'MODELS', & ! blockname
      .true., & ! required
      .true. & ! aggregate
    ), &
    InputBlockDefinitionType( &
      'EXCHANGES', & ! blockname
      .true., & ! required
      .true. & ! aggregate
    ), &
    InputBlockDefinitionType( &
      'SOLUTIONGROUP', & ! blockname
      .true., & ! required
      .true. & ! aggregate
    ) &
  ]

end module SimNamInputModule

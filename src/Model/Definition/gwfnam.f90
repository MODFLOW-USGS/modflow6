module GwfNamInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, InputBlockDefinitionType
  private
  public gwf_nam_param_definitions
  public gwf_nam_aggregate_definitions
  public gwf_nam_block_definitions
  type(InputParamDefinitionType), parameter :: gwf_nam_param_definitions(*) = &
  [ &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'NAM', &   ! subcomponent
      'OPTIONS', &   ! block
      'LIST', &   ! tag name
      'LIST', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'NAM', &   ! subcomponent
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
      'NAM', &   ! subcomponent
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
      'NAM', &   ! subcomponent
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
      'NAM', &   ! subcomponent
      'OPTIONS', &   ! block
      'NEWTONOPTIONS', &   ! tag name
      'NEWTONOPTIONS', &   ! fortran variable
      'RECORD NEWTON UNDER_RELAXATION', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'NAM', &   ! subcomponent
      'OPTIONS', &   ! block
      'NEWTON', &   ! tag name
      'NEWTON', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'NAM', &   ! subcomponent
      'OPTIONS', &   ! block
      'UNDER_RELAXATION', &   ! tag name
      'UNDER_RELAXATION', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'NAM', &   ! subcomponent
      'PACKAGES', &   ! block
      'FTYPE', &   ! tag name
      'FTYPE', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'NAM', &   ! subcomponent
      'PACKAGES', &   ! block
      'FNAME', &   ! tag name
      'FNAME', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .true. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'NAM', &   ! subcomponent
      'PACKAGES', &   ! block
      'PNAME', &   ! tag name
      'PNAME', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ) &
  ]

  type(InputParamDefinitionType), parameter :: gwf_nam_aggregate_definitions(*) = &
  [ &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'NAM', &   ! subcomponent
      'PACKAGES', &   ! block
      'PACKAGES', &   ! tag name
      'PACKAGES', &   ! fortran variable
      'RECARRAY FTYPE FNAME PNAME', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ) &
  ]

  type(InputBlockDefinitionType), parameter :: gwf_nam_block_definitions(*) = &
  [ &
    InputBlockDefinitionType( &
      'OPTIONS', & ! blockname
      .true., & ! required
      .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
      'PACKAGES', & ! blockname
      .true., & ! required
      .true. & ! aggregate
    ) &
  ]

end module GwfNamInputModule

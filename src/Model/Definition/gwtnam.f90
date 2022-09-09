module GwtNamInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, InputBlockDefinitionType
  private
  public gwt_nam_param_definitions
  public gwt_nam_aggregate_definitions
  public gwt_nam_block_definitions
  type(InputParamDefinitionType), parameter :: gwt_nam_param_definitions(*) = &
  [ &
    InputParamDefinitionType( &
      'GWT', &   ! component
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
      'GWT', &   ! component
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
      'GWT', &   ! component
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
      'GWT', &   ! component
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
      'GWT', &   ! component
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
      'GWT', &   ! component
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
      'GWT', &   ! component
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

  type(InputParamDefinitionType), parameter :: gwt_nam_aggregate_definitions(*) = &
  [ &
    InputParamDefinitionType( &
      'GWT', &   ! component
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

  type(InputBlockDefinitionType), parameter :: gwt_nam_block_definitions(*) = &
  [ &
    InputBlockDefinitionType( &
      'OPTIONS', & ! blockname
      .false., & ! required
      .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
      'PACKAGES', & ! blockname
      .true., & ! required
      .true. & ! aggregate
    ) &
  ]

end module GwtNamInputModule

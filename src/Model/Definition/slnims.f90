module SlnImsInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, InputBlockDefinitionType
  private
  public sln_ims_param_definitions
  public sln_ims_aggregate_definitions
  public sln_ims_block_definitions
  type(InputParamDefinitionType), parameter :: sln_ims_param_definitions(*) = &
  [ &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'PRINT_OPTION', &   ! tag name
      'PRINT_OPTION', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'COMPLEXITY', &   ! tag name
      'COMPLEXITY', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'CSV_OUTPUT_FILERECORD', &   ! tag name
      'CSV_OUTREC', &   ! fortran variable
      'RECORD CSV_OUTPUT FILEOUT CSVFILE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'CSV_OUTPUT', &   ! tag name
      'CSV_OUTPUT', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'CSVFILE', &   ! tag name
      'CSVFILE', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .true. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'CSV_OUTER_OUTPUT_FILERECORD', &   ! tag name
      'CSV_OUTER_OUTREC', &   ! fortran variable
      'RECORD CSV_OUTER_OUTPUT FILEOUT OUTER_CSVFILE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'CSV_OUTER_OUTPUT', &   ! tag name
      'CSV_OUTER_OUTPUT', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'FILEOUT', &   ! tag name
      'FILEOUT', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'OUTER_CSVFILE', &   ! tag name
      'OUTER_CSVFILE', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .true. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'CSV_INNER_OUTPUT_FILERECORD', &   ! tag name
      'CSV_INNER_OUTREC', &   ! fortran variable
      'RECORD CSV_INNER_OUTPUT FILEOUT INNER_CSVFILE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'CSV_INNER_OUTPUT', &   ! tag name
      'CSV_INNER_OUTPUT', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'INNER_CSVFILE', &   ! tag name
      'INNER_CSVFILE', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .true. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'NO_PTCRECORD', &   ! tag name
      'NO_PTCRECORD', &   ! fortran variable
      'RECORD NO_PTC NO_PTC_OPTION', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'NO_PTC', &   ! tag name
      'NO_PTC', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'NO_PTC_OPTION', &   ! tag name
      'NO_PTC_OPTION', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'OPTIONS', &   ! block
      'ATS_OUTER_MAXIMUM_FRACTION', &   ! tag name
      'ATSFRAC', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'OUTER_HCLOSE', &   ! tag name
      'OUTER_HCLOSE', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'OUTER_DVCLOSE', &   ! tag name
      'OUTER_DVCLOSE', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'OUTER_RCLOSEBND', &   ! tag name
      'OUTER_RCLOSEBND', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'OUTER_MAXIMUM', &   ! tag name
      'OUTER_MAXIMUM', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'UNDER_RELAXATION', &   ! tag name
      'UNDER_RELAXATION', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'UNDER_RELAXATION_GAMMA', &   ! tag name
      'GAMMA', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'UNDER_RELAXATION_THETA', &   ! tag name
      'THETA', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'UNDER_RELAXATION_KAPPA', &   ! tag name
      'AKAPPA', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'UNDER_RELAXATION_MOMENTUM', &   ! tag name
      'AMOMENTUM', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'BACKTRACKING_NUMBER', &   ! tag name
      'NUMTRACK', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'BACKTRACKING_TOLERANCE', &   ! tag name
      'BTOL', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'BACKTRACKING_REDUCTION_FACTOR', &   ! tag name
      'BREDUC', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'NONLINEAR', &   ! block
      'BACKTRACKING_RESIDUAL_LIMIT', &   ! tag name
      'RES_LIM', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'INNER_MAXIMUM', &   ! tag name
      'INNER_MAXIMUM', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'INNER_HCLOSE', &   ! tag name
      'INNER_HCLOSE', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'INNER_DVCLOSE', &   ! tag name
      'INNER_DVCLOSE', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'RCLOSERECORD', &   ! tag name
      'RCLOSERECORD', &   ! fortran variable
      'RECORD INNER_RCLOSE RCLOSE_OPTION', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'INNER_RCLOSE', &   ! tag name
      'INNER_RCLOSE', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'RCLOSE_OPTION', &   ! tag name
      'RCLOSE_OPTION', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .true., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'LINEAR_ACCELERATION', &   ! tag name
      'ILINMETH', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'RELAXATION_FACTOR', &   ! tag name
      'RELAX', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'PRECONDITIONER_LEVELS', &   ! tag name
      'LEVEL', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'PRECONDITIONER_DROP_TOLERANCE', &   ! tag name
      'DROPTOL', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'NUMBER_ORTHOGONALIZATIONS', &   ! tag name
      'NORTH', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'SCALING_METHOD', &   ! tag name
      'SCALING_METHOD', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ), &
    InputParamDefinitionType( &
      'SLN', &   ! component
      'IMS', &   ! subcomponent
      'LINEAR', &   ! block
      'REORDERING_METHOD', &   ! tag name
      'IORD', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ) &
  ]

  type(InputParamDefinitionType), parameter :: sln_ims_aggregate_definitions(*) = &
  [ &
    InputParamDefinitionType:: &
  ]

  type(InputBlockDefinitionType), parameter :: sln_ims_block_definitions(*) = &
  [ &
    InputBlockDefinitionType( &
      'OPTIONS', & ! blockname
      .true., & ! required
      .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
      'NONLINEAR', & ! blockname
      .true., & ! required
      .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
      'LINEAR', & ! blockname
      .true., & ! required
      .false. & ! aggregate
    ) &
  ]

end module SlnImsInputModule

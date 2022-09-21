module GwfDisInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_dis_param_definitions
  public gwf_dis_aggregate_definitions
  public gwf_dis_block_definitions
  type(InputParamDefinitionType), parameter :: &
    gwf_dis_param_definitions(*) = &
    [ &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'OPTIONS', 'LENGTH_UNITS', 'LENGTH_UNITS', 'STRING', '', &
    .false., .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'OPTIONS', 'NOGRB', 'NOGRB', 'KEYWORD', '', .false., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'OPTIONS', 'XORIGIN', 'XORIGIN', 'DOUBLE', '', .false., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'OPTIONS', 'YORIGIN', 'YORIGIN', 'DOUBLE', '', .false., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'OPTIONS', 'ANGROT', 'ANGROT', 'DOUBLE', '', .false., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'DIMENSIONS', 'NLAY', 'NLAY', 'INTEGER', '', .true., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'DIMENSIONS', 'NROW', 'NROW', 'INTEGER', '', .true., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'DIMENSIONS', 'NCOL', 'NCOL', 'INTEGER', '', .true., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'GRIDDATA', 'DELR', 'DELR', 'DOUBLE1D', 'NCOL', .true., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'GRIDDATA', 'DELC', 'DELC', 'DOUBLE1D', 'NROW', .true., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'GRIDDATA', 'TOP', 'TOP', 'DOUBLE2D', 'NCOL, NROW', .true., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'GRIDDATA', 'BOTM', 'BOTM', 'DOUBLE3D', 'NCOL, NROW, NLAY', &
    .true., .false., .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'DIS', 'GRIDDATA', 'IDOMAIN', 'IDOMAIN', 'INTEGER3D', &
    'NCOL, NROW, NLAY', .false., .false., .false., .true. &
    ) &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwf_dis_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType :: &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_dis_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'DIMENSIONS', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ) &
    ]

end module GwfDisInputModule

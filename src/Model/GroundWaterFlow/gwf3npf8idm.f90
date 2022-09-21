module GwfNpfInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_npf_param_definitions
  public gwf_npf_aggregate_definitions
  public gwf_npf_block_definitions
  type(InputParamDefinitionType), parameter :: &
    gwf_npf_param_definitions(*) = &
    [ &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'SAVE_FLOWS', 'IPAKCB', 'KEYWORD', '', .false., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'PRINT_FLOWS', 'IPRFLOW', 'KEYWORD', '', .false., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'ALTERNATIVE_CELL_AVERAGING', 'CELLAVG', &
    'STRING', '', .false., .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'THICKSTRT', 'ITHICKSTRT', 'KEYWORD', '', &
    .false., .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'CVOPTIONS', 'CVOPTIONS', &
    'RECORD VARIABLECV DEWATERED', '', .false., .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'VARIABLECV', 'IVARCV', 'KEYWORD', '', .true., &
    .true., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'DEWATERED', 'IDEWATCV', 'KEYWORD', '', .false., &
    .true., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'PERCHED', 'IPERCHED', 'KEYWORD', '', .false., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'REWET_RECORD', 'REWET_RECORD', &
    'RECORD REWET WETFCT IWETIT IHDWET', '', .false., .false., .false., &
    .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'REWET', 'IREWET', 'KEYWORD', '', .true., .true., &
    .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'WETFCT', 'WETFCT', 'DOUBLE', '', .true., .true., &
    .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'IWETIT', 'IWETIT', 'INTEGER', '', .true., &
    .true., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'IHDWET', 'IHDWET', 'INTEGER', '', .true., &
    .true., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'XT3DOPTIONS', 'XT3DOPTIONS', 'RECORD XT3D RHS', &
    '', .false., .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'XT3D', 'IXT3D', 'KEYWORD', '', .true., .true., &
    .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'RHS', 'IXT3DRHS', 'KEYWORD', '', .false., &
    .true., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'SAVE_SPECIFIC_DISCHARGE', 'ISAVSPDIS', &
    'KEYWORD', '', .false., .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'SAVE_SATURATION', 'ISAVSAT', 'KEYWORD', '', &
    .false., .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'K22OVERK', 'IK22OVERK', 'KEYWORD', '', .false., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'K33OVERK', 'IK33OVERK', 'KEYWORD', '', .false., &
    .false., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'TVK_FILERECORD', 'TVK_FILERECORD', &
    'RECORD TVK6 FILEIN TVK6_FILENAME', '', .false., .false., .false., &
    .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'TVK6', 'TVK6', 'KEYWORD', '', .true., .true., &
    .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'FILEIN', 'FILEIN', 'KEYWORD', '', .true., &
    .true., .false., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'OPTIONS', 'TVK6_FILENAME', 'TVK6_FILENAME', 'STRING', '', &
    .true., .true., .true., .false. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'GRIDDATA', 'ICELLTYPE', 'ICELLTYPE', 'INTEGER1D', 'NODES', &
    .true., .false., .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'GRIDDATA', 'K', 'K', 'DOUBLE1D', 'NODES', .true., .false., &
    .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'GRIDDATA', 'K22', 'K22', 'DOUBLE1D', 'NODES', .false., &
    .false., .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'GRIDDATA', 'K33', 'K33', 'DOUBLE1D', 'NODES', .false., &
    .false., .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'GRIDDATA', 'ANGLE1', 'ANGLE1', 'DOUBLE1D', 'NODES', &
    .false., .false., .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'GRIDDATA', 'ANGLE2', 'ANGLE2', 'DOUBLE1D', 'NODES', &
    .false., .false., .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'GRIDDATA', 'ANGLE3', 'ANGLE3', 'DOUBLE1D', 'NODES', &
    .false., .false., .false., .true. &
    ), &
    InputParamDefinitionType( &
    ! component, subcomponent, block, tag name, fortran variable, type, shape, required, multi-record, preserve case, layered
    'GWF', 'NPF', 'GRIDDATA', 'WETDRY', 'WETDRY', 'DOUBLE1D', 'NODES', &
    .false., .false., .false., .true. &
    ) &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwf_npf_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType :: &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_npf_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ) &
    ]

end module GwfNpfInputModule

module GwfIcInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, InputBlockDefinitionType
  private
  public gwf_ic_param_definitions
  public gwf_ic_aggregate_definitions
  public gwf_ic_block_definitions
  type(InputParamDefinitionType), parameter :: gwf_ic_param_definitions(*) = &
  [ &
    InputParamDefinitionType( &
      'GWF', &   ! component
      'IC', &   ! subcomponent
      'GRIDDATA', &   ! block
      'STRT', &   ! tag name
      'STRT', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NODES', &   ! shape
      .true., &   ! required
      .false., &   ! multi-record
      .false. &   ! preserve case
    ) &
  ]

  type(InputParamDefinitionType), parameter :: gwf_ic_aggregate_definitions(*) = &
  [ &
    InputParamDefinitionType:: &
  ]

  type(InputBlockDefinitionType), parameter :: gwf_ic_block_definitions(*) = &
  [ &
    InputBlockDefinitionType( &
      'GRIDDATA', & ! blockname
      .true., & ! required
      .false. & ! aggregate
    ) &
  ]

end module GwfIcInputModule

! ** Do Not Modify! MODFLOW 6 system generated file. **
module SimParInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public sim_par_param_definitions
  public sim_par_aggregate_definitions
  public sim_par_block_definitions
  public SimParParamFoundType
  public sim_par_multi_package

  type SimParParamFoundType
    logical :: log_mpi = .false.
    logical :: mrank = .false.
  end type SimParParamFoundType

  logical :: sim_par_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    simpar_log_mpi = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'PAR', & ! subcomponent
    'OPTIONS', & ! block
    'LOG_MPI', & ! tag name
    'LOG_MPI', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    simpar_mrank = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'PAR', & ! subcomponent
    'PARTITIONS', & ! block
    'MRANK', & ! tag name
    'MRANK', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    sim_par_param_definitions(*) = &
    [ &
    simpar_log_mpi, &
    simpar_mrank &
    ]

  type(InputParamDefinitionType), parameter :: &
    simpar_partitions = InputParamDefinitionType &
    ( &
    'SIM', & ! component
    'PAR', & ! subcomponent
    'PARTITIONS', & ! block
    'PARTITIONS', & ! tag name
    'PARTITIONS', & ! fortran variable
    'RECARRAY MRANK', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    sim_par_aggregate_definitions(*) = &
    [ &
    simpar_partitions &
    ]

  type(InputBlockDefinitionType), parameter :: &
    sim_par_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'PARTITIONS', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module SimParInputModule

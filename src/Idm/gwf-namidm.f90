! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwfNamInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_nam_param_definitions
  public gwf_nam_aggregate_definitions
  public gwf_nam_block_definitions
  public GwfNamParamFoundType
  public gwf_nam_multi_package

  type GwfNamParamFoundType
    logical :: list = .false.
    logical :: print_input = .false.
    logical :: print_flows = .false.
    logical :: save_flows = .false.
    logical :: newtonoptions = .false.
    logical :: newton = .false.
    logical :: under_relaxation = .false.
    logical :: ftype = .false.
    logical :: fname = .false.
    logical :: pname = .false.
  end type GwfNamParamFoundType

  logical :: gwf_nam_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    gwfnam_list = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'LIST', & ! tag name
    'LIST', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_print_input = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_INPUT', & ! tag name
    'PRINT_INPUT', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_print_flows = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_FLOWS', & ! tag name
    'PRINT_FLOWS', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_save_flows = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_FLOWS', & ! tag name
    'SAVE_FLOWS', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_newtonoptions = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'NEWTONOPTIONS', & ! tag name
    'NEWTONOPTIONS', & ! fortran variable
    'RECORD NEWTON UNDER_RELAXATION', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_newton = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'NEWTON', & ! tag name
    'NEWTON', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_under_relaxation = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'UNDER_RELAXATION', & ! tag name
    'UNDER_RELAXATION', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_ftype = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'PACKAGES', & ! block
    'FTYPE', & ! tag name
    'FTYPE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_fname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'PACKAGES', & ! block
    'FNAME', & ! tag name
    'FNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_pname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'PACKAGES', & ! block
    'PNAME', & ! tag name
    'PNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_nam_param_definitions(*) = &
    [ &
    gwfnam_list, &
    gwfnam_print_input, &
    gwfnam_print_flows, &
    gwfnam_save_flows, &
    gwfnam_newtonoptions, &
    gwfnam_newton, &
    gwfnam_under_relaxation, &
    gwfnam_ftype, &
    gwfnam_fname, &
    gwfnam_pname &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwfnam_packages = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'PACKAGES', & ! block
    'PACKAGES', & ! tag name
    'PACKAGES', & ! fortran variable
    'RECARRAY FTYPE FNAME PNAME', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_nam_aggregate_definitions(*) = &
    [ &
    gwfnam_packages &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_nam_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'PACKAGES', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module GwfNamInputModule

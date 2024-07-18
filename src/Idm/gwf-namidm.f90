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
  public gwf_nam_subpackages

  type GwfNamParamFoundType
    logical :: list = .false.
    logical :: print_input = .false.
    logical :: print_flows = .false.
    logical :: save_flows = .false.
    logical :: newtonoptions = .false.
    logical :: newton = .false.
    logical :: under_relaxation = .false.
    logical :: export_netcdf = .false.
    logical :: nc_filerecord = .false.
    logical :: netcdf = .false.
    logical :: filein = .false.
    logical :: netcdf_fname = .false.
    logical :: ftype = .false.
    logical :: fname = .false.
    logical :: pname = .false.
  end type GwfNamParamFoundType

  logical :: gwf_nam_multi_package = .false.

  character(len=16), parameter :: &
    gwf_nam_subpackages(*) = &
    [ &
    '                ' &
    ]

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
    'name of listing file', & ! longname
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
    'print input to listing file', & ! longname
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
    'print calculated flows to listing file', & ! longname
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
    'save flows for all packages to budget file', & ! longname
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
    'newton keyword and options', & ! longname
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
    'keyword to activate Newton-Raphson formulation', & ! longname
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
    'keyword to activate Newton-Raphson UNDER_RELAXATION option', & ! longname
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_export_netcdf = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'EXPORT_NETCDF', & ! tag name
    'EXPORT_NETCDF', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'export model output netcdf file.', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_nc_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'NC_FILERECORD', & ! tag name
    'NC_FILERECORD', & ! fortran variable
    'RECORD NETCDF FILEIN NETCDF_FILENAME', & ! type
    '', & ! shape
    '', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_netcdf = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'NETCDF', & ! tag name
    'NETCDF', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'netcdf keyword', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_filein = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'FILEIN', & ! tag name
    'FILEIN', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'file keyword', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfnam_netcdf_fname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NAM', & ! subcomponent
    'OPTIONS', & ! block
    'NETCDF_FILENAME', & ! tag name
    'NETCDF_FNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'netcdf input filename', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
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
    'package type', & ! longname
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
    'file name', & ! longname
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
    'user name for package', & ! longname
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
    gwfnam_export_netcdf, &
    gwfnam_nc_filerecord, &
    gwfnam_netcdf, &
    gwfnam_filein, &
    gwfnam_netcdf_fname, &
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
    'package list', & ! longname
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

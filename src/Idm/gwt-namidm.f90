! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwtNamInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwt_nam_param_definitions
  public gwt_nam_aggregate_definitions
  public gwt_nam_block_definitions
  public GwtNamParamFoundType
  public gwt_nam_multi_package
  public gwt_nam_subpackages

  type GwtNamParamFoundType
    logical :: list = .false.
    logical :: print_input = .false.
    logical :: print_flows = .false.
    logical :: save_flows = .false.
    logical :: export_netcdf = .false.
    logical :: nc_filerecord = .false.
    logical :: netcdf = .false.
    logical :: filein = .false.
    logical :: netcdf_fname = .false.
    logical :: ftype = .false.
    logical :: fname = .false.
    logical :: pname = .false.
  end type GwtNamParamFoundType

  logical :: gwt_nam_multi_package = .false.

  character(len=16), parameter :: &
    gwt_nam_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwtnam_list = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwtnam_print_input = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwtnam_print_flows = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwtnam_save_flows = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwtnam_export_netcdf = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwtnam_nc_filerecord = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwtnam_netcdf = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwtnam_filein = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwtnam_netcdf_fname = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwtnam_ftype = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwtnam_fname = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwtnam_pname = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwt_nam_param_definitions(*) = &
    [ &
    gwtnam_list, &
    gwtnam_print_input, &
    gwtnam_print_flows, &
    gwtnam_save_flows, &
    gwtnam_export_netcdf, &
    gwtnam_nc_filerecord, &
    gwtnam_netcdf, &
    gwtnam_filein, &
    gwtnam_netcdf_fname, &
    gwtnam_ftype, &
    gwtnam_fname, &
    gwtnam_pname &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwtnam_packages = InputParamDefinitionType &
    ( &
    'GWT', & ! component
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
    gwt_nam_aggregate_definitions(*) = &
    [ &
    gwtnam_packages &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwt_nam_block_definitions(*) = &
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

end module GwtNamInputModule

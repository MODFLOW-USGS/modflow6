! ** Do Not Modify! MODFLOW 6 system generated file. **
module ChfDis2DInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public chf_dis2d_param_definitions
  public chf_dis2d_aggregate_definitions
  public chf_dis2d_block_definitions
  public ChfDis2dParamFoundType
  public chf_dis2d_multi_package
  public chf_dis2d_subpackages

  type ChfDis2dParamFoundType
    logical :: length_units = .false.
    logical :: nogrb = .false.
    logical :: xorigin = .false.
    logical :: yorigin = .false.
    logical :: angrot = .false.
    logical :: export_ascii = .false.
    logical :: nrow = .false.
    logical :: ncol = .false.
    logical :: delr = .false.
    logical :: delc = .false.
    logical :: bottom = .false.
    logical :: idomain = .false.
  end type ChfDis2dParamFoundType

  logical :: chf_dis2d_multi_package = .false.

  character(len=16), parameter :: &
    chf_dis2d_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    chfdis2d_length_units = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DIS2D', & ! subcomponent
    'OPTIONS', & ! block
    'LENGTH_UNITS', & ! tag name
    'LENGTH_UNITS', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'model length units', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdis2d_nogrb = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DIS2D', & ! subcomponent
    'OPTIONS', & ! block
    'NOGRB', & ! tag name
    'NOGRB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'do not write binary grid file', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdis2d_xorigin = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DIS2D', & ! subcomponent
    'OPTIONS', & ! block
    'XORIGIN', & ! tag name
    'XORIGIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'x-position of the model grid origin', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdis2d_yorigin = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DIS2D', & ! subcomponent
    'OPTIONS', & ! block
    'YORIGIN', & ! tag name
    'YORIGIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'y-position of the model grid origin', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdis2d_angrot = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DIS2D', & ! subcomponent
    'OPTIONS', & ! block
    'ANGROT', & ! tag name
    'ANGROT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'rotation angle', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdis2d_export_ascii = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DIS2D', & ! subcomponent
    'OPTIONS', & ! block
    'EXPORT_ARRAY_ASCII', & ! tag name
    'EXPORT_ASCII', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'export array variables to layered ascii files.', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdis2d_nrow = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DIS2D', & ! subcomponent
    'DIMENSIONS', & ! block
    'NROW', & ! tag name
    'NROW', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'number of rows', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdis2d_ncol = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DIS2D', & ! subcomponent
    'DIMENSIONS', & ! block
    'NCOL', & ! tag name
    'NCOL', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'number of columns', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdis2d_delr = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DIS2D', & ! subcomponent
    'GRIDDATA', & ! block
    'DELR', & ! tag name
    'DELR', & ! fortran variable
    'DOUBLE1D', & ! type
    'NCOL', & ! shape
    'spacing along a row', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdis2d_delc = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DIS2D', & ! subcomponent
    'GRIDDATA', & ! block
    'DELC', & ! tag name
    'DELC', & ! fortran variable
    'DOUBLE1D', & ! type
    'NROW', & ! shape
    'spacing along a column', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdis2d_bottom = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DIS2D', & ! subcomponent
    'GRIDDATA', & ! block
    'BOTTOM', & ! tag name
    'BOTTOM', & ! fortran variable
    'DOUBLE2D', & ! type
    'NCOL NROW', & ! shape
    'cell bottom elevation', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfdis2d_idomain = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'DIS2D', & ! subcomponent
    'GRIDDATA', & ! block
    'IDOMAIN', & ! tag name
    'IDOMAIN', & ! fortran variable
    'INTEGER2D', & ! type
    'NCOL NROW', & ! shape
    'idomain existence array', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chf_dis2d_param_definitions(*) = &
    [ &
    chfdis2d_length_units, &
    chfdis2d_nogrb, &
    chfdis2d_xorigin, &
    chfdis2d_yorigin, &
    chfdis2d_angrot, &
    chfdis2d_export_ascii, &
    chfdis2d_nrow, &
    chfdis2d_ncol, &
    chfdis2d_delr, &
    chfdis2d_delc, &
    chfdis2d_bottom, &
    chfdis2d_idomain &
    ]

  type(InputParamDefinitionType), parameter :: &
    chf_dis2d_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType &
    ( &
    '', & ! component
    '', & ! subcomponent
    '', & ! block
    '', & ! tag name
    '', & ! fortran variable
    '', & ! type
    '', & ! shape
    '', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    ) &
    ]

  type(InputBlockDefinitionType), parameter :: &
    chf_dis2d_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'DIMENSIONS', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module ChfDis2DInputModule

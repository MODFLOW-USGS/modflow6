! ** Do Not Modify! MODFLOW 6 system generated file. **
module OlfCxsInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public olf_cxs_param_definitions
  public olf_cxs_aggregate_definitions
  public olf_cxs_block_definitions
  public OlfCxsParamFoundType
  public olf_cxs_multi_package
  public olf_cxs_subpackages

  type OlfCxsParamFoundType
    logical :: iprpak = .false.
    logical :: nsections = .false.
    logical :: npoints = .false.
    logical :: idcxs = .false.
    logical :: nxspoints = .false.
    logical :: xfraction = .false.
    logical :: height = .false.
    logical :: manfraction = .false.
  end type OlfCxsParamFoundType

  logical :: olf_cxs_multi_package = .false.

  character(len=16), parameter :: &
    olf_cxs_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    olfcxs_iprpak = InputParamDefinitionType &
    ( &
    'OLF', & ! component
    'CXS', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_INPUT', & ! tag name
    'IPRPAK', & ! fortran variable
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
    olfcxs_nsections = InputParamDefinitionType &
    ( &
    'OLF', & ! component
    'CXS', & ! subcomponent
    'DIMENSIONS', & ! block
    'NSECTIONS', & ! tag name
    'NSECTIONS', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'number of reaches', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    olfcxs_npoints = InputParamDefinitionType &
    ( &
    'OLF', & ! component
    'CXS', & ! subcomponent
    'DIMENSIONS', & ! block
    'NPOINTS', & ! tag name
    'NPOINTS', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'total number of points defined for all reaches', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    olfcxs_idcxs = InputParamDefinitionType &
    ( &
    'OLF', & ! component
    'CXS', & ! subcomponent
    'PACKAGEDATA', & ! block
    'IDCXS', & ! tag name
    'IDCXS', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'reach number for this entry', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    olfcxs_nxspoints = InputParamDefinitionType &
    ( &
    'OLF', & ! component
    'CXS', & ! subcomponent
    'PACKAGEDATA', & ! block
    'NXSPOINTS', & ! tag name
    'NXSPOINTS', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'number of points used to define cross section', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    olfcxs_xfraction = InputParamDefinitionType &
    ( &
    'OLF', & ! component
    'CXS', & ! subcomponent
    'CROSSSECTIONDATA', & ! block
    'XFRACTION', & ! tag name
    'XFRACTION', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'fractional width', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    olfcxs_height = InputParamDefinitionType &
    ( &
    'OLF', & ! component
    'CXS', & ! subcomponent
    'CROSSSECTIONDATA', & ! block
    'HEIGHT', & ! tag name
    'HEIGHT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'depth', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    olfcxs_manfraction = InputParamDefinitionType &
    ( &
    'OLF', & ! component
    'CXS', & ! subcomponent
    'CROSSSECTIONDATA', & ! block
    'MANFRACTION', & ! tag name
    'MANFRACTION', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'Mannings roughness coefficient', & ! longname
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    olf_cxs_param_definitions(*) = &
    [ &
    olfcxs_iprpak, &
    olfcxs_nsections, &
    olfcxs_npoints, &
    olfcxs_idcxs, &
    olfcxs_nxspoints, &
    olfcxs_xfraction, &
    olfcxs_height, &
    olfcxs_manfraction &
    ]

  type(InputParamDefinitionType), parameter :: &
    olfcxs_packagedata = InputParamDefinitionType &
    ( &
    'OLF', & ! component
    'CXS', & ! subcomponent
    'PACKAGEDATA', & ! block
    'PACKAGEDATA', & ! tag name
    'PACKAGEDATA', & ! fortran variable
    'RECARRAY IDCXS NXSPOINTS', & ! type
    'NSECTIONS', & ! shape
    '', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    olfcxs_crosssectiondata = InputParamDefinitionType &
    ( &
    'OLF', & ! component
    'CXS', & ! subcomponent
    'CROSSSECTIONDATA', & ! block
    'CROSSSECTIONDATA', & ! tag name
    'CROSSSECTIONDATA', & ! fortran variable
    'RECARRAY XFRACTION HEIGHT MANFRACTION', & ! type
    'NPOINTS', & ! shape
    '', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    olf_cxs_aggregate_definitions(*) = &
    [ &
    olfcxs_packagedata, &
    olfcxs_crosssectiondata &
    ]

  type(InputBlockDefinitionType), parameter :: &
    olf_cxs_block_definitions(*) = &
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
    'PACKAGEDATA', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'CROSSSECTIONDATA', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module OlfCxsInputModule

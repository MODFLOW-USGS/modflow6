! ** Do Not Modify! MODFLOW 6 system generated file. **
module PrtDisInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public prt_dis_param_definitions
  public prt_dis_aggregate_definitions
  public prt_dis_block_definitions
  public PrtDisParamFoundType
  public prt_dis_multi_package

  type PrtDisParamFoundType
    logical :: length_units = .false.
    logical :: nogrb = .false.
    logical :: xorigin = .false.
    logical :: yorigin = .false.
    logical :: angrot = .false.
    logical :: nlay = .false.
    logical :: nrow = .false.
    logical :: ncol = .false.
    logical :: delr = .false.
    logical :: delc = .false.
    logical :: top = .false.
    logical :: botm = .false.
    logical :: idomain = .false.
  end type PrtDisParamFoundType

  logical :: prt_dis_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    prtdis_length_units = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'OPTIONS', & ! block
    'LENGTH_UNITS', & ! tag name
    'LENGTH_UNITS', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdis_nogrb = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'OPTIONS', & ! block
    'NOGRB', & ! tag name
    'NOGRB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdis_xorigin = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'OPTIONS', & ! block
    'XORIGIN', & ! tag name
    'XORIGIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdis_yorigin = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'OPTIONS', & ! block
    'YORIGIN', & ! tag name
    'YORIGIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdis_angrot = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'OPTIONS', & ! block
    'ANGROT', & ! tag name
    'ANGROT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdis_nlay = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'DIMENSIONS', & ! block
    'NLAY', & ! tag name
    'NLAY', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdis_nrow = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'DIMENSIONS', & ! block
    'NROW', & ! tag name
    'NROW', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdis_ncol = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'DIMENSIONS', & ! block
    'NCOL', & ! tag name
    'NCOL', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdis_delr = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'GRIDDATA', & ! block
    'DELR', & ! tag name
    'DELR', & ! fortran variable
    'DOUBLE1D', & ! type
    'NCOL', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdis_delc = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'GRIDDATA', & ! block
    'DELC', & ! tag name
    'DELC', & ! fortran variable
    'DOUBLE1D', & ! type
    'NROW', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdis_top = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'GRIDDATA', & ! block
    'TOP', & ! tag name
    'TOP', & ! fortran variable
    'DOUBLE2D', & ! type
    'NCOL NROW', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdis_botm = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'GRIDDATA', & ! block
    'BOTM', & ! tag name
    'BOTM', & ! fortran variable
    'DOUBLE3D', & ! type
    'NCOL NROW NLAY', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prtdis_idomain = InputParamDefinitionType &
    ( &
    'PRT', & ! component
    'DIS', & ! subcomponent
    'GRIDDATA', & ! block
    'IDOMAIN', & ! tag name
    'IDOMAIN', & ! fortran variable
    'INTEGER3D', & ! type
    'NCOL NROW NLAY', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    prt_dis_param_definitions(*) = &
    [ &
    prtdis_length_units, &
    prtdis_nogrb, &
    prtdis_xorigin, &
    prtdis_yorigin, &
    prtdis_angrot, &
    prtdis_nlay, &
    prtdis_nrow, &
    prtdis_ncol, &
    prtdis_delr, &
    prtdis_delc, &
    prtdis_top, &
    prtdis_botm, &
    prtdis_idomain &
    ]

  type(InputParamDefinitionType), parameter :: &
    prt_dis_aggregate_definitions(*) = &
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
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    ) &
    ]

  type(InputBlockDefinitionType), parameter :: &
    prt_dis_block_definitions(*) = &
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

end module PrtDisInputModule

module GwfNpfInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_npf_param_definitions
  public gwf_npf_aggregate_definitions
  public gwf_npf_block_definitions

  type(InputParamDefinitionType), parameter :: &
    mf6var_ipakcb = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_FLOWS', & ! tag name
    'IPAKCB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_iprflow = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_FLOWS', & ! tag name
    'IPRFLOW', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_cellavg = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'ALTERNATIVE_CELL_AVERAGING', & ! tag name
    'CELLAVG', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_ithickstrt = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'THICKSTRT', & ! tag name
    'ITHICKSTRT', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_cvoptions = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'CVOPTIONS', & ! tag name
    'CVOPTIONS', & ! fortran variable
    'RECORD VARIABLECV DEWATERED', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_ivarcv = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'VARIABLECV', & ! tag name
    'IVARCV', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_idewatcv = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'DEWATERED', & ! tag name
    'IDEWATCV', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_iperched = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'PERCHED', & ! tag name
    'IPERCHED', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_rewet_record = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'REWET_RECORD', & ! tag name
    'REWET_RECORD', & ! fortran variable
    'RECORD REWET WETFCT IWETIT IHDWET', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_irewet = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'REWET', & ! tag name
    'IREWET', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_wetfct = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'WETFCT', & ! tag name
    'WETFCT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_iwetit = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'IWETIT', & ! tag name
    'IWETIT', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_ihdwet = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'IHDWET', & ! tag name
    'IHDWET', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_xt3doptions = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'XT3DOPTIONS', & ! tag name
    'XT3DOPTIONS', & ! fortran variable
    'RECORD XT3D RHS', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_ixt3d = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'XT3D', & ! tag name
    'IXT3D', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_ixt3drhs = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'RHS', & ! tag name
    'IXT3DRHS', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_isavspdis = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_SPECIFIC_DISCHARGE', & ! tag name
    'ISAVSPDIS', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_isavsat = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_SATURATION', & ! tag name
    'ISAVSAT', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_ik22overk = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'K22OVERK', & ! tag name
    'IK22OVERK', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_ik33overk = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'K33OVERK', & ! tag name
    'IK33OVERK', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_tvk_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'TVK_FILERECORD', & ! tag name
    'TVK_FILERECORD', & ! fortran variable
    'RECORD TVK6 FILEIN TVK6_FILENAME', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_tvk6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'TVK6', & ! tag name
    'TVK6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_filein = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'FILEIN', & ! tag name
    'FILEIN', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_tvk6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'TVK6_FILENAME', & ! tag name
    'TVK6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_inewton = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'DEV_NO_NEWTON', & ! tag name
    'INEWTON', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_iusgnrhc = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'DEV_MODFLOWUSG_UPSTREAM_WEIGHTED_SATURATION', & ! tag name
    'IUSGNRHC', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_inwtupw = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'DEV_MODFLOWNWT_UPSTREAM_WEIGHTING', & ! tag name
    'INWTUPW', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_satmin = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'DEV_MINIMUM_SATURATED_THICKNESS', & ! tag name
    'SATMIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_satomega = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'OPTIONS', & ! block
    'DEV_OMEGA', & ! tag name
    'SATOMEGA', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_icelltype = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'GRIDDATA', & ! block
    'ICELLTYPE', & ! tag name
    'ICELLTYPE', & ! fortran variable
    'INTEGER1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_k = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'GRIDDATA', & ! block
    'K', & ! tag name
    'K', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_k22 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'GRIDDATA', & ! block
    'K22', & ! tag name
    'K22', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_k33 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'GRIDDATA', & ! block
    'K33', & ! tag name
    'K33', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_angle1 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'GRIDDATA', & ! block
    'ANGLE1', & ! tag name
    'ANGLE1', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_angle2 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'GRIDDATA', & ! block
    'ANGLE2', & ! tag name
    'ANGLE2', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_angle3 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'GRIDDATA', & ! block
    'ANGLE3', & ! tag name
    'ANGLE3', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    mf6var_wetdry = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'NPF', & ! subcomponent
    'GRIDDATA', & ! block
    'WETDRY', & ! tag name
    'WETDRY', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_npf_param_definitions(*) = &
    [ &
    mf6var_ipakcb, &
    mf6var_iprflow, &
    mf6var_cellavg, &
    mf6var_ithickstrt, &
    mf6var_cvoptions, &
    mf6var_ivarcv, &
    mf6var_idewatcv, &
    mf6var_iperched, &
    mf6var_rewet_record, &
    mf6var_irewet, &
    mf6var_wetfct, &
    mf6var_iwetit, &
    mf6var_ihdwet, &
    mf6var_xt3doptions, &
    mf6var_ixt3d, &
    mf6var_ixt3drhs, &
    mf6var_isavspdis, &
    mf6var_isavsat, &
    mf6var_ik22overk, &
    mf6var_ik33overk, &
    mf6var_tvk_filerecord, &
    mf6var_tvk6, &
    mf6var_filein, &
    mf6var_tvk6_filename, &
    mf6var_inewton, &
    mf6var_iusgnrhc, &
    mf6var_inwtupw, &
    mf6var_satmin, &
    mf6var_satomega, &
    mf6var_icelltype, &
    mf6var_k, &
    mf6var_k22, &
    mf6var_k33, &
    mf6var_angle1, &
    mf6var_angle2, &
    mf6var_angle3, &
    mf6var_wetdry &
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

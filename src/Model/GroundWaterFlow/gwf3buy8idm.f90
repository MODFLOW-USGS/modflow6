module GwfBuyInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_buy_param_definitions
  public gwf_buy_aggregate_definitions
  public gwf_buy_block_definitions
  public GwfBuyParamFoundType

  type GwfBuyParamFoundType
    logical :: hhform_rhs = .false.
    logical :: denseref = .false.
    logical :: density_filerecord = .false.
    logical :: density = .false.
    logical :: fileout = .false.
    logical :: densityfile = .false.
    logical :: dev_efh_form = .false.
    logical :: nrhospecies = .false.
    logical :: irhospec = .false.
    logical :: drhodc = .false.
    logical :: crhoref = .false.
    logical :: modelname = .false.
    logical :: auxspeciesname = .false.
  end type GwfBuyParamFoundType

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_hhform_rhs = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'OPTIONS', & ! block
    'HHFORMULATION_RHS', & ! tag name
    'HHFORM_RHS', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_denseref = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'OPTIONS', & ! block
    'DENSEREF', & ! tag name
    'DENSEREF', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_density_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'OPTIONS', & ! block
    'DENSITY_FILERECORD', & ! tag name
    'DENSITY_FILERECORD', & ! fortran variable
    'RECORD DENSITY FILEOUT DENSITYFILE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_density = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'OPTIONS', & ! block
    'DENSITY', & ! tag name
    'DENSITY', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_fileout = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'OPTIONS', & ! block
    'FILEOUT', & ! tag name
    'FILEOUT', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_densityfile = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'OPTIONS', & ! block
    'DENSITYFILE', & ! tag name
    'DENSITYFILE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_dev_efh_form = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'OPTIONS', & ! block
    'DEV_EFH_FORMULATION', & ! tag name
    'DEV_EFH_FORM', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_nrhospecies = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'DIMENSIONS', & ! block
    'NRHOSPECIES', & ! tag name
    'NRHOSPECIES', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_irhospec = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'PACKAGEDATA', & ! block
    'IRHOSPEC', & ! tag name
    'IRHOSPEC', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_drhodc = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'PACKAGEDATA', & ! block
    'DRHODC', & ! tag name
    'DRHODC', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_crhoref = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'PACKAGEDATA', & ! block
    'CRHOREF', & ! tag name
    'CRHOREF', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_modelname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'PACKAGEDATA', & ! block
    'MODELNAME', & ! tag name
    'MODELNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_auxspeciesname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'PACKAGEDATA', & ! block
    'AUXSPECIESNAME', & ! tag name
    'AUXSPECIESNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_buy_param_definitions(*) = &
    [ &
    gwfbuy_hhform_rhs, &
    gwfbuy_denseref, &
    gwfbuy_density_filerecord, &
    gwfbuy_density, &
    gwfbuy_fileout, &
    gwfbuy_densityfile, &
    gwfbuy_dev_efh_form, &
    gwfbuy_nrhospecies, &
    gwfbuy_irhospec, &
    gwfbuy_drhodc, &
    gwfbuy_crhoref, &
    gwfbuy_modelname, &
    gwfbuy_auxspeciesname &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwfbuy_packagedata = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'BUY', & ! subcomponent
    'PACKAGEDATA', & ! block
    'PACKAGEDATA', & ! tag name
    'PACKAGEDATA', & ! fortran variable
    'RECARRAY IRHOSPEC DRHODC CRHOREF MODELNAME AUXSPECIESNAME', & ! type
    'NRHOSPECIES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_buy_aggregate_definitions(*) = &
    [ &
    gwfbuy_packagedata &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_buy_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'DIMENSIONS', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'PACKAGEDATA', & ! blockname
    .false., & ! required
    .true. & ! aggregate
    ) &
    ]

end module GwfBuyInputModule

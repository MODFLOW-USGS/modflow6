module GwfVscInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_vsc_param_definitions
  public gwf_vsc_aggregate_definitions
  public gwf_vsc_block_definitions
  public GwfVscParamFoundType

  type GwfVscParamFoundType
    logical :: viscref = .false.
    logical :: name_temp_spec = .false.
    logical :: thermal_form = .false.
    logical :: thermal_a2 = .false.
    logical :: thermal_a3 = .false.
    logical :: thermal_a4 = .false.
    logical :: viscosity_filerecord = .false.
    logical :: viscosity = .false.
    logical :: fileout = .false.
    logical :: viscosityfile = .false.
    logical :: nviscspecies = .false.
    logical :: iviscspec = .false.
    logical :: dviscdc = .false.
    logical :: cviscref = .false.
    logical :: modelname = .false.
    logical :: auxspeciesname = .false.
  end type GwfVscParamFoundType

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_viscref = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'OPTIONS', & ! block
    'VISCREF', & ! tag name
    'VISCREF', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_name_temp_spec = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'OPTIONS', & ! block
    'TEMPERATURE_SPECIES_NAME', & ! tag name
    'NAME_TEMP_SPEC', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_thermal_form = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'OPTIONS', & ! block
    'THERMAL_FORMULATION', & ! tag name
    'THERMAL_FORM', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_thermal_a2 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'OPTIONS', & ! block
    'THERMAL_A2', & ! tag name
    'THERMAL_A2', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_thermal_a3 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'OPTIONS', & ! block
    'THERMAL_A3', & ! tag name
    'THERMAL_A3', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_thermal_a4 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'OPTIONS', & ! block
    'THERMAL_A4', & ! tag name
    'THERMAL_A4', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_viscosity_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'OPTIONS', & ! block
    'VISCOSITY_FILERECORD', & ! tag name
    'VISCOSITY_FILERECORD', & ! fortran variable
    'RECORD VISCOSITY FILEOUT VISCOSITYFILE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_viscosity = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'OPTIONS', & ! block
    'VISCOSITY', & ! tag name
    'VISCOSITY', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_fileout = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
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
    gwfvsc_viscosityfile = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'OPTIONS', & ! block
    'VISCOSITYFILE', & ! tag name
    'VISCOSITYFILE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_nviscspecies = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'DIMENSIONS', & ! block
    'NVISCSPECIES', & ! tag name
    'NVISCSPECIES', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_iviscspec = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'PACKAGEDATA', & ! block
    'IVISCSPEC', & ! tag name
    'IVISCSPEC', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_dviscdc = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'PACKAGEDATA', & ! block
    'DVISCDC', & ! tag name
    'DVISCDC', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_cviscref = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'PACKAGEDATA', & ! block
    'CVISCREF', & ! tag name
    'CVISCREF', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_modelname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'PACKAGEDATA', & ! block
    'MODELNAME', & ! tag name
    'MODELNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_auxspeciesname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'PACKAGEDATA', & ! block
    'AUXSPECIESNAME', & ! tag name
    'AUXSPECIESNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_vsc_param_definitions(*) = &
    [ &
    gwfvsc_viscref, &
    gwfvsc_name_temp_spec, &
    gwfvsc_thermal_form, &
    gwfvsc_thermal_a2, &
    gwfvsc_thermal_a3, &
    gwfvsc_thermal_a4, &
    gwfvsc_viscosity_filerecord, &
    gwfvsc_viscosity, &
    gwfvsc_fileout, &
    gwfvsc_viscosityfile, &
    gwfvsc_nviscspecies, &
    gwfvsc_iviscspec, &
    gwfvsc_dviscdc, &
    gwfvsc_cviscref, &
    gwfvsc_modelname, &
    gwfvsc_auxspeciesname &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwfvsc_packagedata = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSC', & ! subcomponent
    'PACKAGEDATA', & ! block
    'PACKAGEDATA', & ! tag name
    'PACKAGEDATA', & ! fortran variable
    'RECARRAY IVISCSPEC DVISCDC CVISCREF MODELNAME AUXSPECIESNAME', & ! type
    'NVISCSPECIES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_vsc_aggregate_definitions(*) = &
    [ &
    gwfvsc_packagedata &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_vsc_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'DIMENSIONS', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'PACKAGEDATA', & ! blockname
    .true., & ! required
    .true. & ! aggregate
    ) &
    ]

end module GwfVscInputModule

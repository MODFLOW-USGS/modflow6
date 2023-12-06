!> @brief This module contains the InputDefinitionModule
!!
!! This module contains helper objects for storing
!! information about how to read modflow input files.
!!
!<
module InputDefinitionModule

  use KindModule, only: LGP
  use ConstantsModule, only: LINELENGTH, LENVARNAME, LENCOMPONENTNAME

  implicit none
  private
  public :: InputParamDefinitionType, &
            InputBlockDefinitionType

  !> @brief type for storing input definition
  !!
  !! This type is used to store information for
  !! each modflow input record
  !!
  !<
  type InputParamDefinitionType
    character(len=LENCOMPONENTNAME) :: component_type = ''
    character(len=LENCOMPONENTNAME) :: subcomponent_type = ''
    character(len=LENCOMPONENTNAME) :: blockname = ''
    character(len=LINELENGTH) :: tagname = ''
    character(len=LENVARNAME) :: mf6varname = ''
    character(len=LINELENGTH) :: datatype = ''
    character(len=LINELENGTH) :: shape = ''
    logical(LGP) :: required = .false.
    logical(LGP) :: in_record = .false.
    logical(LGP) :: preserve_case = .false.
    logical(LGP) :: layered = .false.
    logical(LGP) :: timeseries = .false.
  end type InputParamDefinitionType

  !> @brief type for storing block information
  !!
  !! This type is used to store information for
  !! how to read a modflow block
  !!
  !<
  type InputBlockDefinitionType
    character(len=LENCOMPONENTNAME) :: blockname = ''
    logical(LGP) :: required = .false.
    logical(LGP) :: aggregate = .false.
    logical(LGP) :: block_variable = .false.
  end type InputBlockDefinitionType

end module InputDefinitionModule

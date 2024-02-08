!> @brief This module contains the InputDefinitionModule
!!
!! This module contains helper objects for storing
!! information about how to read modflow input files.
!!
!<
module InputDefinitionModule

  use KindModule, only: LGP
  use ConstantsModule, only: LENVARNAME

  implicit none
  private
  public :: InputParamDefinitionType, &
            InputBlockDefinitionType

  !> @brief derived type for storing input definition
  !!
  !! This derived type is used to store information for
  !! each modflow input record
  !!
  !<
  type InputParamDefinitionType
    character(len=100) :: component_type = ''
    character(len=100) :: subcomponent_type = ''
    character(len=100) :: blockname = ''
    character(len=100) :: tagname = ''
    character(len=LENVARNAME) :: mf6varname = ''
    character(len=120) :: datatype = ''
    character(len=100) :: shape = ''
    logical(LGP) :: required = .false.
    logical(LGP) :: in_record = .false.
    logical(LGP) :: preserve_case = .false.
    logical(LGP) :: layered = .false.
    logical(LGP) :: timeseries = .false.
  end type InputParamDefinitionType

  !> @brief derived type for storing block information
  !!
  !! This derived type is used to store information for
  !! how to read a modflow block
  !!
  !<
  type InputBlockDefinitionType
    character(len=100) :: blockname = ''
    logical(LGP) :: required = .false.
    logical(LGP) :: aggregate = .false.
    logical(LGP) :: block_variable = .false.
  end type InputBlockDefinitionType

end module InputDefinitionModule

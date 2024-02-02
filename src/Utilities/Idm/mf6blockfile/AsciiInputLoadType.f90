!> @brief This module contains the AsciiInputLoadTypeModule
!!
!! This module defines an abstract type that support generic
!! IDP dynamic input loading for traditional MODFLOW 6 ascii
!! files.
!!
!<
module AsciiInputLoadTypeModule

  use KindModule, only: DP, I4B, LGP
  use InputLoadTypeModule, only: DynamicPkgLoadType
  use BlockParserModule, only: BlockParserType
  use ModflowInputModule, only: ModflowInputType

  implicit none
  private
  public :: AsciiDynamicPkgLoadBaseType

  !> @brief base abstract type for ascii source dynamic load
  !!
  !<
  type, abstract, extends(DynamicPkgLoadType) :: AsciiDynamicPkgLoadBaseType
  contains
    procedure(load_init_if), deferred :: ainit !< source loader init
    procedure(period_load_if), deferred :: rp !< source loader read and prepare
  end type AsciiDynamicPkgLoadBaseType

  abstract interface
    subroutine period_load_if(this, parser)
      import AsciiDynamicPkgLoadBaseType, BlockParserType
      class(AsciiDynamicPkgLoadBaseType), intent(inout) :: this
      type(BlockParserType), pointer, intent(inout) :: parser !< block parser
    end subroutine
    subroutine load_init_if(this, mf6_input, component_name, &
                            component_input_name, input_name, &
                            iperblock, parser, iout)
      import I4B, AsciiDynamicPkgLoadBaseType, BlockParserType, ModflowInputType
      class(AsciiDynamicPkgLoadBaseType), intent(inout) :: this
      type(ModflowInputType), intent(in) :: mf6_input !< description of input
      character(len=*), intent(in) :: component_name !< component name
      character(len=*), intent(in) :: component_input_name !< component input name, e.g. model name file
      character(len=*), intent(in) :: input_name !< input name, e.g. package *.chd file
      integer(I4B), intent(in) :: iperblock !< index of period block on block definition list
      type(BlockParserType), pointer, intent(inout) :: parser !< block parser
      integer(I4B), intent(in) :: iout
    end subroutine
  end interface

end module AsciiInputLoadTypeModule

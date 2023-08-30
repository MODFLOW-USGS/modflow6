module AsciiInputLoadTypeModule

  use KindModule, only: DP, I4B, LGP
  use InputLoadTypeModule, only: DynamicPkgLoadType
  use BlockParserModule, only: BlockParserType

  implicit none
  private
  public :: AsciiDynamicPkgLoadBaseType

  !> @brief base abstract type for ascii source dynamic load
  !!
  !<
  type, abstract, extends(DynamicPkgLoadType) :: AsciiDynamicPkgLoadBaseType
  contains
    procedure(ascii_period_load_if), deferred :: rp
  end type AsciiDynamicPkgLoadBaseType

  abstract interface
    subroutine ascii_period_load_if(this, parser)
      import AsciiDynamicPkgLoadBaseType, BlockParserType
      class(AsciiDynamicPkgLoadBaseType), intent(inout) :: this
      type(BlockParserType), pointer, intent(inout) :: parser !< block parser
    end subroutine
  end interface

end module AsciiInputLoadTypeModule

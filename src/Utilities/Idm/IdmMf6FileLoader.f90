module IdmMf6FileLoaderModule

  use KindModule, only: DP, I4B, LGP
  use BlockParserModule, only: BlockParserType
  use IdmTypesModule, only: ModflowInputType, ModflowInput

  implicit none
  private
  public :: input_load

  type :: PackageLoad
    procedure(IPackageLoad), nopass, pointer, public :: load_package => null()
  end type PackageLoad

  abstract interface
    subroutine IPackageLoad(parser, mf6_input, iout)
      use KindModule, only: DP, I4B
      use BlockParserModule, only: BlockParserType
      use IdmTypesModule, only: ModflowInputType
      type(BlockParserType), intent(inout) :: parser
      type(ModflowInputType), pointer, intent(in) :: mf6_input
      integer(I4B), intent(in) :: iout
    end subroutine IPackageLoad
  end interface

contains

  subroutine generic_mf6_load(parser, mf6_input, iout)
    use LoadMf6FileTypeModule, only: idm_load
    type(BlockParserType), intent(inout) :: parser
    type(ModflowInputType), pointer, intent(in) :: mf6_input
    integer(I4B), intent(in) :: iout

    call idm_load(parser, mf6_input%file_type, &
                  mf6_input%component_type, mf6_input%subcomponent_type, &
                  mf6_input%component_name, mf6_input%subcomponent_name, &
                  mf6_input%subpackages, iout)

  end subroutine generic_mf6_load

  subroutine input_load(parser, filetype, &
                        component_type, subcomponent_type, &
                        component_name, subcomponent_name, &
                        subpackages, iout)
    type(BlockParserType), intent(inout) :: parser
    character(len=*), intent(in) :: filetype
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    character(len=*), dimension(:), intent(in) :: subpackages
    integer(I4B), intent(in) :: iout
    type(ModflowInputType), pointer :: mf6_input
    type(PackageLoad) :: pkgloader

    mf6_input => ModflowInput(filetype, component_type, &
                              subcomponent_type, component_name, &
                              subcomponent_name, subpackages)
    !
    ! -- set mf6 parser based package loader by file type
    select case (filetype)
    case default
      pkgloader%load_package => generic_mf6_load
    end select

    call pkgloader%load_package(parser, mf6_input, iout)
  end subroutine input_load

end module IdmMf6FileLoaderModule

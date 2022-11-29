!> @brief This module contains the IdmMf6FileLoaderModule
!!
!! This module contains the high-level routines for loading
!! a MODFLOW input file into the __INPUT__ memory manager
!! space.
!!
!<
module IdmMf6FileLoaderModule

  use KindModule, only: DP, I4B, LGP
  use BlockParserModule, only: BlockParserType
  use ModflowInputModule, only: ModflowInputType, getModflowInput

  implicit none
  private
  public :: input_load

  !> @brief derived type for storing package loader
  !!
  !! This derived type is used to store a pointer to a
  !! package load procedure.  This could be used to write
  !! a custom package loader as a way to override the
  !! generic_mf6_load routine.
  !!
  !<
  type :: PackageLoad
    procedure(IPackageLoad), nopass, pointer, public :: load_package => null() !< procedure pointer to the load routine
  end type PackageLoad

  abstract interface
    subroutine IPackageLoad(parser, mf6_input, iout)
      use KindModule, only: DP, I4B
      use BlockParserModule, only: BlockParserType
      use ModflowInputModule, only: ModflowInputType
      type(BlockParserType), intent(inout) :: parser !< block parser
      type(ModflowInputType), intent(in) :: mf6_input !< ModflowInputType object that describes the input
      integer(I4B), intent(in) :: iout !< unit number for output
    end subroutine IPackageLoad
  end interface

contains

  !> @brief generic procedure to MODFLOW 6 load routine
  !<
  subroutine generic_mf6_load(parser, mf6_input, iout)
    use LoadMf6FileTypeModule, only: idm_load
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(ModflowInputType), intent(in) :: mf6_input !< ModflowInputType object that describes the input
    integer(I4B), intent(in) :: iout !< unit number for output

    call idm_load(parser, mf6_input%file_type, &
                  mf6_input%component_type, mf6_input%subcomponent_type, &
                  mf6_input%component_name, mf6_input%subcomponent_name, &
                  mf6_input%subpackages, iout)

  end subroutine generic_mf6_load

  !> @brief main entry to mf6 input load
  !<
  subroutine input_load(parser, filetype, &
                        component_type, subcomponent_type, &
                        component_name, subcomponent_name, &
                        subpackages, iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    character(len=*), intent(in) :: filetype !< file type to load, such as DIS6, DISV6, NPF6
    character(len=*), intent(in) :: component_type !< component type, such as GWF or GWT
    character(len=*), intent(in) :: subcomponent_type !< subcomponent type, such as DIS or NPF
    character(len=*), intent(in) :: component_name !< component name, such as MYGWFMODEL
    character(len=*), intent(in) :: subcomponent_name !< subcomponent name, such as MYWELLPACKAGE
    character(len=*), dimension(:), intent(in) :: subpackages !< array of subpackage types, such as ["TVK6", "OBS6"]
    integer(I4B), intent(in) :: iout !< unit number for output
    type(ModflowInputType) :: mf6_input
    type(PackageLoad) :: pkgloader

    mf6_input = getModflowInput(filetype, component_type, &
                                subcomponent_type, component_name, &
                                subcomponent_name, subpackages)
    !
    ! -- set mf6 parser based package loader by file type
    select case (filetype)
    case default
      pkgloader%load_package => generic_mf6_load
    end select
    !
    ! -- invoke the selected load routine
    call pkgloader%load_package(parser, mf6_input, iout)
    !
    ! -- release allocated memory
    call mf6_input%destroy()
  end subroutine input_load

end module IdmMf6FileLoaderModule

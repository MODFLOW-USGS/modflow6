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

    call idm_load(parser, mf6_input%pkgtype, &
                  mf6_input%component_type, mf6_input%subcomponent_type, &
                  mf6_input%component_name, mf6_input%subcomponent_name, &
                  iout)

  end subroutine generic_mf6_load

  !> @brief input load for traditional mf6 simulation input file
  !<
  subroutine input_load(pkgtype, &
                        component_type, subcomponent_type, &
                        component_name, subcomponent_name, &
                        inunit, iout)
    character(len=*), intent(in) :: pkgtype !< pkgtype to load, such as DIS6, DISV6, NPF6
    character(len=*), intent(in) :: component_type !< component type, such as GWF or GWT
    character(len=*), intent(in) :: subcomponent_type !< subcomponent type, such as DIS or NPF
    character(len=*), intent(in) :: component_name !< component name, such as MYGWFMODEL
    character(len=*), intent(in) :: subcomponent_name !< subcomponent name, such as MYWELLPACKAGE
    integer(I4B), intent(in) :: inunit !< unit number for input
    integer(I4B), intent(in) :: iout !< unit number for output
    type(BlockParserType), allocatable :: parser !< block parser
    type(ModflowInputType) :: mf6_input
    type(PackageLoad) :: pkgloader
    !
    ! -- create description of input
    mf6_input = getModflowInput(pkgtype, component_type, &
                                subcomponent_type, component_name, &
                                subcomponent_name)
    !
    ! -- set mf6 parser based package loader by file type
    select case (pkgtype)
    case default
      allocate (parser)
      call parser%Initialize(inunit, iout)
      pkgloader%load_package => generic_mf6_load
    end select
    !
    ! -- invoke the selected load routine
    call pkgloader%load_package(parser, mf6_input, iout)
    !
    ! -- close files and deallocate
    if (allocated(parser)) then
      !call parser%clear()
      deallocate (parser)
    end if
    !
    ! -- return
    return
  end subroutine input_load

end module IdmMf6FileLoaderModule

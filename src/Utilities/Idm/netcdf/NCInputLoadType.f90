!> @brief This module contains the NCInputLoadTypeModule
!!
!! This module defines an abstract type that support generic
!! IDP dynamic input loading for MODFLOW 6 netcdf files.
!!
!<
module NCInputLoadTypeModule

  use KindModule, only: DP, I4B, LGP
  use InputLoadTypeModule, only: DynamicPkgLoadType
  use NCModelInputsModule, only: NCModelPackageInputType
  use ModflowInputModule, only: ModflowInputType

  implicit none
  private
  public :: NCDynamicPkgLoadBaseType

  !> @brief base abstract type for netcdf source dynamic load
  !<
  type, abstract, extends(DynamicPkgLoadType) :: NCDynamicPkgLoadBaseType
  contains
    procedure(nc_period_load_if), deferred :: rp
    procedure(nc_validate_if), deferred :: validate
  end type NCDynamicPkgLoadBaseType

  abstract interface
    subroutine nc_period_load_if(this, ncid, ncpkg)
      import NCDynamicPkgLoadBaseType, NCModelPackageInputType, I4B
      class(NCDynamicPkgLoadBaseType), intent(inout) :: this
      integer(I4B), intent(in) :: ncid
      type(NCModelPackageInputType), pointer, intent(inout) :: ncpkg
    end subroutine
    subroutine nc_validate_if(this, ncpkg)
      import NCDynamicPkgLoadBaseType, NCModelPackageInputType, I4B
      class(NCDynamicPkgLoadBaseType), intent(inout) :: this
      type(NCModelPackageInputType), pointer, intent(inout) :: ncpkg
    end subroutine
  end interface

end module NCInputLoadTypeModule

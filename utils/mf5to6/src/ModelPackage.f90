module ModelPackageModule

  use ConstantsModule, only: LENMODELNAME, LENPACKAGENAME
  use ListModule, only: ListType
  use SimListVariablesModule, only: ModelPacks

  implicit none

  private
  public :: ModelPackageType, ConstructModelPackageType, GetModelPack

  ! Type to contain information on each package
  type :: ModelPackageType
    integer :: IGrid
    character(len=LENMODELNAME) :: ModelName = ''
    character(len=LENPACKAGENAME) :: PackageName = ''
  end type ModelPackageType

  interface GetModelPack
    module procedure GetModelPackByIndex, GetModelPackByPackage
  end interface GetModelPack

contains

  function ConstructModelPackageType(igrid, modelName, packageName) &
               result (newModPkg)
    ! dummy
    integer, intent(in) :: igrid
    character(len=*), intent(in) :: modelName, packageName
    type(ModelPackageType), pointer :: newModPkg
    !
    allocate(newModPkg)
    newModPkg%IGrid = igrid
    newModPkg%ModelName = modelName
    newModPkg%PackageName = packageName
    call AddModelPackageToList(ModelPacks, newModPkg)
    !
    return
  end function ConstructModelPackageType

  subroutine AddModelPackageToList(modpkglist, modpkg)
    ! dummy
    type(ListType) :: modpkglist
    type(ModelPackageType), pointer, intent(inout) :: modpkg
    ! local
    class(*), pointer :: obj => null()
    !
    obj => modpkg
    call modpkglist%Add(obj)
    !
    return
  end subroutine AddModelPackageToList

  function GetModelPackageFromList(modpkgList, idx) result (res)
    implicit none
    ! dummy
    type(ListType) :: modpkgList
    integer, intent(in) :: idx
    type(ModelPackageType), pointer :: res
    ! local
    class(*), pointer :: obj => null()
    !
    obj => modpkgList%GetItem(idx)
    res => CastAsModelPackageType(obj)
    !
    return
  end function GetModelPackageFromList

  function CastAsModelPackageType(obj) result(res)
    ! dummy
    class(*), pointer, intent(inout) :: obj
    type(ModelPackageType), pointer :: res
    !
    res => null()
    select type(obj)
    type is (ModelPackageType)
      res => obj
    end select
    !
    return
  end function CastAsModelPackageType

  function GetModelPackByIndex(indx) result(res)
    ! dummy
    integer, intent(in) :: indx
    type(ModelPackageType), pointer :: res
    !
    res => GetModelPackageFromList(ModelPacks, indx)
    !
    return
  end function GetModelPackByIndex

  function GetModelPackByPackage(packageName) result(res)
    ! dummy
    character(len=*), intent(in) :: packageName
    type(ModelPackageType), pointer :: res
    ! local
    integer :: i, n
    type(ModelPackageType), pointer :: modpack
    !
    res => null()
    n = ModelPacks%Count()
    loop: do i=1,n
      modpack => GetModelPackByIndex(i)
      if (modpack%PackageName == packageName) then
        res => modpack
        exit loop
      endif
    enddo loop
    !
    return
  end function GetModelPackByPackage

end module ModelPackageModule

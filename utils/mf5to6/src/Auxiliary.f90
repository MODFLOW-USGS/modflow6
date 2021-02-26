module AuxiliaryModule
  
  use ConstantsModule, only: LENAUXNAME
  use ListModule, only: ListType
  
  private
  public :: AuxiliaryType, CastAsAuxiliaryType, ConstructAuxiliaryType, &
            AddAuxiliaryToList, GetAuxiliaryFromList
  
  type :: AuxiliaryType
    character(len=LENAUXNAME) :: Name
    real                      :: Value
  end type AuxiliaryType
  
  interface ConstructAuxiliaryType
    module procedure ConstructAuxiliaryTypeBlank, &
                     ConstructAuxiliaryTypePop
  end interface ConstructAuxiliaryType
  
contains

  function CastAsAuxiliaryType(obj) result(res)
    implicit none
    ! dummy
    class(*), pointer :: obj
    type(AuxiliaryType), pointer :: res
    ! local
    !
    res => null()
    select type(obj)
    type is (AuxiliaryType)
      res => obj
    end select
    !
    return
  end function CastAsAuxiliaryType
  
  subroutine ConstructAuxiliaryTypeBlank(newAuxiliary)
    implicit none
    ! dummy
    type(AuxiliaryType), pointer :: newAuxiliary
    !
    allocate(newAuxiliary)
    newAuxiliary%Name = ''
    newAuxiliary%Value = 0.0
    !
    return
  end subroutine ConstructAuxiliaryTypeBlank
  
  subroutine ConstructAuxiliaryTypePop(newAuxiliary, name, value)
    implicit none
    ! dummy
    type(AuxiliaryType), pointer :: newAuxiliary
    character(len=*) :: name
    real :: value
    !
    allocate(newAuxiliary)
    newAuxiliary%Name = name
    newAuxiliary%Value = value
    !
    return
  end subroutine ConstructAuxiliaryTypePop
  
  subroutine AddAuxiliaryToList(list, auxiliary)
    implicit none
    ! dummy
    type(ListType), pointer :: list
    type(AuxiliaryType), pointer :: auxiliary
    ! local
    class(*), pointer :: obj => null()
    !
    obj => auxiliary
    call list%Add(obj)
    !
    return
  end subroutine AddAuxiliaryToList
  
  function GetAuxiliaryFromList(list, idx) result(res)
    implicit none
    ! dummy
    type(ListType), pointer :: list
    integer, intent(in) :: idx
    type(AuxiliaryType), pointer :: res
    ! local
    class(*), pointer :: obj => null()
    !
    res => null()
    obj => list%GetItem(idx)
    select type(obj)
    type is (AuxiliaryType)
      res => obj
    end select
    !
    return
  end function GetAuxiliaryFromList
  
end module AuxiliaryModule

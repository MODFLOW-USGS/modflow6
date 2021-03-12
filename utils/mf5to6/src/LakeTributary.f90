module LakeTributaryModule

  use ConstantsModule, only: DZERO, LENPACKAGENAME
  use ListModule, only: ListType
  
  private
  public :: LakeTributaryType, ConstructLakeTributary, &
            CastAsLakeTributaryType, AddLakeTributaryToList, &
            GetTributaryFromList
  
  type LakeTributaryType
    integer                       :: iTribNum = 0
    integer                       :: LakeOut = 0
    integer                       :: iSegnum = 0
    integer                       :: IgridProv = 0
    character(len=LENPACKAGENAME) :: provPkgName = ''
  end type LakeTributaryType
  
contains

  ! Non-type-bound procedures

  subroutine ConstructLakeTributary(newLakeTrib)
    ! dummy
    type(LakeTributaryType), pointer :: newLakeTrib
    !
    allocate(newLakeTrib)
    !
    return
  end subroutine ConstructLakeTributary
  
  function CastAsLakeTributaryType(obj) result (res)
    ! dummy
    class(*), pointer, intent(inout) :: obj
    type(LakeTributaryType), pointer :: res
    !
    res => null()
    select type (obj)
    type is (LakeTributaryType)
      res => obj
    end select
    !
    return
  end function CastAsLakeTributaryType

  subroutine AddLakeTributaryToList(list, lakeTrib)
    implicit none
    ! dummy
    type(ListType), pointer :: list
    type(LakeTributaryType), pointer :: lakeTrib
    ! local
    class(*), pointer :: obj => null()
    !
    obj => lakeTrib
    call list%Add(obj)
    !
    return
  end subroutine AddLakeTributaryToList

  function GetTributaryFromList(list, idx) result (res)
    implicit none
    ! dummy
    type(ListType), pointer :: list
    integer, intent(in) :: idx
    type(LakeTributaryType), pointer :: res
    ! local
    class(*), pointer :: obj => null()
    !
    obj => list%GetItem(idx)
    res => CastAsLakeTributaryType(obj)
    !
    return
  end function GetTributaryFromList
  
end module LakeTributaryModule

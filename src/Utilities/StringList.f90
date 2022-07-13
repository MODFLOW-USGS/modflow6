module StringListModule

  use KindModule, only: DP, I4B
  use ListModule, only: ListType

  private
  public :: AddStringToList, GetStringFromList

  type :: CharacterContainerType
    character(len=:), allocatable :: charstring
  end type CharacterContainerType

contains

  subroutine ConstructCharacterContainer(newCharCont, text)
    implicit none
    type(CharacterContainerType), pointer, intent(out) :: newCharCont
    character(len=*), intent(in) :: text
    !
    allocate (newCharCont)
    newCharCont%charstring = text
    return
  end subroutine ConstructCharacterContainer

  function CastAsCharacterContainerType(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    type(CharacterContainerType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    type is (CharacterContainerType)
      res => obj
    end select
    return
  end function CastAsCharacterContainerType

  subroutine AddStringToList(list, string)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    character(len=*), intent(in) :: string
    ! -- local
    class(*), pointer :: obj
    type(CharacterContainerType), pointer :: newCharacterContainer
    !
    newCharacterContainer => null()
    call ConstructCharacterContainer(newCharacterContainer, string)
    if (associated(newCharacterContainer)) then
      obj => newCharacterContainer
      call list%Add(obj)
    end if
    !
    return
  end subroutine AddStringToList

  function GetStringFromList(list, indx) result(string)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: indx
    character(len=:), allocatable :: string
    ! -- local
    class(*), pointer :: obj
    type(CharacterContainerType), pointer :: charcont
    !
    string = ''
    obj => list%GetItem(indx)
    charcont => CastAsCharacterContainerType(obj)
    if (associated(charcont)) then
      string = charcont%charstring
    end if
    !
    return
  end function GetStringFromList

end module StringListModule

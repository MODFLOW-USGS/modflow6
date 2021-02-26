module CharacterContainerModule
  
  use ConstantsModule, only: LINELENGTH
  use ListModule, only: ListType

  private
  public :: CharacterContainerType, CastAsCharacterContainerType, &
            ConstructCharacterContainer, AddStringToList, GetStringFromList

  type :: CharacterContainerType
    character(len=LINELENGTH) :: charstring
  end type CharacterContainerType

contains

  subroutine ConstructCharacterContainer (newCharCont, text)
    implicit none
    type(CharacterContainerType), pointer, intent(out) :: newCharCont
    character(len=*), intent(in) :: text
    !
    allocate(newCharCont)
    newCharCont%charstring = text
    return
  end subroutine ConstructCharacterContainer

  function CastAsCharacterContainerType(obj) result (res)
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

  ! Non-type-bound procedures

  subroutine AddStringToList(list, string)
    implicit none
    ! dummy
    type(ListType), intent(inout) :: list
    character(len=*), intent(in) :: string
    ! local
    class(*), pointer :: obj => null()
    type(CharacterContainerType), pointer :: newCharacterContainer
    !
    newCharacterContainer => null()
    call ConstructCharacterContainer(newCharacterContainer, string)
    if (associated(newCharacterContainer)) then
      obj => newCharacterContainer
      call list%Add(obj)
    endif
    !
    return
  end subroutine AddStringToList
  
  function GetStringFromList(list, indx) result (string)
    implicit none
    ! dummy
    type(ListType), intent(inout) :: list
    integer, intent(in) :: indx
    character(len=:), allocatable :: string
    ! local
    class(*), pointer :: obj => null()
    type(CharacterContainerType), pointer :: charcont
    !
    string = ''
    obj => list%GetItem(indx)
    charcont => CastAsCharacterContainerType(obj)
    if (associated(charcont)) then
      string = charcont%charstring
    endif
    !
    return
  end function GetStringFromList

end module CharacterContainerModule

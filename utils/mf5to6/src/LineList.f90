module LineListModule

  use CharacterContainerModule, only: CharacterContainerType, &
          CastAsCharacterContainerType, ConstructCharacterContainer
  use ConstantsModule, only: LENBIGLINE
  use InputOutputModule, only: same_word
  use ListModule, only: ListType

  private
  public :: LineListType, same_lines

  type :: LineListType
    type(ListType), pointer, private :: List => null()
  contains
    procedure :: InitializeLineList
    procedure :: AddLine
    procedure :: Clear
    procedure :: CountLines
    procedure :: GetLine
    procedure :: Includes
  end type LineListType

contains

  ! Type-bound procedures for LineListType

  subroutine InitializeLineList(this)
    implicit none
    ! dummy
    class(LineListType) :: this
    !
    allocate(this%List)
    !
    return
  end subroutine InitializeLineList

  subroutine AddLine(this, line)
    implicit none
    ! dummy
    class(LineListType) :: this
    character(len=*), intent(in) :: line
    ! local
    class(*), pointer :: obj => null()
    type(CharacterContainerType), pointer :: charCont => null()
    !
    call ConstructCharacterContainer(charCont, line)
    obj => charCont
    call this%List%Add(obj)
    !
    return
  end subroutine AddLine

  subroutine GetLine(this, indx, line)
    implicit none
    ! dummy
    class(LineListType) :: this
    integer, intent(in) :: indx
    character(len=*) :: line
    ! local
    class(*), pointer :: obj => null()
    type(CharacterContainerType), pointer :: charCont => null()
    !
    obj => this%List%GetItem(indx)
    charCont => CastAsCharacterContainerType(obj)
    line = charCont%charstring
    !
    return
  end subroutine GetLine

  integer function CountLines(this)
    implicit none
    ! dummy
    class(LineListType) :: this
    !
    CountLines = this%List%Count()
    !
    return
  end function CountLines

  subroutine Clear(this, destroy)
    implicit none
    ! dummy
    class(LineListType) :: this
    logical, intent(in), optional :: destroy
    ! local
    logical :: destroylocal
    !
    if (present(destroy)) then
      destroylocal = destroy
    else
      destroylocal = .false.
    endif
    !
    call this%List%Clear(destroylocal)
    !
    return
  end subroutine Clear

  function Includes(this, line, caseSensitive) result(incl)
    implicit none
    ! dummy
    class(LineListType), intent(in) :: this
    character(len=*), intent(in) :: line
    logical, intent(in) :: caseSensitive
    logical :: incl
    ! local
    integer :: i, n
    character(len=LENBIGLINE) :: linelocal
    !
    incl = .false.
    n = this%List%Count()
    do i=1,n
      call this%GetLine(i,linelocal)
      if (caseSensitive) then
        if (line == linelocal) then
          incl = .true.
          exit
        endif
      else
        if (same_word(line,linelocal)) then
          incl = .true.
          exit
        endif
      endif
    enddo
    !
    return
  end function Includes

  ! Non-type-bound procedures

  logical function same_lines(listA, listB)
    implicit none
    ! dummy
    class(LineListType) :: listA, listB
    ! local
    integer :: i, kListA, kListB
    character(len=200) :: lineA, lineB
    !
    kListA = listA%CountLines()
    kListB = listB%CountLines()
    !
    same_lines = .true.
    if (kListA==kListB) then
      do i=1,kListA
        call listA%GetLine(i, lineA)
        call listB%GetLine(i, lineB)
        if (lineA /= lineB) then
          same_lines = .false.
          exit
        endif
      enddo
    else
      same_lines = .false.
    endif
    !
    return
  end function same_lines

end module LineListModule

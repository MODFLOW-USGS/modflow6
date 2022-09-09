module StructVectorModule

  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: LENMEMPATH, LENVARNAME
  use CharacterStringModule, only: CharacterStringType

  implicit none
  private
  public :: StructVectorType

  type StructVectorType
    character(len=LENVARNAME) :: varname
    character(len=LENMEMPATH) :: memoryPath
    integer(I4B) :: memtype = 0
    logical(LGP) :: preserve_case = .false.
    integer(I4B), dimension(:), pointer, contiguous :: int1d => null()
    real(DP), dimension(:), pointer, contiguous :: dbl1d => null()
    !character(len=:), dimension(:), pointer, contiguous :: str1d => null()
    type(CharacterStringType), dimension(:), pointer, contiguous :: str1d => null()
  end type StructVectorType

contains

end module StructVectorModule

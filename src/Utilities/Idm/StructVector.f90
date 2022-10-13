!> @brief This module contains the StructVectorModule
!!
!! This module contains a generic type for storing
!! different types of vectors.
!!
!<
module StructVectorModule

  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: LENMEMPATH, LENVARNAME
  use CharacterStringModule, only: CharacterStringType
  use VectorIntModule, only: VectorInt

  implicit none
  private
  public :: StructVectorType

  !> @brief derived type for generic vector
  !!
  !! This derived type is used in the StructArrayType to
  !! store any type of vector.
  !!
  !<
  type StructVectorType
    character(len=LENVARNAME) :: varname
    character(len=LENMEMPATH) :: memoryPath
    integer(I4B) :: memtype = 0
    logical(LGP) :: preserve_case = .false.
    integer(I4B), dimension(:), pointer, contiguous :: int1d => null()
    real(DP), dimension(:), pointer, contiguous :: dbl1d => null()
    type(CharacterStringType), dimension(:), pointer, contiguous :: &
      str1d => null()
    type(VectorInt), pointer :: intvector => null()
    integer(I4B), dimension(:), pointer, contiguous :: intvector_shape => null()

  end type StructVectorType

end module StructVectorModule

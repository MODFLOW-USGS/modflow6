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
  use STLVecIntModule, only: STLVecInt

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
    character(len=100) :: tagname
    character(len=LENVARNAME) :: shapevar
    character(len=LENMEMPATH) :: mempath
    integer(I4B) :: memtype = 0
    integer(I4B) :: size = 0
    logical(LGP) :: preserve_case = .false.
    integer(I4B), dimension(:), pointer, contiguous :: int1d => null()
    real(DP), dimension(:), pointer, contiguous :: dbl1d => null()
    type(CharacterStringType), dimension(:), pointer, contiguous :: &
      charstr1d => null()
    type(STLVecInt), pointer :: intvector => null()
    integer(I4B), dimension(:), pointer, contiguous :: intvector_shape => null()

  end type StructVectorType

end module StructVectorModule

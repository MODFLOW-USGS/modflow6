module CharacterStringModule
  
  implicit none
  private
  public :: CharacterStringType
  
  !> This class is used to store a single deferred-length
  !! character string.  It was designed to work in an
  !! array implementation so that a jagged character array
  !! could be used in MODFLOW and stored in the memory
  !! manager.
  !!
  !! The overloaded methods allow instances to behave like
  !! a regular string and work with intrinsic Fortran
  !! character strings.  Ideas for the implmentation were
  !! inspired by: 
  !!   https://gitlab.com/everythingfunctional/iso_varying_string
  !!
  !<
  type :: CharacterStringType
    private
    character(len=:), allocatable :: charstring
  contains
    procedure, pass(lhs) :: assign_to_charstring
    procedure, pass(rhs) :: assign_from_charstring
    procedure, pass(rhs) :: character_eq_charstring
    procedure, pass(lhs) :: charstring_eq_character
    generic :: assignment(=) => assign_to_charstring, assign_from_charstring
    generic :: operator(==) => character_eq_charstring, charstring_eq_character
  end type CharacterStringType

  contains
  
  subroutine assign_to_charstring(lhs, rhs)
    class(CharacterStringType), intent(out) :: lhs
    character(len=*), intent(in) :: rhs
    lhs%charstring = rhs
  end subroutine assign_to_charstring 

  subroutine assign_from_charstring(lhs, rhs)
    character(len=*), intent(out) :: lhs
    class(CharacterStringType), intent(in) :: rhs
    lhs = rhs%charstring
  end subroutine assign_from_charstring
  
  elemental function character_eq_charstring(lhs, rhs) result(equals)
    character(len=*), intent(in) :: lhs
    class(CharacterStringType), intent(in) :: rhs
    logical :: equals
    equals = lhs == rhs%charstring
  end function character_eq_charstring

  elemental function charstring_eq_character(lhs, rhs) result(equals)
    class(CharacterStringType), intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical :: equals
    equals = lhs%charstring == rhs
  end function charstring_eq_character
  
end module CharacterStringModule
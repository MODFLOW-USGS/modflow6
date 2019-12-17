
module BudgetTermModule

  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENBUDTXT

  implicit none

  public :: BudgetTermType
  
  type :: BudgetTermType
    
    character(len=LENBUDTXT) :: name
    integer(I4B) :: nlist
    integer(I4B) :: naux
    integer(I4B), dimension(:), pointer :: id1 => null()
    integer(I4B), dimension(:), pointer :: id2 => null()
    real(DP), dimension(:), pointer :: flow => null()
    real(DP), dimension(:, :), pointer :: auxvar => null()
    character(len=LENBUDTXT), dimension(:), allocatable :: auxtxt
    
  contains
    
  end type BudgetTermType

end module BudgetTermModule
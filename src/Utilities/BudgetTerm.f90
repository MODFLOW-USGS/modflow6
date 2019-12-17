
module BudgetTermModule

  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENBUDTXT

  implicit none

  public :: BudgetTermType
  
  type :: BudgetTermType
    
    character(len=LENBUDTXT) :: name1                        ! model
    character(len=LENBUDTXT) :: name2                        ! to model
    character(len=LENBUDTXT) :: textid1                      ! package/model
    character(len=LENBUDTXT) :: textid2                      ! to package/model
    integer(I4B) :: maxbound                                 ! allocated size of arrays
    integer(I4B) :: nbound                                   ! size of arrays for this period
    integer(I4B) :: naux                                     ! number of auxiliary variables
    integer(I4B), dimension(:), pointer :: id1 => null()     ! first id (maxbound)
    integer(I4B), dimension(:), pointer :: id2 => null()     ! second id (maxbound)
    real(DP), dimension(:), pointer :: flow => null()        ! point this to simvals or simtomvr (maxbound)
    real(DP), dimension(:, :), pointer :: auxvar => null()   ! auxiliary variables (naux, maxbound)
    character(len=LENBUDTXT), dimension(:), allocatable :: auxtxt
    
  contains
    
  end type BudgetTermType

end module BudgetTermModule
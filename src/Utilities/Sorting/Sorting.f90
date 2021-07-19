module Sorting
contains
  ! Sort an array of integers
  subroutine SortGridCells(lessThan, arraySize, array)
      interface
        logical function lessThan(a,b)
          integer, intent(in) :: a,b
        endfunction lessThan
      endinterface
      integer, intent(in) :: arraySize
      integer, intent(inout), dimension(arraySize) :: array
      integer :: QSORT_THRESHOLD = 8

      include "qsort_inline.inc"      
      contains
        subroutine init()
        end subroutine init

        subroutine swap(a,b)
          integer, intent(in) :: a,b
          integer :: hold
          hold=array(a)
          array(a)=array(b)
          array(b)=hold
        end subroutine swap
        
      ! circular shift-right by one:
        subroutine rshift(left,right)
          integer, intent(in) :: left, right
          integer :: hold
          hold=array(right)
          array(left+1:right)=array(left:right-1)
          array(left)=hold
        end subroutine rshift
  end subroutine SortGridCells
end module Sorting

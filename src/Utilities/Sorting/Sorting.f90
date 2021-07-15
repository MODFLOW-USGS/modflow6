module Sorting
contains
  ! Sort an array of integers
  subroutine SortGridCells(arraySize, array)
      integer, intent(in) :: arraySize
      integer, intent(inout), dimension(arraySize) :: array
      integer :: QSORT_THRESHOLD = 8
      include "qsort_inline.inc"      
      contains
        subroutine init()
        end subroutine init

        logical function less_than(a,b)
          integer, intent(in) :: a,b
          if ( array(a) == array(b) ) then
            less_than = a < b
          else
            less_than = array(a) < array(b)
          end if
        end function less_than

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

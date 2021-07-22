module GridSorting  
  use KindModule, only: I4B, DP, LGP
  use TopologyModule, only: GlobalCellType
  use GenericUtilitiesModule, only: is_same
  implicit none
  private

  public :: quickSortGrid

contains
  ! Sort an array of integers
  subroutine quickSortGrid(array, arraySize, idxToGlobal)
      integer, intent(inout), dimension(:) :: array      
      integer, intent(in) :: arraySize
      type(GlobalCellType), dimension(:), pointer :: idxToGlobal
      ! local
      integer :: QSORT_THRESHOLD = 8
      include "qsort_inline.inc"
          
      contains
        subroutine init()
        end subroutine init

        ! Compare two grid cells, this doesn't work as
        ! smooth for staggered discretizations though...
        function lessThan(n, m) result(isLess)
          integer(I4B), intent(in) :: n
          integer(I4B), intent(in) :: m
          logical(LGP) :: isLess
          ! local
          type(GlobalCellType), pointer :: gcn, gcm
          real(DP) :: xn, yn, zn, xm, ym, zm
            
          ! get coordinates
          gcn => idxToGlobal(array(n))
          gcm => idxToGlobal(array(m))
  
          call gcn%model%dis%get_cellxy(gcn%index, xn, yn)
          xn = xn + gcn%model%dis%xorigin
          yn = yn + gcn%model%dis%yorigin
          zn = gcn%model%dis%top(gcn%index)
  
          call gcm%model%dis%get_cellxy(gcm%index, xm, ym)
          xm = xm + gcm%model%dis%xorigin
          ym = ym + gcm%model%dis%yorigin
          zm = gcm%model%dis%top(gcm%index)
  
          ! compare
          if (.not. is_same(zn, zm, epsilon(zn))) then
            isLess = zn > zm
          else if (.not. is_same(yn, ym, epsilon(yn))) then
            isLess = yn > ym
          else if (.not. is_same(xn, xm, epsilon(xn))) then
            isLess = xn < xm
          else
            isLess = .false.
          end if

        end function lessThan

        ! swap indices
        subroutine swap(a,b)
          integer, intent(in) :: a,b
          integer :: hold

          hold=array(a)
          array(a)=array(b)
          array(b)=hold

        end subroutine swap
        
        ! circular shift-right by one
        subroutine rshift(left,right)
          integer, intent(in) :: left, right
          integer :: hold

          hold=array(right)
          array(left+1:right)=array(left:right-1)
          array(left)=hold

        end subroutine rshift
  end subroutine quickSortGrid
end module GridSorting

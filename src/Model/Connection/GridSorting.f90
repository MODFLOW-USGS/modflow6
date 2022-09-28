module GridSorting
  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: DHALF
  use CellWithNbrsModule, only: GlobalCellType
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
      real(DP) :: xnloc, ynloc, xmloc, ymloc
      real(DP) :: xn, yn, zn, xm, ym, zm
      real(DP), dimension(:), pointer, contiguous :: dis_top_n, dis_bot_n, &
                                                     dis_top_m, dis_bot_m

      ! get coordinates
      gcn => idxToGlobal(array(n))
      gcm => idxToGlobal(array(m))

      ! load model data
      ! TODO_MJR: we should probably cache this
      call gcn%dmodel%load(dis_top_n, 'TOP', 'DIS')
      call gcn%dmodel%load(dis_bot_n, 'BOT', 'DIS')
      call gcm%dmodel%load(dis_top_m, 'TOP', 'DIS')
      call gcm%dmodel%load(dis_bot_m, 'BOT', 'DIS')

      ! convert coordinates
      call gcn%model%dis%get_cellxy(gcn%index, xnloc, ynloc)
      call gcn%model%dis%transform_xy(xnloc, ynloc, xn, yn)
      zn = DHALF * (dis_top_n(gcn%index) + &
                    dis_bot_n(gcn%index))

      call gcm%model%dis%get_cellxy(gcm%index, xmloc, ymloc)
      call gcm%model%dis%transform_xy(xmloc, ymloc, xm, ym)
      zm = DHALF * (dis_top_m(gcm%index) + &
                    dis_bot_m(gcm%index))

      ! compare
      if (.not. is_same(zn, zm, 10 * epsilon(zn))) then
        isLess = zn > zm
      else if (.not. is_same(yn, ym, 10 * epsilon(yn))) then
        isLess = yn > ym
      else if (.not. is_same(xn, xm, 10 * epsilon(xn))) then
        isLess = xn < xm
      else
        isLess = .false.
      end if

    end function lessThan

    ! swap indices
    subroutine swap(a, b)
      integer, intent(in) :: a, b
      integer :: hold

      hold = array(a)
      array(a) = array(b)
      array(b) = hold

    end subroutine swap

    ! circular shift-right by one
    subroutine rshift(left, right)
      integer, intent(in) :: left, right
      integer :: hold

      hold = array(right)
      array(left + 1:right) = array(left:right - 1)
      array(left) = hold

    end subroutine rshift
  end subroutine quickSortGrid
end module GridSorting

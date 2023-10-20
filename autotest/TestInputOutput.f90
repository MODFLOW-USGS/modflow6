module TestInputOutput
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use ConstantsModule, only: LINELENGTH
  use InputOutputModule, only: get_node, get_ijk
  implicit none
  private
  public :: collect_inputoutput

contains

  subroutine collect_inputoutput(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("get_node_get_ijk", test_get_node_get_ijk) &
                ]
  end subroutine collect_inputoutput

  subroutine test_get_node_get_ijk(error)
    type(error_type), allocatable, intent(out) :: error
    integer :: ilay
    integer :: irow
    integer :: icol
    integer :: nlay
    integer :: nrow
    integer :: ncol
    integer :: nnum
    integer :: ncls
    integer :: k, i, j

    ! trivial grid with 1 cell
    nnum = get_node(1, 1, 1, 1, 1, 1)
    call get_ijk(nnum, 1, 1, 1, ilay, irow, icol)
    call check(error, nnum == 1)
    call check(error, ilay == 1)
    call check(error, irow == 1)
    call check(error, icol == 1)
    if (allocated(error)) return

    ! small grid, 3x4x5
    nlay = 3
    nrow = 4
    ncol = 5
    ncls = nlay * nrow * ncol
    do k = 1, nlay
      do i = 1, nrow
        do j = 1, ncol
          ! node number from ijk
          nnum = get_node(k, i, j, nlay, nrow, ncol)
          call check(error, nnum == (k - 1) * nrow * ncol + (i - 1) * ncol + j)
          if (allocated(error)) return

          ! ijk from node number
          call get_ijk(nnum, nrow, ncol, nlay, irow, icol, ilay)
          call check(error, ilay == k)
          call check(error, irow == i)
          call check(error, icol == j)
          if (allocated(error)) return
        end do
      end do
    end do
  end subroutine test_get_node_get_ijk

end module TestInputOutput

MODULE IMSLinearMisc

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE

  private
  public :: ims_misc_thomas

CONTAINS

  !> @brief Tridiagonal solve using the Thomas algorithm
    !!
    !! Subroutine to solve tridiagonal linear equations using the
    !! Thomas algorithm.
    !!
  !<
  subroutine ims_misc_thomas(n, tl, td, tu, b, x, w)
    implicit none
    ! -- dummy variables
    integer(I4B), intent(in) :: n !< number of matrix rows
    real(DP), dimension(n), intent(in) :: tl !< lower matrix terms
    real(DP), dimension(n), intent(in) :: td !< diagonal matrix terms
    real(DP), dimension(n), intent(in) :: tu !< upper matrix terms
    real(DP), dimension(n), intent(in) :: b !< right-hand side vector
    real(DP), dimension(n), intent(inout) :: x !< solution vector
    real(DP), dimension(n), intent(inout) :: w !< work vector
    ! -- local variables
    integer(I4B) :: j
    real(DP) :: bet
    real(DP) :: beti
    !
    ! -- initialize variables
    w(1) = DZERO
    bet = td(1)
    beti = DONE / bet
    x(1) = b(1) * beti
    !
    ! -- decomposition and forward substitution
    do j = 2, n
      w(j) = tu(j - 1) * beti
      bet = td(j) - tl(j) * w(j)
      beti = DONE / bet
      x(j) = (b(j) - tl(j) * x(j - 1)) * beti
    end do
    !
    ! -- backsubstitution
    do j = n - 1, 1, -1
      x(j) = x(j) - w(j + 1) * x(j + 1)
    end do
  end subroutine ims_misc_thomas

END MODULE IMSLinearMisc

module SVDModule
  use KindModule, only: DP, LGP, I4B
  use ConstantsModule, only: DONE, DZERO, DPREC, DSAME
  use MathUtilModule, only: eye, zeros, outer_product

  implicit none
  private

  public :: SVD2
  public :: bidiagonal_decomposition
  public :: bidiagonal_qr_decomposition

contains

  pure function HouseholderMatrix(x) result(Q)
    ! dummy
    REAL(DP), INTENT(IN) :: x(:)
    REAL(DP), allocatable, DIMENSION(:, :) :: Q
    ! locals
    REAL(DP) :: x_norm, y_norm
    REAL(DP), allocatable :: v(:)
    REAL(DP), allocatable :: w(:)

    x_norm = NORM2(x)
    y_norm = NORM2(x(2:))

    Q = Eye(SIZE(x))

    if (dabs(y_norm) < DSAME) then
      return
    end if

    v = x
    if (x(1) > 0) then
      ! Parlett (1971) suggested this modification to avoid cancellation
      v(1) = -(y_norm**2) / (X(1) + x_norm)
    else
      v(1) = v(1) - x_norm
    end if

    w = v / NORM2(v)

    Q = Q - 2.0_dp * outer_product(w, w)

  END function HouseholderMatrix

!> @brief bidiagonal matrix  decomposition
!!
!! Decompose the matrix A into a bidiagonal matrix using Householder transformations
  pure SUBROUTINE bidiagonal_decomposition(A, P, Qt)
    ! dummy
    REAL(DP), INTENT(INOUT), DIMENSION(:, :) :: A
    REAL(DP), INTENT(OUT), DIMENSION(:, :), allocatable :: P, Qt
    ! locals
    integer(I4B) :: M, N
    integer(I4B) :: I
    REAL(DP), allocatable, DIMENSION(:, :) :: Qi, Pi
    REAL(DP), allocatable, DIMENSION(:) :: h

    M = SIZE(A, DIM=1) ! Number of rows
    N = SIZE(A, DIM=2) ! Number of columns

    Qt = Eye(N)
    P = Eye(M)

    DO I = 1, min(M, N)
      ! columns
      h = A(I:M, I)
      Pi = HouseholderMatrix(h)
      A(I:M, :) = MATMUL(Pi, A(I:M, :)) ! Apply householder transformation from left
      P(:, I:M) = MATMUL(P(:, I:M), Pi)

      ! rows
      if (I < N) then
        h = A(I, I + 1:N)
        Qi = TRANSPOSE(HouseholderMatrix(h))
        A(:, I + 1:N) = MATMUL(A(:, I + 1:N), Qi) ! Apply householder transformation from right
        Qt(I + 1:N, :) = MATMUL(Qi, Qt(I + 1:N, :))
      end if

    END DO

  END SUBROUTINE bidiagonal_decomposition

  pure function GivensRotation(a, b) result(G)
    ! dummy
    REAL(DP), INTENT(IN) :: a, b
    REAL(DP), DIMENSION(2, 2) :: G
    ! locals
    REAL(DP) :: c, s, h, d

    if (abs(b) < DPREC) then
      G = Eye(2)
      return
    end if

    h = hypot(a, b)
    d = 1.0 / h
    c = abs(a) * d
    s = sign(d, a) * b

    G(1, 1) = c
    G(1, 2) = s
    G(2, 1) = -s
    G(2, 2) = c

  END function GivensRotation

  pure function compute_shift(A) result(mu)
    ! dummy
    REAL(DP), INTENT(IN), DIMENSION(:, :) :: A
    Real(DP) :: mu
    ! locals
    INTEGER(I4B) :: m, n, min_mn
    REAL(DP) T11, T12, T21, T22
    REAL(DP) dm, fmmin, fm, dn
    REAL(DP) :: mean, product, mu1, mu2

    m = SIZE(A, DIM=1) ! Number of rows
    n = SIZE(A, DIM=2) ! Number of columns

    min_mn = MIN(m, n)

    dn = A(min_mn, min_mn)
    dm = A(min_mn - 1, min_mn - 1)
    fm = A(min_mn - 1, min_mn)
    if (min_mn > 2) then
      fmmin = A(min_mn - 2, min_mn - 1)
    else
      fmmin = 0.0_DP
    end if

    T11 = dm**2 + fmmin**2
    T12 = dm * fm
    T21 = T12
    T22 = fm**2 + dn**2

    mean = (T11 + T22) / 2.0_DP
    product = T11 * T22 - T12 * T21
    mu1 = mean - SQRT(mean**2 - product)
    mu2 = mean + SQRT(mean**2 - product)
    if (abs(T22 - mu1) < abs(T22 - mu2)) then
      mu = mu1
    else
      mu = mu2
    end if

  end function compute_shift

  pure subroutine bidiagonal_qr_decomposition(A, U, VT)
    ! dummy
    REAL(DP), INTENT(INOUT), DIMENSION(:, :) :: A
    REAL(DP), INTENT(INOUT), DIMENSION(:, :) :: U, Vt
    ! locals
    INTEGER(I4B) :: m, n, I, J
    REAL(DP), DIMENSION(:, :), allocatable :: G
    REAL(DP) :: t11, t12, mu

    m = SIZE(A, DIM=1) ! Number of rows
    n = SIZE(A, DIM=2) ! Number of columns

    DO I = 1, n - 1
      J = I + 1
      if (I == 1) then
        ! For the first iteration use the implicit shift
        mu = compute_shift(A)
        ! Apply the shift to the full tri-diagonal matrix.
        t11 = A(1, 1)**2 - mu
        t12 = A(1, 1) * A(1, 2)

        G = GivensRotation(t11, t12)
      else
        G = GivensRotation(A(I - 1, I), A(I - 1, J))
      end if

      A(:, I:J) = MATMUL(A(:, I:J), transpose(G))
      Vt(I:J, :) = MATMUL(G, Vt(I:J, :))

      if (j > m) cycle
      G = GivensRotation(A(I, I), A(J, I))
      A(I:J, :) = MATMUL(G, A(I:J, :))
      U(:, I:J) = MATMUL(U(:, I:J), transpose(G))
    END DO

  END SUBROUTINE bidiagonal_qr_decomposition

  pure subroutine handle_zero_diagonal(A, U, VT)
    ! dummy
    REAL(DP), INTENT(INOUT), DIMENSION(:, :) :: A
    REAL(DP), INTENT(INOUT), DIMENSION(:, :) :: U, Vt
    ! locals
    INTEGER(I4B) :: m, n, I
    REAL(DP), DIMENSION(:, :), allocatable :: G
    INTEGER(I4B) :: zero_index

    m = SIZE(A, DIM=1) ! Number of rows
    n = SIZE(A, DIM=2) ! Number of columns

    do I = 1, MIN(m, n)
      if (abs(A(I, I)) < DPREC) then
        zero_index = I
        exit
      end if
    end do

    if (zero_index == min(n, m)) then
      ! If the zero index is the last element of the diagonal then zero out the column
      DO I = zero_index - 1, 1, -1
        G = GivensRotation(A(I, I), A(I, zero_index))
        A(:, [I, zero_index]) = MATMUL(A(:, [I, zero_index]), transpose(G))
        Vt([I, zero_index], :) = MATMUL(G, Vt([I, zero_index], :))
      END DO

    else
      ! Else zero out the row
      DO I = zero_index + 1, n
        G = GivensRotation(A(I, I), A(zero_index, I))
        A([zero_index, I], :) = MATMUL(transpose(G), A([zero_index, I], :))
        U(:, [zero_index, I]) = MATMUL(U(:, [zero_index, I]), G)
      end do
    end if
  END SUBROUTINE handle_zero_diagonal

  pure function superdiagonal_norm(A) result(norm)
    ! Calculate the infinity norm of the superdiagonal elements
    REAL(DP), INTENT(IN) :: A(:, :)
    REAL(DP) :: norm
    ! locals
    INTEGER(I4B) :: m, n, I

    m = SIZE(A, DIM=1) ! Number of rows
    n = SIZE(A, DIM=2) ! Number of columns

    norm = 0.0_DP
    DO I = 1, MIN(m, n) - 1
      norm = MAX(norm, ABS(A(I, I + 1)))
    END DO

  END function superdiagonal_norm

  pure subroutine find_nonzero_superdiagonal(A, p, q)
    ! dummy
    REAL(DP), INTENT(IN), DIMENSION(:, :) :: A
    INTEGER(I4B), INTENT(OUT) :: p, q
    ! locals
    INTEGER(I4B) :: m, n, j, min_mn

    m = SIZE(A, DIM=1) ! Number of rows
    n = SIZE(A, DIM=2) ! Number of columns

    min_mn = MIN(m, n)
    p = 1
    q = min_mn

    Do j = min_mn, 2, -1
      if (abs(A(j - 1, j)) > DPREC) then
        q = j
        exit
      end if
    end do

    Do j = q - 1, 2, -1
      if (abs(A(j - 1, j)) < DPREC) then
        p = j
        exit
      end if
    end do
  end subroutine find_nonzero_superdiagonal

  pure function has_zero_diagonal(A) result(has_zero)
    ! Check if the matrix has a zero diagonal element
    REAL(DP), INTENT(IN) :: A(:, :)
    LOGICAL(LGP) :: has_zero
    ! locals
    INTEGER(I4B) :: m, n, I

    m = SIZE(A, DIM=1) ! Number of rows
    n = SIZE(A, DIM=2) ! Number of columns

    has_zero = .FALSE.
    DO I = 1, MIN(m, n)
      if (abs(A(I, I)) < DPREC) then
        has_zero = .TRUE.
        exit
      end if
    END DO

  END function has_zero_diagonal

  pure subroutine make_matrix_square(A, Qt)
    ! dummy
    REAL(DP), INTENT(INOUT), DIMENSION(:, :) :: A
    REAL(DP), INTENT(INOUT), DIMENSION(:, :), allocatable :: Qt
    ! locals
    REAL(DP), DIMENSION(:, :), allocatable :: G
    INTEGER(I4B) :: m, n, I

    m = SIZE(A, DIM=1) ! Number of rows
    n = SIZE(A, DIM=2) ! Number of columns

    DO I = m, 1, -1
      G = GivensRotation(A(I, I), A(I, M + 1))
      A(:, [I, M + 1]) = MATMUL(A(:, [I, M + 1]), transpose(G))
      Qt([I, M + 1], :) = MATMUL(G, Qt([I, M + 1], :))
    END DO

  end subroutine make_matrix_square

  pure subroutine clean_superdiagonal(A)
    ! dummy
    REAL(DP), INTENT(INOUT), DIMENSION(:, :) :: A
    ! locals
    INTEGER(I4B) :: n, m, j

    m = SIZE(A, DIM=1) ! Number of rows
    n = SIZE(A, DIM=2) ! Number of columns

    do j = 1, min(n, m) - 1
      if (abs(A(j, j + 1)) <= DPREC * (abs(A(j, j)) + abs(A(j + 1, j + 1)))) then
        A(j, j + 1) = 0.0_DP
      end if
    end do

  end subroutine clean_superdiagonal

!> @brief Singular Value Decomposition
!!
!! This method decomposes the matrix A into U, S and VT.
!! It follows the algorithm as described by Golub and Reinsch.
!!
!! The first step is to decompose the matrix A into a bidiagonal matrix.
!! This is done using Householder transformations.
!! Then second step is to decompose the bidiagonal matrix into U, S and VT
!! by repetitively applying the QR algorithm.
!! If there is a zero on the diagonal or superdiagonal the matrix can be split
!! into two smaller matrices and the QR algorithm can be applied to the smaller
!! matrices.
!!
!! The matrix U is the eigenvectors of A*A^T
!! The matrix VT is the eigenvectors of A^T*A
!! The matrix S is the square root of the eigenvalues of A*A^T or A^T*A
!!
!<
  pure SUBROUTINE SVD2(A, U, S, VT)
    ! dummy
    REAL(DP), INTENT(IN), DIMENSION(:, :) :: A
    REAL(DP), INTENT(OUT), DIMENSION(:, :), allocatable :: U
    REAL(DP), INTENT(OUT), DIMENSION(:, :), allocatable :: S
    REAL(DP), INTENT(OUT), DIMENSION(:, :), allocatable :: VT
    ! locals
    integer(I4B) :: i, m, n
    integer(I4B) :: max_itr
    real(DP) :: error
    integer(I4B) :: r, q

    max_itr = 100

    m = SIZE(A, DIM=1) ! Number of rows
    n = SIZE(A, DIM=2) ! Number of columns
    S = A

    call bidiagonal_decomposition(S, U, Vt)

    if (n > m) then
      call make_matrix_square(S, Vt)
    end if

    do i = 1, max_itr
      call find_nonzero_superdiagonal(S, r, q)

      ! find zero diagonal
      if (has_zero_diagonal(S(r:q, r:q))) then
        ! write(*,*) 'Iteration: ', i, ' handle zero diagonal element'
        call handle_zero_diagonal(S(r:q, r:q), U(:, r:q), Vt(r:q, :))
      else
        call bidiagonal_qr_decomposition(S(r:q, r:q), U(:, r:q), Vt(r:q, :))
        call clean_superdiagonal(S(r:q, r:q))

        error = superdiagonal_norm(S)
        ! write(*,*) 'Iteration: ', i, ' Error: ', error
        if (error < DPREC) exit
      end if
    end do

  END SUBROUTINE SVD2

end module

subroutine amux ( n, x, y, a, ja, ia )

  !*****************************************************************************80
  !
  !! AMUX multiplies a CSR matrix A times a vector.
  !
  !  Discussion:
  !
  !    This routine multiplies a matrix by a vector using the dot product form.
  !    Matrix A is stored in compressed sparse row storage.
  !
  !  Modified:
  !
  !    07 January 2004
  !
  !  Author:
  !
  !    Youcef Saad
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) N, the row dimension of the matrix.
  !
  !    Input, real X(*), and array of length equal to the column dimension 
  !    of A.
  !
  !    Input, real A(*), integer ( kind = 4 ) JA(*), IA(NROW+1), the matrix in CSR
  !    Compressed Sparse Row format.
  !
  !    Output, real Y(N), the product A * X.
  !
    implicit none
  
    integer ( kind = 4 ) n
  
    real ( kind = 8 ) a(*)
    integer ( kind = 4 ) i
    integer ( kind = 4 ) ia(*)
    integer ( kind = 4 ) ja(*)
    integer ( kind = 4 ) k
    real ( kind = 8 ) t
    real ( kind = 8 ) x(*)
    real ( kind = 8 ) y(n)
  
    do i = 1, n
  !
  !  Compute the inner product of row I with vector X.
  !
      t = 0.0D+00
      do k = ia(i), ia(i+1)-1
        t = t + a(k) * x(ja(k))
      end do
  
      y(i) = t
  
    end do
  
    return
  end

subroutine dvperm ( n, x, perm )

  !*****************************************************************************80
  !
  !! DVPERM performs an in-place permutation of a real vector.
  !
  !  Discussion:
  !
  !    This routine permutes a real vector X using a permutation PERM.
  !
  !    On return, the vector X satisfies,
  !
  !      x(perm(j)) :== x(j), j = 1,2,.., n
  !
  !  Modified:
  !
  !    07 January 2004
  !
  !  Author:
  !
  !    Youcef Saad
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) N, the length of X.
  !
  !    Input/output, real X(N), the vector to be permuted.
  !
  !    Input, integer ( kind = 4 ) PERM(N), the permutation.
  !
    implicit none
  
    integer ( kind = 4 ) n
  
    integer ( kind = 4 ) ii
    integer ( kind = 4 ) init
    integer ( kind = 4 ) k
    integer ( kind = 4 ) next
    integer ( kind = 4 ) perm(n)
    real ( kind = 8 ) tmp
    real ( kind = 8 ) tmp1
    real ( kind = 8 ) x(n)
  
    init = 1
    tmp = x(init)
    ii = perm(init)
    perm(init)= -perm(init)
    k = 0
  !
  !  The main loop.
  !
   6  continue
  
     k = k + 1
  !
  !  Save the chased element.
  !
    tmp1 = x(ii)
    x(ii) = tmp
    next = perm(ii)
  
    if ( next < 0 ) then
      go to 65
    end if
  !
  !  Test for end.
  !
    if ( n < k ) then
      perm(1:n) = -perm(1:n)
      return
    end if
  
    tmp = tmp1
    perm(ii) = -perm(ii)
    ii = next
  !
  !  End of the loop.
  !
    go to 6
  !
  !  Reinitialize cycle.
  !
   65   continue
  
    init = init + 1
  
    if ( n < init ) then 
      perm(1:n) = -perm(1:n)
      return
    end if
  
    if ( perm(init) < 0 ) then
      go to 65
    end if
  
    tmp = x(init)
    ii = perm(init)
    perm(init) = -perm(init)
    go to 6
  
end subroutine dvperm

subroutine cperm ( nrow, a, ja, ia, ao, jao, iao, perm, job )

  !*****************************************************************************80
  !
  !! CPERM permutes the columns of a matrix.
  !
  !  Discussion:
  !
  !    This routine permutes the columns of a matrix a, ja, ia.
  !    The result is written in the output matrix  ao, jao, iao.
  !    cperm computes B = A P, where  P is a permutation matrix
  !    that maps column j into column perm(j), i.e., on return
  !    a(i,j) becomes a(i,perm(j)) in new matrix
  !
  !    if job=1 then ao, iao are not used.
  !    This routine is in place: ja, jao can be the same.
  !    If the matrix is initially sorted (by increasing column number)
  !    then ao,jao,iao  may not be on return.
  !
  !  Modified:
  !
  !    07 January 2004
  !
  !  Author:
  !
  !    Youcef Saad
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) NROW, the row dimension of the matrix.
  !
  !    Input, real A(*), integer ( kind = 4 ) JA(*), IA(NROW+1), the matrix in CSR
  !    Compressed Sparse Row format.
  !
  ! perm      = integer ( kind = 4 ) array of length ncol (number of columns of A
  !         containing the permutation array  the columns:
  !         a(i,j) in the original matrix becomes a(i,perm(j))
  !         in the output matrix.
  !
  ! job      = integer ( kind = 4 ) indicating the work to be done:
  !             job = 1      permute a, ja, ia into ao, jao, iao
  !                       (including the copying of real values ao and
  !                       the array iao).
  !             job /= 1 :  ignore real values ao and ignore iao.
  !
  !
  ! on return:
  !
  ! ao, jao, iao = input matrix in a, ja, ia format (array ao not needed)
  !
    implicit none
  
    integer ( kind = 4 ) nrow
  
    real ( kind = 8 ) a(*)
    real ( kind = 8 ) ao(*)
    integer ( kind = 4 ) ia(nrow+1)
    integer ( kind = 4 ) iao(nrow+1)
    integer ( kind = 4 ) ja(*)
    integer ( kind = 4 ) jao(*)
    integer ( kind = 4 ) job
    integer ( kind = 4 ) k
    integer ( kind = 4 ) nnz
    integer ( kind = 4 ) perm(*)
  
    nnz = ia(nrow+1)-1
  
    do k = 1, nnz
      jao(k) = perm(ja(k))
    end do
  !
  !  Done with the JA array.  Return if no need to touch values.
  !
    if ( job /= 1 ) then
      return
    end if
  !
  !  Else get new pointers, and copy values too.
  !
    iao(1:nrow+1) = ia(1:nrow+1)
  
    ao(1:nnz) = a(1:nnz)
  
    return
end subroutine cperm

subroutine rperm ( nrow, a, ja, ia, ao, jao, iao, perm, job )

  !*****************************************************************************80
  !
  !! RPERM permutes the rows of a matrix in CSR format.
  !
  !  Discussion:
  !
  !    This routine computes B = P*A  where P is a permutation matrix.
  !    the permutation P is defined through the array perm: for each j,
  !    perm(j) represents the destination row number of row number j.
  !
  !  Modified:
  !
  !    07 January 2004
  !
  !  Author:
  !
  !    Youcef Saad
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) NROW, the row dimension of the matrix.
  !
  !    Input, real A(*), integer ( kind = 4 ) JA(*), IA(NROW+1), the matrix in CSR
  !    Compressed Sparse Row format.
  !
  ! perm       = integer ( kind = 4 ) array of length nrow containing the 
  !    permutation arrays
  !        for the rows: perm(i) is the destination of row i in the
  !         permuted matrix.
  !         ---> a(i,j) in the original matrix becomes a(perm(i),j)
  !         in the output  matrix.
  !
  ! job      = integer ( kind = 4 ) indicating the work to be done:
  !             job = 1      permute a, ja, ia into ao, jao, iao
  !                       (including the copying of real values ao and
  !                       the array iao).
  !             job /= 1 :  ignore real values.
  !                     (in which case arrays a and ao are not needed nor
  !                      used).
  !
  !    Output, real AO(*), integer ( kind = 4 ) JAO(*), IAO(NROW+1), the permuted
  !    matrix in CSR Compressed Sparse Row format.
  !
  ! note :
  !        if (job/=1)  then the arrays a and ao are not used.
  !
    implicit none
  
    integer ( kind = 4 ) nrow
  
    real ( kind = 8 ) a(*)
    real ( kind = 8 ) ao(*)
    integer ( kind = 4 ) i
    integer ( kind = 4 ) ia(nrow+1)
    integer ( kind = 4 ) iao(nrow+1)
    integer ( kind = 4 ) ii
    integer ( kind = 4 ) j
    integer ( kind = 4 ) ja(*)
    integer ( kind = 4 ) jao(*)
    integer ( kind = 4 ) job
    integer ( kind = 4 ) k
    integer ( kind = 4 ) ko
    integer ( kind = 4 ) perm(nrow)
    logical values
  
    values = ( job == 1 )
  !
  !  Determine pointers for output matrix.
  !
    do j = 1, nrow
      i = perm(j)
      iao(i+1) = ia(j+1) - ia(j)
    end do
  !
  !  Get pointers from lengths.
  !
    iao(1) = 1
    do j = 1, nrow
      iao(j+1) = iao(j+1) + iao(j)
    end do
  !
  !  Copying.
  !
    do ii = 1, nrow
  !
  !  Old row = II is new row IPERM(II), and KO is the new pointer.
  !
      ko = iao(perm(ii))

      do k = ia(ii), ia(ii+1)-1
        jao(ko) = ja(k)
        if ( values ) then
          ao(ko) = a(k)
        end if
        ko = ko + 1
      end do
  
    end do
  
    return
end subroutine rperm

subroutine dperm ( nrow, a, ja, ia, ao, jao, iao, perm, qperm, job )

  !*****************************************************************************80
  !
  !! DPERM permutes the rows and columns of a matrix stored in CSR format. 
  !
  !  Discussion:
  !
  !    This routine computes P*A*Q, where P and Q are permutation matrices.
  !    P maps row i into row perm(i) and Q maps column j into column qperm(j).
  !    A(I,J) becomes A(perm(i),qperm(j)) in the new matrix.
  !
  !    In the particular case where Q is the transpose of P (symmetric
  !    permutation of A) then qperm is not needed.
  !    note that qperm should be of length ncol (number of columns) but this
  !    is not checked.
  !
  !    The algorithm is "in place".
  !
  !    The column indices may not be sorted on return even if they are
  !    sorted on entry.
  !
  !    In case job == 2 or job == 4, a and ao are never referred to
  !    and can be dummy arguments.
  !
  !  Modified:
  !
  !    07 January 2004
  !
  !  Author:
  !
  !    Youcef Saad
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) NROW, the order of the matrix.
  !
  !    Input, real A(*), integer ( kind = 4 ) JA(*), IA(NROW+1), the matrix in CSR
  !    Compressed Sparse Row format.
  !
  !    Input, integer ( kind = 4 ) PERM(NROW), the permutation array for the rows: PERM(I)
  !    is the destination of row I in the permuted matrix; also the destination
  !    of column I in case the permutation is symmetric (JOB <= 2).
  !
  !    Input, integer ( kind = 4 ) QPERM(NROW), the permutation array for the columns.
  !    This should be provided only if JOB=3 or JOB=4, that is, only in 
  !    the case of a nonsymmetric permutation of rows and columns. 
  !    Otherwise QPERM is a dummy argument.
  !
  ! job      = integer ( kind = 4 ) indicating the work to be done:
  ! * job = 1,2 permutation is symmetric  Ao :== P * A * transp(P)
  !             job = 1      permute a, ja, ia into ao, jao, iao
  !             job = 2 permute matrix ignoring real values.
  ! * job = 3,4 permutation is non-symmetric  Ao :== P * A * Q
  !             job = 3      permute a, ja, ia into ao, jao, iao
  !             job = 4 permute matrix ignoring real values.
  !
  !    Output, real AO(*), JAO(*), IAO(NROW+1), the permuted matrix in CSR
  !    Compressed Sparse Row format.
  !
    implicit none
  
    integer ( kind = 4 ) nrow
  
    real ( kind = 8 ) a(*)
    real ( kind = 8 ) ao(*)
    integer ( kind = 4 ) ia(nrow+1)
    integer ( kind = 4 ) iao(nrow+1)
    integer ( kind = 4 ) ja(*)
    integer ( kind = 4 ) jao(*)
    integer ( kind = 4 ) job
    integer ( kind = 4 ) locjob
    integer ( kind = 4 ) perm(nrow)
    integer ( kind = 4 ) qperm(nrow)
  !
  !  LOCJOB indicates whether or not real values must be copied.
  !
    locjob = mod ( job, 2 )
  !
  !  Permute the rows first.
  !
    call rperm ( nrow, a, ja, ia, ao, jao, iao, perm, locjob )
  !
  !  Permute the columns.
  !
    locjob = 0
  
    if ( job <= 2 ) then
      call cperm ( nrow, ao, jao, iao, ao, jao, iao, perm, locjob )
    else
      call cperm ( nrow, ao, jao, iao, ao, jao, iao, qperm, locjob )
    end if
  
    return
end subroutine dperm


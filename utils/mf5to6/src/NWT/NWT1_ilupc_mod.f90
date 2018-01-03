MODULE ilupc_mod
  !----------------------------------------------------------------------
  !                   ILU Preconditioners Module 
  !----------------------------------------------------------------------
  !    adapted from Sparskit Version 8/13/96
  ! Contents:                                                            
  ! ILUT    : Incomplete LU factorization with truncation and fill limit
  ! ILUK    : level-k ILU                                                
  ! QSPLIT  : quick split routine used by ilut to sort out the k largest 
  !           elements in absolute value                                 
  !----------------------------------------------------------------------
  IMPLICIT NONE
  PUBLIC :: ilut, iluk, fill_stor 
  PRIVATE :: qsplit

CONTAINS

  SUBROUTINE ilut(n,a,ja,ia,lfil,droptol,alu,jlu,ju,ierr)
    !                      *** ILUT preconditioner ***                     
    !      incomplete LU factorization with truncation mechanism
    ! ... Adapted from  Yousef Saad 5/90 &  revision 8/96
    !----------------------------------------------------------------------
    ! Notes:
    ! The diagonal elements of the A matrix must be nonzero
    !----------------------------------------------------------------------
    !  We recommend using a nonzero droptol
    ! (droptol=.005 or .001 usually gives good results) in ILUT. Use a large
    ! lfil whenever possible (e.g. lfil = 5 to 10). The higher lfil the 
    ! more reliable the code is. Efficiency may also be much improved.
    !---- Dual drop strategy works as follows.                             
    ! ... 1) Threshold in L and U is set by droptol. Any element whose  
    ! ...    magnitude is less than the tolerance, droptol (relative to the abs    
    ! ...    value of diagonal element in u) is dropped.                   
    ! ... 2) Keep only the largest lfil elements in the each row of L   
    ! ...    and U (excluding diagonal elements).                                           
    ! ...                                                                  
    ! Flexibility: one  can use  droptol=0  to get  a strategy  based on   
    ! keeping  the largest lfil elements in  each row  of L  and U.   Taking   
    ! droptol > 0 and lfil = n will give  the usual threshold drop strategy   
    ! (however, fill-in is then unpredictable).                            
    ! Note that lfil = n and droptol = 0 will yield the same factors as
    ! Gaussian elimination without pivoting.                      
    !----------------------------------------------------------------------
    USE machine_constants, ONLY: kdp
    USE GLOBAL, ONLY: iout
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n    ! ... The row dimension of A
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: a     ! ... Matrix A in compressed
    INTEGER, DIMENSION(:), INTENT(IN) :: ja           ! ... sparse row format
    INTEGER, DIMENSION(:), INTENT(IN) :: ia
    INTEGER, INTENT(IN) :: lfil         ! ...  fill-in parameter. Each row of L and 
                                        ! ...    of U will have a maximum of lfil elements 
                                        ! ...    (excluding the diagonal element). must be >= 0.
    DOUBLE PRECISION, INTENT(IN) :: droptol     ! ... the threshold for dropping small terms in 
                                              ! ...   the factorization. See below for details.
    DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: alu     ! ...  LU matrix stored in Modified 
                                                         ! ...    Sparse Row (MSR) format
                                                         ! ...  The diagonal (stored in alu(1:n))
                                                         ! ...   is inverted. alu(n+1) is unused. 
    INTEGER, DIMENSION(:), INTENT(OUT) :: jlu    ! ... Each i-th row of alu,jlu matrices
                                                 ! ...   contains the i-th row of L (excluding 
                                                 ! ...   the unit diagonal) followed by the i-th
                                                 ! ...   row of U.
    INTEGER, DIMENSION(:), INTENT(OUT) :: ju     ! ... The pointers to the beginning of each row 
                                                 ! ...   of U in the matrices alu,jlu
    INTEGER, INTENT(OUT) :: ierr     ! ... Error message flag
                                     ! ... 0 : successful factorizaton
                                     !    +i : zero pivot encountered at factorization step i
                                     !    -1 : input matrix may be wrong. The factorization 
                                     ! ...     process has generated a row in L or U whose 
                                     ! ...     length is greater than n
                                     !    -2 : The matrix L overflows the array alu
                                     !    -3 : The matrix U overflows the array alu
                                     !    -4 : Illegal value for lfil
                                     !    -5 : zero row encountered during factorization
    !
    ! ... Patch since automatic arrays clash with Java using Visual Fortran90 v6.0
!!$    DOUBLE PRECISION, DIMENSION(n+1), TARGET :: w     ! ... work array
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, TARGET :: w     ! ... work array
!!$    INTEGER, DIMENSION(2*n), TARGET :: jw           ! ... work array
    INTEGER, DIMENSION(:), ALLOCATABLE, TARGET :: jw           ! ... work array
    ! ...     w, jw (1:n) store the working array [1:ii-1 = L-part, ii:n = U-part]
    ! ...     jw(n+1:2n)  stores indices of nonzero elements
    DOUBLE PRECISION, DIMENSION(:), POINTER :: wp     
    INTEGER, DIMENSION(:), POINTER :: jwp           
    INTEGER :: ju0,k,j1,j2,j,ii,i,lenl,lenu,jj,jrow,jpos,len
    INTEGER :: a_err, da_err
    DOUBLE PRECISION :: fact, s, t, tnorm
    ! ... Set string for use with RCS ident command
    CHARACTER(LEN=80) :: ident_string='$RCSfile: ilupc_mod.f90,v $//$Revision: 6461 $'
    !     ------------------------------------------------------------------
    !...
    IF (lfil < 0) THEN         ! ... illegal lfil entered.
       ierr = -4
       RETURN
    END IF
    ! ... allocate work space
    ALLOCATE(w(n+1), jw(2*n),  &
         STAT=a_err)
    IF (a_err /= 0) THEN  
       PRINT *, "Array allocation failed: ilupc-ilut"
       ierr = -6
       RETURN
    ENDIF
    ! ... initialize ju0 (points to next element to be added to alu,jlu)
    ! ... and pointer array.
    ju0 = n+2
    jlu(1) = ju0
    alu = 0._kdp          !*** possibly necessary
!!$    ju = 0                ! *** possibly necessary
    ! ... initialize nonzero indicator array.
    DO  j=1,n
       jw(n+j)  = 0
    END DO
    DO  ii = 1, n             ! ... beginning of main loop.
       j1 = ia(ii)
       j2 = ia(ii+1) - 1
       tnorm = 0.0_kdp
       DO  k=j1,j2
          tnorm = tnorm+ABS(a(k))
       END DO
       IF (tnorm == 0.0_kdp) THEN              ! ... zero row encountered
          ierr = -5
          RETURN
       END IF
       tnorm = tnorm/REAL(j2-j1+1)
       ! ... unpack L-part and U-part of row of A into arrays w,jw
       lenu = 1
       lenl = 0
       jw(ii) = ii
       w(ii) = 0.0_kdp
       jw(n+ii) = ii
       DO  j = j1, j2
          k = ja(j)
          t = a(j)
          IF (k < ii) THEN
             lenl = lenl+1
             jw(lenl) = k
             w(lenl) = t
             jw(n+k) = lenl
          ELSE IF (k == ii) THEN
             w(ii) = t
          ELSE            ! ... k > ii
             lenu = lenu+1
             jpos = ii+lenu-1
             jw(jpos) = k
             w(jpos) = t
             jw(n+k) = jpos
          END IF
       END DO
       jj = 0
       len = 0
       ! ... eliminate previous rows
       DO
          jj = jj+1
          IF (jj > lenl) EXIT
          ! ... in order to do the elimination in the correct order we must select
          ! ... the smallest column index among jw(k), k=jj+1, ..., lenl.
          jrow = jw(jj)
          k = jj
          ! ... find the smallest column index
          DO  j=jj+1,lenl
             IF (jw(j) < jrow) THEN
                jrow = jw(j)
                k = j
             END IF
          END DO
          IF (k /= jj) THEN
             ! ... exchange in jw
             j = jw(jj)
             jw(jj) = jw(k)
             jw(k) = j
             ! ... exchange in jw_pointers
             jw(n+jrow) = jj
             jw(n+j) = k
             ! ... exchange in w
             s = w(jj)
             w(jj) = w(k)
             w(k) = s
          END IF
          ! ... zero out element in row by setting jw(n+jrow) to zero.
          jw(n+jrow) = 0
          ! ... get the multiplier for row to be eliminated (jrow).
          fact = w(jj)*alu(jrow)
          IF (ABS(fact) <= droptol) CYCLE
          ! ... combine current row and row jrow
          DO  k = ju(jrow), jlu(jrow+1)-1
             s = fact*alu(k)
             j = jlu(k)
             jpos = jw(n+j)
             IF (j >= ii) THEN                  ! ... dealing with upper part.
                IF (jpos == 0) THEN
                   ! ... this is a fill-in element
                   lenu = lenu+1
                   IF (lenu > n) THEN    ! ... incomprehensible error. Matrix must be wrong.
                      ierr = -1
                      RETURN
                   END IF
                   i = ii+lenu-1
                   jw(i) = j
                   jw(n+j) = i
                   w(i) = -s
                ELSE
                   ! ... this is not a fill-in element
                   w(jpos) = w(jpos) - s
                END IF
             ELSE                  ! ... dealing  with lower part.
                IF (jpos == 0) THEN
                   ! ... this is a fill-in element
                   lenl = lenl+1
                   IF (lenl > n) THEN       ! ... incomprehensible error. Matrix must be wrong.
                      ierr = -1
                      RETURN
                   END IF
                   jw(lenl) = j
                   jw(n+j) = lenl
                   w(lenl) = -s
                ELSE
                   ! ... this is not a fill-in element
                   w(jpos) = w(jpos) - s
                END IF
             END IF
          END DO
          ! ... store this pivot element -- (from left to right -- no danger of
          ! ... overlap with the working elements in L (pivots).
          len = len+1
          w(len) = fact
          jw(len)  = jrow
       END DO
       ! ... Reset double-pointer to zero (U-part)
       DO  k=1, lenu
          jw(n+jw(ii+k-1)) = 0
       END DO
       ! ... update L-matrix
       lenl = len
       len = MIN(lenl,lfil)
       ! ... sort by quick-split
       CALL qsplit(w,jw,lenl,len)
       ! ... store L-part
       DO  k=1, len
          IF (ju0 > SIZE(jlu)) THEN      ! ... insufficient storage in L.
             ierr = -2
             RETURN
          END IF
          alu(ju0) = w(k)
          jlu(ju0) = jw(k)
          ju0 = ju0+1
       END DO
       ! ... save pointer to beginning of row ii of U
       ju(ii) = ju0
       ! ... update U-matrix -- first apply dropping strategy
       len = 0
       DO k=1, lenu-1
          IF (ABS(w(ii+k)) > droptol*tnorm) THEN
             len = len+1
             w(ii+len) = w(ii+k)
             jw(ii+len) = jw(ii+k)
          END IF
       END DO
       lenu = len+1
       len = MIN(lenu,lfil)
       wp => w(ii+1:)
       jwp => jw(ii+1:)
       CALL qsplit(wp,jwp,lenu-1,len)
       ! ... copy
       t = ABS(w(ii))
       IF (len + ju0 > SIZE(jlu)) THEN    ! ... insufficient storage in U.
          ierr = -3
          RETURN
       END IF
       DO  k=ii+1,ii+len-1
          jlu(ju0) = jw(k)
          alu(ju0) = w(k)
          t = t + ABS(w(k))
          ju0 = ju0+1
       END DO
       ! ... store inverse of diagonal element of u
       IF (w(ii) == 0.0_kdp) w(ii) = (0.0001_kdp + droptol)*tnorm
       alu(ii) = 1.0_kdp/w(ii)
       ! ... set pointer to beginning of next row of U.
       jlu(ii+1) = ju0
    END DO                ! ... end main loop
    ierr = 0
    DEALLOCATE(w, jw,  &
         STAT=da_err)
    IF (da_err /= 0) THEN  
       PRINT *, "Array deallocation failed: ilupc-ilut"
       ierr = -7
    ENDIF
  END SUBROUTINE ilut

  SUBROUTINE iluk(n,a,ja,ia,lfil,alu,jlu,ju,ierr)
    ! ...                  *** ILUK preconditioner ***                     
    ! ...  incomplete LU factorization with level of fill in of K; ILU(k)
    !----------------------------------------------------------------------
    ! Notes/known bugs: Storage is not efficient.
    ! ...   For example: Only the part of the array levs associated with
    ! ...   the U-matrix is needed in the routine. So some storage can
    ! ...   be saved if needed. The levels of fills in the LU matrix are
    ! ...   output for information only -- they are not needed by LU-solve.
    !----------------------------------------------------------------------
    ! Notes:
    ! All the diagonal elements of the A matrix must be  nonzero.
    !----------------------------------------------------------------------
    USE machine_constants, ONLY: kdp
    USE global, only: iout
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n    ! ... The row dimension of A
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: a     ! ... Matrix A in compressed
    INTEGER, DIMENSION(:), INTENT(IN) :: ja           ! ... sparse row format
    INTEGER, DIMENSION(:), INTENT(IN) :: ia
    INTEGER, INTENT(IN) :: lfil         ! ...  fill-in parameter. Each row of L and 
                                        ! ...    of U will have lfil levels of fill-in 
                                        ! ...    (excluding the diagonal element). must be >= 0.
    DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: alu     ! ...  LU matrix stored in Modified 
                                                         ! ...    Sparse Row (MSR) format
                                                         ! ...  The diagonal (stored in alu(1:n))
                                                         ! ...   is inverted. alu(n+1) is unused. 
    INTEGER, DIMENSION(:), INTENT(OUT) :: jlu    ! ... Each i-th row of alu,jlu matrices
                                                 ! ...   contains the i-th row of L (excluding 
                                                 ! ...   the unit diagonal) followed by the i-th
                                                 ! ...   row of U.
    INTEGER, DIMENSION(:), INTENT(OUT) :: ju     ! ... The pointers to the beginning of each row 
                                                 ! ...   of U in the matrices alu,jlu
    INTEGER, INTENT(OUT) :: ierr     ! ... Error message flag
                                     ! ... 0 : successful factorizaton
                                     !    +i : zero pivot encountered at factorization step i
                                     !    -1 : input matrix may be wrong. The factorization 
                                     ! ...     process has generated a row in L or U whose 
                                     ! ...     length is greater than n
                                     !    -2 : The matrix L overflows the array alu
                                     !    -3 : The matrix U overflows the array alu
                                     !    -4 : Illegal value for lfil
                                     !    -5 : zero row encountered during factorization
    !
    ! ... Patch since automatic arrays clash with Java using Visual Fortran90 v6.0
!!$    DOUBLE PRECISION, DIMENSION(n) :: w     ! ... work array
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: w     ! ... work array
!!$    INTEGER, DIMENSION(3*n) :: jw           ! ... work array
    INTEGER, DIMENSION(:), ALLOCATABLE :: jw           ! ... work array
    ! ...     w, jw (1:n) store the working array [1:ii-1 = L-part, ii:n = U-part]
    ! ...     jw(n+1:2n)  stores indices of nonzero elements
!!$    INTEGER, DIMENSION(SIZE(jlu)) :: levs   ! ... work array. Level of fill for elements
    INTEGER, DIMENSION(:), ALLOCATABLE :: levs   ! ... work array. Level of fill for elements
                                            ! ...   in alu, jlu; could be passed to caller
    INTEGER :: ju0, k, j1, j2, j, ii, i, lenl, lenu, jj, jrow, jpos, n2, jlev
    INTEGER :: a_err, da_err
    DOUBLE PRECISION :: t, s, fact
    ! ... Set string for use with RCS ident command
    CHARACTER(LEN=80) :: ident_string='$RCSfile: ilupc_mod.f90,v $//$Revision: 6461 $'
    ! ... ------------------------------------------------------------------
    !...
    IF (lfil < 0) THEN         ! ... illegal lfil entered
       ierr = -4
       RETURN
    END IF
    ! ... allocate work space
    ALLOCATE(w(n), jw(3*n), levs(SIZE(jlu)),  &
         STAT=a_err)
    IF (a_err /= 0) THEN  
       PRINT *, "Array allocation failed: ilupc-iluk"
       ierr = -6
       RETURN
    ENDIF
    ! ... initialize ju0 (points to next element to be added to alu,jlu)
    ! ... and pointer array
    n2 = n+n
    ju0 = n+2
    jlu(1) = ju0
    alu = 0._kdp          !*** possibly necessary
    ju = 0                ! *** possibly necessary
    ! ... initialize nonzero indicator jw array and jw_levs subarray
    DO  j=1,2*n
       jw(j)  = 0
    END DO
    DO  ii = 1, n             ! ... beginning of main loop.
       j1 = ia(ii)
       j2 = ia(ii+1) - 1
       ! ... unpack L-part and U-part of row of A into arrays w,jw
       lenu = 1
       lenl = 0
       jw(ii) = ii
       w(ii) = 0.0_kdp
       jw(n+ii) = ii
       DO   j = j1, j2
          k = ja(j)
          t = a(j)
!!$          IF (t == 0.0_kdp) CYCLE
          IF (k < ii) THEN
             lenl = lenl+1
             jw(lenl) = k
             w(lenl) = t
             jw(n2+lenl) = 0
             jw(n+k) = lenl
          ELSE IF (k == ii) THEN
             w(ii) = t
             jw(n2+ii) = 0
          ELSE            ! ... k > ii
             lenu = lenu+1
             jpos = ii+lenu-1
             jw(jpos) = k
             w(jpos) = t
             jw(n2+jpos) = 0
             jw(n+k) = jpos
          END IF
       END DO
       jj = 0
       ! ... eliminate previous rows
       DO
          jj = jj+1
          IF (jj > lenl) EXIT
          ! ... in order to do the elimination in the correct order we must select
          ! ... the smallest column index among jw(k), k=jj+1, ..., lenl.
          jrow = jw(jj)
          k = jj
          ! ... determine smallest column index
          DO  j=jj+1,lenl
             IF (jw(j) < jrow) THEN
                jrow = jw(j)
                k = j
             END IF
          END DO
          IF (k /= jj) THEN
             ! ... exchange in jw
             j = jw(jj)
             jw(jj) = jw(k)
             jw(k) = j
             ! ... exchange in jw_pointers
             jw(n+jrow) = jj
             jw(n+j) = k
             ! ... exchange in jw_levels
             j = jw(n2+jj)
             jw(n2+jj)  = jw(n2+k)
             jw(n2+k) = j
             ! ... exchange in w
             s = w(jj)
             w(jj) = w(k)
             w(k) = s
          END IF
          ! ... zero out element in row by resetting jw(n+jrow) to zero.
          jw(n+jrow) = 0
          ! ... get the multiplier for row to be eliminated (jrow) and its level
          fact = w(jj)*alu(jrow)
          jlev = jw(n2+jj)
          IF (jlev > lfil) CYCLE
          ! ... combine current row and row jrow
          DO  k = ju(jrow), jlu(jrow+1)-1
             s = fact*alu(k)
             j = jlu(k)
             jpos = jw(n+j)
             IF (j >= ii) THEN                ! ... dealing with upper part.
                IF (jpos == 0) THEN
                   ! ... this is a fill-in element
                   lenu = lenu+1
                   IF (lenu > n) THEN    ! ... incomprehensible error. Matrix must be wrong.
                      ierr = -1
                      RETURN
                   END IF
                   i = ii+lenu-1
                   jw(i) = j
                   jw(n+j) = i
                   w(i) = -s
                   jw(n2+i) = jlev+levs(k)+1
                ELSE
                   ! ... this is not a fill-in element
                   w(jpos) = w(jpos) - s
                   jw(n2+jpos) = MIN(jw(n2+jpos),jlev+levs(k)+1)
                END IF
             ELSE                ! ... dealing with lower part.
                IF (jpos == 0) THEN
                   ! ... this is a fill-in element
                   lenl = lenl+1
                   IF (lenl > n) THEN       ! ... incomprehensible error. Matrix must be wrong.
                      ierr = -1
                      RETURN
                   END IF
                   jw(lenl) = j
                   jw(n+j) = lenl
                   w(lenl) = -s
                   jw(n2+lenl) = jlev+levs(k)+1
                ELSE
                   ! ... this is not a fill-in element
                   w(jpos) = w(jpos) - s
                   jw(n2+jpos) = MIN(jw(n2+jpos),jlev+levs(k)+1)
                END IF
             END IF
          END DO
          w(jj) = fact
          jw(jj) = jrow
       END DO
       ! ... Reset double-pointer to zero (U-part)
       DO  k=1, lenu
          jw(n+jw(ii+k-1)) = 0
       END DO
       ! ... update L-matrix
       DO  k=1, lenl
          IF (ju0 > SIZE(jlu)) THEN      ! ... insufficient storage in L.
             ierr = -2
             RETURN
          END IF
          IF (jw(n2+k) <= lfil) THEN
             alu(ju0) = w(k)
             jlu(ju0) = jw(k)
             ju0 = ju0+1
          END IF
       END DO
       ! ... save pointer to beginning of row ii of U
       ju(ii) = ju0
       ! ... update U-matrix
       DO  k=ii+1,ii+lenu-1
          IF (jw(n2+k) <= lfil) THEN
             jlu(ju0) = jw(k)
             alu(ju0) = w(k)
             levs(ju0) = jw(n2+k)
             ju0 = ju0+1
          END IF
       END DO
       ! ... store inverse of diagonal element of u
       IF (w(ii) == 0.0_kdp) THEN    ! ... zero row encountered in A or U.
          ierr = -5
          RETURN
       END IF
       alu(ii) = 1.0_kdp/w(ii)
       ! ... update pointer to beginning of next row of U.
       jlu(ii+1) = ju0
    END DO       ! ... end main loop
    ierr = 0
    DEALLOCATE(w, jw, levs,  &
         STAT=da_err)
    IF (da_err /= 0) THEN  
       PRINT *, "Array deallocation failed: ilupc-iluk"
       ierr = -7
    ENDIF
  END SUBROUTINE iluk

  SUBROUTINE qsplit(a,ind,n,ncut)
    !-----------------------------------------------------------------------
    ! ... does a quick-sort split of a real array.
    ! ... on input a(1:n). is a real array
    ! ... on output a(1:n) is permuted such that its elements satisfy:

    ! ... abs(a(i)) >= abs(a(ncut)) for i < ncut and
    ! ... abs(a(i)) <= abs(a(ncut)) for i > ncut

    ! ... ind(1:n) is an integer array which permuted in the same way as a.
    !-----------------------------------------------------------------------
    USE machine_constants, ONLY: kdp
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN OUT) :: a
    INTEGER, DIMENSION(:), INTENT(IN OUT) :: ind
    INTEGER, INTENT(IN) :: ncut
    !
    INTEGER :: j, mid
    DOUBLE PRECISION :: tmp, abskey
    INTEGER :: itmp, first, last
    ! ... Set string for use with RCS ident command
    CHARACTER(LEN=80) :: ident_string='$RCSfile: ilupc_mod.f90,v $//$Revision: 6461 $'
    ! ... ------------------------------------------------------------------
    !...
    first = 1
    last = n
    IF (ncut < first .OR. ncut > last) RETURN
    ! ... outer loop -- while mid \= ncut do
    DO
       mid = first
       abskey = ABS(a(mid))
       DO  j=first+1, last
          IF (ABS(a(j)) > abskey) THEN
             mid = mid+1
             ! ... interchange
             tmp = a(mid)
             itmp = ind(mid)
             a(mid) = a(j)
             ind(mid) = ind(j)
             a(j)  = tmp
             ind(j) = itmp
          END IF
       END DO
       ! ... interchange
       tmp = a(mid)
       a(mid) = a(first)
       a(first)  = tmp
       itmp = ind(mid)
       ind(mid) = ind(first)
       ind(first) = itmp
       IF (mid == ncut) EXIT     ! ... normal exit
       IF (mid > ncut) THEN
          last = mid-1
       ELSE
          first = mid+1
       END IF
    END DO
  END SUBROUTINE qsplit

  SUBROUTINE fill_stor(n,ja,ia,levfill,nnz_fill)
    ! ... Compute the total ILU storage needed with the k-level of fill specified
    ! ... Uses diagonal storage of the ff array of fill indices
    ! ... Fill indices are integers such that the ILU with level k fill has
    ! ...    non-zero elements at positions corresponding to positions in ff
    ! ...    where ff <= k+1
    !-----------------------------------------------------------------
!!$    USE machine_constants, ONLY: kdp
!    USE control, ONLY: ierr   RGN commented this
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n       ! ... The row dimension of A
    INTEGER, DIMENSION(:), INTENT(IN) :: ja   ! ... Column index of A in CSR format
    INTEGER, DIMENSION(:), INTENT(IN) :: ia ! ... Row index of A in CSR format
    INTEGER, INTENT(IN) :: levfill            ! ... level of fill specified
    INTEGER, INTENT(OUT) :: nnz_fill          ! ... number of array elements needed
    ! ...   to store ILU in MSR format
    !
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: ff     ! ... work array of fill levels
    INTEGER :: a_err, da_err, bw, bwl, bwu, ffmin, i, j, jp, jpt, k, t, tp 
    !
    ! ... Set string for use with RCS ident command
    CHARACTER(LEN=80) :: ident_string='$RCSfile: ilupc_mod.f90,v $//$Revision: 6461 $'
    ! ... ------------------------------------------------------------------
    !...
    ! ... Determine bandwidth and allocate ff array
    CALL bandwidth(n,ja,ia,bwl,bwu)
    bw = bwl           ! ... symmetric incidence matrix assumed
    ALLOCATE(ff(n,-bw:bw),  &
         STAT=a_err)
    IF (a_err /= 0) THEN  
       PRINT *, "Array allocation failed: fill_stor"
!       ierr(199) = .TRUE.
       RETURN
    ENDIF
    ff = 0
    ! ... Load ff with the non-zero pattern of matrix a
    DO i=1,n
       DO k=ia(i),ia(i+1)-1
          ff(i,ja(k)-i) = 1
       END DO
    END DO
    ! ... Load ff with level of fill indices
    DO i=1,n
       DO j=MAX(i-bw,1),MIN(i+bw,n)
          jp = j-i
          IF((i == 1 .OR. j == 1) .AND. ff(i,jp) == 0) THEN
             ff(i,jp) = 1000     ! ... border elements not ever filled
          ELSEIF(jp == 0) THEN
             ff(i,jp) = 0     ! ... diagonal
          ELSEIF(ff(i,jp) ==1) THEN
             CYCLE            ! ... original incidence
          ELSE     ! ... find fill level 
             ffmin = 2000
             DO t=MAX(i-bw,j-bw,1),MIN(i,j)-1
                tp = t-i
                jpt = j-t 
                ffmin = MIN(ffmin,(ff(i,tp)+ff(t,jpt)))
             END DO
             ff(i,jp) = ffmin
          END IF
       END DO
    END DO
    ! ... Restore the original incidence flag of the main diagonal for counting
    DO i=1,n
       ff(i,0) = 1
    END DO
    ! ... Count the number of non-zero elements in ff
    nnz_fill = 0
    DO i=1,n
       DO jp=MAX(i-bw,1)-i,MIN(i+bw,n)-i      ! ... don't count outside triangles
          IF(ff(i,jp) > 0 .AND. ff(i,jp) <= levfill+1) nnz_fill = nnz_fill+1
       END DO
    END DO
    DEALLOCATE(ff,  &
         stat=da_err)
    IF (da_err /= 0) THEN  
       PRINT *, "Array deallocation failed: fill_stor"
!       ierr(193) = .TRUE.
       RETURN
    ENDIF

  CONTAINS

    SUBROUTINE bandwidth(n,ja,ia,ml,mu)
      ! ... Computes the lower and upper bandwidths of matrix in CSR format
      ! ... adapted from Y.Saad Sparskit 7/2001
      !-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n       ! ... The row dimension of A
      INTEGER, DIMENSION(:), INTENT(IN) :: ja           ! ... column index array
      INTEGER, DIMENSION(:), INTENT(IN) :: ia         ! ... row index array
                                                      ! ...  for sparse row format
      INTEGER, INTENT(OUT) :: ml, mu                ! ... bandwidths; lower, upper
      !
      INTEGER :: i, jminc, jmaxc, k
      ! ... ------------------------------------------------------------------
      ml = -n
      mu = -n
      DO  i=1,n
         jminc = n
         jmaxc = 1
         DO k=ia(i),ia(i+1)-1
            jminc = MIN(jminc,ja(k))
            jmaxc = MAX(ja(k),jmaxc)
         END DO
         ml = MAX(ml,i-jminc)
         mu = MAX(mu,jmaxc-i)
      END DO
    END SUBROUTINE bandwidth

  END SUBROUTINE fill_stor

END MODULE ilupc_mod

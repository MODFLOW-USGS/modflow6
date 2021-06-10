  !       SUBSET OF SPARSKIT VERSION 2 SOURCE CODE
  !
  !  SPARSKIT VERSION 2 SUBROUTINES INCLUDED INCLUDE:
  !
  !    1 - IMSLINEARSUB_PCMILUT
  !    2 - IMSLINEARSUB_PCMILUT_LUSOL
  !    3 - IMSLINEARSUB_PCMILUT_QSPLIT
  !
  ! ----------------------------------------------------------------------
  !                   S P A R S K I T   V E R S I O N  2.
  ! ----------------------------------------------------------------------
  !
  !Latest update : Tue Mar  8 11:01:12 CST 2005
  !
  ! ----------------------------------------------------------------------
  !
  !Welcome  to SPARSKIT  VERSION  2.  SPARSKIT is  a  package of  FORTRAN
  !subroutines  for working  with  sparse matrices.  It includes  general
  !sparse  matrix  manipulation  routines  as  well as  a  few  iterative
  !solvers, see detailed description of contents below.
  !
  ! Copyright (C) 2005, the Regents of the University of Minnesota
  !
  !SPARSKIT is  free software; you  can redistribute it and/or  modify it
  !under the terms of the  GNU Lesser General Public License as published
  !by the  Free Software Foundation [version  2.1 of the  License, or any
  !later version.]
  !
  !A copy of  the licencing agreement is attached in  the file LGPL.  For
  !additional information  contact the Free Software  Foundation Inc., 59
  !Temple Place - Suite 330, Boston, MA 02111, USA or visit the web-site
  !
  ! http://www.gnu.org/copyleft/lesser.html
  !
  !
  !DISCLAIMER
  ! ---------
  !
  !SPARSKIT  is distributed  in  the hope  that  it will  be useful,  but
  !WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  !MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  !Lesser General Public License for more details.
  !
  !For more information contact saad@cs.umn.edu
  !
  !
  MODULE IMSLinearSparseKitModule
  ! -- modules
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH,                                         &
                             DZERO, DEM4, DONE,                                  &
                             VDEBUG
  use GenericUtilitiesModule, only: sim_message

  IMPLICIT NONE


  contains

  SUBROUTINE ims_sk_pcmilut(n, a, ja, ia, lfil, droptol, relax,       &
    alu, jlu, ju, iwk, w, jw, ierr,           &
    izero, delta)
  ! ----------------------------------------------------------------------
  integer(I4B) :: n
  real(DP) :: a(*),alu(*),w(n+1),droptol,relax
  integer(I4B) :: ja(*),ia(n+1),jlu(*),ju(n),jw(2*n),lfil,iwk,ierr
  integer(I4B) :: izero
  real(DP) :: delta
  ! ---------------------------------------------------------------------*
  !                      *** ILUT preconditioner ***                     *
  !      incomplete LU factorization with dual truncation mechanism      *
  ! ---------------------------------------------------------------------*
  !     Author: Yousef Saad *May, 5, 1990, Latest revision, August 1996  *
  ! ---------------------------------------------------------------------*
  ! PARAMETERS
  ! ----------
  !
  ! on entry:
  !==========
  ! n       = integer. The row dimension of the matrix A. The matrix
  !
  ! a,ja,ia = matrix stored in Compressed Sparse Row format.
  !
  ! lfil    = integer. The fill-in parameter. Each row of L and each row
  !           of U will have a maximum of lfil elements (excluding the
  !           diagonal element). lfil must be .ge. 0.
  !           ** WARNING: THE MEANING OF LFIL HAS CHANGED WITH RESPECT TO
  !           EARLIER VERSIONS.
  !
  ! droptol = real. Sets the threshold for dropping small terms
  !           in the factorization. See below for details on dropping
  !           strategy.
  !
  !
  ! iwk     = integer. The lengths of arrays alu and jlu. If the arrays
  !           are not big enough to store the ILU factorizations, ilut
  !           will stop with an error message.
  !
  ! On return:
  !===========
  !
  ! alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
  !           the L and U factors together. The diagonal (stored in
  !           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
  !           contains the i-th row of L (excluding the diagonal entry=1)
  !           followed by the i-th row of U.
  !
  ! ju      = integer array of length n containing the pointers to
  !           the beginning of each row of U in the matrix alu,jlu.
  !
  ! ierr    = integer. Error message with the following meaning.
  !           ierr  = 0    --> successful return.
  !           ierr .gt. 0  --> zero pivot encountered at step number ierr.
  !           ierr  = -1   --> Error. input matrix may be wrong.
  !                            (The elimination process has generated a
  !                            row in L or U whose length is .gt.  n.)
  !           ierr  = -2   --> The matrix L overflows the array al.
  !           ierr  = -3   --> The matrix U overflows the array alu.
  !           ierr  = -4   --> Illegal value for lfil.
  !           ierr  = -5   --> zero row encountered.
  !
  ! work arrays:
  !=============
  ! jw      = integer work array of length 2*n.
  ! w       = real work array of length n+1.
  !
  ! ---------------------------------------------------------------------
  ! w, ju (1:n) store the working array [1:ii-1 = L-part, ii:n = u]
  ! jw(n+1:2n)  stores nonzero indicators
  !
  ! Notes:
  ! ------
  ! The diagonal elements of the input matrix must be  nonzero (at least
  ! 'structurally').
  !
  ! ---------------------------------------------------------------------*
  !---- Dual drop strategy works as follows.                             *
  !                                                                      *
  !     1) Thresholding in L and U as set by droptol. Any element whose  *
  !        magnitude is less than some tolerance (relative to the abs    *
  !        value of diagonal element in u) is dropped.                   *
  !                                                                      *
  !     2) Keeping only the largest lfil elements in the i-th row of L   *
  !        and the largest lfil elements in the i-th row of U (excluding *
  !        diagonal elements).                                           *
  !                                                                      *
  ! Flexibility: one  can use  droptol=0  to get  a strategy  based on   *
  ! keeping  the largest  elements in  each row  of L  and U.   Taking   *
  ! droptol .ne.  0 but lfil=n will give  the usual threshold strategy   *
  ! (however, fill-in is then unpredictable).                            *
  ! ---------------------------------------------------------------------*
  !     locals
  character(len=LINELENGTH) :: line
  integer(I4B) :: ju0,k,j1,j2,j,ii,i,lenl,lenu,jj,jrow,jpos,ilen
  real(DP) :: tnorm, t, abs, s, fact
  real(DP) :: rs, d, sd1, tl
  !     format
  character(len=*), parameter :: fmterr = "(//,1x,a)"
  !     code
  if (lfil .lt. 0) goto 998
  ! ----------------------------------------------------------------------
  !     initialize ju0 (points to next element to be added to alu,jlu)
  !     and pointer array.
  ! ----------------------------------------------------------------------
  ju0 = n+2
  jlu(1) = ju0
  !
  !     initialize nonzero indicator array.
  !
  do j = 1, n
    jw(n+j)  = 0
  end do
  ! ----------------------------------------------------------------------
  !     beginning of main loop.
  ! ----------------------------------------------------------------------
  main: do ii = 1, n
    j1 = ia(ii)
    j2 = ia(ii+1) - 1
    rs = DZERO
    tnorm = DZERO
    do k = j1, j2
      tnorm = tnorm+abs(a(k))
    end do
    if (tnorm .eq. DZERO) goto 999
    tnorm = tnorm/real(j2-j1+1)
    !
    !     unpack L-part and U-part of row of A in arrays w
    !
    lenu = 1
    lenl = 0
    jw(ii) = ii
    w(ii) = DZERO
    jw(n+ii) = ii
    !
    do j = j1, j2
      k = ja(j)
      t = a(j)
      if (k .lt. ii) then
        lenl = lenl+1
        jw(lenl) = k
        w(lenl) = t
        jw(n+k) = lenl
      else if (k .eq. ii) then
        w(ii) = t
      else
        lenu = lenu+1
        jpos = ii+lenu-1
        jw(jpos) = k
        w(jpos) = t
        jw(n+k) = jpos
      end if
    end do
    jj = 0
    ilen = 0
    !
    !     eliminate previous rows
    !
150 jj = jj+1
    if (jj .gt. lenl) goto 160
    ! ----------------------------------------------------------------------
    !     in order to do the elimination in the correct order we must select
    !     the smallest column index among jw(k), k=jj+1, ..., lenl.
    ! ----------------------------------------------------------------------
    jrow = jw(jj)
    k = jj
    !
    !     determine smallest column index
    !
    do j = jj+1, lenl
      if (jw(j) .lt. jrow) then
        jrow = jw(j)
        k = j
      end if
    end do
    !
    if (k .ne. jj) then
      !     exchange in jw
      j = jw(jj)
      jw(jj) = jw(k)
      jw(k) = j
      !     exchange in jr
      jw(n+jrow) = jj
      jw(n+j) = k
      !     exchange in w
      s = w(jj)
      w(jj) = w(k)
      w(k) = s
    end if
    !
    !     zero out element in row by setting jw(n+jrow) to zero.
    !
    jw(n+jrow) = 0
    !
    !     get the multiplier for row to be eliminated (jrow).
    !
    fact = w(jj)*alu(jrow)
    if (abs(fact) .le. droptol) then
      rs = rs + w(jj)
      goto 150
    end if
    !
    !     combine current row and row jrow
    !
    do k = ju(jrow), jlu(jrow+1)-1
      s = fact*alu(k)
      j = jlu(k)
      jpos = jw(n+j)
      if (j .ge. ii) then
        !
        !     dealing with upper part.
        !
        if (jpos .eq. 0) then
          !
          !     this is a fill-in element
          !
          lenu = lenu+1
          if (lenu .gt. n) goto 995
          i = ii+lenu-1
          jw(i) = j
          jw(n+j) = i
          w(i) = - s
        else
          !
          !     this is not a fill-in element
          !
          w(jpos) = w(jpos) - s

        end if
      else
        !
        !     dealing  with lower part.
        !
        if (jpos .eq. 0) then
          !
          !     this is a fill-in element
          !
          lenl = lenl+1
          if (lenl .gt. n) goto 995
          jw(lenl) = j
          jw(n+j) = lenl
          w(lenl) = - s
        else
          !
          !     this is not a fill-in element
          !
          w(jpos) = w(jpos) - s
        end if
      end if
    end do
    !
    !     store this pivot element -- (from left to right -- no danger of
    !     overlap with the working elements in L (pivots).
    !
    ilen = ilen+1
    w(ilen) = fact
    jw(ilen) = jrow
    goto 150
160 continue
    !
    !     reset double-pointer to zero (U-part)
    !
    do k = 1, lenu
      jw(n+jw(ii+k-1)) = 0
    end do
    !
    !     update L-matrix
    !
    lenl = ilen
    ilen = min0(lenl,lfil)
    !
    !     sort by quick-split
    !
    call IMSLINEARSUB_PCMILUT_QSPLIT(lenl, w, jw, ilen)
    !
    !     store L-part
    !
    do k = 1, ilen
      !            if (ju0 .gt. iwk) goto 996
      if (ju0 .gt. iwk) then
        write(line, '(2i10)') ju0, iwk
        call sim_message(line, fmt=fmterr, level=VDEBUG)
        goto 996
      end if
      alu(ju0) =  w(k)
      jlu(ju0) =  jw(k)
      ju0 = ju0+1
    end do
    !
    !     save pointer to beginning of row ii of U
    !
    ju(ii) = ju0
    !
    !     update U-matrix -- first apply dropping strategy
    !
    ilen = 0
    do k = 1, lenu-1
      if (abs(w(ii+k)) .gt. droptol*tnorm) then
        ilen = ilen+1
        w(ii+ilen) = w(ii+k)
        jw(ii+ilen) = jw(ii+k)
      else
        rs = rs + w(ii+k)
      end if
    end do
    lenu = ilen+1
    ilen = min0(lenu,lfil)
    !
    call IMSLINEARSUB_PCMILUT_QSPLIT(lenu-1, w(ii+1), jw(ii+1), ilen)
    !
    !     copy
    !
    t = abs(w(ii))
    !         if (ilen + ju0 .gt. iwk) goto 997
    if (ilen + ju0 .gt. iwk) then
      write(line, '(2i10)') (ilen + ju0), iwk
      call sim_message(line, fmt=fmterr, level=VDEBUG)
      goto 997
    end if
    do k = ii+1, ii+ilen-1
      jlu(ju0) = jw(k)
      alu(ju0) = w(k)
      t = t + abs(w(k) )
      ju0 = ju0+1
    end do
    !!
    !!     add dropped terms to diagonal element
    !!
    !IF (relax > DZERO) THEN
    !  w(ii) = w(ii) + relax * rs
    !END IF
    !!
    !!     store inverse of diagonal element of u
    !!
    !if (w(ii) == DZERO) w(ii) = (DEM4 + droptol)*tnorm
    !!
    !alu(ii) = DONE / w(ii)

    !    diagonal - calculate inverse of diagonal for solution
    d   = w(ii)
    tl  = ( DONE + delta ) * d + ( relax * rs )

    !    ensure that the sign of the diagonal has not changed
    sd1 = SIGN(d,tl)
    IF (sd1.NE.d) THEN
      !  use small value if diagonal scaling is not effective for
      !    pivots that change the sign of the diagonal
      IF (izero > 1) THEN
        tl = SIGN(DONE,d) * (DEM4 + droptol) * tnorm
        !  diagonal scaling continues to be effective
      ELSE
        izero = 1
        exit main
      END IF
    END IF
    !    ensure that the diagonal is not zero
    IF (ABS(tl) == DZERO) THEN
      !  use small value if diagonal scaling is not effective
      !    zero pivots
      IF (izero > 1) THEN
        tl = SIGN(DONE,d) * (DEM4 + droptol) * tnorm
        !  diagonal scaling continues to be effective
      ELSE
        izero = 1
        exit main
      END IF
    END IF
    w(ii) = tl
    alu(ii) = DONE / w(ii)
    !
    !     update pointer to beginning of next row of U.
    !
    jlu(ii+1) = ju0
    ! ----------------------------------------------------------------------
    !     end main loop
    ! ----------------------------------------------------------------------
  end do main
  ierr = 0
  return
  !
  !     incomprehensible error. Matrix must be wrong.
  !
995 ierr = -1
  return
  !
  !     insufficient storage in L.
  !
996 ierr = -2
  return
  !
  !     insufficient storage in U.
  !
997 ierr = -3
  return
  !
  !     illegal lfil entered.
  !
998 ierr = -4
  return
  !
  !     zero row encountered
  !
999 ierr = -5
  return
  ! ---------------end-of-ilut--------------------------------------------
  ! ----------------------------------------------------------------------
  END SUBROUTINE ims_sk_pcmilut

  ! ----------------------------------------------------------------------
  SUBROUTINE ims_sk_pcmilut_lusol(n, y, x, alu, jlu, ju)
  integer(I4B) :: n
  real(DP) :: x(n), y(n), alu(*)
  integer(I4B) :: jlu(*), ju(*)
  ! ----------------------------------------------------------------------
  !
  ! This routine solves the system (LU) x = y,
  ! given an LU decomposition of a matrix stored in (alu, jlu, ju)
  ! modified sparse row format
  !
  ! ----------------------------------------------------------------------
  ! on entry:
  ! n   = dimension of system
  ! y   = the right-hand-side vector
  ! alu, jlu, ju
  !     = the LU matrix as provided from the ILU routines.
  !
  ! on return
  ! x   = solution of LU x = y.
  ! ----------------------------------------------------------------------
  !
  ! Note: routine is in place: call IMSLINEARSUB_PCMILUT_LUSOL (n, x, x, alu, jlu, ju)
  !       will solve the system with rhs x and overwrite the result on x .
  !
  ! ----------------------------------------------------------------------
  ! ------local
  !
  integer(I4B) :: i, k
  !
  ! forward solve
  !
  do i = 1, n
    x(i) = y(i)
    do k = jlu(i), ju(i)-1
      x(i) = x(i) - alu(k)* x(jlu(k))
    end do
  end do
  !
  !     backward solve.
  !
  do i = n, 1, -1
    do k = ju(i), jlu(i+1)-1
      x(i) = x(i) - alu(k)*x(jlu(k))
    end do
    x(i) = alu(i)*x(i)
  end do
  !
  return
  ! ---------------end of IMSLINEARSUB_PCMILUT_LUSOL ------------------------------------------
  ! ----------------------------------------------------------------------
  END SUBROUTINE ims_sk_pcmilut_lusol

  ! ----------------------------------------------------------------------
  SUBROUTINE IMSLINEARSUB_PCMILUT_QSPLIT(n, a, ind, ncut)
  integer(I4B) :: n
  real(DP) :: a(n)
  integer(I4B) :: ind(n), ncut
  ! ----------------------------------------------------------------------
  !     does a quick-sort split of a real array.
  !     on input a(1:n). is a real array
  !     on output a(1:n) is permuted such that its elements satisfy:
  !
  !     abs(a(i)) .ge. abs(a(ncut)) for i .lt. ncut and
  !     abs(a(i)) .le. abs(a(ncut)) for i .gt. ncut
  !
  !     ind(1:n) is an integer array which permuted in the same way as a(*
  ! ----------------------------------------------------------------------
  real(DP) :: tmp, abskey
  integer(I4B) :: itmp, first, last
  integer(I4B) :: mid
  integer(I4B) :: j
  !-----
  first = 1
  last = n
  if (ncut .lt. first .or. ncut .gt. last) return
  !
  !     outer loop -- while mid .ne. ncut do
  !
00001 mid = first
  abskey = abs(a(mid))
  do j = first+1, last
    if (abs(a(j)) .gt. abskey) then
      mid = mid+1
      !     interchange
      tmp = a(mid)
      itmp = ind(mid)
      a(mid) = a(j)
      ind(mid) = ind(j)
      a(j)  = tmp
      ind(j) = itmp
    end if
  end do
  !
  !     interchange
  !
  tmp = a(mid)
  a(mid) = a(first)
  a(first)  = tmp
  !
  itmp = ind(mid)
  ind(mid) = ind(first)
  ind(first) = itmp
  !
  !     test for while loop
  !
  if (mid .eq. ncut) return
  if (mid .gt. ncut) then
    last = mid-1
  else
    first = mid+1
  end if
  goto 1
  ! ---------------end-of-IMSLINEARSUB_PCMILUT_QSPLIT------------------------------------------
  ! ----------------------------------------------------------------------
  END SUBROUTINE IMSLINEARSUB_PCMILUT_QSPLIT

  END MODULE IMSLinearSparseKitModule


      MODULE GMRESMODULE
      USE machine_constants, ONLY: kdp
      IMPLICIT NONE
      DOUBLE PRECISION, SAVE, POINTER :: Stop_tol_gmres
      INTEGER, SAVE, POINTER :: IorderILU, Idir, Msdr
      INTEGER, SAVE, POINTER :: Ilu_method, Lev_fill
      INTEGER, SAVE, POINTER :: Maxitr_gmres, Istor_gmres
      DOUBLE PRECISION, SAVE, POINTER :: DROP_TOL
      DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER :: Alu
      INTEGER, SAVE, DIMENSION(:), POINTER :: Jlu, Ju
      END MODULE GMRESMODULE
!C------------------------------------------------------------------
      SUBROUTINE GMRES7AR(IN)
!rgn------REVISION NUMBER CHANGED TO BE CONSISTENT WITH NWT RELEASE
!rgn------NEW VERSION NUMBER 1.0.9:  July 1, 2014

      USE GLOBAL, ONLY: IOUT,STRT,IBOUND
      USE GMRESMODULE
      USE GWFNWTMODULE, ONLY: IPRNWT,NUMACTIVE,IA,JA,NJA,IFDPARAM, &
                              MxIterInner, RCloseLinear
      USE GWFNWTMODULE, ONLY: levelNWT
      use utl7module, only: urdcom, urword
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      INTRINSIC INT
      !EXTERNAL URDCOM, URWORD
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER IN
!     ------------------------------------------------------------------
!     LOCAL VARIABLES
!     ------------------------------------------------------------------
      INTEGER lloc, istart, istop, i
      CHARACTER(LEN=200) line
      double precision :: R, Stop_toldum
!     ------------------------------------------------------------------
!
!1------IDENTIFY PACKAGE AND INITIALIZE.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,'GMRES -- LINEAR SOLUTION BY GMRES PACKAGE ',  &
      /1X,'    adapted for HYDROTHERM by Kipp and others (2008)',  &
      /1X,'    using software originally developed by Saad (1990)',I3,  &
      /1X,'    using algorithms described in Saad (2003)',I3)
! ALLOCATE GMRES data
      ALLOCATE (Stop_tol_gmres,IorderILU, Idir, Msdr,Ilu_method)
      ALLOCATE (Lev_fill,Maxitr_gmres, Istor_gmres, DROP_TOL)
           
!-----GMRES INPUT

!IorderILU is type of ILU(0) factorization for preconditioner
!         1 - Standard ILU
!         2 - Modified ILU; Row-sum preserved.      
      IorderILU = 2
! ILU Method of ILU factorization for preconditioner. 
!         1 - ILU with drop tolerance and fill limit. Fill-in terms less than
!             drop tolerance times the diagonal are discarded. The number
!             of fill in terms in each row of L and U is limited to the fill limit.
!             The fill-limit largest elements are kept in the L and U factors.
!         2 - ILU(k), Order k incomplete LU factorization. Fill-in terms of higher
!             order than k in the factorization are discarded.
      Ilu_method = 2
! Lev_fill - the level of fill for method 2, the fill limit for method 1.
!           Recommended values: 5-10 for method 1, 0-2 for method 2.
      Lev_fill = 2
! Drop_tol - the drop tolerance for method 1. Default is 0.001      
      drop_tol = 0.001
! Msdr   - number of iterations between restarts of the gmres
      Msdr = 10
!         solution algorithm. Default is 5 but 10 or 20 might work
!         better for large problems.
! Stop_tol_gmres - Tolerance for convergence of the iterative solver
!         This is the residual of the linear equations scaled by
!         the norm of the rhs.
!         Usually 10^-8 to 10^-12 works ok.  
      Stop_toldum = 1.0D0-10
! Idir  - index for reordering direction permutation [1-6]
!        the actual order is not needed to be known by the
!        user. But the convergence rate will depend on the 
!        reordering for strong anisotropy in conductance or
!       cell dimensions, especially for the red-black reordering.
!        The d4zigzag is much less sensitive, which is why we
!        invented it. 
      Idir = 1
!-----GMRES INPUT
      IF ( IFDPARAM.EQ.4 )CALL URDCOM(In, Iout, line)
      lloc = 1
      i = 1
      IF ( IFDPARAM.EQ.4 ) THEN
      CALL URWORD(line, lloc, istart, istop, 2, Maxitr_gmres, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Ilu_method, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Lev_fill, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, I, Stop_toldum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Msdr, r, Iout, In)
      ELSEIF ( IFDPARAM.EQ.1 ) THEN
        Maxitr_gmres = 50
        Ilu_method = 2
        Lev_fill = 1
        Stop_toldum = 1.0e-10
        Msdr = 5
      ELSEIF ( IFDPARAM.EQ.2 ) THEN
        Maxitr_gmres = 50
        Ilu_method = 2
        Lev_fill = 1
        Stop_toldum = 1.0e-10
        Msdr = 10
      ELSEIF ( IFDPARAM.EQ.3 ) THEN
        Maxitr_gmres = 50
        Ilu_method = 2
        Lev_fill = 1
        Stop_toldum = 1.0e-10
        Msdr = 15
      END IF
 !     
      Stop_tol_gmres = Stop_toldum
      Istor_gmres = 0
! Allocate GMRES arrays.
      Istor_gmres = 4
      Istor_gmres = Istor_gmres*NJA
      ALLOCATE (Alu(Istor_gmres), Jlu(Istor_gmres), Ju(Istor_gmres))
      Jlu = 0
      Ju = 0
      Alu = 0
      !
      ! for MF5to6
      RCloseLinear = Stop_tol_gmres
      MxIterInner = Maxitr_gmres
      levelNwt = Lev_fill
      !
      RETURN
      END 
    

   SUBROUTINE gmres(n,msdr,rhs,sol,stop_tol,maxits,aa,ja,ia,alu,jlu,ju,iierr,n_iter,r_norm)
  !                 *** ILU - Preconditioned GMRES ***                  
  !----------------------------------------------------------------------
  ! This is a simple version of the ILU preconditioned GMRES algorithm. 
  ! GMRES uses the L and U matrices generated 
  ! from the subroutine ILU to precondition the GMRES algorithm.        
  ! The preconditioning is applied to the right. The stopping criterion  
  ! utilized is based simply on reducing the relative residual norm to stop_tol
  !     absolute stop_tol (eps_a) is used to handle small initial rhs.
  !                                                                      
  ! USAGE: first call ILUT or ILUK to set up preconditioner and 
  !    then call gmres.                                                    
  !----------------------------------------------------------------------
  ! adapted from  Y. Saad - 5/90
  ! see also chp.9.3.2 of Saad (2003) book for algorithm 9.5
  !----------------------------------------------------------------------
  ! subroutines called :                                           
  ! amux   : SPARSKIT routine to do the matrix*vector multiplication 
  ! lusol : combined forward and backward solves from preconditioning
  ! several BLAS1 routines                                                
  !----------------------------------------------------------------------
  USE machine_constants, ONLY: kdp, epsmac
  USE GWFNWTMODULE, ONLY: Numnonzero, Numcell
  USE GMRESMODULE, ONLY:  Istor_gmres
! RGN commented out next 2 lines until I get modules.
!  USE f_units, ONLY: fuclog, fustdout
!  USE control, ONLY: ierr, ioptpr
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n        ! ... The row dimension of A
  INTEGER, INTENT(IN) :: msdr     ! ... Size of Krylov subspace
!  DOUBLE PRECISION, DIMENSION(:), INTENT(IN OUT) :: rhs     ! ... right hand side vector
  DOUBLE PRECISION, INTENT(IN OUT) :: rhs(Numcell) 
!  DOUBLE PRECISION, DIMENSION(:), INTENT(IN OUT) :: sol     ! ... solution vector, initial guess
!                                                          ! ...   on input
  DOUBLE PRECISION, INTENT(IN OUT) :: sol(Numcell)
  DOUBLE PRECISION, INTENT(IN) :: stop_tol    ! ... tolerance for stopping criterion. Iterations stop
  ! ...   when L2-norm(current residual)/L2-norm(initial residual) <= stop_tol
  ! ...   a small absolute tolerance is also used 
  INTEGER, INTENT(IN) :: maxits     ! ... maximum number of iterations allowed
!  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: aa    ! ... Matrix A in compressed
  DOUBLE PRECISION, INTENT(IN) :: aa(Numnonzero)
!  INTEGER, DIMENSION(:), INTENT(IN) :: ja           ! ... sparse row format
  INTEGER, INTENT(IN) :: ja(Numnonzero)
!  INTEGER, DIMENSION(:), INTENT(IN) :: ia
  INTEGER, INTENT(IN) :: ia(Numcell+1)
!  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: alu     ! ...  LU matrix stored in Modified 
                                                      ! ...    Sparse Row (MSR) format
                                                      ! ...  The diagonal (stored in alu(1:n))
                                                      ! ...   is inverted. 
  DOUBLE PRECISION, INTENT(IN) :: alu(Istor_gmres)
  ! INTEGER, DIMENSION(:), INTENT(IN) :: jlu    ! ... Each i-th row of alu,jlu matrices
  ! ...   contains the i-th row of L (excluding 
  ! ...   the unit diagonal) followed by the i-th
  ! ...   row of U.
  INTEGER, INTENT(IN) :: jlu(Istor_gmres)
  !INTEGER, DIMENSION(:), INTENT(IN) :: ju     ! ... The pointers to the beginning of each row 
  ! ...   of U in the matrices alu,jlu
  INTEGER, INTENT(IN) :: ju(Istor_gmres)
  INTEGER, INTENT(OUT) :: iierr     ! ... Error message flag
  !            0 : successful convergence to solution
  !            1 : iteration limit reached without convergence
  !           -1 : initial solution gives residual of zero
  INTEGER, INTENT(OUT):: n_iter           ! ... Total iterations at convergence
  DOUBLE PRECISION, INTENT(OUT) :: r_norm    ! ... Norm of relative residual at convergence
  !
  ! ... Patch since automatic arrays clash with Java using Visual Fortran90 v6.0
!!$  DOUBLE PRECISION, DIMENSION(n,msdr+1), TARGET :: vv     ! ... work array. stores the Arnoldi
  DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE, TARGET :: vv    ! ... work array. stores the Arnoldi
                                                               ! ...   basis vectors
  !
  DOUBLE PRECISION, DIMENSION(:), POINTER :: vvp1   ! ... work array pointer to vector slice
  DOUBLE PRECISION, DIMENSION(:), POINTER :: vvp2   ! ... work array pointer to vector slice
  DOUBLE PRECISION, EXTERNAL :: ddot, dnrm2
  !
  INTEGER :: i, i1, ii, itno, j, jj, k, k1
  INTEGER :: a_err, da_err
  DOUBLE PRECISION :: eps1, gam, ro, t
!!$  DOUBLE PRECISION, DIMENSION(msdr+1,msdr) :: hh
!!$  DOUBLE PRECISION, DIMENSION(msdr) :: c, s
!!$  DOUBLE PRECISION, DIMENSION(msdr+1) :: rs
  DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: hh
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: c, s, rs, mvy
  DOUBLE PRECISION, PARAMETER :: eps_a=1.e-16_kdp   ! *** make multiple of mach eps
  ! ... Set string for use with RCS ident command
  CHARACTER(LEN=80) :: ident_string='$RCSfile: gmres.f90,v $//$Revision: 6461 $'
  !     ------------------------------------------------------------------
  !...
  ! ... comments follow Templates p.20 as closely as possible
  ! ... see also algorithm 9.5 of Saad (2003) book
  ! ... allocate work space
  ALLOCATE(vv(n,msdr+1), hh(msdr+1,msdr), c(msdr), s(msdr), rs(msdr+1), mvy(Numnonzero),  &
       STAT=a_err)
  IF (a_err /= 0) THEN  
     PRINT *, "Array allocation failed: gmres"
!     ierr(196) = .TRUE.
     RETURN
  ENDIF
  ! ...  compute initial residual vector; inside the outer loop in Templates
  vvp1 => vv(:,1)
  CALL amux(sol, vvp1, aa, ja, ia)
  DO  j=1,n
     vv(j,1) = rhs(j) - vv(j,1)      ! ... initial residual vector
  END DO
  itno = 0                           ! ... iteration counter
  DO           ! ... outer loop of iteration
     vvp1 => vv(:,1)
     ro = dnrm2(n, vvp1, 1)         ! ... current residual norm
     IF(itno == 0) THEN   ! RGN commented out next lines until I get module "control"
!        IF(ioptpr(3) == 2) THEN
!           WRITE(fuclog,2001) 'GMRES:','Iteration ',itno,  &
!                '; Norm of residual .... ',ro
!           WRITE(fustdout,2001) 'GMRES:','Iteration ',itno,  &
!                '; Norm of residual .... ',ro
!2001       FORMAT(tr5,a/tr5,a,i4,a,1pe12.5)
!        END IF
!        IF(PRESENT(n_iter)) n_iter = itno
!        IF(PRESENT(r_norm)) r_norm = 1._kdp
        r_norm = 1._kdp
        n_iter = itno
        eps1 = stop_tol*ro     ! ... for later convergence test
        eps1 = 1e-3
     END IF
     ! ... Drop this test. At steady state, null solution vector is expected.
!!$     IF(ro <= stop_tol + eps_a) THEN     ! ... residual less than tolerance, sol is sufficient
!!$        iierr = -1
!!$        RETURN
!!$     END IF
     t = 1.0_kdp/ro
     DO  j=1,n
        vv(j,1) = vv(j,1)*t       ! ...  v-1 vector; initial vector of Krylov space
     END DO
     ! ... initialize first term of rhs of hessenberg system; s in Templates
     rs(1) = ro
     DO i=1,msdr          ! ... inner loop of iteration; msdr steps between restarts
        itno = itno + 1
        i1 = i + 1
        vvp1 => vv(:,i)
        vvp2 => vv(:,i1)
        CALL lusol(vvp1, rhs, alu, jlu, ju)
        CALL amux(rhs, vvp2, aa, ja, ia)         ! ... vvp2 contains w of Templates
        ! ... Build the orthogonal Krylov vector space using modified Gram-Schmidt algorithm
        ! ...     applying the Arnoldi algorithm
        DO  j=1,i
           vvp1 => vv(:,j)
           t = ddot(n, vvp1, 1, vvp2, 1)
           hh(j,i) = t
           CALL daxpy(n, -t, vvp1, 1, vvp2, 1)
        END DO
        t = dnrm2(n, vvp2, 1)
        hh(i1,i) = t
        IF (t /= 0.0_kdp) THEN        ! ... test for breakdown but no reothorgonalization done
           t = 1.0_kdp/t
           DO   k=1,n
              vv(k,i1) = vv(k,i1)*t
           END DO
        END IF
        NULLIFY(vvp1,vvp2)
        ! ... vv vectors contain the set of orthonormal basis vectors
        !     done with modified gram schimdt and arnoldi step
        !     now  update QR factorization of hh, Hessenberg matrix,
        ! ...    using Givens rotations
        ! ... Apply previous rotations to i-th column of hh
        DO  k=2,i
           k1 = k-1
           t = hh(k1,i)
           hh(k1,i) = c(k1)*t + s(k1)*hh(k,i)
           hh(k,i) = -s(k1)*t + c(k1)*hh(k,i)
        END DO
        gam = SQRT(hh(i,i)**2 + hh(i1,i)**2)
        !     if gamma is zero then any small value will do
        !     will affect only residual estimate; seems like only hh(i,i) may be zero
        ! ...       by Greenbaum p.40. maybe breakdown makes gamma be zero.
        IF (gam <= 0.0_kdp) gam = epsmac
        ! ... calculate next rotation
        c(i) = hh(i,i)/gam       ! ... Kelley algorithm
        s(i) = hh(i1,i)/gam      ! ... more robust than Greenbaum
        rs(i1) = -s(i)*rs(i)     ! ... s in Templates
        rs(i) =  c(i)*rs(i)
        hh(i,i) = c(i)*hh(i,i) + s(i)*hh(i1,i)   ! ... in Greenbaum and Kelley not Templates
        ! ... Calculate residual norm and test for convergence
        ro = ABS(rs(i1))
  ! RGN commented out next few lines until I get "control" module
!        IF(ioptpr(3) == 2) THEN
!           WRITE(fuclog,2002) 'Iteration ',itno,  &
!                '; Norm of residual .... ',ro
!           WRITE(fustdout,2002) 'Iteration ',itno,  &
!                '; Norm of residual .... ',ro
!2002       FORMAT(tr5,a,i4,a,1pe12.5)
!        END IF
!        IF(PRESENT(n_iter)) n_iter = itno
!        IF(PRESENT(r_norm)) r_norm = (ro*stop_tol)/eps1
        n_iter = itno
        r_norm = (ro*stop_tol)/eps1
        IF (ro <= eps1 + eps_a) EXIT     ! ... convergence on relative residual 
     END DO
     i = MIN(i,msdr)
     ! ... Update step in Templates, for either i or msdr step
     ! ... First solve upper triangular Hessenberg system
     rs(i) = rs(i)/hh(i,i)
     DO  ii=2,i
        k = i-ii+1
        k1 = k+1
        t=rs(k)
        DO  j=k1,i
           t = t - hh(k,j)*rs(j)
        END DO
        rs(k) = t/hh(k,k)     ! ... rs now contains y of Templates
     END DO
     !     form linear combination of vv(*,i)'s to get updated solution
     t = rs(1)
     DO  k=1,n
        rhs(k) = t*vv(k,1)
     END DO
     DO  j=2,i
        t = rs(j)
        DO  k=1,n
           rhs(k) = rhs(k) + t*vv(k,j)     ! ... rhs contains solution to precond system
        END DO                             ! ...   without the initial guess
     END DO
     ! ... apply preconditioning operation to recover solution to original 
     ! ...     unpreconditioned problem without initial guess
     CALL lusol(rhs, mvy, alu, jlu, ju)
     ! ... add in initial guess
     DO  k=1,n
        sol(k) = sol(k) + mvy(k)     ! ... current solution
     END DO
     rhs = mvy
     IF (ro <= eps1 + eps_a) EXIT          ! ... convergence on relative residual
     IF (itno >= maxits) THEN   !   RGN changed this. Used to be IF (itno == maxits) THEN
        iierr = 1     ! ... iteration limit reached
        RETURN
     END IF
     ! ... Compute residual vector and continue; done before convergence test in Templates
     ! ... Uses results of minimization not explicit residual formula
     ! ... Seems redundant
     DO  j=1,i
        jj = i1-j+1
        rs(jj-1) = -s(jj-1)*rs(jj)
        rs(jj) = c(jj-1)*rs(jj)
     END DO
     vvp2 => vv(:,1)
     DO  j=1,i1
        t = rs(j)
        IF (j == 1)  t = t-1.0_kdp
        vvp1 => vv(:,j)
        CALL daxpy(n, t, vvp1, 1, vvp2, 1)
     END DO
  END DO                           ! ... end outer loop
  iierr = 0     ! ... convergence
  DEALLOCATE(vv, hh, c, s, rs, mvy,  &
       STAT=da_err)
  IF (da_err /= 0) THEN  
     PRINT *, "Array deallocation failed: gmres"
!     ierr(191) = .TRUE.
  ENDIF

CONTAINS

  SUBROUTINE lusol(y, x, alu, jlu, ju)
    ! ... Solves the system LU*x = y,
    ! given an LU decomposition of a matrix stored in (alu, jlu, ju)
    ! in modified sparse row format MSR
    IMPLICIT NONE
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: y     ! ... right hand side vector
    DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: x    ! ... solution vector
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: alu   ! ... LU matrix stored in Modified 
                                                      ! ...   Sparse Row (MSR) format
                                                      ! ... Provided by ILU routine
    INTEGER, DIMENSION(:), INTENT(IN) :: jlu    ! ... Each i-th row of alu,jlu matrices
    ! ...   contains the i-th row of L (excluding 
    ! ...   the unit diagonal) followed by the i-th
    ! ...   row of U.
    INTEGER, DIMENSION(:), INTENT(IN) :: ju     ! ... The pointers to the beginning of each row 
                                                ! ...   of U in the matrices alu,jlu
    !
    INTEGER :: i, k
    ! ... Set string for use with RCS ident command
    CHARACTER(LEN=80) :: ident_string='$RCSfile: gmres.f90,v $//$Revision: 6461 $'
    !     ------------------------------------------------------------------
    !...
    ! ... forward solve
    DO  i=1,n              ! ... n is known from host
       x(i) = y(i)
       DO  k=jlu(i),ju(i)-1
          x(i) = x(i) - alu(k)*x(jlu(k))
       END DO
    END DO
    ! ... backward solve
    DO  i=n,1,-1
       DO  k=ju(i),jlu(i+1)-1
          x(i) = x(i) - alu(k)*x(jlu(k))
       END DO
       x(i) = alu(i)*x(i)
    END DO
  END SUBROUTINE lusol

  SUBROUTINE amux(x,y,a,ja,ia)
    ! ... Calculates product of matrix A times vector x
    ! multiplies a matrix by a vector using the dot product form
    ! Matrix A is stored in compressed sparse row storage, CSR.
    ! ... y = A*x
    !-----------------------------------------------------------------------
    IMPLICIT NONE
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x     ! ... vector x
    DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: y    ! ... result vector y 
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: a     ! ... Matrix A in compressed
    INTEGER, DIMENSION(:), INTENT(IN) :: ja           ! ... sparse row format
    INTEGER, DIMENSION(:), INTENT(IN) :: ia
    !
    INTEGER :: i, k
    DOUBLE PRECISION :: t
    ! ... Set string for use with RCS ident command
    CHARACTER(LEN=80) :: ident_string='$RCSfile: gmres.f90,v $//$Revision: 6461 $'
    !     ------------------------------------------------------------------
    !...
    DO  i = 1,n                ! ... n is known from host
       ! ... Compute the inner product of row i with vector x
       t = 0.0_kdp
       DO  k=ia(i),ia(i+1)-1
          t = t + a(k)*x(ja(k))
       END DO
       y(i) = t
    END DO
  END SUBROUTINE amux

  END SUBROUTINE gmres
!
!
      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
!     .. Scalar Arguments ..
      DOUBLE PRECISION DA
      INTEGER INCX,INCY,N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION DX(*),DY(*)
!     ..
!
!  Purpose
!  =======
!
!     constant times a vector plus a vector.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
!
!     .. Local Scalars ..
      INTEGER I,IX,IY,M,MP1
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC MOD
!     ..
      IF (N.LE.0) RETURN
      IF (DA.EQ.0.0d0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
          DY(IY) = DY(IY) + DA*DX(IX)
          IX = IX + INCX
          IY = IY + INCY
   10 CONTINUE
      RETURN
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
   20 M = MOD(N,4)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
          DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF (N.LT.4) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
          DY(I) = DY(I) + DA*DX(I)
          DY(I+1) = DY(I+1) + DA*DX(I+1)
          DY(I+2) = DY(I+2) + DA*DX(I+2)
          DY(I+3) = DY(I+3) + DA*DX(I+3)
   50 CONTINUE
      RETURN
      END
!
!
      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
!     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION DX(*),DY(*)
!     ..
!
!  Purpose
!  =======
!
!     forms the dot product of two vectors.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
!
!    .. Local Scalars ..
      DOUBLE PRECISION DTEMP
      INTEGER I,IX,IY,M,MP1
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC MOD
!     ..
      DDOT = 0.0d0
      DTEMP = 0.0d0
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
          DTEMP = DTEMP + DX(IX)*DY(IY)
          IX = IX + INCX
          IY = IY + INCY
   10 CONTINUE
      DDOT = DTEMP
      RETURN
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
   20 M = MOD(N,5)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
          DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF (N.LT.5) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
          DTEMP = DTEMP + DX(I)*DY(I) + DX(I+1)*DY(I+1) + DX(I+2)*DY(I+2) + DX(I+3)*DY(I+3) + DX(I+4)*DY(I+4)
   50 CONTINUE
   60 DDOT = DTEMP
      RETURN
      END
 !
 !
       DOUBLE PRECISION FUNCTION DNRM2(N,X,INCX)
!     .. Scalar Arguments ..
      INTEGER INCX,N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION X(*)
!     ..
!
!  Purpose
!  =======
!
!  DNRM2 returns the euclidean norm of a vector via the function
!  name, so that
!
!     DNRM2 := sqrt( x'*x )
!
!
!  -- This version written on 25-October-1982.
!     Modified on 14-October-1993 to inline the call to DLASSQ.
!     Sven Hammarling, Nag Ltd.
!
!
!     .. Parameters ..
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION ABSXI,NORM,SCALE,SSQ
      INTEGER IX
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC ABS,SQRT
!     ..
      IF (N.LT.1 .OR. INCX.LT.1) THEN
          NORM = ZERO
      ELSE IF (N.EQ.1) THEN
          NORM = ABS(X(1))
      ELSE
          SCALE = ZERO
          SSQ = ONE
!        The following loop is equivalent to this call to the LAPACK
!        auxiliary routine:
!        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
!
          DO 10 IX = 1,1 + (N-1)*INCX,INCX
              IF (X(IX).NE.ZERO) THEN
                  ABSXI = ABS(X(IX))
                  IF (SCALE.LT.ABSXI) THEN
                      SSQ = ONE + SSQ* (SCALE/ABSXI)**2
                      SCALE = ABSXI
                  ELSE
                      SSQ = SSQ + (ABSXI/SCALE)**2
                  END IF
              END IF
   10     CONTINUE
          NORM = SCALE*SQRT(SSQ)
      END IF
!
      DNRM2 = NORM
      RETURN
!
!     End of DNRM2.
!
      END
!
     SUBROUTINE GMRES7DA(IGRID)
!  DEALLOCATE GLOBAL DATA
      USE GMRESMODULE
      INTEGER ALLOC_ERR
      DEALLOCATE(Alu, STAT = ALLOC_ERR)
      DEALLOCATE(Jlu, STAT = ALLOC_ERR)
      DEALLOCATE(Ju, STAT = ALLOC_ERR)
      RETURN
      END






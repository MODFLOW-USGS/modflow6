module MathUtilModule
  use KindModule, only: DP, I4B, LGP
  use ErrorUtilModule, only: pstop
  use ConstantsModule, only: MAXCHARLEN, LENHUGELINE, &
                             DZERO, DPREC, DSAME, &
                             LINELENGTH, LENHUGELINE, VSUMMARY

  implicit none
  private
  public :: mod_offset, is_close, zeroch, zeroin, zerotest

  interface mod_offset
    module procedure :: mod_offset_int, mod_offset_dbl
  end interface mod_offset

contains

  !> @brief Check if a real value is approximately equal to another.
  !!
  !! By default the determination is symmetric in a and b, as in
  !! Python's math.isclose, with relative difference scaled by a
  !! factor of the larger absolute value of a and b. The formula
  !! is: abs(a - b) <= max(rtol * max(abs(a), abs(b)), atol).
  !!
  !! If symmetric is set to false the test is asymmetric in a and
  !! b, with b taken to be the reference value, and the alternate
  !! formula (abs(a - b) <= (atol + rtol * abs(b))) is used. This
  !! is the approach taken by numpy.allclose.
  !!
  !! Defaults for rtol and atol are DSAME and DZERO, respectively.
  !! If a or b are near 0 (especially if either is 0), an absolute
  !! tolerance suitable for the particular case should be provided.
  !! For a justification of a zero absolute tolerance default see:
  !! https://peps.python.org/pep-0485/#absolute-tolerance-default
  !<
  pure logical function is_close(a, b, rtol, atol, symmetric)
    ! dummy
    real(DP), intent(in) :: a !< first real
    real(DP), intent(in) :: b !< second real (reference value if asymmetric)
    real(DP), intent(in), optional :: rtol !< relative tolerance (default=DSAME)
    real(DP), intent(in), optional :: atol !< absolute tolerance (default=DZERO)
    logical(LGP), intent(in), optional :: symmetric !< toggle (a)symmetric comparison
    ! local
    real(DP) :: lrtol, latol
    logical(LGP) :: lsymmetric

    ! check for exact equality
    if (a == b) then
      is_close = .true.
      return
    end if

    ! process optional arguments
    if (.not. present(rtol)) then
      lrtol = DSAME
    else
      lrtol = rtol
    end if
    if (.not. present(atol)) then
      latol = DZERO
    else
      latol = atol
    end if
    if (.not. present(symmetric)) then
      lsymmetric = .true.
    else
      lsymmetric = symmetric
    end if

    if (lsymmetric) then
      ! "weak" symmetric test, https://peps.python.org/pep-0485/#which-symmetric-test
      is_close = abs(a - b) <= max(lrtol * max(abs(a), abs(b)), latol)
    else
      ! asymmetric, https://numpy.org/doc/stable/reference/generated/numpy.isclose.html
      is_close = (abs(a - b) <= (latol + lrtol * abs(b)))
    end if
  end function is_close

  !> @brief Modulo with offset for integer values.
  pure function mod_offset_int(a, n, d) result(mo)
    ! -- dummy
    integer(I4B), intent(in) :: a !< dividend
    integer(I4B), intent(in) :: n !< divisor
    integer(I4B), intent(in), optional :: d !< offset
    integer(I4B) :: mo
    ! -- local
    integer(I4B) :: ld

    if (present(d)) then
      ld = d
    else
      ld = 0
    end if
    mo = a - n * floor(real(a - ld) / n)
  end function mod_offset_int

  !> @brief Modulo with offset for double precision values.
  pure function mod_offset_dbl(a, n, d) result(mo)
    ! -- dummy
    real(DP), intent(in) :: a !< dividend
    real(DP), intent(in) :: n !< divisor
    real(DP), intent(in), optional :: d !< offset
    real(DP) :: mo
    ! -- local
    real(DP) :: ld

    if (present(d)) then
      ld = d
    else
      ld = 0
    end if
    mo = a - n * floor((a - ld) / n)
  end function mod_offset_dbl

  !> @brief Compute zeros on an interval using Chadrupatla's method
  !!
  !! A zero of the function f{x} is computed in the interval (x0, x1)
  !! given tolerance epsa using Chandrupatla's method. FORTRAN code based
  !! generally on pseudocode in Scherer, POJ (2013) "Computational Physics:
  !! Simulation of Classical and Quantum Systems," 2nd ed., Springer, New York.
  !!
  !<
  function zeroch(x0, x1, f, epsa)
    implicit double precision(a - h, o - z)

    epsm = epsilon(x0)
    b = x0
    a = x1
    c = x1
    aminusb = a - b
    fb = f(b)
    fa = f(a)
    fc = f(c)
    t = 5d-1

    do while (.true.)
      ! xt = a + t*(b - a)
      xt = a - t * aminusb
      ft = f(xt)
      if (sign(ft, fa) == ft) then
        c = a
        fc = fa
        a = xt
        fa = ft
      else
        c = b
        b = a
        a = xt
        fc = fb
        fb = fa
        fa = ft
      end if
      aminusb = a - b
      cminusb = c - b
      faminusfb = fa - fb
      fcminusfb = fc - fb
      xm = a
      fm = fa
      if (dabs(fb) < dabs(fa)) then
        xm = b
        fm = fb
      end if
      tol = 2d0 * epsm * dabs(xm) + epsa
      ! tl = tol/dabs(b - c)
      tl = tol / dabs(cminusb)
      if ((tl > 5d-1) .or. (fm == 0d0)) then
        zeroch = xm
        return
      end if
      ! xi = (a - b)/(c - b)
      xi = aminusb / cminusb
      ! phi = (fa - fb)/(fc - fb)
      phi = faminusfb / fcminusfb
      philo = 1d0 - dsqrt(1d0 - xi)
      phihi = dsqrt(xi)
      if ((phi > philo) .and. (phi < phihi)) then
        ! rab = fa/(fb - fa)
        ! rab = -fa/faminusfb
        ! rcb = fc/(fb - fc)
        ! rcb = -fc/fcminusfb
        ! rac = fa/(fc - fa)
        ! rbc = fb/(fc - fb)
        ! rbc = fb/fcminusfb
        ! t = rab*rcb + rac*rbc*(c - a)/(b - a)
        ! t = rab*rcb - rac*rbc*(c - a)/aminusb
        racb = fa / fcminusfb
        rcab = fc / faminusfb
        rbca = fb / (fc - fa)
        t = racb * (rcab - rbca * (c - a) / aminusb)
        if (t < tl) then
          t = tl
        else
          tlc = 1d0 - tl
          if (t > tlc) then
            t = tlc
          end if
        end if
      else
        t = 5d-1
      end if
      ! if (t < tl) t = tl
      ! if (t > 1d0 - tl) t = 1d0 - tl
    end do
  end function

  !> @brief Compute a zero of the function f(x) in the interval (x0, x1).
  !!
  !! A zero of the function  f(x)  is computed in the interval ax,bx.
  !!
  !! Input:
  !!
  !! ax     left endpoint of initial interval
  !! bx     right endpoint of initial interval
  !! f      function subprogram which evaluates f(x) for any x in
  !!        the interval  ax,bx
  !! tol    desired length of the interval of uncertainty of the
  !!        final result (.ge.0.)
  !!
  !! Output:
  !!
  !! zeroin abscissa approximating a zero of  f  in the interval ax,bx
  !!
  !!     it is assumed  that   f(ax)   and   f(bx)   have  opposite  signs
  !! this is checked, and an error message is printed if this is not
  !! satisfied.   zeroin  returns a zero  x  in the given interval
  !! ax,bx  to within a tolerance  4*macheps*abs(x)+tol, where macheps  is
  !! the  relative machine precision defined as the smallest representable
  !! number such that  1.+macheps .gt. 1.
  !!     this function subprogram is a slightly  modified  translation  of
  !! the algol 60 procedure  zero  given in  richard brent, algorithms for
  !! minimization without derivatives, prentice-hall, inc. (1973).
  !<
  function zeroin(ax, bx, f, tol)
    implicit double precision(a - h, o - z)

    eps = epsilon(ax)
    tol1 = eps + 1.0d0

    a = ax
    b = bx
    fa = f(a)
    fb = f(b)

    ! check that f(ax) and f(bx) have different signs
    if (.not. ((fa .eq. 0.0d0 .or. fb .eq. 0.0d0) .or. &
               (fa * (fb / dabs(fb)) .le. 0.0d0))) &
      call pstop(1, 'f(ax) and f(bx) do not have different signs,')

20  c = a
    fc = fa
    d = b - a
    e = d

30  if (dabs(fc) .ge. dabs(fb)) go to 40
    a = b
    b = c
    c = a
    fa = fb
    fb = fc
    fc = fa
40  tol1 = 2.0d0 * eps * dabs(b) + 0.5d0 * tol
    xm = 0.5d0 * (c - b)
    if ((dabs(xm) .le. tol1) .or. (fb .eq. 0.0d0)) go to 150

    ! see if a bisection is forced
    if ((dabs(e) .ge. tol1) .and. (dabs(fa) .gt. dabs(fb))) go to 50
    d = xm
    e = d
    go to 110
50  s = fb / fa
    if (a .ne. c) go to 60
    ! linear interpolation
    p = 2.0d0 * xm * s
    q = 1.0d0 - s
    go to 70

    ! inverse quadratic interpolation
60  q = fa / fc
    r = fb / fc
    p = s * (2.0d0 * xm * q * (q - r) - (b - a) * (r - 1.0d0))
    q = (q - 1.0d0) * (r - 1.0d0) * (s - 1.0d0)
70  if (p .le. 0.0d0) go to 80
    q = -q
    go to 90
80  p = -p
90  s = e
    e = d
    if (((2.0d0 * p) .ge. (3.0d0 * xm * q - dabs(tol1 * q))) .or. &
        (p .ge. dabs(0.5d0 * s * q))) go to 100
    d = p / q
    go to 110
100 d = xm
    e = d
110 a = b
    fa = fb
    if (dabs(d) .le. tol1) go to 120
    b = b + d
    go to 140
120 if (xm .le. 0.0d0) go to 130
    b = b + tol1
    go to 140
130 b = b - tol1
140 fb = f(b)
    if ((fb * (fc / dabs(fc))) .gt. 0.0d0) go to 20
    go to 30
150 zeroin = b
    return
  end function zeroin

  !> @brief Compute a zero of the function f(x) in the interval (x0, x1)
  function zerotest(x0, x1, f, epsa)
    implicit double precision(a - h, o - z)
    logical(LGP) :: retainedxa, retainedxb

    epsm = epsilon(x0)
    f0 = f(x0)
    if (f0 .eq. 0d0) then
      zerotest = x0
      return
    else if (f0 .lt. 0d0) then
      ya = x0
      yb = x1
      xa = f0
      xb = f(yb)
    else
      ya = x1
      yb = x0
      xa = f(ya)
      xb = f0
    end if
    ema = 1d0
    emb = 1d0
    retainedxa = .false.
    retainedxb = .false.

    do while (.true.)
      ! yl = ya - xa*(yb - ya)/(xb - xa)
      yl = (ya * xb * emb - yb * xa * ema) / (xb * emb - xa * ema)
      tol = 4d0 * epsm * dabs(yl) + epsa
      if (dabs(yb - ya) .le. tol) then
        zerotest = yl
        return
      else
        xl = f(yl)
        if (xl .eq. 0d0) then
          zerotest = yl
          return
        else if (xl .gt. 0d0) then
          if (retainedxa) then
            ! ema = 1d0 - xl/xb
            ! if (ema <= 0d0) ema = 5d-1
            ema = 5d-1 ! kluge illinois
          else
            ema = 1d0
          end if
          emb = 1d0
          yb = yl
          xb = xl
          retainedxa = .true.
          retainedxb = .false.
        else
          if (retainedxb) then
            ! emb = 1d0 - xl/xa
            ! if (emb <= 0d0) emb = 5d-1
            emb = 5d-1 ! kluge illinois
          else
            emb = 1d0
          end if
          ema = 1d0
          ya = yl
          xa = xl
          retainedxa = .false.
          retainedxb = .true.
        end if
      end if
    end do
  end function

end module MathUtilModule

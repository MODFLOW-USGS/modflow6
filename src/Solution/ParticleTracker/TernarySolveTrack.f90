module TernarySolveTrack

  use KindModule, only: I4B, DP, LGP
  use GeomUtilModule, only: skew
  use MathUtilModule, only: f1d, zero_ch, zero_br, zero_test
  use ErrorUtilModule, only: pstop

  private
  public :: traverse_triangle
  public :: canonical
  public :: get_w
  public :: solve_coefs
  public :: step_analytical
  public :: step_euler
  public :: find_exit_bary
  public :: get_t_alpt
  public :: get_bet_outflow_bary
  public :: get_bet_soln_limits
  public :: soln_brent
  public :: soln_chand
  public :: soln_test
  public :: soln_euler
  public :: alpfun

  ! global data
  real(DP) ca1, ca2, ca3, cb1, cb2 !< Analytical solution coefficients
  real(DP) waa, wab, wba, wbb !< Elements of the "velocity matrix," W
  real(DP) :: cv0(2), cv1(2), cv2(2) !< "Canonical" velocity components at corners of triangular subcell
  integer(I4B) icase !< Case index for analytical solution

contains

  !> @brief Traverse triangular cell
  subroutine traverse_triangle(isolv, tol, step, texit, &
                               alpexit, betexit, &
                               itrifaceenter, itrifaceexit, &
                               rxx, rxy, ryx, ryy, &
                               alp0, bet0, alp1, bet1, alp2, bet2, alpi, beti, &
                               vziodz, az, &
                               bary)
    ! -- dummy
    integer(I4B), intent(in) :: isolv !< solution method
    real(DP), intent(in) :: tol !< solution tolerance
    real(DP), intent(in) :: step !< stepsize for numerical methods (e.g. euler)
    real(DP), intent(out) :: texit !< time particle exits the cell
    real(DP) :: alpexit
    real(DP) :: betexit !< alpha and beta coefficients
    integer(I4B) :: itrifaceenter
    integer(I4B) :: itrifaceexit !< entry and exit faces
    real(DP) :: rxx
    real(DP) :: rxy
    real(DP) :: ryx
    real(DP) :: ryy !< rotation matrix
    real(DP) :: alp0
    real(DP) :: bet0
    real(DP) :: alp1
    real(DP) :: bet1
    real(DP) :: alp2
    real(DP) :: bet2
    real(DP) :: alpi
    real(DP) :: beti !< alpha and beta coefficients
    real(DP) :: vziodz
    real(DP) :: az
    logical(LGP), intent(in) :: bary !< whether to use barycentric coordinates
    ! -- local
    real(DP) :: texit0
    real(DP) :: alpexit0
    real(DP) :: betexit0
    real(DP) :: texit1
    real(DP) :: alpexit1
    real(DP) :: betexit1
    real(DP) :: texit2
    real(DP) :: alpexit2
    real(DP) :: betexit2

    ! -- Compute elements of matrix W
    call get_w(alp1, bet1, alp2, bet2, waa, wab, wba, wbb, bary)

    ! -- Determine alpha and beta analytically as functions of time
    call solve_coefs(alpi, beti)

    ! -- Compute exit time (travel time to exit) and exit location
    call find_exit_bary(isolv, 0, itrifaceenter, &
                        alpi, beti, &
                        tol, step, vziodz, az, &
                        texit0, alpexit0, betexit0)
    call find_exit_bary(isolv, 1, itrifaceenter, &
                        alpi, beti, &
                        tol, step, vziodz, az, &
                        texit1, alpexit1, betexit1)
    call find_exit_bary(isolv, 2, itrifaceenter, &
                        alpi, beti, &
                        tol, step, vziodz, az, &
                        texit2, alpexit2, betexit2)
    texit = min(texit0, texit1, texit2)

    ! -- Note that while the numbering of triangle faces is generally zero-based
    ! -- (0, 1, 2), itrifaceexit, which gets passed out, is one-based (1, 2, 3).
    if (texit .eq. texit0) then
      alpexit = alpexit0
      betexit = betexit0
      itrifaceexit = 1
    else if (texit .eq. texit1) then
      alpexit = alpexit1
      betexit = betexit1
      itrifaceexit = 2
    else if (texit .eq. texit2) then
      alpexit = alpexit2
      betexit = betexit2
      itrifaceexit = 3
    end if
    if (texit .eq. huge(1d0)) itrifaceexit = 0

  end subroutine

  !> @brief Set coordinates to "canonical" configuration
  subroutine canonical(x0, y0, x1, y1, x2, y2, &
                       v0x, v0y, v1x, v1y, v2x, v2y, &
                       xi, yi, &
                       rxx, rxy, ryx, ryy, &
                       sxx, sxy, syy, &
                       alp0, bet0, alp1, bet1, alp2, bet2, alpi, beti, &
                       bary)
    ! -- dummy
    real(DP) :: x0
    real(DP) :: y0
    real(DP) :: x1
    real(DP) :: y1
    real(DP) :: x2
    real(DP) :: y2
    real(DP) :: v0x
    real(DP) :: v0y
    real(DP) :: v1x
    real(DP) :: v1y
    real(DP) :: v2x
    real(DP) :: v2y
    real(DP) :: xi
    real(DP) :: yi
    real(DP) :: rxx
    real(DP) :: rxy
    real(DP) :: ryx
    real(DP) :: ryy !< rotation matrix
    real(DP), intent(inout) :: sxx, sxy, syy !< skew matrix entries (top left, top right, bottom right)
    real(DP) :: alp0
    real(DP) :: bet0
    real(DP) :: alp1
    real(DP) :: bet1
    real(DP) :: alp2
    real(DP) :: bet2
    real(DP) :: alpi
    real(DP) :: beti !< alpha and beta coefficients
    logical(LGP), intent(in) :: bary !< whether to use barycentric coordinates
    ! -- local
    real(DP) :: baselen
    real(DP) :: oobaselen
    real(DP) :: sinomega
    real(DP) :: cosomega
    real(DP) :: x1diff
    real(DP) :: y1diff
    real(DP) :: x2diff
    real(DP) :: y2diff
    real(DP) :: xidiff
    real(DP) :: yidiff
    real(DP) :: rot(2, 2), res(2)

    ! -- Translate and rotate coordinates to "canonical" configuration
    x1diff = x1 - x0
    y1diff = y1 - y0
    x2diff = x2 - x0
    y2diff = y2 - y0
    baselen = dsqrt(x1diff * x1diff + y1diff * y1diff)
    oobaselen = 1d0 / baselen
    cosomega = x1diff * oobaselen
    sinomega = y1diff * oobaselen
    rxx = cosomega
    rxy = sinomega
    ryx = -sinomega
    ryy = cosomega
    alp0 = 0d0
    bet0 = 0d0
    alp1 = baselen
    bet1 = 0d0

    rot = reshape((/rxx, ryx, rxy, ryy/), shape(rot))
    res = matmul(rot, (/x2diff, y2diff/))
    alp2 = res(1)
    bet2 = res(2)

    cv0 = matmul(rot, (/v0x, v0y/))
    cv1 = matmul(rot, (/v1x, v1y/))
    cv2 = matmul(rot, (/v2x, v2y/))

    xidiff = xi - x0
    yidiff = yi - y0
    res = matmul(rot, (/xidiff, yidiff/))
    alpi = res(1)
    beti = res(2)

    if (bary) then
      sxx = 1d0 / alp1
      syy = 1d0 / bet2
      sxy = -alp2 * sxx * syy
      alp1 = 1d0
      alp2 = 0d0
      bet2 = 1d0
      cv0 = skew(cv0, (/sxx, sxy, syy/))
      cv1 = skew(cv1, (/sxx, sxy, syy/))
      cv2 = skew(cv2, (/sxx, sxy, syy/))
      res = (/alpi, beti/)
      res = skew(res, (/sxx, sxy, syy/))
      alpi = res(1)
      beti = res(2)
    end if

  end subroutine

  !> @brief Compute elements of W matrix
  subroutine get_w( &
    alp1, bet1, alp2, bet2, &
    waa, wab, wba, wbb, &
    bary)
    ! -- dummy
    real(DP) :: alp1
    real(DP) :: bet1
    real(DP) :: alp2
    real(DP) :: bet2 !< triangle face points
    real(DP) :: waa
    real(DP) :: wab
    real(DP) :: wba
    real(DP) :: wbb !< w matrix
    logical(LGP), intent(in), optional :: bary !< barycentric coordinates
    ! -- local
    logical(LGP) :: lbary
    real(DP) :: v1alpdiff
    real(DP) :: v2alpdiff
    real(DP) :: v2betdiff
    real(DP) :: ooalp1
    real(DP) :: oobet2
    real(DP) :: vterm

    if (present(bary)) then
      lbary = bary
    else
      lbary = .true.
    end if

    ! -- Note: wab is the "alpha,beta" entry in matrix W
    !    and the alpha component of the w^(beta) vector
    v1alpdiff = cv1(1) - cv0(1)
    v2alpdiff = cv2(1) - cv0(1)
    v2betdiff = cv2(2) - cv0(2)
    if (bary) then
      waa = v1alpdiff
      wab = v2alpdiff
      wba = 0d0
      wbb = v2betdiff
    else
      ooalp1 = 1d0 / alp1
      oobet2 = 1d0 / bet2
      vterm = v1alpdiff * ooalp1
      waa = vterm
      wab = (v2alpdiff - alp2 * vterm) * oobet2
      wba = 0d0
      wbb = v2betdiff * oobet2
    end if

  end subroutine

  !> @brief Compute analytical solution coefficients depending on case
  subroutine solve_coefs(alpi, beti)
    ! -- dummy
    real(DP) :: alpi
    real(DP) :: beti
    ! -- local
    real(DP) :: zerotol
    real(DP) :: wratv
    real(DP) :: acoef
    real(DP) :: bcoef
    real(DP) :: afact
    real(DP) :: bfact
    real(DP) :: vfact
    real(DP) :: oowaa

    zerotol = 1d-10 ! kluge
    if (dabs(wbb) .gt. zerotol) then
      wratv = (wab / wbb) * cv0(2)
      acoef = cv0(1) - wratv
      bcoef = wratv + wab * beti
      afact = acoef / waa
      vfact = cv0(2) / wbb
      ! -- Coefs for beta do not depend on whether waa = 0 or not
      cb1 = -vfact ! const term in beta
      cb2 = vfact + beti ! coef for e(wbb*t) term in beta
      ! -- Coefs for alpha
      if (dabs(waa) .gt. zerotol) then
        ! -- Case waa <> 0, wbb <> 0
        if (dabs(wbb - waa) .gt. zerotol) then
          ! -- Subcase wbb <> waa
          bfact = bcoef / (wbb - waa)
          ca1 = -afact ! const term in alpha
          ca2 = alpi + afact - bfact ! coef for exp(waa*t) term in alpha
          ca3 = bfact ! coef for exp(wbb*t) term in alpha
          icase = 1
        else
          ! -- Subcase wbb = waa
          ca1 = -afact ! const term in alpha
          ca2 = alpi + afact ! coef for exp(waa*t) term in alpha
          ca3 = bcoef ! coef for t*exp(waa*t) term in alpha
          icase = -1
        end if
      else
        ! -- Case waa = 0, wbb <> 0
        bfact = bcoef / wbb
        ca1 = alpi - bfact ! const term in alpha
        ca2 = acoef ! coef for t term in alpha
        ca3 = bfact ! coef for exp(wbb*t) term in alpha
        icase = 2
      end if
    else
      ! -- Coefs for beta do not depend on whether waa = 0 or not
      cb1 = beti ! const term in beta
      cb2 = cv0(2) ! coef for t term in beta
      if (dabs(waa) .gt. zerotol) then
        ! -- Case waa <> 0, wbb = 0
        oowaa = 1d0 / waa
        vfact = (wab * oowaa) * cv0(2)
        ca1 = -oowaa * (cv0(1) + wab * beti + vfact) ! const term in alpha
        ca2 = -vfact ! coef for t term in alpha
        ca3 = alpi - ca1 ! coef for exp(waa*t) term in alpha
        icase = 3
      else
        ! -- Case waa = 0, wbb = 0
        ca1 = alpi ! const term in alpha
        ca2 = cv0(1) + wab * beti ! coef for t term in alpha
        ca3 = 5d-1 * wab * cv0(2) ! coef for t^2 term in alpha
        icase = 4
      end if
    end if

  end subroutine

  !> @brief Step (evaluate) analytically depending on case
  subroutine step_analytical(t, alp, bet)
    ! -- dummy
    real(DP), intent(in) :: t
    real(DP) :: alp
    real(DP) :: bet

    if (icase .eq. 1) then
      alp = ca1 + ca2 * dexp(waa * t) + ca3 * dexp(wbb * t)
      bet = cb1 + cb2 * dexp(wbb * t)
    else if (icase .eq. -1) then
      alp = ca1 + (ca2 + ca3 * t) * dexp(waa * t)
      bet = cb1 + cb2 * dexp(wbb * t)
    else if (icase .eq. 2) then
      alp = ca1 + ca2 * t + ca3 * dexp(wbb * t)
      bet = cb1 + cb2 * dexp(wbb * t)
    else if (icase .eq. 3) then
      alp = ca1 + ca2 * t + ca3 * dexp(waa * t)
      bet = cb1 + cb2 * t
    else if (icase .eq. 4) then
      alp = ca1 + (ca2 + ca3 * t) * t
      bet = cb1 + cb2 * t
    end if

  end subroutine

  !> @brief Step (evaluate) numerically depending in case
  subroutine step_euler(nt, step, vziodz, az, alpi, beti, t, alp, bet)
    ! -- dummy
    integer(I4B) :: nt
    real(DP), intent(in) :: step
    real(DP) :: vziodz
    real(DP) :: az
    real(DP) :: alpi
    real(DP) :: beti
    real(DP), intent(inout) :: t
    real(DP) :: alp
    real(DP) :: bet
    ! -- local
    real(DP) :: alpproj
    real(DP) :: betproj
    real(DP) :: valp
    real(DP) :: vbet
    real(DP) :: vz
    real(DP) :: vmeasure
    real(DP) :: delt
    real(DP) :: thalf
    real(DP) :: rkn1
    real(DP) :: rln1
    real(DP) :: rkn2
    real(DP) :: rln2
    real(DP) :: rkn3
    real(DP) :: rln3
    real(DP) :: rkn4
    real(DP) :: rln4

    if (nt .eq. 0) then
      ! -- Initial location
      alp = alpi
      bet = beti
      t = 0d0
    else
      ! -- Step numerically
      valp = cv0(1) + waa * alp + wab * bet
      vbet = cv0(2) + wba * alp + wbb * bet
      if (step .lt. 0d0) then
        ! -- Compute time step based on abs value of step, interpreting the latter
        ! -- as a distance in canonical coordinates (alpha, beta, and scaled z)
        vz = vziodz * dexp(az * t)
        vmeasure = dsqrt(valp * valp + vbet * vbet + vz * vz)
        delt = -step / vmeasure
      else
        ! -- Set time step directly to step
        delt = step
      end if
      ikluge = 2 ! kluge
      if (ikluge .eq. 1) then
        t = t + delt
        alp = alp + valp * delt
        bet = bet + vbet * delt
      else
        rkn1 = valp
        rln1 = vbet
        thalf = t + 5d-1 * delt
        call step_analytical(thalf, alpproj, betproj)
        rkn2 = cv0(1) + waa * alpproj + wab * betproj
        rln2 = cv0(2) + wba * alpproj + wbb * betproj
        rkn3 = rkn2
        rln3 = rln2
        t = t + delt
        call step_analytical(t, alpproj, betproj)
        rkn4 = cv0(1) + waa * alpproj + wab * betproj
        rln4 = cv0(2) + wba * alpproj + wbb * betproj
        alp = alp + delt * (rkn1 + 2d0 * rkn2 + 2d0 * rkn3 + rkn4) / 6d0
        bet = bet + delt * (rln1 + 2d0 * rln2 + 2d0 * rln3 + rln4) / 6d0
      end if
    end if

  end subroutine

  !> @brief Find the exit time and location in barycentric coordinates.
  subroutine find_exit_bary(isolv, itriface, itrifaceenter, &
                            alpi, beti, &
                            tol, step, vziodz, az, &
                            texit, alpexit, betexit)
    ! -- dummy
    integer(I4B) :: isolv
    integer(I4B) :: itriface
    integer(I4B) :: itrifaceenter
    real(DP) :: alpi
    real(DP) :: beti
    real(DP) :: tol
    real(DP) :: step
    real(DP) :: vziodz
    real(DP) :: az
    real(DP) :: texit
    real(DP) :: alpexit
    real(DP) :: betexit
    ! -- local
    real(DP) :: alplo
    real(DP) :: alphi
    real(DP) :: alpt
    real(DP) :: alplim
    real(DP) :: fax
    real(DP) :: fbx
    real(DP) :: t
    real(DP) :: tlo
    real(DP) :: thi
    real(DP) :: v0alpstar
    real(DP) :: valpi
    real(DP) :: v1n
    real(DP) :: v2n
    real(DP) :: vbeti
    real(DP) :: zerotol
    real(DP) :: betlo
    real(DP) :: bethi
    real(DP) :: betsollo
    real(DP) :: betsolhi
    real(DP) :: betoutlo
    real(DP) :: betouthi
    integer(I4B) :: ibettrend

    ! -- Use iterative scheme or numerical integration indicated by isolv.
    zerotol = 1d-10 ! kluge
    if (itriface .eq. 0) then
      ! -- Checking for exit on canonical face 0 (beta = 0)
      if (itrifaceenter .eq. 0) then
        ! -- Entrance face, so no exit. (Normal velocity is uniform along face 0,
        ! -- so it cannot be both an entrance and an exit.)
        texit = huge(1d0)
      else
        ! -- Not the entrance face, so check for outflow
        if (cv0(2) .ge. 0d0) then
          ! -- Inflow or no flow, so no exit
          texit = huge(1d0)
        else
          ! -- Outflow, so check beta-velocity at the initial location,
          ! -- recognizing that it will never change sign along the
          ! -- trajectory (and will not be blocked from zero by an asymptote)
          vbeti = cv0(2) + wbb * beti
          if (vbeti .ge. 0d0) then
            ! -- Can't exit along beta = 0
            texit = huge(1d0)
          else
            ! -- get alpt and check it
            call get_t_alpt(0d0, t, alpt)
            if ((alpt .ge. 0d0) .and. (alpt .le. 1d0)) then
              ! -- alpt within the edge, so exit found
              texit = t
              alpexit = alpt
              betexit = 0d0
            else
              ! -- alpt not within the edge, so not an exit
              texit = huge(1d0)
            end if
          end if
        end if
      end if
      ! -- End canonical face 0 (beta = 0)
    else
      ! -- Checking for exit on canonical face 1 (gamma = 0.) or 2 (alpha = 0.)
      if (itriface .eq. 1) then
        ! -- Normal velocities (gamma components) at ends of canonical face 1
        v1n = -cv1(1) - cv1(2)
        v2n = -cv2(1) - cv2(2)
      else
        ! -- Normal velocities (alpha components) at ends of canonical face 2
        v1n = cv0(1)
        v2n = cv2(1)
      end if
      if ((v1n .ge. 0d0) .and. (v2n .ge. 0d0)) then
        ! -- No outflow at vn1 and vn2 corners; no outflow interval, so no exit.
        texit = huge(1d0)
      else
        ! -- Find outflow interval
        call get_bet_outflow_bary(v1n, v2n, betoutlo, betouthi)
        ! -- Find trend of and limits on beta from beta{t} solution
        call get_bet_soln_limits(beti, betsollo, betsolhi, ibettrend)
        ! -- Look for exit
        if (ibettrend .eq. 0) then
          ! -- Beta is constant, so check if it's within the outflow interval;
          ! -- if not, no exit; if so, solve for t and alpha
          if ((beti .gt. betouthi) .or. (beti .lt. betoutlo)) then
            texit = huge(1d0)
          else
            ! -- Check alpha-velocity at the initial location,
            ! -- recognizing that it will never change sign along the
            ! -- trajectory (and will not be blocked from zero by an asymptote)
            ! -- in this special case
            v0alpstar = cv0(1) + wab * beti
            valpi = v0alpstar + waa * alpi
            if ((itriface .eq. 1) .and. (valpi .le. 0d0)) then
              ! -- Can't exit along gamma = 0.
              texit = huge(1d0)
            else if ((itriface .eq. 2) .and. (valpi .ge. 0d0)) then
              ! -- Can't exit along alpha = 0.
              texit = huge(1d0)
            else
              ! -- get exit
              if (itriface .eq. 1) then
                alpexit = 1d0 - beti
              else
                alpexit = 0d0
              end if ! kluge note: seems like in this case (beta=const) this
              betexit = beti !   must be the ONLY exit; no need to check other edges??
              if (waa .ne. 0d0) then
                alplim = -v0alpstar / waa
                texit = dlog(alpexit - alplim / (alpi - alplim)) / waa
              else
                texit = (alpexit - alpi) / v0alpstar
              end if
            end if
          end if
          ! -- End constant-beta case
        else
          ! -- Beta varies along trajectory; combine outflow and soln limits on beta
          bethi = min(betouthi, betsolhi)
          betlo = max(betoutlo, betsollo)
          if (betlo .ge. bethi) then
            ! -- If bounds on bet leave no feasible interval, no exit
            texit = huge(1d0)
          else
            ! -- Check sign of function value at beta bounds
            call get_t_alpt(bethi, thi, alphi)
            call get_t_alpt(betlo, tlo, alplo)
            if (itriface .eq. 1) then
              fax = 1d0 - betlo - alplo
              fbx = 1d0 - bethi - alphi
            else
              fax = alplo
              fbx = alphi
            end if
            if (fax * fbx .gt. 0d0) then
              ! -- Root not bracketed; no exit
              texit = huge(1d0)
            else
              if (isolv .eq. 0) then
                ! -- Use Euler integration to find exit
                call soln_euler(itriface, alpi, beti, step, vziodz, az, &
                                texit, alpexit, betexit)
              else if (isolv .eq. 1) then
                ! -- Use Brent's method with initial bounds on beta of betlo and bethi,
                ! -- assuming they bound the root
                call soln_brent(itriface, betlo, bethi, tol, texit, &
                                alpexit, betexit)
              else if (isolv .eq. 2) then
                ! -- Use Chandrupatla's method with initial bounds on beta of betlo and bethi,
                ! -- assuming they bound the root
                call soln_chand(itriface, betlo, bethi, tol, texit, &
                                alpexit, betexit)
              else if (isolv .eq. 3) then
                ! -- Use a test method with initial bounds on beta of betlo and bethi,
                ! -- assuming they bound the root
                call soln_test(itriface, betlo, bethi, tol, texit, &
                               alpexit, betexit)
              else
                call pstop(1, "Invalid isolv, expected 0, 1, 2, or 3")
              end if
            end if
          end if
          ! -- End variable-beta case
        end if
      end if
      ! -- End canonical face 1 (gamma = 0.) or 2 (alpha = 0.)
    end if

    if (texit .ne. huge(1d0) .and. texit .lt. 0d0) &
      call pstop(1, "texit is negative (unexpected)") ! shouldn't get here

  end subroutine

  !> @brief Brent's method applied to canonical face 1 (gamma = 0)
  function fbary1(bet) result(fb)
    ! -- dummy
    real(DP), intent(in) :: bet
    real(DP) :: fb
    ! -- local
    real(DP) :: t
    real(DP) :: alpt

    ! -- Evaluate gamma{t{beta}} = 1. - alpha{t{beta}} - beta
    call get_t_alpt(bet, t, alpt)
    fb = 1d0 - alpt - bet
  end function

  !> @brief Brent's method applied to canonical face 2 (alpha = 0)
  function fbary2(bet) result(fb)
    ! -- dummy
    real(DP), intent(in) :: bet
    real(DP) :: fb
    ! -- local
    real(DP) :: t
    real(DP) :: alpt

    ! -- Evaluate alpha{t{beta}}
    call get_t_alpt(bet, t, alpt)
    fb = alpt
  end function

  !> @brief Given beta evaluate t and alpha depending on case
  subroutine get_t_alpt(bet, t, alp)
    ! -- dummy
    real(DP), intent(in) :: bet
    real(DP) :: t
    real(DP) :: alp
    ! -- local
    real(DP) :: term
    real(DP) :: zerotol

    ! kluge note: assumes cb2<>0, wbb<>0 as appropriate
    zerotol = 1d-10 ! kluge
    term = (bet - cb1) / cb2
    if (icase .eq. 1) then
      term = max(term, zerotol)
      t = dlog(term) / wbb
      alp = ca1 + ca2 * dexp(waa * t) + ca3 * dexp(wbb * t)
    else if (icase .eq. -1) then
      term = max(term, zerotol)
      t = dlog(term) / wbb
      alp = ca1 + (ca2 + ca3 * t) * dexp(waa * t)
    else if (icase .eq. 2) then
      term = max(term, zerotol)
      t = dlog(term) / wbb
      alp = ca1 + ca2 * t + ca3 * dexp(wbb * t)
    else if (icase .eq. 3) then
      t = term
      alp = ca1 + ca2 * t + ca3 * dexp(waa * t)
    else if (icase .eq. 4) then
      t = term
      alp = ca1 + (ca2 + ca3 * t) * t
    end if

  end subroutine

  !> @brief Find outflow interval
  subroutine get_bet_outflow_bary(vn1, vn2, betoutlo, betouthi)
    ! -- dummy
    real(DP) :: vn1
    real(DP) :: vn2
    real(DP) :: betoutlo
    real(DP) :: betouthi
    ! -- local
    real(DP) :: vndiff

    vndiff = vn2 - vn1
    if (vn1 .lt. 0d0) then
      ! -- Outflow at vn1 corner
      betoutlo = 0d0
      if (vn2 .le. 0d0) then
        ! -- Outflow along entire edge (except possibly no-flow right at vn2 corner)
        betouthi = 1d0
      else
        ! -- Outflow along part of edge
        betouthi = -vn1 / vndiff
      end if
    else
      ! -- Outflow at vn2 corner
      betouthi = 1d0
      if (vn1 .le. 0d0) then
        ! -- Outflow along entire edge (except possibly no-flow right at vn1 corner)
        betoutlo = 0d0
      else
        ! -- Outflow along part of edge
        betoutlo = -vn1 / vndiff
      end if
    end if

  end subroutine

  !> @brief Find trend of and limits on beta from beta{t} solution
  subroutine get_bet_soln_limits(beti, betsollo, betsolhi, ibettrend)
    ! -- dummy
    real(DP), intent(in) :: beti
    real(DP) :: betsollo
    real(DP) :: betsolhi
    integer(I4B), intent(inout) :: ibettrend
    ! -- local
    real(DP) :: betlim

    if (wbb .gt. 0d0) then
      betlim = -cv0(2) / wbb
      if (beti .gt. betlim) then
        betsolhi = huge(1d0)
        betsollo = beti
        ibettrend = 1
      else if (beti .lt. betlim) then
        betsolhi = beti
        betsollo = -huge(1d0)
        ibettrend = -1
      else
        betsolhi = beti
        betsollo = beti
        ibettrend = 0
      end if
    else if (wbb .lt. 0d0) then
      betlim = -cv0(2) / wbb
      if (beti .gt. betlim) then
        betsolhi = beti
        betsollo = betlim
        ibettrend = -1
      else if (beti .lt. betlim) then
        betsolhi = betlim
        betsollo = beti
        ibettrend = 1
      else
        betsolhi = beti
        betsollo = beti
        ibettrend = 0
      end if
    else ! kluge note: use zerotol and elsewhere?
      if (cv0(2) .gt. 0d0) then
        betsolhi = huge(1d0)
        betsollo = beti
        ibettrend = 1
      else if (cv0(2) .lt. 0d0) then
        betsolhi = beti
        betsollo = -huge(1d0)
        ibettrend = -1
      else
        betsolhi = beti
        betsollo = beti
        ibettrend = 0
      end if
    end if

  end subroutine

  !> @brief Use Brent's method with initial bounds on beta of betlo and bethi
  subroutine soln_brent(itriface, betlo, bethi, tol, &
                        texit, alpexit, betexit)
    ! -- dummy
    integer(I4B), intent(in) :: itriface
    real(DP) :: betlo
    real(DP) :: bethi
    real(DP), intent(in) :: tol
    real(DP) :: texit
    real(DP) :: alpexit
    real(DP) :: betexit
    ! -- local
    real(DP) :: itmax
    real(DP) :: itact
    real(DP) :: blo
    real(DP) :: bhi
    procedure(f1d), pointer :: f

    ! -- assuming betlo and bethi bracket the root
    ! --
    ! tol = 1d-7               ! kluge
    itmax = 50 ! kluge
    itact = itmax + 1 ! kluge
    blo = betlo
    bhi = bethi
    if (itriface .eq. 1) then
      f => fbary1
      betexit = zero_br(blo, bhi, f, tol)
    else
      f => fbary2
      betexit = zero_br(blo, bhi, f, tol)
    end if
    call get_t_alpt(betexit, texit, alpexit)

  end subroutine

  !> @brief Use Chandrupatla's method with initial bounds on beta of betlo and bethi
  subroutine soln_chand(itriface, betlo, bethi, tol, &
                        texit, alpexit, betexit)
    ! -- dummy
    integer(I4B), intent(in) :: itriface
    real(DP) :: betlo
    real(DP) :: bethi
    real(DP), intent(in) :: tol
    real(DP) :: texit
    real(DP) :: alpexit
    real(DP) :: betexit
    ! -- local
    real(DP) :: itmax
    real(DP) :: itact
    real(DP) :: blo
    real(DP) :: bhi
    procedure(f1d), pointer :: f

    ! -- note: assuming betlo and bethi bracket the root
    ! tol = 1d-7               ! kluge
    itmax = 50 ! kluge
    itact = itmax + 1 ! kluge
    blo = betlo
    bhi = bethi
    if (itriface .eq. 1) then
      f => fbary1
      betexit = zero_ch(blo, bhi, f, tol)
    else
      f => fbary2
      betexit = zero_ch(blo, bhi, f, tol)
    end if
    call get_t_alpt(betexit, texit, alpexit)

  end subroutine

  !> @brief Use a test method with initial bounds on beta of betlo and bethi
  subroutine soln_test(itriface, betlo, bethi, tol, &
                       texit, alpexit, betexit)
    ! -- dummy
    integer(I4B), intent(in) :: itriface
    real(DP) :: betlo
    real(DP) :: bethi
    real(DP), intent(in) :: tol
    real(DP) :: texit
    real(DP) :: alpexit
    real(DP) :: betexit
    ! -- local
    real(DP) :: itmax
    real(DP) :: itact
    real(DP) :: blo
    real(DP) :: bhi
    procedure(f1d), pointer :: f

    ! -- assuming betlo and bethi bracket the root
    ! tol = 1d-7               ! kluge
    itmax = 50 ! kluge
    itact = itmax + 1 ! kluge
    blo = betlo
    bhi = bethi
    if (itriface .eq. 1) then
      f => fbary1
      betexit = zero_test(blo, bhi, f, tol)
    else
      f => fbary2
      betexit = zero_test(blo, bhi, f, tol)
    end if
    call get_t_alpt(betexit, texit, alpexit)

  end subroutine

  !> @brief Use Euler integration to find exit
  subroutine soln_euler(itriface, alpi, beti, step, vziodz, &
                        az, texit, alpexit, betexit)
    ! -- dummy
    integer(I4B), intent(in) :: itriface
    real(DP) :: alpi
    real(DP) :: beti
    real(DP), intent(in) :: step
    real(DP) :: vziodz
    real(DP) :: az
    real(DP) :: texit
    real(DP) :: alpexit
    real(DP) :: betexit
    ! -- local
    real(DP) :: alp
    real(DP) :: bet
    real(DP) :: gam
    real(DP) :: alpold
    real(DP) :: betold
    real(DP) :: gamold
    real(DP) :: wt
    real(DP) :: omwt
    real(DP) :: t
    real(DP) :: told

    t = 0d0
    alp = alpi
    bet = beti
    if (itriface .eq. 1) gam = 1d0 - alpi - beti
    do nt = 1, 1000000000 ! kluge hardwired
      ! -- Save current time, alpha, and beta
      told = t
      alpold = alp
      betold = bet
      ! -- Step forward in time
      ! t = dble(nt)*step
      call step_euler(nt, step, vziodz, az, alpi, beti, t, alp, bet)
      ! if (nt.eq.0) then
      !   znum = zi
      ! else
      !   vz = vzbot + az*(znum - zbot)
      !   znum = znum + vz*delt     ! kluge note: can be smart about checking z
      ! end if
      if (itriface .eq. 1) then
        ! -- If gamma has crossed zero, interpolate linearly
        ! -- to find crossing (exit) point
        gamold = gam
        gam = 1d0 - alp - bet
        if (gam .lt. 0d0) then
          wt = gamold / (gamold - gam)
          omwt = 1d0 - wt
          texit = omwt * told + wt * t
          alpexit = omwt * alpold + wt * alp
          betexit = omwt * betold + wt * bet
          exit
        end if
      else
        ! -- If alpha has crossed zero, interpolate linearly
        ! -- to find crossing (exit) point
        if (alp .lt. 0d0) then
          wt = alpold / (alpold - alp)
          omwt = 1d0 - wt
          texit = omwt * told + wt * t
          alpexit = omwt * alpold + wt * alp
          betexit = omwt * betold + wt * bet
          exit
        end if
      end if
      ! -- End time step loop
    end do
    if (nt .gt. 1000000000) then ! kluge hardwired
      ! -- Exit not found after max number of time steps
      call pstop(1, "Didn't find exit in soln_euler")
    end if

  end subroutine

end module TernarySolveTrack

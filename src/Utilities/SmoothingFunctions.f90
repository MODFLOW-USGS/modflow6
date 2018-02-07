module SmoothingModule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DHALF, DONE, DTWO, DTHREE, DFOUR, &
 &                           DSIX, DPREC, DEM2, DEM4, DEM5, DEM6, DEM14 
  implicit none
  
  contains
    
  subroutine sSCurve(x,range,dydx,y)
! ******************************************************************************
! COMPUTES THE S CURVE FOR SMOOTH DERIVATIVES BETWEEN X=0 AND X=1
! FROM mfusg smooth SUBROUTINE in gwf2wel7u1.f
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    real(DP), intent(in) :: x
    real(DP), intent(in) :: range
    real(DP), intent(inout) :: dydx
    real(DP), intent(inout) :: y
    !--local variables
    real(DP) :: s
    real(DP) :: xs
! ------------------------------------------------------------------------------
!   code
!
    s = range
    if ( s < DPREC ) s = DPREC
    xs = x / s
    if (xs < DZERO) xs = DZERO
    if (xs <= DZERO) then
      y = DZERO
      dydx = DZERO
    elseif(xs < DONE)then
      y = -DTWO * xs**DTHREE + DTHREE * xs**DTWO
      dydx = -DSIX * xs**DTWO + DSIX * xs
    else
      y = DONE
      dydx = DZERO
    endif
    return
  end subroutine sSCurve
  
  subroutine sCubicLinear(x,range,dydx,y)
! ******************************************************************************
! COMPUTES THE S CURVE WHERE DY/DX = 0 at X=0; AND DY/DX = 1 AT X=1.
! Smooths from zero to a slope of 1.
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    real(DP), intent(in) :: x
    real(DP), intent(in) :: range
    real(DP), intent(inout) :: dydx
    real(DP), intent(inout) :: y
    !--local variables
    real(DP) :: s
    real(DP) :: xs
! ------------------------------------------------------------------------------
!   code
!
    s = range
    if ( s < DPREC ) s = DPREC
    xs = x / s
    if (xs < DZERO) xs = DZERO
    if (xs <= DZERO) then
      y = DZERO
      dydx = DZERO
    elseif(xs < DONE)then
      y = -DONE * xs**DTHREE + DTWO * xs**DTWO
      dydx = -DTHREE * xs**DTWO + DFOUR * xs
    else
      y = DONE
      dydx = DZERO
    endif
    return
  end subroutine sCubicLinear

  subroutine sCubic(x,range,dydx,y)
! ******************************************************************************
! Nonlinear smoothing function returns value between 0-1; cubic function
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    real(DP), intent(inout) :: x
    real(DP), intent(inout) :: range
    real(DP), intent(inout) :: dydx
    real(DP), intent(inout) :: y
    !--local variables
    real(DP) :: s, aa, bb
    real(DP) :: cof1, cof2, cof3
! ------------------------------------------------------------------------------
!   code
!
    dydx = DZERO
    y = DZERO
    if ( range < DPREC ) range = DPREC
    if ( x < DPREC ) x = DPREC
    s = range
    aa = -DSIX/(s**DTHREE)
    bb = -DSIX/(s**DTWO)
    cof1 = x**DTWO
    cof2 = -(DTWO*x)/(s**DTHREE)
    cof3 = DTHREE/(s**DTWO)
    y = cof1 * (cof2 + cof3)
    dydx = (aa*x**DTWO - bb*x)
    if ( x <= DZERO ) then
      y = DZERO
      dydx = DZERO
    else if ( (x - s) > -DPREC ) then
      y = DONE
      dydx = DZERO
    end if
    return
  end subroutine sCubic
  
  subroutine sLinear(x,range,dydx,y)
! ******************************************************************************
! Linear smoothing function returns value between 0-1
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    real(DP), intent(inout) :: x
    real(DP), intent(inout) :: range
    real(DP), intent(inout) :: dydx
    real(DP), intent(inout) :: y
    !--local variables
    real(DP) :: s
! ------------------------------------------------------------------------------
!   code
!
    dydx = DZERO
    y = DZERO
    if ( range < DPREC ) range = DPREC
    if ( x < DPREC ) x = DPREC
    s = range
    y = DONE - (s - x)/s
    dydx = DONE/s
    if ( y > DONE ) then
      y = DONE
      dydx = DZERO
    end if
    return
  end subroutine sLinear
    
  subroutine sQuadratic(x,range,dydx,y)
! ******************************************************************************
! Nonlinear smoothing function returns value between 0-1; quadratic function
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    real(DP), intent(inout) :: x
    real(DP), intent(inout) :: range
    real(DP), intent(inout) :: dydx
    real(DP), intent(inout) :: y
    !--local variables
    real(DP) :: s
! ------------------------------------------------------------------------------
!   code
!
    dydx = DZERO
    y = DZERO
    if ( range < DPREC ) range = DPREC
    if ( x < DPREC ) x = DPREC
    s = range
    y = (x**DTWO) / (s**DTWO)
    dydx = DTWO*x/(s**DTWO)
    if ( y > DONE ) then
      y = DONE
      dydx = DZERO
    end if
    return
  end subroutine sQuadratic

  subroutine sChSmooth(h, smooth, dwdh)
! ******************************************************************************
! Function to smooth channel variables during channel drying
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    real(DP), intent(in) :: h
    real(DP), intent(inout) :: smooth
    real(DP), intent(inout) :: dwdh
    !--local variables
    real(DP) :: s, aa, ad, b, x, y
! ------------------------------------------------------------------------------
!   code
!    
    smooth = DZERO
    s = DEM5
    x = h
    if ( x-s > DZERO ) then
      smooth = DONE
      dwdh = DZERO
    else
      aa = -DONE / (s**DTWO)
      ad = -DTWO / (s**DTWO)
      b = DTWO / s
      y = aa * x**DTWO + b*x
      dwdh = (ad*x + b)
      if ( x <= DZERO ) then
        y = DZERO
        dwdh = DZERO
      else if ( x-s > -DEM14 ) then
        y = DONE
        dwdh = DZERO
      end if
      smooth = y
    end if
    return
end subroutine sChSmooth
 
  function sLinearSaturation(top, bot, x) result(y)
! ******************************************************************************
! Linear smoothing function returns value between 0-1;
! Linear saturation function
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: y
    ! -- dummy variables
    real(DP), intent(in) :: top
    real(DP), intent(in) :: bot
    real(DP), intent(in) :: x
    ! -- local
    real(DP) :: b
! ------------------------------------------------------------------------------
!   code
!
    b = top - bot
    if (x < bot) then
      y = DZERO
    else if (x > top) then
      y = DONE
    else
      y = (x - bot) / b
    end if
    return
  end function sLinearSaturation


  function sCubicSaturation(top, bot, x, eps) result(y)
! ******************************************************************************
! Nonlinear smoothing function returns value between 0-1;
! Quadratic saturation function
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: y
    ! -- dummy variables
    real(DP), intent(in) :: top
    real(DP), intent(in) :: bot
    real(DP), intent(in) :: x
    real(DP), intent(in), optional :: eps
    ! -- local
    real(DP) :: teps
    real(DP) :: w
    real(DP) :: b
    real(DP) :: s
    real(DP) :: cof1
    real(DP) :: cof2
! ------------------------------------------------------------------------------
!   code
!
    if (present(eps)) then
      teps = eps
    else
      teps = DEM2
    end if
    w = x - bot
    b = top - bot
    s = teps * b
    cof1 = DONE / (s**DTWO)
    cof2 = DTWO / s
    if (w < DZERO) then
      y = DZERO
    else if (w < s) then
      y = -cof1 * (w**DTHREE) + cof2 * (w**DTWO)
    else if (w < (b-s)) then
      y = w / b
    else if (w < b) then
      y = DONE + cof1 * ((b - w)**DTHREE) - cof2 * ((b - w)**DTWO)
    else
      y = DONE
    end if
    
    return
  end function sCubicSaturation

  
  function sQuadraticSaturation(top, bot, x, eps) result(y)
! ******************************************************************************
! Nonlinear smoothing function returns value between 0-1;
! Quadratic saturation function
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: y
    ! -- dummy variables
    real(DP), intent(in) :: top
    real(DP), intent(in) :: bot
    real(DP), intent(in) :: x
    real(DP), optional, intent(in) :: eps
    ! -- local
    real(DP) :: teps
    real(DP) :: b
    real(DP) :: br
    real(DP) :: bri
    real(DP) :: av
! ------------------------------------------------------------------------------
!   code
!
    if (present(eps)) then
      teps = eps
    else
      teps = DEM6
    end if
    b = top - bot
    if (b > DZERO) then
      if (x < bot) then
        br = DZERO
      else if (x > top) then
        br = DONE
      else
        br = (x - bot) / b
      end if
      av = DONE / (DONE - teps) 
      bri = DONE - br
      if (br < DZERO) then
        y = DZERO
      elseif (br < teps) then
        y = av * DHALF * (br*br) / teps
      elseif (br < (DONE-teps)) then
        y = av * br + DHALF * (DONE - av)
      elseif (br < DONE) then
        y = DONE - ((av * DHALF * (bri * bri)) / teps)
      else
        y = DONE
      end if
    else
      if (x < bot) then
        y = DZERO
      else
        y = DONE
      end if
    end if
    
    return
  end function sQuadraticSaturation

  
  function svanGenuchtenSaturation(top, bot, x, alpha, beta, sr) result(y)
! ******************************************************************************
! Nonlinear smoothing function returns value between 0-1;
! van Genuchten saturation function
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: y
    ! -- dummy variables
    real(DP), intent(in) :: top
    real(DP), intent(in) :: bot
    real(DP), intent(in) :: x
    real(DP), intent(in) :: alpha
    real(DP), intent(in) :: beta
    real(DP), intent(in) :: sr
    ! -- local
    real(DP) :: b
    real(DP) :: pc
    real(DP) :: gamma
    real(DP) :: seff
! ------------------------------------------------------------------------------
!   code
!
    b = top - bot
    pc = (DHALF * b) - x
    if (pc <= DZERO) then
      y = DZERO
    else
      gamma = DONE - (DONE / beta)
      seff = (DONE + (alpha * pc)**beta)**gamma
      seff = DONE / seff
      y = seff * (DONE - sr) + sr
    end if

    return
  end function svanGenuchtenSaturation
 
  
  function sQuadraticSaturationDerivative(top, bot, x, eps) result(y)
! ******************************************************************************
! Derivative of nonlinear smoothing function returns value between 0-1;
! Derivative of the quadratic saturation function
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: y
    ! -- dummy variables
    real(DP), intent(in) :: top
    real(DP), intent(in) :: bot
    real(DP), intent(in) :: x
    real(DP), optional, intent(in) :: eps
    ! -- local
    real(DP) :: teps
    real(DP) :: b
    real(DP) :: br
    real(DP) :: bri
    real(DP) :: av
! ------------------------------------------------------------------------------
!   code
!
    if (present(eps)) then
      teps = eps
    else
      teps = DEM6
    end if
    b = top - bot
    if (x < bot) then
      br = DZERO
    else if (x > top) then
      br = DONE
    else
      br = (x - bot) / b
    end if
    av = DONE / (DONE - teps) 
    bri = DONE - br
    if (br < DZERO) then
      y = DZERO
    elseif (br < teps) then
      y = av * br / teps
    elseif (br < (DONE-teps)) then
      y = av
    elseif (br < DONE) then
      y = av * bri / teps
    else
      y = DZERO
    end if
    y = y / b
    
    return
  end function sQuadraticSaturationDerivative



  function sQSaturation(top, bot, x) result(y)
! ******************************************************************************
! Nonlinear smoothing function returns value between 0-1;
! Cubic saturation function
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: y
    ! -- dummy variables
    real(DP), intent(in) :: top
    real(DP), intent(in) :: bot
    real(DP), intent(in) :: x
    ! -- local
    real(DP) :: w
    real(DP) :: b
    real(DP) :: s
    real(DP) :: cof1
    real(DP) :: cof2
! ------------------------------------------------------------------------------
!   code
!
    w = x - bot
    b = top - bot
    s = w / b
    cof1 = -DTWO / b**DTHREE
    cof2 = DTHREE / b**DTWO
    if (s < DZERO) then
      y = DZERO
    else if(s < DONE) then
      y = cof1 * w**DTHREE + cof2 * w**DTWO
    else
      y = DONE
    end if
    
    return
  end function sQSaturation
  
  function sQSaturationDerivative(top, bot, x) result(y)
! ******************************************************************************
! Nonlinear smoothing function returns value between 0-1;
! Cubic saturation function
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: y
    ! -- dummy variables
    real(DP), intent(in) :: top
    real(DP), intent(in) :: bot
    real(DP), intent(in) :: x
    ! -- local
    real(DP) :: w
    real(DP) :: b
    real(DP) :: s
    real(DP) :: cof1
    real(DP) :: cof2
! ------------------------------------------------------------------------------
!   code
!
    w = x - bot
    b = top - bot
    s = w / b
    cof1 = -DSIX / b**DTHREE
    cof2 = DSIX / b**DTWO
    if (s < DZERO) then
      y = DZERO
    else if(s < DONE) then
      y = cof1 * w**DTWO + cof2 * w
    else
      y = DZERO
    end if
    
    return
  end function sQSaturationDerivative
  
  subroutine sPChip_set_derivatives(num_points, x, f, d)
! ******************************************************************************
! Sets derivatives needed to determine a monotone piecewise
!            cubic Hermite interpolant to given data.
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), intent(in) :: num_points
    real(DP), intent(in) :: x(*), f(*)
    real(DP), intent(out) :: d(*)
    ! -- local
    integer(I4B) ::  i
    real(DP) :: del1, del2, dmax, dmin, dsave, h1, h2, hsum, hsumt3, w1, w2
    real(DP) :: drat1, drat2
! ------------------------------------------------------------------------------
    !
    ! -- check points increasing
    do i = 2, num_points
      if ( x(i) .le. x(i-1) )  then
        stop 'PChip x values not strictly increasing'
      endif
    enddo

    h1 = x(2) - x(1)
    del1 = (f(2) - f(1))/h1
    dsave = del1

    ! -- two points use linear interpolation
    if (num_points .eq. 2 ) then
      d(1) = del1
      d(num_points) = del1
    else
      ! -- more than two points
      h2 = x(3) - x(2)
      del2 = (f(3) - f(2))/h2
      !
      ! -- set d(1) via non-centered three-point formula, adjusted to be
      !     shape-preserving.
      hsum = h1 + h2
      w1 = (h1 + hsum)/hsum
      w2 = -h1/hsum
      d(1) = w1 * del1 + w2 * del2
      if (sTest_sign(d(1),del1) .le. DZERO)  then
        d(1) = DZERO
      else if (sTest_sign(del1,del2) .lt. DZERO)  then
        ! -- need do this check only if monotonicity switches.
        dmax = DTHREE * del1
        if (abs(d(1)) .gt. abs(dmax))  d(1) = dmax
      ENDIF
      !
      ! --  loop through interior points.
      !
      do  i = 2, num_points - 1
        if (i .ne. 2) then
           h1 = h2
           h2 = x(i+1) - x(I)
           hsum = h1 + h2
           del1 = del2
           del2 = (f(i+1) - f(i))/h2
         endif
         !
         ! --  set d(i)=0 unless data are strictly monotonic.
         d(i) = DZERO
         select case (int(sTest_sign(del1,del2)))
           ! -- count number of changes in direction of monotonicity.
           !
         case(0)
           if (del2 .eq. DZERO)  cycle
           dsave = del2
           cycle
         case(:-1)
           dsave = del2
           cycle
         case(1:)
           !
           ! --  use Brodlie modification of Butland formula.
           !
           hsumt3 = hsum + hsum + hsum
           w1 = (hsum + h1)/hsumt3
           w2 = (hsum + h2)/hsumt3
           dmax = max( abs(del1), abs(del2) )
           dmin = min( abs(del1), abs(del2) )
           drat1 = del1/dmax
           drat2 = del2/dmax
           d(i) = dmin/(w1 * drat1 + w2 * drat2)
         end select
       enddo
       !
       ! -- set d(num_points) via non-centered three-point formula, adjusted to be
       !     shape-preserving.
       !
       w1 = -h2/hsum
       w2 = (h2 + hsum)/hsum
       d(num_points) = w1 * del1 + w2 * del2
       if (sTest_sign(d(num_points),del2) .le. DZERO) then
         d(num_points) = DZERO
       else if (sTest_sign(del1, del2) .lt. DZERO) then
         ! -- need do this check only if monotonicity switches.
         dmax = DTHREE * del2
         if (abs(d(num_points)) .gt. abs(dmax))  d(num_points) = dmax
       endif
     end if

    end subroutine sPChip_set_derivatives
    
    real(DP) function sTest_sign (arg1, arg2)
! ******************************************************************************
! Test_sign -- Utility function for PChip
!
!     Returns:
!        -1. if arg1 and ARG2 are of opposite sign.
!         0. if either argument is zero.
!        +1. if arg1 and arg2 are of the same sign.
! ******************************************************************************
!      
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
      ! -- dummy
      real(DP), intent(in) :: arg1, arg2
      ! -- local
      ! -- formats
      ! -- data
! ------------------------------------------------------------------------------
      !
      sTest_sign = sign(DONE, arg1) * sign(DONE, arg2)
      if((arg1 .eq. DZERO) .or. (arg2 .eq. DZERO)) sTest_sign = DZERO
    end function sTest_sign

!-----------------------------------------------------------------------------
    
    real(DP) function sPChip_integrate(num_points, x, f, d, a, b)
! ******************************************************************************
! sPChip_integrate -- Evaluate PChip over interval a - b
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
      ! -- dummy
      integer(I4B), intent(in) :: num_points
      real(DP), intent(in) :: x(*), f(*), d(*), a, b
      ! -- local
      integer(I4B) :: i, ia, ib, ierd, il, ir
      real(DP) :: val, xa, xb

      val = DZERO
      !
      ! -- compute integral value.
      !
      if (a .ne. b)  then
         xa = min(a, b)
         xb = max(a, b)
         if (xb .le. x(2)) then
           ! -- interval is to left of x(2), so use first cubic.
           !
           val = sPChip_eval_ext(x(1), x(2), f(1), f(2), d(1), d(2), a, b)
           !
         else if (xa .ge. x(num_points-1)) then
           ! -- interval is to right of x(num_points-1) so use last cubic.
           !
           val = sPChip_eval_ext(x(num_points-1), x(num_points), f(num_points-1), &
               f(num_points), d(num_points-1),d(num_points), a, b)
           !
         else
           ! -- 'normal' case -- xa < xb, xa < x(n-1), xb > x(2).
           !    locate ia and ib such that
           !    x(ia-1) < xa < x(ia) < x(ib) < xb < x(ib+1)
           ia = 1
           do I = 1, num_points-1
             if (xa .gt. x(i)) ia = i + 1
           enddo
           ! -- ia = 1 implies xa < x(1)
           ! otherwise ia is largest index such that x(ia-1) < xa,
           !
           ib = num_points
           do i = num_points, ia, -1
             if (xb .lt. x(i)) ib = i - 1
           enddo
           ! -- ib = num_points implies xb > x(num_points)
           ! otherwise ib is smallest index such that xb < x(ib+1)
           !
           ! -- compute the integral.
           if (ib .lt. ia)  then
             ! -- this means ib = ia-1 and (a,b) is a subset of (x(ib),x(ia)).
               val = sPChip_eval_ext (X(IB),X(IA), F(IB), F(IA), D(IB),D(IA), A, B)

             else
               !
               ! -- first compute integral over (x(ia), x(ib))
               ! Case (ib == ia) is taken care of by initialization of val to DZERO
               if (ib .gt. ia)  then
                  val = sPChip_eval_int(num_points, x, f, d, ia, ib)
               endif
               !
               ! -- then add on integral over (xa,x(ia)).
               if (xa .lt. x(ia)) then
                 il = max(1, ia-1)
                 ir = il + 1
                 val = val + sPChip_eval_ext(x(il), x(ir), f(il),f(ir), d(il), d(ir), xa, x(ia))
               endif
               !
               ! -- then add on integral over (x(ib), xb)
               if (xb .gt. x(ib)) then
                 ir = min (ib+1, num_points)
                 il = ir - 1
                 val = val + sPChip_eval_ext(x(il),x(ir), f(il),f(ir), d(il),d(ir), x(ib), xb)
               endif
               !
               ! -- finally, adjust sign if necessary.
               if (a .gt. b)  val = - val
             endif
           endif
         endif

         sPChip_integrate = val

       end function sPChip_integrate


      real(DP) function sPChip_eval_int(num_points, x, f, d, ia, ib)
! ******************************************************************************
! sPChip_eval_int -- Evaluate PChip over interval x(ia), x(ib)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
        ! -- dummy
        integer(I4B), intent(in) :: num_points, ia, ib
        real(DP), intent(in) :: x(*), f(*), d(*)
        ! -- local
        integer(I4B) :: i, iup, low
        real(DP) :: H, tot, val

        val = DZERO
        !
        ! -- compute integral value.
        !
        if (ia .ne. ib) then
          low = min(ia, ib)
          iup = max(ia, ib) - 1
          tot = DZERO
          do i = low, iup
            h = x(i+1) - x(i)
            tot = tot + h * ((f(i) + f(i+1)) + (d(i) - d(i+1)) * (h/DSIX))
          enddo
          val = DHALF * tot
          if (ia .gt. ib)  val = -val
        endif

        sPChip_eval_int = val

      end function sPChip_eval_int


      real(DP) function sPChip_eval_ext(x1, x2, f1, f2, d1, d2, a, b)
! ******************************************************************************
! sPChip_eval_ext -- Evaluate PChip over arbitrary interval a - b
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
      ! -- dummy
      real(DP), intent(in) :: x1, x2, f1, f2, d1, d2, a, b
      ! -- local        
      real(DP) :: dterm, fterm, h, phia1, phia2, &
          phib1, phib2, psia1, psia2, psib1, psib2, ta1, ta2, &
          tb1, tb2, ua1, ua2, ub1, ub2                 

      if (x1 .eq. x2) then
        sPChip_eval_ext = 0
      else
        h = x2 - x1
        ta1 = (a - x1) / h
        ta2 = (x2 - a) / h
        tb1 = (b - x1) / h
        tb2 = (x2 - b) / h
      
        ua1 = ta1**3
        phia1 = ua1 * (DTWO - ta1)
        psia1 = ua1 * (DTHREE*ta1 - DFOUR)
        ua2 = ta2**3
        phia2 =  ua2 * (DTWO - ta2)
        psia2 = -ua2 * (DTHREE*ta2 - DFOUR)

        ub1 = tb1**3
        phib1 = ub1 * (DTWO - tb1)
        psib1 = ub1 * (DTHREE * tb1 - DFOUR)
        ub2 = tb2**3
        phib2 =  ub2 * (DTWO - tb2)
        psib2 = -ub2 * (DTHREE * tb2 - DFOUR)
      
        fterm =   f1 * (phia2 - phib2) + f2 * (phib1 - phia1)
        dterm = (d1 * (psia2 - psib2) + d2 * (psib1 - psia1)) * (h/DSIX)
      
        sPChip_eval_ext = (DHALF * h) * (fterm + dterm)
        
      endif

    end function sPChip_eval_ext
      
end module SmoothingModule
    
    

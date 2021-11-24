function dasum ( n, x, incx )

  !*****************************************************************************80
  !
  !! DASUM takes the sum of the absolute values of a vector.
  !
  !  Discussion:
  !
  !    This routine uses double precision real arithmetic.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license. 
  !
  !  Modified:
  !
  !    15 February 2001
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Charles Lawson, Richard Hanson, 
  !    David Kincaid, Fred Krogh.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Algorithm 539, 
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    ACM Transactions on Mathematical Software, 
  !    Volume 5, Number 3, September 1979, pages 308-323.
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) N, the number of entries in the vector.
  !
  !    Input, real ( kind = 8 ) X(*), the vector to be examined.
  !
  !    Input, integer ( kind = 4 ) INCX, the increment between successive
  !    entries of X.  INCX must not be negative.
  !
  !    Output, real ( kind = 8 ) DASUM, the sum of the absolute values of X.
  !
    implicit none
  
    real ( kind = 8 ) dasum
    integer ( kind = 4 ) incx
    integer ( kind = 4 ) n
    real ( kind = 8 ) x(*)
  
    dasum = sum ( abs ( x(1:1+(n-1)*incx:incx) ) )
  
    return
  end
  subroutine daxpy ( n, da, dx, incx, dy, incy )
  
  !*****************************************************************************80
  !
  !! DAXPY computes constant times a vector plus a vector.
  !
  !  Discussion:
  !
  !    This routine uses double precision real arithmetic.
  !
  !    This routine uses unrolled loops for increments equal to one.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license. 
  !
  !  Modified:
  !
  !    16 May 2005
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Charles Lawson, Richard Hanson, 
  !    David Kincaid, Fred Krogh.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Algorithm 539, 
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    ACM Transactions on Mathematical Software, 
  !    Volume 5, Number 3, September 1979, pages 308-323.
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) N, the number of elements in DX and DY.
  !
  !    Input, real ( kind = 8 ) DA, the multiplier of DX.
  !
  !    Input, real ( kind = 8 ) DX(*), the first vector.
  !
  !    Input, integer ( kind = 4 ) INCX, the increment between successive 
  !    entries of DX.
  !
  !    Input/output, real ( kind = 8 ) DY(*), the second vector.
  !    On output, DY(*) has been replaced by DY(*) + DA * DX(*).
  !
  !    Input, integer ( kind = 4 ) INCY, the increment between successive 
  !    entries of DY.
  !
    implicit none
  
    real ( kind = 8 ) da
    real ( kind = 8 ) dx(*)
    real ( kind = 8 ) dy(*)
    integer ( kind = 4 ) i
    integer ( kind = 4 ) incx
    integer ( kind = 4 ) incy
    integer ( kind = 4 ) ix
    integer ( kind = 4 ) iy
    integer ( kind = 4 ) m
    integer ( kind = 4 ) n
  
    if ( n <= 0 ) then
      return
    end if
  
    if ( da == 0.0D+00 ) then
      return
    end if
  !
  !  Code for unequal increments or equal increments
  !  not equal to 1.
  !
    if ( incx /= 1 .or. incy /= 1 ) then
  
      if ( 0 <= incx ) then
        ix = 1
      else
        ix = ( - n + 1 ) * incx + 1
      end if
  
      if ( 0 <= incy ) then
        iy = 1
      else
        iy = ( - n + 1 ) * incy + 1
      end if
  
      do i = 1, n
        dy(iy) = dy(iy) + da * dx(ix)
        ix = ix + incx
        iy = iy + incy
      end do
  !
  !  Code for both increments equal to 1.
  !
    else
  
      m = mod ( n, 4 )
  
      dy(1:m) = dy(1:m) + da * dx(1:m)
  
      do i = m + 1, n, 4
        dy(i  ) = dy(i  ) + da * dx(i  )
        dy(i+1) = dy(i+1) + da * dx(i+1)
        dy(i+2) = dy(i+2) + da * dx(i+2)
        dy(i+3) = dy(i+3) + da * dx(i+3)
      end do
  
    end if
  
    return
  end
  subroutine dcopy ( n, dx, incx, dy, incy )
  
  !*****************************************************************************80
  !
  !! DCOPY copies a vector X to a vector Y.
  !
  !  Discussion:
  !
  !    This routine uses double precision real arithmetic.
  !
  !    The routine uses unrolled loops for increments equal to one.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license. 
  !
  !  Modified:
  !
  !    16 May 2005
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Charles Lawson, Richard Hanson, 
  !    David Kincaid, Fred Krogh.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Algorithm 539, 
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    ACM Transactions on Mathematical Software, 
  !    Volume 5, Number 3, September 1979, pages 308-323.
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) N, the number of elements in DX and DY.
  !
  !    Input, real ( kind = 8 ) DX(*), the first vector.
  !
  !    Input, integer ( kind = 4 ) INCX, the increment between successive 
  !    entries of DX.
  !
  !    Output, real ( kind = 8 ) DY(*), the second vector.
  !
  !    Input, integer ( kind = 4 ) INCY, the increment between successive 
  !    entries of DY.
  !
    implicit none
  
    integer ( kind = 4 ) i
    integer ( kind = 4 ) incx
    integer ( kind = 4 ) incy
    integer ( kind = 4 ) ix
    integer ( kind = 4 ) iy
    integer ( kind = 4 ) m
    integer ( kind = 4 ) n
    real ( kind = 8 ) dx(*)
    real ( kind = 8 ) dy(*)
  
    if ( n <= 0 ) then
      return
    end if
  
    if ( incx == 1 .and. incy == 1 ) then
  
      m = mod ( n, 7 )
  
      if ( m /= 0 ) then
  
        dy(1:m) = dx(1:m)
  
      end if
  
      do i = m+1, n, 7
        dy(i) = dx(i)
        dy(i + 1) = dx(i + 1)
        dy(i + 2) = dx(i + 2)
        dy(i + 3) = dx(i + 3)
        dy(i + 4) = dx(i + 4)
        dy(i + 5) = dx(i + 5)
        dy(i + 6) = dx(i + 6)
      end do
  
    else
  
      if ( 0 <= incx ) then
        ix = 1
      else
        ix = ( -n + 1 ) * incx + 1
      end if
  
      if ( 0 <= incy ) then
        iy = 1
      else
        iy = ( -n + 1 ) * incy + 1
      end if
  
      do i = 1, n
        dy(iy) = dx(ix)
        ix = ix + incx
        iy = iy + incy
      end do
  
    end if
  
    return
  end
  function ddot ( n, dx, incx, dy, incy )
  
  !*****************************************************************************80
  !
  !! DDOT forms the dot product of two vectors.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license. 
  !
  !  Modified:
  !
  !    06 June 2014
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Charles Lawson, Richard Hanson, 
  !    David Kincaid, Fred Krogh.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Algorithm 539, 
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    ACM Transactions on Mathematical Software, 
  !    Volume 5, Number 3, September 1979, pages 308-323.
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
  !
  !    Input, real ( kind = 8 ) DX(*), the first vector.
  !
  !    Input, integer ( kind = 4 ) INCX, the increment between successive 
  !    entries in DX.
  !
  !    Input, real ( kind = 8 ) DY(*), the second vector.
  !
  !    Input, integer ( kind = 4 ) INCY, the increment between successive 
  !    entries in DY.
  !
  !    Output, real ( kind = 8 ) DDOT, the sum of the product of the 
  !    corresponding entries of DX and DY.
  !
    implicit none
  
    real ( kind = 8 ) ddot
    real ( kind = 8 ) dx(*)
    real ( kind = 8 ) dy(*)
    integer ( kind = 4 ) incx
    integer ( kind = 4 ) incy
    integer ( kind = 4 ) n
    integer ( kind = 4 ) x1
    integer ( kind = 4 ) xi
    integer ( kind = 4 ) xn
    integer ( kind = 4 ) y1
    integer ( kind = 4 ) yi
    integer ( kind = 4 ) yn
  !
  !  Let the FORTRAN90 function DOT_PRODUCT take care of optimization.
  !
    if ( 0 < incx ) then
      x1 = 1
      xn = 1 + ( n - 1 ) * incx
      xi = incx
    else
      x1 = 1 + ( n - 1 ) * incx
      xn = 1
      xi = - incx
    end if
  
    if ( 0 < incy ) then
      y1 = 1
      yn = 1 + ( n - 1 ) * incy
      yi = incy
    else
      y1 = 1 + ( n - 1 ) * incy
      yn = 1
      yi = - incy
    end if
  
    ddot = dot_product ( dx(x1:xn:xi), dy(y1:yn:yi) )
  
    return
  end
  function dnrm2 ( n, x, incx )
  
  !*****************************************************************************80
  !
  !! DNRM2 returns the euclidean norm of a vector.
  !
  !  Discussion:
  !
  !    This routine uses double precision real arithmetic.
  !
  !     DNRM2 ( X ) = sqrt ( X' * X )
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license. 
  !
  !  Modified:
  !
  !    16 May 2005
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Charles Lawson, Richard Hanson, 
  !    David Kincaid, Fred Krogh.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Algorithm 539, 
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    ACM Transactions on Mathematical Software,
  !    Volume 5, Number 3, September 1979, pages 308-323.
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) N, the number of entries in the vector.
  !
  !    Input, real ( kind = 8 ) X(*), the vector whose norm is to be computed.
  !
  !    Input, integer ( kind = 4 ) INCX, the increment between successive 
  !    entries of X.
  !
  !    Output, real ( kind = 8 ) DNRM2, the Euclidean norm of X.
  !
    implicit none
  
    real ( kind = 8 ) absxi
    real ( kind = 8 ) dnrm2
    integer ( kind = 4 ) incx
    integer ( kind = 4 ) ix
    integer ( kind = 4 ) n
    real ( kind = 8 ) norm
    real ( kind = 8 ) scale
    real ( kind = 8 ) ssq
    real ( kind = 8 ) x(*)
  
    if ( n < 1 .or. incx < 1 ) then
  
      norm  = 0.0D+00
  
    else if ( n == 1 ) then
  
      norm  = abs ( x(1) )
  
    else
  
      scale = 0.0D+00
      ssq = 1.0D+00
  
      do ix = 1, 1 + ( n - 1 ) * incx, incx
        if ( x(ix) /= 0.0D+00 ) then
          absxi = abs ( x(ix) )
          if ( scale < absxi ) then
            ssq = 1.0D+00 + ssq * ( scale / absxi )**2
            scale = absxi
          else
            ssq = ssq + ( absxi / scale )**2
          end if
        end if
      end do
      norm  = scale * sqrt ( ssq )
    end if
  
    dnrm2 = norm
  
    return
  end
  subroutine drot ( n, x, incx, y, incy, c, s )
  
  !*****************************************************************************80
  !
  !! DROT applies a plane rotation.
  !
  !  Discussion:
  !
  !    This routine uses double precision real arithmetic.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license. 
  !
  !  Modified:
  !
  !    08 April 1999
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Charles Lawson, Richard Hanson, 
  !    David Kincaid, Fred Krogh.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Algorithm 539, 
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    ACM Transactions on Mathematical Software,
  !    Volume 5, Number 3, September 1979, pages 308-323.
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
  !
  !    Input/output, real ( kind = 8 ) X(*), one of the vectors to be rotated.
  !
  !    Input, integer ( kind = 4 ) INCX, the increment between successive 
  !    entries of X.
  !
  !    Input/output, real ( kind = 8 ) Y(*), one of the vectors to be rotated.
  !
  !    Input, integer ( kind = 4 ) INCY, the increment between successive
  !    elements of Y.
  !
  !    Input, real ( kind = 8 ) C, S, parameters (presumably the cosine and
  !    sine of some angle) that define a plane rotation.
  !
    implicit none
  
    real ( kind = 8 ) c
    integer ( kind = 4 ) i
    integer ( kind = 4 ) incx
    integer ( kind = 4 ) incy
    integer ( kind = 4 ) ix
    integer ( kind = 4 ) iy
    integer ( kind = 4 ) n
    real ( kind = 8 ) s
    real ( kind = 8 ) stemp
    real ( kind = 8 ) x(*)
    real ( kind = 8 ) y(*)
  
    if ( n <= 0 ) then
  
    else if ( incx == 1 .and. incy == 1 ) then
  
      do i = 1, n
        stemp = c * x(i) + s * y(i)
        y(i) = c * y(i) - s * x(i)
        x(i) = stemp
      end do
  
    else
  
      if ( 0 <= incx ) then
        ix = 1
      else
        ix = ( - n + 1 ) * incx + 1
      end if
  
      if ( 0 <= incy ) then
        iy = 1
      else
        iy = ( - n + 1 ) * incy + 1
      end if
  
      do i = 1, n
        stemp = c * x(ix) + s * y(iy)
        y(iy) = c * y(iy) - s * x(ix)
        x(ix) = stemp
        ix = ix + incx
        iy = iy + incy
      end do
  
    end if
  
    return
  end
  subroutine drotg ( sa, sb, c, s )
  
  !*****************************************************************************80
  !
  !! DROTG constructs a Givens plane rotation.
  !
  !  Discussion:
  !
  !    Given values A and B, this routine computes
  !
  !    SIGMA = sign ( A ) if abs ( A ) >  abs ( B )
  !          = sign ( B ) if abs ( A ) <= abs ( B );
  !
  !    R     = SIGMA * ( A * A + B * B );
  !
  !    C = A / R if R is not 0
  !      = 1     if R is 0;
  !
  !    S = B / R if R is not 0,
  !        0     if R is 0.
  !
  !    The computed numbers then satisfy the equation
  !
  !    (  C  S ) ( A ) = ( R )
  !    ( -S  C ) ( B ) = ( 0 )
  !
  !    The routine also computes
  !
  !    Z = S     if abs ( A ) > abs ( B ),
  !      = 1 / C if abs ( A ) <= abs ( B ) and C is not 0,
  !      = 1     if C is 0.
  !
  !    The single value Z encodes C and S, and hence the rotation:
  !
  !    If Z = 1, set C = 0 and S = 1;
  !    If abs ( Z ) < 1, set C = sqrt ( 1 - Z * Z ) and S = Z;
  !    if abs ( Z ) > 1, set C = 1/ Z and S = sqrt ( 1 - C * C );
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license. 
  !
  !  Modified:
  !
  !    15 May 2006
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Charles Lawson, Richard Hanson, 
  !    David Kincaid, Fred Krogh.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Algorithm 539, 
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    ACM Transactions on Mathematical Software,
  !    Volume 5, Number 3, September 1979, pages 308-323.
  !
  !  Parameters:
  !
  !    Input/output, real ( kind = 8 ) SA, SB.  On input, SA and SB are the values
  !    A and B.  On output, SA is overwritten with R, and SB is
  !    overwritten with Z.
  !
  !    Output, real ( kind = 8 ) C, S, the cosine and sine of the
  !    Givens rotation.
  !
    implicit none
  
    real ( kind = 8 ) c
    real ( kind = 8 ) r
    real ( kind = 8 ) roe
    real ( kind = 8 ) s
    real ( kind = 8 ) sa
    real ( kind = 8 ) sb
    real ( kind = 8 ) scale
    real ( kind = 8 ) z
  
    if ( abs ( sb ) < abs ( sa ) ) then
      roe = sa
    else
      roe = sb
    end if
  
    scale = abs ( sa ) + abs ( sb )
  
    if ( scale == 0.0D+00 ) then
      c = 1.0D+00
      s = 0.0D+00
      r = 0.0D+00
    else
      r = scale * sqrt ( ( sa / scale )**2 + ( sb / scale )**2 )
      r = sign ( 1.0D+00, roe ) * r
      c = sa / r
      s = sb / r
    end if
  
    if ( 0.0D+00 < abs ( c ) .and. abs ( c ) <= s ) then
      z = 1.0D+00 / c
    else
      z = s
    end if
  
    sa = r
    sb = z
  
    return
  end
  subroutine dscal ( n, sa, x, incx )
  
  !*****************************************************************************80
  !
  !! DSCAL scales a vector by a constant.
  !
  !  Discussion:
  !
  !    This routine uses double precision real arithmetic.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license. 
  !
  !  Modified:
  !
  !    08 April 1999
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Charles Lawson, Richard Hanson, 
  !    David Kincaid, Fred Krogh.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Algorithm 539, 
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    ACM Transactions on Mathematical Software,
  !    Volume 5, Number 3, September 1979, pages 308-323.
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) N, the number of entries in the vector.
  !
  !    Input, real ( kind = 8 ) SA, the multiplier.
  !
  !    Input/output, real ( kind = 8 ) X(*), the vector to be scaled.
  !
  !    Input, integer ( kind = 4 ) INCX, the increment between successive 
  !    entries of X.
  !
    implicit none
  
    integer ( kind = 4 ) i
    integer ( kind = 4 ) incx
    integer ( kind = 4 ) ix
    integer ( kind = 4 ) m
    integer ( kind = 4 ) n
    real ( kind = 8 ) sa
    real ( kind = 8 ) x(*)
  
    if ( n <= 0 ) then
  
    else if ( incx == 1 ) then
  
      m = mod ( n, 5 )
  
      x(1:m) = sa * x(1:m)
  
      do i = m+1, n, 5
        x(i)   = sa * x(i)
        x(i+1) = sa * x(i+1)
        x(i+2) = sa * x(i+2)
        x(i+3) = sa * x(i+3)
        x(i+4) = sa * x(i+4)
      end do
  
    else
  
      if ( 0 <= incx ) then
        ix = 1
      else
        ix = ( - n + 1 ) * incx + 1
      end if
  
      do i = 1, n
        x(ix) = sa * x(ix)
        ix = ix + incx
      end do
  
    end if
  
    return
  end
  subroutine dswap ( n, x, incx, y, incy )
  
  !*****************************************************************************80
  !
  !! DSWAP interchanges two vectors.
  !
  !  Discussion:
  !
  !    This routine uses double precision real arithmetic.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license. 
  !
  !  Modified:
  !
  !    08 April 1999
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Charles Lawson, Richard Hanson, 
  !    David Kincaid, Fred Krogh.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Algorithm 539, 
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    ACM Transactions on Mathematical Software, 
  !    Volume 5, Number 3, September 1979, pages 308-323.
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
  !
  !    Input/output, real ( kind = 8 ) X(*), one of the vectors to swap.
  !
  !    Input, integer ( kind = 4 ) INCX, the increment between successive 
  !    entries of X.
  !
  !    Input/output, real ( kind = 8 ) Y(*), one of the vectors to swap.
  !
  !    Input, integer ( kind = 4 ) INCY, the increment between successive 
  !    elements of Y.
  !
    implicit none
  
    integer ( kind = 4 ) i
    integer ( kind = 4 ) incx
    integer ( kind = 4 ) incy
    integer ( kind = 4 ) ix
    integer ( kind = 4 ) iy
    integer ( kind = 4 ) m
    integer ( kind = 4 ) n
    real ( kind = 8 ) temp
    real ( kind = 8 ) x(*)
    real ( kind = 8 ) y(*)
  
    if ( n <= 0 ) then
  
    else if ( incx == 1 .and. incy == 1 ) then
  
      m = mod ( n, 3 )
  
      do i = 1, m
        temp = x(i)
        x(i) = y(i)
        y(i) = temp
      end do
  
      do i = m + 1, n, 3
  
        temp = x(i)
        x(i) = y(i)
        y(i) = temp
  
        temp = x(i+1)
        x(i+1) = y(i+1)
        y(i+1) = temp
  
        temp = x(i+2)
        x(i+2) = y(i+2)
        y(i+2) = temp
  
      end do
  
    else
  
      if ( 0 <= incx ) then
        ix = 1
      else
        ix = ( - n + 1 ) * incx + 1
      end if
  
      if ( 0 <= incy ) then
        iy = 1
      else
        iy = ( - n + 1 ) * incy + 1
      end if
  
      do i = 1, n
        temp = x(ix)
        x(ix) = y(iy)
        y(iy) = temp
        ix = ix + incx
        iy = iy + incy
      end do
  
    end if
  
    return
  end
  function idamax ( n, dx, incx )
  
  !*****************************************************************************80
  !
  !! IDAMAX indexes the array element of maximum absolute value.
  !
  !  Discussion:
  !
  !    This routine uses double precision real arithmetic.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license. 
  !
  !  Modified:
  !
  !    08 April 1999
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Charles Lawson, Richard Hanson, 
  !    David Kincaid, Fred Krogh.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Algorithm 539, 
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    ACM Transactions on Mathematical Software,
  !    Volume 5, Number 3, September 1979, pages 308-323.
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) N, the number of entries in the vector.
  !
  !    Input, real ( kind = 8 ) X(*), the vector to be examined.
  !
  !    Input, integer ( kind = 4 ) INCX, the increment between successive 
  !    entries of SX.
  !
  !    Output, integer ( kind = 4 ) IDAMAX, the index of the element of SX of 
  !    maximum absolute value.
  !
    implicit none
  
    real ( kind = 8 ) dmax
    real ( kind = 8 ) dx(*)
    integer ( kind = 4 ) i
    integer ( kind = 4 ) idamax
    integer ( kind = 4 ) incx
    integer ( kind = 4 ) ix
    integer ( kind = 4 ) n
  
    idamax = 0
  
    if ( n < 1 .or. incx <= 0 ) then
      return
    end if
  
    idamax = 1
  
    if ( n == 1 ) then
      return
    end if
  
    if ( incx == 1 ) then
  
      dmax = abs ( dx(1) )
  
      do i = 2, n
        if ( dmax < abs ( dx(i) ) ) then
          idamax = i
          dmax = abs ( dx(i) )
        end if
      end do
  
    else
  
      ix = 1
      dmax = abs ( dx(1) )
      ix = ix + incx
  
      do i = 2, n
        if ( dmax < abs ( dx(ix) ) ) then
          idamax = i
          dmax = abs ( dx(ix) )
        end if
        ix = ix + incx
      end do
  
    end if
  
    return
  end
  
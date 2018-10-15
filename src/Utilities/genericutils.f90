module GenericUtilities
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DPREC, DSAME
  !
  implicit none 
  !
  ! -- all members are public
  public

  contains
  
  function is_same(a, b, eps) result(lvalue)
    ! -- return
    logical :: lvalue
    ! -- dummy arguments
    real(DP), intent(in)   :: a
    real(DP), intent(in)   :: b
    real(DP), intent(in), optional :: eps
    ! -- local definitions 
    real(DP) :: epsloc
    real(DP) :: denom
    real(DP) :: rdiff
    ! -- parameters
    ! -- functions
    ! -- code
    if (present(eps)) then
      epsloc = eps
    else
      epsloc = DSAME
    endif
    lvalue = .FALSE.
    if (a == b) then
      lvalue = .TRUE.
    else
      if (abs(b) > abs(a)) then
        denom = b
      else
        denom = a
        if (abs(denom) == DZERO) then
          denom = DPREC
        end if
      end if
      rdiff = abs( (a - b) / denom )
      if (rdiff <= epsloc) then
        lvalue = .TRUE.
      end if
    end if
    ! -- return
    return
  end function is_same
  
end module GenericUtilities
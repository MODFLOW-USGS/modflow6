module GenericUtilities
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DPREC, DSAME
  !
  implicit none 
  !
  ! -- all members are public
  public

  contains
  
  function is_same(a, b) result(ivalue)
    ! -- return
    integer(I4B) :: ivalue
    ! -- dummy arguments
    real(DP), intent(in)   :: a
    real(DP), intent(in)   :: b
    ! -- local definitions 
    real(DP) :: denom
    real(DP) :: rdiff
    ! -- parameters
    ! -- functions
    ! -- code
    ivalue = 0
    if (a == b) then
      ivalue = 1
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
      if (rdiff <= DSAME) then
        ivalue = 1
      end if
    end if
    ! -- return
    return
  end function is_same
  
end module GenericUtilities
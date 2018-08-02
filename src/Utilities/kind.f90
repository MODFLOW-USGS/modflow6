module KindModule
  
  implicit none

  public
  
  integer, parameter :: DP = KIND(1.0D0)              ! Precision of all real variables
  integer, parameter :: I4B = SELECTED_INT_KIND(8)    ! Integer kind
  integer, parameter :: I8B = SELECTED_INT_KIND(18)   ! Long integer kind

  contains
  
  subroutine write_kindinfo(iout)
! ******************************************************************************
! write_kindinfo -- write out information about the kind variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    integer(I4B), intent(in) :: iout
    real(DP) :: rdum = 0.
    integer(I4B) :: idum = 0
! ------------------------------------------------------------------------------
    !
    write(iout, '(a)') 'Real Variables'
    write(iout, '(2x,a,i0)') 'PRECISION: ', precision(rdum)
    write(iout, '(2x,a,i0)') 'KIND: ', DP
    write(iout, '(2x,a,1pg15.6)') 'TINY (smallest non-zero value): ', tiny(rdum)
    write(iout, '(2x,a,1pg15.6)') 'HUGE (largest value): ', huge(rdum)
    write(iout, '(a)') 'Integer Variables'
    write(iout, '(2x,a,i0)') 'KIND: ', I4B
    write(iout, '(2x,a,i0)') 'HUGE (largest value): ', huge(idum)
    write(iout, '(2x,a,i0)') 'BIT_SIZE: ', bit_size(idum)
    !
    ! -- Return
    return
  end subroutine write_kindinfo
  
end module KindModule

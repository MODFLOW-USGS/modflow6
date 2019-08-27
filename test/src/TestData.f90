module TestData
  use KindModule
  use ftnunit
  
  implicit none
  private
  
  character(len=*), public, parameter :: TESTDATADIR = '../test/data/'
  
  public :: assert_comparable_realdp
  
contains 

  subroutine assert_comparable_realdp(a, b, tol, msg)  
    real(DP) :: a, b, tol
    character(len=*) :: msg
    
    call assert_comparable_real(real(a),real(b),real(tol),msg)
    
  end subroutine
end module
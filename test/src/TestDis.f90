module TestDisModule
  use ftnunit
  use testdata
  
  use KindModule, only: I4B, DP
  use BaseDisModule
  use GwfDisModule
  
  implicit none
  private
  public :: testAllDis

contains

  subroutine testAllDis
    call test(testDisGetCellxy, "Dis, get xy coordinate for node")
  end subroutine
  
  subroutine testDisGetCellxy
    class(DisBaseType), pointer :: dis   
    real(DP) :: x, y
    
    ! create a dis with 2x6 (delr=100.0,delc=100.0) cells
    open(unit=6666, file=TESTDATADIR//"discretizations/dis_2x6.dis", status='old', action='read')
    call dis_cr(dis, 'somename4dis3d', 6666, -1)
    call dis%dis_df()
    close(6666)
    
    call dis%get_cellxy(2, x, y)
    call assert_equal_realdp(x, 150.0_DP, "x coordinate cell 2")
    call assert_equal_realdp(y, 150.0_DP, "y coordinate cell 2")
  
    call dis%get_cellxy(12, x, y)
    call assert_equal_realdp(x, 550.0_DP, "x coordinate cell 12")
    call assert_equal_realdp(y, 50.0_DP, "y coordinate cell 12")
    
  end subroutine
  
end module
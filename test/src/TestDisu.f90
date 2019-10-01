module TestDisuModule
  use ftnunit
  use testdata
  
  use KindModule, only: I4B, DP
  use BaseDisModule
  use GwfDisuModule
  
  implicit none
  private
  public :: testAllDisu
  
contains ! module procedures
    
  subroutine testAllDisu()
  
    call test(testDisuGetCellxy, "DisU, get xy coordinate for node")
     
  end subroutine
  
  subroutine testDisuGetCellxy()
    class(DisBaseType), pointer :: disu   
    real(DP) :: x, y
    
    ! create a disu
    ! in this case: 3x3 equally sized, square cells with x and y within (0,3)
    open(unit=6666, file=TESTDATADIR//"discretizations/disu_3x3.disu", status='old', action='read')
    call disu_cr(disu, 'somename4disu', 6666, -1)
    call disu%dis_df()   
    close(6666)
  
    call disu%get_cellxy(1, x, y) 
    call assert_equal_realdp(x, 0.5_DP, "x coordinate cell 1")
    call assert_equal_realdp(y, 0.5_DP, "y coordinate cell 1")
  
    call disu%get_cellxy(6, x, y) 
    call assert_equal_realdp(x, 1.5_DP, "x coordinate cell 6")
    call assert_equal_realdp(y, 2.5_DP, "y coordinate cell 6")
    
    call disu%get_cellxy(8, x, y) 
    call assert_equal_realdp(x, 2.5_DP, "x coordinate cell 6")
    call assert_equal_realdp(y, 1.5_DP, "y coordinate cell 6")
    
  end subroutine
  
end module TestDisuModule
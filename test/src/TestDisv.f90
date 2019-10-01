module TestDisvModule
  use ftnunit
  use testdata
  
  use KindModule, only: I4B, DP
  use BaseDisModule
  use GwfDisvModule
  
  implicit none
  private
  public :: testAllDisv

contains

  subroutine testAllDisv
    call test(testDisvGetCellxy, "DisV, get xy coordinate for node")
  end subroutine
  
  subroutine testDisvGetCellxy()
    class(DisBaseType), pointer :: disv   
    real(DP) :: x, y
    
    ! create a disv
    open(unit=6666, file=TESTDATADIR//"discretizations/disv_grid.disv", status='old', action='read')
    call disv_cr(disv, 'somename4disv', 6666, -1)
    call disv%dis_df()   
    close(6666)
    
    call disv%get_cellxy(14, x, y)
    call assert_equal_realdp(x, 650.0_DP, "x coordinate cell 14")
    call assert_equal_realdp(y, 550.0_DP, "y coordinate cell 14")
  
    call disv%get_cellxy(81, x, y)
    call assert_equal_realdp(x, 350.0_DP, "x coordinate cell 81")
    call assert_equal_realdp(y, 350.0_DP, "y coordinate cell 81")
    
  end subroutine
  
end module
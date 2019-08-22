module TestDisuModule
  use ftnunit
  
  use KindModule, only: I4B, DP
  use BaseDisModule
  use GwfDisuModule
  
  implicit none
  private
  public :: testAllDisu
  
contains ! module procedures
    
  subroutine testAllDisu()
  
     call test(testDisuAddCell, "DisU, set nr of nodes")
     
  end subroutine
  
  subroutine testDisuAddCell
    class(GwfDisuType), pointer :: disu
    class(DisBaseType), pointer :: disbase
    
    call disu_cr(disbase, "modelname", -1, -1)
    
    
  end subroutine
  
end module TestDisuModule
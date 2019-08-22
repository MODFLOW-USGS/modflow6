module TestGwfInterfaceModelModule
  use ftnunit  
  use KindModule, only: I4B, DP
  use GwfInterfaceModelModule
  use GridConnectionModule
  use NumericalModelModule
  
  implicit none
  private
  public :: testAllGwfInterfaceModel
  
contains ! module procedures
    
  subroutine testAllGwfInterfaceModel()
  
     call test(testConstruct, "GwfInterfaceModel, test creation of minimal interface model")
     call test(testBuildDiscretization, "GwfInterfaceModel, test building discretization")
     
  end subroutine
  
  subroutine testConstruct
    type(GwfInterfaceModelType) :: model
        
    call model%construct("mname19")     
    call assert_equal(model%name, "mname19", "model name should match")    
  end subroutine
  
  subroutine testBuildDiscretization
    class(NumericalModelType), pointer :: numModel
    type(GwfInterfaceModelType) :: ifModel
    type(GridConnectionType) :: gridConn
        
    allocate(numModel)
    call gridConn%construct(numModel, 10, "gwf19")
        
    call ifModel%construct("mname19")
    call ifModel%buildDiscretization(gridConn)
    
    call assert_true(associated(ifModel%dis),"DIS not created")
    
  end subroutine
  
end module TestGwfInterfaceModelModule
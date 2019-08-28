module TestGwfInterfaceModelModule  
  use ftnunit
  use testdata
  
  use KindModule, only: I4B, DP
  use GwfInterfaceModelModule
  use GridConnectionModule
  use NumericalModelModule
  use GwfModule
  
  use GwfDisuModule
  use GwfDisModule
  
  implicit none
  private
  public :: testAllGwfInterfaceModel
  
contains ! module procedures
    
  subroutine testAllGwfInterfaceModel()
  
     call test(testConstruct, "GwfInterfaceModel, test creation of minimal interface model")
     call test(testBuildDiscretization, "GwfInterfaceModel, test building discretization")
     call test(testModelMatrixCoefficientsNpf, "GwfInterfaceModel has correct conductivity coeffs")
     call test(testModelMatrixCoefficientsXt3d, "GwfInterfaceModel has correct conductivity coeffs for XT3D")
     
  end subroutine
  
  subroutine testConstruct
    type(GwfInterfaceModelType) :: model        
    call model%construct("mname19")     
    call assert_equal(model%name, "mname19", "model name should match")    
  end subroutine
  
  subroutine testBuildDiscretization
    type(GwfInterfaceModelType), pointer :: ifModel
    
    ifModel => getSimpleInterfaceModel("testBuildDiscretization")     
    
    call assert_true(associated(ifModel%dis),"DIS not created")
    call assert_comparable_real(real(ifModel%dis%top(1)), 0.0, 0.0, "DIS TOP values are incorrect")
    call assert_comparable_real(real(ifModel%dis%bot(4)), -2.5, 0.0, "DIS BOT values are incorrect")
    call assert_comparable_real(real(ifModel%dis%area(2)), 10000.0, 0.0, "DIS AREA values are incorrect")    
    call assert_true(associated(ifModel%dis%con), "Connectivity need to be defined")
    call assert_equal(ifModel%dis%con%njas, 2, "Expected nr. of symmetric connections (njas)")
        
  end subroutine
  
  subroutine testModelMatrixCoefficientsNpf
    class(GwfInterfaceModelType), pointer :: ifModel
    integer(I4B) :: kiter, inwtflag, nja
    real(DP), dimension(:), allocatable :: amat
    
    ifModel => getSimpleInterfaceModel("ifmodel_01")    
    
    kiter = 1
    inwtflag = 0
    nja = ifModel%dis%con%nja
    allocate(amat(nja))
    
    ! continue here:
    ! call ifModel%model_fc(kiter, amat, nja, inwtflag)
    
    ! test on amat here:
    call assert_true(.false., "Interface model should calculate npf conductivity")
    
  end subroutine
  
  subroutine testModelMatrixCoefficientsXt3d
    call assert_true(.false., "Interface model should calculate xt3d conductivity")
  end subroutine
  
  ! get simple interface model from dis_A.dis and dis_B.dis
  function getSimpleInterfaceModel(name) result(ifmodel)
    character(len=*), intent(in)  :: name
    class(GwfInterfaceModelType), pointer :: ifmodel
    type(GridConnectionType) :: gc
    
    allocate(ifmodel)
    
    ! set up if model
    gc = createGridConnectionBetween2by3with2by5grid("ifmodel_02")    
    call ifmodel%construct("ifmodel_02")    
    call ifModel%createModel(gc)
    
    ! call ifModel%model_ar()
    
  end function
  
  ! create connection between two models
  function createGridConnectionBetween2by3with2by5grid(name) result(gc)
    character(len=*), intent(in)  :: name
    type(GridConnectionType) :: gc
    class(GwfModelType), pointer :: gwfModelA, gwfModelB
    class(NumericalModelType), pointer :: numModel
    
    ! create models
    ! 2x3
    allocate(gwfModelA)
    call gwfModelA%allocate_scalars("TestModelA")
    gwfModelA%name = "TestModelA"
    open(unit=901, file=TESTDATADIR//"dis_A.dis", status='old', action='read')
    call dis_cr(gwfModelA%dis, gwfModelA%name, 901, -1)
    call gwfModelA%dis%dis_df()
    close(901)
    
    ! 2x6
    allocate(gwfModelB)
    call gwfModelB%allocate_scalars("TestModelB")
    gwfModelB%name = "TestModelB"
    open(unit=902, file=TESTDATADIR//"dis_B.dis", status='old', action='read')
    call dis_cr(gwfModelB%dis, gwfModelB%name, 902, -1)
    call gwfModelB%dis%dis_df()
    close(902)
    
    ! cast up and construct grid connection
    numModel => gwfModelA
    call gc%construct(numModel, 2, name)
        
    ! add links between models
    numModel => gwfModelB
    call gc%addLink(3, 1, 50.0_DP, 50.0_DP, 100.0_DP, 1, numModel)
    call gc%addLink(6, 7, 50.0_DP, 50.0_DP, 100.0_DP, 1, numModel)
    
    ! build conn. matrix
    call gc%extendConnection(0, 0)
    
    call assert_true(gc%nrOfLinks == 2, "Nr of links in GC should be 2")  
    call assert_equal(size(gc%idxToGlobal), 4, "We need index mapping for interface cells")
    call assert_equal(gc%connections%nja, 8, "Expecting 4 (diag) + 2*2 (conn) nozeros in matrix")
    
  end function
  
  
end module TestGwfInterfaceModelModule
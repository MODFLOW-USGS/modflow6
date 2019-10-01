module TestGwfInterfaceModelModule  
  use ftnunit
  use testdata
  
  use KindModule, only: I4B, DP
  use GwfInterfaceModelModule
  use GridConnectionModule
  use NumericalModelModule
  use NumericalExchangeModule
  use GwfModule
  
  use GwfDisuModule
  use GwfDisModule
  use GwfNpfModule
  
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
    integer(I4B) :: n, idiag, ioff
    
    ifModel => getSimpleInterfaceModel("ifmodel_01")    
    
    kiter = 1
    inwtflag = 0
    nja = ifModel%dis%con%nja
    allocate(amat(nja))
    amat = -1.0_DP
    
    ! continue here:
    call ifModel%model_fc(kiter, amat, nja, inwtflag)
    
    ! test on amat here:
    do n = 1, ifModel%dis%nodes
      idiag = ifModel%dis%con%ia(n)
      call assert_true(amat(ifModel%idxglo(idiag)) < 0, "Diagonal elements should be negative")
      do ioff = ifModel%dis%con%ia(n) + 1, ifModel%dis%con%ia(n+1) - 1
        call assert_true(amat(ifModel%idxglo(ioff)) > 0, "Off-diagonal elements should be positive")
      end do
    end do
    
  end subroutine
  
  subroutine testModelMatrixCoefficientsXt3d
    call assert_true(.false., "Interface model should calculate xt3d conductivity")
  end subroutine
  
  ! get simple interface model from dis_A.dis and dis_B.dis
  function getSimpleInterfaceModel(name) result(ifmodel)
    character(len=*), intent(in)  :: name
    class(GwfInterfaceModelType), pointer :: ifmodel
    class(GridConnectionType), pointer :: gc
    
    allocate(ifmodel)
    allocate(gc)
    
    ! set up if model
    gc => createGridConnectionBetween2by3with2by6grid("ifmodel_02")    
    call ifmodel%construct("ifmodel_02")    
    call ifModel%createModel(gc)
    
    ! call ifModel%model_ar()
    
  end function
  
  ! create connection between two models
  function createGridConnectionBetween2by3with2by6grid(name) result(gc)
    use TestNumericalExchangeHelperModule
    character(len=*), intent(in)  :: name
    class(GridConnectionType), pointer :: gc
    class(GwfModelType), pointer :: gwfModelA, gwfModelB
    class(NumericalModelType), pointer :: numModel, numModel2
    class(NumericalExchangeType), pointer :: numEx
    
    ! create models
    ! 2x3
    allocate(gwfModelA)
    call gwfModelA%allocate_scalars("TestModelA")
    allocate(gwfModelA%x(6))
    allocate(gwfModelA%ibound(6))
    gwfModelA%x = 0.0_DP
    gwfModelA%ibound = 1
    gwfModelA%name = "TestModelA"
    open(unit=1980, file=TESTDATADIR//"discretizations/dis_2x3.dis", status='old', action='read')
    call dis_cr(gwfModelA%dis, gwfModelA%name, 1980, -1)
    call gwfModelA%dis%dis_df()    
    close(1980)
    call npf_cr(gwfModelA%npf, gwfModelA%name, -1, -1)
    allocate(gwfModelA%npf%k11(gwfModelA%dis%nodes))
    allocate(gwfModelA%npf%icelltype(gwfModelA%dis%nodes))
    gwfModelA%npf%k11 = 1.0_DP
    gwfModelA%npf%icelltype = 1
    
    ! 2x6
    allocate(gwfModelB)
    call gwfModelB%allocate_scalars("TestModelB")
    allocate(gwfModelB%x(12))
    allocate(gwfModelB%ibound(12))
    gwfModelB%x = 0.0_DP
    gwfModelB%ibound = 1
    gwfModelB%name = "TestModelB"
    open(unit=1980, file=TESTDATADIR//"discretizations/dis_2x6.dis", status='old', action='read')
    call dis_cr(gwfModelB%dis, gwfModelB%name, 1980, -1)
    call gwfModelB%dis%dis_df()
    close(1980)
    call npf_cr(gwfModelB%npf, gwfModelB%name, -1, -1)
    allocate(gwfModelB%npf%k11(gwfModelB%dis%nodes))
    allocate(gwfModelB%npf%icelltype(gwfModelB%dis%nodes))
    gwfModelB%npf%k11 = 2.0_DP
    gwfModelB%npf%icelltype = 1
    
    ! cast up and construct grid connection
    allocate(gc)
    numModel => gwfModelA
    call gc%construct(numModel, 2, name)
        
    ! add links between models
    numModel2 => gwfModelB
    call gc%connectCell(3, numModel, 1, numModel2)
    call gc%connectCell(6, numModel, 7, numModel2)
    
    ! create model topology
    numEx => getNumericalExchange(gc%model, numModel2, 2)
    numEx%nodem1(1) = 3
    numEx%nodem2(1) = 1
    numEx%nodem1(2) = 6
    numEx%nodem2(2) = 7
    call gc%addModelLink(numEx, 1)
    
    ! build conn. matrix
    call gc%extendConnection(1, 1)
    
    call assert_true(gc%nrOfBoundaryCells == 2, "Nr of links in GC should be 2")  
    call assert_equal(size(gc%idxToGlobal), gc%nrOfCells, "We need index mapping for interface cells")
    call assert_equal(gc%connections%nja, 8, "Expecting 4 (diag) + 2*2 (conn) nozeros in matrix")
    
  end function
  
  
end module TestGwfInterfaceModelModule
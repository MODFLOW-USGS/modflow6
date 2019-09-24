module TestGridConnectionModule
  use ftnunit
  use TestData
  
  use KindModule, only: I4B, DP
  use SparseModule, only: sparsematrix
  use GridConnectionModule
  use NumericalExchangeModule
  use NumericalModelModule
  use GwfDisuModule
  use GwfDisModule
  
  implicit none
  private
  public :: testAllGridConnection
  
contains ! module procedures
    
  subroutine testAllGridConnection()
  
    call test(testConstructor, "GridConnection, test constructor")
    call test(testAddSomeLinks, "GridConnection, test adding some links")
    call test(testBasicConnectivity, "GridConnection, test creating a basic connectivity matrix")
    call test(testNbrOfNbrConnectivity, "GridConnection, test creating a connectivity matrix for XT3D")
    call test(testTVDConnectivity, "GridConnection, test creating a connectivity matrix for TVD")
    call test(testModelConnectivity, "GridConnection, test connecting models up to nbrs-of-nbrs")
    
  end subroutine
  
  subroutine testConstructor()
    type(GridConnectionType) :: gridConnection
    
    gridConnection = getSimpleGridConnectionWithModel("dummy")        
    
    call assert_true(gridConnection%linkCapacity == 2, "space reserved should be equal to 10")    
    call assert_true(size(gridConnection%boundaryCells) == 2, "local nodes array not properly allocated")
    call assert_true(size(gridConnection%connectedCells) == 2, "connected nodes array not properly allocated") 
    
  end subroutine
  
  subroutine testAddSomeLinks()
    type(GridConnectionType) :: gridConnection    
    class(NumericalModelType), pointer :: nbrModel
    
    gridConnection = getSimpleGridConnectionWithModel("dummy2")   
    
    allocate(nbrModel)
    call nbrModel%allocate_scalars("neighbor")
    
    
    call gridConnection%connectCell(1, gridConnection%model, 4, nbrModel) ! horizontal links
    call gridConnection%connectCell(2, gridConnection%model, 5, nbrModel)
    
    call assert_true(gridConnection%nrOfBoundaryCells == 2, "nr of links should have been 2")    
    
  end subroutine
  
  subroutine testBasicConnectivity
    use TestNumericalExchangeHelperModule
    type(GridConnectionType) :: gridConnection    
    class(NumericalModelType), pointer :: nbrModel  
    class(NumericalExchangeType), pointer :: numEx
    type(sparsematrix) :: connectivity
    
    ! setup connection
    gridConnection = getSimpleGridConnectionWithModel("someModel")    
    allocate(nbrModel)
    call nbrModel%allocate_scalars("someNeighbor")
    nbrModel%moffset = 100
    open(unit=1980, file=TESTDATADIR//"dis_2x3.dis", status='old', action='read')
    call dis_cr(nbrModel%dis, nbrModel%name, 1980, -1)
    call nbrModel%dis%dis_df()    
    close(1980)
    
    ! build connectivity, among other things
    numEx => getNumericalExchange(gridConnection%model, nbrModel, 2)
    numEx%nodem1(1) = 6
    numEx%nodem2(1) = 1
    numEx%nodem1(2) = 12    
    numEx%nodem2(2) = 4
    
    call gridConnection%connectCell(6, gridConnection%model, 1, nbrModel) ! horizontal links
    call gridConnection%connectCell(12, gridConnection%model, 4, nbrModel)
    
    call gridConnection%addModelLink(numEx, 1)
    call gridConnection%extendConnection(1, 1)
    
    call assert_equal(gridConnection%connections%nodes, 4, "nr. of nodes wrong")
    call assert_equal(gridConnection%connections%njas, 2, "nr. of (sym) connections wrong")
    
  end subroutine
  
  subroutine testNbrOfNbrConnectivity
    use TestNumericalExchangeHelperModule
    type(GridConnectionType) :: gridConnection    
    class(NumericalModelType), pointer :: nbrModel  
    class(NumericalExchangeType), pointer :: numEx
    type(sparsematrix) :: connectivity
    
    ! setup connection
    gridConnection = getSimpleGridConnectionWithModel("someModel2")    
    
    ! get nbr model
    allocate(nbrModel)
    call nbrModel%allocate_scalars("someNeighbor2")
    nbrModel%moffset = 12    
    open(unit=1980, file=TESTDATADIR//"dis_2x3.dis", status='old', action='read')
    call dis_cr(nbrModel%dis, nbrModel%name, 1980, -1)
    call nbrModel%dis%dis_df()    
    close(1980)
    
    numEx => getNumericalExchange(gridConnection%model, nbrModel, 2)
    numEx%nodem1(1) = 6
    numEx%nodem2(1) = 1
    numEx%nodem1(2) = 12
    numEx%nodem2(2) = 4
    call gridConnection%connectCell(6, gridConnection%model, 1, nbrModel) ! horizontal links
    call gridConnection%connectCell(12, gridConnection%model, 4, nbrModel)
    
      
    call gridConnection%addModelLink(numEx, 1)
    
    call gridConnection%extendConnection(3, 2)
    
    call assert_equal(gridConnection%boundaryCells(1)%nrOfNbrs, 2, "Nr. of nbrs for local cell 1")  
    call assert_equal(gridConnection%boundaryCells(1)%neighbors(1)%nrOfNbrs, 2, "Nr. of nbrs for nbr 1 of local cell 1, should be 2")
    
    call assert_equal(gridConnection%connectedCells(1)%nrOfNbrs, 2, "Nr. of nbrs for remote cell 1")
    call assert_equal(gridConnection%connectedCells(2)%nrOfNbrs, 2, "Nr. of nbrs for remote cell 2")
    call assert_equal(gridConnection%connectedCells(2)%neighbors(2)%nrOfNbrs, 0, "nrOfNbrs for nbr 2 for remote cell 2 should be zero")
    
  end subroutine
  
  subroutine testTVDConnectivity
    call assert_true(.false., "not implemented")
  end subroutine
  
  subroutine testModelConnectivity
    type(GridConnectionType) :: gc  
    class(NumericalModelType), pointer :: numericalModel, numModTemp
    class(NumericalModelType), pointer :: nbr1, nbr2, nbr3, nnbr11, nnbr12, nnbr21, nnbr31, nnnbr111
    class(NumericalExchangeType), pointer :: ex1, ex2, ex3, ex11, ex12, ex21, ex31, ex111
    integer(I4B) :: nrNbrsOfNbrs, im
    
    
    !
    ! nnbr21 -- nbr2 -- model -- nbr1 -- nnbr11 -- nbr111
    !                     |       |
    !         nnbr31 -- nbr3    nnbr12
    !
    allocate(numericalModel)
    allocate(nbr1, nbr2, nbr3, nnbr11, nnbr12, nnbr21, nnbr31, nnnbr111)
    allocate(ex1, ex2, ex3, ex11, ex12, ex21, ex31, ex111)
    
    call gc%construct(numericalModel, 10, "someGridConn")
    
    ex21%m1 => nnbr21
    ex21%m2 => nbr2
    ex2%m1 => nbr2
    ex2%m2 => numericalModel
    ex1%m1 => numericalModel
    ex1%m2 => nbr1
    ex11%m1 => nbr1
    ex11%m2 => nnbr11
    ex12%m1 => nbr1
    ex12%m2 => nnbr12
    ex3%m1 => numericalModel
    ex3%m2 => nbr3
    ex31%m1 => nnbr31
    ex31%m2 => nbr3
    
    ! add links
    call gc%addModelLink(ex1, 2)
    call gc%addModelLink(ex2, 2)
    call gc%addModelLink(ex3, 2)
    call gc%addModelLink(ex11, 2)
    call gc%addModelLink(ex12, 2)
    call gc%addModelLink(ex21, 2)
    call gc%addModelLink(ex31, 2)
    call gc%addModelLink(ex111, 2) ! this should not be added at depth == 2
    
    call assert_true(associated(gc%modelWithNbrs), "Model with nbrs should be allocated")
    call assert_equal(gc%modelWithNbrs%nrOfNbrs, 3, "Number of neighbors should match")
    call assert_true(associated(gc%modelWithNbrs%neighbors(1)%model, nbr1), "Neighboring model should be assigned")
    
    do im = 1, gc%regionalModels%Count()
      numModTemp => GetNumericalModelFromList(gc%regionalModels, im)
      if (associated(numModTemp, nnnbr111)) then
        call assert_true(.false., "This model should not have been part of the region")
      end if
    end do
      
  end subroutine
  
  ! helper function to setup basic gridconn
  function getSimpleGridConnectionWithModel(name) result(gc)
    type(GridConnectionType) :: gc  
    character(len=*) :: name
    class(NumericalModelType), pointer :: numericalModel
    
    allocate(numericalModel)    
    call numericalModel%allocate_scalars(name)
    numericalModel%moffset = 0
    open(unit=1980, file=TESTDATADIR//"dis_2x6.dis", status='old', action='read')
    call dis_cr(numericalModel%dis, numericalModel%name, 1980, -1)
    call numericalModel%dis%dis_df()    
    close(1980)
    
    call gc%construct(numericalModel, 2, "gridConnName") 
    
  end function
  
end module TestGridConnectionModule 
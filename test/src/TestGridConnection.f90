module TestGridConnectionModule
  use ftnunit
  use TestData
  
  use KindModule, only: I4B, DP
  use SparseModule, only: sparsematrix
  use GridConnectionModule
  use NumericalModelModule, only: NumericalModelType
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
    call assert_true(size(gridConnection%localCells) == 2, "local nodes array not properly allocated")
    call assert_true(size(gridConnection%connectedCells) == 2, "connected nodes array not properly allocated")    
    call assert_true(size(gridConnection%linkGeometries) == 2, "link geometries array not properly allocated")   
    
  end subroutine
  
  subroutine testAddSomeLinks()
    type(GridConnectionType) :: gridConnection    
    class(NumericalModelType), pointer :: nbrModel
    
    gridConnection = getSimpleGridConnectionWithModel("dummy2")   
    
    allocate(nbrModel)
    call nbrModel%allocate_scalars("neighbor")
    
    
    call gridConnection%addLink(1, 4, 10.0_DP, 10.0_DP, 20.0_DP, 1, nbrModel) ! horizontal links
    call gridConnection%addLink(2, 5, 10.0_DP, 10.0_DP, 20.0_DP, 1, nbrModel)
    
    call assert_true(gridConnection%nrOfLinks == 2, "nr of links should have been 2")
    
    call gridConnection%addLink(999, 999, 10.0_DP, 10.0_DP, 20.0_DP, 1, nbrModel)
    call assert_true(gridConnection%nrOfLinks == 2, "cannot add beyond capacity, should have been 2")
    
  end subroutine
  
  subroutine testBasicConnectivity
    type(GridConnectionType) :: gridConnection    
    class(NumericalModelType), pointer :: nbrModel  
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
    
    call gridConnection%addLink(1, 1, 10.0_DP, 20.0_DP, 30.0_DP, 1, nbrModel) ! horizontal links
    call gridConnection%addLink(2, 2, 11.0_DP, 21.0_DP, 31.0_DP, 1, nbrModel)
    
    ! build connectivity, among other things
    call gridConnection%extendConnection(0, 0)
    
    call assert_equal(gridConnection%connections%nodes, 4, "nr. of nodes wrong")
    call assert_equal(gridConnection%connections%njas, 2, "nr. of (sym) connections wrong")
    
    call assert_comparable_realdp(gridConnection%connections%cl1(1), 10.0_DP, 0.0_DP, "conn. length wrong")
    call assert_comparable_realdp(gridConnection%connections%cl2(2), 21.0_DP, 0.0_DP, "conn. length wrong")
    call assert_comparable_realdp(gridConnection%connections%hwva(2), 31.0_DP, 0.0_DP, "hwva area wrong")
    
  end subroutine
  
  subroutine testNbrOfNbrConnectivity
    type(GridConnectionType) :: gridConnection    
    class(NumericalModelType), pointer :: nbrModel  
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
    
    call gridConnection%addLink(6, 1, 10.0_DP, 10.0_DP, 20.0_DP, 1, nbrModel) ! horizontal links
    call gridConnection%addLink(12, 4, 10.0_DP, 10.0_DP, 20.0_DP, 1, nbrModel)
    
    call gridConnection%extendConnection(2, 1)
    
    call assert_equal(gridConnection%localCells(1)%nrOfNbrs, 2, "Nr. of nbrs for local cell 1")  
    call assert_equal(gridConnection%localCells(1)%neighbors(1)%nrOfNbrs, 2, "Nr. of nbrs for nbr 1 of local cell 1, should be 2")
    
    call assert_equal(gridConnection%connectedCells(1)%nrOfNbrs, 2, "Nr. of nbrs for remote cell 1")
    call assert_equal(gridConnection%connectedCells(2)%nrOfNbrs, 2, "Nr. of nbrs for remote cell 2")
    call assert_equal(gridConnection%connectedCells(2)%neighbors(2)%nrOfNbrs, 0, "nrOfNbrs for nbr 2 for remote cell 2 should be zero")
    
  end subroutine
  
  subroutine testTVDConnectivity
    call assert_true(.false., "not implemented")
  end subroutine
  
  subroutine testModelConnectivity
    type(GridConnectionType) :: gc  
    class(NumericalModelType), pointer :: numericalModel
    class(NumericalModelType), pointer :: nbr1, nbr2, nbr3, nnbr11, nnbr12, nnbr21, nnbr31
    integer(I4B) :: nrNbrsOfNbrs
    
    
    !
    ! nnbr21 -- nbr2 -- model -- nbr1 -- nnbr11
    !                     |       |
    !         nnbr31 -- nbr3    nnbr12
    !
    allocate(numericalModel)
    call gc%construct(numericalModel, 10, "someGridConn") 
    
    allocate(nbr1, nbr2, nbr3, nnbr11, nnbr12, nnbr21, nnbr31)
    
    ! add links
    call gc%addModelLink(numericalModel, nbr1)
    call gc%addModelLink(nbr2, numericalModel)
    call gc%addModelLink(numericalModel, nbr3)
    call gc%addModelLink(nbr1, nnbr11)
    call gc%addModelLink(nbr1, nnbr12)
    call gc%addModelLink(nbr2, nnbr21)
    call gc%addModelLink(nbr3, nnbr31)
    
    call assert_true(associated(gc%modelWithNbrs), "Model with nbrs should be allocated")
    call assert_equal(gc%modelWithNbrs%nrOfNbrs, 3, "Number of neighbors should match")
    call assert_true(associated(gc%modelWithNbrs%neighbors(1)%model, nbr1), "Neighboring model should be assigned")
    call assert_equal(gc%modelWithNbrs%neighbors(1)%nrofNbrs, 2, "Nr of neighbors of neighbors should equal 2")
    
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
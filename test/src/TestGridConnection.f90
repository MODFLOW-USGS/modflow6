module TestGridConnectionModule
  use ftnunit
  
  use KindModule, only: I4B, DP
  use SparseModule, only: sparsematrix
  use GridConnectionModule
  use NumericalModelModule, only: NumericalModelType
  use GwfDisuModule
  
  implicit none
  private
  public :: testAllGridConnection
  
contains ! module procedures
    
  subroutine testAllGridConnection()
  
    call test(testConstructor, "GridConnection, test constructor")
    call test(testAddSomeLinks, "GridConnection, test adding some links")
    call test(testBasicConnectivity, "GridConnection, test creating a basic connectivity matrix")
    call test(testXT3DConnectivity, "GridConnection, test creating a connectivity matrix for XT3D")
    call test(testTVDConnectivity, "GridConnection, test creating a connectivity matrix for TVD")
    
  end subroutine
  
  subroutine testConstructor()
    type(GridConnectionType) :: gridConnection
    
    gridConnection = getSimpleGridConnectionWithModel()        
    
    call assert_true(gridConnection%linkCapacity == 3, "space reserved should be equal to 10")    
    call assert_true(size(gridConnection%localCells) == 3, "local nodes array not properly allocated")
    call assert_true(size(gridConnection%connectedCells) == 3, "connected nodes array not properly allocated")    
    call assert_true(size(gridConnection%linkGeometries) == 3, "link geometries array not properly allocated")   
    
  end subroutine
  
  subroutine testAddSomeLinks()
    type(GridConnectionType) :: gridConnection    
    class(NumericalModelType), pointer :: nbrModel
    
    gridConnection = getSimpleGridConnectionWithModel()   
    
    allocate(nbrModel)
    call nbrModel%allocate_scalars("neighbor")
    
    
    call gridConnection%addLink(1, 4, 10.0_DP, 10.0_DP, 20.0_DP, 1, nbrModel) ! horizontal links
    call gridConnection%addLink(2, 5, 10.0_DP, 10.0_DP, 20.0_DP, 1, nbrModel)
    call gridConnection%addLink(3, 6, 10.0_DP, 10.0_DP, 20.0_DP, 1, nbrModel)
    
    call assert_true(gridConnection%nrOfLinks == 3, "nr of links should have been 3")
    
    call gridConnection%addLink(999, 999, 10.0_DP, 10.0_DP, 20.0_DP, 1, nbrModel)
    call assert_true(gridConnection%nrOfLinks == 3, "cannot add beyond capacity, should have been 3")
    
  end subroutine
  
  subroutine testBasicConnectivity
    type(GridConnectionType) :: gridConnection    
    class(NumericalModelType), pointer :: nbrModel  
    type(sparsematrix) :: connectivity
    
    ! setup connection
    gridConnection = getSimpleGridConnectionWithModel()    
    allocate(nbrModel)
    call nbrModel%allocate_scalars("someNeighbor")
    nbrModel%moffset = 100
    call disu_cr(nbrModel%dis, nbrModel%name, -1, -1)
    
    call gridConnection%addLink(1, 1, 10.0_DP, 10.0_DP, 20.0_DP, 1, nbrModel) ! horizontal links
    call gridConnection%addLink(2, 2, 10.0_DP, 10.0_DP, 20.0_DP, 1, nbrModel)
    call gridConnection%addLink(3, 3, 10.0_DP, 10.0_DP, 20.0_DP, 1, nbrModel)
    
    ! build connectivity, among other things
    call gridConnection%extendConnection(0, 0)
    
    call assert_equal(gridConnection%connectivity%nrow, 6, "nr. of rows wrong")
    call assert_equal(gridConnection%connectivity%ncol, 6, "nr. of columns wrong")
    call assert_equal(gridConnection%connectivity%nnz, 9, "nr. of nonzeros wrong")
    
  end subroutine
  
  subroutine testXT3DConnectivity
    call assert_true(.false., "not implemented")
  end subroutine
  
  subroutine testTVDConnectivity
    call assert_true(.false., "not implemented")
  end subroutine
  
  ! helper function to setup basic gridconn
  function getSimpleGridConnectionWithModel() result(gc)
    type(GridConnectionType) :: gc    
    class(NumericalModelType), pointer :: numericalModel
    
    allocate(numericalModel)    
    call numericalModel%allocate_scalars("someModel")
    numericalModel%moffset = 0
    call disu_cr(numericalModel%dis, numericalModel%name, -1, -1)
    
    call gc%construct(numericalModel, 3, "gridConnName") 
    
  end function
  
end module TestGridConnectionModule 
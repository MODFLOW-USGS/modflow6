module TestMeshConnectionModule
  use ftnunit
  use MeshConnectionModule
  
  implicit none
  private
  public :: testMeshConnection
  
contains ! module procedures
    
  subroutine testMeshConnection()
  
    call test(testConstructor, "MeshConnection constructor")
    call test(testMemory, "Meshconnection memory management")
    call test(testAddConnection, "Add connection")
    
  end subroutine
  
  subroutine testConstructor()
  
    type(MeshConnectionType) :: meshConnection
    call meshConnection%construct(10, "meshConnName")    
        
    call assert_true(meshConnection%nrOfConnections == 10, "nr. of connections not equal to 10")    
    call assert_true(size(meshConnection%localNodes) == 10, "local nodes array not properly allocated")
    call assert_true(size(meshConnection%connectedNodes) == 10, "connected nodes array not properly allocated")    
    
  end subroutine
  
  subroutine testMemory()
    use MemoryManagerModule
    use MemoryTypeModule
    
    logical :: found
    type(MemoryType), pointer :: mt    
    type(MeshConnectionType) :: meshConnection
    
    integer, pointer :: nconn    
    
    call meshConnection%construct(10, "meshConnName")     
    call mem_setptr(nconn, "NRCONN", meshConnection%memOrigin)
    
    call assert_true(associated(nconn), "nr of connections unallocated: NCONN")
    call assert_true(nconn == 10, "nr of connections has wrong value")
    
  end subroutine
  
  subroutine testAddConnection()
    type(MeshConnectionType) :: meshConnection
    integer :: i
    integer, dimension(10) :: arr
    
    arr =  (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /)
    
    call meshConnection%construct(10, "meshConnName")    
    do i=1,10
      call meshConnection%addConnection(i, i, i+10)
    end do
    
    call assert_equal(meshConnection%localNodes, arr, "local nodes do not match")
    
    arr = arr + 10
    call assert_equal(meshConnection%connectedNodes, arr, "connected nodes do not match")
    
  end subroutine
  
end module TestMeshConnectionModule 
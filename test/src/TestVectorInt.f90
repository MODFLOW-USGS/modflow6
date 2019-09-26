module TestVectorIntModule
  use ftnunit
  use KindModule, only: I4B
  use VectorIntModule
  
  implicit none
  private
  public :: testAllVectorInt
  
contains
 
  subroutine testAllVectorInt()
  
     call test(testInit, "Initialize vector")
     call test(testPushBack, "Push elements into vector")
     call test(testPushBackAndExpand, "Push elements into vector and expand when needed")
     call test(testAt, "Get random access elements from vector")
     call test(testClear, "Clear all elements from vector, should leave memory unchanged")
     call test(testShrinkToFit, "Deletes excess memory to have a capacity matching the vector size")
     call test(testDestroy, "Deletes memory for the vector")
     
  end subroutine
  
  subroutine testInit()
    type(VectorInt) :: vec
    
    call vec%init()
    
    call assert_equal(vec%capacity, 4, "Default capacity should be set after init")
    call assert_equal(vec%size, 0, "Initial size should be zero")
    
  end subroutine
  
  subroutine testPushBack()
    type(VectorInt) :: vec
    integer(I4B) :: i
    
    call vec%init(10)
    
    do i = 1, 10
      call vec%push_back(i)
    end do
    
    call assert_equal(vec%size, 10, "Vector should contain 10 elements")
  end subroutine
  
  subroutine testPushBackAndExpand()
    type(VectorInt) :: vec
    integer(I4B) :: i
    
    call vec%init(4)
    
    do i = 1, 10
      call vec%push_back(i)
    end do
    
    call assert_equal(vec%size, 10, "Vector should have expanded and contain 10 elements")
  
  end subroutine
  
  subroutine testAt()
    type(VectorInt) :: vec
    integer(I4B) :: i
    
    call vec%init()
    do i = 1, 10
      call vec%push_back(i)
    end do
    
    call assert_equal(vec%at(3), 3, "Vector element should match")
    call assert_equal(vec%at(9), 9, "Vector element should match")
    
  end subroutine
  
  subroutine testClear()
    type(VectorInt) :: vec
    integer(I4B) :: i, capacityBefore
    
    ! fill some elements
    call vec%init()
    do i = 1, 100
      call vec%push_back(i)  
    end do
    
    call assert_equal(vec%size, 100, "Vector should contain elements here")
  
    capacityBefore = vec%capacity
    call vec%clear()
    
    call assert_equal(vec%size, 0, "Vector should be empty now")    
    call assert_equal(vec%capacity, capacityBefore, "Vector capacity should not be changed") 
    
  end subroutine
  
  subroutine testShrinkToFit()
    type(VectorInt) :: vec
    integer(I4B) :: i
    
    ! fill some elements
    call vec%init()
    do i = 1, 100
      call vec%push_back(i)  
    end do
    
    call vec%shrink_to_fit()
    call assert_equal(vec%size, vec%capacity, "Shrunk vector so size should be equal to capacity")
    
    call vec%push_back(101)
    call vec%shrink_to_fit()
    call assert_equal(vec%size, vec%capacity, "Shrunk vector so size should be equal to capacity (take 2)")
    
  end subroutine
  
  subroutine testDestroy()
    type(VectorInt) :: vec
    
    call vec%init()
    call vec%destroy()
    
    call assert_equal(vec%capacity, 0, "Vector should be deleted now")
  end subroutine
  
end module
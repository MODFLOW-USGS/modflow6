program test_mf6lib
  use ftnunit
  
  ! use modules with tests:
  use TestNpfModule
  use TestGridConnectionModule
  use TestDisModule
  use TestDisvModule
  use TestDisuModule
  use TestGwfInterfaceModelModule
  use TestVectorIntModule
  
  implicit none
  
  ! setup ftnunit run file
  call prepareTests()
  call runtests_init()
  
  ! call all tests
  call testAllNpf()
  call testAllGridConnection()
  call testAllDis()
  call testAllDisv()
  call testAllDisu()
  call testAllGwfInterfaceModel()
  call testAllVectorInt()
  
  ! show results in browser 
  call showResult()
  call runtests_final()
  
  
contains ! program procedures
  
  ! Routine to start the testing
  ! Note: This routine merely takes care that the unit tests are indeed run
  subroutine prepareTests

      integer  :: lun   !< LU-number

      ! cleanup
      call system( 'del ftnunit.html' )
      
      open( newunit=lun, file = 'ftnunit.run' )
      write( lun, '(a)' ) 'ALL'
      close( lun )

  end subroutine prepareTests

  ! Start the browser to show the result
  subroutine showResult
      !character(len=1) :: answer
      !
      !write(*,*)     'Press ENTER ...'
      !read(*,'(a)' ) answer

      call system( 'ftnunit.html' )

  end subroutine showResult
  
end program 
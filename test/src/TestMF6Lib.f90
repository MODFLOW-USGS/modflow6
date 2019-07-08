program test_mf6lib
  use ftnunit
  
  ! use modules with tests:
  use TestMeshConnectionModule
  
  implicit none
  
  ! setup ftnunit run file
  call prepareTests()
  call runtests_init()
  
  ! call all tests
  call testMeshConnection()
  
  ! show results in browser 
  ! call showResult()
  call runtests_final()
  
  
contains ! program procedures
  
  ! Routine to start the testing
  ! Note: This routine merely takes care that the unit tests are indeed run
  subroutine prepareTests

      integer  :: lun   !< LU-number

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
!> @brief MODFLOW 6 main entry point
!!
!! This is the main entry point for the MODFLOW 6 program.
!!
!<
program mf6
  ! -- modules
  use Mf6CoreModule
#include <petsc/finclude/petscsys.h>
  use petscsys

  PetscErrorCode ierr

  call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
  if (ierr .ne. 0) then
    print*,'Unable to initialize PETSc'
    stop
  endif
  !
  ! -- run
  call Mf6Run()

  call PetscFinalize(ierr)
  
end program
  
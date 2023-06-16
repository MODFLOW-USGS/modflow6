module DefinedMacros
  ! -- modules
  use KindModule, only: LGP, I4B
  use ConstantsModule, only: OSUNDEF, OSLINUX, OSMAC, OSWIN
  implicit none
  private
  public :: get_os, is_pro, using_petsc
contains

  !> @brief Get operating system
  !!
  !! Function to get the integer operating system enum value.
  !!
  !! @return      ios                operating system enum
  !<
  function get_os() result(ios)
    ! -- local variables
    integer(I4B) :: ios !< operating system
    !
    ! -- initialize ios
    ios = OSUNDEF
    !
    ! -- set operating system variables
#ifdef __GFORTRAN__
# ifdef __linux__
    ios = OSLINUX
# endif
# ifdef __APPLE__
    ios = OSMAC
# endif
# ifdef _WIN32
    ios = OSWIN
# endif
#endif
#ifdef __INTEL_COMPILER
# ifdef __linux__
    ios = OSLINUX
# endif
# ifdef __APPLE__
    ios = OSMAC
# endif
# ifdef _WIN32
    ios = OSWIN
# endif
#endif
    !
    ! return
    return
  end function get_os

  !> @brief Determine if this is the professional version
  !!
  !! Function to get a logical indicating if this is the
  !! professional version of MODFLOW.
  !!
  !! @return      ispro                pro version logical
  !<
  function is_pro() result(ispro)
    ! -- local variables
    logical(LGP) :: ispro !< pro version logical
    !
    ! -- check if using petsc
    ispro = using_petsc()
    !
    ! return
    return
  end function is_pro

  !> @brief Determine if using petsc
  !!
  !! Function to get a logical indicating if petsc is
  !! being used.
  !!
  !! @return      petscavail                petsc used logical
  !<
  function using_petsc() result(petscused)
    ! -- local variables
    logical(LGP) :: petscused !< petsc used logical
    !
    ! -- initialize petscavail
    petscused = .FALSE.
    !
    ! -- set operating system variables
#ifdef __WITH_PETSC__
    petscused = .TRUE.
#endif
    !
    ! return
    return
  end function using_petsc

end module DefinedMacros

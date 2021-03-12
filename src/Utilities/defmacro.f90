module DefinedMacros
  ! -- modules
  use KindModule, only: I4B
  use ConstantsModule, only: OSUNDEF, OSLINUX, OSMAC, OSWIN
  implicit none
  private
  public :: get_os
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

end module DefinedMacros

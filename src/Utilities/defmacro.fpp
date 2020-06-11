      module DefinedMacros
        ! -- modules
        use KindModule, only: I4B
        use ConstantsModule, only: OSUNDEF, OSLINUX, OSMAC, OSWIN
        implicit none
        private
        public :: get_os
        contains

        function get_os() result(ios)
          integer(I4B) :: ios
          !
          ! -- initialize ios
          ios = OSUNDEF
          !
          ! -- set variables
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
        
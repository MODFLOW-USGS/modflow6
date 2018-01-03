      module CompilerVersion
        ! -- modules
        implicit none
        private
        ! -- compiler version
        character(len=10) :: ccompiler
        character(len=10) :: cversion
        character(len=20) :: cdate
        integer :: icompiler = 0
        integer :: iversion = 0
        public :: get_compiler
        contains
  
        subroutine get_compiler(txt)
          character(len=80), intent(inout) :: txt
        
        ! -- set variables
#ifdef __GFORTRAN__ 
          icompiler = 1
          cversion = __VERSION__
          cdate = __DATE__ // ' ' // __TIME__
#endif
#ifdef __INTEL_COMPILER
          icompiler = 2
          iversion = __INTEL_COMPILER
          cdate = __DATE__ // ' ' // __TIME__
#endif

          if (icompiler < 1) then
            ccompiler = 'UNKNOWN'
            cversion = '??.??'
            cdate = '??? ?? ???? ??:??:??'
          else if (icompiler == 1) then
            ccompiler = 'GFORTRAN'
          else if (icompiler == 2) then
            ccompiler = 'IFORT'
            write(cversion,'(i4)') iversion
            write(cversion,'(3a)') cversion(1:2), '.', cversion(3:4)
          end if
          
          write(txt,'(a,5(1x,a),a)') 
     2      'MODFLOW 6 compiled', trim(adjustl(cdate)), 
     3      'with', trim(adjustl(ccompiler)), 
     4      'compiler (ver.', trim(adjustl(cversion)), ')' 
  
          ! return
          return
        end subroutine get_compiler
      end module CompilerVersion
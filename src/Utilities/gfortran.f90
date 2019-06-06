      module CompilerVersion
        ! -- modules
        use KindModule, only: I4B
        implicit none
        private
        ! -- compiler version
        character(len=10) :: ccompiler
        character(len=10) :: cversion
        character(len=20) :: cdate
        integer(I4B) :: icompiler = 0
        integer(I4B) :: iversion = 0
        integer(I4B) :: imajor = 0
        integer(I4B) :: iminor = 0
        integer(I4B) :: imicro = 0
        public :: get_compiler, get_compile_date
        contains

        subroutine get_compiler(txt)
          character(len=80), intent(inout) :: txt

        ! -- set variables

          icompiler = 1
          cversion = 'dummy' ! __VERSION__
          cdate = 'dummy' ! __DATE__ // ' ' // __TIME__


          if (icompiler < 1) then
            ccompiler = 'UNKNOWN'
            cversion = '??.??'
            cdate = '??? ?? ???? ??:??:??'
          else if (icompiler == 1) then
            ccompiler = 'GFORTRAN'
          else if (icompiler == 2) then
            ccompiler = 'IFORT'
            write(cversion,'(i4)') iversion
            read(cversion(1:2), '(i2)') imajor
            read(cversion(3:4), '(i2)') iminor
            write(cversion,'(i0,2(".",i0))') imajor, iminor, imicro
          end if

          write(txt,'(a,5(1x,a),a)')
     2      'MODFLOW 6 compiled', trim(adjustl(cdate)),
     3      'with', trim(adjustl(ccompiler)),
     4      'compiler (ver.', trim(adjustl(cversion)), ')'

          ! return
          return
        end subroutine get_compiler

        subroutine get_compile_date(txt)
          character(len=20), intent(inout) :: txt
        ! -- set variables

          cdate = 'dummy' ! __DATE__ // ' ' // __TIME__

          write(txt,'(a)') trim(adjustl(cdate))
          !
          ! return
          return
        end subroutine get_compile_date

      end module CompilerVersion
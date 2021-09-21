module CompilerVersion
  ! -- modules
  use KindModule, only: I4B
  implicit none
  private
  ! -- compiler version
  character(len=10) :: ccompiler  !< compiler string
  character(len=10) :: cversion   !< compiler version string
  character(len=20) :: cdate      !< compilation date
  integer(I4B) :: icompiler = 0   !< compiler enum
  integer(I4B) :: iversion = 0    !< compiler version number
  integer(I4B) :: imajor = 0      !< compiler major version number
  integer(I4B) :: iminor = 0      !< compiler minor version number
  integer(I4B) :: imicro = 0      !< compiler micro version number
  public :: get_compiler, get_compile_date
contains

  !> @ brief Get compiler information
  !!
  !!  Subroutine returns a string with compilation date and compiler.
  !!
  !!  @param[in,out]  txt   string with compiler information
  !!
  !<
  subroutine get_compiler(txt)
    ! -- dummy variables
    character(len=80), intent(inout) :: txt !< compiler information
    !
    ! -- set variables
#ifdef __GFORTRAN__
    icompiler = 1
    cversion = __VERSION__
    cdate = __DATE__//' '//__TIME__
#endif
#ifdef __INTEL_COMPILER
    icompiler = 2
    iversion = __INTEL_COMPILER
    cdate = __DATE__//' '//__TIME__
    imicro = __INTEL_COMPILER_UPDATE
#endif
    !
    ! -- set compiler strings
    if (icompiler < 1) then
      ccompiler = 'UNKNOWN'
      cversion = '??.??'
      cdate = '??? ?? ???? ??:??:??'
    else if (icompiler == 1) then
      ccompiler = 'GFORTRAN'
    else if (icompiler == 2) then
      ccompiler = 'IFORT'
      write (cversion, '(i4)') iversion
      read (cversion(1:2), '(i2)') imajor
      read (cversion(3:4), '(i2)') iminor
      write (cversion, '(i0,2(".",i0))') imajor, iminor, imicro
    end if
    !
    ! -- write string with compiler information
    write (txt, '(a,5(1x,a),a)') &
      'MODFLOW 6 compiled', trim(adjustl(cdate)), &
      'with', trim(adjustl(ccompiler)), &
      'compiler (ver.', trim(adjustl(cversion)), ')'
    !
    ! -- return
    return
  end subroutine get_compiler

  !> @ brief Get compilation date
  !!
  !!  Subroutine returns a string with compilation date
  !!
  !!  @param[in,out]  txt   string with compilation date
  !!
  !<
  subroutine get_compile_date(txt)
    ! -- dummy variables
    character(len=20), intent(inout) :: txt  !< compilation date
    ! -- set variables
#ifdef __GFORTRAN__
    cdate = __DATE__//' '//__TIME__
#endif
#ifdef __INTEL_COMPILER
    cdate = __DATE__//' '//__TIME__
#endif
    !
    ! -- write compilation date string
    write (txt, '(a)') trim(adjustl(cdate))
    !
    ! -- return
    return
  end subroutine get_compile_date

end module CompilerVersion

module VersionModule
  use KindModule
  implicit none
  public
  ! -- modflow 6 version
  integer(I4B), parameter :: IDEVELOPMODE = 0
  character(len=40), parameter :: VERSION = '6.2.1 02/18/2021'
  character(len=10), parameter :: MFVNAM = ' 6'
  character(len=*), parameter  :: MFTITLE =                                     &
    'U.S. GEOLOGICAL SURVEY MODULAR HYDROLOGIC MODEL'
  character(len=*), parameter :: FMTTITLE =                                     &
    "(/,34X,'MODFLOW',A,/,                                                      &
    &16X,'U.S. GEOLOGICAL SURVEY MODULAR HYDROLOGIC MODEL',                     &
    &/,23X,'Version ',A/)"
  ! -- disclaimer must be appropriate for version (release or release candidate)
  character(len=*), parameter :: FMTDISCLAIMER =                                &
    "(/,                                                                        &
    &'This software has been approved for release by the U.S. Geological ',/,   &
    &'Survey (USGS). Although the software has been subjected to rigorous ',/,  &
    &'review, the USGS reserves the right to update the software as needed ',/, &
    &'pursuant to further analysis and review. No warranty, expressed or ',/,   &
    &'implied, is made by the USGS or the U.S. Government as to the ',/,        &
    &'functionality of the software and related material nor shall the ',/,     &
    &'fact of release constitute any such warranty. Furthermore, the ',/,       &
    &'software is released on condition that neither the USGS nor the U.S. ',/, &
    &'Government shall be held liable for any damages resulting from its ',/,   &
    &'authorized or unauthorized use. Also refer to the USGS Water ',/,         &
    &'Resources Software User Rights Notice for complete use, copyright, ',/,   &
    &'and distribution information.',/)"

  contains

  subroutine write_listfile_header(iout, cmodel_type, write_sys_command, &
                                   write_kind_info)
! ******************************************************************************
! write_listfile_header -- write a header to the simulation or model list file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! - module
    use ConstantsModule, only: LENBIGLINE, DZERO
    use GenericUtilitiesModule, only: write_centered
    use CompilerVersion, only: get_compiler
    ! -- dummy
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in), optional :: cmodel_type
    logical(LGP), intent(in), optional :: write_sys_command
    logical(LGP), intent(in), optional :: write_kind_info
    ! -- local
    character(len=LENBIGLINE) :: syscmd
    character(len=80) :: compiler
    integer(I4B) :: iheader_width = 80
    logical(LGP) :: wki
    logical(LGP) :: wsc
! ------------------------------------------------------------------------------
    !
    ! -- Write title to list file
    call write_centered('MODFLOW'//MFVNAM, iheader_width, iunit=iout)
    call write_centered(MFTITLE, iheader_width, iunit=iout)
    !
    ! -- Write model type to list file
    if (present(cmodel_type)) then
      call write_centered(cmodel_type, iheader_width, iunit=iout)
    end if
    !
    ! -- Write version
    call write_centered('VERSION '//VERSION, iheader_width, iunit=iout)
    !
    ! -- Write if develop mode
    if (IDEVELOPMODE == 1) then
      call write_centered('***DEVELOP MODE***', iheader_width, iunit=iout)
    end if
    !
    ! -- Write compiler version
    call get_compiler(compiler)
    call write_centered(' ', iheader_width, iunit=iout)
    call write_centered(trim(adjustl(compiler)), iheader_width, iunit=iout)
    !
    ! -- Write disclaimer
    write(iout, FMTDISCLAIMER)
    !
    ! -- Write the system command used to initiate simulation
    wsc = .true.
    if (present(write_sys_command)) wsc = write_sys_command
    if (wsc) then
      call GET_COMMAND(syscmd)
      write(iout, '(/,a,/,a)') 'System command used to initiate simulation:',  &
                               trim(syscmd)
    end if
    !
    ! -- Write precision of real variables
    wki = .true.
    if (present(write_kind_info)) wki = write_kind_info
    if (wki) then
      write(iout, '(/,a)') 'MODFLOW was compiled using uniform precision.'
      call write_kindinfo(iout)
    end if
    write(iout, *)
    !
    ! -- return
    return
  end subroutine write_listfile_header

end module VersionModule


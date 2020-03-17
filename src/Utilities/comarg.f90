module CommandArguments
  use KindModule
  use ConstantsModule, only: LINELENGTH, LENHUGELINE,                            &
                             VSUMMARY, VALL, VDEBUG
  use VersionModule,          only: VERSION, MFVNAM, IDEVELOPMODE
  use CompilerVersion
  use SimVariablesModule,     only: istdout, isim_level,                         &
                                    simfile, simlstfile, simstdout
  use GenericUtilitiesModule, only: sim_message
  use SimModule, only: store_error, ustop, store_error_unit,                     &
                       store_error_filename
  use InputOutputModule, only: upcase, getunit
  !
  implicit none
  !
  private
  public :: GetCommandLineArguments
  !
  contains
  
  subroutine GetCommandLineArguments()
! ******************************************************************************
! Write information on command line arguments
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    ! -- local
    character(len=LENHUGELINE) :: line
    character(len=LENHUGELINE) :: ucline
    character(len=LINELENGTH) :: clevel
    character(len=LINELENGTH) :: header
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: cexe
    character(len=80) :: compiler
    character(len=20) :: cdate
    character(len=17) :: ctyp
    logical :: ltyp
    logical :: lexist
    logical :: lstop
    integer(I4B) :: iscrn
    integer(I4B) :: icountcmd
    integer(I4B) :: ipos
    integer(I4B) :: ilen
    integer(I4B) :: iarg
! ------------------------------------------------------------------------------
    !
    ! -- initialize local variables
    lstop = .FALSE.
    iscrn = istdout
    !
    ! -- set mf6 executable name
    icountcmd = command_argument_count()
    call get_command_argument(0, cexe)
    cexe = adjustl(cexe)
    !
    ! -- find the program basename, not including the path (this should be 
    !    mf6.exe, mf6d.exe, etc.)
    ipos = index(cexe, '/', back=.TRUE.)
    if (ipos == 0) then
      ipos = index(cexe, '\', back=.TRUE.)
    end if
    if (ipos /= 0) then
      ipos = ipos + 1
    end if
    cexe = cexe(ipos:)
    !
    ! -- write header
    call get_compile_date(cdate)
    write(header, '(a,4(1x,a),a)') &
      trim(adjustl(cexe)), '- MODFLOW',                                          &
      trim(adjustl(VERSION)), '(compiled', trim(adjustl(cdate)), ')'
    !
    ! -- set ctyp
    if (IDEVELOPMODE == 1) then
      ctyp = 'Release Candidate'
      ltyp = .TRUE.
    else
      ctyp = 'Release'
      ltyp = .FALSE.
    end if
    !
    ! -- Read remaining arguments
    iarg = 0
    do
      !
      ! -- terminate loop if lstop is true
      if (lstop) then
        exit
      end if
      !
      ! -- increment iarg and determine if loop should be terminated
      iarg = iarg + 1
      if (iarg > icountcmd) then
        exit
      end if
      !
      ! -- get command line argument
      call get_command_argument(iarg, line)
      ucline = line
      call upcase(ucline)
      !
      ! -- skip commands without - or --
      ipos = index(ucline, '-')
      if (ipos < 1) then
        cycle
      end if
      !
      ! -- parse level string, if necessary
      clevel = ' '
      ipos = index(ucline, '--LEVEL=')
      if (ipos > 0) then
        ipos = index(line, '=')
        ilen = len_trim(line)
        clevel = line(ipos+1:ilen)
        call upcase(clevel)
        ucline = line(1:ipos-1)
        call upcase(ucline)
      end if
      !
      ! -- evaluate the command line argument in line
      select case(trim(adjustl(ucline)))
        case('-H', '-?', '--HELP')
          lstop = .TRUE.
          call write_usage(trim(adjustl(header)), trim(adjustl(cexe)), iscrn)
        case('-V', '--VERSION')
          lstop = .TRUE.
          write(istdout,'(2a,2(1x,a))')                                          &
            trim(adjustl(cexe)), ':', trim(adjustl(VERSION)), ctyp
        case('-DEV', '--DEVELOP')
          lstop = .TRUE.
          write(istdout,'(2a,g0)')                                               &
            trim(adjustl(cexe)), ': develop version ', ltyp
        case('-C', '--COMPILER') 
          lstop = .TRUE.
          call get_compiler(compiler)
          write(istdout,'(2a,1x,a)')                                             &
            trim(adjustl(cexe)), ':', trim(adjustl(compiler))
        case('-S', '--SILENT') 
          istdout = getunit()
          open(unit=istdout, file=trim(adjustl(simstdout)))
        case('-L', '--LEVEL')
          if (len_trim(clevel) < 1) then
            iarg = iarg + 1
            call get_command_argument(iarg, clevel)
            call upcase(clevel)
          end if
          select case(trim(adjustl(clevel)))
            case('SUMMARY')
              isim_level = VSUMMARY
            case('DEBUG')
              isim_level = VDEBUG
            case default
              call write_usage(trim(adjustl(header)), trim(adjustl(cexe)), iscrn)
              write(errmsg, '(2a,1x,a)') &
                trim(adjustl(cexe)), ': illegal STDOUT level option -',          &
                trim(adjustl(clevel))
              call store_error(errmsg)
          end select
        case default
          lstop = .TRUE.
          call write_usage(trim(adjustl(header)), trim(adjustl(cexe)), iscrn)
          write(errmsg, '(2a,1x,a)') &
            trim(adjustl(cexe)), ': illegal option -', trim(adjustl(ucline))
          call store_error(errmsg)
      end select
    end do
    !
    ! -- check if simfile exists
    inquire(file=trim(adjustl(simfile)), exist=lexist)
    if (.NOT. lexist) then
      lstop = .TRUE.
      write(errmsg, '(2a,2(1x,a))')                                              &
          trim(adjustl(cexe)), ':', trim(adjustl(simfile)),                      &
          'is not present in working directory.'
      call store_error(errmsg)
    end if
    !
    ! -- terminate program if lstop
    if (lstop) then
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine GetCommandLineArguments
  
  subroutine write_usage(header, cexe, iu)
    ! -- dummy
    character(len=*), intent(in) :: header
    character(len=*), intent(in) :: cexe
    integer(I4B), intent(in) :: iu
    ! -- local
    character(len=LINELENGTH) :: line
    ! -- format
    character(len=*), parameter :: OPTIONSFMT =                                  &
      "(/,                                                                       &
      &'Options   GNU long option   Meaning ',/,                                 &
      &' -h, -?    --help           Show this message',/,                        &
      &' -v        --version        Display program version information.',/,     &
      &' -dev      --develop        Display program develop option mode.',/,     &
      &' -c        --compiler       Display compiler information.',/,            &
      &' -s        --silent         All STDOUT to mfsim.stdout.',/,              &
      &' -l <str>  --level <str>    STDOUT output to screen based on <str>.',/,  &
      &'                            <str>=summary Limited output to STDOUT.',/,  &
      &'                            <str>=debug   Enhanced output to STDOUT.',/, &
      &'                                                                    ',/, &
      &'Bug reporting and contributions are welcome from the community. ',/,     &
      &'Questions can be asked on the issues page[1]. Before creating a new',/,  &
      &'issue, please take a moment to search and make sure a similar issue',/,  &
      &'does not already exist. If one does exist, you can comment (most',/,     &
      &'simply even with just :+1:) to show your support for that issue.',/,     &
      &'                                                                    ',/, &
      &'[1] https://github.com/MODFLOW-USGS/modflow6/issues',/)"
! ------------------------------------------------------------------------------
    !
    ! -- write command line usage information to the screen
    call sim_message(header, iunit=iu)
    write(line, '(a,1x,a,15x,a,2(1x,a),2a)')                                     &
      'usage:', cexe, 'run MODFLOW', trim(adjustl(MFVNAM)),                      &
      'using "', trim(adjustl(simfile)), '"'
    call sim_message(line, iunit=iu)
    write(line, '(a,1x,a,1x,a,5x,a)')                                            &
      '   or:', cexe, '[options]',                                               &
      'retrieve program information'
    call sim_message(line, iunit=iu)
    call sim_message('', iunit=iu, fmt=OPTIONSFMT)
    !
    ! -- return
    return
  end subroutine write_usage
  
end module CommandArguments
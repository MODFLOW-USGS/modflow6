module CommandArguments
  use KindModule
  use ConstantsModule, only: ISTDOUT, LINELENGTH, LENHUGELINE
  use VersionModule,          only: VERSION, MFVNAM, IDEVELOPMODE
  use CompilerVersion
  use SimVariablesModule,     only: simfile
  use SimModule, only: store_error, ustop, store_error_unit,                   &
                       store_error_filename
  use InputOutputModule, only: urword
  !
  implicit none
  !
  private
  public :: GetCommandLineArguments
  !
  contains
  
  subroutine GetCommandLineArguments()
    ! -- dummy
    ! -- local
    character(len=LENHUGELINE) :: line
    character(len=LINELENGTH) :: header
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: cexe
    character(len=80) :: compiler
    character(len=20) :: cdate
    character(len=17) :: ctyp
    logical :: ltyp
    logical :: lexist
    integer(I4B) :: inunit = 0
    integer(I4B) :: ilen
    integer(I4B) :: istat
    integer(I4B) :: lloc
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: ival
    integer(I4B) :: i
    integer(I4B) :: ipos
    integer(I4B) :: iarg
    integer(I4B) :: iterm
    real(DP) :: rval
! ------------------------------------------------------------------------------
    !
    ! -- Get the command line string
    call GET_COMMAND(line, ilen, istat)
    !
    ! -- This will read mf6 executable
    lloc = 1
    call urword(line, lloc, istart, istop, 0, ival, rval, 0, inunit)
    !
    ! -- remove quotes around the mf6 executable
    do i = istart, istop
      if (line(i:i) == '"' .or. line(i:i) == "'") then
        line(i:i) = ' '
      end if
    end do
    !
    ! -- find name of executable without path
    ipos = index(line(istart:istop), '/', back=.TRUE.)
    if (ipos == 0) then
      ipos = index(line(istart:istop), '\', back=.TRUE.)
    end if
    if (ipos /= 0) then
      istart = ipos + 1
    end if
    !
    ! -- set mf6 executable name
    cexe = adjustl(line(istart:istop))
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
    iterm = 0
    do
      call urword(line, lloc, istart, istop, 1, ival, rval, 0, inunit)
      if (line(istart:istop) == ' ') exit
      iarg = iarg + 1
      iterm = 1
      select case(line(istart:istop))
        case('-H', '-?', '--HELP')
          call write_usage(trim(adjustl(header)), trim(adjustl(cexe)))
        case('-V', '--VERSION')
          write(ISTDOUT,'(2a,2(1x,a))') &
            trim(adjustl(cexe)), ':', trim(adjustl(VERSION)), ctyp
        case('-DEV', '--DEVELOP')
          write(ISTDOUT,'(2a,l)') &
            trim(adjustl(cexe)), ': develop version', ltyp
        case('-C', '--COMPILER') 
          call get_compiler(compiler)
          write(ISTDOUT,'(2a,1x,a)') &
            trim(adjustl(cexe)), ':', trim(adjustl(compiler))
        case default 
          call write_usage(trim(adjustl(header)), trim(adjustl(cexe)))
          write(errmsg, '(2a,1x,a)') &
            trim(adjustl(cexe)), ': illegal option -', line(istart:istop)
          call store_error(errmsg)
      end select
    end do
    !
    ! -- no command line arguments - check if mfsim.nam exists
    if  (iarg == 0) then
      inquire(file=simfile, exist=lexist)
      if (.NOT. lexist) then
        iterm = 1
        write(errmsg, '(2a,2(1x,a))') &
           trim(adjustl(cexe)), ':', simfile, 'is not present in working directory.'
        call store_error(errmsg)
      end if
    end if
    !
    ! -- command line arguments present
    if (iterm > 0) then
      call USTOP()
    end if
    !
    ! -- return
    return
  end subroutine GetCommandLineArguments
  
  subroutine write_usage(header, cexe)
    ! -- dummy
    character(len=*), intent(in) :: header
    character(len=*), intent(in) :: cexe
    ! -- local
    character(len=*), parameter :: OPTIONSFMT =                                    &
      "(/,                                                                      &
      &'Options   GNU long option   Meaning ',/,                                &
      &' -h, -?   --help            Show this message',/,                       &
      &' -v       --version         Display program version information.',/,    &
      &' -dev     --develop         Display program develop option mode.',/,    &
      &' -c       --compiler        Display compiler information.',/,           &
      &'                                                                    ',/,&
      &'Bug reporting and contributions are welcome from the community. ',/,    &
      &'Questions can be asked on the issues page[1]. Before creating a new',/, &
      &'issue, please take a moment to search and make sure a similar issue',/, &
      &'does not already exist. If one does exist, you can comment (most',/,    &
      &'simply even with just :+1:) to show your support for that issue.',/,    &
      &'                                                                    ',/,&
      &'[1] https://github.com/MODFLOW-USGS/modflow6/issues',/)"
! ------------------------------------------------------------------------------
    write(ISTDOUT,'(a,/,a,1x,a,15x,a,2(1x,a),2a,/,a,1x,a,1x,a,5x,a)') &
      trim(adjustl(header)),                                              &
      'usage:', cexe, 'run MODFLOW', trim(adjustl(MFVNAM)),               &
      'using "', trim(adjustl(simfile)), '"',                             &
      '   or:', cexe, '[options]',                                        &
      'retrieve program information'
    write(ISTDOUT, OPTIONSFMT)
    !
    ! -- return
    return
  end subroutine write_usage
  
end module CommandArguments
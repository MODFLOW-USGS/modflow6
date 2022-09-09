program idmloader
  use KindModule
  use ConstantsModule, only: LINELENGTH, LENHUGELINE, MAXCHARLEN
  use VersionModule, only: VERSION
  use SimVariablesModule, only: iout, errmsg
  use SimModule, only: store_error
  use GenericUtilitiesModule, only: sim_message, write_centered
  use InputOutputModule,  only: openfile
  use IdmModule, only: IdmType, &
                       Idm
  
  implicit none

  type(IdmType) :: pIdm
  character(len=10), parameter :: mfvnam=' Version 6'
  character(len=MAXCHARLEN), parameter :: namfile='mfsim.nam'
  character(len=LINELENGTH) :: line
  character(len=LENHUGELINE) :: flst
  integer(I4B) :: iunit_lst = 20
  
  ! -- Write title to screen
  call write_centered('IDMLOADER'//mfvnam, 80)
  call write_centered('U.S. GEOLOGICAL SURVEY', 80)
  call write_centered('VERSION '//VERSION, 80)
  !  
  ! -- Open list file and write title
  iout = iunit_lst
  flst = 'out.lst'
  call openfile(iunit_lst, 0, flst, 'LIST', filstat_opt='REPLACE')
  call write_centered('IDMLOADER'//mfvnam, 80, iunit=iout)
  call write_centered('U.S. GEOLOGICAL SURVEY', 80, iunit=iout)
  call write_centered('VERSION '//VERSION, 80, iunit=iout)
  !
  ! -- build memory manager input paths
  pIdm = Idm(iunit_lst)
  call pIdm%idm_load(namfile)
  !
  ! -- load data into memory
  call finalize_data()
  !
  ! -- close output files
  write(iunit_lst, '(/, a)') 'Normal Termination'
  close(iunit_lst)
  write(line,'(a)') 'Normal Termination'
  call sim_message(line, skipbefore=1)
  !
  ! -- end of program
  end program idmloader
  
  subroutine finalize_data()
    use SimVariablesModule, only: iout, errmsg
    use MemoryManagerModule,  only: mem_set_print_option, mem_write_usage, mem_da
    implicit none
    call mem_set_print_option(iout, 'ALL', errmsg)
    call mem_write_usage(iout)
    call mem_da()
  end subroutine finalize_data

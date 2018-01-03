program zonbudmf6
  use SimModule, only: ustop
  use ConstantsModule, only: LENHUGELINE, VERSION
  use SimVariablesModule, only: iout
  use InputOutputModule,  only: openfile, write_centered
  
  implicit none
  
  character(len=10), parameter :: mfvnam=' Version 6'
  character(len=LENHUGELINE) :: fnam, flst, fcsv
  integer :: iunit_lst = 20
  integer :: iunit_csv = 21
  integer :: iunit_nam = 22
  integer :: iunit_bud = 23
  integer :: iunit_zon = 24
  integer :: iunit_grb = 25
  logical :: exists
  
  ! -- Write title to screen
  call write_centered('ZONEBUDGET'//mfvnam, 6, 80)
  call write_centered('U.S. GEOLOGICAL SURVEY', 6, 80)
  call write_centered('VERSION '//VERSION, 6, 80)
  !
  ! -- Find name of zone budget name file and lst file
  fnam = 'zbud.nam'
  call parse_command_line(fnam, flst, fcsv)
  inquire(file=fnam, exist=exists)
  if (.not. exists) then
    write(6, *)
    write(6, '(a)') 'ERROR.  Name file not found.'
    write(6, '(a)') 'Looking for: ' // trim(fnam)
    call ustop()
  endif
  !  
  ! -- Open list file and write title
  iout = iunit_lst
  call openfile(iunit_lst, 0, flst, 'LIST', filstat_opt='REPLACE')
  call write_centered('ZONEBUDGET'//mfvnam, iout, 80)
  call write_centered('U.S. GEOLOGICAL SURVEY', iout, 80)
  call write_centered('VERSION '//VERSION, iout, 80)
  !
  ! -- Open name file, read name file, and open csv file
  call openfile(iunit_nam, iout, fnam, 'NAM')
  call read_namefile(iunit_nam, iunit_bud, iunit_zon, iunit_grb)
  call openfile(iunit_csv, iout, fcsv, 'CSV', filstat_opt='REPLACE')
  !
  ! -- Process the budget file and write output
  call process_budget(iunit_csv, iunit_bud, iunit_zon, iunit_grb)
  !
  ! -- close output files
  write(iunit_lst, '(/, a)') 'Normal Termination'
  close(iunit_lst)
  close(iunit_csv)
  write(6, '(a)') 'Normal Termination'
  !
end program zonbudmf6

subroutine read_namefile(iunit_nam, iunit_bud, iunit_zon, iunit_grb)
! ******************************************************************************
! read_namefile
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  use SimVariablesModule, only: iout
  use SimModule, only: store_error, ustop
  use ConstantsModule, only: LENHUGELINE, LINELENGTH
  use InputOutputModule,  only: openfile
  use OpenSpecModule, only: form, access
  use BlockParserModule, only: BlockParserType
  implicit none
  ! -- dummy
  integer, intent(in) :: iunit_nam
  integer, intent(in) :: iunit_bud
  integer, intent(in) :: iunit_zon
  integer, intent(inout) :: iunit_grb
  ! -- local
  type(BlockParserType) :: parser
  integer :: ierr, iu
  logical :: isfound, endOfBlock
  character(len=LINELENGTH) :: keyword, errmsg
  character(len=LENHUGELINE) :: filename
  character(len=20) :: fm, acc
! ------------------------------------------------------------------------------
  !
  call parser%Initialize(iunit_nam, iout)
  call parser%GetBlock('ZONEBUDGET', isfound, ierr)
  if(isfound) then
    do
      call parser%GetNextLine(endOfBlock)
      if (endOfBlock) exit
      call parser%GetStringCaps(keyword)
      fm = 'FORMATTED'
      acc = 'SEQUENTIAL'
      select case (keyword)
        case ('BUD')
          iu = iunit_bud
          fm = form
          acc = access
          call parser%GetString(filename)
        case ('ZON')
          iu = iunit_zon
          call parser%GetString(filename)
        case ('GRB')
          iu = iunit_grb
          fm = form
          acc = access
          call parser%GetString(filename)
        case default
          write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN ZONEBUDGET ENTRY: ',            &
                                    trim(keyword)
          call store_error(errmsg)
          call parser%StoreErrorUnit()
          call ustop()
        end select
        call openfile(iu, iout, trim(filename), trim(keyword), fm, acc)
    end do
  else
    write(errmsg,'(1x,a)')'ERROR.  REQUIRED ZONEBUDGET BLOCK NOT FOUND.'
    call store_error(errmsg)
    call parser%StoreErrorUnit()
    call ustop()
  end if
  !
  ! -- close name file
  close(iunit_nam)
  !
  ! -- return
  return
end subroutine read_namefile
  
subroutine process_budget(iunit_csv, iunit_bud, iunit_zon, iunit_grb)
! ******************************************************************************
! process_budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  use ConstantsModule, only: LINELENGTH
  use SimVariablesModule, only: iout
  use SimModule, only: store_error, ustop
  use BudgetDataModule, only: budgetdata_init, budgetdata_read,                &
                              budgetdata_finalize,                             &
                              ia, ja, budtxt, nbudterms,                       &
                              nodesrc, nodedst, flowdata, flowja, kper, kstp,  &
                              delt, totim, dstpackagename, hasimeth1flowja,    &
                              srcmodelname, dstmodelname
  use ZoneModule,       only: zone_init, clear_accumulators,                   &
                              flowja_accumulate, flowiaja_accumulate,          &
                              flow_accumulate,                                 &
                              flowch_setich, flowch_accumulate,                &
                              zone_finalize, nmznfl, vbvl, vbznfl, maxzone
  use ZoneOutputModule, only: zoneoutput_init, zoneoutput_write,               &
                              zoneoutput_finalize
  use GrbModule,        only: read_grb
  implicit none
  ! -- dummy
  integer, intent(in) :: iunit_csv
  integer, intent(in) :: iunit_bud
  integer, intent(in) :: iunit_zon
  integer, intent(in) :: iunit_grb
  ! -- local
  character(len=16), dimension(:), allocatable :: budtxtarray
  character(len=16), dimension(:), allocatable :: packagenamearray
  integer, dimension(:), allocatable :: internalflow
  integer, allocatable, dimension(:) :: mshape
  integer :: ibudterm
  integer :: itime = 1
  integer :: ncrgrb
  integer :: ncrbud = 0
  integer :: ncr
  logical :: opengrb
  logical :: success
  logical :: hasiaja = .false.
  logical :: foundchd = .false.
  character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
  !
  ! -- Initialize budget data
  call budgetdata_init(iunit_bud, iout, ncrbud)
  !
  ! -- Check to see if GRB is required, and read it if necessary
  ncrgrb = 0
  if (hasimeth1flowja) then
    inquire(unit=iunit_grb, opened=opengrb)
    if (opengrb) then
      hasiaja = .true.
      call read_grb(iunit_grb, ia, ja, mshape)
      ncrgrb = size(ia) - 1
    else
      errmsg = 'BUDGET FILE HAS "FLOW-JA-FACE" RECORD BUT NO GRB FILE SPECIFIED.'
      call store_error(errmsg)
      errmsg = 'ADD GRB ENTRY TO ZONE BUDGET NAME FILE.'
      call store_error(errmsg)
      call ustop()
    endif
  else
    inquire(unit=iunit_grb, opened=opengrb)
    if (opengrb) then
      errmsg = 'BINARY GRID FILE IS PRESENT, BUT BUDGET FILE DOES NOT HAVE &
        &"FLOW-JA-FACE" RECORD IN THE IMETH=1 FORMAT. CHECK TO MAKE SURE &
        &FLOWS ARE SAVED TO THE BUDGET FILE'
      call store_error(errmsg)
      call ustop()
    endif
    !
    ! -- At this point, must be a budget file from an advanced package without
    !    the IMETH=1 flow-ja-face record.
    allocate(mshape(1))
    mshape(1) = ncrgrb
  endif
  !
  ! -- Read the zone file to get number of cells/reaches
  ncr = ncrgrb
  call zone_init(iunit_zon, nbudterms, ncr, mshape)
  !
  ! -- Initialize zone and zoneoutput modules
  !call zone_init(iunit_zon, nbudterms, ncr)
  call zoneoutput_init(iout, iunit_csv, maxzone, nbudterms)
  allocate(budtxtarray(nbudterms))
  allocate(packagenamearray(nbudterms))
  allocate(internalflow(nbudterms))
  !
  ! -- time loop
  timeloop: do
    !
    ! -- Clear budget accumulators and loop through budget terms
    call clear_accumulators()
    write(iout, '(/, a)') 'Reading records from budget file'
    do ibudterm = 1, nbudterms
      !
      ! -- read data
      call budgetdata_read(success, iout)
      if (.not. success) then
        write(iout, '(a)') 'Done reading records.  Exiting time loop.'
        exit timeloop
      endif
      !
      ! -- write message and check
      write(6, '(a)', advance='no') '.'
      if (itime == 1) then
        budtxtarray(ibudterm) = budtxt
        packagenamearray(ibudterm) = dstpackagename
        if (trim(adjustl(budtxt)) == 'FLOW-JA-FACE' .and. &
            srcmodelname == dstmodelname) then
          internalflow(ibudterm) = 1
        else
          internalflow(ibudterm) = 0
        endif
      else
        if (budtxt /= budtxtarray(ibudterm) .or.                               &
            dstpackagename /= packagenamearray(ibudterm)) then
          errmsg = 'Expecting ' // trim(packagenamearray(itime)) // '-' //  &
            trim(budtxtarray(itime)) // ' but found ' // trim(dstpackagename)  &
            // '-' // trim(budtxt)
          call store_error(errmsg)
          call ustop()
        endif
      endif
      !
      ! -- Accumulate flow terms (or set ich for constant heads)
      if (internalflow(ibudterm) == 1) then
        if (hasiaja) then
          call flowiaja_accumulate(ia, ja, flowja)
        else
          call flowja_accumulate(nodesrc, nodedst, flowdata)
        endif
      else
        if(trim(adjustl(budtxt)) == 'CONSTANT HEAD') then
          call flowch_setich(ibudterm, nodesrc)
          foundchd = .true.
        else
          call flow_accumulate(ibudterm, nodesrc, flowdata)
        endif
      endif
      !
    enddo
    write(iout, '(a)') 'Done reading records from budget file'
    !
    ! -- Now that all constant heads read, can process budgets for them
    if(hasiaja .and. foundchd) then
      call flowch_accumulate(ia, ja, flowja)
    endif
    !
    ! -- Write information for this time
    call zoneoutput_write(itime, kstp, kper, delt, totim, nbudterms, nmznfl,   &
                          vbvl, vbznfl, packagenamearray, budtxtarray,         &
                          internalflow)
    itime = itime + 1

  enddo timeloop
  !
  ! -- Finalize
  write(6, '(a)') '.'
  call budgetdata_finalize()
  call zoneoutput_finalize()
  call zone_finalize()
  !
  ! -- return
  return
end subroutine process_budget
      
  subroutine parse_command_line(fnam, flst, fcsv)
! ******************************************************************************
! Parse command line arguments
!   Assign zone budget name file as first argument.
!   Assign file names for list and csv files based on root name of name file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: urword
    use ConstantsModule, only: LENHUGELINE
    implicit none
    ! -- dummy
    character(len=*), intent(inout) :: fnam
    character(len=*), intent(inout) :: flst
    character(len=*), intent(inout) :: fcsv
    ! -- local
    character(len=LENHUGELINE) :: line
    integer :: inunit = 0
    integer :: ilen
    integer :: istat
    integer :: lloc
    integer :: istart
    integer :: istop
    integer :: ival
    integer :: i
    double precision :: rval
! ------------------------------------------------------------------------------
    !
    ! -- Get the command line string
    call GET_COMMAND(line, ilen, istat)
    !
    ! -- This will read zonebudget executable
    lloc = 1
    call urword(line, lloc, istart, istop, 0, ival, rval, 0, inunit)
    !
    ! -- This will read first argument (zone budget name file)
    call urword(line, lloc, istart, istop, 0, ival, rval, 0, inunit)
    if (istart < len(line)) fnam = line(istart:istop)
    !
    ! -- Set lst and csv file names by replacing fnam suffix with .lst
    istart = 0
    istop = len_trim(fnam)
    do i = istop, 1, -1
      if (fnam(i:i) == '.') then
        istart = i
        exit
      endif
    enddo
    if (istart == 0) istart = istop + 1
    !
    ! -- Create flst name
    flst = fnam(1:istart)
    istop = istart + 3
    flst(istart:istop) = '.lst'
    !
    ! -- Create fcsv name
    fcsv = fnam(1:istart)
    istop = istart + 3
    fcsv(istart:istop) = '.csv'
    !
    ! -- Return
    return
  end subroutine parse_command_line


!> @brief Model output control.
module OutputControlModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENMODELNAME, LENMEMPATH
  use SimVariablesModule, only: errmsg
  use OutputControlDataModule, only: OutputControlDataType, ocd_cr
  use BlockParserModule, only: BlockParserType
  use InputOutputModule, only: GetUnit, openfile

  implicit none
  private
  public OutputControlType, oc_cr

  !> @ brief Controls model output. Overridden for each model type.
  type OutputControlType
    character(len=LENMEMPATH) :: memoryPath !< path to data stored in the memory manager
    character(len=LENMODELNAME), pointer :: name_model => null() !< name of the model
    integer(I4B), pointer :: inunit => null() !< unit number for input file
    integer(I4B), pointer :: iout => null() !< unit number for output file
    integer(I4B), pointer :: ibudcsv => null() !< unit number for budget csv output file
    integer(I4B), pointer :: iperoc => null() !< stress period number for next output control
    integer(I4B), pointer :: iocrep => null() !< output control repeat flag (period 0 step 0)
    type(OutputControlDataType), pointer, contiguous :: ocds(:) => null() !< output control objects
    type(BlockParserType) :: parser
  contains
    procedure :: oc_df
    procedure :: oc_rp
    procedure :: oc_ot
    procedure :: oc_da
    procedure :: allocate_scalars => allocate
    procedure :: read_options
    procedure :: oc_save
    procedure :: oc_print
    procedure :: oc_save_unit
    procedure :: set_print_flag
  end type OutputControlType

contains

  !> @brief Create a new output control object.
  subroutine oc_cr(oc, name_model, inunit, iout)
    type(OutputControlType), pointer :: oc !< OutputControlType object
    character(len=*), intent(in) :: name_model !< name of the model
    integer(I4B), intent(in) :: inunit !< unit number for input
    integer(I4B), intent(in) :: iout !< unit number for output

    allocate (oc)
    call oc%allocate_scalars(name_model)
    oc%inunit = inunit
    oc%iout = iout
    call oc%parser%Initialize(inunit, iout)
  end subroutine oc_cr

  !> @ brief Define the output control type. Placeholder routine.
  subroutine oc_df(this)
    class(OutputControlType) :: this !< this instance
  end subroutine oc_df

  !> @ brief Read period block options and prepare the output control type.
  subroutine oc_rp(this)
    ! modules
    use TdisModule, only: kper, nper
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, store_error_unit, count_errors
    ! dummy
    class(OutputControlType) :: this !< this instance
    ! local
    integer(I4B) :: ierr, ival, ipos
    logical :: isfound, found, endOfBlock
    character(len=:), allocatable :: line
    character(len=LINELENGTH) :: ermsg, keyword1, keyword2
    character(len=LINELENGTH) :: printsave
    class(OutputControlDataType), pointer :: ocdobjptr
    ! formats
    character(len=*), parameter :: fmtboc = &
      &"(1X,/1X,'BEGIN READING OUTPUT CONTROL FOR STRESS PERIOD ',I0)"
    character(len=*), parameter :: fmteoc = &
      &"(/,1X,'END READING OUTPUT CONTROL FOR STRESS PERIOD ',I0)"
    character(len=*), parameter :: fmterr = &
      &"(' ERROR READING OUTPUT CONTROL PERIOD BLOCK: ')"
    character(len=*), parameter :: fmtroc = &
      "(1X,/1X,'OUTPUT CONTROL FOR STRESS PERIOD ',I0, &
      &' IS REPEATED USING SETTINGS FROM A PREVIOUS STRESS PERIOD.')"
    character(len=*), parameter :: fmtpererr = &
      &"(1x,'CURRENT STRESS PERIOD GREATER THAN PERIOD IN OUTPUT CONTROL.')"
    character(len=*), parameter :: fmtpererr2 = &
      &"(1x,'CURRENT STRESS PERIOD: ',I0,' SPECIFIED STRESS PERIOD: ',I0)"

    ! Read next block header if kper greater than last one read
    if (this%iperoc < kper) then

      ! Get period block
      call this%parser%GetBlock('PERIOD', isfound, ierr, &
                                supportOpenClose=.true., &
                                blockRequired=.false.)

      ! If end of file, set iperoc past kper, else parse line
      if (ierr < 0) then
        this%iperoc = nper + 1
        write (this%iout, '(/,1x,a)') 'END OF FILE DETECTED IN OUTPUT CONTROL.'
        write (this%iout, '(1x,a)') 'CURRENT OUTPUT CONTROL SETTINGS WILL BE '
        write (this%iout, '(1x,a)') 'REPEATED UNTIL THE END OF THE SIMULATION.'
      else

        ! Read period number
        ival = this%parser%GetInteger()

        ! Check to see if this is a valid kper
        if (ival <= 0 .or. ival > nper) then
          write (ermsg, '(a,i0)') 'PERIOD NOT VALID IN OUTPUT CONTROL: ', ival
          call store_error(ermsg)
          write (ermsg, '(a, a)') 'LINE: ', trim(adjustl(line))
          call store_error(ermsg)
        end if

        ! Check to see if specified is less than kper
        if (ival < kper) then
          write (ermsg, fmtpererr)
          call store_error(ermsg)
          write (ermsg, fmtpererr2) kper, ival
          call store_error(ermsg)
          write (ermsg, '(a, a)') 'LINE: ', trim(adjustl(line))
          call store_error(ermsg)
        end if

        ! Stop or set iperoc and continue
        if (count_errors() > 0) then
          call this%parser%StoreErrorUnit()
        end if
        this%iperoc = ival
      end if
    end if

    ! Read the stress period block
    if (this%iperoc == kper) then

      ! Clear io flags
      do ipos = 1, size(this%ocds)
        ocdobjptr => this%ocds(ipos)
        call ocdobjptr%psm%init()
      end do

      ! Output control time step matches simulation time step.
      write (this%iout, fmtboc) this%iperoc

      ! loop to read records
      recordloop: do

        ! Read the line
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword1)

        ! Set printsave string and then read the record type (e.g.
        ! BUDGET, HEAD)
        printsave = keyword1
        call this%parser%GetStringCaps(keyword2)

        ! Look through the output control data objects that are
        ! available and set ocdobjptr to the correct one based on
        ! cname.  Set found to .false. if not a valid record type.
        found = .false.
        do ipos = 1, size(this%ocds)
          ocdobjptr => this%ocds(ipos)
          if (keyword2 == trim(ocdobjptr%cname)) then
            found = .true.
            exit
          end if
        end do
        if (.not. found) then
          call this%parser%GetCurrentLine(line)
          write (ermsg, fmterr)
          call store_error(ermsg)
          call store_error('UNRECOGNIZED KEYWORD: '//keyword2)
          call store_error(trim(line))
          call this%parser%StoreErrorUnit()
        end if
        call this%parser%GetRemainingLine(line)
        call ocdobjptr%psm%read(trim(printsave)//' '//line, &
                                this%iout)
        call ocdobjptr%ocd_rp_check(this%parser%iuactive)
      end do recordloop
      write (this%iout, fmteoc) this%iperoc
    else

      ! Write message that output control settings are from a previous
      ! stress period.
      write (this%iout, fmtroc) kper
    end if
  end subroutine oc_rp

  !> @ brief Write output.
  !!
  !! Go through each output control data type and print
  !! and/or save data based on user-specified controls.
  !<
  subroutine oc_ot(this, ipflg)
    ! modules
    use TdisModule, only: kstp, endofperiod
    ! dummy
    class(OutputControlType) :: this !< OutputControlType object
    integer(I4B), intent(inout) :: ipflg !< flag indicating if data was printed
    ! local
    integer(I4B) :: ipos
    type(OutputControlDataType), pointer :: ocdobjptr

    ! Clear printout flag(ipflg).  This flag indicates that an array was
    ! printed to the listing file.
    ipflg = 0

    do ipos = 1, size(this%ocds)
      ocdobjptr => this%ocds(ipos)
      call ocdobjptr%ocd_ot(ipflg, kstp, endofperiod, this%iout)
    end do
  end subroutine oc_ot

  !> @ brief Deallocate method for OutputControlType
  !!
  !!  Deallocate member variables.
  !!
  !<
  subroutine oc_da(this)
    ! modules
    use MemoryManagerModule, only: mem_deallocate
    ! dummy
    class(OutputControlType) :: this !< OutputControlType object
    ! local
    integer(I4B) :: i

    do i = 1, size(this%ocds)
      call this%ocds(i)%ocd_da()
    end do
    deallocate (this%ocds)

    deallocate (this%name_model)
    call mem_deallocate(this%inunit)
    call mem_deallocate(this%iout)
    call mem_deallocate(this%ibudcsv)
    call mem_deallocate(this%iperoc)
    call mem_deallocate(this%iocrep)
  end subroutine oc_da

  !> @ brief Allocate variables for the output control object
  subroutine allocate (this, name_model)
    ! modules
    use MemoryManagerModule, only: mem_allocate
    use MemoryHelperModule, only: create_mem_path
    ! dummy
    class(OutputControlType) :: this !< this instance
    character(len=*), intent(in) :: name_model !< name of model

    this%memoryPath = create_mem_path(name_model, 'OC')

    allocate (this%name_model)
    call mem_allocate(this%inunit, 'INUNIT', this%memoryPath)
    call mem_allocate(this%iout, 'IOUT', this%memoryPath)
    call mem_allocate(this%ibudcsv, 'IBUDCSV', this%memoryPath)
    call mem_allocate(this%iperoc, 'IPEROC', this%memoryPath)
    call mem_allocate(this%iocrep, 'IOCREP', this%memoryPath)

    this%name_model = name_model
    this%inunit = 0
    this%iout = 0
    this%ibudcsv = 0
    this%iperoc = 0
    this%iocrep = 0
  end subroutine allocate

  !> @ brief Read the output control options block
  subroutine read_options(this)
    ! modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, store_error_unit
    ! dummy
    class(OutputControlType) :: this !< this instance
    ! local
    character(len=LINELENGTH) :: keyword
    character(len=LINELENGTH) :: keyword2
    character(len=LINELENGTH) :: fname
    character(len=:), allocatable :: line
    integer(I4B) :: ierr
    integer(I4B) :: ipos
    logical :: isfound, found, endOfBlock
    type(OutputControlDataType), pointer :: ocdobjptr

    ! get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)

    ! parse options block if detected
    if (isfound) then
      write (this%iout, '(/,1x,a,/)') 'PROCESSING OC OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        found = .false.
        if (keyword == 'BUDGETCSV') then
          call this%parser%GetStringCaps(keyword2)
          if (keyword2 /= 'FILEOUT') then
            errmsg = "BUDGETCSV must be followed by FILEOUT and then budget &
              &csv file name.  Found '"//trim(keyword2)//"'."
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
          end if
          call this%parser%GetString(fname)
          this%ibudcsv = GetUnit()
          call openfile(this%ibudcsv, this%iout, fname, 'CSV', &
                        filstat_opt='REPLACE')
          found = .true.
        end if

        if (.not. found) then
          do ipos = 1, size(this%ocds)
            ocdobjptr => this%ocds(ipos)
            if (keyword == trim(ocdobjptr%cname)) then
              found = .true.
              exit
            end if
          end do
          if (.not. found) then
            errmsg = "UNKNOWN OC OPTION '"//trim(keyword)//"'."
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
          end if
          call this%parser%GetRemainingLine(line)
          call ocdobjptr%set_option(line, this%parser%iuactive, this%iout)
        end if
      end do
      write (this%iout, '(1x,a)') 'END OF OC OPTIONS'
    end if
  end subroutine read_options

  !> @ brief Determine if it is time to save.
  logical function oc_save(this, cname)
    ! modules
    use TdisModule, only: kstp, endofperiod
    ! dummy
    class(OutputControlType) :: this !< OutputControlType object
    character(len=*), intent(in) :: cname !< character string for data name
    ! local
    integer(I4B) :: ipos
    logical :: found
    class(OutputControlDataType), pointer :: ocdobjptr
    !
    oc_save = .false.
    found = .false.
    do ipos = 1, size(this%ocds)
      ocdobjptr => this%ocds(ipos)
      if (cname == trim(ocdobjptr%cname)) then
        found = .true.
        exit
      end if
    end do
    if (found) then
      oc_save = ocdobjptr%psm%should_save(kstp, endofperiod)
    end if
  end function oc_save

  !> @ brief Determine if it is time to print.
  logical function oc_print(this, cname)
    ! modules
    use TdisModule, only: kstp, endofperiod
    ! dummy
    class(OutputControlType) :: this !< OutputControlType object
    character(len=*), intent(in) :: cname !< character string for data name
    ! local
    integer(I4B) :: ipos
    logical :: found
    class(OutputControlDataType), pointer :: ocdobjptr

    oc_print = .false.
    found = .false.
    do ipos = 1, size(this%ocds)
      ocdobjptr => this%ocds(ipos)
      if (cname == trim(ocdobjptr%cname)) then
        found = .true.
        exit
      end if
    end do
    if (found) then
      oc_print = ocdobjptr%psm%should_print(kstp, endofperiod)
    end if
  end function oc_print

  !> @ brief Determine unit number for saving
  function oc_save_unit(this, cname)
    ! -- modules
    ! -- return
    integer(I4B) :: oc_save_unit
    ! -- dummy
    class(OutputControlType) :: this !< OutputControlType object
    character(len=*), intent(in) :: cname !< character string for data name
    ! -- local
    integer(I4B) :: ipos
    logical :: found
    class(OutputControlDataType), pointer :: ocdobjptr
    !
    oc_save_unit = 0
    found = .false.
    do ipos = 1, size(this%ocds)
      ocdobjptr => this%ocds(ipos)
      if (cname == trim(ocdobjptr%cname)) then
        found = .true.
        exit
      end if
    end do
    if (found) then
      oc_save_unit = ocdobjptr%idataun
    end if
  end function oc_save_unit

  !> @ brief Set the print flag based on convergence and other parameters
  function set_print_flag(this, cname, icnvg, endofperiod) result(iprint_flag)
    ! -- modules
    use SimVariablesModule, only: isimcontinue
    ! -- return
    integer(I4B) :: iprint_flag
    ! -- dummy
    class(OutputControlType) :: this !< OutputControlType object
    character(len=*), intent(in) :: cname !< character string for data name
    integer(I4B), intent(in) :: icnvg !< convergence flag
    logical, intent(in) :: endofperiod !< end of period logical flag
    ! -- local
    !
    ! -- default is to not print
    iprint_flag = 0
    !
    ! -- if the output control file indicates that cname should be printed
    if (this%oc_print(cname)) iprint_flag = 1
    !
    ! -- if it is not a CONTINUE run, then set to print if not converged
    if (isimcontinue == 0) then
      if (icnvg == 0) iprint_flag = 1
    end if
    !
    ! -- if it's the end of the period, then set flag to print
    if (endofperiod) iprint_flag = 1
  end function set_print_flag

end module OutputControlModule

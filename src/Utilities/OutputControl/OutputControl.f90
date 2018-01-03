module OutputControlModule

  use KindModule, only: DP, I4B
  use ConstantsModule,    only: LENMODELNAME, LENORIGIN
  use OutputControlData, only: OutputControlDataType, ocd_cr
  use BlockParserModule, only: BlockParserType

  implicit none
  private
  public OutputControlType, oc_cr

  type OutputControlType
    character(len=LENMODELNAME), pointer                :: name_model => null() !name of the model
    character(len=LENORIGIN), pointer                   :: cid        => null() !character id of this object
    integer(I4B), pointer                               :: inunit     => null() !unit number for input file
    integer(I4B), pointer                               :: iout       => null() !unit number for output file
    integer(I4B), pointer                               :: iperoc     => null() !stress period number for next output control
    integer(I4B), pointer                               :: iocrep     => null() !output control repeat flag (period 0 step 0)
    type(OutputControlDataType), dimension(:), pointer  :: ocdobj     => null() !output control objects
    type(BlockParserType)                               :: parser
  contains
    procedure :: oc_df
    procedure :: oc_rp
    procedure :: oc_ot
    procedure :: oc_da
    procedure :: allocate_scalars
    procedure :: read_options
    procedure :: oc_save
    procedure :: oc_print
    procedure :: oc_save_unit
  end type OutputControlType

  contains

  subroutine oc_cr(ocobj, name_model, inunit, iout)
! ******************************************************************************
! oc_cr -- Create a new oc object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(OutputControlType), pointer :: ocobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(ocobj)
    !
    ! -- Allocate scalars
    call ocobj%allocate_scalars(name_model)
    !
    ! -- Save unit numbers
    ocobj%inunit = inunit
    ocobj%iout = iout
    !
    ! -- Initialize block parser
    call ocobj%parser%Initialize(inunit, iout)
    !
    ! -- Return
    return
  end subroutine oc_cr

  subroutine oc_df(this)
! ******************************************************************************
! oc_df -- define the Oc Object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(OutputControlType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine oc_df

  subroutine oc_rp(this)
! ******************************************************************************
! Read and prepare output control for this stress period
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule,              only: kper, nper
    use ConstantsModule,         only: LINELENGTH
    use SimModule, only: ustop, store_error, store_error_unit, count_errors
    ! -- dummy
    class(OutputControlType) :: this
    ! -- local
    integer(I4B) :: ierr, ival, ipos
    logical :: isfound, found, endOfBlock
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: ermsg, keyword1, keyword2
    character(len=5) :: printsave
    class(OutputControlDataType), pointer :: ocdobjptr
    ! -- formats
    character(len=*), parameter :: fmtboc =                                    &
      "(1X,/1X,'BEGIN READING OUTPUT CONTROL FOR STRESS PERIOD ',I0)"
    character(len=*), parameter :: fmteoc =                                    &
      "(/,1X,'END READING OUTPUT CONTROL FOR STRESS PERIOD ',I0)"
    character(len=*), parameter :: fmterr =                                    &
      "(' ERROR READING OUTPUT CONTROL PERIOD BLOCK: ')"
    character(len=*), parameter :: fmtroc =                                    &
      "(1X,/1X,'OUTPUT CONTROL FOR STRESS PERIOD ',I0,                         &
        ' IS REPEATED USING SETTINGS FROM A PREVIOUS STRESS PERIOD.')"
    character(len=*), parameter :: fmtpererr =                                 &
      "(1x,'CURRENT STRESS PERIOD GREATER THAN PERIOD IN OUTPUT CONTROL.')"
    character(len=*), parameter :: fmtpererr2 =                                 &
      "(1x,'CURRENT STRESS PERIOD: ',I0,' SPECIFIED STRESS PERIOD: ',I0)"
! ------------------------------------------------------------------------------
    !
    ! -- Read next block header if kper greater than last one read
    if (this%iperoc < kper) then
      !
      ! -- Get period block
      call this%parser%GetBlock('PERIOD', isfound, ierr, &
                                supportOpenClose=.true.)
      !
      ! -- If end of file, set iperoc past kper, else parse line
      if (ierr < 0) then
        this%iperoc = nper + 1
        write(this%iout, '(/,1x,a)') 'END OF FILE DETECTED IN OUTPUT CONTROL.'
        write(this%iout, '(1x,a)') 'CURRENT OUTPUT CONTROL SETTINGS WILL BE '
        write(this%iout, '(1x,a)') 'REPEATED UNTIL THE END OF THE SIMULATION.'
      else
        !
        ! -- Read period number
        ival = this%parser%GetInteger()
        !
        ! -- Check to see if this is a valid kper
        if(ival <= 0 .or. ival > nper) then
          write(ermsg, '(a,i0)') 'PERIOD NOT VALID IN OUTPUT CONTROL: ', ival
          call store_error(ermsg)
          write(ermsg, '(a, a)') 'LINE: ', trim(adjustl(line))
          call store_error(ermsg)
        endif
        !
        ! -- Check to see if specified is less than kper
        if(ival < kper) then
          write(ermsg, fmtpererr)
          call store_error(ermsg)
          write(ermsg, fmtpererr2) kper, ival
          call store_error(ermsg)
          write(ermsg, '(a, a)') 'LINE: ', trim(adjustl(line))
          call store_error(ermsg)
        endif
        !
        ! -- Stop or set iperoc and continue
        if(count_errors() > 0) then
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        this%iperoc = ival
      endif
    end if
    !
    ! -- Read the stress period block
    if (this%iperoc == kper) then
      !
      ! -- Clear io flags
      do ipos = 1, size(this%ocdobj)
        ocdobjptr => this%ocdobj(ipos)
        call ocdobjptr%psmobj%init()
      enddo
      !
      ! -- Output control time step matches simulation time step.
      write(this%iout,fmtboc) this%iperoc
      !
      ! -- loop to read records
      recordloop: do
        !
        ! -- Read the line
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword1)
        !
        ! -- Set printsave string and then read the record type (e.g.
        !    BUDGET, HEAD)
        printsave = keyword1
        call this%parser%GetStringCaps(keyword2)
        !
        ! -- Look through the output control data objects that are
        !    available and set ocdobjptr to the correct one based on
        !    cname.  Set found to .false. if not a valid record type.
        found = .false.
        do ipos = 1, size(this%ocdobj)
          ocdobjptr => this%ocdobj(ipos)
          if(keyword2 == trim(ocdobjptr%cname)) then
            found = .true.
            exit
          endif
        enddo
        if (.not. found) then
          call this%parser%GetCurrentLine(line)
          write(ermsg, fmterr)
          call store_error(ermsg)
          call store_error('UNRECOGNIZED KEYWORD: '//keyword2)
          call store_error(trim(line))
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        call this%parser%GetRemainingLine(line)
        call ocdobjptr%psmobj%rp(trim(printsave)//' '//line,      &
                                 this%iout)
        call ocdobjptr%ocd_rp_check(this%parser%iuactive)
        !
        ! -- End of recordloop
      enddo recordloop
      write(this%iout,fmteoc) this%iperoc
    else
      !
      ! -- Write message that output control settings are from a previous
      !    stress period.
      write(this%iout, fmtroc) kper
    endif
    !
    ! -- return
    return
  end subroutine oc_rp

  subroutine oc_ot(this, ipflg)
! ******************************************************************************
! oc_ot -- output information
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper, nstp
    ! -- dummy
    class(OutputControlType) :: this
    integer(I4B), intent(inout) :: ipflg
    ! -- local
    integer(I4B) :: ipos
    type(OutputControlDataType), pointer   :: ocdobjptr
! ------------------------------------------------------------------------------
    !
    ! -- Clear printout flag(ipflg).  This flag indicates that an array was
    !    printed to the listing file.
    ipflg = 0
    !
    do ipos = 1, size(this%ocdobj)
      ocdobjptr => this%ocdobj(ipos)
      call ocdobjptr%ocd_ot(ipflg, kstp, nstp(kper), this%iout)
    enddo
    !
    ! -- Return
    return
  end subroutine oc_ot

  subroutine oc_da(this)
! ******************************************************************************
! oc_da -- deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(OutputControlType) :: this
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    do i = 1, size(this%ocdobj)
      call this%ocdobj(i)%ocd_da()
    enddo
    deallocate(this%ocdobj)
    !
    deallocate(this%name_model)
    deallocate(this%cid)
    call mem_deallocate(this%inunit)
    call mem_deallocate(this%iout)
    call mem_deallocate(this%iperoc)
    call mem_deallocate(this%iocrep)
    !
    ! -- return
    return
  end subroutine oc_da

  subroutine allocate_scalars(this, name_model)
! ******************************************************************************
! allocate_scalars -- Allocate scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(OutputControlType) :: this
    character(len=*), intent(in) :: name_model
! ------------------------------------------------------------------------------
    !
    allocate(this%name_model)
    allocate(this%cid)
    this%cid = trim(adjustl(name_model)) // ' OC2'
    call mem_allocate(this%inunit, 'INUNIT', this%cid)
    call mem_allocate(this%iout, 'IOUT', this%cid)
    call mem_allocate(this%iperoc, 'IPEROC', this%cid)
    call mem_allocate(this%iocrep, 'IOCREP', this%cid)
    !
    this%name_model = name_model
    this%inunit = 0
    this%iout = 0
    this%iperoc = 0
    this%iocrep = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars

  subroutine read_options(this)
! ******************************************************************************
! read_options -- read oc options block
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, store_error_unit
    ! -- dummy
    class(OutputControlType) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, keyword
    integer(I4B) :: ierr
    integer(I4B) :: ipos
    logical :: isfound, found, endOfBlock
    type(OutputControlDataType), pointer   :: ocdobjptr
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING OC2 OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        found = .false.
        do ipos = 1, size(this%ocdobj)
          ocdobjptr => this%ocdobj(ipos)
          if(keyword == trim(ocdobjptr%cname)) then
            found = .true.
            exit
          endif
        enddo
        if (.not. found) then
          write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN OC OPTION: ',       &
                                 keyword
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        call this%parser%GetRemainingLine(line)
        call ocdobjptr%set_option(line, this%parser%iuactive, this%iout)
      end do
      write(this%iout,'(1x,a)')'END OF OC OPTIONS'
    end if
    !
    ! -- return
    return
  end subroutine read_options

  logical function oc_save(this, cname)
! ******************************************************************************
! oc_save -- determine if it is time to save cname
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper, nstp
    ! -- dummy
    class(OutputControlType) :: this
    character(len=*), intent(in) :: cname
    ! -- local
    integer(I4B) :: ipos
    logical :: found
    class(OutputControlDataType), pointer :: ocdobjptr
! ------------------------------------------------------------------------------
    !
    oc_save = .false.
    found = .false.
    do ipos = 1, size(this%ocdobj)
      ocdobjptr => this%ocdobj(ipos)
      if(cname == trim(ocdobjptr%cname)) then
        found = .true.
        exit
      endif
    enddo
    if(found) then
      oc_save = ocdobjptr%psmobj%kstp_to_save(kstp, nstp(kper))
    endif
    !
    ! -- Return
    return
  end function oc_save

  logical function oc_print(this, cname)
! ******************************************************************************
! oc_print -- determine if it is time to print cname
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper, nstp
    ! -- dummy
    class(OutputControlType) :: this
    character(len=*), intent(in) :: cname
    ! -- local
    integer(I4B) :: ipos
    logical :: found
    class(OutputControlDataType), pointer :: ocdobjptr
! ------------------------------------------------------------------------------
    !
    oc_print = .false.
    found = .false.
    do ipos = 1, size(this%ocdobj)
      ocdobjptr => this%ocdobj(ipos)
      if(cname == trim(ocdobjptr%cname)) then
        found = .true.
        exit
      endif
    enddo
    if(found) then
      oc_print = ocdobjptr%psmobj%kstp_to_print(kstp, nstp(kper))
    endif
    !
    ! -- Return
    return
  end function oc_print

  function oc_save_unit(this, cname)
! ******************************************************************************
! oc_save_unit -- determine unit number for saving
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    integer(I4B) :: oc_save_unit
    ! -- dummy
    class(OutputControlType) :: this
    character(len=*), intent(in) :: cname
    ! -- local
    integer(I4B) :: ipos
    logical :: found
    class(OutputControlDataType), pointer :: ocdobjptr
! ------------------------------------------------------------------------------
    !
    oc_save_unit = 0
    found = .false.
    do ipos = 1, size(this%ocdobj)
      ocdobjptr => this%ocdobj(ipos)
      if(cname == trim(ocdobjptr%cname)) then
        found = .true.
        exit
      endif
    enddo
    if(found) then
      oc_save_unit = ocdobjptr%idataun
    endif
    !
    ! -- Return
    return
  end function oc_save_unit

end module OutputControlModule

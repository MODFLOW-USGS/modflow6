module OutputControlData
  
  use BaseDisModule,          only: DisBaseType
  use InputOutputModule,      only: print_format
  use KindModule,             only: DP, I4B
  use PrintSaveManagerModule, only: PrintSaveManagerType
  
  implicit none
  private
  public OutputControlDataType, ocd_cr
  
  type OutputControlDataType
    character(len=16), pointer              :: cname    => null()
    character(len=60), pointer              :: cdatafmp => null()
    integer(I4B), pointer                   :: idataun  => null()
    character(len=1), pointer               :: editdesc => null()
    integer(I4B), pointer                   :: nvaluesp => null()
    integer(I4B), pointer                   :: nwidthp  => null()
    real(DP), pointer                       :: dnodata  => null()
    integer(I4B), pointer                   :: inodata  => null()
    real(DP), dimension(:), pointer         :: dblvec   => null()
    integer(I4B), dimension(:), pointer     :: intvec   => null()
    class(DisBaseType), pointer             :: dis      => null()
    type(PrintSaveManagerType), pointer     :: psmobj   => null()
  contains
    procedure :: allocate_scalars
    procedure :: init_int
    procedure :: init_dbl
    procedure :: set_option
    procedure :: ocd_rp_check
    procedure :: ocd_ot
    procedure :: ocd_da
  end type OutputControlDataType
  
  contains
  
  subroutine ocd_cr(ocdobj)
! ******************************************************************************
! ocd_cr -- Create a new ocd object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(OutputControlDataType), pointer :: ocdobj
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(ocdobj)
    !
    ! -- Allocate scalars
    call ocdobj%allocate_scalars()
    !
    ! -- Return
    return
  end subroutine ocd_cr
  
  subroutine ocd_rp_check(this, inunit)
! ******************************************************************************
! ocd_rp_check -- Check to make sure settings are consistent
! ******************************************************************************
!
!    Specifications:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_unit, ustop
    ! -- dummy
    class(OutputControlDataType) :: this
    integer(I4B), intent(in) :: inunit
    ! -- locals
    character(len=LINELENGTH) :: errmsg
    ! -- formats
    character(len=*), parameter :: fmtocsaveerr =                              &
      "(1X,'REQUESTING TO SAVE ',A,' BUT ',A,' SAVE FILE NOT SPECIFIED. ',     &
       &A,' SAVE FILE MUST BE SPECIFIED IN OUTPUT CONTROL OPTIONS.')"
! ------------------------------------------------------------------------------
    !
    ! -- Check to make sure save file was specified
    if(this%psmobj%save_detected) then
      if(this%idataun == 0) then
        write(errmsg, fmtocsaveerr) trim(adjustl(this%cname)),                 &
                                    trim(adjustl(this%cname)),                 &
                                    trim(adjustl(this%cname))
        call store_error(errmsg)
      endif
    endif
    !
    if(count_errors() > 0) then
      call store_error_unit(inunit)
      call ustop()
    endif
    !
    ! -- return
    return
  end subroutine ocd_rp_check

  subroutine ocd_ot(this, ipflg, kstp, nstp, iout)
! ******************************************************************************
! ocd_ot -- record information
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(OutputControlDataType) :: this
    integer(I4B), intent(inout) :: ipflg
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: nstp
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: iprint
    integer(I4B) :: idataun
! ------------------------------------------------------------------------------
    !
    iprint = 0
    if(this%psmobj%kstp_to_print(kstp, nstp)) then
      iprint = 1
      ipflg = 1
    endif
    idataun = 0
    if(this%psmobj%kstp_to_save(kstp, nstp)) idataun = this%idataun
    !
    ! -- Record double precision array
    if(associated(this%dblvec))                                                &
    call this%dis%record_array(this%dblvec, iout, iprint, idataun,         &
                               this%cname, this%cdatafmp, this%nvaluesp,   &
                               this%nwidthp, this%editdesc, this%dnodata)
    !
    ! -- Record integer array (not supported yet)
    !if(associated(this%intvec))                                                &
    !call this%dis%record_array(this%intvec, iout, iprint, idataun,         &
    !                               this%cname, this%cdatafmp, this%nvaluesp,   &
    !                               this%nwidthp, this%editdesc, this%inodata)
    !
    ! -- Return
    return
  end subroutine ocd_ot
  
  subroutine ocd_da(this)
! ******************************************************************************
! ocd_da --deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(OutputControlDataType) :: this
! ------------------------------------------------------------------------------
    !
    deallocate(this%cname)
    deallocate(this%cdatafmp)
    deallocate(this%idataun)
    deallocate(this%editdesc)
    deallocate(this%nvaluesp)
    deallocate(this%nwidthp)
    deallocate(this%dnodata)
    deallocate(this%inodata)
    deallocate(this%psmobj)
    !
    ! -- return
    return
  end subroutine ocd_da  
  
  subroutine init_dbl(this, cname, dblvec, dis, cdefpsm, cdeffmp, iout,    &
                      dnodata)
! ******************************************************************************
! init_int -- Initialize integer variable
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(OutputControlDataType) :: this
    character(len=*), intent(in) :: cname
    real(DP), dimension(:), pointer, intent(in) :: dblvec
    class(DisBaseType), pointer, intent(in) :: dis
    character(len=*), intent(in) :: cdefpsm
    character(len=*), intent(in) :: cdeffmp
    integer(I4B), intent(in) :: iout
    real(DP), intent(in) :: dnodata
! ------------------------------------------------------------------------------
    !
    this%cname = cname
    this%dblvec => dblvec
    this%dis => dis
    this%dnodata = dnodata
    call this%psmobj%init()
    if (cdefpsm /= '') call this%psmobj%rp(cdefpsm, iout)
    call print_format(cdeffmp, this%cdatafmp,  &
                      this%editdesc, this%nvaluesp, this%nwidthp, 0)
    !
    ! -- return
    return
  end subroutine init_dbl
  
  subroutine init_int(this, cname, intvec, dis, cdefpsm, cdeffmp, iout,    &
                      inodata)
! ******************************************************************************
! init_int -- Initialize integer variable
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(OutputControlDataType) :: this
    character(len=*), intent(in) :: cname
    integer(I4B), dimension(:), pointer, intent(in) :: intvec
    class(DisBaseType), pointer, intent(in) :: dis
    character(len=*), intent(in) :: cdefpsm
    character(len=*), intent(in) :: cdeffmp
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(in) :: inodata
! ------------------------------------------------------------------------------
    !
    this%cname = cname
    this%intvec => intvec
    this%dis => dis
    this%inodata = inodata
    this%editdesc = 'I'
    call this%psmobj%init()
    if (cdefpsm /= '') call this%psmobj%rp(cdefpsm, iout)
    call print_format(cdeffmp, this%cdatafmp, this%editdesc, this%nvaluesp,    &
                      this%nwidthp, 0)
    !
    ! -- return
    return
  end subroutine init_int  
  
  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- Allocate scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(OutputControlDataType) :: this
! ------------------------------------------------------------------------------
    !
    allocate(this%cname)
    allocate(this%cdatafmp)
    allocate(this%idataun)
    allocate(this%editdesc)
    allocate(this%nvaluesp)
    allocate(this%nwidthp)
    allocate(this%dnodata)
    allocate(this%inodata)
    allocate(this%psmobj)
    !
    this%cname = ''
    this%cdatafmp = ''
    this%idataun = 0
    this%editdesc = ''
    this%nvaluesp = 0
    this%nwidthp = 0
    this%dnodata = DZERO
    this%inodata = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars  
  
  subroutine set_option(this, linein, inunit, iout)
! ******************************************************************************
! allocate_scalars -- Allocate scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use OpenSpecModule, only: access, form
    use InputOutputModule, only: urword, getunit, openfile
    use SimModule, only: store_error, store_error_unit, count_errors, ustop
    ! -- dummy
    class(OutputControlDataType) :: this
    character(len=*), intent(in) :: linein
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=len(linein)) :: line
    integer(I4B) :: lloc, istart, istop, ival
    real(DP) :: rval
    ! -- format
    character(len=*),parameter :: fmtocsave = &
      "(4X,A,' INFORMATION WILL BE WRITTEN TO:',                               &
       &/,6X,'UNIT NUMBER: ', I0,/,6X, 'FILE NAME: ', A)"
! ------------------------------------------------------------------------------
    !
    line(:) = linein(:)
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
    select case(line(istart:istop))
    case('FILEOUT')
      call urword(line, lloc, istart, istop, 0, ival, rval, 0, 0)
      this%idataun = getunit()
      write(iout, fmtocsave) trim(adjustl(this%cname)), this%idataun,          &
                             line(istart:istop)
      call openfile(this%idataun, iout, line(istart:istop), 'DATA(BINARY)',    &
                    form, access, 'REPLACE')
    case('PRINT_FORMAT')
      call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
      call print_format(line(istart:), this%cdatafmp, this%editdesc,           &
                        this%nvaluesp, this%nwidthp, inunit)
    case default
       call store_error('Looking for FILEOUT or PRINT_FORMAT.  Found:')
       call store_error(trim(adjustl(line)))
       call store_error_unit(inunit)
       call ustop()
    end select
    !
    ! -- return
    return
  end subroutine set_option  
  
end module OutputControlData

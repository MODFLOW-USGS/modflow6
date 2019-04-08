module MvrModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENMODELNAME, LENPACKAGENAME, LINELENGTH,         &
                             LENBUDTXT, LENAUXNAME, LENBOUNDNAME, DZERO, DONE, &
                             LENORIGIN
  use PackageMoverModule, only: PackageMoverType
  
  implicit none
  private
  public :: MvrType

  character(len=12), dimension(4) :: mvrtypes =                                &
    [character(len=12) :: 'FACTOR', 'EXCESS', 'THRESHOLD', 'UPTO']
  
  type MvrType
    character(len=LENMODELNAME+LENPACKAGENAME+1) :: pname1 = ''                  !provider package name
    character(len=LENMODELNAME+LENPACKAGENAME+1) :: pname2 = ''                  !receiver package name
    integer(I4B)                                 :: irch1 = 0                    !provider reach number
    integer(I4B)                                 :: irch2 = 0                    !receiver reach number
    integer(I4B)                                 :: imvrtype = 0                 !mover type (1, 2, 3, 4) corresponds to mvrtypes
    real(DP)                                     :: value = DZERO                !factor or rate depending on mvrtype
    real(DP)                                     :: qpold = DZERO                !provider rate from last time step
    real(DP)                                     :: qpnew = DZERO                !new provider rate
    real(DP)                                     :: qpactual = DZERO             !rate provided to the receiver
    real(DP)                                     :: qanew = DZERO                !rate available at time of providing
    real(DP)                                     :: qaold = DZERO                !rate available fromtime step
    real(DP), pointer                            :: qtformvr_ptr => null()       !pointer to total available flow (qtformvr)
    real(DP), pointer                            :: qformvr_ptr => null()        !pointer to available flow after consumed (qformvr)
    real(DP), pointer                            :: qtomvr_ptr => null()         !pointer to provider flow rate (qtomvr)
    real(DP), pointer                            :: qfrommvr_ptr => null()       !pointer to receiver flow rate (qfrommvr)
  contains
    procedure :: set
    procedure :: set_qpold
    procedure :: echo
    procedure :: advance
    procedure :: fc
    procedure :: qrcalc
    procedure :: writeflow
  end type MvrType
  
  contains
  
  subroutine set(this, line, inunit, iout, mname, pakorigins, pakmovers)
! ******************************************************************************
! set -- Setup mvr object
!        If mname == '', then read mname out of line. pakorigins is an array
!        of strings, which are model names and package names.  The mover
!        entries must be in pakorigins, or this routine will terminate with
!        an error.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: urword, extract_idnum_or_bndname
    use SimModule, only: ustop, store_error, store_error_unit, count_errors
    ! -- dummy
    class(MvrType) :: this
    character(len=*), intent(inout) :: line
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    character(len=LENMODELNAME), intent(in) :: mname
    character(len=LENORIGIN+1),                                                &
      dimension(:), pointer, contiguous              :: pakorigins
    type(PackageMoverType), dimension(:), pointer, contiguous    :: pakmovers
    ! -- local
    integer(I4B) :: lloc, istart, istop, ival
    real(DP) :: rval
    real(DP), dimension(:), pointer, contiguous :: temp_ptr => null()
    character(len=LINELENGTH) :: errmsg
    character(len=LENBOUNDNAME) :: bndname
    logical :: mnamel, found
    integer(I4B) :: i
    integer(I4B) :: ipakloc1, ipakloc2
! ------------------------------------------------------------------------------
    !
    ! -- Check for valid mname and set logical mnamel flag
    if(mname == '') then
      mnamel = .false.
    else
      mnamel = .true.
    endif
    !    
    ! -- Set lloc for line
    lloc = 1
    !
    ! -- Construct provider name, which is modelname followed by packagename
    if(mnamel) then
      this%pname1 = trim(adjustl(mname))
    else
      call urword(line, lloc, istart, istop, 1, ival, rval, iout, inunit)
      this%pname1 = line(istart:istop)
    endif
    call urword(line, lloc, istart, istop, 1, ival, rval, iout, inunit)
    this%pname1 = trim(this%pname1) // ' ' // line(istart:istop)
    !
    ! -- Read id for the provider
    call extract_idnum_or_bndname(line, lloc, istart, istop, ival, bndname)
    this%irch1 = ival
    !
    ! -- Construct receiver name, which is modelname followed by packagename
    if(mnamel) then
      this%pname2 = trim(adjustl(mname))
    else
      call urword(line, lloc, istart, istop, 1, ival, rval, iout, inunit)
      this%pname2 = line(istart:istop)
    endif
    call urword(line, lloc, istart, istop, 1, ival, rval, iout, inunit)
    this%pname2 = trim(this%pname2) // ' ' // line(istart:istop)
    !
    ! -- Read id for the receiver
    call extract_idnum_or_bndname(line, lloc, istart, istop, ival, bndname)
    this%irch2 = ival
    !
    ! -- Read mover type
    call urword(line, lloc, istart, istop, 1, ival, rval, iout, inunit)
    select case(line(istart:istop))
      case('FACTOR')
        this%imvrtype = 1
      case('EXCESS')
        this%imvrtype = 2
      case('THRESHOLD')
        this%imvrtype = 3
      case('UPTO')
        this%imvrtype = 4
      case default
        call store_error('ERROR. INVALID MOVER TYPE: '//trim(line(istart:istop)) )
        call store_error_unit(inunit)
        call ustop()
    end select
    !
    ! -- Read mover value
    call urword(line, lloc, istart, istop, 3, ival, rval, iout, inunit)
    this%value = rval
    !
    ! -- initialize values to zero
    call this%set_qpold(DZERO)
    !
    ! -- Check to make sure provider and receiver are not the same
    if(this%pname1 == this%pname2 .and. this%irch1 == this%irch2) then
      call store_error('ERROR. PROVIDER AND RECEIVER ARE THE SAME: '//         &
        trim(line))
      call store_error_unit(inunit)
      call ustop()
    endif
    !
    ! -- Check to make sure pname1 and pname2 are both listed in pakorigins
    !    pname1 is the provider package; pname2 is the receiver package
    found = .false.
    ipakloc1 = 0
    do i = 1, size(pakorigins)
      if (this%pname1 == pakorigins(i)) then
        found = .true.
        ipakloc1 = i
        exit
      endif
    end do
    if (.not. found) then
      call store_error('MOVER CAPABILITY NOT ACTIVATED IN '//this%pname1)
      call store_error('ADD "MOVER" KEYWORD TO PACKAGE OPTIONS BLOCK.')
    end if
    found = .false.
    ipakloc2 = 0
    do i = 1, size(pakorigins)
      if (this%pname2 == pakorigins(i)) then
        found = .true.
        ipakloc2 = i
        exit
      endif
    end do
    if (.not. found) then
      call store_error('MOVER CAPABILITY NOT ACTIVATED IN '//this%pname2)
      call store_error('ADD "MOVER" KEYWORD TO PACKAGE OPTIONS BLOCK.')
    end if
    if (count_errors() > 0) then
      call store_error_unit(inunit)
      call ustop()
    end if
    !
    ! -- Set pointer to QTOMVR array in the provider boundary package
    temp_ptr => pakmovers(ipakloc1)%qtomvr
    if(this%irch1 < 1 .or. this%irch1 > size(temp_ptr)) then
      call store_error('ERROR. PROVIDER ID < 1 OR GREATER THAN PACKAGE SIZE ')
      write(errmsg, '(4x,a,i0,a,i0)') 'PROVIDER ID = ', this%irch1,            &
        '; PACKAGE SIZE = ', size(temp_ptr)
      call store_error(trim(errmsg))
      call store_error_unit(inunit)
      call ustop()
    endif
    this%qtomvr_ptr => temp_ptr(this%irch1)
    !
    ! -- Set pointer to QFORMVR array in the provider boundary package
    temp_ptr => pakmovers(ipakloc1)%qformvr
    this%qformvr_ptr => temp_ptr(this%irch1)
    !
    ! -- Set pointer to QTFORMVR array in the provider boundary package
    temp_ptr => pakmovers(ipakloc1)%qtformvr
    this%qtformvr_ptr => temp_ptr(this%irch1)
    !
    ! -- Set pointer to QFROMMVR array in the receiver boundary package
    temp_ptr => pakmovers(ipakloc2)%qfrommvr
    if(this%irch2 < 1 .or. this%irch2 > size(temp_ptr)) then
      call store_error('ERROR. PROVIDER ID < 1 OR GREATER THAN PACKAGE SIZE ')
      write(errmsg, '(4x,a,i0,a,i0)') 'RECEIVER ID = ', this%irch2,            &
        '; PACKAGE SIZE = ', size(temp_ptr)
      call store_error(trim(errmsg))
      call store_error_unit(inunit)
      call ustop()
    endif
    this%qfrommvr_ptr => temp_ptr(this%irch2)
    !
    ! -- return
    return
  end subroutine set
  
  subroutine set_qpold(this, value)
! ******************************************************************************
! set_qpold -- Set the value of qpold
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(MvrType) :: this
    real(DP), intent(in) :: value
    ! -- local
! ------------------------------------------------------------------------------
    !
    this%qpold = value
    !
    ! -- return
    return
  end subroutine set_qpold
  
  subroutine echo(this, iout)
! ******************************************************************************
! echo -- Write the mover info that was read from file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(MvrType) :: this
    integer(I4B), intent(in) :: iout
    ! -- local
! ------------------------------------------------------------------------------
    !
    write(iout, '(4x, a, a, a, i0)') 'FROM PACKAGE: ', trim(this%pname1),      &
      ' FROM ID: ', this%irch1
    write(iout, '(4x, a, a, a, i0)') 'TO PACKAGE: ', trim(this%pname2),        &
      ' TO ID: ', this%irch2
    write(iout, '(4x, a, a, a, 1pg15.6,/)') 'MOVER TYPE: ',                    &
      trim(mvrtypes(this%imvrtype)), ' ', this%value
    !
    ! -- return
    return
  end subroutine echo
  
  subroutine advance(this)
! ******************************************************************************
! advance -- Advance the mover
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(MvrType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    this%qpold = this%qpactual
    this%qaold = this%qanew
    !
    ! -- return
    return
  end subroutine advance
  
  subroutine fc(this, omega)
! ******************************************************************************
! fc -- formulate coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(MvrType) :: this
    real(DP), intent(in) :: omega
    ! -- local
    real(DP) :: qanew, qtanew, qpnew, qpactual
! ------------------------------------------------------------------------------
    !
    ! -- Set qa and this%qanew equal to available water in package (qtomvr)
    qanew = this%qformvr_ptr
    qtanew = this%qtformvr_ptr
    this%qanew = qanew
    !
    ! -- Using the mover rules, calculate how much of the available water will
    !    be provided from the mover to the receiver.
    qpnew = this%qrcalc(qanew, qtanew)
    !
    ! -- Calculate weighted value for qpactual using qpnew and qpold
    qpactual = omega * qpnew
    !
    ! -- Store qpactual
    this%qpactual = qpactual
    !
    ! -- Add the calculated qpactual term directly into the receiver package
    !    qfrommvr array.  
    this%qfrommvr_ptr = this%qfrommvr_ptr + qpactual
    !
    ! -- Add the calculated qpactual term directly into the provider package 
    !    qtomvr array.
    this%qtomvr_ptr = this%qtomvr_ptr + qpactual
    !
    ! -- Reduce the amount of water that is available in the provider package
    !    qformvr array.
    this%qformvr_ptr = this%qformvr_ptr - qpactual
    !
    ! -- return
    return
  end subroutine fc

  function qrcalc(this, qa, qta) result(qr)
! ******************************************************************************
! qrcalc -- Calculate the rate of water provided to the receiver
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    real(DP) :: qr
    ! -- dummy
    class(MvrType) :: this
    real(DP), intent(in) :: qa
    real(DP), intent(in) :: qta
    ! -- local
! ------------------------------------------------------------------------------
    ! -- Using the mover rules, calculate how much of the available water will
    !    go to the receiver.
    qr = DZERO
    ! -- Calculate qr
    select case (this%imvrtype)
      case(1)
        ! -- FACTOR uses total available to make calculation, and then
        !    limits qr by consumed available
        if(qta > DZERO) qr = qta * this%value
        qr = min(qr, qa)
      case(2)
        ! -- EXCESS
        if(qa > this%value) then
          qr = qa - this%value
        else
          qr = DZERO
        endif
      case(3)
        ! -- THRESHOLD
        if(this%value > qa) then
          qr = DZERO
        else
          qr = this%value
        endif
      case(4)
        ! -- UPTO
        if(qa > this%value) then
          qr = this%value
        else
          qr = qa
        endif
    end select
    !
    ! -- return
    return
  end function qrcalc

  subroutine writeflow(this, iout)
! ******************************************************************************
! writeflow -- Write mover flow information
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(MvrType) :: this
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=*), parameter :: fmt = &
      "(1x, a, ' ID ', i0, ' AVAILABLE ', 1(1pg15.6), " // &
      "' PROVIDED ', 1(1pg15.6), ' TO ', a, ' ID ', i0)"
! ------------------------------------------------------------------------------
    !
    write(iout, fmt) trim(this%pname1), this%irch1, this%qanew, this%qpactual, &
      trim(this%pname2), this%irch2
    !
    ! -- return
    return
  end subroutine writeflow
  
end module MvrModule
  

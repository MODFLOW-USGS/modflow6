module BudgetFileReaderModule

  use KindModule
  use SimModule, only: store_error, store_error_unit, ustop
  use ConstantsModule, only: LINELENGTH

  implicit none
  
  private
  public :: BudgetFileReaderType
  
  type :: BudgetFileReaderType
    
    logical :: hasimeth1flowja = .false.
    integer(I4B) :: inunit
    integer(I4B) :: nbudterms = 0
    integer(I4B) :: kstp
    integer(I4B) :: kper
    character(len=16) :: budtxt
    integer(I4B) :: nval
    integer(I4B) :: idum1
    integer(I4B) :: idum2
    integer(I4B) :: imeth
    real(DP) :: delt
    real(DP) :: pertim
    real(DP) :: totim
    character(len=16) :: srcmodelname
    character(len=16) :: srcpackagename
    integer(I4B) :: ndat
    character(len=16), dimension(:), allocatable :: auxtxt
    integer(I4B) :: nlist
    integer(I4B), allocatable, dimension(:) :: ia
    integer(I4B), allocatable, dimension(:) :: ja
    real(DP), dimension(:), allocatable :: flowja
    integer(I4B), dimension(:), allocatable :: nodesrc
    integer(I4B), dimension(:), allocatable :: nodedst
    real(DP), dimension(:, :), allocatable :: flowdata
    character(len=16) :: dstmodelname
    character(len=16) :: dstpackagename
  
  contains
  
    procedure :: initialize
    procedure :: read_record
    procedure :: finalize
  
  end type BudgetFileReaderType
  
  contains
  
  subroutine initialize(this, iu, iout, ncrbud)
! ******************************************************************************
! initialize
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BudgetFileReaderType) :: this
    integer(I4B), intent(in) :: iu
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(out) :: ncrbud
    ! -- local
    integer(I4B) :: icount, kstp_last, kper_last
    logical :: success
! ------------------------------------------------------------------------------
    this%inunit = iu
    icount = 0
    ncrbud = 0
    !
    ! -- Read the first budget data record to set kstp_last, kstp_last
    call this%read_record(success)
    kstp_last = this%kstp
    kper_last = this%kper
    rewind(this%inunit)
    !
    ! -- Determine number of budget terms within a time step
    if (iout > 0) &
      write(iout, '(a)') &
        'Reading budget file to determine number of terms per time step.'
    icount = 1
    do
      call this%read_record(success, iout)
      if (.not. success) exit
      if (kstp_last /= this%kstp .or. kper_last /= this%kper) exit
      icount = icount + 1
      this%nbudterms = this%nbudterms + 1
      if (trim(adjustl(this%budtxt)) == 'FLOW-JA-FACE' .and. &
          this%srcmodelname == this%dstmodelname) then
        if(allocated(this%nodesrc)) ncrbud = maxval(this%nodesrc)
      endif
    enddo
    rewind(this%inunit)
    if (iout > 0) &
    write(iout, '(a, i0, a)') 'Detected ', this%nbudterms, &
      ' unique flow terms in budget file.'
    !
    ! -- return
    return
  end subroutine initialize
  
  subroutine read_record(this, success, iout_opt)
! ******************************************************************************
! read_record
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BudgetFileReaderType) :: this
    logical, intent(out) :: success
    integer(I4B), intent(in), optional :: iout_opt
    ! -- local
    integer(I4B) :: i, n, iostat, iout
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    if (present(iout_opt)) then
      iout = iout_opt
    else
      iout = 0
    endif
    !
    this%kstp = 0
    this%kper = 0
    this%budtxt = ''
    this%nval = 0
    this%idum1 = 0
    this%idum2 = 0
    this%srcmodelname = ''
    this%srcpackagename = ''
    this%dstmodelname = ''
    this%dstpackagename = ''
     
    success = .true.
    read(this%inunit, iostat=iostat) this%kstp, this%kper, this%budtxt, &
      this%nval, this%idum1, this%idum2
    if (iostat /= 0) then
      success = .false.
      return
    endif
    read(this%inunit) this%imeth, this%delt, this%pertim, this%totim
    if(this%imeth == 1) then
      if (trim(adjustl(this%budtxt)) == 'FLOW-JA-FACE') then
        if(allocated(this%flowja)) deallocate(this%flowja)
        allocate(this%flowja(this%nval))
        read(this%inunit) this%flowja
        this%hasimeth1flowja = .true.
      else
        this%nval = this%nval * this%idum1 * abs(this%idum2)
        if(allocated(this%flowdata)) deallocate(this%flowdata)
        allocate(this%flowdata(1, this%nval))
        if(allocated(this%nodesrc)) deallocate(this%nodesrc)
        allocate(this%nodesrc(this%nval))
        read(this%inunit) this%flowdata
        do i = 1, this%nval
          this%nodesrc(i) = i
        enddo
      endif
    elseif (this%imeth == 6) then
      ! -- method code 6
      read(this%inunit) this%srcmodelname
      read(this%inunit) this%srcpackagename
      read(this%inunit) this%dstmodelname
      read(this%inunit) this%dstpackagename
      read(this%inunit) this%ndat
      if(allocated(this%auxtxt)) deallocate(this%auxtxt)
      allocate(this%auxtxt(this%ndat-1))
      read(this%inunit) this%auxtxt
      read(this%inunit) this%nlist
      if(allocated(this%nodesrc)) deallocate(this%nodesrc)
      allocate(this%nodesrc(this%nlist))
      if(allocated(this%nodedst)) deallocate(this%nodedst)
      allocate(this%nodedst(this%nlist))
      if(allocated(this%flowdata)) deallocate(this%flowdata)
      allocate(this%flowdata(this%ndat, this%nlist))
      read(this%inunit) (this%nodesrc(n), this%nodedst(n), &
        (this%flowdata(i,n), i = 1, this%ndat), n = 1, this%nlist)
    else
      write(errmsg, '(a, a)') 'ERROR READING: ', trim(this%budtxt)
      call store_error(errmsg)
      write(errmsg, '(a, i0)') 'INVALID METHOD CODE DETECTED: ', this%imeth
      call store_error(errmsg)
      call store_error_unit(this%inunit)
      call ustop()
    endif
    if (iout > 0) then
      write(iout, '(1pg15.6, a, 1x, a)') this%totim, this%budtxt, &
        this%dstpackagename
    endif
    !
    ! -- return
    return
  end subroutine read_record
  
  subroutine finalize(this)
! ******************************************************************************
! budgetdata_finalize
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BudgetFileReaderType) :: this
! ------------------------------------------------------------------------------
    close(this%inunit)
    if(allocated(this%auxtxt)) deallocate(this%auxtxt)
    if(allocated(this%ia)) deallocate(this%ia)
    if(allocated(this%ja)) deallocate(this%ja)
    if(allocated(this%flowja)) deallocate(this%flowja)
    if(allocated(this%nodesrc)) deallocate(this%nodesrc)
    if(allocated(this%nodedst)) deallocate(this%nodedst)
    if(allocated(this%flowdata)) deallocate(this%flowdata)
    !
    ! -- return
    return
  end subroutine finalize
  
end module BudgetFileReaderModule

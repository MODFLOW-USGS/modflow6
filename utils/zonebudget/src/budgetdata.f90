module BudgetDataModule

  use SimModule, only: store_error, store_error_unit, ustop
  use ConstantsModule, only: LINELENGTH
  implicit none
  
  private
  public :: budgetdata_init
  public :: budgetdata_read
  public :: budgetdata_finalize
  public :: budtxt, ia, ja, flowja, nodesrc, nodedst, flowdata,                &
            dstpackagename, nbudterms, kstp, kper, delt, totim,                &
            srcmodelname, dstmodelname, hasimeth1flowja
  
  logical :: hasimeth1flowja = .false.
  integer :: inunit
  integer :: nbudterms = 0
  integer :: kstp
  integer :: kper
  character(len=16) :: budtxt
  integer :: nval
  integer :: idum1
  integer :: idum2
  integer :: imeth
  double precision :: delt
  double precision :: pertim
  double precision :: totim
  character(len=16) :: srcmodelname
  character(len=16) :: srcpackagename
  integer :: ndat
  character(len=16), dimension(:), allocatable :: auxtxt
  integer :: nlist
  integer, allocatable, dimension(:) :: ia
  integer, allocatable, dimension(:) :: ja
  double precision, dimension(:), allocatable :: flowja
  integer, dimension(:), allocatable :: nodesrc
  integer, dimension(:), allocatable :: nodedst
  double precision, dimension(:, :), allocatable :: flowdata
  character(len=16) :: dstmodelname
  character(len=16) :: dstpackagename
  
  contains
  
  subroutine budgetdata_init(iu, iout, ncrbud)
! ******************************************************************************
! budgetdata_init
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer, intent(in) :: iu
    integer, intent(in) :: iout
    integer, intent(out) :: ncrbud
    ! -- local
    integer :: icount, kstp_last, kper_last
    logical :: success
! ------------------------------------------------------------------------------
    inunit = iu
    icount = 0
    ncrbud = 0
    !
    ! -- Read the first budget data record to set kstp_last, kstp_last
    call budgetdata_read(success)
    kstp_last = kstp
    kper_last = kper
    rewind(inunit)
    !
    ! -- Determine number of budget terms within a time step
    write(iout, '(a)') 'Reading budget file to determine number of terms per time step.'
    icount = 1
    do
      call budgetdata_read(success, iout)
      if (.not. success) exit
      if (kstp_last /= kstp .or. kper_last /= kper) exit
      icount = icount + 1
      nbudterms = nbudterms + 1
      if (trim(adjustl(budtxt)) == 'FLOW-JA-FACE' .and. &
          srcmodelname == dstmodelname) then
        if(allocated(nodesrc)) ncrbud = maxval(nodesrc)
      endif
    enddo
    rewind(inunit)
    write(iout, '(a, i0, a)') 'Detected ', nbudterms, ' unique flow terms in budget file.'
    !
    ! -- return
    return
  end subroutine budgetdata_init
  
  subroutine budgetdata_read(success, iout_opt)
! ******************************************************************************
! budgetdata_read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    logical, intent(out) :: success
    integer, intent(in), optional :: iout_opt
    ! -- local
    integer :: i, n, iostat, iout
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    if (present(iout_opt)) then
      iout = iout_opt
    else
      iout = 0
    endif
    !
    kstp = 0
    kper = 0
    budtxt = ''
    nval = 0
    idum1 = 0
    idum2 = 0
    srcmodelname = ''
    srcpackagename = ''
    dstmodelname = ''
    dstpackagename = ''
     
    success = .true.
    read(inunit, iostat=iostat) kstp, kper, budtxt, nval, idum1, idum2
    if (iostat /= 0) then
      success = .false.
      return
    endif
    read(inunit) imeth, delt, pertim, totim
    if(imeth == 1) then
      if (trim(adjustl(budtxt)) == 'FLOW-JA-FACE') then
        if(allocated(flowja)) deallocate(flowja)
        allocate(flowja(nval))
        read(inunit) flowja
        hasimeth1flowja = .true.
      else
        nval = nval * idum1 * abs(idum2)
        if(allocated(flowdata)) deallocate(flowdata)
        allocate(flowdata(1, nval))
        if(allocated(nodesrc)) deallocate(nodesrc)
        allocate(nodesrc(nval))
        read(inunit) flowdata
        do i = 1, nval
          nodesrc(i) = i
        enddo
      endif
    elseif (imeth == 6) then
      ! -- method code 6
      read(inunit) srcmodelname
      read(inunit) srcpackagename
      read(inunit) dstmodelname
      read(inunit) dstpackagename
      read(inunit) ndat
      if(allocated(auxtxt)) deallocate(auxtxt)
      allocate(auxtxt(ndat-1))
      read(inunit) auxtxt
      read(inunit) nlist
      if(allocated(nodesrc)) deallocate(nodesrc)
      allocate(nodesrc(nlist))
      if(allocated(nodedst)) deallocate(nodedst)
      allocate(nodedst(nlist))
      if(allocated(flowdata)) deallocate(flowdata)
      allocate(flowdata(ndat, nlist))
      read(inunit) (nodesrc(n), nodedst(n), (flowdata(i,n), i = 1, ndat), n = 1, nlist)
    else
      write(errmsg, '(a, a)') 'ERROR READING: ', trim(budtxt)
      call store_error(errmsg)
      write(errmsg, '(a, i0)') 'INVALID METHOD CODE DETECTED: ', imeth
      call store_error(errmsg)
      call store_error_unit(inunit)
      call ustop()
    endif
    if (iout > 0) then
      write(iout, '(1pg15.6, a, 1x, a)') totim, budtxt, dstpackagename
    endif
    !
    ! -- return
    return
  end subroutine budgetdata_read
  
  subroutine budgetdata_finalize()
! ******************************************************************************
! budgetdata_finalize
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
    close(inunit)
    if(allocated(auxtxt)) deallocate(auxtxt)
    if(allocated(ia)) deallocate(ia)
    if(allocated(ja)) deallocate(ja)
    if(allocated(flowja)) deallocate(flowja)
    if(allocated(nodesrc)) deallocate(nodesrc)
    if(allocated(nodedst)) deallocate(nodedst)
    if(allocated(flowdata)) deallocate(flowdata)
    !
    ! -- return
    return
  end subroutine budgetdata_finalize
  
end module BudgetDataModule

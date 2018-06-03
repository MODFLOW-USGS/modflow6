module HfbPackageWriterModule

  use ConstantsModule, only: MAXCHARLEN
  use ConstantsPHMFModule, only: FCINPUT, LENCTYPE
  use FileTypeModule, only: FileType
  use GLOBAL, only: IOUT, iunit, NCOL, NROW, NLAY, NPER, IBOUND, STRT
  use GlobalVariablesModule, only: masteridomain, verbose, echo
  use GWFHFBMODULE, only: NHFB, HFB, MXHFB, HFB, IPRHFB, IHFBPB
  use HfBSubsNwt, only: GWF2HFB7AR
  use LineListModule, only: LineListType
  use ListModule, only: ListType
  use PackageWriterModule, only: PackageWriterType
  
  !      HFB(1,II) = K
  !      HFB(2,II) = I1
  !      HFB(3,II) = J1
  !      HFB(4,II) = I2
  !      HFB(5,II) = J2
  !      HFB(6,II) = FACTOR*SFAC
  !      HFB(7,II) = 0.0

  private
  public :: HfbPackageWriterType

  type, extends(PackageWriterType) :: HfbPackageWriterType
    ! This class is for handling horizontal flow (HFB) boundaries. 
  contains
    procedure, public :: InitializeFile => initialize_hfb
    procedure, public :: MyType
    procedure, public :: ProcessAllocate
    procedure, public :: ProcessStressLoop
    procedure, public :: WriteFile
    procedure, public :: WriteOptions
    procedure, public :: WriteDimensions
    procedure, public :: WriteStressPeriodListData
  end type HfbPackageWriterType

contains

  subroutine initialize_hfb(this, fname, ftype, pkgname)
    implicit none
    ! dummy
    class(HfbPackageWriterType), intent(inout) :: this
    character(len=*), intent(in) :: fname
    character(len=*), intent(in) :: ftype
    character(len=*), intent(in), optional :: pkgname
    !
    ! Invoke superclass initializer
    if (present(pkgname)) then
      call this%FileWriterType%InitializeFile(fname, ftype, pkgname)
    else
      call this%FileWriterType%InitializeFile(fname, ftype, this%PackageName)
    endif
    !
    ! Initialize options
    this%PrintInput = .true.
    this%PackageName = ''
    this%PkgType = 'HFB'
    this%DefaultBudgetText = '[no entry]'
    !
    return
  end subroutine initialize_hfb

  subroutine ProcessAllocate(this, igrid)
    implicit none
    ! dummy
    class(HfbPackageWriterType) :: this
    integer, intent(in) :: igrid
    ! local
    character(len=MAXCHARLEN) :: fname
    character(len=100) :: ctemp
    ! format
    !
    call this%AllocatePointers()
    this%NStressDim = 2
    this%NAux = 0
    this%Active = .true.
    ! 
    this%IuOrig = IUNIT(21)        ! Specific to HFB package
    this%fileobj%FCode = FCINPUT
    !
    this%fileobj%FType = 'HFB6'
    this%PkgType = 'HFB'
!    this%PackageName = 'HFB'
    fname = trim(this%ModelBasename) // '.hfb'
    call this%InitializeFile(fname, this%fileobj%FType, this%PackageName)
    !
    if (this%IuOrig > 0) then
      !
      ! NVALP1 is rlist index for element following minimum required data.
      this%nvalp1 = 7
      !
      ! Assign package-specific pointers
      this%rlist => null()  ! HFB
      this%ipr => IPRHFB
      allocate(this%ICbc)
      this%ICbc = 0
      this%NBndPeriod => NHFB
      this%Aux => null()
      this%IPB => IHFBPB
      this%MaxActiveBnd = this%IPB - 1
      this%NBNDVL => null()
      this%NAux = 0  ! this%NBNDVL - this%nvalp1 + 1
      this%nstop = this%nvalp1 - 1 + this%NAux
    endif
    !
    ! define format for printing boundary data
    write(ctemp,'(i0)')this%NAux + this%NStressDim
    this%fmat = '(3(2x,i0),' // trim(ctemp) // '(2x,g15.8))'
    !
    return
  end subroutine ProcessAllocate

  subroutine ProcessStressLoop(this, igrid)
    implicit none
    ! dummy
    class(HfbPackageWriterType) :: this
    integer, intent(in) :: igrid
    ! local
    integer :: kper
    logical :: currentA
    ! formats
    10 format(/,'Processing HFB package for stress period ',i0)
    !
    ! Write Options and Dimensions blocks
    call this%WriteOptions()
    call this%WriteDimensions()
    !
    ! Initially, current block is BlockA; alternate each stress period
    this%CurrentBlock => this%BlockA
    this%PreviousBlock => this%BlockB
    currentA = .true.
    !
    kper = 1
    write(iout,10) kper
    ! Write MF6 input for current stress period to a LineList
    call this%CurrentBlock%Clear(.true.)
    call this%WriteStressPeriodListData(this%CurrentBlock)
    ! Write block to MF6 input file if needed
    call this%WriteBlockIfNeeded(kper)
    !
    ! switcheroo
    if (currentA) then
      this%CurrentBlock => this%BlockB
      this%PreviousBlock => this%BlockA
      currentA = .false.
    else
      this%CurrentBlock => this%BlockA
      this%PreviousBlock => this%BlockB
      currentA = .true.
    endif
    !enddo
    !
    return
  end subroutine ProcessStressLoop

  subroutine WriteFile(this, igrid)
    implicit none
    ! dummy
    class(HfbPackageWriterType), intent(inout) :: this
    integer, intent(in) :: igrid
    ! local
    ! formats
    20 format(2x,i0,2x,i0,2x,i0,2x,g15.8)
    60 format(a)
    70 format(/,a)
    !
    if (.not. this%Active) return
    !
    call this%ProcessAllocate(igrid)
    call this%WriteOptions()
    call this%WriteDimensions()
    call this%ProcessStressLoop(igrid)
    call this%CloseFile()
    !
    return
  end subroutine WriteFile

  subroutine WriteOptions(this)
    implicit none
    ! dummy
    class(HfbPackageWriterType) :: this
    ! local
    integer :: iu
    ! formats
    30 format()
    40 format(2x,a)
    50 format(2x,a,2x,a)
    60 format(a)
    !
    iu = this%fileobj%IUnit
    write(iu,30)
    write(iu,60)'BEGIN Options'
    !
    ! Print_Input
    if (this%PrintInput) write(iu,40)'PRINT_INPUT'
    !
    write(iu,60)'END Options'
    !
    return
  end subroutine WriteOptions

  subroutine WriteDimensions(this)
    implicit none
    ! dummy
    class(HfbPackageWriterType) :: this
    ! local
    integer :: iu, n
    ! formats
    30 format('')
    50 format(2x,a,2x,i0)
    60 format(a)
    !
    iu = this%fileobj%IUnit
    write(iu,30)
    write(iu,60)'BEGIN Dimensions'
    n = this%MaxActiveBnd
    write(iu,50)'MAXHFB',n
    write(iu,60)'END Dimensions'
    !
    return
  end subroutine WriteDimensions

  subroutine WriteStressPeriodListData(this, lineList)
    ! Override if needed
    implicit none
    ! dummy
    class(HfbPackageWriterType) :: this
    type(LineListType), pointer :: lineList
    ! local
    double precision :: hydchr
    integer :: irow1, irow2, ii, iu, jcol1, jcol2, klay, kold, n
    character(len=MAXCHARLEN) :: line, msg
    ! format
20  format(6(2x,i0),2x,g15.8)
    10 format(a,' boundary removed at (',i0,',',i0,',',i0,')')
    !
    ! Write data to define HFBs
    iu = this%fileobj%IUnit
    n = NHFB
    do ii=1,n
      ! Ned todo: Account for layer shift due to quasi-3d confining unit(s)
      kold = nint(HFB(1,ii))
      klay = this%Layptr(kold)
      irow1 = nint(hfb(2,ii))
      jcol1 = nint(hfb(3,ii))
      irow2 = nint(hfb(4,ii))
      jcol2 = nint(hfb(5,ii))
      if (associated(masteridomain)) then
        if (masteridomain(jcol1,irow1,klay)==0) then
          if (verbose) then
            msg = ''
            write(msg,10)trim(this%PkgType),klay,irow1,jcol1
            write(*,*)trim(msg)
          endif
          cycle
        endif
        if (masteridomain(jcol2,irow2,klay)==0) then
          if (verbose) then
            msg = ''
            write(msg,10)trim(this%PkgType),klay,irow2,jcol2
            write(*,*)trim(msg)
          endif
          cycle
        endif
      endif
      hydchr = hfb(6,ii)
      write(line,20)klay,irow1,jcol1,klay,irow2,jcol2,hydchr
      call lineList%AddLine(line)
    enddo
    !
    return
  end subroutine WriteStressPeriodListData

  function MyType(this) result (ctype)
    ! dummy 
    class(HfbPackageWriterType) :: this
    character(len=LENCTYPE) :: ctype
    !
    ctype = 'HfbPackageWriterType'
    !
    return
  end function MyType
  
end module HfbPackageWriterModule

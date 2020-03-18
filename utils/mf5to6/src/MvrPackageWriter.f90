module MvrPackageWriterModule

  use ConstantsModule, only: LENPACKAGENAME, MAXCHARLEN
  use ConstantsPHMFModule, only: FCINPUT, LENCTYPE
  use LineListModule, only: LineListType
  use ListModule, only: ListType
  use MoverModule, only: MoverType, AddMoverToList, GetMoverFromList
  use PackageWriterModule, only: PackageWriterType
  use SimListVariablesModule, only: SimMovers

  implicit none

  private
  public :: MvrPackageWriterType

  type, extends(PackageWriterType) :: MvrPackageWriterType
    integer :: MaxMvr = 0
    integer :: MaxPackages = 0
    logical :: WriteModelNames = .false.
    type(LineListType) :: PackageNames
    type(ListType), pointer :: Movers => null()
  contains
    procedure :: AddMover
    procedure :: GatherPackages
    procedure :: GetMover
    procedure :: InitializeMvrWriter
    procedure :: MyType
    procedure :: ProcessAllocate
    procedure :: ProcessStressLoop
    procedure :: WriteDimensions
    procedure :: WriteMvrFile
    procedure :: WriteOptions
    procedure :: WritePackages
    procedure :: WritePeriod
  end type MvrPackageWriterType

contains

  subroutine InitializeMvrWriter(this, moverList)
    ! dummy
    class(MvrPackageWriterType) :: this
    type(ListType), pointer, intent(in) :: moverList
    !
    call this%AllocatePointers()
    this%Movers => moverList
    call this%fileobj%Initialize()
    !
    return
  end subroutine InitializeMvrWriter

  subroutine WriteMvrFile(this)
    implicit none
    ! dummy
    class(MvrPackageWriterType) :: this
    !
    call this%GatherPackages()
    call this%WriteOptions()
    call this%WriteDimensions()
    call this%WritePackages()
    call this%WritePeriod()
    close(this%fileobj%IUnit)
    !
    return
  end subroutine WriteMvrFile

  subroutine WriteOptions(this)
    implicit none
    ! dummy
    class(MvrPackageWriterType) :: this
    ! local
    integer :: iu
    ! formats
    1 format()
    10 format(a)
    20 format(2x,a)
    30 format(2x,a,2x,g0)
    !
    iu = this%fileobj%IUnit
    write(iu,1)
    write(iu,10)'BEGIN Options'
    write(iu,20)'PRINT_INPUT'
    write(iu,20)'PRINT_FLOWS'
    if(this%WriteModelNames) write(iu,20) 'MODELNAMES'
    write(iu,10)'END Options'
    !
    return
  end subroutine WriteOptions

  subroutine WriteDimensions(this)
    implicit none
    ! dummy
    class(MvrPackageWriterType) :: this
    ! local
    integer :: iu
    ! formats
    1 format()
    10 format(a)
    30 format(2x,a,2x,i0)
    !
    iu = this%fileobj%IUnit
    write(iu,1)
    write(iu,10)'BEGIN Dimensions'
    write(iu,30)'MAXMVR', this%MaxMvr
    write(iu,30)'MAXPACKAGES', this%MaxPackages
    write(iu,10)'END Dimensions'
    !
    return
  end subroutine WriteDimensions

  subroutine WritePackages(this)
    implicit none
    ! dummy
    class(MvrPackageWriterType) :: this
    ! local
    integer :: i, iu
    character(len=50) :: pkgname
    ! formats
    1 format()
    10 format(a)
    20 format(2x,a,2x,a)
    !
    iu = this%fileobj%IUnit
    write(iu,1)
    write(iu,10)'BEGIN Packages'
    do i=1,this%MaxPackages
      call this%PackageNames%GetLine(i, pkgname)
      write(iu,20)trim(pkgname)
    enddo
    write(iu,10)'END Packages'
    !
    return
  end subroutine WritePackages

  subroutine WritePeriod(this)
    implicit none
    ! dummy
    class(MvrPackageWriterType) :: this
    ! local
    integer :: i, iu
    type(MoverType), pointer :: mvr
    ! formats
    1 format()
    10 format(a)
    20 format(2x,a,2x,a,2x,i0,2x,a,2x,a,2x,i0,2x,a,2x,g0)
    30 format(2x,a,2x,i0,2x,a,2x,i0,2x,a,2x,g0)
    !
    iu = this%fileobj%IUnit
    write(iu,1)
    write(iu,10)'BEGIN Period 1'
    do i=1,this%MaxMvr
      mvr => GetMoverFromList(this%Movers, i)
      if (this%WriteModelNames) then
        write(iu,20)trim(mvr%ProvModelName), &
                    trim(mvr%ProvPkgName), &
                    mvr%IdProvider, &
                    trim(mvr%RecModelName), &
                    trim(mvr%RecPkgName), &
                    mvr%IdReceiver, &
                    trim(mvr%MvrType), &
                    mvr%Valu
      else
        write(iu,30)trim(mvr%ProvPkgName), &
                    mvr%IdProvider, &
                    trim(mvr%RecPkgName), &
                    mvr%IdReceiver, &
                    trim(mvr%MvrType), &
                    mvr%Valu
      endif
    enddo
    write(iu,10)'END Period'
    !
    !
    return
  end subroutine WritePeriod

  subroutine ProcessAllocate(this, igrid)
    implicit none
    ! dummy
    class(MvrPackageWriterType) :: this
    integer, intent(in) :: igrid
    ! local
    character(len=MAXCHARLEN) :: fname
    ! format
    !
    call this%AllocatePointers()
    this%DefaultBudgetText = 'MOVERS'
    this%Active = .true.
    this%IuOrig = 0
    this%fileobj%FCode = FCINPUT
    !
    this%fileobj%FType = 'MVR6'
    this%PkgType = 'MVR'
    fname = trim(this%ModelBasename) // '.mvr'
    call this%FileWriterType%InitializeFile(fname, this%fileobj%FType, &
                                            this%PackageName)
    !
    return
  end subroutine ProcessAllocate

  subroutine ProcessStressLoop(this, igrid)
    implicit none
    ! dummy
    class(MvrPackageWriterType) :: this
    integer, intent(in) :: igrid
    !
    return
  end subroutine ProcessStressLoop

  subroutine AddMover(this, mover)
    implicit none
    ! dummy
    class(MvrPackageWriterType) :: this
    type(MoverType), pointer :: mover
    !
    if (mover%ProvModelName == mover%RecModelName) then
      call AddMoverToList(this%Movers, mover)
    else
      call AddMoverToList(SimMovers, mover)
    endif
    !
    return
  end subroutine AddMover

  function GetMover(this, idx) result (res)
    implicit none
    ! dummy
    class(MvrPackageWriterType) :: this
    integer, intent(in) :: idx
    type(MoverType), pointer :: res
    !
    res => GetMoverFromList(this%Movers, idx)
    !
    return
  end function GetMover

  subroutine GatherPackages(this)
    implicit none
    ! dummy
    class(MvrPackageWriterType) :: this
    ! local
    integer :: i
    character(len=50) :: pkgname1, pkgname2
    type(MoverType), pointer :: mover
    logical :: caseSensitive
    !
    call this%PackageNames%InitializeLineList()
    this%MaxMvr = this%Movers%Count()
    !
    caseSensitive = .false.
    do i=1,this%MaxMvr
      mover => this%GetMover(i)
      if (this%WriteModelNames) then
        pkgname1 = trim(mover%ProvModelName) // '  ' // trim(mover%ProvPkgName)
      else
        pkgname1 = trim(mover%ProvPkgName)
      endif
      if (.not. this%PackageNames%Includes(pkgname1, caseSensitive)) then
        call this%PackageNames%AddLine(pkgname1)
      endif
      if (this%WriteModelNames) then
        pkgname2 = trim(mover%RecModelName) // '  ' // trim(mover%RecPkgName)
      else
        pkgname2 = trim(mover%RecPkgName)
      endif
      if (.not. this%PackageNames%Includes(pkgname2, caseSensitive)) then
        call this%PackageNames%AddLine(pkgname2)
      endif
    enddo
    !
    this%MaxPackages = this%PackageNames%CountLines()
    !
    return
  end subroutine GatherPackages

  function MyType(this) result (ctype)
    ! dummy 
    class(MvrPackageWriterType) :: this
    character(len=LENCTYPE) :: ctype
    !
    ctype = 'MvrPackageWriterType'
    !
    return
  end function MyType

end module MvrPackageWriterModule

module FileTypeModule

  use ConstantsModule, only: LENFTYPE, LENPACKAGENAME, &
                             LINELENGTH
  use ConstantsPHMFModule, only: FCUNKNOWN, LENOBSNAMENEW
  use PRECUTLSMOD, only: HEADPRECISION

  private
  public :: FileType, ConstructFileType, CastAsFileType, statuses

  type :: FileType
    ! FType is file type, e.g. RIV6, CHD6, etc.
    ! FCode is file code, as defined in ConstantsModule
    character(len=LENFTYPE), pointer,   public :: FType => null()
    character(len=LINELENGTH), pointer, public :: FName => null()
    character(len=LENPACKAGENAME),      public :: PkgName = ''
    integer, pointer,                   public :: IUnit => null()
    integer, pointer,                   public :: FCode => null()
    integer, pointer,                   public :: IPrec => null()
    ! For PostObsMF
    logical,                            public :: binary = .false.
    integer,                            public :: ObsFmt = 0  ! 1 for SINGLE, 2 for CONTINUOUS
    integer,                            public :: lenobsnamebsv = 12
    character(len=LENOBSNAMENEW), allocatable, dimension(:),   public :: obsnames
    double precision,             allocatable, dimension(:,:), public :: simvals
    double precision,             allocatable, dimension(:),   public :: times
  contains
    procedure, public :: Initialize
  end type FileType

  ! Statuses correspond to file codes defined in ConstantsModule
  character(len=7), dimension(0:6) :: statuses
  data statuses/'UNKNOWN', & ! FCUNKNOWN
                'OLD    ', & ! FCINPUT
                'OLD    ', & ! FCDATAIN
                'OLD    ', & ! FCDATABIN
                'REPLACE', & ! FCOUTPUT
                'REPLACE', & ! FCDATAOUT
                'REPLACE'/   ! FCDATABOUT

contains

  subroutine Initialize(this)
    ! dummy
    class(FileType) :: this
    !
    ! Allocate pointers
    allocate(this%FType)
    allocate(this%FName)
    allocate(this%IUnit)
    allocate(this%FCode)
    allocate(this%IPrec)
    !
    ! Initialize variables
    this%FType = ''
    this%FName = ''
    this%IUnit = 0
    this%FCode = FCUNKNOWN
    this%IPrec = 0  ! Precision unknown
    !
    return
  end subroutine Initialize

  subroutine ConstructFileType(newFile)
    implicit none
    type(FileType), pointer, intent(inout) :: newFile
    !
    allocate(newFile)
    call newFile%Initialize()
    return
  end subroutine ConstructFileType

  function CastAsFileType(obj) result(res)
    ! Cast an unlimited polymorphic object as class(FileType)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(FileType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (FileType)
      res => obj
    end select
    return
  end function CastAsFileType

end module FileTypeModule

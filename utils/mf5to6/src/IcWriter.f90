module IcWriterModule

  use FileWriterModule, only: FileWriterType
  use GLOBAL, only: NCOL, NROW, NLAY

  private
  public :: IcWriterType

  type, extends(FileWriterType) :: IcWriterType
  contains
    procedure, public :: InitializeFile => initialize_ic
    procedure, public :: WriteOptions
  end type IcWriterType

contains

  subroutine initialize_ic(this, fname, ftype, pkgname)
    implicit none
    ! dummy
    class(IcWriterType), intent(inout) :: this
    character(len=*), intent(in) :: fname
    character(len=*), intent(in) :: ftype
    character(len=*), intent(in), optional :: pkgname
    !
    ! Invoke superclass initializer
    if (present(pkgname)) then
      call this%FileWriterType%InitializeFile(fname, ftype, pkgname)
    else
      call this%FileWriterType%InitializeFile(fname, ftype)
    endif
    !
    ! IC is always active
    this%Active = .true.
    !
    return
  end subroutine initialize_ic

  subroutine WriteOptions(this)
    implicit none
    ! dummy
    class(IcWriterType), intent(inout) :: this
    ! local
    integer :: iu
    ! formats
    5  format()
    10 format('BEGIN Options')
    20 format('END Options')
    !
    iu = this%fileobj%IUnit
    ! No known options yet, so just write begin & end
    write(iu,5)
    write(iu,10)
    write(iu,20)
    write(iu,5)
    !
    return
  end subroutine WriteOptions

end module IcWriterModule


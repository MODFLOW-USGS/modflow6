module MemoryTypeModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENORIGIN, LENTIMESERIESNAME, LENVARNAME
  implicit none
  private
  public :: MemoryTSType, MemoryType

  type :: MemoryTSType
    character (len=LENTIMESERIESNAME), pointer :: name => null()
    real(DP), pointer :: value => null()
  end type MemoryTSType
  
  
  type MemoryType
    character(len=LENVARNAME)                              :: name                   !name of the array
    character(len=LENORIGIN)                               :: origin                 !name of origin
    character(len=50)                                      :: memtype                !type (INTEGER or DOUBLE)
    integer(I4B)                                           :: id                     !id, not used
    integer(I4B)                                           :: nrealloc = 0           !number of times reallocated
    integer(I4B)                                           :: isize                  !size of the array
    logical, pointer                                       :: logicalsclr => null()  !pointer to the logical
    integer(I4B), pointer                                  :: intsclr     => null()  !pointer to the integer
    real(DP), pointer                                      :: dblsclr     => null()  !pointer to the double
    integer(I4B), dimension(:), pointer, contiguous        :: aint1d      => null()  !pointer to 1d integer array
    integer(I4B), dimension(:, :), pointer, contiguous     :: aint2d      => null()  !pointer to 2d integer array
    real(DP), dimension(:), pointer, contiguous            :: adbl1d      => null()  !pointer to 1d double array
    real(DP), dimension(:, :), pointer, contiguous         :: adbl2d      => null()  !pointer to 2d double array
    type (MemoryTSType), dimension(:), pointer, contiguous :: ats1d       => null()  !pointer to a time series array
  contains
    procedure :: table_entry
    procedure :: mt_associated
  end type
  
  contains
  
  subroutine table_entry(this, msg)
    class(MemoryType) :: this
    character(len=*), intent(inout) :: msg
    character(len=*), parameter :: fmt = "(1x, a40, a20, a20, i10, i10, a2)"
    character(len=1) :: dastr
    !
    ! -- Create the msg table entry
    dastr = ''
    if (this%mt_associated() .and. this%isize > 0) dastr='*'
    write(msg, fmt) this%origin, this%name, this%memtype, this%isize,          &
                    this%nrealloc, dastr
  end subroutine table_entry

  function mt_associated(this) result(al)
    class(MemoryType) :: this
    logical :: al
    al = .false.
    if(associated(this%logicalsclr)) al = .true.
    if(associated(this%intsclr)) al = .true.
    if(associated(this%dblsclr)) al = .true.
    if(associated(this%aint1d)) al = .true.
    if(associated(this%aint2d)) al = .true.
    if(associated(this%adbl1d)) al = .true.
    if(associated(this%adbl2d)) al = .true. 
    if(associated(this%ats1d)) al = .true. 
  end function mt_associated
  
end module MemoryTypeModule
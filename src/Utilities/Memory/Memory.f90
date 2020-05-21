module MemoryTypeModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENORIGIN, LENTIMESERIESNAME, LENVARNAME,           &
                             MAXMEMRANK, LENMEMTYPE,                             &
                             TABSTRING, TABINTEGER,                              &
                             TABCENTER, TABLEFT, TABRIGHT
  use InputOutputModule, only: UWWORD
  implicit none
  private
  public :: MemoryType
 
  type MemoryType
    character(len=LENVARNAME)                              :: name                   !name of the array
    character(len=LENORIGIN)                               :: origin                 !name of origin
    character(len=LENMEMTYPE)                              :: memtype                !type (INTEGER or DOUBLE)
    integer(I4B)                                           :: id                     !id, not used
    integer(I4B)                                           :: nrealloc = 0           !number of times reallocated
    integer(I4B)                                           :: isize                  !size of the array
    logical                                                :: master = .true.        !master copy, others point to this one
    logical, pointer                                       :: logicalsclr => null()  !pointer to the logical
    integer(I4B), pointer                                  :: intsclr     => null()  !pointer to the integer
    real(DP), pointer                                      :: dblsclr     => null()  !pointer to the double
    integer(I4B), dimension(:), pointer, contiguous        :: aint1d      => null()  !pointer to 1d integer array
    integer(I4B), dimension(:, :), pointer, contiguous     :: aint2d      => null()  !pointer to 2d integer array
    integer(I4B), dimension(:, :, :), pointer, contiguous  :: aint3d      => null()  !pointer to 3d integer array
    real(DP), dimension(:), pointer, contiguous            :: adbl1d      => null()  !pointer to 1d double array
    real(DP), dimension(:, :), pointer, contiguous         :: adbl2d      => null()  !pointer to 2d double array
    real(DP), dimension(:, :, :), pointer, contiguous      :: adbl3d      => null()  !pointer to 3d double array
  contains
    procedure :: table_entry
    procedure :: mt_associated
  end type
  
  contains
  
  subroutine table_entry(this, line)
    ! -- dummy
    class(MemoryType) :: this
    character(len=*), intent(inout) :: line
    ! -- local
    character(len=16) :: cmem
    character(len=10) :: cnalloc
    character(len=5) :: cptr
    character(len=5) :: dastr
    integer(I4B) :: ipos
    integer(I4B) :: iloc
    integer(I4B) :: ival
    real(DP) :: rval
    ! -- formats
    !
    ! -- determine memory type
    ipos = index(this%memtype, ' (')
    if (ipos < 1) then
      ipos = 16
    else
      ipos = min(16,ipos-1)
    end if
    cmem = this%memtype(1:ipos)
    !
    ! -- set reallocation string
    cnalloc = '--'
    if (this%nrealloc > 0) then
      write(cnalloc, '(i0)') this%nrealloc
    end if
    !
    ! -- Set pointer and deallocation string
    cptr = '--'
    if (.not. this%master) then
      cptr = 'TRUE'
    end if
    dastr = '--'
    if (this%mt_associated() .and. this%isize > 0) then
      dastr='FALSE'
    end if
    iloc = 1
    line = ''
    call UWWORD(line, iloc, LENORIGIN, TABSTRING, this%origin, ival, rval,       &
                ALIGNMENT=TABLEFT, SEP=' ')
    call UWWORD(line, iloc, LENVARNAME, TABSTRING, this%name, ival, rval,        &
                ALIGNMENT=TABLEFT, SEP=' ')
    call UWWORD(line, iloc, 16, TABSTRING, cmem, ival, rval,                     &
                ALIGNMENT=TABLEFT, SEP=' ')
    call UWWORD(line, iloc, 20, TABINTEGER, 'SIZE', this%isize, rval,            &
                ALIGNMENT=TABRIGHT, SEP=' ')
    call UWWORD(line, iloc, 10, TABSTRING, cnalloc, ival, rval,                  &
                ALIGNMENT=TABCENTER, SEP=' ')
    call UWWORD(line, iloc, 10, TABSTRING, cptr, ival, rval,                     &
                ALIGNMENT=TABCENTER, SEP=' ')    
    call UWWORD(line, iloc, 10, TABSTRING, dastr, ival, rval,                    &
                ALIGNMENT=TABCENTER)    
    !
    ! -- return
    return
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
    if(associated(this%aint3d)) al = .true.
    if(associated(this%adbl1d)) al = .true.
    if(associated(this%adbl2d)) al = .true. 
    if(associated(this%adbl3d)) al = .true. 
  end function mt_associated
  
end module MemoryTypeModule
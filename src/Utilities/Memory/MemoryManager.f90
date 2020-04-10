module MemoryManagerModule

  use KindModule,             only: DP, I4B, I8B
  use ConstantsModule,        only: DZERO, LENORIGIN, LENVARNAME, LINELENGTH,    &
                                    LENMEMTYPE
  use SimModule,              only: store_error, ustop
  use MemoryTypeModule,       only: MemoryTSType, MemoryType
  use MemoryListModule,       only: MemoryListType
  
  implicit none
  private
  public :: mem_allocate
  public :: mem_reallocate
  public :: mem_setptr
  public :: mem_copyptr
  public :: mem_reassignptr
  public :: mem_deallocate
  public :: mem_usage
  public :: mem_da
  public :: mem_set_print_option
  
  public :: get_mem_type
  public :: get_mem_rank
  public :: get_mem_size
  public :: get_mem_shape
  public :: get_isize
  public :: copy_dbl1d
  
  type(MemoryListType) :: memorylist
  integer(I8B) :: nvalues_alogical = 0
  integer(I8B) :: nvalues_achr = 0
  integer(I8B) :: nvalues_aint = 0
  integer(I8B) :: nvalues_adbl = 0
  integer(I8B) :: nvalues_ats = 0
  integer(I4B) :: iprmem = 0

  interface mem_allocate
    module procedure allocate_logical,                                         &
                     allocate_int, allocate_int1d, allocate_int2d,             &
                     allocate_int3d,                                           &
                     allocate_dbl, allocate_dbl1d, allocate_dbl2d,             &
                     allocate_dbl3d,                                           &
                     allocate_ts1d
  end interface mem_allocate
  
  interface mem_reallocate
    module procedure reallocate_int1d, reallocate_int2d, reallocate_dbl1d,     &
                     reallocate_dbl2d
  end interface mem_reallocate
  
  interface mem_setptr
    module procedure setptr_logical,                                           &
                     setptr_int, setptr_int1d, setptr_int2d,                   &
                     setptr_dbl, setptr_dbl1d, setptr_dbl2d
  end interface mem_setptr
  
  interface mem_copyptr
    module procedure copyptr_int1d, copyptr_int2d, &
                     copyptr_dbl1d, copyptr_dbl2d
  end interface mem_copyptr

  interface mem_reassignptr
    module procedure reassignptr_int1d, reassignptr_int2d, &
                     reassignptr_dbl1d, reassignptr_dbl2d
  end interface mem_reassignptr

  interface mem_deallocate
    module procedure deallocate_logical,                                         &
                     deallocate_int, deallocate_int1d, deallocate_int2d,         &
                     deallocate_int3d,                                           &
                     deallocate_dbl, deallocate_dbl1d, deallocate_dbl2d,         &
                     deallocate_dbl3d,                                           &
                     deallocate_ts1d
  end interface mem_deallocate

  contains
  
  subroutine get_mem_type(name, origin, var_type)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=LENMEMTYPE), intent(out) :: var_type    
    ! local
    type(MemoryType), pointer :: mt
    logical :: found
        
    mt => null()
    var_type = 'UNKNOWN'
    call get_from_memorylist(name, origin, mt, found)
    if (found) then
      var_type = mt%memtype
    end if
    
  end subroutine get_mem_type
  
  subroutine get_mem_rank(name, origin, rank)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(out)    :: rank    
    ! local
    type(MemoryType), pointer :: mt
    logical :: found
        
    mt => null()
    rank = -1
    call get_from_memorylist(name, origin, mt, found)
    if (found) then
      if(associated(mt%logicalsclr)) rank = 0
      if(associated(mt%intsclr)) rank = 0
      if(associated(mt%dblsclr)) rank = 0
      if(associated(mt%aint1d)) rank = 1
      if(associated(mt%aint2d)) rank = 2
      if(associated(mt%aint3d)) rank = 3
      if(associated(mt%adbl1d)) rank = 1
      if(associated(mt%adbl2d)) rank = 2 
      if(associated(mt%adbl3d)) rank = 3 
      if(associated(mt%ats1d)) rank = 1      
    end if    
    
  end subroutine get_mem_rank 
    
  subroutine get_mem_size(name, origin, size)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(out)    :: size
    ! local
    type(MemoryType), pointer :: mt
    logical :: found
        
    mt => null()
    call get_from_memorylist(name, origin, mt, found)
        
    size = -1
    if (found) then      
      select case(mt%memtype(1:index(mt%memtype,' ')))
      case ('INTEGER')
        size = 4
      case ('DOUBLE')
        size = 8
      end select 
    end if    
    
  end subroutine get_mem_size
  
  subroutine get_mem_shape(name, origin, mem_shape)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), dimension(:), intent(out) :: mem_shape
    ! local
    type(MemoryType), pointer :: mt
    logical :: found
    
    mt => null()
    call get_from_memorylist(name, origin, mt, found)
    if (found) then
      if(associated(mt%logicalsclr)) mem_shape = shape(mt%logicalsclr) 
      if(associated(mt%intsclr)) mem_shape = shape(mt%logicalsclr)
      if(associated(mt%dblsclr)) mem_shape = shape(mt%dblsclr)
      if(associated(mt%aint1d)) mem_shape = shape(mt%aint1d)
      if(associated(mt%aint2d)) mem_shape = shape(mt%aint2d)
      if(associated(mt%aint3d)) mem_shape = shape(mt%aint3d)
      if(associated(mt%adbl1d)) mem_shape = shape(mt%adbl1d)
      if(associated(mt%adbl2d)) mem_shape = shape(mt%adbl2d)
      if(associated(mt%adbl3d)) mem_shape = shape(mt%adbl3d)
      if(associated(mt%ats1d)) mem_shape = shape(mt%ats1d)
    else
      ! to communicate failure
      mem_shape(1) = -1
    end if    
    
  end subroutine get_mem_shape
  
  subroutine get_isize(name, origin, isize)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(out)    :: isize
    ! local
    type(MemoryType), pointer :: mt
    logical :: found
        
    mt => null()
    call get_from_memorylist(name, origin, mt, found)
    if (found) then
      isize = mt%isize
    else
      isize = -1
    end if
  end subroutine get_isize
  
  subroutine get_from_memorylist(name, origin, mt, found, check)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    type(MemoryType), pointer, intent(out) :: mt
    logical,intent(out) :: found
    logical, intent(in), optional :: check
    integer(I4B) :: ipos
    logical check_opt
    character(len=LINELENGTH) :: ermsg
    mt => null()
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        found = .true.
        exit
      endif
    enddo
    check_opt = .true.
    if (present(check)) check_opt = check
    if (check_opt) then
      if (.not. found) then
        ermsg = 'Programming error in memory manager. Variable ' // name // &
          ' in origin ' // origin // &
          ' cannot be assigned because it does not exist in memory manager. '
        call store_error(ermsg)
        call ustop()
      endif
    end if
    return
  end subroutine get_from_memorylist
  
  subroutine allocate_error(varname, origin, istat, errmsg, isize)
    use SimModule, only: store_error, ustop
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: origin
    integer(I4B), intent(in) :: istat
    character(len=*), intent(in) :: errmsg
    integer(I4B), intent(in) :: isize
    character(len=20) :: cint
    call store_error('Error trying to allocate memory.')
    call store_error('  Origin: ' // origin)
    call store_error('  Variable name: ' // varname)
    write(cint, '(i0)') isize
    call store_error('  Size: ' // cint)
    call store_error('  Error message: ' // errmsg)
    cint = ''
    write(cint, '(i0)') istat
    call store_error('  Status code: ' // cint)
    call ustop()
  end subroutine allocate_error
  
  subroutine check_varname(name)
    character(len=*), intent(in) :: name
    character(len=LINELENGTH) :: ermsg
    if(len(name) > LENVARNAME) then
      write(ermsg, '(*(G0))')                                                  &
        'Programming error in Memory Manager. Variable ', name, ' must be ',   &
        LENVARNAME, ' characters or less.'
      call store_error(ermsg)
      call ustop()
    endif
  end subroutine check_varname

  subroutine allocate_logical(logicalsclr, name, origin)
    logical, pointer, intent(inout) :: logicalsclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    allocate(logicalsclr, stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, 1)
    nvalues_alogical = nvalues_alogical + 1
    allocate(mt)
    mt%logicalsclr => logicalsclr
    mt%isize = 1
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a)") 'LOGICAL'
    call memorylist%add(mt)
  end subroutine allocate_logical

  subroutine allocate_int(intsclr, name, origin)
    integer(I4B), pointer, intent(inout) :: intsclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    call check_varname(name)
    allocate(intsclr, stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, 1)
    nvalues_aint = nvalues_aint + 1
    allocate(mt)
    mt%intsclr => intsclr
    mt%isize = 1
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a)") 'INTEGER'
    call memorylist%add(mt)
  end subroutine allocate_int
  
  subroutine allocate_int1d(aint, isize, name, origin)
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: aint
    integer(I4B), intent(in) :: isize
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    call check_varname(name)
    allocate(aint(isize), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    nvalues_aint = nvalues_aint + isize
    allocate(mt)
    mt%aint1d => aint
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,')')") 'INTEGER', isize
    call memorylist%add(mt)
  end subroutine allocate_int1d
  
  subroutine allocate_int2d(aint, ncol, nrow, name, origin)
    integer(I4B), dimension(:, :), pointer, contiguous, intent(inout) :: aint
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    integer(I4B) :: isize
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    call check_varname(name)
    isize = ncol * nrow
    allocate(aint(ncol, nrow), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    nvalues_aint = nvalues_aint + isize
    allocate(mt)
    mt%aint2d => aint
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,',',i0,')')") 'INTEGER', ncol, nrow
    call memorylist%add(mt)
  end subroutine allocate_int2d
    
  subroutine allocate_int3d(aint, ncol, nrow, nlay, name, origin)
    integer(I4B), dimension(:, :, :), pointer, contiguous, intent(inout) :: aint
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    integer(I4B), intent(in) :: nlay
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    integer(I4B) :: isize
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    call check_varname(name)
    isize = ncol * nrow * nlay
    allocate(aint(ncol, nrow, nlay), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    nvalues_aint = nvalues_aint + isize
    allocate(mt)
    mt%aint3d => aint
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,',',i0,',',i0,')')") 'INTEGER', ncol,          &
                                                       nrow, nlay
    call memorylist%add(mt)
  end subroutine allocate_int3d

  subroutine allocate_dbl(dblsclr, name, origin)
    real(DP), pointer, intent(inout) :: dblsclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    call check_varname(name)
    allocate(dblsclr, stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, 1)
    nvalues_aint = nvalues_aint + 1
    allocate(mt)
    mt%dblsclr => dblsclr
    mt%isize = 1
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a)") 'DOUBLE'
    call memorylist%add(mt)
  end subroutine allocate_dbl
  
  subroutine allocate_dbl1d(adbl, isize, name, origin)
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl
    integer(I4B), intent(in) :: isize
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    call check_varname(name)
    allocate(adbl(isize), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    nvalues_adbl = nvalues_adbl + isize
    allocate(mt)
    mt%adbl1d => adbl
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,')')") 'DOUBLE', isize
    call memorylist%add(mt)
  end subroutine allocate_dbl1d
  
  subroutine allocate_dbl2d(adbl, ncol, nrow, name, origin)
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: adbl
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    integer(I4B) :: isize
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    call check_varname(name)
    isize = ncol * nrow
    allocate(adbl(ncol, nrow), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    nvalues_adbl = nvalues_adbl + isize
    allocate(mt)
    mt%adbl2d => adbl
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,',',i0,')')") 'DOUBLE', ncol, nrow
    call memorylist%add(mt)
  end subroutine allocate_dbl2d
  
  subroutine allocate_dbl3d(adbl, ncol, nrow, nlay, name, origin)
    real(DP), dimension(:, :, :), pointer, contiguous, intent(inout) :: adbl
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    integer(I4B), intent(in) :: nlay
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    integer(I4B) :: isize
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    call check_varname(name)
    isize = ncol * nrow * nlay
    allocate(adbl(ncol, nrow, nlay), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    nvalues_adbl = nvalues_adbl + isize
    allocate(mt)
    mt%adbl3d => adbl
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,',',i0,',',i0,')')") 'DOUBLE', ncol,           &
                                                       nrow, nlay
    call memorylist%add(mt)
  end subroutine allocate_dbl3d

  subroutine allocate_ts1d(ats, isize, name, origin)
    type (MemoryTSType), dimension(:), pointer, contiguous, intent(inout) :: ats
    integer(I4B), intent(in) :: isize
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    integer(I4B) :: i
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    call check_varname(name)
    allocate(ats(isize), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    do i = 1, isize
      allocate(ats(i)%name, stat=istat, errmsg=ermsg)
      if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
      ats(i)%name = ''
      allocate(ats(i)%value, stat=istat, errmsg=ermsg)
      ats(i)%value = DZERO
      if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    end do
    nvalues_ats = nvalues_ats + isize
    allocate(mt)
    mt%ats1d => ats
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,')')") 'TIMESERIES', isize
    call memorylist%add(mt)
  end subroutine allocate_ts1d
  
  subroutine reallocate_int1d(aint, isize, name, origin)
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: aint
    integer(I4B), intent(in) :: isize
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    integer(I4B) :: i, isizeold
    integer(I4B) :: ifill
    character(len=100) :: ermsg
    logical :: found
    !
    ! -- Find and assign mt
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- Allocate aint and then refill
    isizeold = size(mt%aint1d)
    ifill = min(isizeold, isize)
    allocate(aint(isize), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    do i = 1, ifill
      aint(i) = mt%aint1d(i)
    enddo
    !
    ! -- deallocate mt pointer, repoint, recalculate isize
    deallocate(mt%aint1d)
    mt%aint1d => aint
    mt%isize = isize
    mt%nrealloc = mt%nrealloc + 1
    mt%master = .true.
    nvalues_aint = nvalues_aint + isize - isizeold
    !
    ! -- return
    return
  end subroutine reallocate_int1d
  
  subroutine reallocate_int2d(aint, ncol, nrow, name, origin)
    integer(I4B), dimension(:, :), pointer, contiguous, intent(inout) :: aint
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    integer(I4B), dimension(2) :: ishape
    integer(I4B) :: i, j, isize, isizeold
    character(len=100) :: ermsg
    logical :: found
    !
    ! -- Find and assign mt
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- Allocate aint and then refill
    ishape = shape(mt%aint2d)
    isize = nrow * ncol
    isizeold = ishape(1) * ishape(2)
    allocate(aint(ncol, nrow), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    do i = 1, ishape(2)
      do j = 1, ishape(1)
        aint(j, i) = mt%aint2d(j, i)
      enddo
    enddo
    !
    ! -- deallocate mt pointer, repoint, recalculate isize
    deallocate(mt%aint2d)
    mt%aint2d => aint
    mt%isize = isize
    mt%nrealloc = mt%nrealloc + 1
    mt%master = .true.
    nvalues_aint = nvalues_aint + isize - isizeold
    write(mt%memtype, "(a,' (',i0,',',i0,')')") 'INTEGER', ncol, nrow
    !
    ! -- return
    return
  end subroutine reallocate_int2d
  
  subroutine reallocate_dbl1d(adbl, isize, name, origin)
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl
    integer(I4B), intent(in) :: isize
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    integer(I4B) :: i, isizeold
    integer(I4B) :: ifill
    character(len=100) :: ermsg
    logical :: found
    !
    ! -- Find and assign mt
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- Allocate adbl and then refill
    isizeold = size(mt%adbl1d)
    ifill = min(isizeold, isize)
    allocate(adbl(isize), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    do i = 1, ifill
      adbl(i) = mt%adbl1d(i)
    enddo
    !
    ! -- deallocate mt pointer, repoint, recalculate isize
    deallocate(mt%adbl1d)
    mt%adbl1d => adbl
    mt%isize = isize
    mt%nrealloc = mt%nrealloc + 1
    mt%master = .true.
    nvalues_adbl = nvalues_adbl + isize - isizeold
    write(mt%memtype, "(a,' (',i0,')')") 'DOUBLE', isize
    !
    ! -- return
    return
  end subroutine reallocate_dbl1d
  
  subroutine reallocate_dbl2d(adbl, ncol, nrow, name, origin)
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: adbl
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    integer(I4B), dimension(2) :: ishape
    integer(I4B) :: i, j, isize, isizeold
    character(len=100) :: ermsg
    logical :: found
    !
    ! -- Find and assign mt
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- Allocate adbl and then refill
    ishape = shape(mt%adbl2d)
    isize = nrow * ncol
    isizeold = ishape(1) * ishape(2)
    allocate(adbl(ncol, nrow), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    do i = 1, ishape(2)
      do j = 1, ishape(1)
        adbl(j, i) = mt%adbl2d(j, i)
      enddo
    enddo
    !
    ! -- deallocate mt pointer, repoint, recalculate isize
    deallocate(mt%adbl2d)
    mt%adbl2d => adbl
    mt%isize = isize
    mt%nrealloc = mt%nrealloc + 1
    mt%master = .true.
    nvalues_adbl = nvalues_adbl + isize - isizeold
    write(mt%memtype, "(a,' (',i0,',',i0,')')") 'DOUBLE', ncol, nrow
    !
    ! -- return
    return
  end subroutine reallocate_dbl2d
  
  subroutine setptr_logical(logicalsclr, name, origin)
    logical, pointer, intent(inout) :: logicalsclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    type(MemoryType), pointer :: mt
    logical :: found
    call get_from_memorylist(name, origin, mt, found)    
    logicalsclr => mt%logicalsclr
  end subroutine setptr_logical
  
  subroutine setptr_int(intsclr, name, origin)
    integer(I4B), pointer, intent(inout) :: intsclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    type(MemoryType), pointer :: mt
    logical :: found
    call get_from_memorylist(name, origin, mt, found)    
    intsclr => mt%intsclr
  end subroutine setptr_int
  
  subroutine setptr_int1d(aint, name, origin)
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    type(MemoryType), pointer :: mt
    logical :: found
    call get_from_memorylist(name, origin, mt, found)    
    aint => mt%aint1d
  end subroutine setptr_int1d
  
  subroutine setptr_int2d(aint, name, origin)
    integer(I4B), dimension(:, :), pointer, contiguous, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    type(MemoryType), pointer :: mt
    logical :: found
    call get_from_memorylist(name, origin, mt, found)    
    aint => mt%aint2d
  end subroutine setptr_int2d
  
  subroutine setptr_dbl(dblsclr, name, origin)
    real(DP), pointer, intent(inout) :: dblsclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    type(MemoryType), pointer :: mt
    logical :: found
    call get_from_memorylist(name, origin, mt, found)    
    dblsclr => mt%dblsclr
  end subroutine setptr_dbl
  
  subroutine setptr_dbl1d(adbl, name, origin)
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    type(MemoryType), pointer :: mt
    logical :: found
    call get_from_memorylist(name, origin, mt, found)    
    adbl => mt%adbl1d
  end subroutine setptr_dbl1d
  
  subroutine setptr_dbl2d(adbl, name, origin)
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    type(MemoryType), pointer :: mt
    logical :: found
    call get_from_memorylist(name, origin, mt, found)    
    adbl => mt%adbl2d
  end subroutine setptr_dbl2d

  subroutine copyptr_int1d(aint, name, origin, origin2)
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in), optional :: origin2
    type(MemoryType), pointer :: mt
    integer(I4B) :: n
    logical :: found
    call get_from_memorylist(name, origin, mt, found)    
    aint => null()
    ! -- check the copy into the memory manager
    if (present(origin2)) then
      call allocate_int1d(aint, size(mt%aint1d), mt%name, origin2)
    ! -- create a local copy
    else
      allocate(aint(size(mt%aint1d)))
    end if
    do n = 1, size(mt%aint1d)
      aint(n) = mt%aint1d(n)
    end do
  end subroutine copyptr_int1d

  subroutine copyptr_int2d(aint, name, origin, origin2)
    integer(I4B), dimension(:,:), pointer, contiguous, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in), optional :: origin2
    type(MemoryType), pointer :: mt
    integer(I4B) :: i, j
    integer(I4B) :: ncol, nrow
    logical :: found
    call get_from_memorylist(name, origin, mt, found)    
    aint => null()
    ncol = size(mt%aint2d, dim=1)
    nrow = size(mt%aint2d, dim=2)
    ! -- check the copy into the memory manager
    if (present(origin2)) then
      call allocate_int2d(aint, ncol, nrow, mt%name, origin2)
    ! -- create a local copy
    else
      allocate(aint(ncol,nrow))
    end if
    do i = 1, nrow
      do j = 1, ncol
        aint(j,i) = mt%aint2d(j,i)
      end do
    end do
  end subroutine copyptr_int2d
  
  subroutine copyptr_dbl1d(adbl, name, origin, origin2)
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in), optional :: origin2
    type(MemoryType), pointer :: mt
    integer(I4B) :: n
    logical :: found
    call get_from_memorylist(name, origin, mt, found)    
    adbl => null()
    ! -- check the copy into the memory manager
    if (present(origin2)) then
      call allocate_dbl1d(adbl, size(mt%adbl1d), mt%name, origin2)
    ! -- create a local copy
    else
      allocate(adbl(size(mt%adbl1d)))
    end if
    do n = 1, size(mt%adbl1d)
      adbl(n) = mt%adbl1d(n)
    end do
  end subroutine copyptr_dbl1d

  subroutine copyptr_dbl2d(adbl, name, origin, origin2)
    real(DP), dimension(:,:), pointer, contiguous, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in), optional :: origin2
    type(MemoryType), pointer :: mt
    integer(I4B) :: i, j
    integer(I4B) :: ncol, nrow
    logical :: found
    call get_from_memorylist(name, origin, mt, found)    
    adbl => null()
    ncol = size(mt%adbl2d, dim=1)
    nrow = size(mt%adbl2d, dim=2)
    ! -- check the copy into the memory manager
    if (present(origin2)) then
      call allocate_dbl2d(adbl, ncol, nrow, mt%name, origin2)
    ! -- create a local copy
    else
      allocate(adbl(ncol,nrow))
    end if
    do i = 1, nrow
      do j = 1, ncol
        adbl(j,i) = mt%adbl2d(j,i)
      end do
    end do
  end subroutine copyptr_dbl2d
  
  subroutine copy_dbl1d(adbl, name, origin)
    real(DP), dimension(:), intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    type(MemoryType), pointer :: mt
    integer(I4B) :: n
    logical :: found
    
    call get_from_memorylist(name, origin, mt, found)
    do n = 1, size(mt%adbl1d)
      adbl(n) = mt%adbl1d(n)
    end do
    
  end subroutine copy_dbl1d
  
  subroutine reassignptr_int1d(aint1d, name, origin, name2, origin2)
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: aint1d
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: name2
    character(len=*), intent(in) :: origin2
    type(MemoryType), pointer :: mt, mt2
    logical :: found
    call get_from_memorylist(name, origin, mt, found)
    call get_from_memorylist(name2, origin2, mt2, found)
    if (size(aint1d) > 0) then
      nvalues_aint = nvalues_aint - size(aint1d)
      deallocate(aint1d)
    end if
    aint1d => mt2%aint1d
    mt%aint1d => aint1d
    mt%isize = size(aint1d)
    write(mt%memtype, "(a,' (',i0,')')") 'INTEGER', mt%isize
    mt%master = .false.
    return
  end subroutine reassignptr_int1d

  subroutine reassignptr_int2d(aint2d, name, origin, name2, origin2)
    integer(I4B), dimension(:,:), pointer, contiguous, intent(inout) :: aint2d
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: name2
    character(len=*), intent(in) :: origin2
    integer(I4B) :: ncol, nrow
    type(MemoryType), pointer :: mt, mt2
    logical :: found
    call get_from_memorylist(name, origin, mt, found)
    call get_from_memorylist(name2, origin2, mt2, found)
    if (size(aint2d) > 0) then
      nvalues_aint = nvalues_aint - size(aint2d)
      deallocate(aint2d)
    end if
    aint2d => mt2%aint2d
    mt%aint2d => aint2d
    mt%isize = size(aint2d)
    ncol = size(aint2d, dim=1)
    nrow = size(aint2d, dim=2)
    write(mt%memtype, "(a,' (',i0,',',i0,')')") 'INTEGER', ncol, nrow
    mt%master = .false.
    return
  end subroutine reassignptr_int2d

  subroutine reassignptr_dbl1d(adbl1d, name, origin, name2, origin2)
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl1d
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: name2
    character(len=*), intent(in) :: origin2
    type(MemoryType), pointer :: mt, mt2
    logical :: found
    call get_from_memorylist(name, origin, mt, found)
    call get_from_memorylist(name2, origin2, mt2, found)
    if (size(adbl1d) > 0) then
      nvalues_adbl = nvalues_adbl - size(adbl1d)
      deallocate(adbl1d)
    end if
    adbl1d => mt2%adbl1d
    mt%adbl1d => adbl1d
    mt%isize = size(adbl1d)
    write(mt%memtype, "(a,' (',i0,')')") 'DOUBLE', mt%isize
    mt%master = .false.
    return
  end subroutine reassignptr_dbl1d

  subroutine reassignptr_dbl2d(adbl2d, name, origin, name2, origin2)
    real(DP), dimension(:,:), pointer, contiguous, intent(inout) :: adbl2d
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: name2
    character(len=*), intent(in) :: origin2
    integer(I4B) :: ncol, nrow
    type(MemoryType), pointer :: mt, mt2
    logical :: found
    call get_from_memorylist(name, origin, mt, found)
    call get_from_memorylist(name2, origin2, mt2, found)
    if (size(adbl2d) > 0) then
      nvalues_adbl = nvalues_adbl - size(adbl2d)
      deallocate(adbl2d)
    end if
    adbl2d => mt2%adbl2d
    mt%adbl2d => adbl2d
    mt%isize = size(adbl2d)
    ncol = size(adbl2d, dim=1)
    nrow = size(adbl2d, dim=2)
    write(mt%memtype, "(a,' (',i0,',',i0,')')") 'DOUBLE', ncol, nrow
    mt%master = .false.
    return
  end subroutine reassignptr_dbl2d

  subroutine deallocate_logical(logicalsclr)
    logical, pointer, intent(inout) :: logicalsclr
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(associated(mt%logicalsclr, logicalsclr)) then
        nullify(mt%logicalsclr)
        found = .true.
        exit
      endif
    enddo
    if (.not. found) then
      call store_error('programming error in deallocate_logical')
      call ustop()
    else
      if (mt%master) then
        deallocate(logicalsclr)
      else
        nullify(logicalsclr)
      end if
    endif
  end subroutine deallocate_logical
  
  subroutine deallocate_int(intsclr)
    integer(I4B), pointer, intent(inout) :: intsclr
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(associated(mt%intsclr, intsclr)) then
        nullify(mt%intsclr)
        found = .true.
        exit
      endif
    enddo
    if (.not. found) then
      call store_error('programming error in deallocate_int')
      call ustop()
    else
      if (mt%master) then
        deallocate(intsclr)
      else
        nullify(intsclr)
      end if
    endif
  end subroutine deallocate_int
  
  subroutine deallocate_dbl(dblsclr)
    real(DP), pointer, intent(inout) :: dblsclr
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(associated(mt%dblsclr, dblsclr)) then
        nullify(mt%dblsclr)
        found = .true.
        exit
      endif
    enddo
    if (.not. found) then
      call store_error('programming error in deallocate_dbl')
      call ustop()
    else
      if (mt%master) then
        deallocate(dblsclr)
      else
        nullify (dblsclr)
      end if
    endif
  end subroutine deallocate_dbl
  
  subroutine deallocate_int1d(aint1d, name, origin)
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: aint1d
    character(len=*), optional :: name
    character(len=*), optional :: origin
    type(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    if (present(name) .and. present(origin)) then
      call get_from_memorylist(name, origin, mt, found)
      nullify(mt%aint1d)
    else
      found = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if(associated(mt%aint1d, aint1d)) then
          nullify(mt%aint1d)
          found = .true.
          exit
        endif
      enddo
    end if
    if (.not. found .and. size(aint1d) > 0 ) then
      call store_error('programming error in deallocate_int1d')
      call ustop()
    else
      if (mt%master) then
        deallocate(aint1d)
      else
        nullify(aint1d)
      end if
    endif
  end subroutine deallocate_int1d
  
  subroutine deallocate_int2d(aint2d, name, origin)
    integer(I4B), dimension(:, :), pointer, contiguous, intent(inout) :: aint2d
    character(len=*), optional :: name
    character(len=*), optional :: origin
    type(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    if (present(name) .and. present(origin)) then
      call get_from_memorylist(name, origin, mt, found)
      nullify(mt%aint2d)
    else
      found = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if(associated(mt%aint2d, aint2d)) then
          nullify(mt%aint2d)
          found = .true.
          exit
        endif
      enddo
    end if
    if (.not. found .and. size(aint2d) > 0 ) then
      call store_error('programming error in deallocate_int2d')
      call ustop()
    else
      if (mt%master) then
        deallocate(aint2d)
      else
        nullify(aint2d)
      end if
    endif
  end subroutine deallocate_int2d
  
  subroutine deallocate_int3d(aint3d, name, origin)
    integer(I4B), dimension(:, :, :), pointer, contiguous, intent(inout) :: aint3d
    character(len=*), optional :: name
    character(len=*), optional :: origin
    type(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    if (present(name) .and. present(origin)) then
      call get_from_memorylist(name, origin, mt, found)
      nullify(mt%aint3d)
    else
      found = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if(associated(mt%aint3d, aint3d)) then
          nullify(mt%aint3d)
          found = .true.
          exit
        endif
      enddo
    end if
    if (.not. found .and. size(aint3d) > 0 ) then
      call store_error('programming error in deallocate_int3d')
      call ustop()
    else
      if (mt%master) then
        deallocate(aint3d)
      else
        nullify(aint3d)
      end if
    endif
  end subroutine deallocate_int3d
  
  subroutine deallocate_dbl1d(adbl1d, name, origin)
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl1d
    character(len=*), optional :: name
    character(len=*), optional :: origin
    type(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    if (present(name) .and. present(origin)) then
      call get_from_memorylist(name, origin, mt, found)
      nullify(mt%adbl1d)
    else
      found = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if(associated(mt%adbl1d, adbl1d)) then
          nullify(mt%adbl1d)
          found = .true.
          exit
        endif
      enddo
    end if
    if (.not. found .and. size(adbl1d) > 0 ) then
      call store_error('programming error in deallocate_dbl1d')
      call ustop()
    else
      if (mt%master) then
        deallocate(adbl1d)
      else
        nullify(adbl1d)
      end if
    endif
  end subroutine deallocate_dbl1d
  
  subroutine deallocate_dbl2d(adbl2d, name, origin)
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: adbl2d
    character(len=*), optional :: name
    character(len=*), optional :: origin
    type(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    if (present(name) .and. present(origin)) then
      call get_from_memorylist(name, origin, mt, found)
      nullify(mt%adbl2d)
    else
      found = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if(associated(mt%adbl2d, adbl2d)) then
          nullify(mt%adbl2d)
          found = .true.
          exit
        endif
      enddo
    end if
    if (.not. found .and. size(adbl2d) > 0 ) then
      call store_error('programming error in deallocate_dbl2d')
      call ustop()
    else
      if (mt%master) then
        deallocate(adbl2d)
      else
        nullify(adbl2d)
      end if
    endif
  end subroutine deallocate_dbl2d
  
  subroutine deallocate_dbl3d(adbl3d, name, origin)
    real(DP), dimension(:, :, :), pointer, contiguous, intent(inout) :: adbl3d
    character(len=*), optional :: name
    character(len=*), optional :: origin
    type(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    if (present(name) .and. present(origin)) then
      call get_from_memorylist(name, origin, mt, found)
      nullify(mt%adbl3d)
    else
      found = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if(associated(mt%adbl3d, adbl3d)) then
          nullify(mt%adbl3d)
          found = .true.
          exit
        endif
      enddo
    end if
    if (.not. found .and. size(adbl3d) > 0 ) then
      call store_error('programming error in deallocate_dbl3d')
      call ustop()
    else
      if (mt%master) then
        deallocate(adbl3d)
      else
        nullify(adbl3d)
      end if
    endif
  end subroutine deallocate_dbl3d
  
  subroutine deallocate_ts1d(ats1d)
    type (MemoryTSType), dimension(:), pointer, contiguous, intent(inout) :: ats1d
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    integer(I4B) :: i
    logical :: found 
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if (associated(mt%ats1d, ats1d)) then
        nullify(mt%ats1d)
        found = .true.
        exit
      end if
    end do
    if (.not. found .and. size(ats1d) > 0 ) then
      call store_error('programming error in deallocate_ts1d')
      call ustop()
    else
      do i = 1, size(ats1d)
        deallocate(ats1d(i)%name)
        deallocate(ats1d(i)%value)
      enddo
      if (mt%master) then
        deallocate(ats1d)
      else
        nullify(ats1d)
      end if
    endif
    return
  end subroutine deallocate_ts1d
  
  subroutine mem_set_print_option(iout, keyword, errmsg)
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: keyword
    character(len=*), intent(inout) :: errmsg
    select case (keyword)
      case ('NONE')
        iprmem = 0
        write(iout, '(4x, a)')                                                 &
              'LIMITED MEMORY INFORMATION WILL BE WRITTEN.'
      case ('SUMMARY')
        iprmem = 1
        write(iout, '(4x, a)')                                                 &
              'A SUMMARY OF SIMULATION MEMORY INFORMATION WILL BE WRITTEN.'
      case ('ALL')
        iprmem = 2
        write(iout, '(4x, a)')                                                 &
              'ALL SIMULATION MEMORY INFORMATION WILL BE WRITTEN.'
      case default
        write(errmsg,'(4x,a,a)')                                               &
          'UNKNOWN MEMORY PRINT OPTION: ', trim(keyword)
    end select
    return
  end subroutine mem_set_print_option
  
  subroutine mem_usage(iout)
    integer(I4B), intent(in) :: iout
    class(MemoryType), pointer :: mt
    character(len=*), parameter :: fmt = "(1x, a, i0)"
    character(len=*), parameter :: fmtd = "(1x, a, 1(1pg15.6))"
    character(len=*), parameter :: fmttitle = "(/, 1x, a)"
    character(len=*), parameter :: fmtheader = &
      "(1x, a40, a20, a20, a10, a10, a10, /, 1x, 110('-'))"
    character(len=200) :: msg
    character(len=LENORIGIN), allocatable, dimension(:) :: cunique
    real(DP) :: bytesmb
    integer(I4B) :: ipos
    integer(I4B) :: icomp, ilen
    integer(I8B) :: nint, nreal
    !
    ! -- Write info to simulation list file
    write(iout, fmttitle) 'INFORMATION ON VARIABLES STORED IN THE MEMORY MANAGER'
    !
    ! -- Write summary table for simulatation components
    if (iprmem == 1) then
      !
      ! -- Find unique names of simulation componenets
      call mem_unique_origins(cunique)
      write(iout, '(*(G0))') '      COMPONENT     ', &
                             '   NINTS  ', &
                             '   NREAL  ', &
                             '      MBYTES    '
      write(iout, "(56('-'))")
      do icomp = 1, size(cunique)
        nint = 0
        nreal = 0
        bytesmb = DZERO
        ilen = len_trim(cunique(icomp))
        do ipos = 1, memorylist%count()
          mt => memorylist%Get(ipos)
          if (cunique(icomp) /= mt%origin(1:ilen)) cycle
          if (.not. mt%master) cycle
          if (mt%memtype(1:7) == 'INTEGER') nint = nint + mt%isize
          if (mt%memtype(1:6) == 'DOUBLE') nreal = nreal + mt%isize
        enddo
        bytesmb = (nint * I4B + nreal * DP) / 1000000.d0
        write(iout, '(a20, i10, i10, 1pg16.2)') cunique(icomp), nint, nreal, bytesmb
      enddo
    endif
    !
    ! -- Write table with all variables for iprmem == 2
    if (iprmem == 2) then
      write(iout, *)
      write(iout, fmtheader) '                 ORIGIN                 ',       &
                             '        NAME        ',                           &
                             '        TYPE        ',                           &
                             '   SIZE   ',                                     &
                             ' NREALLOC ',                                     &
                             '  POINTER '
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        call mt%table_entry(msg)
        write(iout, '(a)') msg 
      enddo
    endif
    !
    ! -- Calculate and write total memory allocation
    bytesmb = (nvalues_aint * I4B + &
               nvalues_adbl * DP + &
               nvalues_ats * DP) / 1000000.d0
    write(iout, *)
    write(iout, fmt) 'Number of allocated integer variables:   ', nvalues_aint
    write(iout, fmt) 'Number of allocated real variables:    ', nvalues_adbl + nvalues_ats
    write(iout, fmtd) 'Allocated memory in megabytes: ', bytesmb
    write(iout, *)
  end subroutine mem_usage
  
  subroutine mem_da()
    use SimModule, only: store_error, ustop, count_errors
    use VersionModule, only: IDEVELOPMODE
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    character(len=LINELENGTH) :: errmsg
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if (IDEVELOPMODE == 1) then
        if (mt%mt_associated() .and. mt%isize > 0) then
          errmsg = trim(adjustl(mt%origin)) // ' ' // &
                   trim(adjustl(mt%name)) // ' not deallocated'
          call store_error(trim(errmsg))
        end if
      end if
      deallocate(mt)
    enddo
    call memorylist%clear()
    if (count_errors() > 0) call ustop()
  end subroutine mem_da
  
  subroutine mem_unique_origins(cunique)
    use ArrayHandlersModule, only: ExpandArray, ifind
    use InputOutputModule, only: ParseLine
    character(len=LENORIGIN), allocatable, dimension(:), intent(inout) :: cunique
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    integer(I4B) :: ipa
    integer(I4B) :: nwords
    character(len=LENORIGIN), allocatable, dimension(:) :: words
    allocate(cunique(0))
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      call ParseLine(mt%origin, nwords, words)
      ipa = ifind(cunique, words(1))
      if(ipa < 1) then
        call ExpandArray(cunique, 1)
        cunique(size(cunique)) = words(1)
      endif
    enddo
    return
  end subroutine mem_unique_origins
  
end module MemoryManagerModule

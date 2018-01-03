module MemoryManagerModule

  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DZERO, LENORIGIN, LENVARNAME
  use SimModule,              only: store_error, ustop
  use MemoryTypeModule,       only: MemoryTSType, MemoryType
  use MemoryListModule,       only: MemoryListType
  
  implicit none
  private
  public :: mem_allocate
  public :: mem_reallocate
  public :: mem_setptr
  public :: mem_copyptr
  public :: mem_deallocate
  public :: mem_usage
  public :: mem_da
  public :: mem_set_print_option
    
  type(MemoryListType) :: memorylist
  integer(I4B) :: nvalues_alogical = 0
  integer(I4B) :: nvalues_achr = 0
  integer(I4B) :: nvalues_aint = 0
  integer(I4B) :: nvalues_adbl = 0
  integer(I4B) :: nvalues_ats= 0
  integer(I4B) :: iprmem = 0

  interface mem_allocate
    module procedure allocate_logical,                                         &
                     allocate_int, allocate_int1d, allocate_int2d,             &
                     allocate_dbl, allocate_dbl1d, allocate_dbl2d,             &
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

  interface mem_deallocate
    module procedure deallocate_logical,                                         &
                     deallocate_int, deallocate_int1d, deallocate_int2d,         &
                     deallocate_dbl, deallocate_dbl1d, deallocate_dbl2d,         &
                     deallocate_ts1d
  end interface mem_deallocate

contains
  
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
    if(len(name) > LENVARNAME) then
      write(ermsg, '(*(G0))')                                                  &
        'Programming error. Variable ', name, ' must be ', LENVARNAME,         &
        ' characters or less.'
      call store_error(ermsg)
      call ustop()
    endif
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
    integer(I4B), dimension(:), pointer, intent(inout) :: aint
    integer(I4B), intent(in) :: isize
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    if(len(name) > LENVARNAME) then
      write(ermsg, '(*(G0))')                                                  &
        'Programming error. Variable ', name, ' must be ', LENVARNAME,         &
        ' characters or less.'
      call store_error(ermsg)
      call ustop()
    endif
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
    integer(I4B), dimension(:, :), pointer, intent(inout) :: aint
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    integer(I4B) :: isize
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    if(len(name) > LENVARNAME) then
      write(ermsg, '(*(G0))')                                                  &
        'Programming error. Variable ', name, ' must be ', LENVARNAME,         &
        ' characters or less.'
      call store_error(ermsg)
      call ustop()
    endif
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
  
  subroutine allocate_dbl(dblsclr, name, origin)
    real(DP), pointer, intent(inout) :: dblsclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    if(len(name) > LENVARNAME) then
      write(ermsg, '(*(G0))')                                                  &
        'Programming error. Variable ', name, ' must be ', LENVARNAME,         &
        ' characters or less.'
      call store_error(ermsg)
      call ustop()
    endif
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
    real(DP), dimension(:), pointer, intent(inout) :: adbl
    integer(I4B), intent(in) :: isize
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    if(len(name) > LENVARNAME) then
      write(ermsg, '(*(G0))')                                                  &
        'Programming error. Variable ', name, ' must be ', LENVARNAME,         &
        ' characters or less.'
      call store_error(ermsg)
      call ustop()
    endif
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
    real(DP), dimension(:, :), pointer, intent(inout) :: adbl
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    integer(I4B) :: isize
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    if(len(name) > LENVARNAME) then
      write(ermsg, '(*(G0))')                                                  &
        'Programming error. Variable ', name, ' must be ', LENVARNAME,         &
        ' characters or less.'
      call store_error(ermsg)
      call ustop()
    endif
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

  subroutine allocate_ts1d(ats, isize, name, origin)
    type (MemoryTSType), dimension(:), pointer, intent(inout) :: ats
    integer(I4B), intent(in) :: isize
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    integer(I4B) :: i
    type(MemoryType), pointer :: mt
    character(len=100) :: ermsg
    if(len(name) > LENVARNAME) then
      write(ermsg, '(*(G0))')                                                  &
        'Programming error. Variable ', name, ' must be ', LENVARNAME,         &
        ' characters or less.'
      call store_error(ermsg)
      call ustop()
    endif
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
    integer(I4B), dimension(:), pointer, intent(inout) :: aint
    integer(I4B), intent(in) :: isize
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    integer(I4B) :: ipos, i, isizeold
    character(len=100) :: ermsg
    logical :: found
    !
    ! -- Find and assign mt
    mt => null()
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        found = .true.
        exit
      endif
    enddo
    !
    if(.not. found) call allocate_error(name, origin, 0,                       &
      'Variable not found in MemoryManager', isize)
    !
    ! -- Allocate aint and then refill
    isizeold = size(mt%aint1d)
    allocate(aint(isize), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    do i = 1, isizeold
      aint(i) = mt%aint1d(i)
    enddo
    !
    ! -- deallocate mt pointer, repoint, recalculate isize
    deallocate(mt%aint1d)
    mt%aint1d => aint
    mt%isize = isize
    mt%nrealloc = mt%nrealloc + 1
    nvalues_aint = nvalues_aint + isize - isizeold
    !
    ! -- return
    return
  end subroutine reallocate_int1d
  
  subroutine reallocate_int2d(aint, ncol, nrow, name, origin)
    integer(I4B), dimension(:, :), pointer, intent(inout) :: aint
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    integer(I4B), dimension(2) :: ishape
    integer(I4B) :: ipos, i, j, isize, isizeold
    character(len=100) :: ermsg
    logical :: found
    !
    ! -- Find and assign mt
    mt => null()
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        found = .true.
        exit
      endif
    enddo
    !
    if(.not. found) call allocate_error(name, origin, 0,                       &
      'Variable not found in MemoryManager', isize)
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
    nvalues_aint = nvalues_aint + isize - isizeold
    write(mt%memtype, "(a,' (',i0,',',i0,')')") 'INTEGER', ncol, nrow
    !
    ! -- return
    return
  end subroutine reallocate_int2d
  
  subroutine reallocate_dbl1d(adbl, isize, name, origin)
    real(DP), dimension(:), pointer, intent(inout) :: adbl
    integer(I4B), intent(in) :: isize
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    integer(I4B) :: ipos, i, isizeold
    character(len=100) :: ermsg
    logical :: found
    !
    ! -- Find and assign mt
    mt => null()
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        found = .true.
        exit
      endif
    enddo
    !
    if(.not. found) call allocate_error(name, origin, 0,                       &
      'Variable not found in MemoryManager', isize)
    !
    ! -- Allocate adbl and then refill
    isizeold = size(mt%adbl1d)
    allocate(adbl(isize), stat=istat, errmsg=ermsg)
    if(istat /= 0) call allocate_error(name, origin, istat, ermsg, isize)
    do i = 1, isizeold
      adbl(i) = mt%adbl1d(i)
    enddo
    !
    ! -- deallocate mt pointer, repoint, recalculate isize
    deallocate(mt%adbl1d)
    mt%adbl1d => adbl
    mt%isize = isize
    mt%nrealloc = mt%nrealloc + 1
    nvalues_adbl = nvalues_adbl + isize - isizeold
    write(mt%memtype, "(a,' (',i0,')')") 'DOUBLE', isize
    !
    ! -- return
    return
  end subroutine reallocate_dbl1d
  
  subroutine reallocate_dbl2d(adbl, ncol, nrow, name, origin)
    real(DP), dimension(:, :), pointer, intent(inout) :: adbl
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    integer(I4B), dimension(2) :: ishape
    integer(I4B) :: ipos, i, j, isize, isizeold
    character(len=100) :: ermsg
    logical :: found
    !
    ! -- Find and assign mt
    mt => null()
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        found = .true.
        exit
      endif
    enddo
    !
    if(.not. found) call allocate_error(name, origin, 0,                       &
      'Variable not found in MemoryManager', isize)
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
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logicalsclr => null()
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        logicalsclr => mt%logicalsclr
        exit
      endif
    enddo
  end subroutine setptr_logical
  
  subroutine setptr_int(intsclr, name, origin)
    integer(I4B), pointer, intent(inout) :: intsclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    intsclr => null()
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        intsclr => mt%intsclr
        exit
      endif
    enddo
  end subroutine setptr_int
  
  subroutine setptr_int1d(aint, name, origin)
    integer(I4B), dimension(:), pointer, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    aint => null()
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        aint => mt%aint1d
        exit
      endif
    enddo
  end subroutine setptr_int1d
  
  subroutine setptr_int2d(aint, name, origin)
    integer(I4B), dimension(:, :), pointer, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    aint => null()
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        aint => mt%aint2d
        exit
      endif
    enddo
  end subroutine setptr_int2d
  
  subroutine setptr_dbl(dblsclr, name, origin)
    real(DP), pointer, intent(inout) :: dblsclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    dblsclr => null()
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        dblsclr => mt%dblsclr
        exit
      endif
    enddo
  end subroutine setptr_dbl
  
  subroutine setptr_dbl1d(adbl, name, origin)
    real(DP), dimension(:), pointer, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    adbl => null()
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        adbl => mt%adbl1d
        exit
      endif
    enddo
  end subroutine setptr_dbl1d
  
  subroutine setptr_dbl2d(adbl, name, origin)
    real(DP), dimension(:, :), pointer, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    adbl => null()
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        adbl => mt%adbl2d
        exit
      endif
    enddo
  end subroutine setptr_dbl2d

  
  subroutine copyptr_int1d(aint, name, origin, origin2)
    integer(I4B), dimension(:), pointer, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in), optional :: origin2
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    integer(I4B) :: n
    aint => null()
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
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
        exit
      endif
    enddo
  end subroutine copyptr_int1d

  subroutine copyptr_int2d(aint, name, origin, origin2)
    integer(I4B), dimension(:,:), pointer, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in), optional :: origin2
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    integer(I4B) :: i, j
    integer(I4B) :: ncol, nrow
    aint => null()
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
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
        exit
      endif
    enddo
  end subroutine copyptr_int2d
  
  subroutine copyptr_dbl1d(adbl, name, origin, origin2)
    real(DP), dimension(:), pointer, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in), optional :: origin2
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    integer(I4B) :: n
    adbl => null()
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
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
        exit
      endif
    enddo
  end subroutine copyptr_dbl1d

  subroutine copyptr_dbl2d(adbl, name, origin, origin2)
    real(DP), dimension(:,:), pointer, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in), optional :: origin2
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    integer(I4B) :: i, j
    integer(I4B) :: ncol, nrow
    adbl => null()
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
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
        exit
      endif
    enddo
  end subroutine copyptr_dbl2d
  
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
      deallocate(logicalsclr)
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
      deallocate(intsclr)
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
      deallocate(dblsclr)
    endif
  end subroutine deallocate_dbl
  
  subroutine deallocate_int1d(aint1d)
    integer(I4B), dimension(:), pointer, intent(inout) :: aint1d
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(associated(mt%aint1d, aint1d)) then
        nullify(mt%aint1d)
        found = .true.
        exit
      endif
    enddo
    if (.not. found .and. size(aint1d) > 0 ) then
      call store_error('programming error in deallocate_int1d')
      call ustop()
    else
      deallocate(aint1d)
    endif
  end subroutine deallocate_int1d
  
  subroutine deallocate_int2d(aint2d)
    integer(I4B), dimension(:, :), pointer, intent(inout) :: aint2d
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(associated(mt%aint2d, aint2d)) then
        nullify(mt%aint2d)
        found = .true.
        exit
      endif
    enddo
    if (.not. found .and. size(aint2d) > 0 ) then
      call store_error('programming error in deallocate_int2d')
      call ustop()
    else
      deallocate(aint2d)
    endif
  end subroutine deallocate_int2d
  
  subroutine deallocate_dbl1d(adbl1d)
    real(DP), dimension(:), pointer, intent(inout) :: adbl1d
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(associated(mt%adbl1d, adbl1d)) then
        nullify(mt%adbl1d)
        found = .true.
        exit
      endif
    enddo
    if (.not. found .and. size(adbl1d) > 0 ) then
      call store_error('programming error in deallocate_dbl1d')
      call ustop()
    else
      deallocate(adbl1d)
    endif
  end subroutine deallocate_dbl1d
  
  subroutine deallocate_dbl2d(adbl2d)
    real(DP), dimension(:, :), pointer, intent(inout) :: adbl2d
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    logical :: found
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(associated(mt%adbl2d, adbl2d)) then
        nullify(mt%adbl2d)
        found = .true.
        exit
      endif
    enddo
    if (.not. found .and. size(adbl2d) > 0 ) then
      call store_error('programming error in deallocate_dbl2d')
      call ustop()
    else
      deallocate(adbl2d)
    endif
  end subroutine deallocate_dbl2d
  
  subroutine deallocate_ts1d(ats1d)
    type (MemoryTSType), dimension(:), pointer, intent(inout) :: ats1d
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
      deallocate(ats1d)
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
      "(1x, a40, a20, a20, a10, a10, /, 1x, 100('-'))"
    character(len=200) :: msg
    character(len=LENORIGIN), allocatable, dimension(:) :: cunique
    real(DP) :: bytesmb
    integer(I4B) :: ipos
    integer(I4B) :: icomp, ilen, nint, nreal
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
                             ' NREALLOC '
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
    class(MemoryType), pointer :: mt
    integer(I4B) :: ipos
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      deallocate(mt)
    enddo
    call memorylist%clear()
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

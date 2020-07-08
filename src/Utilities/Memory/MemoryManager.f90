module MemoryManagerModule

  use KindModule,             only: DP, LGP, I4B, I8B
  use ConstantsModule,        only: DZERO, DONE,                                 &
                                    DEM3, DEM6, DEM9, DEP3, DEP6, DEP9,          &
                                    LENORIGIN, LENVARNAME,                       &
                                    LINELENGTH, LENMEMTYPE,                      &
                                    TABSTRING, TABUCSTRING, TABINTEGER, TABREAL, &
                                    TABCENTER, TABLEFT, TABRIGHT,                &
                                    MEMHIDDEN, MEMREADONLY, MEMREADWRITE
  use SimVariablesModule,     only: errmsg
  use SimModule,              only: store_error, count_errors, ustop
  use MemoryTypeModule,       only: MemoryType
  use MemoryListModule,       only: MemoryListType
  use TableModule,            only: TableType, table_cr
  
  implicit none
  private
  public :: mem_allocate
  public :: mem_checkin
  public :: mem_reallocate
  public :: mem_setptr
  public :: mem_copyptr
  public :: mem_reassignptr
  public :: mem_deallocate
  public :: mem_write_usage
  public :: mem_da
  public :: mem_set_print_option
  
  public :: get_mem_type
  public :: get_mem_rank
  public :: get_mem_size
  public :: get_mem_shape
  public :: get_isize
  public :: copy_dbl1d
  
  type(MemoryListType) :: memorylist
  type(TableType), pointer :: memtab => null()
  integer(I8B) :: nvalues_alogical = 0
  integer(I8B) :: nvalues_achr = 0
  integer(I8B) :: nvalues_astr = 0
  integer(I8B) :: nvalues_aint = 0
  integer(I8B) :: nvalues_adbl = 0
  integer(I4B) :: iprmem = 0

  interface mem_allocate
    module procedure allocate_logical,                                           &
                     allocate_str, allocate_str1d,                               &
                     allocate_int, allocate_int1d, allocate_int2d,               &
                     allocate_int3d,                                             &
                     allocate_dbl, allocate_dbl1d, allocate_dbl2d,               &
                     allocate_dbl3d
  end interface mem_allocate
  
  interface mem_checkin
    module procedure checkin_int1d,                                              &
                     checkin_dbl1d
  end interface mem_checkin
  
  interface mem_reallocate
    module procedure reallocate_int1d, reallocate_int2d, reallocate_dbl1d,       &
                     reallocate_dbl2d, reallocate_str1d
  end interface mem_reallocate
  
  interface mem_setptr
    module procedure setptr_logical,                                             &
                     setptr_int, setptr_int1d, setptr_int2d,                     &
                     setptr_dbl, setptr_dbl1d, setptr_dbl2d
  end interface mem_setptr
  
  interface mem_copyptr
    module procedure copyptr_int1d, copyptr_int2d,                               &
                     copyptr_dbl1d, copyptr_dbl2d
  end interface mem_copyptr

  interface mem_reassignptr
    module procedure reassignptr_int1d, reassignptr_int2d,                       &
                     reassignptr_dbl1d, reassignptr_dbl2d
  end interface mem_reassignptr

  interface mem_deallocate
    module procedure deallocate_logical,                                         &
                     deallocate_str, deallocate_str1d,                           &
                     deallocate_int, deallocate_int1d, deallocate_int2d,         &
                     deallocate_int3d,                                           &
                     deallocate_dbl, deallocate_dbl1d, deallocate_dbl2d,         &
                     deallocate_dbl3d
  end interface mem_deallocate

  contains
  
  subroutine get_mem_type(name, origin, var_type)
! ******************************************************************************
! Get the variable memory type 
!
! -- Arguments are as follows:
!       NAME         : variable name
!       ORIGIN       : variable origin
!       VAR_TYPE     : returned variable memory type   
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=LENMEMTYPE), intent(out) :: var_type    
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    ! -- code    
    mt => null()
    var_type = 'UNKNOWN'
    call get_from_memorylist(name, origin, mt, found)
    if (found) then
      var_type = mt%memtype
    end if
    !
    ! -- return
    return
  end subroutine get_mem_type
  
  subroutine get_mem_rank(name, origin, rank)
! ******************************************************************************
! Get the variable rank 
!
! -- Arguments are as follows:
!       NAME         : variable name
!       ORIGIN       : variable origin
!       RANK         : returned variable rank   
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(out)    :: rank    
    ! -- local
    type(MemoryType), pointer :: mt => null()
    logical(LGP) :: found
    ! -- code    
    !
    ! -- initialize rank to a value to communicate failure
    rank = -1
    !
    ! -- get the entry from the memory manager
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- set rank
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
    end if    
    !
    ! -- return
    return
  end subroutine get_mem_rank 
    
  subroutine get_mem_size(name, origin, size)
! ******************************************************************************
! Get the variable size 
!
! -- Arguments are as follows:
!       NAME         : variable name
!       ORIGIN       : variable origin
!       SIZE         : returned variable size   
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(out)    :: size
    ! -- local
    type(MemoryType), pointer :: mt => null()
    logical(LGP) :: found
    ! -- code
    !
    ! -- initialize size to a value to communicate failure
    size = -1
    !
    ! -- get the entry from the memory manager
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- set memory size
    if (found) then      
      select case(mt%memtype(1:index(mt%memtype,' ')))
      case ('STRING')
        size = 1
      case ('LOGICAL')
        size = 4
      case ('INTEGER')
        size = 4
      case ('DOUBLE')
        size = 8
      end select 
    end if
    !
    ! -- return
    return
  end subroutine get_mem_size
  
  subroutine get_mem_shape(name, origin, mem_shape)
! ******************************************************************************
! Get the variable shape 
!
! -- Arguments are as follows:
!       NAME         : variable name
!       ORIGIN       : variable origin
!       MEM_SHAPE    : returned variable shape (integer vector)   
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), dimension(:), intent(out) :: mem_shape
    ! -- local
    type(MemoryType), pointer :: mt => null()
    logical(LGP) :: found
    ! -- code
    !
    ! -- get the entry from the memory manager
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- set shape
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
    ! -- to communicate failure
    else
      mem_shape(1) = -1
    end if
    !
    ! -- return
    return
  end subroutine get_mem_shape
  
  subroutine get_isize(name, origin, isize)
! ******************************************************************************
! Get the variable number of elements (flattened) 
!
! -- Arguments are as follows:
!       NAME         : variable name
!       ORIGIN       : variable origin
!       ISIZE        : returned variable number of elements (flattened)  
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(out)    :: isize
    ! -- local
    type(MemoryType), pointer :: mt => null()
    logical(LGP) :: found
    ! -- code    
    !
    ! -- initialize isize to a value to communicate failure
    isize = -1
    !
    ! -- get the entry from the memory manager
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- set isize
    if (found) then
      isize = mt%isize
    end if
    !
    ! -- return
    return
  end subroutine get_isize
  
  subroutine get_from_memorylist(name, origin, mt, found, check)
! ******************************************************************************
! Get a memory type entry (MT) from the memory list 
!
! -- Arguments are as follows:
!       NAME         : variable name
!       ORIGIN       : variable origin
!       MT           : returned memory type entry in the memory list
!       FOUND        : returned boolean indicating if varible with origin
!                      was found
!       CHECK        : optional boolean flag to make sure the variable exists
!                      in the memory list
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    type(MemoryType), pointer, intent(inout) :: mt
    logical(LGP),intent(out) :: found
    logical(LGP), intent(in), optional :: check
    ! -- local
    integer(I4B) :: ipos
    logical(LGP) check_opt
    ! -- code
    !
    ! -- initialize
    mt => null()
    found = .false.
    !
    ! -- iterate over the memory list
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(mt%name == name .and. mt%origin == origin) then
        found = .true.
        exit
      end if
    end do
    check_opt = .true.
    if (present(check)) then
      check_opt = check
    end if
    if (check_opt) then
      if (.not. found) then
        errmsg = "Programming error in memory manager. Variable '" //            &
          trim(name) // "' in origin '" // trim(origin) // "' cannot be " //     &
          "assigned because it does not exist in memory manager."
        call store_error(errmsg)
        call ustop()
      end if
    end if
    !
    ! -- return
    return
  end subroutine get_from_memorylist
  
  subroutine allocate_error(varname, origin, istat, isize)
! ******************************************************************************
! Issue allocation error message and stop program execution 
!
! -- Arguments are as follows:
!       VARNAME      : variable name
!       ORIGIN       : variable origin
!       ISTAT        : status code
!       ISIZE        : variable size (flattened)   
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: origin
    ! -- local
    character(len=20) :: csize
    character(len=20) :: cstat
    integer(I4B), intent(in) :: istat
    integer(I4B), intent(in) :: isize
    ! -- code
    !
    ! -- initialize character variables
    write(csize, '(i0)') isize
    write(cstat, '(i0)') istat
    !
    ! -- create error message
    errmsg = "Error trying to allocate memory. Origin '" // trim(origin) //      &
      "' variable name '" // trim(varname) // "' size '" // trim(csize) //       &
      "'. Error message is '" // trim(adjustl(errmsg)) //                        &
      "'. Status code is " //  trim(cstat) // '.'
    !
    ! -- store error and stop program execution
    call store_error(errmsg)
    call ustop()
  end subroutine allocate_error
  
  subroutine check_varname(name)
! ******************************************************************************
! Check the size of the variable name. Stop program execution if the variable
! name exceeds LENVARNAME
!
! -- Arguments are as follows:
!       NAME         : variable name
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: name
    ! -- local
    ! -- code
    !
    ! -- evaluate the length of the variable name
    if(len(name) > LENVARNAME) then
      !
      ! -- create error message
      write(errmsg, '(*(G0))')                                                   &
        'Programming error in Memory Manager. Variable ', name, ' must be ',     &
        LENVARNAME, ' characters or less.'
      !
      ! -- store error and stop program execution
      call store_error(errmsg)
      call ustop()
    end if
  end subroutine check_varname

  subroutine allocate_logical(sclr, name, origin)
! ******************************************************************************
! Allocate a logical scalar 
!
! -- Arguments are as follows:
!       SCLR         : returned logical scalar
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    logical(LGP), pointer, intent(inout) :: sclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    ! -- code
    !
    ! -- check varible name length
    call check_varname(name)
    !
    ! -- allocate the logical scalar
    allocate(sclr, stat=istat, errmsg=errmsg)
    if(istat /= 0) then
      call allocate_error(name, origin, istat, 1)
    end if
    !
    ! -- update counter
    nvalues_alogical = nvalues_alogical + 1
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%logicalsclr => sclr
    mt%isize = 1
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a)") 'LOGICAL'
    !
    ! -- add memory type to the memory list
    call memorylist%add(mt)
    !
    ! -- return
    return
  end subroutine allocate_logical

  subroutine allocate_str(sclr, ilen, name, origin)
! ******************************************************************************
! Allocate a character scalar 
!
! -- Arguments are as follows:
!       SCLR         : returned character scalar
!       ILEN         : character length
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), intent(in) :: ilen
    character(len=ilen), pointer, intent(inout) :: sclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    integer(I4B) :: istat
    type(MemoryType), pointer :: mt
    ! -- format
    ! -- code
    !
    ! -- make sure ilen is greater than 0
    if (ilen < 1) then
      errmsg = 'Programming error in allocate_str. ILEN must be greater than 0.' 
      call store_error(errmsg)
      call ustop()
    end if
    !
    ! -- check varible name length
    call check_varname(name)
    !
    ! -- allocate string
    allocate(character(len=ilen) :: sclr, stat=istat, errmsg=errmsg)
    if (istat /= 0) then
      call allocate_error(name, origin, istat, 1)
    end if
    !
    ! -- set sclr to a empty string
    sclr = ' '
    !
    ! -- update counter
    nvalues_astr = nvalues_astr + ilen
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%isize = ilen
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' LEN=',i0)") 'STRING', ilen
    !
    ! -- add defined length string to the memory manager list
    call memorylist%add(mt)
    !
    ! -- return
    return
  end subroutine allocate_str
  
  subroutine allocate_str1d(astr, ilen, nrow, name, origin)
! ******************************************************************************
! Allocate a 1-dimensional defined length string array 
!
! -- Arguments are as follows:
!       ASTR         : returned defined length 1-dimensional character array
!       ILEN         : character length
!       NROW         : character array number of rows
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy variables
    integer(I4B), intent(in) :: ilen
    integer(I4B), intent(in) :: nrow
    character(len=ilen), dimension(:), pointer, contiguous, intent(inout) :: astr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local variables
    type(MemoryType), pointer :: mt
    character(len=ilen) :: string
    integer(I4B) :: n
    integer(I4B) :: istat
    integer(I4B) :: isize
    ! -- code
    !
    ! -- initialize string
    string = ''
    !
    ! -- make sure ilen is greater than 0
    if (ilen < 1) then
      errmsg = 'Programming error in allocate_str1d. ' //                        &
        'ILEN must be greater than 0.' 
      call store_error(errmsg)
      call ustop()
    end if
    !
    ! -- check varible name length
    call check_varname(name)
    !
    ! -- calculate isize
    isize = ilen * nrow
    !
    ! -- allocate defined length string array
    allocate(character(len=ilen) :: astr(nrow), stat=istat, errmsg=errmsg)
    !
    ! -- check for error condition
    if (istat /= 0) then
      call allocate_error(name, origin, istat, isize)
    end if
    !
    ! -- fill deferred length string with empty string
    do n = 1, nrow
      astr(n) = string
    end do
    !
    ! -- update counter
    nvalues_astr = nvalues_astr + isize
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' LEN=',i0,' (',i0,')')") 'STRING', ilen, nrow
    !
    ! -- add deferred length character array to the memory manager list
    call memorylist%add(mt)
    !
    ! -- return
    return
  end subroutine allocate_str1d

  subroutine allocate_int(sclr, name, origin, memtype)
! ******************************************************************************
! Allocate a integer scalar 
!
! -- Arguments are as follows:
!       SCLR         : returned integer scalar
!       NAME         : variable name
!       ORIGIN       : variable origin
!       MEMTYPE      : optional integer value that defines memaccess for 
!                      variable name. valid values are MEMHIDDEN, MEMREADONLY, 
!                      and MEMREADWRITE.  
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), pointer, intent(inout) :: sclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(in), optional :: memtype
    ! -- local
    type(MemoryType), pointer :: mt
    integer(I4B) :: istat
    ! -- code
    !
    ! -- check variable name length
    call check_varname(name)
    !
    ! -- allocate integer scalar
    allocate(sclr, stat=istat, errmsg=errmsg)
    if (istat /= 0) then
      call allocate_error(name, origin, istat, 1)
    end if
    !
    ! -- update counter
    nvalues_aint = nvalues_aint + 1
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%intsclr => sclr
    mt%isize = 1
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a)") 'INTEGER'
    !
    ! -- set memory access permission
    if (present(memtype)) then
      mt%memaccess = memtype
    end if
    !
    ! -- add memory type to the memory list
    call memorylist%add(mt)
    !
    ! -- return
    return
  end subroutine allocate_int
  
  subroutine allocate_int1d(aint, nrow, name, origin, memtype)
! ******************************************************************************
! Allocate a 1-dimensional integer array 
!
! -- Arguments are as follows:
!       AINT         : returned 1-dimensional integer array
!       NROW        : integer array number of rows
!       NAME         : variable name
!       ORIGIN       : variable origin
!       MEMTYPE      : optional integer value that defines memaccess for 
!                      variable name. valid values are MEMHIDDEN, MEMREADONLY, 
!                      and MEMREADWRITE.  
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: aint
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(in), optional :: memtype
    ! --local
    type(MemoryType), pointer :: mt
    integer(I4B) :: istat
    integer(I4B) :: isize
    ! -- code
    !
    ! -- check variable name length
    call check_varname(name)
    !
    ! -- set isize
    isize = nrow
    !
    ! -- allocate integer array
    allocate(aint(nrow), stat=istat, errmsg=errmsg)
    if (istat /= 0) then
      call allocate_error(name, origin, istat, isize)
    end if
    !
    ! -- update counter
    nvalues_aint = nvalues_aint + isize
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%aint1d => aint
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,')')") 'INTEGER', isize
    !
    ! -- set memory access permission
    if (present(memtype)) then
      mt%memaccess = memtype
    end if
    !
    ! -- add memory type to the memory list
    call memorylist%add(mt)
    !
    ! -- return
    return
  end subroutine allocate_int1d
  
  subroutine allocate_int2d(aint, ncol, nrow, name, origin, memtype)
! ******************************************************************************
! Allocate a 2-dimensional integer array 
!
! -- Arguments are as follows:
!       AINT         : returned 2-dimensional integer array
!       NCOL         : integer array number of columns
!       NROW         : integer array number of rows
!       NAME         : variable name
!       ORIGIN       : variable origin
!       MEMTYPE      : optional integer value that defines memaccess for 
!                      variable name. valid values are MEMHIDDEN, MEMREADONLY, 
!                      and MEMREADWRITE.  
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:, :), pointer, contiguous, intent(inout) :: aint
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(in), optional :: memtype
    ! -- local
    type(MemoryType), pointer :: mt
    integer(I4B) :: istat
    integer(I4B) :: isize
    ! -- code
    !
    ! -- check the variable name length
    call check_varname(name)
    !
    ! -- set isize
    isize = ncol * nrow
    !
    ! -- allocate the integer array
    allocate(aint(ncol, nrow), stat=istat, errmsg=errmsg)
    if (istat /= 0) then
      call allocate_error(name, origin, istat, isize)
    end if
    !
    ! -- update the counter
    nvalues_aint = nvalues_aint + isize
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%aint2d => aint
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,',',i0,')')") 'INTEGER', ncol, nrow
    !
    ! -- set memory access permission
    if (present(memtype)) then
      mt%memaccess = memtype
    end if
    !
    ! -- add memory type to the memory list
    call memorylist%add(mt)
    !
    ! -- return
  end subroutine allocate_int2d
    
  subroutine allocate_int3d(aint, ncol, nrow, nlay, name, origin, memtype)
! ******************************************************************************
! Allocate a 3-dimensional integer array 
!
! -- Arguments are as follows:
!       AINT         : returned 3-dimensional integer array
!       NCOL         : integer array number of columns
!       NROW         : integer array number of rows
!       NLAY         : integer array number of layers
!       NAME         : variable name
!       ORIGIN       : variable origin
!       MEMTYPE      : optional integer value that defines memaccess for 
!                      variable name. valid values are MEMHIDDEN, MEMREADONLY, 
!                      and MEMREADWRITE.  
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:, :, :), pointer, contiguous, intent(inout) :: aint
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    integer(I4B), intent(in) :: nlay
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(in), optional :: memtype
    ! -- local
    type(MemoryType), pointer :: mt
    integer(I4B) :: istat
    integer(I4B) :: isize
    ! -- code
    !
    ! -- check variable name length
    call check_varname(name)
    !
    ! -- set isize
    isize = ncol * nrow * nlay
    !
    ! -- allocate integer array
    allocate(aint(ncol, nrow, nlay), stat=istat, errmsg=errmsg)
    if(istat /= 0) then
      call allocate_error(name, origin, istat, isize)
    end if
    !
    ! -- update counter 
    nvalues_aint = nvalues_aint + isize
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%aint3d => aint
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,',',i0,',',i0,')')") 'INTEGER', ncol,          &
                                                       nrow, nlay
    !
    ! -- set memory access permission
    if (present(memtype)) then
      mt%memaccess = memtype
    end if
    !
    ! -- add memory type to the memory list
    call memorylist%add(mt)
    !
    ! -- return
    return
  end subroutine allocate_int3d

  subroutine allocate_dbl(sclr, name, origin, memtype)
! ******************************************************************************
! Allocate a real scalar 
!
! -- Arguments are as follows:
!       SCLR         : returned real scalar
!       NAME         : variable name
!       ORIGIN       : variable origin
!       MEMTYPE      : optional integer value that defines memaccess for 
!                      variable name. valid values are MEMHIDDEN, MEMREADONLY, 
!                      and MEMREADWRITE.  
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), pointer, intent(inout) :: sclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(in), optional :: memtype
    ! -- local
    type(MemoryType), pointer :: mt
    integer(I4B) :: istat
    ! -- code
    !
    ! -- check variable name length
    call check_varname(name)
    !
    ! -- allocate real scalar
    allocate(sclr, stat=istat, errmsg=errmsg)
    if (istat /= 0) then
      call allocate_error(name, origin, istat, 1)
    end if
    !
    ! -- update counter
    nvalues_aint = nvalues_aint + 1
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%dblsclr => sclr
    mt%isize = 1
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a)") 'DOUBLE'
    !
    ! -- set memory access permission
    if (present(memtype)) then
      mt%memaccess = memtype
    end if
    !
    ! -- add memory type to the memory list
    call memorylist%add(mt)
    !
    ! -- return
    return
  end subroutine allocate_dbl
  
  subroutine allocate_dbl1d(adbl, nrow, name, origin, memtype)
! ******************************************************************************
! Allocate a 1-dimensional real array 
!
! -- Arguments are as follows:
!       ADBL         : returned 1-dimensional real array
!       NROW         : real array number of rows
!       NAME         : variable name
!       ORIGIN       : variable origin
!       MEMTYPE      : optional integer value that defines memaccess for 
!                      variable name. valid values are MEMHIDDEN, MEMREADONLY, 
!                      and MEMREADWRITE.  
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(in), optional :: memtype
    ! -- local
    type(MemoryType), pointer :: mt
    integer(I4B) :: istat
    integer(I4B) :: isize
    ! -- code
    !
    ! -- check the variable name length
    call check_varname(name)
    !
    ! -- set isize
    isize = nrow
    !
    ! -- allocate the real array
    allocate(adbl(nrow), stat=istat, errmsg=errmsg)
    if (istat /= 0) then
      call allocate_error(name, origin, istat, isize)
    end if
    !
    ! -- update counter
    nvalues_adbl = nvalues_adbl + isize
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%adbl1d => adbl
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,')')") 'DOUBLE', isize
    !
    ! -- set memory access permission
    if (present(memtype)) then
      mt%memaccess = memtype
    end if
    !
    ! -- add memory type to the memory list
    call memorylist%add(mt)
    !
    ! -- return
    return
  end subroutine allocate_dbl1d
  
  subroutine allocate_dbl2d(adbl, ncol, nrow, name, origin, memtype)
! ******************************************************************************
! Allocate a 2-dimensional real array 
!
! -- Arguments are as follows:
!       ADBL         : returned 2-dimensional real array
!       NCOL         : real array number of columns
!       NROW         : real array number of rows
!       NAME         : variable name
!       ORIGIN       : variable origin
!       MEMTYPE      : optional integer value that defines memaccess for 
!                      variable name. valid values are MEMHIDDEN, MEMREADONLY, 
!                      and MEMREADWRITE.  
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: adbl
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(in), optional :: memtype
    ! -- local
    type(MemoryType), pointer :: mt
    integer(I4B) :: istat
    integer(I4B) :: isize
    ! -- code
    !
    ! -- check the variable name length
    call check_varname(name)
    !
    ! -- set isize
    isize = ncol * nrow
    !
    ! -- allocate the real array
    allocate(adbl(ncol, nrow), stat=istat, errmsg=errmsg)
    if (istat /= 0) then
      call allocate_error(name, origin, istat, isize)
    end if
    !
    ! -- update counter
    nvalues_adbl = nvalues_adbl + isize
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%adbl2d => adbl
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,',',i0,')')") 'DOUBLE', ncol, nrow
    !
    ! -- set memory access permission
    if (present(memtype)) then
      mt%memaccess = memtype
    end if
    !
    ! -- add memory type to the memory list
    call memorylist%add(mt)
    !
    ! -- return
    return
  end subroutine allocate_dbl2d
  
  subroutine allocate_dbl3d(adbl, ncol, nrow, nlay, name, origin, memtype)
! ******************************************************************************
! Allocate a 3-dimensional real array 
!
! -- Arguments are as follows:
!       ADBL         : returned 3-dimensional real array
!       NCOL         : real array number of columns
!       NROW         : real array number of rows
!       NLAY         : real array number of layers
!       NAME         : variable name
!       ORIGIN       : variable origin
!       MEMTYPE      : optional integer value that defines memaccess for 
!                      variable name. valid values are MEMHIDDEN, MEMREADONLY, 
!                      and MEMREADWRITE.  
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:, :, :), pointer, contiguous, intent(inout) :: adbl
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    integer(I4B), intent(in) :: nlay
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    integer(I4B), intent(in), optional :: memtype
    ! -- local
    type(MemoryType), pointer :: mt
    integer(I4B) :: istat
    integer(I4B) :: isize
    ! -- code
    !
    ! -- check the variable name length
    call check_varname(name)
    !
    ! -- set isize
    isize = ncol * nrow * nlay
    !
    ! -- allocate the real array
    allocate(adbl(ncol, nrow, nlay), stat=istat, errmsg=errmsg)
    if (istat /= 0) then
      call allocate_error(name, origin, istat, isize)
    end if
    !
    ! -- update the counter
    nvalues_adbl = nvalues_adbl + isize
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%adbl3d => adbl
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,',',i0,',',i0,')')") 'DOUBLE', ncol,           &
                                                       nrow, nlay
    !
    ! -- set memory access permission
    if (present(memtype)) then
      mt%memaccess = memtype
    end if
    !
    ! -- add memory type to the memory list
    call memorylist%add(mt)
    !
    ! -- return
    return
  end subroutine allocate_dbl3d
  
  subroutine checkin_int1d(aint, name, origin, name2, origin2, memtype)
! ******************************************************************************
! Check in am existing 1-dimensional integer array to the memory manager
!
! -- Arguments are as follows:
!       AINT         : returned 1-dimensional integer array
!       NAME         : variable name
!       ORIGIN       : variable origin
!       NAME2        : second variable name
!       ORIGIN2      : second variable origin
!       MEMTYPE      : optional integer value that defines memaccess for 
!                      variable name. valid values are MEMHIDDEN, MEMREADONLY, 
!                      and MEMREADWRITE.  
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: name2
    character(len=*), intent(in) :: origin2
    integer(I4B), intent(in), optional :: memtype
    ! --local
    type(MemoryType), pointer :: mt
    integer(I4B) :: isize
    ! -- code
    !
    ! -- check variable name length
    call check_varname(name)
    !
    ! -- set isize
    isize = size(aint)
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%aint1d => aint
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,')')") 'INTEGER', isize
    !
    ! -- set master information
    mt%master = .false.
    mt%mastername = name2
    mt%masterorigin = origin2
    !
    ! -- set memory access permission
    if (present(memtype)) then
      mt%memaccess = memtype
    end if
    !
    ! -- add memory type to the memory list
    call memorylist%add(mt)
    !
    ! -- return
    return
  end subroutine checkin_int1d
  
  subroutine checkin_dbl1d(adbl, name, origin, name2, origin2, memtype)
! ******************************************************************************
! Check in an existing 1-dimensional real array to the memory manager
!
! -- Arguments are as follows:
!       ADBL         : returned 1-dimensional real array
!       NAME         : variable name
!       ORIGIN       : variable origin
!       NAME2        : second variable name
!       ORIGIN2      : second variable origin
!       MEMTYPE      : optional integer value that defines memaccess for 
!                      variable name. valid values are MEMHIDDEN, MEMREADONLY, 
!                      and MEMREADWRITE.  
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: name2
    character(len=*), intent(in) :: origin2
    integer(I4B), intent(in), optional :: memtype
    ! -- local
    type(MemoryType), pointer :: mt
    integer(I4B) :: isize
    ! -- code
    !
    ! -- check the variable name length
    call check_varname(name)
    !
    ! -- set isize
    isize = size(adbl)
    !
    ! -- allocate memory type
    allocate(mt)
    !
    ! -- set memory type
    mt%adbl1d => adbl
    mt%isize = isize
    mt%name = name
    mt%origin = origin
    write(mt%memtype, "(a,' (',i0,')')") 'DOUBLE', isize
    !
    ! -- set master information
    mt%master = .false.
    mt%mastername = name2
    mt%masterorigin = origin2
    !
    ! -- set memory access permission
    if (present(memtype)) then
      mt%memaccess = memtype
    end if
    !
    ! -- add memory type to the memory list
    call memorylist%add(mt)
    !
    ! -- return
    return
  end subroutine checkin_dbl1d
  
  subroutine reallocate_str1d(astr, ilen, nrow, name, origin)
! ******************************************************************************
! Reallocate a 1-dimensional defined length string array 
!
! -- Arguments are as follows:
!       ASTR         : returned defined length 1-dimensional character array
!       ILEN         : character length
!       NROW         : character array number of rows
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), intent(in) :: ilen
    integer(I4B), intent(in) :: nrow
    character(len=ilen), dimension(:), pointer, contiguous, intent(inout) :: astr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    character(len=ilen), dimension(:), allocatable :: astrtemp
    integer(I4B) :: istat
    integer(I4B) :: isize
    integer(I4B) :: isize_old
    integer(I4B) :: nrow_old
    integer(I4B) :: n
    !
    ! -- Find and assign mt
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- reallocate astr1d
    if (found) then
      isize_old = mt%isize
      if (isize_old > 0) then
        nrow_old = size(astr)
      else
        nrow_old = 0
      end if
      !
      ! -- calculate isize
      isize = ilen * nrow
      !
      ! -- allocate astrtemp
      allocate(astrtemp(nrow), stat=istat, errmsg=errmsg)
      if (istat /= 0) then
        call allocate_error(name, origin, istat, isize)
      end if
      !
      ! -- copy existing values
      do n = 1, nrow_old
        astrtemp(n) = astr(n)
      end do
      !
      ! -- fill new values with missing values
      do n = nrow_old + 1, nrow
        astrtemp(n) = ''
      end do
      !
      ! -- deallocate mt pointer, repoint, recalculate isize
      if (isize_old > 0) then
        deallocate(astr)
      end if
      !
      ! -- allocate astr1d
      allocate(astr(nrow), stat=istat, errmsg=errmsg)
      if (istat /= 0) then
        call allocate_error(name, origin, istat, isize)
      end if
      !
      ! -- fill the reallocate character array
      do n = 1, nrow
        astr(n) = astrtemp(n) 
      end do
      !
      ! -- deallocate temporary storage
      deallocate(astrtemp)
      !
      ! -- reset memory manager values
      mt%isize = isize
      mt%nrealloc = mt%nrealloc + 1
      mt%master = .true.
      nvalues_astr = nvalues_astr + isize - isize_old
      write(mt%memtype, "(a,' LEN=',i0,' (',i0,')')") 'STRING', ilen, nrow
    else
      errmsg = "Programming error, varible '" // trim(name) // "' from '" //     &
        trim(origin) // "' is not defined in the memory manager. Use " //        &
        "mem_allocate instead."
      call store_error(errmsg)
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine reallocate_str1d
  
  subroutine reallocate_int1d(aint, nrow, name, origin)
! ******************************************************************************
! Reallocate a 1-dimensional integer array 
!
! -- Arguments are as follows:
!       AINT         : returned 1-dimensional integer array
!       NROW         : integer array number of rows
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: aint
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: istat
    integer(I4B) :: isize
    integer(I4B) :: i
    integer(I4B) :: isizeold
    integer(I4B) :: ifill
    ! -- code
    !
    ! -- Find and assign mt
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- Allocate aint and then refill
    isize = nrow
    isizeold = size(mt%aint1d)
    ifill = min(isizeold, isize)
    allocate(aint(nrow), stat=istat, errmsg=errmsg)
    if(istat /= 0) then
      call allocate_error(name, origin, istat, isize)
    end if
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
! ******************************************************************************
! Reallocate a 2-dimensional integer array 
!
! -- Arguments are as follows:
!       AINT         : returned 2-dimensional integer array
!       NCOL         : integer array number of columns
!       NROW         : integer array number of rows
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:, :), pointer, contiguous, intent(inout) :: aint
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: istat
    integer(I4B), dimension(2) :: ishape
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: isize
    integer(I4B) :: isizeold
    ! -- code
    !
    ! -- Find and assign mt
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- Allocate aint and then refill
    ishape = shape(mt%aint2d)
    isize = nrow * ncol
    isizeold = ishape(1) * ishape(2)
    allocate(aint(ncol, nrow), stat=istat, errmsg=errmsg)
    if (istat /= 0) then
      call allocate_error(name, origin, istat, isize)
    end if
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
  
  subroutine reallocate_dbl1d(adbl, nrow, name, origin)
! ******************************************************************************
! Reallocate a 1-dimensional real array 
!
! -- Arguments are as follows:
!       ADBL         : returned 1-dimensional real array
!       NCOL         : real array number of columns
!       NROW         : real array number of rows
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    integer(I4B) :: istat
    integer(I4B) :: isize
    integer(I4B) :: i
    integer(I4B) :: isizeold
    integer(I4B) :: ifill
    logical(LGP) :: found
    ! -- code
    !
    ! -- Find and assign mt
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- Allocate adbl and then refill
    isize = nrow
    isizeold = size(mt%adbl1d)
    ifill = min(isizeold, isize)
    allocate(adbl(nrow), stat=istat, errmsg=errmsg)
    if (istat /= 0) then
      call allocate_error(name, origin, istat, isize)
    end if
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
! ******************************************************************************
! Reallocate a 2-dimensional real array 
!
! -- Arguments are as follows:
!       ADBL         : returned 2-dimensional real array
!       NCOL         : real array number of columns
!       NROW         : real array number of rows
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: adbl
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: istat
    integer(I4B), dimension(2) :: ishape
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: isize
    integer(I4B) :: isizeold
    ! -- code
    !
    ! -- Find and assign mt
    call get_from_memorylist(name, origin, mt, found)
    !
    ! -- Allocate adbl and then refill
    ishape = shape(mt%adbl2d)
    isize = nrow * ncol
    isizeold = ishape(1) * ishape(2)
    allocate(adbl(ncol, nrow), stat=istat, errmsg=errmsg)
    if(istat /= 0) then
      call allocate_error(name, origin, istat, isize)
    end if
    do i = 1, ishape(2)
      do j = 1, ishape(1)
        adbl(j, i) = mt%adbl2d(j, i)
      end do
    end do
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
  
  subroutine setptr_logical(sclr, name, origin)
! ******************************************************************************
! Set pointer to a logical scalar 
!
! -- Arguments are as follows:
!       SCLR         : returned pointer to a logical scalar
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    logical(LGP), pointer, intent(inout) :: sclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    ! -- code
    call get_from_memorylist(name, origin, mt, found)    
    sclr => mt%logicalsclr
    !
    ! -- return
    return
  end subroutine setptr_logical
  
  subroutine setptr_int(sclr, name, origin)
! ******************************************************************************
! Set pointer to a integer scalar 
!
! -- Arguments are as follows:
!       SCLR         : returned pointer to a integer scalar
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), pointer, intent(inout) :: sclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    ! -- code
    call get_from_memorylist(name, origin, mt, found)    
    sclr => mt%intsclr
    !
    ! -- return
    return
  end subroutine setptr_int
  
  subroutine setptr_int1d(aint, name, origin)
! ******************************************************************************
! Set pointer to a 1-dimensional integer array 
!
! -- Arguments are as follows:
!       AINT         : returned pointer to a 1-dimensional integer array
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    ! -- code
    call get_from_memorylist(name, origin, mt, found)    
    aint => mt%aint1d
    !
    ! -- return
    return
  end subroutine setptr_int1d
  
  subroutine setptr_int2d(aint, name, origin)
! ******************************************************************************
! Set pointer to a 2-dimensional integer array 
!
! -- Arguments are as follows:
!       AINT         : returned pointer to a 2-dimensional integer array
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:, :), pointer, contiguous, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    ! -- code
    call get_from_memorylist(name, origin, mt, found)    
    aint => mt%aint2d
    !
    ! -- return
    return
  end subroutine setptr_int2d
  
  subroutine setptr_dbl(sclr, name, origin)
! ******************************************************************************
! Set pointer to a real scalar 
!
! -- Arguments are as follows:
!       SCLR         : returned pointer to a real scalar
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), pointer, intent(inout) :: sclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    ! -- code
    call get_from_memorylist(name, origin, mt, found)    
    sclr => mt%dblsclr
    !
    ! -- return
    return
  end subroutine setptr_dbl
  
  subroutine setptr_dbl1d(adbl, name, origin)
! ******************************************************************************
! Set pointer to a 1-dimensional real array 
!
! -- Arguments are as follows:
!       ADBL         : returned pointer to a 1-dimensional real array
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    ! -- code
    call get_from_memorylist(name, origin, mt, found)    
    adbl => mt%adbl1d
    !
    ! -- return
    return
  end subroutine setptr_dbl1d
  
  subroutine setptr_dbl2d(adbl, name, origin)
! ******************************************************************************
! Set pointer to a 2-dimensional real array 
!
! -- Arguments are as follows:
!       ADBL         : returned pointer to a 2-dimensional real array
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    ! -- code
    call get_from_memorylist(name, origin, mt, found)    
    adbl => mt%adbl2d
    !
    ! -- return
    return
  end subroutine setptr_dbl2d

  subroutine copyptr_int1d(aint, name, origin, origin2)
! ******************************************************************************
! Make a copy of a 1-dimensional integer array 
!
! -- Arguments are as follows:
!       AINT         : returned copy of a 1-dimensional integer array
!       NAME         : variable name
!       ORIGIN       : variable origin
!       ORIGIN2      : optional additional variable origin. If passed then the
!                      copy is added to the memory manager
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in), optional :: origin2
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: n
    ! -- code
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
    !
    ! -- return
    return
  end subroutine copyptr_int1d

  subroutine copyptr_int2d(aint, name, origin, origin2)
! ******************************************************************************
! Make a copy of a 2-dimensional integer array 
!
! -- Arguments are as follows:
!       AINT         : returned copy of a 2-dimensional integer array
!       NAME         : variable name
!       ORIGIN       : variable origin
!       ORIGIN2      : optional additional variable origin. If passed then the
!                      copy is added to the memory manager
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:,:), pointer, contiguous, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in), optional :: origin2
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: ncol
    integer(I4B) :: nrow
    ! -- code
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
    !
    ! -- return
    return
  end subroutine copyptr_int2d
  
  subroutine copyptr_dbl1d(adbl, name, origin, origin2)
! ******************************************************************************
! Make a copy of a 1-dimensional real array 
!
! -- Arguments are as follows:
!       ADBL         : returned copy of a 1-dimensional real array
!       NAME         : variable name
!       ORIGIN       : variable origin
!       ORIGIN2      : optional additional variable origin. If passed then the
!                      copy is added to the memory manager
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in), optional :: origin2
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: n
    ! -- code
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
    !
    ! -- return
    return
  end subroutine copyptr_dbl1d

  subroutine copyptr_dbl2d(adbl, name, origin, origin2)
! ******************************************************************************
! Make a copy of a 2-dimensional real array 
!
! -- Arguments are as follows:
!       ADBL         : returned copy of a 2-dimensional real array
!       NAME         : variable name
!       ORIGIN       : variable origin
!       ORIGIN2      : optional additional variable origin. If passed then the
!                      copy is added to the memory manager
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:,:), pointer, contiguous, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in), optional :: origin2
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: ncol
    integer(I4B) :: nrow
    ! -- code
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
    !
    ! -- return
    return
  end subroutine copyptr_dbl2d
  
  subroutine copy_dbl1d(adbl, name, origin)
! ******************************************************************************
! Copy values from a 1-dimensional real array in the memory manage to a passed
! 1-dimensional real array
!
! -- Arguments are as follows:
!       ADBL         : returned 1-dimensional real array
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:), intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: n
    ! -- code
    call get_from_memorylist(name, origin, mt, found)
    do n = 1, size(mt%adbl1d)
      adbl(n) = mt%adbl1d(n)
    end do
    !
    ! -- return
    return
  end subroutine copy_dbl1d
  
  subroutine reassignptr_int1d(aint, name, origin, name2, origin2)
! ******************************************************************************
! Set the pointer for a 1-dimensional integer array to a second 1-dimensional 
! integer array already in the memory manager 
!
! -- Arguments are as follows:
!       AINT         : returned 2-dimensional integer array
!       NAME         : variable name
!       ORIGIN       : variable origin
!       NAME2        : second variable name
!       ORIGIN2      : second variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: name2
    character(len=*), intent(in) :: origin2
    ! -- local
    type(MemoryType), pointer :: mt
    type(MemoryType), pointer :: mt2
    logical(LGP) :: found
    ! -- code
    call get_from_memorylist(name, origin, mt, found)
    call get_from_memorylist(name2, origin2, mt2, found)
    if (size(aint) > 0) then
      nvalues_aint = nvalues_aint - size(aint)
      deallocate(aint)
    end if
    aint => mt2%aint1d
    mt%aint1d => aint
    mt%isize = size(aint) 
    write(mt%memtype, "(a,' (',i0,')')") 'INTEGER', mt%isize
    !
    ! -- set master information
    mt%master = .false.
    mt%mastername = name2
    mt%masterorigin = origin2
    !
    ! -- return
    return
  end subroutine reassignptr_int1d

  subroutine reassignptr_int2d(aint, name, origin, name2, origin2)
! ******************************************************************************
! Set the pointer for a 2-dimensional integer array to a second 2-dimensional 
! integer array already in the memory manager 
!
! -- Arguments are as follows:
!       AINT         : returned 2-dimensional integer array
!       NAME         : variable name
!       ORIGIN       : variable origin
!       NAME2        : second variable name
!       ORIGIN2      : second variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:,:), pointer, contiguous, intent(inout) :: aint
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: name2
    character(len=*), intent(in) :: origin2
    ! -- local
    type(MemoryType), pointer :: mt
    type(MemoryType), pointer :: mt2
    logical(LGP) :: found
    integer(I4B) :: ncol
    integer(I4B) :: nrow
    ! -- code
    call get_from_memorylist(name, origin, mt, found)
    call get_from_memorylist(name2, origin2, mt2, found)
    if (size(aint) > 0) then
      nvalues_aint = nvalues_aint - size(aint)
      deallocate(aint)
    end if
    aint => mt2%aint2d
    mt%aint2d => aint
    mt%isize = size(aint)
    ncol = size(aint, dim=1)
    nrow = size(aint, dim=2)
    write(mt%memtype, "(a,' (',i0,',',i0,')')") 'INTEGER', ncol, nrow
    !
    ! -- set master information
    mt%master = .false.
    mt%mastername = name2
    mt%masterorigin = origin2
    !
    ! -- return
    return
  end subroutine reassignptr_int2d

  subroutine reassignptr_dbl1d(adbl, name, origin, name2, origin2)
! ******************************************************************************
! Set the pointer for a 1-dimensional real array to a second 1-dimensional 
! real array already in the memory manager 
!
! -- Arguments are as follows:
!       ADBL         : returned 1-dimensional real array
!       NAME         : variable name
!       ORIGIN       : variable origin
!       NAME2        : second variable name
!       ORIGIN2      : second variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: name2
    character(len=*), intent(in) :: origin2
    ! -- local
    type(MemoryType), pointer :: mt
    type(MemoryType), pointer :: mt2
    logical(LGP) :: found
    ! -- code
    call get_from_memorylist(name, origin, mt, found)
    call get_from_memorylist(name2, origin2, mt2, found)
    if (size(adbl) > 0) then
      nvalues_adbl = nvalues_adbl - size(adbl)
      deallocate(adbl)
    end if
    adbl => mt2%adbl1d
    mt%adbl1d => adbl
    mt%isize = size(adbl) 
    write(mt%memtype, "(a,' (',i0,')')") 'DOUBLE', mt%isize
    !
    ! -- set master information
    mt%master = .false.
    mt%mastername = name2
    mt%masterorigin = origin2
    !
    ! -- return
    return
  end subroutine reassignptr_dbl1d

  subroutine reassignptr_dbl2d(adbl, name, origin, name2, origin2)
! ******************************************************************************
! Set the pointer for a 2-dimensional real array to a second 2-dimensional 
! real array already in the memory manager 
!
! -- Arguments are as follows:
!       ADBL         : returned 2-dimensional real array
!       NAME         : variable name
!       ORIGIN       : variable origin
!       NAME2        : second variable name
!       ORIGIN2      : second variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:,:), pointer, contiguous, intent(inout) :: adbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: name2
    character(len=*), intent(in) :: origin2
    ! -- local
    type(MemoryType), pointer :: mt
    type(MemoryType), pointer :: mt2
    logical(LGP) :: found
    integer(I4B) :: ncol
    integer(I4b) :: nrow
    ! -- code
    call get_from_memorylist(name, origin, mt, found)
    call get_from_memorylist(name2, origin2, mt2, found)
    if (size(adbl) > 0) then
      nvalues_adbl = nvalues_adbl - size(adbl)
      deallocate(adbl)
    end if
    adbl => mt2%adbl2d
    mt%adbl2d => adbl
    mt%isize = size(adbl)
    ncol = size(adbl, dim=1)
    nrow = size(adbl, dim=2)
    write(mt%memtype, "(a,' (',i0,',',i0,')')") 'DOUBLE', ncol, nrow
    !
    ! -- set master information
    mt%master = .false.
    mt%mastername = name2
    mt%masterorigin = origin2
    !
    ! -- return
    return
  end subroutine reassignptr_dbl2d

  subroutine deallocate_str(sclr, name, origin)
! ******************************************************************************
! Deallocate a character scalar 
!
! -- Arguments are as follows:
!       SCLR         : character scalar
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy  
    character(len=*), pointer, intent(inout) :: sclr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    ! -- code
    if (associated(sclr)) then
      call get_from_memorylist(name, origin, mt, found, check=.FALSE.)
      if (.not. found) then
        call store_error('Programming error in deallocate_str.')
        call ustop()
      else
        deallocate(sclr)
      end if
    end if
    !
    ! -- return
    return
  end subroutine deallocate_str
  
  subroutine deallocate_str1d(astr, name, origin)
! ******************************************************************************
! Deallocate a defined length character array 
!
! -- Arguments are as follows:
!       ASTR         : defined length character array
!       NAME         : variable name
!       ORIGIN       : variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), dimension(:), pointer, contiguous, intent(inout) :: astr
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    ! -- code
    if (associated(astr)) then
      call get_from_memorylist(name, origin, mt, found, check=.FALSE.)
      if (.not. found) then
        errmsg = "Programming error in deallocate_str1d. Variable '" //          &
          trim(name) // "' from origin '" // trim(origin) // "' is not " //      &
          "present in the memory manager but is associated."
        call store_error(errmsg)
        call ustop()
      else
        deallocate(astr)
      end if
    end if
    !
    ! -- return
    return
  end subroutine deallocate_str1d

  subroutine deallocate_logical(sclr)
! ******************************************************************************
! Deallocate a logical scalar 
!
! -- Arguments are as follows:
!       SCLR         : logical scalar
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    logical(LGP), pointer, intent(inout) :: sclr
    ! -- local
    class(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: ipos
    ! -- code
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(associated(mt%logicalsclr, sclr)) then
        nullify(mt%logicalsclr)
        found = .true.
        exit
      end if
    end do
    if (.not. found) then
      call store_error('programming error in deallocate_logical')
      call ustop()
    else
      if (mt%master) then
        deallocate(sclr)
      else
        nullify(sclr)
      end if
    end if
    !
    ! -- return
    return
  end subroutine deallocate_logical
  
  subroutine deallocate_int(sclr)
! ******************************************************************************
! Deallocate a integer scalar 
!
! -- Arguments are as follows:
!       SCLR         : integer scalar
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), pointer, intent(inout) :: sclr
    ! -- local
    class(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: ipos
    ! -- code
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(associated(mt%intsclr, sclr)) then
        nullify(mt%intsclr)
        found = .true.
        exit
      end if
    end do
    if (.not. found) then
      call store_error('Programming error in deallocate_int.')
      call ustop()
    else
      if (mt%master) then
        deallocate(sclr)
      else
        nullify(sclr)
      end if
    end if
    !
    ! -- return
    return
  end subroutine deallocate_int
  
  subroutine deallocate_dbl(sclr)
! ******************************************************************************
! Deallocate a real scalar 
!
! -- Arguments are as follows:
!       SCLR         : real scalar
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), pointer, intent(inout) :: sclr
    ! -- local
    class(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: ipos
    ! -- code
    found = .false.
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      if(associated(mt%dblsclr, sclr)) then
        nullify(mt%dblsclr)
        found = .true.
        exit
      end if
    end do
    if (.not. found) then
      call store_error('Programming error in deallocate_dbl.')
      call ustop()
    else
      if (mt%master) then
        deallocate(sclr)
      else
        nullify (sclr)
      end if
    end if
    !
    ! -- return
    return
  end subroutine deallocate_dbl
  
  subroutine deallocate_int1d(aint, name, origin)
! ******************************************************************************
! Deallocate a 1-dimensional integer array 
!
! -- Arguments are as follows:
!       AINT         : 1-dimensional integer array
!       NAME         : optional variable name
!       ORIGIN       : optional variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: aint
    character(len=*), optional :: name
    character(len=*), optional :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: ipos
    ! -- code
    !
    ! -- process optional variables
    if (present(name) .and. present(origin)) then
      call get_from_memorylist(name, origin, mt, found)
      nullify(mt%aint1d)
    else
      found = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if (associated(mt%aint1d, aint)) then
          nullify(mt%aint1d)
          found = .true.
          exit
        end if
      end do
    end if
    if (.not. found .and. size(aint) > 0 ) then
      call store_error('programming error in deallocate_int1d')
      call ustop()
    else
      if (mt%master) then
        deallocate(aint)
      else
        nullify(aint)
      end if
    end if
    !
    ! -- return
    return
  end subroutine deallocate_int1d
  
  subroutine deallocate_int2d(aint, name, origin)
! ******************************************************************************
! Deallocate a 2-dimensional integer array 
!
! -- Arguments are as follows:
!       AINT         : 2-dimensional integer array
!       NAME         : optional variable name
!       ORIGIN       : optional variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:, :), pointer, contiguous, intent(inout) :: aint
    character(len=*), optional :: name
    character(len=*), optional :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: ipos
    ! -- code
    !
    ! -- process optional variables
    if (present(name) .and. present(origin)) then
      call get_from_memorylist(name, origin, mt, found)
      nullify(mt%aint2d)
    else
      found = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if(associated(mt%aint2d, aint)) then
          nullify(mt%aint2d)
          found = .true.
          exit
        end if
      end do
    end if
    if (.not. found .and. size(aint) > 0 ) then
      call store_error('programming error in deallocate_int2d')
      call ustop()
    else
      if (mt%master) then
        deallocate(aint)
      else
        nullify(aint)
      end if
    end if
    !
    ! -- return
    return
  end subroutine deallocate_int2d
  
  subroutine deallocate_int3d(aint, name, origin)
! ******************************************************************************
! Deallocate a 3-dimensional integer array 
!
! -- Arguments are as follows:
!       AINT         : 3-dimensional integer array
!       NAME         : optional variable name
!       ORIGIN       : optional variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:, :, :), pointer, contiguous, intent(inout) :: aint
    character(len=*), optional :: name
    character(len=*), optional :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: ipos
    ! -- code
    !
    ! -- process optional variables
    if (present(name) .and. present(origin)) then
      call get_from_memorylist(name, origin, mt, found)
      nullify(mt%aint3d)
    else
      found = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if(associated(mt%aint3d, aint)) then
          nullify(mt%aint3d)
          found = .true.
          exit
        end if
      end do
    end if
    if (.not. found .and. size(aint) > 0 ) then
      call store_error('programming error in deallocate_int3d')
      call ustop()
    else
      if (mt%master) then
        deallocate(aint)
      else
        nullify(aint)
      end if
    end if
    !
    ! -- return
    return
  end subroutine deallocate_int3d
  
  subroutine deallocate_dbl1d(adbl, name, origin)
! ******************************************************************************
! Deallocate a 1-dimensional real array 
!
! -- Arguments are as follows:
!       ADBL         : 1-dimensional real array
!       NAME         : optional variable name
!       ORIGIN       : optional variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: adbl
    character(len=*), optional :: name
    character(len=*), optional :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: ipos
    ! -- code
    !
    ! -- process optional variables
    if (present(name) .and. present(origin)) then
      call get_from_memorylist(name, origin, mt, found)
      nullify(mt%adbl1d)
    else
      found = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if(associated(mt%adbl1d, adbl)) then
          nullify(mt%adbl1d)
          found = .true.
          exit
        end if
      end do
    end if
    if (.not. found .and. size(adbl) > 0 ) then
      call store_error('programming error in deallocate_dbl1d')
      call ustop()
    else
      if (mt%master) then
        deallocate(adbl)
      else
        nullify(adbl)
      end if
    end if
    !
    ! -- return
    return
  end subroutine deallocate_dbl1d
  
  subroutine deallocate_dbl2d(adbl, name, origin)
! ******************************************************************************
! Deallocate a 2-dimensional real array 
!
! -- Arguments are as follows:
!       ADBL         : 2-dimensional real array
!       NAME         : optional variable name
!       ORIGIN       : optional variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: adbl
    character(len=*), optional :: name
    character(len=*), optional :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: ipos
    ! -- code
    !
    ! -- process optional variables
    if (present(name) .and. present(origin)) then
      call get_from_memorylist(name, origin, mt, found)
      nullify(mt%adbl2d)
    else
      found = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if(associated(mt%adbl2d, adbl)) then
          nullify(mt%adbl2d)
          found = .true.
          exit
        end if
      end do
    end if
    if (.not. found .and. size(adbl) > 0 ) then
      call store_error('programming error in deallocate_dbl2d')
      call ustop()
    else
      if (mt%master) then
        deallocate(adbl)
      else
        nullify(adbl)
      end if
    end if
    !
    ! -- return
    return
  end subroutine deallocate_dbl2d
  
  subroutine deallocate_dbl3d(adbl, name, origin)
! ******************************************************************************
! Deallocate a 3-dimensional real array 
!
! -- Arguments are as follows:
!       ADBL         : 3-dimensional real array
!       NAME         : optional variable name
!       ORIGIN       : optional variable origin
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:, :, :), pointer, contiguous, intent(inout) :: adbl
    character(len=*), optional :: name
    character(len=*), optional :: origin
    ! -- local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    integer(I4B) :: ipos
    ! -- code
    !
    ! -- process optional variables
    if (present(name) .and. present(origin)) then
      call get_from_memorylist(name, origin, mt, found)
      nullify(mt%adbl3d)
    else
      found = .false.
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        if(associated(mt%adbl3d, adbl)) then
          nullify(mt%adbl3d)
          found = .true.
          exit
        end if
      end do
    end if
    if (.not. found .and. size(adbl) > 0 ) then
      call store_error('programming error in deallocate_dbl3d')
      call ustop()
    else
      if (mt%master) then
        deallocate(adbl)
      else
        nullify(adbl)
      end if
    end if
    !
    ! -- return
    return
  end subroutine deallocate_dbl3d
  
  subroutine mem_set_print_option(iout, keyword, errmsg)
! ******************************************************************************
! Set the memory print option 
!
! -- Arguments are as follows:
!       IOUT         : unit number for mfsim.lst
!       KEYWORD      : memory print option
!       ERRMSG       : returned error message if keyword is not valid option
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy 
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: keyword
    character(len=*), intent(inout) :: errmsg
    ! -- local
    ! -- format
    ! -- code
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
        errmsg = "Unknown memory print option '" // trim(keyword) // "."
    end select
    return
  end subroutine mem_set_print_option
  
  subroutine mem_summary_table(iout, nrows, cunits)
! ******************************************************************************
! Create a table if memory_print_option is 'SUMMARY' 
!
! -- Arguments are as follows:
!       IOUT         : unit number for mfsim.lst
!       NROWS        : number of table rows
!       CUNITS       : memory units (bytes, kilobytes, megabytes, or gigabytes)
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(in) :: nrows
    character(len=*), intent(in) :: cunits
    ! -- local
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: text
    integer(I4B) :: nterms
    ! -- formats
    ! -- code
    nterms = 6
    !
    ! -- set up table title
    title = 'SUMMARY INFORMATION ON VARIABLES STORED IN THE MEMORY MANAGER, ' // &
            'IN ' // trim(cunits)
    !
    ! -- set up stage tableobj
    call table_cr(memtab, 'MEM SUM', title)
    call memtab%table_df(nrows, nterms, iout)
    !
    ! -- data type
    text = 'COMPONENT'
    call memtab%initialize_column(text, 20, alignment=TABLEFT)
    !
    ! -- memory allocated for characters
    text = 'CHARACTER'
    call memtab%initialize_column(text, 15, alignment=TABCENTER)
    !
    ! -- memory allocated for logical
    text = 'LOGICAL'
    call memtab%initialize_column(text, 15, alignment=TABCENTER)
    !
    ! -- memory allocated for integers
    text = 'INTEGER'
    call memtab%initialize_column(text, 15, alignment=TABCENTER)
    !
    ! -- memory allocated for reals
    text = 'REAL'
    call memtab%initialize_column(text, 15, alignment=TABCENTER)
    !
    ! -- total memory allocated
    text = 'TOTAL'
    call memtab%initialize_column(text, 15, alignment=TABCENTER)
    !
    ! -- return
    return
  end subroutine mem_summary_table 
  
  subroutine mem_detailed_table(iout, nrows)
! ******************************************************************************
! Create a table if memory_print_option is 'ALL' 
!
! -- Arguments are as follows:
!       IOUT         : unit number for mfsim.lst
!       NROWS        : number of table rows
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(in) :: nrows
    ! -- local
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: text
    integer(I4B) :: iptrlen
    integer(I4B) :: nterms
    ! -- formats
    ! -- code
    iptrlen = LENORIGIN + LENVARNAME
    nterms = 6
    !
    ! -- set up table title
    title = 'DETAILED INFORMATION ON VARIABLES STORED IN THE MEMORY MANAGER'
    !
    ! -- set up stage tableobj
    call table_cr(memtab, 'MEM DET', title)
    call memtab%table_df(nrows, nterms, iout)
    !
    ! -- origin
    text = 'ORIGIN'
    call memtab%initialize_column(text, LENORIGIN, alignment=TABLEFT)
    !
    ! -- variable
    text = 'VARIABLE NAME'
    call memtab%initialize_column(text, LENVARNAME, alignment=TABLEFT)
    !
    ! -- data type
    text = 'DATA TYPE'
    call memtab%initialize_column(text, 16, alignment=TABLEFT)
    !
    ! -- size
    text = 'NUMBER OF ITEMS'
    call memtab%initialize_column(text, 20, alignment=TABRIGHT)
    !
    ! -- is it a pointer
    text = 'ASSOCIATED VARIABLE'
    call memtab%initialize_column(text, iptrlen, alignment=TABLEFT)
    !
    ! -- is it a pointer
    text = 'MEMORY ACCESS PERMISSION'
    call memtab%initialize_column(text, 16, alignment=TABLEFT)
    !
    ! -- return
    return
  end subroutine mem_detailed_table  
  
  subroutine mem_summary_line(component, rchars, rlog, rint, rreal, bytes)
! ******************************************************************************
! Write a row for the memory_print_option 'SUMMARY' table 
!
! -- Arguments are as follows:
!       COMPONENT    : character defining the program component (for example,
!                      solution)
!       RCHARS       : allocated size of characters (in common units)
!       RLOG         : allocated size of logical (in common units)
!       RINT         : allocated size of integer variables (in common units)
!       RREAL        : allocated size of real variables (in common units)
!       BYTES        : total allocated memory in memory manager (in common units)
!       CUNITS       : memory units (bytes, kilobytes, megabytes, or gigabytes)
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: component
    real(DP), intent(in) :: rchars
    real(DP), intent(in) :: rlog
    real(DP), intent(in) :: rint
    real(DP), intent(in) :: rreal
    real(DP), intent(in) :: bytes
    ! -- formats
    ! -- code
    !
    ! -- write line
    call memtab%add_term(component)
    call memtab%add_term(rchars)
    call memtab%add_term(rlog)
    call memtab%add_term(rint)
    call memtab%add_term(rreal)
    call memtab%add_term(bytes)
    !
    ! -- return
    return
  end subroutine mem_summary_line 

  subroutine mem_units(bytes, fact, cunits)
    ! -- dummy
    real(DP), intent(in) :: bytes
    real(DP), intent(inout) :: fact
    character(len=*), intent(inout) :: cunits
    ! -- local
    ! -- formats
    ! -- code
    !
    ! -- initialize factor and unit string
    cunits = 'UNKNOWN'
    fact = DONE
    !
    ! -- set factor
    if (bytes < DEP3) then
      fact = DONE
      cunits = 'BYTES'
    else if (bytes < DEP6) then
      fact = DEM3
      cunits = 'KILOBYTES'
    else if (bytes < DEP9) then
      fact = DEM6
      cunits = 'MEGABYTES'
    else 
      fact = DEM9
      cunits = 'GIGABYTES'
    end if
    !
    ! -- return
    return
  end subroutine mem_units 
  
  subroutine mem_summary_total(iout, bytes)
! ******************************************************************************
! Create and fill a table with the total allocated memory in the memory manager
!
! -- Arguments are as follows:
!       IOUT         : unit number for mfsim.lst
!       BYTES        : total number of bytes allocated in the memory manager
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), intent(in) :: iout
    real(DP), intent(in) :: bytes
    ! -- local
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: text
    character(LEN=10) :: cunits
    integer(I4B) :: nterms
    integer(I4B) :: nrows
    real(DP) :: fact
    real(DP) :: smb
    ! -- formats
    ! -- code
    !
    ! -- calculate factor and memory units
    call mem_units(bytes, fact, cunits)
    !
    ! -- set table terms
    nterms = 2
    nrows = 5
    !
    ! -- set up table title
    title = 'MEMORY MANAGER TOTAL STORAGE BY DATA TYPE, IN ' // trim(cunits)
    !
    ! -- set up stage tableobj
    call table_cr(memtab, 'MEM TOT', title)
    call memtab%table_df(nrows, nterms, iout)
    !
    ! -- data type
    text = 'DATA TYPE'
    call memtab%initialize_column(text, 15, alignment=TABLEFT)
    !
    ! -- number of values
    text = 'ALLOCATED MEMORY'
    call memtab%initialize_column(text, 15, alignment=TABCENTER)
    !
    ! -- write data
    !
    ! -- characters
    smb = real(nvalues_astr, DP) * fact
    call memtab%add_term('Character')
    call memtab%add_term(smb)
    !
    ! -- logicals
    smb = real(nvalues_alogical * LGP, DP) * fact
    call memtab%add_term('Logical')
    call memtab%add_term(smb)
    !
    ! -- integers
    smb = real(nvalues_aint * I4B, DP) * fact
    call memtab%add_term('Integer')
    call memtab%add_term(smb)
    !
    ! -- reals
    smb = real(nvalues_adbl * DP, DP) * fact
    call memtab%add_term('Real')
    call memtab%add_term(smb)
    !
    ! -- total memory usage
    call memtab%print_separator()
    smb = bytes * fact
    call memtab%add_term('Total')
    call memtab%add_term(smb)
    !
    ! -- deallocate table
    call mem_cleanup_table()
    !
    ! -- return
    return
  end subroutine mem_summary_total  
  
  subroutine mem_cleanup_table()
! ******************************************************************************
! Generic function to clean a memory manager table 
!
! -- Arguments are as follows:
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    ! -- local
    ! -- formats
    ! -- code
    call memtab%table_da()
    deallocate(memtab)
    nullify(memtab)
    !
    ! -- return
    return
  end subroutine mem_cleanup_table 
  
  
  subroutine mem_write_usage(iout)
! ******************************************************************************
! Write memory manager memory usage based on the user-specified 
! memory_print_option. The total memory usage by data types (int, real, etc.)
! is written for every simulation. 
!
! -- Arguments are as follows:
!       IOUT         : unit number for mfsim.lst
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), intent(in) :: iout
    ! -- local
    class(MemoryType), pointer :: mt
    character(len=LENORIGIN), allocatable, dimension(:) :: cunique
    character(LEN=10) :: cunits
    integer(I4B) :: ipos
    integer(I4B) :: icomp
    integer(I4B) :: ilen
    integer(I8B) :: nchars
    integer(I8B) :: nlog
    integer(I8B) :: nint
    integer(I8B) :: nreal
    real(DP) :: simbytes
    real(DP) :: fact
    real(DP) :: rchars
    real(DP) :: rlog
    real(DP) :: rint
    real(DP) :: rreal
    real(DP) :: bytes
    ! -- formats
    ! -- code
    !
    ! -- Calculate simulation memory allocation
    simbytes = (nvalues_astr +                                                   &
                nvalues_alogical * LGP +                                         &
                nvalues_aint * I4B +                                             &
                nvalues_adbl * DP)
    simbytes = real(simbytes, DP)
    !
    ! -- calculate factor and memory units
    call mem_units(simbytes, fact, cunits)
    !
    ! -- Write summary table for simulation components
    if (iprmem == 1) then
      !
      ! -- Find unique names of simulation components
      call mem_unique_origins(cunique)
      call mem_summary_table(iout, size(cunique), cunits)
      do icomp = 1, size(cunique)
        nchars = 0
        nlog = 0
        nint = 0
        nreal = 0
        bytes = DZERO
        ilen = len_trim(cunique(icomp))
        do ipos = 1, memorylist%count()
          mt => memorylist%Get(ipos)
          if (cunique(icomp) /= mt%origin(1:ilen)) cycle
          if (.not. mt%master) cycle
          if (mt%memtype(1:6) == 'STRING') then
            nchars = nchars + mt%isize
          else if (mt%memtype(1:7) == 'LOGICAL') then
            nlog = nlog + mt%isize
          else if (mt%memtype(1:7) == 'INTEGER') then
            nint = nint + mt%isize
          else if (mt%memtype(1:6) == 'DOUBLE') then
            nreal = nreal + mt%isize
          end if
        end do
        !
        ! -- calculate size of each data type in bytes
        rchars = real(nchars, DP) * fact
        rlog = real(nlog * LGP, DP) * fact
        rint = real(nint * I4B, DP) * fact
        rreal = real(nreal * DP, DP) * fact
        !
        ! -- calculate total storage in bytes
        bytes = rchars + rlog + rint + rreal
        !
        ! -- write data
        call mem_summary_line(cunique(icomp), rchars, rlog, rint, rreal, bytes)
      end do
      call mem_cleanup_table()
    end if
    !
    ! -- Write table with all variables for iprmem == 2
    if (iprmem == 2) then
      call mem_detailed_table(iout, memorylist%count())
      do ipos = 1, memorylist%count()
        mt => memorylist%Get(ipos)
        call mt%table_entry(memtab)
      end do
      call mem_cleanup_table()
    end if
    !
    ! -- Write total memory allocation
    call mem_summary_total(iout, simbytes)
    !
    ! -- return
    return
  end subroutine mem_write_usage
  
  subroutine mem_da()
! ******************************************************************************
! Deallocate memory in the memory manager.
!
! -- Arguments are as follows:
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use VersionModule, only: IDEVELOPMODE
    ! -- dummy
    class(MemoryType), pointer :: mt
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: ipos
    ! -- code
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
    if (count_errors() > 0) then
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine mem_da
  
  subroutine mem_unique_origins(cunique)
! ******************************************************************************
! Create a character array that contains the unique origins in the memory 
! manager. Only the first component of the origin is evaluated.  
!
! -- Arguments are as follows:
!       CUNIQUE      : returned defined length (LENORIGIN) character array with
!                      unique first component origin entries.
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ArrayHandlersModule, only: ExpandArray, ifind
    use InputOutputModule, only: ParseLine
    ! -- dummy
    character(len=LENORIGIN), allocatable, dimension(:), intent(inout) :: cunique
    ! -- local
    class(MemoryType), pointer :: mt
    character(len=LENORIGIN), allocatable, dimension(:) :: words
    integer(I4B) :: ipos
    integer(I4B) :: ipa
    integer(I4B) :: nwords
    ! -- code
    !
    ! -- initialize cunique
    allocate(cunique(0))
    !
    ! -- find unique origins
    do ipos = 1, memorylist%count()
      mt => memorylist%Get(ipos)
      call ParseLine(mt%origin, nwords, words)
      ipa = ifind(cunique, words(1))
      if(ipa < 1) then
        call ExpandArray(cunique, 1)
        cunique(size(cunique)) = words(1)
      end if
    end do
    !
    ! -- return
    return
  end subroutine mem_unique_origins
  
end module MemoryManagerModule

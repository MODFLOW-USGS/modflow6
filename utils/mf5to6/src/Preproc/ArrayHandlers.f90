module ArrayHandlersModule

  use ConstantsModule, only: MAXCHARLEN
  use GLOBAL, only: iout
  private
  public :: ExpandArray, ExtendPtrArray
  public :: ifind

  interface ExpandArray
    ! This interface is for use with ALLOCATABLE arrays.
    ! IMPORTANT: Do not use pointers to elements of arrays when using
    ! ExpandArray to increase the array size!  The locations of array
    ! elements in memory are changed when ExpandArray is invoked.
    module procedure expand_integer, expand_double, &
                     expand_character, expand_real
  end interface ExpandArray

  interface ExtendPtrArray
    ! This interface is for use with POINTERS to arrays.
    module procedure extend_double, extend_integer
  end interface

  interface ifind
    module procedure ifind_character
  end interface ifind

contains

  ! -- Specific procedures that implement ExpandArray for allocatable arrays

  subroutine expand_integer(array, increment)
    implicit none
    ! -- dummy arguments
    integer, allocatable, intent(inout) :: array(:)
    integer, optional,    intent(in)    :: increment
    ! -- local variables
    integer :: inclocal, isize, newsize
    integer, allocatable, dimension(:) :: array_temp
    !
    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    endif
    !
    ! -- increase size of array by inclocal, retaining
    !    contained data
    if (allocated(array)) then
      isize = size(array)
      newsize = isize + inclocal
      allocate(array_temp(newsize))
      array_temp(1:isize) = array
      deallocate(array)
      call move_alloc(array_temp, array)
    else
      allocate(array(inclocal))
    endif
    !
    return
  end subroutine expand_integer

  subroutine expand_double(array, increment)
    implicit none
    ! -- dummy arguments
    double precision, allocatable, intent(inout) :: array(:)
    integer, optional,             intent(in)    :: increment
    ! -- local variables
    integer :: inclocal, isize, newsize
    double precision, allocatable, dimension(:) :: array_temp
    !
    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    endif
    !
    ! -- increase size of array by inclocal, retaining
    !    contained data
    if (allocated(array)) then
      isize = size(array)
      newsize = isize + inclocal
      allocate(array_temp(newsize))
      array_temp(1:isize) = array
      deallocate(array)
      call move_alloc(array_temp, array)
    else
      allocate(array(inclocal))
    endif
    !
    return
  end subroutine expand_double

  subroutine expand_real(array, increment)
    implicit none
    ! -- dummy arguments
    real, allocatable, intent(inout) :: array(:)
    integer, optional,             intent(in)    :: increment
    ! -- local variables
    integer :: inclocal, isize, newsize
    real, allocatable, dimension(:) :: array_temp
    !
    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    endif
    !
    ! -- increase size of array by inclocal, retaining
    !    contained data
    if (allocated(array)) then
      isize = size(array)
      newsize = isize + inclocal
      allocate(array_temp(newsize))
      array_temp(1:isize) = array
      deallocate(array)
      call move_alloc(array_temp, array)
    else
      allocate(array(inclocal))
    endif
    !
    return
  end subroutine expand_real

  subroutine expand_character(array, increment)
    implicit none
    ! -- dummy arguments
    character(len=*), allocatable, intent(inout) :: array(:)
    integer, optional,             intent(in)    :: increment
    ! -- local variables
    integer :: i, inclocal, isize, lenc, newsize
    character(len=MAXCHARLEN), allocatable, dimension(:) :: array_temp
    !
    ! -- check character length
    lenc = len(array)
    if (lenc>MAXCHARLEN) then
      ! Can't use store_error or ustop here because SimModule
      ! is dependent on ArrayHandlersModule.
      write(iout,*)'Error in ArrayHandlersModule: Need to increase MAXCHARLEN'
      write(*,*)'Error in ArrayHandlersModule: Need to increase MAXCHARLEN'
      write(iout,*)'Stopping...'
      write(*,*)'Stopping...'
      stop
    endif
    !
    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    endif
    !
    ! -- increase size of array by inclocal, retaining
    !    contained data
    ! [Ned TODO: may be able to use mold here, e.g.:
    !       allocate(values(num), mold=proto)]
    if (allocated(array)) then
      isize = size(array)
      newsize = isize + inclocal
      allocate(array_temp(isize))
      do i=1,isize
        array_temp(i) = array(i)
      enddo
      deallocate(array)
      allocate(array(newsize))
      do i=1,isize
        array(i) = array_temp(i)
      enddo
      do i=isize+1,newsize
        array(i) = ''
      enddo
      deallocate(array_temp)
    else
      allocate(array(inclocal))
    endif
    !
    return
  end subroutine expand_character

  ! -- Specific procedures that implement ExtendPtrArray for pointer arrays

  subroutine extend_double(array, increment)
    implicit none
    ! -- dummy arguments
    double precision, pointer, dimension(:), intent(inout) :: array
    integer, optional, intent(in) :: increment
    ! -- local variables
    integer :: i, inclocal, isize, istat, newsize
    double precision, pointer, dimension(:) :: array_temp
    character(len=100) :: ermsg
    !
    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    endif
    !
    ! -- increase size of array by inclocal, retaining
    !    contained data
    if (associated(array)) then
      isize = size(array)
      newsize = isize + inclocal
      allocate(array_temp(newsize), stat=istat, errmsg=ermsg)
      if (istat /= 0) goto 99
      do i=1,isize
        array_temp(i) = array(i)
      enddo
      deallocate(array)
      array => array_temp
    else
      allocate(array(inclocal))
    endif
    !
    return  ! normal return
    !
    ! -- Error reporting
    99 continue
      ! Can't use store_error or ustop here because SimModule
      ! is dependent on ArrayHandlersModule.
      write(iout,*)'Error encountered while trying to increase array size:'
      write(iout,'(a)')trim(ermsg)
      write(iout,*)'Stopping...'
      write(*,*)'Error encountered while trying to increase array size:'
      write(iout,'(a)')trim(ermsg)
      write(*,*)'Stopping...'
      stop
  end subroutine extend_double

  subroutine extend_integer(array, increment)
    implicit none
    ! -- dummy arguments
    integer, pointer, dimension(:), intent(inout) :: array
    integer, optional, intent(in) :: increment
    ! -- local variables
    integer :: i, inclocal, isize, istat, newsize
    integer, pointer, dimension(:) :: array_temp
    character(len=100) :: ermsg
    !
    ! -- initialize
    if (present(increment)) then
      inclocal = increment
    else
      inclocal = 1
    endif
    !
    ! -- increase size of array by inclocal, retaining
    !    contained data
    if (associated(array)) then
      isize = size(array)
      newsize = isize + inclocal
      allocate(array_temp(newsize), stat=istat, errmsg=ermsg)
      if (istat /= 0) goto 99
      do i=1,isize
        array_temp(i) = array(i)
      enddo
      deallocate(array)
      array => array_temp
    else
      allocate(array(inclocal))
    endif
    !
    return
    !
    ! -- Error reporting
    99 continue
      ! Can't use store_error or ustop here because SimModule
      ! is dependent on ArrayHandlersModule.
      write(iout,*)'Error encountered while trying to increase array size:'
      write(iout,'(a)')trim(ermsg)
      write(iout,*)'Stopping...'
      write(*,*)'Error encountered while trying to increase array size:'
      write(iout,'(a)')trim(ermsg)
      write(*,*)'Stopping...'
      stop
  end subroutine extend_integer

  integer function ifind_character(array, str)
    ! -- Find the first array element containing str
    ! -- Return -1 if not found.
    implicit none
    character(len=*), dimension(:) :: array
    character(len=*) :: str
    !local
    integer :: i
    ifind_character = -1
    findloop: do i=1,size(array)
      if(array(i) == str) then
        ifind_character = i
        exit findloop
      endif
    enddo findloop
    return
  end function ifind_character

  subroutine remove_character(array, ipos)
    !remove the ipos position from array
    implicit none
    ! -- dummy arguments
    character(len=*), allocatable, intent(inout) :: array(:)
    integer,                       intent(in)    :: ipos
    ! -- local variables
    integer :: i, isize, lenc, newsize, inew
    character(len=MAXCHARLEN), allocatable, dimension(:) :: array_temp
    !
    ! -- check character length
    lenc = len(array)
    if (lenc>MAXCHARLEN) then
      ! Can't use store_error or ustop here because SimModule
      ! is dependent on ArrayHandlersModule.
      write(iout,*)'Error in ArrayHandlersModule: Need to increase MAXCHARLEN'
      write(*,*)'Error in ArrayHandlersModule: Need to increase MAXCHARLEN'
      write(iout,*)'Stopping...'
      write(*,*)'Stopping...'
      stop
    endif
    !
    ! -- calculate sizes
    isize = size(array)
    newsize = isize - 1
    !
    ! -- copy array to array_temp
    allocate(array_temp(isize))
    do i = 1, isize
      array_temp(i) = array(i)
    enddo
    !
    deallocate(array)
    allocate(array(newsize))
    inew = 1
    do i = 1, isize
      if(i /= ipos) then
        array(inew) = array_temp(i)
        inew = inew + 1
      endif
    enddo
    deallocate(array_temp)
    !
    return
  end subroutine remove_character

end module ArrayHandlersModule

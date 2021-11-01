module ArrayHandlersModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, MAXCHARLEN, DZERO, DTEN
  use SimVariablesModule, only: iout
  use GenericUtilitiesModule, only: sim_message, stop_with_error
  private
  public :: ExpandArray, ExpandArrayWrapper, ExtendPtrArray
  public :: ifind
  public :: remove_character
  
  interface ExpandArrayWrapper
    module procedure expand_integer_wrapper
  end interface

  interface ExpandArray
    ! This interface is for use with ALLOCATABLE arrays.
    ! IMPORTANT: Do not use pointers to elements of arrays when using
    ! ExpandArray to increase the array size!  The locations of array
    ! elements in memory are changed when ExpandArray is invoked.
    module procedure expand_integer, expand_double,                              &
                     expand_character  !, expand_real
  end interface ExpandArray

  interface ExtendPtrArray
    ! This interface is for use with POINTERS to arrays.
    module procedure extend_double, extend_integer
  end interface

  interface ifind
    module procedure ifind_character, ifind_integer
  end interface ifind

contains

  subroutine expand_integer_wrapper(nsize, array, minvalue, loginc)
    implicit none
    ! -- dummy
    integer(I4B), intent(in) :: nsize
    integer(I4B), allocatable, intent(inout) :: array(:)
    integer(I4B), intent(in), optional :: minvalue
    logical(LGP), intent(in), optional :: loginc
    ! -- local
    logical(LGP) :: log_increment
    integer(I4B) :: minimum_increment
    integer(I4B) :: increment
    integer(I4B) :: isize
    integer(I4B) :: n
    !
    ! -- process optional variables
    if (present(minvalue)) then
      minimum_increment = minvalue
    else
      minimum_increment = 100
    end if
    if (present(loginc)) then
      log_increment = loginc
    else
      log_increment = .FALSE.
    end if
    !
    ! -- determine current size of the array
    isize = size(array)
    !
    ! -- expand the array if necessary
    if (nsize > isize) then
      !
      ! -- increase array size by 1, 10, 100, 1000, etc.
      !    from 1 to 9, 10 to 99, 100 to 999, 1000 to 10000, etc.
      if (loginc) then
        increment = int(log10(real(nsize, DP)), I4B)
        increment = int(DTEN**increment, I4B)
      !
      ! -- increase increment by a multiplier and a value no
      !    smaller than a default or specified minimum size
      else
        increment =  int(nsize * 0.2_DP)
        increment = max(minimum_increment, increment)
      end if
      !
      ! -- expand the array
      call ExpandArray(array, increment)
      !
      ! -- initialize expanded array elements
      do n = isize + 1, size(array)
        array(n) = 0
      end do
    end if
    !
    ! -- return
    return
  end subroutine expand_integer_wrapper

  ! -- Specific procedures that implement ExpandArray for allocatable arrays

  subroutine expand_integer(array, increment)
    implicit none
    ! -- dummy
    integer(I4B), allocatable, intent(inout) :: array(:)
    integer(I4B), optional,    intent(in)    :: increment
    ! -- local
    integer(I4B) :: inclocal, isize, newsize
    integer(I4B), allocatable, dimension(:) :: array_temp
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
    ! -- dummy
    real(DP), allocatable, intent(inout) :: array(:)
    integer(I4B), optional,             intent(in)    :: increment
    ! -- local
    integer(I4B) :: inclocal, isize, newsize
    real(DP), allocatable, dimension(:) :: array_temp
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

  subroutine expand_character(array, increment)
    implicit none
    ! -- dummy
    character(len=*), allocatable, intent(inout) :: array(:)
    integer(I4B), optional,             intent(in)    :: increment
    ! -- local
    character(len=LINELENGTH) :: line
    character(len=MAXCHARLEN), allocatable, dimension(:) :: array_temp
    integer(I4B) :: i, inclocal, isize, lenc, newsize
    ! -- format
    character(len=*), parameter :: stdfmt = "(/,'ERROR REPORT:',/,1x,a)"
    !
    ! -- check character length
    lenc = len(array)
    if (lenc > MAXCHARLEN) then
      write(line, '(a)') 'Error in ArrayHandlersModule: ' //                     &
                         'Need to increase MAXCHARLEN'
      call sim_message(line, iunit=iout, fmt=stdfmt)
      call sim_message(line, fmt=stdfmt)
      !
      ! -- stop message
      write(line, '(a)') 'Stopping...'
      call sim_message(line, iunit=iout)
      call sim_message(line)
      call stop_with_error(138)
    end if
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
    ! -- dummy
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: array
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    character(len=LINELENGTH) :: line
    character(len=100) :: ermsg
    integer(I4B) :: i, inclocal, isize, istat, newsize
    real(DP), dimension(:), pointer, contiguous :: array_temp => null()
    ! -- format
    character(len=*), parameter :: stdfmt = "(/,'ERROR REPORT:',/,1x,a)"
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
    ! -- normal return
    return  
    !
    ! -- Error reporting
99  continue

    write(line, '(a)') 'Error in ArrayHandlersModule: ' //                       &
                        'Could not increase array size'
    call sim_message(line, iunit=iout, fmt=stdfmt)
    call sim_message(line, fmt=stdfmt)
    !
    ! -- error message
    call sim_message(ermsg, iunit=iout)
    call sim_message(ermsg)
    !
    ! -- stop message
    write(line, '(a)') 'Stopping...'
    call sim_message(line, iunit=iout)
    call sim_message(line)
    call stop_with_error(138)
      
  end subroutine extend_double

  subroutine extend_integer(array, increment)
    implicit none
    ! -- dummy
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: array
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    character(len=LINELENGTH) :: line
    character(len=100) :: ermsg
    integer(I4B) :: i, inclocal, isize, istat, newsize
    integer(I4B), dimension(:), pointer, contiguous :: array_temp => null()
    ! -- format
    character(len=*), parameter :: stdfmt = "(/,'ERROR REPORT:',/,1x,a)"
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
    ! -- normal return
    return
    !
    ! -- Error reporting
99  continue

    write(line, '(a)') 'Error in ArrayHandlersModule: ' //                       &
                        'Could not increase array size'
    call sim_message(line, iunit=iout, fmt=stdfmt)
    call sim_message(line, fmt=stdfmt)
    !
    ! -- error message
    call sim_message(ermsg, iunit=iout)
    call sim_message(ermsg)
    !
    ! -- stop message
    write(line, '(a)') 'Stopping...'
    call sim_message(line, iunit=iout)
    call sim_message(line)
    call stop_with_error(138)
      
  end subroutine extend_integer

  function ifind_character(array, str)
    ! -- Find the first array element containing str
    ! -- Return -1 if not found.
    implicit none
    ! -- return
    integer(I4B) :: ifind_character
    ! -- dummy
    character(len=*), dimension(:) :: array
    character(len=*) :: str
    ! -- local
    integer(I4B) :: i
    ifind_character = -1
    findloop: do i=1,size(array)
      if(array(i) == str) then
        ifind_character = i
        exit findloop
      endif
    enddo findloop
    return
  end function ifind_character

  function ifind_integer(iarray, ival)
    ! -- Find the first array element containing str
    ! -- Return -1 if not found.
    implicit none
    ! -- return
    integer(I4B) :: ifind_integer
    ! -- dummy
    integer(I4B), dimension(:) :: iarray
    integer(I4B) :: ival
    ! -- local
    integer(I4B) :: i
    ifind_integer = -1
    findloop: do i = 1, size(iarray)
      if(iarray(i) == ival) then
        ifind_integer = i
        exit findloop
      endif
    enddo findloop
    return
  end function ifind_integer

  subroutine remove_character(array, ipos)
    !remove the ipos position from array
    implicit none
    ! -- dummy
    character(len=*), allocatable, intent(inout) :: array(:)
    integer(I4B),                       intent(in)    :: ipos
    ! -- local
    character(len=LINELENGTH) :: line
    character(len=MAXCHARLEN), allocatable, dimension(:) :: array_temp
    integer(I4B) :: i, isize, lenc, newsize, inew
    ! -- format
    character(len=*), parameter :: stdfmt = "(/,'ERROR REPORT:',/,1x,a)"
    !
    ! -- check character length
    lenc = len(array)
    if (lenc > MAXCHARLEN) then

      write(line, '(a)') 'Error in ArrayHandlersModule: ' //                       &
                         'Need to increase MAXCHARLEN'
      call sim_message(line, iunit=iout, fmt=stdfmt)
      call sim_message(line, fmt=stdfmt)
      !
      ! -- stop message
      write(line, '(a)') 'Stopping...'
      call sim_message(line, iunit=iout)
      call sim_message(line)
      call stop_with_error(138)
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

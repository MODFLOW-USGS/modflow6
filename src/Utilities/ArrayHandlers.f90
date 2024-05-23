module ArrayHandlersModule

  use KindModule, only: DP, I4B, LGP
  use ErrorUtilModule, only: pstop
  use ConstantsModule, only: LINELENGTH, MAXCHARLEN, DZERO, DTEN
  implicit none
  private
  public :: ExpandArray, ExpandArray2D, ExpandArrayWrapper, ExtendPtrArray
  public :: ConcatArray
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
    module procedure expand_integer, expand_double, expand_logical, &
      expand_character
  end interface ExpandArray

  interface ExpandArray2D
    ! This interface is for use with ALLOCATABLE arrays.
    ! IMPORTANT: Do not use pointers to elements of arrays when using
    ! ExpandArray2D to increase the array size!  The locations of array
    ! elements in memory are changed when ExpandArray2D is invoked.
    module procedure expand_integer_2d, expand_double_2d
  end interface ExpandArray2D

  interface ExtendPtrArray
    ! This interface is for use with POINTERS to arrays.
    module procedure extend_double, extend_integer, &
      extend_string
  end interface

  interface ConcatArray
    module procedure concat_integer
  end interface

  interface ifind
    module procedure ifind_character, ifind_integer, ifind_charstring
  end interface ifind

contains

  subroutine expand_integer_wrapper(nsize, array, minvalue, loginc)
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
        increment = int(nsize * 0.2_DP)
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

  end subroutine expand_integer_wrapper

  ! -- Specific procedures that implement ExpandArray for allocatable arrays

  subroutine expand_integer(array, increment)
    ! -- dummy
    integer(I4B), allocatable, intent(inout) :: array(:)
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    integer(I4B) :: inc, lb, n
    integer(I4B), allocatable, dimension(:) :: temp

    ! -- default to expanding by 1
    if (present(increment)) then
      inc = increment
      if (inc == 0) return
      if (inc < 0) call pstop(1, "increment must be nonnegative")
    else
      inc = 1
    end if

    ! -- expand array to the requested size, keeping
    !    existing items and the existing lower bound,
    !    or allocate the array if still unallocated
    if (allocated(array)) then
      lb = lbound(array, 1)
      n = size(array)
      allocate (temp(lb:(lb + n + inc - 1)))
      temp(lb:(lb + n - 1)) = array
      deallocate (array)
      call move_alloc(temp, array)
    else
      allocate (array(inc))
    end if
  end subroutine expand_integer

  subroutine expand_double(array, increment)
    ! -- dummy
    real(DP), allocatable, intent(inout) :: array(:)
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    integer(I4B) :: inc, lb, n
    real(DP), allocatable, dimension(:) :: temp

    ! -- default to expanding by 1
    if (present(increment)) then
      inc = increment
      if (inc == 0) return
      if (inc < 0) call pstop(1, "increment must be nonnegative")
    else
      inc = 1
    end if

    ! -- expand array to the requested size, keeping
    !    existing items and the existing lower bound,
    !    or allocate the array if still unallocated
    if (allocated(array)) then
      lb = lbound(array, 1)
      n = size(array)
      allocate (temp(lb:(lb + n + inc - 1)))
      temp(lb:(lb + n - 1)) = array
      deallocate (array)
      call move_alloc(temp, array)
    else
      allocate (array(inc))
    end if

  end subroutine expand_double

  subroutine expand_logical(array, increment)
    ! -- dummy
    logical(LGP), allocatable, intent(inout) :: array(:)
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    integer(I4B) :: inc, lb, n
    logical(LGP), allocatable, dimension(:) :: temp

    ! -- default to expanding by 1
    if (present(increment)) then
      inc = increment
      if (inc == 0) return
      if (inc < 0) call pstop(1, "increment must be nonnegative")
    else
      inc = 1
    end if

    ! -- expand array to the requested size, keeping
    !    existing items and the existing lower bound,
    !    or allocate the array if still unallocated
    if (allocated(array)) then
      lb = lbound(array, 1)
      n = size(array)
      allocate (temp(lb:(lb + n + inc - 1)))
      temp(lb:(lb + n - 1)) = array
      deallocate (array)
      call move_alloc(temp, array)
    else
      allocate (array(inc))
    end if

  end subroutine expand_logical

  subroutine expand_character(array, increment)
    ! -- dummy
    character(len=*), allocatable, intent(inout) :: array(:)
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    character(len=MAXCHARLEN), allocatable, dimension(:) :: temp
    integer(I4B) :: i, inc, nold, nnew, lenc

    ! -- check character length
    lenc = len(array)
    if (lenc > MAXCHARLEN) &
      call pstop(138, 'Error in ArrayHandlersModule: '// &
                 'Need to increase MAXCHARLEN. Stopping...')

    ! -- default to expanding by 1
    if (present(increment)) then
      inc = increment
      if (inc == 0) return
      if (inc < 0) call pstop(1, "increment must be nonnegative")
    else
      inc = 1
    end if

    ! -- expand array to the requested size, keeping
    !    existing items, or allocate if still needed
    !    TODO: may be able to use mold here, e.g.:
    !          allocate(values(num), mold=proto)
    if (allocated(array)) then
      nold = size(array)
      nnew = nold + inc
      allocate (temp(nold))
      do i = 1, nold
        temp(i) = array(i)
      end do
      deallocate (array)
      allocate (array(nnew))
      do i = 1, nold
        array(i) = temp(i)
      end do
      do i = nold + 1, nnew
        array(i) = ''
      end do
      deallocate (temp)
    else
      allocate (array(inc))
    end if

  end subroutine expand_character

  ! -- Specific procedures that implement ExtendArray2D

  subroutine expand_integer_2d(array, increment1, increment2)
    ! -- dummy
    integer(I4B), allocatable, intent(inout) :: array(:, :)
    integer(I4B), optional, intent(in) :: increment1
    integer(I4B), optional, intent(in) :: increment2
    ! -- local
    integer(I4B) :: inc1, inc2, lb1, lb2, n1, n2
    integer(I4B), allocatable, dimension(:, :) :: temp

    ! -- default to expanding both dimensions by 1
    if (present(increment1)) then
      inc1 = increment1
    else
      inc1 = 1
    end if
    if (present(increment2)) then
      inc2 = increment2
    else
      inc2 = 1
    end if
    if (inc1 == 0 .and. inc2 == 0) return
    if (inc1 < 0 .or. inc2 < 0) &
      call pstop(1, "increments must be nonnegative")

    ! -- expand array to the requested size, keeping
    !    existing items and the existing lower bound,
    !    or allocate the array if still unallocated
    if (allocated(array)) then
      lb1 = lbound(array, 1)
      lb2 = lbound(array, 2)
      n1 = size(array, 1)
      n2 = size(array, 2)
      allocate (temp( &
                lb1:(lb1 + n1 + inc1 - 1), &
                lb2:(lb2 + n2 + inc2 - 1)))
      temp( &
        lb1:(lb1 + n1 - 1), &
        lb2:(lb2 + n2 - 1)) = array
      deallocate (array)
      call move_alloc(temp, array)
    else
      allocate (array(inc1, inc2))
    end if

  end subroutine expand_integer_2d

  subroutine expand_double_2d(array, increment1, increment2)
    ! -- dummy
    real(DP), allocatable, intent(inout) :: array(:, :)
    integer(I4B), optional, intent(in) :: increment1
    integer(I4B), optional, intent(in) :: increment2
    ! -- local
    integer(I4B) :: inc1, inc2, lb1, lb2, n1, n2
    real(DP), allocatable, dimension(:, :) :: temp

    ! -- default to expanding both dimensions by 1
    if (present(increment1)) then
      inc1 = increment1
    else
      inc1 = 1
    end if
    if (present(increment2)) then
      inc2 = increment2
    else
      inc2 = 1
    end if
    if (inc1 == 0 .and. inc2 == 0) return
    if (inc1 < 0 .or. inc2 < 0) &
      call pstop(1, "increments must be nonnegative")

    ! -- expand array to the requested size, keeping
    !    existing items and the existing lower bound,
    !    or allocate the array if still unallocated
    if (allocated(array)) then
      lb1 = lbound(array, 1)
      lb2 = lbound(array, 2)
      n1 = size(array, 1)
      n2 = size(array, 2)
      allocate (temp( &
                lb1:(lb1 + n1 + inc1 - 1), &
                lb2:(lb2 + n2 + inc2 - 1)))
      temp( &
        lb1:(lb1 + n1 - 1), &
        lb2:(lb2 + n2 - 1)) = array
      deallocate (array)
      call move_alloc(temp, array)
    else
      allocate (array(inc1, inc2))
    end if

  end subroutine expand_double_2d

  ! -- Specific procedures that implement ExtendPtrArray for pointer arrays

  subroutine extend_double(array, increment)
    ! -- dummy
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: array
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    character(len=100) :: ermsg
    integer(I4B) :: i, inc, lb, n, istat
    real(DP), dimension(:), pointer, contiguous :: temp => null()

    ! -- default to expanding by 1
    if (present(increment)) then
      inc = increment
      if (inc == 0) return
      if (inc < 0) call pstop(1, "increment must be nonnegative")
    else
      inc = 1
    end if

    ! -- expand array to the requested size, keeping
    !    existing items and the existing lower bound,
    !    or allocate the array if still unallocated
    if (associated(array)) then
      lb = lbound(array, 1)
      n = size(array)
      allocate (temp(lb:(lb + n + inc - 1)), stat=istat, errmsg=ermsg)
      if (istat /= 0) &
        call pstop(138, 'Error in ArrayHandlersModule, '// &
                   'could not increase array size:'//ermsg)
      do i = lb, lb + n - 1
        temp(i) = array(i)
      end do
      deallocate (array)
      array => temp
    else
      allocate (array(inc))
    end if

  end subroutine extend_double

  subroutine extend_integer(array, increment)
    ! -- dummy
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: array
    integer(I4B), optional, intent(in) :: increment
    ! -- local
    character(len=100) :: ermsg
    integer(I4B) :: i, inc, lb, n, istat
    integer(I4B), dimension(:), pointer, contiguous :: temp => null()

    ! -- default to expanding by 1
    if (present(increment)) then
      inc = increment
      if (inc == 0) return
      if (inc < 0) call pstop(1, "increment must be nonnegative")
    else
      inc = 1
    end if

    ! -- expand array to the requested size, keeping
    !    existing items and the existing lower bound,
    !    or allocate the array if still unallocated
    if (associated(array)) then
      lb = lbound(array, 1)
      n = size(array)
      allocate (temp(lb:(lb + n + inc - 1)), stat=istat, errmsg=ermsg)
      if (istat /= 0) &
        call pstop(138, 'Error in ArrayHandlersModule, '// &
                   'could not increase array size:'//ermsg)
      do i = lb, lb + n - 1
        temp(i) = array(i)
      end do
      deallocate (array)
      array => temp
    else
      allocate (array(inc))
    end if

  end subroutine extend_integer

  subroutine extend_string(array, increment)
    ! -- dummy
    character(len=*), dimension(:), pointer, contiguous :: array
    integer(I4B), optional :: increment
    ! -- local
    integer(I4B) :: inc, i, n
    character(len=len(array)), dimension(:), pointer, contiguous :: temp

    if (present(increment)) then
      inc = increment
      if (inc == 0) return
      if (inc < 0) call pstop(1, "increment must be nonnegative")
    else
      inc = 1
    end if

    if (associated(array)) then
      n = size(array)
      temp => array
      allocate (array(n + inc))
      do i = 1, n
        array(i) = temp(i)
      end do
      deallocate (temp)
    else
      allocate (array(inc))
    end if

  end subroutine extend_string

  !> @brief Concatenate integer arrays.
  subroutine concat_integer(array, array_to_add)
    integer(I4B), dimension(:), pointer, contiguous :: array
    integer(I4B), dimension(:), pointer, contiguous :: array_to_add
    ! local
    integer(I4B) :: i, n

    n = size(array)
    call ExtendPtrArray(array, increment=size(array_to_add))
    do i = 1, size(array_to_add)
      array(n + i) = array_to_add(i)
    end do
  end subroutine concat_integer

  !> @brief Find the 1st array element containing str, or -1 if not found.
  function ifind_character(array, str)
    ! -- return
    integer(I4B) :: ifind_character
    ! -- dummy
    character(len=*), dimension(:) :: array
    character(len=*) :: str
    ! -- local
    integer(I4B) :: i

    ifind_character = -1
    findloop: do i = 1, size(array)
      if (array(i) == str) then
        ifind_character = i
        exit findloop
      end if
    end do findloop
  end function ifind_character

  !> @brief Find the 1st array element containing str, or -1 if not found.
  !<
  function ifind_charstring(array, str)
    use CharacterStringModule
    ! -- return
    integer(I4B) :: ifind_charstring
    ! -- dummy
    type(CharacterStringType), dimension(:) :: array
    type(CharacterStringType) :: str
    ! -- local
    integer(I4B) :: i

    ifind_charstring = -1
    findloop: do i = 1, size(array)
      if (array(i) == str) then
        ifind_charstring = i
        exit findloop
      end if
    end do findloop
  end function ifind_charstring

  !> @brief Find the first element containing ival, or -1 if not found.
  function ifind_integer(iarray, ival)
    ! -- return
    integer(I4B) :: ifind_integer
    ! -- dummy
    integer(I4B), dimension(:) :: iarray
    integer(I4B) :: ival
    ! -- local
    integer(I4B) :: i

    ifind_integer = -1
    findloop: do i = 1, size(iarray)
      if (iarray(i) == ival) then
        ifind_integer = i
        exit findloop
      end if
    end do findloop
  end function ifind_integer

  !> @brief Remove the element at ipos from the array.
  subroutine remove_character(array, ipos)
    ! -- dummy
    character(len=*), allocatable, intent(inout) :: array(:)
    integer(I4B), intent(in) :: ipos
    ! -- local
    character(len=MAXCHARLEN), allocatable, dimension(:) :: temp
    integer(I4B) :: i, inew, n

    ! -- check character length
    if (len(array) > MAXCHARLEN) &
      call pstop(138, 'Error in ArrayHandlersModule: '// &
                 'Need to increase MAXCHARLEN. Stopping...')

    ! -- calculate size
    n = size(array)

    ! -- copy array to temp
    allocate (temp(n))
    do i = 1, n
      temp(i) = array(i)
    end do

    ! -- de/reallocate and copy back to array,
    !    omitting the specified element
    deallocate (array)
    allocate (array(n - 1))
    inew = 1
    do i = 1, n
      if (i /= ipos) then
        array(inew) = temp(i)
        inew = inew + 1
      end if
    end do
    deallocate (temp)

  end subroutine remove_character

end module ArrayHandlersModule

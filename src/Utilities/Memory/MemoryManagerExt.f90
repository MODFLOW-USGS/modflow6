module MemoryManagerExtModule

  use KindModule, only: DP, LGP, I4B, I8B
  use SimModule, only: store_error
  use MemoryTypeModule, only: MemoryType
  use MemoryManagerModule, only: get_from_memorylist

  implicit none
  private
  public :: mem_set_value
  public :: mempath_has_names

  interface mem_set_value
    module procedure mem_set_value_logical, mem_set_value_int, &
      mem_set_value_str_mapped_int, &
      mem_set_value_int1d, mem_set_value_int2d, &
      mem_set_value_int3d, mem_set_value_dbl, &
      mem_set_value_dbl1d, mem_set_value_dbl2d, &
      mem_set_value_dbl3d
  end interface mem_set_value

contains

  function mempath_has_names(memory_path, names_list) result(found)
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    character(len=*), dimension(:), intent(in) :: names_list
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: i

    do i = 1, size(names_list)
      call get_from_memorylist(names_list(i), memory_path, mt, found, checkfail)
      if (.not. found) then
        return
      end if
    end do

    return
  end function mempath_has_names

  !> @brief Set pointer to value of memory list logical variable
  !<
  subroutine mem_set_value_logical(p_mem, varname, memory_path)
    logical(LGP), pointer, intent(inout) :: p_mem !< pointer to logical scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    logical(LGP) :: checkfail = .false.

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'LOGICAL') then
      p_mem = mt%logicalsclr
    end if
  end subroutine mem_set_value_logical

  !> @brief Set pointer to value of memory list int variable
  !<
  subroutine mem_set_value_int(p_mem, varname, memory_path)
    integer(I4B), pointer, intent(inout) :: p_mem !< pointer to int scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    logical(LGP) :: checkfail = .false.

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'INTEGER') then
      p_mem = mt%intsclr
    end if
  end subroutine mem_set_value_int

  subroutine mem_set_value_str_mapped_int(p_mem, varname, memory_path, str_list)
    integer(I4B), pointer, intent(inout) :: p_mem !< pointer to int scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    character(len=*), dimension(:), intent(in) :: str_list
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: i

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'STRING') then
      do i = 1, size(str_list)
        if (mt%strsclr == str_list(i)) then
          p_mem = i
        end if
      end do
    end if
  end subroutine mem_set_value_str_mapped_int

  !> @brief Set pointer to value of memory list 1d int array variable
  !<
  subroutine mem_set_value_int1d(p_mem, varname, memory_path)
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 1d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: n

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'INTEGER') then
      if (size(mt%aint1d) /= size(p_mem)) then
        call store_error('mem_set_value() size mismatch int1d', terminate=.TRUE.)
      end if
      do n = 1, size(mt%aint1d)
        p_mem(n) = mt%aint1d(n)
      end do
    end if
  end subroutine mem_set_value_int1d

  !> @brief Set pointer to value of memory list 2d int array variable
  !<
  subroutine mem_set_value_int2d(p_mem, varname, memory_path)
    integer(I4B), dimension(:, :), pointer, contiguous, intent(inout) :: p_mem !< pointer to 2d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: i, j

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'INTEGER') then
      if (size(mt%aint2d, dim=1) /= size(p_mem, dim=1) .or. &
          size(mt%aint2d, dim=2) /= size(p_mem, dim=2)) then
        call store_error('mem_set_value() size mismatch int2d', terminate=.TRUE.)
      end if
      do i = 1, size(mt%aint2d, dim=2)
        do j = 1, size(mt%aint2d, dim=1)
          p_mem(j, i) = mt%aint2d(j, i)
        end do
      end do
    end if
  end subroutine mem_set_value_int2d

  !> @brief Set pointer to value of memory list 3d int array variable
  !<
  subroutine mem_set_value_int3d(p_mem, varname, memory_path)
    integer(I4B), dimension(:, :, :), pointer, contiguous, intent(inout) :: p_mem !< pointer to 3d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: i, j, k

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'INTEGER') then
      if (size(mt%aint3d, dim=1) /= size(p_mem, dim=1) .or. &
          size(mt%aint3d, dim=2) /= size(p_mem, dim=2) .or. &
          size(mt%aint3d, dim=3) /= size(p_mem, dim=3)) then
        call store_error('mem_set_value() size mismatch int3d', terminate=.TRUE.)
      end if
      do i = 1, size(mt%aint3d, dim=3)
        do j = 1, size(mt%aint3d, dim=2)
          do k = 1, size(mt%aint3d, dim=1)
            p_mem(k, j, i) = mt%aint3d(k, j, i)
          end do
        end do
      end do
    end if
  end subroutine mem_set_value_int3d

  !> @brief Set pointer to value of memory list double variable
  !<
  subroutine mem_set_value_dbl(p_mem, varname, memory_path)
    real(DP), pointer, intent(inout) :: p_mem !< pointer to dbl scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    logical(LGP) :: checkfail = .false.

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'DOUBLE') then
      p_mem = mt%dblsclr
    end if
  end subroutine mem_set_value_dbl

  !> @brief Set pointer to value of memory list 1d dbl array variable
  !<
  subroutine mem_set_value_dbl1d(p_mem, varname, memory_path)
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 1d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: n

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'DOUBLE') then
      if (size(mt%adbl1d) /= size(p_mem)) then
        call store_error('mem_set_value() size mismatch dbl1d', terminate=.TRUE.)
      end if
      do n = 1, size(mt%adbl1d)
        p_mem(n) = mt%adbl1d(n)
      end do
    end if
  end subroutine mem_set_value_dbl1d

  !> @brief Set pointer to value of memory list 2d dbl array variable
  !<
  subroutine mem_set_value_dbl2d(p_mem, varname, memory_path)
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: p_mem !< pointer to 2d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: i, j

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'DOUBLE') then
      if (size(mt%adbl2d, dim=1) /= size(p_mem, dim=1) .or. &
          size(mt%adbl2d, dim=2) /= size(p_mem, dim=2)) then
        call store_error('mem_set_value() size mismatch dbl2d', terminate=.TRUE.)
      end if
      do i = 1, size(mt%adbl2d, dim=2)
        do j = 1, size(mt%adbl2d, dim=1)
          p_mem(j, i) = mt%adbl2d(j, i)
        end do
      end do
    end if
  end subroutine mem_set_value_dbl2d

  !> @brief Set pointer to value of memory list 3d dbl array variable
  !<
  subroutine mem_set_value_dbl3d(p_mem, varname, memory_path)
    real(DP), dimension(:, :, :), pointer, contiguous, intent(inout) :: p_mem !< pointer to 3d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: memory_path !< path where variable is stored
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    logical(LGP) :: checkfail = .false.
    integer(I4B) :: i, j, k

    call get_from_memorylist(varname, memory_path, mt, found, checkfail)
    if (found .and. mt%memtype(1:index(mt%memtype, ' ')) == 'DOUBLE') then
      if (size(mt%adbl3d, dim=1) /= size(p_mem, dim=1) .or. &
          size(mt%adbl3d, dim=2) /= size(p_mem, dim=2) .or. &
          size(mt%adbl3d, dim=3) /= size(p_mem, dim=3)) then
        call store_error('mem_set_value() size mismatch dbl3d', terminate=.TRUE.)
      end if
      do i = 1, size(mt%adbl3d, dim=3)
        do j = 1, size(mt%adbl3d, dim=2)
          do k = 1, size(mt%adbl3d, dim=1)
            p_mem(k, j, i) = mt%adbl3d(k, j, i)
          end do
        end do
      end do
    end if
  end subroutine mem_set_value_dbl3d

end module MemoryManagerExtModule

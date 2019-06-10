module access_memory


use KindModule, only: DP, I4B, I8B
use MemoryManagerModule, only: mem_setptr
use shared_data

    implicit none

    contains

    subroutine get_int(name, origin)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        integer(I4B), pointer :: ivalue => NULL()

        call mem_setptr(ivalue, name, origin)
        int_pointer = ivalue
    end subroutine get_int

    subroutine set_int(name, origin, value)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        integer(I4B), target:: value
        integer(I4B), pointer :: ivalue => NULL()
        call mem_setptr(ivalue, name, origin)
        ivalue = value
    end subroutine set_int

    subroutine get_float(name, origin)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        real(DP), pointer :: fvalue => NULL()

        call mem_setptr(fvalue, name, origin)
        afloat = fvalue + 0.0
    end subroutine get_float

    subroutine set_float(name, origin, value)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        real(8), target:: value
        real(DP), pointer :: fvalue => NULL()
        call mem_setptr(fvalue, name, origin)
        fvalue = value + 0.0
    end subroutine set_float

    subroutine get_int_1d(name, origin, dim)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        integer(I4B) dim
        integer(I4B), dimension(:), pointer, contiguous:: ivalue => NULL()
        if (allocated(int_1d) .and. size(int_1d) /= dim) then
            deallocate(int_1d)
        endif
        if (.not. allocated(int_1d)) then
           allocate (int_1d(dim))
        endif

        call mem_setptr(ivalue, name, origin)
        int_1d = ivalue
    end subroutine get_int_1d

    subroutine set_int_1d(name, origin, value)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        integer(4), dimension(:):: value
        integer(I4B), dimension(:), pointer, contiguous:: ivalue => NULL()

        call mem_setptr(ivalue, name, origin)
        ivalue = value
    end subroutine set_int_1d

    subroutine get_float_1d(name, origin, dim)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        integer(I4B) dim
        real(DP), dimension(:), pointer, contiguous:: fvalue => NULL()
        if (allocated(float_1d) .and. size(float_1d) /= dim) then
            deallocate(float_1d)
        endif
        if (.not. allocated(float_1d)) then
           allocate (float_1d(dim))
        endif

        call mem_setptr(fvalue, name, origin)
        float_1d = fvalue
    end subroutine get_float_1d

    subroutine set_float_1d(name, origin, value)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        real(8), dimension(:):: value
        real(DP), dimension(:), pointer, contiguous:: fvalue => NULL()

        call mem_setptr(fvalue, name, origin)
        fvalue = value
    end subroutine set_float_1d

    subroutine get_int_2d(name, origin, ncol, nrow)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        integer(I4B):: ncol
        integer(I4B):: nrow
        integer(I4B), dimension(:, :), pointer, contiguous:: ivalue => NULL()
        if (allocated(int_2d) .and.                                            &
            ((size(int_2d, dim=1) /= ncol) .and.                               &
             (size(int_2d, dim=2) /= nrow)))                                   &
        then
            deallocate(int_2d)
        endif
        if (.not. allocated(int_2d)) then
           allocate (int_2d(ncol, nrow))
        endif

        call mem_setptr(ivalue, name, origin)
        int_2d = ivalue
    end subroutine get_int_2d

    subroutine set_int_2d(name, origin, value)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        integer(4), dimension(:, :), target:: value
        integer(I4B), dimension(:, :), pointer, contiguous:: ivalue => NULL()
        call mem_setptr(ivalue, name, origin)
        ivalue = value
    end subroutine set_int_2d

    subroutine get_float_2d(name, origin, ncol, nrow)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        integer(I4B):: ncol
        integer(I4B):: nrow
        real(DP), dimension(:, :), pointer, contiguous:: fvalue => NULL()
        if (allocated(float_2d) .and.                                          &
            ((size(float_2d, dim=1) /= ncol) .and.                             &
             (size(float_2d, dim=2) /= nrow)))                                 &
        then
            deallocate(int_2d)
        endif
        if (.not. allocated(int_2d)) then
           allocate (int_2d(ncol, nrow))
        endif

        call mem_setptr(fvalue, name, origin)
        float_2d = fvalue
    end subroutine get_float_2d

    subroutine set_float_2d(name, origin, value)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        real(8), dimension(:, :), target:: value
        real(DP), dimension(:, :), pointer, contiguous:: fvalue => NULL()
        call mem_setptr(fvalue, name, origin)
        fvalue = value
    end subroutine set_float_2d

end module access_memory

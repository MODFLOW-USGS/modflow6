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

    end subroutine

    subroutine set_int(name, origin, value)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        integer(I4B), target:: value
        integer(I4B), pointer :: ivalue => NULL()
        call mem_setptr(ivalue, name, origin)
        ivalue = value

    end subroutine

    subroutine get_float(name, origin)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        real(DP), pointer :: fvalue => NULL()

        call mem_setptr(fvalue, name, origin)
        afloat = fvalue + 0.0

    end subroutine

    subroutine set_float(name, origin, value)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: origin
        real(8), target:: value
        real(DP), pointer :: fvalue => NULL()
        call mem_setptr(fvalue, name, origin)
        fvalue = value + 0.0

    end subroutine

end module access_memory

module shared_data

! Data exchange module
! The data declares in this module will be exchanged between Fortran
! and Python.

    use KindModule, only: DP, I4B, I8B

    implicit none
    ! Keep these values over the whole program run.
    save

    integer(I4B), pointer :: int_pointer
    real(8), pointer :: float_pointer
    integer(I4B), target :: anint = 10
    real(8), target :: afloat = 60.0D0
    integer(I4B), allocatable, dimension(:), target :: int_1d
    real(8), allocatable, dimension(:), target :: float_1d
    integer(I4B), allocatable, dimension(:, :), target :: int_2d
    real(8), allocatable, dimension(:, :), target :: float_2d

    contains

    subroutine init_data()
        nullify(int_pointer, float_pointer)
        int_pointer => anint
        float_pointer => afloat
    end subroutine



end module shared_data

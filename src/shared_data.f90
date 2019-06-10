module shared_data

! Data exchange module
! The data declares in this module will be exchanged between Fortran
! and Python.

    use KindModule, only: DP, I4B, I8B

    implicit none
    ! Keep these values over the whole program run.
    save

    integer(I4B), target :: int_scalar
    real(8), target :: float_scalar
    integer(I4B), allocatable, dimension(:), target :: int_1d
    real(8), allocatable, dimension(:), target :: float_1d
    integer(I4B), allocatable, dimension(:, :), target :: int_2d
    real(8), allocatable, dimension(:, :), target :: float_2d

end module shared_data

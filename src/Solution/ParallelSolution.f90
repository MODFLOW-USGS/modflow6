module ParallelSolutionModule
  use KindModule, only: DP, LGP, I4B
  use ConstantsModule, only: LENPAKLOC, DONE, DZERO
  use NumericalSolutionModule, only: NumericalSolutionType
  use mpi
  use MpiWorldModule
  implicit none
  private

  public :: ParallelSolutionType

  type, extends(NumericalSolutionType) :: ParallelSolutionType
  contains
    ! override
    procedure :: sln_has_converged => par_has_converged
    procedure :: sln_package_convergence => par_package_convergence
    procedure :: sln_sync_newtonur_flag => par_sync_newtonur_flag
    procedure :: sln_nur_has_converged => par_nur_has_converged
    procedure :: sln_calc_ptc => par_calc_ptc
    procedure :: sln_underrelax => par_underrelax
    procedure :: sln_backtracking_xupdate => par_backtracking_xupdate

  end type ParallelSolutionType

contains

  !> @brief Check global convergence. The local maximum dependent
  !! variable change is reduced over MPI with all other processes
  !< that are running this parallel numerical solution.
  function par_has_converged(this, max_dvc) result(has_converged)
    class(ParallelSolutionType) :: this !< parallel solution
    real(DP) :: max_dvc !< the LOCAL maximum dependent variable change
    logical(LGP) :: has_converged !< True, when GLOBALLY converged
    ! local
    real(DP) :: global_max_dvc
    real(DP) :: abs_max_dvc
    integer :: ierr
    type(MpiWorldType), pointer :: mpi_world

    mpi_world => get_mpi_world()

    has_converged = .false.
    abs_max_dvc = abs(max_dvc)
    call MPI_Allreduce(abs_max_dvc, global_max_dvc, 1, MPI_DOUBLE_PRECISION, &
                       MPI_MAX, mpi_world%comm, ierr)
    call CHECK_MPI(ierr)
    if (global_max_dvc <= this%dvclose) then
      has_converged = .true.
    end if

  end function par_has_converged

  function par_package_convergence(this, dpak, cpakout, iend) &
    result(icnvg_global)
    class(ParallelSolutionType) :: this !< parallel solution instance
    real(DP), intent(in) :: dpak !< Newton Under-relaxation flag
    character(len=LENPAKLOC), intent(in) :: cpakout
    integer(I4B), intent(in) :: iend
    ! local
    integer(I4B) :: icnvg_global
    integer(I4B) :: icnvg_local
    integer :: ierr
    type(MpiWorldType), pointer :: mpi_world

    mpi_world => get_mpi_world()

    icnvg_local = &
      this%NumericalSolutionType%sln_package_convergence(dpak, cpakout, iend)

    call MPI_Allreduce(icnvg_local, icnvg_global, 1, MPI_INTEGER, &
                       MPI_MIN, mpi_world%comm, ierr)
    call CHECK_MPI(ierr)

  end function par_package_convergence

  function par_sync_newtonur_flag(this, inewtonur) result(ivalue)
    class(ParallelSolutionType) :: this !< parallel solution instance
    integer(I4B), intent(in) :: inewtonur !< local Newton Under-relaxation flag
    ! local
    integer(I4B) :: ivalue !< Maximum of all local values (1 = under-relaxation applied)
    integer :: ierr
    type(MpiWorldType), pointer :: mpi_world

    mpi_world => get_mpi_world()
    call MPI_Allreduce(inewtonur, ivalue, 1, MPI_INTEGER, &
                       MPI_MAX, mpi_world%comm, ierr)
    call CHECK_MPI(ierr)

  end function par_sync_newtonur_flag

  function par_nur_has_converged(this, dxold_max, hncg) &
    result(has_converged)
    class(ParallelSolutionType) :: this !< parallel solution instance
    real(DP), intent(in) :: dxold_max !< the maximum dependent variable change for cells not adjusted by NUR
    real(DP), intent(in) :: hncg !< largest dep. var. change at end of Picard iter.
    logical(LGP) :: has_converged !< True, when converged
    ! local
    integer(I4B) :: icnvg_local
    integer(I4B) :: icnvg_global
    integer :: ierr
    type(MpiWorldType), pointer :: mpi_world

    mpi_world => get_mpi_world()

    has_converged = .false.
    icnvg_local = 0
    if (this%NumericalSolutionType%sln_nur_has_converged( &
        dxold_max, hncg)) then
      icnvg_local = 1
    end if

    call MPI_Allreduce(icnvg_local, icnvg_global, 1, MPI_INTEGER, &
                       MPI_MIN, mpi_world%comm, ierr)
    call CHECK_MPI(ierr)
    if (icnvg_global == 1) has_converged = .true.

  end function par_nur_has_converged

  !> @brief Calculate pseudo-transient continuation factor
  !< for the parallel case
  subroutine par_calc_ptc(this, iptc, ptcf)
    class(ParallelSolutionType) :: this !< parallel solution
    integer(I4B) :: iptc !< PTC (1) or not (0)
    real(DP) :: ptcf !< the (global) PTC factor calculated
    ! local
    integer(I4B) :: iptc_loc
    real(DP) :: ptcf_loc, ptcf_glo_max
    integer :: ierr
    type(MpiWorldType), pointer :: mpi_world

    mpi_world => get_mpi_world()
    call this%NumericalSolutionType%sln_calc_ptc(iptc_loc, ptcf_loc)
    if (iptc_loc == 0) ptcf_loc = DZERO

    ! now reduce
    call MPI_Allreduce(ptcf_loc, ptcf_glo_max, 1, MPI_DOUBLE_PRECISION, &
                       MPI_MAX, mpi_world%comm, ierr)
    call CHECK_MPI(ierr)

    iptc = 0
    ptcf = DZERO
    if (ptcf_glo_max > DZERO) then
      iptc = 1
      ptcf = ptcf_glo_max
    end if

  end subroutine par_calc_ptc

  !> @brief apply under-relaxation in sync over all processes
  !<
  subroutine par_underrelax(this, kiter, bigch, neq, active, x, xtemp)
    class(ParallelSolutionType) :: this !< parallel instance
    integer(I4B), intent(in) :: kiter !< Picard iteration number
    real(DP), intent(in) :: bigch !< maximum dependent-variable change
    integer(I4B), intent(in) :: neq !< number of equations
    integer(I4B), dimension(neq), intent(in) :: active !< active cell flag (1)
    real(DP), dimension(neq), intent(inout) :: x !< current dependent-variable
    real(DP), dimension(neq), intent(in) :: xtemp !< previous dependent-variable
    ! local
    real(DP) :: dvc_global_max, dvc_global_min
    integer :: ierr
    type(MpiWorldType), pointer :: mpi_world

    mpi_world => get_mpi_world()

    ! first reduce largest change over all processes
    call MPI_Allreduce(bigch, dvc_global_max, 1, MPI_DOUBLE_PRECISION, &
                       MPI_MAX, mpi_world%comm, ierr)
    call CHECK_MPI(ierr)
    call MPI_Allreduce(bigch, dvc_global_min, 1, MPI_DOUBLE_PRECISION, &
                       MPI_MIN, mpi_world%comm, ierr)
    call CHECK_MPI(ierr)

    if (abs(dvc_global_min) > abs(dvc_global_max)) then
      dvc_global_max = dvc_global_min
    end if

    ! call local underrelax routine
    call this%NumericalSolutionType%sln_underrelax(kiter, dvc_global_max, &
                                                   neq, active, x, xtemp)

  end subroutine par_underrelax

  !> @brief synchronize backtracking flag over processes
  !<
  subroutine par_backtracking_xupdate(this, btflag)
    ! -- dummy variables
    class(ParallelSolutionType), intent(inout) :: this !< ParallelSolutionType instance
    integer(I4B), intent(inout) :: btflag !< global backtracking flag (1) backtracking performed (0) backtracking not performed
    ! -- local variables
    integer(I4B) :: btflag_local
    type(MpiWorldType), pointer :: mpi_world
    integer :: ierr

    mpi_world => get_mpi_world()

    btflag_local = 0
    call this%NumericalSolutionType%sln_backtracking_xupdate(btflag_local)

    call MPI_Allreduce(btflag_local, btflag, 1, MPI_INTEGER, &
                       MPI_MAX, mpi_world%comm, ierr)
    call CHECK_MPI(ierr)

  end subroutine par_backtracking_xupdate

end module ParallelSolutionModule

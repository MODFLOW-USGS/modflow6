module ParallelSolutionModule
  use KindModule, only: DP, LGP, I4B
  use ConstantsModule, only: DONE, DZERO
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
    procedure :: sln_calc_ptc => par_calc_ptc
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
    global_max_dvc = huge(0.0)
    abs_max_dvc = abs(max_dvc)
    call MPI_Allreduce(abs_max_dvc, global_max_dvc, 1, MPI_DOUBLE_PRECISION, &
                       MPI_MAX, mpi_world%comm, ierr)
    if (global_max_dvc <= this%dvclose) then
      has_converged = .true.
    end if

  end function par_has_converged

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

    iptc = 0
    ptcf = DZERO
    if (ptcf_glo_max > DZERO) then
      iptc = 1
      ptcf = ptcf_glo_max
    end if

  end subroutine par_calc_ptc

end module ParallelSolutionModule

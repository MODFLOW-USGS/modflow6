module ParallelSolutionModule
  use KindModule, only: DP, LGP
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
    procedure :: sln_l2norm => par_l2norm
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

  subroutine par_l2norm(this, l2norm)
    class(ParallelSolutionType), intent(inout) :: this !< parallel solution
    real(DP), intent(inout) :: l2norm !< calculated L-2 norm
    
    l2norm = this%linear_solver%get_l2_norm(this%vec_x, this%vec_rhs, &
                                            this%active)

  end subroutine par_l2norm

end module ParallelSolutionModule

module ConvergenceSummaryModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENMEMPATH, DZERO
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, mem_reallocate

  implicit none
  private

  public :: ConvergenceSummaryType

  !> This structure stores the generic convergence info for a solution
  !<
  type :: ConvergenceSummaryType
    character(len=LENMEMPATH) :: memory_path !< the path for storing solution variables in the memory manager
    integer(I4B) :: iter_cnt !< tracks the iteration number within the timestep
    integer(I4B), pointer :: convnmod => null() !< number of models in the solution
    integer(I4B), dimension(:), pointer :: model_bounds => null() !< the start and stop index of the models in the solution
    integer(I4B), pointer :: nitermax => null() !< max. nr. of iterations in a timestep
    integer(I4B), dimension(:), pointer, contiguous :: itinner => null() !< inner iteration number within each picard iteration
    integer(I4B), dimension(:), pointer, contiguous :: locdv => null() !< location of the maximum dependent-variable change in the solution
    real(DP), dimension(:), pointer, contiguous :: dvmax => null() !< maximum dependent-variable change in the solution
    integer(I4B), dimension(:), pointer, contiguous :: locr => null() !< location of the maximum flow change in the solution
    real(DP), dimension(:), pointer, contiguous :: rmax => null() !< maximum flow change in the solution
    integer(I4B), pointer, dimension(:, :), contiguous :: convlocdv => null() !< location of the maximum dependent-variable change in each model in the solution
    real(DP), pointer, dimension(:, :), contiguous :: convdvmax => null() !< maximum dependent-variable change for each model in the solution
    integer(I4B), pointer, dimension(:, :), contiguous :: convlocr => null() !< location of the maximum flow change in each model in the solution
    real(DP), pointer, dimension(:, :), contiguous :: convrmax => null() !< maximum flow change in each model in the solution
  contains
    procedure :: init
    procedure :: reinit
    procedure :: destroy
    ! private
    procedure, private :: set_defaults
  end type ConvergenceSummaryType

contains

  !> @brief Initialize the convergence summary for a solution
  subroutine init(this, nr_models, model_bounds, mem_path)
    class(ConvergenceSummaryType) :: this
    integer(I4B) :: nr_models !< the number of models in the solution
    integer(I4B), dimension(:), pointer :: model_bounds !< the start and stop index of the models
    character(len=*) :: mem_path !< the memory path of the owning solution

    this%memory_path = 'TMP'//mem_path
    this%iter_cnt = 0
    this%model_bounds => model_bounds

    call mem_allocate(this%convnmod, 'CONVNMOD', this%memory_path)
    call mem_allocate(this%nitermax, 'NITERMAX', this%memory_path)
    this%convnmod = nr_models
    this%nitermax = 0

    call mem_allocate(this%itinner, 0, 'ITINNER', this%memory_path)
    call mem_allocate(this%locdv, this%convnmod, 'LOCDV', this%memory_path)
    call mem_allocate(this%dvmax, this%convnmod, 'DVMAX', this%memory_path)
    call mem_allocate(this%locr, this%convnmod, 'LOCDR', this%memory_path)
    call mem_allocate(this%rmax, this%convnmod, 'DRMAX', this%memory_path)
    call mem_allocate(this%convdvmax, this%convnmod, 0, 'CONVDVMAX', &
                      this%memory_path)
    call mem_allocate(this%convlocdv, this%convnmod, 0, 'CONVLOCDV', &
                      this%memory_path)
    call mem_allocate(this%convrmax, this%convnmod, 0, 'CONVDRMAX', &
                      this%memory_path)
    call mem_allocate(this%convlocr, this%convnmod, 0, 'CONVLOCDR', &
                      this%memory_path)

    call this%set_defaults()

  end subroutine init

  subroutine reinit(this, niter_max)
    class(ConvergenceSummaryType) :: this
    integer(I4B) :: niter_max !< max. nr. of iterations in a timestep

    this%nitermax = niter_max
    call mem_reallocate(this%itinner, niter_max, 'ITINNER', this%memory_path)
    call mem_reallocate(this%convdvmax, this%convnmod, niter_max, 'CONVDVMAX', &
                        this%memory_path)
    call mem_reallocate(this%convlocdv, this%convnmod, niter_max, 'CONVLOCDV', &
                        this%memory_path)
    call mem_reallocate(this%convrmax, this%convnmod, niter_max, 'CONVDRMAX', &
                        this%memory_path)
    call mem_reallocate(this%convlocr, this%convnmod, niter_max, 'CONVLOCDR', &
                        this%memory_path)

    call this%set_defaults()

  end subroutine reinit

  subroutine set_defaults(this)
    class(ConvergenceSummaryType) :: this
    ! local
    integer(I4B) :: i, j

    do i = 1, this%convnmod
      this%locr(i) = 0
      this%dvmax(i) = DZERO
      this%locdv(i) = 0
      this%rmax(i) = DZERO
    end do

    do i = 1, this%nitermax
      this%itinner(i) = 0
      do j = 1, this%convnmod
        this%convdvmax(j, i) = DZERO
        this%convlocdv(j, i) = 0
        this%convrmax(j, i) = DZERO
        this%convlocr(j, i) = 0
      end do
    end do

  end subroutine set_defaults

  !> @brief Cleanup
  !<
  subroutine destroy(this)
    class(ConvergenceSummaryType) :: this

    ! scalars
    call mem_deallocate(this%convnmod)
    call mem_deallocate(this%nitermax)

    call mem_deallocate(this%locr)
    call mem_deallocate(this%rmax)
    call mem_deallocate(this%locdv)
    call mem_deallocate(this%dvmax)

    ! arrays
    call mem_deallocate(this%itinner)
    call mem_deallocate(this%convdvmax)
    call mem_deallocate(this%convlocdv)
    call mem_deallocate(this%convrmax)
    call mem_deallocate(this%convlocr)

  end subroutine destroy

end module ConvergenceSummaryModule

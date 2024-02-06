module MpiUnitCacheModule
  use KindModule, only: I4B, LGP
  use ListModule
  use STLVecIntModule
  use SimStagesModule, only: NR_SIM_STAGES
  use mpi
  implicit none
  private

  integer(I4B), public, parameter :: NO_CACHED_VALUE = -1

  type, public :: MpiUnitCacheType
    ! private
    type(STLVecInt), private :: cached_ranks
    type(STLVecInt), private :: cached_messages
    integer(I4B), private :: nr_stages
    integer(I4B), private :: nr_steps
  contains
    procedure :: init => cc_init
    procedure :: get_cached => cc_get_cached
    procedure :: cache => mc_cache
    procedure :: destroy => cc_destroy
    ! private
    procedure, private :: is_rank_cached
    procedure, private :: add_rank_cache
    procedure, private :: get_rank_index
    procedure, private :: get_msg_index
  end type MpiUnitCacheType

contains

  !> @brief Initialize the unit cache.
  !<
  subroutine cc_init(this, nr_stages, nr_steps)
    class(MpiUnitCacheType) :: this
    integer(I4B) :: nr_stages !< number of (simulation) stages
    integer(I4B) :: nr_steps !< number of routing steps per stage

    this%nr_stages = nr_stages
    this%nr_steps = nr_steps
    call this%cached_ranks%init()
    call this%cached_messages%init()

  end subroutine cc_init

  !> @brief Get the cached mpi type for this rank and
  !< stage. Equal to NO_CACHED_VALUE when not present.
  function cc_get_cached(this, rank, stage, step) result(mpi_type)
    class(MpiUnitCacheType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage
    integer(I4B) :: step
    integer :: mpi_type
    ! local
    integer(I4B) :: msg_idx

    mpi_type = NO_CACHED_VALUE
    msg_idx = this%get_msg_index(rank, stage, step)
    if (msg_idx > 0) then
      mpi_type = this%cached_messages%at(msg_idx)
    end if

  end function cc_get_cached

  !> @brief Cache the mpi datatype for this particular
  !! rank and stage. The datatype should be committed
  !< to the type database externally.
  subroutine mc_cache(this, rank, stage, step, mpi_type)
    class(MpiUnitCacheType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage
    integer(I4B) :: step
    integer :: mpi_type
    ! local
    integer(I4B) :: msg_idx

    ! add if rank not present in cache yet
    if (.not. this%is_rank_cached(rank)) then
      call this%add_rank_cache(rank)
    end if

    ! rank has been added to cache, now set
    ! mpi datatype for this stage's message:
    msg_idx = this%get_msg_index(rank, stage, step)
    call this%cached_messages%set(msg_idx, mpi_type)

  end subroutine mc_cache

  function is_rank_cached(this, rank) result(in_cache)
    class(MpiUnitCacheType) :: this
    integer(I4B) :: rank
    logical(LGP) :: in_cache

    in_cache = this%cached_ranks%contains(rank)

  end function is_rank_cached

  subroutine add_rank_cache(this, rank)
    class(MpiUnitCacheType) :: this
    integer(I4B) :: rank
    ! local
    integer(I4B) :: i, j

    call this%cached_ranks%push_back(rank)
    do i = 1, this%nr_stages
      do j = 1, this%nr_steps
        call this%cached_messages%push_back(NO_CACHED_VALUE)
      end do
    end do

  end subroutine add_rank_cache

  !> @Brief returns -1 when not present
  !<
  function get_rank_index(this, rank) result(rank_index)
    class(MpiUnitCacheType) :: this
    integer(I4B) :: rank
    integer(I4B) :: rank_index

    rank_index = this%cached_ranks%get_index(rank)

  end function get_rank_index

  !> @Brief returns -1 when not present
  !<
  function get_msg_index(this, rank, stage, step) result(msg_index)
    class(MpiUnitCacheType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage
    integer(I4B) :: step
    integer(I4B) :: msg_index
    ! local
    integer(I4B) :: rank_idx
    integer(I4B) :: rank_offset, stage_offset

    msg_index = -1
    rank_idx = this%get_rank_index(rank)
    if (rank_idx < 1) return

    rank_offset = (rank_idx - 1) * (this%nr_stages * this%nr_steps)
    stage_offset = (stage - 1) * this%nr_steps
    msg_index = rank_offset + stage_offset + step

  end function get_msg_index

  !> @brief Clean up the unit cache.
  !<
  subroutine cc_destroy(this)
    class(MpiUnitCacheType) :: this
    ! local
    integer(I4B) :: i
    integer :: mpi_type, ierr

    do i = 1, this%cached_messages%size
      mpi_type = this%cached_messages%at(i)
      if (mpi_type /= NO_CACHED_VALUE) then
        call MPI_Type_free(mpi_type, ierr)
      end if
    end do

    call this%cached_ranks%destroy()
    call this%cached_messages%destroy()

  end subroutine cc_destroy

end module

module MpiMessageCacheModule
  use KindModule, only: I4B, LGP
  use STLVecIntModule
  use SimStagesModule, only: NR_SIM_STAGES
  implicit none
  private

  integer(I4B), parameter :: NO_CACHED_VALUE = -1

  type, public :: MpiMessageCacheType
    type(STLVecInt) :: cached_ranks
    type(STLVecInt) :: cached_messages
  contains
    procedure :: init => mc_init
    procedure :: is_cached => mc_is_cached
    procedure :: cache => mc_cache
    procedure :: get_cached => mc_get_cached
    procedure :: destroy => mc_destroy
    ! private
    procedure, private :: is_rank_cached
    procedure, private :: is_stage_cached
    procedure, private :: add_rank_cache
    procedure, private :: get_rank_index
    procedure, private :: get_msg_index
  end type MpiMessageCacheType

contains

  subroutine mc_init(this)
    class(MpiMessageCacheType) :: this

    call this%cached_ranks%init()
    call this%cached_messages%init()

  end subroutine mc_init

  function mc_is_cached(this, rank, stage) result(in_cache)
    class(MpiMessageCacheType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage
    logical(LGP) :: in_cache
    ! local
    integer(I4B) :: msg_index

    in_cache = .false.
    msg_index = this%get_msg_index(rank, stage)
    if (msg_index > 0) then
      in_cache = (this%cached_messages%at(msg_index) /= NO_CACHED_VALUE)
    end if

  end function mc_is_cached

  subroutine mc_cache(this, rank, stage, mpi_type)
    class(MpiMessageCacheType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage
    integer :: mpi_type
    ! local
    integer(I4B) :: msg_idx

    ! add if rank not present in cache yet
    if (.not. this%is_rank_cached(rank)) then
      call this%add_rank_cache(rank)
    end if

    ! rank has been added to cache, now set
    ! mpi datatype for this stage's message:
    msg_idx = this%get_msg_index(rank, stage)
    call this%cached_messages%set(msg_idx, mpi_type)

  end subroutine mc_cache

  function mc_get_cached(this, rank, stage) result(mpi_type)
    class(MpiMessageCacheType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage
    integer :: mpi_type
    ! local
    integer(I4B) :: msg_idx

    msg_idx = this%get_msg_index(rank, stage)
    mpi_type = this%cached_messages%at(msg_idx)

  end function mc_get_cached

  function is_rank_cached(this, rank) result(in_cache)
    class(MpiMessageCacheType) :: this
    integer(I4B) :: rank
    logical(LGP) :: in_cache

    in_cache = this%cached_ranks%contains(rank)

  end function is_rank_cached

  function is_stage_cached(this, rank, stage) result(in_cache)
    class(MpiMessageCacheType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage
    logical(LGP) :: in_cache

    in_cache = .false.

  end function is_stage_cached

  subroutine add_rank_cache(this, rank)
    class(MpiMessageCacheType) :: this
    integer(I4B) :: rank
    ! local
    integer(I4B) :: i

    call this%cached_ranks%push_back(rank)
    do i = 1, NR_SIM_STAGES
      call this%cached_messages%push_back(NO_CACHED_VALUE)
    end do

  end subroutine add_rank_cache

  !> @Brief returns -1 when not present
  !<
  function get_rank_index(this, rank) result(rank_index)
    class(MpiMessageCacheType) :: this
    integer(I4B) :: rank
    integer(I4B) :: rank_index

    rank_index = this%cached_ranks%get_index(rank)

  end function get_rank_index

  !> @Brief returns -1 when not present
  !<
  function get_msg_index(this, rank, stage) result(msg_index)
    class(MpiMessageCacheType) :: this
    integer(I4B) :: rank
    integer(I4B) :: stage
    integer(I4B) :: msg_index
    ! local
    integer(I4B) :: rank_idx

    msg_index = -1
    rank_idx = this%get_rank_index(rank)
    if (rank_idx < 1) return

    msg_index = (rank_idx - 1) * NR_SIM_STAGES + stage

  end function get_msg_index

  subroutine mc_destroy(this)
    class(MpiMessageCacheType) :: this

    call this%cached_ranks%destroy()
    call this%cached_messages%destroy()

  end subroutine mc_destroy

end module

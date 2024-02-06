module MpiMessageCacheModule
  use KindModule, only: I4B
  use SimStagesModule, only: NR_SIM_STAGES
  use ListModule
  use STLVecIntModule
  use MpiUnitCacheModule
  implicit none
  private

  ! the communication steps of routing a simulation stage:

  integer(I4B), public, parameter :: MPI_HDR_RCV = 1 !< receiving headers from ranks
  integer(I4B), public, parameter :: MPI_HDR_SND = 2 !< sending headers to ranks
  integer(I4B), public, parameter :: MPI_MAP_RCV = 3 !< receiving requested data maps from ranks
  integer(I4B), public, parameter :: MPI_MAP_SND = 4 !< sending requested data maps to ranks
  integer(I4B), public, parameter :: MPI_BDY_RCV = 5 !< receiving data (body) from ranks
  integer(I4B), public, parameter :: MPI_BDY_SND = 6 !< sending data (body) to ranks
  integer(I4B), public, parameter :: NR_COMM_STEPS = 6 !< the total number of communication steps in the routing a stage, for a unit

  ! expose this from the unit cache module
  public :: NO_CACHED_VALUE

  !> @brief Facility to cache the constructed MPI datatypes.
  !! This will avoid having to construct them over and over
  !! again for the communication inside the timestep loop.
  !! This class deals with separate caches for different
  !! units (solutions mostly) and for different parts
  !< (elements) of the message.
  type, public :: MpiMessageCacheType
    type(STLVecInt) :: cached_ids !< a vector with ids for the cached units (e.g. solution ids or 0 for global)
    type(ListType) :: unit_caches !< a cache list per communication step in routing a stage,
    !< with the list containing a cache per unit
  contains
    procedure :: init => mmc_init
    procedure :: get => mmc_get
    procedure :: put => mmc_put
    procedure :: destroy => mmc_destroy
  end type MpiMessageCacheType

contains

  !< @brief Initialize the MPI type cache system.
  !<
  subroutine mmc_init(this)
    class(MpiMessageCacheType) :: this !< the message cache

    call this%cached_ids%init()

  end subroutine mmc_init

  !< @brief Get the cached mpi datatype for the given
  !! unit, rank, stage, and message element. Returns
  !< NO_CACHED_VALUE when not in cache.
  function mmc_get(this, unit, rank, stage, step) result(mpi_type)
    class(MpiMessageCacheType) :: this !< the message cache
    integer(I4B) :: unit !< the unit (e.g. solution or global)
    integer(I4B) :: rank !< the rank of the MPI process to communicate with
    integer(I4B) :: stage !< the simulation stage at which the message is sent
    integer(I4B) :: step !< the communication step as an integer between 1 and NR_COMM_STEPS (see above for predefined values)
    integer :: mpi_type !< the resulting mpi datatype
    ! local
    integer(I4B) :: unit_idx
    class(*), pointer :: obj_ptr

    mpi_type = NO_CACHED_VALUE

    unit_idx = this%cached_ids%get_index(unit)
    if (unit_idx == -1) return ! not cached

    obj_ptr => this%unit_caches%GetItem(unit_idx)
    select type (obj_ptr)
    class is (MpiUnitCacheType)
      mpi_type = obj_ptr%get_cached(rank, stage, step)
    end select

  end function mmc_get

  !> @brief Put the mpi datatype for this particular unit,
  !! rank, and stage in cache. The datatype should be
  !< committed to the type database externally.
  subroutine mmc_put(this, unit, rank, stage, step, mpi_type)
    class(MpiMessageCacheType) :: this !< the message cache
    integer(I4B) :: unit !< the unit (e.g. solution or global)
    integer(I4B) :: rank !< the rank of the MPI process to communicate with
    integer(I4B) :: stage !< the simulation stage at which the message is sent
    integer(I4B) :: step !< the communication step as an integer between 1 and NR_COMM_STEPS (see above for predefined values)
    integer :: mpi_type !< the mpi datatype to cache
    ! local
    integer(I4B) :: unit_idx
    type(MpiUnitCacheType), pointer :: new_cache
    class(*), pointer :: obj_ptr

    unit_idx = this%cached_ids%get_index(unit)
    if (unit_idx == -1) then
      ! add to vector with cached unit ids
      call this%cached_ids%push_back(unit)
      ! create and add unit cache
      allocate (new_cache)
      call new_cache%init(NR_SIM_STAGES, NR_COMM_STEPS)
      obj_ptr => new_cache
      call this%unit_caches%Add(obj_ptr)
      unit_idx = this%cached_ids%size
    end if

    ! get the cache for this unit and communication step
    obj_ptr => this%unit_caches%GetItem(unit_idx)
    select type (obj_ptr)
    class is (MpiUnitCacheType)
      call obj_ptr%cache(rank, stage, step, mpi_type)
    end select

  end subroutine mmc_put

  !< @brief Destroy the MPI type cache system.
  !<
  subroutine mmc_destroy(this)
    class(MpiMessageCacheType) :: this !< the message cache
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj_ptr

    ! clear caches
    do i = 1, this%cached_ids%size
      obj_ptr => this%unit_caches%GetItem(i)
      select type (obj_ptr)
      class is (MpiUnitCacheType)
        call obj_ptr%destroy()
      end select
    end do
    call this%unit_caches%Clear(destroy=.true.)

    call this%cached_ids%destroy()

  end subroutine mmc_destroy

end module

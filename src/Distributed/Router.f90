module RouterModule
  use KindModule, only: I4B, LGP
  use SimStagesModule
  use VectorIntModule
  use NumericalSolutionModule, only: NumericalSolutionType
  use DistributedModelModule
  use MemoryTypeModule, only: MemoryType
  use MemoryManagerModule, only: get_from_memorylist
  use RemoteMemoryModule
  use DistributedExchangeModule
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                          GetSpatialModelConnectionFromList
  implicit none
  private

  public :: RouterType

  type :: RouterType
    integer(I4B) :: nr_solutions
    type(VectorInt), dimension(:), pointer :: halo_models
    type(VectorInt), dimension(:), pointer :: halo_exchanges
  contains
    procedure :: init
    procedure :: add_solution
    procedure :: init_connectivity
    procedure :: route
    procedure :: destroy

    procedure, private :: route_item_map
    procedure, private :: route_item_nomap
  end type RouterType

  contains

  subroutine init(this, n_sol)
    class(RouterType) :: this
    ! local
    integer(I4B) :: n_sol

    allocate(this%halo_models(n_sol))
    allocate(this%halo_exchanges(n_sol))

    this%nr_solutions = 0

  end subroutine init

  ! aggregate the halo models and exchanges over interfaces
  subroutine add_solution(this, num_sol)
    class(RouterType) :: this
    class(NumericalSolutionType), pointer :: num_sol
    ! local
    integer(I4B) :: isol = 0
    integer(I4B) :: ix, im, ihx
    integer(I4B) :: model_id, exg_id
    class(SpatialModelConnectionType), pointer :: mod_conn

    isol = this%nr_solutions + 1

    call this%halo_models(isol)%init()
    call this%halo_exchanges(isol)%init()

    do ix = 1, num_sol%exchangelist%Count()
      mod_conn => GetSpatialModelConnectionFromList(num_sol%exchangelist, ix)
      if (.not. associated(mod_conn)) cycle

      ! extend
      do im = 1, mod_conn%haloModels%size
        model_id = mod_conn%haloModels%at(im)
        if (.not. this%halo_models(isol)%contains(model_id)) then
          call this%halo_models(isol)%push_back(model_id)
        end if                    
      end do
      do ihx = 1, mod_conn%haloExchanges%size
        exg_id = mod_conn%haloExchanges%at(ihx)
        if (.not. this%halo_exchanges(isol)%contains(exg_id)) then
          call this%halo_exchanges(isol)%push_back(exg_id)
        end if
      end do
    end do
    
    this%nr_solutions = this%nr_solutions + 1

  end subroutine add_solution

  subroutine init_connectivity(this)
    class(RouterType) :: this
    ! local
    integer(I4B) :: isol, imod, iexg
    class(DistributedModelType), pointer :: dist_model
    class(DistributedExchangeType), pointer :: dist_exg
    
    ! prepare for connection data
    do isol = 1, this%nr_solutions
      do imod = 1, this%halo_models(isol)%size
        dist_model => get_dist_model(this%halo_models(isol)%at(imod))
        call dist_model%init_connectivity()
      end do
      do iexg = 1, this%halo_exchanges(isol)%size
        dist_exg => get_dist_exg(this%halo_exchanges(isol)%at(iexg))
        call dist_exg%init_connectivity()
      end do
    end do

  end subroutine init_connectivity

  ! TODO_MJR: rename
  subroutine route(this, stage)
    class(RouterType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: isol, imod, imem
    class(DistributedModelType), pointer :: dist_model
    class(RemoteMemoryType), pointer :: rmt_mem
    integer(I4B), dimension(:), pointer :: src_idx

    do isol = 1, this%nr_solutions
      do imod = 1, this%halo_models(isol)%size
        dist_model => get_dist_model(this%halo_models(isol)%at(imod))
        do imem = 1, dist_model%remote_mem_items%Count()
          rmt_mem => dist_model%get_rmt_mem(imem)
          if (rmt_mem%stage == stage) then

            if (rmt_mem%map_type == MAP_TYPE_NA) then
              call this%route_item_nomap(rmt_mem)
            else
              src_idx => dist_model%get_src_map(rmt_mem%map_type)
              call this%route_item_map(rmt_mem, src_idx)
            end if

          end if
        end do
      end do
    end do

  end subroutine route

  subroutine route_item_nomap(this, rmt_mem)
    class(RouterType) :: this
    class(RemoteMemoryType), pointer :: rmt_mem
    ! local
    type(MemoryType), pointer :: mt
    type(MemoryType), pointer :: rmt_mt
    logical(LGP) :: found
    integer(I4B) :: i

    mt => rmt_mem%local_mt
    call get_from_memorylist(rmt_mem%var_name, rmt_mem%mem_path, rmt_mt, found)
    if (associated(mt%intsclr)) then
      mt%intsclr = rmt_mt%intsclr
    else if (associated(mt%aint1d)) then
      do i = 1, size(mt%aint1d)
        mt%aint1d(i) = rmt_mt%aint1d(i)
      end do
    else if (associated(mt%dblsclr)) then
      mt%dblsclr = rmt_mt%dblsclr
    else if (associated(mt%adbl1d)) then
      do i = 1, size(mt%adbl1d)
        mt%adbl1d(i) = rmt_mt%adbl1d(i)
      end do
    end if

  end subroutine route_item_nomap

  subroutine route_item_map(this, rmt_mem, src_idx)
    class(RouterType) :: this
    class(RemoteMemoryType), pointer :: rmt_mem
    integer(I4B), dimension(:), pointer :: src_idx
    ! local
    type(MemoryType), pointer :: mt
    type(MemoryType), pointer :: rmt_mt
    logical(LGP) :: found
    integer(I4B) :: i

    mt => rmt_mem%local_mt
    call get_from_memorylist(rmt_mem%var_name, rmt_mem%mem_path, rmt_mt, found)
    if (associated(mt%aint1d)) then
      do i = 1, size(mt%aint1d)
        mt%aint1d(i) = rmt_mt%aint1d(src_idx(i))
      end do
    else if (associated(mt%adbl1d)) then
      do i = 1, size(mt%adbl1d)
        mt%adbl1d(i) = rmt_mt%adbl1d(src_idx(i))
      end do
    end if
    
  end subroutine route_item_map

  subroutine destroy(this)
    class(RouterType) :: this
    ! local
    integer(I4B) :: i

    do i = 1, this%nr_solutions
      call this%halo_models(i)%destroy()
      call this%halo_exchanges(i)%destroy()
    end do
    deallocate(this%halo_models)
    deallocate(this%halo_exchanges)

  end subroutine destroy
  

end module RouterModule
module RouterModule
  use KindModule, only: I4B, LGP
  use SimStagesModule
  use VectorIntModule
  use ListsModule, only: basesolutionlist
  use NumericalSolutionModule, only: NumericalSolutionType, &
                                     GetNumericalSolutionFromList
  use DistributedBaseModule
  use DistributedModelModule  
  use DistributedExchangeModule
  use MemoryTypeModule, only: MemoryType
  use MemoryManagerModule, only: get_from_memorylist
  use RemoteMemoryModule
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                          GetSpatialModelConnectionFromList
  use InterfaceMapModule
  implicit none
  private

  public :: RouterType

  type :: RouterType
    integer(I4B) :: nr_solutions
    integer(I4B), dimension(:), pointer :: solution_ids => null()
    type(VectorInt), dimension(:), pointer :: model_ids => null()
    type(VectorInt), dimension(:), pointer :: exchange_ids => null()
    class(InterfaceMapType), dimension(:), pointer :: interface_maps => null()
  contains
    procedure :: init
    procedure :: add_solution
    procedure :: init_connectivity
    procedure :: create_interface_map
    procedure :: init_interface
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

    allocate(this%solution_ids(n_sol))
    allocate(this%model_ids(n_sol))
    allocate(this%exchange_ids(n_sol))
    allocate(this%interface_maps(n_sol))

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
    this%solution_ids(isol) = num_sol%id

    call this%model_ids(isol)%init()
    call this%exchange_ids(isol)%init()

    do ix = 1, num_sol%exchangelist%Count()
      mod_conn => GetSpatialModelConnectionFromList(num_sol%exchangelist, ix)
      if (.not. associated(mod_conn)) cycle

      ! add primary model for this connection
      model_id = mod_conn%owner%id
      call this%model_ids(isol)%push_back_unique(model_id)

      ! extend
      do im = 1, mod_conn%haloModels%size
        model_id = mod_conn%haloModels%at(im)
        call this%model_ids(isol)%push_back_unique(model_id)
      end do
      do ihx = 1, mod_conn%haloExchanges%size
        exg_id = mod_conn%haloExchanges%at(ihx)
        call this%exchange_ids(isol)%push_back_unique(exg_id)
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
      do imod = 1, this%model_ids(isol)%size
        dist_model => get_dist_model(this%model_ids(isol)%at(imod))
        call dist_model%init_connectivity()
      end do
      do iexg = 1, this%exchange_ids(isol)%size
        dist_exg => get_dist_exg(this%exchange_ids(isol)%at(iexg))
        call dist_exg%init_connectivity()
      end do
    end do

  end subroutine init_connectivity

  subroutine create_interface_map(this)
    class(RouterType) :: this
    ! local
    class(NumericalSolutionType), pointer :: num_sol
    integer(I4B) :: isol, sol_idx
    integer(I4B) :: iexg
    class(SpatialModelConnectionType), pointer :: conn
    type(VectorInt) :: models, exchanges

    do isol = 1, this%nr_solutions
      ! this index should always match a numerical solution
      sol_idx = this%solution_ids(isol)
      num_sol => GetNumericalSolutionFromList(basesolutionlist, sol_idx)

      ! first count nr. of unique models and exchanges in the
      ! interface maps to allocate memory for the combined map
      call models%init()
      call exchanges%init()
      do iexg = 1, num_sol%exchangelist%Count()
        conn => GetSpatialModelConnectionFromList(num_sol%exchangelist, iexg)
        if (.not. associated(conn)) cycle
        ! collect unique models
        call models%add_array_unique(conn%interfaceMap%model_ids)
        call exchanges%add_array_unique(conn%interfaceMap%exchange_ids)
      end do

      ! initialize the merged map for this solution    
      sol_idx = findloc(this%solution_ids, num_sol%id, dim=1)
      call this%interface_maps(sol_idx)%init(models%size, exchanges%size)    
      call models%destroy()
      call exchanges%destroy()

      ! add the map data
      do iexg = 1, num_sol%exchangelist%Count()
        conn => GetSpatialModelConnectionFromList(num_sol%exchangelist, iexg)
        if (.not. associated(conn)) cycle
          ! add another map to the router map
          call this%interface_maps(sol_idx)%add(conn%interfaceMap)
      end do

    end do

  end subroutine create_interface_map

  subroutine init_interface(this)
    class(RouterType) :: this
    ! local
    integer(I4B) :: isol, imod, iexg
    integer(I4B) :: model_id, exchange_id
    class(DistributedModelType), pointer :: dist_model
    class(DistributedExchangeType), pointer :: dist_exg
    class(InterfaceMapType), pointer :: iface_map

    ! for each solution ...
    do isol = 1, this%nr_solutions
      iface_map => this%interface_maps(isol)
      ! loop over all models that are part of this solution's interface      
      do imod = 1, iface_map%nr_models
        model_id = iface_map%model_ids(imod)
        dist_model => get_dist_model(model_id)        
        call dist_model%setup_remote_memory(iface_map%node_map(imod), &
                                        iface_map%connection_map(imod))
      end do

      do iexg = 1, iface_map%nr_exchanges
        exchange_id = iface_map%exchange_ids(iexg)
        dist_exg => get_dist_exg(exchange_id)
        call dist_exg%setup_remote_memory(iface_map%exchange_map(iexg))
      end do

    end do

  end subroutine init_interface

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
      do imod = 1, this%model_ids(isol)%size
        dist_model => get_dist_model(this%model_ids(isol)%at(imod))
        do imem = 1, dist_model%remote_mem_items%Count()
          rmt_mem => dist_model%get_rmt_mem(imem)
          if (findloc(rmt_mem%stages, stage, dim=1) > 0) then
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
        if (src_idx(i) > size(rmt_mt%adbl1d)) then
          write(*,*) 'oops'
        end if
        mt%adbl1d(i) = rmt_mt%adbl1d(src_idx(i))
      end do
    end if
    
  end subroutine route_item_map

  subroutine destroy(this)
    class(RouterType) :: this
    ! local
    integer(I4B) :: i

    do i = 1, this%nr_solutions
      call this%model_ids(i)%destroy()
      call this%exchange_ids(i)%destroy()
    end do
    deallocate(this%model_ids)
    deallocate(this%exchange_ids)

  end subroutine destroy
  

end module RouterModule
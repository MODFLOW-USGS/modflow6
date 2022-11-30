module MapperModule
  use KindModule, only: I4B, LGP
  use ConstantsModule, only: LENVARNAME, LENMEMPATH  
  use MemoryHelperModule, only: create_mem_path
  use IndexMapModule
  use DistributedModelModule
  use DistributedExchangeModule
  use InterfaceMapModule
  use DistVariableModule
  use MappedMemoryModule
  use ListModule
  implicit none
  private

  public :: MapperType

  type :: MapperType

    type(ListType) :: mapped_data_list

    contains

    procedure :: init
    procedure :: add_dist_vars
    procedure :: scatter
    procedure :: destroy

    procedure, private :: map_model_data
    procedure, private :: map_exg_data
    procedure, private :: map_data

  end type MapperType

  contains

  subroutine init(this)
    class(MapperType) :: this

  end subroutine init

  !> @brief Add distributed interface variables as memory mapped items
  !<
  subroutine add_dist_vars(this, sol_id, var_list, interface_map)    
    class(MapperType) :: this
    integer(I4B) :: sol_id
    type(ListType) :: var_list
    type(InterfaceMapType) :: interface_map
    ! local
    integer(I4B) :: i, m, e
    type(DistVarType), pointer :: distvar
    type(IndexMapType), pointer :: idx_map

    ! loop over variables
    do i = 1, var_list%Count()
      distvar => GetDistVarFromList(var_list, i)
      if (distvar%map_type == SYNC_NODES .or. &
          distvar%map_type == SYNC_CONNECTIONS) then
        ! map data for all models in this interface
        do m = 1, interface_map%nr_models

          ! pick the right index map: connection based or node based
          if (distvar%map_type == SYNC_NODES) then
            idx_map => interface_map%node_map(m)
          else if (distvar%map_type == SYNC_CONNECTIONS) then
            idx_map => interface_map%connection_map(m)
          end if

          ! and map ...
          call this%map_model_data(sol_id, &
                                   distvar%comp_name, &
                                   distvar%subcomp_name, &
                                   distvar%var_name, &
                                   interface_map%model_ids(m), &
                                   idx_map, &
                                   distvar%sync_stages)
        end do
      else if (distvar%map_type == SYNC_EXCHANGES) then
        ! map data from the exchanges to the interface
        do e = 1, interface_map%nr_exchanges
          call this%map_exg_data(sol_id, &
                                 distvar%comp_name, &
                                 distvar%subcomp_name, &
                                 distvar%var_name, &
                                 interface_map%exchange_ids(e), &
                                 distvar%exg_var_name, &
                                 interface_map%exchange_map(e), &
                                 distvar%sync_stages)
        end do
      end if
    end do

  end subroutine add_dist_vars

  !> @brief Map data from model memory to a target memory entry,
  !! with the specified map. The source and target items have
  !< the same name and (optionally) subcomponent name.
  subroutine map_model_data(this, controller_id, tgt_model_name, &
                            tgt_subcomp_name, tgt_var_name, src_model_id, &
                            index_map, stages)
    use SimModule, only: ustop
    use MemoryManagerModule, only: get_from_memorylist
    class(MapperType) :: this
    integer(I4B) :: controller_id !< e.g. the numerical solution where synchr. is controlled
    character(len=*), intent(in) :: tgt_model_name
    character(len=*), intent(in) :: tgt_subcomp_name
    character(len=*), intent(in) :: tgt_var_name
    integer(I4B), intent(in) :: src_model_id
    type(IndexMapType), intent(in) :: index_map
    integer(I4B), dimension(:), intent(in) :: stages !< array with 1 or multiple stages for synchronization
    ! local
    character(len=LENVARNAME) :: src_var_name
    character(len=LENMEMPATH) :: src_mem_path, tgt_mem_path
    class(DistributedModelType), pointer :: dist_model

    dist_model => get_dist_model(src_model_id) 
    src_mem_path = dist_model%get_local_mem_path(tgt_subcomp_name)   

    if (len_trim(tgt_subcomp_name) > 0) then
      tgt_mem_path = create_mem_path(tgt_model_name, tgt_subcomp_name)
    else
      tgt_mem_path = create_mem_path(tgt_model_name)
    end if

    src_var_name = tgt_var_name
    call this%map_data(controller_id, &
                       tgt_var_name, tgt_mem_path, index_map%tgt_idx, &
                       src_var_name, src_mem_path, index_map%src_idx, &
                       null(), stages)

  end subroutine map_model_data

  !> @brief Map memory from a Exchange to the specified memory entry,
  !< using the index map
  subroutine map_exg_data(this, controller_id, tgt_model_name, &
                          tgt_subcomp_name, tgt_var_name, src_exg_id, &
                          src_var_name, index_map_sgn, stages)
    use SimModule, only: ustop
    use MemoryManagerModule, only: get_from_memorylist
    class(MapperType) :: this
    integer(I4B) :: controller_id !< e.g. the numerical solution where synchr. is controlled
    character(len=*), intent(in) :: tgt_model_name
    character(len=*), intent(in) :: tgt_subcomp_name
    character(len=*), intent(in) :: tgt_var_name
    integer(I4B), intent(in) :: src_exg_id
    character(len=*), intent(in) :: src_var_name
    type(IndexMapSgnType), intent(in) :: index_map_sgn
    integer(I4B), dimension(:), intent(in) :: stages !< array with 1 or multiple stages for synchronization
    ! local
    character(len=LENMEMPATH) :: src_mem_path, tgt_mem_path
    class(DistributedExchangeType), pointer :: dist_exg

    dist_exg => get_dist_exg(src_exg_id)
    src_mem_path = dist_exg%get_local_mem_path('')

    if (len_trim(tgt_subcomp_name) > 0) then
      tgt_mem_path = create_mem_path(tgt_model_name, tgt_subcomp_name)
    else
      tgt_mem_path = create_mem_path(tgt_model_name)
    end if

    call this%map_data(controller_id, &
                       tgt_var_name, tgt_mem_path, index_map_sgn%tgt_idx, &
                       src_var_name, src_mem_path, index_map_sgn%src_idx, &
                       index_map_sgn%sign, stages)

  end subroutine map_exg_data
  
  !> @brief Generic mapping between two variables in memory, using
  !< an optional sign conversion
  subroutine map_data(this, controller_id, tgt_name, tgt_path, tgt_idx, &
                      src_name, src_path, src_idx, sign_array, stages)
    class(MapperType) :: this
    integer(I4B) :: controller_id
    character(len=*), intent(in) :: tgt_name
    character(len=*), intent(in) :: tgt_path
    integer(I4B), dimension(:), pointer :: tgt_idx
    character(len=*), intent(in) :: src_name
    character(len=*), intent(in) :: src_path
    integer(I4B), dimension(:), pointer :: src_idx
    integer(I4B), dimension(:), pointer :: sign_array
    integer(I4B), dimension(:), intent(in) :: stages
    ! local
    integer(I4B) :: istage, i
    type(MappedMemoryType), pointer :: mapped_data
    class(*), pointer :: obj

    ! loop and set stage bits
    istage = 0
    do i = 1, size(stages)
      istage = ibset(istage, stages(i))
    end do

    ! create MappedVariable and add to list
    allocate (mapped_data)
    mapped_data%controller_id = controller_id
    mapped_data%sync_stage = istage
    mapped_data%src_name = src_name
    mapped_data%src_path = src_path
    mapped_data%src => null()
    mapped_data%tgt_name = tgt_name
    mapped_data%tgt_path = tgt_path
    mapped_data%tgt => null()
    mapped_data%src_idx => src_idx
    mapped_data%tgt_idx => tgt_idx
    mapped_data%sign => sign_array
    obj => mapped_data
    call this%mapped_data_list%Add(obj)

  end subroutine map_data

  !> @brief Scatter the mapped memory, typically into  
  !< the memory space of the interface models
  subroutine scatter(this, controller_id, stage)
    class(MapperType) :: this
    integer(I4B) :: controller_id
    integer(I4B), intent(in) :: stage
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj
    class(MappedMemoryType), pointer :: mapped_data

    ! sync all variables (src => tgt) for a given stage
    do i = 1, this%mapped_data_list%Count()
      obj => this%mapped_data_list%GetItem(i)
      mapped_data => CastAsMappedData(obj)
      if (controller_id > 0 .and. &
          mapped_data%controller_id /= controller_id) cycle
      if (.not. check_stage(mapped_data%sync_stage, stage)) cycle

      ! copy data
      call mapped_data%sync()
    end do

  end subroutine scatter

  ! TODO_MJR: get rid of this bit manipulation stuff...
  function check_stage(var_stage, current_stage) result(is_sync)
    integer(I4B) :: var_stage
    integer(I4B) :: current_stage
    logical(LGP) :: is_sync

    is_sync = iand(var_stage, ibset(0, current_stage)) == ibset(0, current_stage)

  end function check_stage

  subroutine destroy(this)
    class(MapperType) :: this

    call this%mapped_data_list%Clear(destroy=.true.)

  end subroutine destroy

end module MapperModule
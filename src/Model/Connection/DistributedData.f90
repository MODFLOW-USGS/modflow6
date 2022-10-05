module DistributedDataModule
  use ConstantsModule, only: LENMEMPATH, LENCOMPONENTNAME, LENVARNAME
  use KindModule, only: I4B, LGP
  use MemoryTypeModule, only: MemoryType
  use MemoryHelperModule, only: create_mem_path
  use MemoryListModule, only: MemoryListType
  use ListModule, only: ListType
  use MappedVariableModule, only: MappedVariableType, CastAsMappedVariable
  use InterfaceMapModule

  implicit none
  private

  ! stages for synchronization
  integer(I4B), public, parameter :: BEFORE_AR = 1
  integer(I4B), public, parameter :: AFTER_AR = 2
  integer(I4B), public, parameter :: BEFORE_AD = 3
  integer(I4B), public, parameter :: BEFORE_CF = 4
  integer(I4B), public, parameter :: BEFORE_FC = 5

  ! types of variables
  integer(I4B), public, parameter :: SYNC_SCALAR = 0
  integer(I4B), public, parameter :: SYNC_NODES = 1
  integer(I4B), public, parameter :: SYNC_CONNECTIONS = 2
  integer(I4B), public, parameter :: SYNC_EXCHANGES = 3

  type, public :: DistVarType
    character(len=LENVARNAME) :: var_name !< name of variable, e.g. "K11"
    character(len=LENCOMPONENTNAME) :: subcomp_name !< subcomponent, e.g. "NPF"
    character(len=LENCOMPONENTNAME) :: comp_name !< component, e.g. the model or exchange name
    integer(I4B) :: map_type !< can be 0 = scalar, 1 = node based, 2 = connection based,
                             !! 3 = exchange based (connections crossing model boundaries)
    character(len=LENVARNAME) :: exg_var_name !< needed for exchange variables, e.g. SIMVALS
    integer(I4B), dimension(:), allocatable :: sync_stages !< when to sync, e.g. (/ STAGE_AD, STAGE_CF /)
                                                           !! which is before AD and CF
  end type DistVarType

  type, public :: DistributedDataType
    type(MemoryListType) :: remote_memory_list !< all remote data as a list of MemoryType
    type(ListType) :: variable_list !< all distributed variables, NB: not necessarily 1-to-1
                                    !!with list of data items
  contains
    procedure :: map_variables
    procedure :: get_dist_data
    procedure :: synchronize
    procedure :: destroy

    procedure, private :: map_model_data
    procedure, private :: map_exg_data
    procedure, private :: map_data
    procedure, private :: print_variables
  end type DistributedDataType

  ! HACK: global access
  type(DistributedDataType), public :: distributed_data

contains

  subroutine map_variables(this, sol_id, dist_vars, interface_map)
    class(DistributedDataType) :: this
    integer(I4B) :: sol_id
    type(ListType) :: dist_vars
    type(InterfaceMapType), pointer :: interface_map
    ! local
    integer(I4B) :: i, m, e
    type(DistVarType), pointer :: distvar
    type(IndexMapType), pointer :: idx_map

    ! loop over variables
    do i = 1, dist_vars%Count()
      distvar => GetDistVarFromList(dist_vars, i)
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
          call distributed_data%map_model_data(sol_id, &
                                               distvar%comp_name, &
                                               distvar%subcomp_name, &
                                               distvar%var_name, &
                                               interface_map%model_names(m), &
                                               idx_map, &
                                               distvar%sync_stages)
        end do
      else if (distvar%map_type == SYNC_EXCHANGES) then
        ! map data from the exchanges to the interface
        do e = 1, interface_map%nr_exchanges
          call distributed_data%map_exg_data(sol_id, &
                                             distvar%comp_name, &
                                             distvar%subcomp_name, &
                                             distvar%var_name, &
                                             interface_map%exchange_names(e), &
                                             distvar%exg_var_name, &
                                             interface_map%exchange_map(e), &
                                             distvar%sync_stages)
        end do
      end if
    end do

  end subroutine map_variables

  !> @brief Map data from model memory to a target memory entry,
  !! with the specified map. The source and target items have
  !< the same name and (optionally) subcomponent name.
  subroutine map_model_data(this, controller_id, tgt_model_name, &
                            tgt_subcomp_name, tgt_var_name, src_model_name, &
                            index_map, stages)
    use SimModule, only: ustop
    use MemoryManagerModule, only: get_from_memorylist
    class(DistributedDataType) :: this
    integer(I4B) :: controller_id !< e.g. the numerical solution where synchr. is controlled
    character(len=*), intent(in) :: tgt_model_name
    character(len=*), intent(in) :: tgt_subcomp_name
    character(len=*), intent(in) :: tgt_var_name
    character(len=*), intent(in) :: src_model_name
    type(IndexMapType), intent(in) :: index_map
    integer(I4B), dimension(:), intent(in) :: stages !< array with 1 or multiple stages for synchronization
    ! local
    character(len=LENVARNAME) :: src_var_name
    character(len=LENMEMPATH) :: src_mem_path, tgt_mem_path

    if (len_trim(tgt_subcomp_name) > 0) then
      src_mem_path = create_mem_path(src_model_name, tgt_subcomp_name)
      tgt_mem_path = create_mem_path(tgt_model_name, tgt_subcomp_name)
    else
      src_mem_path = create_mem_path(src_model_name)
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
                          tgt_subcomp_name, tgt_var_name, src_exg_name, &
                          src_var_name, index_map_sgn, stages)
    use SimModule, only: ustop
    use MemoryManagerModule, only: get_from_memorylist
    class(DistributedDataType) :: this
    integer(I4B) :: controller_id !< e.g. the numerical solution where synchr. is controlled
    character(len=*), intent(in) :: tgt_model_name
    character(len=*), intent(in) :: tgt_subcomp_name
    character(len=*), intent(in) :: tgt_var_name
    character(len=*), intent(in) :: src_exg_name
    character(len=*), intent(in) :: src_var_name
    type(IndexMapSgnType), intent(in) :: index_map_sgn
    integer(I4B), dimension(:), intent(in) :: stages !< array with 1 or multiple stages for synchronization
    ! local
    character(len=LENMEMPATH) :: src_mem_path, tgt_mem_path

    src_mem_path = create_mem_path(src_exg_name)
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
  !! an optional sign conversion
  !<
  subroutine map_data(this, controller_id, tgt_name, tgt_path, tgt_idx, &
                      src_name, src_path, src_idx, sign_array, stages)
    class(DistributedDataType) :: this
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
    type(MappedVariableType), pointer :: mapped_var
    class(*), pointer :: obj

    ! loop and set stage bits
    istage = 0
    do i = 1, size(stages)
      istage = ibset(istage, stages(i))
    end do

    ! create MappedVariable and add to list
    allocate (mapped_var)
    mapped_var%controller_id = controller_id
    mapped_var%sync_stage = istage
    mapped_var%src_name = src_name
    mapped_var%src_path = src_path
    mapped_var%src => null()
    mapped_var%tgt_name = tgt_name
    mapped_var%tgt_path = tgt_path
    mapped_var%tgt => null()
    mapped_var%src_idx => src_idx
    mapped_var%tgt_idx => tgt_idx
    mapped_var%sign => sign_array
    obj => mapped_var
    call this%variable_list%Add(obj)

  end subroutine map_data

  function get_dist_data(this) result(dist_data)
    class(DistributedDataType) :: this
    type(MemoryType), pointer :: dist_data

    ! get from memory list

  end function get_dist_data

  subroutine synchronize(this, controller_id, stage)
    class(DistributedDataType) :: this
    integer(I4B) :: controller_id
    integer(I4B), intent(in) :: stage
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj
    class(MappedVariableType), pointer :: var

    ! sync all variables (src => tgt) for a given stage
    do i = 1, this%variable_list%Count()
      obj => this%variable_list%GetItem(i)
      var => CastAsMappedVariable(obj)
      if (controller_id > 0 .and. var%controller_id /= controller_id) cycle
      if (.not. check_stage(var%sync_stage, stage)) cycle

      ! copy data
      call var%sync()
    end do

  end subroutine synchronize

  function check_stage(var_stage, current_stage) result(is_sync)
    integer(I4B) :: var_stage
    integer(I4B) :: current_stage
    logical(LGP) :: is_sync

    is_sync = iand(var_stage, ibset(0, current_stage)) == ibset(0, current_stage)

  end function check_stage

  subroutine destroy(this)
    class(DistributedDataType) :: this

    !call this%print_variables()

    call this%variable_list%Clear(destroy=.true.)
    call this%remote_memory_list%clear()

  end subroutine destroy

  function GetDistVarFromList(list, idx) result(res)
    implicit none
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: idx
    class(DistVarType), pointer :: res
    ! local
    class(*), pointer :: obj

    obj => list%GetItem(idx)
    res => CastAsDistVar(obj)
    return

  end function GetDistVarFromList

  function CastAsDistVar(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(DistVarType), pointer :: res

    res => null()
    if (.not. associated(obj)) return

    select type (obj)
    class is (DistVarType)
      res => obj
    end select
    return
  end function CastAsDistVar

  subroutine print_variables(this)
    class(DistributedDataType) :: this
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj
    class(MappedVariableType), pointer :: var

    write (*, *) "Debug: print variables..."
    do i = 1, this%variable_list%Count()
      obj => this%variable_list%GetItem(i)
      var => CastAsMappedVariable(obj)
      write (*, *) trim(var%src%name), " ", trim(var%src%path), &
        " to ", trim(var%tgt%name), " ", trim(var%tgt%path)
    end do

  end subroutine print_variables

end module DistributedDataModule

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
  integer(I4B), public, parameter :: BEFORE_AR = 0
  integer(I4B), public, parameter :: BEFORE_DF = 1
  integer(I4B), public, parameter :: BEFORE_AD = 2
  integer(I4B), public, parameter :: BEFORE_CF = 3
  integer(I4B), public, parameter :: BEFORE_FC = 4

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
    procedure, private :: print_variables
  end type DistributedDataType
  
  ! HACK: global access
  type(DistributedDataType), public :: distributed_data

contains

  subroutine map_variables(this, sol_id, dist_vars, interface_map)
    class(DistributedDataType) :: this
    integer(I4B) :: sol_id
    type(DistVarType), dimension(:), pointer :: dist_vars
    type(InterfaceMapType), pointer :: interface_map
    ! local
    integer(I4B) :: i, m, e

    ! loop over variables
    do i = 1, size(dist_vars)
      if (dist_vars(i)%map_type == SYNC_NODES) then        
        ! map node data for all models in this interface
        do m = 1, interface_map%nr_models
          call distributed_data%map_model_data(sol_id, &
                                              dist_vars(i)%comp_name, &
                                              dist_vars(i)%subcomp_name, &
                                              dist_vars(i)%var_name, &
                                              interface_map%model_names(m), &
                                              interface_map%node_map(m), &
                                              dist_vars(i)%sync_stages)
        end do
      end if
      if (dist_vars(i)%map_type == SYNC_CONNECTIONS) then        
        ! map connection data for all models in this interface,
        ! this includes connections managed by the exchanges
        do m = 1, interface_map%nr_models
          call distributed_data%map_model_data(sol_id, &
                                              dist_vars(i)%comp_name, &
                                              dist_vars(i)%subcomp_name, &
                                              dist_vars(i)%var_name, &
                                              interface_map%model_names(m), &
                                              interface_map%connection_map(m), &
                                              dist_vars(i)%sync_stages)
        end do
      end if
      if (dist_vars(i)%map_type == SYNC_EXCHANGES) then
        ! map data at the exchanges to the connections in the interface
        do e = 1, interface_map%nr_exchanges
          call distributed_data%map_exg_data(sol_id, &
                                             dist_vars(i)%comp_name, &
                                             dist_vars(i)%subcomp_name, &
                                             dist_vars(i)%var_name, &
                                             interface_map%exchange_names(e), &
                                             dist_vars(i)%exg_var_name, &
                                             interface_map%exchange_map(e), &
                                             dist_vars(i)%sync_stages)
        end do
      end if
    end do

  end subroutine map_variables

  !> @brief Map data from model memory to a target memory entry,
  !< with the specified map
  subroutine map_model_data(this, controller_id, tgt_model_name, tgt_comp_name, &
                            tgt_var_name, src_model_name, index_map, stages)
    use SimModule, only: ustop
    use MemoryManagerModule, only: get_from_memorylist
    class(DistributedDataType) :: this
    integer(I4B) :: controller_id !< e.g. the numerical solution where synchr. is controlled
    character(len=*), intent(in) :: tgt_model_name
    character(len=*), intent(in) :: tgt_comp_name
    character(len=*), intent(in) :: tgt_var_name
    character(len=*), intent(in) :: src_model_name
    type(IndexMapType), intent(in) :: index_map
    integer(I4B), dimension(:), intent(in) :: stages !< array with 1 or multiple stages for synchronization
    ! local
    type(MemoryType), pointer :: src_mt, tgt_mt
    logical(LGP) :: found
    type(MappedVariableType), pointer :: mapped_var
    class(*), pointer :: obj
    integer(I4B) :: istage, i
    character(len=LENMEMPATH) :: src_mem_path, tgt_mem_path
    character(len=LENVARNAME) :: src_var_name

    ! get source memory item
    src_var_name = tgt_var_name
    if (len_trim(tgt_comp_name) > 0) then
      src_mem_path = create_mem_path(src_model_name, tgt_comp_name)
    else
      src_mem_path = create_mem_path(src_model_name)
    end if
    call get_from_memorylist(src_var_name, src_mem_path, src_mt, found, check=.false.)
    if (.not. found) then
      ! this should branch to MPI synchronized memory,
      ! - create memorytype
      ! - add to remote_memory_list
      ! - set up MPI
      ! - create reduced mapping
      ! - ...??
      write(*,*) "Error, remote memory in DD not supported yet"
      call ustop()
    end if
   
    ! get target memory item
    if (len_trim(tgt_comp_name) > 0) then
      tgt_mem_path = create_mem_path(tgt_model_name, tgt_comp_name)
    else
      tgt_mem_path = create_mem_path(tgt_model_name)
    end if
    call get_from_memorylist(tgt_var_name, tgt_mem_path, tgt_mt, found)
    
    ! loop and set stage bits
    istage = 0
    do i = 1, size(stages)
      istage = ibset(istage, stages(i))
    end do

    ! create MappedVariable and add to list
    allocate(mapped_var)
    mapped_var%controller_id = controller_id
    mapped_var%sync_stage = istage
    mapped_var%src => src_mt
    mapped_var%tgt => tgt_mt
    mapped_var%src_idx => index_map%src_idx
    mapped_var%tgt_idx => index_map%tgt_idx
    mapped_var%sign => null()
    obj => mapped_var
    call this%variable_list%Add(obj)

  end subroutine map_model_data

  !> @brief Map memory from a Exchange to the specified memory entry,
  !< using the index map
  subroutine map_exg_data(this, controller_id, tgt_model_name, tgt_comp_name, &
                          tgt_var_name, src_exg_name, src_var_name, &
                          index_map_sgn, stages)
    use SimModule, only: ustop
    use MemoryManagerModule, only: get_from_memorylist
    class(DistributedDataType) :: this
    integer(I4B) :: controller_id !< e.g. the numerical solution where synchr. is controlled
    character(len=*), intent(in) :: tgt_model_name
    character(len=*), intent(in) :: tgt_comp_name
    character(len=*), intent(in) :: tgt_var_name
    character(len=*), intent(in) :: src_exg_name
    character(len=*), intent(in) :: src_var_name
    type(IndexMapSgnType), intent(in) :: index_map_sgn
    integer(I4B), dimension(:), intent(in) :: stages !< array with 1 or multiple stages for synchronization
    ! local
    type(MemoryType), pointer :: src_mt, tgt_mt
    logical(LGP) :: found
    type(MappedVariableType), pointer :: mapped_var
    class(*), pointer :: obj
    integer(I4B) :: istage, i
    character(len=LENMEMPATH) :: src_mem_path, tgt_mem_path

    src_mem_path = create_mem_path(src_exg_name)
    call get_from_memorylist(src_var_name, src_mem_path, src_mt, found, check=.false.)
    if (.not. found) then
      ! this should branch to MPI synchronized memory,
      ! - create memorytype
      ! - add to remote_memory_list
      ! - set up MPI
      ! - create reduced mapping
      ! - ...??
      write(*,*) "Error mapping exchange data (remote memory in DD not supported yet):"
      write(*,*) "  ", src_var_name, "@", src_mem_path
      call ustop()
    end if
   
    ! get target memory item
    if (len_trim(tgt_comp_name) > 0) then
      tgt_mem_path = create_mem_path(tgt_model_name, tgt_comp_name)
    else
      tgt_mem_path = create_mem_path(tgt_model_name)
    end if
    call get_from_memorylist(tgt_var_name, tgt_mem_path, tgt_mt, found)
    
    ! loop and set stage bits
    istage = 0
    do i = 1, size(stages)
      istage = ibset(istage, stages(i))
    end do

    ! create MappedVariable and add to list
    allocate(mapped_var)
    mapped_var%controller_id = controller_id
    mapped_var%sync_stage = istage
    mapped_var%src => src_mt
    mapped_var%tgt => tgt_mt
    mapped_var%src_idx => index_map_sgn%src_idx
    mapped_var%tgt_idx => index_map_sgn%tgt_idx
    mapped_var%sign => index_map_sgn%sign
    obj => mapped_var
    call this%variable_list%Add(obj)

  end subroutine map_exg_data

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
      if (.not. var%controller_id == controller_id) cycle
      if (.not. check_stage(var%sync_stage,stage)) cycle

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

  subroutine print_variables(this)
    class(DistributedDataType) :: this
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj
    class(MappedVariableType), pointer :: var

    write(*,*) "Debug: print variables..."
    do i = 1, this%variable_list%Count()
      obj => this%variable_list%GetItem(i)
      var => CastAsMappedVariable(obj)
      write(*,*) trim(var%src%name), " ", trim(var%src%path), " to ", trim(var%tgt%name), " ", trim(var%tgt%path)
    end do

  end subroutine print_variables

end module DistributedDataModule

module DistributedDataModule
  use ConstantsModule, only: LENMEMPATH, LENMODELNAME, &
                             LENCOMPONENTNAME, LENVARNAME
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

  type, public :: DistributedDataType
    type(MemoryListType) :: remote_memory_list !< all remote data as a list of MemoryType
    type(ListType) :: variable_list !< all distributed variables, NB: not necessarily 1-to-1
                                !!with list of data items
  contains
    procedure :: map_model_data
    procedure :: get_dist_data
    procedure :: synchronize
  end type DistributedDataType
  
  ! HACK: global access
  type(DistributedDataType), public :: distributed_data

contains

  subroutine map_model_data(this, tgt_model_name, tgt_comp_name, tgt_var_name, src_model_id, index_map, stages)
    use SimModule, only: ustop
    use MemoryManagerModule, only: get_from_memorylist
    class(DistributedDataType) :: this
    character(len=*), intent(in) :: tgt_model_name
    character(len=*), intent(in) :: tgt_comp_name
    character(len=*), intent(in) :: tgt_var_name
    integer(I4B), intent(in) :: src_model_id
    type(IndexMap), pointer, intent(in) :: index_map
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
      src_mem_path = create_mem_path(getModelNameFromId(src_model_id), &
                                     tgt_comp_name)
    else
      src_mem_path = create_mem_path(getModelNameFromId(src_model_id))
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
    mapped_var%sync_stage = istage
    mapped_var%src => src_mt
    mapped_var%tgt => tgt_mt
    mapped_var%src_idx => index_map%src_idx
    mapped_var%tgt_idx => index_map%tgt_idx
    obj => mapped_var
    call this%variable_list%Add(obj)

  end subroutine map_model_data

  
  function getModelNameFromId(id) result(name)
    use ListsModule, only: basemodellist
    use BaseModelModule, only: BaseModelType, GetBaseModelFromList
    use MemoryHelperModule, only: create_mem_path
    integer(I4B) :: id
    character(len=LENMODELNAME) :: name
    ! local
    class(BaseModelType), pointer :: model
        
    model => GetBaseModelFromList(basemodellist, id)
    name = model%name

  end function getModelNameFromId

  function get_dist_data(this) result(dist_data)
    class(DistributedDataType) :: this
    type(MemoryType), pointer :: dist_data

    ! get from memory list

  end function get_dist_data

  subroutine synchronize(this, stage)
    class(DistributedDataType) :: this
    integer(I4B), intent(in) :: stage
    ! local
    integer(I4B) :: i
    class(*), pointer :: obj
    class(MappedVariableType), pointer :: var

    ! sync all variables (src => tgt) for a given stage
    do i = 1, this%variable_list%Count()
      obj => this%variable_list%GetItem(i)
      var => CastAsMappedVariable(obj)
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

end module DistributedDataModule

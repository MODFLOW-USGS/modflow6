module DistributedModelModule
  use KindModule, only: I4B, LGP, DP
  use SimModule, only: ustop
  use ListModule, only: ListType
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use MemoryHelperModule, only: create_mem_path
  use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList
  use ListsModule, only: basemodellist
  use DistListsModule, only: distmodellist
  use DistributedBaseModule
  use SimStagesModule
  use IndexMapModule
  implicit none
  private

  public :: DistributedModelType
  public :: add_dist_model, get_dist_model
  public :: AddDistModelToList, GetDistModelFromList

  type, extends(DistributedBaseType) :: DistributedModelType
    ! variables always available
    integer(I4B), pointer :: moffset => null()
    
    ! == geometrical/connection data needed to build interface grid == 
    ! DIS:
    integer(I4B), pointer :: dis_nodes => null()
    integer(I4B), pointer :: dis_nja => null()
    integer(I4B), pointer :: dis_njas => null()
    real(DP), pointer :: dis_xorigin => null()
    real(DP), pointer :: dis_yorigin => null()
    real(DP), pointer :: dis_angrot => null()
    real(DP), dimension(:), pointer, contiguous :: dis_xc => null()
    real(DP), dimension(:), pointer, contiguous :: dis_yc => null()
    real(DP), dimension(:), pointer, contiguous :: dis_top => null()
    real(DP), dimension(:), pointer, contiguous :: dis_bot => null()

    ! CON:
    integer(I4B), dimension(:), pointer, contiguous :: con_ia => null() ! in full
    integer(I4B), dimension(:), pointer, contiguous :: con_ja => null() ! in full
    integer(I4B), dimension(:), pointer, contiguous :: con_jas => null()
    integer(I4B), dimension(:), pointer, contiguous :: con_ihc => null()
    real(DP), dimension(:), pointer, contiguous :: con_hwva => null()
    real(DP), dimension(:), pointer, contiguous :: con_cl1 => null()
    real(DP), dimension(:), pointer, contiguous :: con_cl2 => null()
    real(DP), dimension(:), pointer, contiguous :: con_anglex => null()

    ! Numerical model data
    real(DP), private, dimension(:), pointer, contiguous :: x => null()
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null()
    real(DP), private, dimension(:), pointer, contiguous :: x_old => null()
    integer(I4B), dimension(:), pointer, contiguous :: icelltype => null()

    ! GWF model data
    integer(I4B), pointer :: npf_iangle1
    integer(I4B), pointer :: npf_iangle2
    integer(I4B), pointer :: npf_iangle3
    integer(I4B), pointer :: npf_iwetdry
    real(DP), private, dimension(:), pointer, contiguous :: npf_k11 => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_k22 => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_k33 => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_angle1 => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_angle2 => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_angle3 => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_wetdry => null()

    ! GWT model data

    ! this is strictly private, use access() instead
    class(NumericalModelType), private, pointer :: model !< implementation if local, null otherwise
  contains
    procedure :: create
    procedure :: init_connectivity
    procedure :: setup_remote_memory
    procedure :: get_src_map

    generic :: operator(==) => equals_dist_model, equals_num_model
    procedure :: destroy
    procedure :: access
    ! private    
    procedure, private :: init_gwf_rmt_mem
    procedure, private :: init_gwt_rmt_mem  
    procedure, private :: equals_dist_model
    procedure, private :: equals_num_model
  end type DistributedModelType

contains

  subroutine add_dist_model(model_index, model_name)
    integer(I4B) :: model_index
    character(len=*) :: model_name
    ! local
    class(NumericalModelType), pointer :: num_model
    class(DistributedModelType), pointer :: dist_model

    num_model => GetNumericalModelFromList(basemodellist, model_index)

    allocate (dist_model)
    call dist_model%create(num_model, model_index, model_name)
    call AddDistModelToList(distmodellist, dist_model)

  end subroutine add_dist_model

  subroutine create(this, model, m_id, m_name)
    class(DistributedModelType) :: this
    class(NumericalModelType), pointer :: model
    integer(I4B) :: m_id
    character(len=*) :: m_name
    
    this%id = model%id
    this%name = m_name
    this%model => model
    this%is_local = associated(model)

    call this%load(this%moffset, 'MOFFSET', '', (/STG_INIT/), MAP_TYPE_NA)
    call this%load(this%dis_nodes, 'NODES', 'DIS', (/STG_INIT/), MAP_TYPE_NA)
    call this%load(this%dis_nja, 'NJA', 'DIS', (/STG_INIT/), MAP_TYPE_NA)
    call this%load(this%dis_njas, 'NJAS', 'DIS', (/STG_INIT/), MAP_TYPE_NA)

  end subroutine create
  
  subroutine init_connectivity(this)
    use NumericalModelModule
    class(DistributedModelType), intent(inout) :: this

    call this%load(this%dis_xorigin, 'XORIGIN', 'DIS', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%dis_yorigin, 'YORIGIN', 'DIS', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%dis_angrot, 'ANGROT', 'DIS', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%dis_xc, this%dis_nodes, 'XC', 'DIS', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%dis_yc, this%dis_nodes, 'YC', 'DIS', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%dis_top, this%dis_nodes, 'TOP', 'DIS', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%dis_bot, this%dis_nodes, 'BOT', 'DIS', (/STG_BEFORE_DF/), MAP_TYPE_NA)

    call this%load(this%con_ia, this%dis_nodes + 1, 'IA', 'CON', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%con_ja, this%dis_nja, 'JA', 'CON', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%con_jas, this%dis_nja, 'JAS', 'CON', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%con_ihc, this%dis_njas, 'IHC', 'CON', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%con_hwva, this%dis_njas, 'HWVA', 'CON', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%con_cl1, this%dis_njas, 'CL1', 'CON', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%con_cl2, this%dis_njas, 'CL2', 'CON', (/STG_BEFORE_DF/), MAP_TYPE_NA)
    call this%load(this%con_anglex, this%dis_njas, 'ANGLEX', 'CON', (/STG_BEFORE_DF/), MAP_TYPE_NA)

  end subroutine init_connectivity

  function get_src_map(this, map_type) result(src_map)
    class(DistributedModelType) :: this
    integer(I4B) :: map_type
    integer(I4B), dimension(:), pointer :: src_map

    src_map => null()
    if (map_type == MAP_TYPE_NODE) then
      src_map => this%src_map_node
    else if (map_type == MAP_TYPE_CONN) then
      src_map => this%src_map_conn
    end if

  end function get_src_map

  subroutine setup_remote_memory(this, node_map, connection_map)
    class(DistributedModelType), intent(inout) :: this
    type(IndexMapType) :: node_map
    type(IndexMapType) :: connection_map

    ! nothing to be done for local models (no routing)
    if (this%is_local) return

    this%src_map_node => node_map%src_idx
    this%src_map_conn => connection_map%src_idx

    if (this%macronym == 'GWF') then
      call this%init_gwf_rmt_mem()
    else if (this%macronym == 'GWT') then
      call this%init_gwt_rmt_mem()
    end if

  end subroutine setup_remote_memory

  subroutine init_gwf_rmt_mem(this)
    class(DistributedModelType), intent(inout) :: this
    ! local
    integer(I4B) :: nr_iface_nodes

    nr_iface_nodes = size(this%src_map_node)
    call this%load(this%npf_k11, nr_iface_nodes, 'K11', 'NPF', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
    call this%load(this%npf_k22, nr_iface_nodes, 'K22', 'NPF', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
    call this%load(this%npf_k33, nr_iface_nodes, 'K33', 'NPF', (/STG_BEFORE_AR/), MAP_TYPE_NODE)

  end subroutine init_gwf_rmt_mem

  subroutine init_gwt_rmt_mem(this)
    class(DistributedModelType), intent(inout) :: this


  end subroutine init_gwt_rmt_mem
  
  function equals_dist_model(this, dist_model) result(is_equal)
    class(DistributedModelType), intent(in) :: this
    class(DistributedModelType), intent(in) :: dist_model
    logical(LGP) :: is_equal

    is_equal = (this%id == dist_model%id)

  end function equals_dist_model

  function equals_num_model(this, num_model) result(is_equal)
    class(DistributedModelType), intent(in) :: this
    class(NumericalModelType), intent(in) :: num_model
    logical(LGP) :: is_equal

    is_equal = (this%id == num_model%id)

  end function equals_num_model

  subroutine destroy(this)
    class(DistributedModelType) :: this

    call this%DistributedBaseType%destroy()

    if (.not. this%is_local) then

      ! these are always available:
      call mem_deallocate(this%moffset)
      call mem_deallocate(this%dis_nodes)
      call mem_deallocate(this%dis_nja)
      call mem_deallocate(this%dis_njas)

      ! these when the model was not local and
      ! a candidate for the halo
      if (associated(this%con_ia)) then
        call mem_deallocate(this%dis_xorigin)
        call mem_deallocate(this%dis_yorigin)
        call mem_deallocate(this%dis_angrot)
        call mem_deallocate(this%dis_xc)
        call mem_deallocate(this%dis_yc)
        call mem_deallocate(this%dis_top)
        call mem_deallocate(this%dis_bot)

        call mem_deallocate(this%con_ia)
        call mem_deallocate(this%con_ja)
        call mem_deallocate(this%con_jas)
        call mem_deallocate(this%con_ihc)
        call mem_deallocate(this%con_hwva)
        call mem_deallocate(this%con_cl1)
        call mem_deallocate(this%con_cl2)
        call mem_deallocate(this%con_anglex)
      end if

    end if

  end subroutine destroy

  function access(this) result(model)
    class(DistributedModelType) :: this
    class(NumericalModelType), pointer :: model

    if (associated(this%model)) then
      model => this%model
    else
      write (*, *) 'Error: illegal access to remote memory, abort'
      call ustop()
    end if

  end function access

  !> @brief Gets the distributed model structure for
  !! a particular model id
  !<
  function get_dist_model(model_id) result(dist_model)
    integer(I4B) :: model_id !< the model id
    class(DistributedModelType), pointer :: dist_model !< the distributed model returned

    dist_model => GetDistModelFromList(distmodellist, model_id)

  end function get_dist_model

  function CastAsDistModelClass(obj) result(res)
    class(*), pointer, intent(inout) :: obj
    class(DistributedModelType), pointer :: res

    res => null()
    if (.not. associated(obj)) return

    select type (obj)
    class is (DistributedModelType)
      res => obj
    end select
    return

  end function CastAsDistModelClass

  subroutine AddDistModelToList(list, model)
    type(ListType), intent(inout) :: list
    class(DistributedModelType), pointer, intent(inout) :: model
    ! local
    class(*), pointer :: obj

    obj => model
    call list%Add(obj)
    return

  end subroutine AddDistModelToList

  function GetDistModelFromList(list, idx) result(res)
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: idx
    class(DistributedModelType), pointer :: res
    ! local
    class(*), pointer :: obj

    obj => list%GetItem(idx)
    res => CastAsDistModelClass(obj)
    return

  end function GetDistModelFromList

end module DistributedModelModule

module DistributedModelModule
  use KindModule, only: I4B, LGP, DP
  use SimModule, only: ustop
  use ListModule, only: ListType
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, mem_reallocate
  use MemoryHelperModule, only: create_mem_path
  use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList
  use ListsModule, only: basemodellist
  use DistListsModule, only: distmodellist
  use DistributedBaseModule
  use RemoteMemoryModule
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
    real(DP), dimension(:), pointer, contiguous :: dis_area => null()

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

    ! GWF model data
    integer(I4B), pointer :: npf_iangle1 => null()
    integer(I4B), pointer :: npf_iangle2 => null()
    integer(I4B), pointer :: npf_iangle3 => null()
    integer(I4B), pointer :: npf_iwetdry => null()
    integer(I4B), dimension(:), pointer, contiguous :: npf_icelltype => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_k11 => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_k22 => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_k33 => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_angle1 => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_angle2 => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_angle3 => null()
    real(DP), private, dimension(:), pointer, contiguous :: npf_wetdry => null()

    ! GWT model data
    integer(I4B), pointer :: indsp => null()
    integer(I4B), pointer :: inmst => null()    
    integer(I4B), pointer :: dsp_idiffc => null()
    integer(I4B), pointer :: dsp_idisp => null()
    real(DP), private, dimension(:), pointer, contiguous :: dsp_diffc => null()
    real(DP), private, dimension(:), pointer, contiguous :: dsp_alh => null()
    real(DP), private, dimension(:), pointer, contiguous :: dsp_alv => null()
    real(DP), private, dimension(:), pointer, contiguous :: dsp_ath1 => null()
    real(DP), private, dimension(:), pointer, contiguous :: dsp_ath2 => null()
    real(DP), private, dimension(:), pointer, contiguous :: dsp_atv => null()

    real(DP), private, dimension(:), pointer, contiguous :: fmi_gwfhead => null()
    real(DP), private, dimension(:), pointer, contiguous :: fmi_gwfsat => null()
    real(DP), private, dimension(:,:), pointer, contiguous :: fmi_gwfspdis => null()
    real(DP), private, dimension(:), pointer, contiguous :: fmi_gwfflowja => null()

    real(DP), private, dimension(:), pointer, contiguous :: mst_porosity => null()

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

  subroutine add_dist_model(model_index, model_name, macronym)
    integer(I4B) :: model_index
    character(len=*) :: model_name    
    character(len=3) :: macronym
    ! local
    class(NumericalModelType), pointer :: num_model
    class(DistributedModelType), pointer :: dist_model

    num_model => GetNumericalModelFromList(basemodellist, model_index)

    allocate (dist_model)
    call dist_model%create(num_model, model_index, model_name, macronym)
    call AddDistModelToList(distmodellist, dist_model)

  end subroutine add_dist_model

  subroutine create(this, model, m_id, m_name, macronym)
    class(DistributedModelType) :: this
    class(NumericalModelType), pointer :: model
    integer(I4B) :: m_id
    character(len=*) :: m_name
    character(len=3) :: macronym
    
    this%id = model%id
    this%name = m_name
    this%model => model
    this%is_local = associated(model)
    this%macronym = macronym

    call this%load(this%moffset, 'MOFFSET', '', (/STG_BEFORE_AC/), MAP_TYPE_NA)
    call this%load(this%dis_nodes, 'NODES', 'DIS', (/STG_BEFORE_INIT/), MAP_TYPE_NA)
    call this%load(this%dis_nja, 'NJA', 'DIS', (/STG_BEFORE_INIT/), MAP_TYPE_NA)
    call this%load(this%dis_njas, 'NJAS', 'DIS', (/STG_BEFORE_INIT/), MAP_TYPE_NA)

    if (this%macronym == 'GWF') then
      call this%load(this%npf_iangle1, 'IANGLE1', 'NPF', (/STG_BEFORE_INIT/), MAP_TYPE_NA)
      call this%load(this%npf_iangle2, 'IANGLE2', 'NPF', (/STG_BEFORE_INIT/), MAP_TYPE_NA)
      call this%load(this%npf_iangle3, 'IANGLE3', 'NPF', (/STG_BEFORE_INIT/), MAP_TYPE_NA)
      call this%load(this%npf_iwetdry, 'IWETDRY', 'NPF', (/STG_BEFORE_INIT/), MAP_TYPE_NA)
    end if

    if (this%macronym == 'GWT') then
      call this%load(this%dsp_idiffc, 'IDIFFC', 'DSP', (/STG_BEFORE_INIT/), MAP_TYPE_NA)
      call this%load(this%dsp_idisp, 'IDISP', 'DSP', (/STG_BEFORE_INIT/), MAP_TYPE_NA)
      call this%load(this%indsp, 'INDSP', '', (/STG_BEFORE_INIT/), MAP_TYPE_NA)
      call this%load(this%inmst, 'INMST', '', (/STG_BEFORE_INIT/), MAP_TYPE_NA)
    end if

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
    ! local
    type(RemoteMemoryType), pointer :: rmt_mem
    integer(I4B) :: nr_iface_nodes

    ! nothing to be done for local models (no routing)
    if (this%is_local) return

    this%src_map_node => node_map%src_idx
    this%src_map_conn => connection_map%src_idx
    nr_iface_nodes = size(this%src_map_node)

    call this%load(this%dis_area, nr_iface_nodes, 'AREA', 'DIS', (/STG_BEFORE_AR/), MAP_TYPE_NODE)

    ! reset local memory for reduced arrays
    if (.not. this%is_local) then
      call mem_reallocate(this%dis_top, nr_iface_nodes, 'TOP', this%get_local_mem_path('DIS'))
      rmt_mem => this%get_rmt_mem('TOP', 'DIS')
      rmt_mem%map_type = MAP_TYPE_NODE
      rmt_mem%stages = (/STG_BEFORE_AR/)
      call mem_reallocate(this%dis_bot, nr_iface_nodes, 'BOT', this%get_local_mem_path('DIS'))
      rmt_mem => this%get_rmt_mem('BOT', 'DIS')
      rmt_mem%map_type = MAP_TYPE_NODE
      rmt_mem%stages = (/STG_BEFORE_AR/)
    end if
    
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
    call this%load(this%x, nr_iface_nodes, 'X', '', (/STG_BEFORE_AR, STG_BEFORE_AD, STG_BEFORE_CF/), MAP_TYPE_NODE)
    call this%load(this%ibound, nr_iface_nodes, 'IBOUND', '', (/STG_BEFORE_AR, STG_BEFORE_AD, STG_BEFORE_CF/), MAP_TYPE_NODE)
    call this%load(this%x_old, nr_iface_nodes, 'XOLD', '',  (/STG_BEFORE_AD, STG_BEFORE_CF/), MAP_TYPE_NODE)

    call this%load(this%npf_icelltype, nr_iface_nodes, 'ICELLTYPE', 'NPF', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
    call this%load(this%npf_k11, nr_iface_nodes, 'K11', 'NPF', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
    call this%load(this%npf_k22, nr_iface_nodes, 'K22', 'NPF', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
    call this%load(this%npf_k33, nr_iface_nodes, 'K33', 'NPF', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
    if (this%npf_iangle1 > 0) then
      call this%load(this%npf_angle1, nr_iface_nodes, 'ANGLE1', 'NPF', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
    else if (.not. this%is_local) then
      call this%load(this%npf_angle1, 0, 'ANGLE1', 'NPF', (/STG_NEVER/), MAP_TYPE_NODE)
    end if
    if (this%npf_iangle2 > 0) then
      call this%load(this%npf_angle2, nr_iface_nodes, 'ANGLE2', 'NPF', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
    end if
    if (this%npf_iangle3 > 0) then
      call this%load(this%npf_angle3, nr_iface_nodes, 'ANGLE3', 'NPF', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
    end if
    if (this%npf_iwetdry > 0) then
      call this%load(this%npf_wetdry, nr_iface_nodes, 'WETDRY', 'NPF', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
    end if

  end subroutine init_gwf_rmt_mem

  subroutine init_gwt_rmt_mem(this)
    class(DistributedModelType), intent(inout) :: this
    ! local
    integer(I4B) :: nr_iface_nodes
    integer(I4B) :: nr_iface_conns

    nr_iface_nodes = size(this%src_map_node)
    call this%load(this%x, nr_iface_nodes, 'X', '', (/STG_BEFORE_AR, STG_BEFORE_AD, STG_BEFORE_CF/), MAP_TYPE_NODE)
    call this%load(this%ibound, nr_iface_nodes, 'IBOUND', '', (/STG_BEFORE_AR/), MAP_TYPE_NODE)

    if (this%dsp_idiffc > 0) then
      call this%load(this%dsp_diffc, nr_iface_nodes, 'DIFFC', 'DSP', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
    end if
    if (this%dsp_idisp > 0) then
      call this%load(this%dsp_alh, nr_iface_nodes, 'ALH', 'DSP', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
      call this%load(this%dsp_alv, nr_iface_nodes, 'ALV', 'DSP', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
      call this%load(this%dsp_ath1, nr_iface_nodes, 'ATH1', 'DSP', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
      call this%load(this%dsp_ath2, nr_iface_nodes, 'ATH2', 'DSP', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
      call this%load(this%dsp_atv, nr_iface_nodes, 'ATV', 'DSP', (/STG_BEFORE_AR/), MAP_TYPE_NODE)
    end if

    nr_iface_conns = size(this%src_map_conn)
    call this%load(this%fmi_gwfhead, nr_iface_nodes, 'GWFHEAD', 'FMI', (/STG_BEFORE_AD/), MAP_TYPE_NODE)
    call this%load(this%fmi_gwfsat, nr_iface_nodes, 'GWFSAT', 'FMI', (/STG_BEFORE_AD/), MAP_TYPE_NODE)
    call this%load(this%fmi_gwfspdis, 3, nr_iface_nodes, 'GWFSPDIS', 'FMI', (/STG_BEFORE_AD/), MAP_TYPE_NODE)
    call this%load(this%fmi_gwfflowja, nr_iface_conns, 'GWFFLOWJA', 'FMI', (/STG_BEFORE_AD/), MAP_TYPE_CONN)

    if (this%indsp > 0 .and. this%inmst > 0) then
      call this%load(this%mst_porosity, nr_iface_nodes, 'POROSITY', 'MST', (/STG_AFTER_AR/), MAP_TYPE_NODE)
    end if

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

      if (associated(this%npf_icelltype)) then        
        call mem_deallocate(this%npf_icelltype)
        call mem_deallocate(this%npf_k11)
        call mem_deallocate(this%npf_k22)
        call mem_deallocate(this%npf_k33)
        if (this%npf_iwetdry > 0) call mem_deallocate(this%npf_wetdry)
        if (this%npf_iangle1 > 0) call mem_deallocate(this%npf_angle1)
        if (this%npf_iangle2 > 0) call mem_deallocate(this%npf_angle2)
        if (this%npf_iangle3 > 0) call mem_deallocate(this%npf_angle3)
      end if

      if (associated(this%dis_area)) call mem_deallocate(this%dis_area)

      ! scalars:
      call mem_deallocate(this%moffset)
      call mem_deallocate(this%dis_nodes)
      call mem_deallocate(this%dis_nja)
      call mem_deallocate(this%dis_njas)
      
      if (this%macronym == 'GWF') then
        ! these only when ...
        if (associated(this%x)) then
          call mem_deallocate(this%x)
          call mem_deallocate(this%x_old)
          call mem_deallocate(this%ibound)
        end if

        call mem_deallocate(this%npf_iwetdry)
        call mem_deallocate(this%npf_iangle1)
        call mem_deallocate(this%npf_iangle2)
        call mem_deallocate(this%npf_iangle3)
      end if

      if (this%macronym == 'GWT') then
        ! only when ...
        if (associated(this%x)) then
          call mem_deallocate(this%x)
          call mem_deallocate(this%ibound)

          call mem_deallocate(this%fmi_gwfhead)
          call mem_deallocate(this%fmi_gwfsat)
          call mem_deallocate(this%fmi_gwfflowja)
          call mem_deallocate(this%fmi_gwfspdis)

          if (this%dsp_idisp > 0) then
            call mem_deallocate(this%dsp_alh)
            call mem_deallocate(this%dsp_alv)
            call mem_deallocate(this%dsp_ath1)
            call mem_deallocate(this%dsp_ath2)
            call mem_deallocate(this%dsp_atv)
          end if

          if (this%dsp_idiffc > 0) then
            call mem_deallocate(this%dsp_diffc)
          end if

          if (this%indsp > 0 .and. this%inmst > 0) then
            call mem_deallocate(this%mst_porosity)
          end if
        end if

        call mem_deallocate(this%indsp)
        call mem_deallocate(this%inmst)
        call mem_deallocate(this%dsp_idiffc)
        call mem_deallocate(this%dsp_idisp)
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

module DistributedModelModule
  use KindModule, only: I4B, LGP
  use SimModule, only: ustop
  use ListModule, only: ListType
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use MemoryHelperModule, only: create_mem_path
  use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList
  use ListsModule, only: basemodellist, distmodellist
  use DistributedBaseModule
  implicit none
  private

  public :: DistributedModelType
  public :: add_dist_model, get_dist_model
  public :: AddDistModelToList, GetDistModelFromList

  character(len=7), parameter :: LOCAL_MEM_CTX = '__LOC__'

  type, extends(DistributedBaseType) :: DistributedModelType
    integer(I4B), pointer :: moffset => null()
    integer(I4B), pointer :: dis_nodes => null()
    integer(I4B), pointer :: dis_nja => null()
    integer(I4B), dimension(:), pointer, contiguous :: con_ia => null()
    integer(I4B), dimension(:), pointer, contiguous :: con_ja => null()

    ! this is strictly private, use access() instead
    class(NumericalModelType), private, pointer :: model !< implementation if local, null otherwise
  contains
    generic :: create => create_local, create_remote
    procedure :: init_connectivity
    generic :: operator(==) => equals_dist_model, equals_num_model
    procedure :: deallocate
    procedure :: access
    ! private
    procedure, private :: create_local
    procedure, private :: create_remote    
    procedure, private :: equals_dist_model
    procedure, private :: equals_num_model
  end type DistributedModelType

contains

  subroutine add_dist_model(model_index)
    integer(I4B) :: model_index
    ! local
    class(NumericalModelType), pointer :: num_model
    class(DistributedModelType), pointer :: dist_model

    num_model => GetNumericalModelFromList(basemodellist, model_index)

    allocate (dist_model)
    call dist_model%create_local(num_model)
    call AddDistModelToList(distmodellist, dist_model)

  end subroutine add_dist_model

  subroutine create_local(this, model)
    class(DistributedModelType) :: this
    class(NumericalModelType), pointer :: model

    this%id = model%id
    this%name = model%name
    this%model => model
    this%is_local = .true.

    this%moffset => this%model%moffset
    !call mem_allocate(this%moffset, 'MOFFSET', create_mem_path(this%name, context=LOCAL_MEM_CTX))
    call mem_allocate(this%dis_nodes, 'NODES', create_mem_path(this%name, 'DIS', context=LOCAL_MEM_CTX))
    call mem_allocate(this%dis_nja, 'NJA', create_mem_path(this%name, 'DIS', context=LOCAL_MEM_CTX))

  end subroutine create_local

  subroutine create_remote(this, m_id)
    class(DistributedModelType) :: this
    integer(I4B) :: m_id

    this%id = m_id
    this%name = 'TBD'
    this%model => null()
    this%is_local = .false.

    ! TODO_MJR: this should prepare a memory space
    ! where the remote data can live, and then
    ! also connect cache (if we decide to use that)

  end subroutine create_remote

  subroutine init_connectivity(this)        
    use NumericalModelModule
    class(DistributedModelType), intent(inout) :: this

    if (.not. this%is_local) then
      ! something is wrong here
      write(*,*) 'Cannot set up connectivity, only local supported for now'
      call ustop()
    end if

    ! TODO_MJR: this should go after routing has been impl.  
    !this%moffset = this%model%moffset  
    this%dis_nodes = this%model%dis%nodes
    this%dis_nja = this%model%dis%nja

    ! allocate memory space
    call mem_allocate(this%con_ia, this%dis_nodes + 1, 'IA', create_mem_path(this%name, 'CON', context=LOCAL_MEM_CTX))
    call mem_allocate(this%con_ja, this%dis_nja, 'JA', create_mem_path(this%name, 'CON', context=LOCAL_MEM_CTX))

    ! add this image to the serial router
    ! ...

  end subroutine init_connectivity

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

  subroutine deallocate(this)
    class(DistributedModelType) :: this

    !call mem_deallocate(this%moffset)
    call mem_deallocate(this%dis_nodes)    
    call mem_deallocate(this%dis_nja)
    if (associated(this%con_ia)) then
      call mem_deallocate(this%con_ia)
      call mem_deallocate(this%con_ja)
    end if

  end subroutine deallocate

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

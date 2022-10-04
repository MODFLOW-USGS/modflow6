module DistributedModelModule
  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: LENMODELNAME, LENCOMPONENTNAME, &
                             LENVARNAME, LENMEMPATH
  use SimModule, only: ustop
  use ListModule, only: ListType
  use MemoryTypeModule, only: MemoryType
  use MemoryManagerModule, only: get_from_memorylist
  use MemoryHelperModule, only: create_mem_path
  use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList, &
                                  AddNumericalModelToList

  use ListsModule, only: basemodellist, distmodellist
  implicit none
  private

  public :: add_dist_model
  public :: GetDistModelFromList, AddDistModelToList

  type, public :: DistributedModelType
    integer(I4B) :: id !< universal identifier: id of the model
    character(len=LENMODELNAME) :: name !< model name

    ! cached variables:
    integer(I4B), pointer :: moffset => null()

    ! this is strictly private, use access() instead
    class(NumericalModelType), private, pointer :: model !< implementation if local, null otherwise
  contains
    generic :: create => create_local, create_remote
    generic :: load => load_intsclr, load_int1d, load_dblsclr, load_double1d
    generic :: operator(==) => equals_dist_model, equals_num_model
    procedure :: access

    ! private
    procedure, private :: create_local
    procedure, private :: create_remote
    procedure, private :: load_intsclr
    procedure, private :: load_int1d
    procedure, private :: load_dblsclr
    procedure, private :: load_double1d
    procedure, private :: equals_dist_model
    procedure, private :: equals_num_model
  end type DistributedModelType

contains

  subroutine add_dist_model(model_index)
    integer :: model_index
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

    ! connect cached variables
    call this%load(this%moffset, 'MOFFSET')

  end subroutine create_local

  subroutine create_remote(this, m_id)
    class(DistributedModelType) :: this
    integer(I4B) :: m_id

    this%id = m_id
    this%name = 'TBD'
    this%model => null()

    ! TODO_MJR: this should prepare a memory space
    ! where the remote data can live, and then
    ! also connect cache (if we decide to use that)

  end subroutine create_remote

  subroutine load_intsclr(this, intsclr, var_name, subcomp_name)
    class(DistributedModelType) :: this
    integer(I4B), pointer :: intsclr
    character(len=*) :: var_name
    character(len=*), optional :: subcomp_name
    ! local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    character(len=LENMEMPATH) :: mem_path

    if (present(subcomp_name)) then
      mem_path = create_mem_path(this%name, subcomp_name)
    else
      mem_path = create_mem_path(this%name)
    end if

    call get_from_memorylist(var_name, mem_path, mt, found)
    intsclr => mt%intsclr

  end subroutine load_intsclr

  subroutine load_int1d(this, aint1d, var_name, subcomp_name)
    class(DistributedModelType) :: this
    integer(I4B), dimension(:), pointer, contiguous :: aint1d
    character(len=*) :: var_name
    character(len=*), optional :: subcomp_name
    ! local
    character(len=LENMEMPATH) :: mem_path
    type(MemoryType), pointer :: mt
    logical(LGP) :: found

    if (present(subcomp_name)) then
      mem_path = create_mem_path(this%name, subcomp_name)
    else
      mem_path = create_mem_path(this%name)
    end if

    call get_from_memorylist(var_name, mem_path, mt, found)
    aint1d => mt%aint1d

  end subroutine load_int1d

  subroutine load_dblsclr(this, dblsclr, var_name, subcomp_name)
    class(DistributedModelType) :: this
    real(DP), pointer :: dblsclr
    character(len=*) :: var_name
    character(len=*), optional :: subcomp_name
    ! local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found
    character(len=LENMEMPATH) :: mem_path

    if (present(subcomp_name)) then
      mem_path = create_mem_path(this%name, subcomp_name)
    else
      mem_path = create_mem_path(this%name)
    end if

    call get_from_memorylist(var_name, mem_path, mt, found)
    dblsclr => mt%dblsclr

  end subroutine load_dblsclr

  subroutine load_double1d(this, adbl1d, var_name, subcomp_name)
    class(DistributedModelType) :: this
    real(DP), dimension(:), pointer, contiguous :: adbl1d
    character(len=*) :: var_name
    character(len=*), optional :: subcomp_name
    ! local
    character(len=LENMEMPATH) :: mem_path
    type(MemoryType), pointer :: mt
    logical(LGP) :: found

    if (present(subcomp_name)) then
      mem_path = create_mem_path(this%name, subcomp_name)
    else
      mem_path = create_mem_path(this%name)
    end if

    call get_from_memorylist(var_name, mem_path, mt, found)
    adbl1d => mt%adbl1d

  end subroutine load_double1d

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

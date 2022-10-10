module DistributedModelModule
  use KindModule, only: I4B, LGP
  use SimModule, only: ustop
  use ListModule, only: ListType
  use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList
  use ListsModule, only: basemodellist, distmodellist
  use DistributedBaseModule
  implicit none
  private

  public :: DistributedModelType
  public :: add_dist_model, get_dist_model
  public :: AddDistModelToList, GetDistModelFromList

  type, extends(DistributedBaseType) :: DistributedModelType
    ! example for cached variables:
    integer(I4B), pointer :: moffset => null()

    ! this is strictly private, use access() instead
    class(NumericalModelType), private, pointer :: model !< implementation if local, null otherwise
  contains
    generic :: create => create_local, create_remote
    generic :: operator(==) => equals_dist_model, equals_num_model
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

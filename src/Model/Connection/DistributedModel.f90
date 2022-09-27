module DistributedModelModule
  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: LENMODELNAME, LENCOMPONENTNAME, &
                             LENVARNAME
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
  public :: GetDistModelFromList

  type, public :: DistributedModelType
    integer(I4B) :: model_id !< universal identifier: id of the model
    character(len=LENMODELNAME) :: name !< memory path where the model data is stored
    class(NumericalModelType), private, pointer :: model !< implementation if local, null otherwise
  contains
    generic :: create => create_local, create_remote
    generic :: load => load_int1d, load_double1d
    procedure :: access

    ! private
    procedure, private :: create_local
    procedure, private :: create_remote
    procedure, private :: load_double1d
    procedure, private :: load_int1d
  end type DistributedModelType

  contains

  subroutine add_dist_model(model_index)
    integer :: model_index
    ! local
    class(NumericalModelType), pointer :: num_model
    class(DistributedModelType), pointer :: dist_model

    num_model => GetNumericalModelFromList(basemodellist, model_index)

    allocate(dist_model)
    call dist_model%create_local(num_model)
    call AddDistModelToList(distmodellist, dist_model)

  end subroutine add_dist_model

  subroutine create_local(this, model)
    class(DistributedModelType) :: this
    class(NumericalModelType), pointer :: model

    this%model_id = model%id
    this%name = model%name
    this%model => model

  end subroutine create_local

  subroutine create_remote(this, m_id)
    class(DistributedModelType) :: this
    integer(I4B) :: m_id

    this%model_id = m_id
    this%name = 'TBD'
    this%model => null()

  end subroutine create_remote

  subroutine load_int1d(this, aint1d, subcomp_name, var_name)
    class(DistributedModelType) :: this
    integer(I4B), dimension(:), pointer, contiguous :: aint1d
    character(len=*) :: subcomp_name
    character(len=*) :: var_name
    ! local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found

    call get_from_memorylist(var_name, &
                             create_mem_path(this%name, subcomp_name), &
                             mt, &
                             found)
    aint1d => mt%aint1d

  end subroutine load_int1d

  subroutine load_double1d(this, adbl1d, subcomp_name, var_name)
    class(DistributedModelType) :: this
    real(DP), dimension(:), pointer, contiguous :: adbl1d
    character(len=*) :: subcomp_name
    character(len=*) :: var_name
    ! local
    type(MemoryType), pointer :: mt
    logical(LGP) :: found

    call get_from_memorylist(var_name, &
                             create_mem_path(this%name, subcomp_name), &
                             mt, &
                             found)
    adbl1d => mt%adbl1d

  end subroutine load_double1d

  function access(this) result(model)
    class(DistributedModelType) :: this
    class(NumericalModelType), pointer :: model

    if (associated(this%model)) then
      model => this%model
    else
      write(*,*) 'Error: illegal access to remote memory, abort'
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
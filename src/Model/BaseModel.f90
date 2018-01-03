!Highest level model class.  All models inherit from this parent class.

module BaseModelModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENMODELNAME
  use ListModule, only: ListType
  implicit none

  private
  public :: BaseModelType, CastAsBaseModelClass, AddBaseModelToList, &
            GetBaseModelFromList

  type :: BaseModelType
    character(len=LENMODELNAME), pointer :: name             => null()          ! name of the model
    character(len=3), pointer            :: macronym         => null()          ! 3 letter model acronym (GWF, GWT, ...)
    integer(I4B), pointer                :: idsoln           => null()          ! id of the solution model is in
    integer(I4B), pointer                :: id               => null()          ! model id
    integer(I4B), pointer                :: iout             => null()          ! output unit number
    integer(I4B), pointer                :: inewton          => null()          ! newton-raphson flag
    integer(I4B), pointer                :: iprpak           => null()          ! integer flag to echo input
    integer(I4B), pointer                :: iprflow          => null()          ! flag to print simulated flows
    integer(I4B), pointer                :: ipakcb           => null()          ! save_flows flag
    logical, pointer                     :: single_model_run => null()          ! indicate if it is a single model run
  contains
    procedure :: model_df
    procedure :: model_ar
    procedure :: model_rp
    procedure :: model_ot
    procedure :: model_fp
    procedure :: model_da
    procedure :: allocate_scalars
  end type BaseModelType

  contains
  
  subroutine model_df(this)
! ******************************************************************************
! modeldf -- Define the model
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BaseModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine model_df
  
  subroutine model_ar(this)
! ******************************************************************************
! modelar -- Allocate and read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BaseModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine model_ar
  
  subroutine model_rp(this)
! ******************************************************************************
! modelrp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BaseModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine model_rp
  
  subroutine model_ot(this)
! ******************************************************************************
! model_ot -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BaseModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine model_ot
  
  subroutine model_fp(this)
! ******************************************************************************
! model_fp -- Final processing
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BaseModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine model_fp
  
  subroutine allocate_scalars(this, modelname)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(BaseModelType) :: this
    character(len=*), intent(in)  :: modelname
! ------------------------------------------------------------------------------
    !
    allocate(this%name)
    allocate(this%macronym)
    allocate(this%single_model_run)
    call mem_allocate(this%id, 'ID', modelname)
    call mem_allocate(this%iout, 'IOUT', modelname)
    call mem_allocate(this%inewton, 'INEWTON', modelname)
    call mem_allocate(this%iprpak, 'IPRPAK', modelname)
    call mem_allocate(this%iprflow, 'IPRFLOW', modelname)
    call mem_allocate(this%ipakcb, 'IPAKCB', modelname)
    call mem_allocate(this%idsoln, 'IDSOLN', modelname)
    !
    this%name = modelname
    this%macronym = ''
    this%idsoln = 0
    this%id = 0
    this%iout = 0
    this%iprpak = 0
    this%iprflow = 0
    this%ipakcb = 0
    this%inewton = 0 !default is standard formulation
    this%single_model_run = .false.
    !
    ! -- return
    return
  end subroutine allocate_scalars
  
  subroutine model_da(this)
! ******************************************************************************
! deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(BaseModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Strings
    deallocate(this%name)
    deallocate(this%macronym)
    !
    ! -- Scalars
    deallocate(this%single_model_run)
    call mem_deallocate(this%id)
    call mem_deallocate(this%iout)
    call mem_deallocate(this%inewton)
    call mem_deallocate(this%iprpak)
    call mem_deallocate(this%iprflow)
    call mem_deallocate(this%ipakcb)
    call mem_deallocate(this%idsoln)
    !
    ! -- return
    return
  end subroutine model_da

  function CastAsBaseModelClass(obj) result (res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(BaseModelType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (BaseModelType)
      res => obj
    end select
    return
  end function CastAsBaseModelClass

  subroutine AddBaseModelToList(list, model)
    implicit none
    ! -- dummy
    type(ListType),       intent(inout) :: list
    class(BaseModelType), pointer, intent(inout) :: model
    ! -- local
    class(*), pointer :: obj
    !
    obj => model
    call list%Add(obj)
    !
    return
  end subroutine AddBaseModelToList
  
  function GetBaseModelFromList(list, idx) result (res)
    implicit none
    ! -- dummy
    type(ListType),       intent(inout) :: list
    integer(I4B),              intent(in)    :: idx
    class(BaseModelType), pointer       :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsBaseModelClass(obj)
    !
    return
  end function GetBaseModelFromList

end module BaseModelModule

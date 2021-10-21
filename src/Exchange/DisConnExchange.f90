module DisConnExchangeModule
use KindModule, only: I4B, DP
use ConstantsModule, only: LENAUXNAME, LENBOUNDNAME
use ListModule, only: ListType
use NumericalModelModule, only: NumericalModelType
use NumericalExchangeModule, only: NumericalExchangeType
implicit none

private
public :: DisConnExchangeType
public :: CastAsDisConnExchangeClass, AddDisConnExchangeToList,                &
          GetDisConnExchangeFromList

!> Exchange based on connection between discretizations of DisBaseType.
!! The data specifies the connections, similar to the information stored
!! in the connections object: DisBaseType%con
!<
type, extends(NumericalExchangeType) :: DisConnExchangeType

  class(NumericalModelType), pointer               :: model1      => null()    !< model 1
  class(NumericalModelType), pointer               :: model2      => null()    !< model 2
  integer(I4B), pointer                            :: nexg        => null()    !< number of exchanges
  integer(I4B), dimension(:), pointer, contiguous  :: nodem1      => null()    !< node numbers in model 1
  integer(I4B), dimension(:), pointer, contiguous  :: nodem2      => null()    !< node numbers in model 2
  integer(I4B), dimension(:), pointer, contiguous  :: ihc         => null()    !< horizontal connection indicator array, size: nexg
  real(DP), dimension(:), pointer, contiguous      :: cl1         => null()    !< connection length 1, size: nexg
  real(DP), dimension(:), pointer, contiguous      :: cl2         => null()    !< connection length 2, size: nexg
  real(DP), dimension(:), pointer, contiguous      :: hwva        => null()    !< horizontal widths, vertical flow areas, size: nexg
  integer(I4B), pointer                            :: naux        => null()    !< number of auxiliary variables
  character(len=LENAUXNAME), dimension(:),                                     &
                              pointer, contiguous  :: auxname     => null()    !< vector of auxname
  real(DP), dimension(:, :), pointer, contiguous   :: auxvar      => null()    !< array of auxiliary variable values
  integer(I4B), pointer                            :: ianglex     => null()    !< flag indicating anglex was read, if read, ianglex is index in auxvar
  integer(I4B), pointer                            :: icdist      => null()    !< flag indicating cdist was read, if read, icdist is index in auxvar

  integer(I4B), pointer                            :: ixt3d       => null()    !< flag indicating if XT3D should be applied on the interface

  contains

  procedure :: allocate_scalars
  procedure :: allocate_arrays
  procedure :: disconnex_da
  

end type DisConnExchangeType

contains

!> @brief allocate scalars and initialize to defaults
!<
subroutine allocate_scalars(this)
  use MemoryManagerModule, only: mem_allocate
  class(DisConnExchangeType) :: this !< instance of exchange object

  call mem_allocate(this%nexg, 'NEXG', this%memoryPath)  
  call mem_allocate(this%naux, 'NAUX', this%memoryPath)
  call mem_allocate(this%ianglex, 'IANGLEX', this%memoryPath)
  call mem_allocate(this%icdist, 'ICDIST', this%memoryPath)
  call mem_allocate(this%ixt3d, 'IXT3D', this%memoryPath)
  
  this%nexg = 0  
  this%naux = 0
  this%ianglex = 0
  this%icdist = 0
  this%ixt3d = 0

end subroutine allocate_scalars

!> @brief allocate array data, using the number of
!! connected nodes @param nexg
!<
subroutine allocate_arrays(this)
  use MemoryManagerModule, only: mem_allocate
  class(DisConnExchangeType) :: this !< instance of exchange object
  
  call mem_allocate(this%nodem1, this%nexg, 'NODEM1', this%memoryPath)
  call mem_allocate(this%nodem2, this%nexg, 'NODEM2', this%memoryPath)  
  call mem_allocate(this%ihc, this%nexg, 'IHC', this%memoryPath)
  call mem_allocate(this%cl1, this%nexg, 'CL1', this%memoryPath)
  call mem_allocate(this%cl2, this%nexg, 'CL2', this%memoryPath)
  call mem_allocate(this%hwva, this%nexg, 'HWVA', this%memoryPath)  
  ! NB: auxname array is currently allocated while parsing
  call mem_allocate(this%auxvar, this%naux, this%nexg, 'AUXVAR', this%memoryPath)

end subroutine allocate_arrays

!> @brief clean up all scalars and arrays
!<
subroutine disconnex_da(this)
  use MemoryManagerModule, only: mem_deallocate
  class(DisConnExchangeType) :: this !< instance of exchange object
  
  ! arrays
  call mem_deallocate(this%nodem1)
  call mem_deallocate(this%nodem2)  
  call mem_deallocate(this%ihc)
  call mem_deallocate(this%cl1)
  call mem_deallocate(this%cl2)
  call mem_deallocate(this%hwva)  
  call mem_deallocate(this%auxvar)
  
  ! scalars
  call mem_deallocate(this%nexg)
  call mem_deallocate(this%naux)
  call mem_deallocate(this%auxname, 'AUXNAME', trim(this%memoryPath))
  call mem_deallocate(this%ianglex)
  call mem_deallocate(this%icdist)
  call mem_deallocate(this%ixt3d)

end subroutine disconnex_da

function CastAsDisConnExchangeClass(obj) result (res)
  implicit none
  class(*), pointer, intent(inout) :: obj
  class(DisConnExchangeType), pointer :: res
  !
  res => null()
  if (.not. associated(obj)) return
  !
  select type (obj)
  class is (DisConnExchangeType)
    res => obj
  end select
  return
end function CastAsDisConnExchangeClass

subroutine AddDisConnExchangeToList(list, exchange)
  implicit none
  ! -- dummy
  type(ListType),       intent(inout) :: list
  class(DisConnExchangeType), pointer, intent(in) :: exchange
  ! -- local
  class(*), pointer :: obj
  !
  obj => exchange
  call list%Add(obj)
  !
  return
end subroutine AddDisConnExchangeToList

function GetDisConnExchangeFromList(list, idx) result (res)
  implicit none
  ! -- dummy
  type(ListType),            intent(inout) :: list
  integer(I4B),                   intent(in)    :: idx
  class(DisConnExchangeType), pointer    :: res
  ! -- local
  class(*), pointer :: obj
  !
  obj => list%GetItem(idx)
  res => CastAsDisConnExchangeClass(obj)
  !
  return
end function GetDisConnExchangeFromList

end module DisConnExchangeModule
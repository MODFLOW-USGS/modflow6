module TopologyModule
  use KindModule, only: I4B
  use NumericalModelModule, only: NumericalModelType
  implicit none
  private

  integer(I4B), parameter :: defaultCapacity = 6

  !> Data structure to hold a global cell identifier,
  !! using a pointer to the model and its local cell 
  !< index
  type, public :: GlobalCellType
    integer(I4B) :: index                                 !< the index on the model grid
    class(NumericalModelType), pointer :: model => null() !< the model
  end type
  
  ! a global cell with neighbors
  type, public :: CellWithNbrsType
    type(GlobalCellType) :: cell
    integer(I4B) :: nrOfNbrs = 0
    type(CellWithNbrsType), dimension(:), pointer, contiguous :: neighbors => null()
  contains
    procedure :: addNbr
  end type
  
  ! a model with neighbors
  type, public :: ModelWithNbrsType
      class(NumericalModelType), pointer :: model => null()
      integer(I4B) :: nrOfNbrs = 0
      type(ModelWithNbrsType), dimension(:), pointer, contiguous :: neighbors => null()
  end type

  contains

  subroutine addNbr(this, index, model)
    class(CellWithNbrsType) :: this
    integer(I4B) :: index
    class(NumericalModelType), pointer :: model
    ! local
    integer(I4B) :: nbrCnt, currentSize, i
    type(CellWithNbrsType), dimension(:), pointer,  contiguous :: newNeighbors
    type(CellWithNbrsType), dimension(:), pointer,  contiguous :: oldNeighbors

    if (.not. associated(this%neighbors)) then
      allocate(this%neighbors(defaultCapacity))
      this%nrOfNbrs = 0
    end if
    
    nbrCnt = this%nrOfNbrs
    currentSize = size(this%neighbors)
    if (nbrCnt + 1 > currentSize) then

      ! inflate
      oldNeighbors => this%neighbors
      allocate(newNeighbors(currentSize + defaultCapacity))
      do i=1, currentSize
        newNeighbors(i) = oldNeighbors(i)
      end do
      this%neighbors => newNeighbors

      ! clean up
      deallocate(oldNeighbors)
      nullify(oldNeighbors)
    end if
        
    this%neighbors(nbrCnt + 1)%cell%index = index
    this%neighbors(nbrCnt + 1)%cell%model => model
    this%nrOfNbrs = nbrCnt + 1
    
  end subroutine addNbr

end module
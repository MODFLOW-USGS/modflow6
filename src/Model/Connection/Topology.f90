module TopologyModule
  use KindModule, only: I4B
  use NumericalModelModule, only: NumericalModelType
  implicit none
  private

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
  end type
  
  ! a model with neighbors
  type, public :: ModelWithNbrsType
      class(NumericalModelType), pointer :: model => null()
      integer(I4B) :: nrOfNbrs = 0
      type(ModelWithNbrsType), dimension(:), pointer, contiguous :: neighbors => null()
  end type

end module
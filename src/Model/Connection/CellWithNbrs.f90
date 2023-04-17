module CellWithNbrsModule
  use KindModule, only: I4B, LGP
  use NumericalModelModule, only: NumericalModelType
  use VirtualModelModule, only: VirtualModelType
  implicit none
  private

  integer(I4B), parameter :: defaultCapacity = 6

  !> Data structure to hold a global cell identifier,
  !! using a pointer to the model and its local cell
  !< index
  type, public :: GlobalCellType
    integer(I4B) :: index !< the index on the model grid
    class(VirtualModelType), pointer :: v_model => null() !< virtual model
  end type

  ! a global cell with neighbors
  type, public :: CellWithNbrsType
    type(GlobalCellType) :: cell
    integer(I4B) :: nrOfNbrs = 0
    type(CellWithNbrsType), dimension(:), pointer, &
      contiguous :: neighbors => null()
  contains
    procedure :: addNbrCell
  end type

contains

  subroutine addNbrCell(this, index, v_model)
    class(CellWithNbrsType) :: this
    integer(I4B) :: index
    class(VirtualModelType), pointer :: v_model
    ! local
    integer(I4B) :: nbrCnt, currentSize, i
    type(CellWithNbrsType), dimension(:), pointer, contiguous :: newNeighbors
    type(CellWithNbrsType), dimension(:), pointer, contiguous :: oldNeighbors

    if (.not. associated(this%neighbors)) then
      allocate (this%neighbors(defaultCapacity))
      this%nrOfNbrs = 0
    end if

    nbrCnt = this%nrOfNbrs
    currentSize = size(this%neighbors)
    if (nbrCnt + 1 > currentSize) then

      ! inflate
      oldNeighbors => this%neighbors
      allocate (newNeighbors(currentSize + defaultCapacity))
      do i = 1, currentSize
        newNeighbors(i) = oldNeighbors(i)
      end do
      this%neighbors => newNeighbors

      ! clean up
      deallocate (oldNeighbors)
      nullify (oldNeighbors)
    end if

    this%neighbors(nbrCnt + 1)%cell%index = index
    this%neighbors(nbrCnt + 1)%cell%v_model => v_model

    this%nrOfNbrs = nbrCnt + 1

  end subroutine addNbrCell

end module

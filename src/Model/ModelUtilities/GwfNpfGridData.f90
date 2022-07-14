module GwfNpfGridDataModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO
  implicit none
  private

  !> Data structure and helper methods for passing NPF grid data
  !! into npf_ar, as an alternative to reading those from file.
  !! As this is a temporary object, the variables are not
  !! allocated inside the memory manager.
  !<
  type, public :: GwfNpfGridDataType
    ! grid data
    integer(I4B) :: ik22 !< flag equals 1 when present
    integer(I4B) :: ik33 !< flag equals 1 when present
    integer(I4B) :: iwetdry !< flag equals 1 when present
    integer(I4B) :: iangle1 !< flag equals 1 when present
    integer(I4B) :: iangle2 !< flag equals 1 when present
    integer(I4B) :: iangle3 !< flag equals 1 when present
    integer(I4B), dimension(:), pointer, contiguous :: icelltype => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous :: k11 => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous :: k22 => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous :: k33 => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous :: wetdry => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous :: angle1 => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous :: angle2 => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous :: angle3 => null() !< same as npf variable
  contains
    procedure, pass(this) :: construct
    procedure, pass(this) :: destroy
  end type GwfNpfGridDataType

contains

!> @brief construct the input data object, allocating
!! the arrays at proper size and initializing the variables
!! at their defaults
!<
  subroutine construct(this, nodes)
    class(GwfNpfGridDataType), intent(inout) :: this !< the NPF grid data, as in the input GRIDDATA block
    integer(I4B) :: nodes !< the number of nodes in the solution
    ! local
    integer(I4B) :: i

    this%ik22 = 0
    this%ik33 = 0
    this%iwetdry = 0
    this%iangle1 = 0
    this%iangle2 = 0
    this%iangle3 = 0

    allocate (this%icelltype(nodes))
    allocate (this%k11(nodes))
    allocate (this%k22(nodes))
    allocate (this%k33(nodes))
    allocate (this%wetdry(nodes))
    allocate (this%angle1(nodes))
    allocate (this%angle2(nodes))
    allocate (this%angle3(nodes))

    do i = 1, nodes
      this%icelltype(i) = DZERO
      this%k11(i) = DZERO
      this%k22(i) = DZERO
      this%k33(i) = DZERO
      this%wetdry(i) = DZERO
      this%angle1(i) = DZERO
      this%angle2(i) = DZERO
      this%angle3(i) = DZERO
    end do

  end subroutine construct

!> @brief clean up, deallocate, etc.
!<
  subroutine destroy(this)
    class(GwfNpfGridDataType), intent(inout) :: this !< the data structure

    deallocate (this%icelltype)
    deallocate (this%k11)
    deallocate (this%k22)
    deallocate (this%k33)
    deallocate (this%wetdry)
    deallocate (this%angle1)
    deallocate (this%angle2)
    deallocate (this%angle3)

  end subroutine destroy

end module GwfNpfGridDataModule

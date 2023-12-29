module GwfBuyInputDataModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENMODELNAME, LENAUXNAME, DZERO

  implicit none
  private

  !> Data structure to transfer input configuration to the
  !< BUY package, as opposed to reading from file
  type, public :: GwfBuyInputDataType

    ! options
    integer(I4B) :: iform !< see BUY for description
    real(DP) :: denseref !< see BUY for description
    ! dim
    integer(I4B) :: nrhospecies !< see BUY for description

    ! pkg data
    real(DP), dimension(:), pointer, contiguous :: drhodc => null() !< see BUY for description
    real(DP), dimension(:), pointer, contiguous :: crhoref => null() !< see BUY for description
    character(len=LENMODELNAME), dimension(:), allocatable :: cmodelname !< see BUY for description
    character(len=LENAUXNAME), dimension(:), allocatable :: cauxspeciesname !< see BUY for description

  contains
    procedure, pass(this) :: construct
    procedure, pass(this) :: destruct
  end type GwfBuyInputDataType

contains

!> @brief Allocate the input data
!<
  subroutine construct(this, nrhospecies)
    class(GwfBuyInputDataType) :: this !< the input data block
    integer(I4B) :: nrhospecies !< the number of species

    allocate (this%drhodc(nrhospecies))
    allocate (this%crhoref(nrhospecies))
    allocate (this%cmodelname(nrhospecies))
    allocate (this%cauxspeciesname(nrhospecies))

  end subroutine construct

  !> @brief clean up
  !<
  subroutine destruct(this)
    class(GwfBuyInputDataType) :: this !< the input data block

    deallocate (this%drhodc)
    deallocate (this%crhoref)
    deallocate (this%cmodelname)
    deallocate (this%cauxspeciesname)

  end subroutine destruct

end module GwfBuyInputDataModule

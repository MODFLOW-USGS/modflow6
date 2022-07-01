module GwtDspGridDataModule
  use KindModule, only: DP, I4B
  implicit none
  private

!> @brief data structure and helpers for passing dsp grid data
!< into the package, as opposed to reading from file
  type, public :: GwtDspGridDataType
    real(DP), dimension(:), pointer, contiguous :: diffc => null() !< molecular diffusion coefficient for each cell
    real(DP), dimension(:), pointer, contiguous :: alh => null() !< longitudinal horizontal dispersivity
    real(DP), dimension(:), pointer, contiguous :: alv => null() !< longitudinal vertical dispersivity
    real(DP), dimension(:), pointer, contiguous :: ath1 => null() !< transverse horizontal dispersivity
    real(DP), dimension(:), pointer, contiguous :: ath2 => null() !< transverse horizontal dispersivity
    real(DP), dimension(:), pointer, contiguous :: atv => null() !< transverse vertical dispersivity
  contains
    procedure, pass(this) :: construct
    procedure, pass(this) :: destroy
  end type GwtDspGridDataType

contains

!> @brief allocate data structure
!<
  subroutine construct(this, nodes)
    class(GwtDspGridDataType) :: this
    integer(I4B) :: nodes

    allocate (this%diffc(nodes))
    allocate (this%alh(nodes))
    allocate (this%alv(nodes))
    allocate (this%ath1(nodes))
    allocate (this%ath2(nodes))
    allocate (this%atv(nodes))

  end subroutine construct

!> @brief clean up
!<
  subroutine destroy(this)
    class(GwtDspGridDataType) :: this

    deallocate (this%diffc)
    deallocate (this%alh)
    deallocate (this%alv)
    deallocate (this%ath1)
    deallocate (this%ath2)
    deallocate (this%atv)

  end subroutine destroy

end module GwtDspGridDataModule

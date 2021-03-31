module GwfNpfInputDataModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DONE
  implicit none
  private

  !> Data structure and helper methods for passing NPF options and grid data 
  !! into npf_df and npf_ar, as an alternative to reading those from file
  !<
  type, public :: GwfNpfInputDataType
    ! options:  
    integer(I4B) :: icellavg
    integer(I4B) :: ithickstrt
    integer(I4B) :: iperched
    integer(I4B) :: ivarcv
    integer(I4B) :: idewatcv
    integer(I4B) :: irewet
    real(DP) :: wetfct
    integer(I4B) :: iwetit
    integer(I4B) :: ihdwet
    integer(I4B) :: ixt3d

    ! grid data
    integer(I4B) :: ik22    !< flag equals 1 when present
    integer(I4B) :: ik33    !< flag equals 1 when present
    integer(I4B) :: iwetdry !< flag equals 1 when present
    integer(I4B) :: iangle1 !< flag equals 1 when present
    integer(I4B) :: iangle2 !< flag equals 1 when present
    integer(I4B) :: iangle3 !< flag equals 1 when present
    integer(I4B), dimension(:), pointer, contiguous :: icelltype    => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous     :: k11          => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous     :: k22          => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous     :: k33          => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous     :: wetdry       => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous     :: angle1       => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous     :: angle2       => null() !< same as npf variable
    real(DP), dimension(:), pointer, contiguous     :: angle3       => null() !< same as npf variable
  contains
    procedure, pass(this) :: construct
    procedure, pass(this) :: destroy
  end type GwfNpfInputDataType

contains

!> @brief construct the input data object, allocating
!! the arrays at proper size and initializing the variables
!! at their defaults
!<
subroutine construct(this, nodes)
  class(GwfNpfInputDataType), intent(inout) :: this  !< the NPF input data structure
  integer(I4B) :: nodes                             !< the number of nodes in the solution
  
  ! options
  this%icellavg = 0
  this%ithickstrt = 0
  this%iperched = 0
  this%ivarcv = 0
  this%idewatcv = 0
  this%irewet = 0
  this%wetfct = DONE
  this%iwetit = 1
  this%ihdwet = 0
  this%ixt3d = 0

  ! grid data
  this%ik22 = 0
  this%ik33 = 0
  this%iwetdry = 0
  this%iangle1 = 0
  this%iangle2 = 0
  this%iangle3 = 0
  allocate(this%icelltype(nodes))
  allocate(this%k11(nodes))
  allocate(this%k22(nodes))
  allocate(this%k33(nodes))
  allocate(this%wetdry(nodes))
  allocate(this%angle1(nodes))
  allocate(this%angle2(nodes))
  allocate(this%angle3(nodes))

end subroutine construct

!> @brief clean up, deallocate, etc.
!<
subroutine destroy(this)
  class(GwfNpfInputDataType), intent(inout) :: this  !< the data structure

  deallocate(this%icelltype)
  deallocate(this%k11)
  deallocate(this%k22)
  deallocate(this%k33)
  deallocate(this%wetdry)
  deallocate(this%angle1)
  deallocate(this%angle2)
  deallocate(this%angle3)

end subroutine destroy


end module GwfNpfInputDataModule
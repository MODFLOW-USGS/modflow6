module GwfStorageUtilsModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DONE

  implicit none
  private
  public :: SsCapacity
  public :: SyCapacity

contains

  !> @brief Calculate the specific storage capacity
  !!
  !! Function to calculate the specific storage capacity using
  !! the cell geometry and the specific storage or storage coefficient.
  !!
  !! @return      sc1               specific storage capacity
  !<
  pure function SsCapacity(isfac, top, bot, area, ss) result(sc1)
    ! -- dummy variables
    integer(I4B), intent(in) :: isfac  !< flag indicating if ss is the storage coefficient
    real(DP), intent(in) :: top        !< top of cell
    real(DP), intent(in) :: bot        !< bottom of cell
    real(DP), intent(in) :: area       !< horizontal cell area
    real(DP), intent(in) :: ss         !< specific storage or storage coefficient
    ! -- local variables
    real(DP) :: sc1
    real(DP) :: thick
    ! -- calculate specific storage capacity
    if (isfac == 0) then
      thick = top - bot
    else
      thick = DONE
    end if
    sc1 = ss*thick*area
    !
    ! -- return
    return
  end function SsCapacity

  !> @brief Calculate the specific yield capacity
  !!
  !! Function to calculate the specific yield capacity using
  !! the cell area and the specific yield.
  !!
  !! @return      sc2               specific yield capacity
  !<
  pure function SyCapacity(area, sy) result(sc2)
    ! -- dummy variables
    real(DP), intent(in) :: area       !< horizontal cell area
    real(DP), intent(in) :: sy         !< specific yield
    ! -- local variables
    real(DP) :: sc2
    ! -- calculate specific yield capacity
    sc2 = sy*area
    !
    ! -- return
    return
  end function SyCapacity

end module GwfStorageUtilsModule

module AveragingModule

  use KindModule,                 only: DP, I4B
  use ConstantsModule,            only: DZERO, DEM9, DEM8, DEM7, DEM6, DEM2,    &
                                        DHALF, DP9, DONE, DLNLOW, DLNHIGH,      &
                                        DHNOFLO, DHDRY, DEM10, LENORIGIN,       &
                                        LINELENGTH

  implicit none

  private
  public :: condmean

contains

  function condmean(k1, k2, thick1, thick2, cl1, cl2, width, iavgmeth)
! ******************************************************************************
! condmean -- Calculate the conductance between two cells
!
!   k1 is hydraulic conductivity for cell 1 (in the direction of cell2)
!   k2 is hydraulic conductivity for cell 2 (in the direction of cell1)
!   thick1 is the saturated thickness for cell 1
!   thick2 is the saturated thickness for cell 2
!   cl1 is the distance from the center of cell1 to the shared face with cell2
!   cl2 is the distance from the center of cell2 to the shared face with cell1
!   h1 is the head for cell1
!   h2 is the head for cell2
!   width is the width perpendicular to flow
!   iavgmeth is the averaging method:
!     0 is harmonic averaging
!     1 is logarithmic averaging
!     2 is arithmetic averaging of sat thickness and logarithmic averaging of
!       hydraulic conductivity
!     3 is arithmetic averaging of sat thickness and harmonic averaging of
!       hydraulic conductivity
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: condmean
    ! -- dummy
    real(DP), intent(in) :: k1
    real(DP), intent(in) :: k2
    real(DP), intent(in) :: thick1
    real(DP), intent(in) :: thick2
    real(DP), intent(in) :: cl1
    real(DP), intent(in) :: cl2
    real(DP), intent(in) :: width
    integer(I4B), intent(in) :: iavgmeth
    ! -- local
    real(DP) :: t1
    real(DP) :: t2
    real(DP) :: tmean, kmean, denom
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    t1 = k1 * thick1
    t2 = k2 * thick2
    !
    ! -- Averaging
    select case (iavgmeth)
    !
    ! -- Harmonic-mean method
    case(0)
      !
      if (t1*t2 > DZERO) then
        condmean = width * t1 * t2 / (t1 * cl2 + t2 * cl1)
      else
        condmean = DZERO
      end if
    !
    ! -- Logarithmic-mean method
    case(1)
      if (t1*t2 > DZERO) then
        tmean = logmean(t1, t2)
      else
        tmean = DZERO
      endif
      condmean = tmean * width / (cl1 + cl2)
    !
    ! -- Arithmetic-mean thickness and logarithmic-mean hydraulic conductivity
    case(2)
      if (k1*k2 > DZERO) then
        kmean = logmean(k1, k2)
      else
        kmean = DZERO
      endif
      condmean = kmean * DHALF * (thick1 + thick2) * width / (cl1 + cl2)
    !
    ! -- Arithmetic-mean thickness and harmonic-mean hydraulic conductivity
    case(3)
      denom = (k1 * cl2 + k2 * cl1)
      if (denom > DZERO) then
        kmean = k1 * k2 / denom
      else
        kmean = DZERO
      end if
      condmean = kmean * DHALF * (thick1 + thick2) * width
    end select
    !
    ! -- Return
    return
  end function condmean

  function logmean(d1, d2)
! ******************************************************************************
! logmean -- Calculate the the logarithmic mean of two double precision
!            numbers.  Use an approximation if the ratio is near 1.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: logmean
    ! -- dummy
    real(DP), intent(in) :: d1
    real(DP), intent(in) :: d2
    ! -- local
    real(DP) :: drat
! ------------------------------------------------------------------------------
    !
    drat = d2 / d1
    if(drat <= DLNLOW .or. drat >= DLNHIGH) then
      logmean = (d2 - d1) / log(drat)
    else
      logmean = DHALF * (d1 + d2)
    endif
    !
    ! -- Return
    return
  end function logmean

end module AveragingModule

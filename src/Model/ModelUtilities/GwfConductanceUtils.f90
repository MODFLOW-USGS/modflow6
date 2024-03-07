!> @brief This module contains stateless storage subroutines and functions
!!
!! This module contains the functions to calculate the specific
!! storage (SC1) and specific yield (SC2) capacities that are used in
!! the storage (STO) package. It also contains subroutines to calculate
!! the amat and rhs terms for specific storage and specific yield.
!! This module does not depend on the STO package.
!!
!<
module GwfConductanceUtilsModule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DHALF, DONE, &
                             DLNLOW, DLNHIGH

  implicit none
  private
  public :: hcond
  public :: vcond
  public :: condmean
  public :: thksatnm
  public :: staggered_thkfrac

contains

  !> @brief Horizontal conductance between two cells
  !!
  !! inwtup: if 1, then upstream-weight condsat, otherwise recalculate
  !!
  !! This function uses a weighted transmissivity in the harmonic mean
  !! conductance calculations. This differs from the MODFLOW-NWT and
  !! MODFLOW-USG conductance calculations for the Newton-Raphson formulation
  !! which use a weighted hydraulic conductivity.
  !<
  function hcond(ibdn, ibdm, ictn, ictm, inewton, inwtup, ihc, icellavg, &
                 condsat, hn, hm, satn, satm, hkn, hkm, topn, topm, &
                 botn, botm, cln, clm, fawidth, satomega) &
    result(condnm)
    ! -- return
    real(DP) :: condnm
    ! -- dummy
    integer(I4B), intent(in) :: ibdn
    integer(I4B), intent(in) :: ibdm
    integer(I4B), intent(in) :: ictn
    integer(I4B), intent(in) :: ictm
    integer(I4B), intent(in) :: inewton
    integer(I4B), intent(in) :: inwtup
    integer(I4B), intent(in) :: ihc
    integer(I4B), intent(in) :: icellavg
    real(DP), intent(in) :: condsat
    real(DP), intent(in) :: hn
    real(DP), intent(in) :: hm
    real(DP), intent(in) :: satn
    real(DP), intent(in) :: satm
    real(DP), intent(in) :: hkn
    real(DP), intent(in) :: hkm
    real(DP), intent(in) :: topn
    real(DP), intent(in) :: topm
    real(DP), intent(in) :: botn
    real(DP), intent(in) :: botm
    real(DP), intent(in) :: cln
    real(DP), intent(in) :: clm
    real(DP), intent(in) :: fawidth
    real(DP), intent(in) :: satomega
    ! -- local
    real(DP) :: thksatn
    real(DP) :: thksatm
    !
    ! -- If either n or m is inactive then conductance is zero
    if (ibdn == 0 .or. ibdm == 0) then
      condnm = DZERO
      !
      ! -- if both cells are non-convertible then use condsat
    elseif (ictn == 0 .and. ictm == 0) then
      condnm = condsat
      !
      ! -- At least one of the cells is convertible and using the
      !    newton-raphson conductance formulation
    else if (inwtup == 1) then
      if (hn > hm) then
        condnm = satn
      else
        condnm = satm
      end if
      !
      ! -- multiply condsat by condnm factor
      condnm = condnm * condsat
      !
      ! -- At least one of the cells is convertible and using the
      !    standard conductance formulation
    else
      !
      ! -- If staggered connection, subtract parts of cell that are above and
      !    below the sill top and bottom elevations
      if (ihc == 2) then
        thksatn = staggered_thkfrac(topn, botn, satn, topm, botm)
        thksatm = staggered_thkfrac(topm, botm, satm, topn, botn)
      else
        thksatn = satn * (topn - botn)
        thksatm = satm * (topm - botm)
      end if
      ! -- calculate the appropriate mean
      condnm = condmean(hkn, hkm, thksatn, thksatm, cln, clm, &
                        fawidth, icellavg)
    end if
    return
  end function hcond

  !> @brief Vertical conductance between two cells
  !<
  function vcond(ibdn, ibdm, ictn, ictm, inewton, ivarcv, idewatcv, &
                 condsat, hn, hm, vkn, vkm, satn, satm, topn, topm, botn, &
                 botm, flowarea) result(condnm)
    ! -- return
    real(DP) :: condnm
    ! -- dummy
    integer(I4B), intent(in) :: ibdn
    integer(I4B), intent(in) :: ibdm
    integer(I4B), intent(in) :: ictn
    integer(I4B), intent(in) :: ictm
    integer(I4B), intent(in) :: inewton
    integer(I4B), intent(in) :: ivarcv
    integer(I4B), intent(in) :: idewatcv
    real(DP), intent(in) :: condsat
    real(DP), intent(in) :: hn
    real(DP), intent(in) :: hm
    real(DP), intent(in) :: vkn
    real(DP), intent(in) :: vkm
    real(DP), intent(in) :: satn
    real(DP), intent(in) :: satm
    real(DP), intent(in) :: topn
    real(DP), intent(in) :: topm
    real(DP), intent(in) :: botn
    real(DP), intent(in) :: botm
    real(DP), intent(in) :: flowarea
    ! -- local
    real(DP) :: satntmp, satmtmp
    real(DP) :: bovk1
    real(DP) :: bovk2
    real(DP) :: denom
    !
    ! -- If either n or m is inactive then conductance is zero
    if (ibdn == 0 .or. ibdm == 0) then
      condnm = DZERO
      !
      ! -- if constantcv then use condsat
    elseif (ivarcv == 0) then
      condnm = condsat
      !
      ! -- if both cells are non-convertible then use condsat
    elseif (ictn == 0 .and. ictm == 0) then
      condnm = condsat
      !
      ! -- if both cells are fully saturated then use condsat
    elseif (hn >= topn .and. hm >= topm) then
      condnm = condsat
      !
      ! -- At least one cell is partially saturated, so recalculate vertical
      ! -- conductance for this connection
      ! -- todo: upstream weighting?
    else
      !
      ! -- Default is for CV correction (dewatered option); use underlying
      !    saturation of 1.
      satntmp = satn
      satmtmp = satm
      if (idewatcv == 0) then
      if (botn > botm) then
        ! -- n is above m
        satmtmp = DONE
      else
        ! -- m is above n
        satntmp = DONE
      end if
      end if
      bovk1 = satntmp * (topn - botn) * DHALF / vkn
      bovk2 = satmtmp * (topm - botm) * DHALF / vkm
      denom = (bovk1 + bovk2)
      if (denom /= DZERO) then
        condnm = flowarea / denom
      else
        condnm = DZERO
      end if
    end if
    return
  end function vcond

  !> @brief Calculate the conductance between two cells
  !!
  !! k1 is hydraulic conductivity for cell 1 (in the direction of cell2)
  !! k2 is hydraulic conductivity for cell 2 (in the direction of cell1)
  !! thick1 is the saturated thickness for cell 1
  !! thick2 is the saturated thickness for cell 2
  !! cl1 is the distance from the center of cell1 to the shared face with cell2
  !! cl2 is the distance from the center of cell2 to the shared face with cell1
  !! h1 is the head for cell1
  !! h2 is the head for cell2
  !! width is the width perpendicular to flow
  !! iavgmeth is the averaging method:
  !!   0 is harmonic averaging
  !!   1 is logarithmic averaging
  !!   2 is arithmetic averaging of sat thickness and logarithmic averaging of
  !!     hydraulic conductivity
  !!   3 is arithmetic averaging of sat thickness and harmonic averaging of
  !!     hydraulic conductivity
  !<
  function condmean(k1, k2, thick1, thick2, cl1, cl2, width, iavgmeth)
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
    !
    ! -- Initialize
    t1 = k1 * thick1
    t2 = k2 * thick2
    !
    ! -- Averaging
    select case (iavgmeth)
      !
      ! -- Harmonic-mean method
    case (0)
      if (t1 * t2 > DZERO) then
        condmean = width * t1 * t2 / (t1 * cl2 + t2 * cl1)
      else
        condmean = DZERO
      end if
      !
      ! -- Logarithmic-mean method
    case (1)
      if (t1 * t2 > DZERO) then
        tmean = logmean(t1, t2)
      else
        tmean = DZERO
      end if
      condmean = tmean * width / (cl1 + cl2)
      !
      ! -- Arithmetic-mean thickness and logarithmic-mean hydraulic conductivity
    case (2)
      if (k1 * k2 > DZERO) then
        kmean = logmean(k1, k2)
      else
        kmean = DZERO
      end if
      condmean = kmean * DHALF * (thick1 + thick2) * width / (cl1 + cl2)
      !
      ! -- Arithmetic-mean thickness and harmonic-mean hydraulic conductivity
    case (3)
      denom = (k1 * cl2 + k2 * cl1)
      if (denom > DZERO) then
        kmean = k1 * k2 / denom
      else
        kmean = DZERO
      end if
      condmean = kmean * DHALF * (thick1 + thick2) * width
    end select
    return
  end function condmean

  !> @brief Calculate the the logarithmic mean of two double precision numbers
  !!
  !! Use an approximation if the ratio is near 1
  !<
  function logmean(d1, d2)
    ! -- return
    real(DP) :: logmean
    ! -- dummy
    real(DP), intent(in) :: d1
    real(DP), intent(in) :: d2
    ! -- local
    real(DP) :: drat
    drat = d2 / d1
    if (drat <= DLNLOW .or. drat >= DLNHIGH) then
      logmean = (d2 - d1) / log(drat)
    else
      logmean = DHALF * (d1 + d2)
    end if
    return
  end function logmean

  !> @brief Calculate saturated thickness at interface between two cells
  !<
  function thksatnm(ibdn, ibdm, ictn, ictm, inwtup, ihc, &
                    hn, hm, satn, satm, topn, topm, botn, botm, &
                    satomega) result(res)
    ! -- return
    real(DP) :: res
    ! -- dummy
    integer(I4B), intent(in) :: ibdn
    integer(I4B), intent(in) :: ibdm
    integer(I4B), intent(in) :: ictn
    integer(I4B), intent(in) :: ictm
    integer(I4B), intent(in) :: inwtup
    integer(I4B), intent(in) :: ihc
    real(DP), intent(in) :: hn
    real(DP), intent(in) :: hm
    real(DP), intent(in) :: satn
    real(DP), intent(in) :: satm
    real(DP), intent(in) :: topn
    real(DP), intent(in) :: topm
    real(DP), intent(in) :: botn
    real(DP), intent(in) :: botm
    real(DP), intent(in) :: satomega
    ! -- local
    real(DP) :: sn
    real(DP) :: sm
    real(DP) :: thksatn
    real(DP) :: thksatm
    real(DP) :: sill_top, sill_bot
    !
    ! -- If either n or m is inactive then saturated thickness is zero
    if (ibdn == 0 .or. ibdm == 0) then
      res = DZERO
      !
      ! -- if both cells are non-convertible then use average cell thickness
    elseif (ictn == 0 .and. ictm == 0) then
      !
      ! -- If staggered connection, subtract parts of cell that are above and
      !    below the sill top and bottom elevations
      if (ihc == 2) then
        !
        ! -- Calculate sill_top and sill_bot
        sill_top = min(topn, topm)
        sill_bot = max(botn, botm)
        !
        ! -- Saturated thickness is sill_top - sill_bot
        thksatn = max(sill_top - sill_bot, DZERO)
        thksatm = thksatn
      else
        thksatn = topn - botn
        thksatm = topm - botm
      end if
      res = DHALF * (thksatn + thksatm)
      !
      ! -- At least one of the cells is convertible and using the
      !    Newton-Raphson conductance formulation
    elseif (inwtup == 1) then
      sn = satn
      sm = satm
      !
      ! -- upstream weight the thickness
      if (hn > hm) then
        res = sn * (topn - botn)
      else
        res = sm * (topm - botm)
      end if
      !
      !
      ! -- At least one of the cells is convertible and using the
      !    standard conductance formulation
    else
      !
      ! -- If staggered connection, subtract parts of cell that are above and
      !    below the sill top and bottom elevations
      if (ihc == 2) then
        thksatn = staggered_thkfrac(topn, botn, satn, topm, botm)
        thksatm = staggered_thkfrac(topm, botm, satm, topn, botn)
      else
        thksatn = satn * (topn - botn)
        thksatm = satm * (topm - botm)
      end if
      res = DHALF * (thksatn + thksatm)
    end if
    return
  end function thksatnm

  !> @brief Calculate the thickness fraction for staggered grids
  !<
  function staggered_thkfrac(top, bot, sat, topc, botc) result(res)
    ! -- return
    real(DP) :: res !< staggered thickness fraction for cell
    ! -- dummy
    real(DP) :: top !< top of cell
    real(DP) :: bot !< bottom of cell
    real(DP) :: sat !< cell saturation
    real(DP) :: topc !< top of connected cell
    real(DP) :: botc !< bottom of connected cells
    ! -- local
    real(DP) :: sill_top
    real(DP) :: sill_bot
    real(DP) :: tp
    !
    ! -- Calculate sill_top and sill_bot
    sill_top = min(top, topc)
    sill_bot = max(bot, botc)
    !
    ! -- Calculate tp
    tp = bot + sat * (top - bot)

    ! -- Calculate saturated thickness
    res = max(min(tp, sill_top) - sill_bot, DZERO)
    return
  end function staggered_thkfrac

end module GwfConductanceUtilsModule

submodule(SfrModule) SfrModuleTransient
contains

  !> @brief Method for solving for transient reaches
    !!
    !! Method to solve the continuity equation for a SFR package
    !! reach using the kinematic wave approximation.
    !!
  !<
  module procedure sfr_calc_transient
  use TdisModule, only: delt

  integer(I4B) :: igwfconn
  integer(I4B) :: number_picard
  integer(I4B) :: iconverged
  integer(I4B) :: i
  integer(I4B) :: j
  real(DP) :: kinematic_residual
  real(DP) :: kinematic_storage
  real(DP) :: weight
  ! real(DP) :: weightinv
  real(DP) :: dq
  real(DP) :: qtol
  real(DP) :: qsrc
  real(DP) :: qlat
  real(DP) :: da
  real(DP) :: db
  real(DP) :: dc
  real(DP) :: dd
  real(DP) :: qa
  real(DP) :: qb
  real(DP) :: qc
  real(DP) :: xsa_a
  real(DP) :: xsa_b
  real(DP) :: xsa_c
  ! real(DP) :: xsa_d
  real(DP) :: d1old
  real(DP) :: qd2
  real(DP) :: ad
  real(DP) :: ad2
  real(DP) :: qgwf_pul
  ! real(DP) :: f11
  ! real(DP) :: f12
  real(DP) :: residual
  real(DP) :: residual2
  real(DP) :: residual_final
  real(DP) :: qderv
  real(DP) :: delq
  real(DP) :: delh
  real(DP) :: dd2

  weight = this%storage_weight
  dq = this%deps !DEM6 !DEM4 !this%deps
  qtol = dq * DTWO !1e-9 !dq * DTWO

  qgwf = DZERO
  qgwf_pul = DZERO

  ! calculate the flow at end of the reach
  ! excluding groundwater leakage
  qsrc = qu + qi + qr - qe + qro + qfrommvr

  qlat = (qr + qro - qe) / this%length(n)

  this%usinflow(n) = qu + qi + qfrommvr

  qa = this%usinflowold(n)
  qb = this%dsflowold(n)
  qc = this%usinflow(n)
  call this%sfr_calc_reach_depth(n, qa, da)
  call this%sfr_calc_reach_depth(n, qb, db)
  call this%sfr_calc_reach_depth(n, qc, dc)

  xsa_a = this%calc_area_wet(n, da)
  xsa_b = this%calc_area_wet(n, db)
  xsa_c = this%calc_area_wet(n, dc)

  ! estimate qd
  qd = (qc + qb) * DHALF
  call this%sfr_calc_reach_depth(n, qd, dd)
  ad = this%calc_area_wet(n, dd)

  ! -- estimate the depth at the midpoint
  d1 = (dc + dd) * DHALF
  d1old = d1

  igwfconn = this%sfr_gwf_conn(n)
  number_picard = this%maxsfrpicard
  if (igwfconn == 1) then
    number_picard = this%maxsfrpicard
  else
    number_picard = 1
  end if

  kinematicpicard: do i = 1, number_picard
    if (igwfconn == 1) then
      call this%sfr_calc_qgwf(n, d1, hgwf, qgwf)
      if (qgwf > qsrc) then
        qgwf = qsrc
      end if
    end if

    qsrc = qlat - qgwf / this%length(n)

    newton: do j = 1, this%maxsfrit
      qd2 = qd + dq
      call this%sfr_calc_reach_depth(n, qd2, dd2)
      ad2 = this%calc_area_wet(n, dd2)

      residual = kinematic_residual(qa, qb, qc, qd, &
                                    xsa_a, xsa_b, xsa_c, ad, &
                                    qsrc, this%length(n), weight, delt)

      residual2 = kinematic_residual(qa, qb, qc, qd2, &
                                     xsa_a, xsa_b, xsa_c, ad2, &
                                     qsrc, this%length(n), weight, delt)
      qderv = (residual2 - residual) / dq
      if (qderv > DZERO) then
        delq = -residual / qderv
      else
        delq = DZERO
      end if

      if (qd + delq < DEM30) then
        delq = -qd
      end if

      qd = qd + delq

      call this%sfr_calc_reach_depth(n, qd, dd)
      ad = this%calc_area_wet(n, dd)
      residual_final = kinematic_residual(qa, qb, qc, qd, &
                                          xsa_a, xsa_b, xsa_c, ad, &
                                          qsrc, this%length(n), weight, delt)

      if (abs(delq) < qtol) then ! .and. abs(residual_final) < qtol) then
        exit newton
      end if

    end do newton

    qd = max(qd, DZERO)
    d1 = (dc + dd) * DHALF
    ! if (qd == DZERO) then
    !   d1 = DZERO
    ! else
    !   d1 = (dc + dd) * DHALF
    ! end if
    delh = (d1 - d1old)

    iconverged = 0
    if (i == 1 .and. qd == DZERO) then
      iconverged = 1
    end if
    if (i > 1 .and. abs(delh) < this%dmaxchg) then
      iconverged = 1
    end if
    if (iconverged == 1) then
      exit kinematicpicard
    end if

  end do kinematicpicard

  this%storage(n) = kinematic_storage(qc, qd)

  end procedure sfr_calc_transient

end submodule

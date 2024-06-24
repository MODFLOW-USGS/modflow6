submodule(SfrModule) sm_gwf_sfr_constant
contains

  !> @brief Method for solving for standard reaches
    !!
    !! Method to solve the continuity equation for a SFR package
    !! reach using the standard approach.
    !!
  !<
  module subroutine sfr_calc_constant(this, n, d1, hgwf, qgwf, qd)
    ! -- dummy variables
    class(SfrType) :: this !< SfrType object
    integer(I4B), intent(in) :: n !< reach number
    real(DP), intent(inout) :: d1 !< current reach depth estimate
    real(DP), intent(in) :: hgwf !< head in gw cell
    real(DP), intent(inout) :: qgwf !< reach-aquifer exchange
    real(DP), intent(inout) :: qd !< reach outflow
    ! -- local variables
    real(DP) :: qsrc

    this%stage(n) = this%sstage(n)
    d1 = max(DZERO, this%stage(n) - this%strtop(n))

    call this%sfr_calc_qsource(n, d1, qsrc)

    if (this%sfr_gwf_conn(n) == 1) then
      call this%sfr_calc_qgwf(n, d1, hgwf, qgwf)
      qgwf = -qgwf
    else
      qgwf = DZERO
    end if

    ! -- leakage exceeds inflow
    if (qgwf > qsrc) then
      d1 = DZERO
      call this%sfr_calc_qsource(n, d1, qsrc)
      qgwf = qsrc
    end if

    ! -- set qd
    qd = qsrc - qgwf

  end subroutine sfr_calc_constant

end submodule

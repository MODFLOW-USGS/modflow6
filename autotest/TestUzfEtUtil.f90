module TestUzfEtUtil

  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO, DONE, DTWO, DEM1, DHALF, D1P1, DSIX
  use MathUtilModule, only: is_close
  use testdrive, only: check, error_type, new_unittest, test_failed, &
                       to_string, unittest_type
  use UzfETUtilModule, only: etfunc_lin, calc_lin_scaling_fac

  implicit none
  private
  public :: collect_uzfetutil

contains

  subroutine collect_uzfetutil(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("etfunc_lin", test_etfunc_lin), &
                new_unittest("calc_lin_scaling_fac", &
                             test_calc_lin_scaling_fac) &
                ]
  end subroutine collect_uzfetutil

  subroutine test_calc_lin_scaling_fac(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, calc_lin_scaling_fac(0.25_DP, 1.0_DP, 1.0_DP) == 0.25_DP)
    if (allocated(error)) return
    call check(error, calc_lin_scaling_fac(0.5_DP, 1.0_DP, 1.0_DP) == 0.5_DP)
    if (allocated(error)) return
    call check(error, calc_lin_scaling_fac(1.0_DP, 1.0_DP, 1.0_DP) == 1.0_DP)
    if (allocated(error)) return
    call check(error, calc_lin_scaling_fac(2.0_DP, 1.0_DP, 1.0_DP) == 1.0_DP)
    if (allocated(error)) return
  end subroutine test_calc_lin_scaling_fac

  subroutine test_etfunc_lin(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP) :: rate !< calculated pET rate
    ! local
    real(DP) :: deriv_et !< derivative of gw ET for Newton addition to equations in _fn()
    real(DP) :: extdp !< extinction depth
    real(DP) :: hgwf !< groundwater head
    real(DP) :: pET !< potential evapotranspiration
    real(DP) :: trhs !< total uzf rhs contribution to GWF model
    real(DP) :: thcof !< total uzf hcof contribution to GWF model
    real(DP) :: celtop !< elevation of the top of the cell
    real(DP) :: celbot !< elevation of the bottom of the cell

    ! water table exactly in the middle of the extinction depth
    deriv_et = DZERO
    extdp = DONE
    pET = DEM1
    trhs = DZERO
    thcof = DZERO
    celtop = DTWO
    celbot = DZERO
    hgwf = 1.5_DP
    deriv_et = DZERO
    trhs = DZERO
    thcof = DZERO
    rate = etfunc_lin(celtop, extdp, pET, deriv_et, trhs, thcof, &
                      hgwf, celtop, celbot)
    call check(error, is_close(rate, pET * DHALF))
    if (allocated(error)) return

    ! water table below extdp, should return 0.0
    hgwf = DHALF
    deriv_et = DZERO
    trhs = DZERO
    thcof = DZERO
    rate = etfunc_lin(celtop, extdp, pET, deriv_et, trhs, thcof, &
                      hgwf, celtop, celbot)
    call check(error, is_close(rate, DZERO))
    if (allocated(error)) return

    ! water table at an arbitrary location within the extinction depth interval
    hgwf = D1P1
    deriv_et = DZERO
    trhs = DZERO
    thcof = DZERO
    rate = etfunc_lin(celtop, extdp, pET, deriv_et, trhs, thcof, &
                      hgwf, celtop, celbot)
    call check(error, is_close(rate, &
                               pET * (D1P1 - (celtop - (celtop - extdp)))))
    if (allocated(error)) return

    ! water table at the top of the extinction depth
    hgwf = DTWO
    deriv_et = DZERO
    trhs = DZERO
    thcof = DZERO
    rate = etfunc_lin(celtop, extdp, pET, deriv_et, trhs, thcof, &
                      hgwf, celtop, celbot)
    call check(error, is_close(rate, pET))
    if (allocated(error)) return

    ! water table well above the top of the extinction depth
    hgwf = DSIX
    deriv_et = DZERO
    trhs = DZERO
    thcof = DZERO
    rate = etfunc_lin(celtop, extdp, pET, deriv_et, trhs, thcof, &
                      hgwf, celtop, celbot)
    call check(error, is_close(rate, pET))
    if (allocated(error)) return
  end subroutine test_etfunc_lin

end module TestUzfEtUtil

  function kinematic_residual(qa, qb, qc, qd, &
                              aa, ab, ac, ad, &
                              qsrc, length, weight, delt)
    use KindModule, only: DP
    use ConstantsModule, only: DZERO, DONE, DTWO
    ! -- return variable
    real(DP) :: kinematic_residual !< kinematic-wave residual
    ! -- dummy variables
    real(DP), intent(in) :: qa
    real(DP), intent(in) :: qb
    real(DP), intent(in) :: qc
    real(DP), intent(in) :: qd
    real(DP), intent(in) :: aa
    real(DP), intent(in) :: ab
    real(DP), intent(in) :: ac
    real(DP), intent(in) :: ad
    real(DP), intent(in) :: qsrc
    real(DP), intent(in) :: length
    real(DP), intent(in) :: weight
    real(DP), intent(in) :: delt
    ! --local variables
    real(DP) :: weightinv
    real(DP) :: f11
    real(DP) :: f12

    weightinv = DONE - weight

    f11 = (qd - qc) / length
    f12 = (ad - ac) / delt
    ! if ((qb + qa) == DZERO) then
    !   f11 = (qd - qc) / length
    !   f12 = (ad - ac) / delt
    ! else
    !   f11 = (weight * (qd - qc) + weightinv * (qb - qa)) / length
    !   f12 = ((ad - ab) + (ac - aa)) / (delt * DTWO)
    ! end if
    kinematic_residual = f11 + f12 - qsrc

  end function kinematic_residual


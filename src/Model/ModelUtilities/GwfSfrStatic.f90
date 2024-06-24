  function kinematic_residual(qa, qb, qc, qd, &
                              aa, ab, ac, ad, &
                              qsrc, length, weight, delt)
    use KindModule, only: DP
    use ConstantsModule, only: DZERO, DHALF, DONE, DTWO
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
    real(DP) :: f11
    real(DP) :: f12

    f11 = (weight * (qd - qc) + (DONE - weight) * (qb - qa)) / length
    f12 = DHALF * ((ad - ab) + (ac - aa)) / delt
    kinematic_residual = f11 + f12 - qsrc

  end function kinematic_residual

  function kinematic_storage(qc, qd)
    use KindModule, only: DP
    use ConstantsModule, only: DHALF
    ! -- return variable
    real(DP) :: kinematic_storage !< kinematic-wave storage change
    ! -- dummy variables
    real(DP), intent(in) :: qc
    real(DP), intent(in) :: qd

    kinematic_storage = (qd - qc)

  end function kinematic_storage

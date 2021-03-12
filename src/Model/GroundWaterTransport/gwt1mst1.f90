! -- Mobile Storage and Transfer (MST) Module
!    GwtMstType is responsible for adding the effects of
!      1. Changes in dissolved solute mass
!      2. Decay of dissolved solute mass
!      3. Sorption
!      4. Decay of sorbed solute mass

module GwtMstModule
  
  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DONE, DZERO, DTWO, DHALF, LENBUDTXT
  use SimVariablesModule,     only: errmsg, warnmsg
  use SimModule,              only: ustop, store_error, count_errors,          &
                                    store_warning
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule,          only: DisBaseType
  use GwtFmiModule,           only: GwtFmiType
  
  implicit none
  public :: GwtMstType
  public :: mst_cr
  !
  integer(I4B), parameter :: NBDITEMS = 4
  character(len=LENBUDTXT), dimension(NBDITEMS) :: budtxt
  data budtxt / ' STORAGE-AQUEOUS', '   DECAY-AQUEOUS', &
                '  STORAGE-SORBED', '    DECAY-SORBED' /

  type, extends(NumericalPackageType) :: GwtMstType
    !
    ! -- storage
    real(DP), dimension(:), pointer, contiguous      :: porosity => null()      ! porosity
    real(DP), dimension(:), pointer, contiguous      :: prsity2 => null()       ! sum of immobile porosity
    real(DP), dimension(:), pointer, contiguous      :: ratesto => null()       ! rate of mobile storage
    !
    ! -- decay
    integer(I4B), pointer                            :: idcy => null()          ! order of decay rate (0:none, 1:first, 2:zero)
    real(DP), dimension(:), pointer, contiguous      :: decay => null()         ! first or zero order decay rate (aqueous)
    real(DP), dimension(:), pointer, contiguous      :: decay_sorbed => null()  ! first or zero order decay rate (sorbed)
    real(DP), dimension(:), pointer, contiguous      :: ratedcy => null()       ! rate of decay
    !
    ! -- sorption
    integer(I4B), pointer                            :: isrb => null()          ! sorption active flag (0:off, 1:linear, 2:freundlich, 3:langmuir)
    real(DP), dimension(:), pointer, contiguous      :: bulk_density => null()  ! bulk density
    real(DP), dimension(:), pointer, contiguous      :: distcoef => null()      ! kd distribution coefficient
    real(DP), dimension(:), pointer, contiguous      :: sp2 => null()           ! second sorption parameter
    real(DP), dimension(:), pointer, contiguous      :: ratesrb => null()       ! rate of sorption
    real(DP), dimension(:), pointer, contiguous      :: ratedcys => null()      ! rate of sorbed mass decay
    !
    ! -- misc
    integer(I4B), dimension(:), pointer, contiguous  :: ibound => null()        ! pointer to model ibound
    type(GwtFmiType), pointer                        :: fmi => null()           ! pointer to fmi object

  contains
  
    procedure :: mst_ar
    procedure :: mst_fc
    procedure :: mst_fc_sto
    procedure :: mst_fc_dcy
    procedure :: mst_fc_srb
    procedure :: mst_fc_dcy_srb
    procedure :: mst_cq
    procedure :: mst_cq_sto
    procedure :: mst_cq_dcy
    procedure :: mst_cq_srb
    procedure :: mst_cq_dcy_srb
    procedure :: mst_bd
    procedure :: mst_ot_flow
    procedure :: mst_da
    procedure :: allocate_scalars
    procedure :: addto_prsity2
    procedure :: get_thetamfrac
    procedure :: get_thetaimfrac
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data
  
  end type GwtMstType
  
  contains
  
  subroutine mst_cr(mstobj, name_model, inunit, iout, fmi)
! ******************************************************************************
! mst_cr -- Create a new object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtMstType), pointer :: mstobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwtFmiType), intent(in), target :: fmi
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(mstobj)
    !
    ! -- create name and memory path
    call mstobj%set_names(1, name_model, 'MST', 'MST')
    !
    ! -- Allocate scalars
    call mstobj%allocate_scalars()
    !
    ! -- Set variables
    mstobj%inunit = inunit
    mstobj%iout = iout
    mstobj%fmi => fmi
    !
    ! -- Initialize block parser
    call mstobj%parser%Initialize(mstobj%inunit, mstobj%iout)
    !
    ! -- Return
    return
  end subroutine mst_cr

  subroutine mst_ar(this, dis, ibound)
! ******************************************************************************
! mst_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMstType), intent(inout) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtmst =                                    &
      "(1x,/1x,'MST -- MOBILE STORAGE AND TRANSFER PACKAGE, VERSION 1, &
      &7/29/2020 INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the immobile domain package.
    write(this%iout, fmtmst) this%inunit
    !
    ! -- Read options
    call this%read_options()
    !
    ! -- store pointers to arguments that were passed in
    this%dis     => dis
    this%ibound  => ibound
    !
    ! -- Allocate arrays
    call this%allocate_arrays(dis%nodes)
    !
    ! -- read the data block
    call this%read_data()
    !
    ! -- Return
    return
  end subroutine mst_ar
  
  subroutine mst_fc(this, nodes, cold, nja, njasln, amatsln, idxglo, cnew, rhs)
! ******************************************************************************
! mst_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMstType) :: this
    integer, intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: nja
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(nja) :: idxglo
    real(DP), intent(inout), dimension(nodes) :: rhs
    real(DP), intent(in), dimension(nodes) :: cnew
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- storage contribution
    call this%mst_fc_sto(nodes, cold, nja, njasln, amatsln, idxglo, rhs)
    !
    ! -- decay contribution
    if (this%idcy /= 0) then
      call this%mst_fc_dcy(nodes, cold, nja, njasln, amatsln, idxglo, rhs)
    end if
    !
    ! -- sorption contribution
    if (this%isrb /= 0) then
      call this%mst_fc_srb(nodes, cold, nja, njasln, amatsln, idxglo, rhs, cnew)
    end if
    !
    ! -- decay sorbed contribution
    if (this%isrb /= 0 .and. this%idcy /= 0) then
      call this%mst_fc_dcy_srb(nodes, cold, nja, njasln, amatsln, idxglo, rhs, &
                               cnew)
    end if
    !
    ! -- Return
    return
  end subroutine mst_fc
  
  subroutine mst_fc_sto(this, nodes, cold, nja, njasln, amatsln, idxglo, rhs)
! ******************************************************************************
! mst_fc_sto -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtMstType) :: this
    integer, intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: nja
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(nja) :: idxglo
    real(DP), intent(inout), dimension(nodes) :: rhs
    ! -- local
    integer(I4B) :: n, idiag
    real(DP) :: tled
    real(DP) :: hhcof, rrhs
    real(DP) :: vnew, vold
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- loop through and calculate storage contribution to hcof and rhs
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vnew = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n)) * &
             this%fmi%gwfsat(n) * this%porosity(n)
      vold = vnew
      if (this%fmi%igwfstrgss /= 0) vold = vold + this%fmi%gwfstrgss(n) * delt
      if (this%fmi%igwfstrgsy /= 0) vold = vold + this%fmi%gwfstrgsy(n) * delt
      !
      ! -- add terms to diagonal and rhs accumulators
      hhcof = -vnew * tled
      rrhs = -vold * tled * cold(n)
      idiag = this%dis%con%ia(n)
      amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) + hhcof
      rhs(n) = rhs(n) + rrhs
    enddo
    !
    ! -- Return
    return
  end subroutine mst_fc_sto
  
  subroutine mst_fc_dcy(this, nodes, cold, nja, njasln, amatsln, idxglo, rhs)
! ******************************************************************************
! mst_fc_dcy -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMstType) :: this
    integer, intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: nja
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(nja) :: idxglo
    real(DP), intent(inout), dimension(nodes) :: rhs
    ! -- local
    integer(I4B) :: n, idiag
    real(DP) :: hhcof, rrhs
    real(DP) :: swtpdt
    real(DP) :: vcell
! ------------------------------------------------------------------------------
    !
    ! -- loop through and calculate decay contribution to hcof and rhs
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swtpdt = this%fmi%gwfsat(n)
      !
      ! -- add decay rate terms to accumulators
      if (this%idcy == 1) then
        !
        ! -- first order decay rate is a function of concentration, so add
        !    to left hand side
        hhcof = -this%decay(n) * vcell * swtpdt * this%porosity(n)
        idiag = this%dis%con%ia(n)
        amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) + hhcof
      elseif (this%idcy == 2) then
        !
        ! -- zero-order decay rate is not a function of concentration, so add
        !    to right hand side
        rrhs = this%decay(n) * vcell * swtpdt * this%porosity(n)
        rhs(n) = rhs(n) + rrhs
      endif
      !
    enddo
    !
    ! -- Return
    return
  end subroutine mst_fc_dcy
  
  subroutine mst_fc_srb(this, nodes, cold, nja, njasln, amatsln, idxglo, rhs,  &
                        cnew)
! ******************************************************************************
! mst_fc_srb -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtMstType) :: this
    integer, intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: nja
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(nja) :: idxglo
    real(DP), intent(inout), dimension(nodes) :: rhs
    real(DP), intent(in), dimension(nodes) :: cnew
    ! -- local
    integer(I4B) :: n, idiag
    real(DP) :: tled
    real(DP) :: hhcof, rrhs
    real(DP) :: swt, swtpdt
    real(DP) :: vcell
    real(DP) :: const1
    real(DP) :: const2
    real(DP) :: thetamfrac
    real(DP) :: rhob
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- loop through and calculate sorption contribution to hcof and rhs
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- assign variables
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swtpdt = this%fmi%gwfsat(n)
      swt = this%fmi%gwfsatold(n, delt)
      idiag = this%dis%con%ia(n)
      thetamfrac = this%get_thetamfrac(n)
      const1 = this%distcoef(n)
      const2 = 0.
      if (this%isrb > 1) const2 = this%sp2(n)
      rhob = this%bulk_density(n)
      call mst_srb_term(this%isrb, thetamfrac, rhob, vcell, tled, cnew(n),     &
                        cold(n), swtpdt, swt, const1, const2,                  &
                        hcofval=hhcof, rhsval=rrhs) 
      !
      ! -- Add hhcof to diagonal and rrhs to right-hand side
      amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) + hhcof
      rhs(n) = rhs(n) + rrhs
      !
    enddo
    !
    ! -- Return
    return
  end subroutine mst_fc_srb
  
  subroutine mst_srb_term(isrb, thetamfrac, rhob, vcell, tled, cnew, cold,     &
                          swnew, swold, const1, const2, rate, hcofval, rhsval) 
! ******************************************************************************
! mst_srb_term -- Calculate sorption terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    integer(I4B), intent(in) :: isrb
    real(DP), intent(in) :: thetamfrac
    real(DP), intent(in) :: rhob
    real(DP), intent(in) :: vcell
    real(DP), intent(in) :: tled
    real(DP), intent(in) :: cnew
    real(DP), intent(in) :: cold
    real(DP), intent(in) :: swnew
    real(DP), intent(in) :: swold
    real(DP), intent(in) :: const1
    real(DP), intent(in) :: const2
    real(DP), intent(out), optional :: rate
    real(DP), intent(out), optional :: hcofval
    real(DP), intent(out), optional :: rhsval
    ! -- local
    real(DP) :: term
    real(DP) :: derv
    real(DP) :: cbarnew
    real(DP) :: cbarold
    real(DP) :: cavg
    real(DP) :: cbaravg
! ------------------------------------------------------------------------------
    if (isrb == 1) then
      ! -- linear
      term = - thetamfrac * rhob * vcell * tled * const1
      if (present(hcofval)) hcofval = term * swnew
      if (present(rhsval)) rhsval = term * swold * cold
      if (present(rate)) rate = term * swnew * cnew - term * swold * cold
    else
      if (isrb == 2) then
        ! -- freundlich
        cbarnew = get_freundlich_conc(cnew, const1, const2)
        cbarold = get_freundlich_conc(cold, const1, const2)
        cavg = DHALF * (cold + cnew) 
        derv = get_freundlich_derivative(cavg, const1, const2)
      else if (isrb == 3) then
        ! -- langmuir
        cbarnew = get_langmuir_conc(cnew, const1, const2)
        cbarold = get_langmuir_conc(cold, const1, const2)
        cavg = DHALF * (cold + cnew) 
        derv = get_langmuir_derivative(cavg, const1, const2)
      end if
      !
      ! -- calculate hcof, rhs, and rate for freundlich and langmuir
      term = - thetamfrac * rhob * vcell * tled
      cbaravg = (cbarold + cbarnew) * DHALF
      if (present(hcofval)) then
        hcofval = term * derv * (swnew + swold) * DHALF
      end if
      if (present(rhsval)) then
        rhsval = term * derv * cold - term * cbaravg * (swnew - swold)
      end if
      if (present(rate)) then
        rate = term * derv * (swnew + swold) / DTWO * (cnew - cold) &
               + term * cbaravg * (swnew - swold)
      end if
    end if
    return
  end subroutine mst_srb_term

  function get_freundlich_conc(conc, kf, a) result(cbar)
! ******************************************************************************
! get_freundlich_conc -- Calculate cbar for Freundlich
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    real(DP), intent(in) :: conc
    real(DP), intent(in) :: kf
    real(DP), intent(in) :: a
    real(DP) :: cbar
! ------------------------------------------------------------------------------
    if (conc > DZERO) then
      cbar = kf * conc ** a
    else
      cbar = DZERO
    end if
    return
  end function 
  
  function get_langmuir_conc(conc, kl, sbar) result(cbar)
! ******************************************************************************
! get_langmuir_conc -- Calculate cbar for Langmuir
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    real(DP), intent(in) :: conc
    real(DP), intent(in) :: kl
    real(DP), intent(in) :: sbar
    real(DP) :: cbar
! ------------------------------------------------------------------------------
    if (conc > DZERO) then
      cbar = (kl * sbar * conc) / (DONE + kl * conc)
    else
      cbar = DZERO
    end if
    return
  end function 
  
  function get_freundlich_derivative(conc, kf, a) result(derv)
! ******************************************************************************
! get_freundlich_derivative -- Calculate derivative for Freundlich
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    real(DP), intent(in) :: conc
    real(DP), intent(in) :: kf
    real(DP), intent(in) :: a
    real(DP) :: derv
! ------------------------------------------------------------------------------
    if (conc > DZERO) then
      derv = kf * a * conc ** (a - DONE)
    else
      derv = DZERO
    end if
    return
  end function 
  
  function get_langmuir_derivative(conc, kl, sbar) result(derv)
! ******************************************************************************
! get_langmuir_derivative -- Calculate derivative for Langmuir
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    real(DP), intent(in) :: conc
    real(DP), intent(in) :: kl
    real(DP), intent(in) :: sbar
    real(DP) :: derv
! ------------------------------------------------------------------------------
    if (conc > DZERO) then
      derv = (kl * sbar) / (DONE + kl * conc) ** DTWO
    else
      derv = DZERO
    end if
    return
  end function 
  
  subroutine mst_fc_dcy_srb(this, nodes, cold, nja, njasln, amatsln, idxglo,   &
                            rhs, cnew)
! ******************************************************************************
! mst_fc_dcy_srb -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMstType) :: this
    integer, intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: nja
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(nja) :: idxglo
    real(DP), intent(inout), dimension(nodes) :: rhs
    real(DP), intent(in), dimension(nodes) :: cnew
    ! -- local
    integer(I4B) :: n, idiag
    real(DP) :: hhcof, rrhs
    real(DP) :: vcell
    real(DP) :: swnew
    real(DP) :: distcoef
    real(DP) :: thetamfrac
    real(DP) :: term
    real(DP) :: csrb
! ------------------------------------------------------------------------------
    !
    ! -- loop through and calculate sorption contribution to hcof and rhs
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- set variables
      hhcof = DZERO
      rrhs = DZERO
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swnew =  this%fmi%gwfsat(n)
      distcoef = this%distcoef(n)
      idiag = this%dis%con%ia(n)
      thetamfrac = this%get_thetamfrac(n)
      term = this%decay_sorbed(n) * thetamfrac * this%bulk_density(n) *        &
             swnew * vcell
      !
      ! -- add sorbed mass decay rate terms to accumulators
      if (this%idcy == 1) then
        !
        if (this%isrb == 1) then
          !
          ! -- first order decay rate is a function of concentration, so add
          !    to left hand side
          hhcof = - term * distcoef
        else if (this%isrb == 2) then
          !
          ! -- nonlinear Freundlich sorption, so add to RHS
          csrb = get_freundlich_conc(cnew(n), distcoef, this%sp2(n))
          rrhs = term * csrb
        else if (this%isrb == 3) then
          !
          ! -- nonlinear Lanmuir sorption, so add to RHS
          csrb = get_freundlich_conc(cnew(n), distcoef, this%sp2(n))
          rrhs = term * csrb
        end if
      elseif (this%idcy == 2) then
        !
        ! -- zero-order decay rate is not a function of concentration, so add
        !    to right hand side
        if (distcoef > DZERO) then
          ! -- Add zero order sorption term only if distribution coefficient > 0
          rrhs = term
        end if
      endif
      !
      ! -- Add hhcof to diagonal and rrhs to right-hand side
      amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) + hhcof
      rhs(n) = rhs(n) + rrhs
      !
    enddo
    !
    ! -- Return
    return
  end subroutine mst_fc_dcy_srb
  
  subroutine mst_cq(this, nodes, cnew, cold, flowja)
! ******************************************************************************
! mst_cq -- Calculate mass flow terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMstType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cnew
    real(DP), intent(in), dimension(nodes) :: cold
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! - storage
    call this%mst_cq_sto(nodes, cnew, cold, flowja)
    !
    ! -- decay
    if (this%idcy /= 0) then
      call this%mst_cq_dcy(nodes, cnew, cold, flowja)
    end if
    !
    ! -- sorption
    if (this%isrb /= 0) then
      call this%mst_cq_srb(nodes, cnew, cold, flowja)
    end if
    !
    ! -- decay sorbed
    if (this%isrb /= 0 .and. this%idcy /= 0) then
      call this%mst_cq_dcy_srb(nodes, cnew, cold, flowja)
    end if
    !
    ! -- Return
    return
  end subroutine mst_cq

  subroutine mst_cq_sto(this, nodes, cnew, cold, flowja)
! ******************************************************************************
! mst_cq_sto -- Calculate budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtMstType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cnew
    real(DP), intent(in), dimension(nodes) :: cold
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: rate
    real(DP) :: tled
    real(DP) :: vnew, vold
    real(DP) :: hhcof, rrhs
! ------------------------------------------------------------------------------
    !
    ! -- initialize 
    tled = DONE / delt
    !
    ! -- Calculate storage change
    do n = 1, nodes
      this%ratesto(n) = DZERO
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vnew = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n)) * &
             this%fmi%gwfsat(n) * this%porosity(n)
      vold = vnew
      if (this%fmi%igwfstrgss /= 0) vold = vold + this%fmi%gwfstrgss(n) * delt
      if (this%fmi%igwfstrgsy /= 0) vold = vold + this%fmi%gwfstrgsy(n) * delt
      !
      ! -- calculate rate
      hhcof = -vnew * tled
      rrhs = -vold * tled * cold(n)
      rate = hhcof * cnew(n) - rrhs
      this%ratesto(n) = rate
      idiag = this%dis%con%ia(n)
      flowja(idiag) = flowja(idiag) + rate
    enddo
    !
    ! -- Return
    return
  end subroutine mst_cq_sto

  subroutine mst_cq_dcy(this, nodes, cnew, cold, flowja)
! ******************************************************************************
! mst_cq_dcy -- Calculate budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMstType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cnew
    real(DP), intent(in), dimension(nodes) :: cold
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: rate
    real(DP) :: swtpdt
    real(DP) :: hhcof, rrhs
    real(DP) :: vcell
! ------------------------------------------------------------------------------
    !
    ! -- initialize 
    !
    ! -- Calculate decay change
    do n = 1, nodes
      !
      ! -- skip if transport inactive
      this%ratedcy(n) = DZERO
      if(this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swtpdt = this%fmi%gwfsat(n)
      !
      ! -- calculate decay gains and losses
      rate = DZERO
      hhcof = DZERO
      rrhs = DZERO
      if (this%idcy == 1) then
        hhcof = -this%decay(n) * vcell * swtpdt * this%porosity(n)
      elseif (this%idcy == 2) then
        rrhs = this%decay(n) * vcell * swtpdt * this%porosity(n)
      endif
      rate = hhcof * cnew(n) - rrhs
      this%ratedcy(n) = rate
      idiag = this%dis%con%ia(n)
      flowja(idiag) = flowja(idiag) + rate
      !
    enddo
    !
    ! -- Return
    return
  end subroutine mst_cq_dcy

  subroutine mst_cq_srb(this, nodes, cnew, cold, flowja)
! ******************************************************************************
! mst_cq_srb -- Calculate budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtMstType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cnew
    real(DP), intent(in), dimension(nodes) :: cold
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: rate
    real(DP) :: tled
    real(DP) :: swt, swtpdt
    real(DP) :: vcell
    real(DP) :: rhob
    real(DP) :: const1
    real(DP) :: const2
    real(DP) :: thetamfrac
! ------------------------------------------------------------------------------
    !
    ! -- initialize 
    tled = DONE / delt
    !
    ! -- Calculate sorption change
    do n = 1, nodes
      !
      ! -- initialize rates
      this%ratesrb(n) = DZERO
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- assign variables
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swtpdt = this%fmi%gwfsat(n)
      swt = this%fmi%gwfsatold(n, delt)
      thetamfrac = this%get_thetamfrac(n)
      rhob = this%bulk_density(n)
      const1 = this%distcoef(n)
      const2 = 0.
      if (this%isrb > 1) const2 = this%sp2(n)
      call mst_srb_term(this%isrb, thetamfrac, rhob, vcell, tled, cnew(n),     &
                        cold(n), swtpdt, swt, const1, const2,                  &
                        rate=rate) 
      this%ratesrb(n) = rate
      idiag = this%dis%con%ia(n)
      flowja(idiag) = flowja(idiag) + rate
      !
    enddo
    !
    ! -- Return
    return
  end subroutine mst_cq_srb

  subroutine mst_cq_dcy_srb(this, nodes, cnew, cold, flowja)
! ******************************************************************************
! mst_cq_dcy_srb -- Calculate budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMstType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cnew
    real(DP), intent(in), dimension(nodes) :: cold
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: rate
    real(DP) :: hhcof, rrhs
    real(DP) :: vcell
    real(DP) :: swnew
    real(DP) :: distcoef
    real(DP) :: thetamfrac
    real(DP) :: term
    real(DP) :: csrb
! ------------------------------------------------------------------------------
    !
    ! -- Calculate sorbed decay change
    !    This routine will only be called if sorption and decay are active
    do n = 1, nodes
      !
      ! -- initialize rates
      this%ratedcys(n) = DZERO
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- set variables
      hhcof = DZERO
      rrhs = DZERO
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swnew =  this%fmi%gwfsat(n)
      distcoef = this%distcoef(n)
      thetamfrac = this%get_thetamfrac(n)
      term = this%decay_sorbed(n) * thetamfrac * this%bulk_density(n) *        &
             swnew * vcell
      !
      ! -- add sorbed mass decay rate terms to accumulators
      if (this%idcy == 1) then
        !
        if (this%isrb == 1) then
          !
          ! -- first order decay rate is a function of concentration, so add
          !    to left hand side
          hhcof = - term * distcoef
        else if (this%isrb == 2) then
          !
          ! -- nonlinear Freundlich sorption, so add to RHS
          csrb = get_freundlich_conc(cnew(n), distcoef, this%sp2(n))
          rrhs = term * csrb
        else if (this%isrb == 3) then
          !
          ! -- nonlinear Lanmuir sorption, so add to RHS
          csrb = get_freundlich_conc(cnew(n), distcoef, this%sp2(n))
          rrhs = term * csrb
        end if
      elseif (this%idcy == 2) then
        !
        ! -- zero-order decay rate is not a function of concentration, so add
        !    to right hand side
        if (distcoef > DZERO) then
          ! -- Add zero order sorption term only if distribution coefficient > 0
          rrhs = term
        end if
      endif
      !
      ! -- calculate rate
      rate = hhcof * cnew(n) - rrhs
      this%ratedcys(n) = rate
      idiag = this%dis%con%ia(n)
      flowja(idiag) = flowja(idiag) + rate
      !
    enddo
    !
    ! -- Return
    return
  end subroutine mst_cq_dcy_srb

  subroutine mst_bd(this, isuppress_output, model_budget)
! ******************************************************************************
! mst_bd -- Calculate budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    ! -- dummy
    class(GwtMstType) :: this
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    ! -- local
    real(DP) :: rin
    real(DP) :: rout
! ------------------------------------------------------------------------------
    !
    ! -- sto
    call rate_accumulator(this%ratesto, rin, rout)
    call model_budget%addentry(rin, rout, delt, budtxt(1),                     &
                               isuppress_output, rowlabel=this%packName)
    !
    ! -- dcy
    if (this%idcy /= 0) then
      call rate_accumulator(this%ratedcy, rin, rout)
      call model_budget%addentry(rin, rout, delt, budtxt(1),                   &
                               isuppress_output, rowlabel=this%packName)
    end if
    !
    ! -- srb
    if (this%isrb /= 0) then
      call rate_accumulator(this%ratesrb, rin, rout)
      call model_budget%addentry(rin, rout, delt, budtxt(2),                   &
                               isuppress_output, rowlabel=this%packName)
    end if
    !
    ! -- srb dcy
    if (this%isrb /= 0 .and. this%idcy /= 0) then
      call rate_accumulator(this%ratedcys, rin, rout)
      call model_budget%addentry(rin, rout, delt, budtxt(3),                   &
                               isuppress_output, rowlabel=this%packName)
    end if
    !
    ! -- Return
    return
  end subroutine mst_bd
  
  subroutine mst_ot_flow(this, icbcfl, icbcun)
! ******************************************************************************
! mst_ot_flow -- Save budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtMstType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    integer(I4B) :: ibinun
    !character(len=16), dimension(2) :: aname
    integer(I4B) :: iprint, nvaluesp, nwidthp
    character(len=1) :: cdatafmp=' ', editdesc=' '
    real(DP) :: dinact
! ------------------------------------------------------------------------------
    !
    ! -- Set unit number for binary output
    if(this%ipakcb < 0) then
      ibinun = icbcun
    elseif(this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    endif
    if(icbcfl == 0) ibinun = 0
    !
    ! -- Record the storage rate if requested
    if(ibinun /= 0) then
      iprint = 0
      dinact = DZERO
      !
      ! -- sto
      call this%dis%record_array(this%ratesto, this%iout, iprint, -ibinun,     &
                                 budtxt(1), cdatafmp, nvaluesp,                &
                                 nwidthp, editdesc, dinact)
      !
      ! -- dcy
      if (this%idcy /= 0) &
      call this%dis%record_array(this%ratedcy, this%iout, iprint, -ibinun,     &
                                 budtxt(2), cdatafmp, nvaluesp,                &
                                 nwidthp, editdesc, dinact)
      !
      ! -- srb
      if (this%isrb /= 0) &
      call this%dis%record_array(this%ratesrb, this%iout, iprint, -ibinun,     &
                                 budtxt(3), cdatafmp, nvaluesp,                &
                                 nwidthp, editdesc, dinact)
      !
      ! -- dcy srb
      if (this%isrb /= 0 .and. this%idcy /= 0) &
      call this%dis%record_array(this%ratedcys, this%iout, iprint, -ibinun,    &
                                 budtxt(4), cdatafmp, nvaluesp,                &
                                 nwidthp, editdesc, dinact)
    endif
    !
    ! -- Return
    return
  end subroutine mst_ot_flow

  subroutine mst_da(this)
! ******************************************************************************
! mst_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtMstType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if(this%inunit > 0) then
      call mem_deallocate(this%porosity)
      call mem_deallocate(this%prsity2)
      call mem_deallocate(this%ratesto)
      call mem_deallocate(this%idcy)
      call mem_deallocate(this%decay)
      call mem_deallocate(this%decay_sorbed)
      call mem_deallocate(this%ratedcy)
      call mem_deallocate(this%isrb)
      call mem_deallocate(this%bulk_density)
      call mem_deallocate(this%distcoef)
      call mem_deallocate(this%sp2)
      call mem_deallocate(this%ratesrb)
      call mem_deallocate(this%ratedcys)
      this%ibound => null()
      this%fmi => null()
    endif
    !
    ! -- Scalars
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine mst_da

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(GwtMstType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%isrb, 'ISRB', this%memoryPath)
    call mem_allocate(this%idcy, 'IDCY', this%memoryPath)
    !
    ! -- Initialize
    this%isrb = 0
    this%idcy = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this, nodes)
! ******************************************************************************
! allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate
    !modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtMstType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Allocate
    ! -- sto
    call mem_allocate(this%porosity, nodes, 'POROSITY', this%memoryPath)
    call mem_allocate(this%prsity2, nodes, 'PRSITY2', this%memoryPath)
    call mem_allocate(this%ratesto, nodes, 'RATESTO', this%memoryPath)
    !
    ! -- dcy
    if (this%idcy == 0) then
      call mem_allocate(this%ratedcy, 1, 'RATEDCY', this%memoryPath)
      call mem_allocate(this%decay, 1, 'DECAY', this%memoryPath)
    else
      call mem_allocate(this%ratedcy, this%dis%nodes, 'RATEDCY', this%memoryPath)
      call mem_allocate(this%decay, nodes, 'DECAY', this%memoryPath)
    end if
    if (this%idcy /= 0 .and. this%isrb /= 0) then
        call mem_allocate(this%ratedcys, this%dis%nodes, 'RATEDCYS',           &
                          this%memoryPath)
    else
        call mem_allocate(this%ratedcys, 1, 'RATEDCYS', this%memoryPath)
    endif
    call mem_allocate(this%decay_sorbed, 1, 'DECAY_SORBED',                    &
                      this%memoryPath)
    !
    ! -- srb
    if (this%isrb == 0) then
      call mem_allocate(this%bulk_density, 1, 'BULK_DENSITY', this%memoryPath)
      call mem_allocate(this%sp2, 1, 'SP2', this%memoryPath)
      call mem_allocate(this%distcoef,  1, 'DISTCOEF', this%memoryPath)
      call mem_allocate(this%ratesrb, 1, 'RATESRB', this%memoryPath)
    else
      call mem_allocate(this%bulk_density, nodes, 'BULK_DENSITY', this%memoryPath)
      call mem_allocate(this%distcoef,  nodes, 'DISTCOEF', this%memoryPath)
      call mem_allocate(this%ratesrb, nodes, 'RATESRB', this%memoryPath)
      if (this%isrb == 1) then
        call mem_allocate(this%sp2, 1, 'SP2', this%memoryPath)
      else
        call mem_allocate(this%sp2, nodes, 'SP2', this%memoryPath)
      end if
    end if
    !
    ! -- Initialize
    do n = 1, nodes
      this%porosity(n) = DZERO
      this%prsity2(n) = DZERO
      this%ratesto(n) = DZERO
    enddo
    do n = 1, size(this%decay)
      this%decay(n) = DZERO
      this%ratedcy(n) = DZERO
    end do
    do n = 1, size(this%bulk_density)
      this%bulk_density(n) = DZERO
      this%distcoef(n) = DZERO
      this%ratesrb(n) = DZERO
    end do
    do n = 1, size(this%sp2)
      this%sp2(n) = DZERO
    end do
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule,   only: LINELENGTH
    ! -- dummy
    class(GwtMstType) :: this
    ! -- local
    character(len=LINELENGTH) :: keyword, keyword2
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtisvflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //    &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtisrb =                                   &
      "(4x,'LINEAR SORPTION IS ACTIVE. ')"
    character(len=*), parameter :: fmtfreundlich =                             &
      "(4x,'FREUNDLICH SORPTION IS ACTIVE. ')"
    character(len=*), parameter :: fmtlangmuir =                               &
      "(4x,'LANGMUIR SORPTION IS ACTIVE. ')"
    character(len=*), parameter :: fmtidcy1 =                                  &
      "(4x,'FIRST-ORDER DECAY IS ACTIVE. ')"
    character(len=*), parameter :: fmtidcy2 =                                  &
      "(4x,'ZERO-ORDER DECAY IS ACTIVE. ')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false., &
                              supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)') 'PROCESSING MOBILE STORAGE AND TRANSFER OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtisvflow)
          case ('SORBTION', 'SORPTION')
            this%isrb = 1
            call this%parser%GetStringCaps(keyword2)
            if (trim(adjustl(keyword2)) == 'LINEAR') this%isrb = 1
            if (trim(adjustl(keyword2)) == 'FREUNDLICH') this%isrb = 2
            if (trim(adjustl(keyword2)) == 'LANGMUIR') this%isrb = 3
            select case (this%isrb)
            case(1)
              write(this%iout, fmtisrb)
            case(2)
              write(this%iout, fmtfreundlich)
            case(3)
              write(this%iout, fmtlangmuir)
            end select
          case ('FIRST_ORDER_DECAY')
            this%idcy = 1
            write(this%iout, fmtidcy1)
          case ('ZERO_ORDER_DECAY')
            this%idcy = 2
            write(this%iout, fmtidcy2)
          case default
            write(errmsg,'(a,a)') 'UNKNOWN MST OPTION: ', trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)') 'END OF MOBILE STORAGE AND TRANSFER OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_data(this)
! ******************************************************************************
! read_data -- read the immodbile domain (griddata) block
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule,   only: LINELENGTH
    use MemoryManagerModule, only: mem_reallocate, mem_reassignptr
    ! -- dummy
    class(GwtMstType) :: this
    ! -- local
    character(len=LINELENGTH) :: keyword
    character(len=:), allocatable :: line
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    logical, dimension(6) :: lname
    character(len=24), dimension(6) :: aname
    ! -- formats
    ! -- data
    data aname(1) /'  MOBILE DOMAIN POROSITY'/
    data aname(2) /'            BULK DENSITY'/
    data aname(3) /'DISTRIBUTION COEFFICIENT'/
    data aname(4) /'              DECAY RATE'/
    data aname(5) /'       DECAY SORBED RATE'/
    data aname(6) /'   SECOND SORPTION PARAM'/
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    isfound = .false.
    lname(:) = .false.
    !
    ! -- get griddata block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    if(isfound) then
      write(this%iout,'(1x,a)')'PROCESSING GRIDDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        call this%parser%GetRemainingLine(line)
        lloc = 1
        select case (keyword)
          case ('POROSITY')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%porosity,  &
                                         aname(1))
            lname(1) = .true.
          case ('BULK_DENSITY')
            if (this%isrb == 0) &
              call mem_reallocate(this%bulk_density, this%dis%nodes,           &
                                  'BULK_DENSITY', trim(this%memoryPath))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive,                 &
                                         this%bulk_density, aname(2))
            lname(2) = .true.
          case ('DISTCOEF')
            if (this%isrb == 0) &
              call mem_reallocate(this%distcoef, this%dis%nodes, 'DISTCOEF',   &
                                trim(this%memoryPath))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%distcoef,  &
                                         aname(3))
            lname(3) = .true.
          case ('DECAY')
            if (this%idcy == 0) &
              call mem_reallocate(this%decay, this%dis%nodes, 'DECAY',         &
                                 trim(this%memoryPath))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%decay,     &
                                         aname(4))
            lname(4) = .true.
          case ('DECAY_SORBED')
            call mem_reallocate(this%decay_sorbed, this%dis%nodes,             &
                                'DECAY_SORBED', trim(this%memoryPath))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive,                 &
                                         this%decay_sorbed, aname(5))
            lname(5) = .true.
          case ('SP2')
            if (this%isrb < 2) &
              call mem_reallocate(this%sp2, this%dis%nodes, 'SP2',             &
                                trim(this%memoryPath))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%sp2,       &
                                         aname(6))
            lname(6) = .true.
          case default
            write(errmsg,'(a,a)') 'UNKNOWN GRIDDATA TAG: ', trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)') 'END PROCESSING GRIDDATA'
    else
      write(errmsg,'(a)') 'REQUIRED GRIDDATA BLOCK NOT FOUND.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Check for rquired porosity
    if(.not. lname(1)) then
      write(errmsg, '(a)') 'POROSITY NOT SPECIFIED IN GRIDDATA BLOCK.'
      call store_error(errmsg)
    end if
    !
    ! -- Check for required sorption variables
    if (this%isrb > 0) then
      if (.not. lname(2)) then
        write(errmsg, '(a)') 'SORPTION IS ACTIVE BUT BULK_DENSITY &
          &NOT SPECIFIED.  BULK_DENSITY MUST BE SPECIFIED IN GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
      if (.not. lname(3)) then
        write(errmsg, '(a)') 'SORPTION IS ACTIVE BUT DISTRIBUTION &
          &COEFFICIENT NOT SPECIFIED.  DISTCOEF MUST BE SPECIFIED IN &
          &GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
      if (this%isrb > 1) then
        if (.not. lname(6)) then
          write(errmsg, '(a)') 'FREUNDLICH OR LANGMUIR SORPTION IS ACTIVE &
            &BUT SP2 NOT SPECIFIED.  SP2 MUST BE SPECIFIED IN &
            &GRIDDATA BLOCK.'
          call store_error(errmsg)
        end if
      end if
    else
      if (lname(2)) then
        write(warnmsg, '(a)') 'SORPTION IS NOT ACTIVE BUT &
          &BULK_DENSITY WAS SPECIFIED.  BULK_DENSITY WILL HAVE NO AFFECT ON &
          &SIMULATION RESULTS.'
        call store_warning(warnmsg)
        write(this%iout, '(1x,a)') 'WARNING.  ' // warnmsg
      endif
      if (lname(3)) then
        write(warnmsg, '(a)') 'SORPTION IS NOT ACTIVE BUT &
          &DISTRIBUTION COEFFICIENT WAS SPECIFIED.  DISTCOEF WILL HAVE &
          &NO AFFECT ON SIMULATION RESULTS.'
        call store_warning(warnmsg)
        write(this%iout, '(1x,a)') 'WARNING.  ' // warnmsg
      endif
      if (lname(6)) then
        write(warnmsg, '(a)') 'SORPTION IS NOT ACTIVE BUT &
          &SP2 WAS SPECIFIED.  SP2 WILL HAVE &
          &NO AFFECT ON SIMULATION RESULTS.'
        call store_warning(warnmsg)
        write(this%iout, '(1x,a)') 'WARNING.  ' // warnmsg
      endif
    endif
    !
    ! -- Check for required decay/production rate coefficients
    if (this%idcy > 0) then
      if (.not. lname(4)) then
        write(errmsg, '(a)') 'FIRST OR ZERO ORDER DECAY IS &
          &ACTIVE BUT THE FIRST RATE COEFFICIENT IS NOT SPECIFIED.  DECAY &
          &MUST BE SPECIFIED IN GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
      if (.not. lname(5)) then
        !
        ! -- If DECAY_SORBED not specified and sorption is active, then
        !    terminate with an error
        if (this%isrb > 0) then
          write(errmsg, '(a)') 'DECAY_SORBED not provided in GRIDDATA &
            &block but decay and sorption are active.  Specify DECAY_SORBED &
            &in GRIDDATA block.'
          call store_error(errmsg)
        endif
      endif
    else
      if (lname(4)) then
        write(warnmsg, '(a)') 'FIRST OR ZERO ORER DECAY &
          &IS NOT ACTIVE BUT DECAY WAS SPECIFIED.  DECAY WILL &
          &HAVE NO AFFECT ON SIMULATION RESULTS.'
        call store_warning(warnmsg)
        write(this%iout, '(1x,a)') 'WARNING.  ' // warnmsg
      endif
      if (lname(5)) then
        write(warnmsg, '(a)') 'FIRST OR ZERO ORER DECAY &
          &IS NOT ACTIVE BUT DECAY_SORBED WAS SPECIFIED.  &
          &DECAY_SORBED WILL HAVE NO AFFECT ON SIMULATION RESULTS.'
        call store_warning(warnmsg)
        write(this%iout, '(1x,a)') 'WARNING.  ' // warnmsg
      endif
    endif
    !
    ! -- terminate if errors
    if(count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine read_data

  subroutine addto_prsity2(this, thetaim)
! ******************************************************************************
! Add immobile porosity to prsity2
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMstType) :: this
    real(DP), dimension(:), intent(in) :: thetaim
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Add to prsity2
    do n = 1, this%dis%nodes
      if (this%ibound(n) == 0) cycle
      this%prsity2(n) = this%prsity2(n) + thetaim(n)
    end do
    !
    ! -- Return
    return
  end subroutine addto_prsity2

  function get_thetamfrac(this, node) result(thetamfrac)
! ******************************************************************************
! Calculate and return the fraction of the total porosity that is mobile
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMstType) :: this
    integer(I4B), intent(in) :: node
    ! -- return
    real(DP) :: thetamfrac
! ------------------------------------------------------------------------------
    !
    thetamfrac = this%porosity(node) / &
                 (this%porosity(node) + this%prsity2(node))
    !
    ! -- Return
    return
  end function get_thetamfrac
  
  function get_thetaimfrac(this, node, thetaim) result(thetaimfrac)
! ******************************************************************************
! Calculate and return the fraction of the total porosity that is immobile
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMstType) :: this
    integer(I4B), intent(in) :: node
    real(DP), intent(in) :: thetaim
    ! -- return
    real(DP) :: thetaimfrac
! ------------------------------------------------------------------------------
    !
    thetaimfrac = thetaim / &
                 (this%porosity(node) + this%prsity2(node))
    !
    ! -- Return
    return
  end function get_thetaimfrac
  
  
  
end module GwtMstModule
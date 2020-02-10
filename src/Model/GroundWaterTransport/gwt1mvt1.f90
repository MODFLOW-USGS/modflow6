module GwtMvtModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO
  use BaseDisModule, only: DisBaseType
  use NumericalPackageModule, only: NumericalPackageType
  use GwtFmiModule, only: GwtFmiType

  implicit none
  private
  public :: GwtMvtType
  public :: mvt_cr
  
  type, extends(NumericalPackageType) :: GwtMvtType
    type(GwtFmiType), pointer                          :: fmi => null()         ! pointer to fmi object
  contains
    procedure :: mvt_fc
    procedure :: mvt_cc
    procedure :: mvt_da
  end type GwtMvtType

  contains

  subroutine mvt_cr(mvt, name_model, inunit, iout, fmi)
! ******************************************************************************
! mvt_cr -- Create a new initial conditions object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtMvtType), pointer :: mvt
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwtFmiType), intent(in), target :: fmi
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(mvt)
    !
    ! -- create name and origin
    call mvt%set_names(1, name_model, 'MVT', 'MVT')
    !
    ! -- Allocate scalars
    call mvt%allocate_scalars()
    !
    mvt%inunit = inunit
    mvt%iout = iout
    !
    ! -- set pointers
    mvt%fmi => fmi
    !
    ! -- Return
    return
  end subroutine mvt_cr

  subroutine mvt_fc(this, nodes, cold, nja, njasln, amatsln, idxglo, rhs)
! ******************************************************************************
! mvt_fc -- Calculate coefficients and fill amat and rhs
!
!   The mvt package adds the mass flow rate to the provider qmfrommvr
!   array.  The advanced packages know enough to subract any mass that is
!   leaving, so the mvt just adds mass coming in from elsewhere.  Because the
!   movers change change by stress period, their solute effects must be
!   added to the right-hand side of the gwt matrix equations.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMvtType) :: this
    integer, intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: nja
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(nja) :: idxglo
    real(DP), intent(inout), dimension(nodes) :: rhs
    ! -- local
    integer(I4B) :: i, n
    integer(I4B) :: id1, id2, nlist
    integer(I4B) :: ipr, irc
    real(DP) :: q, cp
! ------------------------------------------------------------------------------
    !
    ! -- initialize the mass flow into advanced package from the mover
    do i = 1, this%fmi%nflowpack
      if (this%fmi%iatp(i) == 0) cycle
      do n = 1, size(this%fmi%datp(i)%qmfrommvr)
        this%fmi%datp(i)%qmfrommvr(n) = DZERO
      end do
    end do
    !
    ! -- Add mover terms?
    if (associated(this%fmi%mvrbudobj)) then
      
      do i = 1, this%fmi%mvrbudobj%nbudterm
        nlist = this%fmi%mvrbudobj%budterm(i)%nlist
        if (nlist > 0) then
          call this%fmi%get_package_index(this%fmi%mvrbudobj%budterm(i)%text2id1, ipr)
          call this%fmi%get_package_index(this%fmi%mvrbudobj%budterm(i)%text2id2, irc)
          do n = 1, nlist
            !
            ! -- lak/sfr/maw/uzf id1 (provider) and id2 (receiver)
            id1 = this%fmi%mvrbudobj%budterm(i)%id1(n)
            id2 = this%fmi%mvrbudobj%budterm(i)%id2(n)
            !
            ! -- mover flow rate
            q = this%fmi%mvrbudobj%budterm(i)%flow(n)
            !
            ! -- concentration of the provider
            cp = DZERO
            if (this%fmi%iatp(ipr) /= 0) then
              cp = this%fmi%datp(ipr)%concpack(id1)
            else
              ! check for aux?
            end if
            !
            ! -- add the mover rate times the provider concentration into the receiver
            !    make sure these are accumulated since multiple providers can move
            !    water into the same receiver
            if (this%fmi%iatp(irc) /= 0) then
              this%fmi%datp(irc)%qmfrommvr(id2) = this%fmi%datp(irc)%qmfrommvr(id2) - q * cp
            end if
          end do
        end if
      end do
      
    end if
    !
    ! -- Return
    return
  end subroutine mvt_fc

  subroutine mvt_cc(this, kiter, iend, icnvg)
! ******************************************************************************
! mvt_cc -- extra convergence check for mover
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtMvtType) :: this
    integer(I4B),intent(in) :: kiter
    integer(I4B),intent(in) :: iend
    integer(I4B),intent(inout) :: icnvg
    ! -- local
    ! -- formats
    character(len=*),parameter :: fmtmvrcnvg = &
      "(/,1x,'MOVER PACKAGE REQUIRES AT LEAST TWO OUTER ITERATIONS. CONVERGE &
      &FLAG HAS BEEN RESET TO FALSE.')"
! ------------------------------------------------------------------------------
    !
    ! -- If there are active movers, then at least 2 outers required
    if (associated(this%fmi%mvrbudobj)) then
      if (icnvg == 1 .and. kiter == 1) then
        icnvg = 0
        write(this%iout, fmtmvrcnvg)
      endif
    endif
    !
    ! -- return
    return
  end subroutine mvt_cc
  
  subroutine mvt_bd()
    ! -- todo: this needs to be added here.  Should create a new budget object?
  end subroutine mvt_bd
  
  subroutine mvt_da(this)
! ******************************************************************************
! mvt_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtMvtType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Scalars
    this%fmi => null()
    !
    ! -- deallocate scalars in NumericalPackageType
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine mvt_da

end module GwtMvtModule
  
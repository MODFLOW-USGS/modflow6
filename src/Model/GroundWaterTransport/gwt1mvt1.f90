module GwtMvtModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, MAXCHARLEN, DZERO, LENPAKLOC, DNODATA
  use SimModule, only: store_error, ustop
  use BaseDisModule, only: DisBaseType
  use NumericalPackageModule, only: NumericalPackageType
  use GwtFmiModule, only: GwtFmiType

  implicit none
  private
  public :: GwtMvtType
  public :: mvt_cr
  
  type, extends(NumericalPackageType) :: GwtMvtType
    type(GwtFmiType), pointer                          :: fmi => null()         ! pointer to fmi object
    integer(I4B), pointer                              :: ibudgetout => null()  ! unit number for budget output file
  contains
    procedure :: mvt_df
    procedure :: mvt_rp
    procedure :: mvt_fc
    procedure :: mvt_cc
    procedure :: mvt_bd
    procedure :: mvt_da
    procedure :: allocate_scalars
    procedure :: read_options
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
    ! -- Initialize block parser
    call mvt%parser%Initialize(inunit, iout)
    !
    ! -- Return
    return
  end subroutine mvt_cr

  subroutine mvt_df(this)
! ******************************************************************************
! mvt_df -- Define
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMvtType) :: this
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtmvt =                                    &
      "(1x,/1x,'MVT -- MOVER TRANSPORT PACKAGE, VERSION 1, 4/15/2020',         &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the MVT package.
    write(this%iout, fmtmvt) this%inunit
    !
    ! -- Read mvt options
    call this%read_options()
    !
    ! -- Return
    return
  end subroutine mvt_df
  
  subroutine mvt_rp(this)
! ******************************************************************************
! mvt_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kper, nper
    ! -- dummy
    class(GwtMvtType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! --Check to make sure mover flows are available, otherwise terminate with
    !   error.  This cannot be checked until RP because exchange doesn't set
    !   a pointer to mvrbudobj until exg_ar().
    if (kper * nper == 1) then
      if (.not. associated(this%fmi%mvrbudobj)) then
        write(errmsg,'(4x,a)') 'GWF WATER MOVER TERMS ARE NOT AVAILABLE &
          &TO THE GWT MVT PACKAGE.  ACTIVATE GWF-GWT EXCHANGE OR SPECIFY &
          &GWFMOVER IN FMI PACKAGEDATA.'
        call store_error(errmsg)
        call ustop()
      end if
    end if
    !
    ! -- Return
    return
  end subroutine mvt_rp
  
  subroutine mvt_fc(this, nodes, cold, nja, njasln, amatsln, idxglo, cnew, rhs)
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
    real(DP), intent(in), dimension(nodes) :: cnew
    real(DP), intent(inout), dimension(nodes) :: rhs
    ! -- local
    integer(I4B) :: i, n
    integer(I4B) :: id1, id2, nlist
    integer(I4B) :: ipr, irc
    integer(I4B) :: igwtnode
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
            ! -- Must be a regular stress package
            igwtnode = this%fmi%gwfpackages(ipr)%nodelist(id1)
            cp = cnew(igwtnode)
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
    !
    ! -- Return
    return
  end subroutine mvt_fc

  subroutine mvt_cc(this, kiter, iend, icnvgmod, cpak, dpak)
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
    integer(I4B),intent(in) :: icnvgmod
    character(len=LENPAKLOC), intent(inout) :: cpak
    real(DP), intent(inout) :: dpak
    ! -- local
    ! -- formats
    character(len=*),parameter :: fmtmvrcnvg = &
      "(/,1x,'MOVER PACKAGE REQUIRES AT LEAST TWO OUTER ITERATIONS. CONVERGE &
      &FLAG HAS BEEN RESET TO FALSE.')"
! ------------------------------------------------------------------------------
    !
    ! -- If there are active movers, then at least 2 outers required
    if (associated(this%fmi%mvrbudobj)) then
      if (icnvgmod == 1 .and. kiter == 1) then
        dpak = DNODATA
        cpak = trim(this%name)
        write(this%iout, fmtmvrcnvg)
      endif
    endif
    !
    ! -- return
    return
  end subroutine mvt_cc
  
  subroutine mvt_bd(this, icbcfl, ibudfl, isuppress_output, cnew)
! ******************************************************************************
! mvt_bd -- Write mover terms to listing file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(GwtMvtType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: isuppress_output
    real(DP), intent(in), dimension(:) :: cnew
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: nlist
    integer(I4B) :: id1
    integer(I4B) :: id2
    integer(I4B) :: ipr
    integer(I4B) :: irc
    integer(I4B) :: igwtnode
    real(DP) :: q
    real(DP) :: cp
    real(DP) :: rate
    ! -- formats
    character(len=*), parameter :: fmttkk = &
      "(1X,/1X,A,'   PERIOD ',I0,'   STEP ',I0)"
    character(len=*), parameter :: fmtmvt = &
      "(1x, a, ' ID ', i0, ' PROVIDED ', 1(1pg15.6), ' TO ', a, ' ID ', i0)"
! ------------------------------------------------------------------------------
    !
    ! -- Write header to indicate MVT summary will follow
    if(ibudfl /= 0 .and. this%iprflow == 1 .and. isuppress_output == 0) then
      write(this%iout, fmttkk) '     MVT SUMMARY', kper, kstp
    
      do i = 1, this%fmi%mvrbudobj%nbudterm
        nlist = this%fmi%mvrbudobj%budterm(i)%nlist
        if (nlist > 0) then
          call this%fmi%get_package_index(this%fmi%mvrbudobj%budterm(i)%text2id1, ipr)
          call this%fmi%get_package_index(this%fmi%mvrbudobj%budterm(i)%text2id2, irc)
          do n = 1, nlist
            !
            ! -- lak/sfr/maw/uzf id1 (provider) and id2 (receiver)
            ! -- todo: these might not be right when the provider is DRN/RIV/GHB
            !    because we need to know the model node number to get the concentration
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
              ! -- Must be a regular stress package
              ! -- todo: what about advanced package but not using SFT/LKT/MWT/UZT?
              igwtnode = this%fmi%gwfpackages(ipr)%nodelist(id1)
              cp = cnew(igwtnode)
            end if
            !
            ! -- Calculate solute mover rate
            if (this%fmi%iatp(irc) /= 0) then
              rate = -q * cp
              write(this%iout, fmtmvt) &
                this%fmi%mvrbudobj%budterm(i)%text2id1, id1, rate, &
                this%fmi%mvrbudobj%budterm(i)%text2id2, id2
            end if
          end do
        end if
      end do
    
    end if
    !
    ! -- return
    return
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
    call mem_deallocate(this%ibudgetout)
    !
    ! -- deallocate scalars in NumericalPackageType
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine mvt_da

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
    class(GwtMvtType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%ibudgetout, 'IBUDGETOUT', this%origin)
    !
    ! -- Initialize
    this%ibudgetout = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read Options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use OpenSpecModule, only: access, form
    use InputOutputModule, only: getunit, openfile
    ! -- dummy
    class(GwtMvtType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    character(len=MAXCHARLEN) :: fname
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    character(len=*),parameter :: fmtflow = &
      "(4x, a, 1x, a, 1x, ' WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
    character(len=*),parameter :: fmtflow2 = &
      "(4x, 'FLOWS WILL BE SAVED TO BUDGET FILE')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING MVT OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtflow2)
          case ('PRINT_INPUT')
            this%iprpak = 1
            write(this%iout,'(4x,a)') 'MVT INPUT WILL BE PRINTED.'
          case ('PRINT_FLOWS')
            this%iprflow = 1
            write(this%iout,'(4x,a)') &
              'MVT FLOWS WILL BE PRINTED TO LISTING FILE.'
          case('BUDGET')
            call this%parser%GetStringCaps(keyword)
            if (keyword == 'FILEOUT') then
              call this%parser%GetString(fname)
              this%ibudgetout = getunit()
              call openfile(this%ibudgetout, this%iout, fname, 'DATA(BINARY)',  &
                            form, access, 'REPLACE')
              write(this%iout,fmtflow) 'MVT', 'BUDGET', fname, this%ibudgetout
            else
              call store_error('OPTIONAL BUDGET KEYWORD MUST BE FOLLOWED BY FILEOUT')
            end if
          case default
            write(errmsg,'(4x,a,a)')'***ERROR. UNKNOWN MVT OPTION: ', &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)') 'END OF MVT OPTIONS'
    end if
    !
    ! -- return
    return
  end subroutine read_options

end module GwtMvtModule
  
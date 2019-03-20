module WelModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DEM1, DONE, LENFTYPE
  use BndModule, only: BndType
  use ObsModule, only: DefaultObsIdProcessor
  use SmoothingModule,  only: sQSaturation, sQSaturationDerivative
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use BlockParserModule, only: BlockParserType
  !
  implicit none
  !
  private
  public :: wel_create
  !
  character(len=LENFTYPE) :: ftype = 'WEL'
  character(len=16)       :: text  = '             WEL'
  !
  type, extends(BndType) :: WelType
    integer(I4B), pointer :: iflowred => null()
    real(DP), pointer :: flowred => null()
  contains
    procedure :: allocate_scalars => wel_allocate_scalars
    procedure :: bnd_options => wel_options
    procedure :: bnd_cf => wel_cf
    procedure :: bnd_fc => wel_fc
    procedure :: bnd_fn => wel_fn
    procedure :: bnd_da => wel_da
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => wel_obs_supported
    procedure, public :: bnd_df_obs => wel_df_obs
    ! -- methods for time series
    procedure, public :: bnd_rp_ts => wel_rp_ts
  end type weltype

contains

  subroutine wel_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! wel_create -- Create a New Well Package
! Subroutine: (1) create new-style package
!             (2) point bndobj to the new package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B),intent(in) :: id
    integer(I4B),intent(in) :: ibcnum
    integer(I4B),intent(in) :: inunit
    integer(I4B),intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    ! -- local
    type(WelType), pointer :: welobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(welobj)
    packobj => welobj
    !
    ! -- create name and origin
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call welobj%allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()

    packobj%inunit=inunit
    packobj%iout=iout
    packobj%id=id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd=1
    packobj%iscloc=1
    packobj%ictorigin = 'NPF'
    !
    ! -- return
    return
  end subroutine wel_create

  subroutine wel_da(this)
! ******************************************************************************
! wel_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(WelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate parent package
    call this%BndType%bnd_da()
    !
    ! -- scalars
    call mem_deallocate(this%iflowred)
    call mem_deallocate(this%flowred)
    !
    ! -- return
    return
  end subroutine wel_da

  subroutine wel_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(WelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- allocate the object and assign values to object variables
    call mem_allocate(this%iflowred, 'IFLOWRED', this%origin)
    call mem_allocate(this%flowred, 'FLOWRED', this%origin)
    !
    ! -- Set values
    this%iflowred = 0
    this%flowred = DZERO
    !
    ! -- return
    return
  end subroutine wel_allocate_scalars

  subroutine wel_options(this, option, found)
! ******************************************************************************
! wel_options -- set options specific to WelType
!
! rch_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use InputOutputModule, only: urword
    ! -- dummy
    class(WelType),   intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical,          intent(inout) :: found
    ! -- local
    real(DP) :: r
    ! -- formats
    character(len=*),parameter :: fmtflowred = &
      "(4x, 'AUTOMATIC FLOW REDUCTION OF WELLS IMPLEMENTED.')"
    character(len=*),parameter :: fmtflowredv = &
      "(4x, 'AUTOMATIC FLOW REDUCTION FRACTION (',g15.7,').')"
! ------------------------------------------------------------------------------
    !
    ! -- Check for 'AUTO_FLOW_REDUCE' and set this%iflowred
    select case (option)
      case('AUTO_FLOW_REDUCE')
        this%iflowred = 1
        r = this%parser%GetDouble()
        if (r <= DZERO) then
          r = DEM1
        else if (r > DONE) then
          r = DONE
        end if
        this%flowred = r
        !
        ! -- Write option and return with found set to true
        if(this%iflowred > 0) &
          write(this%iout, fmtflowred)
          write(this%iout, fmtflowredv) this%flowred
        found = .true.
      case('MOVER')
        this%imover = 1
        write(this%iout, '(4x,A)') 'MOVER OPTION ENABLED'
        found = .true.
      case default
        !
        ! -- No options found
        found = .false.
    end select
    !
    ! -- return
    return
  end subroutine wel_options

  subroutine wel_cf(this)
! ******************************************************************************
! wel_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip in no wells
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(WelType) :: this
    ! -- local
    integer(I4B) :: i, node, ict
    real(DP) :: qmult
    real(DP) :: q
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: thick
! ------------------------------------------------------------------------------
    !
    ! -- Return if no wells
    if(this%nbound == 0) return
    !
    ! -- pakmvrobj cf
    if(this%imover == 1) then
      call this%pakmvrobj%cf()
    endif
    !
    ! -- Calculate hcof and rhs for each well entry
    do i = 1, this%nbound
      node = this%nodelist(i)
      this%hcof(i) = DZERO
      if(this%ibound(node) <= 0) then
        this%rhs(i) = DZERO
        cycle
      end if
      q = this%bound(1,i)
      if (this%iflowred /= 0 .and. q < DZERO) then
        ict = this%icelltype(node)
        if (ict /= 0) then
          tp = this%dis%top(node)
          bt = this%dis%bot(node)
          thick = tp - bt
          tp = bt + this%flowred * thick
          qmult = sQSaturation(tp, bt, this%xnew(node))
          q = q * qmult
        endif
      end if
      this%rhs(i) = -q
    enddo
    !
    return
  end subroutine wel_cf

  subroutine wel_fc(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! wel_fc -- Copy rhs and hcof into solution rhs and amat
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(WelType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i, n, ipos
! --------------------------------------------------------------------------
    !
    ! -- pakmvrobj fc
    if(this%imover == 1) then
      call this%pakmvrobj%fc()
    endif
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      n = this%nodelist(i)
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + this%hcof(i)
      !
      ! -- If mover is active and this well is discharging,
      !    store available water (as positive value).
      if(this%imover == 1 .and. this%rhs(i) > DZERO) then
        call this%pakmvrobj%accumulate_qformvr(i, this%rhs(i))
      endif
    enddo
    !
    ! -- return
    return
  end subroutine wel_fc

  subroutine wel_fn(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! wel_fn -- Fill newton terms
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(WelType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i, node, ipos, ict
    real(DP) :: drterm
    real(DP) :: q
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: thick
! --------------------------------------------------------------------------

    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      node = this%nodelist(i)
      !
      ! -- test if node is constant or inactive
      if(this%ibound(node) <= 0) then
        cycle
      end if
      !
      ! -- well rate is possibly head dependent
      ict = this%icelltype(node)
      if (this%iflowred /= 0 .and. ict /= 0) then
        ipos = ia(node)
        q = -this%rhs(i)
        if (q < DZERO) then
          ! -- calculate derivative for well
          tp = this%dis%top(node)
          bt = this%dis%bot(node)
          thick = tp - bt
          tp = bt + this%flowred * thick
          drterm = sQSaturationDerivative(tp, bt, this%xnew(node))
          drterm = drterm * this%bound(1,i)
          !--fill amat and rhs with newton-raphson terms
          amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + drterm
          rhs(node) = rhs(node) + drterm * this%xnew(node)
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine wel_fn


  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(WelType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- create the header list label
    this%listlabel = trim(this%filtyp) // ' NO.'
    if(this%dis%ndim == 3) then
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'ROW'
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'COL'
    elseif(this%dis%ndim == 2) then
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'CELL2D'
    else
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'NODE'
    endif
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'STRESS RATE'
    if(this%inamedbound == 1) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    endif
    !
    ! -- return
    return
  end subroutine define_listlabel

  ! -- Procedures related to observations
  logical function wel_obs_supported(this)
  ! ******************************************************************************
  ! wel_obs_supported
  !   -- Return true because WEL package supports observations.
  !   -- Overrides BndType%bnd_obs_supported()
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    implicit none
    class(WelType) :: this
  ! ------------------------------------------------------------------------------
    wel_obs_supported = .true.
    return
  end function wel_obs_supported

  subroutine wel_df_obs(this)
  ! ******************************************************************************
  ! wel_df_obs (implements bnd_df_obs)
  !   -- Store observation type supported by WEL package.
  !   -- Overrides BndType%bnd_df_obs
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(WelType) :: this
    ! -- local
    integer(I4B) :: indx
  ! ------------------------------------------------------------------------------
    call this%obs%StoreObsType('wel', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine wel_df_obs

  ! -- Procedure related to time series

  subroutine wel_rp_ts(this)
    ! -- Assign tsLink%Text appropriately for
    !    all time series in use by package.
    !    In the WEL package only the Q variable
    !    can be controlled by time series.
    ! -- dummy
    class(WelType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, nlinks
    type(TimeSeriesLinkType), pointer :: tslink => null()
    !
    nlinks = this%TsManager%boundtslinks%Count()
    do i=1,nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        if (tslink%JCol==1) then
          tslink%Text = 'Q'
        endif
      endif
    enddo
    !
    return
  end subroutine wel_rp_ts

end module WelModule

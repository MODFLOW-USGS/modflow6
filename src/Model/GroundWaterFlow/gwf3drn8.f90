module DrnModule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE, DTWO, &
                             LENFTYPE, LENPACKAGENAME, LENAUXNAME, LINELENGTH
  use MemoryHelperModule, only: create_mem_path
  use SmoothingModule, only: sQSaturation, sQSaturationDerivative, &
                             sQuadraticSaturation
  use BndModule, only: BndType
  use ObsModule, only: DefaultObsIdProcessor
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  !
  implicit none
  !
  private
  public :: drn_create
  public :: DrnType
  !
  character(len=LENFTYPE) :: ftype = 'DRN'
  character(len=LENPACKAGENAME) :: text = '             DRN'
  !
  type, extends(BndType) :: DrnType

    integer(I4B), pointer :: iauxddrncol => null()
    integer(I4B), pointer :: icubic_scaling => null()

  contains
    procedure :: allocate_scalars => drn_allocate_scalars
    procedure :: bnd_options => drn_options
    procedure :: bnd_ck => drn_ck
    procedure :: bnd_cf => drn_cf
    procedure :: bnd_fc => drn_fc
    procedure :: bnd_fn => drn_fn
    procedure :: bnd_da => drn_da
    procedure :: define_listlabel
    procedure :: get_drain_elevations
    procedure :: get_drain_factor
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => drn_obs_supported
    procedure, public :: bnd_df_obs => drn_df_obs
    ! -- method for time series
    procedure, public :: bnd_rp_ts => drn_rp_ts
  end type DrnType

contains

  subroutine drn_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! drn_create -- Create a New Drn Package
! Subroutine: (1) create new-style package
!             (2) point packobj to the new package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B), intent(in) :: id
    integer(I4B), intent(in) :: ibcnum
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    ! -- local
    type(DrnType), pointer :: drnobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate (drnobj)
    packobj => drnobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call drnobj%allocate_scalars()
    !s
    ! -- initialize package
    call packobj%pack_initialize()
    !
    ! -- initialize
    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 2 ! drnelev, conductance
    packobj%iscloc = 2 !sfac applies to conductance
    packobj%ictMemPath = create_mem_path(namemodel, 'NPF')
    !
    ! -- return
    return
  end subroutine drn_create

  subroutine drn_da(this)
! ******************************************************************************
! drn_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(DrnType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate parent package
    call this%BndType%bnd_da()
    !
    ! -- scalars
    call mem_deallocate(this%iauxddrncol)
    call mem_deallocate(this%icubic_scaling)
    !
    ! -- return
    return
  end subroutine drn_da

  subroutine drn_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(DrnType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- allocate the object and assign values to object variables
    call mem_allocate(this%iauxddrncol, 'IAUXDDRNCOL', this%memoryPath)
    call mem_allocate(this%icubic_scaling, 'ICUBIC_SCALING', this%memoryPath)
    !
    ! -- Set values
    this%iauxddrncol = 0
    if (this%inewton /= 0) then
      this%icubic_scaling = 1
    else
      this%icubic_scaling = 0
    end if
    !
    ! -- return
    return
  end subroutine drn_allocate_scalars

  subroutine drn_options(this, option, found)
! ******************************************************************************
! drn_options -- set options specific to DrnType
!
! drn_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use InputOutputModule, only: urword
    use SimModule, only: store_error
    ! -- dummy
    class(DrnType), intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical, intent(inout) :: found
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LENAUXNAME) :: ddrnauxname
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    select case (option)
    case ('MOVER')
      this%imover = 1
      write (this%iout, '(4x,A)') 'MOVER OPTION ENABLED'
      found = .true.
    case ('AUXDEPTHNAME')
      call this%parser%GetStringCaps(ddrnauxname)
      this%iauxddrncol = -1
      write (this%iout, '(4x,a,a)') &
        'AUXILIARY DRAIN DEPTH NAME: ', trim(ddrnauxname)
      found = .true.
      !
      ! -- right now these are options that are only available in the
      !    development version and are not included in the documentation.
      !    These options are only available when IDEVELOPMODE in
      !    constants module is set to 1
    case ('DEV_CUBIC_SCALING')
      call this%parser%DevOpt()
      this%icubic_scaling = 1
      write (this%iout, '(4x,a,1x,a)') &
        'CUBIC SCALING will be used for drains with non-zero DDRN values', &
        'even if the NEWTON-RAPHSON method is not being used.'
      found = .true.
    case default
      !
      ! -- No options found
      found = .false.
    end select
    !
    ! -- DDRN was specified, so find column of auxvar that will be used
    if (this%iauxddrncol < 0) then
      !
      ! -- Error if no aux variable specified
      if (this%naux == 0) then
        write (errmsg, '(a,2(1x,a))') &
          'AUXDDRNNAME WAS SPECIFIED AS', trim(adjustl(ddrnauxname)), &
          'BUT NO AUX VARIABLES SPECIFIED.'
        call store_error(errmsg)
      end if
      !
      ! -- Assign ddrn column
      this%iauxddrncol = 0
      do n = 1, this%naux
        if (ddrnauxname == this%auxname(n)) then
          this%iauxddrncol = n
          exit
        end if
      end do
      !
      ! -- Error if aux variable cannot be found
      if (this%iauxddrncol == 0) then
        write (errmsg, '(a,2(1x,a))') &
          'AUXDDRNNAME WAS SPECIFIED AS', trim(adjustl(ddrnauxname)), &
          'BUT NO AUX VARIABLE FOUND WITH THIS NAME.'
        call store_error(errmsg)
      end if
    end if
    !
    ! -- return
    return
  end subroutine drn_options

  subroutine drn_ck(this)
! ******************************************************************************
! drn_ck -- Check drain boundary condition data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_unit
    ! -- dummy
    class(DrnType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: i
    integer(I4B) :: node
    real(DP) :: bt
    real(DP) :: drndepth
    real(DP) :: drntop
    real(DP) :: drnbot
    ! -- formats
    character(len=*), parameter :: fmtddrnerr = &
      "('SCALED-CONDUCTANCE DRN BOUNDARY (',i0,') BOTTOM ELEVATION &
      &(',f10.3,') IS LESS THAN CELL BOTTOM (',f10.3,')')"
    character(len=*), parameter :: fmtdrnerr = &
      "('DRN BOUNDARY (',i0,') ELEVATION (',f10.3,') IS LESS THAN CELL &
      &BOTTOM (',f10.3,')')"
! ------------------------------------------------------------------------------
    !
    ! -- check stress period data
    do i = 1, this%nbound
      node = this%nodelist(i)
      bt = this%dis%bot(node)
      !
      ! -- calculate the drainage depth and the top and bottom of
      !    the conductance scaling elevations
      call this%get_drain_elevations(i, drndepth, drntop, drnbot)
      !
      ! -- accumulate errors
      if (drnbot < bt .and. this%icelltype(node) /= 0) then
        if (drndepth /= DZERO) then
          write (errmsg, fmt=fmtddrnerr) i, drnbot, bt
        else
          write (errmsg, fmt=fmtdrnerr) i, drnbot, bt
        end if
        call store_error(errmsg)
      end if
    end do
    !
    ! -- write summary of drain package error messages
    if (count_errors() > 0) then
      call store_error_unit(this%inunit)
    end if
    !
    ! -- return
    return
  end subroutine drn_ck

  subroutine drn_cf(this, reset_mover)
! ******************************************************************************
! drn_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip if no drains
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DrnType) :: this
    logical, intent(in), optional :: reset_mover
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: node
    real(DP) :: cdrn
    real(DP) :: drnbot
    real(DP) :: fact
    logical :: lrm
! ------------------------------------------------------------------------------
    !
    ! -- Return if no drains
    if (this%nbound == 0) return
    !
    ! -- pakmvrobj cf
    lrm = .true.
    if (present(reset_mover)) lrm = reset_mover
    if (this%imover == 1 .and. lrm) then
      call this%pakmvrobj%cf()
    end if
    !
    ! -- Calculate hcof and rhs for each drn entry
    do i = 1, this%nbound
      node = this%nodelist(i)
      if (this%ibound(node) <= 0) then
        this%hcof(i) = DZERO
        this%rhs(i) = DZERO
        cycle
      end if
      !
      ! -- set local variables for this drain
      cdrn = this%bound(2, i)
      !
      ! -- calculate the drainage scaling factor
      call this%get_drain_factor(i, fact, drnbot)
      !
      ! -- calculate rhs and hcof
      this%rhs(i) = -fact * cdrn * drnbot
      this%hcof(i) = -fact * cdrn
    end do
    !
    ! -- return
    return
  end subroutine drn_cf

  subroutine drn_fc(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! drn_fc -- Copy rhs and hcof into solution rhs and amat
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(DrnType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: ipos
    real(DP) :: fact
    real(DP) :: drnbot
    real(DP) :: drncond
    real(DP) :: qdrn
! --------------------------------------------------------------------------
    !
    ! -- packmvrobj fc
    if (this%imover == 1) then
      call this%pakmvrobj%fc()
    end if
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      n = this%nodelist(i)
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + this%hcof(i)
      !
      ! -- calculate the drainage scaling factor
      call this%get_drain_factor(i, fact, drnbot)
      !
      ! -- If mover is active and this drain is discharging,
      !    store available water (as positive value).
      if (this%imover == 1 .and. fact > DZERO) then
        drncond = this%bound(2, i)
        qdrn = fact * drncond * (this%xnew(n) - drnbot)
        call this%pakmvrobj%accumulate_qformvr(i, qdrn)
      end if
    end do
    !
    ! -- return
    return
  end subroutine drn_fc

  subroutine drn_fn(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! drn_fn -- Fill newton terms
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(DrnType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: node
    integer(I4B) :: ipos
    real(DP) :: cdrn
    real(DP) :: xnew
    real(DP) :: drndepth
    real(DP) :: drntop
    real(DP) :: drnbot
    real(DP) :: drterm
! --------------------------------------------------------------------------

    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    if (this%iauxddrncol /= 0) then
      do i = 1, this%nbound
        node = this%nodelist(i)
        !
        ! -- test if node is constant or inactive
        if (this%ibound(node) <= 0) then
          cycle
        end if
        !
        ! -- set local variables for this drain
        cdrn = this%bound(2, i)
        xnew = this%xnew(node)
        !
        ! -- calculate the drainage depth and the top and bottom of
        !    the conductance scaling elevations
        call this%get_drain_elevations(i, drndepth, drntop, drnbot)
        !
        ! -- calculate scaling factor
        if (drndepth /= DZERO) then
          drterm = sQSaturationDerivative(drntop, drnbot, xnew, &
                                          c1=-DONE, c2=DTWO)
          drterm = drterm * cdrn * (drnbot - xnew)
          !
          ! -- fill amat and rhs with newton-raphson terms
          ipos = ia(node)
          amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + drterm
          rhs(node) = rhs(node) + drterm * xnew
        end if
      end do
    end if
    !
    ! -- return
    return
  end subroutine drn_fn

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(DrnType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- create the header list label
    this%listlabel = trim(this%filtyp)//' NO.'
    if (this%dis%ndim == 3) then
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'ROW'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'COL'
    elseif (this%dis%ndim == 2) then
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'CELL2D'
    else
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'NODE'
    end if
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'DRAIN EL.'
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'CONDUCTANCE'
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- return
    return
  end subroutine define_listlabel

  subroutine get_drain_elevations(this, i, drndepth, drntop, drnbot)
! ******************************************************************************
! get_drain_elevations -- Define drain depth and the top and bottom elevations
!                         used to scale the drain conductance.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DrnType), intent(inout) :: this
    integer(I4B), intent(in) :: i
    real(DP), intent(inout) :: drndepth
    real(DP), intent(inout) :: drntop
    real(DP), intent(inout) :: drnbot
    ! -- local
    real(DP) :: drnelev
    real(DP) :: elev
! ------------------------------------------------------------------------------
    !
    ! -- initialize dummy and local variables
    drndepth = DZERO
    drnelev = this%bound(1, i)
    !
    ! -- set the drain depth
    if (this%iauxddrncol > 0) then
      drndepth = this%auxvar(this%iauxddrncol, i)
    end if
    !
    ! -- calculate the top and bottom drain elevations
    if (drndepth /= DZERO) then
      elev = drnelev + drndepth
      drntop = max(elev, drnelev)
      drnbot = min(elev, drnelev)
    else
      drntop = drnelev
      drnbot = drnelev
    end if
    !
    ! -- return
    return
  end subroutine get_drain_elevations

  subroutine get_drain_factor(this, i, factor, opt_drnbot)
! ******************************************************************************
! get_drain_factor -- Get the drain conductance scale factor.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(DrnType), intent(inout) :: this
    integer(I4B), intent(in) :: i
    real(DP), intent(inout) :: factor
    real(DP), intent(inout), optional :: opt_drnbot
    ! -- local
    integer(I4B) :: node
    real(DP) :: xnew
    real(DP) :: drndepth
    real(DP) :: drntop
    real(DP) :: drnbot
! ------------------------------------------------------------------------------
    !
    ! -- set local variables for this drain
    node = this%nodelist(i)
    xnew = this%xnew(node)
    !
    ! -- calculate the drainage depth and the top and bottom of
    !    the conductance scaling elevations
    call this%get_drain_elevations(i, drndepth, drntop, drnbot)
    !
    ! -- set opt_drnbot to drnbot if passed as dummy variable
    if (present(opt_drnbot)) then
      opt_drnbot = drnbot
    end if
    !
    ! -- calculate scaling factor
    if (drndepth /= DZERO) then
      if (this%icubic_scaling /= 0) then
        factor = sQSaturation(drntop, drnbot, xnew, c1=-DONE, c2=DTWO)
      else
        factor = sQuadraticSaturation(drntop, drnbot, xnew, eps=DZERO)
      end if
    else
      if (xnew <= drnbot) then
        factor = DZERO
      else
        factor = DONE
      end if
    end if
    !
    ! -- return
    return
  end subroutine get_drain_factor

  ! -- Procedures related to observations

  logical function drn_obs_supported(this)
! ******************************************************************************
! drn_obs_supported
!   -- Return true because DRN package supports observations.
!   -- Overrides BndType%bnd_obs_supported()
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(DrnType) :: this
! ------------------------------------------------------------------------------
    drn_obs_supported = .true.
    !
    ! -- return
    return
  end function drn_obs_supported

  subroutine drn_df_obs(this)
! ******************************************************************************
! drn_df_obs (implements bnd_df_obs)
!   -- Store observation type supported by DRN package.
!   -- Overrides BndType%bnd_df_obs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(DrnType) :: this
    ! -- local
    integer(I4B) :: indx
! ------------------------------------------------------------------------------
    call this%obs%StoreObsType('drn', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine drn_df_obs

  ! -- Procedure related to time series

  subroutine drn_rp_ts(this)
    ! -- Assign tsLink%Text appropriately for
    !    all time series in use by package.
    !    In DRN package variables ELEV and COND
    !    can be controlled by time series.
    ! -- dummy
    class(DrnType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, nlinks
    type(TimeSeriesLinkType), pointer :: tslink => null()
    !
    nlinks = this%TsManager%boundtslinks%Count()
    do i = 1, nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        select case (tslink%JCol)
        case (1)
          tslink%Text = 'ELEV'
        case (2)
          tslink%Text = 'COND'
        end select
      end if
    end do
    !
    return
  end subroutine drn_rp_ts

end module DrnModule

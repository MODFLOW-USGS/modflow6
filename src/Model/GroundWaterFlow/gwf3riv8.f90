module rivmodule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, LENFTYPE, LENPACKAGENAME
  use MemoryHelperModule, only: create_mem_path
  use BndModule, only: BndType
  use ObsModule, only: DefaultObsIdProcessor
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  !
  implicit none
  !
  private
  public :: riv_create
  public :: RivType
  !
  character(len=LENFTYPE) :: ftype = 'RIV'
  character(len=LENPACKAGENAME) :: text = '             RIV'
  !
  type, extends(BndType) :: RivType
  contains
    procedure :: bnd_options => riv_options
    procedure :: bnd_ck => riv_ck
    procedure :: bnd_cf => riv_cf
    procedure :: bnd_fc => riv_fc
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => riv_obs_supported
    procedure, public :: bnd_df_obs => riv_df_obs
    ! -- method for time series
    procedure, public :: bnd_rp_ts => riv_rp_ts
  end type RivType

contains

  subroutine riv_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! riv_create -- Create a New Riv Package
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
    type(RivType), pointer :: rivobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate (rivobj)
    packobj => rivobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call rivobj%allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()

    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 3 ! stage, conductance, rbot
    packobj%iscloc = 2 !sfac applies to conductance
    packobj%ictMemPath = create_mem_path(namemodel, 'NPF')
    !
    ! -- return
    return
  end subroutine riv_create

  subroutine riv_options(this, option, found)
! ******************************************************************************
! riv_options -- set options specific to RivType
!
! riv_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use InputOutputModule, only: urword
    ! -- dummy
    class(RivType), intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical, intent(inout) :: found
    ! -- local
! ------------------------------------------------------------------------------
    !
    select case (option)
    case ('MOVER')
      this%imover = 1
      write (this%iout, '(4x,A)') 'MOVER OPTION ENABLED'
      found = .true.
    case default
      !
      ! -- No options found
      found = .false.
    end select
    !
    ! -- return
    return
  end subroutine riv_options

  subroutine riv_ck(this)
! ******************************************************************************
! riv_ck -- Check river boundary condition data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_unit
    ! -- dummy
    class(RivType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: i
    integer(I4B) :: node
    real(DP) :: bt
    real(DP) :: stage
    real(DP) :: rbot
    ! -- formats
    character(len=*), parameter :: fmtriverr = &
      "('RIV BOUNDARY (',i0,') RIVER BOTTOM (',f10.4,') IS LESS &
      &THAN CELL BOTTOM (',f10.4,')')"
    character(len=*), parameter :: fmtriverr2 = &
      "('RIV BOUNDARY (',i0,') RIVER STAGE (',f10.4,') IS LESS &
      &THAN RIVER BOTTOM (',f10.4,')')"
    character(len=*), parameter :: fmtriverr3 = &
      "('RIV BOUNDARY (',i0,') RIVER STAGE (',f10.4,') IS LESS &
      &THAN CELL BOTTOM (',f10.4,')')"
! ------------------------------------------------------------------------------
    !
    ! -- check stress period data
    do i = 1, this%nbound
      node = this%nodelist(i)
      bt = this%dis%bot(node)
      stage = this%bound(1, i)
      rbot = this%bound(3, i)
      ! -- accumulate errors
      if (rbot < bt .and. this%icelltype(node) /= 0) then
        write (errmsg, fmt=fmtriverr) i, rbot, bt
        call store_error(errmsg)
      end if
      if (stage < rbot) then
        write (errmsg, fmt=fmtriverr2) i, stage, rbot
        call store_error(errmsg)
      end if
      if (stage < bt .and. this%icelltype(node) /= 0) then
        write (errmsg, fmt=fmtriverr3) i, stage, bt
        call store_error(errmsg)
      end if
    end do
    !
    ! -- write summary of river package error messages
    if (count_errors() > 0) then
      call store_error_unit(this%inunit)
    end if
    !
    ! -- return
    return
  end subroutine riv_ck

  subroutine riv_cf(this, reset_mover)
! ******************************************************************************
! riv_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip in no rivs
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(RivType) :: this
    logical, intent(in), optional :: reset_mover
    ! -- local
    integer(I4B) :: i, node
    real(DP) :: hriv, criv, rbot
    logical :: lrm
! ------------------------------------------------------------------------------
    !
    ! -- Return if no rivs
    if (this%nbound .eq. 0) return
    !
    ! -- pakmvrobj cf
    lrm = .true.
    if (present(reset_mover)) lrm = reset_mover
    if (this%imover == 1 .and. lrm) then
      call this%pakmvrobj%cf()
    end if
    !
    ! -- Calculate hcof and rhs for each riv entry
    do i = 1, this%nbound
      node = this%nodelist(i)
      if (this%ibound(node) <= 0) then
        this%hcof(i) = DZERO
        this%rhs(i) = DZERO
        cycle
      end if
      hriv = this%bound(1, i)
      criv = this%bound(2, i)
      rbot = this%bound(3, i)
      if (this%xnew(node) <= rbot) then
        this%rhs(i) = -criv * (hriv - rbot)
        this%hcof(i) = DZERO
      else
        this%rhs(i) = -criv * hriv
        this%hcof(i) = -criv
      end if
    end do
    !
    ! -- return
    return
  end subroutine riv_cf

  subroutine riv_fc(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! riv_fc -- Copy rhs and hcof into solution rhs and amat
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(RivType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i, n, ipos
    real(DP) :: cond, stage, qriv !, rbot
! --------------------------------------------------------------------------
    !
    ! -- pakmvrobj fc
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
      ! -- If mover is active and this river cell is discharging,
      !    store available water (as positive value).
      stage = this%bound(1, i)
      if (this%imover == 1 .and. this%xnew(n) > stage) then
        cond = this%bound(2, i)
        qriv = cond * (this%xnew(n) - stage)
        call this%pakmvrobj%accumulate_qformvr(i, qriv)
      end if
    end do
    !
    ! -- return
    return
  end subroutine riv_fc

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(RivType), intent(inout) :: this
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
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'STAGE'
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'CONDUCTANCE'
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOTTOM EL.'
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- return
    return
  end subroutine define_listlabel

  ! -- Procedures related to observations

  logical function riv_obs_supported(this)
! ******************************************************************************
! riv_obs_supported
!   -- Return true because RIV package supports observations.
!   -- Overrides BndType%bnd_obs_supported()
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(RivType) :: this
! ------------------------------------------------------------------------------
    riv_obs_supported = .true.
    return
  end function riv_obs_supported

  subroutine riv_df_obs(this)
    ! ******************************************************************************
    ! riv_df_obs (implements bnd_df_obs)
    !   -- Store observation type supported by RIV package.
    !   -- Overrides BndType%bnd_df_obs
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(RivType) :: this
    ! -- local
    integer(I4B) :: indx
    ! ------------------------------------------------------------------------------
    call this%obs%StoreObsType('riv', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine riv_df_obs

  ! -- Procedure related to time series

  subroutine riv_rp_ts(this)
    ! -- Assign tsLink%Text appropriately for
    !    all time series in use by package.
    !    In RIV package variables STAGE, COND, and RBOT
    !    can be controlled by time series.
    ! -- dummy
    class(RivType), intent(inout) :: this
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
          tslink%Text = 'STAGE'
        case (2)
          tslink%Text = 'COND'
        case (3)
          tslink%Text = 'RBOT'
        end select
      end if
    end do
    !
    return
  end subroutine riv_rp_ts

end module rivmodule

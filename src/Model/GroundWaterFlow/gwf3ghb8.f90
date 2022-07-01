module ghbmodule
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
  public :: ghb_create
  public :: GhbType
  !
  character(len=LENFTYPE) :: ftype = 'GHB'
  character(len=LENPACKAGENAME) :: text = '             GHB'
  !
  type, extends(BndType) :: GhbType
  contains
    procedure :: bnd_options => ghb_options
    procedure :: bnd_ck => ghb_ck
    procedure :: bnd_cf => ghb_cf
    procedure :: bnd_fc => ghb_fc
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => ghb_obs_supported
    procedure, public :: bnd_df_obs => ghb_df_obs
    ! -- method for time series
    procedure, public :: bnd_rp_ts => ghb_rp_ts
  end type GhbType

contains

  subroutine ghb_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! ghb_create -- Create a New Ghb Package
! Subroutine: (1) create new-style package
!             (2) point bndobj to the new package
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
    type(GhbType), pointer :: ghbobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate (ghbobj)
    packobj => ghbobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call packobj%allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()
    !
    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 2
    packobj%iscloc = 2
    packobj%ictMemPath = create_mem_path(namemodel, 'NPF')
    !
    ! -- return
    return
  end subroutine ghb_create

  subroutine ghb_options(this, option, found)
! ******************************************************************************
! ghb_options -- set options specific to GhbType
!
! ghb_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GhbType), intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical, intent(inout) :: found
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
  end subroutine ghb_options

  subroutine ghb_ck(this)
! ******************************************************************************
! ghb_ck -- Check ghb boundary condition data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_unit
    ! -- dummy
    class(GhbType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: i
    integer(I4B) :: node
    real(DP) :: bt
    ! -- formats
    character(len=*), parameter :: fmtghberr = &
      "('GHB BOUNDARY (',i0,') HEAD (',f10.3,') IS LESS THAN CELL &
      &BOTTOM (',f10.3,')')"
! ------------------------------------------------------------------------------
    !
    ! -- check stress period data
    do i = 1, this%nbound
      node = this%nodelist(i)
      bt = this%dis%bot(node)
      ! -- accumulate errors
      if (this%bound(1, i) < bt .and. this%icelltype(node) /= 0) then
        write (errmsg, fmt=fmtghberr) i, this%bound(1, i), bt
        call store_error(errmsg)
      end if
    end do
    !
    !write summary of ghb package error messages
    if (count_errors() > 0) then
      call store_error_unit(this%inunit)
    end if
    !
    ! -- return
    return
  end subroutine ghb_ck

  subroutine ghb_cf(this, reset_mover)
! ******************************************************************************
! ghb_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip if no ghbs
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GhbType) :: this
    logical, intent(in), optional :: reset_mover
    ! -- local
    integer(I4B) :: i, node
    logical :: lrm
! ------------------------------------------------------------------------------
    !
    ! -- Return if no ghbs
    if (this%nbound .eq. 0) return
    !
    ! -- packmvrobj cf
    lrm = .true.
    if (present(reset_mover)) lrm = reset_mover
    if (this%imover == 1 .and. lrm) then
      call this%pakmvrobj%cf()
    end if
    !
    ! -- Calculate hcof and rhs for each ghb entry
    do i = 1, this%nbound
      node = this%nodelist(i)
      if (this%ibound(node) .le. 0) then
        this%hcof(i) = DZERO
        this%rhs(i) = DZERO
        cycle
      end if
      this%hcof(i) = -this%bound(2, i)
      this%rhs(i) = -this%bound(2, i) * this%bound(1, i)
    end do
    !
    ! -- return
    return
  end subroutine ghb_cf

  subroutine ghb_fc(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! ghb_fc -- Copy rhs and hcof into solution rhs and amat
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(GhbType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i, n, ipos
    real(DP) :: cond, bhead, qghb
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
      ! -- If mover is active and this boundary is discharging,
      !    store available water (as positive value).
      bhead = this%bound(1, i)
      if (this%imover == 1 .and. this%xnew(n) > bhead) then
        cond = this%bound(2, i)
        qghb = cond * (this%xnew(n) - bhead)
        call this%pakmvrobj%accumulate_qformvr(i, qghb)
      end if
    end do
    !
    ! -- return
    return
  end subroutine ghb_fc

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GhbType), intent(inout) :: this
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
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- return
    return
  end subroutine define_listlabel

  ! -- Procedures related to observations

  logical function ghb_obs_supported(this)
    ! ******************************************************************************
    ! ghb_obs_supported
    !   -- Return true because GHB package supports observations.
    !   -- Overrides BndType%bnd_obs_supported()
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    implicit none
    class(GhbType) :: this
    ! ------------------------------------------------------------------------------
    ghb_obs_supported = .true.
    return
  end function ghb_obs_supported

  subroutine ghb_df_obs(this)
    ! ******************************************************************************
    ! ghb_df_obs (implements bnd_df_obs)
    !   -- Store observation type supported by GHB package.
    !   -- Overrides BndType%bnd_df_obs
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(GhbType) :: this
    ! -- local
    integer(I4B) :: indx
    ! ------------------------------------------------------------------------------
    call this%obs%StoreObsType('ghb', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine ghb_df_obs
  !
  ! -- Procedure related to time series
  !
  subroutine ghb_rp_ts(this)
    ! -- Assign tsLink%Text appropriately for
    !    all time series in use by package.
    !    In GHB package variables BHEAD and COND
    !    can be controlled by time series.
    ! -- dummy
    class(GhbType), intent(inout) :: this
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
          tslink%Text = 'BHEAD'
        case (2)
          tslink%Text = 'COND'
        end select
      end if
    end do
    !
    return
  end subroutine ghb_rp_ts

end module ghbmodule

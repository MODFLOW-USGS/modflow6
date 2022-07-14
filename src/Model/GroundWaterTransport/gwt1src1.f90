module GwtSrcModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DEM1, DONE, LENFTYPE
  use BndModule, only: BndType
  use ObsModule, only: DefaultObsIdProcessor
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use BlockParserModule, only: BlockParserType
  !
  implicit none
  !
  private
  public :: src_create
  !
  character(len=LENFTYPE) :: ftype = 'SRC'
  character(len=16) :: text = '             SRC'
  !
  type, extends(BndType) :: GwtSrcType
  contains
    procedure :: allocate_scalars => src_allocate_scalars
    procedure :: bnd_cf => src_cf
    procedure :: bnd_fc => src_fc
    procedure :: bnd_da => src_da
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => src_obs_supported
    procedure, public :: bnd_df_obs => src_df_obs
    ! -- methods for time series
    procedure, public :: bnd_rp_ts => src_rp_ts
  end type GwtSrcType

contains

  subroutine src_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! src_create -- Create a New Src Package
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
    type(GwtSrcType), pointer :: srcobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate (srcobj)
    packobj => srcobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call srcobj%allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()

    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 1
    packobj%iscloc = 1
    !
    ! -- return
    return
  end subroutine src_create

  subroutine src_da(this)
! ******************************************************************************
! src_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtSrcType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate parent package
    call this%BndType%bnd_da()
    !
    ! -- scalars
    !
    ! -- return
    return
  end subroutine src_da

  subroutine src_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtSrcType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- allocate the object and assign values to object variables
    !
    ! -- Set values
    !
    ! -- return
    return
  end subroutine src_allocate_scalars

  subroutine src_cf(this, reset_mover)
! ******************************************************************************
! src_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip if no sources
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSrcType) :: this
    logical, intent(in), optional :: reset_mover
    ! -- local
    integer(I4B) :: i, node
    real(DP) :: q
    logical :: lrm
! ------------------------------------------------------------------------------
    !
    ! -- Return if no sources
    if (this%nbound == 0) return
    !
    ! -- pakmvrobj cf
    lrm = .true.
    if (present(reset_mover)) lrm = reset_mover
    if (this%imover == 1 .and. lrm) then
      call this%pakmvrobj%cf()
    end if
    !
    ! -- Calculate hcof and rhs for each source entry
    do i = 1, this%nbound
      node = this%nodelist(i)
      this%hcof(i) = DZERO
      if (this%ibound(node) <= 0) then
        this%rhs(i) = DZERO
        cycle
      end if
      q = this%bound(1, i)
      this%rhs(i) = -q
    end do
    !
    return
  end subroutine src_cf

  subroutine src_fc(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! src_fc -- Copy rhs and hcof into solution rhs and amat
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(GwtSrcType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i, n, ipos
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
      ! -- If mover is active and mass is being withdrawn,
      !    store available mass (as positive value).
      if (this%imover == 1 .and. this%rhs(i) > DZERO) then
        call this%pakmvrobj%accumulate_qformvr(i, this%rhs(i))
      end if
    end do
    !
    ! -- return
    return
  end subroutine src_fc

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwtSrcType), intent(inout) :: this
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
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'STRESS RATE'
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- return
    return
  end subroutine define_listlabel

  ! -- Procedures related to observations
  logical function src_obs_supported(this)
    ! ******************************************************************************
    ! src_obs_supported
    !   -- Return true because SRC package supports observations.
    !   -- Overrides BndType%bnd_obs_supported()
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    implicit none
    class(GwtSrcType) :: this
    ! ------------------------------------------------------------------------------
    src_obs_supported = .true.
    return
  end function src_obs_supported

  subroutine src_df_obs(this)
    ! ******************************************************************************
    ! src_df_obs (implements bnd_df_obs)
    !   -- Store observation type supported by SRC package.
    !   -- Overrides BndType%bnd_df_obs
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(GwtSrcType) :: this
    ! -- local
    integer(I4B) :: indx
    ! ------------------------------------------------------------------------------
    call this%obs%StoreObsType('src', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine src_df_obs

  ! -- Procedure related to time series

  subroutine src_rp_ts(this)
    ! -- Assign tsLink%Text appropriately for
    !    all time series in use by package.
    !    In the SRC package only the SMASSRATE variable
    !    can be controlled by time series.
    ! -- dummy
    class(GwtSrcType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, nlinks
    type(TimeSeriesLinkType), pointer :: tslink => null()
    !
    nlinks = this%TsManager%boundtslinks%Count()
    do i = 1, nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        if (tslink%JCol == 1) then
          tslink%Text = 'SMASSRATE'
        end if
      end if
    end do
    !
    return
  end subroutine src_rp_ts

end module GwtSrcModule

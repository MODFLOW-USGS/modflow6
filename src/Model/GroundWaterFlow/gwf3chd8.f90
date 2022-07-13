module ChdModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE, NAMEDBOUNDFLAG, LENFTYPE, &
                             LINELENGTH, LENPACKAGENAME
  use MemoryHelperModule, only: create_mem_path
  use ObsModule, only: DefaultObsIdProcessor
  use BndModule, only: BndType
  use ObserveModule, only: ObserveType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  !
  implicit none
  !
  private
  public :: chd_create, ChdType
  !
  character(len=LENFTYPE) :: ftype = 'CHD'
  character(len=LENPACKAGENAME) :: text = '             CHD'
  !
  type, extends(BndType) :: ChdType
    real(DP), dimension(:), pointer, contiguous :: ratechdin => null() !simulated flows into constant head (excluding other chds)
    real(DP), dimension(:), pointer, contiguous :: ratechdout => null() !simulated flows out of constant head (excluding to other chds)
  contains
    procedure :: bnd_rp => chd_rp
    procedure :: bnd_ad => chd_ad
    procedure :: bnd_ck => chd_ck
    procedure :: bnd_fc => chd_fc
    procedure :: bnd_cq => chd_cq
    procedure :: bnd_bd => chd_bd
    procedure :: bnd_da => chd_da
    procedure :: allocate_arrays => chd_allocate_arrays
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => chd_obs_supported
    procedure, public :: bnd_df_obs => chd_df_obs
    ! -- method for time series
    procedure, public :: bnd_rp_ts => chd_rp_ts
  end type ChdType

contains

  subroutine chd_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! chd_create -- Create a New Constant Head Package
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
    type(ChdType), pointer :: chdobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate (chdobj)
    packobj => chdobj
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
    ! -- store values
    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 1
    packobj%iscloc = 1
    packobj%ictMemPath = create_mem_path(namemodel, 'NPF')
    !
    ! -- return
    return
  end subroutine chd_create

  subroutine chd_allocate_arrays(this, nodelist, auxvar)
! ******************************************************************************
! allocate_scalars -- allocate arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(ChdType) :: this
    integer(I4B), dimension(:), pointer, contiguous, optional :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, optional :: auxvar
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_arrays()
    !
    ! -- allocate ratechdex
    call mem_allocate(this%ratechdin, this%maxbound, 'RATECHDIN', this%memoryPath)
    call mem_allocate(this%ratechdout, this%maxbound, 'RATECHDOUT', &
                      this%memoryPath)
    do i = 1, this%maxbound
      this%ratechdin(i) = DZERO
      this%ratechdout(i) = DZERO
    end do
    !
    ! -- return
    return
  end subroutine chd_allocate_arrays

  subroutine chd_rp(this)
! ******************************************************************************
! chd_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SimModule, only: store_error
    ! -- dummy
    class(ChdType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=30) :: nodestr
    integer(I4B) :: i, node, ibd, ierr
! ------------------------------------------------------------------------------
    !
    ! -- Reset previous CHDs to active cell
    do i = 1, this%nbound
      node = this%nodelist(i)
      this%ibound(node) = this%ibcnum
    end do
    !
    ! -- Call the parent class read and prepare
    call this%BndType%bnd_rp()
    !
    ! -- Set ibound to -(ibcnum + 1) for constant head cells
    ierr = 0
    do i = 1, this%nbound
      node = this%nodelist(i)
      ibd = this%ibound(node)
      if (ibd < 0) then
        call this%dis%noder_to_string(node, nodestr)
        write (errmsg, '(3a)') &
          'Cell is already a constant head (', trim(adjustl(nodestr)), ').'
        call store_error(errmsg)
        ierr = ierr + 1
      else
        this%ibound(node) = -this%ibcnum
      end if
    end do
    !
    ! -- Stop if errors detected
    if (ierr > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- return
    return
  end subroutine chd_rp

  subroutine chd_ad(this)
! ******************************************************************************
! chd_ad -- Advance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(ChdType) :: this
    ! -- local
    integer(I4B) :: i, node
    real(DP) :: hb
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Advance the time series
    call this%TsManager%ad()
    !
    ! -- Process each entry in the specified-head cell list
    do i = 1, this%nbound
      node = this%nodelist(i)
      hb = this%bound(1, i)
      this%xnew(node) = hb
      this%xold(node) = this%xnew(node)
    end do
    !
    ! -- For each observation, push simulated value and corresponding
    !    simulation time from "current" to "preceding" and reset
    !    "current" value.
    call this%obs%obs_ad()
    !
    ! -- return
    return
  end subroutine chd_ad

  subroutine chd_ck(this)
! ******************************************************************************
! chd_ck -- Check chd boundary condition data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors
    ! -- dummy
    class(ChdType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=30) :: nodestr
    integer(I4B) :: i
    integer(I4B) :: node
    real(DP) :: bt
    ! -- formats
    character(len=*), parameter :: fmtchderr = &
      "('CHD BOUNDARY ',i0,' HEAD (',g0,') IS LESS THAN CELL &
      &BOTTOM (',g0,')',' FOR CELL ',a)"
! ------------------------------------------------------------------------------
    !
    ! -- check stress period data
    do i = 1, this%nbound
      node = this%nodelist(i)
      bt = this%dis%bot(node)
      ! -- accumulate errors
      if (this%bound(1, i) < bt .and. this%icelltype(node) /= 0) then
        call this%dis%noder_to_string(node, nodestr)
        write (errmsg, fmt=fmtchderr) i, this%bound(1, i), bt, trim(nodestr)
        call store_error(errmsg)
      end if
    end do
    !
    !write summary of chd package error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- return
    return
  end subroutine chd_ck

  subroutine chd_fc(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! chd_fc -- Override bnd_fc and do nothing
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(ChdType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
! --------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine chd_fc

  subroutine chd_cq(this, x, flowja, iadv)
! ******************************************************************************
! chd_cq -- Calculate constant head flow.  This method overrides bnd_cq().
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(ChdType), intent(inout) :: this
    real(DP), dimension(:), intent(in) :: x
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    integer(I4B), optional, intent(in) :: iadv
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: ipos
    integer(I4B) :: node
    integer(I4B) :: n2
    integer(I4B) :: idiag
    real(DP) :: rate
    real(DP) :: ratein, rateout
    real(DP) :: q
! ------------------------------------------------------------------------------
    !
    ! -- If no boundaries, skip flow calculations.
    if (this%nbound > 0) then
      !
      ! -- Loop through each boundary calculating flow.
      do i = 1, this%nbound
        node = this%nodelist(i)
        idiag = this%dis%con%ia(node)
        rate = DZERO
        ratein = DZERO
        rateout = DZERO
        !
        ! -- Calculate the flow rate into the cell.
        do ipos = this%dis%con%ia(node) + 1, &
          this%dis%con%ia(node + 1) - 1
          q = flowja(ipos)
          rate = rate - q
          ! -- only accumulate chin and chout for active
          !    connected cells
          n2 = this%dis%con%ja(ipos)
          if (this%ibound(n2) > 0) then
            if (q < DZERO) then
              ratein = ratein - q
            else
              rateout = rateout + q
            end if
          end if
        end do
        !
        ! -- For chd, store total flow in rhs so it is available for other
        !    calculations
        this%rhs(i) = -rate
        this%hcof(i) = DZERO
        !
        ! -- Save simulated value to simvals array.
        this%simvals(i) = rate
        this%ratechdin(i) = ratein
        this%ratechdout(i) = rateout
        flowja(idiag) = flowja(idiag) + rate
        !
      end do
      !
    end if
    !
    ! -- return
    return
  end subroutine chd_cq

  subroutine chd_bd(this, model_budget)
    ! -- add package ratin/ratout to model budget
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    class(ChdType) :: this
    type(BudgetType), intent(inout) :: model_budget
    real(DP) :: ratin
    real(DP) :: ratout
    real(DP) :: dum
    integer(I4B) :: isuppress_output
    isuppress_output = 0
    call rate_accumulator(this%ratechdin(1:this%nbound), ratin, dum)
    call rate_accumulator(this%ratechdout(1:this%nbound), ratout, dum)
    call model_budget%addentry(ratin, ratout, delt, this%text, &
                               isuppress_output, this%packName)
  end subroutine chd_bd

  subroutine chd_da(this)
! ******************************************************************************
! chd_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(ChdType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate parent package
    call this%BndType%bnd_da()
    !
    ! -- arrays
    call mem_deallocate(this%ratechdin)
    call mem_deallocate(this%ratechdout)
    !
    ! -- return
    return
  end subroutine chd_da

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(ChdType), intent(inout) :: this
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
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'HEAD'
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- return
    return
  end subroutine define_listlabel

  ! -- Procedures related to observations

  logical function chd_obs_supported(this)
! ******************************************************************************
! chd_obs_supported
!   -- Return true because CHD package supports observations.
!   -- Overrides packagetype%_obs_supported()
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(ChdType) :: this
! ------------------------------------------------------------------------------
    chd_obs_supported = .true.
    return
  end function chd_obs_supported

  subroutine chd_df_obs(this)
! ******************************************************************************
! chd_df_obs (implements bnd_df_obs)
!   -- Store observation type supported by CHD package.
!   -- Overrides BndType%bnd_df_obs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ChdType) :: this
    ! -- local
    integer(I4B) :: indx
! ------------------------------------------------------------------------------
    call this%obs%StoreObsType('chd', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    return
  end subroutine chd_df_obs

  ! -- Procedure related to time series

  subroutine chd_rp_ts(this)
    ! -- Assign tsLink%Text appropriately for
    !    all time series in use by package.
    !    In CHD package variable HEAD
    !    can be controlled by time series.
    ! -- dummy
    class(ChdType), intent(inout) :: this
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
          tslink%Text = 'HEAD'
        end select
      end if
    end do
    !
    return
  end subroutine chd_rp_ts

end module ChdModule

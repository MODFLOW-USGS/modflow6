module GwtCncModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE, NAMEDBOUNDFLAG, LENFTYPE, &
                             LENPACKAGENAME
  use ObsModule, only: DefaultObsIdProcessor
  use BndModule, only: BndType
  use ObserveModule, only: ObserveType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  !
  implicit none
  !
  private
  public :: cnc_create
  !
  character(len=LENFTYPE) :: ftype = 'CNC'
  character(len=LENPACKAGENAME) :: text = '             CNC'
  !
  type, extends(BndType) :: GwtCncType
    real(DP), dimension(:), pointer, contiguous :: ratecncin => null() !simulated flows into constant conc (excluding other concs)
    real(DP), dimension(:), pointer, contiguous :: ratecncout => null() !simulated flows out of constant conc (excluding to other concs)
  contains
    procedure :: bnd_rp => cnc_rp
    procedure :: bnd_ad => cnc_ad
    procedure :: bnd_ck => cnc_ck
    procedure :: bnd_fc => cnc_fc
    procedure :: bnd_cq => cnc_cq
    procedure :: bnd_bd => cnc_bd
    procedure :: bnd_da => cnc_da
    procedure :: allocate_arrays => cnc_allocate_arrays
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => cnc_obs_supported
    procedure, public :: bnd_df_obs => cnc_df_obs
    ! -- method for time series
    procedure, public :: bnd_rp_ts => cnc_rp_ts
  end type GwtCncType

contains

  subroutine cnc_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! cnc_create -- Create a New Constant Concentration Package
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
    type(GwtCncType), pointer :: cncobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate (cncobj)
    packobj => cncobj
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
    !
    ! -- return
    return
  end subroutine cnc_create

  subroutine cnc_allocate_arrays(this, nodelist, auxvar)
! ******************************************************************************
! allocate_scalars -- allocate arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtCncType) :: this
    integer(I4B), dimension(:), pointer, contiguous, optional :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, optional :: auxvar
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_arrays()
    !
    ! -- allocate ratecncex
    call mem_allocate(this%ratecncin, this%maxbound, 'RATECNCIN', this%memoryPath)
    call mem_allocate(this%ratecncout, this%maxbound, 'RATECNCOUT', &
                      this%memoryPath)
    do i = 1, this%maxbound
      this%ratecncin(i) = DZERO
      this%ratecncout(i) = DZERO
    end do
    !
    ! -- return
    return
  end subroutine cnc_allocate_arrays

  subroutine cnc_rp(this)
! ******************************************************************************
! cnc_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SimModule, only: store_error
    implicit none
    class(GwtCncType), intent(inout) :: this
    integer(I4B) :: i, node, ibd, ierr
    character(len=30) :: nodestr
! ------------------------------------------------------------------------------
    !
    ! -- Reset previous CNCs to active cell
    do i = 1, this%nbound
      node = this%nodelist(i)
      this%ibound(node) = this%ibcnum
    end do
    !
    ! -- Call the parent class read and prepare
    call this%BndType%bnd_rp()
    !
    ! -- Set ibound to -(ibcnum + 1) for constant concentration cells
    ierr = 0
    do i = 1, this%nbound
      node = this%nodelist(i)
      ibd = this%ibound(node)
      if (ibd < 0) then
        call this%dis%noder_to_string(node, nodestr)
        call store_error('Error.  Cell is already a constant concentration: ' &
                         //trim(adjustl(nodestr)))
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
  end subroutine cnc_rp

  subroutine cnc_ad(this)
! ******************************************************************************
! cnc_ad -- Advance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtCncType) :: this
    ! -- local
    integer(I4B) :: i, node
    real(DP) :: cb
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Advance the time series
    call this%TsManager%ad()
    !
    ! -- Process each entry in the constant concentration cell list
    do i = 1, this%nbound
      node = this%nodelist(i)
      cb = this%bound(1, i)
      this%xnew(node) = cb
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
  end subroutine cnc_ad

  subroutine cnc_ck(this)
! ******************************************************************************
! cnc_ck -- Check cnc boundary condition data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_unit
    ! -- dummy
    class(GwtCncType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=30) :: nodestr
    integer(I4B) :: i
    integer(I4B) :: node
    ! -- formats
    character(len=*), parameter :: fmtcncerr = &
      &"('CNC BOUNDARY ',i0,' CONC (',g0,') IS LESS THAN ZERO FOR CELL', a)"
! ------------------------------------------------------------------------------
    !
    ! -- check stress period data
    do i = 1, this%nbound
      node = this%nodelist(i)
      ! -- accumulate errors
      if (this%bound(1, i) < DZERO) then
        call this%dis%noder_to_string(node, nodestr)
        write (errmsg, fmt=fmtcncerr) i, this%bound(1, i), trim(nodestr)
        call store_error(errmsg)
      end if
    end do
    !
    ! -- write summary of cnc package error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- return
    return
  end subroutine cnc_ck

  subroutine cnc_fc(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! cnc_fc -- Override bnd_fc and do nothing
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(GwtCncType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
! --------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine cnc_fc

  subroutine cnc_cq(this, x, flowja, iadv)
! ******************************************************************************
! cnc_cq -- Calculate constant concenration flow.  This method overrides bnd_cq().
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtCncType), intent(inout) :: this
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
        ! -- For CNC, store total flow in rhs so it is available for other
        !    calculations
        this%rhs(i) = -rate
        this%hcof(i) = DZERO
        !
        ! -- Save simulated value to simvals array.
        this%simvals(i) = rate
        this%ratecncin(i) = ratein
        this%ratecncout(i) = rateout
        flowja(idiag) = flowja(idiag) + rate
        !
      end do
      !
    end if
    !
    ! -- return
    return
  end subroutine cnc_cq

  subroutine cnc_bd(this, model_budget)
    ! -- add package ratin/ratout to model budget
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    class(GwtCncType) :: this
    type(BudgetType), intent(inout) :: model_budget
    real(DP) :: ratin
    real(DP) :: ratout
    real(DP) :: dum
    integer(I4B) :: isuppress_output
    isuppress_output = 0
    call rate_accumulator(this%ratecncin(1:this%nbound), ratin, dum)
    call rate_accumulator(this%ratecncout(1:this%nbound), ratout, dum)
    call model_budget%addentry(ratin, ratout, delt, this%text, &
                               isuppress_output, this%packName)
  end subroutine cnc_bd

  subroutine cnc_da(this)
! ******************************************************************************
! cnc_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtCncType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate parent package
    call this%BndType%bnd_da()
    !
    ! -- arrays
    call mem_deallocate(this%ratecncin)
    call mem_deallocate(this%ratecncout)
    !
    ! -- return
    return
  end subroutine cnc_da

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwtCncType), intent(inout) :: this
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
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'CONCENTRATION'
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- return
    return
  end subroutine define_listlabel

  ! -- Procedures related to observations

  logical function cnc_obs_supported(this)
! ******************************************************************************
! cnc_obs_supported
!   -- Return true because CNC package supports observations.
!   -- Overrides packagetype%_obs_supported()
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtCncType) :: this
! ------------------------------------------------------------------------------
    !
    cnc_obs_supported = .true.
    !
    ! -- return
    return
  end function cnc_obs_supported

  subroutine cnc_df_obs(this)
! ******************************************************************************
! cnc_df_obs (implements bnd_df_obs)
!   -- Store observation type supported by CNC package.
!   -- Overrides BndType%bnd_df_obs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtCncType) :: this
    ! -- local
    integer(I4B) :: indx
! ------------------------------------------------------------------------------
    !
    call this%obs%StoreObsType('cnc', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine cnc_df_obs

  ! -- Procedure related to time series

  subroutine cnc_rp_ts(this)
! ******************************************************************************
! -- Assign tsLink%Text appropriately for
!    all time series in use by package.
!    In CNC package variable CONCENTRATION
!    can be controlled by time series.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtCncType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, nlinks
    type(TimeSeriesLinkType), pointer :: tslink => null()
! ------------------------------------------------------------------------------
    !
    nlinks = this%TsManager%boundtslinks%Count()
    do i = 1, nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        select case (tslink%JCol)
        case (1)
          tslink%Text = 'CONCENTRATION'
        end select
      end if
    end do
    !
    ! -- return
    return
  end subroutine cnc_rp_ts

end module GwtCncModule

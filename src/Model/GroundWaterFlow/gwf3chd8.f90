module ChdModule
  !
  use KindModule,           only: DP, I4B
  use ConstantsModule,      only: DZERO, DONE, NAMEDBOUNDFLAG, LENFTYPE,       &
                             LENPACKAGENAME
  use ObsModule,            only: DefaultObsIdProcessor
  use BndModule,            only: BndType
  use ObserveModule,        only: ObserveType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  !
  implicit none
  !
  private
  public :: chd_create, ChdType
  !
  character(len=LENFTYPE)       :: ftype = 'CHD'
  character(len=LENPACKAGENAME) :: text  = '             CHD'
  !
  type, extends(BndType) :: ChdType
    contains
    procedure :: bnd_rp => chd_rp
    procedure :: bnd_ad => chd_ad
    procedure :: bnd_ck => chd_ck
    procedure :: bnd_fc => chd_fc
    procedure :: bnd_bd => chd_bd
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
    integer(I4B),intent(in) :: id
    integer(I4B),intent(in) :: ibcnum
    integer(I4B),intent(in) :: inunit
    integer(I4B),intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    ! -- local
    type(ChdType), pointer :: chdobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(chdobj)
    packobj => chdobj
    !
    ! -- create name and origin
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
  end subroutine chd_create

  subroutine chd_rp(this)
! ******************************************************************************
! chd_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SimModule, only: ustop, store_error
    implicit none
    class(ChdType), intent(inout) :: this
    integer(I4B) :: i, node, ibd, ierr
    character(len=30) :: nodestr
! ------------------------------------------------------------------------------
    !
    ! -- Reset previous CHDs to active cell
    do i=1,this%nbound
        node = this%nodelist(i)
        this%ibound(node) = this%ibcnum
    enddo
    !
    ! -- Call the parent class read and prepare
    call this%BndType%bnd_rp()
    !
    ! -- Set ibound to -(ibcnum + 1) for constant head cells
    ierr = 0
    do i=1,this%nbound
      node = this%nodelist(i)
      ibd = this%ibound(node)
      if(ibd < 0) then
        call this%dis%noder_to_string(node, nodestr)
        call store_error('Error.  Cell is already a constant head: ' &
                         // trim(adjustl(nodestr)))
        ierr = ierr + 1
      else
        this%ibound(node) = -this%ibcnum
      endif
    enddo
    !
    ! -- Stop if errors detected
    if(ierr > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
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
    enddo
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
    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    ! -- dummy
    class(ChdType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=30) :: nodestr
    integer(I4B) :: i
    integer(I4B) :: node
    real(DP) :: bt
    ! -- formats
    character(len=*), parameter :: fmtchderr = &
      "('CHD BOUNDARY ',i0,' HEAD (',g0,') IS LESS THAN CELL " //            &
      "BOTTOM (',g0,')',' FOR CELL ',a)"
! ------------------------------------------------------------------------------
    !
    ! -- check stress period data
    do i=1,this%nbound
        node=this%nodelist(i)
        bt = this%dis%bot(node)
        ! -- accumulate errors
        if (this%bound(1,i) < bt .and. this%icelltype(node) /= 0) then
          call this%dis%noder_to_string(node, nodestr)
          write(errmsg, fmt=fmtchderr) i, this%bound(1,i), bt, trim(nodestr)
          call store_error(errmsg)
        end if
    end do
    !
    !write summary of chd package error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
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

  subroutine chd_bd(this, x, idvfl, icbcfl, ibudfl, icbcun, iprobs,            &
                    isuppress_output, model_budget, imap, iadv)
! ******************************************************************************
! chd_bd -- Calculate constant head flow budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper, delt
    use ConstantsModule, only: LENBOUNDNAME
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(ChdType) :: this
    real(DP),dimension(:),intent(in) :: x
    integer(I4B), intent(in) :: idvfl
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    integer(I4B), intent(in) :: iprobs
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    integer(I4B), dimension(:), optional, intent(in) :: imap
    integer(I4B), optional, intent(in) :: iadv
    ! -- local
    integer(I4B) :: i, node, ibinun, n2
    real(DP) :: rrate, chin, chout, q
    integer(I4B) :: ibdlbl, naux, ipos
    ! -- for observations
    character(len=LENBOUNDNAME) :: bname
    ! -- formats
    character(len=*), parameter :: fmttkk = &
      "(1X,/1X,A,'   PERIOD ',I0,'   STEP ',I0)"
! ------------------------------------------------------------------------------
    !
    chin = DZERO
    chout = DZERO
    ibdlbl = 0
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
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if(ibinun /= 0) then
      naux = this%naux
      call this%dis%record_srcdst_list_header(this%text, this%name_model,  &
                  this%name_model, this%name_model, this%name, naux,           &
                  this%auxname, ibinun, this%nbound, this%iout)
    endif
    !
    ! -- If no boundaries, skip flow calculations.
    if(this%nbound > 0) then
      !
      ! -- Loop through each boundary calculating flow.
      do i = 1, this%nbound
        node = this%nodelist(i)
        rrate = DZERO
        ! -- assign boundary name
        if (this%inamedbound>0) then
          bname = this%boundname(i)
        else
          bname = ''
        endif
        !
        ! -- Calculate the flow rate into the cell.
        do ipos = this%dis%con%ia(node) + 1, &
                  this%dis%con%ia(node + 1) - 1
          q = this%flowja(ipos)
          rrate = rrate - q
          ! -- only accumulate chin and chout for active
          !    connected cells
          n2 = this%dis%con%ja(ipos)
          if (this%ibound(n2) < 1) cycle
          if (q < DZERO) then
            chin = chin - q
          else
            chout = chout + q
          end if
        end do
        !
        ! -- For chd, store total flow in rhs so it is available for other calculations
        this%rhs(i) = -rrate
        this%hcof(i) = DZERO
        !
        ! -- Print the individual rates if requested(this%iprflow<0)
        if (ibudfl /= 0) then
          if(this%iprflow /= 0) then
            if (ibdlbl == 0) write(this%iout,fmttkk) this%text, kper, kstp
            call this%dis%print_list_entry(i, node, rrate, this%iout, &
                    bname)
            ibdlbl=1
          end if
        end if
        !
        ! -- If saving cell-by-cell flows in list, write flow
        if (ibinun /= 0) then
          n2 = i
          if (present(imap)) n2 = imap(i)
          call this%dis%record_mf6_list_entry(ibinun, node, n2, rrate,      &
                                                  naux, this%auxvar(:,i),       &
                                                  olconv2=.FALSE.)
        end if
        !
        ! -- Save simulated value to simvals array.
        this%simvals(i) = rrate
        !
      end do
      !
    end if
    !
    ! -- Store the rates
    call model_budget%addentry(chin, chout, delt, this%text,                   &
                               isuppress_output, this%name)
    !
    ! -- Save the simulated values to the ObserveType objects
    if (this%obs%npakobs > 0 .and. iprobs > 0) then
      call this%bnd_bd_obs()
    end if
    !
    ! -- return
    return
  end subroutine chd_bd

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
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'HEAD'
    if(this%inamedbound == 1) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    endif
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
  call this%obs%StoreObsType('chd-flow', .true., indx)
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
    do i=1,nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        select case (tslink%JCol)
        case (1)
          tslink%Text = 'HEAD'
        end select
      endif
    enddo
    !
    return
  end subroutine chd_rp_ts

end module ChdModule

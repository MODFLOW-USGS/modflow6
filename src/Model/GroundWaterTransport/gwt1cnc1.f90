module GwtCncModule
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
  public :: cnc_create
  !
  character(len=LENFTYPE)       :: ftype = 'CNC'
  character(len=LENPACKAGENAME) :: text  = '             CNC'
  !
  type, extends(BndType) :: GwtCncType
    contains
    procedure :: bnd_rp => cnc_rp
    procedure :: bnd_ad => cnc_ad
    procedure :: bnd_ck => cnc_ck
    procedure :: bnd_fc => cnc_fc
    procedure :: bnd_bd => cnc_bd
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
    integer(I4B),intent(in) :: id
    integer(I4B),intent(in) :: ibcnum
    integer(I4B),intent(in) :: inunit
    integer(I4B),intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    ! -- local
    type(GwtCncType), pointer :: cncobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(cncobj)
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

  subroutine cnc_rp(this)
! ******************************************************************************
! cnc_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SimModule, only: ustop, store_error
    implicit none
    class(GwtCncType), intent(inout) :: this
    integer(I4B) :: i, node, ibd, ierr
    character(len=30) :: nodestr
! ------------------------------------------------------------------------------
    !
    ! -- Reset previous CNCs to active cell
    do i=1,this%nbound
        node = this%nodelist(i)
        this%ibound(node) = this%ibcnum
    enddo
    !
    ! -- Call the parent class read and prepare
    call this%BndType%bnd_rp()
    !
    ! -- Set ibound to -(ibcnum + 1) for constant concentration cells
    ierr = 0
    do i=1,this%nbound
      node = this%nodelist(i)
      ibd = this%ibound(node)
      if(ibd < 0) then
        call this%dis%noder_to_string(node, nodestr)
        call store_error('Error.  Cell is already a constant concentration: ' &
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
    enddo
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
    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    ! -- dummy
    class(GwtCncType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=30) :: nodestr
    integer(I4B) :: i
    integer(I4B) :: node
    ! -- formats
    character(len=*), parameter :: fmtcncerr = &
      "('CNC BOUNDARY ',i0,' CONC (',g0,') IS LESS THAN ZERO FOR CELL', a)"
! ------------------------------------------------------------------------------
    !
    ! -- check stress period data
    do i = 1, this%nbound
        node = this%nodelist(i)
        ! -- accumulate errors
        if (this%bound(1,i) < DZERO) then
          call this%dis%noder_to_string(node, nodestr)
          write(errmsg, fmt=fmtcncerr) i, this%bound(1,i), trim(nodestr)
          call store_error(errmsg)
        end if
    end do
    !
    ! -- write summary of cnc package error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
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

  subroutine cnc_bd(this, x, idvfl, icbcfl, ibudfl, icbcun, iprobs,            &
                    isuppress_output, model_budget, imap, iadv)
! ******************************************************************************
! cnc_bd -- Calculate constant concentration flow budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt, kstp, kper
    use ConstantsModule, only: LENBOUNDNAME
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(GwtCncType) :: this
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
    character(len=20) :: nodestr
    integer(I4B) :: nodeu
    integer(I4B) :: i, node, ibinun, n2
    real(DP) :: rrate, chin, chout, q
    integer(I4B) :: ibdlbl, naux, ipos
    ! -- for observations
    character(len=LENBOUNDNAME) :: bname
    ! -- formats
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
      call this%dis%record_srcdst_list_header(this%text, this%name_model,      &
                  this%name_model, this%name_model, this%packName, naux,           &
                  this%auxname, ibinun, this%nbound, this%iout)
    endif
    !
    ! -- If no boundaries, skip flow calculations.
    if(this%nbound > 0) then
      !
      ! -- reset size of table
      if (this%iprflow /= 0) then
        call this%outputtab%set_kstpkper(kstp, kper)
        call this%outputtab%set_maxbound(this%nbound)
      end if
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
        ! -- For cnc, store total flow in rhs so it is available for other 
        !    calculations
        this%rhs(i) = -rrate
        this%hcof(i) = DZERO
        !
        ! -- Print the individual rates if requested(this%iprflow<0)
        if (ibudfl /= 0) then
          if(this%iprflow /= 0) then
            !
            ! -- set nodestr and write outputtab table
            nodeu = this%dis%get_nodeuser(node)
            call this%dis%nodeu_to_string(nodeu, nodestr)
            call this%outputtab%print_list_entry(i, nodestr, rrate, bname)
          end if
        end if
        !
        ! -- If saving cell-by-cell flows in list, write flow
        if (ibinun /= 0) then
          n2 = i
          if (present(imap)) n2 = imap(i)
          call this%dis%record_mf6_list_entry(ibinun, node, n2, rrate,         &
                                                  naux, this%auxvar(:,i),      &
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
                               isuppress_output, this%packName)
    !
    ! -- Save the simulated values to the ObserveType objects
    if (this%obs%npakobs > 0 .and. iprobs > 0) then
      call this%bnd_bd_obs()
    end if
    !
    ! -- return
    return
  end subroutine cnc_bd

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
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'CONCENTRATION'
    if(this%inamedbound == 1) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    endif
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
    do i=1,nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        select case (tslink%JCol)
        case (1)
          tslink%Text = 'CONCENTRATION'
        end select
      endif
    enddo
    !
    ! -- return
    return
  end subroutine cnc_rp_ts

end module GwtCncModule

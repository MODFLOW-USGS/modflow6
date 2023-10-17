module TspSdvModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE, NAMEDBOUNDFLAG, LENFTYPE, &
                             LENPACKAGENAME, LENVARNAME
  use ObsModule, only: DefaultObsIdProcessor
  use BndModule, only: BndType
  use ObserveModule, only: ObserveType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use InputOutputModule, only: str_pad_left
  use MatrixBaseModule
  !
  implicit none
  !
  private
  public :: sdv_create
  !
  character(len=LENFTYPE) :: ftype = '   '
  character(len=LENPACKAGENAME) :: text = ''
  !
  type, extends(BndType) :: TspSdvType

    real(DP), dimension(:), pointer, contiguous :: ratesdvin => null() !simulated flows into constant conc (excluding other concs)
    real(DP), dimension(:), pointer, contiguous :: ratesdvout => null() !simulated flows out of constant conc (excluding to other concs)
    character(len=LENVARNAME) :: depvartype = '' !< stores string of dependent variable type, depending on model type

  contains

    procedure :: bnd_rp => sdv_rp
    procedure :: bnd_ad => sdv_ad
    procedure :: bnd_ck => sdv_ck
    procedure :: bnd_fc => sdv_fc
    procedure :: bnd_cq => sdv_cq
    procedure :: bnd_bd => sdv_bd
    procedure :: bnd_da => sdv_da
    procedure :: allocate_arrays => sdv_allocate_arrays
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => sdv_obs_supported
    procedure, public :: bnd_df_obs => sdv_df_obs
    ! -- method for time series
    procedure, public :: bnd_rp_ts => sdv_rp_ts

  end type TspSdvType

contains

  !> @brief Create a new constant concentration or temperature package
  !!
  !! Routine points packobj to the newly created package
  !<
  subroutine sdv_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        depvartype, ftype)
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B), intent(in) :: id
    integer(I4B), intent(in) :: ibcnum
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    character(len=LENVARNAME), intent(in) :: depvartype
    character(len=LENFTYPE), intent(in) :: ftype
    ! -- local
    type(TspSdvType), pointer :: sdvobj
    character(len=LENPACKAGENAME) :: text
    !
    ! -- allocate the object and assign values to object variables
    allocate (sdvobj)
    packobj => sdvobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    text = str_pad_left(ftype, LENPACKAGENAME)
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
    ! -- Store the appropriate label based on the dependent variable
    sdvobj%depvartype = depvartype
    !
    ! -- Return
    return
  end subroutine sdv_create

  !> @brief Allocate arrays specific to the constant concentration/tempeature
  !! package.
  !<
  subroutine sdv_allocate_arrays(this, nodelist, auxvar)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(TspSdvType) :: this
    integer(I4B), dimension(:), pointer, contiguous, optional :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, optional :: auxvar
    ! -- local
    integer(I4B) :: i
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_arrays()
    !
    ! -- allocate ratesdvex
    call mem_allocate(this%ratesdvin, this%maxbound, 'RATESDVIN', this%memoryPath)
    call mem_allocate(this%ratesdvout, this%maxbound, 'RATESDVOUT', &
                      this%memoryPath)
    do i = 1, this%maxbound
      this%ratesdvin(i) = DZERO
      this%ratesdvout(i) = DZERO
    end do
    !
    ! -- Return
    return
  end subroutine sdv_allocate_arrays

  !> @brief Constant concentration/temperature read and prepare (rp) routine
  !<
  subroutine sdv_rp(this)
    use SimModule, only: store_error
    use InputOutputModule, only: lowcase
    implicit none
    class(TspSdvType), intent(inout) :: this
    integer(I4B) :: i, node, ibd, ierr
    character(len=30) :: nodestr
    character(len=LENVARNAME) :: dvtype
    !
    ! -- Reset previous CNCs (or CNTs) to active cell
    do i = 1, this%nbound
      node = this%nodelist(i)
      this%ibound(node) = this%ibcnum
    end do
    !
    ! -- Call the parent class read and prepare
    call this%BndType%bnd_rp()
    !
    ! -- Set ibound to -(ibcnum + 1) for constant concentration/temperature cells
    ierr = 0
    do i = 1, this%nbound
      node = this%nodelist(i)
      ibd = this%ibound(node)
      if (ibd < 0) then
        call this%dis%noder_to_string(node, nodestr)
        dvtype = trim(this%depvartype)
        call lowcase(dvtype)
        call store_error('Cell is already a constant ' &
                         //dvtype//': '//trim(adjustl(nodestr)))
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
    ! -- Return
    return
  end subroutine sdv_rp

  !> @brief Constant concentration/temperature package advance routine
  !!
  !! Add package connections to matrix
  !<
  subroutine sdv_ad(this)
    ! -- modules
    ! -- dummy
    class(TspSdvType) :: this
    ! -- local
    integer(I4B) :: i, node
    real(DP) :: cb
    ! -- formats
    !
    ! -- Advance the time series
    call this%TsManager%ad()
    !
    ! -- Process each entry in the constant concentration/temperature cell list
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
    ! -- Return
    return
  end subroutine sdv_ad

  !> @brief Check constant concentration/temperature boundary condition data
  !<
  subroutine sdv_ck(this)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_unit
    ! -- dummy
    class(TspSdvType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=30) :: nodestr
    integer(I4B) :: i
    integer(I4B) :: node
    ! -- formats
    character(len=*), parameter :: fmtsdverr = &
      &"('Specified dependent variable (SDV) boundary ',i0, &
      &' conc (',g0,') is less than zero for cell', a)"
    !
    ! -- check stress period data
    do i = 1, this%nbound
      node = this%nodelist(i)
      ! -- accumulate errors
      if (this%bound(1, i) < DZERO) then
        call this%dis%noder_to_string(node, nodestr)
        write (errmsg, fmt=fmtsdverr) i, this%bound(1, i), trim(nodestr)
        call store_error(errmsg)
      end if
    end do
    !
    ! -- write summary of SDV package error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Return
    return
  end subroutine sdv_ck

  !> @brief Override bnd_fc and do nothing
  !!
  !! For constant concentration/temperature boundary type, the call to bnd_fc
  !! needs to be overwritten to prevent logic found therein from being applied
  !<
  subroutine sdv_fc(this, rhs, ia, idxglo, matrix_sln)
    ! -- dummy
    class(TspSdvType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    class(MatrixBaseType), pointer :: matrix_sln
    ! -- local
    !
    ! -- Return
    return
  end subroutine sdv_fc

  !> @brief Calculate flow associated with constant concentration/temperature
  !! boundary
  !!
  !! This method overrides bnd_cq()
  !<
  subroutine sdv_cq(this, x, flowja, iadv)
    ! -- modules
    ! -- dummy
    class(TspSdvType), intent(inout) :: this
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
          ! flowja is already in terms of energy for heat transport
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
        ! -- For SDV, store total flow in rhs so it is available for other
        !    calculations
        this%rhs(i) = -rate
        this%hcof(i) = DZERO
        !
        ! -- Save simulated value to simvals array.
        this%simvals(i) = rate
        this%ratesdvin(i) = ratein
        this%ratesdvout(i) = rateout
        flowja(idiag) = flowja(idiag) + rate
        !
      end do
      !
    end if
    !
    ! -- Return
    return
  end subroutine sdv_cq

  !> @brief Add package ratin/ratout to model budget
  !<
  subroutine sdv_bd(this, model_budget)
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    ! -- dummy
    class(TspSdvType) :: this
    type(BudgetType), intent(inout) :: model_budget
    ! -- local
    real(DP) :: ratin
    real(DP) :: ratout
    real(DP) :: dum
    integer(I4B) :: isuppress_output
    !
    isuppress_output = 0
    call rate_accumulator(this%ratesdvin(1:this%nbound), ratin, dum)
    call rate_accumulator(this%ratesdvout(1:this%nbound), ratout, dum)
    call model_budget%addentry(ratin, ratout, delt, this%text, &
                               isuppress_output, this%packName)
    !
    ! -- Return
    return
  end subroutine sdv_bd

  !> @brief Deallocate memory
  !!
  !!  Method to deallocate memory for the package.
  !<
  subroutine sdv_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(TspSdvType) :: this
    !
    ! -- Deallocate parent package
    call this%BndType%bnd_da()
    !
    ! -- arrays
    call mem_deallocate(this%ratesdvin)
    call mem_deallocate(this%ratesdvout)
    !
    ! -- Return
    return
  end subroutine sdv_da

  !> @brief Define labels used in list file
  !!
  !! Define the list heading that is written to iout when PRINT_INPUT option
  !! is used.
  !<
  subroutine define_listlabel(this)
    ! -- dummy
    class(TspSdvType), intent(inout) :: this
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
    write (this%listlabel, '(a, a16)') trim(this%listlabel), &
      trim(this%depvartype)
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- Return
    return
  end subroutine define_listlabel

  !> @brief Procedure related to observation processing
  !!
  !! This routine:
  !!   - returns true because the SDV package supports observations,
  !!   - overrides packagetype%_obs_supported()
  logical function sdv_obs_supported(this)
    ! -- dummy
    class(TspSdvType) :: this
    !
    sdv_obs_supported = .true.
    !
    ! -- Return
    return
  end function sdv_obs_supported

  !> @brief Procedure related to observation processing
  !!
  !! This routine:
  !!   - defines observations
  !!   - stores observation types supported by either of the SDV packages 
  !!     (CNC or CNT),
  !!   - overrides BndType%bnd_df_obs
  !<
  subroutine sdv_df_obs(this)
    ! -- dummy
    class(TspSdvType) :: this
    ! -- local
    integer(I4B) :: indx
    !
    call this%obs%StoreObsType(this%filtyp, .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Return
    return
  end subroutine sdv_df_obs

  !> @brief Procedure related to time series
  !!
  !! Assign tsLink%Text appropriately for all time series in use by package.
  !! In SDV package, variable CONCENTRATION or TEMPERATURE can be controlled
  !! by time series.
  !<
  subroutine sdv_rp_ts(this)
    ! -- dummy
    class(TspSdvType), intent(inout) :: this
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
          tslink%Text = trim(this%depvartype)
        end select
      end if
    end do
    !
    ! -- Return
    return
  end subroutine sdv_rp_ts

end module TspSdvModule

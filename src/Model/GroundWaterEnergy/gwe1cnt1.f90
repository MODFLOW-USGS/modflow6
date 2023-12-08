module GweCntModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE, NAMEDBOUNDFLAG, LENFTYPE, &
                             LENPACKAGENAME, LENVARNAME
  use SimVariablesModule, only: errmsg
  use SimModule, only: count_errors, store_error, store_error_filename
  use ObsModule, only: DefaultObsIdProcessor
  use BndModule, only: BndType
  use BndExtModule, only: BndExtType
  use ObserveModule, only: ObserveType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use InputOutputModule, only: str_pad_left
  use MatrixBaseModule
  !
  implicit none
  !
  private
  public :: cnt_create
  !
  character(len=LENFTYPE) :: ftype = 'CNT'
  character(len=LENPACKAGENAME) :: text = '             CNT'
  !
  type, extends(BndExtType) :: GweCntType

    real(DP), dimension(:), pointer, contiguous :: tspvar => null() !< constant temperature array
    real(DP), dimension(:), pointer, contiguous :: ratecntin => null() !< simulated flows into constant temperature (excluding other CNTs)
    real(DP), dimension(:), pointer, contiguous :: ratecntout => null() !< simulated flows out of constant temperature (excluding to other CNTs)
    character(len=LENVARNAME) :: depvartype = '' !< stores string of dependent variable type, depending on model type
  contains
    procedure :: bnd_rp => cnt_rp
    procedure :: bnd_ad => cnt_ad
    procedure :: bnd_ck => cnt_ck
    procedure :: bnd_fc => cnt_fc
    procedure :: bnd_cq => cnt_cq
    procedure :: bnd_bd => cnt_bd
    procedure :: bnd_da => cnt_da
    procedure :: allocate_arrays => cnt_allocate_arrays
    procedure :: define_listlabel
    procedure :: bound_value => cnt_bound_value
    procedure :: temp_mult
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => cnt_obs_supported
    procedure, public :: bnd_df_obs => cnt_df_obs
    ! -- method for time series
    procedure, public :: bnd_rp_ts => cnt_rp_ts
  end type GweCntType

contains

  !> @brief Create a new constant temperature package
  !!
  !! Routine points packobj to the newly created package
  !<
  subroutine cnt_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        depvartype, mempath)
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B), intent(in) :: id
    integer(I4B), intent(in) :: ibcnum
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    character(len=LENVARNAME), intent(in) :: depvartype
    character(len=*), intent(in) :: mempath
    ! -- local
    type(GweCntType), pointer :: cntobj
    !
    ! -- allocate the object and assign values to object variables
    allocate (cntobj)
    packobj => cntobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype, mempath)
    packobj%text = text
    !
    ! -- allocate scalars
    call cntobj%allocate_scalars()
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
    cntobj%depvartype = depvartype
    !
    ! -- Return
    return
  end subroutine cnt_create

  !> @brief Allocate arrays specific to the constant temperature package
  !<
  subroutine cnt_allocate_arrays(this, nodelist, auxvar)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr, mem_checkin
    ! -- dummy
    class(GweCntType) :: this
    integer(I4B), dimension(:), pointer, contiguous, optional :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, optional :: auxvar
    ! -- local
    integer(I4B) :: i
    !
    ! -- call standard BndType allocate scalars
    call this%BndExtType%allocate_arrays(nodelist, auxvar)
    !
    ! -- allocate ratecntex
    call mem_allocate(this%ratecntin, this%maxbound, 'RATECNTIN', this%memoryPath)
    call mem_allocate(this%ratecntout, this%maxbound, 'RATECNTOUT', &
                      this%memoryPath)
    do i = 1, this%maxbound
      this%ratecntin(i) = DZERO
      this%ratecntout(i) = DZERO
    end do
    ! -- set constant head array input context pointer
    call mem_setptr(this%tspvar, 'TSPVAR', this%input_mempath)
    !
    ! -- checkin constant head array input context pointer
    call mem_checkin(this%tspvar, 'TSPVAR', this%memoryPath, &
                     'TSPVAR', this%input_mempath)
    !
    !
    ! -- Return
    return
  end subroutine cnt_allocate_arrays

  !> @brief Constant temperature read and prepare (rp) routine
  !<
  subroutine cnt_rp(this)
    ! -- modules
    use SimModule, only: store_error
    use InputOutputModule, only: lowcase
    implicit none
    ! -- dummy
    class(GweCntType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, node, ibd, ierr
    character(len=30) :: nodestr
    character(len=LENVARNAME) :: dvtype
    !
    ! -- Reset previous CNTs to active cell
    do i = 1, this%nbound
      node = this%nodelist(i)
      this%ibound(node) = this%ibcnum
    end do
    !
    ! -- Call the parent class read and prepare
    call this%BndExtType%bnd_rp()
    !
    ! -- Set ibound to -(ibcnum + 1) for constant temperature cells
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
      call store_error_filename(this%input_fname)
    end if
    !
    ! -- Write the list to iout if requested
    if (this%iprpak /= 0) then
      call this%write_list()
    end if
    !
    ! -- Return
    return
  end subroutine cnt_rp

  !> @brief Constant temperature package advance routine
  !!
  !! Add package connections to matrix
  !<
  subroutine cnt_ad(this)
    ! -- dummy
    class(GweCntType) :: this
    ! -- local
    integer(I4B) :: i, node
    real(DP) :: cb
    !
    ! -- Advance the time series
    call this%TsManager%ad()
    !
    ! -- Process each entry in the constant temperature cell list
    do i = 1, this%nbound
      node = this%nodelist(i)
      cb = this%temp_mult(i)
      !
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
  end subroutine cnt_ad

  !> @brief Check constant temperature boundary condition data
  !<
  subroutine cnt_ck(this)
    ! -- dummy
    class(GweCntType), intent(inout) :: this
    ! -- local
    character(len=30) :: nodestr
    integer(I4B) :: i
    integer(I4B) :: node
    ! -- formats
    character(len=*), parameter :: fmtcnterr = &
      &"('Specified dependent variable boundary ',i0, &
      &' temperature (',g0,') is less than zero for cell', a)"
    !
    ! -- check stress period data
    do i = 1, this%nbound
      node = this%nodelist(i)
      ! -- accumulate errors
      if (this%temp_mult(i) < DZERO) then
        call this%dis%noder_to_string(node, nodestr)
        write (errmsg, fmt=fmtcnterr) i, this%tspvar(i), trim(nodestr)
        call store_error(errmsg)
      end if
    end do
    !
    ! -- write summary of cnt package error messages
    if (count_errors() > 0) then
      call store_error_filename(this%input_fname)
    end if
    !
    ! -- Return
    return
  end subroutine cnt_ck

  !> @brief Override bnd_fc and do nothing
  !!
  !! For constant temperature boundary type, the call to bnd_fc needs to be
  !! overwritten to prevent logic found in bnd from being executed
  !<
  subroutine cnt_fc(this, rhs, ia, idxglo, matrix_sln)
    ! -- dummy
    class(GweCntType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    class(MatrixBaseType), pointer :: matrix_sln
    !
    ! -- Return
    return
  end subroutine cnt_fc

  !> @brief Calculate flow associated with constant temperature boundary
  !!
  !! This method overrides bnd_cq()
  !<
  subroutine cnt_cq(this, x, flowja, iadv)
    ! -- dummy
    class(GweCntType), intent(inout) :: this
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
          q = flowja(ipos)
          rate = rate - q
          ! -- Only accumulate chin and chout for active
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
        ! -- For CNT, store total flow in rhs so it is available for other
        !    calculations
        this%rhs(i) = -rate
        this%hcof(i) = DZERO
        !
        ! -- Save simulated value to simvals array.
        this%simvals(i) = rate
        this%ratecntin(i) = ratein
        this%ratecntout(i) = rateout
        flowja(idiag) = flowja(idiag) + rate
        !
      end do
      !
    end if
    !
    ! -- Return
    return
  end subroutine cnt_cq

  !> @brief Add package ratin/ratout to model budget
  !<
  subroutine cnt_bd(this, model_budget)
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    ! -- dummy
    class(GweCntType) :: this
    ! -- local
    type(BudgetType), intent(inout) :: model_budget
    real(DP) :: ratin
    real(DP) :: ratout
    real(DP) :: dum
    integer(I4B) :: isuppress_output
    !
    isuppress_output = 0
    call rate_accumulator(this%ratecntin(1:this%nbound), ratin, dum)
    call rate_accumulator(this%ratecntout(1:this%nbound), ratout, dum)
    call model_budget%addentry(ratin, ratout, delt, this%text, &
                               isuppress_output, this%packName)
    !
    ! -- Return
    return
  end subroutine cnt_bd

  !> @brief Deallocate memory
  !!
  !!  Method to deallocate memory for the package.
  !<
  subroutine cnt_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GweCntType) :: this
    !
    ! -- Deallocate parent package
    call this%BndExtType%bnd_da()
    !
    ! -- arrays
    call mem_deallocate(this%ratecntin)
    call mem_deallocate(this%ratecntout)
    call mem_deallocate(this%tspvar, 'TSPVAR', this%memoryPath)
    !
    ! -- Return
    return
  end subroutine cnt_da

  !> @brief Define labels used in list file
  !!
  !! Define the list heading that is written to iout when PRINT_INPUT option
  !! is used.
  !<
  subroutine define_listlabel(this)
    ! -- dummy
    class(GweCntType), intent(inout) :: this
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
  logical function cnt_obs_supported(this)
    ! -- dummy
    class(GweCntType) :: this
    !
    cnt_obs_supported = .true.
    !
    ! -- Return
    return
  end function cnt_obs_supported

  !> @brief Procedure related to observation processing
  !!
  !! This routine:
  !!   - defines observations
  !!   - stores observation types supported by either of the SDV packages
  !!     (CNT or CNT),
  !!   - overrides BndExtType%bnd_df_obs
  !<
  subroutine cnt_df_obs(this)
    ! -- dummy
    class(GweCntType) :: this
    ! -- local
    integer(I4B) :: indx
    !
    call this%obs%StoreObsType(this%filtyp, .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Return
    return
  end subroutine cnt_df_obs

  ! -- Procedure related to time series

  !> @brief Procedure related to time series
  !!
  !! Assign tsLink%Text appropriately for all time series in use by package.
  !! For the constant temperature packages, the dependent variable can also be
  !! controlled by a time series.
  !<
  subroutine cnt_rp_ts(this)
    ! -- dummy
    class(GweCntType), intent(inout) :: this
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
  end subroutine cnt_rp_ts

  !> @brief Apply auxiliary multiplier to specified temperature if
  !< appropriate
  function temp_mult(this, row) result(temp)
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GweCntType), intent(inout) :: this !< BndExtType object
    integer(I4B), intent(in) :: row
    ! -- result
    real(DP) :: temp
    !
    if (this%iauxmultcol > 0) then
      temp = this%tspvar(row) * this%auxvar(this%iauxmultcol, row)
    else
      temp = this%tspvar(row)
    end if
    !
    ! -- Return
    return
  end function temp_mult

  !> @ brief Return a bound value
  !!
  !!  Return a bound value associated with an ncolbnd index and row.
  !<
  function cnt_bound_value(this, col, row) result(bndval)
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy variables
    class(GweCntType), intent(inout) :: this !< BndExtType object
    integer(I4B), intent(in) :: col
    integer(I4B), intent(in) :: row
    ! -- result
    real(DP) :: bndval
    !
    select case (col)
    case (1)
      bndval = this%temp_mult(row)
    case default
      write (errmsg, '(3a)') 'Programming error. ', &
               & adjustl(trim(this%filtyp)), ' bound value requested column '&
               &'outside range of ncolbnd (1).'
      call store_error(errmsg)
      call store_error_filename(this%input_fname)
    end select
    !
    ! -- Return
    return
  end function cnt_bound_value

end module GweCntModule

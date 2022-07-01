!> @brief This module contains the WEL package methods
!!
!! This module contains the overridden methods for the standard WEL package.
!! Several methods need to be overridden because of the AUTO_FLOW_REDUCE
!! option. Overridden methods include:
!!   - bnd_cf (AUTO_FLOW_REDUCE)
!!   - bnd_fc (AUTO_FLOW_REDUCE)
!!   - bnd_fn (AUTO_FLOW_REDUCE Newton-Raphson terms)
!!   - bnd_ot_package_flows (write AUTO_FLOW_REDUCE terms to csv file)
!!   - bnd_da (deallocate AUTO_FLOW_REDUCE variables)
!!   - bnd_bd_obs (wel-reduction observation added)
!!
!<

module WelModule
  ! -- modules used by WelModule methods
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DEM1, DONE, LENFTYPE, DNODATA, MAXCHARLEN
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use MemoryHelperModule, only: create_mem_path
  use BndModule, only: BndType
  use ObsModule, only: DefaultObsIdProcessor
  use SmoothingModule, only: sQSaturation, sQSaturationDerivative
  use ObserveModule, only: ObserveType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use BlockParserModule, only: BlockParserType
  use InputOutputModule, only: GetUnit, openfile
  !
  implicit none
  !
  private
  public :: wel_create
  !
  character(len=LENFTYPE) :: ftype = 'WEL' !< package ftype
  character(len=16) :: text = '             WEL' !< package flow text string
  !
  type, extends(BndType) :: WelType
    integer(I4B), pointer :: iflowred => null() !< flag indicating if the AUTO_FLOW_REDUCE option is active
    real(DP), pointer :: flowred => null() !< AUTO_FLOW_REDUCE variable
    integer(I4B), pointer :: ioutafrcsv => null() !< unit number for CSV output file containing wells with reduced puping rates
  contains
    procedure :: allocate_scalars => wel_allocate_scalars
    procedure :: bnd_options => wel_options
    procedure :: bnd_cf => wel_cf
    procedure :: bnd_fc => wel_fc
    procedure :: bnd_fn => wel_fn
    procedure :: bnd_da => wel_da
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => wel_obs_supported
    procedure, public :: bnd_df_obs => wel_df_obs
    procedure, public :: bnd_bd_obs => wel_bd_obs
    ! -- methods for time series
    procedure, public :: bnd_rp_ts => wel_rp_ts
    ! -- afr
    procedure, private :: wel_afr_csv_init
    procedure, private :: wel_afr_csv_write
  end type weltype

contains

  !> @ brief Create a new package object
  !!
  !!  Create a new WEL Package object
  !!
  !<
  subroutine wel_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
    ! -- dummy variables
    class(BndType), pointer :: packobj !< pointer to default package type
    integer(I4B), intent(in) :: id !< package id
    integer(I4B), intent(in) :: ibcnum !< boundary condition number
    integer(I4B), intent(in) :: inunit !< unit number of WEL package input file
    integer(I4B), intent(in) :: iout !< unit number of model listing file
    character(len=*), intent(in) :: namemodel !< model name
    character(len=*), intent(in) :: pakname !< package name
    ! -- local variables
    type(WelType), pointer :: welobj
    !
    ! -- allocate the object and assign values to object variables
    allocate (welobj)
    packobj => welobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call welobj%allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()

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
  end subroutine wel_create

  !> @ brief Deallocate package memory
    !!
    !!  Deallocate WEL package scalars and arrays.
    !!
  !<
  subroutine wel_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy variables
    class(WelType) :: this !< WelType object
    !
    ! -- Deallocate parent package
    call this%BndType%bnd_da()
    !
    ! -- scalars
    call mem_deallocate(this%iflowred)
    call mem_deallocate(this%flowred)
    call mem_deallocate(this%ioutafrcsv)
    !
    ! -- return
    return
  end subroutine wel_da

  !> @ brief Allocate scalars
    !!
    !! Allocate and initialize scalars for the WEL package. The base model
    !! allocate scalars method is also called.
    !!
  !<
  subroutine wel_allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy variables
    class(WelType) :: this !< WelType object
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- allocate the object and assign values to object variables
    call mem_allocate(this%iflowred, 'IFLOWRED', this%memoryPath)
    call mem_allocate(this%flowred, 'FLOWRED', this%memoryPath)
    call mem_allocate(this%ioutafrcsv, 'IOUTAFRCSV', this%memoryPath)
    !
    ! -- Set values
    this%iflowred = 0
    this%ioutafrcsv = 0
    this%flowred = DZERO
    !
    ! -- return
    return
  end subroutine wel_allocate_scalars

  !> @ brief Read additional options for package
    !!
    !!  Read additional options for WEL package.
    !!
  !<
  subroutine wel_options(this, option, found)
    ! -- modules
    use InputOutputModule, only: urword
    ! -- dummy variables
    class(WelType), intent(inout) :: this !< WelType object
    character(len=*), intent(inout) :: option !< option keyword string
    logical, intent(inout) :: found !< boolean indicating if option found
    ! -- local variables
    real(DP) :: r
    character(len=MAXCHARLEN) :: fname
    character(len=MAXCHARLEN) :: keyword
    ! -- formats
    character(len=*), parameter :: fmtflowred = &
      &"(4x, 'AUTOMATIC FLOW REDUCTION OF WELLS IMPLEMENTED.')"
    character(len=*), parameter :: fmtflowredv = &
      &"(4x, 'AUTOMATIC FLOW REDUCTION FRACTION (',g15.7,').')"
    !
    ! -- Check for 'AUTO_FLOW_REDUCE' and set this%iflowred
    select case (option)
    case ('AUTO_FLOW_REDUCE')
      this%iflowred = 1
      r = this%parser%GetDouble()
      if (r <= DZERO) then
        r = DEM1
      else if (r > DONE) then
        r = DONE
      end if
      this%flowred = r
      !
      ! -- Write option and return with found set to true
      if (this%iflowred > 0) &
        write (this%iout, fmtflowred)
      write (this%iout, fmtflowredv) this%flowred
      found = .true.
    case ('AUTO_FLOW_REDUCE_CSV')
      call this%parser%GetStringCaps(keyword)
      if (keyword == 'FILEOUT') then
        call this%parser%GetString(fname)
        call this%wel_afr_csv_init(fname)
      else
        call store_error('OPTIONAL AUTO_FLOW_REDUCE_CSV KEYWORD MUST BE &
          &FOLLOWED BY FILEOUT')
      end if
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
  end subroutine wel_options

  !> @ brief Formulate the package hcof and rhs terms.
    !!
    !!  Formulate the hcof and rhs terms for the WEL package that will be
    !!  added to the coefficient matrix and right-hand side vector.
    !!
  !<
  subroutine wel_cf(this, reset_mover)
    ! -- dummy variables
    class(WelType) :: this !< WelType object
    logical, intent(in), optional :: reset_mover !< boolean for resetting mover
    ! -- local variables
    integer(I4B) :: i, node, ict
    real(DP) :: qmult
    real(DP) :: q
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: thick
    logical :: lrm
    !
    ! -- Return if no wells
    if (this%nbound == 0) return
    !
    ! -- pakmvrobj cf
    lrm = .true.
    if (present(reset_mover)) lrm = reset_mover
    if (this%imover == 1 .and. lrm) then
      call this%pakmvrobj%cf()
    end if
    !
    ! -- Calculate hcof and rhs for each well entry
    do i = 1, this%nbound
      node = this%nodelist(i)
      this%hcof(i) = DZERO
      if (this%ibound(node) <= 0) then
        this%rhs(i) = DZERO
        cycle
      end if
      q = this%bound(1, i)
      if (this%iflowred /= 0 .and. q < DZERO) then
        ict = this%icelltype(node)
        if (ict /= 0) then
          tp = this%dis%top(node)
          bt = this%dis%bot(node)
          thick = tp - bt
          tp = bt + this%flowred * thick
          qmult = sQSaturation(tp, bt, this%xnew(node))
          q = q * qmult
        end if
      end if
      this%rhs(i) = -q
    end do
    !
    return
  end subroutine wel_cf

  !> @ brief Copy hcof and rhs terms into solution.
    !!
    !!  Add the hcof and rhs terms for the WEL package to the
    !!  coefficient matrix and right-hand side vector.
    !!
  !<
  subroutine wel_fc(this, rhs, ia, idxglo, amatsln)
    ! -- dummy variables
    class(WelType) :: this !< WelType object
    real(DP), dimension(:), intent(inout) :: rhs !< right-hand side vector for model
    integer(I4B), dimension(:), intent(in) :: ia !< solution CRS row pointers
    integer(I4B), dimension(:), intent(in) :: idxglo !< mapping vector for model (local) to solution (global)
    real(DP), dimension(:), intent(inout) :: amatsln !< solution coefficient matrix
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: ipos
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
      ! -- If mover is active and this well is discharging,
      !    store available water (as positive value).
      if (this%imover == 1 .and. this%rhs(i) > DZERO) then
        call this%pakmvrobj%accumulate_qformvr(i, this%rhs(i))
      end if
    end do
    !
    ! -- return
    return
  end subroutine wel_fc

  !> @ brief Add Newton-Raphson terms for package into solution.
    !!
    !!  Calculate and add the Newton-Raphson terms for the WEL package to the
    !!  coefficient matrix and right-hand side vector.
    !!
  !<
  subroutine wel_fn(this, rhs, ia, idxglo, amatsln)
    ! -- dummy variables
    class(WelType) :: this !< WelType object
    real(DP), dimension(:), intent(inout) :: rhs !< right-hand side vector for model
    integer(I4B), dimension(:), intent(in) :: ia !< solution CRS row pointers
    integer(I4B), dimension(:), intent(in) :: idxglo !< mapping vector for model (local) to solution (global)
    real(DP), dimension(:), intent(inout) :: amatsln !< solution coefficient matrix
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: node
    integer(I4B) :: ipos
    integer(I4B) :: ict
    real(DP) :: drterm
    real(DP) :: q
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: thick
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      node = this%nodelist(i)
      !
      ! -- test if node is constant or inactive
      if (this%ibound(node) <= 0) then
        cycle
      end if
      !
      ! -- well rate is possibly head dependent
      ict = this%icelltype(node)
      if (this%iflowred /= 0 .and. ict /= 0) then
        ipos = ia(node)
        q = -this%rhs(i)
        if (q < DZERO) then
          ! -- calculate derivative for well
          tp = this%dis%top(node)
          bt = this%dis%bot(node)
          thick = tp - bt
          tp = bt + this%flowred * thick
          drterm = sQSaturationDerivative(tp, bt, this%xnew(node))
          drterm = drterm * this%bound(1, i)
          !--fill amat and rhs with newton-raphson terms
          amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + drterm
          rhs(node) = rhs(node) + drterm * this%xnew(node)
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine wel_fn

  !> @brief Initialize the auto flow reduce csv output file
  subroutine wel_afr_csv_init(this, fname)
    ! -- dummy variables
    class(WelType), intent(inout) :: this !< WelType object
    character(len=*), intent(in) :: fname
    ! -- format
    character(len=*), parameter :: fmtafrcsv = &
      "(4x, 'AUTO FLOW REDUCE INFORMATION WILL BE SAVED TO FILE: ', a, /4x, &
      &'OPENED ON UNIT: ', I0)"

    this%ioutafrcsv = getunit()
    call openfile(this%ioutafrcsv, this%iout, fname, 'CSV', &
                  filstat_opt='REPLACE')
    write (this%iout, fmtafrcsv) trim(adjustl(fname)), &
      this%ioutafrcsv
    write (this%ioutafrcsv, '(a)') &
      'time,period,step,boundnumber,cellnumber,rate-requested,&
      &rate-actual,wel-reduction'
    return
  end subroutine wel_afr_csv_init

  !> @brief Write out auto flow reductions only when & where they occur
  subroutine wel_afr_csv_write(this)
    ! -- modules
    use TdisModule, only: totim, kstp, kper
    ! -- dummy variables
    class(WelType), intent(inout) :: this !< WelType object
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: nodereduced
    integer(I4B) :: nodeuser
    real(DP) :: v
    ! -- format
    do i = 1, this%nbound
      nodereduced = this%nodelist(i)
      !
      ! -- test if node is constant or inactive
      if (this%ibound(nodereduced) <= 0) then
        cycle
      end if
      v = this%bound(1, i) + this%rhs(i)
      if (v < DZERO) then
        nodeuser = this%dis%get_nodeuser(nodereduced)
        write (this%ioutafrcsv, '(*(G0,:,","))') &
          totim, kper, kstp, i, nodeuser, this%bound(1, i), this%simvals(i), v
      end if
    end do
  end subroutine wel_afr_csv_write

  !> @ brief Define the list label for the package
    !!
    !!  Method defined the list label for the WEL package. The list label is
    !!  the heading that is written to iout when PRINT_INPUT option is used.
    !!
  !<
  subroutine define_listlabel(this)
    ! -- dummy variables
    class(WelType), intent(inout) :: this !< WelType object
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

  !> @brief Determine if observations are supported.
    !!
    !! Function to determine if observations are supported by the WEL package.
    !! Observations are supported by the WEL package.
    !!
    !! @return  wel_obs_supported       boolean indicating if observations are supported
    !!
  !<
  logical function wel_obs_supported(this)
    ! -- dummy variables
    class(WelType) :: this !< WelType object
    !
    ! -- set boolean
    wel_obs_supported = .true.
    !
    ! -- return
    return
  end function wel_obs_supported

  !> @brief Define the observation types available in the package
    !!
    !! Method to define the observation types available in the WEL package.
    !!
  !<
  subroutine wel_df_obs(this)
    ! -- dummy variables
    class(WelType) :: this !< WelType object
    ! -- local variables
    integer(I4B) :: indx
    !
    ! -- initialize observations
    call this%obs%StoreObsType('wel', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for wel-reduction observation type.
    call this%obs%StoreObsType('wel-reduction', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine wel_df_obs

  !> @brief Save observations for the package
    !!
    !! Method to save simulated values for the WEL package.
    !!
  !<
  subroutine wel_bd_obs(this)
    ! -- dummy variables
    class(WelType) :: this !< WelType object
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: jj
    real(DP) :: v
    type(ObserveType), pointer :: obsrv => null()
    !
    ! -- clear the observations
    call this%obs%obs_bd_clear()
    !
    ! -- Save simulated values for all of package's observations.
    do i = 1, this%obs%npakobs
      obsrv => this%obs%pakobs(i)%obsrv
      if (obsrv%BndFound) then
        do n = 1, obsrv%indxbnds_count
          v = DNODATA
          jj = obsrv%indxbnds(n)
          select case (obsrv%ObsTypeId)
          case ('TO-MVR')
            if (this%imover == 1) then
              v = this%pakmvrobj%get_qtomvr(jj)
              if (v > DZERO) then
                v = -v
              end if
            end if
          case ('WEL')
            v = this%simvals(jj)
          case ('WEL-REDUCTION')
            if (this%iflowred > 0) then
              v = this%bound(1, jj) + this%rhs(jj)
            end if
          case default
            errmsg = 'Unrecognized observation type: '//trim(obsrv%ObsTypeId)
            call store_error(errmsg)
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      else
        call this%obs%SaveOneSimval(obsrv, DNODATA)
      end if
    end do
    !
    ! -- Write the auto flow reduce csv file entries for this step
    if (this%ioutafrcsv > 0) then
      call this%wel_afr_csv_write()
    end if
    !
    ! -- return
    return
  end subroutine wel_bd_obs

  ! -- Procedure related to time series

  !> @brief Assign time series links for the package
    !!
    !! Assign the time series links for the WEL package. Only
    !! the Q variable can be defined with time series.
    !!
  !<
  subroutine wel_rp_ts(this)
    ! -- dummy variables
    class(WelType), intent(inout) :: this !< WelType object
    ! -- local variables
    integer(I4B) :: i, nlinks
    type(TimeSeriesLinkType), pointer :: tslink => null()
    !
    ! -- set up the time series links
    nlinks = this%TsManager%boundtslinks%Count()
    do i = 1, nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        if (tslink%JCol == 1) then
          tslink%Text = 'Q'
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine wel_rp_ts

end module WelModule

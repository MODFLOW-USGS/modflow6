!> -- @ brief Energy Storage and Transfer (EST) Module
!!
!!    The GweEstModule contains the GweEstType, which is related
!!    to GwtEstModule; however, there are some important differences
!!    owing to the fact that a sorbed phase is not considered.
!!    Instead, a single temperature is simulated for each grid
!!    cell and is representative of both the aqueous and solid
!!    phases (i.e., instantaneous thermal equilibrium is
!!    assumed).  Also, "thermal bleeding" is accommodated, where
!!    conductive processes can transport into, through, or
!!    out of dry cells that are part of the active domain.
!<
module GweEstModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DONE, DZERO, DTWO, DHALF, LENBUDTXT, DEP3
  use SimVariablesModule, only: errmsg, warnmsg
  use SimModule, only: store_error, count_errors, &
                       store_warning
  use MatrixBaseModule
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType
  use TspFmiModule, only: TspFmiType
  use GweInputDataModule, only: GweInputDataType

  implicit none
  public :: GweEstType
  public :: est_cr
  !
  integer(I4B), parameter :: NBDITEMS = 2
  character(len=LENBUDTXT), dimension(NBDITEMS) :: budtxt
  data budtxt/' STORAGE-CELLBLK', '   DECAY-AQUEOUS'/

  !> @ brief Energy storage and transfer
  !!
  !!  Data and methods for handling changes in temperature
  !<
  type, extends(NumericalPackageType) :: GweEstType
    !
    ! -- storage
    real(DP), pointer :: cpw => null() !< heat capacity of water
    real(DP), pointer :: rhow => null() !< density of water
    real(DP), pointer :: latheatvap => null() !< latent heat of vaporization
    real(DP), dimension(:), pointer, contiguous :: cps => null() !< heat capacity of solid
    real(DP), dimension(:), pointer, contiguous :: rhos => null() !< density of solid
    real(DP), dimension(:), pointer, contiguous :: porosity => null() !< porosity
    real(DP), dimension(:), pointer, contiguous :: ratesto => null() !< rate of energy storage
    !
    ! -- decay
    integer(I4B), pointer :: idcy => null() !< order of decay rate (0:none, 1:first, 2:zero)
    real(DP), dimension(:), pointer, contiguous :: decay => null() !< first or zero order decay rate (aqueous)
    real(DP), dimension(:), pointer, contiguous :: ratedcy => null() !< rate of decay
    real(DP), dimension(:), pointer, contiguous :: decaylast => null() !< decay rate used for last iteration (needed for zero order decay)
    !
    ! -- misc
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null() !< pointer to model ibound
    type(TspFmiType), pointer :: fmi => null() !< pointer to fmi object
    type(GweInputDataType), pointer :: gwecommon => null() !< pointer to shared gwe data used by multiple packages but set in est
    real(DP), pointer :: eqnsclfac => null() !< governing equation scale factor; =rhow*cpw for energy

  contains

    procedure :: est_ar
    procedure :: est_fc
    procedure :: est_fc_sto
    procedure :: est_fc_dcy
    procedure :: est_cq
    procedure :: est_cq_sto
    procedure :: est_cq_dcy
    procedure :: est_bd
    procedure :: est_ot_flow
    procedure :: est_da
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data

  end type GweEstType

contains

  !> @ brief Create a new EST package object
  !!
  !!  Create a new EST package
  !<
  subroutine est_cr(estobj, name_model, inunit, iout, fmi, eqnsclfac, gwecommon)
    ! -- dummy
    type(GweEstType), pointer :: estobj !< unallocated new est object to create
    character(len=*), intent(in) :: name_model !< name of the model
    integer(I4B), intent(in) :: inunit !< unit number of WEL package input file
    integer(I4B), intent(in) :: iout !< unit number of model listing file
    type(TspFmiType), intent(in), target :: fmi !< fmi package for this GWE model
    real(DP), intent(in), pointer :: eqnsclfac !< governing equation scale factor
    type(GweInputDataType), intent(in), target :: gwecommon !< shared data container for use by multiple GWE packages
    !
    ! -- Create the object
    allocate (estobj)
    !
    ! -- create name and memory path
    call estobj%set_names(1, name_model, 'EST', 'EST')
    !
    ! -- Allocate scalars
    call estobj%allocate_scalars()
    !
    ! -- Set variables
    estobj%inunit = inunit
    estobj%iout = iout
    estobj%fmi => fmi
    estobj%eqnsclfac => eqnsclfac
    estobj%gwecommon => gwecommon
    !
    ! -- Initialize block parser
    call estobj%parser%Initialize(estobj%inunit, estobj%iout)
    !
    ! -- Return
    return
  end subroutine est_cr

  !> @ brief Allocate and read method for package
  !!
  !!  Method to allocate and read static data for the package.
  !<
  subroutine est_ar(this, dis, ibound)
    ! -- modules
    use GweInputDataModule, only: set_gwe_dat_ptrs
    ! -- dummy
    class(GweEstType), intent(inout) :: this !< GweEstType object
    class(DisBaseType), pointer, intent(in) :: dis !< pointer to dis package
    integer(I4B), dimension(:), pointer, contiguous :: ibound !< pointer to GWE ibound array
    ! -- formats
    character(len=*), parameter :: fmtest = &
      "(1x,/1x,'EST -- ENERGY STORAGE AND TRANSFER PACKAGE, VERSION 1, &
      &7/29/2020 INPUT READ FROM UNIT ', i0, //)"
    !
    ! --print a message identifying the energy storage and transfer package.
    write (this%iout, fmtest) this%inunit
    !
    ! -- Read options
    call this%read_options()
    !
    ! -- store pointers to arguments that were passed in
    this%dis => dis
    this%ibound => ibound
    !
    ! -- Allocate arrays
    call this%allocate_arrays(dis%nodes)
    !
    ! -- read the gridded data
    call this%read_data()
    !
    ! -- set data required by other packages
    call this%gwecommon%set_gwe_dat_ptrs(this%rhow, this%cpw, this%latheatvap, &
                                         this%rhos, this%cps)
    !
    ! -- Return
    return
  end subroutine est_ar

  !> @ brief Fill coefficient method for package
  !!
  !!  Method to calculate and fill coefficients for the package.
  !<
  subroutine est_fc(this, nodes, cold, nja, matrix_sln, idxglo, cnew, &
                    rhs, kiter)
    ! -- modules
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    integer, intent(in) :: nodes !< number of nodes
    real(DP), intent(in), dimension(nodes) :: cold !< temperature at end of last time step
    integer(I4B), intent(in) :: nja !< number of GWE connections
    class(MatrixBaseType), pointer :: matrix_sln !< solution matrix
    integer(I4B), intent(in), dimension(nja) :: idxglo !< mapping vector for model (local) to solution (global)
    real(DP), intent(inout), dimension(nodes) :: rhs !< right-hand side vector for model
    real(DP), intent(in), dimension(nodes) :: cnew !< temperature at end of this time step
    integer(I4B), intent(in) :: kiter !< solution outer iteration number
    ! -- local
    !
    ! -- storage contribution
    call this%est_fc_sto(nodes, cold, nja, matrix_sln, idxglo, rhs)
    !
    ! -- decay contribution
    if (this%idcy /= 0) then
      call this%est_fc_dcy(nodes, cold, cnew, nja, matrix_sln, idxglo, &
                           rhs, kiter)
    end if
    !
    ! -- Return
    return
  end subroutine est_fc

  !> @ brief Fill storage coefficient method for package
  !!
  !!  Method to calculate and fill storage coefficients for the package.
  !<
  subroutine est_fc_sto(this, nodes, cold, nja, matrix_sln, idxglo, rhs)
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    integer, intent(in) :: nodes !< number of nodes
    real(DP), intent(in), dimension(nodes) :: cold !< temperature at end of last time step
    integer(I4B), intent(in) :: nja !< number of GWE connections
    class(MatrixBaseType), pointer :: matrix_sln !< solution coefficient matrix
    integer(I4B), intent(in), dimension(nja) :: idxglo !< mapping vector for model (local) to solution (global)
    real(DP), intent(inout), dimension(nodes) :: rhs !< right-hand side vector for model
    ! -- local
    integer(I4B) :: n, idiag
    real(DP) :: tled
    real(DP) :: hhcof, rrhs
    real(DP) :: vnew, vold, vcell, vsolid, term
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- loop through and calculate storage contribution to hcof and rhs
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      if (this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes and solid volume
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      vnew = vcell * this%fmi%gwfsat(n) * this%porosity(n)
      vold = vnew
      if (this%fmi%igwfstrgss /= 0) vold = vold + this%fmi%gwfstrgss(n) * delt
      if (this%fmi%igwfstrgsy /= 0) vold = vold + this%fmi%gwfstrgsy(n) * delt
      vsolid = vcell * (DONE - this%porosity(n))
      !
      ! -- add terms to diagonal and rhs accumulators
      term = (this%rhos(n) * this%cps(n)) * vsolid
      hhcof = -(this%eqnsclfac * vnew + term) * tled
      rrhs = -(this%eqnsclfac * vold + term) * tled * cold(n)
      idiag = this%dis%con%ia(n)
      call matrix_sln%add_value_pos(idxglo(idiag), hhcof)
      rhs(n) = rhs(n) + rrhs
    end do
    !
    ! -- Return
    return
  end subroutine est_fc_sto

  !> @ brief Fill decay coefficient method for package
  !!
  !!  Method to calculate and fill decay coefficients for the package.
  !<
  subroutine est_fc_dcy(this, nodes, cold, cnew, nja, matrix_sln, &
                        idxglo, rhs, kiter)
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    integer, intent(in) :: nodes !< number of nodes
    real(DP), intent(in), dimension(nodes) :: cold !< temperature at end of last time step
    real(DP), intent(in), dimension(nodes) :: cnew !< temperature at end of this time step
    integer(I4B), intent(in) :: nja !< number of GWE connections
    class(MatrixBaseType), pointer :: matrix_sln !< solution coefficient matrix
    integer(I4B), intent(in), dimension(nja) :: idxglo !< mapping vector for model (local) to solution (global)
    real(DP), intent(inout), dimension(nodes) :: rhs !< right-hand side vector for model
    integer(I4B), intent(in) :: kiter !< solution outer iteration number
    ! -- local
    integer(I4B) :: n, idiag
    real(DP) :: hhcof, rrhs
    real(DP) :: swtpdt
    real(DP) :: vcell
    real(DP) :: decay_rate
    !
    ! -- loop through and calculate decay contribution to hcof and rhs
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      if (this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swtpdt = this%fmi%gwfsat(n)
      !
      ! -- add decay rate terms to accumulators
      idiag = this%dis%con%ia(n)
      if (this%idcy == 1) then
        !
        ! -- first order decay rate is a function of temperature, so add       ! note: May want to remove first-order decay for temperature and support only zero-order
        !    to left hand side
        hhcof = -this%decay(n) * vcell * swtpdt * this%porosity(n) &
                * this%eqnsclfac
        call matrix_sln%add_value_pos(idxglo(idiag), hhcof)
      elseif (this%idcy == 2) then
        !
        ! -- Call function to get zero-order decay rate, which may be changed
        !    from the user-specified rate to prevent negative temperatures     ! Important note: still need to think through negative temps
        decay_rate = get_zero_order_decay(this%decay(n), this%decaylast(n), &
                                          kiter, cold(n), cnew(n), delt)
        ! -- This term does get divided by eqnsclfac for fc purposes because it
        !    should start out being a rate of energy
        this%decaylast(n) = decay_rate
        rrhs = decay_rate * vcell * swtpdt * this%porosity(n)
        rhs(n) = rhs(n) + rrhs
      end if
      !
    end do
    !
    ! -- Return
    return
  end subroutine est_fc_dcy

  !> @ brief Calculate flows for package
  !!
  !!  Method to calculate flows for the package.
  !<
  subroutine est_cq(this, nodes, cnew, cold, flowja)
    ! -- modules
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    integer(I4B), intent(in) :: nodes !< number of nodes
    real(DP), intent(in), dimension(nodes) :: cnew !< temperature at end of this time step
    real(DP), intent(in), dimension(nodes) :: cold !< temperature at end of last time step
    real(DP), dimension(:), contiguous, intent(inout) :: flowja !< flow between two connected control volumes
    ! -- local
    !
    ! - storage
    call this%est_cq_sto(nodes, cnew, cold, flowja)
    !
    ! -- decay
    if (this%idcy /= 0) then
      call this%est_cq_dcy(nodes, cnew, cold, flowja)
    end if
    !
    ! -- Return
    return
  end subroutine est_cq

  !> @ brief Calculate storage terms for package
  !!
  !!  Method to calculate storage terms for the package.
  !<
  subroutine est_cq_sto(this, nodes, cnew, cold, flowja)
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    integer(I4B), intent(in) :: nodes !< number of nodes
    real(DP), intent(in), dimension(nodes) :: cnew !< temperature at end of this time step
    real(DP), intent(in), dimension(nodes) :: cold !< temperature at end of last time step
    real(DP), dimension(:), contiguous, intent(inout) :: flowja !< flow between two connected control volumes
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: rate
    real(DP) :: tled
    real(DP) :: vwatnew, vwatold, vcell, vsolid, term
    real(DP) :: hhcof, rrhs
    !
    ! -- initialize
    tled = DONE / delt
    !
    ! -- Calculate storage change
    do n = 1, nodes
      this%ratesto(n) = DZERO
      !
      ! -- skip if transport inactive
      if (this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes and solid volume
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      vwatnew = vcell * this%fmi%gwfsat(n) * this%porosity(n)
      vwatold = vwatnew
      if (this%fmi%igwfstrgss /= 0) vwatold = vwatold + this%fmi%gwfstrgss(n) &
                                              * delt
      if (this%fmi%igwfstrgsy /= 0) vwatold = vwatold + this%fmi%gwfstrgsy(n) &
                                              * delt
      vsolid = vcell * (DONE - this%porosity(n))
      !
      ! -- calculate rate
      term = (this%rhos(n) * this%cps(n)) * vsolid
      hhcof = -(this%eqnsclfac * vwatnew + term) * tled
      rrhs = -(this%eqnsclfac * vwatold + term) * tled * cold(n)
      rate = hhcof * cnew(n) - rrhs
      this%ratesto(n) = rate
      idiag = this%dis%con%ia(n)
      flowja(idiag) = flowja(idiag) + rate
    end do
    !
    ! -- Return
    return
  end subroutine est_cq_sto

  !> @ brief Calculate decay terms for package
  !!
  !!  Method to calculate decay terms for the package.
  !<
  subroutine est_cq_dcy(this, nodes, cnew, cold, flowja) ! Important note: this handles only decay in water; need to add zero-order (but not first-order?) decay in solid
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    integer(I4B), intent(in) :: nodes !< number of nodes
    real(DP), intent(in), dimension(nodes) :: cnew !< temperature at end of this time step
    real(DP), intent(in), dimension(nodes) :: cold !< temperature at end of last time step
    real(DP), dimension(:), contiguous, intent(inout) :: flowja !< flow between two connected control volumes
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: rate
    real(DP) :: swtpdt
    real(DP) :: hhcof, rrhs
    real(DP) :: vcell
    real(DP) :: decay_rate
    !
    ! -- initialize
    !
    ! -- Calculate decay change
    do n = 1, nodes
      !
      ! -- skip if transport inactive
      this%ratedcy(n) = DZERO
      if (this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swtpdt = this%fmi%gwfsat(n)
      !
      ! -- calculate decay gains and losses
      rate = DZERO
      hhcof = DZERO
      rrhs = DZERO
      if (this%idcy == 1) then ! Important note: do we need/want first-order decay for temperature???
        hhcof = -this%decay(n) * vcell * swtpdt * this%porosity(n) &
                * this%eqnsclfac
      elseif (this%idcy == 2) then
        decay_rate = get_zero_order_decay(this%decay(n), this%decaylast(n), &
                                          0, cold(n), cnew(n), delt)
        rrhs = decay_rate * vcell * swtpdt * this%porosity(n) ! Important note: this term does NOT get multiplied by eqnsclfac for cq purposes because it should already be a rate of energy
      end if
      rate = hhcof * cnew(n) - rrhs
      this%ratedcy(n) = rate
      idiag = this%dis%con%ia(n)
      flowja(idiag) = flowja(idiag) + rate
      !
    end do
    !
    ! -- Return
    return
  end subroutine est_cq_dcy

  !> @ brief Calculate budget terms for package
  !!
  !!  Method to calculate budget terms for the package.
  !<
  subroutine est_bd(this, isuppress_output, model_budget)
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    integer(I4B), intent(in) :: isuppress_output !< flag to suppress output
    type(BudgetType), intent(inout) :: model_budget !< model budget object
    ! -- local
    real(DP) :: rin
    real(DP) :: rout
    !
    ! -- sto
    call rate_accumulator(this%ratesto, rin, rout)
    call model_budget%addentry(rin, rout, delt, budtxt(1), &
                               isuppress_output, rowlabel=this%packName)
    !
    ! -- dcy
    if (this%idcy /= 0) then
      call rate_accumulator(this%ratedcy, rin, rout)
      call model_budget%addentry(rin, rout, delt, budtxt(2), &
                                 isuppress_output, rowlabel=this%packName)
    end if
    !
    ! -- Return
    return
  end subroutine est_bd

  !> @ brief Output flow terms for package
  !!
  !!  Method to output terms for the package.
  !<
  subroutine est_ot_flow(this, icbcfl, icbcun)
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    integer(I4B), intent(in) :: icbcfl !< flag and unit number for cell-by-cell output
    integer(I4B), intent(in) :: icbcun !< flag indication if cell-by-cell data should be saved
    ! -- local
    integer(I4B) :: ibinun
    !character(len=16), dimension(2) :: aname
    integer(I4B) :: iprint, nvaluesp, nwidthp
    character(len=1) :: cdatafmp = ' ', editdesc = ' '
    real(DP) :: dinact
    !
    ! -- Set unit number for binary output
    if (this%ipakcb < 0) then
      ibinun = icbcun
    elseif (this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    end if
    if (icbcfl == 0) ibinun = 0
    !
    ! -- Record the storage rate if requested
    if (ibinun /= 0) then
      iprint = 0
      dinact = DZERO
      !
      ! -- sto
      call this%dis%record_array(this%ratesto, this%iout, iprint, -ibinun, &
                                 budtxt(1), cdatafmp, nvaluesp, &
                                 nwidthp, editdesc, dinact)
      !
      ! -- dcy
      if (this%idcy /= 0) &
        call this%dis%record_array(this%ratedcy, this%iout, iprint, -ibinun, &
                                   budtxt(2), cdatafmp, nvaluesp, &
                                   nwidthp, editdesc, dinact)
    end if
    !
    ! -- Return
    return
  end subroutine est_ot_flow

  !> @brief Deallocate memory
  !!
  !!  Method to deallocate memory for the package.
  !<
  subroutine est_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    !
    ! -- Deallocate arrays if package was active
    if (this%inunit > 0) then
      call mem_deallocate(this%porosity)
      call mem_deallocate(this%ratesto)
      call mem_deallocate(this%idcy)
      call mem_deallocate(this%decay)
      call mem_deallocate(this%ratedcy)
      call mem_deallocate(this%decaylast)
      call mem_deallocate(this%cpw)
      call mem_deallocate(this%cps)
      call mem_deallocate(this%rhow)
      call mem_deallocate(this%rhos)
      call mem_deallocate(this%latheatvap)
      this%ibound => null()
      this%fmi => null()
    end if
    !
    ! -- Scalars
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine est_da

  !> @ brief Allocate scalar variables for package
  !!
  !!  Method to allocate scalar variables for the package.
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    ! -- local
    !
    ! -- Allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%cpw, 'CPW', this%memoryPath)
    call mem_allocate(this%rhow, 'RHOW', this%memoryPath)
    call mem_allocate(this%latheatvap, 'LATHEATVAP', this%memoryPath)
    call mem_allocate(this%idcy, 'IDCY', this%memoryPath)
    !
    ! -- Initialize
    this%cpw = DZERO
    this%rhow = DZERO
    this%latheatvap = DZERO
    this%idcy = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  !> @ brief Allocate arrays for package
  !!
  !!  Method to allocate arrays for the package.
  !<
  subroutine allocate_arrays(this, nodes)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    integer(I4B), intent(in) :: nodes !< number of nodes
    ! -- local
    integer(I4B) :: n
    !
    ! -- Allocate
    ! -- sto
    call mem_allocate(this%porosity, nodes, 'POROSITY', this%memoryPath)
    call mem_allocate(this%ratesto, nodes, 'RATESTO', this%memoryPath)
    call mem_allocate(this%cps, nodes, 'CPS', this%memoryPath)
    call mem_allocate(this%rhos, nodes, 'RHOS', this%memoryPath)
    !
    ! -- dcy
    if (this%idcy == 0) then
      call mem_allocate(this%ratedcy, 1, 'RATEDCY', this%memoryPath)
      call mem_allocate(this%decay, 1, 'DECAY', this%memoryPath)
      call mem_allocate(this%decaylast, 1, 'DECAYLAST', this%memoryPath)
    else
      call mem_allocate(this%ratedcy, this%dis%nodes, 'RATEDCY', this%memoryPath)
      call mem_allocate(this%decay, nodes, 'DECAY', this%memoryPath)
      call mem_allocate(this%decaylast, nodes, 'DECAYLAST', this%memoryPath)
    end if
    !
    ! -- Initialize
    do n = 1, nodes
      this%porosity(n) = DZERO
      this%ratesto(n) = DZERO
      this%cps(n) = DZERO
      this%rhos(n) = DZERO
    end do
    do n = 1, size(this%decay)
      this%decay(n) = DZERO
      this%ratedcy(n) = DZERO
      this%decaylast(n) = DZERO
    end do
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  !> @ brief Read options for package
  !!
  !!  Method to read options for the package.
  !<
  subroutine read_options(this)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    ! -- local
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtisvflow = &
            &"(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY "// &
            &"FILE WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtidcy1 = &
                                   "(4x,'FIRST-ORDER DECAY IS ACTIVE. ')"
    character(len=*), parameter :: fmtidcy2 = &
                                   "(4x,'ZERO-ORDER DECAY IS ACTIVE. ')"
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false., &
                              supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING ENERGY STORAGE AND TRANSFER OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('SAVE_FLOWS')
          this%ipakcb = -1
          write (this%iout, fmtisvflow)
        case ('FIRST_ORDER_DECAY')
          this%idcy = 1
          write (this%iout, fmtidcy1)
        case ('ZERO_ORDER_DECAY')
          this%idcy = 2
          write (this%iout, fmtidcy2)
        case ('HEAT_CAPACITY_WATER')
          this%cpw = this%parser%GetDouble()
          write (this%iout, '(4x,a,1pg15.6)') &
            'Heat capacity of the water has been set to: ', &
            this%cpw
        case ('DENSITY_WATER')
          this%rhow = this%parser%GetDouble()
          write (this%iout, '(4x,a,1pg15.6)') &
            'Density of the water has been set to: ', &
            this%rhow
        case ('LATENT_HEAT_VAPORIZATION')
          this%latheatvap = this%parser%GetDouble()
          write (this%iout, '(4x,a,1pg15.6)') &
            'Latent heat of vaporization of the water has been set to: ', &
            this%latheatvap
        case default
          write (errmsg, '(a,a)') 'UNKNOWN EST OPTION: ', trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF ENERGY STORAGE AND TRANSFER OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  !> @ brief Read data for package
  !!
  !!  Method to read data for the package.
  !<
  subroutine read_data(this)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use MemoryManagerModule, only: mem_reallocate, mem_reassignptr
    ! -- dummy
    class(GweEstType) :: this !< GweEstType object
    ! -- local
    character(len=LINELENGTH) :: keyword
    character(len=:), allocatable :: line
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    logical, dimension(4) :: lname
    character(len=24), dimension(4) :: aname
    ! -- formats
    ! -- data
    data aname(1)/'  MOBILE DOMAIN POROSITY'/
    data aname(2)/'              DECAY RATE'/
    data aname(3)/' HEAT CAPACITY OF SOLIDS'/
    data aname(4)/'       DENSITY OF SOLIDS'/
    !
    ! -- initialize
    isfound = .false.
    lname(:) = .false.
    !
    ! -- get griddata block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING GRIDDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        call this%parser%GetRemainingLine(line)
        lloc = 1
        select case (keyword)
        case ('POROSITY')
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%porosity, &
                                        aname(1))
          lname(1) = .true.
        case ('DECAY')
          if (this%idcy == 0) &
            call mem_reallocate(this%decay, this%dis%nodes, 'DECAY', &
                                trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%decay, &
                                        aname(2))
          lname(2) = .true.
        case ('CPS')
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%cps, &
                                        aname(3))
          lname(3) = .true.
        case ('RHOS')
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%rhos, &
                                        aname(4))
          lname(4) = .true.
        case default
          write (errmsg, '(a,a)') 'UNKNOWN GRIDDATA TAG: ', trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END PROCESSING GRIDDATA'
    else
      write (errmsg, '(a)') 'REQUIRED GRIDDATA BLOCK NOT FOUND.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Check for required porosity
    if (.not. lname(1)) then
      write (errmsg, '(a)') 'POROSITY NOT SPECIFIED IN GRIDDATA BLOCK.'
      call store_error(errmsg)
    end if
    if (.not. lname(3)) then
      write (errmsg, '(a)') 'CPS NOT SPECIFIED IN GRIDDATA BLOCK.'
      call store_error(errmsg)
    end if
    if (.not. lname(4)) then
      write (errmsg, '(a)') 'RHOS NOT SPECIFIED IN GRIDDATA BLOCK.'
      call store_error(errmsg)
    end if
    !
    ! -- Check for required decay/production rate coefficients
    if (this%idcy > 0) then
      if (.not. lname(2)) then
        write (errmsg, '(a)') 'FIRST OR ZERO ORDER DECAY IS &
          &ACTIVE BUT THE FIRST RATE COEFFICIENT IS NOT SPECIFIED.  DECAY &
          &MUST BE SPECIFIED IN GRIDDATA BLOCK.'
        call store_error(errmsg)
      end if
    else
      if (lname(2)) then
        write (warnmsg, '(a)') 'FIRST OR ZERO ORER DECAY &
          &IS NOT ACTIVE BUT DECAY WAS SPECIFIED.  DECAY WILL &
          &HAVE NO AFFECT ON SIMULATION RESULTS.'
        call store_warning(warnmsg)
        write (this%iout, '(1x,a)') 'WARNING.  '//warnmsg
      end if
    end if
    !
    ! -- terminate if errors
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Return
    return
  end subroutine read_data

  !> @ brief Calculate zero-order decay rate and constrain if necessary
  !!
  !!  Function to calculate the zero-order decay rate from the user specified
  !!  decay rate.  If the decay rate is positive, then the decay rate must
  !!  be constrained so that more energy is not removed than is available.
  !!  Without this constraint, negative temperatures could result from
  !!  zero-order decay (no freezing).
  !<
  function get_zero_order_decay(decay_rate_usr, decay_rate_last, kiter, &
                                cold, cnew, delt) result(decay_rate)
    ! -- dummy
    real(DP), intent(in) :: decay_rate_usr !< user-entered decay rate
    real(DP), intent(in) :: decay_rate_last !< decay rate used for last iteration
    integer(I4B), intent(in) :: kiter !< Picard iteration counter
    real(DP), intent(in) :: cold !< temperature at end of last time step
    real(DP), intent(in) :: cnew !< temperature at end of this time step
    real(DP), intent(in) :: delt !< length of time step
    ! -- return
    real(DP) :: decay_rate !< returned value for decay rate
    !
    ! -- Return user rate if production, otherwise constrain, if necessary
    if (decay_rate_usr < DZERO) then
      !
      ! -- Production, no need to limit rate
      decay_rate = decay_rate_usr
    else
      !
      ! -- Need to ensure decay does not result in negative
      !    temperature, so reduce the rate if it would result in
      !    removing more energy than is in the cell.          ! kluge note: think through
      if (kiter == 1) then
        decay_rate = min(decay_rate_usr, cold / delt) ! kluge note: actually want to use rhow*cpw*cold and rhow*cpw*cnew for rates here and below
      else
        decay_rate = decay_rate_last
        if (cnew < DZERO) then
          decay_rate = decay_rate_last + cnew / delt
        else if (cnew > cold) then
          decay_rate = decay_rate_last + cnew / delt
        end if
        decay_rate = min(decay_rate_usr, decay_rate)
      end if
      decay_rate = max(decay_rate, DZERO)
    end if
    return
  end function get_zero_order_decay

end module GweEstModule

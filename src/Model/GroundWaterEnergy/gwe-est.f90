!>  @ brief Energy Storage and Transfer (EST) Module
!!
!!  The GweEstModule contains the GweEstType, which is related
!!  to GwtEstModule; however, there are some important differences
!!  owing to the fact that a sorbed phase is not considered.
!!  Instead, a single temperature is simulated for each grid
!!  cell and is representative of both the aqueous and solid
!!  phases (i.e., instantaneous thermal equilibrium is
!!  assumed).  Also, "thermal bleeding" is accommodated, where
!!  conductive processes can transport into, through, or
!!  out of dry cells that are part of the active domain.
!<
module GweEstModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DONE, IZERO, DZERO, DTWO, DHALF, LENBUDTXT, DEP3
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
  integer(I4B), parameter :: NBDITEMS = 3
  character(len=LENBUDTXT), dimension(NBDITEMS) :: budtxt
  data budtxt/' STORAGE-CELLBLK', '   DECAY-AQUEOUS', '     DECAY-SOLID'/

  !> @brief Enumerator that defines the decay options
  !<
  ENUM, BIND(C)
    ENUMERATOR :: DECAY_OFF = 0 !< Decay (or production) of thermal energy inactive (default)
    ENUMERATOR :: DECAY_ZERO_ORDER = 2 !< Zeroth-order decay
    ENUMERATOR :: DECAY_WATER = 1 !< Zeroth-order decay in water only
    ENUMERATOR :: DECAY_SOLID = 2 !< Zeroth-order decay in solid only
    ENUMERATOR :: DECAY_BOTH = 3 !< Zeroth-order decay in water and solid
  END ENUM

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
    integer(I4B), pointer :: idcy => null() !< order of decay rate (0:none, 1:first, 2:zero (aqueous and/or solid))
    integer(I4B), pointer :: idcysrc => null() !< decay source (or sink) (1: aqueous only, 2: solid only, 3: both phases
    real(DP), dimension(:), pointer, contiguous :: decay_water => null() !< first or zero order decay rate (aqueous)
    real(DP), dimension(:), pointer, contiguous :: decay_solid => null() !< first or zero order decay rate (solid)
    real(DP), dimension(:), pointer, contiguous :: ratedcyw => null() !< rate of decay in aqueous phase
    real(DP), dimension(:), pointer, contiguous :: ratedcys => null() !< rate of decay in solid phase
    real(DP), dimension(:), pointer, contiguous :: decaylastw => null() !< aqueous phase decay rate used for last iteration (needed for zero order decay)
    real(DP), dimension(:), pointer, contiguous :: decaylasts => null() !< solid phase decay rate used for last iteration (needed for zero order decay)
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
    procedure :: est_fc_dcy_water
    procedure :: est_fc_dcy_solid
    procedure :: est_cq
    procedure :: est_cq_sto
    procedure :: est_cq_dcy
    procedure :: est_cq_dcy_solid
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
  end subroutine est_ar

  !> @ brief Fill coefficient method for package
  !!
  !!  Method to calculate and fill coefficients for the package.
  !<
  subroutine est_fc(this, nodes, cold, nja, matrix_sln, idxglo, cnew, &
                    rhs, kiter)
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
    !
    ! -- storage contribution
    call this%est_fc_sto(nodes, cold, nja, matrix_sln, idxglo, rhs)
    !
    ! -- decay contribution
    if (this%idcy == DECAY_ZERO_ORDER) then
      call this%est_fc_dcy_water(nodes, cold, cnew, nja, matrix_sln, idxglo, &
                                 rhs, kiter)
      call this%est_fc_dcy_solid(nodes, cold, nja, matrix_sln, idxglo, rhs, &
                                 cnew, kiter)
    end if
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
  end subroutine est_fc_sto

  !> @ brief Fill decay coefficient method for package
  !!
  !!  Method to calculate and fill decay coefficients for the package.
  !<
  subroutine est_fc_dcy_water(this, nodes, cold, cnew, nja, matrix_sln, &
                              idxglo, rhs, kiter)
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
    integer(I4B) :: n
    real(DP) :: rrhs
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
      ! -- add zero-order decay rate terms to accumulators
      if (this%idcy == DECAY_ZERO_ORDER .and. (this%idcysrc == DECAY_WATER .or. &
                                               this%idcysrc == DECAY_BOTH)) then
        !
        decay_rate = this%decay_water(n)
        ! -- This term does get divided by eqnsclfac for fc purposes because it
        !    should start out being a rate of energy
        this%decaylastw(n) = decay_rate
        rrhs = decay_rate * vcell * swtpdt * this%porosity(n)
        rhs(n) = rhs(n) + rrhs
      end if
      !
    end do
  end subroutine est_fc_dcy_water

  !> @ brief Fill solid decay coefficient method for package
  !!
  !!  Method to calculate and fill energy decay coefficients for the solid phase.
  !<
  subroutine est_fc_dcy_solid(this, nodes, cold, nja, matrix_sln, idxglo, &
                              rhs, cnew, kiter)
    ! -- dummy
    class(GweEstType) :: this !< GwtMstType object
    integer, intent(in) :: nodes !< number of nodes
    real(DP), intent(in), dimension(nodes) :: cold !< temperature at end of last time step
    integer(I4B), intent(in) :: nja !< number of GWE connections
    class(MatrixBaseType), pointer :: matrix_sln !< solution coefficient matrix
    integer(I4B), intent(in), dimension(nja) :: idxglo !< mapping vector for model (local) to solution (global)
    real(DP), intent(inout), dimension(nodes) :: rhs !< right-hand side vector for model
    real(DP), intent(in), dimension(nodes) :: cnew !< temperature at end of this time step
    integer(I4B), intent(in) :: kiter !< solution outer iteration number
    ! -- local
    integer(I4B) :: n
    real(DP) :: rrhs
    real(DP) :: vcell
    real(DP) :: decay_rate
    !
    ! -- loop through and calculate sorption contribution to hcof and rhs
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      if (this%ibound(n) <= 0) cycle
      !
      ! -- set variables
      rrhs = DZERO
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      !
      ! -- account for zero-order decay rate terms in rhs
      if (this%idcy == DECAY_ZERO_ORDER .and. (this%idcysrc == DECAY_SOLID .or. &
                                               this%idcysrc == DECAY_BOTH)) then
        !
        ! -- negative temps are currently not checked for or prevented since a
        !    user can define a temperature scale of their own choosing.  if
        !    negative temps result from the specified zero-order decay value,
        !    it is up to the user to decide if the calculated temperatures are
        !    acceptable
        decay_rate = this%decay_solid(n)
        this%decaylasts(n) = decay_rate
        rrhs = decay_rate * vcell * (1 - this%porosity(n)) * this%rhos(n)
        rhs(n) = rhs(n) + rrhs
      end if
    end do
  end subroutine est_fc_dcy_solid

  !> @ brief Calculate flows for package
  !!
  !!  Method to calculate flows for the package.
  !<
  subroutine est_cq(this, nodes, cnew, cold, flowja)
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
    if (this%idcy == DECAY_ZERO_ORDER) then
      if (this%idcysrc == DECAY_WATER .or. this%idcysrc == DECAY_BOTH) then
        call this%est_cq_dcy(nodes, cnew, cold, flowja)
      end if
      if (this%idcysrc == DECAY_SOLID .or. this%idcysrc == DECAY_BOTH) then
        call this%est_cq_dcy_solid(nodes, cnew, cold, flowja)
      end if
    end if
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
  end subroutine est_cq_sto

  !> @ brief Calculate decay terms for aqueous phase
  !!
  !!  Method to calculate decay terms for the aqueous phase.
  !<
  subroutine est_cq_dcy(this, nodes, cnew, cold, flowja)
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
    ! -- Calculate decay change
    do n = 1, nodes
      !
      ! -- skip if transport inactive
      this%ratedcyw(n) = DZERO
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
      ! -- zero order decay aqueous phase
      if (this%idcy == DECAY_ZERO_ORDER .and. &
          (this%idcysrc == DECAY_WATER .or. this%idcysrc == DECAY_BOTH)) then
        decay_rate = this%decay_water(n)
        ! -- this term does NOT get multiplied by eqnsclfac for cq purposes
        !    because it should already be a rate of energy
        rrhs = decay_rate * vcell * swtpdt * this%porosity(n)
      end if
      rate = hhcof * cnew(n) - rrhs
      this%ratedcyw(n) = rate
      idiag = this%dis%con%ia(n)
      flowja(idiag) = flowja(idiag) + rate
      !
    end do
  end subroutine est_cq_dcy

  !> @ brief Calculate decay terms for solid phase
  !!
  !!  Method to calculate decay terms for the solid phase.
  !<
  subroutine est_cq_dcy_solid(this, nodes, cnew, cold, flowja)
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
    real(DP) :: hhcof, rrhs
    real(DP) :: vcell
    real(DP) :: decay_rate
    !
    ! -- calculate decay change
    do n = 1, nodes
      !
      ! -- skip if transport inactive
      this%ratedcys(n) = DZERO
      if (this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      !
      ! -- calculate decay gains and losses
      rate = DZERO
      hhcof = DZERO
      rrhs = DZERO
      ! -- first-order decay (idcy=1) is not supported for temperature modeling
      if (this%idcy == DECAY_ZERO_ORDER .and. &
          (this%idcysrc == DECAY_SOLID .or. this%idcysrc == DECAY_BOTH)) then ! zero order decay in the solid phase
        decay_rate = this%decay_solid(n)
        ! -- this term does NOT get multiplied by eqnsclfac for cq purposes
        !    because it should already be a rate of energy
        rrhs = decay_rate * vcell * (1 - this%porosity(n)) * this%rhos(n)
      end if
      rate = hhcof * cnew(n) - rrhs
      this%ratedcys(n) = rate
      idiag = this%dis%con%ia(n)
      flowja(idiag) = flowja(idiag) + rate
    end do
  end subroutine est_cq_dcy_solid

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
    if (this%idcy == DECAY_ZERO_ORDER) then
      if (this%idcysrc == DECAY_WATER .or. this%idcysrc == DECAY_BOTH) then
        ! -- aqueous phase
        call rate_accumulator(this%ratedcyw, rin, rout)
        call model_budget%addentry(rin, rout, delt, budtxt(2), &
                                   isuppress_output, rowlabel=this%packName)
      end if
      if (this%idcysrc == DECAY_SOLID .or. this%idcysrc == DECAY_BOTH) then
        ! -- solid phase
        call rate_accumulator(this%ratedcys, rin, rout)
        call model_budget%addentry(rin, rout, delt, budtxt(3), &
                                   isuppress_output, rowlabel=this%packName)
      end if
    end if
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
      if (this%idcy == DECAY_ZERO_ORDER) then
        if (this%idcysrc == DECAY_WATER .or. this%idcysrc == DECAY_BOTH) then
          ! -- aqueous phase
          call this%dis%record_array(this%ratedcyw, this%iout, iprint, &
                                     -ibinun, budtxt(2), cdatafmp, nvaluesp, &
                                     nwidthp, editdesc, dinact)
        end if
        if (this%idcysrc == DECAY_SOLID .or. this%idcysrc == DECAY_BOTH) then
          ! -- solid phase
          call this%dis%record_array(this%ratedcys, this%iout, iprint, &
                                     -ibinun, budtxt(3), cdatafmp, nvaluesp, &
                                     nwidthp, editdesc, dinact)
        end if
      end if
    end if
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
      call mem_deallocate(this%idcysrc)
      call mem_deallocate(this%decay_water)
      call mem_deallocate(this%decay_solid)
      call mem_deallocate(this%ratedcyw)
      call mem_deallocate(this%ratedcys)
      call mem_deallocate(this%decaylastw)
      call mem_deallocate(this%decaylasts)
      call mem_deallocate(this%cpw)
      call mem_deallocate(this%cps)
      call mem_deallocate(this%rhow)
      call mem_deallocate(this%rhos)
      call mem_deallocate(this%latheatvap)
      this%ibound => null()
      this%fmi => null()
    end if
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
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
    !
    ! -- Allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%cpw, 'CPW', this%memoryPath)
    call mem_allocate(this%rhow, 'RHOW', this%memoryPath)
    call mem_allocate(this%latheatvap, 'LATHEATVAP', this%memoryPath)
    call mem_allocate(this%idcy, 'IDCY', this%memoryPath)
    call mem_allocate(this%idcysrc, 'IDCYSRC', this%memoryPath)
    !
    ! -- Initialize
    this%cpw = DZERO
    this%rhow = DZERO
    this%latheatvap = DZERO
    this%idcy = IZERO
    this%idcysrc = IZERO
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
    if (this%idcy == DECAY_OFF) then
      call mem_allocate(this%ratedcyw, 1, 'RATEDCYW', this%memoryPath)
      call mem_allocate(this%ratedcys, 1, 'RATEDCYS', this%memoryPath)
      call mem_allocate(this%decay_water, 1, 'DECAY_WATER', this%memoryPath)
      call mem_allocate(this%decay_solid, 1, 'DECAY_SOLID', this%memoryPath)
      call mem_allocate(this%decaylastw, 1, 'DECAYLASTW', this%memoryPath)
      call mem_allocate(this%decaylasts, 1, 'DECAYLAST', this%memoryPath)
    else
      call mem_allocate(this%ratedcyw, this%dis%nodes, 'RATEDCYW', &
                        this%memoryPath)
      call mem_allocate(this%ratedcys, this%dis%nodes, 'RATEDCYS', &
                        this%memoryPath)
      call mem_allocate(this%decay_water, nodes, 'DECAY_WATER', this%memoryPath)
      call mem_allocate(this%decay_solid, nodes, 'DECAY_SOLID', this%memoryPath)
      call mem_allocate(this%decaylastw, nodes, 'DECAYLASTW', this%memoryPath)
      call mem_allocate(this%decaylasts, nodes, 'DECAYLASTS', this%memoryPath)
    end if
    !
    ! -- Initialize
    do n = 1, nodes
      this%porosity(n) = DZERO
      this%ratesto(n) = DZERO
      this%cps(n) = DZERO
      this%rhos(n) = DZERO
    end do
    do n = 1, size(this%decay_water)
      this%decay_water(n) = DZERO
      this%decay_solid(n) = DZERO
      this%ratedcyw(n) = DZERO
      this%ratedcys(n) = DZERO
      this%decaylastw(n) = DZERO
      this%decaylasts(n) = DZERO
    end do
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
                                   "(4x,'ZERO-ORDER DECAY IN THE AQUEOUS "// &
                                   &"PHASE IS ACTIVE. ')"
    character(len=*), parameter :: fmtidcy3 = &
                                   "(4x,'ZERO-ORDER DECAY IN THE SOLID "// &
                                   &"PHASE IS ACTIVE. ')"
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
        case ('ZERO_ORDER_DECAY_WATER')
          this%idcy = DECAY_ZERO_ORDER
          ! -- idcysrc > 0 indicates decay in the solid phase is active
          !    in which case the idcysrc should now be upgraded to both phases
          if (this%idcysrc > IZERO) then
            this%idcysrc = DECAY_BOTH
          else
            this%idcysrc = DECAY_WATER
          end if
          write (this%iout, fmtidcy2)
        case ('ZERO_ORDER_DECAY_SOLID')
          this%idcy = DECAY_ZERO_ORDER
          ! -- idcysrc > 0 indicates decay in active in water in which case
          !    the idcysrc should now be upgraded to both phases
          if (this%idcysrc > IZERO) then
            this%idcysrc = DECAY_BOTH
          else
            this%idcysrc = DECAY_SOLID
          end if
          write (this%iout, fmtidcy3)
        case ('HEAT_CAPACITY_WATER')
          this%cpw = this%parser%GetDouble()
          if (this%cpw <= 0.0) then
            write (errmsg, '(a)') 'Specified value for the heat capacity of &
              &water must be greater than 0.0.'
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
          else
            write (this%iout, '(4x,a,1pg15.6)') &
              'Heat capacity of the water has been set to: ', &
              this%cpw
          end if
        case ('DENSITY_WATER')
          this%rhow = this%parser%GetDouble()
          if (this%rhow <= 0.0) then
            write (errmsg, '(a)') 'Specified value for the density of &
              &water must be greater than 0.0.'
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
          else
            write (this%iout, '(4x,a,1pg15.6)') &
              'Density of the water has been set to: ', &
              this%rhow
          end if
        case ('LATENT_HEAT_VAPORIZATION')
          this%latheatvap = this%parser%GetDouble()
          write (this%iout, '(4x,a,1pg15.6)') &
            'Latent heat of vaporization of the water has been set to: ', &
            this%latheatvap
        case default
          write (errmsg, '(a,a)') 'Unknown EST option: ', trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF ENERGY STORAGE AND TRANSFER OPTIONS'
    end if
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
    logical, dimension(5) :: lname
    character(len=24), dimension(5) :: aname
    ! -- formats
    ! -- data
    data aname(1)/'  MOBILE DOMAIN POROSITY'/
    data aname(2)/'DECAY RATE AQUEOUS PHASE'/
    data aname(3)/'  DECAY RATE SOLID PHASE'/
    data aname(4)/' HEAT CAPACITY OF SOLIDS'/
    data aname(5)/'       DENSITY OF SOLIDS'/
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
        case ('DECAY_WATER')
          if (this%idcy == DECAY_OFF) &
            call mem_reallocate(this%decay_water, this%dis%nodes, 'DECAY_WATER', &
                                trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%decay_water, &
                                        aname(2))
          lname(2) = .true.
        case ('DECAY_SOLID')
          if (this%idcy == DECAY_OFF) &
            call mem_reallocate(this%decay_solid, this%dis%nodes, 'DECAY_SOLID', &
                                trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%decay_solid, &
                                        aname(3))
          lname(3) = .true.
        case ('HEAT_CAPACITY_SOLID')
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%cps, &
                                        aname(4))
          lname(4) = .true.
        case ('DENSITY_SOLID')
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%rhos, &
                                        aname(5))
          lname(5) = .true.
        case default
          write (errmsg, '(a,a)') 'Unknown griddata tag: ', trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END PROCESSING GRIDDATA'
    else
      write (errmsg, '(a)') 'Required griddata block not found.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Check for required porosity
    if (.not. lname(1)) then
      write (errmsg, '(a)') 'Porosity not specified in griddata block.'
      call store_error(errmsg)
    end if
    if (.not. lname(4)) then
      write (errmsg, '(a)') 'HEAT_CAPACITY_SOLID not specified in griddata block.'
      call store_error(errmsg)
    end if
    if (.not. lname(5)) then
      write (errmsg, '(a)') 'DENSITY_SOLID not specified in griddata block.'
      call store_error(errmsg)
    end if
    !
    ! -- Check for required decay/production rate coefficients
    if (this%idcy == DECAY_ZERO_ORDER) then
      if (.not. lname(2) .and. .not. lname(3)) then
        write (errmsg, '(a)') 'Zero order decay in either the aqueous &
          &or solid phase is active but the corresponding zero-order &
          &rate coefficient is not specified. Either DECAY_WATER or &
          &DECAY_SOLID must be specified in the griddata block.'
        call store_error(errmsg)
      end if
    else
      if (lname(2)) then
        write (warnmsg, '(a)') 'Zero order decay in the aqueous phase has &
          &not been activated but DECAY_WATER has been specified. Zero &
          &order decay in the aqueous phase will have no affect on &
          &simulation results.'
        call store_warning(warnmsg)
        write (this%iout, '(1x,a)') 'WARNING.  '//warnmsg
      else if (lname(3)) then
        write (warnmsg, '(a)') 'Zero order decay in the solid phase has not &
          &been activated but DECAY_SOLID has been specified.  Zero order &
          &decay in the solid phase will have no affect on simulation &
          &results.'
        call store_warning(warnmsg)
        write (this%iout, '(1x,a)') 'WARNING.  '//warnmsg
      end if
    end if
    !
    ! -- terminate if errors
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
  end subroutine read_data

end module GweEstModule

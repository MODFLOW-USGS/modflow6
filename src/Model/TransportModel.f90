! Generalized Transport Base Class
! Base class for solute (mass) and energy (thermal) transport
!   (The following copied from gwt1.f90)
!   * Add check that discretization is the same between both models
!   * Program GWT-GWT exchange transport (awaiting implementation of interface model)
!   * Consider implementation of steady-state transport (affects MST, IST)
!   * Check and handle pore space discrepancy between flow and transport (porosity vs specific yield)
!   * UZT may not have the required porosity term

module TransportModelModule
  use KindModule, only: DP, I4B
  use InputOutputModule, only: ParseLine
  use VersionModule, only: write_listfile_header
  use ConstantsModule, only: LENFTYPE, DZERO, LENPAKLOC
  use SimVariablesModule, only: errmsg
  use NumericalModelModule, only: NumericalModelType
  use NumericalPackageModule, only: NumericalPackageType
  use TspLabelsModule, only: TspLabelsType
  use BndModule, only: BndType, GetBndFromList
  use GwtMstModule, only: GwtMstType, CastAsGwtMstType
  use GweMstModule, only: GweMstType
  use TspIcModule, only: TspIcType
  use TspFmiModule, only: TspFmiType
  use TspAdvModule, only: TspAdvType
  use TspSsmModule, only: TspSsmType
  use TspMvtModule, only: TspMvtType
  use TspOcModule, only: TspOcType
  use TspObsModule, only: TspObsType
  use BudgetModule, only: BudgetType
  use MatrixModule

  implicit none

  private

  public :: TransportModelType
  public :: niunit, cunit

  type, extends(NumericalModelType) :: TransportModelType

    ! Generalized transport package types common to either GWT or GWE
    class(*), pointer :: tspmst => null() !< flavor of MST package associated with this model type (GWT or GWE)
    type(TspAdvType), pointer :: adv => null() ! advection package
    type(TspFmiType), pointer :: fmi => null() ! flow model interface
    type(TspIcType), pointer :: ic => null() ! initial conditions package
    type(TspMvtType), pointer :: mvt => null() ! mover transport package
    type(TspObsType), pointer :: obs => null() ! observation package
    type(TspOcType), pointer :: oc => null() ! output control package
    type(TspSsmType), pointer :: ssm => null() ! source sink mixing package
    type(TspLabelsType), pointer :: tsplab => null() ! object defining the appropriate labels
    type(BudgetType), pointer :: budget => null() ! budget object
    integer(I4B), pointer :: inic => null() ! unit number IC
    integer(I4B), pointer :: infmi => null() ! unit number FMI
    integer(I4B), pointer :: inmvt => null() ! unit number MVT
    integer(I4B), pointer :: inadv => null() ! unit number ADV
    integer(I4B), pointer :: inssm => null() ! unit number SSM
    integer(I4B), pointer :: inoc => null() ! unit number OC
    integer(I4B), pointer :: inobs => null() ! unit number OBS
    integer(I4B), pointer :: inmst => null() ! unit number MST
    integer(I4B), pointer :: indsp => null() ! unit number DSP

  contains
  
    ! -- public
    procedure :: allocate_scalars
    procedure, public :: tsp_cr
    procedure, public :: tsp_df
    procedure, public :: tsp_ac
    procedure, public :: tsp_mc
    procedure, public :: tsp_ar
    procedure, public :: tsp_rp
    procedure, public :: tsp_ad
    procedure, public :: tsp_fc
    procedure, public :: tsp_cc
    procedure, public :: tsp_cq
    procedure, public :: tsp_bd
    procedure, public :: tsp_ot
    procedure, private :: ftype_check
    procedure, private :: tsp_ot_obs
    procedure, private :: tsp_ot_flow
    procedure, private :: tsp_ot_flowja
    procedure, private :: tsp_ot_dv
    procedure, private :: tsp_ot_bdsummary

  end type TransportModelType

  ! -- Module variables constant for simulation
  integer(I4B), parameter :: NIUNIT = 100
  character(len=LENFTYPE), dimension(NIUNIT) :: cunit
  data cunit/'DIS6 ', 'DISV6', 'DISU6', 'IC6  ', 'MST6 ', & !  5
    'ADV6 ', 'DSP6 ', 'SSM6 ', '     ', 'CNC6 ', & ! 10
    'OC6  ', 'OBS6 ', 'FMI6 ', 'SRC6 ', 'IST6 ', & ! 15
    'LKT6 ', 'SFT6 ', 'MWT6 ', 'UZT6 ', 'MVT6 ', & ! 20
    'API6 ', '     ', 'SFE6 ', '     ', '     ', & ! 25
    75*'     '/

contains

  subroutine allocate_scalars(this, modelname)
! ******************************************************************************
! allocate_scalars -- Allocate memory for non-allocatable members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(TransportModelType) :: this
    character(len=*), intent(in) :: modelname
! ------------------------------------------------------------------------------
    !
    ! -- allocate members from parent class
    call this%NumericalModelType%allocate_scalars(modelname)
    !
    ! -- allocate members that are part of model class
    !call mem_allocate(this%inic , 'INIC',  this%memoryPath)
    !call mem_allocate(this%infmi, 'INFMI', this%memoryPath)
    !call mem_allocate(this%inmvt, 'INMVT', this%memoryPath)
    !call mem_allocate(this%inmst, 'INMST', this%memoryPath)
    !call mem_allocate(this%inadv, 'INADV', this%memoryPath)
    !call mem_allocate(this%indsp, 'INDSP', this%memoryPath)
    !call mem_allocate(this%inssm, 'INSSM', this%memoryPath)
    !call mem_allocate(this%inoc,  'INOC ', this%memoryPath)
    !call mem_allocate(this%inobs, 'INOBS', this%memoryPath)
    !
    !this%inic  = 0
    !this%infmi = 0
    !this%inmvt = 0
    !this%inmst = 0
    !this%inadv = 0
    !this%indsp = 0
    !this%inssm = 0
    !this%inoc  = 0
    !this%inobs = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars

end module TransportModelModule

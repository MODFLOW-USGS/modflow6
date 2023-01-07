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
  use ConstantsModule, only: LENFTYPE
  use SimVariablesModule, only: errmsg
  use NumericalModelModule, only: NumericalModelType
  use TspLabelsModule, only: TspLabelsType

  implicit none

  private

  public :: TransportModelType
  public :: niunit, cunit

  type, extends(NumericalModelType) :: TransportModelType

  contains

  end type TransportModelType

  ! -- Module variables constant for simulation
  integer(I4B), parameter :: NIUNIT = 100
  character(len=LENFTYPE), dimension(NIUNIT) :: cunit
  data cunit/'DIS6 ', 'DISV6', 'DISU6', 'IC6  ', 'MST6 ', & !  5
    'ADV6 ', 'DSP6 ', 'SSM6 ', '     ', '     ', & ! 10
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

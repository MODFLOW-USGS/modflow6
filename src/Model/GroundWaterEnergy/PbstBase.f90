!> @brief This module contains common process-based stream temperature functionality
!!
!! This module contains methods for implementing functionality associated with
!! heat fluxes to a stream reach.  Four sources of thermal energy commonly 
!! accounted for in process-based stream temperature modeling include short-
!! wave radiation, long-wave radiation, sensible heat flux, and latent heat 
!! flux.  
!<
module PbstBaseModule
  use ConstantsModule, only: LINELENGTH, MAXCHARLEN, DZERO
  use KindModule, only: I4B, DP
  use NumericalPackageModule, only: NumericalPackageType
  use SimModule, only: count_errors, store_error, ustop
  use SimVariablesModule, only: errmsg
  use TdisModule, only: kper, nper, kstp
  use TimeSeriesLinkModule, only: TimeSeriesLinkType
  use TimeSeriesManagerModule, only: TimeSeriesManagerType, tsmanager_cr, &
                                     read_value_or_time_series_adv

  implicit none

  private
  
  public :: PbstBaseType
  public :: pbstbase_da
  
  type, abstract, extends(NumericalPackageType) :: PbstBaseType
    
    integer(I4B), pointer :: ncv => null() !< number of control volumes
    logical, pointer, public :: active => null() !< logical indicating if a sensible heat flux object is active
    character(len=LINELENGTH), pointer, public :: inputFilename => null() !< a particular pbst input file name, could be for sensible heat flux or latent heat flux subpackages, for example
    type(TimeSeriesManagerType), pointer, private :: tsmanager => null()
  
  contains

    procedure :: init
    procedure, private :: pbstbase_allocate_scalars
    procedure :: da => pbstbase_da
  
  end type PbstBaseType
  
  abstract interface
  
  end interface
  
contains

  !> @brief Initialize the PbstBaseType object
  !!
  !! Allocate and initialize data members of the object.
  !<
  subroutine init(this, name_model, pakname, ftype, inunit, iout)
    ! -- dummy
    class(PbstBaseType) :: this
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: pakname
    character(len=*), intent(in) :: ftype
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    !
    call this%set_names(1, name_model, pakname, ftype)
    call this%pbstbase_allocate_scalars()
    this%inunit = inunit
    this%iout = iout
    call this%parser%Initialize(this%inunit, this%iout)
  end subroutine init
  
  
  !> @brief Allocate scalar variables
  !!
  !! Allocate scalar data members of the object.
  !<
  subroutine pbstbase_allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(PbstBaseType) :: this
    !
    allocate (this%active)
    allocate (this%inputFilename)
    !
    ! -- initialize
    this%active = .false.
    this%inputFilename = ''
    !
    ! -- call standard NumericalPackageType allocate scalars
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- allocate
    call mem_allocate(this%ncv, 'NCV', this%memoryPath)
    !
    ! -- initialize
    this%ncv = 0
    !
    ! -- allocate time series manager
    allocate (this%tsmanager)
  end subroutine pbstbase_allocate_scalars

  !> @brief Deallocate package memory
  !!
  !! Deallocate package scalars and arrays.
  !<
  subroutine pbstbase_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(PbstBaseType) :: this
    !
    deallocate (this%active)
    deallocate (this%inputFilename)
    !
    ! -- Deallocate time series manager
    deallocate (this%tsmanager)
    ! 
    ! -- deallocate scalars
    call mem_deallocate(this%ncv)
    !
    ! -- Deallocate parent
    call this%NumericalPackageType%da()
  end subroutine pbstbase_da

end module PbstBaseModule
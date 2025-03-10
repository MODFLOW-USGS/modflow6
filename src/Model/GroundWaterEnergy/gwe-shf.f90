!> @brief This module contains methods for calculating sensible heat flux 
!!
!! This module contains the methods used to calculate the sensible heat flux
!! for surface-water boundaries, like streams and lakes.  In its current form,
!! this class acts like a package to a package, similar to the TVK package that
!! can be invoked from the NPF package.  Once this package is completed in its
!! prototyped form, it will likely be moved around.
!<

module SensHeatModule
  use ConstantsModule, only: LINELENGTH, LENMEMPATH, DZERO
  use KindModule, only: I4B, DP
  use MemoryManagerModule, only: mem_setptr
  use MemoryHelperModule, only: create_mem_path
  use SimModule, only: store_error
  use SimVariablesModule, only: errmsg
  use PbstBaseModule, only: PbstBaseType, pbstbase_da
  
  implicit none

  private
  
  public :: ShfType
  public :: shf_cr
  
  type, extends(PbstBaseType) :: ShfType

    real(DP), pointer :: rhoa => null() !< desity of air
    real(DP), pointer :: cpa => null() !< heat capacity of air
    real(DP), pointer :: cd => null() !< drag coefficient
    real(DP), dimension(:), pointer, contiguous :: va => null() !< wind speed
    real(DP), dimension(:), pointer, contiguous :: tatm => null() !< temperature of the atmosphere

  contains

    procedure :: da => shf_da
    procedure :: read_option => shf_read_option
    procedure :: pbst_options => shf_options
    procedure, private :: shf_allocate_scalars
  
  end type ShfType
  
contains
  
  !> @brief Create a new ShfType object
  !!
  !! Create a new sensible heat flux (ShfType) object. Initially for use with 
  !! the SFE package.
  !<
  subroutine shf_cr(shf, name_model, inunit, iout)
    ! -- dummy
    type(ShfType), pointer, intent(out) :: shf
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    !
    allocate (shf)
    call shf%init(name_model, 'SHF', 'SHF', inunit, iout)
    !
    ! -- allocate scalars
    call shf%shf_allocate_scalars()
  end subroutine shf_cr
  

  !> @brief Allocate scalars specific to the streamflow energy transport (SFE)
  !! package.
  !<
  subroutine shf_allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(ShfType) :: this
    ! 
    ! -- allocate
    call mem_allocate(this%rhoa, 'RHOA', this%memoryPath)
    call mem_allocate(this%cpa, 'CPA', this%memoryPath)
    call mem_allocate(this%cd, 'CD', this%memoryPath)
    !
    ! -- initialize to default values
    this%rhoa = 1.225  ! kg/m3
    this%cpa = 717.0  ! J/kg/C
    this%cd = 0.002  ! unitless
  end subroutine shf_allocate_scalars
  
  !> @brief Allocate arrays specific to the sensible heat flux (SHF) package
  !< 
  subroutine shf_allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(ShfType), intent(inout) :: this
    ! -- local
    integer(I4B) :: n
    !
    ! -- time series
    call mem_allocate(this%va, this%ncv, 'VA', this%memoryPath)
    call mem_allocate(this%tatm, this%ncv, 'TATM', this%memoryPath)
    !
    ! -- initialize
    do n = 1, this%ncv
      this%va(n) = DZERO
      this%tatm(n) = DZERO
    end do
  end subroutine

  !> @brief Set options specific to the ShfType
  !!
  !! This routine overrides PbstBaseType%bnd_options
  !<
  subroutine shf_options(this, option, found)
    ! -- dummy
    class(ShfType), intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical, intent(inout) :: found
    !
    found = .true.
    select case (option)
    case ('DENSITY_AIR')
      this%rhoa = this%parser%GetDouble()
      if (this%rhoa <= 0.0) then
        write (errmsg, '(a)') 'Specified value for the density of &
          &the atmosphere must be greater than 0.0.'
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      else
        write (this%iout, '(4x,a,1pg15.6)') &
          "The density of the atmosphere has been set to: ", this%rhoa
      end if
    case ('HEAT_CAPACITY_AIR')
      this%cpa = this%parser%GetDouble()
      if (this%cpa <= 0.0) then
        write (errmsg, '(a)') 'Specified value for the heat capacity of &
          &the atmosphere must be greater than 0.0.'
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      else
        write (this%iout, '(4x,a,1pg15.6)') &
          "The heat capacity of the atmosphere has been set to: ", this%cpa
      end if
    case ('DRAG_COEFFICIENT')
      this%cd = this%parser%GetDouble()
      if (this%cd <= 0.0) then
        write (errmsg, '(a)') 'Specified value for the drag coefficient &
          &must be greater than 0.0.'
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      else
        write (this%iout, '(4x,a,1pg15.6)') &
          "The heat capacity of the atmosphere has been set to: ", this%cpa
      end if
    case default
      write (errmsg, '(a,a)') 'Unknown SHF option: ', trim(option)
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end select
  end subroutine shf_options
  
  !> @brief Deallocate package memory
  !!
  !! Deallocate TVK package scalars and arrays.
  !<
  subroutine shf_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(ShfType) :: this
    !
    ! -- Nullify pointers to other package variables
    call mem_deallocate(this%rhoa)
    call mem_deallocate(this%cpa)
    call mem_deallocate(this%cd)
    !
    ! -- Deallocate parent
    call pbstbase_da(this)
  end subroutine shf_da
  
  !> @brief Read a SHF-specific option from the OPTIONS block
  !!
  !! Process a single SHF-specific option. Used when reading the OPTIONS block
  !! of the SHF package input file.
  !<
  function shf_read_option(this, keyword) result(success)
    ! -- dummy
    class(ShfType) :: this
    character(len=*), intent(in) :: keyword
    ! -- return
    logical :: success
    !
    ! -- There are no TVK-specific options, so just return false
    success = .false.
  end function shf_read_option
  
end module SensHeatModule

!> @brief This module contains common time-varying property functionality
!!
!! This module contains methods implementing functionality common to both
!! time-varying hydraulic conductivity (TVK) and time-varying storage (TVS)
!! packages.
!!
!<
module TvBaseModule
  use BaseDisModule, only: DisBaseType
  use ConstantsModule, only: LINELENGTH, MAXCHARLEN, DZERO
  use KindModule, only: I4B, DP, LGP
  use NumericalPackageModule, only: NumericalPackageType
  use SimModule, only: count_errors, store_error, store_error_filename, ustop
  use SimVariablesModule, only: errmsg
  use TdisModule, only: kper, nper, kstp
  use MemoryManagerModule, only: mem_setptr
  use CharacterStringModule, only: CharacterStringType

  implicit none

  private

  public :: TvBaseType
  public :: tvbase_da

  type, abstract, extends(NumericalPackageType) :: TvBaseType
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: setting => null() !< per period reallocated array containing setting names
    integer(I4B), pointer :: iper
    logical(LGP) :: ts_active = .false.
  contains
    procedure :: init
    procedure :: ar
    procedure :: rp
    procedure :: ad
    procedure :: da => tvbase_da
    procedure, private :: tvbase_allocate_scalars
    procedure(ar_set_pointers), deferred :: ar_set_pointers
    procedure(get_pointer_to_value), deferred :: get_pointer_to_value
    procedure(set_changed_at), deferred :: set_changed_at
    procedure(reset_change_flags), deferred :: reset_change_flags
    procedure(validate_change), deferred :: validate_change
    procedure(source_options), deferred :: source_options
  end type TvBaseType

  abstract interface

    !> @brief Announce package and set pointers to variables
    !!
    !! Deferred procedure called by the TvBaseType code to announce the
    !! specific package version and set any required array and variable
    !! pointers from other packages.
    !!
    !<
    subroutine ar_set_pointers(this)
      ! -- modules
      import TvBaseType
      ! -- dummy variables
      class(TvBaseType) :: this
    end subroutine

    !> @brief Get an array value pointer given a variable name and node index
    !!
    !! Deferred procedure called by the TvBaseType code to retrieve a pointer
    !! to a given node's value for a given named variable.
    !!
    !<
    function get_pointer_to_value(this, n, varName) result(bndElem)
      ! -- modules
      use KindModule, only: I4B, DP
      import TvBaseType
      ! -- dummy variables
      class(TvBaseType) :: this
      integer(I4B), intent(in) :: n
      character(len=*), intent(in) :: varName
      ! -- return
      real(DP), pointer :: bndElem
    end function

    !> @brief Mark property changes as having occurred at (kper, kstp)
    !!
    !! Deferred procedure called by the TvBaseType code when a property value
    !! change occurs at (kper, kstp).
    !!
    !<
    subroutine set_changed_at(this, kper, kstp)
      ! -- modules
      use KindModule, only: I4B
      import TvBaseType
      ! -- dummy variables
      class(TvBaseType) :: this
      integer(I4B), intent(in) :: kper
      integer(I4B), intent(in) :: kstp
    end subroutine

    !> @brief Clear all per-node change flags
    !!
    !! Deferred procedure called by the TvBaseType code when a new time step
    !! commences, indicating that any previously set per-node property value
    !! change flags should be reset.
    !!
    !<
    subroutine reset_change_flags(this)
      ! -- modules
      import TvBaseType
      ! -- dummy variables
      class(TvBaseType) :: this
    end subroutine

    !> @brief Check that a given property value is valid
    !!
    !! Deferred procedure called by the TvBaseType code after a property value
    !! change occurs to perform any required validity checks on the value of
    !! the given variable at the given node. Perform any required updates to
    !! the property value if it is valid, or log an error if not.
    !!
    !<
    subroutine validate_change(this, n, varName)
      ! -- modules
      use KindModule, only: I4B
      import TvBaseType
      ! -- dummy variables
      class(TvBaseType) :: this
      integer(I4B), intent(in) :: n
      character(len=*), intent(in) :: varName
    end subroutine

    !> @brief Source OPTIONS from input context
    !!
    !! Deferred procedure called by the TvBaseType code to source
    !! package options from the input context.
    !!
    !<
    subroutine source_options(this)
      ! -- modules
      import TvBaseType
      ! -- dummy variables
      class(TvBaseType) :: this
    end subroutine source_options

  end interface

contains

  !> @brief Initialise the TvBaseType object
  !!
  !! Allocate and initialize data members of the object.
  !!
  !<
  subroutine init(this, name_model, pakname, ftype, mempath, inunit, iout)
    ! -- dummy variables
    class(TvBaseType) :: this
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: pakname
    character(len=*), intent(in) :: ftype
    character(len=*), intent(in) :: mempath
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    !
    call this%set_names(1, name_model, pakname, ftype, mempath)
    call this%tvbase_allocate_scalars()
    this%inunit = inunit
    this%iout = iout
    !
    return
  end subroutine init

  !> @brief Allocate scalar variables
  !!
  !! Allocate scalar data members of the object.
  !!
  !<
  subroutine tvbase_allocate_scalars(this)
    ! -- dummy variables
    class(TvBaseType) :: this
    !
    ! -- Call standard NumericalPackageType allocate scalars
    call this%NumericalPackageType%allocate_scalars()
    !
    return
  end subroutine tvbase_allocate_scalars

  !> @brief Allocate and read method for package
  !!
  !! Allocate and read static data for the package.
  !!
  !<
  subroutine ar(this, dis)
    ! -- dummy variables
    class(TvBaseType) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    !
    ! -- Set pointers to other package variables and announce package
    this%dis => dis
    call this%ar_set_pointers()
    call mem_setptr(this%iper, 'IPER', this%input_mempath)
    !
    ! -- Source options
    call this%source_options
    !
    ! -- Terminate if any errors were encountered
    if (count_errors() > 0) then
      call store_error_filename(this%input_fname)
      call ustop()
    end if
    !
    return
  end subroutine ar

  !> @brief Read and prepare method for package
  !!
  !! Read and prepare stress period data for the package.
  !!
  !<
  subroutine rp(this)
    ! -- modules
    use TdisModule, only: kper
    ! -- dummy variables
    class(TvBaseType) :: this
    ! -- local variables
    integer(I4B), dimension(:, :), pointer, contiguous :: cellid
    real(DP), dimension(:), pointer, contiguous :: setting_value
    real(DP), pointer :: setval
    integer(I4B) :: n, node
    character(len=LINELENGTH) :: varName
    logical :: haveChanges
    character(len=LINELENGTH) :: cellstr
    ! -- formats
    character(len=*), parameter :: fmtvalchg = &
      "(a, ' package: Setting ', a, ' value for cell ', a, ' at start of &
      &stress period ', i0, ' = ', g12.5)"
    !
    if (this%iper /= kper) return
    !
    ! -- Reset input context pointers to reallocated arrays
    call mem_setptr(cellid, 'CELLID', this%input_mempath)
    call mem_setptr(this%setting, 'SETTING', this%input_mempath)
    call mem_setptr(setting_value, 'SETTING_VALUE', this%input_mempath)
    !
    ! -- Reset per-node property change flags
    call this%reset_change_flags()
    !
    haveChanges = .false.
    !
    do n = 1, size(this%setting)
      node = this%dis%get_nodenumber(cellid(1, n), cellid(2, n), cellid(3, n), 1)
      !
      ! -- Validate cell ID
      if (node < 1 .or. node > this%dis%nodes) then
        write (errmsg, '(a,2(1x,a))') &
          'CELLID', cellid, 'is not in the active model domain.'
        call store_error(errmsg)
        cycle
      end if
      !
      ! -- Set the variable name
      varname = this%setting(n)
      !
      setval => this%get_pointer_to_value(node, varName)
      if (.not. associated(setval)) then
        write (errmsg, '(a,3(1x,a),a)') &
          'Unknown', trim(adjustl(this%packName)), &
          "variable '", trim(varName), "'."
        call store_error(errmsg)
        cycle
      end if
      !
      ! -- Set the new value
      setval = setting_value(n)
      !
      ! -- Validate the new property value
      call this%validate_change(node, varName)
      haveChanges = .true.
      !
      ! -- Report value change
      if (this%iprpak /= 0) then
        call this%dis%noder_to_string(node, cellstr)
        write (this%iout, fmtvalchg) &
          trim(adjustl(this%packName)), trim(varName), trim(cellstr), &
          kper, setval
      end if
    end do
    !
    ! -- Record that any changes were made at the first time step of the
    ! -- stress period
    if (haveChanges) then
      call this%set_changed_at(kper, 1)
    end if
    !
    ! -- Terminate if errors were encountered in the PERIOD block
    if (count_errors() > 0) then
      call store_error_filename(this%input_fname)
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine rp

  !> @brief Advance the package
  !!
  !! Verify advanced data for a new time step.
  !!
  !<
  subroutine ad(this)
    ! -- dummy variables
    class(TvBaseType) :: this
    ! -- local variables
    character(len=LINELENGTH) :: varName
    integer(I4B) :: n
    !
    if (this%iper /= kper) return
    !
    ! -- Validate changes for period if timeseries is active
    if (size(this%setting) > 0 .and. this%ts_active) then
      ! -- Record that changes were made at the current time step
      call this%set_changed_at(kper, kstp)
      ! -- Reset node K change flags at all time steps except the first of eac
      call this%reset_change_flags()
      ! -- Validate all period updated property values
      do n = 1, size(this%setting)
        varName = this%setting(n)
        call this%validate_change(n, varName)
      end do
    end if
    !
    ! -- Terminate if there were errors
    if (count_errors() > 0) then
      call store_error_filename(this%input_fname)
      call ustop()
    end if
    !
    return
  end subroutine ad

  !> @brief Deallocate package memory
  !!
  !! Deallocate package scalars and arrays.
  !!
  !<
  subroutine tvbase_da(this)
    ! -- dummy variables
    class(TvBaseType) :: this
    !
    ! -- Deallocate parent
    call this%NumericalPackageType%da()
    !
    return
  end subroutine tvbase_da

end module TvBaseModule

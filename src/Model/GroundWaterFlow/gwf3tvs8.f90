module TvsModule
  use BaseDisModule, only: DisBaseType
  use ConstantsModule, only: LINELENGTH, LENMEMPATH, DZERO
  use KindModule, only: I4B, DP
  use MemoryManagerModule, only: mem_setptr
  use MemoryHelperModule, only: create_mem_path
  use SimModule, only: store_error
  use SimVariablesModule, only: errmsg
  use TvBaseModule, only: TvBaseType, tvbase_da

  implicit none

  private

  public :: TvsType
  public :: tvs_cr

  type, extends(TvBaseType) :: TvsType
    integer(I4B), pointer :: isfac => null()                      ! STO flag indicating if ss is read as storativity
    integer(I4B), pointer :: integratechanges => null()           ! STO flag indicating if ss is read as storativity
    integer(I4B), pointer :: iusesy => null()                     ! STO flag set if any cell is convertible (0, 1)
    real(DP), dimension(:), pointer, contiguous :: sc1 => null()  ! STO primary storage capacity (when cell is fully saturated)
    real(DP), dimension(:), pointer, contiguous :: sc2 => null()  ! STO secondary storage capacity (when cell is partially saturated)
  contains
    procedure :: da => tvs_da
    procedure :: ar_set_pointers => tvs_ar_set_pointers
    procedure :: read_option => tvs_read_option
    procedure :: get_pointer_to_value => tvs_get_pointer_to_value
    procedure :: set_changed_at => tvs_set_changed_at
    procedure :: reset_change_flags => tvs_reset_change_flags
    procedure :: validate_change => tvs_validate_change
  end type TvsType

contains

  subroutine tvs_cr(tvs, name_model, inunit, iout)
! ******************************************************************************
! tvs_cr -- Create a new TvsType object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    type(TvsType), pointer, intent(out) :: tvs
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    allocate(tvs)
    call tvs%init(name_model, 'TVS', 'TVS', inunit, iout)
    !
    return
  end subroutine tvs_cr

  subroutine tvs_ar_set_pointers(this)
! ******************************************************************************
! tvs_ar_set_pointers -- Announce package and set pointers to variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvsType) :: this
    !
    character(len=LENMEMPATH) :: stoMemoryPath
    !
    character(len=*), parameter :: fmttvs =                                    &
      "(1x,/1x,'TVS -- TIME-VARYING S PACKAGE, VERSION 1, 03/02/2021',         &
     &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- Print a message identifying the TVS package
    write(this%iout, fmttvs) this%inunit
    !
    ! -- Set pointers to other package variables
    ! -- STO
    stoMemoryPath = create_mem_path(this%name_model, 'STO')
    call mem_setptr(this%isfac, 'ISFAC', stoMemoryPath)
    call mem_setptr(this%integratechanges, 'INTEGRATECHANGES', stoMemoryPath)
    call mem_setptr(this%iusesy, 'IUSESY', stoMemoryPath)
    call mem_setptr(this%sc1, 'SC1', stoMemoryPath)
    call mem_setptr(this%sc2, 'SC2', stoMemoryPath)
    !
    ! -- Instruct STO to integrate storage changes by default, since TVS is active
    this%integratechanges = 1
    !
    return
  end subroutine tvs_ar_set_pointers

  function tvs_read_option(this, keyword) result(success)
! ******************************************************************************
! tvs_read_option -- Read a TVS-specific setting from the OPTIONS block
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvsType) :: this
    character(len=*), intent(in) :: keyword
    !
    logical :: success
    !
    character(len=*), parameter :: fmtdsci =                                   &
      "(4X,'DISABLE_STORAGE_CHANGE_INTEGRATION OPTION:',/,                     &
      &1X,'Storage derivative terms will not be added to STO matrix formulation')"
! ------------------------------------------------------------------------------
    !
    select case (keyword)
      case ('DISABLE_STORAGE_CHANGE_INTEGRATION')
        success = .true.
        this%integratechanges = 0
        write(this%iout, fmtdsci)
      case default
        success = .false.
    end select
    !
    return
  end function tvs_read_option

  function tvs_get_pointer_to_value(this, n, varName) result(bndElem)
! ******************************************************************************
! tvs_get_pointer_to_value -- Return a pointer to the property value for node n
!                             and the given variable name
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvsType) :: this
    integer(I4B), intent(in) :: n
    character(len=*), intent(in) :: varName
    !
    real(DP), pointer :: bndElem
! ------------------------------------------------------------------------------
    !
    select case (varName)
      case ('SS')
        bndElem => this%sc1(n)
      case ('SY')
        bndElem => this%sc2(n)
      case default
        bndElem => null()
    end select
    !
    return
  end function tvs_get_pointer_to_value

  subroutine tvs_set_changed_at(this, kper, kstp)
! ******************************************************************************
! tvs_set_changed_at -- Mark property changes as having occurred at (kper, kstp)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvsType) :: this
    integer(I4B), intent(in) :: kper
    integer(I4B), intent(in) :: kstp
! ------------------------------------------------------------------------------
    !
    ! -- No need to record TVS/STO changes, as no other packages cache SC1 or SC2 values
    !
    return
  end subroutine tvs_set_changed_at

  subroutine tvs_reset_change_flags(this)
! ******************************************************************************
! tvs_reset_change_flags -- Clear all per-node change flags
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvsType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- No need to record TVS/STO changes, as no other packages cache SC1 or SC2 values
    !
    return
  end subroutine tvs_reset_change_flags

  subroutine tvs_validate_change(this, n, varName)
! ******************************************************************************
! tvs_validate_change -- Multiply by area and thickness as necessary to get
!                        storage capacity, and check value is non-negative
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvsType) :: this
    integer(I4B), intent(in) :: n
    character(len=*), intent(in) :: varName
    !
    character(len=LINELENGTH) :: cellstr
    real(DP) :: thick
    !
    character(len=*), parameter :: fmtserr =                                   &
      "(1x, a, ' changed storage property ',a,' is < 0 for cell ',a, ' ', 1pg15.6)"
    character(len=*), parameter :: fmtsyerr =                                  &
      "(1x, a, ' cannot change ',a,' for cell ',a, ' because SY is unused in this model (all ICONVERT flags are 0).')"
! ------------------------------------------------------------------------------
    !
    ! -- Check the changed value is ok and convert to storage capacity
    if(varName == 'SS') then
      if(this%sc1(n) < DZERO) then
        call this%dis%noder_to_string(n, cellstr)
        write(errmsg, fmtserr) trim(adjustl(this%packName)), 'SS', trim(cellstr), this%sc1(n)
        call store_error(errmsg)
      else
        !
        ! -- Multiply by cell volume (if using specific storage) or area (if using storativity) to get primary storage capacity
        if(this%isfac == 0) then
          thick = this%dis%top(n) - this%dis%bot(n)
          this%sc1(n) = this%sc1(n) * thick * this%dis%area(n)
        else
          this%sc1(n) = this%sc1(n) * this%dis%area(n)
        endif
      endif
    elseif(varName == 'SY') then
      if(this%iusesy /= 1) then
        call this%dis%noder_to_string(n, cellstr)
        write(errmsg, fmtsyerr) trim(adjustl(this%packName)), 'SY', trim(cellstr)
        call store_error(errmsg)
      elseif(this%sc2(n) < DZERO) then
        call this%dis%noder_to_string(n, cellstr)
        write(errmsg, fmtserr) trim(adjustl(this%packName)), 'SY', trim(cellstr), this%sc2(n)
        call store_error(errmsg)
      else
        !
        ! -- Multiply by cell area to get secondary storage capacity
        this%sc2(n) = this%sc2(n) * this%dis%area(n)
      endif
    end if
    !
    return
  end subroutine tvs_validate_change

  subroutine tvs_da(this)
! ******************************************************************************
! tvs_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvsType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Nullify pointers to other package variables
    nullify(this%isfac)
    nullify(this%integratechanges)
    nullify(this%iusesy)
    nullify(this%sc1)
    nullify(this%sc2)
    !
    ! -- Deallocate parent
    call tvbase_da(this)
    !
    return
  end subroutine tvs_da

end module TvsModule

module TvkModule
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

  public :: TvkType
  public :: tvk_cr

  type, extends(TvBaseType) :: TvkType
    integer(I4B), pointer :: ik22overk => null()                             ! NPF flag that k22 is specified as anisotropy ratio
    integer(I4B), pointer :: ik33overk => null()                             ! NPF flag that k33 is specified as anisotropy ratio
    real(DP), dimension(:), pointer, contiguous :: k11 => null()             ! NPF hydraulic conductivity; if anisotropic, then this is Kx prior to rotation
    real(DP), dimension(:), pointer, contiguous :: k22 => null()             ! NPF hydraulic conductivity; if specified then this is Ky prior to rotation
    real(DP), dimension(:), pointer, contiguous :: k33 => null()             ! NPF hydraulic conductivity; if specified then this is Kz prior to rotation
    integer(I4B), pointer :: kchangeper => null()                            ! NPF last stress period in which any node K (or K22, or K33) values were changed (0 if unchanged from start of simulation)
    integer(I4B), pointer :: kchangestp => null()                            ! NPF last time step in which any node K (or K22, or K33) values were changed (0 if unchanged from start of simulation)
    integer(I4B), dimension(:), pointer, contiguous :: nodekchange => null() ! NPF grid array of flags indicating for each node whether its K (or K22, or K33) value changed (1) at (kchangeper, kchangestp) or not (0)
  contains
    procedure :: da => tvk_da
    procedure :: ar_set_pointers => tvk_ar_set_pointers
    procedure :: read_option => tvk_read_option
    procedure :: get_pointer_to_value => tvk_get_pointer_to_value
    procedure :: set_changed_at => tvk_set_changed_at
    procedure :: reset_change_flags => tvk_reset_change_flags
    procedure :: validate_change => tvk_validate_change
  end type TvkType

contains

  subroutine tvk_cr(tvk, name_model, inunit, iout)
! ******************************************************************************
! tvk_cr -- Create a new TvkType object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    type(TvkType), pointer, intent(out) :: tvk
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    allocate(tvk)
    call tvk%init(name_model, 'TVK', 'TVK', inunit, iout)
    !
    return
  end subroutine tvk_cr

  subroutine tvk_ar_set_pointers(this)
! ******************************************************************************
! tvk_ar_set_pointers -- Announce package and set pointers to variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvkType) :: this
    !
    character(len=LENMEMPATH) :: npfMemoryPath
    !
    character(len=*), parameter :: fmttvk =                                    &
      "(1x,/1x,'TVK -- TIME-VARYING K PACKAGE, VERSION 1, 03/02/2021',         &
     &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- Print a message identifying the TVK package
    write(this%iout, fmttvk) this%inunit
    !
    ! -- Set pointers to other package variables
    ! -- NPF
    npfMemoryPath = create_mem_path(this%name_model, 'NPF')
    call mem_setptr(this%ik22overk, 'IK22OVERK', npfMemoryPath)
    call mem_setptr(this%ik33overk, 'IK33OVERK', npfMemoryPath)
    call mem_setptr(this%k11, 'K11', npfMemoryPath)
    call mem_setptr(this%k22, 'K22', npfMemoryPath)
    call mem_setptr(this%k33, 'K33', npfMemoryPath)
    call mem_setptr(this%kchangeper, 'KCHANGEPER', npfMemoryPath)
    call mem_setptr(this%kchangestp, 'KCHANGESTP', npfMemoryPath)
    call mem_setptr(this%nodekchange, 'NODEKCHANGE', npfMemoryPath)
    !
    return
  end subroutine tvk_ar_set_pointers

  function tvk_read_option(this, keyword) result(success)
! ******************************************************************************
! tvk_read_option -- Read a TVK-specific setting from the OPTIONS block
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvkType) :: this
    character(len=*), intent(in) :: keyword
    !
    logical :: success
! ------------------------------------------------------------------------------
    !
    ! -- There are no TVK-specific options, so just return false
    success = .false.
    !
    return
  end function tvk_read_option

  function tvk_get_pointer_to_value(this, n, varName) result(bndElem)
! ******************************************************************************
! tvk_get_pointer_to_value -- Return a pointer to the property value for node n
!                             and the given variable name
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvkType) :: this
    integer(I4B), intent(in) :: n
    character(len=*), intent(in) :: varName
    !
    real(DP), pointer :: bndElem
! ------------------------------------------------------------------------------
    !
    select case (varName)
      case ('K')
        bndElem => this%k11(n)
      case ('K22')
        bndElem => this%k22(n)
      case ('K33')
        bndElem => this%k33(n)
      case default
        bndElem => null()
    end select
    !
    return
  end function tvk_get_pointer_to_value

  subroutine tvk_set_changed_at(this, kper, kstp)
! ******************************************************************************
! tvk_set_changed_at -- Mark property changes as having occurred at (kper, kstp)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvkType) :: this
    integer(I4B), intent(in) :: kper
    integer(I4B), intent(in) :: kstp
! ------------------------------------------------------------------------------
    !
    this%kchangeper = kper
    this%kchangestp = kstp
    !
    return
  end subroutine tvk_set_changed_at

  subroutine tvk_reset_change_flags(this)
! ******************************************************************************
! tvk_reset_change_flags -- Clear all per-node change flags
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvkType) :: this
    !
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- Clear NPF's nodekchange array
    do i = 1, this%dis%nodes
      this%nodekchange(i) = 0
    end do
    !
    return
  end subroutine tvk_reset_change_flags

  subroutine tvk_validate_change(this, n, varName)
! ******************************************************************************
! tvk_validate_change -- Multiply out K22 and K33 factors (if the corresponding
!                        NPF options are active), mark change and check value
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvkType) :: this
    integer(I4B), intent(in) :: n
    character(len=*), intent(in) :: varName
    !
    character(len=LINELENGTH) :: cellstr
    !
    character(len=*), parameter :: fmtkerr =                                   &
      "(1x, a, ' changed hydraulic property ',a,' is <= 0 for cell ',a, ' ', 1pg15.6)"
! ------------------------------------------------------------------------------
    !
    ! -- Mark the node as being changed this time step
    this%nodekchange(n) = 1
    !
    ! -- Check the changed value is ok
    if(varName == 'K') then
      if(this%k11(n) <= DZERO) then
        call this%dis%noder_to_string(n, cellstr)
        write(errmsg, fmtkerr) trim(adjustl(this%packName)), 'K', trim(cellstr), this%k11(n)
        call store_error(errmsg)
      end if
    elseif(varName == 'K22') then
      if(this%ik22overk == 1) then
        this%k22(n) = this%k22(n) * this%k11(n)
      end if
      if(this%k22(n) <= DZERO) then
        call this%dis%noder_to_string(n, cellstr)
        write(errmsg, fmtkerr) trim(adjustl(this%packName)), 'K22', trim(cellstr), this%k22(n)
        call store_error(errmsg)
      end if
    elseif(varName == 'K33') then
      if(this%ik33overk == 1) then
        this%k33(n) = this%k33(n) * this%k33(n)
      end if
      if(this%k33(n) <= DZERO) then
        call this%dis%noder_to_string(n, cellstr)
        write(errmsg, fmtkerr) trim(adjustl(this%packName)), 'K33', trim(cellstr), this%k33(n)
        call store_error(errmsg)
      end if
    end if
    !
    return
  end subroutine tvk_validate_change

  subroutine tvk_da(this)
! ******************************************************************************
! tvk_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TvkType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Nullify pointers to other package variables
    nullify(this%ik22overk)
    nullify(this%ik33overk)
    nullify(this%k11)
    nullify(this%k22)
    nullify(this%k33)
    nullify(this%kchangeper)
    nullify(this%kchangestp)
    nullify(this%nodekchange)
    !
    ! -- Deallocate parent
    call tvbase_da(this)
    !
    return
  end subroutine tvk_da

end module TvkModule

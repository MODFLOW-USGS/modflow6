module GwfOcModule

  use BaseDisModule,       only: DisBaseType
  use KindModule,          only: DP, I4B
  use ConstantsModule,     only: LENMODELNAME, LENORIGIN
  use OutputControlModule, only: OutputControlType
  use OutputControlData,   only: OutputControlDataType, ocd_cr

  implicit none
  private
  public GwfOcType, oc_cr

  type, extends(OutputControlType) :: GwfOcType
  contains
    procedure :: oc_ar
  end type GwfOcType
  
  contains

  subroutine oc_cr(ocobj, name_model, inunit, iout)
! ******************************************************************************
! oc_cr -- Create a new oc object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwfOcType), pointer :: ocobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(ocobj)
    !
    ! -- Allocate scalars
    call ocobj%allocate_scalars(name_model)
    !
    ! -- Save unit numbers
    ocobj%inunit = inunit
    ocobj%iout = iout
    !
    ! -- Initialize block parser
    call ocobj%parser%Initialize(inunit, iout)
    !
    ! -- Return
    return
  end subroutine oc_cr

  subroutine oc_ar(this, head, dis, dnodata)
! ******************************************************************************
! oc_ar -- allocate and read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfOcType) :: this
    real(DP), dimension(:), pointer, contiguous, intent(in) :: head
    class(DisBaseType), pointer, intent(in) :: dis
    real(DP), intent(in) :: dnodata
    ! -- local
    integer(I4B) :: i, nocdobj, inodata
    type(OutputControlDataType), pointer   :: ocdobjptr
    real(DP), dimension(:), pointer, contiguous :: nullvec => null()
! ------------------------------------------------------------------------------
    !
    ! -- Initialize variables
    inodata = 0
    nocdobj = 2
    allocate(this%ocdobj(nocdobj))
    do i = 1, nocdobj
      call ocd_cr(ocdobjptr)
      select case (i)
      case (1)
        call ocdobjptr%init_dbl('BUDGET', nullvec, dis, 'PRINT LAST ',     &
                                'COLUMNS 10 WIDTH 11 DIGITS 4 GENERAL ',       &
                                this%iout, dnodata)
      case (2)
        call ocdobjptr%init_dbl('HEAD', head, dis, 'PRINT LAST ',          &
                                'COLUMNS 10 WIDTH 11 DIGITS 4 GENERAL ',       &
                                this%iout, dnodata)
      end select
      this%ocdobj(i) = ocdobjptr
      deallocate(ocdobjptr)
    enddo
    !
    ! -- Read options or set defaults if this package not on
    if(this%inunit > 0) then
      call this%read_options()
    endif
    !
    ! -- Return
    return
  end subroutine oc_ar
 
end module GwfOcModule

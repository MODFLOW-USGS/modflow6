module TspOcModule

  use BaseDisModule, only: DisBaseType
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENMODELNAME
  use OutputControlModule, only: OutputControlType
  use OutputControlDataModule, only: OutputControlDataType, ocd_cr

  implicit none
  private
  public TspOcType, oc_cr

  !> @ brief Output control
  !!
  !!  Concrete implementation of OutputControlType for a
  !!  Transport Model
  !<
  type, extends(OutputControlType) :: TspOcType
  contains
    procedure :: oc_ar
  end type TspOcType

contains

  !> @ brief Create TspOcType
  !!
  !!  Create by allocating a new TspOcType object and initializing
  !!  member variables.
  !<
  subroutine oc_cr(ocobj, name_model, inunit, iout)
    ! -- dummy
    type(TspOcType), pointer :: ocobj !< TspOcType object
    character(len=*), intent(in) :: name_model !< name of the model
    integer(I4B), intent(in) :: inunit !< unit number for input
    integer(I4B), intent(in) :: iout !< unit number for output
    !
    ! -- Create the object
    allocate (ocobj)
    !
    ! -- Allocate variables
    call ocobj%allocate_scalars(name_model)
    !
    ! -- Save unit numbers
    ocobj%inunit = inunit
    ocobj%iout = iout
    !
    ! -- Initialize block parser
    call ocobj%parser%Initialize(inunit, iout)
  end subroutine oc_cr

  !> @ brief Allocate and read TspOcType
  !!
  !!  Setup dependent variable (e.g., concentration or temperature)
  !!  and budget as output control variables.
  !!
  !<
  subroutine oc_ar(this, depvar, dis, dnodata, dvname)
    ! -- dummy
    class(TspOcType) :: this !< TspOcType object
    real(DP), dimension(:), pointer, contiguous, intent(in) :: depvar !< model concentration
    character(len=*), intent(in) :: dvname !< name of dependent variable solved by generalized transport model (concentration, temperature)
    class(DisBaseType), pointer, intent(in) :: dis !< model discretization package
    real(DP), intent(in) :: dnodata !< no data value
    ! -- local
    integer(I4B) :: i, nocdobj, inodata
    type(OutputControlDataType), pointer :: ocdobjptr
    real(DP), dimension(:), pointer, contiguous :: nullvec => null()
    !
    ! -- Initialize variables
    inodata = 0
    nocdobj = 2
    allocate (this%ocds(nocdobj))
    do i = 1, nocdobj
      call ocd_cr(ocdobjptr)
      select case (i)
      case (1)
        call ocdobjptr%init_dbl('BUDGET', nullvec, dis, 'PRINT LAST ', &
                                'COLUMNS 10 WIDTH 11 DIGITS 4 GENERAL ', &
                                this%iout, dnodata)
      case (2)
        call ocdobjptr%init_dbl(trim(dvname), depvar, dis, 'PRINT LAST ', &
                                'COLUMNS 10 WIDTH 11 DIGITS 4 GENERAL ', &
                                this%iout, dnodata)
      end select
      this%ocds(i) = ocdobjptr
      deallocate (ocdobjptr)
    end do
    !
    ! -- Read options or set defaults if this package not on
    if (this%inunit > 0) then
      call this%read_options()
    end if
  end subroutine oc_ar

end module TspOcModule

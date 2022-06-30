module GwtIcModule

  use KindModule, only: DP, I4B
  use GwfIcModule, only: GwfIcType
  use BlockParserModule, only: BlockParserType
  use BaseDisModule, only: DisBaseType

  implicit none
  private
  public :: GwtIcType
  public :: ic_cr

  ! -- Most of the GwtIcType functionality comes from GwfIcType
  type, extends(GwfIcType) :: GwtIcType
  contains
    procedure :: read_data
  end type GwtIcType

contains

  subroutine ic_cr(ic, name_model, inunit, iout, dis)
! ******************************************************************************
! ic_cr -- Create a new initial conditions object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtIcType), pointer :: ic
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    class(DisBaseType), pointer, intent(in) :: dis
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate (ic)
    !
    ! -- create name and memory path
    call ic%set_names(1, name_model, 'IC', 'IC')
    !
    ! -- Allocate scalars
    call ic%allocate_scalars()
    !
    ic%inunit = inunit
    ic%iout = iout
    !
    ! -- set pointers
    ic%dis => dis
    !
    ! -- Initialize block parser
    call ic%parser%Initialize(ic%inunit, ic%iout)
    !
    ! -- Return
    return
  end subroutine ic_cr

  subroutine read_data(this)
! ******************************************************************************
! read_data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    ! -- dummy
    class(GwtIcType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    character(len=:), allocatable :: line
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    character(len=24) :: aname(1)
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Setup the label
    aname(1) = 'INITIAL CONCENTRATION'
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
        case ('STRT')
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%strt, &
                                        aname(1))
        case default
          write (errmsg, '(4x,a,a)') 'ERROR. UNKNOWN GRIDDATA TAG: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END PROCESSING GRIDDATA'
    else
      call store_error('ERROR.  REQUIRED GRIDDATA BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Return
    return
  end subroutine read_data

end module GwtIcModule

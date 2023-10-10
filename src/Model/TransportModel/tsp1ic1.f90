module TspIcModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENVARNAME
  use GwfIcModule, only: GwfIcType
  use BlockParserModule, only: BlockParserType
  use BaseDisModule, only: DisBaseType

  implicit none
  private
  public :: TspIcType
  public :: ic_cr

  ! -- Most of the TspIcType functionality comes from GwfIcType
  type, extends(GwfIcType) :: TspIcType
    ! -- strings
    character(len=LENVARNAME) :: depvartype = ''

  contains

    procedure :: read_data

  end type TspIcType

contains

  !> @brief Create a new initial conditions object
  !<
  subroutine ic_cr(ic, name_model, inunit, iout, dis, depvartype)
    ! -- dummy
    type(TspIcType), pointer :: ic
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    class(DisBaseType), pointer, intent(in) :: dis
    character(len=LENVARNAME), intent(in) :: depvartype
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
    ! -- Give package access to the assigned labelsd based on dependent variable
    ic%depvartype = depvartype
    !
    ! -- Initialize block parser
    call ic%parser%Initialize(ic%inunit, ic%iout)
    !
    ! -- Return
    return
  end subroutine ic_cr

  !> @brief Read initial conditions
  !!
  !! Read initial concentrations or temperatures depending on model type
  !<
  subroutine read_data(this)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    ! -- dummy
    class(TspIcType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    character(len=:), allocatable :: line
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    character(len=24) :: aname(1)
    ! -- formats
    !
    ! -- Setup the label
    write (aname(1), '(a,1x,a)') 'INITIAL', trim(adjustl(this%depvartype))
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
          write (errmsg, '(a,a)') 'Unknown GRIDDATA tag: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END PROCESSING GRIDDATA'
    else
      call store_error('Required GRIDDATA block not found.')
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Return
    return
  end subroutine read_data

end module TspIcModule

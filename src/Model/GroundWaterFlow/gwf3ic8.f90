module GwfIcModule

  use KindModule, only: DP, I4B
  use NumericalPackageModule, only: NumericalPackageType
  use BlockParserModule, only: BlockParserType
  use BaseDisModule, only: DisBaseType

  implicit none
  private
  public :: GwfIcType
  public :: ic_cr

  type, extends(NumericalPackageType) :: GwfIcType
    real(DP), dimension(:), pointer, contiguous :: strt => null() ! starting head
  contains
    procedure :: ic_ar
    procedure :: ic_da
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure :: read_data
  end type GwfIcType

contains

  subroutine ic_cr(ic, name_model, inunit, iout, dis)
! ******************************************************************************
! ic_cr -- Create a new initial conditions object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwfIcType), pointer :: ic
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

  subroutine ic_ar(this, x)
! ******************************************************************************
! ic_ar -- Allocate and read initial conditions
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use BaseDisModule, only: DisBaseType
    use SimModule, only: store_error
    ! -- dummy
    class(GwfIcType) :: this
    real(DP), dimension(:), intent(inout) :: x
    ! -- locals
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Print a message identifying the initial conditions package.
    write (this%iout, 1) this%inunit
1   format(1x, /1x, 'IC -- INITIAL CONDITIONS PACKAGE, VERSION 8, 3/28/2015', &
           ' INPUT READ FROM UNIT ', i0)
    !
    ! -- Allocate arrays
    call this%allocate_arrays(this%dis%nodes)
    !
    ! -- Read options
    call this%read_options()
    !
    ! -- Read data
    call this%read_data()
    !
    ! -- Assign x equal to strt
    do n = 1, this%dis%nodes
      x(n) = this%strt(n)
    end do
    !
    ! -- Return
    return
  end subroutine ic_ar

  subroutine ic_da(this)
! ******************************************************************************
! ic_da -- Deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwfIcType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Scalars
    !
    ! -- Arrays
    call mem_deallocate(this%strt)
    !
    ! -- Return
    return
  end subroutine ic_da

  subroutine allocate_arrays(this, nodes)
! ******************************************************************************
! allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfIcType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Allocate
    call mem_allocate(this%strt, nodes, 'STRT', this%memoryPath)
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine read_options(this)
! ******************************************************************************
! read_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    ! -- dummy
    class(GwfIcType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING IC OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case default
          write (errmsg, '(4x,a,a)') 'Unknown IC option: ', trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF IC OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

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
    class(GwfIcType) :: this
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
    aname(1) = '    INITIAL HEAD'
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
          write (errmsg, '(4x,a,a)') 'Unknown GRIDDATA tag: ', trim(keyword)
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

end module GwfIcModule

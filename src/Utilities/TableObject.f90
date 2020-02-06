! Comprehensive table object that stores all of the 
! intercell flows, and the inflows and the outflows for 
! an advanced package.
module TableObjectModule
  
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LINELENGTH, LENBUDTXT
  use TableModule, only : TableType, table_cr
  use TableTermModule, only: TableTermType
  use BaseDisModule, only: DisBaseType
  use DeferredStringModule, only: deferred_string_type

  
  implicit none
  
  public :: TableObjectType
  public :: tableobject_cr
  
  type :: TableObjectType
    !
    ! -- name, number of control volumes, and number of table terms
    character(len=LENBUDTXT) :: name
    integer(I4B) :: ncv
    integer(I4B) :: ntableterm
    !
    ! -- array of table terms, with one separate entry for each term
    !    such as rainfall, et, leakage, etc.
    integer(I4B) :: iterm
    type(TableTermType), dimension(:), allocatable :: tableterm
    !
    ! -- table table object, for writing the typical MODFLOW table
    type(TableType), pointer :: table => null()
    
    type(deferred_string_type), pointer :: linesep => null()
    type(deferred_string_type), pointer :: dataline => null()
    type(deferred_string_type), dimension(:), pointer :: header => null()

    
  contains
  
    procedure :: tableobject_df
    procedure :: set_header
    procedure :: accumulate_terms
    procedure :: write_table
    procedure :: save_flows
    procedure :: tableobject_da
    
    procedure, private :: allocate_strings
    
  end type TableObjectType
  
  contains

  subroutine tableobject_cr(this, name, title, transient)
! ******************************************************************************
! tableobject_cr -- Create a new table object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    type(TableObjectType), pointer :: this
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: title
    logical, intent(in), optional :: transient
    ! -- local
    logical :: ltran
! ------------------------------------------------------------------------------
    !
    ! -- process optional variables
    if (present(transient)) then
      ltran = transient
    else
      ltran = .FALSE.
    end if
    !
    ! -- Create the object
    allocate(this)
    !
    ! -- initialize variables
    this%name = name
    this%ncv = 0
    this%ntableterm = 0
    this%iterm = 0
    !
    ! -- initialize table
    call table_cr(this%table, name, title, transient=ltran)
    !
    ! -- Return
    return
  end subroutine tableobject_cr

  subroutine tableobject_df(this, ncv, ntableterm)
! ******************************************************************************
! tableobject_df -- Define the new table object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableObjectType) :: this
    integer(I4B), intent(in) :: ncv
    integer(I4B), intent(in) :: ntableterm
! ------------------------------------------------------------------------------
    !
    ! -- set values
    this%ncv = ncv
    this%ntableterm = ntableterm
    !
    ! -- allocate space for tableterm
    allocate(this%tableterm(ntableterm))
    !
    ! -- setup the table table object
    call this%table%table_df(ntableterm)
    !
    ! -- return
    return
  end subroutine tableobject_df

  subroutine set_header(this)
! ******************************************************************************
! set_header -- Set the table object header
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableObjectType) :: this
    ! -- local
    integer(I4B) :: width
    integer(I4B) :: nlines
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    width = 0
    nlines = 0
    !
    ! -- determine total width and maximum number of lines
    do n = 1, this%ntableterm
      width = width + this%tableterm(n)%get_width()
      nlines = max(nlines, this%tableterm(n)%get_header_lines())
    end do
    !
    ! -- allocate the header and line separator
    call this%allocate_strings(width, nlines)
    !
    ! -- return
    return
  end subroutine set_header
  
  subroutine allocate_strings(this, width, nlines)
! ******************************************************************************
! allocate_header -- Allocate deferred length strings
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableObjectType) :: this
    integer(I4B) :: width
    integer(I4B) :: nlines
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- allocate deferred length strings
    !
    ! -- return
    return
  end subroutine allocate_strings  
  
  
  subroutine accumulate_terms(this)
! ******************************************************************************
! accumulate_terms -- add up accumulators and submit to table table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(TableObjectType) :: this
    ! -- dummy
    character(len=LENBUDTXT) :: flowtype    
    integer(I4B) :: i
    real(DP) :: ratin, ratout
! ------------------------------------------------------------------------------
    !
    ! -- reset the table table
    call this%table%reset()
    !
    ! -- calculate the table table terms
    do i = 1, this%ntableterm
      !
      ! -- accumulate positive and negative flows for each table term
!      flowtype = this%tableterm(i)%flowtype
!      select case (trim(adjustl(flowtype)))
!      case ('FLOW-JA-FACE')
!        ! skip
!      case default
!        !
!        ! -- calculate sum of positive and negative flows
!!        call this%tableterm(i)%accumulate_flow(ratin, ratout)
!        !
!        ! -- pass accumulators into the table table
!        call this%table%addentry(ratin, ratout, delt, flowtype)
!      end select
    end do
    !
    ! -- return
    return
  end subroutine accumulate_terms

  subroutine write_table(this, kstp, kper, iout)
! ******************************************************************************
! write_table -- Write the table table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableObjectType) :: this
    integer(I4B),intent(in) :: kstp
    integer(I4B),intent(in) :: kper
    integer(I4B),intent(in) :: iout
    ! -- dummy
! ------------------------------------------------------------------------------
    !
    ! -- write the table
    call this%table%table_ot(kstp, kper, iout)
    !
    ! -- return
    return
  end subroutine write_table
  
  subroutine save_flows(this, dis, ibinun, kstp, kper, delt, &
                        pertim, totim, iout)
! ******************************************************************************
! write_table -- Write the table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableObjectType) :: this
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: ibinun
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    real(DP), intent(in) :: delt
    real(DP), intent(in) :: pertim
    real(DP), intent(in) :: totim
    integer(I4B), intent(in) :: iout
    ! -- dummy
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- save flows for each table term
    do i = 1, this%ntableterm
      !call this%tableterm(i)%save_flows(dis, ibinun, kstp, kper, delt, &
      !                                pertim, totim, iout)
    end do
    !
    ! -- return
    return
  end subroutine save_flows
  
  subroutine tableobject_da(this)
! ******************************************************************************
! tableobject_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableObjectType) :: this
    ! -- dummy
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- save flows for each table term
    do i = 1, this%ntableterm
      call this%tableterm(i)%da()
    end do
    !
    ! -- Return
    return
  end subroutine tableobject_da
  
end module TableObjectModule
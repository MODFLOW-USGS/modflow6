! Comprehensive table object that stores all of the 
! intercell flows, and the inflows and the outflows for 
! an advanced package.
module TableModule
  
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LINELENGTH, LENBUDTXT,                              &
                             TABSTRING, TABUCSTRING, TABINTEGER, TABREAL
  use TableTermModule, only: TableTermType
  use BaseDisModule, only: DisBaseType
  use DeferredStringModule, only: deferred_string_type
  use InputOutputModule, only: UWWORD

  
  implicit none
  
  public :: TableType
  public :: table_cr
  
  type :: TableType
    !
    ! -- name, number of control volumes, and number of table terms
    character(len=LENBUDTXT) :: name
    character(len=LINELENGTH) :: title
    logical :: first_entry
    integer(I4B) :: iout
    integer(I4B) :: ncv
    integer(I4B) :: nheaderlines
    integer(I4B) :: nlinewidth
    integer(I4B) :: ntableterm
    !
    ! -- array of table terms, with one separate entry for each term
    !    such as rainfall, et, leakage, etc.
    integer(I4B) :: iterm
    type(TableTermType), dimension(:), allocatable :: tableterm
    !
    ! -- table table object, for writing the typical MODFLOW table
    type(TableType), pointer :: table => null()
    
    !type(deferred_string_type), pointer :: linesep => null()
    !type(deferred_string_type), pointer :: dataline => null()
    !type(deferred_string_type), dimension(:), pointer :: header => null()
    character(len=LINELENGTH), pointer :: linesep => null()
    character(len=LINELENGTH), pointer :: dataline => null()
    character(len=LINELENGTH), dimension(:), pointer :: header => null()

    
  contains
  
    procedure :: table_df
    procedure :: set_header
    procedure :: write_line
    procedure :: finalize_table
    procedure :: table_da
    
    procedure, private :: allocate_strings
    procedure, private :: write_header
    
  end type TableType
  
  contains

  subroutine table_cr(this, name, title, iout)
! ******************************************************************************
! table_cr -- Create a new table object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    type(TableType), pointer :: this
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: title
    integer(I4B), intent(in) :: iout
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(this)
    !
    ! -- initialize variables
    this%name = name
    this%title = title
    this%ncv = 0
    this%ntableterm = 0
    this%iout = iout
    this%first_entry = .TRUE.
    !
    ! -- Return
    return
  end subroutine table_cr

  subroutine table_df(this, ncv, ntableterm)
! ******************************************************************************
! table_df -- Define the new table object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableType) :: this
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
    ! -- return
    return
  end subroutine table_df

  subroutine set_header(this)
! ******************************************************************************
! set_header -- Set the table object header
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableType) :: this
    ! -- local
    character(len=LINELENGTH) :: cval
    integer(I4B) :: width
    integer(I4B) :: alignment
    integer(I4B) :: nlines
    integer(I4B) :: iloc
    integer(I4B) :: ival
    real(DP) :: rval
    integer(I4B) :: j
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
    ! -- add length of separators
    width = width + this%ntableterm - 1
    !
    ! -- allocate the header and line separator
    call this%allocate_strings(width, nlines)
    !
    ! -- build final header lines
    do n = 1, this%ntableterm
      call this%tableterm(n)%set_header(nlines)
    end do
    !
    ! -- build header
    do n = 1, nlines
      iloc = 1
      do j = 1, this%ntableterm
        width = this%tableterm(j)%get_width()
        alignment = this%tableterm(j)%get_alignment()
        call this%tableterm(j)%get_header(n, cval)
        if (j == this%ntableterm) then
          call UWWORD(this%header(n+1), iloc, width, TABUCSTRING,                &
                      cval(1:width), ival, rval, ALIGNMENT=alignment)
          !call UWWORD(this%header(n+1)%string, iloc, width, TABUCSTRING,         &
          !            cval(1:width), ival, rval, ALIGNMENT=alignment)
        else
          !call UWWORD(this%header(n+1)%string, iloc, width, TABUCSTRING,         &
          !            cval(1:width), ival, rval, ALIGNMENT=alignment, SEP=' ')
          call UWWORD(this%header(n+1), iloc, width, TABUCSTRING,                &
                      cval(1:width), ival, rval, ALIGNMENT=alignment, SEP=' ')
        end if
      end do
    end do
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
    class(TableType) :: this
    integer(I4B), intent(in) :: width
    integer(I4B), intent(in) :: nlines
    ! -- local
    character(len=width) :: string
    character(len=width) :: linesep
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- initialize local variables
    string = ''
    linesep = repeat('-', width)
    !
    ! -- initialize variables
    this%nheaderlines = nlines + 2
    this%nlinewidth = width
    !
    ! -- allocate deferred length strings
    allocate(this%header(this%nheaderlines))
    allocate(this%linesep)
    allocate(this%dataline)
    !!
    !! -- initialize lines
    !this%linesep%string = linesep(1:width)
    !this%dataline%string = string(1:width)
    !do n = 1, this%nheaderlines
    !  this%header(n)%string = string(1:width)
    !end do
    !!
    !! -- fill first and last header line with
    !!    linesep
    !this%header(1)%string = linesep(1:width)
    !this%header(nlines+2)%string = linesep(1:width)
    !
    ! -- initialize lines
    this%linesep = linesep(1:width)
    this%dataline = string(1:width)
    do n = 1, this%nheaderlines
      this%header(n) = string(1:width)
    end do
    !
    ! -- fill first and last header line with
    !    linesep
    this%header(1) = linesep(1:width)
    this%header(nlines+2) = linesep(1:width)
    !
    ! -- return
    return
  end subroutine allocate_strings  

  subroutine write_line(this, kstp, kper)
! ******************************************************************************
! write_table -- Write the table table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableType) :: this
    integer(I4B),intent(in), optional :: kstp
    integer(I4B),intent(in), optional :: kper
    ! -- local
    character(len=LINELENGTH) :: title
    integer(I4B) :: width
! ------------------------------------------------------------------------------
    !
    ! -- initialize local variables
    width = this%nlinewidth
    !
    ! -- write the table header
    if (this%first_entry .EQV. .TRUE.) then
      title = this%title
      call this%write_header(title)
    end if
    !
    ! -- write the line
    !
    ! -- return
    return
  end subroutine write_line

  subroutine write_header(this, title)
! ******************************************************************************
! write_table -- Write the table table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableType) :: this
    character(len=*), intent(in) :: title
    ! -- local
    integer(I4B) :: width
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- initialize local variables
    width = this%nlinewidth
    !
    ! -- write the table header
    write(this%iout, '(1x,a)') trim(adjustl(title))
    do n = 1, this%nheaderlines
      !write(this%iout, '(1x,a)') this%header(n)%string(1:width)
      write(this%iout, '(1x,a)') this%header(n)(1:width)
    end do
    this%first_entry = .FALSE.
    !
    ! -- return
    return
  end subroutine write_header

  subroutine finalize_table(this)
! ******************************************************************************
! write_table -- Write the table table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableType) :: this
    ! -- local
    integer(I4B) :: width
! ------------------------------------------------------------------------------
    !
    ! -- initialize local variables
    width = this%nlinewidth
    !
    ! -- write the final table seperator
    !write(this%iout, '(1x,a,/)') this%linesep%string(1:width)
    write(this%iout, '(1x,a,/)') this%linesep(1:width)
    !
    ! -- reset first entry
    this%first_entry = .TRUE.
    !
    ! -- return
    return
  end subroutine finalize_table
  
  subroutine table_da(this)
! ******************************************************************************
! table_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableType) :: this
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
  end subroutine table_da
  
end module TableModule
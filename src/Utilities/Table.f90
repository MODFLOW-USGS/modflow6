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
    logical, pointer :: first_entry => null()
    integer(I4B), pointer :: iout => null()
    integer(I4B), pointer :: maxbound => null()
    integer(I4B), pointer :: nheaderlines => null()
    integer(I4B), pointer :: nlinewidth => null()
    integer(I4B), pointer :: ntableterm => null()
    integer(I4B), pointer :: ientry => null()
    integer(I4B), pointer :: iloc => null()
    integer(I4B), pointer :: icount => null()
    !
    ! -- array of table terms, with one separate entry for each term
    !    such as rainfall, et, leakage, etc.
    type(TableTermType), dimension(:), pointer :: tableterm => null()
    !
    ! -- table table object, for writing the typical MODFLOW table
    type(TableType), pointer :: table => null()
    
    character(len=LINELENGTH), pointer :: linesep => null()
    character(len=LINELENGTH), pointer :: dataline => null()
    character(len=LINELENGTH), dimension(:), pointer :: header => null()
    
    contains
  
    procedure :: table_df
    procedure :: table_da
    procedure :: initialize_column
    procedure :: set_maxbound

    procedure, private :: allocate_strings
    procedure :: set_header     ! make private
    procedure :: write_header   ! make private
    procedure :: write_line     ! make private
    procedure :: finalize_table ! make private
    
    generic, public :: add_term => add_integer, add_real, add_string
    procedure, private :: add_integer, add_real, add_string    

  end type TableType
  
  contains

  subroutine table_cr(this, name, title)
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
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(this)
    !
    ! -- initialize variables
    this%name = name
    this%title = title
    !
    ! -- Return
    return
  end subroutine table_cr

  subroutine table_df(this, maxbound, ntableterm, iout)
! ******************************************************************************
! table_df -- Define the new table object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableType) :: this
    integer(I4B), intent(in) :: maxbound
    integer(I4B), intent(in) :: ntableterm
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars
    allocate(this%first_entry)
    allocate(this%iout)
    allocate(this%maxbound)
    allocate(this%nheaderlines)
    allocate(this%nlinewidth)
    allocate(this%ntableterm)
    allocate(this%ientry)
    allocate(this%iloc)
    allocate(this%icount)
    !
    ! -- allocate space for tableterm
    allocate(this%tableterm(ntableterm))
    !
    ! -- initialize values
    this%first_entry = .TRUE.
    this%iout = iout
    this%maxbound = maxbound
    this%ntableterm = ntableterm
    this%ientry = 0
    this%icount = 0
    !
    ! -- return
    return
  end subroutine table_df
  
  subroutine initialize_column(this, text, width, alignment, datatype)
! ******************************************************************************
! initialize_column -- Initialize data for a column
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableType) :: this
    character(len=*), intent(in) :: text
    integer(I4B), intent(in) :: width
    integer(I4B), intent(in) :: alignment
    integer(I4B), intent(in) :: datatype
    ! -- local
    integer(I4B) :: idx
! ------------------------------------------------------------------------------
    !
    ! -- update index for tableterm
    this%ientry = this%ientry + 1
    idx = this%ientry
    !
    ! -- check that ientry is in bounds
    if (this%ientry > this%ntableterm) then
      call this%tableterm(idx)%initialize(text, width,                           &
                                          alignment=alignment,                   &
                                          datatype=datatype)      
    end if
    !
    ! -- create header when all terms have been specified
    if (this%ientry == this%ntableterm) then
      call this%set_header()
    end if
    !
    ! -- return
    return
  end subroutine initialize_column
  
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
      this%iloc = 1
      do j = 1, this%ntableterm
        width = this%tableterm(j)%get_width()
        alignment = this%tableterm(j)%get_alignment()
        call this%tableterm(j)%get_header(n, cval)
        if (j == this%ntableterm) then
          call UWWORD(this%header(n+1), iloc, width, TABUCSTRING,                &
                      cval(1:width), ival, rval, ALIGNMENT=alignment)
        else
          call UWWORD(this%header(n+1), iloc, width, TABUCSTRING,                &
                      cval(1:width), ival, rval, ALIGNMENT=alignment, SEP=' ')
        end if
      end do
    end do
    !
    ! -- reset column count
    this%ientry = 0
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

  subroutine write_header(this, kstp, kper)
! ******************************************************************************
! write_table -- Write the table header
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
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- initialize local variables
    width = this%nlinewidth
    !
    ! -- write the table header
    if (this%first_entry) then
      ! -- write title
      title = this%title
      if (present(kper)) then
        write(title, '(a,a,i6)') trim(adjustl(title)), '   PERIOD ', kper
      end if
      if (present(kstp)) then
        write(title, '(a,a,i8)') trim(adjustl(title)), '   STEP ', kstp
      end if
      write(this%iout, '(1x,a)') trim(adjustl(title))
      !
      ! -- write header
      do n = 1, this%nheaderlines
        write(this%iout, '(1x,a)') this%header(n)(1:width)
      end do
    end if
    !
    ! -- reinitialize variables
    this%first_entry = .FALSE.
    this%icount = 0
    !
    ! -- return
    return
  end subroutine write_header
  
  subroutine write_line(this)
! ******************************************************************************
! write_line -- Write the data line
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
    ! -- write the dataline
    write(this%iout, '(1x,a)') this%dataline(1:width)
    !
    ! -- update column and line counters
    this%ientry = 0
    this%icount = this%icount + 1
    !
    ! -- return
    return
  end subroutine write_line
  
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
    write(this%iout, '(1x,a,/)') this%linesep(1:width)
    !
    ! -- reinitialize variables
    this%ientry = 0
    this%icount = 0
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
    ! -- deallocate scalars
    deallocate(this%first_entry)
    deallocate(this%iout)
    deallocate(this%maxbound)
    deallocate(this%nheaderlines)
    deallocate(this%nlinewidth)
    deallocate(this%ntableterm)
    deallocate(this%ientry)
    deallocate(this%iloc)
    deallocate(this%icount)
    !
    ! -- deallocate each table term
    do i = 1, this%ntableterm
      call this%tableterm(i)%da()
    end do
    !
    ! -- deallocate space for tableterm
    deallocate(this%tableterm)
    !
    ! -- Return
    return
  end subroutine table_da
  
  subroutine add_integer(this, ival)
! ******************************************************************************
! add_integer -- add integer value to the dataline
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableType) :: this
    integer(I4B), intent(in) :: ival
    ! -- dummy
    logical :: line_end
    character(len=LINELENGTH) :: cval
    real(DP) :: rval
    integer(I4B) :: width
    integer(I4B) :: alignment
    integer(I4B) :: j
! ------------------------------------------------------------------------------
    !
    ! -- update index for tableterm
    this%ientry = this%ientry + 1
    !
    ! -- initialize local variables
    j = this%ientry
    width = this%tableterm(j)%get_width()
    alignment = this%tableterm(j)%get_alignment()
    if (j == this%ntableterm) then
      line_end = .TRUE.
      call UWWORD(this%dataline, this%iloc, width, TABINTEGER,                   &
                  cval, ival, rval, ALIGNMENT=alignment)
    else
      line_end = .FALSE.
      call UWWORD(this%dataline, this%iloc, width, TABINTEGER,                   &
                  cval, ival, rval, ALIGNMENT=alignment)
    end if
    !
    ! -- write the data line
    if (line_end) then
      call this%write_line()
    end if
    !
    ! -- finalize the table
    if (this%icount == this%maxbound) then
      call this%finalize_table()
    end if
    !
    ! -- Return
    return
  end subroutine add_integer

  subroutine add_real(this, rval)
! ******************************************************************************
! add_real -- add real value to the dataline
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableType) :: this
    real(DP), intent(in) :: rval
    ! -- dummy
    logical :: line_end
    character(len=LINELENGTH) :: cval
    integer(I4B) :: ival
    integer(I4B) :: j
    integer(I4B) :: width
    integer(I4B) :: alignment
! ------------------------------------------------------------------------------
    !
    ! -- update index for tableterm
    this%ientry = this%ientry + 1
    !
    ! -- initialize local variables
    j = this%ientry
    width = this%tableterm(j)%get_width()
    alignment = this%tableterm(j)%get_alignment()
    if (j == this%ntableterm) then
      line_end = .TRUE.
      call UWWORD(this%dataline, this%iloc, width, TABREAL,                      &
                  cval, ival, rval, ALIGNMENT=alignment)
    else
      line_end = .FALSE.
      call UWWORD(this%dataline, this%iloc, width, TABREAL,                      &
                  cval, ival, rval, ALIGNMENT=alignment)
    end if
    !
    ! -- write the data line
    if (line_end) then
      call this%write_line()
    end if
    !
    ! -- finalize the table
    if (this%icount == this%maxbound) then
      call this%finalize_table()
    end if
    !
    ! -- Return
    return
  end subroutine add_real
  
  subroutine add_string(this, cval)
! ******************************************************************************
! add_string -- add string value to the dataline
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableType) :: this
    character(len=*) :: cval
    ! -- dummy
    logical :: line_end
    integer(I4B) :: j
    integer(I4B) :: ival
    real(DP) :: rval
    integer(I4B) :: width
    integer(I4B) :: alignment
! ------------------------------------------------------------------------------
    !
    ! -- update index for tableterm
    this%ientry = this%ientry + 1
    !
    ! -- initialize local variables
    j = this%ientry
    width = this%tableterm(j)%get_width()
    alignment = this%tableterm(j)%get_alignment()
    if (j == this%ntableterm) then
      line_end = .TRUE.
      call UWWORD(this%dataline, this%iloc, width, TABUCSTRING,                  &
                  cval, ival, rval, ALIGNMENT=alignment)
    else
      line_end = .FALSE.
      call UWWORD(this%dataline, this%iloc, width, TABUCSTRING,                  &
                  cval, ival, rval, ALIGNMENT=alignment)
    end if
    !
    ! -- write the data line
    if (line_end) then
      call this%write_line()
    end if
    !
    ! -- finalize the table
    if (this%icount == this%maxbound) then
      call this%finalize_table()
    end if
    !
    ! -- Return
    return
  end subroutine add_string
  
  subroutine set_maxbound(this, maxbound)
! ******************************************************************************
! set_maxbound -- reset maxbound
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableType) :: this
    integer(I4B), intent(in) :: maxbound
    ! -- local
! ------------------------------------------------------------------------------
    this%maxbound = maxbound
    !
    ! -- return
    return
  end subroutine set_maxbound   
end module TableModule
! A table term is the information needed to describe flow.
! The table object contains an array of table terms.  
! For an advanced package.  The table object describes all of 
! the flows.
module TableTermModule

  use KindModule, only: I4B, DP
  use ConstantsModule, only:  LINELENGTH, LENBUDTXT, DZERO,                     &
                              TABLEFT, TABCENTER, TABRIGHT,                     &
                              TABSTRING, TABUCSTRING, TABINTEGER, TABREAL
  use BaseDisModule, only: DisBaseType
  use InputOutputModule, only: ubdsv06, UPCASE, parseline
  use DeferredStringModule, only: deferred_string_type

  implicit none

  public :: TableTermType
  
  
  type :: TableTermType
    character(len=LINELENGTH), pointer :: tag => null()
    integer(I4B), pointer :: width => null()
    integer(I4B), pointer :: alignment => null()
    integer(I4B), pointer :: datatype => null()
    integer(I4B), pointer :: nheader_lines => null()
    integer(I4B), pointer :: nlist => null()
    
    type(deferred_string_type), dimension(:), pointer :: initial_lines => null()
    type(deferred_string_type), dimension(:), pointer :: header_lines => null()
    
    integer(I4B), pointer :: icounter => null() ! counter variable
  
  contains
  
    procedure :: initialize
    procedure, private :: allocate_scalars
    procedure :: get_width
    procedure :: get_header_lines
    procedure :: set_header
    procedure :: reset
    procedure :: update_term
    procedure :: da
    
  end type TableTermType

  contains
  
  subroutine initialize(this, tag, width, alignment, datatype)
! ******************************************************************************
! initialize -- initialize the table term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableTermType) :: this
    character(len=*), intent(in) :: tag
    integer(I4B), intent(in) :: width
    integer(I4B), intent(in), optional :: alignment
    integer(I4B), intent(in), optional :: datatype
    ! -- local
    character(len=LINELENGTH) :: string
    character(len=LINELENGTH) :: tstring
    character(len=LINELENGTH), allocatable, dimension(:) :: words
    integer(I4B) :: nwords
    integer(I4B) :: ilen
    integer(I4B) :: i
    integer(I4B) :: j

! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars
    call this%allocate_scalars()
    
    ! -- process dummy variables
    this%tag = tag
    
    if (present(alignment)) then
      this%alignment = alignment
    else
      this%alignment = TABCENTER
    end if
    
    this%width = width
    
    if (present(datatype)) then
      this%datatype = datatype
    else
      this%datatype = TABREAL
    end if
    !
    ! -- parse tag into words
    call parseline(tag, nwords, words, 0)
    !
    ! -- combine words that fit into width
    i = 0
    do 
      i = i + 1
      if (i > nwords) then
        exit
      end if
      string = trim(adjustl(words(i)))
      tstring = string
      do j = i + 1, nwords
        if (len(trim(adjustl(string))) > 0) then
          tstring = trim(adjustl(tstring)) // ' ' // trim(adjustl(words(j)))
        else
          tstring = trim(adjustl(words(j)))
        end if
        ilen = len(trim(adjustl(tstring)))
        if (ilen == 0) then
          continue
        else if (ilen <= width) then
          words(j) = ' '
          string = tstring
        else
          exit
        end if
      end do
      words(i) = string
    end do
    !
    ! -- calculate the number of header lines
    do i = 1, nwords
      ilen = len(trim(adjustl(words(i))))
      if (ilen > 0) then
        this%nheader_lines = this%nheader_lines + 1
      end if
    end do
    !
    ! allocate initial_lines and fill with words
    allocate(this%initial_lines(this%nheader_lines))
    do i = 1, this%nheader_lines 
      this%initial_lines(i)%string = words(i)(1:width)
    end do
    !
    ! -- deallocate words
    deallocate(words)
    !
    ! -- return
    return

  end subroutine initialize
  
  function get_width(this)
! ******************************************************************************
! get_width -- get column width 
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return variable
    integer(I4B) :: get_width
    ! -- modules
    ! -- dummy
    class(TableTermType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    get_width = this%width
    !
    ! -- return
    return
  end function get_width
  
  function get_header_lines(this)
! ******************************************************************************
! get_header_lines -- get the number of lines in initial_lines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return variable
    integer(I4B) :: get_header_lines
    ! -- modules
    ! -- dummy
    class(TableTermType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    get_header_lines = this%nheader_lines
    !
    ! -- return
    return
  end function get_header_lines
  
  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate table term scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableTermType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars
    allocate(this%tag)
    allocate(this%alignment)
    allocate(this%width)
    allocate(this%datatype)
    allocate(this%nlist)
    allocate(this%nheader_lines)
    !
    ! -- initialize scalars
    this%nlist = 0
    this%nheader_lines = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars
  
  subroutine da(this)
! ******************************************************************************
! da -- deallocate table terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableTermType) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- deallocate deferred character arrays
    do n = 1, this%nheader_lines
      deallocate(this%header_lines(n)%string)
    end do
    !
    ! -- deallocate scalars 
    deallocate(this%tag)
    deallocate(this%alignment)
    deallocate(this%width)
    deallocate(this%datatype)
    deallocate(this%nlist)
    deallocate(this%nheader_lines)
    deallocate(this%header_lines)
    !
    ! -- return
  end subroutine da
  
  subroutine set_header(this, nsize)
! ******************************************************************************
! da -- deallocate table terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableTermType) :: this
    integer(I4B), intent(in) :: nsize
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! 
    !
    ! -- reinitialize nheader_lines
    this%nheader_lines = nsize
    !
    ! -- deallocate temporary header lines
    !
    ! -- return
  end subroutine set_header
  
  
  
  subroutine reset(this, nlist)
! ******************************************************************************
! reset -- reset the table term and counter so terms can be updated
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableTermType) :: this
    integer(I4B), intent(in) :: nlist
! ------------------------------------------------------------------------------
    this%nlist = nlist
    this%icounter = 1
  end subroutine reset
  
  subroutine update_term(this, id1, id2, flow, auxvar)
! ******************************************************************************
! update_term -- replace the terms in position this%icounter 
!   for id1, id2, flow, and aux
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TableTermType) :: this
    integer(I4B), intent(in) :: id1
    integer(I4B), intent(in) :: id2
    real(DP), intent(in) :: flow
    real(DP), dimension(:), intent(in), optional :: auxvar
! ------------------------------------------------------------------------------
    !this%id1(this%icounter) = id1
    !this%id2(this%icounter) = id2
    !this%flow(this%icounter) = flow
    !this%icounter = this%icounter + 1
  end subroutine update_term
  
  
end module TableTermModule
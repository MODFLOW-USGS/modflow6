module TimeArraySeriesManagerModule

  use KindModule, only: DP, I4B
  use BlockParserModule,         only: BlockParserType
  use ConstantsModule,           only: DZERO, LENTIMESERIESNAME, LINELENGTH, &
                                       MAXCHARLEN, LENHUGELINE
  use InputOutputModule,         only: GetUnit, openfile
  use ListModule,                only: ListType
  use SimModule,                 only: store_error, store_error_unit, ustop
  use TdisModule,                only: delt, totimc, kper, kstp
  use TimeArraySeriesLinkModule, only: TimeArraySeriesLinkType, &
                                       ConstructTimeArraySeriesLink, &
                                       GetTimeArraySeriesLinkFromList, &
                                       AddTimeArraySeriesLinkToList
  use TimeArraySeriesModule,     only: TimeArraySeriesType, &
                                       ConstructTimeArraySeries, &
                                       GetTimeArraySeriesFromList
  use BaseDisModule,             only: DisBaseType

  implicit none

  private
  public :: TimeArraySeriesManagerType

  type TimeArraySeriesManagerType
    ! -- Public members
    integer(I4B),                    public :: iout = 0
    class(DisBaseType), pointer, public :: dis => null()
    ! -- Private members
    type(ListType), pointer, private :: boundTasLinks => null()
  contains
    ! -- Public procedures
    procedure, public :: ad => tasmgr_ad
    procedure, public :: da => tasmgr_da
    procedure, public :: CountLinks
    procedure, public :: GetLink
    procedure, public :: InitializeTimeArraySeriesManager
    procedure, public :: MakeTasLink
    procedure, public :: Reset
    ! -- Private procedures
    procedure, private :: tasmgr_add_link
    procedure, private :: tasmgr_convert_flux
  end type TimeArraySeriesManagerType

contains

! -- Type-bound procedures of TimeArraySeriesManagerType

  ! -- Public procedures

  subroutine InitializeTimeArraySeriesManager(this)
    implicit none
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    !
    allocate(this%boundTasLinks)
    !
    return
  end subroutine InitializeTimeArraySeriesManager

  subroutine tasmgr_ad(this)
    ! Time array series manager -- time step (or subtime step) advance.
    ! Call this each time step or subtime step.
    implicit none
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    ! -- local
    type(TimeArraySeriesLinkType), pointer :: tasLink => null()
    type(TimeArraySeriesType), pointer :: timearrayseries => null()
    integer(I4B) :: i, j, nlinks, nvals, isize1, isize2, inunit
    real(DP) :: begintime, endtime
    character(len=MAXCHARLEN) :: ermsg
    ! formats
    5 format(/,'Time-array-series controlled arrays' &
                ' in stress period ',i0,', time step ',i0,':')
    10  format('"',a'" package: ',a,' array obtained from time-array series "',a,'"')
    !
    ! -- Initialize time variables
    begintime = totimc
    endtime = begintime + delt
    !
    ! -- Iterate through boundtaslinks and update specified
    !    array with array of average values obtained from
    !    appropriate time series.
    if (associated(this%boundTasLinks)) then
      nlinks = this%boundTasLinks%Count()
      do i=1,nlinks
        tasLink => GetTimeArraySeriesLinkFromList(this%boundTasLinks, i)
        if (tasLink%Iprpak == 1 .and. i==1) then
          write(this%iout,5)kper,kstp
        endif
        if (tasLink%UseDefaultProc) then
          timearrayseries => tasLink%timeArraySeries
          nvals = size(tasLink%BndArray)
          call timearrayseries%GetAverageValues(nvals, tasLink%BndArray, &
                                                begintime, endtime)
          ! -- Multiply array elements by multiplier array.
          if (associated(tasLink%RMultArray)) then
            isize1 = size(tasLink%BndArray)
            isize2 = size(tasLink%RMultArray)
            if (isize1 == isize2 .and. isize1 == nvals) then
              do j=1,nvals
                tasLink%BndArray(j) = tasLink%BndArray(j) * tasLink%RMultArray(j)
              enddo
            else
              ermsg = 'Size mismatch between boundary and multiplier arrays' // &
                       ' using time-array series: ' // &
                       trim(tasLink%TimeArraySeries%Name)
              call store_error(ermsg)
              inunit = tasLink%TimeArraySeries%GetInunit()
              call store_error_unit(inunit)
              call ustop()
            endif
          endif
          ! -- If conversion from flux to flow is required, multiply by cell area
          if (tasLink%ConvertFlux) then
            call this%tasmgr_convert_flux(tasLink)
          endif
          ! -- If PRINT_INPUT is specified, write information
          !    regarding source of time-array series data
          if (tasLink%Iprpak == 1) then
            write(this%iout,10)trim(tasLink%PackageName), trim(tasLink%Text), &
                               trim(tasLink%timeArraySeries%Name)
          endif
        endif
        if (i==nlinks) then
          write(this%iout,'()')
        endif
      enddo
    endif
    !
    return
  end subroutine tasmgr_ad

  subroutine tasmgr_da(this)
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    ! -- local
    integer :: i, n
    type(TimeArraySeriesLinkType), pointer :: tasLink => null()
    !
    ! -- Deallocate contents of each TimeArraySeriesType object in list
    !    of time-array series links.
    n = this%boundTasLinks%Count()
    do i=1,n
      tasLink => GetTimeArraySeriesLinkFromList(this%boundTasLinks, i)
      call tasLink%da()
    enddo
    !
    ! -- Deallocate the list of time-array series links.
    call this%boundTasLinks%Clear(.true.)
    deallocate(this%boundTasLinks)
    !
    return
  end subroutine tasmgr_da

  subroutine Reset(this, pkgName)
    ! Call this when a new BEGIN PERIOD block is read for a new
    ! stress period.
    implicit none
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    character(len=*), intent(in) :: pkgName
    ! -- local
    integer(I4B)                      :: i, nlinks
    type(TimeArraySeriesLinkType), pointer :: taslink
    ! Zero out values for time-array-series controlled stresses.
    ! Also deallocate all taslinks too.
    ! Then when time-array series are
    ! specified in this or another stress period,
    ! a new taslink would be set up.
    !
    ! Reassign all linked elements to zero
    if (associated(this%boundTasLinks)) then
      ! Deallocate and remove all links belonging to package
      nlinks = this%boundTasLinks%Count()
      do i=nlinks,1,-1
        taslink => GetTimeArraySeriesLinkFromList(this%boundTasLinks, i)
        if (associated(taslink)) then
          call taslink%da()
          call this%boundTasLinks%RemoveNode(i, .true.)
        endif
      enddo
    endif
    !
    return
  end subroutine Reset

  subroutine MakeTasLink(this, pkgName, bndArray, iprpak, tasFiles, &
                         tasName, dis, text, convertFlux, nodelist, &
                         inunit)
    implicit none
    ! -- dummy
    class(TimeArraySeriesManagerType)          :: this
    character(len=*), intent(in)               :: pkgName
    real(DP), dimension(:), pointer    :: bndArray
    integer(I4B), intent(in)                        :: iprpak
    character(len=*), dimension(:), intent(in) :: tasFiles
    character(len=*),               intent(in) :: tasName
    class(DisBaseType),              pointer    :: dis
    character(len=*),               intent(in) :: text
    logical,                        intent(in) :: convertFlux
    integer(I4B), dimension(:), pointer, intent(in) :: nodelist
    integer(I4B),                        intent(in) :: inunit ! unit from which tasName is read
    ! -- local
    integer(I4B) :: i, nfiles
    character(LINELENGTH) :: ermsg
    character(len=MAXCHARLEN) :: tasFile
    character(len=LENTIMESERIESNAME) :: tasNameTemp
    logical :: lop, lop1
    type(TimeArraySeriesLinkType), pointer :: newTasLink
    type(TimeArraySeriesType),     pointer :: newTimeArraySeries
    !
    ! -- Find time-array series file that contains time series named "tasName"
    tasFile = ''
    nfiles = size(tasFiles)
    lop1  = .false.
    do i = 1,nfiles
      ! If tasFiles(i) is already open, skip over it to avoid file-open conflict
      ! (if it's already open, it is already being read as a time-array series).
      inquire(file=tasFiles(i), opened=lop)
      if (lop) then
        lop1 = .true.
        cycle
      endif
      call get_tasname_from_file(tasFiles(i), tasNameTemp, this%iout)
      if (tasNameTemp == tasName) then
        tasFile = tasFiles(i)
      endif
    enddo
    !
    if (tasFile=='') then
      ermsg = 'Error: Time-array series "' // trim(tasName) // '" not found.'
      call store_error(ermsg)
      if (lop1) then
        ermsg = 'There may be a file-naming conflict in this package.'
        call store_error(ermsg)
      endif
      call store_error_unit(inunit)
      call ustop()
    endif
    !
    ! -- Construct and initialize a time-array series
    call ConstructTimeArraySeries(newTimeArraySeries, tasFile)
    call newTimeArraySeries%InitializeTas(dis, this%iout)
    !
    ! -- Construct a time-array series link
    newTasLink => null()
    call ConstructTimeArraySeriesLink(newTasLink, newTimeArraySeries, &
                                      pkgName, bndArray, iprpak, &
                                      text)
    newTasLink%ConvertFlux = convertFlux
    newTasLink%nodelist => nodelist
    !
    ! -- Add link to list of links
    call this%tasmgr_add_link(newTasLink)
    !
    return
  end subroutine MakeTasLink

  function GetLink(this, indx) result(tasLink)
    implicit none
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    integer(I4B),          intent(in) :: indx
    type(TimeArraySeriesLinkType), pointer :: tasLink
    ! -- local
    !
    tasLink => null()
    !
    if (associated(this%boundTasLinks)) then
      tasLink => GetTimeArraySeriesLinkFromList(this%boundTasLinks, indx)
    endif
    !
    return
  end function GetLink

  function CountLinks(this)
    implicit none
    ! -- return
    integer(I4B) :: CountLinks
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    !
    if (associated(this%boundtaslinks)) then
      CountLinks = this%boundTasLinks%Count()
    else
      CountLinks = 0
    endif
    !
    return
  end function CountLinks

  ! -- Private procedures

  subroutine tasmgr_convert_flux(this, tasLink)
    implicit none
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    type(TimeArraySeriesLinkType), pointer, intent(inout) :: tasLink
    ! -- local
    integer(I4B) :: i, n, noder
    real(DP) :: area
    !
    n = size(tasLink%BndArray)
    do i=1,n
      noder = tasLink%nodelist(i)
      if (noder > 0) then
        area = this%dis%get_area(noder)
        tasLink%BndArray(i) = tasLink%BndArray(i) * area
      endif
    enddo
    !
    return
  end subroutine tasmgr_convert_flux

  subroutine tasmgr_add_link(this, tasLink)
    implicit none
    ! -- dummy
    class(TimeArraySeriesManagerType)      :: this
    type(TimeArraySeriesLinkType), pointer :: tasLink
    ! -- local
    !
    call AddTimeArraySeriesLinkToList(this%boundTasLinks, tasLink)
    !
    return
  end subroutine tasmgr_add_link

  ! -- Non-type-bound procedure

  subroutine get_tasname_from_file(tasFile, tasName, iout)
    ! Open a time-array-series file, read enough of the attributes
    ! block to get the time-array-series name, then close the file.
    implicit none
    ! -- dummy
    character(len=*), intent(in)  :: tasFile
    character(len=*), intent(out) :: tasName
    integer(I4B),          intent(in)  :: iout
    ! -- local
    integer(I4B) :: ierr, inunit
    logical :: found, continueread, endOfBlock
    character(len=MAXCHARLEN) :: msg
    character(len=40) :: keyword, keyvalue
    character(len=LINELENGTH) :: ermsg
    type(BlockParserType) :: parser
    !
    tasName = ''
    inunit = GetUnit()
    call openfile(inunit, 0, tasFile, 'TIME-ARRAY SERIES')
    !
    call parser%Initialize(inunit, iout)
    !
    ! -- read ATTRIBUTES block
    continueread = .false.
    ierr = 0
    call parser%GetBlock('ATTRIBUTES', found, ierr)
    if (ierr /= 0) then
      ! end of file
      ermsg = 'End-of-file encountered while searching for' // &
              ' ATTRIBUTES in time-array series ' // &
              'input file "' // trim(tasFile) // '"'
      call store_error(ermsg)
      call parser%StoreErrorUnit()
      call ustop()
    elseif (.not. found) then
      msg = 'Error: ATTRIBUTES block not found in time-array series file: ' &
            // trim(tasFile)
      call store_error(msg)
      call parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- parse ATTRIBUTES entries
    do
      ! -- read an arbitrary length line from input
      call parser%GetNextLine(endOfBlock)
      if (endOfBlock) exit
      !
      ! -- get the keyword
      call parser%GetStringCaps(keyword)
      !
      ! -- get the word following the keyword (the key value)
      call parser%GetStringCaps(keyvalue)
      select case (keyword)
      case ('NAME')
        tasName = keyvalue
        exit
      end select
    enddo
    !
    ! -- close the file
    call parser%Clear()
    !
    return
  end subroutine get_tasname_from_file

end module TimeArraySeriesManagerModule

